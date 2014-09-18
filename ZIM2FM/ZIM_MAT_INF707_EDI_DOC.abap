*&---------------------------------------------------------------------*
*&  Function Module 명: INF707 EDI Receive                             *
*&              작성자: 나신호 INFOLINK Ltd.                           *
*&              작성일: 2002.07.21                                     *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
FUNCTION ZIM_MAT_INF707_EDI_DOC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_FILENAME) LIKE  ZTDHF1-FILENAME
*"     VALUE(BACK_PATH) LIKE  ZTIMIMGTX-ZFRBAK
*"     REFERENCE(W_DEL) TYPE  C
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"      NOT_FOUND
*"      NO_REFERENCE
*"      DOCUMENT_LOCKED
*"      DATE_ERROR
*"      NOT_FILE_OPEN
*"----------------------------------------------------------------------
  DATA : C_ZFDDFDA1(3),
         WL_VIA(1)      TYPE C,
         L_ZFDHDOC      LIKE ZTDHF1-ZFDHDOC,
         L_NOT_FOUND    TYPE C VALUE 'N',
         W_EDI_RECORD(65535).

  DATA : WL_LENGTH      TYPE I,
         WL_LENGTH1     TYPE I,
         WL_LENGTH2     TYPE I,
         WL_DATLEN      TYPE I,
         WL_COUNT       TYPE I,
         W_READ_CNT     TYPE I.

  DATA: BEGIN OF IT_EDI OCCURS 0,
        RECORD   LIKE     W_EDI_RECORD,
        END OF IT_EDI.

* File 삭제를 위해 필요한 변수 선언.
  DATA: L_COMMAND(100)        TYPE    C.
  DATA: BEGIN OF MTAB_DATA OCCURS 0,
          LINE(132)   TYPE C,
        END OF MTAB_DATA.

  REFRESH : IT_TAB, RETURN.
  L_NOT_FOUND = 'N'.

  CLEAR : L_ZFDHDOC,  W_TABIX.
  OPEN    DATASET    W_FILENAME     FOR     INPUT   IN  TEXT  MODE.

  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  DO.
    READ    DATASET    W_FILENAME     INTO    W_EDI_RECORD.
    IF SY-SUBRC    EQ    4.
      EXIT.
    ENDIF.

    W_READ_CNT = W_READ_CNT + 1.

*>> 문서의 시작.
    IF W_EDI_RECORD(6) EQ 'INF707' .
*      CLEAR   : IT_TAB, IT_EDI.
*      REFRESH : IT_EDI.

      W_READ_CNT = 1.
*         MOVE:   W_EDI_RECORD(06)         TO      IT_TAB-ZFDHDOC.
*         APPEND  IT_TAB.
      W_TABIX = SY-TABIX.
      L_ZFDHDOC = W_EDI_RECORD(06).
    ELSE.
      CASE L_ZFDHDOC.
        WHEN 'INF707'.  ">개설응답서(L/C).
          IF W_READ_CNT GE 3.
            CLEAR   : IT_TAB, IT_EDI.
            REFRESH : IT_EDI.

            MOVE  L_ZFDHDOC  TO  IT_TAB-ZFDHDOC.
            SPLIT W_EDI_RECORD AT '|' INTO TABLE IT_EDI.

            "관리번호.
            READ TABLE IT_EDI INDEX 3.
            MOVE IT_EDI-RECORD(17)  TO  IT_TAB-ZFDOCNO.

            " L/C No.
*            READ TABLE IT_EDI INDEX 26.
*            MOVE IT_EDI-RECORD      TO  IT_TAB-ZFOPNNO.

            "변경일자.
            READ TABLE IT_EDI INDEX 34.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                 EXPORTING
                      DATE_EXTERNAL = IT_EDI-RECORD(8)
                 IMPORTING
                      DATE_INTERNAL = IT_TAB-ZFOPNDT.
            APPEND  IT_TAB.

          ENDIF.
      ENDCASE.
    ENDIF.
  ENDDO.

  OPEN DATASET W_FILENAME FOR INPUT IN TEXT MODE.
  IF SY-SUBRC NE 0.
    MESSAGE E970 WITH W_FILENAME RAISING NOT_FILE_OPEN.
    EXIT.
  ENDIF.

  READ    DATASET   W_FILENAME     INTO    W_EDI_RECORD.
  IF SY-SUBRC    EQ    4.
    EXIT.
  ENDIF.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.

    CLEAR : ZTREQHD, ZTREQST.
* 수입의뢰 문서 SELECT
    SELECT * FROM ZTREQST UP TO 1 ROWS
                          WHERE   ZFDOCNO  EQ IT_TAB-ZFDOCNO
                          AND     ZFAMDNO  GT '00000'
                          ORDER BY ZFREQNO DESCENDING.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      MESSAGE S250 WITH W_ZFDHENO.
      EXIT.
    ENDIF.

    SELECT SINGLE * FROM ZTREQHD
                    WHERE ZFREQNO  EQ  ZTREQST-ZFREQNO.

* LOCK CHECK
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = ZTREQST-ZFREQNO
              ZFAMDNO = ZTREQST-ZFAMDNO
         EXCEPTIONS
              OTHERS  = 1.

    IF SY-SUBRC <> 0.
      MESSAGE S510 WITH SY-MSGV1 '수입의뢰문서'
                          ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
      RAISE DOCUMENT_LOCKED.
      PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                  USING   'E'.
*      CONTINUE.
    ENDIF.

*-----------------------------------------------------------------------
* 변경이력을 위?
    O_ZTREQST = ZTREQST.
*-----------------------------------------------------------------------

    CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
         EXPORTING
              W_ZFCDDOC = 'INF700'
              W_ZFDHSRO = SPACE
              W_ZFDHREF = SPACE
              W_BUKRS   = ZTREQHD-BUKRS
              W_LOG_ID  = 'N'
         CHANGING
              W_ZFDHENO = IT_TAB-ZFDOCNOR
         EXCEPTIONS
              DB_ERROR  = 4
              NO_TYPE   = 8.

*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
* 상태 변?
    MOVE : SY-UNAME        TO   ZTREQST-UNAM,
           SY-DATUM        TO   ZTREQST-UDAT,
           IT_TAB-ZFOPNDT  TO   ZTREQST-ZFOPNDT,
*           IT_TAB-ZFOPNNO  TO   ZTREQST-ZFOPNNO,
           IT_TAB-ZFDOCNOR TO   ZTREQST-ZFDOCNOR,
           'O'             TO   ZTREQST-ZFDOCST,
           'R'             TO   ZTREQST-ZFEDIST.

    UPDATE ZTREQST.
    IF SY-SUBRC NE  0.   RAISE    UPDATE_ERROR.   ENDIF.

*>> HEADER 변경.
    SELECT SINGLE * FROM ZTREQHD
                    WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
    MOVE : ZTREQST-ZFOPNNO   TO ZTREQHD-ZFOPNNO,
           ZTREQHD-ZFREQED   TO ZTREQHD-ZFLASTSD,
           ZTREQHD-ZFREQSD   TO ZTREQHD-ZFLASTED.
    UPDATE ZTREQHD.
    IF SY-SUBRC NE  0.   RAISE    UPDATE_ERROR.   ENDIF.

* CHANGE DOCUMENT
    CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_STATUS'
         EXPORTING
              W_ZFREQNO = ZTREQST-ZFREQNO
              W_ZFAMDNO = ZTREQST-ZFAMDNO
              N_ZTREQST = ZTREQST
              O_ZTREQST = O_ZTREQST.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = ZTREQST-ZFREQNO
              ZFAMDNO = ZTREQST-ZFAMDNO.


    MOVE-CORRESPONDING IT_TAB   TO  ZTDHF1.
    ZTDHF1-ZFDHAPP = 'Y'.
    ZTDHF1-ZFDHSSD = SY-DATUM.
    ZTDHF1-ZFDHSST = SY-UZEIT.
*    ZTDHF1-FILENAME = IT_TAB
    UPDATE  ZTDHF1.

    MESSAGE  S124  WITH  ZTREQST-ZFREQNO ZTREQST-ZFAMDNO '저장'.
    PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                USING   'S'.
  ENDLOOP.
  IF W_DEL EQ 'X'.
     DELETE DATASET  W_FILENAME.
*    IF NOT W_FILENAME IS INITIAL.

*      CONCATENATE 'cmd /c del' W_FILENAME
*             INTO L_COMMAND SEPARATED BY SPACE.

*      CALL FUNCTION  'RFC_REMOTE_PIPE'  DESTINATION  'INFOLINK_FTP'
*         EXPORTING
*            COMMAND  =  L_COMMAND
*            WRITE    =  'X'
*            READ     =  'X'
*         TABLES
*            PIPEDATA =  MTAB_DATA.
*    ENDIF.
  ENDIF.
ENDFUNCTION.
