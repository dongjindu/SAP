*&---------------------------------------------------------------------*
*&  Function Module 명: LOGAUR EDI Receive                             *
*&              작성자: 나신호 INFOLINK Ltd.                           *
*&              작성일: 2002.09.13                                     *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
FUNCTION ZIM_MAT_LOGUAR_EDI_DOC .
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

  REFRESH : IT_TAB_LG, RETURN.
  L_NOT_FOUND = 'N'.

  CLEAR : L_ZFDHDOC,  W_TABIX.
  OPEN    DATASET    W_FILENAME     FOR     INPUT   IN  TEXT  MODE.

  IF SY-SUBRC NE 0.  EXIT.  ENDIF.

  DO.
    READ    DATASET    W_FILENAME     INTO    W_EDI_RECORD.
    IF SY-SUBRC EQ 4.   EXIT.   ENDIF.
    W_READ_CNT = W_READ_CNT + 1.

*>> 문서의 시작.
    IF W_EDI_RECORD(6) EQ 'LOGUAR' .
*      CLEAR   : IT_TAB_LG, IT_EDI.
*      REFRESH : IT_EDI.

      W_READ_CNT = 1.
*      MOVE:   W_EDI_RECORD(06)         TO      IT_TAB_LG-ZFDHDOC.
*      APPEND  IT_TAB_LG.
      W_TABIX = SY-TABIX.
      L_ZFDHDOC = W_EDI_RECORD(06).
    ELSE.
      CASE L_ZFDHDOC.
        WHEN 'LOGUAR'.                 "> Letter of Guarantee.
          IF W_READ_CNT GE 3.
            CLEAR   : IT_TAB_LG, IT_EDI.
            REFRESH : IT_EDI.

            MOVE  L_ZFDHDOC  TO  IT_TAB-ZFDHDOC.
            SPLIT W_EDI_RECORD AT '|' INTO TABLE IT_EDI.

            READ TABLE IT_EDI INDEX 3.             " 관리번호.
            MOVE IT_EDI-RECORD(17)  TO  IT_TAB_LG-ZFDOCNO.

            READ TABLE IT_EDI  INDEX 22.           " 개설번호.
            MOVE IT_EDI-RECORD(35) TO   IT_TAB_LG-ZFLGINO.

            READ TABLE IT_EDI INDEX 31.            " 발급일자.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                 EXPORTING
                      DATE_EXTERNAL = IT_EDI-RECORD(8)
                 IMPORTING
                      DATE_INTERNAL = IT_TAB_LG-ZFLGIDT.
            APPEND  IT_TAB_LG.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDDO.

  CLOSE DATASET    W_FILENAME.

  LOOP AT IT_TAB_LG.
    CLEAR : ZTLG, ZTLGGOD.
* Letter of Guarantee 문서 Select.
    SELECT SINGLE *
             FROM ZTLG
            WHERE ZFDOCNO EQ IT_TAB_LG-ZFDOCNO.
    MOVE ZTLG-ZFBLNO TO IT_TAB_LG-ZFBLNO.
    MODIFY  IT_TAB_LG INDEX  W_TABIX.

    W_TABIX = SY-TABIX.
    IF SY-SUBRC NE 0.
      MESSAGE S250 WITH W_ZFDHENO.
      EXIT.
    ENDIF.

* LOCK CHECK
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTLGDOC'
         EXPORTING
              ZFBLNO  = ZTLG-ZFBLNO
              ZFLGSEQ = ZTLG-ZFLGSEQ.

    IF SY-SUBRC <> 0.
      MESSAGE S510 WITH SY-MSGV1 'L/G Document'
                          ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
      RAISE DOCUMENT_LOCKED.
      PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                  USING   'E'.
    ENDIF.

*-----------------------------------------------------------------------
* 변경이력을 위해.
    O_ZTLG = ZTLG.
*-----------------------------------------------------------------------

    CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
         EXPORTING
              W_ZFCDDOC = 'LOGUAR'
              W_ZFDHSRO = SPACE
              W_ZFDHREF = SPACE
              W_ZFEDIID = ZTIMIMGTX-ZFEDIID
              W_BUKRS   = ZTLG-BUKRS
              W_LOG_ID  = 'N'
         CHANGING
              W_ZFDHENO = IT_TAB_LG-ZFDOCNOR
         EXCEPTIONS
              DB_ERROR  = 4
              NO_TYPE   = 8.

*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
* 상태 변?
    MOVE : SY-UNAME           TO   ZTLG-UNAM,
           SY-DATUM           TO   ZTLG-UDAT,
           IT_TAB_LG-ZFBLNO   TO   ZTLG-ZFBLNO,
           IT_TAB_LG-ZFLGIDT  TO   ZTLG-ZFLGIDT,
           IT_TAB_LG-ZFLGINO  TO   ZTLG-ZFLGINO,
           IT_TAB_LG-ZFDOCNOR TO   ZTLG-ZFDOCNOR,
           'O'                TO   ZTLG-ZFDOCST,
           'R'                TO   ZTLG-ZFEDIST.

    UPDATE ZTLG.
    IF SY-SUBRC NE  0.   RAISE    UPDATE_ERROR.   ENDIF.

*>> HEADER 변경.
*  SELECT SINGLE * FROM ZTREQHD
*                  WHERE ZFREQNO EQ ZTREQST-ZFREQNO.
*  MOVE : ZTREQST-ZFOPNNO   TO ZTREQHD-ZFOPNNO,
*         ZTREQHD-ZFREQED   TO ZTREQHD-ZFLASTSD,
*         ZTREQHD-ZFREQSD   TO ZTREQHD-ZFLASTED.
*  UPDATE ZTREQHD.
*  IF SY-SUBRC NE  0.   RAISE    UPDATE_ERROR.   ENDIF.

* CHANGE DOCUMENT
    CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_LG'
         EXPORTING
              UPD_CHNGIND = 'U'
              N_ZTLG      = ZTLG
              O_ZTLG      = O_ZTLG.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTLGDOC'
         EXPORTING
              ZFBLNO  = ZTLG-ZFBLNO
              ZFLGSEQ = ZTLG-ZFLGSEQ.

    MOVE-CORRESPONDING IT_TAB_LG   TO  ZTDHF1.
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
    IF NOT W_FILENAME IS INITIAL.
      CONCATENATE 'cmd /c del' W_FILENAME
             INTO L_COMMAND SEPARATED BY SPACE.

      CALL FUNCTION  'RFC_REMOTE_PIPE'  DESTINATION  'INFOLINK_FTP'
         EXPORTING
            COMMAND  =  L_COMMAND
            WRITE    =  'X'
            READ     =  'X'
         TABLES
            PIPEDATA =  MTAB_DATA.
    ENDIF.
  ENDIF.
ENDFUNCTION.
