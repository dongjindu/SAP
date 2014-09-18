FUNCTION ZIM_LG_INF700_EDI_DOC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_FILENAME) LIKE  ZTDHF1-FILENAME
*"     REFERENCE(BACK_PATH) LIKE  ZTIMIMGTX-ZFRBAK
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"      NOT_FOUND
*"      NO_REFERENCE
*"      DOCUMENT_LOCKED
*"      DATE_ERROR
*"      NOT_FILE_OPEN
*"----------------------------------------------------------------------
DATA : C_ZFDDFDA1(3),
       WL_VIA(1)    TYPE C,
       L_ZFDHDOC    LIKE ZTDHF1-ZFDHDOC,
       L_NOT_FOUND  TYPE C VALUE 'N',
       W_EDI_RECORD(65535).

  REFRESH : IT_TAB, RETURN.
  CLEAR : L_ZFDHDOC,  W_TABIX, IT_TAB, RETURN.
  L_NOT_FOUND = 'N'.

  OPEN    DATASET   W_FILENAME     FOR     INPUT   IN  TEXT  MODE.
  IF SY-SUBRC NE 0.
     MESSAGE E970 WITH W_FILENAME RAISING NOT_FILE_OPEN.
     EXIT.
  ENDIF.

  DO.
     READ    DATASET   W_FILENAME     INTO    W_EDI_RECORD.
     IF SY-SUBRC    EQ    4.
        EXIT.
     ENDIF.

*>> 문서의 시작.
     IF W_EDI_RECORD(2) EQ '<<'.
        CLEAR: IT_TAB.
        MOVE : W_FILENAME              TO IT_TAB-FILENAME,
               W_EDI_RECORD+24(30)     TO IT_TAB-ZFDOCNOR,
               W_EDI_RECORD+65(06)     TO L_ZFDHDOC.
        APPEND  IT_TAB.
        W_TABIX = SY-TABIX.
        L_NOT_FOUND = 'N'.
      ELSE.
*>> 찾지 못했을 경우, CONTINUE.
         IF L_NOT_FOUND EQ 'Y'.
            CONTINUE.
         ENDIF.

         IF L_ZFDHDOC EQ 'INF700'.       ">개설응답서(L/C).
            CASE  W_EDI_RECORD(02).
               WHEN '01'.     ">송신전자문서번호.
                   MOVE: W_EDI_RECORD+5(35) TO IT_TAB-ZFDOCNO,
                         W_EDI_RECORD+5(35) TO W_ZFDHENO.
                   SELECT SINGLE * INTO IT_TAB FROM  ZTDHF1
                          WHERE ZFDHENO  EQ W_ZFDHENO.
                   IF SY-SUBRC EQ 0.
                      MOVE : W_FILENAME       TO IT_TAB-FILENAME.
                      MODIFY IT_TAB INDEX W_TABIX.
                      L_NOT_FOUND = 'N'.
                   ELSE.
                      MESSAGE S250 WITH W_ZFDHENO.
                      PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                                  USING   'E'.
                      DELETE IT_TAB INDEX W_TABIX.
                      L_NOT_FOUND = 'Y'.
                   ENDIF.
               WHEN '05'.     ">L/C No.
                  IF W_EDI_RECORD+2(03) EQ 'AAC'.
                     MOVE: W_EDI_RECORD+5(35) TO IT_TAB-ZFOPNNO.
                     MODIFY IT_TAB INDEX W_TABIX.
                  ENDIF.
               WHEN '06'.     ">개설일자.
                  IF W_EDI_RECORD+2(03) EQ '182'.
*-----------------------------------------------------------------------
* DATE CONVERT
*-----------------------------------------------------------------------
                     CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                          EXPORTING
                              DATE_EXTERNAL = W_EDI_RECORD+5(08)
                          IMPORTING
                              DATE_INTERNAL = IT_TAB-ZFOPNDT.
                     MODIFY IT_TAB INDEX W_TABIX.
                  ENDIF.
               WHEN OTHERS.
            ENDCASE.
         ENDIF.
      ENDIF.
  ENDDO.

  CLOSE DATASET    W_FILENAME.


  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.

     CLEAR : ZTREQHD, ZTREQST.
* 수입의뢰 문서 SELECT
     SELECT * FROM ZTREQST UP TO 1 ROWS
                           WHERE   ZFDOCNO  EQ IT_TAB-ZFDOCNO
                           AND     ZFAMDNO EQ '00000'
                           ORDER BY ZFREQNO DESCENDING.
        EXIT.
     ENDSELECT.
     IF SY-SUBRC NE 0.
        MESSAGE S250 WITH W_ZFDHENO.
        PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                    USING   'E'.

        CONTINUE.
     ENDIF.

     SELECT SINGLE * FROM ZTREQHD
                     WHERE ZFREQNO  EQ  ZTREQST-ZFREQNO.

* LOCK CHECK
     CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
          EXPORTING
             ZFREQNO                =     ZTREQST-ZFREQNO
             ZFAMDNO                =     ZTREQST-ZFAMDNO
       EXCEPTIONS
             OTHERS        = 1.

     IF SY-SUBRC <> 0.
        MESSAGE S510 WITH SY-MSGV1 '수입의뢰문서'
                            ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
*                  RAISING DOCUMENT_LOCKED.
        PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                    USING   'E'.
        CONTINUE.
     ENDIF.

*-----------------------------------------------------------------------
* 변경이력을 위해
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
* 상태 변경
     MOVE : SY-UNAME        TO   ZTREQST-UNAM,
            SY-DATUM        TO   ZTREQST-UDAT,
            IT_TAB-ZFOPNDT  TO   ZTREQST-ZFOPNDT,
            IT_TAB-ZFOPNNO  TO   ZTREQST-ZFOPNNO,
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
           W_ZFREQNO      =     ZTREQST-ZFREQNO
           W_ZFAMDNO      =     ZTREQST-ZFAMDNO
           N_ZTREQST      =     ZTREQST
           O_ZTREQST      =     O_ZTREQST.

     CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
            EXPORTING
                ZFREQNO                =     ZTREQST-ZFREQNO
                ZFAMDNO                =     ZTREQST-ZFAMDNO.


     MOVE-CORRESPONDING IT_TAB   TO  ZTDHF1.
     ZTDHF1-ZFDHAPP = 'Y'.
     ZTDHF1-ZFDHSSD = SY-DATUM.
     ZTDHF1-ZFDHSST = SY-UZEIT.
*    ZTDHF1-FILENAME = IT_TAB
     UPDATE  ZTDHF1.

     MESSAGE  S124  WITH  ZTREQST-ZFREQNO ZTREQST-ZFAMDNO '저장'.
     PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                 USING   'S'.


     CALL FUNCTION 'ZIM_INBOUND_DATA_BACKUP'
          EXPORTING
             FILE_NAME   =   IT_TAB-FILENAME
             BACK_PATH   =   BACK_PATH
          EXCEPTIONS
             NOT_FOUND   =   4
             NO_DATA     =   8.

   ENDLOOP.

ENDFUNCTION.
