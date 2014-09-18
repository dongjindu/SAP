FUNCTION ZIM_LG_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFBLNO) LIKE  ZTLG-ZFBLNO
*"     VALUE(ZFLGSEQ) LIKE  ZTLG-ZFLGSEQ
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTLG_OLD) LIKE  ZTLG STRUCTURE  ZTLG
*"     VALUE(W_ZTLG) LIKE  ZTLG STRUCTURE  ZTLG
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSLGGOD_OLD STRUCTURE  ZSLGGOD
*"      IT_ZSLGGOD STRUCTURE  ZSLGGOD
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
DATA : W_ZFLGOD     LIKE    ZTLGGOD-ZFLGOD.
* DELETE 여부 SET
   IF W_OK_CODE EQ 'DELE'.   ZFSTATUS = 'X'.   ENDIF.

   MOVE-CORRESPONDING : W_ZTLG      TO   ZTLG.

   MOVE : ZFBLNO      TO     ZTLG-ZFBLNO,
          ZFLGSEQ     TO     ZTLG-ZFLGSEQ,
          SY-MANDT    TO     ZTLG-MANDT.

* 변경일자
  MOVE : SY-DATUM TO ZTLG-UDAT,
         SY-UNAME TO ZTLG-UNAM.

   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         MOVE : SY-DATUM TO ZTLG-CDAT,
                SY-UNAME TO ZTLG-ERNAM.
         INSERT   ZTLG.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document ------------------------------------------------------
         CLEAR : W_ZTLG_OLD.
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_LG'
              EXPORTING
                      UPD_CHNGIND    =    'I'
                      N_ZTLG         =    ZTLG
                      O_ZTLG         =    W_ZTLG_OLD.
*-----------------------------------------------------------------------
* L/G 상품명세
         LOOP AT IT_ZSLGGOD.
            CLEAR : ZTLGGOD.
            MOVE-CORRESPONDING IT_ZSLGGOD   TO ZTLGGOD.
            MOVE : ZFBLNO                 TO ZTLGGOD-ZFBLNO,
                   ZFLGSEQ                TO ZTLGGOD-ZFLGSEQ,
                   SY-MANDT               TO ZTLGGOD-MANDT.

            INSERT   ZTLGGOD.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.
      WHEN 'X'.               " 삭제
         DELETE  FROM ZTLG WHERE ZFBLNO  EQ ZFBLNO
                           AND   ZFLGSEQ EQ ZFLGSEQ.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document ------------------------------------------------------
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_LG'
              EXPORTING
                      UPD_CHNGIND    =    'D'
                      N_ZTLG         =    ZTLG
                      O_ZTLG         =    W_ZTLG_OLD.
*-----------------------------------------------------------------------

         DELETE  FROM ZTLGGOD WHERE ZFBLNO EQ ZFBLNO
                              AND   ZFLGSEQ EQ ZFLGSEQ.

      WHEN OTHERS.            " 변경
         UPDATE   ZTLG.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document ------------------------------------------------------
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_LG'
              EXPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTLG         =    ZTLG
                      O_ZTLG         =    W_ZTLG_OLD.
*-----------------------------------------------------------------------

* L/G 상품명세
         SELECT * FROM ZTLGGOD WHERE ZFBLNO   EQ  ZFBLNO
                               AND   ZFLGSEQ  EQ  ZFLGSEQ.

            READ TABLE IT_ZSLGGOD WITH KEY ZFLGOD  = ZTLGGOD-ZFLGOD
                                  BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSLGGOD TO ZTLGGOD.
               UPDATE ZTLGGOD.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTLGGOD.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSLGGOD.
            SELECT SINGLE * FROM  ZTLGGOD
                            WHERE ZFBLNO   EQ  ZFBLNO
                            AND   ZFLGOD   EQ  IT_ZSLGGOD-ZFLGOD.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSLGGOD TO ZTLGGOD.
               MOVE : ZFBLNO                 TO ZTLGGOD-ZFBLNO,
                      ZFLGSEQ                TO ZTLGGOD-ZFLGSEQ,
                      SY-MANDT               TO ZTLGGOD-MANDT.

               INSERT  ZTLGGOD.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.
   ENDCASE.

ENDFUNCTION.
