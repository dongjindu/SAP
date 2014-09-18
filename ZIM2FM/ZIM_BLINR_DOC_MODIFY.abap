FUNCTION ZIM_BLINR_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFBLNO) LIKE  ZTBLINR-ZFBLNO
*"     VALUE(ZFBTSEQ) LIKE  ZTBLINR-ZFBTSEQ
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTBLINR_OLD) LIKE  ZTBLINR STRUCTURE  ZTBLINR
*"     VALUE(W_ZTBLINR) LIKE  ZTBLINR STRUCTURE  ZTBLINR
*"     VALUE(W_OK_CODE)
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"      NOT_MODIFY
*"----------------------------------------------------------------------
DATA : W_ZFBLNO    LIKE   ZTBLINR-ZFBLNO,
       W_ZFBTSEQ   LIKE   ZTBLINR-ZFBTSEQ.

   MOVE-CORRESPONDING : W_ZTBLINR      TO   ZTBLINR.

   MOVE : ZFBLNO      TO     ZTBLINR-ZFBLNO,
          SY-MANDT    TO     ZTBLINR-MANDT,
          SY-UNAME    TO     ZTBLINR-UNAM,
          SY-DATUM    TO     ZTBLINR-UDAT.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ELSEIF W_OK_CODE EQ 'OPCL'.
      ZFSTATUS = 'P'.
   ENDIF.

   CASE ZFSTATUS.
      WHEN 'C'.
           UPDATE  ZTBLINOU  SET  ZFBINYN = 'X'
           WHERE   ZFBLNO    EQ   ZFBLNO
           AND     ZFBTSEQ   EQ   ZFBTSEQ.
           IF SY-SUBRC NE 0. RAISE  ERROR_UPDATE.   ENDIF.

           MOVE  SY-UNAME  TO  ZTBLINR-ERNAM.
           MOVE  SY-DATUM  TO  ZTBLINR-CDAT.
           INSERT  ZTBLINR.
           IF SY-SUBRC  NE 0.
              UPDATE  ZTBLINOU  SET  ZFBINYN = ' '
              WHERE   ZFBLNO    EQ   ZFBLNO
              AND     ZFBTSEQ   EQ   ZFBTSEQ.
              RAISE  ERROR_UPDATE.
           ENDIF.

* change document -----------------------------------------------------
         CLEAR : W_ZTBLINR_OLD.
         call function 'ZIM_CHANGE_DOCUMENT_BLINR'
              exporting
                      upd_chngind    =    'I'
                      N_ZTBLINR      =    W_ZTBLINR
                      O_ZTBLINR      =    W_ZTBLINR_OLD.
*----------------------------------------------------------------------

      WHEN 'U' OR 'O' OR 'P'.
           IF ZFSTATUS = 'O'.
              ZTBLINR-ZFDOCST = 'O'.
           ELSEIF ZFSTATUS = 'P'.
              ZTBLINR-ZFDOCST = 'N'.
           ENDIF.
           IF W_ZTBLINR_OLD EQ ZTBLINR.  RAISE NOT_MODIFY. ENDIF.

           UPDATE   ZTBLINR.
           IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
           call function 'ZIM_CHANGE_DOCUMENT_BLINR'
                exporting
                        upd_chngind    =    'U'
                        N_ZTBLINR      =    W_ZTBLINR
                        O_ZTBLINR      =    W_ZTBLINR_OLD.
*----------------------------------------------------------------------

      WHEN 'X'.
           DELETE ZTBLINR.
           IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

           UPDATE   ZTBLINOU   SET  ZFBINYN = ' '
           WHERE    ZFBLNO     EQ   ZFBLNO
           AND      ZFBTSEQ    EQ   ZFBTSEQ.
           IF SY-SUBRC NE 0.
              MOVE-CORRESPONDING W_ZTBLINR_OLD  TO ZTBLINR.
              INSERT ZTBLINR.
           ENDIF.
* change document -----------------------------------------------------
           CLEAR : W_ZTBLINR_OLD.
           call function 'ZIM_CHANGE_DOCUMENT_BLINR'
                exporting
                        upd_chngind    =    'D'
                        N_ZTBLINR      =    W_ZTBLINR
                        O_ZTBLINR      =    W_ZTBLINR_OLD.
*----------------------------------------------------------------------
   ENDCASE.
ENDFUNCTION.
