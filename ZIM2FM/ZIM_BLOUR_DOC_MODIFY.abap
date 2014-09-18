FUNCTION ZIM_BLOUR_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFBLNO) LIKE  ZTBLOUR-ZFBLNO
*"     VALUE(ZFBTSEQ) LIKE  ZTBLOUR-ZFBTSEQ
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTBLOUR_OLD) LIKE  ZTBLOUR STRUCTURE  ZTBLOUR
*"     VALUE(W_ZTBLOUR) LIKE  ZTBLOUR STRUCTURE  ZTBLOUR
*"     VALUE(W_OK_CODE)
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"      NOT_MODIFY
*"----------------------------------------------------------------------

   MOVE-CORRESPONDING : W_ZTBLOUR      TO   ZTBLOUR.

   MOVE : ZFBLNO      TO     ZTBLOUR-ZFBLNO,
          SY-MANDT    TO     ZTBLOUR-MANDT,
          SY-UNAME    TO     ZTBLOUR-UNAM,
          SY-DATUM    TO     ZTBLOUR-UDAT.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ELSEIF W_OK_CODE EQ 'OPCL'.
      ZFSTATUS = 'P'.
   ENDIF.

   CASE ZFSTATUS.
      WHEN 'C'.
           UPDATE  ZTBLINOU  SET  ZFBOUYN = 'X'
           WHERE   ZFBLNO    EQ   ZFBLNO
           AND     ZFBTSEQ   EQ   ZFBTSEQ.
           IF SY-SUBRC NE 0. RAISE  ERROR_UPDATE.   ENDIF.

           IF ZTBLOUR-ZFOUTY EQ '61'.   " ¿Ã∞Ìπ›?
              SELECT MAX( ZFMOVENO ) INTO  ZTBLOUR-ZFMOVENO
                                     FROM  ZTBLOUR
                                     WHERE ZFYR  EQ  ZTBLOUR-ZFYR.
              ZTBLOUR-ZFMOVENO = ZTBLOUR-ZFMOVENO + 1.
           ENDIF.
           MOVE  SY-UNAME  TO  ZTBLOUR-ERNAM.
           MOVE  SY-DATUM  TO  ZTBLOUR-CDAT.

           INSERT  ZTBLOUR.
           IF SY-SUBRC  NE 0.
              UPDATE  ZTBLINOU  SET  ZFBOUYN = ' '
              WHERE   ZFBLNO    EQ   ZFBLNO
              AND     ZFBTSEQ   EQ   ZFBTSEQ.
              RAISE  ERROR_UPDATE.
           ENDIF.
* change document -----------------------------------------------------
           CLEAR : W_ZTBLOUR_OLD.
           call function 'ZIM_CHANGE_DOCUMENT_BLOUR'
                exporting
                      upd_chngind    =    'I'
                      N_ZTBLOUR      =    W_ZTBLOUR
                      O_ZTBLOUR      =    W_ZTBLOUR_OLD.
*----------------------------------------------------------------------

      WHEN 'U' OR 'O' OR 'P'.
           IF ZFSTATUS = 'O'.
              ZTBLOUR-ZFDOCST = 'O'.
           ELSEIF ZFSTATUS = 'P'.
              ZTBLOUR-ZFDOCST = 'N'.
           ENDIF.
           IF W_ZTBLOUR_OLD EQ ZTBLOUR.  RAISE NOT_MODIFY. ENDIF.

           UPDATE   ZTBLOUR.
           IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
           call function 'ZIM_CHANGE_DOCUMENT_BLOUR'
                exporting
                      upd_chngind    =    'U'
                      N_ZTBLOUR      =    W_ZTBLOUR
                      O_ZTBLOUR      =    W_ZTBLOUR_OLD.
*----------------------------------------------------------------------
      WHEN 'X'.
           DELETE ZTBLOUR.
           IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

           UPDATE   ZTBLINOU   SET  ZFBOUYN = ' '
           WHERE    ZFBLNO     EQ   ZFBLNO
           AND      ZFBTSEQ    EQ   ZFBTSEQ.
           IF SY-SUBRC NE 0.
              MOVE-CORRESPONDING W_ZTBLOUR_OLD  TO ZTBLOUR.
              INSERT ZTBLOUR.
           ENDIF.
* change document -----------------------------------------------------
           call function 'ZIM_CHANGE_DOCUMENT_BLOUR'
                exporting
                      upd_chngind    =    'D'
                      N_ZTBLOUR      =    W_ZTBLOUR
                      O_ZTBLOUR      =    W_ZTBLOUR_OLD.
*----------------------------------------------------------------------

   ENDCASE.

ENDFUNCTION.
