FUNCTION ZIM_ZTIDRUS_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFIVNO) LIKE  ZTIDRUS-ZFIVNO
*"     VALUE(ZFCLSEQ) LIKE  ZTIDRUS-ZFCLSEQ
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTIDRUS_OLD) LIKE  ZTIDRUS STRUCTURE  ZTIDRUS
*"     VALUE(W_ZTIDRUS) LIKE  ZTIDRUS STRUCTURE  ZTIDRUS
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSIDRUSH_OLD STRUCTURE  ZSIDRUSH OPTIONAL
*"      IT_ZSIDRUSH STRUCTURE  ZSIDRUSH
*"      IT_ZSIDRUSD_OLD STRUCTURE  ZSIDRUSD OPTIONAL
*"      IT_ZSIDRUSD STRUCTURE  ZSIDRUSD
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"      ERROR_DELETE
*"----------------------------------------------------------------------

*>> KSB INSERT.
DATA : L_ZFIVNO          LIKE   ZTIV-ZFIVNO.

   MOVE-CORRESPONDING  W_ZTIDRUS  TO  ZTIDRUS.

   MOVE : W_ZTIDRUS-ZFIVNO TO  L_ZFIVNO,
          ZFIVNO           TO  ZTIDRUS-ZFIVNO,
          ZFCLSEQ          TO  ZTIDRUS-ZFCLSEQ,
          SY-MANDT         TO  ZTIDRUS-MANDT,
          SY-UNAME         TO  ZTIDRUS-UNAM,
          SY-DATUM         TO  ZTIDRUS-UDAT.

   " B/L Data Get.
   SELECT SINGLE * FROM  ZTBL WHERE ZFBLNO = ZTIDRUS-ZFBLNO.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ENDIF.

   CASE ZFSTATUS.
      WHEN 'X'.
         DELETE  FROM ZTIDRUS  WHERE ZFIVNO  EQ ZFIVNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.
         IF SY-SUBRC NE 0.  RAISE  ERROR_DELETE.  ENDIF.

         DELETE  FROM ZTIDRUSH WHERE ZFIVNO  EQ ZFIVNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.

         DELETE  FROM ZTIDRUSD WHERE ZFIVNO  EQ ZFIVNO
                               AND   ZFCLSEQ EQ ZFCLSEQ.

         " INVOICE Status Change.
         IF W_SUBRC EQ 0.
            CALL  FUNCTION 'ZIM_GET_CC_DOCUMENT'
                  EXPORTING
                     ZFIVNO         =    ZFIVNO
                  IMPORTING
                     W_ZTIV         =    ZTIV
                  TABLES
                     IT_ZSIVIT      =    IT_ZSIVIT
                     IT_ZSIVIT_ORG  =    IT_ZSIVIT_ORG
                  EXCEPTIONS
                     NOT_FOUND      =    4
                     NOT_INPUT      =    8.

            MOVE-CORRESPONDING ZTIV TO *ZTIV.

            IF SY-SUBRC EQ 0.
               SELECT SINGLE * FROM ZTIMIMG00.

               CASE ZTIMIMG00-ZFIMPATH.
                  WHEN '1'.
                     MOVE : 'SAVE'    TO   W_OK_CODE,
                            'U'       TO   ZFSTATUS.

                     IF ZTIV-ZFCLCD EQ 'X'.
                        EXIT.
                     ELSE.
                        MOVE : '1'      TO   ZTIV-ZFCUST,
                               SY-DATUM TO   ZTIV-UDAT,
                               SY-UNAME TO   ZTIV-UNAM.
                     ENDIF.

                  WHEN '2' OR '3'.
                     MOVE : 'DELE' TO W_OK_CODE,
                            'D'    TO ZFSTATUS.
                  WHEN OTHERS.
               ENDCASE.

               CALL FUNCTION 'ZIM_CUSTOMS_CLEARANCE_MODIFY'
                    EXPORTING
                      W_OK_CODE           =   W_OK_CODE
                      ZFIVNO              =   ZTIV-ZFIVNO
                      ZFSTATUS            =   ZFSTATUS
                      W_ZTIV              =   ZTIV
                      W_ZTIV_OLD          =  *ZTIV
                   TABLES
                      IT_ZSIVIT           =    IT_ZSIVIT
                      IT_ZSIVIT_OLD       =    IT_ZSIVIT_ORG
                   EXCEPTIONS
                      ERROR_UPDATE        =    4.
               IF SY-SUBRC EQ 0.
                  EXIT.
               ENDIF.
            ENDIF.
         ENDIF.
         EXIT.
      WHEN OTHERS.            " Change
         UPDATE   ZTIDRUS.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         " HS Code Detail
         SELECT * FROM ZTIDRUSD  WHERE ZFIVNO   EQ  ZFIVNO
                                 AND   ZFCLSEQ  EQ  ZFCLSEQ.

            READ TABLE IT_ZSIDRUSD  WITH KEY ZFIVNO  = ZTIDRUSD-ZFIVNO
                                             ZFCLSEQ = ZTIDRUSD-ZFCLSEQ
                                             ZFCONO  = ZTIDRUSD-ZFCONO
                                             ZFRONO  = ZTIDRUSD-ZFRONO
                                    BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSIDRUSD  TO ZTIDRUSD.
               UPDATE ZTIDRUSD.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTIDRUSD.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIDRUSD.
            SELECT SINGLE * FROM  ZTIDRUSD
                            WHERE ZFIVNO   EQ  ZFIVNO
                            AND   ZFCLSEQ  EQ  ZFCLSEQ
                            AND   ZFCONO   EQ  IT_ZSIDRUSD-ZFCONO
                            AND   ZFRONO   EQ  IT_ZSIDRUSD-ZFRONO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIDRUSD  TO ZTIDRUSD.

               MOVE : ZFIVNO                 TO ZTIDRUSD-ZFIVNO,
                      ZFCLSEQ                TO ZTIDRUSD-ZFCLSEQ,
                      SY-MANDT               TO ZTIDRUSD-MANDT.

               INSERT  ZTIDRUSD.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.

         " HS Data.
         SELECT * FROM ZTIDRUSH WHERE ZFIVNO   EQ  ZFIVNO
                                AND   ZFCLSEQ  EQ  ZFCLSEQ.

            READ TABLE IT_ZSIDRUSH WITH KEY ZFIVNO  = ZTIDRUSH-ZFIVNO
                                            ZFCLSEQ = ZTIDRUSH-ZFCLSEQ
                                            ZFCONO  = ZTIDRUSH-ZFCONO
                                   BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSIDRUSH TO ZTIDRUSH.
               UPDATE ZTIDRUSH.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTIDRUSH.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIDRUSH.
            SELECT SINGLE * FROM  ZTIDRUSH
                            WHERE ZFIVNO   EQ  ZFIVNO
                            AND   ZFCLSEQ  EQ  ZFCLSEQ
                            AND   ZFCONO   EQ  IT_ZSIDRUSH-ZFCONO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIDRUSH TO ZTIDRUSH.
               MOVE : ZFIVNO                  TO ZTIDRUSH-ZFIVNO,
                      ZFCLSEQ                 TO ZTIDRUSH-ZFCLSEQ,
                      SY-MANDT                TO ZTIDRUSH-MANDT.

               INSERT  ZTIDRUSH.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.
   ENDCASE.

   MOVE SY-UNAME TO ZTIDRUS-UNAM.
   MOVE SY-DATUM TO ZTIDRUS-UDAT.
   UPDATE ZTIDRUS.
   IF SY-SUBRC NE  0.  RAISE ERROR_UPDATE. ENDIF.

   UPDATE  ZTIV
      SET  ZFCUST  =  '3'
           UNAM    =  SY-UNAME
           UDAT    =  SY-DATUM
    WHERE  ZFIVNO  =  ZTIDRUS-ZFIVNO.
   IF SY-SUBRC NE 0. RAISE  ERROR_UPDATE. ENDIF.

ENDFUNCTION.
