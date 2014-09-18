FUNCTION ZIM_PM_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFPNNO) LIKE  ZTPMTHD-ZFPNNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTPMTHD_OLD) LIKE  ZTPMTHD STRUCTURE  ZTPMTHD
*"     VALUE(W_ZTPMTHD) LIKE  ZTPMTHD STRUCTURE  ZTPMTHD
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSPMTIV_OLD STRUCTURE  ZSPMTIV OPTIONAL
*"      IT_ZSPMTIV STRUCTURE  ZSPMTIV
*"  EXCEPTIONS
*"      ERROR_INSERT
*"      ERROR_UPDATE
*"      ERROR_DELETE
*"----------------------------------------------------------------------
DATA : W_ZFPNIT   LIKE   ZTPMTIV-ZFPNIT.

* DELETE 咯何 SET
   IF W_OK_CODE EQ 'DELE'.   ZFSTATUS = 'X'.   ENDIF.

   MOVE-CORRESPONDING : W_ZTPMTHD      TO   ZTPMTHD.

   MOVE : SY-MANDT    TO     ZTPMTHD-MANDT,
          SY-DATUM    TO     ZTPMTHD-UDAT,
          SY-UNAME    TO     ZTPMTHD-UNAM.

   CASE ZFSTATUS.
      WHEN 'C'.               " 积己
         MOVE : SY-DATUM TO ZTPMTHD-CDAT,
                SY-UNAME TO ZTPMTHD-ERNAM.
         INSERT   ZTPMTHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_INSERT.   ENDIF.
* change document -----------------------------------------------------
           CLEAR : W_ZTPMTHD_OLD.
           call function 'ZIM_CHANGE_DOCUMENT_PMTHD'
                exporting
                      upd_chngind    =    'I'
                      N_ZTPMTHD      =    W_ZTPMTHD
                      O_ZTPMTHD      =    W_ZTPMTHD_OLD.
*----------------------------------------------------------------------
         CLEAR : W_ZFPNIT.
* PAYMENT NOTICE ITEM
         LOOP AT IT_ZSPMTIV.
            CLEAR  ZTPMTIV.
            ADD 10 TO W_ZFPNIT.

            MOVE-CORRESPONDING IT_ZSPMTIV TO ZTPMTIV.
            MOVE : ZFPNNO                 TO ZTPMTIV-ZFPNNO,
                   W_ZFPNIT               TO ZTPMTIV-ZFPNIT.

            INSERT   ZTPMTIV.
            IF SY-SUBRC NE 0.    RAISE ERROR_INSERT.   ENDIF.
* change document -----------------------------------------------------
           CLEAR : *ZTPMTIV.
           call function 'ZIM_CHANGE_DOCUMENT_PMTIV'
                exporting
                      upd_chngind    =    'I'
                      N_ZTPMTIV      =    ZTPMTIV
                      O_ZTPMTIV      =    *ZTPMTIV.
*----------------------------------------------------------------------
         ENDLOOP.
      WHEN 'X'.               " 昏力
         DELETE  FROM ZTPMTHD  WHERE  ZFPNNO  EQ  ZFPNNO.
* change document -----------------------------------------------------
           call function 'ZIM_CHANGE_DOCUMENT_PMTHD'
                exporting
                      upd_chngind    =    'D'
                      N_ZTPMTHD      =    W_ZTPMTHD
                      O_ZTPMTHD      =    W_ZTPMTHD_OLD.
*----------------------------------------------------------------------

         DELETE  FROM ZTPMTIV  WHERE  ZFPNNO  EQ  ZFPNNO.
         LOOP  AT  IT_ZSPMTIV.
* change document -----------------------------------------------------
           CLEAR : *ZTPMTIV.
           call function 'ZIM_CHANGE_DOCUMENT_PMTIV'
                exporting
                      upd_chngind    =    'D'
                      N_ZTPMTIV      =    ZTPMTIV
                      O_ZTPMTIV      =    *ZTPMTIV.
*-----------------------------------------------------------------------
         ENDLOOP.
         UPDATE ZTPMTEDI SET ZFPNNO = SPACE
                             ZFDBYN = 'N'
                             ZFDBDT = SPACE
                             ZFDBTM = SPACE
                             ZFDBID = SPACE
                         WHERE ZFPNNO   EQ ZFPNNO.

      WHEN OTHERS.            " 函版
         UPDATE   ZTPMTHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
           call function 'ZIM_CHANGE_DOCUMENT_PMTHD'
                exporting
                      upd_chngind    =    'U'
                      N_ZTPMTHD      =    W_ZTPMTHD
                      O_ZTPMTHD      =    W_ZTPMTHD_OLD.
*----------------------------------------------------------------------

* PAYMENT NOTICE INVOICE
         SELECT * FROM ZTPMTIV WHERE ZFPNNO   EQ  ZFPNNO.

            READ TABLE IT_ZSPMTIV WITH KEY ZFPNNO  = ZTPMTIV-ZFPNNO
                                           ZFPNIT  = ZTPMTIV-ZFPNIT
                                  BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               IF  IT_ZSPMTIV  NE  ZTPMTIV.
                   MOVE-CORRESPONDING IT_ZSPMTIV TO ZTPMTIV.
                   UPDATE ZTPMTIV.
                   IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
                   CLEAR *ZTPMTIV.
                   READ TABLE IT_ZSPMTIV_OLD WITH KEY
                              ZFPNNO  =  IT_ZSPMTIV-ZFPNNO
                              ZFPNIT  =  IT_ZSPMTIV-ZFPNIT.
                   IF SY-SUBRC EQ 0.
                      MOVE-CORRESPONDING IT_ZSPMTIV_OLD TO *ZTPMTIV.
                   ENDIF.
* change document -----------------------------------------------------
                   CLEAR : *ZTPMTIV.
                   call function 'ZIM_CHANGE_DOCUMENT_PMTIV'
                        exporting
                                upd_chngind    =    'U'
                                N_ZTPMTIV      =    ZTPMTIV
                                O_ZTPMTIV      =    *ZTPMTIV.
*----------------------------------------------------------------------
               ENDIF.
            ELSE.
               DELETE ZTPMTIV.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
               CLEAR : *ZTPMTIV.
               call function 'ZIM_CHANGE_DOCUMENT_PMTIV'
                    exporting
                            upd_chngind    =    'D'
                            N_ZTPMTIV      =    ZTPMTIV
                            O_ZTPMTIV      =    *ZTPMTIV.
*----------------------------------------------------------------------
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSPMTIV.
            SELECT SINGLE * FROM  ZTPMTIV
                            WHERE ZFPNNO   EQ  ZFPNNO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSPMTIV TO ZTPMTIV.
               MOVE : SY-MANDT               TO ZTLGGOD-MANDT.
               SELECT MAX( ZFPNIT ) INTO W_ZFPNIT
                      FROM  ZTPMTIV
                      WHERE ZFPNNO   EQ   ZFPNNO.
               ADD 10 TO W_ZFPNIT.
               MOVE : W_ZFPNIT   TO   ZTPMTIV-ZFPNIT.
               INSERT  ZTPMTIV.

               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
               CLEAR : *ZTPMTIV.
               call function 'ZIM_CHANGE_DOCUMENT_PMTIV'
                    exporting
                            upd_chngind    =    'I'
                            N_ZTPMTIV      =    ZTPMTIV
                            O_ZTPMTIV      =    *ZTPMTIV.
*----------------------------------------------------------------------
            ENDIF.
         ENDLOOP.
   ENDCASE.





ENDFUNCTION.
