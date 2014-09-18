FUNCTION ZIM_BW_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFIVNO) LIKE  ZTBWHD-ZFIVNO
*"     VALUE(ZFGISEQ) LIKE  ZTBWHD-ZFGISEQ
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTBWHD_OLD) LIKE  ZTBWHD STRUCTURE  ZTBWHD
*"     VALUE(W_ZTBWHD) LIKE  ZTBWHD STRUCTURE  ZTBWHD
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSBWIT STRUCTURE  ZSBWIT
*"      IT_ZSBWIT_OLD STRUCTURE  ZSBWIT
*"  CHANGING
*"     REFERENCE(P_ZFGISEQ) LIKE  ZTBWHD-ZFGISEQ
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"      NOT_MODIFY
*"----------------------------------------------------------------------
DATA : W_ZFIVNO    LIKE   ZTBWHD-ZFIVNO,
       W_ZFGISEQ   LIKE   ZTBWHD-ZFGISEQ.

   MOVE-CORRESPONDING : W_ZTBWHD      TO   ZTBWHD.

   MOVE : ZFIVNO      TO     ZTBWHD-ZFIVNO,
          SY-MANDT    TO     ZTBWHD-MANDT,
          SY-UNAME    TO     ZTBWHD-UNAM,
          SY-DATUM    TO     ZTBWHD-UDAT.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
    ENDIF.

   CASE ZFSTATUS.
      WHEN 'C'.
           SELECT MAX( ZFGISEQ ) INTO W_ZFGISEQ
                  FROM ZTBWHD
                  WHERE ZFIVNO = ZTBWHD-ZFIVNO.
           ADD 1 TO W_ZFGISEQ.

           MOVE  W_ZFGISEQ TO  ZTBWHD-ZFGISEQ.
           MOVE  SY-UNAME  TO  ZTBWHD-ERNAM.
           MOVE  SY-DATUM  TO  ZTBWHD-CDAT.
           INSERT  ZTBWHD.
           IF SY-SUBRC  NE 0.
              RAISE  ERROR_UPDATE.
           ENDIF.
           P_ZFGISEQ = W_ZFGISEQ.
* change document -----------------------------------------------------
           CLEAR : W_ZTBWHD_OLD.
           call function 'ZIM_CHANGE_DOCUMENT_ZTBWHD'
              exporting
                      upd_chngind    =    'I'
                      N_ZTBWHD      =    W_ZTBWHD
                      O_ZTBWHD      =    W_ZTBWHD_OLD.

*----------------------------------------------------------------------
           LOOP AT IT_ZSBWIT.
               W_TABIX = SY-TABIX.
               CLEAR ZTBWIT.
               MOVE-CORRESPONDING : IT_ZSBWIT TO ZTBWIT.
               MOVE : ZFIVNO         TO     ZTBWIT-ZFIVNO,
                      ZTBWHD-ZFGISEQ TO     ZTBWIT-ZFGISEQ,
                      SY-MANDT       TO     ZTBWIT-MANDT.


               INSERT ZTBWIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBWIT'
                    EXPORTING
                        UPD_CHNGIND    =    'I'
                        N_ZTBWIT       =   ZTBWIT
                        O_ZTBWIT       =   *ZTBWIT.
*----------------------------------------------------------------------

           ENDLOOP.
      WHEN 'U'.
*          IF W_ZTBWHD_OLD EQ ZTBWHD.  RAISE NOT_MODIFY. ENDIF.
          UPDATE   ZTBWHD.
          IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
          P_ZFGISEQ = ZTBWHD-ZFGISEQ.

* change document -----------------------------------------------------
          call function 'ZIM_CHANGE_DOCUMENT_ZTBWHD'
                exporting
                        upd_chngind    =    'U'
                        N_ZTBWHD      =    W_ZTBWHD
                        O_ZTBWHD      =    W_ZTBWHD_OLD.
*----------------------------------------------------------------------
*>> ITEM 변경.
* B/L 자재 내역.
          SELECT * FROM ZTBWIT  WHERE ZFIVNO   EQ  ZFIVNO
                                  AND ZFGISEQ  EQ  ZFGISEQ.

            MOVE-CORRESPONDING   ZTBWIT    TO      *ZTBWIT.

            READ TABLE IT_ZSBWIT  WITH KEY ZFIVNO  = ZTBWIT-ZFIVNO
                                           ZFIVDNO = ZTBWIT-ZFIVDNO
                                           ZFGISEQ = ZTBWIT-ZFGISEQ
                                  BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               IF IT_ZSBWIT-GIMENGE  IS INITIAL.
                  DELETE ZTBWIT.
               ELSE.
                  MOVE-CORRESPONDING IT_ZSBWIT  TO ZTBWIT.
                  UPDATE ZTBWIT.
               ENDIF.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
*               CLEAR  *ZTBWIT.
*               READ TABLE IT_ZSBWIT_OLD WITH KEY
*                          ZFIVNO    =  IT_ZSBWIT-ZFIVNO
*                          ZFIVDNO =    IT_ZSBWIT-ZFIVDNO
*                          ZFGISEQ   =  IT_ZSBWIT-ZFGISEQ.
*               IF SY-SUBRC EQ 0.
*                  MOVE-CORRESPONDING IT_ZSBWIT_OLD TO *ZTBWIT.
*               ENDIF.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_ZTBWIT'
                    exporting
                           upd_chngind    =    'U'
                           N_ZTBWIT       =    ZTBWIT
                           O_ZTBWIT       =    *ZTBWIT.
*----------------------------------------------------------------------
            ELSE.
               DELETE ZTBWIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_ZTBWIT'
                    exporting
                           upd_chngind    =    'D'
                           N_ZTBWIT       =    ZTBWIT
                           O_ZTBWIT       =    *ZTBWIT.
*----------------------------------------------------------------------
            ENDIF.
          ENDSELECT.

*         LOOP AT IT_ZSBWIT WHERE BLMENGE NE 0.
          LOOP AT IT_ZSBWIT.
            SELECT SINGLE * FROM  ZTBWIT
                            WHERE ZFIVNO    EQ  IT_ZSBWIT-ZFIVNO
                              AND ZFIVDNO   EQ  IT_ZSBWIT-ZFIVDNO
                              AND ZFGISEQ   EQ  IT_ZSBWIT-ZFGISEQ.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSBWIT  TO ZTBWIT.

               MOVE : ZFIVNO                 TO ZTBWIT-ZFIVNO,
                      ZFGISEQ                TO ZTBWIT-ZFGISEQ,
                      SY-MANDT               TO ZTBWIT-MANDT.

               INSERT  ZTBWIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               CLEAR *ZTBWIT.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_ZTBWIT'
                    exporting
                           upd_chngind    =    'I'
                           N_ZTBWIT       =    ZTBWIT
                           O_ZTBWIT       =    *ZTBWIT.
*----------------------------------------------------------------------
            ENDIF.
          ENDLOOP.

      WHEN 'X'.
           DELETE FROM ZTBWHD
            WHERE ZFIVNO  = ZFIVNO
              AND ZFGISEQ = ZFGISEQ.

           IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
           IF SY-SUBRC NE 0.
              MOVE-CORRESPONDING W_ZTBWHD_OLD  TO ZTBWHD.
              INSERT ZTBWHD.
           ENDIF.
* change document -----------------------------------------------------
           call function 'ZIM_CHANGE_DOCUMENT_ZTBWHD'
                exporting
                        upd_chngind    =    'D'
                        N_ZTBWHD      =    W_ZTBWHD
                        O_ZTBWHD      =    W_ZTBWHD_OLD.
*----------------------------------------------------------------------
          DELETE  FROM ZTBWIT
            WHERE ZFIVNO  = ZFIVNO
              AND ZFGISEQ = ZFGISEQ.
          LOOP  AT  IT_ZSBWIT_OLD.
            MOVE-CORRESPONDING IT_ZSBWIT_OLD  TO ZTBWIT.

            CLEAR *ZTBWIT.
* change document -----------------------------------------------------
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBWIT'
                    EXPORTING
                           UPD_CHNGIND    =    'D'      " DELETE.
                           N_ZTBWIT       =    ZTBWIT
                           O_ZTBWIT       =   *ZTBWIT.

*----------------------------------------------------------------------
         ENDLOOP.

   ENDCASE.
ENDFUNCTION.
