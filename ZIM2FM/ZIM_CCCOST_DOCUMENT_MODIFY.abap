FUNCTION ZIM_CCCOST_DOCUMENT_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFCCNO) LIKE  ZTCCHD-ZFCCNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTCCHD_OLD) LIKE  ZTCCHD STRUCTURE  ZTCCHD
*"     VALUE(W_ZTCCHD) LIKE  ZTCCHD STRUCTURE  ZTCCHD
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSCCIT_OLD STRUCTURE  ZSCCIT OPTIONAL
*"      IT_ZSCCIT STRUCTURE  ZSCCIT
*"      IT_ZSBDIV STRUCTURE  ZSBDIV OPTIONAL
*"      IT_ZSBHIS STRUCTURE  ZSBHIS OPTIONAL
*"  CHANGING
*"     REFERENCE(W_ZFCCNO) LIKE  ZTCCHD-ZFCCNO
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
DATA : W_ZFBSEQ      LIKE   ZTBDIV-ZFBSEQ,
       W_BUZEI       LIKE   ZTBDIV-BUZEI,
       W_ZFCCIT      LIKE   ZTCCIT-ZFCCIT,
       W_ZUONR       LIKE   ZTBDIV-ZUONR.

*-----------------------------------------------------------------------
*> 삭제 구분 Bit Set.
*-----------------------------------------------------------------------
   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ENDIF.

   MOVE-CORRESPONDING : W_ZTCCHD      TO   ZTCCHD.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*> 비용 Header Data Move.
*-----------------------------------------------------------------------
* 생성일 경우 다음 번호를 채번..
   IF ZFSTATUS  EQ 'C'.
*      ZTCCHD-GJAHR  =  ZTCCHD-BLDAT(4).
      PERFORM   P2000_GET_NUMBER_NEXT  USING  'CV'
                                              W_ZFCCNO '0000' ''.
   ENDIF.

   MOVE : SY-MANDT     TO     ZTCCHD-MANDT,
          W_ZFCCNO     TO     ZTCCHD-ZFCCNO,
          SY-UNAME     TO     ZTCCHD-UNAM,
          SY-UZEIT     TO     ZTCCHD-UTME,
          SY-DATUM     TO     ZTCCHD-UDAT.

   LOOP AT IT_ZSCCIT.
      W_TABIX = SY-TABIX.
      MOVE : W_TABIX   TO IT_ZSCCIT-ZFCCIT,
             W_ZFCCNO  TO IT_ZSCCIT-ZFCCNO,
             SY-MANDT  TO IT_ZSCCIT-MANDT.
      MODIFY IT_ZSCCIT INDEX W_TABIX.
   ENDLOOP.
*-----------------------------------------------------------------------


   CASE ZFSTATUS.
      WHEN 'C'.               " 생?
         MOVE : SY-UNAME      TO    ZTCCHD-USNAM,
                SY-DATUM      TO    ZTCCHD-CPUDT,
                SY-UZEIT      TO    ZTCCHD-CPUTM.

         INSERT   ZTCCHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

* change document -----------------------------------------------------
         CLEAR : W_ZTCCHD_OLD.
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTCCHD'
              EXPORTING
                      UPD_CHNGIND    =    'I'
                      N_ZTCCHD       =    ZTCCHD
                      O_ZTCCHD       =    W_ZTCCHD_OLD.
*----------------------------------------------------------------------
*>> 비용품목..
         W_ZFCCIT = 0.
         LOOP AT IT_ZSCCIT.
            W_TABIX = SY-TABIX.
            ADD   1    TO    W_ZFCCIT.
            CLEAR : ZTCCIT.
            MOVE-CORRESPONDING IT_ZSCCIT TO ZTCCIT.
            MOVE: SY-MANDT     TO     ZTCCIT-MANDT,
                  W_ZFCCNO     TO     ZTCCIT-ZFCCNO,
                  W_ZFCCIT     TO     ZTCCIT-ZFCCIT,
                  SY-UNAME     TO     ZTCCIT-UNAM,
                  SY-UZEIT     TO     ZTCCIT-UTME,
                  SY-DATUM     TO     ZTCCIT-UDAT,
                  SY-UNAME     TO     ZTCCIT-USNAM,
                  SY-DATUM     TO     ZTCCIT-CPUDT,
                  SY-UZEIT     TO     ZTCCIT-CPUTM.

            INSERT   ZTCCIT.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            CLEAR *ZTCCIT.
* change document -----------------------------------------------------
            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTCCIT'
                 EXPORTING
                        UPD_CHNGIND    =    'I'
                        N_ZTCCIT       =    ZTCCIT
                        O_ZTCCIT       =   *ZTCCIT.
*----------------------------------------------------------------------
            MOVE-CORRESPONDING ZTCCIT TO IT_ZSCCIT.
            MODIFY IT_ZSCCIT  INDEX   W_TABIX.
         ENDLOOP.

      WHEN 'X'.               " 삭?
         DELETE  FROM ZTCCHD
                 WHERE ZFCCNO = W_ZFCCNO.

         IF SY-SUBRC NE 0.     RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTCCHD'
              EXPORTING
                      UPD_CHNGIND    =    'D'
                      N_ZTCCHD       =    ZTCCHD
                      O_ZTCCHD       =    W_ZTCCHD_OLD.
*----------------------------------------------------------------------

         DELETE  FROM ZTCCIT
                 WHERE ZFCCNO = W_ZFCCNO.
         IF SY-SUBRC NE 0.     RAISE ERROR_UPDATE.   ENDIF.

*         DELETE  FROM ZTBDIV
*                 WHERE BUKRS  EQ  BUKRS
*                 AND   BELNR  EQ  BELNR
*                 AND   GJAHR  EQ  GJAHR.
*
*         DELETE  FROM ZTBHIS
*                 WHERE BUKRS  EQ  BUKRS
*                 AND   BELNR  EQ  BELNR
*                 AND   GJAHR  EQ  GJAHR.

         LOOP  AT  IT_ZSCCIT_OLD.
            MOVE-CORRESPONDING IT_ZSCCIT_OLD  TO ZTCCIT.
            CLEAR *ZTCCIT.
* change document -----------------------------------------------------
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTCCIT'
                    EXPORTING
                           UPD_CHNGIND    =    'D'
                           N_ZTCCIT       =    ZTCCIT
                           O_ZTCCIT       =   *ZTCCIT.
*----------------------------------------------------------------------
         ENDLOOP.

      WHEN OTHERS.            " 변경.
         UPDATE   ZTCCHD.
         MOVE ZTCCHD-ZFCCNO TO W_ZFCCNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTCCHD'
              EXPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTCCHD       =    ZTCCHD
                      O_ZTCCHD       =    W_ZTCCHD_OLD.
*----------------------------------------------------------------------
* 비용 품목.
         SELECT * FROM ZTCCIT
                  WHERE ZFCCNO = IT_ZSCCIT-ZFCCNO.
            CLEAR : IT_ZSCCIT.
            READ TABLE IT_ZSCCIT
                       WITH KEY ZFCCNO = ZTCCIT-ZFCCNO
                                ZFCCIT = ZTCCIT-ZFCCIT
                       BINARY SEARCH.
            W_SUBRC  = SY-SUBRC.
            W_TABIX  = SY-TABIX.
            IF W_SUBRC EQ 0.
               MOVE-CORRESPONDING ZTCCIT     TO *ZTCCIT.
               MOVE-CORRESPONDING IT_ZSCCIT  TO ZTCCIT.
               MOVE : SY-MANDT               TO ZTCCIT-MANDT,
                      SY-UNAME               TO ZTCCIT-UNAM,
                      SY-UZEIT               TO ZTCCIT-UTME,
                      SY-DATUM               TO ZTCCIT-UDAT.
               UPDATE ZTCCIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTCCIT'
                    EXPORTING
                            UPD_CHNGIND    =    'U'
                            N_ZTCCIT       =    ZTCCIT
                            O_ZTCCIT       =   *ZTCCIT.
*----------------------------------------------------------------------
               MOVE-CORRESPONDING ZTCCIT     TO IT_ZSCCIT.
               MODIFY IT_ZSCCIT   INDEX   W_TABIX.
            ELSE.
               DELETE ZTCCIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               MOVE-CORRESPONDING ZTCCIT     TO *ZTCCIT.

* change document -----------------------------------------------------
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTCCIT'
                    EXPORTING
                            UPD_CHNGIND    =    'D'
                            N_ZTCCIT       =    ZTCCIT
                            O_ZTCCIT       =   *ZTCCIT.
*----------------------------------------------------------------------
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSCCIT.
            W_TABIX = SY-TABIX.
            SELECT SINGLE * FROM  ZTCCIT
                   WHERE ZFCCNO  EQ  IT_ZSCCIT-ZFCCNO
                   AND   ZFCCIT  EQ  IT_ZSCCIT-ZFCCIT.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSCCIT  TO ZTCCIT.

               SELECT MAX( ZFCCIT ) INTO ZTCCIT-ZFCCIT
                      FROM ZTCCIT
                      WHERE ZFCCNO  EQ  IT_ZSCCIT-ZFCCNO.


               ADD    1    TO   ZTCCIT-ZFCCIT.

               MOVE: SY-MANDT      TO     ZTCCIT-MANDT,
                     ZTCCHD-ZFCCNO TO     ZTCCIT-ZFCCNO,
                     SY-UNAME      TO     ZTCCIT-UNAM,
                     SY-UZEIT      TO     ZTCCIT-UTME,
                     SY-DATUM      TO     ZTCCIT-UDAT,
                     SY-UNAME      TO     ZTCCIT-USNAM,
                     SY-DATUM      TO     ZTCCIT-CPUDT,
                     SY-UZEIT      TO     ZTCCIT-CPUTM.

               INSERT  ZTCCIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               CLEAR *ZTCCIT.
* change document -----------------------------------------------------
               CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTCCIT'
                    EXPORTING
                            UPD_CHNGIND    =    'I'
                            N_ZTCCIT       =    ZTCCIT
                            O_ZTCCIT       =   *ZTCCIT.
*----------------------------------------------------------------------
               MOVE-CORRESPONDING ZTCCIT     TO IT_ZSCCIT.
               MODIFY IT_ZSCCIT   INDEX   W_TABIX.
            ENDIF.
         ENDLOOP.

   ENDCASE.

*------------------------------------------------------------------
*>> 전기내역 배부.
*------------------------------------------------------------------
*   IF ZFSTATUS NE 'X'.
*
*      DELETE  FROM ZTBDIV
*                 WHERE BUKRS  EQ  BUKRS
*                 AND   BELNR  EQ  BELNR
*                 AND   GJAHR  EQ  GJAHR.
*
*      CALL FUNCTION 'ZIM_CHARGE_ITEM_CREATE'
*           EXPORTING
*              BUKRS    =    BUKRS
*              GJAHR    =    GJAHR
*              BELNR    =    BELNR
*              ZTCCHD   =    ZTCCHD
*           TABLES
*              IT_ZSCCIT =   IT_ZSCCIT
*              IT_ZTBDIV =   IT_ZTBDIV.
*
*      INSERT ZTBDIV  FROM TABLE IT_ZTBDIV.
*      IF SY-SUBRC NE 0.
*         RAISE ERROR_UPDATE.
*      ENDIF.
*   ENDIF.

ENDFUNCTION.
