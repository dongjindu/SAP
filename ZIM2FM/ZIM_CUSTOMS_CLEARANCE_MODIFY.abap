FUNCTION ZIM_CUSTOMS_CLEARANCE_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFIVNO) LIKE  ZTIV-ZFIVNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTIV_OLD) LIKE  ZTIV STRUCTURE  ZTIV OPTIONAL
*"     VALUE(W_ZTIV) LIKE  ZTIV STRUCTURE  ZTIV
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSIVIT_OLD STRUCTURE  ZSIVIT OPTIONAL
*"      IT_ZSIVIT STRUCTURE  ZSIVIT OPTIONAL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
DATA : W_ZFIVDNO   LIKE   ZTIVIT-ZFIVDNO,
       W_ZTBL_OLD  LIKE   ZTBL,
       W_ZFBLST    LIKE   ZTBL-ZFBLST.

   MOVE-CORRESPONDING : W_ZTIV      TO   ZTIV.

   SELECT SINGLE * FROM ZTBL WHERE ZFBLNO  EQ  ZTIV-ZFBLNO.

   MOVE : ZFIVNO       TO     ZTIV-ZFIVNO,
          SY-MANDT     TO     ZTIV-MANDT,
          SY-UNAME     TO     ZTIV-UNAM,
          SY-DATUM     TO     ZTIV-UDAT.

   IF W_OK_CODE EQ 'DELE'.
      ZFSTATUS = 'X'.
   ENDIF.

   W_ZFIVDNO = 0.
   LOOP AT IT_ZSIVIT.
      W_TABIX = SY-TABIX.
      IF IT_ZSIVIT-CCMENGE LE 0 AND IT_ZSIVIT-GRMENGE LE 0.
         DELETE IT_ZSIVIT INDEX W_TABIX.
         CONTINUE.
      ELSE.
         IF ZTIV-ZFREQTY EQ 'PU' OR ZTIV-ZFREQTY EQ 'LO'.
            IT_ZSIVIT-CCMENGE = IT_ZSIVIT-GRMENGE.
         ENDIF.
         W_ZFIVDNO = W_ZFIVDNO + 10.
         IT_ZSIVIT-ZFIVDNO = W_ZFIVDNO.
         MODIFY IT_ZSIVIT INDEX W_TABIX.
      ENDIF.
   ENDLOOP.

   CASE ZFSTATUS.
      WHEN 'C'.               " Create
         MOVE : SY-UNAME      TO    ZTIV-ERNAM,
                SY-DATUM      TO    ZTIV-CDAT.

         INSERT   ZTIV.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

* change document -----------------------------------------------------
         CLEAR : W_ZTIV_OLD.
         call function 'ZIM_CHANGE_DOCUMENT_CCHD'
              exporting
                      upd_chngind    =    'I'
                      N_ZTIV         =    W_ZTIV
                      O_ZTIV         =    W_ZTIV_OLD.
*----------------------------------------------------------------------

         LOOP AT IT_ZSIVIT.
            CLEAR : ZTIVIT.
            MOVE-CORRESPONDING IT_ZSIVIT  TO ZTIVIT.
            MOVE : ZFIVNO                 TO ZTIVIT-ZFIVNO,
                   SY-MANDT               TO ZTIVIT-MANDT,
                   SY-UNAME               TO ZTIVIT-ERNAM,
                   SY-DATUM               TO ZTIVIT-CDAT,
                   SY-UNAME               TO ZTIVIT-UNAM,
                   SY-DATUM               TO ZTIVIT-UDAT.

            INSERT   ZTIVIT.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         CLEAR : *ZTIVIT.
         call function 'ZIM_CHANGE_DOCUMENT_CCIT'
              exporting
                      upd_chngind    =    'I'
                      N_ZTIVIT       =    ZTIVIT
                      O_ZTIVIT       =    *ZTIVIT.
*----------------------------------------------------------------------
         ENDLOOP.

*>>> 수입신고 데이타 자동생성 체크...
         SELECT SINGLE * FROM ZTIMIMG00.

         IF NOT ZTIMIMG00-ZFIMPATH  IS INITIAL  AND
                ZTIMIMG00-ZFIMPATH  NE '1' .
            IF ZTIV-ZFCUST EQ '1' AND ZTBL-ZFRPTTY NE SPACE.
               CALL FUNCTION 'ZIM_CUDATA_CREATE_US'
                    EXPORTING
                        W_ZFIVNO            =   ZFIVNO
                    EXCEPTIONS
                        ERROR_INSERT        =   4.
               IF SY-SUBRC NE 0.
                  DELETE  FROM ZTIV     WHERE ZFIVNO  EQ ZFIVNO.
                  DELETE  FROM ZTIVIT   WHERE ZFIVNO  EQ ZFIVNO.
                  DELETE  FROM ZTIVCD   WHERE ZFIVNO  EQ ZFIVNO.
                  DELETE  FROM ZTIVHST  WHERE ZFIVNO  EQ ZFIVNO.
                  DELETE  FROM ZTIVHST1 WHERE ZFIVNO  EQ ZFIVNO.
                  RAISE ERROR_UPDATE.
               ENDIF.
            ENDIF.
         ENDIF.

         SET PARAMETER ID 'ZPIVNO' FIELD ZFIVNO.

      WHEN 'X'.               " 삭제
         DELETE  FROM ZTIV     WHERE ZFIVNO  EQ ZFIVNO.
         IF SY-SUBRC NE 0.     RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         call function 'ZIM_CHANGE_DOCUMENT_CCHD'
              exporting
                      upd_chngind    =    'D'
                      N_ZTIV         =    W_ZTIV
                      O_ZTIV         =    W_ZTIV_OLD.
*----------------------------------------------------------------------

         SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVIT
                       FROM ZTIVIT
                       WHERE ZFIVNO EQ ZFIVNO.

         DELETE  FROM ZTIVIT  WHERE ZFIVNO EQ ZFIVNO.
         LOOP  AT  IT_ZSIVIT.
* change document -----------------------------------------------------
             MOVE-CORRESPONDING IT_ZSIVIT TO : ZTIVIT,
                                              *ZTIVIT.
             call function 'ZIM_CHANGE_DOCUMENT_CCIT'
                 exporting
                         upd_chngind    =    'D'
                         N_ZTIVIT       =    ZTIVIT
                         O_ZTIVIT       =    *ZTIVIT.
*----------------------------------------------------------------------
         ENDLOOP.
         DELETE  FROM ZTIVCD    WHERE ZFIVNO EQ ZFIVNO.
         DELETE  FROM ZTIVHST   WHERE ZFIVNO EQ ZFIVNO.
         DELETE  FROM ZTIVHST1  WHERE ZFIVNO EQ ZFIVNO.
         DELETE  FROM ZTIVHSTIT WHERE ZFIVNO EQ ZFIVNO.
      WHEN OTHERS.            " 변경
         UPDATE   ZTIV.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
         call function 'ZIM_CHANGE_DOCUMENT_CCHD'
              exporting
                      upd_chngind    =    'U'
                      N_ZTIV         =    W_ZTIV
                      O_ZTIV         =    W_ZTIV_OLD.
*----------------------------------------------------------------------
         SELECT * FROM ZTIVIT WHERE ZFIVNO   EQ  ZFIVNO.

            READ TABLE IT_ZSIVIT WITH KEY ZFIVDNO  = ZTIVIT-ZFIVDNO
                                  BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSIVIT TO ZTIVIT.
               MOVE : ZFIVNO                 TO ZTIVIT-ZFIVNO,
                      SY-UNAME               TO ZTIVIT-UNAM,
                      SY-DATUM               TO ZTIVIT-UDAT.
               UPDATE ZTIVIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               CLEAR *ZTIVIT.
               READ TABLE IT_ZSIVIT_OLD WITH KEY
                          ZFIVNO  =  IT_ZSIVIT-ZFIVNO
                          ZFIVDNO =  IT_ZSIVIT-ZFIVDNO.
               IF SY-SUBRC EQ  0.
                  MOVE-CORRESPONDING IT_ZSIVIT_OLD TO *ZTIVIT.
               ENDIF.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CCIT'
                    exporting
                            upd_chngind    =    'U'
                            N_ZTIVIT       =    ZTIVIT
                            O_ZTIVIT       =    *ZTIVIT.
*----------------------------------------------------------------------
            ELSE.
               DELETE ZTIVIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CCIT'
                    exporting
                           upd_chngind    =    'D'
                           N_ZTIVIT       =   ZTIVIT
                           O_ZTIVIT       =   *ZTIVIT.
*----------------------------------------------------------------------
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSIVIT.
            SELECT SINGLE * FROM  ZTIVIT
                            WHERE ZFIVNO  EQ  ZFIVNO
                            AND   ZFIVDNO EQ  IT_ZSIVIT-ZFIVDNO.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSIVIT  TO ZTIVIT.
               MOVE : ZFIVNO                 TO ZTIVIT-ZFIVNO,
                      SY-MANDT               TO ZTIVIT-MANDT,
                      SY-UNAME               TO ZTIVIT-ERNAM,
                      SY-DATUM               TO ZTIVIT-CDAT,
                      SY-UNAME               TO ZTIVIT-UNAM,
                      SY-DATUM               TO ZTIVIT-UDAT.

               INSERT  ZTIVIT.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
               CLEAR  *ZTIVIT.
* change document -----------------------------------------------------
               call function 'ZIM_CHANGE_DOCUMENT_CCIT'
                    exporting
                            upd_chngind    =    'I'
                            N_ZTIVIT       =    ZTIVIT
                            O_ZTIVIT       =    *ZTIVIT.
*----------------------------------------------------------------------
            ENDIF.
         ENDLOOP.

         SELECT SINGLE * FROM ZTIMIMG00.

         IF NOT ZTIMIMG00-ZFIMPATH IS INITIAL  AND
                ZTIMIMG00-ZFIMPATH NE '1' .
            IF ZTIV-ZFCUST EQ '1'.
*--------------------------------------------------------------------
*> 수입신고의뢰 및 수입면허에 통관요청번호 추가작업으로 변경함.
*   desc : 향후 ZTCUCL, ZTCUCLIV, ZTCUCLIVIT 테이블의 무의미해짐.
*--------------------------------------------------------------------
               SELECT SINGLE * FROM ZTIDRUS
                      WHERE    ZFIVNO   EQ  ZTIV-ZFIVNO.

               IF SY-SUBRC NE 0 AND ZTBL-ZFRPTTY NE SPACE.
                  CALL FUNCTION 'ZIM_CUDATA_CREATE_US'
                       EXPORTING
                           W_ZFIVNO            =   ZTIV-ZFIVNO
                       EXCEPTIONS
                           ERROR_INSERT        =   4.

                  IF SY-SUBRC NE 0.
                     RAISE ERROR_UPDATE.
                  ENDIF.
               ENDIF.
            ENDIF.
         ENDIF.
   ENDCASE.

**>> 수입 IMG READ..
*   SELECT SINGLE * FROM ZTIMIMG00.
*
*   IF ZTIMIMG00-BLSTYN EQ 'X' AND NOT ZTIV-ZFBLNO IS INITIAL.
*
*      SELECT SINGLE * FROM ZTBL
*                      WHERE ZFBLNO EQ ZTIV-ZFBLNO.
*      MOVE-CORRESPONDING ZTBL TO W_ZTBL_OLD.
*
*      IF SY-SUBRC EQ 0.
*         SELECT COUNT( * ) INTO W_COUNT FROM ZTIV
*                           WHERE ZFBLNO EQ   ZTIV-ZFBLNO.
*         IF W_COUNT GT 0.
*            SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLIT
*                     FROM ZTBLIT
*                     WHERE ZFBLNO EQ ZTBL-ZFBLNO.
*            W_ZFBLST = '4'.
*            LOOP AT IT_ZSBLIT.
*               SELECT SUM( CCMENGE ) INTO W_MENGE
*                      FROM ZTIVIT
*                      WHERE ZFBLNO EQ IT_ZSBLIT-ZFBLNO
*                      AND   ZFBLIT EQ IT_ZSBLIT-ZFBLIT.
*               IF IT_ZSBLIT-BLMENGE GT W_MENGE OR
*                  IT_ZSBLIT-BLMENGE LT W_MENGE.
*                  W_ZFBLST = 'P'.
*                  EXIT.
*               ENDIF.
*            ENDLOOP.
*            MOVE : W_ZFBLST   TO   ZTBL-ZFBLST.
*         ELSE.
*            MOVE : '3' TO   ZTBL-ZFBLST.
*         ENDIF.
*
*         IF ZTBL-ZFBLST NE W_ZTBL_OLD-ZFBLST.
*            MOVE : SY-UNAME TO ZTBL-ZFCCCNAM,
*                   SY-DATUM TO ZTBL-ZFCCCDT.
*            IF ZTBL-ZFBLST EQ '4'.
*               IF ZTBL-ZFCCRST EQ 'R'.
*                  MOVE : 'A' TO ZTBL-ZFCCRST.
*               ELSEIF ZTBL-ZFCCRST IS INITIAL.
*                  MOVE : 'A' TO ZTBL-ZFCCRST.
*               ENDIF.
*            ENDIF.
*            MOVE : SY-DATUM    TO    ZTBL-UDAT,
*                   SY-UNAME    TO    ZTBL-UNAM.
*            UPDATE  ZTBL.
*
*            CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_BL'
*                 EXPORTING
*                      UPD_CHNGIND    =    'U'
*                      N_ZTBL         =    ZTBL
*                      O_ZTBL         =    W_ZTBL_OLD.
*         ENDIF.
*      ENDIF.
*   ENDIF.

ENDFUNCTION.
