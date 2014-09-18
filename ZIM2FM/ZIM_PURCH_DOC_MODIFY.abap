FUNCTION ZIM_PURCH_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQNO) LIKE  ZTREQHD-ZFREQNO
*"     VALUE(ZFAMDNO) LIKE  ZTREQST-ZFAMDNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTREQHD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     VALUE(W_ZTREQHD_OLD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     VALUE(W_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST
*"     VALUE(W_ZTREQST_OLD) LIKE  ZTREQST STRUCTURE  ZTREQST
*"     VALUE(W_ZTPUR) LIKE  ZTPUR STRUCTURE  ZTPUR
*"     VALUE(W_ZTPUR_OLD) LIKE  ZTPUR STRUCTURE  ZTPUR
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSPURSG1 STRUCTURE  ZSPURSG1
*"      IT_ZSPURSG1G STRUCTURE  ZSPURSG1G
*"      IT_ZSPURSG4 STRUCTURE  ZSPURSG4
*"      IT_ZSREQIT STRUCTURE  ZSREQIT
*"      IT_ZSREQIT_OLD STRUCTURE  ZSREQIT
*"      IT_ZTREQORJ_OLD STRUCTURE  ZSMLCSG7O
*"      IT_ZTREQORJ STRUCTURE  ZSMLCSG7O
*"      IT_ZSREQIL_OLD STRUCTURE  ZSREQIL
*"      IT_ZSREQIL STRUCTURE  ZSREQIL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
   DATA : W_ZFLSG1G    LIKE ZTPURSG1G-ZFLSG1G,
          W_ZFLSG4     LIKE ZTPURSG4-ZFLSG4.
* 삭제 MARK
   IF W_OK_CODE EQ 'DELE'.    ZFSTATUS = 'X'.     ENDIF.

   MOVE-CORRESPONDING : W_ZTPUR     TO   ZTPUR.

   MOVE : ZFREQNO  TO    ZTPUR-ZFREQNO,       " 구매승인서
          ZFAMDNO  TO    ZTPUR-ZFAMDNO,       "      "
          SY-MANDT TO    ZTPUR-MANDT.

*-----------------------------------------------------------------------
* 수입의뢰 HEADER 정의
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_REQUEST_DOC_MODIFY'
       EXPORTING
             ZFREQNO             =   ZFREQNO
             ZFAMDNO             =   ZFAMDNO
             ZFSTATUS            =   ZFSTATUS
             W_ZTREQHD           =   W_ZTREQHD
             W_ZTREQHD_old       =   W_ZTREQHD_old
             W_ZTREQST           =   W_ZTREQST
             W_OK_CODE           =   W_OK_CODE
       TABLES
             IT_ZSREQIT          =   IT_ZSREQIT
             IT_ZSREQIT_OLD      =   IT_ZSREQIT_OLD
             IT_ZSREQIL          =   IT_ZSREQIL
             IT_ZSREQIL_OLD      =   IT_ZSREQIL_OLD
             IT_ZTREQORJ         =   IT_ZTREQORJ
             IT_ZTREQORJ_old     =   IT_ZTREQORJ_old
       EXCEPTIONS
              ERROR_UPDATE.

   IF SY-SUBRC NE  0.   RAISE ERROR_UPDATE.   ENDIF.
* 구매승인서
   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         INSERT     ZTPUR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* 물품명세
         LOOP AT IT_ZSPURSG1.
            CLEAR : ZTPURSG1.
            MOVE-CORRESPONDING IT_ZSPURSG1  TO ZTPURSG1.
            MOVE : ZFREQNO                TO ZTPURSG1-ZFREQNO,
                   ZFAMDNO                TO ZTPURSG1-ZFAMDNO,
                   IT_ZSPURSG1-ZFLSG1     TO ZTPURSG1-ZFLSG1,
                   SY-MANDT               TO ZTPURSG1-MANDT.
            INSERT   ZTPURSG1.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.

* Seg 4
         LOOP AT IT_ZSPURSG4.
            CLEAR : ZTPURSG4.
            MOVE-CORRESPONDING IT_ZSPURSG4  TO ZTPURSG4.
            MOVE : ZFREQNO                TO ZTPURSG4-ZFREQNO,
                   ZFAMDNO                TO ZTPURSG4-ZFAMDNO,
                   IT_ZSPURSG4-ZFLSG4     TO ZTPURSG4-ZFLSG4,
                   SY-MANDT               TO ZTPURSG4-MANDT.
            INSERT   ZTPURSG4.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.

* 물품명세 규격
         LOOP AT IT_ZSPURSG1G.
            CLEAR : ZTPURSG1G.
            MOVE-CORRESPONDING IT_ZSPURSG1G TO ZTPURSG1G.
            MOVE : ZFREQNO                TO ZTPURSG1G-ZFREQNO,
                   ZFAMDNO                TO ZTPURSG1G-ZFAMDNO,
                   IT_ZSPURSG1G-ZFLSG1    TO ZTPURSG1G-ZFLSG1,
                   IT_ZSPURSG1G-ZFLSG1G   TO ZTPURSG1G-ZFLSG1G,
                   SY-MANDT               TO ZTPURSG1G-MANDT.
            INSERT   ZTPURSG1G.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_PU'
              exPORTING
                         UPD_CHNGIND    =    'I'
                         N_ZTPUR        =    w_ztPUR
                         o_ZTPUR        =    w_ztPUR_old
                         N_ZTREQHD      =    w_ZTREQHD
                         o_ZTREQHD      =    w_ZTREQHD_old
                         N_ZTREQst      =    w_ZTREQst
                         o_ZTREQst      =    w_ZTREQst_old.

*-----------------------------------------------------------------------
* DELETE
*-----------------------------------------------------------------------
      WHEN 'X'.               " 삭제
         DELETE FROM ZTPUR WHERE ZFREQNO EQ ZFREQNO
                           AND   ZFAMDNO EQ ZFAMDNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* 물품명세
         DELETE FROM ZTPURSG1  WHERE ZFREQNO EQ ZFREQNO
                               AND   ZFAMDNO EQ ZFAMDNO.
* seg 4
         DELETE FROM ZTPURSG4  WHERE ZFREQNO EQ ZFREQNO
                               AND   ZFAMDNO EQ ZFAMDNO.

* 물품명세 규격
         DELETE FROM ZTPURSG1G WHERE ZFREQNO EQ ZFREQNO
                               AND   ZFAMDNO EQ ZFAMDNO.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_PU'
              exPORTING
                         UPD_CHNGIND    =    'D'
                         N_ZTPUR        =    w_ztPUR
                         o_ZTPUR        =    w_ztPUR_old
                         N_ZTREQHD      =    w_ZTREQHD
                         o_ZTREQHD      =    w_ZTREQHD_old
                         N_ZTREQst      =    w_ZTREQst
                         o_ZTREQst      =    w_ZTREQst_old.

*-----------------------------------------------------------------------
* Update
*-----------------------------------------------------------------------
      WHEN OTHERS.            " 변경
         UPDATE     ZTPUR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

*  품목명세
         SELECT * FROM ZTPURSG1  WHERE ZFREQNO  EQ  ZFREQNO
                                 AND   ZFAMDNO  EQ  ZFAMDNO.

            READ TABLE IT_ZSPURSG1
                 WITH KEY ZFLSG1  = ZTPURSG1-ZFLSG1 BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSPURSG1  TO ZTPURSG1.
               UPDATE ZTPURSG1.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTPURSG1.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSPURSG1.
            SELECT SINGLE * FROM  ZTPURSG1
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFAMDNO  EQ  ZFAMDNO
                            AND   ZFLSG1   EQ  IT_ZSPURSG1-ZFLSG1.

            IF SY-SUBRC NE 0.

               MOVE-CORRESPONDING IT_ZSPURSG1  TO ZTPURSG1.
               MOVE : ZFREQNO                  TO ZTPURSG1-ZFREQNO,
                      ZFAMDNO                  TO ZTPURSG1-ZFAMDNO,
                      SY-MANDT                 TO ZTPURSG1-MANDT.
               INSERT  ZTPURSG1.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.

*  품목명세  규격
      SELECT * FROM ZTPURSG1G  WHERE ZFREQNO  EQ  ZFREQNO
                               AND   ZFAMDNO  EQ  ZFAMDNO.

            READ TABLE IT_ZSPURSG1G
                 WITH KEY ZFLSG1  = ZTPURSG1G-ZFLSG1
                          ZFLSG1G = ZTPURSG1G-ZFLSG1G BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSPURSG1G TO ZTPURSG1G.
               UPDATE ZTPURSG1G.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTPURSG1G.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSPURSG1G.
            SELECT SINGLE * FROM  ZTPURSG1G
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFAMDNO  EQ  ZFAMDNO
                            AND   ZFLSG1   EQ  IT_ZSPURSG1G-ZFLSG1
                            AND   ZFLSG1G  EQ  IT_ZSPURSG1G-ZFLSG1G.

            IF SY-SUBRC NE 0.

               MOVE-CORRESPONDING IT_ZSPURSG1G TO ZTPURSG1G.
               MOVE : ZFREQNO                  TO ZTPURSG1G-ZFREQNO,
                      ZFAMDNO                  TO ZTPURSG1G-ZFAMDNO,
                      IT_ZSPURSG1G-ZFLSG1      TO ZTPURSG1G-ZFLSG1,
                      IT_ZSPURSG1G-ZFLSG1G     TO ZTPURSG1G-ZFLSG1G,
                      SY-MANDT                 TO ZTPURSG1G-MANDT.
               INSERT  ZTPURSG1G.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.
*  SEG 4
         SELECT * FROM ZTPURSG4  WHERE ZFREQNO  EQ  ZFREQNO
                                 AND   ZFAMDNO  EQ  ZFAMDNO.

            READ TABLE IT_ZSPURSG4
                 WITH KEY ZFLSG4  = ZTPURSG4-ZFLSG4 BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSPURSG4  TO ZTPURSG4.
               UPDATE ZTPURSG4.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTPURSG4.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSPURSG4.
            SELECT SINGLE * FROM  ZTPURSG4
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFAMDNO  EQ  ZFAMDNO
                            AND   ZFLSG4   EQ  IT_ZSPURSG4-ZFLSG4.

            IF SY-SUBRC NE 0.

               MOVE-CORRESPONDING IT_ZSPURSG4  TO ZTPURSG4.
               MOVE : ZFREQNO                  TO ZTPURSG4-ZFREQNO,
                      ZFAMDNO                  TO ZTPURSG4-ZFAMDNO,
                      IT_ZSPURSG4-ZFLSG4       TO ZTPURSG4-ZFLSG4,
                      SY-MANDT                 TO ZTPURSG4-MANDT.
               INSERT  ZTPURSG4.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_PU'
              exPORTING
                         UPD_CHNGIND    =    'U'
                         N_ZTPUR        =    w_ztPUR
                         o_ZTPUR        =    w_ztPUR_old
                         N_ZTREQHD      =    w_ZTREQHD
                         o_ZTREQHD      =    w_ZTREQHD_old
                         N_ZTREQst      =    w_ZTREQst
                         o_ZTREQst      =    w_ZTREQst_old.

   ENDCASE.

ENDFUNCTION.
