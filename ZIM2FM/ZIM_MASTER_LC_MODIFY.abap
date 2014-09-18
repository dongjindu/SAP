FUNCTION ZIM_MASTER_LC_MODIFY.
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
*"     VALUE(W_ZTMLCHD) LIKE  ZTMLCHD STRUCTURE  ZTMLCHD
*"     VALUE(W_ZTMLCHD_OLD) LIKE  ZTMLCHD STRUCTURE  ZTMLCHD
*"     VALUE(W_ZTMLCSG2) LIKE  ZTMLCSG2 STRUCTURE  ZTMLCSG2
*"     VALUE(W_ZTMLCSG2_OLD) LIKE  ZTMLCSG2 STRUCTURE  ZTMLCSG2
*"     VALUE(W_ZTMLCSG910) LIKE  ZTMLCSG910 STRUCTURE  ZTMLCSG910
*"     VALUE(W_ZTMLCSG910_OLD) LIKE  ZTMLCSG910 STRUCTURE  ZTMLCSG910
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSMLCSG7G STRUCTURE  ZSMLCSG7G
*"      IT_ZSMLCSG7O STRUCTURE  ZSMLCSG7O
*"      IT_ZSMLCSG8E STRUCTURE  ZSMLCSG8E
*"      IT_ZSMLCSG9O STRUCTURE  ZSMLCSG9O
*"      IT_ZSREQIT STRUCTURE  ZSREQIT
*"      IT_ZSREQIT_OLD STRUCTURE  ZSREQIT
*"      IT_ZTREQORJ STRUCTURE  ZSMLCSG7O
*"      IT_ZTREQORJ_OLD STRUCTURE  ZSMLCSG7O
*"      IT_ZSREQIL STRUCTURE  ZSREQIL
*"      IT_ZSREQIL_OLD STRUCTURE  ZSREQIL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
* 삭제 지시자 검증
   IF W_OK_CODE EQ 'DELE'.   ZFSTATUS = 'X'.  ENDIF.

   MOVE-CORRESPONDING : W_ZTMLCHD   TO   ZTMLCHD,
                        W_ZTMLCSG2  TO   ZTMLCSG2,
                        W_ZTMLCSG910 TO  ZTMLCSG910.

   MOVE : ZFREQNO  TO    ZTMLCHD-ZFREQNO,     " Master L/C Header
          ZFREQNO  TO    ZTMLCSG2-ZFREQNO,    " L/C Seg. 2
          ZFREQNO  TO    ZTMLCSG910-ZFREQNO,  " L/C Seg. 9
          SY-MANDT TO    ZTMLCHD-MANDT,
          SY-MANDT TO    ZTMLCSG2-MANDT,
          SY-MANDT TO    ZTMLCSG910-MANDT.

*-----------------------------------------------------------------------
* 수입의뢰 HEADER 정의
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_REQUEST_DOC_MODIFY'
       EXPORTING
             W_OK_CODE           =   W_OK_CODE
             ZFREQNO             =   ZFREQNO
             ZFAMDNO             =   ZFAMDNO
             ZFSTATUS            =   ZFSTATUS
             W_ZTREQHD           =   W_ZTREQHD
             W_ZTREQHD_OLD       =   W_ZTREQHD_OLD
             W_ZTREQST           =   W_ZTREQST
       TABLES
             IT_ZSREQIT          =   IT_ZSREQIT
             IT_ZSREQIT_OLD      =   IT_ZSREQIT_OLD
             IT_ZSREQIL          =   IT_ZSREQIL
             IT_ZSREQIL_OLD      =   IT_ZSREQIL_OLD
             IT_ZTREQORJ         =   IT_ZTREQORJ
             IT_ZTREQORJ_OLD     =   IT_ZTREQORJ_OLD
       EXCEPTIONS
              ERROR_UPDATE.

   IF SY-SUBRC NE  0.   RAISE ERROR_UPDATE.   ENDIF.

* MASTER L/C MODIFY
   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         INSERT     ZTMLCHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         INSERT     ZTMLCSG2.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         INSERT     ZTMLCSG910.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* L/C 상품명세
         LOOP AT IT_ZSMLCSG7G.
            CLEAR : ZTMLCSG7G.
            MOVE-CORRESPONDING IT_ZSMLCSG7G TO ZTMLCSG7G.
            MOVE : ZFREQNO                TO ZTMLCSG7G-ZFREQNO,
                   SY-MANDT               TO ZTMLCSG7G-MANDT.

            INSERT   ZTMLCSG7G.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.
* L/C 국가코드
         LOOP AT IT_ZSMLCSG7O.
            CLEAR : ZTMLCSG7O.
            MOVE-CORRESPONDING IT_ZSMLCSG7O TO ZTMLCSG7O.
            MOVE : ZFREQNO                TO ZTMLCSG7O-ZFREQNO.

            INSERT   ZTMLCSG7O.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.
* L/C 기타부가조건
         LOOP AT IT_ZSMLCSG8E.
            CLEAR : ZTMLCSG8E.
            MOVE-CORRESPONDING IT_ZSMLCSG8E TO ZTMLCSG8E.
            MOVE : ZFREQNO                TO ZTMLCSG8E-ZFREQNO,
                   SY-MANDT               TO ZTMLCSG8E-MANDT.

            INSERT   ZTMLCSG8E.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.
* 기타구비서류
         LOOP AT IT_ZSMLCSG9O.
            CLEAR : ZTMLCSG9O.
            MOVE-CORRESPONDING IT_ZSMLCSG9O TO ZTMLCSG9O.
            MOVE : ZFREQNO                TO ZTMLCSG9O-ZFREQNO,
                   SY-MANDT               TO ZTMLCSG9O-MANDT.

            INSERT   ZTMLCSG9O.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LC'
              EXPORTING
                      UPD_CHNGIND    =    'I'
                      N_ZTMLCHD      =    w_ztmlchd
                      o_ZTMLCHD      =    w_ztmlchd_old
                      N_ZTMLCSG2     =    w_ZTMLCSG2
                      o_ZTMLCSG2     =    w_ZTMLCSG2_old
                      N_ZTMLCSG910   =    w_ZTMLCSG910
                      o_ZTMLCSG910   =    w_ZTMLCSG910_old
                      N_ZTREQHD      =    w_ZTREQHD
                      o_ZTREQHD      =    w_ZTREQHD_old
                      N_ZTREQst      =    w_ZTREQst
                      o_ZTREQst      =    w_ZTREQst_OLD.
*              TABLES
*                      IT_ZTREQORJ     =    IT_ZTREQORJ
*                      IT_ZTREQORJ_OLD =    IT_ZTREQORJ_OLD.

      WHEN 'X'.               " 삭제
         DELETE  FROM ZTMLCHD    WHERE ZFREQNO EQ ZFREQNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         DELETE  FROM ZTMLCSG2   WHERE ZFREQNO EQ ZFREQNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         DELETE  FROM ZTMLCSG910  WHERE ZFREQNO EQ ZFREQNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* L/C 상품명세
         DELETE  FROM ZTMLCSG7G   WHERE ZFREQNO EQ ZFREQNO.
* L/C 국가코드
         DELETE  FROM ZTMLCSG7O   WHERE ZFREQNO EQ ZFREQNO.
* L/C 기타부가조건
         DELETE  FROM ZTMLCSG8E   WHERE ZFREQNO EQ ZFREQNO.
* 기타구비서류
         DELETE  FROM ZTMLCSG9O   WHERE ZFREQNO EQ ZFREQNO.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LC'
              exPORTING
                      UPD_CHNGIND    =    'D'
                      N_ZTMLCHD      =    w_ztmlchd
                      o_ZTMLCHD      =    w_ztmlchd_old
                      N_ZTMLCSG2     =    w_ZTMLCSG2
                      o_ZTMLCSG2     =    w_ZTMLCSG2_old
                      N_ZTMLCSG910   =    w_ZTMLCSG910
                      o_ZTMLCSG910   =    w_ZTMLCSG910_old
                      N_ZTREQHD      =    w_ZTREQHD
                      o_ZTREQHD      =    w_ZTREQHD_old
                      N_ZTREQst      =    w_ZTREQst
                      o_ZTREQst      =    w_ZTREQst_old.
*              TABLES
*                      IT_ZTREQORJ     =    IT_ZTREQORJ
*                      IT_ZTREQORJ_OLD =    IT_ZTREQORJ_OLD.


*-----------------------------------------------------------------------
* UPDATE
*-----------------------------------------------------------------------
      WHEN OTHERS.            " 변경
         UPDATE     ZTMLCHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         UPDATE     ZTMLCSG2.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         UPDATE     ZTMLCSG910.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

*  Master L/C Seg 7 상품명세
         SELECT * FROM ZTMLCSG7G WHERE ZFREQNO  EQ  ZFREQNO.

            READ TABLE IT_ZSMLCSG7G
                 WITH KEY ZFLSG7G = ZTMLCSG7G-ZFLSG7G BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSMLCSG7G TO ZTMLCSG7G.
               UPDATE ZTMLCSG7G.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTMLCSG7G.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSMLCSG7G.
            SELECT SINGLE * FROM  ZTMLCSG7G
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFLSG7G  EQ  IT_ZSMLCSG7G-ZFLSG7G.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSMLCSG7G TO ZTMLCSG7G.
               MOVE : ZFREQNO                  TO ZTMLCSG7G-ZFREQNO,
                      SY-MANDT                 TO ZTMLCSG7G-MANDT.
               INSERT  ZTMLCSG7G.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.

*  Master L/C Seg 7 원산지
         SELECT * FROM ZTMLCSG7O WHERE ZFREQNO  EQ  ZFREQNO.

            READ TABLE IT_ZSMLCSG7O
                 WITH KEY ZFLSG7O = ZTMLCSG7O-ZFLSG7O BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSMLCSG7O TO ZTMLCSG7O.
               UPDATE ZTMLCSG7O.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTMLCSG7O.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSMLCSG7O.
            SELECT SINGLE * FROM  ZTMLCSG7O
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFLSG7O  EQ  IT_ZSMLCSG7O-ZFLSG7O.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSMLCSG7O TO ZTMLCSG7O.
               MOVE : ZFREQNO                  TO ZTMLCSG7O-ZFREQNO,
                      SY-MANDT                 TO ZTMLCSG7O-MANDT.
               INSERT  ZTMLCSG7O.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.
*  Master L/C Seg 8 기타부가조건
         SELECT * FROM ZTMLCSG8E WHERE ZFREQNO  EQ  ZFREQNO.

            READ TABLE IT_ZSMLCSG8E
                 WITH KEY ZFLSG8E = ZTMLCSG8E-ZFLSG8E BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSMLCSG8E TO ZTMLCSG8E.
               UPDATE ZTMLCSG8E.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTMLCSG8E.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSMLCSG8E.
            SELECT SINGLE * FROM  ZTMLCSG8E
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFLSG8E  EQ  IT_ZSMLCSG8E-ZFLSG8E.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSMLCSG8E TO ZTMLCSG8E.
               MOVE : ZFREQNO                  TO ZTMLCSG8E-ZFREQNO,
                      SY-MANDT                 TO ZTMLCSG8E-MANDT.
               INSERT  ZTMLCSG8E.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.

*  Master L/C Seg 9 기타구비서류
         SELECT * FROM ZTMLCSG9O WHERE ZFREQNO  EQ  ZFREQNO.

            READ TABLE IT_ZSMLCSG9O
                 WITH KEY ZFLSG9O = ZTMLCSG9O-ZFLSG9O BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSMLCSG9O TO ZTMLCSG9O.
               UPDATE ZTMLCSG9O.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTMLCSG9O.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSMLCSG9O.
            SELECT SINGLE * FROM  ZTMLCSG9O
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFLSG9O  EQ  IT_ZSMLCSG9O-ZFLSG9O.

            IF SY-SUBRC NE 0.
               MOVE-CORRESPONDING IT_ZSMLCSG9O TO ZTMLCSG9O.
               MOVE : ZFREQNO                  TO ZTMLCSG9O-ZFREQNO,
                      SY-MANDT                 TO ZTMLCSG9O-MANDT.
               INSERT  ZTMLCSG9O.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LC'
              exPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTMLCHD      =    w_ztmlchd
                      o_ZTMLCHD      =    w_ztmlchd_old
                      N_ZTMLCSG2     =    w_ZTMLCSG2
                      o_ZTMLCSG2     =    w_ZTMLCSG2_old
                      N_ZTMLCSG910   =    w_ZTMLCSG910
                      o_ZTMLCSG910   =    w_ZTMLCSG910_old
                      N_ZTREQHD      =    w_ZTREQHD
                      o_ZTREQHD      =    w_ZTREQHD_old
                      N_ZTREQst      =    w_ZTREQst
                      o_ZTREQst      =    w_ZTREQst_old.
*              TABLES
*                      IT_ZTREQORJ     =    IT_ZTREQORJ
*                      IT_ZTREQORJ_OLD =    IT_ZTREQORJ_OLD.


   ENDCASE.

ENDFUNCTION.
