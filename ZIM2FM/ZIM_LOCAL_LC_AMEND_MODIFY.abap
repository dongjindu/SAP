FUNCTION ZIM_LOCAL_LC_AMEND_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQNO) LIKE  ZTREQHD-ZFREQNO
*"     VALUE(ZFAMDNO) LIKE  ZTREQST-ZFAMDNO
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTREQHD_OLD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     VALUE(W_ZTREQHD) LIKE  ZTREQHD STRUCTURE  ZTREQHD
*"     VALUE(W_ZTREQST_OLD) LIKE  ZTREQST STRUCTURE  ZTREQST
*"     VALUE(W_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST
*"     VALUE(W_ZTLLCAMHD_OLD) LIKE  ZTLLCAMHD STRUCTURE  ZTLLCAMHD
*"     VALUE(W_ZTLLCAMHD) LIKE  ZTLLCAMHD STRUCTURE  ZTLLCAMHD
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSLLCAMSGOF STRUCTURE  ZSLLCAMSGOF
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

   MOVE-CORRESPONDING : W_ZTLLCAMHD   TO   ZTLLCAMHD.

   MOVE : ZFREQNO  TO    ZTLLCAMHD-ZFREQNO,     " Master L/C Header
          ZFAMDNO  TO    ZTLLCAMHD-ZFAMDNO,     " Master L/C Header
          SY-MANDT TO    ZTLLCAMHD-MANDT.
   MOVE : ZFREQNO  TO    W_ZTLLCAMHD-ZFREQNO,     " Master L/C Header
          ZFAMDNO  TO    W_ZTLLCAMHD-ZFAMDNO,     " Master L/C Header
          SY-MANDT TO    W_ZTLLCAMHD-MANDT.
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
             w_ztreqhd_old       =   w_ztreqhd_old
             W_ZTREQST           =   W_ZTREQST
       TABLES
             IT_ZSREQIT          =   IT_ZSREQIT
             IT_ZSREQIT_OLD      =   IT_ZSREQIT_OLD
             IT_ZSREQIL          =   IT_ZSREQIL
             IT_ZSREQIL_old      =   IT_ZSREQIL_old
             IT_ZTREQORJ         =   IT_ZTREQORJ
             IT_ZTREQORJ_old     =   IT_ZTREQORJ_old
       EXCEPTIONS
              ERROR_UPDATE.

   IF SY-SUBRC NE  0.   RAISE ERROR_UPDATE.   ENDIF.

   MOVE-CORRESPONDING : W_ZTLLCAMHD   TO   ZTLLCAMHD.
* MASTER L/C MODIFY
   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         INSERT     ZTLLCAMHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LO_AMEND'
              EXPORTING
                      UPD_CHNGIND     =    'I'
                      N_ZTLLCAMHD     =    W_ZTLLCAMHD
                      O_ZTLLCAMHD     =    W_ZTLLCAMHD_OLD
                      N_ZTREQHD       =    w_ZTREQHD
                      o_ZTREQHD       =    w_ZTREQHD_old
                      N_ZTREQst       =    w_ZTREQst
                      o_ZTREQst       =    w_ZTREQst_OLD.

      WHEN 'X'.               " 삭제
         DELETE  FROM ZTLLCAMHD    WHERE ZFREQNO EQ ZFREQNO
                                   AND   ZFAMDNO EQ ZFAMDNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         DELETE  FROM ZTLLCAMSGOF  WHERE ZFREQNO EQ ZFREQNO
                                   AND   ZFAMDNO EQ ZFAMDNO.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LO_AMEND'
              EXPORTING
                      UPD_CHNGIND     =    'D'
                      N_ZTLLCAMHD     =    W_ZTLLCAMHD
                      O_ZTLLCAMHD     =    W_ZTLLCAMHD_OLD
                      N_ZTREQHD       =    w_ZTREQHD
                      o_ZTREQHD       =    w_ZTREQHD_old
                      N_ZTREQst       =    w_ZTREQst
                      o_ZTREQst       =    w_ZTREQst_OLD.
         EXIT.
*-----------------------------------------------------------------------
* UPDATE
*-----------------------------------------------------------------------
      WHEN OTHERS.            " 변경
         UPDATE     ZTLLCAMHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LO_AMEND'
              EXPORTING
                      UPD_CHNGIND     =    'U'
                      N_ZTLLCAMHD     =    W_ZTLLCAMHD
                      O_ZTLLCAMHD     =    W_ZTLLCAMHD_OLD
                      N_ZTREQHD       =    w_ZTREQHD
                      o_ZTREQHD       =    w_ZTREQHD_old
                      N_ZTREQst       =    w_ZTREQst
                      o_ZTREQst       =    w_ZTREQst_OLD.

   ENDCASE.
* 기타조건명세
   SELECT * FROM ZTLLCAMSGOF WHERE ZFREQNO  EQ  ZFREQNO
                             AND   ZFAMDNO  EQ  ZFAMDNO.

      READ TABLE IT_ZSLLCAMSGOF
           WITH KEY ZFLSGOF = ZTLLCAMSGOF-ZFLSGOF BINARY SEARCH.

      IF SY-SUBRC EQ 0.
         MOVE-CORRESPONDING IT_ZSLLCAMSGOF TO ZTLLCAMSGOF.
         MOVE : ZFREQNO                  TO ZTLLCAMSGOF-ZFREQNO,
                ZFAMDNO                  TO ZTLLCAMSGOF-ZFAMDNO,
                SY-MANDT                 TO ZTLLCAMSGOF-MANDT.
         UPDATE ZTLLCAMSGOF.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ELSE.
         DELETE ZTLLCAMSGOF.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDSELECT.

   LOOP AT IT_ZSLLCAMSGOF.
      SELECT SINGLE * FROM  ZTLLCAMSGOF
                      WHERE ZFREQNO  EQ  ZFREQNO
                      AND   ZFAMDNO  EQ  ZFAMDNO
                      AND   ZFLSGOF  EQ  IT_ZSLLCAMSGOF-ZFLSGOF.

      IF SY-SUBRC NE 0.
         MOVE-CORRESPONDING IT_ZSLLCAMSGOF TO ZTLLCAMSGOF.
         MOVE : ZFREQNO                  TO ZTLLCAMSGOF-ZFREQNO,
                ZFAMDNO                  TO ZTLLCAMSGOF-ZFAMDNO,
                SY-MANDT                 TO ZTLLCAMSGOF-MANDT.
         INSERT  ZTLLCAMSGOF.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDLOOP.


ENDFUNCTION.
