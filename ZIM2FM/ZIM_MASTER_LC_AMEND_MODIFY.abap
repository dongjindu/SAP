FUNCTION ZIM_MASTER_LC_AMEND_MODIFY.
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
*"     VALUE(W_ZTMLCAMHD_OLD) LIKE  ZTMLCAMHD STRUCTURE  ZTMLCAMHD
*"     VALUE(W_ZTMLCAMHD) LIKE  ZTMLCAMHD STRUCTURE  ZTMLCAMHD
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSMLCAMNARR STRUCTURE  ZSMLCAMNARR
*"      IT_ZSREQIT STRUCTURE  ZSREQIT
*"      IT_ZSREQIT_OLD STRUCTURE  ZSREQIT
*"      IT_ZTREQORJ STRUCTURE  ZSMLCSG7O
*"      IT_ZTREQORJ_OLD STRUCTURE  ZSMLCSG7O
*"      IT_ZSREQIL_OLD STRUCTURE  ZSREQIL
*"      IT_ZSREQIL STRUCTURE  ZSREQIL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
* 삭제 지시자 검증
   IF W_OK_CODE EQ 'DELE'.   ZFSTATUS = 'X'.  ENDIF.

   MOVE-CORRESPONDING : W_ZTMLCAMHD   TO   ZTMLCAMHD.

   MOVE : ZFREQNO  TO    ZTMLCAMHD-ZFREQNO,     " Master L/C Header
          ZFAMDNO  TO    ZTMLCAMHD-ZFAMDNO,     " Master L/C Header
          SY-MANDT TO    ZTMLCAMHD-MANDT,
          SY-MANDT TO    W_ZTMLCAMHD-MANDT,
          ZFREQNO  TO    W_ZTMLCAMHD-ZFREQNO,
          ZFAMDNO  TO    W_ZTMLCAMHD-ZFAMDNO,
          SY-MANDT TO    W_ZTMLCAMHD_OLD-MANDT,
          ZFREQNO  TO    W_ZTMLCAMHD_OLD-ZFREQNO,
          ZFAMDNO  TO    W_ZTMLCAMHD_OLD-ZFAMDNO.

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
             W_ZTREQHD_old       =   W_ZTREQHD_old
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

* MASTER L/C MODIFY
   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         MOVE-CORRESPONDING : W_ZTMLCAMHD   TO   ZTMLCAMHD.
         INSERT     ZTMLCAMHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LC_AMEND'
              EXPORTING
                      UPD_CHNGIND     =    'I'
                      N_ZTMLCAMHD     =    ZTMLCAMHD
                      O_ZTMLCAMHD     =    ZTMLCAMHD
                      N_ZTREQHD       =    w_ZTREQHD
                      o_ZTREQHD       =    w_ZTREQHD_old
                      N_ZTREQst       =    w_ZTREQst
                      o_ZTREQst       =    w_ZTREQst_OLD.

      WHEN 'X'.               " 삭제
         DELETE  FROM ZTMLCAMHD    WHERE ZFREQNO EQ ZFREQNO
                                   AND   ZFAMDNO EQ ZFAMDNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         DELETE  FROM ZTMLCAMNARR  WHERE ZFREQNO EQ ZFREQNO
                                   AND   ZFAMDNO EQ ZFAMDNO.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LC_AMEND'
              EXPORTING
                      UPD_CHNGIND     =    'D'
                      N_ZTMLCAMHD     =    W_ZTMLCAMHD
                      O_ZTMLCAMHD     =    W_ZTMLCAMHD_OLD
                      N_ZTREQHD       =    w_ZTREQHD
                      o_ZTREQHD       =    w_ZTREQHD_old
                      N_ZTREQst       =    w_ZTREQst
                      o_ZTREQst       =    w_ZTREQst_OLD.

*        IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         EXIT.
*-----------------------------------------------------------------------
* UPDATE
*-----------------------------------------------------------------------
      WHEN OTHERS.            " 변경
         MOVE-CORRESPONDING : W_ZTMLCAMHD   TO   ZTMLCAMHD.
         UPDATE     ZTMLCAMHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LC_AMEND'
              EXPORTING
                      UPD_CHNGIND     =    'U'
                      N_ZTMLCAMHD     =    W_ZTMLCAMHD
                      O_ZTMLCAMHD     =    W_ZTMLCAMHD_OLD
                      N_ZTREQHD       =    w_ZTREQHD
                      o_ZTREQHD       =    w_ZTREQHD_old
                      N_ZTREQst       =    w_ZTREQst
                      o_ZTREQst       =    w_ZTREQst_OLD.

   ENDCASE.
* 기타조건명세
   SELECT * FROM ZTMLCAMNARR WHERE ZFREQNO  EQ  ZFREQNO
                             AND   ZFAMDNO  EQ  ZFAMDNO.

      READ TABLE IT_ZSMLCAMNARR
           WITH KEY ZFLNARR = ZTMLCAMNARR-ZFLNARR BINARY SEARCH.

      IF SY-SUBRC EQ 0.
         MOVE-CORRESPONDING IT_ZSMLCAMNARR TO ZTMLCAMNARR.
         MOVE : ZFREQNO                  TO ZTMLCAMNARR-ZFREQNO,
                ZFAMDNO                  TO ZTMLCAMNARR-ZFAMDNO,
                SY-MANDT                 TO ZTMLCAMNARR-MANDT.
         UPDATE ZTMLCAMNARR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ELSE.
         DELETE ZTMLCAMNARR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDSELECT.

   LOOP AT IT_ZSMLCAMNARR.
      SELECT SINGLE * FROM  ZTMLCAMNARR
                      WHERE ZFREQNO  EQ  ZFREQNO
                      AND   ZFAMDNO  EQ  ZFAMDNO
                      AND   ZFLNARR  EQ  IT_ZSMLCAMNARR-ZFLNARR.

      IF SY-SUBRC NE 0.
         MOVE-CORRESPONDING IT_ZSMLCAMNARR TO ZTMLCAMNARR.
         MOVE : ZFREQNO                  TO ZTMLCAMNARR-ZFREQNO,
                ZFAMDNO                  TO ZTMLCAMNARR-ZFAMDNO,
                SY-MANDT                 TO ZTMLCAMNARR-MANDT.
         INSERT  ZTMLCAMNARR.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
      ENDIF.
   ENDLOOP.

ENDFUNCTION.
