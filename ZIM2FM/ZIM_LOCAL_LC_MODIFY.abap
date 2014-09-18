FUNCTION ZIM_LOCAL_LC_MODIFY.
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
*"     VALUE(W_ZTLLCHD) LIKE  ZTLLCHD STRUCTURE  ZTLLCHD
*"     VALUE(W_ZTLLCHD_OLD) LIKE  ZTLLCHD STRUCTURE  ZTLLCHD
*"     VALUE(W_ZTLLCSG23) LIKE  ZTLLCSG23 STRUCTURE  ZTLLCSG23
*"     VALUE(W_ZTLLCSG23_OLD) LIKE  ZTLLCSG23 STRUCTURE  ZTLLCSG23
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSLLCOF STRUCTURE  ZSLLCOF
*"      IT_ZSREQIT_OLD STRUCTURE  ZSREQIT
*"      IT_ZSREQIT STRUCTURE  ZSREQIT
*"      IT_ZTREQORJ_OLD STRUCTURE  ZSMLCSG7O
*"      IT_ZTREQORJ STRUCTURE  ZSMLCSG7O
*"      IT_ZSREQIL_OLD STRUCTURE  ZSREQIL
*"      IT_ZSREQIL STRUCTURE  ZSREQIL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
   DATA : W_ZFLSGOF    LIKE ZTLLCOF-ZFLSGOF.
* 삭제 지시 체크
   IF  W_OK_CODE EQ 'DELE'.   ZFSTATUS = 'X'.   ENDIF.

   MOVE-CORRESPONDING : W_ZTLLCHD   TO   ZTLLCHD,
                        W_ZTLLCSG23 TO   ZTLLCSG23.

   MOVE : ZFREQNO  TO    ZTLLCHD-ZFREQNO,     " Master L/C Header
          ZFREQNO  TO    ZTLLCSG23-ZFREQNO,    " L/C Seg. 2
          ZFREQNO  TO    ZTREQHD-ZFREQNO,
          ZFREQNO  TO    ZTREQST-ZFREQNO,
          ZFAMDNO  TO    ZTREQST-ZFAMDNO,
          SY-MANDT TO    ZTLLCHD-MANDT,
          SY-MANDT TO    ZTLLCSG23-MANDT.

*-----------------------------------------------------------------------
* 수입의뢰 HEADER 정의
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_REQUEST_DOC_MODIFY'
       EXPORTING
             ZFREQNO             =   ZFREQNO
             ZFAMDNO             =   ZFAMDNO
             ZFSTATUS            =   ZFSTATUS
             W_ZTREQHD           =   W_ZTREQHD
             W_ZTREQHD_OLD       =   W_ZTREQHD_OLD
             W_ZTREQST           =   W_ZTREQST
             W_OK_CODE           =   W_OK_CODE
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
*
*-----------------------------------------------------------------------
* LOCAL L/C MODIFY
*-----------------------------------------------------------------------
   CASE ZFSTATUS.
      WHEN 'C'.               " 생성
         INSERT     ZTLLCHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         INSERT     ZTLLCSG23.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         LOOP AT IT_ZSLLCOF.
            CLEAR : ZTLLCOF.
            MOVE-CORRESPONDING IT_ZSLLCOF TO ZTLLCOF.
            MOVE : ZFREQNO                TO ZTLLCOF-ZFREQNO,
                   IT_ZSLLCOF-ZFLSGOF     TO ZTLLCOF-ZFLSGOF,
                   SY-MANDT               TO ZTLLCOF-MANDT.

            INSERT   ZTLLCOF.
            IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
         ENDLOOP.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LO'
              exPORTING
                         UPD_CHNGIND    =    'I'
                         N_ZTLLCHD      =    w_ztLlchd
                         o_ZTLLCHD      =    w_ztLlchd_old
                         N_ZTLLCSG23    =    w_ZTLLCSG23
                         o_ZTLLCSG23    =    w_ZTLLCSG23_old
                         N_ZTREQHD      =    w_ZTREQHD
                         o_ZTREQHD      =    w_ZTREQHD_old
                         N_ZTREQst      =    w_ZTREQst
                         o_ZTREQst      =    w_ZTREQst_old.


*-----------------------------------------------------------------------
* 삭제
*-----------------------------------------------------------------------
      WHEN 'X'.               " 삭제
         DELETE FROM ZTLLCHD WHERE ZFREQNO EQ ZFREQNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         DELETE FROM ZTLLCSG23  WHERE ZFREQNO EQ ZFREQNO.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         DELETE FROM ZTLLCOF  WHERE ZFREQNO EQ ZFREQNO.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LO'
              exPORTING
                         UPD_CHNGIND    =    'D'
                         N_ZTLLCHD      =    w_ztLlchd
                         o_ZTLLCHD      =    w_ztLlchd_old
                         N_ZTLLCSG23    =    w_ZTLLCSG23
                         o_ZTLLCSG23    =    w_ZTLLCSG23_old
                         N_ZTREQHD      =    w_ZTREQHD
                         o_ZTREQHD      =    w_ZTREQHD_old
                         N_ZTREQst      =    w_ZTREQst
                         o_ZTREQst      =    w_ZTREQst_old.


*-----------------------------------------------------------------------
* Update
*-----------------------------------------------------------------------
      WHEN OTHERS.            " 변경
         UPDATE     ZTLLCHD.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

         UPDATE     ZTLLCSG23.
         IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
*  물품매도확약서
         SELECT * FROM ZTLLCOF   WHERE ZFREQNO  EQ  ZFREQNO.

            READ TABLE IT_ZSLLCOF
                 WITH KEY ZFLSGOF = ZTLLCOF-ZFLSGOF BINARY SEARCH.

            IF SY-SUBRC EQ 0.
               MOVE-CORRESPONDING IT_ZSLLCOF   TO ZTLLCOF.
               MOVE : ZFREQNO                  TO ZTLLCOF-ZFREQNO,
                      SY-MANDT                 TO ZTLLCOF-MANDT.
               UPDATE ZTLLCOF.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ELSE.
               DELETE ZTLLCOF.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDSELECT.

         LOOP AT IT_ZSLLCOF.
            SELECT SINGLE * FROM  ZTLLCOF
                            WHERE ZFREQNO  EQ  ZFREQNO
                            AND   ZFLSGOF  EQ  IT_ZSLLCOF-ZFLSGOF.

            IF SY-SUBRC NE 0.

               MOVE-CORRESPONDING IT_ZSLLCOF   TO ZTLLCOF.
               MOVE : ZFREQNO                  TO ZTLLCOF-ZFREQNO,
                      IT_ZSLLCOF-ZFLSGOF       TO ZTLLCOF-ZFLSGOF,
                      SY-MANDT                 TO ZTLLCOF-MANDT.
               INSERT  ZTLLCOF.
               IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.
            ENDIF.
         ENDLOOP.
* change document
         call function 'ZIM_CHANGE_DOCUMENT_LO'
              exPORTING
                         UPD_CHNGIND    =    'U'
                         N_ZTLLCHD      =    w_ztLlchd
                         o_ZTLLCHD      =    w_ztLlchd_old
                         N_ZTLLCSG23    =    w_ZTLLCSG23
                         o_ZTLLCSG23    =    w_ZTLLCSG23_old
                         N_ZTREQHD      =    w_ZTREQHD
                         o_ZTREQHD      =    w_ZTREQHD_old
                         N_ZTREQst      =    w_ZTREQst
                         o_ZTREQst      =    w_ZTREQst_old.


   ENDCASE.

ENDFUNCTION.
