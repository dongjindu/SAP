FUNCTION ZIM_OTHER_DOC_MODIFY.
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
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_ZSREQIT_OLD STRUCTURE  ZSREQIT
*"      IT_ZSREQIT STRUCTURE  ZSREQIT
*"      IT_ZTREQORJ_OLD STRUCTURE  ZSMLCSG7O
*"      IT_ZTREQORJ STRUCTURE  ZSMLCSG7O
*"      IT_ZSREQIL_OLD STRUCTURE  ZSREQIL
*"      IT_ZSREQIL STRUCTURE  ZSREQIL
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------

   if  w_ok_code eq 'DELE'.   zfstatus = 'X'.   endif.
   move : zfreqno  to    ztreqst-zfreqno,
          zfamdno  to    ztreqst-zfamdno.

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

*-----------------------------------------------------------------------
* 기타 수입입의뢰 문서
*-----------------------------------------------------------------------
   case zfstatus.
      when 'C'.               " 생성
* change document
         call function 'ZIM_CHANGE_DOCUMENT_OT'
              exporting
                         upd_chngind    =    'I'
                         n_ztreqhd      =    w_ztreqhd
                         o_ztreqhd      =    w_ztreqhd_old
                         n_ztreqst      =    w_ztreqst
                         O_ztreqst      =    w_ztreqst_OLD.
      when 'X'.               " 삭제
* change document
         call function 'ZIM_CHANGE_DOCUMENT_OT'
              exporting
                         upd_chngind    =    'D'
                         n_ztreqhd      =    w_ztreqhd
                         o_ztreqhd      =    w_ztreqhd_old
                         n_ztreqst      =    w_ztreqst
                         O_ztreqst      =    w_ztreqst_OLD.
      when others.            " 변경
* change document
         call function 'ZIM_CHANGE_DOCUMENT_OT'
              exporting
                         upd_chngind    =    'U'
                         n_ztreqhd      =    w_ztreqhd
                         o_ztreqhd      =    w_ztreqhd_old
                         n_ztreqst      =    w_ztreqst
                         O_ztreqst      =    w_ztreqst_OLD.
   endcase.
ENDFUNCTION.
