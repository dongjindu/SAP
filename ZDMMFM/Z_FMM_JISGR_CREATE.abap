FUNCTION Z_FMM_JISGR_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(WA_GDSMVT_HDR) LIKE  BAPI2017_GM_HEAD_01 STRUCTURE
*"        BAPI2017_GM_HEAD_01
*"     VALUE(WA_GDSMVT_CODE) LIKE  BAPI2017_GM_CODE STRUCTURE
*"        BAPI2017_GM_CODE
*"     VALUE(W_TSTRUN) LIKE  BAPI2017_GM_GEN-TESTRUN DEFAULT SPACE
*"  EXPORTING
*"     VALUE(WA_GDSMVT_HDRRTN) LIKE  BAPI2017_GM_HEAD_RET STRUCTURE
*"        BAPI2017_GM_HEAD_RET
*"     VALUE(W_MATDOC) TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC
*"     VALUE(W_MDYEAR) TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR
*"  TABLES
*"      IT_GDSMVT_ITM STRUCTURE  BAPI2017_GM_ITEM_CREATE
*"      IT_GDSMVT_SNO STRUCTURE  BAPI2017_GM_SERIALNUMBER OPTIONAL
*"      IT_RET STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------&
*  Program      : Z_FMM_JISGR_CREATE
*  Author       : Shiva
*  Specification: To improve performance create as RFC for parallel
*                 processing.
*&--------------------------------------------------------------------&*
* Date           Developer       RequestNo      Description
* 04/21/2005     Shiva           UD1K915697      parallel process
*&--------------------------------------------------------------------&*

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
       EXPORTING
            goodsmvt_header       = wa_gdsmvt_hdr
            goodsmvt_code         = wa_gdsmvt_code
            TESTRUN               = w_tstrun
       IMPORTING
            goodsmvt_headret      = wa_gdsmvt_hdrrtn
            materialdocument      = w_matdoc
            matdocumentyear       = w_mdyear
       TABLES
            goodsmvt_item         = it_gdsmvt_itm
            goodsmvt_serialnumber = it_gdsmvt_sno
            return                = it_ret.

  read table it_ret with key type = 'E'.
  if sy-subrc ne 0.
    commit work.
  else.
    rollback work.
  endif.
ENDFUNCTION.
