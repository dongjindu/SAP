FUNCTION z_fmm_6000_02_set_data.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IM_OK_CODE) LIKE  SY-UCOMM OPTIONAL
*"     VALUE(IM_BUKRS) LIKE  ZSMM_6000_02-BUKRS OPTIONAL
*"     VALUE(IM_MATNR) LIKE  ZSMM_6000_02-MATNR OPTIONAL
*"     VALUE(IM_WERKS) LIKE  ZSMM_6000_02-WERKS OPTIONAL
*"     VALUE(IM_LICODE) LIKE  ZSMM_6000_02-LICODE OPTIONAL
*"     VALUE(IM_OPCODE) LIKE  ZSMM_6000_02-OPCODE OPTIONAL
*"     VALUE(IM_ZTCONODISP) LIKE  ZSMM_6000_02-ZTCONO OPTIONAL
*"     VALUE(IM_ZTCONO) LIKE  ZSMM_6000_02-ZTCONO OPTIONAL
*"     VALUE(IM_ZAPPLIED_DATE) LIKE  ZSMM_6000_02-ZAPPLIED_DATE
*"       OPTIONAL
*"     VALUE(IM_ERDAT) LIKE  ZSMM_6000_02-ERDAT OPTIONAL
*"     VALUE(IM_ZCH_DESC_CD) LIKE  ZSMM_6000_02-ZCH_DESC_CD OPTIONAL
*"     VALUE(IM_ZCH_REASON_CD) LIKE  ZSMM_6000_02-ZCH_REASON_CD
*"       OPTIONAL
*"     VALUE(IM_ZINV_PROCESS_CD) LIKE  ZSMM_6000_02-ZINV_PROCESS_CD
*"       OPTIONAL
*"     VALUE(IM_ZCH_MATNR1) LIKE  ZSMM_6000_02-ZCH_MATNR1 OPTIONAL
*"     VALUE(IM_ZCH_MATNR2) LIKE  ZSMM_6000_02-ZCH_MATNR2 OPTIONAL
*"     VALUE(IM_ZXFER_MATNR) LIKE  ZSMM_6000_02-ZXFER_MATNR OPTIONAL
*"     VALUE(IM_FL_ONCE) TYPE  C OPTIONAL
*"     VALUE(IM_VISIBLELINES) LIKE  SY-LOOPC OPTIONAL
*"     VALUE(IM_LINES) TYPE  I OPTIONAL
*"     VALUE(IM_TOP_LINE) TYPE  I OPTIONAL
*"     VALUE(IM_TRTYP) TYPE  TRTYP OPTIONAL
*"  TABLES
*"      IMT_ZSMM_6000_02 STRUCTURE  ZSMM_6000_02 OPTIONAL
*"----------------------------------------------------------------------
* Set all data to external screen
  w_ok_code          = im_ok_code.   "OK Code

  io_bukrs           = im_bukrs.
  io_matnr           = im_matnr.
  io_werks           = im_werks.
  io_ztconodisp      = im_ztconodisp.
  io_ztcono          = im_ztcono.
  io_zapplied_date   = im_zapplied_date.
  io_erdat           = im_erdat.
  io_zch_desc_cd     = im_zch_desc_cd.
  io_zch_reason_cd   = im_zch_reason_cd.
  io_zinv_process_cd = im_zinv_process_cd.
  io_zch_matnr1      = im_zch_matnr1.
  io_zch_matnr2      = im_zch_matnr2.
  io_zxfer_matnr     = im_zxfer_matnr.
  fl_once            = im_fl_once.

  w_visiblelines       = im_visiblelines.
  tc_9001-lines      = im_lines.
  tc_9001-top_line   = im_top_line.

  w_trtyp           = im_trtyp.

  it_zsmm_6000_02 = imt_zsmm_6000_02[].

ENDFUNCTION.
