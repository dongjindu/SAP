FUNCTION z_fmm_6000_01_set_data.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IM_OK_CODE) LIKE  SY-UCOMM OPTIONAL
*"     VALUE(IM_BUKRS) LIKE  ZSMM_6000_01-BUKRS OPTIONAL
*"     VALUE(IM_LICODE) LIKE  ZSMM_6000_01-LICODE OPTIONAL
*"     VALUE(IM_OPCODE) LIKE  ZSMM_6000_01-OPCODE OPTIONAL
*"     VALUE(IM_CBLICODE) OPTIONAL
*"     VALUE(IM_WERKS) LIKE  ZSMM_6000_01-WERKS OPTIONAL
*"     VALUE(IM_FL_ONCE) TYPE  C OPTIONAL
*"     VALUE(IM_VISIBLELINES) LIKE  SY-LOOPC OPTIONAL
*"     VALUE(IM_LINES) TYPE  I OPTIONAL
*"     VALUE(IM_TOP_LINE) TYPE  I OPTIONAL
*"  TABLES
*"      IMT_ZSMM_6000_01 STRUCTURE  ZSMM_6000_01 OPTIONAL
*"----------------------------------------------------------------------
* Set all data to external screen
  w_ok_code = im_ok_code.   "OK Code

  io_bukrs  = im_bukrs.     "Company Code
  io_licode = im_licode.    "Line Code
  io_opcode = im_opcode.    "Operation Code
  cb_licode = im_cblicode.  "Check Box
  io_werks  = im_werks.
  fl_once   = im_fl_once.

  w_visiblelines   = im_visiblelines.
  tc_9001-lines    = im_lines.
  tc_9001-top_line = im_top_line.

  it_zsmm_6000_01 = imt_zsmm_6000_01[].

ENDFUNCTION.
