FUNCTION Z_FMM_6000_05_SET_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(IM_OK_CODE) LIKE  SY-UCOMM
*"     VALUE(IM_MATNR) LIKE  ZSMM_6000_05-MATNR OPTIONAL
*"     VALUE(IM_WERKS) LIKE  ZSMM_6000_05-WERKS OPTIONAL
*"     VALUE(IM_LICODE) LIKE  ZSMM_6000_05-LICODE OPTIONAL
*"     VALUE(IM_OPCODE) LIKE  ZSMM_6000_05-OPCODE OPTIONAL
*"     VALUE(IM_FL_ONCE) TYPE  C OPTIONAL
*"  TABLES
*"      IMT_ZSMM_6000_05 STRUCTURE  ZSMM_6000_05 OPTIONAL
*"----------------------------------------------------------------------
* Set all data to external screen
  w_ok_code          = im_ok_code.   "OK Code

  io_matnr           = im_matnr.
  io_werks           = im_werks.
  io_licode          = im_LICODE.
  io_opcode          = im_OPCODE.
  fl_once            = im_fl_once.

  it_zsmm_6000_05 = imt_zsmm_6000_05[].

ENDFUNCTION.
