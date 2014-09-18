FUNCTION Z_FMM_6000_06_SET_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IM_MATNR) LIKE  ZSMM_6000_05-MATNR OPTIONAL
*"     VALUE(IM_WERKS) LIKE  ZSMM_6000_05-WERKS OPTIONAL
*"     VALUE(IM_LICODE) LIKE  ZSMM_6000_05-LICODE OPTIONAL
*"     VALUE(IM_OPCODE) LIKE  ZSMM_6000_05-OPCODE OPTIONAL
*"     VALUE(IM_FL_ONCE) TYPE  C OPTIONAL
*"  TABLES
*"      IMT_ZSMM_6000_06 STRUCTURE  ZSMM_6000_06 OPTIONAL
*"----------------------------------------------------------------------
* Set all data to external screen
  io_matnr           = im_matnr.
  io_werks           = im_werks.
  io_licode          = im_LICODE.
  io_opcode          = im_OPCODE.
  fl_once            = im_fl_once.

  it_zsmm_6000_06 = imt_zsmm_6000_06[].

ENDFUNCTION.
