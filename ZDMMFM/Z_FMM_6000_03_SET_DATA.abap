FUNCTION Z_FMM_6000_03_SET_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IM_MATNR) LIKE  ZSMM_6000_02-MATNR OPTIONAL
*"     VALUE(IM_WERKS) LIKE  ZSMM_6000_02-WERKS OPTIONAL
*"     VALUE(IM_ZTCONO) LIKE  ZSMM_6000_03-ZTCONO OPTIONAL
*"     VALUE(IM_FL_ONCE) TYPE  C OPTIONAL
*"  TABLES
*"      IMT_ZSMM_6000_03 STRUCTURE  ZSMM_6000_03 OPTIONAL
*"----------------------------------------------------------------------
* Set all data to external screen
  io_matnr           = im_matnr.
  io_werks           = im_werks.
  io_ztcono          = im_ztcono.
  fl_once            = im_fl_once.

  it_zsmm_6000_03 = imt_zsmm_6000_03[].

ENDFUNCTION.
