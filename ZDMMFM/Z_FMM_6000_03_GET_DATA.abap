FUNCTION Z_FMM_6000_03_GET_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(EX_MATNR) LIKE  ZSMM_6000_02-MATNR
*"     VALUE(EX_WERKS) LIKE  ZSMM_6000_02-WERKS
*"     VALUE(EX_ZTCONO) LIKE  ZSMM_6000_03-ZTCONO
*"     VALUE(EX_FL_ONCE) TYPE  C
*"  TABLES
*"      EXT_ZSMM_6000_03 STRUCTURE  ZSMM_6000_03 OPTIONAL
*"----------------------------------------------------------------------
* Get all data from fields of external screen
  ex_matnr           = io_matnr.
  ex_werks           = io_werks.
  ex_ztcono          = io_ztcono.
  ex_fl_once         = fl_once.

  ext_zsmm_6000_03[] = it_zsmm_6000_03.

ENDFUNCTION.
