FUNCTION Z_FMM_6000_05_GET_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(EX_MATNR) LIKE  ZSMM_6000_05-MATNR
*"     VALUE(EX_WERKS) LIKE  ZSMM_6000_05-WERKS
*"     VALUE(EX_LICODE) LIKE  ZSMM_6000_05-LICODE
*"     VALUE(EX_OPCODE) LIKE  ZSMM_6000_05-OPCODE
*"     VALUE(EX_FL_ONCE) TYPE  C
*"  TABLES
*"      EXT_ZSMM_6000_05 STRUCTURE  ZSMM_6000_05 OPTIONAL
*"----------------------------------------------------------------------
* Get all data from fields of external screen
  ex_matnr           = io_matnr.
  ex_werks           = io_werks.
  EX_LICODE          = io_licode.
  EX_OPCODE          = io_opcode.
  ex_fl_once         = fl_once.

  ext_zsmm_6000_05[] = it_zsmm_6000_05.

ENDFUNCTION.
