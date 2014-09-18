FUNCTION z_fmm_6000_01_get_data.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(EX_BUKRS) LIKE  ZSMM_6000_01-BUKRS
*"     VALUE(EX_LICODE) LIKE  ZSMM_6000_01-LICODE
*"     VALUE(EX_OPCODE) LIKE  ZSMM_6000_01-OPCODE
*"     VALUE(EX_CBLICODE)
*"     VALUE(EX_WERKS) LIKE  ZSMM_6000_01-WERKS
*"     VALUE(EX_FL_ONCE) TYPE  C
*"     VALUE(EX_VISIBLELINES) LIKE  SY-LOOPC
*"     VALUE(EX_LINES) TYPE  I
*"     VALUE(EX_TOP_LINE) TYPE  I
*"  TABLES
*"      EXT_ZSMM_6000_01 STRUCTURE  ZSMM_6000_01 OPTIONAL
*"----------------------------------------------------------------------
* Get all data from fields of external screen
  ex_bukrs    = io_bukrs.     "Company Code
  ex_licode   = io_licode.    "Line Code
  ex_opcode   = io_opcode.    "Operation Code
  ex_cblicode = cb_licode.    "Check Box
  ex_werks    = io_werks.
  ex_fl_once  = fl_once.

  ex_visiblelines = w_visiblelines.
  ex_lines        = tc_9001-lines.
  ex_top_line     = tc_9001-top_line.

  FIELD-SYMBOLS: <fs_zsmm_6000_01> LIKE LINE OF it_zsmm_6000_01.
  LOOP AT it_zsmm_6000_01 ASSIGNING <fs_zsmm_6000_01>
               WHERE bukrs = space OR
                     werks = space.
    <fs_zsmm_6000_01>-bukrs = io_bukrs.
    <fs_zsmm_6000_01>-werks = io_werks.
  ENDLOOP.

  ext_zsmm_6000_01[] = it_zsmm_6000_01.

ENDFUNCTION.
