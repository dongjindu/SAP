FUNCTION z_fmm_6000_02_get_data.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(EX_BUKRS) LIKE  ZSMM_6000_02-BUKRS
*"     VALUE(EX_MATNR) LIKE  ZSMM_6000_02-MATNR
*"     VALUE(EX_WERKS) LIKE  ZSMM_6000_02-WERKS
*"     VALUE(EX_LICODE) LIKE  ZSMM_6000_02-LICODE
*"     VALUE(EX_OPCODE) LIKE  ZSMM_6000_02-OPCODE
*"     VALUE(EX_ZTCONODISP) LIKE  ZSMM_6000_02-ZTCONO
*"     VALUE(EX_ZTCONO) LIKE  ZSMM_6000_02-ZTCONO
*"     VALUE(EX_ZAPPLIED_DATE) LIKE  ZSMM_6000_02-ZAPPLIED_DATE
*"     VALUE(EX_ERDAT) LIKE  ZSMM_6000_02-ERDAT
*"     VALUE(EX_ZCH_DESC_CD) LIKE  ZSMM_6000_02-ZCH_DESC_CD
*"     VALUE(EX_ZCH_REASON_CD) LIKE  ZSMM_6000_02-ZCH_REASON_CD
*"     VALUE(EX_ZINV_PROCESS_CD) LIKE  ZSMM_6000_02-ZINV_PROCESS_CD
*"     VALUE(EX_ZCH_MATNR1) LIKE  ZSMM_6000_02-ZCH_MATNR1
*"     VALUE(EX_ZCH_MATNR2) LIKE  ZSMM_6000_02-ZCH_MATNR2
*"     VALUE(EX_ZXFER_MATNR) LIKE  ZSMM_6000_02-ZXFER_MATNR
*"     VALUE(EX_FL_ONCE) TYPE  C
*"     VALUE(EX_VISIBLELINES) LIKE  SY-LOOPC
*"     VALUE(EX_LINES) TYPE  I
*"     VALUE(EX_TOP_LINE) TYPE  I
*"  TABLES
*"      EXT_ZSMM_6000_02 STRUCTURE  ZSMM_6000_02 OPTIONAL
*"----------------------------------------------------------------------
* Get all data from fields of external screen
  ex_bukrs           = io_bukrs.
  ex_matnr           = io_matnr.
  ex_werks           = io_werks.
  ex_ztconodisp      = io_ztconodisp.
  ex_ztcono          = io_ztcono.
  ex_zapplied_date   = io_zapplied_date.
  ex_erdat           = io_erdat.
  ex_zch_desc_cd     = io_zch_desc_cd.
  ex_zch_reason_cd   = io_zch_reason_cd.
  ex_zinv_process_cd = io_zinv_process_cd.
  ex_zch_matnr1      = io_zch_matnr1.
  ex_zch_matnr2      = io_zch_matnr2.
  ex_zxfer_matnr     = io_zxfer_matnr.
  ex_fl_once         = fl_once.

  ex_visiblelines    = w_visiblelines.
  ex_lines           = tc_9001-lines.
  ex_top_line        = tc_9001-top_line.

  FIELD-SYMBOLS: <fs_zsmm_6000_02> LIKE LINE OF it_zsmm_6000_02.
  LOOP AT it_zsmm_6000_02 ASSIGNING <fs_zsmm_6000_02>
              WHERE bukrs  = space OR
                    werks  = space OR
*                     ztcono = space OR
                    matnr  = space.
    <fs_zsmm_6000_02>-bukrs  = io_bukrs.
    <fs_zsmm_6000_02>-werks  = io_werks.
*    <fs_zsmm_6000_02>-ztcono = io_ztcono.
    <fs_zsmm_6000_02>-matnr  = io_matnr.
  ENDLOOP.

  ext_zsmm_6000_02[] = it_zsmm_6000_02.

ENDFUNCTION.
