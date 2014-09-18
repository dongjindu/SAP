FUNCTION Z_BM_CHECK_CONFIGURABLE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATUV) TYPE  RC29L-DATUV
*"     VALUE(DATUB) TYPE  RC29L-DATUB
*"  TABLES
*"      IT_MAST STRUCTURE  MAST
*"      IT_CNFG STRUCTURE  ZSPP0024_9000
*"----------------------------------------------------------------------
REFRESH it_cnfg.
  v_datub = datub.
  v_datuv = datuv.
  xmast[] = it_mast[].

*  PERFORM read_module_id.
  LOOP AT xmast.
    PERFORM search_parants USING xmast-matnr
                                 xmast-werks
                                 xmast-stlan.
  ENDLOOP.
  it_cnfg[] = xcnfg[].
ENDFUNCTION.
