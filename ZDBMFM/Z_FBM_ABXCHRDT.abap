FUNCTION z_fbm_abxchrdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXCHRDT STRUCTURE  ZSBM_ABXCHRDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxchrdt TYPE ztbm_abxchrdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxchrdt.
    MOVE-CORRESPONDING t_abxchrdt TO it_abxchrdt.
    APPEND it_abxchrdt.
  ENDLOOP.
*Issue num : PP-20040728-001 :2004.08.12
*Changed by wskim, Requested by iychoi
*---start
*  INSERT ZTBM_ABXCHRDT FROM TABLE IT_ABXCHRDT ACCEPTING DUPLICATE KEYS.
  MODIFY ztbm_abxchrdt FROM TABLE it_abxchrdt.
*---end
  IF sy-subrc EQ 0.
    t_abxchrdt-zzret = 'S'.
    MODIFY t_abxchrdt TRANSPORTING zzret
                             WHERE chid GE space
                             AND   chvl GE space.
  ELSE.
    t_abxchrdt-zzret = 'E'.
    MODIFY t_abxchrdt TRANSPORTING zzret
                             WHERE chid GE space
                             AND   chvl GE space.
  ENDIF.

ENDFUNCTION.
