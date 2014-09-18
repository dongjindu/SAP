FUNCTION z_fbm_abxodpdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXODPDT STRUCTURE  ZSBM_ABXODPDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxodpdt TYPE ztbm_abxodpdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxodpdt.
    MOVE-CORRESPONDING t_abxodpdt TO it_abxodpdt.
    APPEND it_abxodpdt.
  ENDLOOP.

**>>> Added By Tonkey 06/22/2004.
  MODIFY ztbm_abxodpdt FROM TABLE it_abxodpdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey 06/22/2004.
*  INSERT ZTBM_ABXODPDT FROM TABLE IT_ABXODPDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxodpdt-zzret = 'S'.
    MODIFY t_abxodpdt TRANSPORTING zzret
                             WHERE dpid GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxodpdt-zzret = 'E'.
    MODIFY t_abxodpdt TRANSPORTING zzret
                             WHERE dpid GE space.
  ENDIF.

ENDFUNCTION.
