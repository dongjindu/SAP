FUNCTION Z_FBM_ABYODPDT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABYODPDT STRUCTURE  ZSBM_ABYODPDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abyodpdt TYPE ztbm_abyodpdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abyodpdt.
    MOVE-CORRESPONDING t_abyodpdt TO it_abyodpdt.
    APPEND it_abyodpdt.
  ENDLOOP.

**>>> Added By Tonkey 06/22/2004.
  MODIFY ztbm_abyodpdt FROM TABLE it_abyodpdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey 06/22/2004.
*  INSERT ZTBM_abyodpdt FROM TABLE IT_abyodpdt ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abyodpdt-zzret = 'S'.
    MODIFY t_abyodpdt TRANSPORTING zzret
                             WHERE dpid GE space.
  ELSE.
    ROLLBACK WORK.
    t_abyodpdt-zzret = 'E'.
    MODIFY t_abyodpdt TRANSPORTING zzret
                             WHERE dpid GE space.
  ENDIF.


ENDFUNCTION.
