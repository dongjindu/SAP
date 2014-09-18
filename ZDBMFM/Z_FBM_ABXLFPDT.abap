FUNCTION z_fbm_abxlfpdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXLFPDT STRUCTURE  ZSBM_ABXLFPDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxlfpdt TYPE ztbm_abxlfpdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxlfpdt.
    MOVE-CORRESPONDING t_abxlfpdt TO it_abxlfpdt.
    APPEND it_abxlfpdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxlfpdt FROM TABLE it_abxlfpdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ztbm_abxlfpdt FROM TABLE it_abxlfpdt ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxlfpdt-zzret = 'S'.
    MODIFY t_abxlfpdt TRANSPORTING zzret
                             WHERE mtno GE space
                             AND   plnt GE space
                             AND   vers GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxlfpdt-zzret = 'E'.
    MODIFY t_abxlfpdt TRANSPORTING zzret
                             WHERE mtno GE space
                             AND   plnt GE space
                             AND   vers GE space.
  ENDIF.

ENDFUNCTION.
