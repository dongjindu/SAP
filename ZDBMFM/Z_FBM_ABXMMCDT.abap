FUNCTION z_fbm_abxmmcdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXMMCDT STRUCTURE  ZSBM_ABXMMCDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxmmcdt TYPE ztbm_abxmmcdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxmmcdt.
    MOVE-CORRESPONDING t_abxmmcdt TO it_abxmmcdt.
    APPEND it_abxmmcdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxmmcdt FROM TABLE it_abxmmcdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey.
*  INSERT ztbm_abxmmcdt FROM TABLE it_abxmmcdt ACCEPTING DUPLICATE KEYS.
**<<< Inactivated.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxmmcdt-zzret = 'S'.
    MODIFY t_abxmmcdt TRANSPORTING zzret
                             WHERE mtno GE space
                             AND   plnt GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxmmcdt-zzret = 'E'.
    MODIFY t_abxmmcdt TRANSPORTING zzret
                             WHERE mtno GE space
                             AND   plnt GE space.
  ENDIF.

ENDFUNCTION.
