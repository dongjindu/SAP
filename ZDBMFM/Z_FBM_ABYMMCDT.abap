FUNCTION Z_FBM_ABYMMCDT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABYMMCDT STRUCTURE  ZSBM_ABYMMCDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abymmcdt TYPE ztbm_abymmcdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abymmcdt.
    MOVE-CORRESPONDING t_abymmcdt TO it_abymmcdt.
    APPEND it_abymmcdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abymmcdt FROM TABLE it_abymmcdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey.
*  INSERT ztbm_abymmcdt FROM TABLE it_abymmcdt ACCEPTING DUPLICATE KEYS.
**<<< Inactivated.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abymmcdt-zzret = 'S'.
    MODIFY t_abymmcdt TRANSPORTING zzret
                             WHERE mtno GE space
                             AND   plnt GE space.
  ELSE.
    ROLLBACK WORK.
    t_abymmcdt-zzret = 'E'.
    MODIFY t_abymmcdt TRANSPORTING zzret
                             WHERE mtno GE space
                             AND   plnt GE space.
  ENDIF.

ENDFUNCTION.
