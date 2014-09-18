FUNCTION z_fbm_abxmmrdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXMMRDT STRUCTURE  ZSBM_ABXMMRDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxmmrdt TYPE ztbm_abxmmrdt OCCURS 0 WITH HEADER LINE.
  DATA: it_ammr     TYPE ztbm_ammr     OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxmmrdt.
    MOVE-CORRESPONDING: t_abxmmrdt TO it_abxmmrdt,
                        t_abxmmrdt TO it_ammr.
    it_ammr-zdate = t_abxmmrdt-zedat.
    APPEND it_abxmmrdt.
    APPEND it_ammr.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxmmrdt FROM TABLE it_abxmmrdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXMMRDT FROM TABLE IT_ABXMMRDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.

**>>> Added By Tonkey on 06/22/2004.
    modify ztbm_ammr FROM TABLE it_ammr.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*    INSERT ztbm_ammr FROM TABLE it_ammr ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

    IF sy-subrc EQ 0.
      COMMIT WORK.
      t_abxmmrdt-zzret = 'S'.
      MODIFY t_abxmmrdt TRANSPORTING zzret
                               WHERE mtno GE space
                               AND   plnt GE space.
    ELSE.
      ROLLBACK WORK.
      t_abxmmrdt-zzret = 'E'.
      MODIFY t_abxmmrdt TRANSPORTING zzret
                               WHERE mtno GE space
                               AND   plnt GE space.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    t_abxmmrdt-zzret = 'E'.
    MODIFY t_abxmmrdt TRANSPORTING zzret
                             WHERE mtno GE space
                             AND   plnt GE space.
  ENDIF.
ENDFUNCTION.
