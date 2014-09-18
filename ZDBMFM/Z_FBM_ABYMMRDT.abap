FUNCTION Z_FBM_ABYMMRDT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABYMMRDT STRUCTURE  ZSBM_ABYMMRDT_RFC
*"----------------------------------------------------------------------

  DATA: it_abymmrdt TYPE ztbm_abymmrdt OCCURS 0 WITH HEADER LINE.
  DATA: it_ammr     TYPE ztbm_ammr     OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abymmrdt.
    MOVE-CORRESPONDING: t_abymmrdt TO it_abymmrdt,
                        t_abymmrdt TO it_ammr.
    it_ammr-zdate = t_abymmrdt-zedat.
    APPEND it_abymmrdt.
    APPEND it_ammr.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abymmrdt FROM TABLE it_abymmrdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_abymmrdt FROM TABLE IT_abymmrdt ACCEPTING DUPLICATE KEYS.
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
      t_abymmrdt-zzret = 'S'.
      MODIFY t_abymmrdt TRANSPORTING zzret
                               WHERE mtno GE space
                               AND   plnt GE space.
    ELSE.
      ROLLBACK WORK.
      t_abymmrdt-zzret = 'E'.
      MODIFY t_abymmrdt TRANSPORTING zzret
                               WHERE mtno GE space
                               AND   plnt GE space.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    t_abymmrdt-zzret = 'E'.
    MODIFY t_abymmrdt TRANSPORTING zzret
                             WHERE mtno GE space
                             AND   plnt GE space.
  ENDIF.




ENDFUNCTION.
