FUNCTION Z_FBM_ABYLFPDT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABYLFPDT STRUCTURE  ZSBM_ABYLFPDT_RFC
*"----------------------------------------------------------------------

  DATA: it_abylfpdt TYPE ztbm_abylfpdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abylfpdt.
    MOVE-CORRESPONDING t_abylfpdt TO it_abylfpdt.
    APPEND it_abylfpdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abylfpdt FROM TABLE it_abylfpdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ztbm_abylfpdt FROM TABLE it_abylfpdt ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abylfpdt-zzret = 'S'.
    MODIFY t_abylfpdt TRANSPORTING zzret
                             WHERE mtno GE space
                             AND   plnt GE space
                             AND   vers GE space.
  ELSE.
    ROLLBACK WORK.
    t_abylfpdt-zzret = 'E'.
    MODIFY t_abylfpdt TRANSPORTING zzret
                             WHERE mtno GE space
                             AND   plnt GE space
                             AND   vers GE space.
  ENDIF.





ENDFUNCTION.
