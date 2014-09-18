FUNCTION z_fbm_abxebmdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXEBMDT STRUCTURE  ZSBM_ABXEBMDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxebmdt TYPE ztbm_abxebmdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxebmdt.
    MOVE-CORRESPONDING t_abxebmdt TO it_abxebmdt.
    APPEND it_abxebmdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxebmdt FROM TABLE it_abxebmdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXEBMDT FROM TABLE IT_ABXEBMDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxebmdt-zzret = 'S'.
    MODIFY t_abxebmdt TRANSPORTING zzret WHERE mtno GE space
                                         AND   plnt GE space
                                         AND   usag GE space
                                         AND   altn GE space
                                         AND   pref GE space
                                         AND   comp GE space
                                         AND   suff GE space
                                         AND   sequ GE space
                                         AND   seqc GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxebmdt-zzret = 'E'.
    MODIFY t_abxebmdt TRANSPORTING zzret WHERE mtno GE space
                                         AND   plnt GE space
                                         AND   usag GE space
                                         AND   altn GE space
                                         AND   pref GE space
                                         AND   comp GE space
                                         AND   suff GE space
                                         AND   sequ GE space
                                         AND   seqc GE space.
  ENDIF.

ENDFUNCTION.
