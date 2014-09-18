FUNCTION Z_FBM_ABYALCDT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABYALCDT STRUCTURE  ZSBM_ABYALCDT_RFC
*"----------------------------------------------------------------------

  DATA: it_abyalcdt TYPE ztbm_abyalcdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abyalcdt.
    MOVE-CORRESPONDING t_abyalcdt TO it_abyalcdt.
    APPEND it_abyalcdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abyalcdt FROM TABLE it_abyalcdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_abyalcdt FROM TABLE IT_abyalcdt ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey on 06/22/2004.

  IF sy-subrc EQ 0.
    t_abyalcdt-zzret = 'S'.
    MODIFY t_abyalcdt TRANSPORTING zzret
                             WHERE mtno GE space
                             OR    plnt GE space
                             OR    usag GE space
                             OR    altn GE space.
  ELSE.
    t_abyalcdt-zzret = 'E'.
    MODIFY t_abyalcdt TRANSPORTING zzret
                             WHERE mtno GE space
                             OR    plnt GE space
                             OR    usag GE space
                             OR    altn GE space.
  ENDIF.




ENDFUNCTION.
