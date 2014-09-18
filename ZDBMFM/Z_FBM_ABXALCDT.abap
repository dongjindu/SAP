FUNCTION z_fbm_abxalcdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXALCDT STRUCTURE  ZSBM_ABXALCDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxalcdt TYPE ztbm_abxalcdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxalcdt.
    MOVE-CORRESPONDING t_abxalcdt TO it_abxalcdt.
    APPEND it_abxalcdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxalcdt FROM TABLE it_abxalcdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXALCDT FROM TABLE IT_ABXALCDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey on 06/22/2004.

  IF sy-subrc EQ 0.
    t_abxalcdt-zzret = 'S'.
    MODIFY t_abxalcdt TRANSPORTING zzret
                             WHERE mtno GE space
                             OR    plnt GE space
                             OR    usag GE space
                             OR    altn GE space.
  ELSE.
    t_abxalcdt-zzret = 'E'.
    MODIFY t_abxalcdt TRANSPORTING zzret
                             WHERE mtno GE space
                             OR    plnt GE space
                             OR    usag GE space
                             OR    altn GE space.
  ENDIF.

ENDFUNCTION.
