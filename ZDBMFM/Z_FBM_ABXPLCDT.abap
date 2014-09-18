FUNCTION z_fbm_abxplcdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXPLCDT STRUCTURE  ZSBM_ABXPLCDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxplcdt TYPE ztbm_abxplcdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxplcdt.
    MOVE-CORRESPONDING t_abxplcdt TO it_abxplcdt.
    APPEND it_abxplcdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxplcdt FROM TABLE it_abxplcdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXPLCDT FROM TABLE IT_ABXPLCDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    t_abxplcdt-zzret = 'S'.
    MODIFY t_abxplcdt TRANSPORTING zzret
                             WHERE carx GE space
                             OR    plnt GE space
                             OR    shop GE space
                             OR    line GE space.
  ELSE.
    t_abxplcdt-zzret = 'E'.
    MODIFY t_abxplcdt TRANSPORTING zzret
                             WHERE carx GE space
                             OR    plnt GE space
                             OR    shop GE space
                             OR    line GE space.
  ENDIF.

ENDFUNCTION.
