FUNCTION z_fbm_abxplbdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXPLBDT STRUCTURE  ZSBM_ABXPLBDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxplbdt TYPE ztbm_abxplbdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxplbdt.
    MOVE-CORRESPONDING t_abxplbdt TO it_abxplbdt.
    APPEND it_abxplbdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxplbdt FROM TABLE it_abxplbdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXPLBDT FROM TABLE IT_ABXPLBDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxplbdt-zzret = 'S'.
    MODIFY t_abxplbdt TRANSPORTING zzret
                             WHERE plnt GE space
                             OR    shop GE space
                             OR    line GE space
                             OR    gong GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxplbdt-zzret = 'E'.
    MODIFY t_abxplbdt TRANSPORTING zzret
                             WHERE plnt GE space
                             OR    shop GE space
                             OR    line GE space
                             OR    gong GE space.
  ENDIF.

ENDFUNCTION.
