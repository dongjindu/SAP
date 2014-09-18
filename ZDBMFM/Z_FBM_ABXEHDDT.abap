FUNCTION z_fbm_abxehddt .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXEHDDT STRUCTURE  ZSBM_ABXEHDDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxehddt TYPE ztbm_abxehddt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxehddt.
    MOVE-CORRESPONDING t_abxehddt TO it_abxehddt.
    APPEND it_abxehddt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxehddt FROM TABLE it_abxehddt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXEHDDT FROM TABLE IT_ABXEHDDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxehddt-zzret = 'S'.
    MODIFY t_abxehddt TRANSPORTING zzret
                             WHERE eono GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxehddt-zzret = 'E'.
    MODIFY t_abxehddt TRANSPORTING zzret
                             WHERE eono GE space.
  ENDIF.

ENDFUNCTION.
