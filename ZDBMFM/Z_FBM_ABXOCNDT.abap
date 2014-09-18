FUNCTION z_fbm_abxocndt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXOCNDT STRUCTURE  ZSBM_ABXOCNDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxocndt TYPE ztbm_abxocndt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxocndt.
    MOVE-CORRESPONDING t_abxocndt TO it_abxocndt.
    APPEND it_abxocndt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxocndt FROM TABLE it_abxocndt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXOCNDT FROM TABLE IT_ABXOCNDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.

**>>> Added By Tonkey on 06/22/2004.
    MODIFY ztbm_ocn FROM TABLE it_abxocndt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*    INSERT ztbm_ocn FROM TABLE it_abxocndt ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

    IF sy-subrc EQ 0.
      COMMIT WORK.
      t_abxocndt-zzret = 'S'.
      MODIFY t_abxocndt TRANSPORTING zzret
                               WHERE zyear GE space
                               AND   natn  GE space
                               AND   base  GE space
                               AND   ocno  GE space
                               AND   vers  GE space.
    ELSE.
      ROLLBACK WORK.
      t_abxocndt-zzret = 'E'.
      MODIFY t_abxocndt TRANSPORTING zzret
                               WHERE zyear GE space
                               AND   natn  GE space
                               AND   base  GE space
                               AND   ocno  GE space
                               AND   vers  GE space.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    t_abxocndt-zzret = 'E'.
    MODIFY t_abxocndt TRANSPORTING zzret
                             WHERE zyear GE space
                             AND   natn  GE space
                             AND   base  GE space
                             AND   ocno  GE space
                             AND   vers  GE space.
  ENDIF.
ENDFUNCTION.
