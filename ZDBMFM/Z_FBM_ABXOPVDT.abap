FUNCTION z_fbm_abxopvdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXOPVDT STRUCTURE  ZSBM_ABXOPVDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxopvdt TYPE ztbm_abxopvdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxopvdt.
    MOVE-CORRESPONDING t_abxopvdt TO it_abxopvdt.
    APPEND it_abxopvdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxopvdt FROM TABLE it_abxopvdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXOPVDT FROM TABLE IT_ABXOPVDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

*  IF SY-SUBRC EQ 0.
*   INSERT ZTBM_OPVDT   FROM TABLE IT_ABXOPVDT ACCEPTING DUPLICATE KEYS.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxopvdt-zzret = 'S'.
    MODIFY t_abxopvdt TRANSPORTING zzret
                             WHERE carx GE space
                             AND   clno GE space
                             AND   valu GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxopvdt-zzret = 'E'.
    MODIFY t_abxopvdt TRANSPORTING zzret
                             WHERE carx GE space
                             AND   clno GE space
                             AND   valu GE space.
  ENDIF.
*  ELSE.
*    ROLLBACK WORK.
*    T_ABXOPVDT-ZZRET = 'E'.
*    MODIFY T_ABXOPVDT TRANSPORTING ZZRET
*                               WHERE CARX GE SPACE
*                               AND   CLNO GE SPACE
*                               AND   VALU GE SPACE.
*  ENDIF.
ENDFUNCTION.
