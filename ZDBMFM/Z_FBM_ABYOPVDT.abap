FUNCTION Z_FBM_abyopvdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABYOPVDT STRUCTURE  ZSBM_ABYOPVDT_RFC
*"----------------------------------------------------------------------

  DATA: it_abyopvdt TYPE ztbm_abxopvdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abyopvdt.
    MOVE-CORRESPONDING t_abyopvdt TO it_abyopvdt.
    APPEND it_abyopvdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxopvdt FROM TABLE it_abyopvdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_abyopvdt FROM TABLE IT_abyopvdt ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

*  IF SY-SUBRC EQ 0.
*   INSERT ZTBM_OPVDT   FROM TABLE IT_abyopvdt ACCEPTING DUPLICATE KEYS.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abyopvdt-zzret = 'S'.
    MODIFY t_abyopvdt TRANSPORTING zzret
                             WHERE carx GE space
                             AND   clno GE space
                             AND   valu GE space.
  ELSE.
    ROLLBACK WORK.
    t_abyopvdt-zzret = 'E'.
    MODIFY t_abyopvdt TRANSPORTING zzret
                             WHERE carx GE space
                             AND   clno GE space
                             AND   valu GE space.
  ENDIF.
*  ELSE.
*    ROLLBACK WORK.
*    T_abyopvdt-ZZRET = 'E'.
*    MODIFY T_abyopvdt TRANSPORTING ZZRET
*                               WHERE CARX GE SPACE
*                               AND   CLNO GE SPACE
*                               AND   VALU GE SPACE.
*  ENDIF.




ENDFUNCTION.
