FUNCTION z_fbm_abxpcldt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXPCLDT STRUCTURE  ZSBM_ABXPCLDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxpcldt TYPE ztbm_abxpcldt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxpcldt.
    MOVE-CORRESPONDING t_abxpcldt TO it_abxpcldt.
    APPEND it_abxpcldt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxpcldt FROM TABLE it_abxpcldt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXPCLDT FROM TABLE IT_ABXPCLDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxpcldt-zzret = 'S'.
    MODIFY t_abxpcldt TRANSPORTING zzret
                             WHERE carx GE space
                             OR    gubn GE space
                             OR    hpcc GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxpcldt-zzret = 'E'.
    MODIFY t_abxpcldt TRANSPORTING zzret
                             WHERE carx GE space
                             OR    gubn GE space
                             OR    hpcc GE space.
  ENDIF.





ENDFUNCTION.
