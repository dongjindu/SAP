FUNCTION z_fbm_abxclsdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXCLSDT STRUCTURE  ZSBM_ABXCLSDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxclsdt TYPE ztbm_abxclsdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxclsdt.
    MOVE-CORRESPONDING t_abxclsdt TO it_abxclsdt.
    APPEND it_abxclsdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxclsdt FROM TABLE it_abxclsdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXCLSDT FROM TABLE IT_ABXCLSDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxclsdt-zzret = 'S'.
    MODIFY t_abxclsdt TRANSPORTING zzret
                             WHERE clid GE space
                             AND   clty GE space
                             AND   chid GE space
                             AND   chvl GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxclsdt-zzret = 'E'.
    MODIFY t_abxclsdt TRANSPORTING zzret
                             WHERE clid GE space
                             AND   clty GE space
                             AND   chid EQ space
                             AND   chvl GE space.
  ENDIF.

ENDFUNCTION.
