FUNCTION Z_FBM_ABYCLSDT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABYCLSDT STRUCTURE  ZSBM_ABYCLSDT_RFC
*"----------------------------------------------------------------------

  DATA: it_abyclsdt TYPE ztbm_abyclsdt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abyclsdt.
    MOVE-CORRESPONDING t_abyclsdt TO it_abyclsdt.
    APPEND it_abyclsdt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abyclsdt FROM TABLE it_abyclsdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_abyclsdt FROM TABLE IT_abyclsdt ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abyclsdt-zzret = 'S'.
    MODIFY t_abyclsdt TRANSPORTING zzret
                             WHERE clid GE space
                             AND   clty GE space
                             AND   chid GE space
                             AND   chvl GE space.
  ELSE.
    ROLLBACK WORK.
    t_abyclsdt-zzret = 'E'.
    MODIFY t_abyclsdt TRANSPORTING zzret
                             WHERE clid GE space
                             AND   clty GE space
                             AND   chid EQ space
                             AND   chvl GE space.
  ENDIF.




ENDFUNCTION.
