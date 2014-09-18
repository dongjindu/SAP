FUNCTION z_fbm_abxcfidt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXCFIDT STRUCTURE  ZSBM_ABXCFIDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxcfidt TYPE ztbm_abxcfidt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxcfidt.
    MOVE-CORRESPONDING t_abxcfidt TO it_abxcfidt.
    APPEND it_abxcfidt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxcfidt FROM TABLE it_abxcfidt.
**<<< Added By Tonkey on 06/22/2004.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXCFIDT FROM TABLE IT_ABXCFIDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxcfidt-zzret = 'S'.
    MODIFY t_abxcfidt TRANSPORTING zzret
                             WHERE mtno GE space
                             OR    plnt GE space
                             OR    clid GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxcfidt-zzret = 'E'.
    MODIFY t_abxcfidt TRANSPORTING zzret
                             WHERE mtno GE space
                             OR    plnt GE space
                             OR    clid GE space.
  ENDIF.

ENDFUNCTION.
