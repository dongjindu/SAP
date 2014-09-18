FUNCTION z_fbm_abxplidt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXPLIDT STRUCTURE  ZSBM_ABXPLIDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxplidt TYPE ztbm_abxplidt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxplidt.
    MOVE-CORRESPONDING t_abxplidt TO it_abxplidt.
    APPEND it_abxplidt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxplidt FROM TABLE it_abxplidt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXPLIDT FROM TABLE IT_ABXPLIDT
*    ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey on 06/22/2004.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxplidt-zzret = 'S'.
    MODIFY t_abxplidt TRANSPORTING zzret
                             WHERE carx GE space
                             OR    plnt GE space
                             OR    shop GE space
                             OR    line GE space
                             OR    gubn GE space
                             OR    hpcc GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxplidt-zzret = 'E'.
    MODIFY t_abxplidt TRANSPORTING zzret
                             WHERE carx GE space
                             OR    plnt GE space
                             OR    shop GE space
                             OR    line GE space
                             OR    gubn GE space
                             OR    hpcc GE space.
  ENDIF.

ENDFUNCTION.
