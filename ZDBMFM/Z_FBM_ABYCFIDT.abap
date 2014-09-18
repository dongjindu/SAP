FUNCTION Z_FBM_ABYCFIDT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABYCFIDT STRUCTURE  ZSBM_ABYCFIDT_RFC
*"----------------------------------------------------------------------

  DATA: it_abycfidt TYPE ztbm_abycfidt OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abycfidt.
    MOVE-CORRESPONDING t_abycfidt TO it_abycfidt.
    APPEND it_abycfidt.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abycfidt FROM TABLE it_abycfidt.
**<<< Added By Tonkey on 06/22/2004.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_abycfidt FROM TABLE IT_abycfidt ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abycfidt-zzret = 'S'.
    MODIFY t_abycfidt TRANSPORTING zzret
                             WHERE mtno GE space
                             OR    plnt GE space
                             OR    clid GE space.
  ELSE.
    ROLLBACK WORK.
    t_abycfidt-zzret = 'E'.
    MODIFY t_abycfidt TRANSPORTING zzret
                             WHERE mtno GE space
                             OR    plnt GE space
                             OR    clid GE space.
  ENDIF.





ENDFUNCTION.
