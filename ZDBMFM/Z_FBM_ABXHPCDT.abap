FUNCTION z_fbm_abxhpcdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXHPCDT STRUCTURE  ZSBM_ABXHPCDT_RFC
*"      T_ABXHPCDT01 STRUCTURE  ZSBM_ABXHPCDT01_RFC
*"----------------------------------------------------------------------
  DATA: it_abxhpcdt TYPE ztbm_abxhpcdt OCCURS 0 WITH HEADER LINE.
  DATA: it_abxhpcdt01 TYPE ztbm_abxhpcdt01 OCCURS 0 WITH HEADER LINE.

  LOOP AT t_abxhpcdt.
    MOVE-CORRESPONDING t_abxhpcdt TO it_abxhpcdt.
    APPEND it_abxhpcdt.
  ENDLOOP.

  LOOP AT t_abxhpcdt01.
    MOVE-CORRESPONDING t_abxhpcdt01 TO it_abxhpcdt01.
    APPEND it_abxhpcdt01.
  ENDLOOP.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxhpcdt FROM TABLE it_abxhpcdt.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ZTBM_ABXHPCDT FROM TABLE IT_ABXHPCDT ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxhpcdt-zzret = 'S'.
    MODIFY t_abxhpcdt TRANSPORTING zzret
                             WHERE zwork GE space
                             AND   pqbg GE space
                             AND   cext GE space
                             AND   cint GE space
                             AND   sequ GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxhpcdt-zzret = 'E'.
    MODIFY t_abxhpcdt TRANSPORTING zzret
                             WHERE zwork GE space
                             AND   pqbg GE space
                             AND   cext GE space
                             AND   cint GE space
                             AND   sequ GE space.
  ENDIF.

**>>> Added By Tonkey on 06/22/2004.
  MODIFY ztbm_abxhpcdt01 FROM TABLE it_abxhpcdt01.
**<<< Added By Tonkey.

**>>> Inactivated By Tonkey on 06/22/2004.
*  INSERT ztbm_abxhpcdt01 FROM TABLE it_abxhpcdt01
*                               ACCEPTING DUPLICATE KEYS.
**<<< Inactivated By Tonkey.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxhpcdt-zzret = 'S'.
    MODIFY t_abxhpcdt01 TRANSPORTING zzret
                             WHERE zwork GE space
                             AND   pqbg GE space
                             AND   cext GE space
                             AND   cint GE space
                             AND   sequ GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxhpcdt-zzret = 'E'.
    MODIFY t_abxhpcdt01 TRANSPORTING zzret
                             WHERE zwork GE space
                             AND   pqbg GE space
                             AND   cext GE space
                             AND   cint GE space
                             AND   sequ GE space.
  ENDIF.

ENDFUNCTION.
