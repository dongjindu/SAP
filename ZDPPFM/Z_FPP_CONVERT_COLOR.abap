FUNCTION z_fpp_convert_color.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_MODEL) TYPE  CHAR03
*"     VALUE(I_YEAR) TYPE  CHAR01
*"     VALUE(I_GUBN) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_EXTC) TYPE  CHAR03
*"     REFERENCE(I_INTC) TYPE  CHAR03
*"  EXPORTING
*"     VALUE(E_EXTC) TYPE  CHAR03
*"     VALUE(E_INTC) TYPE  CHAR03
*"----------------------------------------------------------------------

  IF i_gubn = 'X'.    "3 digit -> 2digit (HAC->HMMA)

    IF i_extc IS NOT INITIAL.
      SELECT SINGLE ctrn_conf_colr INTO e_extc
      FROM ztbm_abycoldt
      WHERE ctrn_cars_c  =  i_model
        AND ctrn_year_c2 =  i_year
        AND ctrn_gubn_c  =  'EXT'
        AND ctrn_key_colr = i_extc.
    ENDIF.

    IF i_intc IS NOT INITIAL.
      SELECT SINGLE ctrn_conf_colr INTO e_intc
      FROM ztbm_abycoldt
      WHERE ctrn_cars_c  =  i_model
        AND ctrn_year_c2 =  i_year
        AND ctrn_gubn_c  =  'INT'
        AND ctrn_key_colr = i_intc.
    ENDIF.

  ELSE.                 "2 digit -> 3digit (HMMA->HAC)

    IF i_extc IS NOT INITIAL.
      SELECT SINGLE ctrn_key_colr INTO e_extc
      FROM ztbm_abycoldt
      WHERE ctrn_cars_c  =  i_model
        AND ctrn_year_c2 =  i_year
        AND ctrn_gubn_c  =  'EXT'
        AND ctrn_conf_colr = i_extc.
    ENDIF.
    IF i_intc IS NOT INITIAL.
      SELECT SINGLE ctrn_key_colr INTO e_intc
      FROM ztbm_abycoldt
      WHERE ctrn_cars_c  =  i_model
        AND ctrn_year_c2 =  i_year
        AND ctrn_gubn_c  =  'INT'
        AND ctrn_conf_colr = i_intc.
    ENDIF.

  ENDIF.

ENDFUNCTION.
