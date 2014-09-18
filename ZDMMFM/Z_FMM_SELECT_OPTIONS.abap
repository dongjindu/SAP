FUNCTION z_fmm_select_options.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_LOW) OPTIONAL
*"     VALUE(I_HIGH) OPTIONAL
*"  EXPORTING
*"     VALUE(E_LOW)
*"     VALUE(E_HIGH)
*"----------------------------------------------------------------------

*---
  IF i_low EQ space AND i_high EQ space.
    MOVE : '000000000000000000' TO e_low,
           'ZZZZZZZZZZZZZZZZZZ' TO e_high.
  ELSEIF i_low NE space AND i_high EQ space.
    MOVE : i_low                TO e_low,
           i_low                TO e_high.
  ELSEIF i_low EQ space AND i_high NE space.
    MOVE : '000000000000000000' TO e_low,
           i_high               TO e_high.
  ELSEIF i_low NE space AND i_high NE space.
    MOVE : i_low                TO e_low,
           i_high               TO e_high.
  ENDIF.

*---


ENDFUNCTION.
