FUNCTION z_fca_get_price_unit.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_AMOUNT) TYPE  F
*"  EXPORTING
*"     REFERENCE(E_AMOUNT) TYPE  F
*"     REFERENCE(E_PEINH) LIKE  EKPO-PEINH
*"----------------------------------------------------------------------
  DATA: wa_amt_dec(10)   TYPE p DECIMALS 4,
        wa_amt_chr(20),
        wa_amt_frac      TYPE p DECIMALS 4,
        wa_source_point  LIKE cffsd-rvalf.

  wa_amt_dec = frac( i_amount * 100 ).
  wa_amt_chr = wa_amt_dec.
  SHIFT wa_amt_chr RIGHT DELETING TRAILING space.
  SHIFT wa_amt_chr RIGHT DELETING TRAILING '0'.
  SHIFT wa_amt_chr LEFT  DELETING LEADING  space.
  e_peinh = 1.

  IF wa_amt_chr NE '0.'.
    DO.
      e_peinh = e_peinh * 10.
      wa_amt_frac = frac( wa_amt_chr * e_peinh ).
      IF wa_amt_frac = 0.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  e_amount = i_amount * e_peinh.
ENDFUNCTION.
