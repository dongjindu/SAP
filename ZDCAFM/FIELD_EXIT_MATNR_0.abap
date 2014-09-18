FUNCTION FIELD_EXIT_MATNR_0.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
* Authored by BSBAE
* This program is for adjusting material number input.

output = input.

select single * from mara where matnr = input.
if sy-subrc ne 0.
  concatenate '0000000000' input into wa_matnr.
  select single * from mara where matnr = wa_matnr.
  if sy-subrc eq 0.
    output = wa_matnr.
  endif.
endif.





ENDFUNCTION.
