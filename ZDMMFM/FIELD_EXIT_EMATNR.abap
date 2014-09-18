FUNCTION field_exit_ematnr.
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

  SELECT SINGLE * FROM mara WHERE matnr = input.
  IF sy-subrc NE 0.
    CONCATENATE '0000000000' input INTO wa_matnr.
    SELECT SINGLE * FROM mara WHERE matnr = wa_matnr.
    IF sy-subrc EQ 0.
      output = wa_matnr.
    ENDIF.
  ENDIF.
ENDFUNCTION.
