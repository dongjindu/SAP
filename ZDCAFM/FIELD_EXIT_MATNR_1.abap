FUNCTION field_exit_matnr_1.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
  output = input.

  SELECT SINGLE * FROM mara WHERE matnr = input.
  IF sy-subrc NE 0.
    CONCATENATE '0000000000' input INTO wa_matnr.
    SELECT SINGLE * FROM mara WHERE matnr = wa_matnr.
    IF sy-subrc EQ 0.
      output = wa_matnr.
      get PARAMETER ID 'MAT' FIELD output.
    ENDIF.
  ENDIF.





ENDFUNCTION.
