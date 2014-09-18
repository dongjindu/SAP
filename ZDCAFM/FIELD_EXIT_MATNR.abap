FUNCTION field_exit_matnr.
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
  mara-matnr = output.

*----- Add Range logic (2011/10/31 BY KDM)
  DATA: lv_strlen TYPE i. "String Length of Material Display.

  CLEAR: lv_strlen.
  lv_strlen = strlen( input ).

  IF lv_strlen = 8 AND input >= '20000000' AND input <= '29999999'.

    SELECT SINGLE * FROM mara WHERE matnr = input.
    IF sy-subrc NE 0.
      CONCATENATE '0000000000' input INTO wa_matnr.
      SELECT SINGLE * FROM mara WHERE matnr = wa_matnr.
      IF sy-subrc EQ 0.
        output = wa_matnr.
      ENDIF.
    ENDIF.
  ENDIF.

*----- Appended by BSBAE. 20040803
*----- Request for CO Module
  CASE sy-tcode.
*    WHEN 'CS01' OR 'CS02'.
*      DATA : lw_matnr LIKE mara-matnr.
*      FREE MEMORY ID 'ZMAT'.
*      CLEAR : lw_matnr.
*      lw_matnr = output.
*      EXPORT lw_matnr TO MEMORY ID 'ZMAT'.
*  SET PARAMETER ID 'ZMAT'  FIELD lw_matnr.
    WHEN 'MM02'.
      EXPORT mara-matnr TO MEMORY ID 'ZMATERIAL'.
  ENDCASE.
ENDFUNCTION.
