FUNCTION field_exit_werks_d_a.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
  DATA : lw_werks LIKE marc-werks.

*----- Changed by Bsbae
*----- Changed on 2004.08.04
  CASE sy-tcode.
    WHEN 'CS01' OR 'CS02'.
      CLEAR : lw_werks.
      FREE MEMORY ID 'ZWRK'.
      output = input.
      lw_werks = output.
      EXPORT lw_werks TO MEMORY ID 'ZWRK'.
    WHEN 'MM02'.
      " This is for Field Exit MSTAE
      output = input.
      marc-werks = output.
      EXPORT marc-werks TO MEMORY ID 'ZWRK'.
  ENDCASE.
ENDFUNCTION.
