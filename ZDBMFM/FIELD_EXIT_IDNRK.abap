FUNCTION field_exit_idnrk.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
*Request by CH Hong : 04/26/2004
*Update sortstring in BOM Structure: changed by wskim

  CHECK sy-tcode EQ 'CS01' OR sy-tcode EQ 'CS02'.
*  FREE MEMORY ID 'ZIDN'.
  output = input.
  lw_idnrk = output.
  EXPORT lw_idnrk TO MEMORY ID 'ZIDN'.

ENDFUNCTION.
