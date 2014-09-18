FUNCTION field_exit_stlan.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
  DATA: lw_stlan LIKE mast-stlan. "BOM Usage
  CHECK sy-tcode EQ 'CS01' OR sy-tcode EQ 'CS02'.
  CLEAR : lw_stlan .
  FREE MEMORY ID 'ZCSV'.
  lw_stlan  = output.
  EXPORT lw_stlan  TO MEMORY ID 'ZCSV'.
*  SET PARAMETER ID 'ZCSV'  FIELD lw_stlan.
ENDFUNCTION.
