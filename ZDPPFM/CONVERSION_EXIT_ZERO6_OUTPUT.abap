FUNCTION CONVERSION_EXIT_ZERO6_OUTPUT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(INPUT)
*"       EXPORTING
*"             VALUE(OUTPUT)
*"----------------------------------------------------------------------

  data: n(6) type n.
  n = input.
  OUTPUT = n.

ENDFUNCTION.
