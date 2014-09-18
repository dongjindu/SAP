FUNCTION CONVERSION_EXIT_Z219_OUTPUT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"----------------------------------------------------------------------


  CALL 'CONVERSION_EXIT_ALPHA_INPUT'  ID 'INPUT'  FIELD INPUT
                                      ID 'OUTPUT' FIELD OUTPUT.


ENDFUNCTION.
