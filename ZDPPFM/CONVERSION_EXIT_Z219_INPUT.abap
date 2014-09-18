FUNCTION CONVERSION_EXIT_Z219_INPUT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"----------------------------------------------------------------------


 CALL 'CONVERSION_EXIT_ALPHA_OUTPUT' ID 'INPUT'  FIELD INPUT
                                      ID 'OUTPUT' FIELD OUTPUT.




ENDFUNCTION.
