FUNCTION FIELD_EXIT_QMSTTXT_A.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------

  DATA: LW_STTXT TYPE QMSTTXT.

  MOVE: INPUT TO LW_STTXT.

  IF SY-UNAME = 'SLLEE'. BREAK-POINT. ENDIF.

  EXPORT LW_STTXT TO MEMORY ID 'Z_STATUS'.



ENDFUNCTION.
