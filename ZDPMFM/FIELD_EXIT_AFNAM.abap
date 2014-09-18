FUNCTION FIELD_EXIT_AFNAM.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------

  DATA : W_INGPR LIKE CAUFVD-INGPR.

  GET PARAMETER ID 'IHG' FIELD W_INGPR.

  IF INPUT <> W_INGPR.
    MOVE : W_INGPR TO OUTPUT .
  ENDIF.

ENDFUNCTION.
