FUNCTION FIELD_EXIT_INGRP.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------

  DATA : W_INGPR LIKE CAUFVD-INGPR.

  MOVE : INPUT TO W_INGPR.

  SET PARAMETER ID 'IHG' FIELD W_INGPR.




ENDFUNCTION.
