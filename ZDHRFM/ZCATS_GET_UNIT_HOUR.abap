FUNCTION ZCATS_GET_UNIT_HOUR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             REFERENCE(EXT_CALL)
*"       EXPORTING
*"             REFERENCE(UNIT) TYPE  MSEHI
*"----------------------------------------------------------------------

  PERFORM get_unit_hour USING ext_call.

  unit = unit_of_hour.

ENDFUNCTION.
