FUNCTION Z_FFI_GET_BUKRS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS DEFAULT 'H201'
*"  EXPORTING
*"     REFERENCE(BUTXT) TYPE  BUTXT
*"----------------------------------------------------------------------


select single butxt into butxt from t001 where bukrs = bukrs.


ENDFUNCTION.
