FUNCTION Z_FRF_MM_CHECK_REFNR .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_REFNR) LIKE  LTAK-REFNR
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SY-SUBRC
*"----------------------------------------------------------------------

  DATA: LS_REFNR LIKE LTAK-REFNR.
  SELECT SINGLE REFNR INTO LS_REFNR
    FROM LTAK
    WHERE REFNR = I_REFNR.

    E_SUBRC = SY-SUBRC.




ENDFUNCTION.
