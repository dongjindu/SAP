FUNCTION Z_FPP_HMA_CHK_TESTCAR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_WO_SERIAL) TYPE  ZWOSER
*"  EXPORTING
*"     REFERENCE(E_TYPE) TYPE  C
*"----------------------------------------------------------------------

  CASE I_WO_SERIAL+5(2).
    WHEN 'X7'.
      MOVE: 'BIW' TO E_TYPE.
    WHEN 'X8'.
      MOVE: 'BIP' TO E_TYPE.
    WHEN 'X9'.
      MOVE: 'BIC' TO E_TYPE.
    WHEN OTHERS.
      CLEAR: E_TYPE.
  ENDCASE.


ENDFUNCTION.
