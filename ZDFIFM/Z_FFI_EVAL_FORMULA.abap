FUNCTION Z_FFI_EVAL_FORMULA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_BASA1) LIKE  GLFUNCT-HSL01
*"     REFERENCE(I_BASA2) LIKE  GLFUNCT-HSL01
*"     REFERENCE(I_BASA3) LIKE  GLFUNCT-HSL01
*"     REFERENCE(I_COMA1) LIKE  GLFUNCT-HSL01
*"     REFERENCE(I_COMA2) LIKE  GLFUNCT-HSL01
*"     REFERENCE(I_COMA3) LIKE  GLFUNCT-HSL01
*"     REFERENCE(I_SIGN1) TYPE  C
*"     REFERENCE(I_SIGN2) TYPE  C
*"  EXPORTING
*"     REFERENCE(E_BASEA) LIKE  GLFUNCT-HSL01
*"     REFERENCE(E_COMPA) LIKE  GLFUNCT-HSL01
*"----------------------------------------------------------------------
  CASE I_SIGN1.
    WHEN '+'.
      E_BASEA = I_BASA1 + I_BASA2.
      E_COMPA = I_COMA1 + I_COMA2.
    WHEN '-'.
      E_BASEA = I_BASA1 - I_BASA2.
      E_COMPA = I_COMA1 - I_COMA2.
  ENDCASE.
*
  IF I_SIGN2 <> SPACE.
    CASE I_SIGN2.
      WHEN '+'.
        E_BASEA = E_BASEA + I_BASA3.
        E_COMPA = E_COMPA + I_COMA3.
      WHEN '-'.
        E_BASEA = E_BASEA - I_BASA3.
        E_COMPA = E_COMPA - I_COMA3.
    ENDCASE.
  ENDIF.
ENDFUNCTION.
