FUNCTION Z_CO_GET_MI_CODE_SINGLE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(P_FSC) TYPE  MATNR
*"  EXPORTING
*"     REFERENCE(P_MI) TYPE  MATNR
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------

  DATA $CODE(1).

  IF P_FSC+13(1) EQ SPACE.    " old
    P_MI = P_FSC+6(8).
  ELSE.
    $CODE = P_FSC+5(2).     " eBOM

    SELECT SINGLE VALU INTO $CODE FROM ZTBM_ABXOPVDT
                   WHERE CARX EQ P_FSC+5(2)
                     AND CLNO EQ '002'.
    IF $CODE EQ SPACE.
      CLEAR P_MI.
      RAISE NOT_FOUND.
    ELSE.
      CONCATENATE $CODE P_FSC+7(7) INTO P_MI.
    ENDIF.
  ENDIF.

ENDFUNCTION.
