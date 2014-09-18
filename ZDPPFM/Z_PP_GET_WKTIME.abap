FUNCTION Z_PP_GET_WKTIME.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(C_DATE) TYPE  D OPTIONAL
*"     VALUE(C_TIME) TYPE  T OPTIONAL
*"     REFERENCE(OPCODE) TYPE  C
*"     VALUE(WKTIME) TYPE  I OPTIONAL
*"     REFERENCE(WERKS) LIKE  T001W-WERKS
*"     REFERENCE(ARBPL) LIKE  CRHD-ARBPL
*"  EXPORTING
*"     REFERENCE(T_DATE) TYPE  D
*"     REFERENCE(T_TIME) TYPE  T
*"  EXCEPTIONS
*"      ERROR_OPERATION
*"      CANNOT_READ_WORKING_TIME
*"----------------------------------------------------------------------

  CLEAR: IT_WORKTIME, IT_WORKTIME[].

  CLEAR: V_INPUT_DATE, V_INPUT_TIME, V_WKTIME, V_WERKS, V_SHOP,
         V_OUT_DATE, V_OUT_TIME.

  MOVE: C_DATE TO V_INPUT_DATE,
        C_TIME TO V_INPUT_TIME,
        WKTIME TO V_WKTIME,
        WERKS  TO V_WERKS,
        ARBPL  TO V_SHOP.

  concatenate v_input_date v_input_time into v_basetime.

  CASE OPCODE.
    WHEN '+'.
      PERFORM GET_FURTURE_TIME.
    WHEN '-'.
      PERFORM GET_PREVIOUS_TIME.
    WHEN OTHERS.
      RAISE ERROR_OPERATION.
  ENDCASE.

  MOVE: V_OUT_DATE TO T_DATE,
        V_OUT_TIME TO T_TIME.

ENDFUNCTION.
