FUNCTION z_fpp_compile_condition.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_COND) LIKE  ZSPP_CONDITION-VALS
*"     REFERENCE(I_CHECK) TYPE  ZFLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(O_SUCCESS) TYPE  ZFLAG
*"     REFERENCE(O_FAIL) TYPE  ZFLAG
*"  TABLES
*"      T_WORDER STRUCTURE  ZSPP_MASTER_ASUP
*"      T_COND STRUCTURE  ZSPP_CONDITION
*"  EXCEPTIONS
*"      CONDITION_ERROR
*"      ERR_PAREN
*"      ERR_OPERATION
*"      ERR_RELATION
*"      ERR_VALUES
*"      ERR_FIELDS
*"----------------------------------------------------------------------
  DATA L_LINE   TYPE I.

  IF i_check NE 'X' .
    DESCRIBE TABLE t_worder LINES l_line      .
    IF l_line = 0 .
      PERFORM get_worder_all  TABLES t_worder .
    ENDIF.
  ENDIF.


  CLEAR: it_cals, it_cals[].
  wa_condition = i_cond    .

  DO.
    wa_prior = 1 .
    IF wa_condition CS c_paren_r .
      wa_pos = sy-fdpos + 1 .
    ELSE.
      EXIT.
    ENDIF.
    PERFORM check_paren  USING wa_pos  wa_condition   wa_fin    .
    CASE wa_fin.
      WHEN 'X'.
        EXIT.
      WHEN 'E'.
        O_FAIL = 'X'    .
        RAISE err_paren            .
    ENDCASE.
  ENDDO.

  PERFORM swipped_data .

  LOOP AT wa_cals.
    CLEAR: wa_fin .
    wa_condition = wa_cals-string.
    wa_no = wa_no + 1 .
    wa_prior = wa_prior + 1.
    PERFORM check_operater  USING wa_condition   wa_fin   .
    IF wa_fin NE space. EXIT. ENDIF.
    it_cals-oper = wa_cals-oper.
    MODIFY it_cals TRANSPORTING oper WHERE no =  wa_no .
  ENDLOOP.

  CASE wa_fin.
    WHEN 'O' .
      O_FAIL = 'X'    .
      RAISE err_operation.
    WHEN 'R' .
      O_FAIL = 'X'    .
      RAISE err_relation .
    WHEN 'F' .
      O_FAIL = 'X'    .
      RAISE err_fields   .
    WHEN 'V' .
      O_FAIL = 'X'    .
      RAISE err_values   .
  ENDCASE.

  PERFORM check_vals  USING wa_fin.
  CASE wa_fin.
    WHEN 'O' .
      O_FAIL = 'X'    .
      RAISE err_operation.
    WHEN 'R' .
      O_FAIL = 'X'    .
      RAISE err_relation .
    WHEN 'F' .
      O_FAIL = 'X'    .
      RAISE err_fields   .
    WHEN 'V' .
      O_FAIL = 'X'    .
      RAISE err_values   .
  ENDCASE.

  O_SUCCESS = 'O'    .
  T_COND[]  = IT_CALS[].
  PERFORM calc_qty .
ENDFUNCTION.
