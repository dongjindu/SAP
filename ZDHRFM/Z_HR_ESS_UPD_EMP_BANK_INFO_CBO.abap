FUNCTION z_hr_ess_upd_emp_bank_info_cbo.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"     VALUE(DELETE_ADDTIONAL) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      ZESS_EMP_BANK_DETAIL STRUCTURE  ZESS_EMP_BANK_DETAIL
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 05/19/2011 VALERIAN   UD1K951778  Validate Bank Key and Bank Account
*                                   should only use Numeric Characters
*-----------------------------------------------------------------------

* BEGIN OF UD1K951778
  DATA: l_numchar(10) TYPE c,
        l_cmpchar(20) TYPE c,
        l_err(1)      TYPE c,
        l_len         TYPE i.
* END OF UD1K951778

  READ TABLE zess_emp_bank_detail INDEX 1.

  IF sy-subrc NE 0.
    return-type = 'E'.
    return-message = 'No Data to Create'.
    APPEND return.
    EXIT.
  ENDIF.

* BEGIN OF UD1K951778
* Validate Numeric value for Bank Key and Bank Acct. No.
  CLEAR l_err.
  l_numchar = '1234567890'.

  LOOP AT zess_emp_bank_detail.
    SHIFT zess_emp_bank_detail-bank_key LEFT DELETING LEADING space.
    l_cmpchar = zess_emp_bank_detail-bank_key.
    l_len = strlen( l_cmpchar ).

    IF l_cmpchar(l_len) CN l_numchar.
      return-type = 'E'.
      return-message = 'Bank Key should only use Numeric Characters'.
      APPEND return.
      l_err = 'X'.
      EXIT.
    ENDIF.

    SHIFT zess_emp_bank_detail-bank_account LEFT DELETING LEADING space.
    l_cmpchar = zess_emp_bank_detail-bank_account.
    l_len = strlen( l_cmpchar ).

    IF l_cmpchar(l_len) CN l_numchar.
      return-type = 'E'.
      return-message = 'Bank Acct. should only use Numeric Characters'.
      APPEND return.
      l_err = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK l_err IS INITIAL.
* END OF UD1K951778

  IF delete_addtional EQ 'C'.

* delete request. {
    IF zess_emp_bank_detail-percentage > '0'.
      DELETE FROM zess_bank_if_req WHERE pernr EQ employee_number
         AND bank_account EQ zess_emp_bank_detail-bank_account
         AND percentage EQ zess_emp_bank_detail-percentage
         AND bank_type_code EQ zess_emp_bank_detail-bank_type_code
         AND status NE 'C'.
    ELSE.
      DELETE FROM zess_bank_if_req WHERE pernr EQ employee_number
         AND bank_account EQ zess_emp_bank_detail-bank_account
         AND amount EQ zess_emp_bank_detail-amount
         AND bank_type_code EQ zess_emp_bank_detail-bank_type_code
         AND status NE 'C'.
    ENDIF.
    IF sy-subrc EQ 0.
      COMMIT WORK.

      return-type = 'S'.
      return-message = 'Request has been deleted.'.
      APPEND return.

      EXIT.
    ENDIF.

* }
  ENDIF.

  IF delete_addtional EQ 'X'.
    IF zess_emp_bank_detail-bank_type_code EQ '0'.
      return-type = 'E'.
      return-message = 'Cannot Delete the Main Bank'.
      APPEND return.
      EXIT.
    ENDIF.
  ENDIF.

  CLEAR zess_bank_if_req.

  IF zess_emp_bank_detail-percentage > '0'.
    DELETE FROM zess_bank_if_req WHERE pernr EQ employee_number
       AND bank_account EQ zess_emp_bank_detail-bank_account
       AND percentage EQ zess_emp_bank_detail-percentage
       AND bank_type_code EQ zess_emp_bank_detail-bank_type_code
       AND status NE 'C'.
  ELSE.
    DELETE FROM zess_bank_if_req WHERE pernr EQ employee_number
       AND bank_account EQ zess_emp_bank_detail-bank_account
       AND amount EQ zess_emp_bank_detail-amount
       AND bank_type_code EQ zess_emp_bank_detail-bank_type_code
       AND status NE 'C'.
  ENDIF.

  COMMIT WORK.

  MOVE-CORRESPONDING zess_emp_bank_detail TO zess_bank_if_req.
  zess_bank_if_req-bank_country = 'US'.
  zess_bank_if_req-delete_addtional = delete_addtional.
  zess_bank_if_req-pernr   = employee_number.
  zess_bank_if_req-reqdate = sy-datum.

  INSERT zess_bank_if_req.

  IF sy-subrc NE 0.
     *zess_bank_if_req = zess_bank_if_req.
    DELETE FROM zess_bank_if_req
    WHERE reqdate = zess_bank_if_req-reqdate
      AND pernr = zess_bank_if_req-pernr
      AND bank_type_code = zess_bank_if_req-bank_type_code
      AND delete_addtional = zess_bank_if_req-delete_addtional
      AND status = zess_bank_if_req-status.
    INSERT zess_bank_if_req.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      return-type = 'E'.
      return-message = 'Error Occured when Insert.'.
      APPEND return.
      EXIT.
    ENDIF.
  ENDIF.

  COMMIT WORK.
  return-type = 'S'.
  return-message = 'Success!'.
  APPEND return.

ENDFUNCTION.
