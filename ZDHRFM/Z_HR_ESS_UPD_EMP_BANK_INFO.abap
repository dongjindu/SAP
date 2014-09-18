FUNCTION z_hr_ess_upd_emp_bank_info.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"     VALUE(DELETE_ADDTIONAL) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      ZESS_EMP_BANK_DETAIL STRUCTURE  ZESS_EMP_BANK_DETAIL
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* {

  DATA return1 LIKE bapireturn1 OCCURS 0 WITH HEADER LINE.
  DATA $deleted.

  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
       EXPORTING
            number = employee_number
       IMPORTING
            return = return1.

  IF return1-type EQ 'E'.
    return-type = 'I'.
    return-message = 'Record is locked. Please try again later.'.
    APPEND return.
    EXIT.
  ENDIF.

  CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
       EXPORTING
            number = employee_number
       IMPORTING
            return = return1.
* }

  CLEAR *pa0009.

  DATA : $next_pay_period TYPE pabrp,
         $next_fpbeg TYPE fpbeg,
         $last_fpend TYPE fpend,
         $next_fpend TYPE fpend.

  READ TABLE zess_emp_bank_detail INDEX 1.

  IF sy-subrc NE 0.
    return-type = 'E'.
    return-message = 'No Data to Create'.
    APPEND return.
    EXIT.
  ENDIF.

  IF delete_addtional EQ true.
    IF zess_emp_bank_detail-bank_type_code EQ '0'.
      return-type = 'E'.
      return-message = 'Cannot Deliminate the Main Bank'.
      APPEND return.
      EXIT.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'HR_MX_LAST_NEXT_PAYROLL_PERIOD'
       EXPORTING
            pernr            = employee_number
       IMPORTING
            last_fpend       = $last_fpend
            next_pay_period  = $next_pay_period
            next_fpbeg       = $next_fpbeg
            next_fpend       = $next_fpend
       EXCEPTIONS
            no_rgdir_entries = 1
            it1_error        = 2
            t549a_error      = 3
            t549q_error      = 4
            OTHERS           = 5.

  IF sy-subrc <> 0.
    return-type = 'E'.
    return-message = 'Cannot Detect Next Pay Period'.
    APPEND return.
    EXIT.
  ENDIF.

  IF delete_addtional EQ space.

    if zess_emp_bank_detail-bank_type_code eq '1' or
        zess_emp_bank_detail-bank_type_code eq '9'.

      SELECT SINGLE * FROM pa0009 WHERE pernr EQ employee_number
                AND ( subty EQ '1' or subty EQ '9' )
                AND sprps EQ space
                AND endda EQ '99991231'.

      if sy-subrc eq 0.
        return-type = 'E'.
        return-message = 'Addtional Bank Account is already existed!'.
        APPEND return.
        EXIT.
      endif.

    endif.
  endif.

  if zess_emp_bank_detail-bank_type_code eq '1' or
     zess_emp_bank_detail-bank_type_code eq '9'.
    SELECT SINGLE * FROM pa0009 WHERE pernr EQ employee_number
              AND ( subty EQ '1' or subty EQ '9' )
              AND sprps EQ space
              AND endda EQ '99991231'.
  else.
    SELECT SINGLE * FROM pa0009 WHERE pernr EQ employee_number
              AND subty EQ zess_emp_bank_detail-bank_type_code
              AND sprps EQ space
              AND endda EQ '99991231'.
  endif.

  IF sy-subrc EQ 0.
     *pa0009 = pa0009.

*  In case new record is duplicated
    if zess_emp_bank_detail-bank_type_code eq '1' or
       zess_emp_bank_detail-bank_type_code eq '9'.
      DELETE FROM pa0009
      WHERE pernr EQ employee_number
                  AND ( subty EQ '1' or subty EQ '9' )
                  AND sprps EQ space
                  AND begda EQ $next_fpbeg
                  AND endda EQ '99991231'.
    else.
      DELETE FROM pa0009
      WHERE pernr EQ employee_number
                  AND subty EQ zess_emp_bank_detail-bank_type_code
                  AND sprps EQ space
                  AND begda EQ $next_fpbeg
                  AND endda EQ '99991231'.
    endif.

    IF sy-subrc EQ 0.
    ENDIF.

    UPDATE pa0009 SET endda = $last_fpend
                      aedtm = sy-datum
                      uname = sy-uname
    WHERE pernr EQ employee_number
                AND subty EQ *pa0009-subty
                AND sprps EQ space
                AND endda EQ '99991231'.

    IF delete_addtional eq space or delete_addtional eq 'U'.
       *pa0009-subty = zess_emp_bank_detail-bank_type_code.
       *pa0009-bankn = zess_emp_bank_detail-bank_account.
       *pa0009-zweck = zess_emp_bank_detail-purpose.
       *pa0009-banks = zess_emp_bank_detail-bank_country.
       *pa0009-bankl = zess_emp_bank_detail-bank_key.
       *pa0009-begda = $next_fpbeg.
       *pa0009-endda = '99991231'.
       *pa0009-bnksa = zess_emp_bank_detail-bank_type_code.
       *pa0009-zlsch = 'P'.
       *pa0009-bkont = zess_emp_bank_detail-bkont.

      IF *pa0009-subty EQ '0'.
         *pa0009-betrg = 0.
      ELSE.
         *pa0009-betrg = zess_emp_bank_detail-amount.
         *pa0009-anzhl = zess_emp_bank_detail-percentage.
      ENDIF.

       *pa0009-aedtm = sy-datum.
       *pa0009-uname = sy-uname.
       *pa0009-waers = 'USD'.

      INSERT pa0009 FROM *pa0009.
      IF sy-subrc NE 0.
        ROLLBACK WORK.
        return-type = 'E'.
        return-message = 'Error Occured when Insert'.
        APPEND return.
        EXIT.
      ENDIF.

    ENDIF.

    COMMIT WORK.

  ELSE.

     *pa0009-pernr = employee_number.
     *pa0009-subty = zess_emp_bank_detail-bank_type_code.

     *pa0009-bankn = zess_emp_bank_detail-bank_account.
     *pa0009-zweck = zess_emp_bank_detail-purpose.
     *pa0009-banks = zess_emp_bank_detail-bank_country.
     *pa0009-bankl = zess_emp_bank_detail-bank_key.
     *pa0009-begda = $next_fpbeg.
     *pa0009-endda = '99991231'.
     *pa0009-bnksa = zess_emp_bank_detail-bank_type_code.
     *pa0009-zlsch = 'P'.
     *pa0009-bkont = zess_emp_bank_detail-bkont.

    IF *pa0009-subty EQ '0'.
       *pa0009-betrg = 0.
    ELSE.
       *pa0009-betrg = zess_emp_bank_detail-amount.
       *pa0009-anzhl = zess_emp_bank_detail-percentage.
    ENDIF.

     *pa0009-aedtm = sy-datum.
     *pa0009-uname = sy-uname.
     *pa0009-waers = 'USD'.

    INSERT pa0009 FROM *pa0009.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      return-type = 'E'.
      return-message = 'Error Occured when Insert'.
      APPEND return.
      EXIT.
    ENDIF.
    COMMIT WORK.

  ENDIF.

  return-type = 'S'.
  return-message = 'Success!'.
  APPEND return.

ENDFUNCTION.
