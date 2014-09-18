FUNCTION z_hr_ess_get_pay_date.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"     VALUE(YEAR) TYPE  PABRJ
*"     VALUE(MONTH) TYPE  PABRP
*"  TABLES
*"      ZESS_EMP_PAY_DATE STRUCTURE  ZESS_EMP_PAY_DATE
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
*-----------------------------------------------------------------------
* 02/18/2013 Valerian   UD1K956779  Include Off-Cycle Date in Date
*                                   selection
*-----------------------------------------------------------------------

  DATA pabrp TYPE pabrp.
  DATA $d_diff TYPE i.

  SELECT SINGLE * FROM pa0001 WHERE pernr EQ employee_number
                                AND endda EQ '99991231'.
  IF sy-subrc NE 0.
    return-type = 'E'.
    return-message = 'Invalid Employee Number'.
    APPEND return.
    EXIT.
  ENDIF.

* BEGIN OF UD1K956779
*  SELECT SINGLE * FROM T549A WHERE ABKRS EQ PA0001-ABKRS.
*
*  SELECT * FROM T549S WHERE MOLGA EQ '10'
*                               AND DATMO EQ '01'
*                               AND PERMO EQ T549A-PERMO
*                               AND DATID EQ '01'
*                               AND PABRJ EQ YEAR.
*
*    CHECK T549S-PDATE+4(2) EQ MONTH.
*
*    if T549S-PDATE >= sy-datum.
*
*      $d_diff = T549S-PDATE - sy-datum.
*      if $d_diff > 3.
*        continue.
*      endif.
*
*      select single PABRP into PABRP
*      from t569v where ABKRS eq PA0001-ABKRS
*                            and pabrj eq T549S-pabrj
*                            and pabrp eq T549S-PABRP.
*      if sy-subrc ne 0.
*        continue.
*      endif.
*
*    endif.
*
*    ZESS_EMP_PAY_DATE-PAYDATE = T549S-PDATE.
*    APPEND ZESS_EMP_PAY_DATE.
*
*  ENDSELECT.

  DATA: l_frstday TYPE sy-datum,
        l_lastday TYPE sy-datum,
        lt_return LIKE bapireturn1,
        lt_result LIKE bapi7004_rl OCCURS 0 WITH HEADER LINE.

  CONCATENATE year month '01' INTO l_frstday.
  CALL FUNCTION 'HR_IN_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = l_frstday
    IMPORTING
      last_day_of_month = l_lastday
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_GET_PAYROLL_RESULT_LIST'
      EXPORTING
        employeenumber = employee_number
        fromdate       = l_frstday
        todate         = l_lastday
      IMPORTING
        return         = lt_return
      TABLES
        results        = lt_result.

    IF NOT lt_result[] IS INITIAL.
      LOOP AT lt_result WHERE ocreason <> 'H003'
                          AND ocreason <> 'H070'.
        zess_emp_pay_date-paydate = lt_result-paydate.
        APPEND zess_emp_pay_date.
      ENDLOOP.

      SORT zess_emp_pay_date BY paydate.

      return-type = 'S'.
      return-message = 'Success!'.
      APPEND return.

    ELSE.
      return-type = lt_return-type.
      return-message = lt_return-message.
      APPEND return.
    ENDIF.
  ENDIF.

*  return-type = 'S'.
*  return-message = 'Success!'.
*  APPEND return.
* END OF UD1K956779
ENDFUNCTION.
