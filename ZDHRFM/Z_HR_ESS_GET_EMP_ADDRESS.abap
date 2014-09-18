FUNCTION z_hr_ess_get_emp_address.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_EMP_ADDRESS STRUCTURE  ZESS_EMP_ADDRESS
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Log
* Date        Developer Issue No    Description
*======================================================================
* 08/01/2012  Valerian  UD1K955293  Use communication type "HOME" as
*                                   telephone number
* 08/01/2012 Valerian   UD1K955303  Extend ZESS_EMP_ADDRESS to store
*                                   Home Phone Number.
*----------------------------------------------------------------------

  __cls : zess_emp_address, p0006.

  SELECT SINGLE * FROM pa0000
  WHERE pernr EQ employee_number
    AND endda EQ '99991231'.

  IF sy-subrc EQ 0 AND
  ( pa0000-stat2 EQ '1' OR pa0000-stat2 EQ '3' ).
  ELSE.
    return-type = 'E'.
    return-message = 'Invalid Employee Number or Withdrawn'.
    APPEND return.
    EXIT.
  ENDIF.


  CALL FUNCTION 'HR_READ_INFOTYPE'
       EXPORTING
            pernr         = employee_number
            infty         = '0006'
            begda         = sy-datum
            endda         = '99991231'
            bypass_buffer = 'X'
       TABLES
            infty_tab     = p0006.

  IF sy-subrc NE 0.
    return-type = 'E'.
    return-message = 'Invalid Employee Number'.
    APPEND return.
    EXIT.
  ENDIF.

  LOOP AT p0006.

    zess_emp_address-anssa  = p0006-anssa.
    zess_emp_address-care_of  = p0006-name2.
    zess_emp_address-address1 = p0006-stras.
    zess_emp_address-address2 = p0006-locat.
    zess_emp_address-city     = p0006-ort01.
    zess_emp_address-county  = p0006-ort02.

    SELECT SINGLE landx INTO zess_emp_address-country FROM t005t
    WHERE spras EQ sy-langu
     AND land1 EQ p0006-land1.

    zess_emp_address-state    = p0006-state.

    SELECT SINGLE bezei INTO zess_emp_address-state_desc
    FROM t005u WHERE spras EQ 'E'
                AND land1 EQ 'US'
                AND bland EQ p0006-state.

    zess_emp_address-zip_code = p0006-pstlz.


*   zess_emp_address-telephone = p0006-telnr.               "UD1K955293

    IF p0006-com01 EQ 'CELL'.
      zess_emp_address-cell_num  = p0006-num01.
    ENDIF.
    IF p0006-com02 EQ 'CELL'.
      zess_emp_address-cell_num  = p0006-num02.
    ENDIF.
    IF p0006-com03 EQ 'CELL'.
      zess_emp_address-cell_num  = p0006-num03.
    ENDIF.
* BEGIN OF UD1K955293
    IF p0006-com04 EQ 'CELL'.
      zess_emp_address-cell_num  = p0006-num04.
    ENDIF.
* END OF UD1K955293

    IF p0006-com01 EQ 'WORK'.
      zess_emp_address-work_num  = p0006-num01.
    ENDIF.
    IF p0006-com02 EQ 'WORK'.
      zess_emp_address-work_num  = p0006-num02.
    ENDIF.
    IF p0006-com03 EQ 'WORK'.
      zess_emp_address-work_num  = p0006-num03.
    ENDIF.
* BEGIN OF UD1K955293
    IF p0006-com04 EQ 'WORK'.
      zess_emp_address-work_num  = p0006-num04.
    ENDIF.
* END OF UD1K955293

    IF p0006-com01 EQ 'WCEL'.
      zess_emp_address-wcel_num  = p0006-num01.
    ENDIF.
    IF p0006-com02 EQ 'WCEL'.
      zess_emp_address-wcel_num  = p0006-num02.
    ENDIF.
    IF p0006-com03 EQ 'WCEL'.
      zess_emp_address-wcel_num = p0006-num03.
    ENDIF.
* BEGIN OF UD1K955293
    IF p0006-com04 EQ 'WCEL'.
      zess_emp_address-wcel_num = p0006-num04.
    ENDIF.
* END OF UD1K955293

* BEGIN OF UD1K955293
* BEGIN OF UD1K955303
    IF p0006-com01 EQ 'HOME'.
      zess_emp_address-home_num = p0006-num01.
    ENDIF.
    IF p0006-com02 EQ 'HOME'.
      zess_emp_address-home_num = p0006-num02.
    ENDIF.
    IF p0006-com03 EQ 'HOME'.
      zess_emp_address-home_num = p0006-num03.
    ENDIF.
    IF p0006-com04 EQ 'HOME'.
      zess_emp_address-home_num = p0006-num04.
    ENDIF.

*   IF p0006-com01 EQ 'HOME'.
*     zess_emp_address-telephone = p0006-num01.
*   ENDIF.
*   IF p0006-com02 EQ 'HOME'.
*     zess_emp_address-telephone = p0006-num02.
*   ENDIF.
*   IF p0006-com03 EQ 'HOME'.
*     zess_emp_address-telephone = p0006-num03.
*   ENDIF.
*   IF p0006-com04 EQ 'HOME'.
*     zess_emp_address-telephone = p0006-num04.
*   ENDIF.
* END OF UD1K955303
* END OF UD1K955293

* BEGIN OF UD1K955303
    PERFORM filter_number CHANGING : zess_emp_address-home_num,
*   PERFORM filter_number CHANGING : zess_emp_address-telephone,
* END OF UD1K955303
                                     zess_emp_address-cell_num,
                                     zess_emp_address-work_num,
                                     zess_emp_address-wcel_num.
    APPEND zess_emp_address.
    CLEAR zess_emp_address.

  ENDLOOP.

  return-type = 'S'.
  return-message = 'Success!'.
  APPEND return.

ENDFUNCTION.
