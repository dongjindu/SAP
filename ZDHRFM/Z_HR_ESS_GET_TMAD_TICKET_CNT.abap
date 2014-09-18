FUNCTION z_hr_ess_get_tmad_ticket_cnt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_EMP_TMAD_TICKET_CNT STRUCTURE  ZESS_EMP_TMAD_TICKET_CNT
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  SELECT SINGLE * FROM pa0000
  WHERE pernr EQ employee_number
    AND endda EQ '99991231'.

  IF sy-subrc EQ 0 OR pa0000-stat2 EQ '1'
          OR pa0000-stat2 EQ '3'.
  ELSE.
    return-type = 'E'.
    return-message = 'Invalid Employee Number or Withdrawn'.
    APPEND return.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM zthrtmad
  WHERE pernr EQ employee_number
  and AENAM eq 'RFCESS'.

  IF sy-subrc EQ 0.
    zess_emp_tmad_ticket_cnt-ticket = zthrtmad-ticket.
    write zthrtmad-aedat to zess_emp_tmad_ticket_cnt-rdate.
  Else.
    zess_emp_tmad_ticket_cnt-ticket = '0'.
    zess_emp_tmad_ticket_cnt-rdate = space.
  endif.

  APPEND zess_emp_tmad_ticket_cnt.

  return-type = 'S'.
  return-message = 'Success!'.
  APPEND return.

ENDFUNCTION.
