FUNCTION Z_HR_ESS_UPD_TMAD_TICKET_CNT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"     VALUE(TICKET) LIKE  ZTHRTMAD-TICKET
*"  TABLES
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

  zthrtmad-pernr = EMPLOYEE_NUMBER.
  zthrtmad-TICKET = TICKET.
  zthrtmad-AENAM = sy-uname.
  zthrtmad-AEDAT = sy-datum.
  zthrtmad-AEZET = sy-uzeit.

  modify zthrtmad from zthrtmad.

  IF sy-subrc EQ 0.
    zthrtmad-ticket = TICKET.
  Else.

  endif.

  return-type = 'S'.
  return-message = 'Success!'.
  APPEND return.

ENDFUNCTION.
