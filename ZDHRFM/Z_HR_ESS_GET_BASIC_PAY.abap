FUNCTION Z_HR_ESS_GET_BASIC_PAY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_EMP_BASIC_PAY STRUCTURE  ZESS_EMP_BASIC_PAY
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  DATA $PERSK LIKE PA0001-PERSK.

  SELECT SINGLE PERSK INTO $PERSK
  FROM PA0001 WHERE PERNR EQ EMPLOYEE_NUMBER
                AND ENDDA EQ '99991231'.

  IF SY-SUBRC NE 0.
    RETURN-TYPE = 'E'.
    RETURN-MESSAGE = 'Invalid Emp Number.'.
    APPEND RETURN.
  ENDIF.

  SELECT BEGDA ENDDA TRFAR TRFGB TRFGR TRFST BET01 ANSAL
  INTO CORRESPONDING FIELDS OF TABLE
  ZESS_EMP_BASIC_PAY FROM PA0008 WHERE PERNR EQ EMPLOYEE_NUMBER
  order by begda descending.

  IF SY-SUBRC EQ 0.
    RETURN-TYPE = 'S'.
    RETURN-MESSAGE = 'Success!'.
    APPEND RETURN.
  ELSE.
    RETURN-TYPE = 'E'.
    RETURN-MESSAGE = 'Invalid Emp Number.'.
    APPEND RETURN.
  ENDIF.

  LOOP AT ZESS_EMP_BASIC_PAY.
    ZESS_EMP_BASIC_PAY-PERSK = $PERSK.

    if ZESS_EMP_BASIC_PAY-bet01 is initial.
      select single betrg into  ZESS_EMP_BASIC_PAY-bet01
                      from t510 where MOLGA eq '10'
                                  and TRFAR eq ZESS_EMP_BASIC_PAY-TRFAR
                                  and TRFGB eq ZESS_EMP_BASIC_PAY-TRFGB
                                  and TRFKZ eq '1'
                                  and TRFGR eq ZESS_EMP_BASIC_PAY-TRFGR
                                  and TRFST eq ZESS_EMP_BASIC_PAY-TRFST
                                  and begda <= ZESS_EMP_BASIC_PAY-begda
                                  and endda >= ZESS_EMP_BASIC_PAY-begda.
    endif.

    MODIFY ZESS_EMP_BASIC_PAY INDEX SY-TABIX TRANSPORTING PERSK bet01.
  ENDLOOP.

ENDFUNCTION.
