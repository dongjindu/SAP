FUNCTION Z_HR_ESS_GET_EMP_EMAIL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_EMP_EMAIL STRUCTURE  ZESS_EMP_EMAIL
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  __CLS : ZESS_EMP_EMAIL, P0105.

  CALL FUNCTION 'HR_READ_INFOTYPE'
       EXPORTING
            PERNR         = EMPLOYEE_NUMBER
            INFTY         = '0105'
            BEGDA         = SY-DATUM
            ENDDA         = '99991231'
            BYPASS_BUFFER = 'X'
       TABLES
            INFTY_TAB     = P0105.

  IF SY-SUBRC NE 0.
    RETURN-TYPE = 'E'.
    RETURN-MESSAGE = 'Invalid Employee Number'.
    APPEND RETURN.
    EXIT.
  ENDIF.

  LOOP AT P0105.
    CHECK P0105-USRTY EQ '0010' OR P0105-USRTY EQ '0030'.
    SELECT SINGLE STEXT INTO ZESS_EMP_EMAIL-STEXT
    FROM T591S
    WHERE SPRSL = SY-LANGU
      AND INFTY = '0105'
      AND SUBTY EQ P0105-SUBTY.
    ZESS_EMP_EMAIL-EMAIL = P0105-USRID_LONG.
    ZESS_EMP_EMAIL-CODE = P0105-SUBTY.
    APPEND ZESS_EMP_EMAIL.
  ENDLOOP.

  RETURN-TYPE = 'S'.
  RETURN-MESSAGE = 'Success!'.
  APPEND RETURN.

ENDFUNCTION.
