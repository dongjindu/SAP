FUNCTION Z_HR_ESS_GET_ORG_DATA_WS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_WS STRUCTURE  ZESS_EMP_WS
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  __cls P0007.

  CALL FUNCTION 'HR_READ_INFOTYPE'
       EXPORTING
            PERNR     = EMPLOYEE_NUMBER
            INFTY     = '0007'
            BEGDA     = SY-DATUM
            ENDDA     = '99991231'
            BYPASS_BUFFER = 'X'
       TABLES
            INFTY_TAB = P0007.

  IF SY-SUBRC NE 0.
    RETURN-TYPE = 'E'.
    RETURN-MESSAGE = 'Invalid Employee Number'.
    APPEND RETURN.
    EXIT.
  ENDIF.

  LOOP AT P0007.
    ZESS_WS-WORK_SCH_RULE = P0007-SCHKZ.
    SELECT SINGLE RTEXT INTO  ZESS_WS-WORK_SCH_TEXT
    FROM T508S WHERE
    SCHKZ = P0007-SCHKZ
    AND SPRSL = 'E'.

    ZESS_WS-TIME_MGMT_STAT = P0007-ZTERF.

    select single ztext into     ZESS_WS-TIME_MGMT_STAT_DESC
    from t555v where SPRSL eq sy-langu
                 and ZTERF eq P0007-ZTERF.
    ZESS_WS-PART_TIME_EMP = P0007-TEILK.

    APPEND ZESS_WS.
  ENDLOOP.

  RETURN-TYPE = 'S'.
  RETURN-MESSAGE = 'Success!'.
  APPEND RETURN.

  ENDFUNCTION.
