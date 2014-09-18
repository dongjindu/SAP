FUNCTION Z_HR_ESS_GET_ORG_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_ORG STRUCTURE  ZESS_EMP_ORG
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  DATA $ORGEH LIKE P0001-ORGEH.

  __CLS P0001.

  CALL FUNCTION 'HR_READ_INFOTYPE'
       EXPORTING
            PERNR         = EMPLOYEE_NUMBER
            INFTY         = '0001'
            BEGDA         = SY-DATUM
            ENDDA         = '99991231'
            BYPASS_BUFFER = 'X'
       TABLES
            INFTY_TAB     = P0001.

  IF SY-SUBRC NE 0.
    RETURN-TYPE = 'E'.
    RETURN-MESSAGE = 'Invalid Employee Number'.
    APPEND RETURN.
    EXIT.
  ENDIF.

  $ORGEH = P0001-ORGEH.

  ZESS_ORG-SECTION = P0001-ORGEH.
  SELECT SINGLE STEXT INTO ZESS_ORG-SECTION_DESC FROM HRP1000
  WHERE PLVAR EQ '01'
    AND OTYPE EQ 'O'
    AND OBJID EQ P0001-ORGEH
    AND BEGDA <= SY-DATUM
    AND ENDDA >= SY-DATUM.

  IF ZESS_ORG-SECTION_DESC CP '*DEPARTMENT*'.
    HRP1000-OBJID = P0001-ORGEH.
    HRP1000-MC_STEXT = ZESS_ORG-SECTION_DESC.
  ELSE.

    DO 10 TIMES.

      SELECT SINGLE * FROM HRP1001 WHERE PLVAR EQ '01'
                    AND  OTYPE EQ 'O'
                    AND  ISTAT EQ '1'
                    AND  BEGDA <= SY-DATUM
                    AND  ENDDA >= SY-DATUM
                    AND  SUBTY EQ 'A002'
                    AND  SCLAS EQ 'O'
                    AND  OBJID EQ $ORGEH.

      SELECT SINGLE * FROM HRP1000 WHERE PLVAR EQ '01'
                    AND  OTYPE EQ 'O'
                    AND  ISTAT EQ '1'
                    AND  BEGDA <= SY-DATUM
                    AND  ENDDA >= SY-DATUM
                    AND  LANGU EQ SY-LANGU
                    AND  OBJID EQ HRP1001-SOBID.

      IF HRP1000-MC_STEXT CP '*DEPARTMENT*'.
        EXIT.
      ELSE.
        $ORGEH = HRP1000-OBJID.
      ENDIF.

    ENDDO.
  ENDIF.

  ZESS_ORG-DIVISION = P0001-BTRTL.

  SELECT SINGLE BTEXT INTO ZESS_ORG-DIVISION_DESC FROM T001P
  WHERE WERKS EQ P0001-WERKS
   AND BTRTL  EQ P0001-BTRTL.


  ZESS_ORG-DEPARTMENT = HRP1000-OBJID.
  ZESS_ORG-DEPARTMENT_DESC = HRP1000-STEXT.

  ZESS_ORG-COST_CENTER = P0001-KOSTL.
  ZESS_ORG-EE_GRP = P0001-PERSG.
  ZESS_ORG-EE_SUB_GRP = P0001-PERSK.
  ZESS_ORG-PAYROLL_AREA = P0001-ABKRS.

  ZESS_ORG-POSITION = P0001-PLANS.
  SELECT SINGLE STEXT INTO ZESS_ORG-POSITION_DESC FROM HRP1000
  WHERE PLVAR EQ '01'
    AND OTYPE EQ 'S'
    AND OBJID EQ P0001-PLANS
    AND BEGDA <= SY-DATUM
    AND ENDDA >= SY-DATUM.

  ZESS_ORG-JOB = P0001-STELL.
  SELECT SINGLE STEXT INTO ZESS_ORG-JOB_DESC FROM HRP1000
  WHERE PLVAR EQ '01'
    AND OTYPE EQ 'C'
    AND OBJID EQ P0001-STELL
    AND BEGDA <= SY-DATUM
    AND ENDDA >= SY-DATUM.

  SELECT SINGLE EXMPT INTO ZESS_ORG-EXEMPT
  FROM T5U13 WHERE STELL EQ P0001-STELL
               AND ENDDA EQ '99991231'.

  SELECT SINGLE * FROM T596U WHERE SPRKZ EQ SY-LANGU
                             AND   UMTYP EQ 'I2'.
  TRANSLATE ZESS_ORG-EXEMPT USING T596U-UMTAB.

  ZESS_ORG-TIME_ADMIN = P0001-SACHZ.

  SELECT SINGLE SACHN INTO ZESS_ORG-TIME_ADMIN_DESC FROM T526
  WHERE WERKS EQ 'HMMA'
    AND SACHX EQ P0001-SACHZ.

  SELECT SINGLE KTEXT INTO ZESS_ORG-COST_CENTER_TXT
  FROM CSKT
  WHERE SPRAS EQ SY-LANGU
   AND KOKRS EQ 'H201'
   AND KOSTL EQ ZESS_ORG-COST_CENTER
   AND DATBI GE SY-DATUM.

  SELECT SINGLE * FROM HRP1001
              WHERE  PLVAR EQ '01'
                AND  OTYPE EQ 'S'
                AND  ISTAT EQ '1'
                AND  BEGDA <= SY-DATUM
                AND  ENDDA >= SY-DATUM
                AND  SUBTY EQ 'A002'
                AND  SCLAS EQ 'S'
                AND  OBJID EQ P0001-PLANS.
  IF SY-SUBRC EQ 0.
    SELECT SINGLE * FROM HRP1001
                WHERE  PLVAR EQ '01'
                  AND  OTYPE EQ 'S'
                  AND  ISTAT EQ '1'
                  AND  BEGDA <= SY-DATUM
                  AND  ENDDA >= SY-DATUM
                  AND  SUBTY EQ 'A008'
                  AND  SCLAS EQ 'P'
                  AND  OBJID EQ HRP1001-SOBID.

    IF SY-SUBRC EQ 0.
      SELECT SINGLE ENAME INTO ZESS_ORG-SUPVSR_NAME
          FROM PA0001
           WHERE PERNR EQ HRP1001-SOBID
             AND ENDDA EQ '99991231'.
    ENDIF.

  ELSE.

    $ORGEH = P0001-ORGEH.

    DO 10 TIMES.
      SELECT SINGLE * FROM HRP1001
                  WHERE  PLVAR EQ '01'
                    AND  OTYPE EQ 'O'
                    AND  ISTAT EQ '1'
                    AND  BEGDA <= SY-DATUM
                    AND  ENDDA >= SY-DATUM
                    AND  SUBTY EQ 'B012'
                    AND  SCLAS EQ 'S'
                    AND  OBJID EQ $ORGEH.

      IF SY-SUBRC EQ 0.

        SELECT SINGLE * FROM HRP1001
                    WHERE  PLVAR EQ '01'
                      AND  OTYPE EQ 'S'
                      AND  ISTAT EQ '1'
                      AND  BEGDA <= SY-DATUM
                      AND  ENDDA >= SY-DATUM
                      AND  SUBTY EQ 'A008'
                      AND  SCLAS EQ 'P'
                      AND  OBJID EQ HRP1001-SOBID.

        IF SY-SUBRC EQ 0.

          if hrp1001-sobid eq EMPLOYEE_NUMBER.

            SELECT SINGLE * FROM HRP1001
                        WHERE  PLVAR EQ '01'
                          AND  OTYPE EQ 'O'
                          AND  ISTAT EQ '1'
                          AND  BEGDA <= SY-DATUM
                          AND  ENDDA >= SY-DATUM
                          AND  SUBTY EQ 'A002'
                          AND  SCLAS EQ 'O'
                          AND  OBJID EQ $ORGEH.
            IF SY-SUBRC EQ 0.
              $ORGEH = HRP1001-SOBID.
            ENDIF.

          else.
            SELECT SINGLE ENAME INTO ZESS_ORG-SUPVSR_NAME
                FROM PA0001
                 WHERE PERNR EQ HRP1001-SOBID
                   AND ENDDA EQ '99991231'.
            EXIT.
          endif.

        ENDIF.
      ELSE.
        SELECT SINGLE * FROM HRP1001
                    WHERE  PLVAR EQ '01'
                      AND  OTYPE EQ 'O'
                      AND  ISTAT EQ '1'
                      AND  BEGDA <= SY-DATUM
                      AND  ENDDA >= SY-DATUM
                      AND  SUBTY EQ 'A002'
                      AND  SCLAS EQ 'O'
                      AND  OBJID EQ $ORGEH.
        IF SY-SUBRC EQ 0.
          $ORGEH = HRP1001-SOBID.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.

  APPEND ZESS_ORG.

  RETURN-TYPE = 'S'.
  RETURN-MESSAGE = 'Success!'.
  APPEND RETURN.
ENDFUNCTION.
