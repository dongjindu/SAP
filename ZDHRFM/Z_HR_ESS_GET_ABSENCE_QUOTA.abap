FUNCTION Z_HR_ESS_GET_ABSENCE_QUOTA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"     VALUE(BEGDA) TYPE  BEGDA OPTIONAL
*"     VALUE(ENDDA) TYPE  ENDDA OPTIONAL
*"  TABLES
*"      ZESS_ABSENCE_QUOTA STRUCTURE  ZESS_ABSENCE_QUOTA
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  06/15/2012  Valerian  UD1K955044  Apply logic to check if deduction
*                                    is current.
*
*----------------------------------------------------------------------

  __CLS : ZESS_ABSENCE_QUOTA, P2006.

  CALL FUNCTION 'HR_READ_INFOTYPE'
       EXPORTING
            PERNR         = EMPLOYEE_NUMBER
            INFTY         = '2006'
            BEGDA         = SY-DATUM
            ENDDA         = '99991231'
            BYPASS_BUFFER = 'X'
       TABLES
            INFTY_TAB     = P2006.

  READ TABLE P2006 INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN-TYPE = 'E'.
    RETURN-MESSAGE = 'No data has been found'.
    APPEND RETURN.
    EXIT.
  ENDIF.

  SORT P2006 BY BEGDA DESCENDING.

  IF NOT BEGDA IS INITIAL AND NOT ENDDA IS INITIAL.
    LOOP AT P2006.
      CHECK P2006-KTART EQ '10' OR P2006-KTART EQ '13'.
      IF ( BEGDA <= P2006-BEGDA AND
           P2006-ENDDA <= ENDDA )
      AND SY-DATUM BETWEEN P2006-DESTA AND P2006-DEEND.     "UD1K955044
      SELECT SINGLE KTEXT INTO ZESS_ABSENCE_QUOTA-QUOTA_TYPE FROM T556B
            WHERE SPRSL EQ SY-LANGU
              AND KTART EQ P2006-KTART.

        ZESS_ABSENCE_QUOTA-NUMBER    = P2006-ANZHL.
        ZESS_ABSENCE_QUOTA-DEDUCTION = P2006-KVERB.
        ZESS_ABSENCE_QUOTA-REMAINING = P2006-ANZHL - P2006-KVERB.
        APPEND ZESS_ABSENCE_QUOTA.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT P2006.
      CHECK P2006-KTART EQ '10' OR P2006-KTART EQ '13'.
      IF ( BEGDA <= P2006-BEGDA AND
           P2006-ENDDA <= ENDDA ).
      SELECT SINGLE KTEXT INTO ZESS_ABSENCE_QUOTA-QUOTA_TYPE FROM T556B
            WHERE SPRSL EQ SY-LANGU
              AND KTART EQ P2006-KTART.

        ZESS_ABSENCE_QUOTA-NUMBER    = P2006-ANZHL.
        ZESS_ABSENCE_QUOTA-DEDUCTION = P2006-KVERB.
        ZESS_ABSENCE_QUOTA-REMAINING = P2006-ANZHL - P2006-KVERB.
        APPEND ZESS_ABSENCE_QUOTA.
      ENDIF.
    ENDLOOP.
  ENDIF.

  RETURN-TYPE = 'S'.
  RETURN-MESSAGE = 'Success!'.
  APPEND RETURN.

ENDFUNCTION.
