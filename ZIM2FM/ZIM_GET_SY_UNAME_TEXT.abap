FUNCTION ZIM_GET_SY_UNAME_TEXT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(P_UNAME) LIKE  SY-UNAME
*"  EXPORTING
*"     REFERENCE(W_ADRP) LIKE  ADRP STRUCTURE  ADRP
*"     REFERENCE(W_ADR2) LIKE  ADR2 STRUCTURE  ADR2
*"     REFERENCE(P_FIRST_NAME) LIKE  ADRP-NAME_FIRST
*"     REFERENCE(P_LAST_NAME) LIKE  ADRP-NAME_LAST
*"     REFERENCE(P_TEL_NO) LIKE  ADR2-TEL_NUMBER
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------
  TABLES : ADRP, ADR2, USLOGOND, USDEFAULTS, USADDRESS.
  TYPES : USTYP_T_PARAMETERS.

  DATA : LOGONDATA     LIKE    USLOGOND,
         DEFAULTS      LIKE    USDEFAULTS,
         ADDRESS       LIKE    USADDRESS.

  DATA BEGIN OF PARAMETERS OCCURS 20.
          INCLUDE STRUCTURE USPARAM.
  DATA END OF PARAMETERS.

  CLEAR : W_ADRP, W_ADR2.

  CALL FUNCTION 'SUSR_USER_READ'
       EXPORTING
            USER_NAME            = P_UNAME
            WITH_TEXT            = 'X'
       IMPORTING
            USER_LOGONDATA       = LOGONDATA
            USER_DEFAULTS        = DEFAULTS
            USER_ADDRESS         = ADDRESS
       TABLES
            USER_PARAMETERS      = PARAMETERS
       EXCEPTIONS
            USER_NAME_NOT_EXISTS = 1
            INTERNAL_ERROR       = 2
            OTHERS               = 3.

  IF SY-SUBRC NE 0.
     RAISE  NOT_FOUND.
  ENDIF.

*-----------------------------------------------------------------------
* 荤盔 (吝居林家包府) Table Select...
*-----------------------------------------------------------------------
  SELECT * FROM ADRP
           WHERE PERSNUMBER EQ ADDRESS-PERSNUMBER
           AND   DATE_FROM  <  SY-DATUM.
    EXIT.
  ENDSELECT.

  IF SY-SUBRC NE 0.
     CLEAR : P_LAST_NAME, P_FIRST_NAME.
  ELSE.
     P_LAST_NAME          =    ADRP-NAME_LAST.
     P_FIRST_NAME         =    ADRP-NAME_FIRST.
     W_ADRP               =    ADRP.
  ENDIF.

  SELECT * FROM ADR2
           WHERE PERSNUMBER EQ ADDRESS-PERSNUMBER
           AND   ADDRNUMBER EQ ADDRESS-ADDRNUMBER
           AND   DATE_FROM  <  SY-DATUM.
    EXIT.
  ENDSELECT.

  IF SY-SUBRC NE 0.
     CLEAR : P_TEL_NO.
  ELSE.
     P_TEL_NO = ADR2-TEL_NUMBER.
     W_ADR2               =    ADR2.
  ENDIF.

ENDFUNCTION.
