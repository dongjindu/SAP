*----------------------------------------------------------------------*
*   INCLUDE MZAHR0011I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE BACK_EXIT INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZYEAR_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZYEAR_VALUE INPUT.
  CLEAR IT_YEARS. REFRESH IT_YEARS.
*
  CLEAR ZTHR_PCP02.
  SELECT ZVAL1 INTO ZTHR_PCP02-ZVAL1
    FROM ZTHR_PCP02 WHERE ZMODL = '02'
                      AND ZGRUP = '1020'.
    IT_YEARS-ZYEAR = ZTHR_PCP02-ZVAL1+(4).
    APPEND IT_YEARS. CLEAR IT_YEARS.
  ENDSELECT.
*
  CLEAR IT_FIELD. REFRESH IT_FIELD.
*
  IT_FIELD-TABNAME    = 'ZTHR_PCP03'.
  IT_FIELD-FIELDNAME  = 'ZYEAR'.
  IT_FIELD-SELECTFLAG = 'X'.
  APPEND IT_FIELD. CLEAR IT_FIELD.
*
  CLEAR: W_FNAME, W_TABIX, W_FLDVL.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD      = W_FNAME
       IMPORTING
            IND              = W_TABIX
            SELECT_VALUE     = W_FLDVL
       TABLES
            FIELDS           = IT_FIELD
            FULL_TABLE       = IT_YEARS.
*
  W_ZYEAR = W_FLDVL.
ENDMODULE.                 " GET_ZYEAR_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZVERS_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZVERS_VALUE INPUT.
  CLEAR IT_VERSN. REFRESH IT_VERSN.
*
  CLEAR ZTHR_PCP02.
  SELECT ZVAL1 ZVAL3 INTO (ZTHR_PCP02-ZVAL1, ZTHR_PCP02-ZVAL3)
    FROM ZTHR_PCP02 WHERE ZMODL = '02'
                      AND ZGRUP = '1030'
                      AND ZVAL2 = W_ZYEAR.
    IT_VERSN-ZVERS = ZTHR_PCP02-ZVAL1+(2).
    APPEND IT_VERSN. CLEAR IT_VERSN.
  ENDSELECT.
*
  IF IT_VERSN[] IS INITIAL.
    EXIT.
  ENDIF.
*
  CLEAR IT_FIELD. REFRESH IT_FIELD.
*
  IT_FIELD-TABNAME    = 'ZTHR_PCP03'.
  IT_FIELD-FIELDNAME  = 'ZVERS'.
  IT_FIELD-SELECTFLAG = 'X'.
  APPEND IT_FIELD. CLEAR IT_FIELD.
*
  CLEAR: W_FNAME, W_TABIX, W_FLDVL.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD      = W_FNAME
       IMPORTING
            IND              = W_TABIX
            SELECT_VALUE     = W_FLDVL
       TABLES
            FIELDS           = IT_FIELD
            FULL_TABLE       = IT_VERSN.
*
  W_ZVERS = W_FLDVL.
ENDMODULE.                 " GET_ZVERS_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZGRUP_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZGRUP_VALUE INPUT.
  CLEAR IT_GROUP. REFRESH IT_GROUP.
*
  CLEAR ZTHR_PCP01.
  SELECT ZGRUP ZGTXT INTO (ZTHR_PCP01-ZGRUP, ZTHR_PCP01-ZGTXT)
    FROM ZTHR_PCP01 WHERE ZMODL = '02'
                      AND ZGRUP IN ('1040', '1050').
    IT_GROUP-ZGRUP = ZTHR_PCP01-ZGRUP.
    IT_GROUP-ZGTXT = ZTHR_PCP01-ZGTXT.
    APPEND IT_GROUP. CLEAR IT_GROUP.
  ENDSELECT.
*
  CLEAR IT_FIELD. REFRESH IT_FIELD.
*
  IT_FIELD-TABNAME    = 'ZTHR_PCP01'.
  IT_FIELD-FIELDNAME  = 'ZGRUP'.
  IT_FIELD-SELECTFLAG = 'X'.
  APPEND IT_FIELD. CLEAR IT_FIELD.

  IT_FIELD-TABNAME    = 'ZTHR_PCP01'.
  IT_FIELD-FIELDNAME  = 'ZGTXT'.
  IT_FIELD-SELECTFLAG = ' '.
  APPEND IT_FIELD. CLEAR IT_FIELD.
*
  CLEAR: W_FNAME, W_TABIX, W_FLDVL.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD      = W_FNAME
       IMPORTING
            IND              = W_TABIX
            SELECT_VALUE     = W_FLDVL
       TABLES
            FIELDS           = IT_FIELD
            FULL_TABLE       = IT_GROUP.
*
  W_ZGRUP = W_FLDVL.
*
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.

  READ TABLE IT_GROUP INDEX W_TABIX.
  DYNPFIELDS-FIELDNAME   = 'W_ZGTXT'.
  DYNPFIELDS-FIELDVALUE  = IT_GROUP-ZGTXT.
  APPEND DYNPFIELDS. CLEAR DYNPFIELDS.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME                     = SY-CPROG
            DYNUMB                     = SY-DYNNR
       TABLES
            DYNPFIELDS                 = DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA       = 1
            INVALID_DYNPROFIELD        = 2
            INVALID_DYNPRONAME         = 3
            INVALID_DYNPRONUMMER       = 4
            INVALID_REQUEST            = 5
            NO_FIELDDESCRIPTION        = 6
            UNDEFIND_ERROR             = 7
            OTHERS                     = 8.
*
  CLEAR R_ZJOBC. REFRESH R_ZJOBC.
  CLEAR ZTHR_PCP02.
  SELECT ZVAL1 INTO ZTHR_PCP02-ZVAL1
    FROM ZTHR_PCP02 WHERE ZMODL = '02'
                      AND ZGRUP = W_ZGRUP.
    R_ZJOBC-SIGN = 'I'.
    R_ZJOBC-OPTION = 'EQ'.
    R_ZJOBC-LOW = ZTHR_PCP02-ZVAL1.
    APPEND R_ZJOBC. CLEAR R_ZJOBC.
  ENDSELECT.
ENDMODULE.                 " GET_ZGRUP_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXEC'.
      PERFORM SELECT_MAIN_DATA.
      PERFORM GET_HOURLY_SALARY.
      PERFORM COPY_DATA_BY_JOB.
    WHEN 'BUT2'.  " Period Save
      PERFORM DATA_SAVE_PERIOD.
    WHEN 'BUT1'.  " Year Save
      PERFORM DATA_SAVE_YEAR.
  ENDCASE.
*
  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZKOST_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZKOST_VALUE INPUT.
  DATA: T_BEGDA LIKE SY-DATUM,
        T_ENDDA LIKE SY-DATUM.
*
  T_BEGDA = T_ENDDA = SY-DATUM.
  T_BEGDA+4(4) = '0101'.
  T_ENDDA+4(4) = '1231'.
  CLEAR IT_KOSTL. REFRESH IT_KOSTL.
*
  CLEAR ZTHR_PCP00.
  SELECT DISTINCT ZCOST INTO ZTHR_PCP00-ZCOST
    FROM ZTHR_PCP00 WHERE ZMONS = '01'
                      AND ZVERS = '001'
                      AND ERDAT BETWEEN T_BEGDA AND T_ENDDA.
    IT_KOSTL-ZKOST = ZTHR_PCP00-ZCOST.
    APPEND IT_KOSTL. CLEAR IT_KOSTL.
  ENDSELECT.
*
  IF IT_KOSTL[] IS INITIAL.
    EXIT.
  ENDIF.
*
  LOOP AT IT_KOSTL.
    CLEAR CSKT.
    SELECT SINGLE KTEXT INTO CSKT-KTEXT
      FROM CSKT WHERE SPRAS = SY-LANGU
                  AND KOSTL = IT_KOSTL-ZKOST.
    IT_KOSTL-ZKTXT = CSKT-KTEXT.
    MODIFY IT_KOSTL. CLEAR IT_KOSTL.
  ENDLOOP.
*
  CLEAR IT_FIELD. REFRESH IT_FIELD.
*
  IT_FIELD-TABNAME    = 'CSKT'.
  IT_FIELD-FIELDNAME  = 'KOSTL'.
  IT_FIELD-SELECTFLAG = 'X'.
  APPEND IT_FIELD. CLEAR IT_FIELD.

  IT_FIELD-TABNAME    = 'CSKT'.
  IT_FIELD-FIELDNAME  = 'KTEXT'.
  IT_FIELD-SELECTFLAG = ' '.
  APPEND IT_FIELD. CLEAR IT_FIELD.
*
  CLEAR: W_FNAME, W_TABIX, W_FLDVL.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD      = W_FNAME
       IMPORTING
            IND              = W_TABIX
            SELECT_VALUE     = W_FLDVL
       TABLES
            FIELDS           = IT_FIELD
            FULL_TABLE       = IT_KOSTL.
*
  W_KOSTL = W_FLDVL.
*
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.

  READ TABLE IT_KOSTL INDEX W_TABIX.
  DYNPFIELDS-FIELDNAME   = 'W_KTEXT'.
  DYNPFIELDS-FIELDVALUE  = IT_KOSTL-ZKTXT.
  APPEND DYNPFIELDS. CLEAR DYNPFIELDS.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME                     = SY-CPROG
            DYNUMB                     = SY-DYNNR
       TABLES
            DYNPFIELDS                 = DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA       = 1
            INVALID_DYNPROFIELD        = 2
            INVALID_DYNPRONAME         = 3
            INVALID_DYNPRONUMMER       = 4
            INVALID_REQUEST            = 5
            NO_FIELDDESCRIPTION        = 6
            UNDEFIND_ERROR             = 7
            OTHERS                     = 8.
ENDMODULE.                 " GET_ZKOST_VALUE  INPUT
