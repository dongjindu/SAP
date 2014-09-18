*----------------------------------------------------------------------*
*   INCLUDE MZAHR0006I01                                               *
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
                      AND ZGRUP = '1030'.
    IF ZTHR_PCP02-ZVAL3+(4) = W_ZYEAR.
      IT_VERSN-ZVERS = ZTHR_PCP02-ZVAL1+(3).
      APPEND IT_VERSN. CLEAR IT_VERSN.
    ENDIF.
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
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXEC'.
      PERFORM GET_DATA_FROM_PCP03.
    WHEN 'PSAV'.
      PERFORM SAVE_PCP03_DATA.
    WHEN 'FSAV'.
      PERFORM FINAL_SAVE.
  ENDCASE.
*
  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALCU_TOTAL  INPUT
*&---------------------------------------------------------------------*
MODULE CALCU_TOTAL INPUT.
  IT_PCP03-ZTOTL = IT_PCP03-ZWEKN + IT_PCP03-ZWEKE +
                   IT_PCP03-ZSATU + IT_PCP03-ZHOLI.
*
  IF IT_PCP03-ERNAM = SPACE.
    IT_PCP03-ERDAT = SY-DATUM.
    IT_PCP03-ERZET = SY-UZEIT.
    IT_PCP03-ERNAM = SY-UNAME.
  ELSE.
    IT_PCP03-AEDAT = SY-DATUM.
    IT_PCP03-AEZET = SY-UZEIT.
    IT_PCP03-AENAM = SY-UNAME.
  ENDIF.
*
  MODIFY IT_PCP03 INDEX TC9000-CURRENT_LINE.
ENDMODULE.                 " CALCU_TOTAL  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ORGTX_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ORGTX_VALUE INPUT.
  CHECK W_MASTR = 'X'.
*
  CALL FUNCTION 'RP_PNP_GET_VALUE_ORGEH'
       CHANGING ORGEH = W_ORGEH.
*
  CLEAR T527X.
  SELECT SINGLE ORGTX INTO T527X-ORGTX
    FROM T527X WHERE SPRSL = SY-LANGU
                 AND ORGEH = W_ORGEH
                 AND ENDDA = '99991231'.
*
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.

  DYNPFIELDS-FIELDNAME   = 'W_ORGTX'.
  DYNPFIELDS-FIELDVALUE  = T527X-ORGTX.
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
ENDMODULE.                 " GET_ORGTX_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_WERKS_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE GET_WERKS_VALUE INPUT.
  CHECK W_MASTR = 'X'.
*
  CLEAR IT_PERSA. REFRESH IT_PERSA.
*
  CLEAR T500P.
  SELECT PERSA NAME1 INTO (T500P-PERSA, T500P-NAME1)
    FROM T500P WHERE MOLGA = '10'.
    IT_PERSA-WERKS = T500P-PERSA.
    IT_PERSA-NAME1 = T500P-NAME1.
    APPEND IT_PERSA. CLEAR IT_PERSA.
  ENDSELECT.
*
  CLEAR IT_FIELD. REFRESH IT_FIELD.
*
  IT_FIELD-TABNAME    = 'T500P'.
  IT_FIELD-FIELDNAME  = 'PERSA'.
  IT_FIELD-SELECTFLAG = 'X'.
  APPEND IT_FIELD. CLEAR IT_FIELD.

  IT_FIELD-TABNAME    = 'T500P'.
  IT_FIELD-FIELDNAME  = 'NAME1'.
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
            FULL_TABLE       = IT_PERSA.
*
  W_WERKS = W_FLDVL.
*
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.

  READ TABLE IT_PERSA INDEX W_TABIX.
  DYNPFIELDS-FIELDNAME   = 'W_NAME1'.
  DYNPFIELDS-FIELDVALUE  = IT_PERSA-NAME1.
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
ENDMODULE.                 " GET_WERKS_VALUE  INPUT
