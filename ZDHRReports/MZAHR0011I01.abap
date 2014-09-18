*----------------------------------------------------------------------*
***INCLUDE MZAHR0011I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back_exit INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'EXEC'.
      CLEAR sy-ucomm.
      PERFORM select_main_data.
*      PERFORM GET_HOURLY_SALARY.
      PERFORM copy_data_by_job.
    WHEN 'EXCL'.
      PERFORM excel_down_load.
    WHEN 'BUT2'.  " Period Save
*      PERFORM DATA_SAVE_PERIOD.
    WHEN 'BUT1'.  " Year Save
*      PERFORM DATA_SAVE_YEAR.
  ENDCASE.
*
  CLEAR sy-ucomm.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZKOST_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_zkost_value INPUT.
  DATA: t_begda LIKE sy-datum,
        t_endda LIKE sy-datum.
*
  t_begda = t_endda = sy-datum.
  t_begda+4(4) = '0101'.
  t_endda+4(4) = '1231'.
  CLEAR it_kostl. REFRESH it_kostl.
*
  CLEAR zthr_pcp00.
  SELECT DISTINCT zcost INTO zthr_pcp00-zcost
    FROM zthr_pcp00 WHERE zmons = '01'
                      AND zvers = '001'
                      AND erdat BETWEEN t_begda and t_endda.
    it_kostl-zkost = zthr_pcp00-zcost.
    APPEND it_kostl. CLEAR it_kostl.
  ENDSELECT.
*
  IF it_kostl[] IS INITIAL.
    EXIT.
  ENDIF.
*
  LOOP AT it_kostl.
    CLEAR cskt.
    SELECT SINGLE ktext INTO cskt-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_kostl-zkost.
    IF sy-subrc = 0.
      it_kostl-zktxt = cskt-ktext.
    ELSE.
      SELECT SINGLE zval1 INTO cskt-ktext
          FROM zthr_pcp02
           WHERE zmodl EQ '02'
             AND zgrup EQ '1260'
             AND zctxt EQ  it_kostl-zkost.
      it_kostl-zktxt = cskt-ktext.
    ENDIF.
    MODIFY it_kostl. CLEAR it_kostl.
  ENDLOOP.
*
  CLEAR it_field. REFRESH it_field.
*
  it_field-tabname    = 'CSKT'.
  it_field-fieldname  = 'KOSTL'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.

  it_field-tabname    = 'CSKT'.
  it_field-fieldname  = 'KTEXT'.
  it_field-selectflag = ' '.
  APPEND it_field. CLEAR it_field.
*
  CLEAR: w_fname, w_tabix, w_fldvl.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            selectfield  = w_fname
       IMPORTING
            ind          = w_tabix
            select_value = w_fldvl
       TABLES
            fields       = it_field
            full_table   = it_kostl.
*
  w_kostl = w_fldvl.
*
  CLEAR dynpfields. REFRESH dynpfields.

  READ TABLE it_kostl INDEX w_tabix.
  dynpfields-fieldname   = 'W_KTEXT'.
  dynpfields-fieldvalue  = it_kostl-zktxt.
  APPEND dynpfields. CLEAR dynpfields.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       TABLES
            dynpfields           = dynpfields
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            OTHERS               = 8.
ENDMODULE.                 " GET_ZKOST_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZYEAR_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_zyear_value INPUT.
  CLEAR it_years. REFRESH it_years.
*
  CLEAR zthr_pcp02.
  SELECT zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1020'.
    it_years-zyear = zthr_pcp02-zval1(4).
    APPEND it_years. CLEAR it_years.
  ENDSELECT.
*
  CLEAR it_field. REFRESH it_field.
*
  it_field-tabname    = 'ZTHR_PCP03'.
  it_field-fieldname  = 'ZYEAR'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.
*
  CLEAR: w_fname, w_tabix, w_fldvl.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            selectfield  = w_fname
       IMPORTING
            ind          = w_tabix
            select_value = w_fldvl
       TABLES
            fields       = it_field
            full_table   = it_years.
*
  w_zyear = w_fldvl.

ENDMODULE.                 " GET_ZYEAR_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZGRUP_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_zgrup_value INPUT.
  CLEAR it_group. REFRESH it_group.
*
  CLEAR zthr_pcp01.
  SELECT zgrup zgtxt INTO (zthr_pcp01-zgrup, zthr_pcp01-zgtxt)
    FROM zthr_pcp01 WHERE zmodl = '02'
                      AND zgrup IN ('1040', '1050').
    it_group-zgrup = zthr_pcp01-zgrup.
    it_group-zgtxt = zthr_pcp01-zgtxt.
    APPEND it_group. CLEAR it_group.
  ENDSELECT.
*
  CLEAR it_field. REFRESH it_field.
*
  it_field-tabname    = 'ZTHR_PCP01'.
  it_field-fieldname  = 'ZGRUP'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.

  it_field-tabname    = 'ZTHR_PCP01'.
  it_field-fieldname  = 'ZGTXT'.
  it_field-selectflag = ' '.
  APPEND it_field. CLEAR it_field.
*
  CLEAR: w_fname, w_tabix, w_fldvl.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            selectfield  = w_fname
       IMPORTING
            ind          = w_tabix
            select_value = w_fldvl
       TABLES
            fields       = it_field
            full_table   = it_group.
*
  w_zgrup = w_fldvl.
*
  CLEAR dynpfields. REFRESH dynpfields.

  READ TABLE it_group INDEX w_tabix.
  dynpfields-fieldname   = 'W_ZGTXT'.
  dynpfields-fieldvalue  = it_group-zgtxt.
  APPEND dynpfields. CLEAR dynpfields.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       TABLES
            dynpfields           = dynpfields
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            OTHERS               = 8.
*
*  CLEAR R_ZJOBC. REFRESH R_ZJOBC.
*  CLEAR ZTHR_PCP02.
*  SELECT ZVAL1 INTO ZTHR_PCP02-ZVAL1
*    FROM ZTHR_PCP02 WHERE ZMODL = '02'
*                      AND ZGRUP = W_ZGRUP.
*    R_ZJOBC-SIGN = 'I'.
*    R_ZJOBC-OPTION = 'EQ'.
*    R_ZJOBC-LOW = ZTHR_PCP02-ZVAL1.
*    APPEND R_ZJOBC. CLEAR R_ZJOBC.
*  ENDSELECT.

ENDMODULE.                 " GET_ZGRUP_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZVERS_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_zvers_value INPUT.
  CLEAR it_versn. REFRESH it_versn.
*
  CLEAR zthr_pcp02.
  SELECT zval1 zval3 INTO (zthr_pcp02-zval1, zthr_pcp02-zval3)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1030'
                      AND zval2 = w_zyear.
    it_versn-zvers = zthr_pcp02-zval1+(2).
    APPEND it_versn. CLEAR it_versn.
  ENDSELECT.
*
  IF it_versn[] IS INITIAL.
    EXIT.
  ENDIF.
*
  CLEAR it_field. REFRESH it_field.
*
  it_field-tabname    = 'ZTHR_PCP03'.
  it_field-fieldname  = 'ZVERS'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.
*
  CLEAR: w_fname, w_tabix, w_fldvl.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            selectfield  = w_fname
       IMPORTING
            ind          = w_tabix
            select_value = w_fldvl
       TABLES
            fields       = it_field
            full_table   = it_versn.
*
  w_zvers = w_fldvl.

ENDMODULE.                 " GET_ZVERS_VALUE  INPUT
