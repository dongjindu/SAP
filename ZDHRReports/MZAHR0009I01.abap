*----------------------------------------------------------------------*
*   INCLUDE MZAHR0009I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE back_exit INPUT.

  SET SCREEN 0. LEAVE SCREEN.

ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZYEAR_VALUE  INPUT
*&---------------------------------------------------------------------*
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
*&      Module  GET_ZVERS_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE get_zvers_value INPUT.
  CLEAR it_versn. REFRESH it_versn.
*
  CLEAR zthr_pcp02.
  SELECT zval1 zval3 INTO (zthr_pcp02-zval1, zthr_pcp02-zval3)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1030'
                      AND zval2 = w_zyear.
    it_versn-zvers = zthr_pcp02-zval1(2).
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
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
*'Display/Modify Monthly Basicpay for the whole employee'.
    WHEN 'BUT1'.
      CALL TRANSACTION 'ZAHR0010'.
*'Create/Modify Table for Working Days'
    WHEN 'BUT2'.
      CALL TRANSACTION 'ZAHR0008'.
* 'Calculated Monthly Total Working hrs by Cost Center'.
    WHEN 'BUT3'.
      CALL TRANSACTION 'ZAHR0011'.
*'Recent Times Bonus Payment Ration'.
    WHEN 'BUT4'.
      CALL TRANSACTION 'ZAHR0012'.
* 'Annual Bonus Payment Ration'
    WHEN 'BUT5'.
      CALL TRANSACTION 'ZAHR0013'.
* 'Not Used Holiday Payment Ration'
    WHEN 'BUT6'.
      CALL TRANSACTION 'ZAHR0014'.
* 'Employer 401K Expenses Payment Ration'
    WHEN 'BUT7'.
      CALL TRANSACTION 'ZAHR0015'.
* 'The Others Expenses Payment Ration'
    WHEN 'BUT8'.
      CALL TRANSACTION 'ZAHR0016'.
* 'Basic Database Creation'
    WHEN 'BUT9'.
      CLEAR sy-ucomm.
      PERFORM authority_check.
      PERFORM make_basic_data.
    WHEN 'BUT10'.
      CLEAR sy-ucomm.
      PERFORM make_basic_data_add_new.
    WHEN 'BUT11'.
*      CLEAR sy-ucomm.
*      PERFORM make_basic_data_delete.
    WHEN 'BUTA'.
      CLEAR sy-ucomm.
      PERFORM make_overtime_work.
    WHEN 'BUTB'.
      CLEAR sy-ucomm.
      PERFORM final_data_excel_down_load.
    WHEN 'GOTO'.
      CALL TRANSACTION 'ZAHR0005'.
  ENDCASE.
*
  CLEAR sy-ucomm.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.
  CALL METHOD cl_gui_cfw=>dispatch.
ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE ok_code.
    WHEN 'EXEC'.
      PERFORM data_selection.
      w_event = 'X'.
    WHEN OTHERS .
      w_event = 'X'.
*    WHEN 'EXIT'.
*      PERFORM EXIT_PROGRAM.
*    WHEN OTHERS.
**     do nothing
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT_1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back_exit_1 INPUT.

  CALL METHOD grid1->free.
  CALL METHOD g_custom_container->free.
  CALL METHOD cl_gui_cfw=>flush.
  CLEAR g_custom_container.
  CLEAR grid1.
  SET SCREEN 0.
  LEAVE SCREEN.

ENDMODULE.                 " BACK_EXIT_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_WERKS_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_werks_value INPUT.
*
  CLEAR it_persa. REFRESH it_persa.
*
  CLEAR t500p.
  SELECT persa name1 INTO (t500p-persa, t500p-name1)
    FROM t500p WHERE molga = '10'.
    it_persa-werks = t500p-persa.
    it_persa-name1 = t500p-name1.
    APPEND it_persa. CLEAR it_persa.
  ENDSELECT.
*
  CLEAR it_field. REFRESH it_field.
*
  it_field-tabname    = 'T500P'.
  it_field-fieldname  = 'PERSA'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.

  it_field-tabname    = 'T500P'.
  it_field-fieldname  = 'NAME1'.
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
            full_table   = it_persa.
*
  w_werks = w_fldvl.
*
  CLEAR dynpfields. REFRESH dynpfields.

  READ TABLE it_persa INDEX w_tabix.
  dynpfields-fieldname   = 'W_NAME1'.
  dynpfields-fieldvalue  = it_persa-name1.
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

ENDMODULE.                 " GET_WERKS_VALUE  INPUT
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
    FROM zthr_pcp00 WHERE zmons = w_zmons
                      AND zvers = w_zvers
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
      SELECT SINGLE zval1 INTO  it_kostl-zktxt
       FROM zthr_pcp02
        WHERE zmodl = '02'
          AND ( zgrup = '1260' OR zgrup = '1270' )
          AND zctxt = it_kostl-zkost.
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
  zthr_pcp00-zcost = w_fldvl.
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
