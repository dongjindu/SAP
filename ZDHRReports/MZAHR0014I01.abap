*----------------------------------------------------------------------*
***INCLUDE MZAHR0014I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back_exit INPUT.
  DATA: wl_answer.
  IF w_input = 'X'.
    CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
         EXPORTING
              textline1 = 'Do you want to'
              textline2 = 'continue? '
              titel     = 'Data will be lost'
         IMPORTING
              answer    = wl_answer.

    IF wl_answer = 'J'.
      SET SCREEN 0. LEAVE SCREEN.
    ENDIF.
  ELSE.
    SET SCREEN 0. LEAVE SCREEN.
*    CALL TRANSACTION 'ZAHR0009'.
*    SET SCREEN 0. "LEAVE SCREEN.
*    LEAVE TO TRANSACTION 'ZAHR0009'.
  ENDIF.

ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_9000_CHANGE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_9000_change INPUT.
  CHECK 0 =< it_9000-rate1  AND it_9000-rate1 =< 100.
  it_9000-amunt = it_9000-zhouy *  it_9000-annul * it_9000-rate1
                  / 100.

  MODIFY it_9000
      INDEX tc_9000-current_line.

  w_input = 'X'.


ENDMODULE.                 " IT_9000_CHANGE  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'EXEC'.
      PERFORM select_main_data.
    WHEN 'SAVE'.
      PERFORM save_data_annual_bunus.
    WHEN 'EXCL'.
      PERFORM down_load_excel_file.
  ENDCASE.
*
  CLEAR sy-ucomm.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
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
*&      Module  get_zmons_value  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_zmons_value INPUT.
  CLEAR wa_sel.
  CLEAR it_mons. REFRESH it_mons.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mons
    FROM zthr_month.

  IF it_mons[] IS INITIAL.
    EXIT.
  ENDIF.
*
  CLEAR it_field. REFRESH it_field.
  it_field-tabname    = 'ZTHR_MONTH'.
  it_field-fieldname  = 'ZMONS'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.

  it_field-tabname    = 'ZTHR_MONTH'.
  it_field-fieldname  = 'ZTEXT'.
  it_field-selectflag = ' '.
  APPEND it_field. CLEAR it_field.

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
            full_table   = it_mons.
*
  w_zmons = w_fldvl.
  ON CHANGE OF w_zmons.
    PERFORM select_main_data.
    SET SCREEN 0.
    CALL SCREEN 9000.
    SET SCREEN 0. LEAVE SCREEN.
  ENDON.
ENDMODULE.                 " get_zmons_value  INPUT
