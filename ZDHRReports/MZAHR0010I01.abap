*----------------------------------------------------------------------*
*   INCLUDE MZAHR0010I01                                               *
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
    it_years-zyear = zthr_pcp02-zval1+(4).
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
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'EXEC'.
      PERFORM select_basic_data.
    WHEN 'SAVE'.
      PERFORM data_save_basicpay.
    WHEN 'EXCL'.
      PERFORM excel_down_load_file.

  ENDCASE.
*
  CLEAR sy-ucomm.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHANGE_LIST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_list INPUT.
  MODIFY it_pcp00
      INDEX tc9000-current_line.
ENDMODULE.                 " CHANGE_LIST  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_zmons_value  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_zmons_value INPUT.
  CLEAR it_mons. REFRESH it_mons.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mons
    FROM zthr_month.

  IF it_mons[] IS INITIAL.
    EXIT.
  ENDIF.
*
  CLEAR it_field. REFRESH it_field.
*zthr_ahc01-zmons
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
    PERFORM select_basic_data.
    SET SCREEN 0.
    CALL SCREEN 9000.
    SET SCREEN 0.LEAVE SCREEN.
  ENDON.

ENDMODULE.                 " get_zmons_value  INPUT
