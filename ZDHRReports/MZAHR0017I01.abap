*----------------------------------------------------------------------*
*   INCLUDE MZAHR0005I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE back_exit INPUT.
  w_modes = 0.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5000 INPUT.
  CASE sy-ucomm.
    WHEN 'BUT1'.            " module & group
      PERFORM get_data_from_et01.
    WHEN 'BUT2'.            " code
      PERFORM init_value_and_call_scr.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'MODE'.
      CLEAR it_stats. REFRESH it_stats.
      CASE w_modes.
        WHEN 0.  w_modes = 1.
        WHEN 1.
          w_modes = 0.
          it_stats-fcode = 'SAVE'. APPEND it_stats.
          it_stats-fcode = 'DELE'. APPEND it_stats.
          it_stats-fcode = 'NEWN'. APPEND it_stats.
      ENDCASE.
    WHEN 'DELE'.
      PERFORM delete_line_et01.
    WHEN 'DETL'.
      PERFORM go_to_9100_screen.
    WHEN 'ASOT'.
      GET CURSOR FIELD w_field.
      PERFORM ascending_sort.
    WHEN 'DSOT'.
      GET CURSOR FIELD w_field.
      PERFORM descending_sort.
    WHEN 'NEWN'.
      PERFORM append_new_entries.
    WHEN 'SAVE'.
      PERFORM save_et01_data.
  ENDCASE.
  CLEAR sy-ucomm.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_intab  INPUT
*&---------------------------------------------------------------------*
MODULE modify_intab INPUT.
  IF it_et01-ernam EQ space.
    it_et01-erdat = sy-datum.
    it_et01-erzet = sy-uzeit.
    it_et01-ernam = sy-uname.
  ELSE.
    it_et01-aedat = sy-datum.
    it_et01-aezet = sy-uzeit.
    it_et01-aenam = sy-uname.
  ENDIF.
  MODIFY it_et01 INDEX tc9000-current_line.
ENDMODULE.                 " modify_intab  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_duplication  INPUT
*&---------------------------------------------------------------------*
MODULE check_duplication INPUT.
  CLEAR zthr_et01.
  SELECT SINGLE zgrup INTO zthr_et01-zgrup
    FROM zthr_et01 WHERE zmodl = it_et01-zmodl
                      AND zgrup = it_et01-zgrup.
*
  IF sy-subrc = 0.
    MESSAGE w001 WITH 'input value already exist'.
  ELSE.
    MODIFY it_et01 INDEX tc9000-current_line.
  ENDIF.
ENDMODULE.                 " check_duplication  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_module_value  INPUT
*&---------------------------------------------------------------------*
MODULE get_module_value INPUT.
  CLEAR it_moval. REFRESH it_moval.
*
  CLEAR zthr_et01.
  SELECT DISTINCT zmodl zmtxt
    INTO (zthr_et01-zmodl, zthr_et01-zmtxt)
    FROM zthr_et01 WHERE erdat > '20000101'.
    it_moval-zmodl = zthr_et01-zmodl.
    it_moval-zmtxt = zthr_et01-zmtxt.
    APPEND it_moval. CLEAR it_moval.
  ENDSELECT.
*
  CLEAR it_field. REFRESH it_field.
*
  it_field-tabname    = 'ZTHR_ET01'.
  it_field-fieldname  = 'ZMODL'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.

  it_field-tabname    = 'ZTHR_ET01'.
  it_field-fieldname  = 'ZMTXT'.
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
            full_table   = it_moval.
*
  w_zmodl = w_fldvl.
  PERFORM screen_value_update.
ENDMODULE.                 " get_module_value  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_group_value  INPUT
*&---------------------------------------------------------------------*
MODULE get_group_value INPUT.
  CLEAR it_grval. REFRESH it_grval.

  CLEAR zthr_et01.
  SELECT zgrup zgtxt INTO (zthr_et01-zgrup, zthr_et01-zgtxt)
    FROM zthr_et01 WHERE zmodl = w_zmodl
                      AND erdat > '20000101'.
    it_grval-zgrup = zthr_et01-zgrup.
    it_grval-zgtxt = zthr_et01-zgtxt.
    APPEND it_grval. CLEAR it_grval.
  ENDSELECT.

  CLEAR it_field. REFRESH it_field.

  it_field-tabname    = 'ZTHR_ET01'.
  it_field-fieldname  = 'ZGRUP'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.

  it_field-tabname    = 'ZTHR_ET01'.
  it_field-fieldname  = 'ZGTXT'.
  it_field-selectflag = ' '.
  APPEND it_field. CLEAR it_field.

  CLEAR: w_fname, w_tabix, w_fldvl.

  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            selectfield  = w_fname
       IMPORTING
            ind          = w_tabix
            select_value = w_fldvl
       TABLES
            fields       = it_field
            full_table   = it_grval.

  w_zgrup = w_fldvl.
  PERFORM screen_value_update_1.
ENDMODULE.                 " get_group_value  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE sy-ucomm.
    WHEN 'EXEC'.
      PERFORM get_data_from_et02.
    WHEN 'MODE'.
      CLEAR it_stats. REFRESH it_stats.
      CASE w_modes.
        WHEN 0. w_modes = 1.
        WHEN 1.
          w_modes = 0.
          it_stats-fcode = 'SAVE'. APPEND it_stats.
          it_stats-fcode = 'DELE'. APPEND it_stats.
          it_stats-fcode = 'NEWN'. APPEND it_stats.
      ENDCASE.
    WHEN 'DELE'.
      PERFORM delete_line_et02.
    WHEN 'NEWN'.
      PERFORM append_new_lines.
    WHEN 'SAVE'.
      PERFORM save_et02_data.
  ENDCASE.
  CLEAR sy-ucomm.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_duplication_code  INPUT
*&---------------------------------------------------------------------*
MODULE check_duplication_code INPUT.
  CLEAR zthr_et02.
  SELECT SINGLE zvald INTO zthr_et02-zvald
    FROM zthr_et02 WHERE zmodl = it_et02-zmodl
                      AND zgrup = it_et02-zgrup
                      AND zcode = it_et02-zcode.
  IF sy-subrc = 0.
    MESSAGE w001 WITH 'input value already exist'.
  ELSE.
    it_et02-zvald = sy-datum.
    MODIFY it_et02 INDEX tc9100-current_line.
  ENDIF.
ENDMODULE.                 " check_duplication_code  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_ET02  INPUT
*&---------------------------------------------------------------------*
MODULE modify_et02 INPUT.
  IF it_et02-ernam EQ space.
    it_et02-erdat = sy-datum.
    it_et02-erzet = sy-uzeit.
    it_et02-ernam = sy-uname.
  ELSE.
    it_et02-aedat = sy-datum.
    it_et02-aezet = sy-uzeit.
    it_et02-aenam = sy-uname.
  ENDIF.
  MODIFY it_et02 INDEX tc9100-current_line.
ENDMODULE.                 " modify_pcp02  INPUT
