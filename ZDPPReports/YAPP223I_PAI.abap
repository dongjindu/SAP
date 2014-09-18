*----------------------------------------------------------------------
*
*   INCLUDE YAPP223L_PAI                                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1205  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands
*----------------------------------------------------------------------*
MODULE user_command_1205 INPUT.
  CASE ok_code.
    WHEN 'SRH'.  "SEARCH
*     Check Essential conditions
      PERFORM check_essential_condition_1205.
*     Search Data
      PERFORM search_data_1205.
      CLEAR ok_code.

    WHEN 'UPD'.  "TOGGLE
      wa_upd_1205 = 'X'.
      CLEAR ok_code.

    WHEN 'SAV'.  "SAVE
      CLEAR wa_upd_1205.
      CLEAR ok_code.
      READ TABLE it_app223 WITH KEY mark = 'X'.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
*     Update Data
      PERFORM update_alc_1205.
*     Check Essential Conditions
      PERFORM check_essential_condition_1205.
*     Search Data
      PERFORM search_data_1205.

    WHEN 'DEL'.  "DELETE
      CLEAR ok_code.
*     Delete Data
      PERFORM delete_data_1205.
*     Check Essential Conditions
      PERFORM check_essential_condition_1205.
*     Search Data
      PERFORM search_data_1205.

    WHEN 'EXL'.  "EXCEL
*     Set Data For Downloading
      PERFORM set_excel_1205.
*     Call A Function For Downloading
      PERFORM call_func_download_1205.
      CLEAR ok_code.

    WHEN 'FILE'.  "LOOK FOR PATH

      CLEAR ok_code.

    WHEN 'BCK'.  "BACK COLUMN
      CLEAR ok_code.
      p_key = p_key - 1.
      IF p_key <= 0.
        p_key = 1.
        MESSAGE i000 WITH 'There is not the key 0.'.
      ENDIF.
*     Check Essential Conditions
      PERFORM check_essential_condition_1205.
*     Search Data
      PERFORM search_data_1205.

    WHEN 'NXT'.  "NEXT COLUMN
      p_key = p_key + 1.
*     Check Essential Conditions
      PERFORM check_essential_condition_1205.
*     Search Data
      PERFORM search_data_1205.
      CLEAR ok_code.

    WHEN 'ADD'.  "CREATE NEW DATA
      CLEAR ok_code.
      CLEAR wa_init_1206.
      CALL SCREEN '1206'. " STARTING AT 10 3 ENDING AT 90 22.

    WHEN 'UPDA'.  "UPDATE ALL

      CLEAR ok_code.

    WHEN 'SORTA'. "SORTING ASCENDING.
      CLEAR ok_code.
*     Sort By Ascending
      PERFORM sort_ascending_1205.

    WHEN 'SORTD'. "SORTING DESCENDING.
      CLEAR ok_code.
*     Sort By Descending
      PERFORM sort_descending_1205.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1205  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*       Modification of Internal Table
*----------------------------------------------------------------------*
MODULE modify_data_1205 INPUT.
  MODIFY it_app223 INDEX tc_app223-current_line.

ENDMODULE.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  back  INPUT
*&---------------------------------------------------------------------*
*       Setting Command - Back
*----------------------------------------------------------------------*
MODULE back INPUT.
  LEAVE TO SCREEN 0.

ENDMODULE.                 " back  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_internal_table  INPUT
*&---------------------------------------------------------------------*
*     Modification of Internal Table with Table Control's Current line
*----------------------------------------------------------------------*
MODULE modify_internal_table_1206 INPUT.
  MODIFY it_app223_new INDEX tc_app223_new-current_line.
  IF sy-subrc <> 0.
    APPEND it_app223_new.
  ENDIF.

ENDMODULE.                 " modify_internal_table  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1206  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands
*----------------------------------------------------------------------*
MODULE user_command_1206 INPUT.
  CASE ok_code.
    WHEN 'SAVE'.
      CLEAR ok_code.
*     Check Essential Conditions
      PERFORM check_essential_condition_1205.
*     Create New Data
      PERFORM create_new_data_1206.
      EXIT.
    WHEN 'CLEAR'.
      CLEAR ok_code.
*     Clear Internal Table
      PERFORM clear_it_app223_new.
    WHEN 'BACK'.
      CLEAR ok_code.
    WHEN OTHERS.
      CLEAR ok_code.
*     Set Parameters
      PERFORM setting_parameters_1206.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1206  INPUT
