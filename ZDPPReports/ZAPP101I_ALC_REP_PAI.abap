*----------------------------------------------------------------------*
*   INCLUDE YAPP101L_ALC_REP_PAI                                       *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       Leaving The Program.
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*       Modification of Internal Table's Data  <--- Line number.
*----------------------------------------------------------------------*
MODULE modify_data INPUT.
  MODIFY it_app101 INDEX tc_app101-current_line.

ENDMODULE.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0110  INPUT
*&---------------------------------------------------------------------*
*       Event Collection.
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  CASE ok_code.
    WHEN 'SEL'.
      PERFORM select_data.
      CLEAR ok_code.
    WHEN 'REL'.
      PERFORM release_data.
      CLEAR ok_code.
      CALL SCREEN '0111' STARTING AT 10 3 ENDING AT 90 22.
      CLEAR ok_code.
    WHEN 'EXL'.
      PERFORM process_download.
      CLEAR ok_code.
    WHEN 'DEL'.
      PERFORM delete_data.
      CLEAR ok_code.
    WHEN 'TOG'.
      PERFORM TOGGLE_FOR_CHECK_BOX.
      CLEAR OK_CODE.
    WHEN 'SORTA'. "SORTING ASCENDING.
      PERFORM sort_ascending.
      CLEAR ok_code.

    WHEN 'SORTD'. "SORTING DESCENDING.
      PERFORM sort_descending.
      CLEAR ok_code.
  ENDCASE.

ENDMODULE.                 " user_command_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  back  INPUT
*&---------------------------------------------------------------------*
*       Leaving Screen.
*----------------------------------------------------------------------*
MODULE back INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " back  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_DATA02  INPUT
*&---------------------------------------------------------------------*
*       Modification of Table Control's Current line .
*----------------------------------------------------------------------*
MODULE modify_data02 INPUT.
  MODIFY it_sys_message INDEX tc_app101_t-current_line.
ENDMODULE.                 " MODIFY_DATA02  INPUT
