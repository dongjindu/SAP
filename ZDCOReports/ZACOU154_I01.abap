*&---------------------------------------------------------------------*
*&  Include           ZACOU154_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE g_okcode.
*---BACK/CANCEL---*
    WHEN 'BACK' OR 'CANL'.
      LEAVE TO SCREEN 0.

*---EXIT---*
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
    g_save_ok = g_okcode.

  CLEAR g_okcode.

  CASE g_save_ok.
    WHEN 'SAVE'. "Create Delivery Document.
      PERFORM save_data.
      CALL METHOD g_grid->refresh_table_display.
      CALL METHOD cl_gui_cfw=>flush.
  ENDCASE.

  CLEAR g_save_ok.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
