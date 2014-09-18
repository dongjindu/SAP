*&---------------------------------------------------------------------*
*&  Include           ZEMM_CHANGE_PR_CLOSED_I01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.

  g_ok_code = sy-ucomm.
  CLEAR : sy-ucomm.

  g_save_ok = g_ok_code.

  CASE g_save_ok.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " EXIT_COMMAND  INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  g_ok_code = sy-ucomm.
  CLEAR : sy-ucomm.

  g_save_ok = g_ok_code.

  CASE g_save_ok.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0100  INPUT
