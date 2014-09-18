*&---------------------------------------------------------------------*
*&  Include           ZGHRR00100I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      g_r_grid->free( ).
      LEAVE PROGRAM.
    WHEN 'CANC'.
      g_r_grid->free( ).
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'REFR'.
      PERFORM get_data.
    WHEN 'EDIT'.
      PERFORM get_select_data.
      CHECK g_selidx IS NOT INITIAL.
      PERFORM call_transaction.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
