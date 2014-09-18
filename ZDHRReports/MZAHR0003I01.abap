*----------------------------------------------------------------------*
*   INCLUDE MZAHR0003I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE BACK_EXIT INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXEC'.
      PERFORM SELECT_DATA.
      PERFORM SAVE_DATA.
  ENDCASE.
*
  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
