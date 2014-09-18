*----------------------------------------------------------------------*
***INCLUDE ZRSD05R_ORDER_PRICE_LIST_PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SY-DYNNR.
    WHEN '9000'.
      CASE SAVE_OK_CODE.
        WHEN 'BACK'.
          SET SCREEN 0.
        WHEN 'EXIT'.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          SET SCREEN 0.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
