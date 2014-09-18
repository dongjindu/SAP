*&---------------------------------------------------------------------*
*&  Include           ZHARC00700I001
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  OK_CODE = OKCODE.

  CASE SY-DYNNR.
    WHEN '0100'.
      CASE OK_CODE.
        WHEN 'AOBJ'.
          PERFORM CALL_TRANSACTION.
        WHEN 'PARA'.
          PERFORM CREATE_PARAMETER.
        WHEN 'CREA'.
          PERFORM CREATE_PROGRAM.
        WHEN 'ARIF'.
          PERFORM CREATE_FIELD_CATALOGS.
        WHEN 'DEIN'.
          PERFORM DEACTIVATE_INFOSTRUCTURE.
        WHEN 'MOIF'.
          PERFORM MODIFY_INFOSTRUCTURE.
      ENDCASE.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CANCEL  INPUT
*&---------------------------------------------------------------------*
MODULE CANCEL INPUT.
  OK_CODE = OKCODE.

  CASE SY-DYNNR.
    WHEN '0100'.
      CASE OK_CODE.
        WHEN 'CANC'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

ENDMODULE.                 " CANCEL  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.

  OK_CODE = OKCODE.

  CASE SY-DYNNR.
    WHEN '0100'.
      CASE OK_CODE.
        WHEN 'BACK' OR 'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
