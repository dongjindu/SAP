*&---------------------------------------------------------------------*
*&  Include           ZHACR01000I001
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  OK_CODE = OKCODE.

  CASE SY-DYNNR.
    WHEN '0100'.
      CASE OK_CODE.
        WHEN 'DISP'.
          PERFORM DATA_PROCESSING.
        WHEN 'RESU'.
          PERFORM CALL_TRANSACTION_ZHACR01100.
        WHEN 'ASMA'.
          PERFORM CALL_TRANSACTION_ZHACR01200.
        WHEN 'SARA'.
          PERFORM CALL_TRANSACTION_SARA.
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
