*----------------------------------------------------------------------*
***INCLUDE ZASD03L_RATIO_I_PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK_CODE.
    WHEN 'SAVE'.
      PERFORM SAVE_DATA.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZEF_FROM_TO  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_ZEF_FROM_TO INPUT.
  IF NOT ZTSD_RATIO_I-ZEFFM IS INITIAL.
    IF ZTSD_RATIO_I-ZEFFM > ZTSD_RATIO_I-ZEFTO.
      MESSAGE E000 WITH TEXT-M06.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_ZEF_FROM_TO  INPUT
