*----------------------------------------------------------------------*
***INCLUDE ZASD03L_VEN_WC_PAI .
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
*&      Module  CHECK_ZMY_FROM_TO_PRC  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_ZMY_FROM_TO_PRC INPUT.
  IF NOT ZTSD_PART_PRC-ZMYFM IS INITIAL.
    IF ZTSD_PART_PRC-ZMYFM > ZTSD_PART_PRC-ZMYTO.
      MESSAGE E000 WITH TEXT-M06.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_ZMY_FROM_TO_PRC  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZEF_FROM_TO_PRC  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_ZEF_FROM_TO_PRC INPUT.
  IF NOT ZTSD_PART_PRC-ZEFFM IS INITIAL.
    IF ZTSD_PART_PRC-ZEFFM > ZTSD_PART_PRC-ZEFTO.
      MESSAGE E000 WITH TEXT-M06.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_ZEF_FROM_TO_PRC  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZEF_FROM_TO_HST  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_ZEF_FROM_TO_HST INPUT.
  IF NOT ZTSD_PART_HST-ZEFFM IS INITIAL.
    IF ZTSD_PART_HST-ZEFFM > ZTSD_PART_HST-ZEFTO.
      MESSAGE E000 WITH TEXT-M06.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_ZEF_FROM_TO_HST  INPUT
