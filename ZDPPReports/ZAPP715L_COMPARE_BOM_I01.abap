*----------------------------------------------------------------------*
*   INCLUDE ZAPP715L_COMPARE_BOM_I01                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN INPUT.
  PERFORM MODIFY_SCREEN.
ENDMODULE.                 " MODIFY_SCREEN  INPUT

*&---------------------------------------------------------------------*
*&      Module  SELECT_OPTIONS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SELECT_OPTIONS INPUT.
  PERFORM FILL_RANGES.

ENDMODULE.                 " SELECT_OPTIONS  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CURSOR_FIELD INPUT.
  CASE SY-DYNNR.
    WHEN '9000'.
      CLEAR: WA_TXT9000, WA_LINE9000.
      GET CURSOR FIELD WA_TXT9000 LINE WA_LINE9000.
    WHEN '9100'.
      CLEAR: WA_TXT9100, WA_LINE9100.
      GET CURSOR FIELD WA_TXT9100 LINE WA_LINE9100.
  ENDCASE.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK_CODE.
    WHEN 'PICK'.   "Display Change History
      PERFORM DISPLAY_CHANGE_HISTORY.
    WHEN 'ZRUN'.   "Batch Job
      PERFORM BATCH_JOB.
    WHEN 'INQR'.   "Inquiry
      PERFORM READ_DATA.
    WHEN 'VINQ'.   "display detailed VIN
      PERFORM DETAILED_DISPLAY.
    WHEN 'SDAT' OR 'SMAT' OR 'PSTA'.   "Selection Part No
      PERFORM MORE_ENTRIES USING SAVE_OK_CODE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_LOCK_OBJECT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_LOCK_OBJECT INPUT.
  PERFORM CHECK_LOCK_OBJECT.
ENDMODULE.                 " CHECK_LOCK_OBJECT  INPUT
