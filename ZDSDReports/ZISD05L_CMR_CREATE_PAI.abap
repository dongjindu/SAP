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
          LEAVE TO SCREEN 0.
        WHEN 'EXIT'.
          LEAVE TO SCREEN 0.
        WHEN 'CANC'.
          LEAVE TO SCREEN 0.
      ENDCASE.
    WHEN '9100'.
      CASE SAVE_OK_CODE.
        WHEN 'BACK'.
          LEAVE TO SCREEN 0.
        WHEN 'EXIT'.
          LEAVE TO SCREEN 0.
        WHEN 'CANC'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK_CODE.
    WHEN 'CMR'.
      PERFORM PROCESS_CMR.
    WHEN 'CMRC'.
      PERFORM PROCESS_CMR_CANC.
    WHEN 'CM'.
      PERFORM PROCESS_CM.
    WHEN 'CMA'.
      PERFORM PROCESS_CM_ADJ.
    WHEN 'EDIT'.
      PERFORM PROCESS_EDIT.
    WHEN 'PRIN'.
      PERFORM PROCESS_PRIN.
    WHEN 'REMI'.
      PERFORM PROCESS_REMI.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK_CODE.
    WHEN 'SAVE'.
      PERFORM PROCESS_SAVE.
    WHEN 'DEL'.
      PERFORM PROCESS_DEL.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SHOW_CMR  INPUT
*&---------------------------------------------------------------------*
MODULE SHOW_CMR INPUT.
  PERFORM SHOW_CLICKED_CMR_DOC.
ENDMODULE.                 " SHOW_CMR  INPUT
*&---------------------------------------------------------------------*
*&      Module  SHOW_CM  INPUT
*&---------------------------------------------------------------------*
MODULE SHOW_CM INPUT.
  PERFORM SHOW_CLICKED_CM_DOC.
ENDMODULE.                 " SHOW_CM  INPUT




















************************************************************************
***INCLUDE ZISD05L_CMR_CREATE_PAI .
* INPUT MODULE FOR TABLECONTROL 'TC_9000': MARK TABLE
MODULE TC_9000_MARK INPUT.
  MODIFY G_TC_9000_ITAB
    FROM G_TC_9000_WA
    INDEX TC_9000-CURRENT_LINE
    TRANSPORTING FLAG.
ENDMODULE.

* INPUT MODULE FOR TABLECONTROL 'TC_9000': PROCESS USER COMMAND
MODULE TC_9000_USER_COMMAND INPUT.
***  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TC_9000'
                              'G_TC_9000_ITAB'
                              'FLAG'
                     CHANGING OK_CODE.
ENDMODULE.
