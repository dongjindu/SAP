*----------------------------------------------------------------------*
*   INCLUDE MZPP_APP601I01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_1000 INPUT.
  sv_code = ok_code.
  CLEAR: ok_code.
  CASE sv_code  .
    WHEN 'BACK'.
      CLEAR: WA_1000    .
      PERFORM check_exit.
      IF wa_end = 'X'.
        CLEAR: WA_END    .
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'SAVE'.
      CLEAR: wa_1000    .
      PERFORM save_data .
  ENDCASE.
ENDMODULE.                 "USER_COMMAND_1000  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  sv_code = ok_code.
  CLEAR: ok_code.
  CASE sv_code  .
    WHEN 'BACK'.
      PERFORM check_exit.
      IF wa_end = 'X'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'DELT'.
      wa_change = 'X'  .
      PERFORM delete_DATA .
    WHEN 'SAVE'.
      CLEAR: wa_change.
      PERFORM save_data .
    WHEN 'INST'.
      CALL SCREEN 1000  .
    WHEN 'REFRESH'.
      PERFORM REFRESH_DATA.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      PERFORM check_exit.
      IF wa_end = 'X'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFT_TC9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFT_TC9000 INPUT.
  MODIFY IT_CAR INDEX TC_9000-CURRENT_LINE.
ENDMODULE.                 " MODIFT_TC9000  INPUT
