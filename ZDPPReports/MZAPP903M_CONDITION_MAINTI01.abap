*----------------------------------------------------------------------*
***INCLUDE MZ903M_CODITION_MAINTI01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CALC'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  modify_tc9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_tc9000 INPUT.
  MODIFY it_plan_key INDEX tc_9000-current_line.
ENDMODULE.                 " modify_tc9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  DATA : l_confirm(1).

  sv_code = ok_code.
  CLEAR: ok_code.
  CASE sv_code   .
    WHEN 'LOAD'  .
      PERFORM read_condition.
    WHEN 'CHECK' .
    WHEN 'COPY'  OR 'UPDATE'  OR 'READ'.
      READ TABLE it_plan_key WITH KEY mark = 'X'.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING it_plan_key TO wa_plan .
        CALL SCREEN 9100 .
        PERFORM read_condition.
      ELSE.
      ENDIF.
    WHEN 'DELETE'.
      READ TABLE it_plan_key WITH KEY mark = 'X'.
      IF sy-subrc = 0.
        PERFORM dele_condition.
        PERFORM read_condition.
      ENDIF.
    WHEN 'RESET'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Reset'
          text_question         = text-005
          default_button        = '1'
          display_cancel_button = 'X'
        IMPORTING
          answer                = l_confirm
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK l_confirm = '1'.
      PERFORM reset_serial_no.

    WHEN 'NEW'   .
      CLEAR: wa_plan.
      CALL SCREEN 9100 .
      PERFORM read_condition.
    WHEN 'BACK'  .
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  sv_code = ok_code.
  CLEAR: ok_code.
  CASE sv_code.
    WHEN 'CHECK'.
      PERFORM check_condition .
    WHEN 'BACK'.
      IF wa_edit = 'X'.
        CLEAR: wa_flag.
        PERFORM confirm_change_data  USING wa_flag.
      ENDIF.
      IF wa_flag = 'X'.     " CANCEL
        EXIT.
      ENDIF.
      sv_code = wa_code .
      CLEAR: wa_code, wa_init, wa_plan, wa_flag.
      LEAVE TO SCREEN 0 .
    WHEN 'SAVE'.
      CLEAR: wa_edit.
      PERFORM save_plan_data .
    WHEN 'SERIAL'.
      PERFORM get_next_serial .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9100  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_KEY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_key INPUT.
  DATA: l_plan           LIKE ztpp_plan_key.

  CHECK wa_code = 'NEW' .

  SELECT SINGLE * INTO l_plan
    FROM ztpp_plan_key
   WHERE serial = wa_plan-serial.

  IF sy-subrc = 0.
    MESSAGE e001 WITH text-001 .
  ENDIF.
ENDMODULE.                 " CHECK_KEY  INPUT
