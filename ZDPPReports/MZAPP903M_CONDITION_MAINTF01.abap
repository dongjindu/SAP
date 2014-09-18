*----------------------------------------------------------------------*
***INCLUDE MZ903M_CODITION_MAINTF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_condition.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_plan_key
    FROM ztpp_plan_key    .
*   WHERE DEL_FLAG = SPACE .
  DESCRIBE TABLE it_plan_key LINES tc_9000-lines.
  SORT it_plan_key BY serial .
ENDFORM.                    " READ_CONDITION

*&---------------------------------------------------------------------*
*&      Form  CONFIRM_CHANGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FLAG  text
*----------------------------------------------------------------------*
FORM confirm_change_data USING    pa_flag.

ENDFORM.                    " CONFIRM_CHANGE_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_NEXT_SERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_next_serial.
  SELECT MAX( serial ) INTO wa_plan-serial
    FROM ztpp_plan_key .

  wa_plan-serial = wa_plan-serial + 1 .
ENDFORM.                    " GET_NEXT_SERIAL

*&---------------------------------------------------------------------*
*&      Form  SAVE_PLAN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_plan_data.
  wa_plan-udate  = sy-datum.
  wa_plan-utime  = sy-uzeit.
  wa_plan-uuser  = sy-uname.
  MODIFY ztpp_plan_key FROM wa_plan .
  IF sy-subrc NE 0.
    MESSAGE i001 WITH text-002 sy-subrc.
  ENDIF.
ENDFORM.                    " SAVE_PLAN_DATA

*&---------------------------------------------------------------------*
*&      Form  DELE_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dele_condition.
  MOVE-CORRESPONDING it_plan_key  TO  wa_plan.
  DELETE FROM ztpp_plan_key WHERE serial = wa_plan-serial.
  IF sy-subrc NE 0.
    MESSAGE i001 WITH text-003 sy-subrc .
  ENDIF.
ENDFORM.                    " DELE_CONDITION

*&---------------------------------------------------------------------*
*&      Form  CHECK_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_condition.
  DATA: lt_all         LIKE TABLE OF zspp_master_asup WITH HEADER LINE,
        lt_set         LIKE TABLE OF zspp_master_asup WITH HEADER LINE,
        lt_cond        LIKE TABLE OF zspp_condition   WITH HEADER LINE.

  lt_cond-string = wa_plan-cond.   APPEND lt_cond.
  CALL FUNCTION 'Z_FPP_COMPILE_CONDITION'
    EXPORTING
      i_cond                = lt_cond-string
      i_check               = 'X'
*   IMPORTING
*     O_SUCCESS             =
*     O_FAIL                =
    TABLES
      t_worder              = lt_set
      t_cond                = lt_cond
    EXCEPTIONS
      condition_error       = 1
      err_paren             = 2
      err_operation         = 3
      err_relation          = 4
      err_values            = 5
      err_fields            = 6
      OTHERS                = 7 .

  IF sy-subrc <> 0.
    CASE sy-subrc .
      WHEN 1 .
        MESSAGE e001 WITH text-011 .
      WHEN 2 .
        MESSAGE e001 WITH text-012 .
      WHEN 3 .
        MESSAGE e001 WITH text-013 .
      WHEN 4 .
        MESSAGE e001 WITH text-014 .
      WHEN 5 .
        MESSAGE e001 WITH text-015 .
      WHEN 6 .
        MESSAGE e001 WITH text-016 .
      WHEN 7 .
        MESSAGE e001 WITH text-017 .
    ENDCASE.
  ELSE.
    MESSAGE s001 WITH text-010 .
  ENDIF.
ENDFORM.                    " CHECK_CONDITION
*&---------------------------------------------------------------------*
*&      Form  RESET_SERIAL_NO
*&---------------------------------------------------------------------*
FORM reset_serial_no .
  DATA : l_error_chk.

  CLEAR : l_error_chk.

  CHECK   it_plan_key[] IS NOT INITIAL.
  SORT it_plan_key BY serial .

  LOOP AT it_plan_key.
    it_plan_key-serial  = sy-tabix * 10.
    MODIFY it_plan_key  FROM it_plan_key  TRANSPORTING serial.
  ENDLOOP.

  DELETE FROM ztpp_plan_key.
  IF sy-subrc <> 0.
    l_error_chk = 'X'.
  ENDIF.

  INSERT ztpp_plan_key FROM TABLE  it_plan_key.
  IF sy-subrc <> 0.
    l_error_chk = 'X'.
  ENDIF.

  IF  l_error_chk = 'X'.
    ROLLBACK WORK.
    MESSAGE w000 WITH 'Serial No Reset Error'.
  ELSE.
    COMMIT WORK.
    MESSAGE s000 WITH 'Updated Serial No Reset successfully'.
  ENDIF.
ENDFORM.                    " RESET_SERIAL_NO
