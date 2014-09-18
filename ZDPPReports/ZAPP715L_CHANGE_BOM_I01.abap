*----------------------------------------------------------------------*
*   INCLUDE ZAPP715L_CHANGE_BOM_I01                                    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'ZDIS'.   " Display Screen
      IF sy-dynnr = '9000'.
        CLEAR: IT_DISP, IT_DISP[], TC_5000-LINES.
        CALL SCREEN 5000.
      ELSE.
        LEAVE TO SCREEN 9000.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  SELECT_OPTIONS1 INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE select_options1 INPUT.
  CLEAR: wa_dis1.
  IF ok_code = 'CHAN'.
    CLEAR: ok_code.
    MESSAGE w001 WITH text-201.
  ENDIF.
ENDMODULE.                 " SELECT_OPTIONS1 INPUT

*&---------------------------------------------------------------------*
*&      Module  SELECT_OPTIONS2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE select_options2 INPUT.
  CLEAR: wa_dis2.
  IF ok_code = 'CHAN'.
    CLEAR: ok_code.
    MESSAGE w001 WITH text-201.
  ENDIF.
ENDMODULE.                 " SELECT_OPTIONS2 INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor_field INPUT.
  CLEAR: wa_txt9000, wa_line9000.
  GET CURSOR FIELD wa_txt9000 LINE wa_line9000.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  DATA: l_line           LIKE sy-lilli.

  save_ok_code = ok_code.
  CLEAR ok_code.
  CASE save_ok_code.
    WHEN 'ZFIND'.
      " As the Find Result...  Flag set (WA_DIS1)
      PERFORM read_plan_order  .
      DESCRIBE TABLE it_screen LINES wa_total.
      tc_9000-lines = wa_total.
    WHEN 'ZECM'.
      PERFORM check_ecmno      .
    WHEN 'ZSEL'.   "Select
      PERFORM select_data.
    WHEN 'DSEL'.   "Deselect
      PERFORM deselect_data.
    WHEN 'ZMD13'.  " Navigate to MD13.. (Plan Order)
      GET CURSOR LINE  l_line.
      l_line = tc_9000-top_line + l_line - 1.
      READ TABLE it_screen INDEX l_line.
      SET PARAMETER ID 'PAF'    FIELD it_screen-plnum.
      CALL TRANSACTION 'MD13'   AND SKIP FIRST SCREEN.
*   WHEN 'ZMM03'.  " Navigate to MM03.. (Material Master).
*     GET CURSOR FIELD it_screen-worder.
    WHEN 'CHAN'.   "Change
      PERFORM change_process.
  ENDCASE.

  CLEAR: wa_check.
  LOOP AT it_screen WHERE chk = 'X'.
    wa_check = wa_check + 1 .
  ENDLOOP.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5000 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CASE save_ok_code.
    WHEN 'ZFIND'.
      " As the Find Result...  Flag set (WA_DIS1)
      PERFORM read_history     .
      DESCRIBE TABLE it_disp   LINES tc_5000-lines.
    WHEN 'ASRT' .
      PERFORM SORT_PROCESS     USING 'A' .
    WHEN 'DSRT' .
      PERFORM SORT_PROCESS     USING 'D' .
    WHEN 'ZRES' .  " Return to the Main Screen..
      Leave to screen 9000.
    WHEN 'ZMD13'.  " Navigate to MD13.. (Plan Order)
      GET CURSOR LINE  l_line.
      l_line = tc_5000-top_line + l_line - 1.
      READ TABLE it_DISP INDEX l_line.
      SET PARAMETER ID 'PAF'    FIELD it_DISP-plnum.
      CALL TRANSACTION 'MD13'   AND SKIP FIRST SCREEN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_5000  INPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen INPUT.
  MODIFY it_screen INDEX tc_9000-current_line.
ENDMODULE.                 " MODIFY_SCREEN  INPUT

*&---------------------------------------------------------------------*
*&      Module  MATNR_VALUE_REQUEST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE matnr_value_request INPUT.
  PERFORM matnr_value_request USING 'MATNR'.
ENDMODULE.                 " MATNR_VALUE_REQUEST  INPUT

*&---------------------------------------------------------------------*
*&      Module  VERID_VALUE_REQUEST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verid_value_request INPUT.
  PERFORM matnr_value_request USING 'VERID'.
ENDMODULE.                 " VERID_VALUE_REQUEST  INPUT

*&---------------------------------------------------------------------*
*&      Module  AENNR_VALUE_REQUEST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE AENNR_value_request INPUT.
  PERFORM AENNR_value_request USING 'AENNR'.
ENDMODULE.                 " MATNR_VALUE_REQUEST  INPUT

*&---------------------------------------------------------------------*
*&      Module  IDNRK_VALUE_REQUEST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IDNRK_value_request INPUT.
  PERFORM AENNR_value_request USING 'MATNR'.
ENDMODULE.                 " IDNRK_VALUE_REQUEST  INPUT
