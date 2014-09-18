*&---------------------------------------------------------------------*
*&  Include           ZRDA_QA_DOC_ARCH_I01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back_exit_0210 INPUT.

  LEAVE PROGRAM.

ENDMODULE.                 " BACK_EXIT_0210  INPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0210 INPUT.

  CLEAR: g_flg.  "Input field data check flag
  CLEAR: save_ok.

  save_ok = ok_code.
  CASE ok_code .
    WHEN 'ONLY'.
      PERFORM only_check.
    WHEN 'SAVE'.
      g_flg1 = '1'.
      PERFORM duplicate_check USING g_flg1.
      IF g_flg1 = 'E'.
        EXIT.
      ENDIF.
      CLEAR: ok_code, is_buyback_s.
      is_buyback_s = is_buyback.
      PERFORM input_data_check.
      IF g_flg IS NOT INITIAL.
        EXIT.
      ENDIF.

      IF g_postid_o IS NOT INITIAL.
        g_flg = 'R'.
      ENDIF.

      IF g_flg1 = 'C'.  "Add before Update  status = 'D'
        PERFORM modify_post_data.
      ENDIF.
      PERFORM save_data_buyback .

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0210  INPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0220 INPUT.

  CLEAR: g_flg.  "Input field data check flag

  CASE ok_code .
    WHEN 'ONLY'.
      PERFORM only_check.
    WHEN 'SAVE'.
      g_flg1 = '2'.
      PERFORM duplicate_check USING g_flg1.
      IF g_flg1 = 'E'.
        EXIT.
      ENDIF.

      CLEAR: ok_code, is_legal_s.
      MOVE-CORRESPONDING is_buyback TO is_legal.
      is_legal-last_name  = g_lname.
      is_legal-first_name = g_fname.
      is_legal-title      = g_title.
      is_legal_s          = is_legal.

      PERFORM input_legal_check.
      IF g_flg IS NOT INITIAL.
        EXIT.
      ENDIF.

      IF g_postid_o IS NOT INITIAL.
        g_flg = 'R'.
      ENDIF.

      IF g_flg1 = 'C'.  "Add before Update  status = 'D'
        PERFORM modify_post_data.
      ENDIF.
      PERFORM save_data_legal.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0220  INPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0230  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0230 INPUT.

  CLEAR: g_month, g_cmonth.
  g_month = g_cmonth = ztda_posts-post_month.


  CLEAR: g_flg.  "Input field data check flag

  CASE ok_code .
    WHEN 'SAVE'.
      IF g_proc = 'C'.
        g_flg1 = '3'.
      ELSE.
        g_flg1 = '4'.
      ENDIF.
      CLEAR: is_buyback-acl_no, is_buyback-vin_no.
      PERFORM duplicate_check USING g_flg1.
      IF g_flg1 = 'E'.
        EXIT.
      ENDIF.
      CLEAR: g_flg, ok_code.
      PERFORM input_data_check_claim.
      IF g_flg IS NOT INITIAL.
        EXIT.
      ENDIF.
*
      IF g_postid_o IS NOT INITIAL.
        g_flg = 'R'.
      ENDIF.
      IF g_flg1 = 'C'.  "Add before Update  status = 'D'
        PERFORM modify_post_data.
      ENDIF.
      PERFORM save_data_claim .

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0230  INPUT



*&---------------------------------------------------------------------*
*&      Module  GET_SPMON  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_spmon INPUT.

  DATA: lv_spmon TYPE spmon.

  MOVE: sy-datum(6) TO lv_spmon.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month               = lv_spmon
    IMPORTING
      selected_month             = ztda_posts-post_month
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      month_not_found            = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.                 " GET_SPMON  INPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0240  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0240 INPUT.

  CLEAR: g_flg, g_lifnr, g_name1.  "Input field data check flag
  CLEAR: save_ok.

  g_lifnr = lfa1-lifnr.
  save_ok = ok_code.
  CASE ok_code .
    WHEN ' '.
      IF lfa1-lifnr IS NOT INITIAL.
        PERFORM input_data_check_vendor.
        IF g_flg IS NOT INITIAL.
          EXIT.
        ELSE.
          g_name1 = lfa1-name1.
        ENDIF.
      ENDIF.

    WHEN 'SAVE'.
      g_flg1 = '5'.
***      PERFORM duplicate_check_vendor USING g_flg1.
      PERFORM duplicate_check USING g_flg1.
      IF g_flg1 = 'E'.
        EXIT.
      ENDIF.
      PERFORM input_data_check_vendor.
      IF g_flg IS NOT INITIAL.
        EXIT.
      ELSE.
        g_name1 = lfa1-name1.
      ENDIF.

*****      PERFORM checkbox_check USING g_flg1.
*****      IF g_flg1 = 'E'.
*****        MESSAGE s999 WITH 'Always check for one thing.'
*****        DISPLAY LIKE 'E'.
*****        g_flg = 'E'.
*****        EXIT.
*****      ENDIF.
*****
*****      IF g_postid_o IS NOT INITIAL.
*****        g_flg = 'R'.
*****      ENDIF.

      IF g_flg1 = 'C'.  "Add before Update  status = 'D'
        PERFORM modify_post_data.
      ENDIF.
      PERFORM save_data_vendor.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0240  INPUT
