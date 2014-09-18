*&---------------------------------------------------------------------*
*&  Include           ZRDA_QA_DOC_ARCHR_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back_exit_0210 INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " BACK_EXIT_0210  INPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0210 INPUT.

* Input Condition check
  CLEAR: g_flg.
  PERFORM input_data_check USING g_flg.
  IF g_flg IS NOT INITIAL.
    EXIT.
  ENDIF.

* Data read
  PERFORM buyback_data_get USING g_flg.
  IF g_flg IS NOT INITIAL.
    EXIT.
  ENDIF.

  IF it_data[] IS NOT INITIAL.
    CALL SCREEN 215.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0210  INPUT



*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0215  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0215 INPUT.

  CASE g_proc.
    WHEN 'B'.
      CLEAR: is_buyback-acl_no, is_buyback-vin_no, g_title.
    WHEN 'L'.
      CLEAR: is_buyback-acl_no, is_buyback-vin_no,
             g_title, g_lname, g_fname.
    WHEN 'C' OR 'R'.
      CLEAR: g_year, g_title.
    WHEN 'V'.
      CLEAR: g_first, g_title, lfa1-lifnr, lfa1-name1.
  ENDCASE.
  LEAVE TO SCREEN 0.

ENDMODULE.                 " EXIT_COMMAND_0215  INPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0217  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0217 INPUT.

  DATA lv_flag            TYPE c.
  CASE sy-ucomm.
    WHEN 'SAVE'.
      CLEAR: gv_msg,   g_answer.
***      CASE g_proc.
***        WHEN 'B'. "Buyback
***          gv_msg = 'Buyback data save confirm'.
***        WHEN 'L'. "Legaal
***          gv_msg = 'Legaal data save confirm'.
***        WHEN 'C'. "Claim
***          gv_msg = 'Claim data save confirm'.
***        WHEN 'R'. "Reclaim
***          gv_msg = 'Reclaim data save confirm'.
***        WHEN 'V'. "Vendor Files
***          gv_msg = 'Vendor Files data save confirm'.
***      ENDCASE.
      gv_msg = 'Save Confirm'.

      PERFORM confirm_job  USING g_answer
                                 text-m07.
      IF g_answer  =  c_1.
        CASE g_proc.
          WHEN 'B'. "Buyback
            IF is_buyback-acl_no NE is_data-acl_no OR
               is_buyback-vin_no NE is_data-vin_no OR
               g_title           NE is_data-post_title.
* 20140609 Add s
* Check ACL & Bin
              IF is_buyback-acl_no NE is_data-acl_no.
                lv_flag = 'A'.
                PERFORM input_data_check_re USING lv_flag.
                IF lv_flag IS NOT INITIAL.
                  EXIT.
                ENDIF.
              ENDIF.

              IF is_buyback-vin_no NE is_data-vin_no.
                lv_flag = 'V'.
                PERFORM input_data_check_re USING lv_flag.
                IF lv_flag IS NOT INITIAL.
                  EXIT.
                ENDIF.
              ENDIF.
* 20140609 Add e
              is_data-acl_no     =  is_buyback-acl_no.
              is_data-vin_no     =  is_buyback-vin_no.
              is_data-post_title =  g_title.
              it_data-acl_no     =  is_buyback-acl_no.
              it_data-vin_no     =  is_buyback-vin_no.
              it_data-post_title =  g_title.
              MODIFY it_data INDEX gs_selected_row-row_id
                     TRANSPORTING acl_no vin_no post_title.
              PERFORM ztda_posts_change.
            ENDIF.
          WHEN 'L'. "Legaal
            IF is_buyback-acl_no NE is_data-acl_no     OR
               is_buyback-vin_no NE is_data-vin_no     OR
               g_title           NE is_data-post_title OR
               g_lname           NE is_data-last_name  OR
               g_fname           NE is_data-first_name.
* 20140609 Add s
* Check ACL & Bin
              IF is_buyback-acl_no NE is_data-acl_no.
                lv_flag = 'A'.
                PERFORM input_data_check_re USING lv_flag.
                IF lv_flag IS NOT INITIAL.
                  EXIT.
                ENDIF.
              ENDIF.

              IF is_buyback-vin_no NE is_data-vin_no.
                lv_flag = 'V'.
                PERFORM input_data_check_re USING lv_flag.
                IF lv_flag IS NOT INITIAL.
                  EXIT.
                ENDIF.
              ENDIF.
* 20140609 Add e

              is_data-acl_no     =  is_buyback-acl_no.
              is_data-vin_no     =  is_buyback-vin_no.
              is_data-last_name  =  g_lname.
              is_data-first_name =  g_fname.
              is_data-post_title =  g_title.
              it_data-acl_no     =  is_buyback-acl_no.
              it_data-vin_no     =  is_buyback-vin_no.
              it_data-last_name  =  g_lname.
              it_data-first_name =  g_fname.
              it_data-post_title =  g_title.
              MODIFY it_data INDEX gs_selected_row-row_id
                     TRANSPORTING acl_no vin_no last_name first_name
                                  post_title.
              PERFORM ztda_posts_change.
            ENDIF.
          WHEN 'C' OR 'R'. "Claim & Reclaim
            IF ztda_posts-post_month NE is_data-post_month     OR
               g_title               NE is_data-post_title.
              is_data-post_author =  sy-uname.
              it_data-post_author =  sy-uname.
              is_data-post_month  =  ztda_posts-post_month.
              it_data-post_month  =  ztda_posts-post_month.
              is_data-post_title  =  g_title.
              it_data-post_title  =  g_title.
              MODIFY it_data INDEX gs_selected_row-row_id
                     TRANSPORTING post_author post_month post_title.
              PERFORM ztda_posts_change.
            ENDIF.
          WHEN 'V'. "Vendor Files
            IF g_title               NE is_data-post_title.
              is_data-post_author =  sy-uname.
              it_data-post_author =  sy-uname.
              is_data-post_title  =  g_title.
              it_data-post_title  =  g_title.
              MODIFY it_data INDEX gs_selected_row-row_id
                     TRANSPORTING post_author post_title.
              PERFORM ztda_posts_change.
            ENDIF.
        ENDCASE.
        MESSAGE s999 WITH text-m10.      "Data has been saved successfully!
      ENDIF.

  ENDCASE.
  LEAVE TO SCREEN 0.

ENDMODULE.                 " USER_COMMAND_0217  INPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0220 INPUT.

* Input Condition check
  CLEAR: g_flg.
  PERFORM input_data_check_legal USING g_flg.
  IF g_flg IS NOT INITIAL.
    EXIT.
  ENDIF.

* Data read
  PERFORM legal_data_get USING g_flg.
  IF g_flg IS NOT INITIAL.
    EXIT.
  ENDIF.

  IF it_data[] IS NOT INITIAL.
    CALL SCREEN 215.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0220  INPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0230  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0230 INPUT.

* Input Condition check
  CLEAR: g_flg.
  PERFORM input_data_check_claim USING g_flg.
  IF g_flg IS NOT INITIAL.
    EXIT.
  ENDIF.

* Data read
  PERFORM claim_data_get USING g_flg.
  IF g_flg IS NOT INITIAL.
    EXIT.
  ENDIF.

  IF it_data[] IS NOT INITIAL.
    CALL SCREEN 215.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0230  INPUT



*&---------------------------------------------------------------------*
*&      Module  GET_YEAR  INPUT
*&---------------------------------------------------------------------*
*       text  : Year possible entry function call
*----------------------------------------------------------------------*
MODULE get_year INPUT.

  CALL FUNCTION 'REAL_ESTATE_F4_YEAR'
*    EXPORTING
*      i_year        = '2014'
*     I_POPUP_TITLE = ' '
    IMPORTING
      e_year        = g_year
*     E_SEL_OK      =
    .

ENDMODULE.                 " GET_YEAR  INPUT



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0250  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0250 INPUT.
* Input Condition check
  CLEAR: g_flg.
  PERFORM input_data_check_vondor USING g_flg.
  IF g_flg IS NOT INITIAL.
    EXIT.
  ENDIF.

* Data read
  PERFORM venfor_data_get USING g_flg.
  IF g_flg IS NOT INITIAL.
    EXIT.
  ENDIF.

  IF it_data[] IS NOT INITIAL.
    CALL SCREEN 215.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0250  INPUT
