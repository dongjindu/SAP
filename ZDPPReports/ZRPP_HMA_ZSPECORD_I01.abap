*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_I01                                        *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* MODULE  USER_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
* TEXT :
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA lv_cnt TYPE p.
  DATA lv_ans(1).
  DATA : chk_nonspec(1), lv_text1(70).

  DATA : lt_edidd LIKE TABLE OF edidd WITH HEADER LINE,
         lt_edidc LIKE TABLE OF edidc.

  CLEAR ok_code.
  ok_code = sy-ucomm.

  CASE ok_code.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'PROC'.
*      PERFORM pro_apply_change.    "Victor 06.29.2011

      READ TABLE gt_data WITH KEY select = 'X'.  "Victor 06.27.2011
*      IF gt_data-status IS INITIAL.
      IF sy-subrc  = 0.

*        LOOP AT gt_data.
        CLEAR : chk_nonspec.
        LOOP AT gt_data WHERE select = 'X'.  "Victor 06.27.2011
          IF gt_data-check = ''.
            chk_nonspec = 'X'.        "Non Standard Spec Order
          ENDIF.

          IF NOT gt_data-status IS INITIAL AND
                          gt_data-status   = '@0A@'.
            IF gt_data-check = 'X'.
              MESSAGE e001 WITH 'Please Check Work Order Spec'.
            ELSE.
              MESSAGE e001 WITH 'Not a Spec Work Order.'.
            ENDIF.
          ELSEIF NOT gt_data-status IS INITIAL.
            MESSAGE e001 WITH 'Data has already been Sent.'.
          ENDIF.

          IF gt_data-bmdl IS INITIAL OR gt_data-s219 IS INITIAL.
            MESSAGE e001 WITH 'Some Interface data is missing'
                              ' Model Index or 219 Option'.
          ENDIF.

*-<      Color conversion  2 -> 3 digit Victor 02.17.2012
          IF  gt_data-nation  <> 'B28'.
           perform pro_convert_color.
          ENDIF.
*->
          MOVE-CORRESPONDING gt_data TO wa_data. "Victor 06.30.2011
          lt_edidd-segnam = c_zspseg.
*          lt_edidd-sdata  = gt_data.
          lt_edidd-sdata  = wa_data.             "Victor 06.30.2011

          APPEND lt_edidd.
          CLEAR: lt_edidd.
        ENDLOOP.

        IF  chk_nonspec = 'X'.
          CONCATENATE 'This is non-standard'
              '(not E*Z) spec order.' INTO lv_text1 SEPARATED BY space.
        ELSE.
          lv_text1  =   text-m01.
        ENDIF.
        PERFORM popup_to_confirm USING  'S'
                                          'Confirm'
                                          lv_text1
                                          text-m02
                                          text-m03
                                 CHANGING lv_ans.

        CHECK lv_ans EQ 'J'.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            text   = 'IDOC Generating....'
          EXCEPTIONS
            OTHERS = 1.

        PERFORM  p3000_send_idoc TABLES lt_edidd lt_edidc.
        PERFORM  refresh_display.
      ELSE.
        MESSAGE i001 WITH
        'Please Select line(s)'.
      ENDIF.
    WHEN 'LOG'.

      READ TABLE gt_data INDEX 1.
      CHECK NOT gt_data-status IS INITIAL.
      PERFORM call_log_prg .
  ENDCASE.

ENDMODULE.                 " user_command_0100  INPUT

*----------------------------------------------------------------------*
* MODULE  EXIT  INPUT
*----------------------------------------------------------------------*
* TEXT :
*----------------------------------------------------------------------*
MODULE exit INPUT.
*  PERFORM P1110_DESTROY_OBJECT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " exit  INPUT
