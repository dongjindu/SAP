*----------------------------------------------------------------------*
*   INCLUDE MZMMO_IF010I01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: lv_count TYPE i.
  CLEAR: lv_count, v_btn_flag.

  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'CHG'.
      v_btn_flag = 'X'.
*      CALL SCREEN '0200'.
    WHEN 'SAVE'.
      MODIFY ztmm_if024 FROM TABLE it_sub.
      IF sy-subrc EQ 0.
        MESSAGE s899(f2) WITH 'Change Success'.
        CLEAR gv_first.
      ENDIF.
    ENDCASE.
  ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  cancel  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cancel INPUT.
  CLEAR gv_first.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " cancel  INPUT
*&---------------------------------------------------------------------*
*&      Module  read_tc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_tc INPUT.
  MODIFY it_sub INDEX tc_list-current_line.
ENDMODULE.                 " read_tc  INPUT
