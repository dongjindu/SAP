*----------------------------------------------------------------------*
*   YAPP803L_ZTPPWOSUM_MNTNC_O01
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization OUTPUT.
  IF wa_init_flg IS INITIAL.
    wa_init_flg = 'X'.

** Setting Parameters
*   ID
    CLEAR: nation_list, nation_value.
    name = 'P_WO_SER'.
    PERFORM set_field_wo_ser.
    PERFORM call_function_vrm USING wo_ser_list.
*   Work Center
    CLEAR: nation_list, nation_value.
    name = 'P_NATION'.
    PERFORM set_field_nation.
    PERFORM call_function_vrm USING nation_list.
*   Control Key
    CLEAR: dealer_list, dealer_value.
    name = 'P_DEALER'.
    PERFORM set_field_dealer.
    PERFORM call_function_vrm USING dealer_list.
*   Supply Area
    CLEAR: extc_list, extc_value.
    name = 'P_EXTC'.
    PERFORM set_field_extc.
    PERFORM call_function_vrm USING extc_list.
*   Sort String
    CLEAR: intc_list, intc_value.
    name = 'P_INTC'.
    PERFORM set_field_intc.
    PERFORM call_function_vrm USING intc_list.
  ENDIF.
ENDMODULE.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.

ENDMODULE.                 " status_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_lines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_lines OUTPUT.
  DATA: l_lines(06),
        l_text(30),
        l_top_line(06),
        l_num TYPE n.
  DESCRIBE TABLE it_app803 LINES tc_app803-lines.
  MOVE: tc_app803-top_line TO l_top_line,
        tc_app803-lines to l_lines .
  CONCATENATE 'Entry'
              l_top_line
              'of'
              l_lines
  INTO l_text SEPARATED BY ' '.
  tf_count = l_text.
  SET CURSOR FIELD 'IT_APP803-WO_SER' LINE wa_current_line .
ENDMODULE.                 " modify_lines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  LOOP AT SCREEN.
    IF ( wa_ins_flg = 'X' AND it_app803-new = 'X' ) OR
       ( wa_upd_flg = 'X' AND it_app803-mark = 'X'  AND
         screen-group1 <> 'GRP'                ) OR
         screen-name = 'IT_APP803-MARK' .
      screen-input = '1'.
    ELSE.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR '100'.
ENDMODULE.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0120 output.
  SET PF-STATUS 'STATUS120'.
*  SET TITLEBAR 'xxx'.

endmodule.                 " STATUS_0120  OUTPUT
