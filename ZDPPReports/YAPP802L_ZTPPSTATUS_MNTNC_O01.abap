*----------------------------------------------------------------------*
*   INCLUDE YAPP_ROUTING_O01                                           *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization OUTPUT.
  IF wa_init_flg IS INITIAL.
    wa_init_flg = 'X'.

    DELETE FROM ztpp_status WHERE id = space OR
                                  shop = 'T00TEST'.
    COMMIT WORK .

** Setting Parameters
*   ID
    CLEAR: wc_list, wc_value.
    name = 'P_ID'.
    PERFORM set_field_ID.
    PERFORM call_function_vrm USING ID_list.
*   Work Center
    CLEAR: wc_list, wc_value.
    name = 'P_WC'.
    PERFORM set_field_wc.
    PERFORM call_function_vrm USING wc_list.
*   Control Key
    CLEAR: ck_list, ck_value.
    name = 'P_CK'.
    PERFORM set_field_ck.
    PERFORM call_function_vrm USING ck_list.
*   Supply Area
    CLEAR: sa_list, sa_value.
    name = 'P_SA'.
    PERFORM set_field_sa.
    PERFORM call_function_vrm USING sa_list.
*   Sort String
    CLEAR: ss_list, ss_value.
    name = 'P_SS'.
    PERFORM set_field_ss.
    PERFORM call_function_vrm USING ss_list.
*   Backflush Point
    CLEAR: bf_list, bf_value.
    name = 'P_BF'.
    PERFORM set_field_bf.
    PERFORM call_function_vrm USING bf_list.
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
  DATA: l_line TYPE i.
  DESCRIBE TABLE IT_app802 LINES tc_app802-lines.
  SET CURSOR FIELD 'IT_APP802-ID' LINE wa_current_line .
ENDMODULE.                 " modify_lines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  LOOP AT SCREEN.
    IF ( wa_ins_flg = 'X' AND IT_app802-new = 'X' ) OR
       ( wa_upd_flg = 'X' AND IT_app802-mark = 'X'  AND
         screen-group1 <> 'GRP'                ) OR
         screen-name = 'IT_APP802-MARK' .
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
