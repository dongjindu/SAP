*----------------------------------------------------------------------*
*   INCLUDE YAPP101L_ALC_REP_PBO                                       *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Some parameters.
*----------------------------------------------------------------------*
MODULE initialization OUTPUT.
  IF wa_init_flg IS INITIAL.
    wa_init_flg = 'X'.

**Setting Parameters
*   Company Code.
    p_company = 'HMMA'.
*   Work Order = W/O Serial + Nation + Dealer.
    CLEAR: workorder_list, workorder_value.
    name = 'P_WORKORDER'.
    PERFORM set_field.
    PERFORM call_func_vrm USING workorder_list.
*   Request Date.
    p_reqdate_st = sy-datum.
    p_reqdate_en = sy-datum.
*
*
*    MOVE 'E0208A038' TO it_app101-wo_ser.
*    MOVE 'B28' TO it_app101-nation.
*    MOVE 'AA' TO it_app101-dealer.
*    MOVE ' ' TO it_app101-extc.
*    MOVE ' ' TO it_app101-intc.
*    MOVE ' ' TO it_app101-moye.
*    MOVE ' ' TO it_app101-bmdl.
*    MOVE ' ' TO it_app101-ocnn.
*    MOVE ' ' TO it_app101-vers.
*    MOVE '1' TO it_app101-initqty.
*    MOVE '1' TO it_app101-modqty.
*    MOVE '20000101' TO it_app101-req_date.
*    MOVE '20000101' TO it_app101-crt_date.
*    MOVE '20000101' TO it_app101-chg_date.
*    MOVE ' ' TO it_app101-dest.
*    MOVE ' ' TO it_app101-lcnt.
*    MOVE ' ' TO it_app101-lcno.
*    MOVE ' ' TO it_app101-regn.
*    MOVE ' ' TO it_app101-orzn.
*    MOVE ' ' TO it_app101-s219.
*    APPEND it_app101.
*    MOVE 'E0208A038' TO it_app101-wo_ser.
*    MOVE 'B28' TO it_app101-nation.
*    MOVE 'AA' TO it_app101-dealer.
*    MOVE '***' TO it_app101-extc.
*    MOVE '***' TO it_app101-intc.
*    MOVE ' ' TO it_app101-moye.
*    MOVE ' ' TO it_app101-bmdl.
*    MOVE ' ' TO it_app101-ocnn.
*    MOVE ' ' TO it_app101-vers.
*    MOVE '1' TO it_app101-initqty.
*    MOVE '1' TO it_app101-modqty.
*    MOVE '20000101' TO it_app101-req_date.
*    MOVE '20000101' TO it_app101-crt_date.
*    MOVE '20000101' TO it_app101-chg_date.
*    MOVE ' ' TO it_app101-dest.
*    MOVE ' ' TO it_app101-lcnt.
*    MOVE ' ' TO it_app101-lcno.
*    MOVE ' ' TO it_app101-regn.
*    MOVE ' ' TO it_app101-orzn.
*    MOVE ' ' TO it_app101-s219.
*    APPEND it_app101.
*
*
  ENDIF.

ENDMODULE.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0110  OUTPUT
*&---------------------------------------------------------------------*
*       Setting PF Status & Title Bar
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.
  SET PF-STATUS 'STATUS110'.
  SET TITLEBAR 'APP101'.

ENDMODULE.                 " status_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Table Control's line number
*----------------------------------------------------------------------*
MODULE modify_lines OUTPUT.
  DATA: l_line TYPE i.
  DESCRIBE TABLE it_app101 LINES l_line.
  tc_app101-lines = l_line.

ENDMODULE.                 " MODIFY_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Setting PF Status & Title Bar
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS110'.
  SET TITLEBAR 'APP101'.

ENDMODULE.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0111  OUTPUT
*&---------------------------------------------------------------------*
*       Setting PF Status & Table Control's Line number
*----------------------------------------------------------------------*
MODULE status_0111 OUTPUT.
  SET PF-STATUS 'STATUS0111'.
*  SET TITLEBAR 'xxx'.
  DESCRIBE TABLE it_sys_message LINES l_line.
  tc_app101_t-lines = l_line.


ENDMODULE.                 " STATUS_0111  OUTPUT
