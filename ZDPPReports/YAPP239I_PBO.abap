*----------------------------------------------------------------------*
*   INCLUDE YAPP239L_PBO                                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'APP239'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters
*----------------------------------------------------------------------*
MODULE initialization OUTPUT.
  IF wa_init_flg IS INITIAL.
    wa_init_flg = 'X'.

**Setting Parameters
*   Company Code
    p_company = 'HMMA'.
*   Model(Car Type)
    clear: it_model_list, wa_model_value.
    name = 'P_MODEL'.
    perform set_field_model.
    perform call_function_vrm using it_model_list.
*   Date
    p_cdate_st = sy-datum.
    p_cdate_en = sy-datum.

  ENDIF.
ENDMODULE.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_sreen  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Screen
*----------------------------------------------------------------------*
MODULE modify_sreen OUTPUT.
  MODIFY SCREEN.
ENDMODULE.                 " modify_sreen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_line  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Table Control's total line number
*----------------------------------------------------------------------*
MODULE modify_lines OUTPUT.
  DATA: l_line TYPE i.
  DESCRIBE TABLE it_app239 LINES l_line.
  tc_app239-lines = l_line.
ENDMODULE.                 " modify_lines  OUTPUT
