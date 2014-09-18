*&---------------------------------------------------------------------*
*&      Module  initialization_app240  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization_app240 OUTPUT.
  IF wa_init_flg_app240 IS INITIAL.
    wa_init_flg_app240 = 'X'.
*   Set Dropdown List Boxes
    PERFORM make_dropdown_list_box_app240.
    P_COMPANY_APP240 = 'HMMA'. MODIFY SCREEN.
  ENDIF.
ENDMODULE.                 " initialization_app240  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_app240  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_app240 OUTPUT.

ENDMODULE.                 " status_app240  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_lines_app240  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_lines_app240 OUTPUT.
  DESCRIBE TABLE it_app240 LINES tc_app240-lines.

ENDMODULE.                 " modify_lines_app240  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen_app240  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_app240 OUTPUT.
  modify screen.
ENDMODULE.                 " modify_screen_app240  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR '100'.
ENDMODULE.                 " status_0100  OUTPUT
