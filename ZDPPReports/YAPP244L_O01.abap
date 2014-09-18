*&---------------------------------------------------------------------*
*&      Module  initialization_APP244  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module initialization_app244 output.
  if wa_init_flg_app244 is initial.
    wa_init_flg_app244 = 'X'.
*   Set Dropdown List Boxes
    perform make_dropdown_list_box_app244.
    p_prod_date_app244 = sy-datum.
    p_company_app244   = 'HMMA'  .

  endif.
endmodule.                 " initialization_APP244  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_APP244  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_app244 output.

endmodule.                 " status_APP244  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_lines_APP244  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_lines_app244 output.
  describe table it_app244 lines tc_app244-lines.
endmodule.                 " modify_lines_APP244  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen_APP244  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_screen_app244 output.
  modify screen.
endmodule.                 " modify_screen_APP244  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STATUS100'.
  set titlebar '100'.
endmodule.                 " status_0100  OUTPUT
