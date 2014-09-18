*&---------------------------------------------------------------------*
*&      Module  initialization_APP245  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module initialization_app245 output.
  if wa_init_flg_app245 is initial.
    wa_init_flg_app245 = 'X'.
*   Set Dropdown List Boxes
    perform make_dropdown_list_box_app245.
    concatenate sy-datum+00(06) '01'
      into P_SHOP_DATE_APP245.
    P_END_DATE_APP245 = sy-datum+06(02).
    P_COMPANY_APP245   = 'HMMA'  .

  endif.
endmodule.                 " initialization_APP245  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_APP245  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_app245 output.

endmodule.                 " status_APP245  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_lines_APP245  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_lines_app245 output.
  describe table it_app245 lines tc_app245-lines.
  perform set_date_APP245.
endmodule.                 " modify_lines_APP245  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen_APP245  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_screen_app245 output.
  modify screen.
endmodule.                 " modify_screen_APP245  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STATUS100'.
  set titlebar '100'.
endmodule.                 " status_0100  OUTPUT
