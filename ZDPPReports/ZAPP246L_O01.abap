*&---------------------------------------------------------------------*
*&      Module  initialization_APP246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module initialization_app246 output.
  if wa_init_flg_app246 is initial.
    wa_init_flg_app246 = 'X'.
*   Set Dropdown List Boxes
    perform make_dropdown_list_box_app246.
    p_company_app246   = 'HMMA'  .
    perform get_parameter_app246 .
    wa_alv_called = 'X'.
  endif.
endmodule.                 " initialization_APP246  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_APP246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_app246 output.

endmodule.                 " status_APP246  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STATUS100'.
  set titlebar '100'.
endmodule.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_grid_app246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module create_alv_grid_app246 output.
  if wa_alv_called <> space.
    clear wa_alv_called .
*    CREATE OBJECT GS_APPLICATION.

    create object gs_custom_container
    exporting container_name = wa_container.

    create object alv_grid
        exporting i_parent = gs_custom_container.

    perform  build_variant_app246.
    perform  build_layout_app246.
    perform  build_fieldcat_app246.
    if p_status_app246 = 'D'.
      perform  call_method_det_app246 .
    else.
      perform  call_method_sum_app246.
    endif.

  endif.

endmodule.                 " create_alv_grid_app246  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_cursor_field_app246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_cursor_field_app246 output.
  set cursor field wa_fname_tx line wa_saveline_ix.
endmodule.                 " set_cursor_field_app246  OUTPUT
