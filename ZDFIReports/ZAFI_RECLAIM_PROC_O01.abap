*&---------------------------------------------------------------------*
*&  Include           ZAFI_RECLAIM_PROC_O01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  IF r_01 = 'X'.
    REFRESH it_tab.
    CLEAR   it_tab.
    it_tab-fcode = 'REVERSAL'.
    APPEND it_tab.
    it_tab-fcode = 'POST_UNFLA'.
    APPEND it_tab.
    it_tab-fcode = 'UNDELE'.
    APPEND it_tab.
    SET PF-STATUS 'D100' EXCLUDING it_tab.
  ELSE.
    REFRESH it_tab.
    CLEAR   it_tab.
    it_tab-fcode = 'POST'.
    APPEND it_tab.
    it_tab-fcode = 'POST_FLAG'.
    APPEND it_tab.
    it_tab-fcode = 'DELE'.
    APPEND it_tab.
    SET PF-STATUS 'D100' EXCLUDING it_tab.
  ENDIF.
  SET TITLEBAR '100'.


*  set pf-status '0100'.
*  set titlebar '0100'.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_alv output.
  if g_c_container1 is initial.
*  CHECK g_c_container1  IS NOT BOUND.
*  IF g_c_container1  IS INITIAL.
    perform create_object_0100.

    perform exclude_tb_functions1  changing gt_tb_exclude[].

* grid1
*  PERFORM build_fieldcatalog_grid1.
    perform get_grid_fieldcat using  gt_fieldcat_lvc " GRID FIELD CATALOG
                                     'GT_GRID'.    "IT_FINAL'.


    perform set_layout_grid1.
*  PERFORM set_f4_grid1.
    perform setting_f4_field.
*    perform setting_dropdown.


    call method g_grid1->set_table_for_first_display
      exporting
*      I_BUFFER_ACTIVE               =
*      I_BYPASSING_BUFFER            =
*      I_CONSISTENCY_CHECK           =
*      I_STRUCTURE_NAME              =
        is_variant                    = gs_variant
        i_save                        = 'A'
        i_default                     = 'X'
        is_layout                     = gs_layout_lvc
*      IS_PRINT                      =
*      IT_SPECIAL_GROUPS             =
        it_toolbar_excluding          = gt_tb_exclude[]
*      IT_HYPERLINK                  =
*      IT_ALV_GRAPHICS               =
*      IT_EXCEPT_QINFO               =
*      IR_SALV_ADAPTER               =
      changing
        it_outtab                     = it_final[]
        it_fieldcatalog               = gt_fieldcat_lvc[]
*      it_sort                       = gt_sort_lvc[]
*      IT_FILTER                     =
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4
            .
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

*  event
    create object g_event_receiver_grid1.
    set handler g_event_receiver_grid1->handle_toolbar      for g_grid1.
    set handler g_event_receiver_grid1->handle_user_command for g_grid1.
    set handler g_event_receiver_grid1->handle_data_changed for g_grid1.
    set handler g_event_receiver_grid1->handle_data_changed_fin for g_grid1.
    set handler g_event_receiver_grid1->handle_double_click  for g_grid1.
*  SET HANDLER g_event_receiver_grid1->handle_hotspot_click  FOR g_grid1.
*  SET HANDLER g_event_receiver_grid1->handle_top_of_page    FOR g_grid1.
    set handler g_event_receiver_grid1->handle_onf4            for g_grid1.
*
    call method g_grid1->set_toolbar_interactive.
*
* cell data change modify
    call method g_grid1->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid1->set_ready_for_input
      exporting
        i_ready_for_input = 1.
  else.
    perform refresh_grid  using g_grid1 'X'.

*    call method g_grid1->refresh_table_display.
  endif.

endmodule.                 " SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module init_value output.

*  if gt_hd-blart is initial.
*    gt_hd-blart = 'KO'.
*  endif.

*  data: l_mm(2).
*  if gv_bukrs is initial and gv_gjahr is initial.
*    gv_bukrs = '1000'.
*    l_mm = sy-datum+4(2).
*    if l_mm <= '02'.
*      gv_gjahr = sy-datum(4).
*      gv_gjahr = gv_gjahr - 1.
*    else.
*      gv_gjahr = sy-datum(4).
*    endif.
*  endif.


endmodule.                 " INIT_VALUE  OUTPUT
