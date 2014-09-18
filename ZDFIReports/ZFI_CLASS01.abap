*&---------------------------------------------------------------------*
*&  Include           ZFI_CLASS01
*&---------------------------------------------------------------------*
*     class lcl_event_receiver_grid1 definition
*&---------------------------------------------------------------------*
class lcl_event_receiver_grid1 definition.
  public section.
    methods
      handle_toolbar  for event toolbar of cl_gui_alv_grid
                          importing e_object  e_interactive.
    methods
      handle_user_command for event user_command  of cl_gui_alv_grid
                          importing e_ucomm.
*    METHODS
*      handle_hotspot_click  FOR EVENT hotspot_click OF cl_gui_alv_grid
*                          IMPORTING e_row_id e_column_id es_row_no.
*    METHODS
*      handle_top_of_page    FOR EVENT top_of_page OF cl_gui_alv_grid
*                          IMPORTING e_dyndoc_id table_index.

    methods
      handle_data_changed   for event data_changed of cl_gui_alv_grid
                          importing er_data_changed
                            e_onf4 e_onf4_before e_onf4_after e_ucomm.
    methods
      handle_data_changed_fin for event data_changed_finished of cl_gui_alv_grid
                          importing e_modified  et_good_cells.

    methods
       handle_onf4  for event onf4 of cl_gui_alv_grid
                  importing sender
                            e_fieldname
                            e_fieldvalue
                            es_row_no
                            er_event_data
                            et_bad_cells
                            e_display.

    methods
       handle_double_click for event double_click of cl_gui_alv_grid
         importing e_row e_column es_row_no sender.


endclass.                    "lcl_event_receiver_grid1 DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver_grid1 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_receiver_grid1  implementation.

  method handle_toolbar.
    perform handle_toolbar  using e_object  e_interactive.
  endmethod.                    "handle_hotspot_click

  method handle_user_command.
    perform handle_user_command using e_ucomm.
  endmethod.                    "handle_hotspot_click

*  METHOD handle_hotspot_click.
*    PERFORM handle_hotspot_click  USING e_row_id e_column_id es_row_no.
*  ENDMETHOD.                    "handle_hotspot_click

*  METHOD handle_top_of_page.
*    PERFORM handle_top_of_page  USING e_dyndoc_id table_index.
*  ENDMETHOD.                    "handle_top_of_page
*
  method handle_data_changed.
    perform handle_data_changed using er_data_changed
                            e_onf4 e_onf4_before e_onf4_after e_ucomm.
  endmethod.                    "handle_data_changed

  method handle_data_changed_fin.
    perform handle_data_changed_fin using e_modified  et_good_cells.
  endmethod.                    "handle_data_changed

  method handle_onf4.
    perform handle_f4_grid     using sender
                                     e_fieldname
                                     e_fieldvalue
                                     es_row_no
                                     er_event_data
                                     et_bad_cells
                                     e_display.

  endmethod.                    "HANDLE_ONF4

  method handle_double_click.
    g_sender = sender.
    perform double_click using e_row e_column es_row_no.
  endmethod.                    "HANDLE_DOUBLE_CLICK


endclass.                    "lcl_event_receiver_grid1 IMPLEMENTATION
