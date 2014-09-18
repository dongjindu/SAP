*&---------------------------------------------------------------------*
*&  Include           ZMMR60200_CLS
*&---------------------------------------------------------------------*
****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS : handle_data_changed
              FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed.

    METHODS : handle_user_command
                 FOR EVENT user_command OF cl_gui_alv_grid
                 IMPORTING e_ucomm.

    METHODS : handle_toolbar
                 FOR EVENT toolbar OF cl_gui_alv_grid
                 IMPORTING e_object e_interactive.

    METHODS : handle_hotspot_click
                 FOR EVENT hotspot_click OF cl_gui_alv_grid
                 IMPORTING e_row_id
                           e_column_id.

    METHODS : handle_after_user_command
                 FOR EVENT after_user_command OF cl_gui_alv_grid
                 IMPORTING e_ucomm
                           e_not_processed.

ENDCLASS.  "(LCL_EVENT_RECEIVER DEFINITION)

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.


  METHOD handle_data_changed.
    PERFORM handle_data_changed  USING er_data_changed.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_user_command.
    PERFORM handle_user_command USING e_ucomm.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD handle_toolbar.
    PERFORM handle_toolbar  USING e_object
                                  e_interactive.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_hotspot_click.
    PERFORM hotspot_click USING e_row_id e_column_id.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  METHOD handle_after_user_command.
    PERFORM handle_after_user_command USING e_ucomm
                                            e_not_processed..
  ENDMETHOD.                     "handle_after_user_command

ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
