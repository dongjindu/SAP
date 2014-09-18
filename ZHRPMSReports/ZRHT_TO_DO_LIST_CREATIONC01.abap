*&---------------------------------------------------------------------*
*&  Include           ZRHT_TO_DO_LIST_CREATIONC01
*&---------------------------------------------------------------------*
* Class used to get changed data
CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS: handle_data_changed
              FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed.
ENDCLASS.                    "lcl_event_handler DEFINITION
* Class used to get changed data
CLASS lcl_event_handler IMPLEMENTATION .
*  Handle Data Changed
  METHOD handle_data_changed .
    PERFORM handle_data_changed USING er_data_changed .
  ENDMETHOD.                    "handle_data_changed
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: gr_event_handler  TYPE REF TO lcl_event_handler.
