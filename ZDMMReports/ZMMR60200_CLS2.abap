*----------------------------------------------------------------------*
*   INCLUDE ZMMR60200_CLS2                                             *
*----------------------------------------------------------------------*
****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
CLASS lcl_event_receiver2 DEFINITION.

  PUBLIC SECTION.
    METHODS : handle_f4 FOR EVENT onf4 OF cl_gui_alv_grid
           IMPORTING
             e_fieldname
             e_fieldvalue
             es_row_no
             er_event_data
             et_bad_cells
             e_display.

*    METHODS: check_vtnam
*           IMPORTING
*             e_cell    TYPE lvc_s_modi
*             e_changed TYPE REF TO cl_alv_changed_data_protocol.


ENDCLASS.  "(LCL_EVENT_RECEIVER DEFINITION)

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS lcl_event_receiver2 IMPLEMENTATION.

  METHOD handle_f4.
    PERFORM handle_f4 USING e_fieldname
                            e_fieldvalue
                            es_row_no
                            er_event_data
                            et_bad_cells
                            e_display.
  ENDMETHOD.                                                "handle_f4


ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
