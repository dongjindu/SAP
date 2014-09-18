*----------------------------------------------------------------------*
*   INCLUDE ZIMR_REASON_CODE_ZIM23_CLS2                                *
*----------------------------------------------------------------------*
****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
CLASS LCL_EVENT_RECEIVER2 DEFINITION.

  PUBLIC SECTION.
    METHODS : HANDLE_F4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
           IMPORTING
             E_FIELDNAME
             E_FIELDVALUE
             ES_ROW_NO
             ER_EVENT_DATA
             ET_BAD_CELLS
             E_DISPLAY.

*    METHODS: check_vtnam
*           IMPORTING
*             e_cell    TYPE lvc_s_modi
*             e_changed TYPE REF TO cl_alv_changed_data_protocol.


ENDCLASS.  "(LCL_EVENT_RECEIVER DEFINITION)

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS LCL_EVENT_RECEIVER2 IMPLEMENTATION.

  METHOD HANDLE_F4.
    PERFORM HANDLE_F4 USING E_FIELDNAME
                            E_FIELDVALUE
                            ES_ROW_NO
                            ER_EVENT_DATA
                            ET_BAD_CELLS
                            E_DISPLAY.
  ENDMETHOD.                                                "handle_f4


ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
