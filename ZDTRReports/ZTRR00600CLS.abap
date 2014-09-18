*&---------------------------------------------------------------------*
*&  Include           ZTRR00600CLS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZFMR0030CLS
*&---------------------------------------------------------------------*

CLASS LCL_APPLICATION DEFINITION.

  PUBLIC SECTION.
    METHODS:
       HANDLE_ITEM_DOUBLE_CLICK
        FOR EVENT ITEM_DOUBLE_CLICK OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY
                  ITEM_NAME.

ENDCLASS.                    "LCL_APPLICATION DEFINITION


*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_APPLICATION
*&---------------------------------------------------------------------*
CLASS LCL_APPLICATION IMPLEMENTATION.
  METHOD  HANDLE_ITEM_DOUBLE_CLICK.
    MESSAGE I000 WITH NODE_KEY ITEM_NAME.

*    CASE ITEM_NAME.
*      WHEN C_COL-C03.
*        PERFORM CALL_ZRFFMEPGAX USING NODE_KEY ITEM_NAME
*                                      P_PERIOD P_PERIOD.
*
*      WHEN C_COL-C04.
*        PERFORM CALL_PLAN_LIST USING NODE_KEY ITEM_NAME
*                                     P_PERIOD P_PERIOD.
*
*      WHEN C_COL-C06.
*        PERFORM CALL_ZRFFMEPGAX USING NODE_KEY ITEM_NAME
*                                      '01'     P_PERIOD.
*
*      WHEN C_COL-C07.
*        PERFORM CALL_PLAN_LIST USING NODE_KEY ITEM_NAME
*                                     '01'     P_PERIOD.
*
*      WHEN OTHERS.
*        EXIT.
*    ENDCASE.
  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

ENDCLASS.               "LCL_APPLICATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:

     HANDLE_HEADER_CLICK
                  FOR EVENT HEADER_CLICK OF CL_GUI_ALV_TREE
                  IMPORTING FIELDNAME,

     HANDLE_ITEM_DOUBLE_CLICK
                  FOR EVENT ITEM_DOUBLE_CLICK OF CL_GUI_ALV_TREE
                  IMPORTING FIELDNAME NODE_KEY,

    HANDLE_DATA_CHANGED
                  FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                  IMPORTING ER_DATA_CHANGED.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_HEADER_CLICK.
    MESSAGE I000 WITH FIELDNAME.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD HANDLE_ITEM_DOUBLE_CLICK.
    MESSAGE I000 WITH FIELDNAME NODE_KEY.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED  USING ER_DATA_CHANGED.
  ENDMETHOD.                    "handle_data_changed

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
DATA : G_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.
