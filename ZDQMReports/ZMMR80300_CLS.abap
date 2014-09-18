*&---------------------------------------------------------------------*
*&  Include           ZKEMMR04011_CLS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Include           ZMMMP1073_CLS                                  *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_ALV_GRID DEFINITION INHERITING FROM CL_GUI_ALV_GRID.

  PUBLIC SECTION.
  CLASS-DATA F_ALV  TYPE C.

  METHODS : SET_FIXED_COLUMN,
  SET_OPTIMIZER.

ENDCLASS.                    "LCL_ALV_GRID DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_ALV_GRID IMPLEMENTATION.

  METHOD SET_FIXED_COLUMN.
    CALL METHOD ME->SET_FIXED_COLS
    EXPORTING
      COLS = 5.
  ENDMETHOD.                    "SET_FIXED_COLUMN

  METHOD SET_OPTIMIZER.
    CALL METHOD ME->OPTIMIZE_ALL_COLS
    EXPORTING
      INCLUDE_HEADER = 1.
  ENDMETHOD.                    "SET_OPTIMIZER

ENDCLASS.                    "LCL_ALV_GRID IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
  METHODS : HANDLE_TOOLBAR
  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
  IMPORTING E_OBJECT E_INTERACTIVE.

  METHODS : HANDLE_USER_COMMAND
  FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
  IMPORTING E_UCOMM.

  METHODS : HANDLE_AFTER_USER_COMMAND
  FOR EVENT AFTER_USER_COMMAND OF CL_GUI_ALV_GRID
  IMPORTING  E_UCOMM .

  METHODS : HANDLE_DATA_CHANGED
  FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
  IMPORTING ER_DATA_CHANGED E_ONF4 E_UCOMM.

  METHODS : HANDLE_DATA_CHANGED_FINISHED
  FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
  IMPORTING E_MODIFIED
    ET_GOOD_CELLS.

  METHODS : HANDLE_HOTSPOT_CLICK
  FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
  IMPORTING E_ROW_ID
    E_COLUMN_ID.

  METHODS : HANDLE_DOUBLE_CLICK
  FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
  IMPORTING E_ROW
    E_COLUMN.

  METHODS : HANDLE_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
  IMPORTING SENDER
    E_FIELDNAME
    E_FIELDVALUE
    ES_ROW_NO
    ER_EVENT_DATA
    ET_BAD_CELLS
    E_DISPLAY.


ENDCLASS.  "(LCL_EVENT_RECEIVER DEFINITION)

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

*-- ToolBar ????
  METHOD HANDLE_TOOLBAR.
    PERFORM TOOLBAR_PROS  USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar

*-- ToolBar? ??? ??(Function)?? ??? ??
  METHOD HANDLE_USER_COMMAND.
    PERFORM USER_COMMAND_PROS USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command

*-- Event ???
  METHOD HANDLE_AFTER_USER_COMMAND.
    PERFORM AFTER_USER_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "HANDLE_AFTER_USER_COMMAND

*-- Data change??
  METHOD HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED  USING ER_DATA_CHANGED E_ONF4 E_UCOMM.
  ENDMETHOD.                    "handle_data_changed

*-- Data change? ?
  METHOD HANDLE_DATA_CHANGED_FINISHED.
    PERFORM DATA_CHANGED_FINISHED USING E_MODIFIED
          ET_GOOD_CELLS.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED_FINISHED

*-- Hotspot click
  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID.
  ENDMETHOD.                    "handle_hotspot_click

*-- Double Click
  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM DOUBLE_CLICK  USING E_ROW E_COLUMN.
  ENDMETHOD.    "handle_double_click

*-- On help f4 - Search Help
  METHOD HANDLE_ONF4.
    PERFORM ON_F4 USING SENDER
          E_FIELDNAME
          E_FIELDVALUE
          ES_ROW_NO
          ER_EVENT_DATA
          ET_BAD_CELLS
          E_DISPLAY.
  ENDMETHOD.                    "HANDLE_ONF4



ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
