*----------------------------------------------------------------------*
*   INCLUDE ZIMR_REASON_CODE_ZIM23_CLS                                 *
*----------------------------------------------------------------------*
****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    METHODS : HANDLE_DATA_CHANGED
              FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
              IMPORTING ER_DATA_CHANGED.

*    METHODS : HANDLE_USER_COMMAND
*                 FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
*                 IMPORTING E_UCOMM.
*
*    METHODS : HANDLE_TOOLBAR
*                 FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
*                 IMPORTING E_OBJECT E_INTERACTIVE.
*
*    METHODS : HANDLE_HOTSPOT_CLICK
*                 FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*                 IMPORTING E_ROW_ID
*                           E_COLUMN_ID.
*
*    METHODS : HANDLE_AFTER_USER_COMMAND
*                 FOR EVENT AFTER_USER_COMMAND OF CL_GUI_ALV_GRID
*                 IMPORTING E_UCOMM
*                           E_NOT_PROCESSED.
*
ENDCLASS.  "(LCL_EVENT_RECEIVER DEFINITION)

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.


  METHOD HANDLE_DATA_CHANGED.
    PERFORM HANDLE_DATA_CHANGED  USING ER_DATA_CHANGED.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

*  METHOD HANDLE_USER_COMMAND.
*    PERFORM HANDLE_USER_COMMAND USING E_UCOMM.
*  ENDMETHOD.                    "HANDLE_USER_COMMAND
*
*  METHOD HANDLE_TOOLBAR.
*    PERFORM HANDLE_TOOLBAR  USING E_OBJECT
*                                  E_INTERACTIVE.
*  ENDMETHOD.                    "handle_toolbar

*  METHOD HANDLE_HOTSPOT_CLICK.
*    PERFORM HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID.
*  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK
*
*  METHOD HANDLE_AFTER_USER_COMMAND.
*    PERFORM HANDLE_AFTER_USER_COMMAND USING E_UCOMM
*                                            E_NOT_PROCESSED..
*  ENDMETHOD.                     "handle_after_user_command

ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
