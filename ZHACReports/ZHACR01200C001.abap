*&---------------------------------------------------------------------*
*&  Include           ZHACR01000C001
*&---------------------------------------------------------------------*
************************************************************************
* LOCAL CLASSES: Definition                                            *
************************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
**** ALV 1
*    METHODS HANDLE_CONTEXT_MENU
*      FOR EVENT CONTEXT_MENU_REQUEST OF CL_GUI_ALV_GRID
*      IMPORTING E_OBJECT.
    METHODS : HANDLE_TOOLBAR1
                FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
                IMPORTING E_OBJECT E_INTERACTIVE,
              HANDLE_TOOLBAR2
                FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
                IMPORTING E_OBJECT E_INTERACTIVE,
              HANDLE_TOOLBAR3
                FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
                IMPORTING E_OBJECT E_INTERACTIVE,
              HANDLE_TOOLBAR4
                FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
                IMPORTING E_OBJECT E_INTERACTIVE.
    METHODS : HANDLE_USER_COMMAND1
                FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
                IMPORTING E_UCOMM,
              HANDLE_USER_COMMAND2
                FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
                IMPORTING E_UCOMM,
              HANDLE_USER_COMMAND3
                FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
                IMPORTING E_UCOMM,
              HANDLE_USER_COMMAND4
                FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
                IMPORTING E_UCOMM.
*    METHODS : HANDLE_DATA_CHANGED1
*                FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
*                IMPORTING ER_DATA_CHANGED.
    METHODS: HANDLE_DOUBLE_CLICK1
               FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
               IMPORTING E_ROW E_COLUMN,
             HANDLE_DOUBLE_CLICK2
               FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
               IMPORTING E_ROW E_COLUMN,
             HANDLE_DOUBLE_CLICK3
               FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
               IMPORTING E_ROW E_COLUMN.
ENDCLASS. "(LCL_EVENT_RECEIVER DEFINITION)
************************************************************************
* LOCAL CLASSES: Implementation                                        *
************************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
* ########
  METHOD HANDLE_TOOLBAR1.
*    PERFORM ALV_CL_TOOLBAR_PART1
*                           USING E_OBJECT
*                                 E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar
  METHOD HANDLE_TOOLBAR2.
*    PERFORM ALV_CL_TOOLBAR_PART2
*                           USING E_OBJECT
*                                 E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar
  METHOD HANDLE_TOOLBAR3.
*    PERFORM ALV_CL_TOOLBAR_PART2
*                           USING E_OBJECT
*                                 E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar
  METHOD HANDLE_TOOLBAR4.
*    PERFORM ALV_CL_TOOLBAR_PART2
*                           USING E_OBJECT
*                                 E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar
* ####### ## ##
  METHOD HANDLE_USER_COMMAND1.
*    PERFORM ALV_CL_USER_COMMAND_PART1 USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command
  METHOD HANDLE_USER_COMMAND2.
*    PERFORM ALV_CL_USER_COMMAND_PART2 USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command
  METHOD HANDLE_USER_COMMAND3.
*    PERFORM ALV_CL_USER_COMMAND_PART2 USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command
  METHOD HANDLE_USER_COMMAND4.
*    PERFORM ALV_CL_USER_COMMAND_PART2 USING E_UCOMM.
  ENDMETHOD.                    "handle_user_command
** ####
*  METHOD HANDLE_DATA_CHANGED1.
**    PERFORM ALV_CL_CELL_DATA_CHANGED1 USING ER_DATA_CHANGED.
*  ENDMETHOD.                    "handle_data_changed
* ### ##
  METHOD HANDLE_DOUBLE_CLICK1.
    PERFORM ALV_CL_CELL_DOUBLE_CLICK1  USING E_ROW E_COLUMN.
  ENDMETHOD.                    "handle_double_click
  METHOD HANDLE_DOUBLE_CLICK2.
    PERFORM ALV_CL_CELL_DOUBLE_CLICK2  USING E_ROW E_COLUMN.
  ENDMETHOD.                    "handle_double_click
  METHOD HANDLE_DOUBLE_CLICK3.
    PERFORM ALV_CL_CELL_DOUBLE_CLICK3  USING E_ROW E_COLUMN.
  ENDMETHOD.                    "handle_double_click
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION


DATA: G_EVENT_RECEIVER1 TYPE REF TO LCL_EVENT_RECEIVER.
DATA: G_EVENT_RECEIVER2 TYPE REF TO LCL_EVENT_RECEIVER.
DATA: G_EVENT_RECEIVER3 TYPE REF TO LCL_EVENT_RECEIVER.
DATA: G_EVENT_RECEIVER4 TYPE REF TO LCL_EVENT_RECEIVER.
