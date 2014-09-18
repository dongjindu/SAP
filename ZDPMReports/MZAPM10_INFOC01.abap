*----------------------------------------------------------------------*
*   INCLUDE MZMM_GM6000CLA                                             *
*----------------------------------------------------------------------*
TYPES: TY_FUNC LIKE TABLE OF RSMPE-FUNC.                    "SIZE:20
TYPES: TY_PS(20).
TYPES: TY_TB(60).
TYPES: TY_TITLE(80).
*---------------------------------------------------------------------*
*       CLASS lcl_ps DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_PS DEFINITION.
  PUBLIC SECTION.
    METHODS CONSTRUCTOR
                    IMPORTING IM_PS      TYPE TY_PS
                              IM_IT_FUNC TYPE TY_FUNC
                              IM_TB      TYPE TY_TB
                              IM_TITLE   TYPE TY_TITLE..
  PRIVATE SECTION.
    DATA: PS      TYPE TY_PS,
          TB      TYPE TY_TB,
          TITLE   TYPE TY_TITLE.
    DATA: IT_FUNC TYPE TY_FUNC.  "Internal table
ENDCLASS.                    "lcl_ps DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_ps IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_PS IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    PS      = IM_PS.
    TB      = IM_TB.
    IT_FUNC = IM_IT_FUNC.
    TITLE   = IM_TITLE.
    SET PF-STATUS PS EXCLUDING IT_FUNC.
    SET TITLEBAR  TB WITH TITLE.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_ps IMPLEMENTATION

**** Handler Class of Tree Model
*---------------------------------------------------------------------*
*       CLASS LCL_H_TREE DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_H_TREE DEFINITION.
  PUBLIC SECTION.
    METHODS: HANDLE_NODE_DOUBLE_CLICK
               FOR EVENT   NODE_DOUBLE_CLICK
               OF          CL_COLUMN_TREE_MODEL
               IMPORTING   NODE_KEY,
             HANDLE_HEADER_CLICK
               FOR EVENT    HEADER_CLICK
               OF           CL_COLUMN_TREE_MODEL
               IMPORTING    HEADER_NAME,
             HANDLE_ITEM_DOUBLE_CLICK
               FOR EVENT ITEM_DOUBLE_CLICK
               OF           CL_COLUMN_TREE_MODEL
               IMPORTING    NODE_KEY ITEM_NAME,
             HANDLE_BUTTON_CLICK
               FOR EVENT    BUTTON_CLICK
               OF           CL_COLUMN_TREE_MODEL
               IMPORTING    NODE_KEY ITEM_NAME,
             HANDLE_LINK_CLICK
               FOR EVENT    LINK_CLICK
               OF           CL_COLUMN_TREE_MODEL
               IMPORTING    NODE_KEY ITEM_NAME,
             HANDLE_CHECKBOX_CHANGE
               FOR EVENT    CHECKBOX_CHANGE
               OF           CL_COLUMN_TREE_MODEL
               IMPORTING    NODE_KEY ITEM_NAME CHECKED.
ENDCLASS.                    "LCL_H_TREE DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_H_TREE IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_H_TREE IMPLEMENTATION.
  METHOD HANDLE_NODE_DOUBLE_CLICK.
    " this method handles the node double click event of the tree
    " model instance

    " show the key of the double clicked node in a dynpro field
    IO_EVENT    = 'NODE_DOUBLE_CLICK'.
    IO_NODE_KEY = NODE_KEY.
    MESSAGE S999(ZMMM) WITH IO_EVENT IO_NODE_KEY.
    CLEAR: IO_ITEM_NAME, IO_HEADER_NAME.
  ENDMETHOD.                    "handle_node_double_click

  METHOD HANDLE_HEADER_CLICK.
    " this method handles the node double click event of the tree
    " model instance

    " show the key of the double clicked node in a dynpro field
    IO_EVENT = 'HEADER_CLICK'.
    IO_HEADER_NAME = HEADER_NAME.
    MESSAGE S999(ZMMM) WITH IO_EVENT IO_NODE_KEY.
    CLEAR: IO_NODE_KEY, IO_ITEM_NAME.
  ENDMETHOD.                    "handle_header_click

  METHOD HANDLE_ITEM_DOUBLE_CLICK.
    " this method handles the item double click event of the tree
    " model instance

    " show the key of the node and the name of the item
    " of the double clicked item in a dynpro field
    IO_EVENT     = 'ITEM_DOUBLE_CLICK'.
    IO_NODE_KEY = NODE_KEY.
    MESSAGE S999(ZMMM) WITH IO_EVENT IO_NODE_KEY.
    CLEAR: IO_NODE_KEY, IO_ITEM_NAME.
  ENDMETHOD.                    "handle_item_double_click

  METHOD HANDLE_LINK_CLICK.
    " this method handles the link click event of the tree
    " model instance

    " show the key of the node and the name of the item
    " of the clicked link in a dynpro field
    IO_EVENT = 'LINK_CLICK'.
    IO_NODE_KEY = NODE_KEY.
    IO_ITEM_NAME = ITEM_NAME.
****
    WA_PGM = 'SAPLZGPM_INFO'.

    PERFORM GET_SUBSCREEN USING NODE_KEY
                                WA_0110.

    PERFORM TITLE USING    WA_0110
                  CHANGING TITLE.                  "Title

****
    MESSAGE S999(ZMMM) WITH IO_EVENT IO_NODE_KEY.
    CLEAR IO_HEADER_NAME.
  ENDMETHOD.                    "handle_link_click

  METHOD HANDLE_BUTTON_CLICK.
    " this method handles the button click event of the tree
    " model instance

    " show the key of the node and the name of the item
    " of the clicked button in a dynpro field
    IO_EVENT = 'BUTTON_CLICK'.
    IO_NODE_KEY = NODE_KEY.
    IO_ITEM_NAME = ITEM_NAME.
    MESSAGE S999(ZMMM) WITH IO_EVENT IO_NODE_KEY.
    CLEAR IO_HEADER_NAME.
  ENDMETHOD.                    "handle_button_click

  METHOD HANDLE_CHECKBOX_CHANGE.
    " this method handles the checkbox_change event of the tree
    " model instance

    " show the key of the node and the name of the item
    " of the clicked checkbox in a dynpro field
    IO_EVENT = 'CHECKBOX_CHANGE'.
    IO_NODE_KEY = NODE_KEY.
    IO_ITEM_NAME = ITEM_NAME.
    CLEAR IO_HEADER_NAME.
  ENDMETHOD.                    "handle_checkbox_change
ENDCLASS.                    "LCL_H_TREE IMPLEMENTATION
