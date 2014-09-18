*----------------------------------------------------------------------*
*   INCLUDE ZAPM09_CLASS_DEFINITION                                    *
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_APPLICATION DEFINITION.

  PUBLIC SECTION.
    METHODS:
      HANDLE_NODE_DOUBLE_CLICK
        FOR EVENT NODE_DOUBLE_CLICK
        OF        CL_GUI_SIMPLE_TREE
        IMPORTING NODE_KEY,

       HANDLE_NODE_CONTEXT_MENU_REQ
         FOR EVENT NODE_CONTEXT_MENU_REQUEST
         OF        CL_GUI_SIMPLE_TREE
         IMPORTING NODE_KEY
                   MENU,

       HANDLE_NODE_CONTEXT_MENU_SEL
         FOR EVENT NODE_CONTEXT_MENU_SELECT
         OF        CL_GUI_SIMPLE_TREE
         IMPORTING NODE_KEY
                   FCODE,

**-- Event at EQUI_TREE - START
       HANDLE_NODE_CONTEXT_MENU_REQ2
         FOR EVENT NODE_CONTEXT_MENU_REQUEST
         OF        CL_GUI_COLUMN_TREE
         IMPORTING NODE_KEY
                   MENU,

       HANDLE_NODE_CONTEXT_MENU_SEL2
         FOR EVENT NODE_CONTEXT_MENU_SELECT
         OF        CL_GUI_COLUMN_TREE
         IMPORTING NODE_KEY
                   FCODE,

      HANDLE_NODE_DOUBLE_CLICK2
        FOR EVENT NODE_DOUBLE_CLICK
        OF        CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY,
**--  END

      HANDLE_EXPAND_NO_CHILDREN
        FOR EVENT EXPAND_NO_CHILDREN
        OF        CL_GUI_SIMPLE_TREE
        IMPORTING NODE_KEY.


ENDCLASS.
