*----------------------------------------------------------------------*
*   INCLUDE MZMM_GM6000CLA                                             *
*----------------------------------------------------------------------*
TYPES: ty_func LIKE TABLE OF rsmpe-func.                    "SIZE:20
TYPES: ty_ps(20).
TYPES: ty_tb(60).
TYPES: ty_title(80).
*---------------------------------------------------------------------*
*       CLASS lcl_ps DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_ps DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
                    IMPORTING im_ps      TYPE ty_ps
                              im_it_func TYPE ty_func
                              im_tb      TYPE ty_tb
                              im_title   TYPE ty_title..
  PRIVATE SECTION.
    DATA: ps      TYPE ty_ps,
          tb      TYPE ty_tb,
          title   TYPE ty_title.
    DATA: it_func TYPE ty_func.  "Internal table
ENDCLASS.                    "lcl_ps DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_ps IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_ps IMPLEMENTATION.
  METHOD constructor.
    ps      = im_ps.
    tb      = im_tb.
    it_func = im_it_func.
    title   = im_title.
    SET PF-STATUS ps EXCLUDING it_func.
    SET TITLEBAR  tb WITH title.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_ps IMPLEMENTATION

**** Handler Class of Tree Model
*---------------------------------------------------------------------*
*       CLASS LCL_H_TREE DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_h_tree DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_node_double_click
               FOR EVENT   node_double_click
               OF          cl_column_tree_model
               IMPORTING   node_key,
             handle_header_click
               FOR EVENT    header_click
               OF           cl_column_tree_model
               IMPORTING    header_name,
             handle_item_double_click
               FOR EVENT item_double_click
               OF           cl_column_tree_model
               IMPORTING    node_key item_name,
             handle_button_click
               FOR EVENT    button_click
               OF           cl_column_tree_model
               IMPORTING    node_key item_name,
             handle_link_click
               FOR EVENT    link_click
               OF           cl_column_tree_model
               IMPORTING    node_key item_name,
             handle_checkbox_change
               FOR EVENT    checkbox_change
               OF           cl_column_tree_model
               IMPORTING    node_key item_name checked.
ENDCLASS.                    "LCL_H_TREE DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_H_TREE IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_h_tree IMPLEMENTATION.
  METHOD handle_node_double_click.
    " this method handles the node double click event of the tree
    " model instance

    " show the key of the double clicked node in a dynpro field
    io_event    = 'NODE_DOUBLE_CLICK'.
    io_node_key = node_key.
    MESSAGE s999(zmmm) WITH io_event io_node_key.
    CLEAR: io_item_name, io_header_name.
  ENDMETHOD.                    "handle_node_double_click

  METHOD handle_header_click.
    " this method handles the node double click event of the tree
    " model instance

    " show the key of the double clicked node in a dynpro field
    io_event = 'HEADER_CLICK'.
    io_header_name = header_name.
    MESSAGE s999(zmmm) WITH io_event io_node_key.
    CLEAR: io_node_key, io_item_name.
  ENDMETHOD.                    "handle_header_click

  METHOD handle_item_double_click.
    " this method handles the item double click event of the tree
    " model instance

    " show the key of the node and the name of the item
    " of the double clicked item in a dynpro field
    io_event     = 'ITEM_DOUBLE_CLICK'.
    io_node_key = node_key.
    MESSAGE s999(zmmm) WITH io_event io_node_key.
    CLEAR: io_node_key, io_item_name.
  ENDMETHOD.                    "handle_item_double_click

  METHOD handle_link_click.
    " this method handles the link click event of the tree
    " model instance

    " show the key of the node and the name of the item
    " of the clicked link in a dynpro field
    io_event = 'LINK_CLICK'.
    io_node_key = node_key.
    io_item_name = item_name.
****
    pgm = 'SAPLZGMM_6000_SS'.

    CALL FUNCTION 'Z_FMM_6000_GET_SUBSCREEN'" Get Subscreen
      EXPORTING
        im_node_key = node_key
      IMPORTING
        ex_dynnr    = ss_0110.

    CLEAR w_once_flg.    "To be initial
    PERFORM clear.    "Clear Variables
*    PERFORM tree_off.  "Tree Off
    PERFORM title USING    ss_0110
                  CHANGING w_title.        "Title

****
*    MESSAGE s999(zmmm) WITH io_event io_node_key.
    CLEAR io_header_name.
  ENDMETHOD.                    "handle_link_click

  METHOD handle_button_click.
    " this method handles the button click event of the tree
    " model instance

    " show the key of the node and the name of the item
    " of the clicked button in a dynpro field
    io_event = 'BUTTON_CLICK'.
    io_node_key = node_key.
    io_item_name = item_name.
    MESSAGE s999(zmmm) WITH io_event io_node_key.
    CLEAR io_header_name.
  ENDMETHOD.                    "handle_button_click

  METHOD handle_checkbox_change.
    " this method handles the checkbox_change event of the tree
    " model instance

    " show the key of the node and the name of the item
    " of the clicked checkbox in a dynpro field
    io_event = 'CHECKBOX_CHANGE'.
    io_node_key = node_key.
    io_item_name = item_name.
    CLEAR io_header_name.
  ENDMETHOD.                    "handle_checkbox_change
ENDCLASS.                    "LCL_H_TREE IMPLEMENTATION
