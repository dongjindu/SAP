*----------------------------------------------------------------------*
*   INCLUDE MZPP_APPLICATIONCLASS                                      *
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
*       CLASS LCL_APPLICATION DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_node_double_click
        FOR EVENT node_double_click
        OF cl_gui_simple_tree
        IMPORTING node_key,
      handle_expand_no_children
        FOR EVENT expand_no_children
        OF cl_gui_simple_tree
        IMPORTING node_key.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD  handle_node_double_click.
    " this method handles the node double click event of the tree
    " control instance

    " show the key of the double clicked node in a dynpro field
    g_node_key = node_key.
    CONCATENATE G_NODE_KEY(2) G_NODE_KEY+6(2) INTO SV_DYNNR.
  ENDMETHOD.

  METHOD handle_expand_no_children.
    " this method handles the expand no children event of the tree
    " control instance
    DATA: node_table TYPE node_table_type,
          node TYPE mtreesnode.

    " show the key of the double clicked node in a dynpro field
    g_node_key = node_key.

    CALL METHOD g_tree->add_nodes
      EXPORTING
        table_structure_name = 'MTREESNODE'
        node_table           = node_table
      EXCEPTIONS
        failed                         = 1
        error_in_node_table            = 2
        dp_error                       = 3
        table_structure_name_not_found = 4
        OTHERS                         = 5.
    IF sy-subrc <> 0.
      MESSAGE a001(ZMPP) WITH 'TREE: ADD NODES FAIL!'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
