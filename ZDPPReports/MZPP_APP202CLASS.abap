************************************************************************
* Program Name      : SAPMZPP_APP202
* Author            : Bobby
* Creation Date     : 2003.08.26.
* Specifications By : Bobby
* Development Request No : UD1K902029
* Addl Documentation:
* Description       : [PP] Order Management - Order Color
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE MZPP_APP202CLASS                                           *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_node_double_click
        FOR EVENT node_double_click
        OF cl_column_tree_model
        IMPORTING node_key.
*      handle_expand_no_children
*        FOR EVENT expand_no_children
*        OF cl_gui_simple_tree
*        IMPORTING node_key.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD  handle_node_double_click.
    g_node_key = node_key.

    CASE g_node_key(2) .
      WHEN '01'.
        CASE g_node_key+2(4).
          WHEN '0101'.
            sv_scno = '1101' .
          WHEN '0102'.
            sv_scno = '1102' .
          WHEN '0103'.
            sv_scno = '1103' .
          WHEN '0104'.
            sv_scno = '1104' .
        ENDCASE.
      WHEN '02'.
        CASE g_node_key+2(4).
          WHEN '0101'.
            sv_scno = '1101' .
          WHEN '0102'.
            sv_scno = '1102' .
          WHEN '0103'.
            sv_scno = '1103' .
          WHEN '0104'.
            sv_scno = '1104' .
        ENDCASE.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
