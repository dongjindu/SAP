*----------------------------------------------------------------------*
*   INCLUDE MZPP_APP202F01                                             *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_tree.
  DATA: node_table TYPE treemcitab     ,
        events TYPE cntl_simple_events,
        event TYPE cntl_simple_event.

* create a container for the tree control
  CREATE OBJECT g_custom_container
    EXPORTING
      " the container is linked to the custom control with the
      " name 'TREE_CONTAINER' on the dynpro
      container_name = 'TREE_CONTAINER'
    EXCEPTIONS
      cntl_error = 1
      cntl_system_error = 2
      create_error = 3
      lifetime_error = 4
      lifetime_dynpro_dynpro_link = 5.
  IF sy-subrc <> 0.
    MESSAGE a001(ZMPP) WITH 'ERROR!!!!' .
  ENDIF.


* create a tree control
   CREATE OBJECT g_tree
    EXPORTING
      parent              = g_custom_container
      node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single
    EXCEPTIONS
      lifetime_error              = 1
      cntl_system_error           = 2
      create_error                = 3
      failed                      = 4
      illegal_node_selection_mode = 5.
  IF sy-subrc <> 0.
    MESSAGE a001(ZMPP) WITH 'G_TREE: CREATION ERROR!'.
  ENDIF.

* define the events which will be passed to the backend
  " node double click
  event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  event-appl_event = 'X'. " process PAI if event occurs
  APPEND event TO events.

  " expand no children
  event-eventid = cl_gui_simple_tree=>eventid_expand_no_children.
  event-appl_event = 'X'.
  APPEND event TO events.

* header click
  CALL METHOD g_tree->set_registered_events
    EXPORTING
      events = events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE a001(ZMPP) WITH 'EVENT RESISTERS ERROR!!'.
  ENDIF.

* assign event handlers in the application class to each desired event
  SET HANDLER g_application->handle_node_double_click FOR g_tree.
* SET HANDLER g_application->handle_expand_no_children FOR g_tree.
ENDFORM.                    " CREATE_AND_INIT_TREE

*&---------------------------------------------------------------------*
*&      Form  build_node_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_node_table
  USING
    node_table TYPE treemcitab     .

  DATA: LCL_NO(2)  TYPE N         ,
        item_table TYPE treemcitab,    "Column Tree Model: Item Table
        item       TYPE treemcitem.    "Column Tree Model: Item

    CLEAR item.
    item-item_name = 'Column1'.  "#EC NOTEXT " Item of Column 'Column1'
    item-class = cl_column_tree_model=>item_class_text.
    item-text = 'SUB-ITEM1'  .                              "#EC NOTEXT
    APPEND item TO item_table.

* Column2
    CLEAR item.
    item-item_name = 'Column2'.     "
    item-class = cl_column_tree_model=>item_class_text.
    item-text = 'SUB-ITEM1'  .                              "#EC NOTEXT
    APPEND item TO item_table.

*    CALL METHOD g_tree->add_node
*      EXPORTING
*        node_key                = 'Root'                    "#EC NOTEXT
*        isfolder                = 'X'
*        item_table              = item_table
*      EXCEPTIONS
*        node_key_exists         = 1
*        node_key_empty          = 2
*        illegal_relationship    = 3
*        relative_node_not_found = 4
*        error_in_item_table     = 5.
*
*    IF sy-subrc <> 0.
*      MESSAGE a001(ZMPP) WITH 'ERROR!!!!' .
*    ENDIF.


    DO 6 TIMES .
    LCL_NO = LCL_NO + 1.
    CLEAR item_table.
    CLEAR item.
    item-item_name = 'Column1'.  "#EC NOTEXT " Item of Column 'Column1'
    item-class = cl_column_tree_model=>item_class_link.
    CONCATENATE 'DESCRIPTION-0' LCL_NO INTO ITEM-TEXT .
*   item-text = 'DESCRIPTION-001 '.                         "#EC NOTEXT
    APPEND item TO item_table.

* Column2
    CLEAR item.
    item-item_name = 'Column2'.     "
    item-class = cl_column_tree_model=>item_class_text.
    item-text = space.                                      "#EC NOTEXT
    APPEND item TO item_table.

*    CALL METHOD g_tree->add_node
*      EXPORTING
*        node_key                = 'Child1'                  "#EC NOTEXT
*        relative_node_key       = 'Root'
*        relationship            = cl_tree_model=>relat_last_child
*        isfolder                = space
*        item_table              = item_table
*      EXCEPTIONS
*        node_key_exists         = 1
*        node_key_empty          = 2
*        illegal_relationship    = 3
*        relative_node_not_found = 4
*        error_in_item_table     = 5.
*
*    IF sy-subrc <> 0.
*      MESSAGE a001(ZMPP) WITH 'ERROR!!!!' .
*    ENDIF.
    ENDDO.
ENDFORM.                    " build_node_table
