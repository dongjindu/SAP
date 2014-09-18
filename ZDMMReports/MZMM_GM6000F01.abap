*----------------------------------------------------------------------*
*   INCLUDE MZMM_GM6000F01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  make_it_func
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_func.
  CLEAR: it_func.

  IF w_trtyp = 'A'. "Display mode
    wa_func = 'SAVE'. APPEND wa_func TO it_func.   "Save
  ENDIF.

ENDFORM.                    " make_it_func
*&---------------------------------------------------------------------*
*&      Form  create_and_init_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_tree.

** Column1 of Tree
* setup the hierarchy header
  hierarchy_header-heading = 'Hierarchy Header'.            "#EC NOTEXT
  hierarchy_header-width = 45.     " width: 30 characters
* create a simple tree model instance
  CREATE OBJECT crv_tree_model
    EXPORTING
      node_selection_mode = cl_column_tree_model=>node_sel_mode_single
      item_selection = 'X'
      hierarchy_column_name = 'Column1' "EC NOTEXT
      hierarchy_header = hierarchy_header
    EXCEPTIONS
      illegal_node_selection_mode = 1
      illegal_column_name = 2.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.

** Column2 of Tree
  CALL METHOD crv_tree_model->add_column
    EXPORTING
      name                = 'Column2'                       "#EC NOTEXT
      width               = 50
      header_text         = 'Description'                   "#EC NOTEXT
    EXCEPTIONS
      column_exists       = 1
      illegal_column_name = 2
      too_many_columns    = 3
      illegal_alignment   = 4.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.
**

* create the view (control) of the tree model
  CALL METHOD crv_tree_model->create_tree_control
    EXPORTING
      parent                       = crv_docking_container
    EXCEPTIONS
      lifetime_error               = 1
      cntl_system_error            = 2
      create_error                 = 3
      failed                       = 4
      tree_control_already_created = 5.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.

* define the events which will be passed to the backend

* node double click
  event-eventid = cl_column_tree_model=>eventid_node_double_click.
  event-appl_event = 'X'.              " process PAI if event occurs
  APPEND event TO events.
* item double click
  event-eventid = cl_column_tree_model=>eventid_item_double_click.
  event-appl_event = 'X'.
  APPEND event TO events.
* link click
  event-eventid = cl_column_tree_model=>eventid_link_click.
  event-appl_event = 'X'.
  APPEND event TO events.
* button click
  event-eventid = cl_column_tree_model=>eventid_button_click.
  event-appl_event = 'X'.
  APPEND event TO events.
* checkbox change
  event-eventid = cl_column_tree_model=>eventid_checkbox_change.
  event-appl_event = 'X'.
  APPEND event TO events.

* header click
  event-eventid = cl_column_tree_model=>eventid_header_click.
  event-appl_event = 'X'.
  APPEND event TO events.

* Register Events
  CALL METHOD crv_tree_model->set_registered_events
    EXPORTING
      events                    = events
    EXCEPTIONS
      illegal_event_combination = 1
      unknown_event             = 2.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.

* assign event handlers in the application class to each desired event
  SET HANDLER crv_h_tree->handle_node_double_click
      FOR     crv_tree_model.
  SET HANDLER crv_h_tree->handle_item_double_click
      FOR     crv_tree_model.
  SET HANDLER crv_h_tree->handle_link_click
      FOR     crv_tree_model.
  SET HANDLER crv_h_tree->handle_button_click
      FOR     crv_tree_model.
  SET HANDLER crv_h_tree->handle_checkbox_change
      FOR     crv_tree_model.
  SET HANDLER crv_h_tree->handle_header_click
      FOR     crv_tree_model.

* add nodes to the tree model
  PERFORM add_nodes.

* expand the root node
  CALL METHOD crv_tree_model->expand_node
    EXPORTING
      node_key       = 'Root'                               "#EC NOTEXT
    EXCEPTIONS
      node_not_found = 1.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.

* expand the level 2 node
  CALL METHOD crv_tree_model->expand_node
    EXPORTING
      node_key       = 'Child1'
    EXCEPTIONS
      node_not_found = 1.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.
ENDFORM.                    " create_and_init_tree
*&---------------------------------------------------------------------*
*&      Form  add_nodes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_nodes.
**** Node with key 'Root'
* Column1
  CLEAR item.
  item-item_name = 'Column1'.    "#EC NOTEXT " Item of Column 'Column1'
  item-class = cl_column_tree_model=>item_class_text.
*  item-text = 'Root Col. 1'.                                "#EC NOTEXT
  item-text = 'TIM'.                                        "#EC NOTEXT
  APPEND item TO item_table.

* Column2
  CLEAR item.
  item-item_name = 'Column2'.     "
  item-class = cl_column_tree_model=>item_class_text.
  item-text = 'Tool Information Management'.                "#EC NOTEXT
  APPEND item TO item_table.

  CALL METHOD crv_tree_model->add_node
    EXPORTING
      node_key                = 'Root'                      "#EC NOTEXT
      isfolder                = 'X'
      item_table              = item_table
    EXCEPTIONS
      node_key_exists         = 1
      node_key_empty          = 2
      illegal_relationship    = 3
      relative_node_not_found = 4
      error_in_item_table     = 5.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error'.
  ENDIF.




**** Node with key 'Child1'
* Column1
  CLEAR item_table.
  CLEAR item.
  item-item_name = 'Column1'.
  item-class = cl_column_tree_model=>item_class_link.
*  item-text = 'Child1 Col. 1'.                     "#EC NOTEXT
  item-text = 'Operation Master'.                           "#EC NOTEXT
  APPEND item TO item_table.

* Column2
  CLEAR item.
  item-item_name = 'Column2'.     "
  item-class = cl_column_tree_model=>item_class_text.
  item-text = space.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND item TO item_table.


  CALL METHOD crv_tree_model->add_node
    EXPORTING
      node_key                = 'Child1'                    "#EC NOTEXT
      relative_node_key       = 'Root'                      "#EC NOTEXT
      relationship            = cl_tree_model=>relat_last_child
      isfolder                = space
      item_table              = item_table
    EXCEPTIONS
      node_key_exists         = 1
      node_key_empty          = 2
      illegal_relationship    = 3
      relative_node_not_found = 4
      error_in_item_table     = 5.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.


************
* Column1
  CLEAR item_table.
  CLEAR item.
  item-item_name = 'Column1'.
  item-class = cl_column_tree_model=>item_class_link.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  item-text = 'Operation History Management'.               "#EC NOTEXT
  APPEND item TO item_table.

* Column2
  CLEAR item.
  item-item_name = 'Column2'.     "
  item-class = cl_column_tree_model=>item_class_text.
  item-text = space.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND item TO item_table.

  CALL METHOD crv_tree_model->add_node
    EXPORTING
      node_key                = 'Child2'                    "#EC NOTEXT
      relative_node_key       = 'Root'                      "#EC NOTEXT
      relationship            = cl_tree_model=>relat_last_child
      isfolder                = space
      item_table              = item_table
    EXCEPTIONS
      node_key_exists         = 1
      node_key_empty          = 2
      illegal_relationship    = 3
      relative_node_not_found = 4
      error_in_item_table     = 5.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.

************
* Column1
  CLEAR item_table.
  CLEAR item.
  item-item_name = 'Column1'.
  item-class = cl_column_tree_model=>item_class_link.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  item-text = 'Display Item History'.                       "#EC NOTEXT
  APPEND item TO item_table.

* Column2
  CLEAR item.
  item-item_name = 'Column2'.     "
  item-class = cl_column_tree_model=>item_class_text.
  item-text = space.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND item TO item_table.

  CALL METHOD crv_tree_model->add_node
    EXPORTING
      node_key                = 'Child3'                    "#EC NOTEXT
      relative_node_key       = 'Root'                      "#EC NOTEXT
      relationship            = cl_tree_model=>relat_last_child
      isfolder                = space
      item_table              = item_table
    EXCEPTIONS
      node_key_exists         = 1
      node_key_empty          = 2
      illegal_relationship    = 3
      relative_node_not_found = 4
      error_in_item_table     = 5.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.

************
* Column1
  CLEAR item_table.
  CLEAR item.
  item-item_name = 'Column1'.
  item-class = cl_column_tree_model=>item_class_link.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  item-text = 'Display Operation History by Line'.          "#EC NOTEXT
  APPEND item TO item_table.

* Column2
  CLEAR item.
  item-item_name = 'Column2'.     "
  item-class = cl_column_tree_model=>item_class_text.
  item-text = space.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND item TO item_table.

  CALL METHOD crv_tree_model->add_node
    EXPORTING
      node_key                = 'Child4'                    "#EC NOTEXT
      relative_node_key       = 'Root'                      "#EC NOTEXT
      relationship            = cl_tree_model=>relat_last_child
      isfolder                = space
      item_table              = item_table
    EXCEPTIONS
      node_key_exists         = 1
      node_key_empty          = 2
      illegal_relationship    = 3
      relative_node_not_found = 4
      error_in_item_table     = 5.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.

************
* Column1
  CLEAR item_table.
  CLEAR item.
  item-item_name = 'Column1'.
  item-class = cl_column_tree_model=>item_class_link.
*  item-text = 'Child1 Col. 1'.                   "#EC NOTEXT
  item-text = 'Cutting Condition Management'.               "#EC NOTEXT
  APPEND item TO item_table.

* Column2
  CLEAR item.
  item-item_name = 'Column2'.     "
  item-class = cl_column_tree_model=>item_class_text.
  item-text = space.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND item TO item_table.

  CALL METHOD crv_tree_model->add_node
    EXPORTING
      node_key                = 'Child5'                    "#EC NOTEXT
      relative_node_key       = 'Root'                      "#EC NOTEXT
      relationship            = cl_tree_model=>relat_last_child
      isfolder                = space
      item_table              = item_table
    EXCEPTIONS
      node_key_exists         = 1
      node_key_empty          = 2
      illegal_relationship    = 3
      relative_node_not_found = 4
      error_in_item_table     = 5.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.

************
* Column1
  CLEAR item_table.
  CLEAR item.
  item-item_name = 'Column1'.
  item-class = cl_column_tree_model=>item_class_link.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  item-text = 'Display Cutting Condition by Operation'.     "#EC NOTEXT
  APPEND item TO item_table.

* Column2
  CLEAR item.
  item-item_name = 'Column2'.     "
  item-class = cl_column_tree_model=>item_class_text.
  item-text = space.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND item TO item_table.

  CALL METHOD crv_tree_model->add_node
    EXPORTING
      node_key                = 'Child6'                    "#EC NOTEXT
      relative_node_key       = 'Root'                      "#EC NOTEXT
      relationship            = cl_tree_model=>relat_last_child
      isfolder                = space
      item_table              = item_table
    EXCEPTIONS
      node_key_exists         = 1
      node_key_empty          = 2
      illegal_relationship    = 3
      relative_node_not_found = 4
      error_in_item_table     = 5.
  IF sy-subrc <> 0.
    MESSAGE a999(zmmm) WITH 'Error!'.
  ENDIF.


** add 1000 children to node Child1
*  PERFORM node_3.

ENDFORM.                    " add_nodes
*&---------------------------------------------------------------------*
*&      Form  check_dup
*&---------------------------------------------------------------------*
FORM check_dup CHANGING p_w_check_idx.
  FIELD-SYMBOLS: <fs_zsmm_6000_01> LIKE LINE OF it_exdata_zsmm_6000_01.
*IT_exdata_zsmm_6000_01¿¡¼­
  CLEAR it_dup. REFRESH it_dup.
  LOOP AT it_exdata_zsmm_6000_01 ASSIGNING <fs_zsmm_6000_01>.
    READ TABLE it_dup WITH KEY f1 = <fs_zsmm_6000_01>-bukrs
                               f2 = <fs_zsmm_6000_01>-werks
                               f3 = <fs_zsmm_6000_01>-licode
                               f4 = <fs_zsmm_6000_01>-opcode.
    IF sy-subrc = 0.
      p_w_check_idx = p_w_check_idx + 1.
      MESSAGE i999(zmmm) WITH <fs_zsmm_6000_01>-licode
                              <fs_zsmm_6000_01>-opcode
                              ': Duplicate Data!'.
      p_w_check_idx = p_w_check_idx + 1.
      EXIT.
    ELSE.
      MOVE <fs_zsmm_6000_01>-bukrs    TO it_dup-f1.
      MOVE <fs_zsmm_6000_01>-werks    TO it_dup-f2.
      MOVE <fs_zsmm_6000_01>-licode   TO it_dup-f3.
      MOVE <fs_zsmm_6000_01>-opcode   TO it_dup-f4.
      APPEND it_dup.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_dup
*&---------------------------------------------------------------------*
*&      Form  save_6000_01
*&---------------------------------------------------------------------*
FORM save_6000_01_sub.
** For deletion of items
  PERFORM make_it_exdata_zsmm_6000_01del.

  IF NOT it_exdata_zsmm_6000_01del IS  INITIAL.
* Delete from IT_exdata_zsmm_6000_01del.
    PERFORM d_f_it_exdata_zsmm_6000_01del ON COMMIT.
  ELSE.
* If there is no line selection, then Stop Saving.
    CLEAR: wa_exdata_zsmm_6000_01, w_tabix_idx.
    LOOP AT it_exdata_zsmm_6000_01 INTO wa_exdata_zsmm_6000_01
                                   WHERE sel_flag = 'X'.
      w_tabix_idx = sy-tabix.
    ENDLOOP.
    IF w_tabix_idx IS INITIAL.
      MESSAGE i999(zmmm) WITH
        'There is No Selection! Not Saved!'.
      EXIT.
    ENDIF.
  ENDIF.

** For change or insert of items
  DATA: fl_subrc LIKE sy-subrc.    "Flag for sy-subrc
  IF NOT it_exdata_zsmm_6000_01 IS INITIAL.
    LOOP AT it_exdata_zsmm_6000_01 INTO wa_exdata_zsmm_6000_01
                       WHERE zsection <> space                      AND
                             ( licode <> space OR opcode <> space ) AND
                             sel_flag = 'X'.
* Time Stamp Processing
      SELECT SINGLE erdat erzet ernam
       INTO (wa_exdata_zsmm_6000_01-erdat, wa_exdata_zsmm_6000_01-erzet,
              wa_exdata_zsmm_6000_01-ernam)
        FROM ztmm_6000_01
        WHERE bukrs = 'H201'                         AND
              werks = w_werks                          AND
              licode = wa_exdata_zsmm_6000_01-licode AND
              opcode = wa_exdata_zsmm_6000_01-opcode.

      CALL FUNCTION 'Z_FMM_6001_01_TIME_STAMP'
           CHANGING
                ch_erdat = wa_exdata_zsmm_6000_01-erdat
                ch_erzet = wa_exdata_zsmm_6000_01-erzet
                ch_ernam = wa_exdata_zsmm_6000_01-ernam
                ch_aedat = wa_exdata_zsmm_6000_01-aedat
                ch_aezet = wa_exdata_zsmm_6000_01-aezet
                ch_aenam = wa_exdata_zsmm_6000_01-aenam.

      wa_exdata_zsmm_6000_01-bukrs = 'H201'. "Company Code
      wa_exdata_zsmm_6000_01-werks = w_werks.  "Plant

      MODIFY it_exdata_zsmm_6000_01 FROM wa_exdata_zsmm_6000_01.
      IF sy-subrc <> 0. fl_subrc = 8. EXIT. ENDIF.
    ENDLOOP.

    CHECK fl_subrc IS INITIAL.
    PERFORM modify_ztmm_6000_01 ON COMMIT.

    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s999(zmmm) WITH 'Saved Sucessfully!'.
      it_exdata_zsmm_6000_01ori = it_exdata_zsmm_6000_01.
      "reset orginal itab
    ELSE.
      MESSAGE w999(zmmm) WITH 'Error Occurred When Saved'.
    ENDIF.
  ELSE. "Not Initial.  "When All Data Deleted.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " save_6000_01
*&---------------------------------------------------------------------*
*&      Form  MAKE_IT_EXDATA_ZSMM_6000_01DEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_exdata_zsmm_6000_01del.
  CASE save_ok_code.
    WHEN 'DELE'.
      LOOP AT it_exdata_zsmm_6000_01 INTO wa_exdata_zsmm_6000_01
                                   WHERE sel_flag = 'X'.
        APPEND wa_exdata_zsmm_6000_01 TO it_exdata_zsmm_6000_01del.
      ENDLOOP.
    WHEN 'SAVE'.
* Some duplicate code, but this time, there is no harm.
      FIELD-SYMBOLS:
         <fs_exdata_zsmm_6000_01> LIKE wa_exdata_zsmm_6000_01.
      LOOP AT it_exdata_zsmm_6000_01ori INTO wa_exdata_zsmm_6000_01.
        READ TABLE it_exdata_zsmm_6000_01
                 ASSIGNING <fs_exdata_zsmm_6000_01>
                 WITH KEY bukrs  = 'H201'
                          werks  = w_werks
                          licode = wa_exdata_zsmm_6000_01-licode
                          opcode = wa_exdata_zsmm_6000_01-opcode.
        IF sy-subrc = 0.  "Not Deleted
        ELSE.             "Deleted from source
          APPEND wa_exdata_zsmm_6000_01 TO it_exdata_zsmm_6000_01del.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " MAKE_IT_EXDATA_ZSMM_6000_01DEL
*---------------------------------------------------------------------*
*       FORM make_IT_exdata_zsmm_6000_02del                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM make_it_exdata_zsmm_6000_02del
                         CHANGING value(ie_ck_cnt).
  CASE save_ok_code.
    WHEN 'DELE'.
      LOOP AT it_exdata_zsmm_6000_02 ASSIGNING <fs_exdata_zsmm_6000_02>
                                 WHERE sel_flag = 'X'.
        IF <fs_exdata_zsmm_6000_02>-zstatus <> 'C'.
          "Not 'Delete' Status
          MESSAGE i999(zmmm) WITH <fs_exdata_zsmm_6000_02>-licode
                                  ': Check Status !'.
          ie_ck_cnt = ie_ck_cnt + 1.
        ELSE.
          APPEND <fs_exdata_zsmm_6000_02> TO it_exdata_zsmm_6000_02del.
        ENDIF.
      ENDLOOP.
    WHEN 'SAVE'.
* Some duplicate code, but this time, there is no harm.
      FIELD-SYMBOLS:
         <fs_exdata_zsmm_6000_02> LIKE wa_exdata_zsmm_6000_02.
      LOOP AT it_exdata_zsmm_6000_02ori INTO wa_exdata_zsmm_6000_02.
        READ TABLE it_exdata_zsmm_6000_02
                 ASSIGNING <fs_exdata_zsmm_6000_02>
                 WITH KEY bukrs  = 'H201'
                          werks  = w_werks
                          ztcono = wa_exdata_zsmm_6000_02-ztcono
                          matnr  = w_matnr
                          licode = wa_exdata_zsmm_6000_02-licode
                          opcode = wa_exdata_zsmm_6000_02-opcode.
        IF sy-subrc = 0.  "Not Deleted
        ELSE.             "Deleted from source
          APPEND wa_exdata_zsmm_6000_02 TO it_exdata_zsmm_6000_02del.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  d_f_IT_exdata_zsmm_6000_01del
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM d_f_it_exdata_zsmm_6000_01del.
  DELETE ztmm_6000_01 FROM TABLE it_exdata_zsmm_6000_01del.
ENDFORM.                    " d_f_IT_exdata_zsmm_6000_01del
*---------------------------------------------------------------------*
*       FORM d_f_IT_exdata_zsmm_6000_02del                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM d_f_it_exdata_zsmm_6000_02del.
  DELETE ztmm_6000_02 FROM TABLE it_exdata_zsmm_6000_02del.

* Delete ztmm_6000_05
  FIELD-SYMBOLS: <fs_exdata_zsmm_6000_02del>
                    LIKE LINE OF it_exdata_zsmm_6000_02del.

  DATA: ls_ztmm_6000_05 LIKE ztmm_6000_05.

  LOOP AT it_exdata_zsmm_6000_02del
                     ASSIGNING <fs_exdata_zsmm_6000_02del>.

    SELECT SINGLE * INTO ls_ztmm_6000_05
      FROM ztmm_6000_05
      WHERE bukrs  = <fs_exdata_zsmm_6000_02del>-bukrs  AND
            werks  = <fs_exdata_zsmm_6000_02del>-werks  AND
            matnr  = <fs_exdata_zsmm_6000_02del>-matnr  AND
            licode = <fs_exdata_zsmm_6000_02del>-licode AND
            opcode = <fs_exdata_zsmm_6000_02del>-opcode.
*        ZTVCD
    IF sy-subrc = 0.
      DELETE FROM ztmm_6000_05
        WHERE bukrs  = <fs_exdata_zsmm_6000_02del>-bukrs  AND
              werks  = <fs_exdata_zsmm_6000_02del>-werks  AND
              matnr  = <fs_exdata_zsmm_6000_02del>-matnr  AND
              licode = <fs_exdata_zsmm_6000_02del>-licode AND
              opcode = <fs_exdata_zsmm_6000_02del>-opcode.
*        ZTVCD
    ENDIF.
  ENDLOOP.
ENDFORM.                    " d_f_IT_exdata_zsmm_6000_02del
*&---------------------------------------------------------------------*
*&      Form  modify_ztmm_6000_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_ztmm_6000_01.
*  MODIFY ztmm_6000_01 FROM TABLE IT_exdata_zsmm_6000_01.
  CLEAR: w_tabix_idx, wa_exdata_zsmm_6000_01.
  LOOP AT it_exdata_zsmm_6000_01 INTO wa_exdata_zsmm_6000_01
                                   WHERE sel_flag = 'X'.
    MODIFY ztmm_6000_01 FROM wa_exdata_zsmm_6000_01.
  ENDLOOP.
ENDFORM.                    " modify_ztmm_6000_01
*---------------------------------------------------------------------*
*       FORM modify_ztmm_6000_02                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM modify_ztmm_6000_02.
*  MODIFY ztmm_6000_02 FROM TABLE IT_exdata_zsmm_6000_02.
  CLEAR: w_tabix_idx, wa_exdata_zsmm_6000_02.
  LOOP AT it_exdata_zsmm_6000_02 INTO wa_exdata_zsmm_6000_02
                                   WHERE sel_flag = 'X'.
    MODIFY ztmm_6000_02 FROM wa_exdata_zsmm_6000_02.
  ENDLOOP.
ENDFORM.                    " modify_ztmm_6000_02
*&---------------------------------------------------------------------*
*&      Form  check_dup_6000_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXDATA_ZSMM_6000_02  text
*      <--P_w_check_idx  text
*----------------------------------------------------------------------*
FORM check_dup_6000_02 TABLES   p_itab STRUCTURE wa_exdata_zsmm_6000_02
                       CHANGING value(p_w_check_idx).
  FIELD-SYMBOLS: <fs_itab> LIKE LINE OF p_itab.
** For duplicate check
  DATA: BEGIN OF it_dup OCCURS 0,
          f1(30),
          f2(30),
          f3(30),
          f4(30),
          f5(30),
          f6(30),
        END OF it_dup.

* IN P_ITAB
  CLEAR it_dup. REFRESH it_dup.
  LOOP AT p_itab ASSIGNING <fs_itab>.
    READ TABLE it_dup WITH KEY f1 = <fs_itab>-bukrs
                               f2 = <fs_itab>-werks
                               f3 = <fs_itab>-ztcono
                               f4 = <fs_itab>-matnr
                               f5 = <fs_itab>-licode
                               f6 = <fs_itab>-opcode.
    IF sy-subrc = 0.
      p_w_check_idx = p_w_check_idx + 1.
      MESSAGE i999(zmmm) WITH <fs_itab>-licode
                              <fs_itab>-opcode
                              ': Duplicate Data!'.
      p_w_check_idx = p_w_check_idx + 1.
      EXIT.
    ELSE.
      MOVE <fs_itab>-bukrs    TO it_dup-f1.
      MOVE <fs_itab>-werks    TO it_dup-f2.
      MOVE <fs_itab>-ztcono   TO it_dup-f3.
      MOVE <fs_itab>-matnr    TO it_dup-f4.
      MOVE <fs_itab>-licode   TO it_dup-f5.
      MOVE <fs_itab>-opcode   TO it_dup-f6.
      APPEND it_dup.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "check_dup
*&---------------------------------------------------------------------*
*&      Form  save_6000_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_6000_02_sub.
  DATA: ck_cnt LIKE sy-subrc.
*** For deletion of items
  PERFORM make_it_exdata_zsmm_6000_02del CHANGING ck_cnt.

  IF NOT it_exdata_zsmm_6000_02del IS  INITIAL.
* Delete from IT_exdata_zsmm_6000_01del.
    PERFORM d_f_it_exdata_zsmm_6000_02del ON COMMIT.
  ELSE.
* If there is no line selection, then Stop Saving.
    CLEAR: wa_exdata_zsmm_6000_02, w_tabix_idx.
    LOOP AT it_exdata_zsmm_6000_02 INTO wa_exdata_zsmm_6000_02
                                   WHERE sel_flag = 'X'.
      w_tabix_idx = sy-tabix.
    ENDLOOP.
    IF w_tabix_idx IS INITIAL.
      MESSAGE i999(zmmm) WITH
        'There is No Selection! Not Saved!'.
      EXIT.
    ENDIF.
  ENDIF.

** For change or insert of items

  IF NOT it_exdata_zsmm_6000_02 IS INITIAL.

    LOOP AT it_exdata_zsmm_6000_02 INTO wa_exdata_zsmm_6000_02
                                   WHERE licode <> space.
      IF wa_exdata_zsmm_6000_02-zstatus = 'A'. "Add
        IF w_ztcono IS INITIAL OR w_zapplied_date IS INITIAL.
          MESSAGE i999(zmmm) WITH wa_exdata_zsmm_6000_02-licode
                                  ': Check TCO No. and Applied Date !'.
          ck_cnt = ck_cnt + 1.
          EXIT.
        ENDIF.
      ELSE.

      ENDIF.

* Status Check
      IF wa_exdata_zsmm_6000_02-zstatus_before <> space.
*        IF wa_exdata_zsmm_6000_02-zstatus  = space AND
        IF wa_exdata_zsmm_6000_02-zstatus  <> 'B' AND "Change
           wa_exdata_zsmm_6000_02-sel_flag = 'X'.

          MESSAGE i999(zmmm) WITH wa_exdata_zsmm_6000_02-licode
                                  ': Check Status !'.
          ck_cnt = ck_cnt + 1.
        ENDIF.
      ENDIF.


* Time Stamp Processing
      SELECT SINGLE erdat erzet ernam
       INTO (wa_exdata_zsmm_6000_02-erdat, wa_exdata_zsmm_6000_02-erzet,
              wa_exdata_zsmm_6000_02-ernam)
        FROM ztmm_6000_02
        WHERE bukrs  = 'H201'                        AND
              werks  = w_werks                       AND
              ztcono = w_ztcono                      AND
              matnr  = w_matnr                       AND
              licode = wa_exdata_zsmm_6000_02-licode AND
              opcode = wa_exdata_zsmm_6000_02-opcode.

      CALL FUNCTION 'Z_FMM_6001_01_TIME_STAMP'
           CHANGING
                ch_erdat = wa_exdata_zsmm_6000_02-erdat
                ch_erzet = wa_exdata_zsmm_6000_02-erzet
                ch_ernam = wa_exdata_zsmm_6000_02-ernam
                ch_aedat = wa_exdata_zsmm_6000_02-aedat
                ch_aezet = wa_exdata_zsmm_6000_02-aezet
                ch_aenam = wa_exdata_zsmm_6000_02-aenam.

      wa_exdata_zsmm_6000_02-bukrs  = 'H201'.  "Company Code
      wa_exdata_zsmm_6000_02-matnr  = w_matnr.   "Material
      wa_exdata_zsmm_6000_02-werks  = w_werks.   "Plant

      IF wa_exdata_zsmm_6000_02-sel_flag = 'X'. "Selected
        IF wa_exdata_zsmm_6000_02-zstatus = 'A'.  "Add
          wa_exdata_zsmm_6000_02-ztcono = w_ztcono.  "TCO Number
          wa_exdata_zsmm_6000_02-zapplied_date = w_zapplied_date.
          "Applied Date
          wa_exdata_zsmm_6000_02-zch_desc_cd     = w_zch_desc_cd.
          wa_exdata_zsmm_6000_02-zch_reason_cd   = w_zch_reason_cd.
          wa_exdata_zsmm_6000_02-zinv_process_cd = w_zinv_process_cd.
          wa_exdata_zsmm_6000_02-zch_matnr1  = w_zch_matnr1.
          wa_exdata_zsmm_6000_02-zch_matnr2  = w_zch_matnr2.
          wa_exdata_zsmm_6000_02-zxfer_matnr = w_zxfer_matnr.
        ENDIF.
      ENDIF.

* Get Section
      SELECT SINGLE zsection INTO wa_exdata_zsmm_6000_02-zsection
        FROM ztmm_6000_01
        WHERE bukrs   = 'H201'                        AND
              werks   = wa_exdata_zsmm_6000_02-werks  AND
              licode  = wa_exdata_zsmm_6000_02-licode AND
              opcode  = wa_exdata_zsmm_6000_02-opcode.

      MODIFY it_exdata_zsmm_6000_02 FROM wa_exdata_zsmm_6000_02.
      IF sy-subrc <> 0. ck_cnt = ck_cnt + 1. EXIT. ENDIF.
    ENDLOOP.

* TCO Number Duplicate Check
    PERFORM check_dup_tco_no TABLES   it_exdata_zsmm_6000_02
                             CHANGING w_check_idx.
    IF NOT w_check_idx IS INITIAL.
      ck_cnt = ck_cnt + w_check_idx.
    ENDIF.


    CHECK ck_cnt IS INITIAL.
    PERFORM modify_ztmm_6000_02 ON COMMIT.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s999(zmmm) WITH 'Saved Sucessfully!'.
      it_exdata_zsmm_6000_02ori = it_exdata_zsmm_6000_02.
      "Reset orginal itab
      CLEAR: it_exdata_zsmm_6000_02del.  "Reset
    ELSE.
      MESSAGE w999(zmmm) WITH 'Error Occurred When Saved'.
    ENDIF.
  ELSE. "Not Initial.  "When All Data Deleted.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " save_6000_02
*&---------------------------------------------------------------------*
*&      Form  disp_6000_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM disp_6000_01.
  CLEAR: w_where_condition, it_where_condition.

  CONCATENATE 'WERKS = ''' w_werks '''' INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.

  IF NOT w_licode IS INITIAL.
    CONCATENATE 'AND LICODE LIKE ''' w_licode '%' ''''
                                    INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.
  IF NOT w_opcode IS INITIAL.
    CONCATENATE 'AND OPCODE LIKE ''' w_opcode '%' ''''
                                    INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.
  IF NOT cblicode IS INITIAL.
    CONCATENATE 'AND ZSECTION = ''' 'A' ''''
                                        INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_exdata_zsmm_6000_01
    FROM ztmm_6000_01
    WHERE (it_where_condition).

* Reserve IT_exdata_zsmm_6000_01ori for UnDo
  it_exdata_zsmm_6000_01ori = it_exdata_zsmm_6000_01.

ENDFORM.                    " disp_6000_01
*&---------------------------------------------------------------------*
*&      Form  disp_6000_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM disp_6000_02.
  CLEAR: w_where_condition, it_where_condition.

  CONCATENATE 'WERKS = ''' w_werks '''' INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.

  CONCATENATE 'AND MATNR = ''' w_matnr ''''
                                  INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.

  IF NOT w_ztconodisp IS INITIAL.
    CONCATENATE 'AND ZTCONO = ''' w_ztconodisp ''''
                                    INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_exdata_zsmm_6000_02
    FROM ztmm_6000_02
    WHERE (it_where_condition).

* Get Status Before.
  LOOP AT it_exdata_zsmm_6000_02 ASSIGNING <fs_exdata_zsmm_6000_02>.
    <fs_exdata_zsmm_6000_02>-zstatus_before
                       = <fs_exdata_zsmm_6000_02>-zstatus.
    IF NOT <fs_exdata_zsmm_6000_02>-zstatus IS INITIAL.
      CLEAR <fs_exdata_zsmm_6000_02>-zstatus.
    ENDIF.
  ENDLOOP.

* If NOT w_ztconodisp IS INITIAL
  IF NOT ( it_exdata_zsmm_6000_02 IS INITIAL OR
           w_ztconodisp IS INITIAL ).
   READ TABLE it_exdata_zsmm_6000_02 ASSIGNING <fs_exdata_zsmm_6000_02>
                                                         INDEX 1.
    w_ztcono           = <fs_exdata_zsmm_6000_02>-ztcono.
    w_zapplied_date    = <fs_exdata_zsmm_6000_02>-zapplied_date.
    w_erdat            = <fs_exdata_zsmm_6000_02>-erdat.
    w_zch_desc_cd      = <fs_exdata_zsmm_6000_02>-zch_desc_cd.
    w_zch_reason_cd    = <fs_exdata_zsmm_6000_02>-zch_reason_cd.
    w_zinv_process_cd  = <fs_exdata_zsmm_6000_02>-zinv_process_cd.
    w_zch_matnr1       = <fs_exdata_zsmm_6000_02>-zch_matnr1.
    w_zch_matnr2       = <fs_exdata_zsmm_6000_02>-zch_matnr2.
    w_zxfer_matnr      = <fs_exdata_zsmm_6000_02>-zxfer_matnr.
    w_trtyp            = 'A'.  "For Display mode
  ELSE.
    CLEAR: w_ztcono, w_zapplied_date, w_erdat,
           w_zch_matnr1, w_zch_matnr2,
           w_trtyp.
    w_zch_desc_cd = w_zch_reason_cd = w_zinv_process_cd = '1'.
  ENDIF.

* Reserving IT_exdata_zsmm_6000_02ori for UnDo
  it_exdata_zsmm_6000_02ori = it_exdata_zsmm_6000_02.

ENDFORM.                    " disp_6000_02
*&---------------------------------------------------------------------*
*&      Form  disp_6000_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM disp_6000_05.
  CLEAR: w_where_condition, it_where_condition.

  CONCATENATE 'WERKS = ''' w_werks '''' INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.

  CONCATENATE 'AND MATNR = ''' w_matnr ''''
                                  INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.

  IF NOT w_licode IS INITIAL.
    CONCATENATE 'AND LICODE = ''' w_licode ''''
                                    INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.

  IF NOT w_opcode IS INITIAL.
    CONCATENATE 'AND OPCODE = ''' w_opcode ''''
                                    INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.

  SELECT bukrs werks matnr licode opcode ztvcd zcutseq zunitqty
    INTO CORRESPONDING FIELDS OF TABLE it_exdata_zsmm_6000_05
    FROM ztmm_6000_02
    WHERE (it_where_condition)
    ORDER BY bukrs werks matnr licode opcode ztvcd.

  DELETE ADJACENT DUPLICATES FROM it_exdata_zsmm_6000_05
                   COMPARING bukrs werks matnr licode opcode ztvcd.


  DATA: lv_zcutseq  LIKE zsmm_6000_05-zcutseq.  "Cutting Seq
  DATA: lv_zunitqty LIKE zsmm_6000_05-zunitqty. "Unit Quantity
  LOOP AT it_exdata_zsmm_6000_05 ASSIGNING <fs_exdata_zsmm_6000_05>.
    CLEAR: wa_exdata_zsmm_6000_05.
    SELECT SINGLE * INTO wa_exdata_zsmm_6000_05
      FROM ztmm_6000_05
      WHERE bukrs  = <fs_exdata_zsmm_6000_05>-bukrs  AND
            werks  = <fs_exdata_zsmm_6000_05>-werks  AND
            matnr  = <fs_exdata_zsmm_6000_05>-matnr  AND
            licode = <fs_exdata_zsmm_6000_05>-licode AND
            opcode = <fs_exdata_zsmm_6000_05>-opcode AND
            ztvcd  = <fs_exdata_zsmm_6000_05>-ztvcd.
    IF sy-subrc = 0.
      lv_zunitqty = <fs_exdata_zsmm_6000_05>-zunitqty.
      "Unit Quantity
      lv_zcutseq = <fs_exdata_zsmm_6000_05>-zcutseq.
      "Cutting Sequence
      <fs_exdata_zsmm_6000_05> = wa_exdata_zsmm_6000_05.
      <fs_exdata_zsmm_6000_05>-zunitqty = lv_zunitqty.
      <fs_exdata_zsmm_6000_05>-zcutseq  = lv_zcutseq.
      CLEAR: lv_zcutseq, lv_zunitqty.
    ENDIF.
  ENDLOOP.

* Reserve IT_exdata_zsmm_6000_05ori for UnDo
  it_exdata_zsmm_6000_05ori = it_exdata_zsmm_6000_05.

ENDFORM.                    " disp_6000_05
*&---------------------------------------------------------------------*
*&      Form  save_6000_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_6000_05_sub.
** For change or insert of items
  DATA: fl_subrc LIKE sy-subrc.        "Flag for sy-subrc
  CLEAR: w_tabix_idx.
  IF NOT it_exdata_zsmm_6000_05 IS INITIAL.
    LOOP AT it_exdata_zsmm_6000_05 INTO wa_exdata_zsmm_6000_05
                   WHERE sel_flag = 'X'.
      w_tabix_idx = sy-tabix.
* Time Stamp Processing
      SELECT SINGLE erdat erzet ernam
       INTO (wa_exdata_zsmm_6000_05-erdat, wa_exdata_zsmm_6000_05-erzet,
              wa_exdata_zsmm_6000_05-ernam)
        FROM ztmm_6000_05
        WHERE bukrs  = 'H201'                        AND
              werks  = w_werks                         AND
              matnr  = w_matnr                         AND
              licode = wa_exdata_zsmm_6000_05-licode AND
              opcode = wa_exdata_zsmm_6000_05-opcode AND
              ztvcd  = wa_exdata_zsmm_6000_05-ztvcd.

      CALL FUNCTION 'Z_FMM_6001_01_TIME_STAMP'
           CHANGING
                ch_erdat = wa_exdata_zsmm_6000_05-erdat
                ch_erzet = wa_exdata_zsmm_6000_05-erzet
                ch_ernam = wa_exdata_zsmm_6000_05-ernam
                ch_aedat = wa_exdata_zsmm_6000_05-aedat
                ch_aezet = wa_exdata_zsmm_6000_05-aezet
                ch_aenam = wa_exdata_zsmm_6000_05-aenam.

      wa_exdata_zsmm_6000_05-bukrs  = 'H201'. "Company Code
      wa_exdata_zsmm_6000_05-matnr  = w_matnr.  "Material
      wa_exdata_zsmm_6000_05-werks  = w_werks.  "Plant

* Get Section
      SELECT SINGLE zsection INTO wa_exdata_zsmm_6000_05-zsection
        FROM ztmm_6000_01
        WHERE bukrs  = 'H201'    AND
              werks  = wa_exdata_zsmm_6000_05-werks  AND
              licode = wa_exdata_zsmm_6000_05-licode AND
              opcode  = wa_exdata_zsmm_6000_05-opcode.

      MODIFY it_exdata_zsmm_6000_05 FROM wa_exdata_zsmm_6000_05.
      IF sy-subrc <> 0. fl_subrc = 8. EXIT. ENDIF.
    ENDLOOP.
    IF w_tabix_idx IS INITIAL.
      MESSAGE i999(zmmm) WITH 'There is no selection! Not saved!'.
      EXIT.
    ENDIF.
    CHECK fl_subrc IS INITIAL.
    PERFORM modify_ztmm_6000_05 ON COMMIT.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s999(zmmm) WITH 'Saved Sucessfully!'.
      it_exdata_zsmm_6000_05ori = it_exdata_zsmm_6000_05.
      "reset orginal itab
    ELSE.
      MESSAGE w999(zmmm) WITH 'Error Occurred When Saved'.
    ENDIF.
  ENDIF.
ENDFORM.                    " save_6000_05
*&---------------------------------------------------------------------*
*&      Form  modify_ztmm_6000_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_ztmm_6000_05.
*  MODIFY ztmm_6000_05 FROM TABLE IT_exdata_zsmm_6000_05.
  CLEAR: w_tabix_idx, wa_exdata_zsmm_6000_05.
  LOOP AT it_exdata_zsmm_6000_05 INTO wa_exdata_zsmm_6000_05
                                   WHERE sel_flag = 'X'.
    MODIFY ztmm_6000_05 FROM wa_exdata_zsmm_6000_05.

    UPDATE ztmm_6000_02
         SET zcutseq  = wa_exdata_zsmm_6000_05-zcutseq
         WHERE bukrs  = wa_exdata_zsmm_6000_05-bukrs  AND
               werks  = wa_exdata_zsmm_6000_05-werks  AND
*               ztcono = wa_exdata_zsmm_6000_05-ztcono AND
               matnr  = wa_exdata_zsmm_6000_05-matnr  AND
               licode = wa_exdata_zsmm_6000_05-licode AND
               opcode = wa_exdata_zsmm_6000_05-opcode.
  ENDLOOP.
ENDFORM.                    " modify_ztmm_6000_05
*&---------------------------------------------------------------------*
*&      Form  clear
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear.
  CHECK w_once_flg IS INITIAL.
  CLEAR:
    cblicode,
    it_exdata_zsmm_6000_01,
    it_exdata_zsmm_6000_01del,
    it_exdata_zsmm_6000_01ori,

    it_exdata_zsmm_6000_02,
    it_exdata_zsmm_6000_02del,
    it_exdata_zsmm_6000_02ori,

    it_exdata_zsmm_6000_05,
    it_exdata_zsmm_6000_05del,
    it_exdata_zsmm_6000_05ori,

    w_licode,
    w_matnr,
    w_opcode,
    w_werks,
    w_zapplied_date,
    w_zch_desc_cd,
    w_zch_matnr1,
    w_zch_matnr2,
    w_zch_reason_cd,
    w_zinv_process_cd,
    w_ztcono,
    w_ztconodisp,
    w_zxfer_matnr.

  CASE ss_0110.
    WHEN 9001.
    WHEN 9002.
    WHEN 9003.
    WHEN 9004.
    WHEN 9005.
    WHEN 9006.
    WHEN 9007.
  ENDCASE.
ENDFORM.                    " clear
*&---------------------------------------------------------------------*
*&      Form  tree_off
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tree_off.
  CALL METHOD crv_docking_container->set_visible( c_visible_false ).
  dynftext = c_tree_on.
ENDFORM.                    " tree_off
*&---------------------------------------------------------------------*
*&      Form  disp_6000_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM disp_6000_03.
  CLEAR: w_where_condition, it_where_condition.

  CONCATENATE 'WERKS = ''' w_werks '''' INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.

  CONCATENATE 'AND MATNR = ''' w_matnr ''''
                                  INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.

  IF NOT w_ztcono IS INITIAL.
    CONCATENATE 'AND ZTCONO = ''' w_ztcono ''''
                                    INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_exdata_zsmm_6000_03
    FROM ztmm_6000_02
    WHERE (it_where_condition).
* Get Code Description
  LOOP AT it_exdata_zsmm_6000_03 ASSIGNING <fs_exdata_zsmm_6000_03>.
    SELECT SINGLE zname INTO <fs_exdata_zsmm_6000_03>-desc_zch_desc
      FROM ztmm_ch_desc_cd
      WHERE zch_desc_cd = <fs_exdata_zsmm_6000_03>-zch_desc_cd.

    SELECT SINGLE zname INTO <fs_exdata_zsmm_6000_03>-desc_zch_reason
      FROM ztmm_ch_reas_cd
      WHERE zch_reason_cd = <fs_exdata_zsmm_6000_03>-zch_reason_cd.
  ENDLOOP.


* Reserve IT_exdata_zsmm_6000_03ori for UnDo
  it_exdata_zsmm_6000_03ori = it_exdata_zsmm_6000_03.

ENDFORM.                    " disp_6000_03
*&---------------------------------------------------------------------*
*&      Form  disp_6000_04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM disp_6000_04.
  CLEAR: w_where_condition, it_where_condition.

  CONCATENATE 'WERKS = ''' w_werks '''' INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.

  IF NOT w_matnr IS INITIAL.
    CONCATENATE 'AND MATNR = ''' w_matnr ''''
                                    INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.
*  IF NOT W_LICODE IS INITIAL.
  CONCATENATE 'AND LICODE = ''' w_licode ''''
                                  INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.
*  ENDIF.

  IF NOT w_opcode IS INITIAL.
    CONCATENATE 'AND OPCODE = ''' w_opcode ''''
                                    INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.

  SELECT matnr ztcono zapplied_date
    INTO CORRESPONDING FIELDS OF TABLE it_exdata_zsmm_6000_04
    FROM ztmm_6000_02
    WHERE (it_where_condition).

  LOOP AT it_exdata_zsmm_6000_04 ASSIGNING <fs_exdata_zsmm_6000_04>.
    CLEAR: wa_exdata_zsmm_6000_04.
    SELECT SINGLE maktx
      INTO wa_exdata_zsmm_6000_04-maktx
      FROM makt
      WHERE matnr  = <fs_exdata_zsmm_6000_04>-matnr  AND
            spras  = sy-langu.
    IF sy-subrc = 0.
      <fs_exdata_zsmm_6000_04>-maktx = wa_exdata_zsmm_6000_04-maktx.
    ENDIF.
  ENDLOOP.

* Reserve IT_exdata_zsmm_6000_04ori for UnDo
  it_exdata_zsmm_6000_04ori = it_exdata_zsmm_6000_04.


ENDFORM.                    " disp_6000_04
*&---------------------------------------------------------------------*
*&      Form  disp_6000_06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM disp_6000_06.
  CLEAR: w_where_condition, it_where_condition.

  CONCATENATE 'WERKS = ''' w_werks '''' INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.

  CONCATENATE 'AND MATNR = ''' w_matnr ''''
                                  INTO w_where_condition.
  APPEND w_where_condition TO it_where_condition.

  IF NOT w_licode IS INITIAL.
    CONCATENATE 'AND LICODE = ''' w_licode ''''
                                    INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.

  IF NOT w_opcode IS INITIAL.
    CONCATENATE 'AND OPCODE = ''' w_opcode ''''
                                    INTO w_where_condition.
    APPEND w_where_condition TO it_where_condition.
  ENDIF.

  SELECT bukrs werks matnr licode opcode ztvcd
    INTO CORRESPONDING FIELDS OF TABLE it_exdata_zsmm_6000_06
    FROM ztmm_6000_02
    WHERE (it_where_condition)
    ORDER BY bukrs werks matnr licode opcode ztvcd.
  DELETE ADJACENT DUPLICATES FROM it_exdata_zsmm_6000_06
                   COMPARING bukrs werks matnr licode opcode ztvcd.

  LOOP AT it_exdata_zsmm_6000_06 ASSIGNING <fs_exdata_zsmm_6000_06>.
    CLEAR: wa_exdata_zsmm_6000_06.
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF wa_exdata_zsmm_6000_06
      FROM ztmm_6000_05
      WHERE bukrs  = <fs_exdata_zsmm_6000_06>-bukrs  AND
            werks  = <fs_exdata_zsmm_6000_06>-werks  AND
            matnr  = <fs_exdata_zsmm_6000_06>-matnr  AND
            licode = <fs_exdata_zsmm_6000_06>-licode AND
            opcode = <fs_exdata_zsmm_6000_06>-opcode.
    IF sy-subrc = 0.
* Get desc of Line Code.
      PERFORM desc_licode USING    wa_exdata_zsmm_6000_06-licode "CODE
                                   wa_exdata_zsmm_6000_06-bukrs "COMPANY
                                   wa_exdata_zsmm_6000_06-werks "PLANT
                          CHANGING wa_exdata_zsmm_6000_06-linm.  "DESC
* Get matnr description (MAKT-MAKTX)
      PERFORM desc_matnr USING    sy-langu
                                  wa_exdata_zsmm_6000_06-matnr
                         CHANGING wa_exdata_zsmm_6000_06-maktx.

*
      PERFORM ztlife.
      PERFORM zcut_v.
      PERFORM zfeed_count.
      <fs_exdata_zsmm_6000_06> = wa_exdata_zsmm_6000_06.
    ENDIF.
  ENDLOOP.

* Reserve IT_exdata_zsmm_6000_06ori for UnDo
  it_exdata_zsmm_6000_06ori = it_exdata_zsmm_6000_06.

ENDFORM.                    " disp_6000_06
*&---------------------------------------------------------------------*
*&      Form  ztlife
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ztlife.
*  WA_exdata_zsmm_6000_06-ztlife = WA_exdata_zsmm_6000_06-ztool_ch_cycle
*                           WA_exdata_zsmm_6000_06-ZUSE_COUNT /
*                           WA_exdata_zsmm_6000_06-unit number.
*or
  wa_exdata_zsmm_6000_06-ztlife = wa_exdata_zsmm_6000_06-ztool_ch_cycle
                           * wa_exdata_zsmm_6000_06-zuse_count /
                             wa_exdata_zsmm_6000_06-zedge_count.

ENDFORM.                    " ztlife

*&---------------------------------------------------------------------*
*&      Form  zcut_v
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zcut_v.
  DATA: phi LIKE zsmm_6000_05-zcut_v VALUE '3.14'.
  wa_exdata_zsmm_6000_06-zcut_v = phi *
                           wa_exdata_zsmm_6000_06-zrpm *
                           wa_exdata_zsmm_6000_06-ztool_diameter /
                           1000.
ENDFORM.                    " zcut_v

*&---------------------------------------------------------------------*
*&      Form  zfeed_count
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zfeed_count.
  wa_exdata_zsmm_6000_06-zfeed_count =
                          wa_exdata_zsmm_6000_06-ztransfer_qty *
                          wa_exdata_zsmm_6000_06-zrpm.
ENDFORM.                    " zfeed_count
*&---------------------------------------------------------------------*
*&      Form  desc_licode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EXDATA_ZSMM_6000_06_LICODE  text
*      <--P_WA_EXDATA_ZSMM_6000_06_LINM  text
*----------------------------------------------------------------------*
FORM desc_licode USING    value(p_licode)
                          value(p_bukrs)
                          value(p_werks)
                 CHANGING value(p_linm).
  CLEAR: p_linm.
  SELECT SINGLE linm
    INTO p_linm
    FROM ztmm_6000_01
    WHERE bukrs  = p_bukrs AND
          werks  = p_werks AND
          licode = p_licode.

ENDFORM.                    " desc_licode
*&---------------------------------------------------------------------*
*&      Form  desc_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_LANGU  text
*      -->P_WA_EXDATA_ZSMM_6000_06_MATNR  text
*      <--P_WA_EXDATA_ZSMM_6000_06_MAKTX  text
*----------------------------------------------------------------------*
FORM desc_matnr USING    value(p_langu)
                         value(p_matnr) LIKE mara-matnr
                CHANGING value(p_maktx) LIKE makt-maktx.
  CLEAR: p_maktx.
  SELECT SINGLE maktx INTO p_maktx
    FROM  makt
    WHERE spras = p_langu AND
          matnr = p_matnr.
ENDFORM.                    "desc_matnr
*&---------------------------------------------------------------------*
*&      Form  title
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM title USING    value(p_dynnr)
           CHANGING value(p_title).
  CASE p_dynnr.
    WHEN 9001.
      p_title = 'Operation Master'.
    WHEN 9002.
      p_title = 'Operation History Management'.
    WHEN 9003.
      p_title = 'Display Item History'.
    WHEN 9004.
      p_title = 'Display Operation History by Line'.
    WHEN 9005.
      p_title = 'Cutting Condition Management'.
    WHEN 9006.
      p_title = 'Display Cutting Condition by Operation'.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_6000_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_6000_01.
* Check Plant
  DATA: wa_t001w LIKE t001w. CLEAR wa_t001w.
  SELECT SINGLE * INTO wa_t001w
    FROM t001w
    WHERE werks = w_werks.
  IF sy-subrc <> 0 OR w_werks IS INITIAL.
    MESSAGE i999(zmmm) WITH 'Plant is invalid!'. EXIT.
  ENDIF.

* If IT_exdata_zsmm_6000_01 is initial. Then Stop Saving.

  DELETE it_exdata_zsmm_6000_01 WHERE licode = space.
  " Make itab without initial line.
*  IF IT_exdata_zsmm_6000_01 IS INITIAL.
*    MESSAGE i999(zmmm) WITH
*      'There is No Data! Not Saved!'.
*    EXIT.
*  ENDIF.
*
  READ TABLE it_exdata_zsmm_6000_01
      INTO wa_exdata_zsmm_6000_01
      WITH KEY zsection = 'B'
               opcode = space.
  IF sy-subrc = 0.
    MESSAGE i999(zmmm) WITH 'Line' sy-tabix
                            ': Check Operation Code!'.
    EXIT.
  ENDIF.

*  CHECK NOT IT_exdata_zsmm_6000_01 IS INITIAL.
  CLEAR w_check_idx.
  PERFORM check_dup CHANGING w_check_idx.
  CHECK w_check_idx IS INITIAL.
  PERFORM save_6000_01_sub.
ENDFORM.                    " SAVE_6000_01
*&---------------------------------------------------------------------*
*&      Form  save_6000_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_6000_05.
  IF w_matnr IS INITIAL.
    MESSAGE i999(zmmm) WITH 'Fill the Material number!'.
    EXIT.
  ELSEIF w_werks IS INITIAL.
    MESSAGE i999(zmmm) WITH 'Fill the Plant!'.
    EXIT.
  ENDIF.

  IF it_exdata_zsmm_6000_05 IS INITIAL.
    MESSAGE i999(zmmm) WITH
      'There is No Data! Not Saved!'.
    EXIT.
  ENDIF.
  CHECK NOT it_exdata_zsmm_6000_05 IS INITIAL.
  PERFORM save_6000_05_sub.
ENDFORM.                    " save_6000_05
*&---------------------------------------------------------------------*
*&      Form  save_6000_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_6000_02.
  IF w_matnr IS INITIAL.
    MESSAGE w999(zmmm) WITH 'Fill the Material number!'.
    EXIT.
  ELSEIF w_werks IS INITIAL.
    MESSAGE w999(zmmm) WITH 'Fill the Plant!'.
    EXIT.
  ENDIF.

* If IT_exdata_zsmm_6000_02 is initial. Then Stop Saving.
  DELETE it_exdata_zsmm_6000_02 WHERE licode = space.
  " Make itab without initial line.

*
  CLEAR w_check_idx.
  PERFORM check_dup_6000_02 TABLES   it_exdata_zsmm_6000_02
                            CHANGING w_check_idx.

  CHECK w_check_idx IS INITIAL.
  PERFORM save_6000_02_sub.
ENDFORM.                    " save_6000_02
*&---------------------------------------------------------------------*
*&      Form  check_dup_tco_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXDATA_ZSMM_6000_02  text
*      <--P_W_CHECK_IDX  text
*----------------------------------------------------------------------*
FORM check_dup_tco_no  TABLES   p_itab STRUCTURE wa_exdata_zsmm_6000_02
                       CHANGING value(p_w_check_idx).
  FIELD-SYMBOLS: <fs_itab> LIKE LINE OF p_itab.
** For duplicate check
  DATA: BEGIN OF it_dup OCCURS 0,
          f1(30),
          f2(30),
          f3(30),
          f4(30),
          f5(30),
          f6(30),
        END OF it_dup.

* IN P_ITAB
  CLEAR it_dup. REFRESH it_dup.
  LOOP AT p_itab ASSIGNING <fs_itab>.
    READ TABLE it_dup WITH KEY f1 = <fs_itab>-bukrs
                               f2 = <fs_itab>-werks
                               f3 = <fs_itab>-ztcono
                               f4 = <fs_itab>-matnr.
*                               f5 = <fs_itab>-licode
*                               f6 = <fs_itab>-opcode.
    IF sy-subrc = 0.
      p_w_check_idx = p_w_check_idx + 1.
      MESSAGE i999(zmmm) WITH <fs_itab>-ztcono
                              ': Duplicate Data!'.
      EXIT.
    ELSE.
      MOVE <fs_itab>-bukrs    TO it_dup-f1.
      MOVE <fs_itab>-werks    TO it_dup-f2.
      MOVE <fs_itab>-ztcono   TO it_dup-f3.
      MOVE <fs_itab>-matnr    TO it_dup-f4.
*      MOVE <fs_itab>-licode   TO it_dup-f5.
*      MOVE <fs_itab>-opcode   TO it_dup-f6.
      APPEND it_dup.
    ENDIF.
  ENDLOOP.
ENDFORM.
