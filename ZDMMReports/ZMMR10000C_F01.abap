*----------------------------------------------------------------------*
*   INCLUDE ZMMR10000C_F01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_TREE_FCAT  text
*----------------------------------------------------------------------*
FORM change TABLES  pt_fieldcat TYPE lvc_t_fcat.

  DATA:l_fieldcat_s LIKE pt_fieldcat,
        l_name(3).

  LOOP AT pt_fieldcat INTO l_fieldcat_s.

    CASE l_fieldcat_s-fieldname.
      WHEN 'TOTCNT' OR 'SUCCNT' OR 'ERRCNT'.
        l_fieldcat_s-outputlen = '3'.
      WHEN 'TDESC'.
        l_fieldcat_s-outputlen = '30'.
      WHEN 'CENTY' OR 'IFKEY'.
        l_fieldcat_s-no_out = 'X'.
    ENDCASE.

    MODIFY pt_fieldcat FROM l_fieldcat_s INDEX sy-tabix.

  ENDLOOP.

ENDFORM.                    " CHANGE
*&---------------------------------------------------------------------*
*&      Form  TREE_TOOL_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_TREE_TREE  text
*      -->P_GR_INB_TREE_TBAR  text
*      -->P_LV_SITEM  text
*----------------------------------------------------------------------*
FORM tree_tool_event   USING p_tree   TYPE REF TO cl_gui_alv_tree
*                             P_TBAR   TYPE REF TO CL_GUI_TOOLBAR
                             p_sitem
                             .
  DATA  : lt_event   TYPE cntl_simple_events
         , ls_event   TYPE cntl_simple_event
        .
  CHECK NOT p_tree IS INITIAL.
  CALL METHOD p_tree->get_registered_events
    IMPORTING
      events = lt_event.

  CLEAR : ls_event .
  ls_event-eventid =  cl_gui_column_tree=>eventid_node_double_click.
  ls_event-appl_event = 'X'.
  APPEND ls_event TO lt_event.
  CLEAR : ls_event .
  ls_event-eventid =  cl_gui_column_tree=>eventid_node_context_menu_req.
  ls_event-appl_event = 'X'.
  APPEND ls_event TO lt_event.
  CALL METHOD p_tree->set_ctx_menu_select_event_appl
    EXPORTING
      i_appl_event = 'X'.
  IF ( p_sitem = 'X' ).
    CLEAR : ls_event .
    ls_event-eventid =  cl_gui_column_tree=>eventid_item_double_click.
    ls_event-appl_event = 'X'.
    APPEND ls_event TO lt_event.
    CLEAR : ls_event .
    ls_event-eventid =  cl_gui_column_tree=>eventid_checkbox_change.
    ls_event-appl_event = 'X'.
    APPEND ls_event TO lt_event.
    CLEAR : ls_event .
    ls_event-eventid =  cl_gui_column_tree=>eventid_link_click.
    ls_event-appl_event = 'X'.
    APPEND ls_event TO lt_event.
    CLEAR : ls_event .
    ls_event-eventid =  cl_gui_column_tree=>eventid_button_click.
    ls_event-appl_event = 'X'.
    APPEND ls_event TO lt_event.
  ENDIF.

  CALL METHOD p_tree->set_registered_events
    EXPORTING
      events = lt_event.
  IF (  gr_tree_class IS INITIAL ).
    CREATE OBJECT gr_tree_class.
  ENDIF.
  CHECK NOT gr_tree_class IS INITIAL .
  IF ( p_sitem EQ space ).
    SET HANDLER gr_tree_class->h_ndouble_click
                gr_tree_class->h_nmenu_req
                gr_tree_class->h_nmenu_sel
                FOR p_tree.
  ELSE .
    SET HANDLER gr_tree_class->h_ndouble_click
                gr_tree_class->h_idouble_click
                gr_tree_class->h_checkbox_click
                gr_tree_class->h_link_click
                gr_tree_class->h_button_click
                gr_tree_class->h_nmenu_req
                gr_tree_class->h_nmenu_sel
                gr_tree_class->h_imenu_req
                gr_tree_class->h_imenu_sel
                FOR p_tree.
  ENDIF.
*  SET HANDLER GR_TREE_CLASS->H_FUNC_CLICK   FOR P_TBAR.
*  SET HANDLER GR_TREE_CLASS->H_DROP_CLICK   FOR P_TBAR.

ENDFORM.                    " TREE_TOOL_EVENT
*&---------------------------------------------------------------------*
*&      Form  init_2000_container
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_2000_container.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CON1'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT g_custom_container1
      EXPORTING
        container_name              = 'CON2'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT g_custom_container2
      EXPORTING
        container_name              = 'CON3'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

  ENDIF.

  IF gr_inf_clas IS INITIAL .
    CREATE OBJECT gr_inf_clas.
  ENDIF.

ENDFORM.                    " init_2000_container
*&---------------------------------------------------------------------*
*&      Form  grid_set_height
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_GR1_SPLT  text
*      -->P_1      text
*      -->P_40     text
*----------------------------------------------------------------------*
FORM grid_set_height USING p_splt  TYPE REF TO cl_gui_splitter_container
                           p_id
                           p_height .
  CHECK NOT p_splt IS INITIAL .
  CALL METHOD p_splt->set_row_height
    EXPORTING
      id     = p_id
      height = p_height.
ENDFORM.                    " grid_set_height
*&---------------------------------------------------------------------*
*&      Form  splt_register_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_GR1_SPLT  text
*----------------------------------------------------------------------*
FORM splt_register_event
     USING p_splt TYPE REF TO cl_gui_splitter_container
           .
  CHECK NOT p_splt IS INITIAL .
  IF gr_splt_class IS INITIAL.
    CREATE OBJECT gr_splt_class.
  ENDIF.
  SET HANDLER gr_splt_class->h_splitter_remove FOR p_splt.
  SET HANDLER gr_splt_class->h_splitter_resize FOR p_splt.

ENDFORM.                    " splt_register_event
*&---------------------------------------------------------------------*
*&      Form  init_2000_alvgrid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_2000_alvgrid.
  IF gr_inb_grid-grid IS INITIAL.
    CREATE OBJECT gr_inb_grid-grid
      EXPORTING
        i_parent      = g_custom_container2
        i_appl_events = 'X'.
    IF NOT gr_inb_grid-grid IS INITIAL.
      CLEAR gr_inb_grid-layo.
      gr_inb_grid-layo-sel_mode   = 'A'.
      gr_inb_grid-layo-zebra      = ' '.
      gr_inb_grid-layo-no_rowmark = 'X'.
      gr_inb_grid-layo-totals_bef = ' '.
      gr_inb_grid-layo-cwidth_opt = 'X'.
      gr_inb_grid-layo-stylefname = 'STTAB'.
      gr_inb_grid-layo-ctab_fname = 'COTAB'.

      CLEAR : gr_inb_grid-excf , gr_inb_grid-excf[].
      PERFORM grid_make_exec TABLES gr_inb_grid-excf
        USING : cl_gui_alv_grid=>mc_fc_loc_delete_row
              , cl_gui_alv_grid=>mc_fc_loc_copy_row
              , cl_gui_alv_grid=>mc_fc_loc_append_row
              , cl_gui_alv_grid=>mc_fc_loc_insert_row
              , cl_gui_alv_grid=>mc_fc_loc_move_row
              , cl_gui_alv_grid=>mc_fc_loc_cut
              , cl_gui_alv_grid=>mc_fc_loc_paste
              , cl_gui_alv_grid=>mc_fc_loc_paste_new_row
              , cl_gui_alv_grid=>mc_fc_loc_undo
              , cl_gui_alv_grid=>mc_fc_info
              , cl_gui_alv_grid=>mc_fc_deselect_all
              , cl_gui_alv_grid=>mc_fc_select_all
              , cl_gui_alv_grid=>mc_fc_refresh
              .
      CLEAR : gr_inb_grid-fcat,gr_inb_grid-fcat[].
      PERFORM grid_make_fieldcat TABLES gr_inb_grid-fcat
      USING : 'S' 'FIELDNAME'  'STATE'     ,
              ' ' 'SCRTEXT_A'  'Status'(l21),
              ' ' 'EMPHASIZE'  'C110'      ,
              ' ' 'FIX_COLUMN' 'X'         ,
              ' ' 'ICON'       'X'         ,
              ' ' 'COL_POS'    '00'        ,
              'E' '' '',

              'S' 'FIELDNAME'  'CHECK'     ,
              ' ' 'SCRTEXT_A'  'Seletion'(l22),
              ' ' 'EMPHASIZE'  'C210'      ,
              ' ' 'FIX_COLUMN' 'X',
              ' ' 'CHECKBOX'   'X'         ,
              ' ' 'EDIT'       'X',
              'E' '' '',

              'S' 'FIELDNAME'  'IFSEQ'     ,
              ' ' 'SCRTEXT_A'  'Seq'(l23)  ,
              ' ' 'EMPHASIZE'  'C210'      , ' ' 'FIX_COLUMN' 'X'
            , ' ' 'CHECKBOX'   ' '         , ' ' 'EDIT'       ' '
            , 'E' '' ''
            , 'S' 'FIELDNAME'  'IFP01'     , ' ' 'SCRTEXT_A'
              'Pr/01'(l24)
            , 'E' 'EMPHASIZE'  'C410'
            , 'S' 'FIELDNAME'  'IFP02'     , ' ' 'SCRTEXT_A'
              'Pr/02'(l25)
            , 'E' 'EMPHASIZE'  'C400'
            , 'S' 'FIELDNAME'  'IFP03'     , ' ' 'SCRTEXT_A'
              'Pr/03'(l26)
            , 'E' 'EMPHASIZE'  'C400'
            , 'S' 'FIELDNAME'  'IFP04'     , ' ' 'SCRTEXT_A'
               'Pr/04'(l27)
            , 'E' 'EMPHASIZE'  'C400'
            , 'S' 'FIELDNAME'  'IFP05'     , ' ' 'SCRTEXT_A'
               'Pr/05'(l28)
            , 'E' 'EMPHASIZE'  'C400'
            , 'S' 'FIELDNAME'  'IFP06'     , ' ' 'SCRTEXT_A'
              'Pr/06'(l29)
            , 'E' 'EMPHASIZE'  'C400'
            , 'S' 'FIELDNAME'  'IFP07'     , ' ' 'SCRTEXT_A'
              'Pr/07'(l2a)
            , 'E' 'EMPHASIZE'  'C400'
            , 'S' 'FIELDNAME'  'IFP08'     , ' ' 'SCRTEXT_A'
              'Pr/08'(l2b)
            , 'E' 'EMPHASIZE'  'C400'
            , 'S' 'FIELDNAME'  'IFP09'     , ' ' 'SCRTEXT_A'
              'Pr/09'(l2c)
            , 'E' 'EMPHASIZE'  'C400'
            , 'S' 'FIELDNAME'  'IFP10'     , ' ' 'SCRTEXT_A'
              'Pr/10'(l2d)
            , 'E' 'EMPHASIZE'  'C400'
            , 'S' 'FIELDNAME'  'IFP11'     , ' ' 'SCRTEXT_A'
              'Pr/11'(l2e)
            , 'E' 'EMPHASIZE'  'C400'
            , 'S' 'FIELDNAME'  'IFP12'     , ' ' 'SCRTEXT_A'
              'Pr/12'(l2f)
            , 'E' 'EMPHASIZE'  'C400'
            .

      CLEAR : gr_inb_grid-sort , gr_inb_grid-sort[].
      PERFORM grid_make_sort TABLES gr_inb_grid-sort
        USING : '1' 'STATE'  ' ' 'X' ' '. " POS FIELN UP DOWN SUM

      CALL METHOD gr_inb_grid-grid->set_table_for_first_display
        EXPORTING
          i_default                     = 'X'
          i_bypassing_buffer            = 'X'
          i_save                        = 'A'
          i_structure_name              = 'ZMMS0104'
          is_layout                     = gr_inb_grid-layo
          is_variant                    = gr_inb_grid-vari
          it_toolbar_excluding          = gr_inb_grid-excf
        CHANGING
          it_outtab                     = gt_igrid[]
          it_fieldcatalog               = gr_inb_grid-fcat
          it_sort                       = gr_inb_grid-sort
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      PERFORM grid_register_event USING gr_inb_grid-grid.

      CALL METHOD gr_inb_grid-grid->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = gr_inb_grid-fcat.
      .
      PERFORM grid_refresh_layo USING gr_inb_grid-grid
                                      gr_inb_grid-layo.
      PERFORM grid_set_select_row USING gr_inb_grid-grid 1.
    ENDIF.
  ELSE.
    PERFORM grid_refresh_only USING gr_inb_grid-grid.
  ENDIF.
ENDFORM.                    " init_2000_alvgrid
*&---------------------------------------------------------------------*
*&      Form  GRID_MAKE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_GRID_FCAT  text
*      -->P_1294   text
*      -->P_1295   text
*      -->P_1296   text
*----------------------------------------------------------------------*
FORM grid_make_fieldcat  TABLES pt_fcat TYPE lvc_t_fcat
                         USING  value(p_option)
                                value(p_name)
                                value(p_value).

  STATICS   ls_fcat    TYPE         lvc_s_fcat.
  STATICS   lv_tabix   TYPE         sy-loopc.
  DATA      ls_char    TYPE         char50.
  FIELD-SYMBOLS : <fs> TYPE any.

  IF ( p_option = 'S' ).
    CLEAR : ls_fcat , lv_tabix.
  ENDIF.
  CASE p_name.
    WHEN 'FIELDNAME'.
      READ TABLE pt_fcat
           WITH KEY fieldname = p_value INTO ls_fcat.
      IF ( sy-subrc = 0 ).
        lv_tabix = sy-tabix .
      ELSE.
        lv_tabix = 0.
        CONCATENATE 'LS_FCAT-' p_name INTO ls_char.
        ASSIGN (ls_char) TO <fs>.

        CHECK <fs> IS ASSIGNED.
        MOVE p_value TO <fs>.
      ENDIF.
    WHEN 'SCRTEXT_A'.
      ls_fcat-coltext   = p_value.
      ls_fcat-scrtext_l = p_value.
      ls_fcat-scrtext_m = p_value.
      ls_fcat-scrtext_s = p_value.
    WHEN OTHERS.
      IF NOT ( p_name IS INITIAL ).
        CONCATENATE 'LS_FCAT-' p_name INTO ls_char.
        ASSIGN (ls_char) TO <fs>.

        CHECK <fs> IS ASSIGNED.
        MOVE p_value TO <fs>.
      ENDIF.
  ENDCASE.
  IF ( p_option = 'E' ).
    IF ( lv_tabix > 0 ).
      MODIFY pt_fcat FROM ls_fcat INDEX lv_tabix.
    ELSE.
      APPEND ls_fcat TO pt_fcat.
    ENDIF.
  ENDIF.
ENDFORM.                    " GRID_MAKE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  GRID_REFRESH_LAYO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_GRID_GRID  text
*      -->P_GR_INB_GRID_LAYO  text
*----------------------------------------------------------------------*
FORM grid_refresh_layo USING p_grid   TYPE REF TO cl_gui_alv_grid
                             p_layout TYPE lvc_s_layo.
  DATA  : scroll      TYPE    lvc_s_stbl
        , lv_visible  TYPE    c
        .

  scroll-row = 'X'.
  scroll-col = 'X'.

  CHECK NOT p_grid IS INITIAL .
*  call method P_GRID->GET_VISIBLE
*       importing VISIBLE = LV_VISIBLE .

*  if LV_VISIBLE eq CL_GUI_CONTROL=>VISIBLE_TRUE.

  CALL METHOD p_grid->set_frontend_layout
    EXPORTING
      is_layout = p_layout.
  CALL METHOD p_grid->refresh_table_display
    EXPORTING
      is_stable = scroll.
*  endif.
  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " GRID_REFRESH_LAYO
*&---------------------------------------------------------------------*
*&      Form  GRID_REGISTER_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_GRID_GRID  text
*----------------------------------------------------------------------*
FORM grid_register_event USING p_grid   TYPE REF TO cl_gui_alv_grid
                               .
  CHECK NOT p_grid IS INITIAL.
  IF (  gr_grid_class IS INITIAL ).
    CREATE OBJECT gr_grid_class.
  ENDIF.
*  SET HANDLER gr_grid_class->h_button_click      FOR p_grid.
*  SET HANDLER gr_grid_class->h_double_click      FOR p_grid.
*  SET HANDLER gr_grid_class->h_hotspot_click     FOR p_grid.
*  SET HANDLER gr_grid_class->h_changed_finished  FOR p_grid.
  SET HANDLER gr_grid_class->h_toolbar           FOR p_grid.
  SET HANDLER gr_grid_class->h_user_command      FOR p_grid.
*  SET HANDLER gr_grid_class->h_top_of_page       FOR p_grid.
*  CALL METHOD p_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.                    " GRID_REGISTER_EVENT
*&---------------------------------------------------------------------*
*&      Form  GRID_SET_SELECT_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_GRID_GRID  text
*      -->P_1      text
*----------------------------------------------------------------------*
FORM grid_set_select_row USING p_grid   TYPE REF TO cl_gui_alv_grid
                               p_row    .
  DATA  : lt_rows TYPE lvc_t_roid
        , ls_rows TYPE lvc_s_roid
        .
  CHECK NOT p_grid IS INITIAL.
  ls_rows-row_id = p_row.
  APPEND ls_rows TO lt_rows .
  CALL METHOD p_grid->set_selected_rows
    EXPORTING
      it_row_no = lt_rows.

ENDFORM.                    " GRID_SET_SELECT_ROW
*&---------------------------------------------------------------------*
*&      Form  init_2000_alvtree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_2000_alvtree.
  DATA    : lv_sitem   TYPE c VALUE ' ',
            lv_thirc   TYPE treev_hhdr,
            lt_comm    TYPE slis_t_listheader,
            ls_comm    TYPE slis_listheader,
            lt_menu    TYPE ttb_btnmnu,
            ls_menu    TYPE stb_btnmnu,
            lr_menu    TYPE REF TO cl_ctmenu.
*Create Tree
  IF gr_inb_tree IS INITIAL.

    CREATE OBJECT gr_inb_tree
      EXPORTING
        parent                      = g_custom_container
        node_selection_mode         =
        cl_gui_column_tree=>node_sel_mode_single
*        ITEM_SELECTION              = LV_SITEM
        item_selection              = ' '
        no_html_header              = ' '
        no_toolbar                  = ' '
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZMMS0103'
      CHANGING
        ct_fieldcat      = gt_fieldcat_lvc[].

    PERFORM change TABLES gt_fieldcat_lvc.

    lv_thirc-heading =  'Interface Name'.

    CLEAR : ls_comm , lt_comm , lt_comm[].

    ls_comm-typ  = 'H'.
**RB1 : INBOUND
    IF rb1 EQ 'X'.
      ls_comm-info = 'Inbound Interface'.
    ELSE.
      ls_comm-info = 'Outbound Interface'.
    ENDIF.

    APPEND ls_comm TO lt_comm.

    CALL METHOD gr_inb_tree->set_table_for_first_display
      EXPORTING
        it_list_commentary  = lt_comm
        i_background_id     = 'ALV_BACKGROUND'
        is_hierarchy_header = lv_thirc
      CHANGING
        it_outtab           = gt_itree[]
        it_fieldcatalog     = gt_fieldcat_lvc[].

    PERFORM tree_tool_event USING gr_inb_tree
                                  lv_sitem.
    IF rb1 = 'X'.
      PERFORM add_items_inbound_tree USING gr_inb_tree.
    ELSE.
      PERFORM add_items_outbound_tree USING gr_inb_tree.
    ENDIF.

    PERFORM tree_refresh_optm USING gr_inb_tree.
  ENDIF.

ENDFORM.                    " init_2000_alvtree
*&---------------------------------------------------------------------*
*&      Form  ADD_ITEMS_INBOUND_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_TREE_TREE  text
*----------------------------------------------------------------------*
FORM add_items_inbound_tree USING p_sender TYPE REF TO cl_gui_alv_tree
                              .
  DATA  : lt_head      TYPE zt_zmmt0101
        , ls_head      TYPE LINE OF zt_zmmt0101
        , lt_data      TYPE zmms0101  OCCURS 0 WITH HEADER LINE
        , ls_tree      TYPE gy_itree
        , lt_layi      TYPE lvc_t_layi
        , ls_layn      TYPE lvc_s_layn
        , lv_node      TYPE lvc_nkey
        , lv_lines     TYPE sy-loopc
        , lv_text      TYPE char128
        , lv_pnode1    TYPE lvc_nkey
        , lv_pnode2    TYPE lvc_nkey
        , lv_pnode3    TYPE lvc_nkey
        .
  CHECK NOT p_sender IS INITIAL.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 10
      text       = 'Gathering Inbound Data'.

* Initial Data.
  PERFORM tree_all_delete USING p_sender.

  CLEAR : gt_inb_grop , gt_inb_grop[]
        , gt_itgrp    , gt_itgrp[]
        .

  CHECK NOT gr_inf_clas IS INITIAL.
  lt_head = gr_inf_clas->load_if_list( dirct = 'I'
                                       centy = 'CZ'
                                       modle = 'ALL' ).
  LOOP AT lt_head INTO ls_head .
    CLEAR : lt_data.
    MOVE-CORRESPONDING ls_head TO lt_data.

    SELECT COUNT(*)
           INTO lt_data-succnt
           FROM zmmt0102
           WHERE ifkey EQ lt_data-ifkey
             AND centy EQ lt_data-centy
             AND istat EQ 'S'
*{ ATDAT => ETDAT PAUL: 08/31/11
             AND etdat IN so_atdat.

    SELECT COUNT(*)
           INTO lt_data-errcnt
           FROM zmmt0102
*{ ATDAT => ETDAT PAUL: 08/31/11
          WHERE etdat IN so_atdat
            AND ifkey EQ lt_data-ifkey
            AND centy EQ lt_data-centy
            AND istat EQ 'E'.


    lt_data-totcnt = lt_data-succnt + lt_data-errcnt.

    APPEND lt_data.
  ENDLOOP.

  CLEAR : ls_tree , ls_layn , lt_layi , lt_layi[]
        .
  MOVE  : icon_ben_offer              TO ls_layn-n_image
        , icon_ben_waive_coverage     TO ls_layn-exp_image
        , 'Inbound'                   TO lv_text
        , ''                          TO ls_tree-tdesc
        , 'TOP'                       TO ls_tree-level
        .

  PERFORM tree_add_node   USING p_sender
                                ls_tree lv_text lt_layi ls_layn
                                ''
                       CHANGING lv_node .
  DESCRIBE TABLE lt_data LINES lv_lines.
  IF lv_lines GT 0 .
    APPEND lv_node TO gt_inb_grop.
    gv_inb_node = lv_node.
  ENDIF.
  gv_top_node = lv_node.
  lv_pnode1   = lv_node.

  LOOP AT gt_itree WHERE cnode EQ space.
    gt_itree-cnode = lv_node .
    MODIFY gt_itree.
  ENDLOOP.

  SORT lt_data BY modle ifkey tbpri.
  LOOP AT lt_data.
    AT NEW modle.
      CLEAR : ls_tree , ls_layn , lt_layi , lt_layi[]
            .
      MOVE  : icon_ben_offer              TO ls_layn-n_image
            , icon_ben_waive_coverage     TO ls_layn-exp_image
            , lt_data-modle               TO lv_text
            , ''                          TO ls_tree-tdesc
            , 'MODLE'                     TO ls_tree-level
            .
      PERFORM tree_add_node   USING p_sender
                                    ls_tree lv_text lt_layi ls_layn
                                    lv_pnode1
                           CHANGING lv_node .
      DESCRIBE TABLE lt_data LINES lv_lines.
      APPEND lv_node TO gt_inb_grop.
      CLEAR : gt_itgrp.
      gt_itgrp-mnode = lv_node.
      gt_itgrp-modle = lt_data-modle.
      APPEND gt_itgrp.
      lv_pnode2   = lv_node.

      LOOP AT gt_itree WHERE cnode EQ space.
        gt_itree-cnode = lv_node .
        MODIFY gt_itree.
      ENDLOOP.
    ENDAT.

    CLEAR : ls_tree , ls_layn , lt_layi , lt_layi[]
          .
    MOVE  : icon_wf_workitem_committed  TO ls_layn-n_image
          , icon_bw_info_cube_ina       TO ls_layn-exp_image
          , lt_data-ifkey               TO lv_text
          , 'IFKEY'                     TO ls_tree-level
          .
    MOVE-CORRESPONDING lt_data TO ls_tree.
    PERFORM tree_add_node   USING p_sender
                                  ls_tree lv_text lt_layi ls_layn
                                  lv_pnode2
                         CHANGING lv_node .
    DESCRIBE TABLE lt_data LINES lv_lines.
    lv_pnode3   = lv_node.

    LOOP AT gt_itree WHERE cnode EQ space.
      gt_itree-cnode = lv_node .
      MODIFY gt_itree.
    ENDLOOP.
  ENDLOOP.

  IF NOT ( gt_inb_grop[] IS INITIAL ).
    CALL METHOD p_sender->expand_nodes
      EXPORTING
        it_node_key = gt_inb_grop.
  ENDIF.
  PERFORM tree_set_toppos USING p_sender gv_top_node.

ENDFORM.                    " ADD_ITEMS_INBOUND_TREE
*&---------------------------------------------------------------------*
*&      Form  tree_add_node
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SENDER  text
*      -->P_LS_TREE  text
*      -->P_LV_TEXT  text
*      -->P_LT_LAYI  text
*      -->P_LS_LAYN  text
*      -->P_2429   text
*      <--P_LV_NODE  text
*----------------------------------------------------------------------*
FORM tree_add_node USING    p_tree        TYPE REF TO cl_gui_alv_tree
                            ps_table
                            p_node_text   TYPE char128
                            p_t_layi      TYPE lvc_t_layi
                            p_s_layn      TYPE lvc_s_layn
                            p_parent_key  TYPE lvc_nkey
                   CHANGING p_chiled_key  TYPE lvc_nkey
                            .
  CHECK NOT p_tree IS INITIAL.
  CALL METHOD p_tree->add_node
    EXPORTING
      i_relat_node_key = p_parent_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = p_node_text
      is_outtab_line   = ps_table
      is_node_layout   = p_s_layn
      it_item_layout   = p_t_layi
    IMPORTING
      e_new_node_key   = p_chiled_key.

ENDFORM.                    " tree_add_node
*&---------------------------------------------------------------------*
*&      Form  tree_all_delete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SENDER  text
*----------------------------------------------------------------------*
FORM tree_all_delete     USING p_tree  TYPE REF   TO cl_gui_alv_tree .
  CHECK NOT p_tree IS INITIAL.
  CALL METHOD p_tree->delete_all_nodes
    EXCEPTIONS
      cntl_system_error = 1
      failed            = 2.

ENDFORM.                    " tree_all_delete
*&---------------------------------------------------------------------*
*&      Form  tree_set_toppos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SENDER  text
*      -->P_GV_TOP_NODE  text
*----------------------------------------------------------------------*
FORM tree_set_toppos     USING p_tree  TYPE REF   TO cl_gui_alv_tree
                               p_nkey  TYPE lvc_nkey .
  CHECK NOT p_tree IS INITIAL.
  CHECK NOT p_nkey IS INITIAL.
  CALL METHOD p_tree->set_top_node
    EXPORTING
      i_node_key        = p_nkey
    EXCEPTIONS
      cntl_system_error = 1
      node_not_found    = 2
      failed            = 3.

ENDFORM.                    " tree_set_toppos
*&---------------------------------------------------------------------*
*&      Form  TREE_REFRESH_OPTM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_TREE_TREE  text
*----------------------------------------------------------------------*
FORM tree_refresh_optm USING p_tree   TYPE REF TO cl_gui_alv_tree
                             .
  CHECK NOT p_tree IS INITIAL .
  CALL METHOD p_tree->update_calculations.
  CALL METHOD p_tree->column_optimize.
  CALL METHOD p_tree->frontend_update.
*  call method CL_GUI_CFW=>FLUSH.

ENDFORM.                    " TREE_REFRESH_OPTM
*&---------------------------------------------------------------------*
*&      Form  free_all_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_all_object.
*  free_object gr_inb_tree-tree .
  free_object gr_inb_grid-grid .

  free_object gr_tr1_cont .
  free_object gr_tr2_cont .
  free_object gr_g11_cont .
  free_object gr_gr1_splt .
  free_object gr_gr1_cust .
  free_object gr_tre_dock .
ENDFORM.                    " free_all_object

*&---------------------------------------------------------------------*
*&      Form  init_2000_txtedit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_2000_txtedit.

  IF gr_otb_edit IS INITIAL.

    CREATE OBJECT gr_otb_edit
      EXPORTING
        parent                     = g_custom_container1
        wordwrap_mode              =
        cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        error_cntl_create          = 1
        error_cntl_init            = 2
        error_cntl_link            = 3
        error_dp_create            = 4
        gui_type_not_supported     = 5.

    CALL METHOD gr_otb_edit->set_readonly_mode
      EXPORTING
        readonly_mode          = 1
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2.

  ENDIF.

ENDFORM.                    " init_2000_txtedit

*---------------------------------------------------------------------*
*       FORM tree_ndouble_click                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_SENDER                                                      *
*  -->  P_NODE_KEY                                                    *
*---------------------------------------------------------------------*
FORM tree_ndouble_click  USING p_sender    TYPE REF TO cl_gui_alv_tree
                              p_node_key  TYPE lvc_nkey
                              .
  DATA  : ls_inb_tree TYPE gy_itree .
*      CLEAR : gt_igrid , gt_igrid[]
*            , gt_imgrd , gt_imgrd[]
  .

  CALL METHOD p_sender->get_outtab_line
    EXPORTING
      i_node_key    = p_node_key
    IMPORTING
      e_outtab_line = ls_inb_tree.
  CHECK NOT ls_inb_tree IS INITIAL.
  CASE ls_inb_tree-level .
    WHEN 'IFKEY'.
      PERFORM delete_edit_obj   USING gr_otb_edit.
      CALL METHOD gr_otb_edit->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
      IF rb1 = 'X'.
        MOVE-CORRESPONDING ls_inb_tree TO zmms0102.
        PERFORM load_inbound_head_data .
        PERFORM load_inbound_item_data .
        PERFORM check_select.
      ELSE.
        MOVE-CORRESPONDING ls_inb_tree TO zmms0101.
        PERFORM load_outbound_head_data .
        PERFORM load_outbound_item_data .
        PERFORM check_select.
      ENDIF.
  ENDCASE.

  PERFORM grid_refresh_only USING gr_inb_grid-grid.
ENDFORM.                    "TREE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  load_inbound_head_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_inbound_head_data.
  DATA  : ls_head  TYPE zmmt0101,
          lv_name  TYPE thead-tdname,
          lt_lines LIKE tline  OCCURS 0 WITH HEADER LINE,
          lt_text  TYPE t_edit OCCURS 0 WITH HEADER LINE.

  CHECK NOT gr_inf_clas IS INITIAL.
  ls_head = gr_inf_clas->load_if_head( ifkey = zmms0102-ifkey
                                       centy = zmms0102-centy
                                       modle = 'ALL' ).
  MOVE-CORRESPONDING ls_head TO zmms0102.


  SELECT COUNT(*)
         INTO zmms0102-succnt
         FROM zmmt0102
*{ ATDAT => ETDAT PAUL: 08/31/11
         WHERE etdat IN so_atdat
          AND  ifkey EQ ls_head-ifkey
          AND  centy EQ ls_head-centy
          AND  istat EQ 'S'.

  SELECT COUNT(*)
         INTO zmms0102-errcnt
         FROM zmmt0102
*{ ATDAT => ETDAT PAUL: 08/31/11
        WHERE etdat IN so_atdat
          AND  ifkey EQ ls_head-ifkey
          AND  centy EQ ls_head-centy
          AND  istat EQ 'E'.


  zmms0102-totcnt = zmms0102-succnt + zmms0102-errcnt.

  IF NOT ls_head-znumb IS INITIAL.

    lv_name = ls_head-znumb.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client   = sy-mandt
        id       = 'ZM01'
        language = 'E'
        name     = lv_name
        object   = 'ZMM01'
      TABLES
        lines    = lt_lines.

    LOOP AT lt_lines.
      lt_text-text = lt_lines-tdline.
*      GT_
      APPEND lt_text.
    ENDLOOP.

    gt_usrpg[] = lt_text[].

    CALL METHOD gr_otb_edit->set_text_as_r3table
      EXPORTING
        table  = lt_text[]
      EXCEPTIONS
        OTHERS = 1.

  ENDIF.

*  CHECK NOT gr_otb_edit IS INITIAL.
*  PERFORM SET_EDIT_STRING USING GR_OTB_EDIT LS_HEAD-SUBPG.
*  PERFORM LOAD_IF_ABAPG USING LS_HEAD.

ENDFORM.                    " load_inbound_head_data
*&---------------------------------------------------------------------*
*&      Form  load_inbound_item_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_inbound_item_data.
  DATA  : lt_items  TYPE zmmt0102 OCCURS 0 WITH HEADER LINE
        , ls_mesgs  TYPE zmmt0103
        , lv_lines  TYPE sy-loopc
        .

** Furong on 03/05/12
*  SELECT *
*    FROM ZMMT0102 AS C020
*    INTO TABLE LT_ITEMS
*   WHERE C020~MANDT EQ SY-MANDT
*     AND C020~IFKEY EQ ZMMS0102-IFKEY
*     AND C020~CENTY EQ ZMMS0102-CENTY
**{ ATDAT => ETDAT PAUL: 08/31/11
*     AND C020~ETDAT IN SO_ATDAT
**     AND c020~istat IN so_istat
**     AND c020~atnam IN so_atnam
**     AND c020~attim IN so_attim
  .
  IF cher IS INITIAL.

    SELECT *
    FROM zmmt0102 AS c020
    INTO TABLE lt_items
   WHERE c020~mandt EQ sy-mandt
     AND c020~ifkey EQ zmms0102-ifkey
     AND c020~centy EQ zmms0102-centy
     AND c020~etdat IN so_atdat.
  ELSE.
    SELECT *
      FROM zmmt0102 AS c020
      INTO TABLE lt_items
     WHERE c020~mandt EQ sy-mandt
       AND c020~ifkey EQ zmms0102-ifkey
       AND c020~centy EQ zmms0102-centy
       AND c020~etdat IN so_atdat
       AND c020~istat = 'E'.
  ENDIF.
** End on 03/05/12
  .
  SORT lt_items BY ifseq DESCENDING .

  CLEAR : gt_igrid , gt_igrid[].
  LOOP AT lt_items.
    CLEAR : gt_igrid.
    MOVE-CORRESPONDING lt_items TO gt_igrid.
    CASE lt_items-istat .
      WHEN 'S'.
        gt_igrid-state = icon_green_light.
      WHEN 'E' OR 'F' .
        gt_igrid-state = icon_red_light.
      WHEN 'W' .
        gt_igrid-state = icon_yellow_light.
      WHEN 'A'.
        gt_igrid-state = icon_light_out.
      WHEN OTHERS .
        gt_igrid-state = lt_items-istat.
    ENDCASE.
    gt_igrid-sttab = gv_btn_stat.

    CLEAR : ls_mesgs.
    SELECT SINGLE *
      FROM zmmt0103 AS c021
      INTO ls_mesgs
     WHERE c021~mandt EQ sy-mandt
       AND c021~ifkey EQ zmms0102-ifkey
       AND c021~centy EQ zmms0102-centy
       AND c021~ifseq EQ lt_items-ifseq.

    IF sy-subrc EQ 0.
      gt_igrid-type    = ls_mesgs-type.
      gt_igrid-message = ls_mesgs-message.
    ENDIF.

    APPEND gt_igrid.
    lv_lines = lv_lines + 1.
  ENDLOOP.

  PERFORM grid_refresh_layo USING gr_inb_grid-grid gr_inb_grid-layo.
  IF ( lv_lines GT 0 ).
    READ TABLE gt_igrid INDEX 1.
    IF ( sy-subrc EQ 0 ).
*      PERFORM load_inbound_mesg USING gt_igrid-ifseq .
    ENDIF.
    PERFORM grid_set_select_row USING gr_inb_grid-grid 1 .
  ELSE.
    MESSAGE s999 WITH text-m01.
  ENDIF.

ENDFORM.                    " load_inbound_item_data
*&---------------------------------------------------------------------*
*&      Form  grid_refresh_only
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_GRID_GRID  text
*----------------------------------------------------------------------*
FORM grid_refresh_only USING p_grid   TYPE REF TO cl_gui_alv_grid .
  DATA  : scroll      TYPE         lvc_s_stbl
        , lv_value    TYPE c
        .
  scroll-row = 'X'.
  scroll-col = 'X'.

  CHECK NOT p_grid IS INITIAL .
  CALL METHOD p_grid->refresh_table_display
    EXPORTING
      is_stable      = scroll
      i_soft_refresh = 'X'.

ENDFORM.                    " grid_refresh_only
*&---------------------------------------------------------------------*
*&      Form  load_if_abapg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_HEAD  text
*----------------------------------------------------------------------*
FORM load_if_abapg USING ps_head TYPE zmmt0101.
  DATA  : ls_index   TYPE indx
        , lv_abapg   TYPE string
        , lv_mandt   TYPE sy-mandt
        .
  lv_mandt = sy-mandt.
  CLEAR : gt_abapg[].
  IF ps_head-retry NE space .
    IMPORT tab = gt_abapg FROM DATABASE indx(if) TO ls_index
           CLIENT lv_mandt ID ps_head-ifkey
           .
  ENDIF.
  IF ps_head-retry NE space.
    gv_activ = icon_activate .
  ELSE.
    gv_activ = icon_deactivate .
  ENDIF.

ENDFORM.                    " load_if_abapg
*&---------------------------------------------------------------------*
*&      Form  load_inbound_mesg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_IGRID_IFSEQ  text
*----------------------------------------------------------------------*
FORM  load_inbound_mesg.
  DATA  : lt_mesgs  TYPE zmmt0103 OCCURS 0 WITH HEADER LINE
        , lv_lines  TYPE sy-loopc
        .
  SELECT *
    FROM zmmt0103 AS c021
    INTO TABLE lt_mesgs
   WHERE c021~mandt EQ sy-mandt
     AND c021~ifkey EQ zmms0102-ifkey
     AND c021~centy EQ zmms0102-centy
*     AND c021~ifseq EQ pv_ifseq
         .
  SORT lt_mesgs BY ifser DESCENDING msseq ASCENDING .
*  CLEAR : gt_imgrd , gt_imgrd[].
  LOOP AT lt_mesgs.
    CLEAR : gt_imgrd.
    MOVE-CORRESPONDING lt_mesgs TO gt_imgrd.
    CASE lt_mesgs-type .
      WHEN 'S'.
        gt_imgrd-state = icon_led_green.
      WHEN 'E' OR 'F' .
        gt_imgrd-state = icon_led_red.
      WHEN 'W' .
        gt_imgrd-state = icon_led_yellow.
      WHEN 'A' .
        gt_imgrd-state = icon_led_inactive.
      WHEN 'I' .
        gt_imgrd-state = icon_message_information_small.
      WHEN OTHERS .
        gt_imgrd-state = icon_warning.
    ENDCASE.
    APPEND gt_imgrd.
    ADD 1 TO lv_lines .
  ENDLOOP.
  IF ( lv_lines LE 0 ) .
    MESSAGE s999 WITH text-m02.
  ENDIF.
*  PERFORM grid_refresh_layo USING gr_inm_grid-GRID gr_inm_grid-layo.

ENDFORM.                    " load_inbound_mesg
*&---------------------------------------------------------------------*
*&      Form  set_edit_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_OTB_EDIT  text
*      -->P_LS_HEAD_SUBPG  text
*----------------------------------------------------------------------*
FORM set_edit_string USING pr_edit   TYPE REF   TO cl_gui_textedit
                           pv_str   TYPE zmmt0101-subpg.

  DATA :  lt_text TYPE t_edit OCCURS 0 WITH HEADER LINE.
*  lv_text = pv_str.

  CHECK NOT pr_edit IS INITIAL.
  CALL METHOD pr_edit->set_text_as_stream
    EXPORTING
      text = lt_text[].
*    EXCEPTIONS
*      error_cntl_call_method = 1
*      not_supported_by_gui   = 2.
ENDFORM.                    "SET_EDIT_STRING
*&---------------------------------------------------------------------*
*&      Form  get_edit_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_OTB_EDIT  text
*      -->P_PV_SUBPG  text
*      <--P_LV_CHMOD  text
*----------------------------------------------------------------------*
FORM get_edit_string USING  pr_edit    TYPE REF   TO cl_gui_textedit
                   CHANGING pv_str     LIKE zmmt0101-subpg
                            pv_change  TYPE i
                            .
  DATA  : lt_text     TYPE t_edit   OCCURS 0 WITH HEADER LINE.

  CALL METHOD pr_edit->get_text_as_stream
    IMPORTING
      text   = lt_text[]
    EXCEPTIONS
      OTHERS = 1.
ENDFORM.                    " get_edit_string
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SV_CODE  text
*----------------------------------------------------------------------*
FORM user_command USING pf_code .
  DATA : ls_head    TYPE zmmt0101
       , ls_c019    TYPE zmmt0101
       , lv_retry   TYPE ze_retry
       , lv_subpg   TYPE ze_subpg
       , lv_return  TYPE c
       .
  CASE pf_code.
    WHEN 'SAVE'.
      CHECK NOT gr_inf_clas IS INITIAL .
      IF rb1 EQ 'X' .
        IF ( zmms0102-ifkey IS INITIAL ) OR
           ( zmms0102-centy IS INITIAL ) .
          MESSAGE e999 WITH 'Interface Key & Country exception.'.
          EXIT.
        ENDIF.
        MOVE-CORRESPONDING zmms0102 TO ls_head.
      ELSE.
        IF ( zmms0101-ifkey IS INITIAL ) OR
           ( zmms0101-centy IS INITIAL ) .
          MESSAGE e999 WITH 'Interface Key & Country exception.'.
          EXIT.
        ENDIF.
        MOVE-CORRESPONDING zmms0101 TO ls_head.
      ENDIF.

      CLEAR : ls_head-retry,
              ls_head-subpg.

      PERFORM save_edit_text CHANGING ls_head.

    WHEN 'CREATE'.
      CALL METHOD gr_otb_edit->set_readonly_mode
        EXPORTING
          readonly_mode = 0.
      gv_chk = 'X'.
    WHEN 'CHANGE'.

      IF gv_chk IS INITIAL.
        CALL METHOD gr_otb_edit->set_readonly_mode
          EXPORTING
            readonly_mode = 0.
        gv_chk = 'X'.
      ELSE.
        CALL METHOD gr_otb_edit->set_readonly_mode
          EXPORTING
            readonly_mode = 1.
        CLEAR : gv_chk.
      ENDIF.

    WHEN 'TRANSFER'.
      IF rb1 = 'X'.
        MOVE-CORRESPONDING zmms0102 TO ls_head.
      ELSE.
        MOVE-CORRESPONDING zmms0101 TO ls_head.
      ENDIF.
      PERFORM code_check_generation CHANGING ls_head
                                       ls_head-retry
                                       ls_head-subpg
                                       .
      PERFORM tree_refresh_only USING gr_inb_tree.
  ENDCASE.
ENDFORM.                    " user_command
*&---------------------------------------------------------------------*
*&      Form  code_check_generation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_HEAD  text
*      <--P_LS_HEAD_RETRY  text
*      <--P_LS_HEAD_SUBPG  text
*----------------------------------------------------------------------*
FORM code_check_generation CHANGING value(ps_head)  TYPE zmmt0101
                                    pv_retry TYPE ze_retry
                                    pv_subpg TYPE zmmt0101-subpg
                           .
  DATA: BEGIN OF lt_abapg OCCURS 250,
        line(72),                      "Select-ABAP Code-Tabelle
      END OF lt_abapg.

  DATA: lt_usrpg   TYPE t_edit OCCURS 0 WITH HEADER LINE,
        lv_progm(300),
        lv_messg(300),
        lv_sdump(300),
        lv_str01(300),
        lv_index TYPE i,
        ls_item    TYPE zmms0104,
        ls_mesg    TYPE zmmt0103,
        ls_items   TYPE zmmt0102,
        lt_items   TYPE zt_zmmt0102,
        lt_mesg    TYPE zt_zmmt0103,
        lv_chmod   TYPE i,
        lv_num02   TYPE numc2,
        lv_return  TYPE c
        .
  CLEAR : lt_usrpg , lt_usrpg[] , pv_retry .
**Use Retry Logic
  PERFORM get_edit_obj TABLES gt_usrpg
                       USING gr_otb_edit.

**Coding Retry logic use table GT_USRPG
  APPEND  : 'PROGRAM SubRoutinePool.                 '  TO lt_abapg
          , '                                        '  TO lt_abapg
          , 'define CALL_MACRO_FUNCTION .            '  TO lt_abapg
          .

  LOOP AT gt_usrpg INTO lv_str01 .
    APPEND lv_str01 TO lt_abapg.
  ENDLOOP.

  CONCATENATE 'LV_IFKEY = ''' ps_head-ifkey   ''' .'    INTO lv_str01 .
  APPEND  : 'end-of-definition .                     '  TO lt_abapg
          , '                                        '  TO lt_abapg
          , 'form CALL_MACRO.                        '  TO lt_abapg
          , '  DATA  : LV_IFKEY   type ZE_IFKEY      '  TO lt_abapg
          , '        , LS_ITEM    type ZMMS0104      '  TO lt_abapg
          , '        .                               '  TO lt_abapg
          , lv_str01                                    TO lt_abapg
          , '  import ITEM = LS_ITEM from            '  TO lt_abapg
          , '         memory id LV_IFKEY .           '  TO lt_abapg
          , '  CALL_MACRO_FUNCTION                   '  TO lt_abapg
          .
  IF ( ps_head-cparm GT 0 AND ps_head-cparm LE 12 ).
    CLEAR : lv_num02.
    DO ps_head-cparm TIMES.
      ADD 1 TO lv_num02.
      CONCATENATE 'LS_ITEM-IFP' lv_num02  INTO lv_str01 .
      APPEND : lv_str01 TO lt_abapg.
    ENDDO.
  ENDIF.
  APPEND  : '                      .                 '  TO lt_abapg
          , 'endform.                                '  TO lt_abapg
          , '                                        '  TO lt_abapg
          , '                                        '  TO lt_abapg
          .
**Generate Retry Program
  GENERATE SUBROUTINE POOL lt_abapg NAME lv_progm
           MESSAGE      lv_messg
           SHORTDUMP-ID lv_sdump
           .
  IF sy-subrc = 0.
    gv_activ = icon_activate .
    CALL METHOD gr_inb_grid-grid->check_changed_data.
**Execept Success Line, Only Error and check
    LOOP AT gt_igrid WHERE check EQ 'X' AND type NE 'S' .
      lv_index = sy-tabix.
      MOVE-CORRESPONDING gt_igrid TO ls_item.
      EXPORT item = ls_item TO MEMORY ID ps_head-ifkey.
**Run Generated Program
      PERFORM ('CALL_MACRO') IN PROGRAM (lv_progm) IF FOUND.
      FREE MEMORY ID ps_head-ifkey.
**Changed Data Reselect
      lt_items = gr_inf_clas->load_if_item( ifkey = ps_head-ifkey
                                            centy = ps_head-centy
                                            ifseq = ls_item-ifseq
                                          ).
      lt_mesg =  gr_inf_clas->load_if_mesg( ifkey = ps_head-ifkey
                                            centy = ps_head-centy
                                            ifseq = ls_item-ifseq
                                          ).

      READ TABLE lt_items INTO ls_items INDEX 1.

      IF ( sy-subrc EQ 0 ).
        MOVE-CORRESPONDING ls_items TO gt_igrid.

        gt_igrid-type = ls_items-istat.

        CASE ls_items-istat.
          WHEN 'S'.
            gt_igrid-state = icon_led_green.
          WHEN 'E' OR 'F' .
            gt_igrid-state = icon_led_red.
          WHEN 'W' .
            gt_igrid-state = icon_led_yellow.
          WHEN 'A'.
            gt_igrid-state = icon_led_inactive.
          WHEN OTHERS .
            gt_igrid-state = ls_items-istat.
        ENDCASE.

        READ TABLE lt_mesg  INTO ls_mesg  INDEX 1.

        IF sy-subrc = 0.
          gt_igrid-message = ls_mesg-message.
        ENDIF.

        MODIFY gt_igrid INDEX lv_index.
      ENDIF.
    ENDLOOP.

    CALL METHOD gr_inb_grid-grid->refresh_table_display
      EXPORTING
        i_soft_refresh = 'X'.

  ELSEIF sy-subrc = 4.
    CLEAR : gt_abapg[].
    MESSAGE i999 WITH lv_messg .
  ELSEIF sy-subrc = 8.
    CLEAR : gt_abapg[].
    MESSAGE i999 WITH lv_sdump .
  ENDIF.

ENDFORM.                    "CODE_CHECK_GENERATION
*&---------------------------------------------------------------------*
*&      Form  get_edit_obj
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_USRPG  text
*      -->P_GR_OTB_EDIT  text
*      <--P_LV_CHMOD  text
*----------------------------------------------------------------------*
FORM get_edit_obj TABLES pt_table   TYPE STANDARD TABLE
                   USING pr_edit    TYPE REF   TO cl_gui_textedit.

  CHECK NOT pr_edit IS INITIAL.
  CALL METHOD pr_edit->get_text_as_r3table
    IMPORTING
      table  = pt_table[]
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.                    " get_edit_obj
*&---------------------------------------------------------------------*
*&      Form  add_items_outbound_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_TREE  text
*----------------------------------------------------------------------*
FORM add_items_outbound_tree USING p_sender TYPE REF TO cl_gui_alv_tree.

  DATA  : lt_head      TYPE zt_zmmt0101
        , ls_head      TYPE LINE OF zt_zmmt0101
        , lt_data      TYPE zmms0101  OCCURS 0 WITH HEADER LINE
        , ls_tree      TYPE gy_itree
        , lt_layi      TYPE lvc_t_layi
        , ls_layn      TYPE lvc_s_layn
        , lv_node      TYPE lvc_nkey
        , lv_lines     TYPE sy-loopc
        , lv_text(128)
        , lv_pnode1    TYPE lvc_nkey
        , lv_pnode2    TYPE lvc_nkey
        , lv_pnode3    TYPE lvc_nkey
        .
  CHECK NOT p_sender IS INITIAL.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 20
      text       = 'Gathering outbound Data'.
* Initial Data.
  PERFORM tree_all_delete USING p_sender.

  CHECK NOT gr_inf_clas IS INITIAL.
  lt_head = gr_inf_clas->load_if_list( dirct = 'O'
                                       centy = 'CZ'
                                       modle = 'ALL' ).
  LOOP AT lt_head INTO ls_head .
    CLEAR : lt_data.
    MOVE-CORRESPONDING ls_head TO lt_data.

    SELECT COUNT(*)
           INTO lt_data-succnt
           FROM zmmt0102
*{ ATDAT => ETDAT PAUL: 08/31/11
          WHERE etdat IN so_atdat
            AND ifkey EQ lt_data-ifkey
            AND centy EQ lt_data-centy
            AND istat EQ 'S'.

    SELECT COUNT(*)
           INTO lt_data-errcnt
           FROM zmmt0102
*{ ATDAT => ETDAT PAUL: 08/31/11
          WHERE etdat IN so_atdat
            AND ifkey EQ lt_data-ifkey
            AND centy EQ lt_data-centy
            AND istat EQ 'E'.


    lt_data-totcnt = lt_data-succnt + lt_data-errcnt.

    APPEND lt_data.
  ENDLOOP.

  CLEAR : ls_tree , ls_layn , lt_layi , lt_layi[]
        .
  MOVE  : icon_ben_offer              TO ls_layn-n_image
        , icon_ben_waive_coverage     TO ls_layn-exp_image
        , 'Outbound'                  TO lv_text
        , ''                          TO ls_tree-tdesc
        , 'TOP'                       TO ls_tree-level
        .

*  CONCATENATE  '(' gv_clock_text ')' 'Outbound' INTO lv_text.

  PERFORM tree_add_node   USING p_sender
                                ls_tree lv_text lt_layi ls_layn
                                ''
                      CHANGING lv_node .
  DESCRIBE TABLE lt_data LINES lv_lines.
  IF lv_lines GT 0 .
    APPEND lv_node TO gt_inb_grop.
    gv_inb_node = lv_node.
  ENDIF.
  gv_top_node = lv_node.
  lv_pnode1   = lv_node.

  LOOP AT gt_itree WHERE cnode EQ space.
    gt_itree-cnode = lv_node .
    MODIFY gt_itree.
  ENDLOOP.

  SORT lt_data BY  modle ifkey tbpri.
  LOOP AT lt_data.
    AT NEW modle.
      CLEAR : ls_tree , ls_layn , lt_layi , lt_layi[]
            .
      MOVE  : icon_ben_offer              TO ls_layn-n_image
            , icon_ben_waive_coverage     TO ls_layn-exp_image
            , lt_data-modle               TO lv_text
            , ''                          TO ls_tree-tdesc
            , 'MODLE'                     TO ls_tree-level
            .
      PERFORM tree_add_node   USING p_sender
                                    ls_tree lv_text lt_layi ls_layn
                                    lv_pnode1
                           CHANGING lv_node .
      DESCRIBE TABLE lt_data LINES lv_lines.
      APPEND lv_node TO gt_inb_grop.
      lv_pnode2   = lv_node.
      CLEAR : gt_itgrp.
      gt_itgrp-mnode = lv_node.
      gt_itgrp-modle = lt_data-modle.
      APPEND gt_itgrp.

      LOOP AT gt_itree WHERE cnode EQ space.
        gt_itree-cnode = lv_node .
        MODIFY gt_itree.
      ENDLOOP.
    ENDAT.

    CLEAR : ls_tree , ls_layn , lt_layi , lt_layi[]
          .
    MOVE  : icon_wf_workitem_committed  TO ls_layn-n_image
          , icon_bw_info_cube_ina       TO ls_layn-exp_image
          , lt_data-ifkey               TO lv_text
          , 'IFKEY'                     TO ls_tree-level
          .
    MOVE-CORRESPONDING lt_data TO ls_tree.
    PERFORM tree_add_node   USING p_sender
                                  ls_tree lv_text lt_layi ls_layn
                                  lv_pnode2
                         CHANGING lv_node .
    DESCRIBE TABLE lt_data LINES lv_lines.
    lv_pnode3   = lv_node.
    LOOP AT gt_itree WHERE cnode EQ space.
      gt_itree-cnode = lv_node .
      MODIFY gt_itree.
    ENDLOOP.
  ENDLOOP.

  IF NOT ( gt_inb_grop[] IS INITIAL ).
    CALL METHOD p_sender->expand_nodes
      EXPORTING
        it_node_key = gt_inb_grop.
  ENDIF.
  PERFORM tree_set_toppos USING p_sender gv_top_node.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 30
      text       = 'Gathering outbound Data'.

ENDFORM.                    " add_items_outbound_tree
*&---------------------------------------------------------------------*
*&      Form  SAVE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_OTB_EDIT  text
*      -->P_PV_SUBPG  text
*      <--P_LV_CHMOD  text
*----------------------------------------------------------------------*
FORM save_text TABLES pt_usrpg
               CHANGING ps_head TYPE zmmt0101.
  DATA : g_header    LIKE  thead,
         g_number(10),
         lt_lines LIKE	tline OCCURS 0 WITH HEADER LINE,
         ls_usrpg   TYPE t_edit .

  g_header-tdobject   =  'ZMM01'.    "OBJECT ID
  g_header-tdid       =  'ZM01'.      "TEXT ID
  g_header-tdspras    =  sy-langu.

  SELECT SINGLE znumb
    INTO ps_head-znumb
    FROM zmmt0101
   WHERE ifkey EQ ps_head-ifkey
     AND centy EQ ps_head-centy.

  IF ps_head-znumb IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '1'
        object      = 'ZMM_N01'
        quantity    = '1'
      IMPORTING
        number      = g_number.

    IF sy-subrc = 0.
      CONCATENATE g_header-tdobject g_number INTO g_header-tdname.
    ENDIF.

  ELSE.
    g_header-tdname = ps_head-znumb.
  ENDIF.


  LOOP AT pt_usrpg INTO ls_usrpg.
    lt_lines-tdline = ls_usrpg-text.
    APPEND lt_lines.
  ENDLOOP.


  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header    = g_header
    IMPORTING
      newheader = g_header
    TABLES
      lines     = lt_lines.


  ps_head-znumb = g_header-tdname.

ENDFORM.                    " SAVE_TEXT
*&---------------------------------------------------------------------*
*&      Form  LOAD_OUTBOUND_HEAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_outbound_head_data.
  DATA  : ls_head      TYPE zmmt0101,
          lv_name  TYPE thead-tdname,
          lt_lines LIKE tline  OCCURS 0 WITH HEADER LINE,
          lt_text  TYPE t_edit OCCURS 0 WITH HEADER LINE.

  .
  CHECK NOT gr_inf_clas IS INITIAL.
  ls_head = gr_inf_clas->load_if_head( ifkey = zmms0101-ifkey
                                       centy = zmms0101-centy
                                       modle = 'ALL' ).
  MOVE-CORRESPONDING ls_head TO zmms0101.

  SELECT COUNT(*)
         INTO zmms0101-succnt
         FROM zmmt0102
*{ ATDAT => ETDAT PAUL: 08/31/11
        WHERE etdat IN so_atdat
          AND ifkey EQ ls_head-ifkey
          AND centy EQ ls_head-centy
          AND istat EQ 'S'.

  SELECT COUNT(*)
         INTO zmms0101-errcnt
         FROM zmmt0102
*{ ATDAT => ETDAT PAUL: 08/31/11
        WHERE etdat IN so_atdat
          AND ifkey EQ ls_head-ifkey
          AND centy EQ ls_head-centy
          AND istat EQ 'E'.

  zmms0101-totcnt = zmms0101-succnt + zmms0101-errcnt.

  IF NOT ls_head-znumb IS INITIAL.

    lv_name = ls_head-znumb.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client   = sy-mandt
        id       = 'ZM01'
        language = 'E'
        name     = lv_name
        object   = 'ZMM01'
      TABLES
        lines    = lt_lines.

    LOOP AT lt_lines.
      lt_text-text = lt_lines-tdline.
*      GT_
      APPEND lt_text.
    ENDLOOP.

    gt_usrpg[] = lt_text[].

    CALL METHOD gr_otb_edit->set_text_as_r3table
      EXPORTING
        table  = lt_text[]
      EXCEPTIONS
        OTHERS = 1.

  ENDIF.

*  PERFORM set_edit_string USING gr_otb_edit ls_head-subpg.
*  PERFORM load_if_abapg USING ls_head.

ENDFORM.                    " LOAD_OUTBOUND_HEAD_DATA
*&---------------------------------------------------------------------*
*&      Form  LOAD_OUTBOUND_ITEM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_outbound_item_data.
  DATA  : lt_items  TYPE zmmt0102 OCCURS 0 WITH HEADER LINE
        , ls_mesgs  TYPE zmmt0103
        , lv_lines  TYPE sy-loopc
        .

** Furong on 03/05/12
*  SELECT *
*    FROM zmmt0102 AS c020
*    INTO TABLE lt_items
*   WHERE c020~mandt EQ sy-mandt
*     AND c020~ifkey EQ zmms0101-ifkey
*     AND c020~centy EQ zmms0101-centy
**{ ATDAT => ETDAT PAUL: 08/31/11
*     AND c020~etdat IN so_atdat
**     AND c020~istat IN so_istat
**     AND c020~atnam IN so_atnam
**     AND c020~attim IN so_attim
*         .
  IF cher IS INITIAL.
  SELECT *
    FROM zmmt0102 AS c020
    INTO TABLE lt_items
   WHERE c020~mandt EQ sy-mandt
     AND c020~ifkey EQ zmms0101-ifkey
     AND c020~centy EQ zmms0101-centy
*{ ATDAT => ETDAT PAUL: 08/31/11
     AND c020~etdat IN so_atdat
*     AND c020~istat IN so_istat
*     AND c020~atnam IN so_atnam
*     AND c020~attim IN so_attim
         .
 ELSE.
  SELECT *
    FROM zmmt0102 AS c020
    INTO TABLE lt_items
   WHERE c020~mandt EQ sy-mandt
     AND c020~ifkey EQ zmms0101-ifkey
     AND c020~centy EQ zmms0101-centy
     AND c020~etdat IN so_atdat
     AND c020~istat = 'E'.
 ENDIF.
** End on 03/05/12

  SORT lt_items BY ifseq DESCENDING .
  CLEAR : gt_igrid , gt_igrid[].
  LOOP AT lt_items.
    CLEAR : gt_igrid.
    MOVE-CORRESPONDING lt_items TO gt_igrid.
    CASE lt_items-istat .
      WHEN 'S'.
        gt_igrid-state = icon_green_light.
      WHEN 'E' OR 'F' .
        gt_igrid-state = icon_red_light.
      WHEN 'W' .
        gt_igrid-state = icon_yellow_light.
      WHEN 'A'.
        gt_igrid-state = icon_light_out.
      WHEN OTHERS .
        gt_igrid-state = lt_items-istat.
    ENDCASE.

    IF zmms0101-ifkey = 'MMIF302_ECC_OB'.
      IF gt_igrid-ifp09 = '100'.
        gt_igrid-state = icon_red_light.
      ELSE.
        gt_igrid-state = icon_green_light.
      ENDIF.
    ENDIF.

    IF zmms0101-ifkey = 'MMIF301_ECC_OB'.
      IF gt_igrid-ifp04 = '100'.
        gt_igrid-state = icon_red_light.
      ELSE.
        gt_igrid-state = icon_green_light.
      ENDIF.
    ENDIF.

    CLEAR : ls_mesgs.
    SELECT SINGLE *
      FROM zmmt0103 AS c021
      INTO ls_mesgs
     WHERE c021~mandt EQ sy-mandt
       AND c021~ifkey EQ zmms0101-ifkey
       AND c021~centy EQ zmms0101-centy
       AND c021~ifseq EQ lt_items-ifseq.

    IF sy-subrc EQ 0.
      gt_igrid-type    = ls_mesgs-type.
      gt_igrid-message = ls_mesgs-message.
    ENDIF.

    gt_igrid-sttab = gv_btn_stat.
    APPEND gt_igrid.
    lv_lines = lv_lines + 1.
  ENDLOOP.
  PERFORM grid_refresh_layo USING gr_inb_grid-grid gr_inb_grid-layo.
  IF ( lv_lines GT 0 ).
    READ TABLE gt_igrid INDEX 1.
*    IF ( sy-subrc EQ 0 ).
*      PERFORM load_outbound_mesg USING gt_ogrid-ifseq .
*    ENDIF.
    PERFORM grid_set_select_row USING gr_inb_grid-grid 1 .
  ELSE.
    MESSAGE s999 WITH text-m01.
  ENDIF.
ENDFORM.                    " LOAD_OUTBOUND_ITEM_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_HEAD  text
*----------------------------------------------------------------------*
FORM save_data USING ps_head TYPE zmmt0101.
*  DATA  LS_HEAD LIKE ZMMT0101.
*  CLEAR LS_HEAD.
*
*  SELECT SINGLE *
*    INTO LS_HEAD
*    FROM ZMMT0101
*   WHERE IFKEY EQ PS_HEAD-IFKEY
*     AND CENTY EQ PS_HEAD-CENTY.
*
*  IF SY-SUBRC = 0.
  ps_head-atnam = sy-uname.
  ps_head-atdat = sy-datum.
  ps_head-attim = sy-uzeit.
*  ELSE.
*
*  ENDIF.

  MODIFY zmmt0101 FROM ps_head.
  COMMIT WORK.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_EDIT_OBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_OTB_EDIT  text
*----------------------------------------------------------------------*
FORM delete_edit_obj USING pr_edit     TYPE REF   TO cl_gui_textedit.
  CHECK NOT pr_edit IS INITIAL.
  CALL METHOD pr_edit->delete_text
    EXCEPTIONS
      OTHERS = 1.
ENDFORM.                    " DELETE_EDIT_OBJ
*&---------------------------------------------------------------------*
*&      Form  RUN_MACRO_I
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_HEAD  text
*----------------------------------------------------------------------*
FORM run_macro_i USING ps_head TYPE zmmt0101.
  DATA  : lv_count   TYPE sy-loopc
        , lv_progm   LIKE sy-repid
        , lv_messg(150)
        , lv_sdump(150)
        , ls_item    TYPE zmms0104
        , ls_items   TYPE zmmt0102
        , lt_items   TYPE zt_zmmt0102
        , ls_igrid   TYPE gy_igrid
        , lv_index   TYPE sy-loopc
        .

  IF gt_abapg[] IS INITIAL .
    IF ps_head-znumb IS INITIAL.
      MESSAGE e999 WITH 'Please input the retry logic '.
    ELSE.
      MESSAGE e999 WITH 'Please check and save the retry logic'.
    ENDIF.
    EXIT.
  ENDIF.

  GENERATE SUBROUTINE POOL gt_abapg NAME lv_progm
           MESSAGE      lv_messg
           SHORTDUMP-ID lv_sdump
           .
*  IF SY-SUBRC = 0.
*    MESSAGE S999 WITH 'Generation Success'.
*  ELSEIF SY-SUBRC = 4.
*    MESSAGE I999 WITH LV_MESSG .
*    EXIT.
*  ELSEIF SY-SUBRC = 8.
*    MESSAGE I999 WITH LV_SDUMP .
*    EXIT.
*  ENDIF.
*  CHECK NOT GR_INF_CLAS IS INITIAL .

  LOOP AT gt_igrid WHERE check EQ 'X' .
    ADD 1 TO lv_count.
    MOVE-CORRESPONDING gt_igrid TO ls_item.
    EXPORT item = ls_item TO MEMORY ID ps_head-ifkey.
    PERFORM ('CALL_MACRO') IN PROGRAM (lv_progm) IF FOUND.
    FREE MEMORY ID ps_head-ifkey.
    lt_items = gr_inf_clas->load_if_item( ifkey = ps_head-ifkey
                                          centy = ps_head-centy
                                          ifseq = ls_item-ifseq
                                        ).
    READ TABLE lt_items INTO ls_items INDEX 1.
    IF ( sy-subrc EQ 0 ).
      MOVE-CORRESPONDING ls_items TO gt_igrid.
      CASE ls_items-istat.
        WHEN 'S'.
          gt_igrid-state = icon_led_green.
        WHEN 'E' OR 'F' .
          gt_igrid-state = icon_led_red.
        WHEN 'W' .
          gt_igrid-state = icon_led_yellow.
        WHEN 'A'.
          gt_igrid-state = icon_led_inactive.
        WHEN OTHERS .
          gt_igrid-state = ls_items-istat.
      ENDCASE.
      MODIFY gt_igrid.
    ENDIF.
  ENDLOOP.

  IF lv_count LE 0 .
    MESSAGE e999 WITH 'Please select the wanted item in Retry logic.'.
  ELSE.
    PERFORM grid_refresh_only USING gr_inb_grid-grid.
*    PERFORM grid_get_currpos USING gr_inm_grid-grid lv_index.
*    PERFORM grid_set_select_row USING gr_inm_grid-grid lv_index.
*    READ TABLE gt_igrid INTO ls_igrid INDEX lv_index.
*    IF sy-subrc EQ 0 .
***      PERFORM load_outbound_mesg USING ls_igrid-ifseq.
*    ENDIF.
    MESSAGE s999 WITH 'There is Execute the retry logic Macro .'.
  ENDIF.

ENDFORM.                    " RUN_MACRO_I
*&---------------------------------------------------------------------*
*&      Form  SAVE_EDIT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_HEAD  text
*      <--P_LS_HEAD_RETRY  text
*      <--P_LS_HEAD_SUBPG  text
*----------------------------------------------------------------------*
FORM save_edit_text CHANGING ps_head.
  DATA  : lt_usrpg   TYPE t_edit OCCURS 0 WITH HEADER LINE.
**Retry logic into the LT_USRPG table
  PERFORM get_edit_obj TABLES lt_usrpg
                        USING gr_otb_edit.

  CHECK NOT lt_usrpg[] IS INITIAL .
**Save Retry logic transform SAP-TEXT
  PERFORM save_text TABLES   lt_usrpg
                    CHANGING ps_head.
**Interface Header Modify
  PERFORM save_data USING ps_head.

ENDFORM.                    " SAVE_EDIT_TEXT
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_select.
  DATA : l_fieldcatalog  TYPE lvc_s_fcat.
  DATA : l_celltab       TYPE lvc_s_styl,
         l_index         TYPE i.

*  LOOP AT GT_IGRID WHERE TYPE = 'S'.
  LOOP AT gt_igrid WHERE ( type = 'S'
                       OR type = 'M' ).
    REFRESH gt_igrid-sttab.
    l_index = sy-tabix.
    l_celltab-fieldname = 'CHECK'.
    l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT l_celltab INTO TABLE gt_igrid-sttab.
    MODIFY gt_igrid INDEX l_index.
  ENDLOOP.

ENDFORM.                    " CHECK_SELECT
*&---------------------------------------------------------------------*
*&      Form  GRID_MAKE_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM grid_make_sort TABLES pt_sort  TYPE lvc_t_sort
                     USING value(pv_position)
                           value(pv_fname)
                           value(pv_up)
                           value(pv_down)
                           value(pv_subtot)
                           .
  DATA  : ls_sort TYPE  lvc_s_sort.
  CLEAR ls_sort.
  ls_sort-spos       = pv_position.
  ls_sort-fieldname  = pv_fname.
  ls_sort-up         = pv_up.
  ls_sort-down       = pv_down.
  ls_sort-subtot     = pv_subtot.
  APPEND ls_sort TO pt_sort.

ENDFORM.                    " GRID_MAKE_SORT
*&---------------------------------------------------------------------*
*&      Form  grid_make_exec
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_GRID_EXCF  text
*      -->P_CL_GUI_ALV_GRID=>MC_FC_LOC_DEL  text
*----------------------------------------------------------------------*
FORM grid_make_exec TABLES lt_exclude TYPE ui_functions
                     USING lv_func    TYPE ui_func
                         .
  APPEND lv_func TO lt_exclude.
ENDFORM.                    "GRID_MAKE_EXEC
*&---------------------------------------------------------------------*
*&      Form  grid_append_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_GRID_EXCF  text
*      -->P_CL_GUI_ALV_GRID=>MC_FC_LOC_DEL  text
*----------------------------------------------------------------------*
FORM grid_append_toolbar USING p_sender TYPE REF TO cl_gui_alv_grid
                               p_object TYPE REF TO
                               cl_alv_event_toolbar_set
                               p_interactive
                               .

  DATA: lv_button TYPE stb_button.
  CASE p_sender.
    WHEN gr_inb_grid-grid .

      CLEAR lv_button.
      lv_button-butn_type  = '3'.
      INSERT lv_button INTO p_object->mt_toolbar INDEX 1.

      CLEAR lv_button.
      lv_button-butn_type  = '0'.
      lv_button-function   = 'UMARK'.
      lv_button-icon       = icon_deselect_all.
      lv_button-quickinfo  = 'DeSelect All'.
      lv_button-text       = ''.
      INSERT lv_button INTO p_object->mt_toolbar INDEX 1.

      CLEAR lv_button.
      lv_button-butn_type  = '0'.
      lv_button-function   = 'AMARK'.
      lv_button-icon       = icon_select_all.
      lv_button-quickinfo  = 'Select All'.
      lv_button-text       = ''.
      INSERT lv_button INTO p_object->mt_toolbar INDEX 1.

  ENDCASE.
ENDFORM.                    "GRID_APPEND_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  grid_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_GRID_EXCF  text
*      -->P_CL_GUI_ALV_GRID=>MC_FC_LOC_DEL  text
*----------------------------------------------------------------------*
FORM grid_user_command USING p_sender TYPE REF TO cl_gui_alv_grid
                             p_ucomm.

  CASE p_sender.
    WHEN gr_inb_grid-grid .
      CASE  p_ucomm.
        WHEN 'AMARK'. "Select All
          gt_igrid-check = 'X' .
          MODIFY gt_igrid TRANSPORTING check
          WHERE type NE 'S'
            AND check EQ ''.
        WHEN 'UMARK'. "DeSelect All
          gt_igrid-check = '' .
          MODIFY gt_igrid TRANSPORTING check
          WHERE check EQ 'X'.
      ENDCASE.
  ENDCASE.

ENDFORM.                    "GRID_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  TREE_REFRESH_ONLY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_INB_TREE_TREE  text
*----------------------------------------------------------------------*
FORM tree_refresh_only USING p_tree   TYPE REF TO cl_gui_alv_tree
                             .
  CHECK NOT p_tree IS INITIAL .
  CALL METHOD p_tree->update_calculations.
  CALL METHOD p_tree->frontend_update.
  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " TREE_REFRESH_ONLY
