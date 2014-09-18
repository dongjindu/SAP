*&---------------------------------------------------------------------*
*&  Include           ZKEMMR04011_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_selected_rows.
  DATA : lt_rows TYPE lvc_t_row.
  DATA : l_row   TYPE lvc_s_row.

* Get Selected Rows
  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  DESCRIBE TABLE lt_rows.
  IF sy-tfill = 0.
    MESSAGE e800(13) .
  ENDIF.

  LOOP AT lt_rows INTO l_row.
    READ TABLE gt_display INDEX l_row-index.
    gt_display-check = 'X'.
    MODIFY gt_display INDEX l_row-index.
  ENDLOOP.

ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM screen_modify .
  LOOP AT SCREEN.
    CASE 'X'.
      WHEN ra.
        IF screen-group1 EQ 'G1'.
          screen-invisible = 1.
          screen-input = 0.
        ENDIF.
      WHEN ra_1.
        IF screen-group1 EQ 'G2'.
          screen-invisible = 1.
          screen-input = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY

*&---------------------------------------------------------------------*
*&      Form  refresh_table_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_table_display .

  CHECK NOT g_grid IS INITIAL.

  PERFORM set_cell_attribute.

  l_scroll-row = 'X'.
  l_scroll-col = 'X'.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      i_soft_refresh = 'X'
      is_stable      = l_scroll.     "?? ??? ??? refresh

*  CALL METHOD g_grid->set_optimizer.

ENDFORM.                    " refresh_table_display

*&---------------------------------------------------------------------*
*&      Form  INITIAL_DATE
*&---------------------------------------------------------------------*
FORM initial_date .


*  CLEAR : SO_BADAT, SO_BADAT[].
*  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
*    EXPORTING
*      DAY_IN            = SY-DATUM
*    IMPORTING
*      LAST_DAY_OF_MONTH = SO_BADAT-HIGH   "?? ??
*    EXCEPTIONS
*      OTHERS            = 1.
*  IF SY-SUBRC = 0.
*    SO_BADAT-SIGN     = 'I'.
*    SO_BADAT-OPTION   = 'BT'.
*    SO_BADAT-LOW(6)   = SY-DATUM.
*    SO_BADAT-LOW+6(2) = '01'.
*    APPEND SO_BADAT.
*  ENDIF.

ENDFORM.                    " INITIAL_DATE

*&---------------------------------------------------------------------*
*&      Form  toolbar_pros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toolbar_pros  USING    r_object
      TYPE REF TO cl_alv_event_toolbar_set
      r_interactive.

  DATA : ls_toolbar  TYPE stb_button.

*// ??????
  CLEAR ls_toolbar.
  MOVE 'SALL'               TO  ls_toolbar-function.
  MOVE icon_select_all      TO  ls_toolbar-icon.
  MOVE 'Select ALL'         TO  ls_toolbar-quickinfo.
  MOVE 'Select ALL'         TO  ls_toolbar-text.
  MOVE ' '                  TO  ls_toolbar-disabled.
  APPEND ls_toolbar         TO   r_object->mt_toolbar.


  MOVE   3                  TO   ls_toolbar-butn_type.
  APPEND ls_toolbar         TO   r_object->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE 'MATD'               TO  ls_toolbar-function.
  MOVE icon_budget_update   TO  ls_toolbar-icon.
  MOVE 'Material up-date'   TO  ls_toolbar-quickinfo.
  MOVE 'Material up-date'   TO  ls_toolbar-text.
  MOVE ' '                  TO  ls_toolbar-disabled.
  APPEND ls_toolbar         TO  r_object->mt_toolbar.

ENDFORM.                    " toolbar_pros


*&---------------------------------------------------------------------*
*&      Form  user_command_pros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_pros  USING    r_ucomm.

  READ TABLE gt_display WITH KEY check = 'X'.

  CASE r_ucomm.
    WHEN 'INSE'.
      PERFORM insert_item_data.
      PERFORM set_cell_attribute.
    WHEN 'DELE'.
      PERFORM delete_item_data.
      PERFORM set_cell_attribute.
    WHEN 'MATD'.   "Return
      PERFORM message_popup_screen USING text-102 text-103
      CHANGING gc_continue.

      CHECK gc_continue EQ '1'.
      PERFORM material_master_update.
      PERFORM p1000_initial_data.
      PERFORM p1000_select_data.
      PERFORM p2000_process_data.
      PERFORM set_cell_attribute.
    WHEN 'SALL'.
      PERFORM select_all_item.
      PERFORM set_cell_attribute.
  ENDCASE.


ENDFORM.                    " user_command_pros


*&---------------------------------------------------------------------*
*&      Form  after_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM after_user_command  USING    e_ucomm.

  l_scroll-row = 'X'.
  l_scroll-col = 'X'.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      i_soft_refresh = 'X'
      is_stable      = l_scroll.     "?? ??? ??? refresh

*  CALL METHOD g_grid->set_optimizer.


ENDFORM.                    " after_user_command


*&---------------------------------------------------------------------*
*&      Form  data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed  USING    rr_data_changed
      TYPE REF TO cl_alv_changed_data_protocol
      e_onf4         TYPE char1
      e_ucomm        TYPE sy-ucomm.

  DATA : ls_mod_cells    TYPE lvc_s_modi,
        ls_inserted_row TYPE lvc_s_moce,
        ls_cells        TYPE lvc_s_modi,
        x_iwerk         LIKE t001w-iwerk.

*// ROWS CREATE,INSERT,COPY.
  IF  NOT rr_data_changed->mt_inserted_rows[] IS INITIAL.

*    LOOP AT rr_data_changed->mt_inserted_rows INTO ls_inserted_row.
*
*      CLEAR ls_mod_cells.
*      READ TABLE rr_data_changed->mt_good_cells
*      WITH KEY fieldname = 'STATUS'
*      row_id    = ls_inserted_row-row_id
*      INTO ls_mod_cells.
**// COPY
*      IF NOT ls_mod_cells-value IS INITIAL.
*
*        IF  ls_mod_cells-value = icon_green_light.
*
*          CALL METHOD rr_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = ls_mod_cells-row_id
*              i_fieldname = 'STATUS'
*              i_value     = icon_red_light.
*
*          CALL METHOD rr_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = ls_mod_cells-row_id
*              i_fieldname = 'EQUNR'
*              i_value     = ''.
*
*        ENDIF.
*
*        CALL METHOD rr_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_mod_cells-row_id
*            i_fieldname = 'MSG'
*            i_value     = ''.
*
*      ELSE.
*
*        CALL METHOD rr_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_mod_cells-row_id
*            i_fieldname = 'STATUS'
*            i_value     = icon_red_light.
*
*      ENDIF.
*
*    ENDLOOP.

  ENDIF.

ENDFORM.                    " data_changed


*&---------------------------------------------------------------------*
*&      Form  data_changed_finished
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed_finished  USING    e_modified    TYPE char01
      et_good_cells TYPE lvc_t_modi.
  DATA : ls_good_cells LIKE LINE OF et_good_cells.
  CLEAR : g_tabix2.

  READ TABLE et_good_cells INTO ls_good_cells INDEX 1.

  g_tabix = ls_good_cells-row_id.

  CASE ls_good_cells-fieldname.
    WHEN 'CHECK'.
      PERFORM data_changed_grid.
      PERFORM refresh_table_display.
  ENDCASE.

ENDFORM.                    " data_changed_finished


*&---------------------------------------------------------------------*
*&      Form  hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM hotspot_click  USING    e_row_id
      e_column_id.

  READ TABLE gt_display INDEX e_row_id.

  CASE e_column_id.
*    WHEN 'EBELN'.
*      SET PARAMETER ID 'BES'  FIELD gt_display-ebeln.
*      CALL TRANSACTION 'ME23N'.
*
*    WHEN 'MATNR'.
*      SET PARAMETER ID 'MAT'  FIELD gt_display-matnr.
*      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
*

  ENDCASE.


ENDFORM.                    " hotspot_click


*&---------------------------------------------------------------------*
*&      Form  double_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM double_click  USING    e_row
      e_column.
* ??/???? ??????? ??->???? ????? ??
  DATA: l_htype(4).
  CLEAR: l_htype.
  PERFORM p0000_check_chracter USING e_row l_htype.
  CHECK l_htype EQ 'NUMC'.


ENDFORM.                    " double_click


*&---------------------------------------------------------------------*
*&      Form  on_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM on_f4  USING sender         TYPE REF TO cl_gui_alv_grid
      e_fieldname    TYPE lvc_fname
      e_fieldvalue   TYPE lvc_value
      es_row_no      TYPE lvc_s_roid
      er_event_data  TYPE REF TO cl_alv_event_data
      et_bad_cells   TYPE lvc_t_modi
      e_display      TYPE char01.

  DATA : selectfield   LIKE  help_info-fieldname,
        it_fields     LIKE  help_value OCCURS 1 WITH HEADER LINE,
        select_value  LIKE  help_info-fldvalue,
        ld_tabix      LIKE  sy-tabix,
        ls_modi       TYPE  lvc_s_modi,
        l_matnr       LIKE  mara-matnr.

  RANGES: ls_lgort FOR mard-lgort.

  FIELD-SYMBOLS : <f4tab> TYPE lvc_t_modi.

  IF  e_fieldname = 'LGORT'.

    DATA: BEGIN OF f4_arbpl OCCURS 0,
      lgort     LIKE t001l-lgort,
      lgobe     LIKE t001l-lgobe,
    END   OF f4_arbpl.

    CLEAR : f4_arbpl[].

    CASE p_werks.
      WHEN 'P001'.
        REFRESH: ls_lgort.
        ls_lgort-sign = 'I'.
        ls_lgort-option = 'EQ'.
        ls_lgort-low = 'P600'. APPEND ls_lgort.
        ls_lgort-low = 'P610'. APPEND ls_lgort.
        ls_lgort-low = 'P620'. APPEND ls_lgort.
        ls_lgort-low = 'P630'. APPEND ls_lgort.
        ls_lgort-low = 'P640'. APPEND ls_lgort.
        ls_lgort-low = 'P690'. APPEND ls_lgort.
        SELECT *
              INTO CORRESPONDING FIELDS OF TABLE f4_arbpl
              FROM t001l
              WHERE  lgort IN ls_lgort.
      WHEN 'E001'.
        REFRESH: ls_lgort.
        ls_lgort-sign = 'I'.
        ls_lgort-option = 'EQ'.
        ls_lgort-low = 'E650'. APPEND ls_lgort.
        ls_lgort-low = 'E660'. APPEND ls_lgort.
        SELECT *
              INTO CORRESPONDING FIELDS OF TABLE f4_arbpl
              FROM t001l
              WHERE  lgort IN ls_lgort.
     WHEN 'E002'.
        REFRESH: ls_lgort.
        ls_lgort-sign = 'I'.
        ls_lgort-option = 'EQ'.
        ls_lgort-low = 'N650'. APPEND ls_lgort.
        ls_lgort-low = 'N660'. APPEND ls_lgort.
        SELECT *
              INTO CORRESPONDING FIELDS OF TABLE f4_arbpl
              FROM t001l
              WHERE  lgort IN ls_lgort.

      WHEN OTHERS.
        SELECT *
            INTO CORRESPONDING FIELDS OF TABLE f4_arbpl
            FROM t001l.
*           WHERE  lgort BETWEEN 'M100' AND 'M400'.
    ENDCASE.

    it_fields-tabname    = 'T001L'.
    it_fields-fieldname  = 'LGORT'.
    it_fields-selectflag = 'X'.
    APPEND it_fields.

    it_fields-tabname    = 'T001L'.
    it_fields-fieldname  = 'LGOBE'.
    it_fields-selectflag = ' '.
    APPEND it_fields.

    CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
      EXPORTING
        selectfield                  = selectfield
        cucol                        = 50
        curow                        = 2
      IMPORTING
        ind                          = ld_tabix
        select_value                 = select_value
      TABLES
        fields                       = it_fields
        full_table                   = f4_arbpl
      EXCEPTIONS
        full_table_empty             = 1
        no_tablestructure_given      = 2
        no_tablefields_in_dictionary = 3
        more_then_one_selectfield    = 4
        no_selectfield               = 5
        OTHERS                       = 6.

    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

*- assign the cell table fieldsymbol to the dereferenced data table and
*- fill the table.
      CHECK NOT select_value IS INITIAL.

      ASSIGN er_event_data->m_data->* TO <f4tab>.

      ls_modi-row_id    = es_row_no-row_id.
      ls_modi-fieldname = 'LGORT'.
      ls_modi-value     = select_value.
      APPEND ls_modi TO <f4tab>.

      er_event_data->m_event_handled = 'X'.

    ENDIF.

  ENDIF.

ENDFORM.                                                    " on_f4


*&---------------------------------------------------------------------*
*&      Form  p0000_check_chracter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM p0000_check_chracter USING e_row p_htype .
  DATA: l_string(50).

  CLEAR: l_string, p_htype.
  WRITE e_row           TO l_string.

  CALL FUNCTION 'NUMERIC_CHECK'
    EXPORTING
      string_in  = l_string
    IMPORTING
*     STRING_OUT =
      htype      = p_htype.

ENDFORM.                    " p0000_check_chracter

*&---------------------------------------------------------------------*
*&      Form  create_and_init_controls
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_and_init_controls.

  CHECK g_docking_container IS INITIAL.

*// Create an Instance Of Container
  CREATE OBJECT g_docking_container
    EXPORTING
      dynnr                       = '0100'
      repid                       = sy-repid
      side                        = g_docking_container->dock_at_top
      extension                   = 2000
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc <> 0.
    MESSAGE a003.
  ENDIF.

*// Create an Instance of ALV Control
  CREATE OBJECT g_grid
    EXPORTING
      i_parent      = g_docking_container
*     i_shellstyle  = ws_thickframe  "??????
      i_appl_events = 'X'.


*  CHECK custom_container IS INITIAL.
*
*  CREATE OBJECT custom_container
*    EXPORTING
*      container_name              = 'CC01'
*    EXCEPTIONS
*      cntl_error                  = 1
*      cntl_system_error           = 2
*      create_error                = 3
*      lifetime_error              = 4
*      lifetime_dynpro_dynpro_link = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE a003.
*  ENDIF.
*
**// Create an Instance of ALV Control
*  CREATE OBJECT g_grid
*      EXPORTING i_parent      = custom_container
**                  i_shellstyle  = ws_thickframe  "??????
*                i_appl_events = 'X'.


*// Tool Bar ?? ??
  PERFORM exclude_of_toolbar_button USING 'GT_EXCLUDE'.

*// ??? ?? ??? ??
  PERFORM set_field_catalogs.

*// CELL? ?? ??(Color?)
  PERFORM set_cell_attribute.

*// GRID ?? ??(Display): Display? ?? ??
  PERFORM display_layout_attribute USING gs_layocat.

*// Edit Event ?  Event Handler ??
  PERFORM event_handler_register.

*// Sorting
*  PERFORM build_sort_field.

*// F4 FIELD ??
  PERFORM set_f4_field.

*// TITLE ??
*  CASE 'X'.
*    WHEN P_R1.
*      G_TITLE = TEXT-006.
*    WHEN P_R2.
*      G_TITLE = TEXT-007.
*    WHEN P_R3.
*      G_TITLE = TEXT-008.
*    WHEN P_R4.
*      G_TITLE = TEXT-005.
*  ENDCASE.
*// ALV Grid Display
  PERFORM alv_grid_display.

  CALL METHOD g_grid->set_gridtitle
    EXPORTING
      i_gridtitle = g_title.

*// DETAIL LIST? ?? ??
*  lcl_alv_grid=>f_alv = '1'.

ENDFORM.                    " create_and_init_controls



*&---------------------------------------------------------------------*
*&      Form  exclude_of_toolbar_button
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0756   text
*----------------------------------------------------------------------*
FORM exclude_of_toolbar_button  USING    p_tabname..
  DATA : l_tab_name LIKE feld-name.

  FIELD-SYMBOLS : <table> TYPE ui_functions.

  CONCATENATE p_tabname '[]' INTO  l_tab_name.
  ASSIGN     (l_tab_name)    TO <table>.

*// ??? ?? ??
  PERFORM add_exclude_toolbar_button
  TABLES <table>
*        USING : cl_gui_alv_grid=>mc_fc_excl_all. " ** ?? ???? **
  USING : cl_gui_alv_grid=>mc_fc_loc_undo, " ????&LOCAL&UNDO
        cl_gui_alv_grid=>mc_fc_auf,      " ???? &AUF
        cl_gui_alv_grid=>mc_fc_average,  " &AVERAGE
*                cl_gui_alv_grid=>mc_fc_back_classic,
*                cl_gui_alv_grid=>mc_fc_call_abc, " &ABC
*                cl_gui_alv_grid=>mc_fc_call_chain,
*                cl_gui_alv_grid=>mc_fc_call_crbatch,
*                cl_gui_alv_grid=>mc_fc_call_crweb,
*                cl_gui_alv_grid=>mc_fc_call_lineitems,
*                cl_gui_alv_grid=>mc_fc_call_master_data,
*                cl_gui_alv_grid=>mc_fc_call_more,
*                cl_gui_alv_grid=>mc_fc_call_report,
*                cl_gui_alv_grid=>mc_fc_call_xint,
*                cl_gui_alv_grid=>mc_fc_call_xxl,
*                cl_gui_alv_grid=>mc_fc_col_invisible,
*                cl_gui_alv_grid=>mc_fc_col_optimize,
*                cl_gui_alv_grid=>mc_fc_current_variant,
*                cl_gui_alv_grid=>mc_fc_data_save,
*                cl_gui_alv_grid=>mc_fc_delete_filter,
*                cl_gui_alv_grid=>mc_fc_deselect_all,
        cl_gui_alv_grid=>mc_fc_detail,
*                cl_gui_alv_grid=>mc_fc_expcrdata,
*                cl_gui_alv_grid=>mc_fc_expcrdesig,
*                cl_gui_alv_grid=>mc_fc_expcrtempl,
*                cl_gui_alv_grid=>mc_fc_expmdb,
*                cl_gui_alv_grid=>mc_fc_extend,
*                cl_gui_alv_grid=>mc_fc_f4,
*                cl_gui_alv_grid=>mc_fc_filter,
*                cl_gui_alv_grid=>mc_fc_find,
*                cl_gui_alv_grid=>mc_fc_fix_columns,
        cl_gui_alv_grid=>mc_fc_graph,
*                cl_gui_alv_grid=>mc_fc_help,
        cl_gui_alv_grid=>mc_fc_info,
*                cl_gui_alv_grid=>mc_fc_load_variant,
*                cl_gui_alv_grid=>mc_fc_loc_copy,          " ? ??.
*                cl_gui_alv_grid=>mc_fc_html,
        cl_gui_alv_grid=>mc_fc_loc_copy_row,      " ? ??.
        cl_gui_alv_grid=>mc_fc_loc_cut,           " ??.
        cl_gui_alv_grid=>mc_fc_loc_delete_row,    " ???.
        cl_gui_alv_grid=>mc_fc_loc_insert_row,    " ???.
        cl_gui_alv_grid=>mc_fc_loc_move_row,
        cl_gui_alv_grid=>mc_fc_loc_append_row,    " ????.
        cl_gui_alv_grid=>mc_fc_loc_paste,         " ????.
        cl_gui_alv_grid=>mc_fc_loc_paste_new_row, " ????.
*                cl_gui_alv_grid=>mc_fc_maintain_variant,
*                cl_gui_alv_grid=>mc_fc_maximum,
*                cl_gui_alv_grid=>mc_fc_minimum,
*                cl_gui_alv_grid=>mc_fc_pc_file,
*                cl_gui_alv_grid=>mc_fc_print,
*                cl_gui_alv_grid=>mc_fc_print_back,
*                cl_gui_alv_grid=>mc_fc_print_prev,
        cl_gui_alv_grid=>mc_fc_refresh.
*                cl_gui_alv_grid=>mc_fc_reprep,
*                cl_gui_alv_grid=>mc_fc_save_variant,
*                cl_gui_alv_grid=>mc_fc_select_all,
*                cl_gui_alv_grid=>mc_fc_send,
*                cl_gui_alv_grid=>mc_fc_separator,
*                cl_gui_alv_grid=>mc_fc_sort,
*                cl_gui_alv_grid=>mc_fc_sort_asc,
*                cl_gui_alv_grid=>mc_fc_sort_dsc,
*                cl_gui_alv_grid=>mc_fc_subtot,
*                cl_gui_alv_grid=>mc_mb_sum,
*                cl_gui_alv_grid=>mc_fc_sum.
*                cl_gui_alv_grid=>mc_fc_to_office,
*                cl_gui_alv_grid=>mc_fc_to_rep_tree,
*                cl_gui_alv_grid=>mc_fc_unfix_columns,
*                cl_gui_alv_grid=>mc_fc_views,
*                cl_gui_alv_grid=>mc_fc_view_crystal,
*                cl_gui_alv_grid=>mc_fc_view_excel,
*                cl_gui_alv_grid=>mc_fc_view_grid,
*                cl_gui_alv_grid=>mc_fc_word_processor.

ENDFORM.                    " exclude_of_toolbar_button



*&---------------------------------------------------------------------*
*&      Form  add_exclude_toolbar_button
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_exclude_toolbar_button  TABLES   p_table
USING    p_value.

  DATA: l_exclude TYPE ui_func.

  l_exclude = p_value.
  APPEND l_exclude TO p_table. "????? GT_EXCLUDE? ??? ?? ??

ENDFORM.                    " add_exclude_toolbar_button


*&---------------------------------------------------------------------*
*&      Form  set_field_catalogs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_field_catalogs .


  CLEAR:   gt_fieldcat, gt_fieldcat[].

  PERFORM fill_field_catalogs
  USING:
        'S' 'FIELDNAME'   'CHECK',
        ' ' 'COLTEXT'     'SELECT',
        ' ' 'JUST'        'C',
        ' ' 'KEY'         'X',
        ' ' 'CHECKBOX'    'X',
        ' ' 'EDIT'        'X',
*        ' ' 'NO_OUT'      'X',
        'E' 'OUTPUTLEN'   '3',

          'S' 'FIELDNAME'   'ICON',
          ' ' 'COLTEXT'     'STATUS',
          ' ' 'JUST'        'C',
          ' ' 'KEY'         'X',
*          ' ' 'NO_OUT'      'X',
          'E' 'OUTPUTLEN'   '05'.


  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = c_st_nm
    CHANGING
      ct_fieldcat      = gt_fieldcat.

  PERFORM change TABLES gt_fieldcat.

ENDFORM.                    " set_field_catalogs
*&---------------------------------------------------------------------*
*&      Form  change
*&---------------------------------------------------------------------*
FORM change TABLES  pt_fieldcat TYPE lvc_t_fcat.

  DATA:l_fieldcat_s LIKE pt_fieldcat,
        l_name(3).

  LOOP AT pt_fieldcat INTO l_fieldcat_s.
    CASE l_fieldcat_s-fieldname.
      WHEN 'MATNR'.
        l_fieldcat_s-key = 'X'.
      WHEN 'LGORT'.
        l_fieldcat_s-f4availabl = 'X'.
      WHEN 'LMINB'.
        l_fieldcat_s-coltext = 'MIN'.
      WHEN 'LBSTF'.
        l_fieldcat_s-coltext = 'MAX'.
    ENDCASE.

    MODIFY pt_fieldcat FROM l_fieldcat_s INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " change


*&---------------------------------------------------------------------*
*&      Form  fill_field_catalogs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_field_catalogs  USING  p_gub  p_fname  p_value.

*// 'S' -> Structure? ?? ?? ? Setting? Start ??
*// 'E' -> Structure? ??? ?? ???? ???? ?? ??

  IF p_gub = 'S'.

    CLEAR gs_fieldcat.

  ENDIF.

  DATA l_fname(40).
  FIELD-SYMBOLS <fs> TYPE any.
  CONCATENATE 'GS_FIELDCAT-' p_fname INTO l_fname.

  ASSIGN (l_fname) TO <fs>.
  <fs> = p_value.

  IF p_gub = 'E'.

    APPEND gs_fieldcat TO gt_fieldcat.

  ENDIF.

ENDFORM.                    " fill_field_catalogs


*&---------------------------------------------------------------------*
*&      Form  set_cell_attribute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_cell_attribute .
* ? Cell? ?? ??
  DATA : lt_celltab TYPE lvc_t_styl,
        lt_color   TYPE lvc_t_scol.

  DATA: l_ebeln         LIKE bseg-ebeln.

  LOOP AT gt_display.
    g_index = sy-tabix.

    CLEAR: lt_celltab[], lt_color[].

    PERFORM fill_celltab CHANGING lt_celltab.
    PERFORM fill_color   CHANGING lt_color.

    CLEAR: gt_display-celltab[], gt_display-f_col[].

    INSERT LINES OF lt_celltab INTO TABLE gt_display-celltab.
    INSERT LINES OF lt_color   INTO TABLE gt_display-f_col.

    MODIFY gt_display INDEX g_index.
    CLEAR  gt_display.

  ENDLOOP.

ENDFORM.                    " set_cell_attribute


*&---------------------------------------------------------------------*
*&      Form  fill_celltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_celltab  CHANGING pt_celltab  TYPE lvc_t_styl.

* F4 control? ??
  DATA : ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  DATA : l_fieldcat TYPE lvc_s_fcat.

  LOOP AT gt_fieldcat INTO l_fieldcat.

    ls_celltab-fieldname = l_fieldcat-fieldname.

*// Display ?? ?? ??
    IF  (
          ls_celltab-fieldname  = 'LGORT' OR
          ls_celltab-fieldname  = 'LGPBE' OR
          ls_celltab-fieldname  = 'LMINB' OR
          ls_celltab-fieldname  = 'LBSTF' OR
          ls_celltab-fieldname  = 'CHECK'
         ).
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT ls_celltab INTO TABLE pt_celltab.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled +
      cl_gui_alv_grid=>mc_style_f4_no.
      INSERT ls_celltab INTO TABLE pt_celltab.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " fill_celltab


*&---------------------------------------------------------------------*
*&      Form  fill_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_color   CHANGING pt_color  TYPE lvc_t_scol.

  DATA : ls_celltab TYPE lvc_s_styl,
        ls_color   TYPE lvc_s_scol,
        l_mode     TYPE raw4.

  DATA : l_fieldcat TYPE lvc_s_fcat.

  LOOP AT gt_fieldcat INTO l_fieldcat.
    ls_color-fname = l_fieldcat-fieldname.
    IF gt_display-check = 'X'.
      ls_color-color-col = 4.
      ls_color-color-int = 0.
      INSERT ls_color INTO TABLE pt_color.
    ELSE.
      IF (
           ls_color-fname  = 'LGORT' OR
           ls_color-fname  = 'LGPBE' OR
           ls_color-fname  = 'LMINB' OR
           ls_color-fname  = 'LBSTF' OR
           ls_color-fname  = 'CHECK'
          ).
      ELSE.
        ls_color-color-col = 2.
        ls_color-color-int = 0.
        INSERT ls_color INTO TABLE pt_color.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " fill_color_200


*&---------------------------------------------------------------------*
*&      Form  display_layout_attribute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_layout_attribute  USING  p_layocat TYPE lvc_s_layo.

*// General display options
*  p_layocat-cwidth_opt = 'X'.
  p_layocat-sel_mode   = 'D'.   "A or D ???
*  p_layocat-edit       = 'E'.
  p_layocat-stylefname = 'CELLTAB'.
  p_layocat-ctab_fname = 'F_COL'.
  p_layocat-zebra = 'X'.

ENDFORM.                    " display_layout_attribute

*&---------------------------------------------------------------------*
*&      Form  display_layout_attribute_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_layout_attribute_200 USING  p_layocat TYPE lvc_s_layo.

*// General display options
*  P_LAYOCAT-CWIDTH_OPT = 'X'.
  p_layocat-sel_mode   = 'D'.   "A or D ???
*  p_layocat-edit       = 'E'.
  p_layocat-stylefname = 'CELLTAB'.
  p_layocat-ctab_fname = 'F_COL'.
*   p_layocat-zebra = 'X'.

ENDFORM.                    " display_layout_attribute_200

*&---------------------------------------------------------------------*
*&      Form  event_handler_register
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_handler_register .

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*// Event Handler ??
  CREATE OBJECT g_event_handler.

  SET HANDLER g_event_handler->handle_toolbar            FOR g_grid.
  SET HANDLER g_event_handler->handle_user_command       FOR g_grid.
  SET HANDLER g_event_handler->handle_data_changed       FOR g_grid.
  SET HANDLER g_event_handler->handle_data_changed_finished
  FOR g_grid.
  SET HANDLER g_event_handler->handle_onf4               FOR g_grid.
  SET HANDLER g_event_handler->handle_double_click       FOR g_grid.
  SET HANDLER g_event_handler->handle_after_user_command FOR g_grid.
  SET HANDLER g_event_handler->handle_hotspot_click      FOR g_grid.

ENDFORM.                    " event_handler_register


*&---------------------------------------------------------------------*
*&      Form  build_sort_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_sort_field .
*// Sort ?? ??

  gs_sort-fieldname = 'CHECK'.
  gs_sort-spos      = '1'.
  gs_sort-up        = 'X'.
  gs_sort-subtot    = 'X'.   "SUBTOTAL ??
  APPEND gs_sort TO gt_sort.

ENDFORM.                    " build_sort_field

*&---------------------------------------------------------------------*
*&      Form  build_sort_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_sort_field_200 .
*// Sort ?? ??

*  gs_sort2-fieldname = 'CHECK'.
*  gs_sort2-spos      = '1'.
*  gs_sort2-up        = 'X'.
*  gs_sort2-subtot    = 'X'.   "SUBTOTAL ??
*  APPEND gs_sort2 TO gt_sort2.

ENDFORM.                    " build_sort_field_200

*&---------------------------------------------------------------------*
*&      Form  set_f4_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_f4_field .
  CLEAR : gt_f4, gt_f4[].
*-- F4 FIELD ??.
*-- [Caution]???? ABC??? ??
  gs_f4-fieldname  = 'LGORT'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

  CALL METHOD g_grid->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4.

ENDFORM.                    " set_f4_field


*&---------------------------------------------------------------------*
*&      Form  alv_grid_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_display .
*// 'gs_layout' must at least contain the report-id to allow
  g_repid = sy-repid.
*// 2.At least field REPORT of this structure has to be filled!
  alv_variant-report = g_repid.

*// ????? ?? ??
  lcl_alv_grid=>f_alv = '1'.

*// Display
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = gs_layocat
      it_toolbar_excluding = gt_exclude
      i_save               = 'A'   " ??????.
      i_default            = 'X'   " ???? ???? ??.
      is_variant           = alv_variant  " ?? ?? display
    CHANGING
      it_outtab            = gt_display[]
      it_sort              = gt_sort
      it_fieldcatalog      = gt_fieldcat[].


ENDFORM.                    " alv_grid_display

*&---------------------------------------------------------------------*
*&      Form  p1000_initial_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM p1000_initial_data .

  CLEAR : gt_display, gt_display[],
  gt_itab,    gt_itab[].


ENDFORM.                    " p1000_initial_data

*&---------------------------------------------------------------------*
*&      Form  p1000_select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM p1000_select_data .

  CLEAR : gt_itab,   gt_itab[].

  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE gt_itab
         FROM marc AS a INNER JOIN mara AS b
                        ON a~matnr = b~matnr
                        INNER JOIN mard AS c
                        ON a~matnr = c~matnr
         WHERE a~werks = p_werks
           AND b~mtart = 'ERSA'
*           AND c~lgort LIKE 'M%'
           AND c~lgort IN s_lgort
           AND b~ersda IN s_ersda
           AND c~matnr IN s_matnr.


ENDFORM.                    " p1000_select_data


*&---------------------------------------------------------------------*
*&      Form  p2000_process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM p2000_process_data .

  PERFORM modify_data .
  PERFORM display_append_alvdata.

ENDFORM.                    " p2000_process_data

*&---------------------------------------------------------------------*
*&      Form  display_append_alvdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_append_alvdata .

  DATA : wa_m058  LIKE  zmms0058.

  LOOP AT gt_itab.
    MOVE-CORRESPONDING gt_itab TO gt_display.

    IF gt_itab-lgort IS INITIAL AND
       gt_itab-lgpbe IS INITIAL AND
       gt_itab-lminb IS INITIAL AND
       gt_itab-lbstf IS INITIAL.
      gt_display-icon = icon_red_light.
    ELSE.
      gt_display-icon = icon_yellow_light.
    ENDIF.

    IF ( gt_itab-lgort EQ 'M100' OR
         gt_itab-lgort EQ 'M200' OR
         gt_itab-lgort EQ 'M300' OR
         gt_itab-lgort EQ 'M400' )   AND
       NOT gt_itab-lgpbe IS INITIAL AND
       NOT gt_itab-lminb IS INITIAL AND
       NOT gt_itab-lbstf IS INITIAL.
      gt_display-icon = icon_green_light.
    ENDIF.

    PERFORM read_text USING 'MATNR'
                            gt_itab-matnr
                   CHANGING gt_display-maktx.

    APPEND gt_display. CLEAR gt_display.
  ENDLOOP.


ENDFORM.                    " display_append_alvdata

*&---------------------------------------------------------------------*
*&      Form  modify_data
*&---------------------------------------------------------------------*
*&      ??? line color, column color,
*&---------------------------------------------------------------------*
FORM modify_data .


ENDFORM.                    " modify_data

*&---------------------------------------------------------------------*
*&      Form  refresh_text_edit_5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_class_data.

  IF NOT custom_container IS INITIAL.
    CALL METHOD custom_container->free.
    FREE custom_container.
  ENDIF.

  FREE g_grid.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " refresh_text_edit_5
*&---------------------------------------------------------------------*
*&      Form  data_changed_grid
*&---------------------------------------------------------------------*
FORM data_changed_grid.

  READ TABLE gt_display INTO gs_display INDEX g_tabix.



ENDFORM.                    " data_changed_grid


*&---------------------------------------------------------------------*
*&      Form  INSERT_ITEM_DATA
*&---------------------------------------------------------------------*
FORM insert_item_data .

  APPEND gt_display. CLEAR gt_display.

ENDFORM.                    " INSERT_ITEM_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_ITEM_DATA
*&---------------------------------------------------------------------*
FORM delete_item_data .

  DELETE gt_display WHERE check = 'X'.

  CLEAR : gt_display.

ENDFORM.                    " DELETE_ITEM_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_INITIAL_DATA_2
*&---------------------------------------------------------------------*
FORM p1000_initial_data_2 .

*  CLEAR : gt_display, gt_display[].
*
*loop at bdc.
*    APPEND gt_display.
*  ENDDO.


ENDFORM.                    " P1000_INITIAL_DATA_2
*&---------------------------------------------------------------------*
*&      Form  selection_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FNAME  text
*----------------------------------------------------------------------*
FORM selection_screen  USING  fname.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = ',*.*,*.*.'
      mode             = 'O'
    IMPORTING
      filename         = fname
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

ENDFORM.                    " selection_screen
*&---------------------------------------------------------------------*
*&      Form  get_upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_upload_file .

  CHECK fname NE ' '.

  CALL FUNCTION 'Z_MM_EXCEL_UPLOAD'
    EXPORTING
      filename   = fname
      itab       = 'BDC'
      begin_line = 2
    TABLES
      outab      = bdc.


  LOOP AT bdc.
    MOVE-CORRESPONDING bdc TO gt_itab.

    PERFORM func_conv_matn1  USING gt_itab-matnr.

    gt_itab-werks = p_werks.

    SELECT SINGLE *
           INTO CORRESPONDING FIELDS OF gt_itab
           FROM mara
           WHERE matnr = gt_itab-matnr.

    APPEND gt_itab. CLEAR gt_itab.
  ENDLOOP.


ENDFORM.                    " get_upload_file
*&---------------------------------------------------------------------*
*&      Form  FUNC_CONV_MATN1
*&---------------------------------------------------------------------*
FORM func_conv_matn1  USING    p_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = p_matnr
    IMPORTING
      output       = p_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

ENDFORM.                    " FUNC_CONV_MATN1
*&---------------------------------------------------------------------*
*&      Form  MATERIAL_MASTER_UPDATE
*&---------------------------------------------------------------------*
FORM material_master_update.
  DATA : i_body  LIKE zspm0019,
         e_return LIKE zmms0053.
  DATA: lf_mline TYPE sy-index.

  RANGES: ls_lgort FOR mard-lgort.

  CLEAR : gt_bdcmsg, gt_bdcmsg[].

  LOOP AT gt_display WHERE check = 'X'.

** Fuorng on 02/07/12

*   IF NOT gt_display-lgort BETWEEN 'M100' AND 'M400'.
*      gt_bdcmsg-zrsnum = gt_display-matnr.
*      gt_bdcmsg-type   = 'E'.
*      gt_bdcmsg-message = 'Storage Location is not correct.!'.
*      APPEND gt_bdcmsg. CLEAR gt_bdcmsg.
*      CONTINUE.
*    ENDIF.

    CASE p_werks.
      WHEN 'P001'.
        REFRESH: ls_lgort.
        ls_lgort-sign = 'I'.
        ls_lgort-option = 'EQ'.
        ls_lgort-low = 'P600'. APPEND ls_lgort.
        ls_lgort-low = 'P610'. APPEND ls_lgort.
        ls_lgort-low = 'P620'. APPEND ls_lgort.
        ls_lgort-low = 'P630'. APPEND ls_lgort.
        ls_lgort-low = 'P640'. APPEND ls_lgort.
        ls_lgort-low = 'P690'. APPEND ls_lgort.
      WHEN 'E001'.
        REFRESH: ls_lgort.
        ls_lgort-sign = 'I'.
        ls_lgort-option = 'EQ'.
        ls_lgort-low = 'E650'. APPEND ls_lgort.
        ls_lgort-low = 'E660'. APPEND ls_lgort.
      WHEN 'E002'.
        REFRESH: ls_lgort.
        ls_lgort-sign = 'I'.
        ls_lgort-option = 'EQ'.
        ls_lgort-low = 'N650'. APPEND ls_lgort.
        ls_lgort-low = 'N660'. APPEND ls_lgort.

    ENDCASE.
    IF NOT gt_display-lgort IN ls_lgort.
      gt_bdcmsg-zrsnum = gt_display-matnr.
      gt_bdcmsg-type   = 'E'.
      gt_bdcmsg-message = 'Storage Location is not correct.!'.
      APPEND gt_bdcmsg. CLEAR gt_bdcmsg.
      CONTINUE.
    ENDIF.
** end on 02/07/12

    MOVE-CORRESPONDING gt_display TO i_body.
    ADD 1 TO lf_mline.

    CALL FUNCTION 'Z_MM_MATERIAL_MASTER_UPDATE_2'
      EXPORTING
        i_body   = i_body
      IMPORTING
        e_return = e_return.

    MOVE-CORRESPONDING e_return TO gt_bdcmsg.
    gt_bdcmsg-zrsnum = gt_display-matnr.
    APPEND gt_bdcmsg. CLEAR gt_bdcmsg.
  ENDLOOP.

  IF NOT gt_bdcmsg[] IS INITIAL.
    PERFORM call_message_screen.
  ENDIF.

ENDFORM.                    " MATERIAL_MASTER_UPDATE
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_S_LGORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form HELP_REQUEST_S_LGORT .
   DATA: BEGIN OF LT_T001L OCCURS 0,
          WERKS TYPE T001L-WERKS,
          LGORT TYPE T001L-LGORT,
          LGOBE TYPE T001L-LGOBE,
        END OF LT_T001L.
  DATA L_TABIX TYPE SY-TABIX.
  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.

  PERFORM VALUE_READ USING: 'P_WERKS'.
  LOOP AT DYNPREAD.
    CASE SY-TABIX.
      WHEN 1.
     P_WERKS = DYNPREAD-FIELDVALUE.
    ENDCASE.
  ENDLOOP.

  SELECT WERKS
         LGORT
         LGOBE
         INTO TABLE LT_T001L
         FROM T001L
         WHERE WERKS EQ P_WERKS.
  IF SY-SUBRC EQ 0.
    LOOP AT LT_T001L.
      L_TABIX = SY-TABIX.
      IF LT_T001L-WERKS EQ 'P001'.
        IF NOT LT_T001L-LGORT BETWEEN 'P600' AND 'P699'.
          DELETE LT_T001L INDEX L_TABIX.
        ENDIF.
      ELSEIF LT_T001L-WERKS EQ 'E001'.
        IF NOT LT_T001L-LGORT BETWEEN 'E600' AND 'E699'.
          DELETE LT_T001L INDEX L_TABIX.
        ENDIF.
** Furong on 02/08/12
     ELSEIF LT_T001L-WERKS EQ 'E002'.
        IF NOT LT_T001L-LGORT BETWEEN 'N600' AND 'N699'.
          DELETE LT_T001L INDEX L_TABIX.
        ENDIF.
** end on 02/08/12
      ELSE.
        DELETE LT_T001L INDEX L_TABIX.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT LT_T001L.
    VALUETAB-VALUE = LT_T001L-WERKS.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_T001L-LGORT.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_T001L-LGOBE.
    APPEND VALUETAB. CLEAR VALUETAB.
  ENDLOOP.

  PERFORM ADD_FIELDS USING: 'T001L' 'WERKS' ' ',
                            'T001L' 'LGORT' 'X',
                            'T001L' 'LGOBE' ' '.
  PERFORM HELP_VALUES_GET.


  IF SELECT_INDEX > 0.
    READ TABLE LT_T001L   INDEX SELECT_INDEX.
    PERFORM VALUE_UPDATE USING:
            'X'   'S_LGORT-LOW' LT_T001L-LGORT 0.
  ENDIF.
endform.                    " HELP_REQUEST_S_LGORT
*&---------------------------------------------------------------------*
*&      Form  VALUE_READ
*&---------------------------------------------------------------------*
FORM VALUE_READ USING P_NAME.
  DYNPREAD-FIELDNAME = P_NAME.
  APPEND DYNPREAD.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME                   = SY-CPROG
            DYNUMB                   = SY-DYNNR
       TABLES
            DYNPFIELDS               = DYNPREAD
*      EXCEPTIONS
*           INVALID_ABAPWORKAREA     = 1
*           INVALID_DYNPROFIELD      = 2
*           INVALID_DYNPRONAME       = 3
*           INVALID_DYNPRONUMMER     = 4
*           INVALID_REQUEST          = 5
*           NO_FIELDDESCRIPTION      = 6
*           INVALID_PARAMETER        = 7
*           UNDEFIND_ERROR           = 8
*           DOUBLE_CONVERSION        = 9
*           OTHERS                   = 10
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " VALUE_READ
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDS
*&---------------------------------------------------------------------*
FORM ADD_FIELDS USING  P_TABNAME P_FIELDNAME P_FLAG.
  FIELDS-TABNAME = P_TABNAME.
  FIELDS-FIELDNAME = P_FIELDNAME.
  FIELDS-SELECTFLAG = P_FLAG.
  APPEND FIELDS.      CLEAR FIELDS.
ENDFORM.                    " ADD_FIELDS
*&---------------------------------------------------------------------*
*&      Form  HELP_VALUES_GET
*&---------------------------------------------------------------------*
FORM HELP_VALUES_GET.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            DISPLAY                   = ' '
       IMPORTING
            INDEX                     = SELECT_INDEX
       TABLES
            FIELDS                    = FIELDS
            SELECT_VALUES             = SELECT_VALUES
            VALUETAB                  = VALUETAB
       EXCEPTIONS
            FIELD_NOT_IN_DDIC         = 1
            MORE_THEN_ONE_SELECTFIELD = 2
            NO_SELECTFIELD            = 3
            OTHERS                    = 4.
ENDFORM.                    " HELP_VALUES_GET
*&---------------------------------------------------------------------*
*&      Form  VALUE_UPDATE
*&---------------------------------------------------------------------*
FORM VALUE_UPDATE USING  P_PROCESS
                         P_FIELDNAME
                         P_FIELDVALUE
                         P_STEPL.
  CLEAR DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = P_FIELDNAME.
  DYNPFIELDS-FIELDVALUE = P_FIELDVALUE.
  IF P_STEPL > 0.
    DYNPFIELDS-STEPL = P_STEPL.
  ENDIF.
  APPEND DYNPFIELDS.      CLEAR DYNPFIELDS.

  IF P_PROCESS EQ 'X'.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              DYNAME               = SY-CPROG
              DYNUMB               = SY-DYNNR
         TABLES
              DYNPFIELDS           = DYNPFIELDS
         EXCEPTIONS
              INVALID_ABAPWORKAREA = 1
              INVALID_DYNPROFIELD  = 2
              INVALID_DYNPRONAME   = 3
              INVALID_DYNPRONUMMER = 4
              INVALID_REQUEST      = 5
              NO_FIELDDESCRIPTION  = 6
              UNDEFIND_ERROR       = 7
              OTHERS               = 8.
    REFRESH DYNPFIELDS.
  ENDIF.

ENDFORM.                    " VALUE_UPDATE
