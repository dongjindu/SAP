*&---------------------------------------------------------------------*
*&  Include           ZRDA_QA_DOC_ARCHR_F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  HANDLE_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SENDER  text
*      -->P_E_FIELDNAME  text
*      -->P_E_FIELDVALUE  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM handle_f4  USING sender         TYPE REF TO cl_gui_alv_grid
                      e_fieldname    TYPE lvc_fname
                      e_fieldvalue   TYPE lvc_value
                      es_row_no      TYPE lvc_s_roid
                      er_event_data  TYPE REF TO cl_alv_event_data
                      et_bad_cells   TYPE lvc_t_modi
                      e_display      TYPE char01.


  er_event_data->m_event_handled  =  'X'.

  READ TABLE it_data INDEX es_row_no-row_id.

  CASE e_fieldname.
    WHEN 'LFA1-LIFNR'.
      MODIFY it_data INDEX es_row_no-row_id.

      CALL METHOD g_grid1->refresh_table_display
        EXPORTING
          is_stable = gs_stable.
  ENDCASE.

ENDFORM.                    " HANDLE_F4



*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*      -->P_E_ONF4  text
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM data_changed USING er_data_changed TYPE REF TO
                        cl_alv_changed_data_protocol
                        e_onf4 e_ucomm.

  g_change = 'X'.

  DATA : l_dup_cnt            TYPE i,
         l_usrid              TYPE usnam.


  DATA:  lt_modi  TYPE  lvc_t_modi,
         ls_modi  TYPE  lvc_s_modi.

  DATA:  l_lifnr  TYPE lfa1-lifnr,
         l_name1  TYPE lfa1-name1.

  DATA : l_htype LIKE dd01v-datatype.

  FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

  lt_modi = er_data_changed->mt_good_cells.



  CLEAR : gt_mod_cells, gt_mod_cells[], g_error_flag.

  gt_mod_cells[] = er_data_changed->mt_mod_cells[].


  LOOP AT  gt_mod_cells.
    TRANSLATE  gt_mod_cells-value TO UPPER CASE.



    CLEAR  it_data.
    READ TABLE  it_data  INDEX  gt_mod_cells-row_id.
*    l_usrid    =  it_data-usrid.

    CASE  gt_mod_cells-fieldname.
*      WHEN  'USRID'.
*        l_usrid    =  gt_mod_cells-value.
**
*      WHEN 'TVL_ZIP_CODE'.

    ENDCASE.

*
*    MODIFY it_data         INDEX  gt_mod_cells-row_id.
  ENDLOOP.
*
*  CALL METHOD er_data_changed->display_protocol.
*
*  CALL METHOD g_grid1->refresh_table_display
*    EXPORTING
*      is_stable = gs_stable1.         " Refresh

ENDFORM.                    " DATA_CHANGED



*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MODIFIED  text
*      -->P_ET_GOOD_CELLS  text
*----------------------------------------------------------------------*
FORM data_changed_finished  USING    p_e_modified
                                     p_et_good_cells.

ENDFORM.                    " DATA_CHANGED_FINISHED



*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*----------------------------------------------------------------------*
FORM handle_double_click USING    p_e_row
                                  p_e_column.

ENDFORM.                    " HANDLE_DOUBLE_CLICK



*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM handle_toolbar1  USING i_object TYPE REF TO cl_alv_event_toolbar_set.

  DATA: ls_toolbar TYPE stb_button.

*
  CLEAR ls_toolbar.
  MOVE 3 TO ls_toolbar-butn_type.
  APPEND ls_toolbar TO i_object->mt_toolbar.

*
  CLEAR ls_toolbar.
  MOVE 'CHNG'            TO ls_toolbar-function.
  MOVE  icon_change      TO ls_toolbar-icon.
  MOVE 'CHANGE ROW'      TO ls_toolbar-quickinfo.
  MOVE 'CHANGE'          TO ls_toolbar-text.
  APPEND ls_toolbar      TO i_object->mt_toolbar.
*
*
  CLEAR ls_toolbar.
  MOVE 3 TO ls_toolbar-butn_type.
  APPEND ls_toolbar TO i_object->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE 'DELE'            TO ls_toolbar-function.
  MOVE  icon_delete_row  TO ls_toolbar-icon.
  MOVE 'DELETE ROW'      TO ls_toolbar-quickinfo.
  MOVE 'DELETE'          TO ls_toolbar-text.
  APPEND ls_toolbar      TO i_object->mt_toolbar.

*  CLEAR ls_toolbar.
*  MOVE 3 TO ls_toolbar-butn_type.
*  APPEND ls_toolbar TO i_object->mt_toolbar.
*
*  CLEAR ls_toolbar.
*  MOVE 'LOOK'            TO ls_toolbar-function.
*  MOVE '@3Q@'            TO ls_toolbar-icon.   "ICON_DELETE_ROW
*  MOVE 'ASSET MASTER LOOKUP' TO ls_toolbar-quickinfo.
*  MOVE 'ASSET'    TO ls_toolbar-text.
*  APPEND ls_toolbar      TO i_object->mt_toolbar.
**
**
*  CLEAR ls_toolbar.
*  MOVE 3 TO ls_toolbar-butn_type.
*  APPEND ls_toolbar TO i_object->mt_toolbar.
*
*  CLEAR ls_toolbar.
*  MOVE 'ASHT'                 TO ls_toolbar-function.
*  MOVE icon_history           TO ls_toolbar-icon.   "ICON_DELETE_ROW
*  MOVE 'ASSET MASTER HISTORY' TO ls_toolbar-quickinfo.
*  MOVE 'ASSET HISTORY'    TO ls_toolbar-text.
*  APPEND ls_toolbar      TO i_object->mt_toolbar.

ENDFORM.                    " HANDLE_TOOLBAR1



*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM handle_user_command  USING  i_ucomm.

  DATA: l_select_row_count TYPE i.

  CLEAR: gt_selected_rows[], gs_selected_row, gv_row, gv_value,
         gv_col, gs_row_id, gs_col_id, gs_row_no, gv_input.
*  STATICS : l_chan TYPE int4 VALUE 1.

  CALL METHOD g_grid1->is_ready_for_input
*    EXPORTING
*      i_row_id        =
*      is_col_id       =
    RECEIVING
      ready_for_input = gv_input.

*
  CALL METHOD g_grid1->get_selected_rows
    IMPORTING
      et_row_no = gt_selected_rows.

  DESCRIBE TABLE gt_selected_rows LINES l_select_row_count.
  CALL METHOD g_grid1->get_current_cell
    IMPORTING
      e_row     = gv_row
      e_value   = gv_value
      e_col     = gv_col
      es_row_id = gs_row_id
      es_col_id = gs_col_id
      es_row_no = gs_row_no.

  CASE i_ucomm.
    WHEN 'CHNG'.         " Change Row
      IF l_select_row_count > 1.
        MESSAGE i999 WITH 'Multiple Row Selection not possible'.
        EXIT.
      ENDIF.

      g_change = 'X'.
      PERFORM change_row.
      CALL METHOD g_grid1->refresh_table_display
        EXPORTING
          is_stable = gs_stable1.         " Refresh
    WHEN 'DELE'.
      g_change = 'X'.
      IF gt_selected_rows[] IS NOT INITIAL.
        PERFORM delete_row TABLES gt_selected_rows.
      ELSE.
        MESSAGE i999 WITH 'Please select at least one row'.
      ENDIF.
      CALL METHOD g_grid1->refresh_table_display
        EXPORTING
          is_stable = gs_stable1.         " Refresh

  ENDCASE.

*  CALL METHOD g_grid1->refresh_table_display
*    EXPORTING
*      is_stable = gs_stable1.         " Refresh

ENDFORM.                    " HANDLE_USER_COMMAND



*&---------------------------------------------------------------------*
*&      Form  SET_VARIANT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_VARIANT1  text
*----------------------------------------------------------------------*
FORM set_variant1  CHANGING ls_variant1  TYPE disvariant.

  ls_variant1-report   = sy-repid.
  ls_variant1-username = sy-uname.
*    CASE gv_handle.                   " Change as to different display
*      WHEN 1.
  ls_variant1-handle = '0001'.
*      WHEN 2.
*        gs_variant1-handle = '0002'.
*    ENDCASE.

ENDFORM.                    " SET_VARIANT1



*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYOUT1  text
*----------------------------------------------------------------------*
FORM set_layout1 CHANGING  ls_layout1 TYPE lvc_s_layo..

*  ls_layout1-box_fname  = 'CHECK'. "
*  LS_LAYOUT1-EDIT       = 'X'.     " All Edit Mode
  ls_layout1-sel_mode   = 'D'.      " ALV
  ls_layout1-zebra      = 'X'.
*  LS_LAYOUT1-EXCP_FNAME = 'LIGHT'.  " Field for Traffic Light
  ls_layout1-info_fname = 'LNCOL'.  " Field for Line Color
*  ls_layout1-totals_bef = 'X'.     " Total Line Displey at the Top

*  ls_layout1-grid_title = 'Flights'.
*  ls_layout1-smalltitle = 'X'.
*  ls_layout1-cwidth_opt = 'X'.      " Optimizing Field Length to Header Length
  ls_layout1-stylefname = 'STYLE'.    " Field for Style(Push Button,List Box
  ls_layout1-ctab_fname = 'COLOR'.    " Field for Color

ENDFORM.                    " SET_LAYOUT1



*&---------------------------------------------------------------------*
*&      Form  SORT_SUBTOTAL1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_SORT1  text
*----------------------------------------------------------------------*
FORM sort_subtotal1 CHANGING gt_sort1 TYPE lvc_t_sort.

* Field Category
  DATA: ls_sort TYPE lvc_s_sort.

  CLEAR: gt_sort[], ls_sort.

  ls_sort-fieldname = 'ACL_NO'.
  ls_sort-spos      = '1'.
  ls_sort-up        = 'X'.
*  ls_sort-subtot    = 'X'.
  APPEND ls_sort  TO gt_sort1.

  CLEAR ls_sort.
  ls_sort-fieldname = 'VIN_NO'.
  ls_sort-spos      = '2'.
  ls_sort-up        = 'X'.
*  ls_sort-subtot    = 'X'.
  APPEND ls_sort  TO gt_sort1.

*  CLEAR ls_sort.
*  ls_sort-fieldname = 'POST_AUTHOR'.
*  ls_sort-spos      = '3'.
*  ls_sort-seltext   = 'User ID'.
*  APPEND ls_sort  TO gt_sort1.

  CLEAR ls_sort.
  ls_sort-fieldname = 'POST_DATE'.
  ls_sort-spos      = '4'.
  ls_sort-up        = 'X'.
  APPEND ls_sort  TO gt_sort1.

  CLEAR ls_sort.
  ls_sort-fieldname = 'POST_DATE_TM'.
  ls_sort-spos      = '5'.
  ls_sort-up        = 'X'.
  APPEND ls_sort  TO gt_sort1.

ENDFORM.                    " SORT_SUBTOTAL1



*&---------------------------------------------------------------------*
*&      Form  SET_TOOLBAR1_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_EXCLUDE1  text
*----------------------------------------------------------------------*
FORM set_toolbar1_exclude CHANGING gt_exclude1 TYPE ui_functions .

  DATA  ls_exclude1  TYPE ui_func.

  CLEAR:  ls_exclude1.
  ls_exclude1 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude1 TO gt_exclude1.
  CLEAR:  ls_exclude1.
  ls_exclude1 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude1 TO gt_exclude1.
  CLEAR:  ls_exclude1.
  ls_exclude1 = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude1 TO gt_exclude1.
  CLEAR:  ls_exclude1.
  ls_exclude1 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude1 TO gt_exclude1.
  CLEAR:  ls_exclude1.
  ls_exclude1 = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude1 TO gt_exclude1.
  CLEAR:  ls_exclude1.
  ls_exclude1 = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude1 TO gt_exclude1.
  CLEAR:  ls_exclude1.
  ls_exclude1 = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude1 TO gt_exclude1.
  CLEAR:  ls_exclude1.
  ls_exclude1 = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude1 TO gt_exclude1.
  CLEAR:  ls_exclude1.
  ls_exclude1 = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude1 TO gt_exclude1.
  CLEAR:  ls_exclude1.
  ls_exclude1 = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude1 TO gt_exclude1.


* Edit ## ##(## edit### ### ## ##)
*  ls_exclude = cl_gui_alv_grid=>mc_fg_edit.
*  APPEND ls_exclude TO pt_exclude.

*  PERFORM append_exclude_functions
**        USING : cl_gui_alv_grid=>mc_fc_excl_all. " ** ## #### **
*  USING :
*          cl_gui_alv_grid=>mc_fc_loc_undo, " ####&LOCAL&UNDO
*          cl_gui_alv_grid=>mc_fc_auf,      " #### &AUF
*          cl_gui_alv_grid=>mc_fc_average,  " &AVERAGE
*          cl_gui_alv_grid=>mc_fc_back_classic,
*          cl_gui_alv_grid=>mc_fc_call_abc, " &ABC
*          cl_gui_alv_grid=>mc_fc_call_chain,
*          cl_gui_alv_grid=>mc_fc_call_crbatch,
*          cl_gui_alv_grid=>mc_fc_call_crweb,
*          cl_gui_alv_grid=>mc_fc_call_lineitems,
*          cl_gui_alv_grid=>mc_fc_call_master_data,
*          cl_gui_alv_grid=>mc_fc_call_more,
*          cl_gui_alv_grid=>mc_fc_call_report,
*          cl_gui_alv_grid=>mc_fc_call_xint,
*          cl_gui_alv_grid=>mc_fc_call_xxl,
*          cl_gui_alv_grid=>mc_fc_col_invisible,
*          cl_gui_alv_grid=>mc_fc_col_optimize,
*          cl_gui_alv_grid=>mc_fc_current_variant,
*          cl_gui_alv_grid=>mc_fc_data_save,
*          cl_gui_alv_grid=>mc_fc_delete_filter,
*          cl_gui_alv_grid=>mc_fc_detail,
*          cl_gui_alv_grid=>mc_fc_expcrdata,
*          cl_gui_alv_grid=>mc_fc_expcrdesig,
*          cl_gui_alv_grid=>mc_fc_expcrtempl,
*          cl_gui_alv_grid=>mc_fc_expmdb,
*          cl_gui_alv_grid=>mc_fc_extend,
*          cl_gui_alv_grid=>mc_fc_f4,
*          cl_gui_alv_grid=>mc_fc_filter,
*          cl_gui_alv_grid=>mc_fc_find,
*          cl_gui_alv_grid=>mc_fc_fix_columns,
*          cl_gui_alv_grid=>mc_fc_graph,
*          cl_gui_alv_grid=>mc_fc_help,
*          cl_gui_alv_grid=>mc_fc_info,
*          cl_gui_alv_grid=>mc_fc_load_variant,
*          cl_gui_alv_grid=>mc_fc_loc_copy,          " # ##.
*          cl_gui_alv_grid=>mc_fc_html,
*          cl_gui_alv_grid=>mc_fc_loc_copy_row,      " # ##.
*          cl_gui_alv_grid=>mc_fc_loc_cut,           " ##.
*          cl_gui_alv_grid=>mc_fc_loc_delete_row,    " ###.
*          cl_gui_alv_grid=>mc_fc_loc_insert_row,    " ###.
*          cl_gui_alv_grid=>mc_fc_loc_move_row,
*          cl_gui_alv_grid=>mc_fc_loc_append_row,    " ####.
*          cl_gui_alv_grid=>mc_fc_loc_paste,         " ####.
*          cl_gui_alv_grid=>mc_fc_loc_paste_new_row, " ####.
*          cl_gui_alv_grid=>mc_fc_maintain_variant,
*          cl_gui_alv_grid=>mc_fc_maximum,
*          cl_gui_alv_grid=>mc_fc_minimum,
*          cl_gui_alv_grid=>mc_fc_pc_file,
*          cl_gui_alv_grid=>mc_fc_print,
*          cl_gui_alv_grid=>mc_fc_print_back,
*          cl_gui_alv_grid=>mc_fc_print_prev,
*          cl_gui_alv_grid=>mc_fc_refresh,
*          cl_gui_alv_grid=>mc_fc_reprep,
*          cl_gui_alv_grid=>mc_fc_save_variant,
*          cl_gui_alv_grid=>mc_fc_select_all,
*          cl_gui_alv_grid=>mc_fc_deselect_all,
*          cl_gui_alv_grid=>mc_fc_send,
*          cl_gui_alv_grid=>mc_fc_separator,
*          cl_gui_alv_grid=>mc_fc_sort,
*          cl_gui_alv_grid=>mc_fc_sort_asc,
*          cl_gui_alv_grid=>mc_fc_sort_dsc,
*          cl_gui_alv_grid=>mc_fc_subtot,
*          cl_gui_alv_grid=>mc_fc_sum,
*          cl_gui_alv_grid=>mc_fc_to_office,
*          cl_gui_alv_grid=>mc_fc_to_rep_tree,
*          cl_gui_alv_grid=>mc_fc_unfix_columns,
*          cl_gui_alv_grid=>mc_fc_views,
*          cl_gui_alv_grid=>mc_fc_view_crystal,
*          cl_gui_alv_grid=>mc_fc_view_excel,
*          cl_gui_alv_grid=>mc_fc_view_grid,
*          cl_gui_alv_grid=>mc_fc_word_processor.

ENDFORM.                    " SET_TOOLBAR1_EXCLUDE



*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fieldcat1 .

  PERFORM fieldcat_merge_function.

*  PERFORM fieldcat_make_list.

*  PERFORM FIELDCAT_FORM_FSYMBOL.

ENDFORM.                    " SET_FIELDCAT1



*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_MERGE_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_merge_function .

  CLEAR g_colpos1.

  DATA : l_tabix TYPE sy-tabix.
  DATA : ls_fcat TYPE lvc_s_fcat.
  DATA : lt_fcat TYPE lvc_t_fcat.

  REFRESH gt_fieldcat1.

  CALL FUNCTION 'ZDOC_QA_LVC_FCAT_MERGE'
    EXPORTING
      i_structure_name = 'ZSDA_ARCH03L'  "'ZSDA_ARCH03' ONLY ABAP DICTIONARY
    CHANGING
      ct_fieldcat      = gt_fieldcat1.

  LOOP AT gt_fieldcat1 INTO gs_fieldcat1.
    CASE gs_fieldcat1-fieldname.
      WHEN 'POST_ID'.
        gs_fieldcat1-no_out    = 'X'.
      WHEN 'FIRST_NAME' OR 'LAST_NAME'.
        IF g_proc NE 'L'.
          gs_fieldcat1-no_out    = 'X'.
        ENDIF.
      WHEN 'ACL_NO'.
*        gs_fieldcat1-checkbox = 'X'.
*        gs_fieldcat1-edit = 'X'.
        IF g_proc NE 'B' AND g_proc NE 'L'.
          gs_fieldcat1-no_out    = 'X'.
        ENDIF.
        gs_fieldcat1-outputlen = 12.
      WHEN 'VIN_NO'.
*        gs_fieldcat1-edit = 'X'.
        IF g_proc NE 'B' AND g_proc NE 'L'.
          gs_fieldcat1-no_out    = 'X'.
        ENDIF.
        gs_fieldcat1-outputlen = 18.
      WHEN 'POST_AUTHOR'.
*        gs_fieldcat1-edit = 'X'.
        gs_fieldcat1-scrtext_m ='User ID'.
        gs_fieldcat1-outputlen = 12.
      WHEN 'POST_DATE'.
*        gs_fieldcat1-edit = 'X'.
        gs_fieldcat1-outputlen = 10.
      WHEN 'POST_DATE_TM'.
*        gs_fieldcat1-edit = 'X'.
        gs_fieldcat1-outputlen = 15.
      WHEN 'POST_MONTH'.
        IF g_proc NE 'C' AND g_proc NE 'R'.
          gs_fieldcat1-no_out    = 'X'.
        ENDIF.
        gs_fieldcat1-outputlen = 15.
      WHEN 'META_VALUE'.
        IF g_proc NE 'V'.
          gs_fieldcat1-no_out    = 'X'.
        ENDIF.
        gs_fieldcat1-scrtext_m = 'Vendor Name'.
        gs_fieldcat1-outputlen = 40.
      WHEN 'POST_TITLE'.
        gs_fieldcat1-outputlen = 80.

    ENDCASE.
    MODIFY gt_fieldcat1 FROM gs_fieldcat1.
    CLEAR : gs_fieldcat1.
  ENDLOOP.

ENDFORM.                    " FIELDCAT_MERGE_FUNCTION



*&---------------------------------------------------------------------*
*&      Form  REGISTER_F4_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_f4_field .

***  DATA: lt_f4 TYPE lvc_t_f4 WITH HEADER LINE .
***
***  lt_f4-fieldname = 'CBU'.
***  lt_f4-register = 'X' .
***  lt_f4-getbefore = 'X' .
***  APPEND lt_f4 .
***
***  lt_f4-fieldname = 'LOCAT'.
***  lt_f4-register = 'X' .
**** LT_F4-GETBEFORE = 'X' .
***  lt_f4-chngeafter = 'X'.
***  APPEND lt_f4 .
***
***  CALL METHOD g_grid1->register_f4_for_fields
***    EXPORTING
***      it_f4 = lt_f4[].

ENDFORM.                    " REGISTER_F4_FIELD



*&---------------------------------------------------------------------*
*&      Form  SET_EVENT_HANDLER1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_event_handler1 .

  CREATE OBJECT g_event1.
**----Registering handler methods to handle ALV Grid events
  SET HANDLER g_event1->handle_on_f4                  FOR g_grid1.
  SET HANDLER g_event1->handle_data_changed           FOR g_grid1.
  SET HANDLER g_event1->handle_data_changed_finished  FOR g_grid1.
  SET HANDLER g_event1->handle_double_click           FOR g_grid1.
  SET HANDLER g_event1->handle_toolbar                FOR g_grid1.
  SET HANDLER g_event1->handle_user_command           FOR g_grid1.
  SET HANDLER g_event1->handle_button_click           FOR g_grid1.

ENDFORM.                    " SET_EVENT_HANDLER1



*&---------------------------------------------------------------------*
*&      Form  SET_CELLATTRIBUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_cellattribute .

*  LOOP AT IT_DATA.
*    IF SY-TABIX < 7 .
*      IT_DATA-MENGE = '2'.
*      IT_DATA-LIGHT = '1'.         "Red:Traffic Light Setting
*      IT_DATA-LNCOL = 'C100'.      "ALV Layout 'Info_fname'
*    ELSEIF SY-TABIX < 14 AND SY-TABIX >= 7.
*      IT_DATA-MENGE = '4'.
*      IT_DATA-LIGHT = '2'.        "Yellow:Traffic Light Setting
*      IT_DATA-LNCOL = 'C300'.     "ALV Layout 'Info_fname'
*    ELSEIF SY-TABIX >= 14.
*      IT_DATA-MENGE = 6.
*      IT_DATA-LIGHT = '3'.        "Green:Traffic Light Setting
*      IT_DATA-LNCOL = 'C500'.     "ALV Layout 'Info_fname'
*    ENDIF.
*    MODIFY IT_DATA.
*  ENDLOOP.

** Cell color setting
  DATA: lt_color TYPE lvc_t_scol,
        ls_color TYPE lvc_s_scol.
  DATA: l_tabix TYPE sy-tabix.

*  LOOP AT IT_DATA.
*    CLEAR: LS_COLOR, LT_COLOR.
*    L_TABIX = SY-TABIX.
*    IF IT_DATA-MENGE = '2'.
*      LS_COLOR-FNAME = 'MENGE'.
*      LS_COLOR-COLOR-COL = 6.
*      LS_COLOR-COLOR-INT = 0.
*    ELSEIF IT_DATA-MENGE = '4'.
*      LS_COLOR-FNAME = 'MENGE'.
*      LS_COLOR-COLOR-COL = 6.
*      LS_COLOR-COLOR-INT = 1.
*    ELSEIF IT_DATA-MENGE = '6'.
*      LS_COLOR-FNAME = 'BSART'.
*      LS_COLOR-COLOR-COL = '6'.
*      LS_COLOR-COLOR-INT = 1.
*      LS_COLOR-COLOR-INV = 0.
*    ENDIF.
*    INSERT LS_COLOR INTO TABLE LT_COLOR.
*    INSERT LINES OF LT_COLOR INTO TABLE IT_DATA-COLOR.
*    MODIFY IT_DATA INDEX L_TABIX.
*    CLEAR: IT_DATA.
*  ENDLOOP.

* Cell Style PushButton
  DATA: lt_cellbtn TYPE lvc_t_styl,
        ls_cellbtn TYPE lvc_s_styl,
        ls_fieldcat TYPE lvc_s_fcat,
        l_mode     TYPE raw4,
        l_type     TYPE c,
        index      TYPE i.

*  CLEAR INDEX.
*  LOOP AT IT_DATA.
*    INDEX = INDEX + 1.
*    LOOP AT GT_FIELDCAT1 INTO LS_FIELDCAT.
*      LS_CELLBTN-FIELDNAME = LS_FIELDCAT-FIELDNAME.
*      IF LS_CELLBTN-FIELDNAME = 'BUTON'.
*        LS_CELLBTN-STYLE  = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
*        LS_CELLBTN-STYLE2 = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*      ENDIF.
*      INSERT LS_CELLBTN INTO TABLE LT_CELLBTN.
*    ENDLOOP.
*    INSERT LINES OF LT_CELLBTN INTO TABLE IT_DATA-STYLE.
*    MODIFY IT_DATA INDEX INDEX.
*    CLEAR IT_DATA.
*  ENDLOOP.

ENDFORM.                    " SET_CELLATTRIBUTE



*&---------------------------------------------------------------------*
*&      Form  SET_BUILD_DRDN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_DATA[]  text
*----------------------------------------------------------------------*
FORM set_build_drdn  CHANGING pt_outtab TYPE STANDARD TABLE.

  DATA : ls_outtab LIKE it_data.
  DATA : l_index TYPE i.

*--- Dynamic DropDown List
*  LOOP AT PT_OUTTAB INTO LS_OUTTAB.
*    L_INDEX = SY-TABIX.
*
*    IF LS_OUTTAB-MENGE EQ 2.
*      LS_OUTTAB-DDHDL = '4'.
*    ELSEIF LS_OUTTAB-MENGE EQ 4.
*      LS_OUTTAB-DDHDL = '2'.
*    ELSE.
*      LS_OUTTAB-DDHDL = '3'.
*    ENDIF.
*    MODIFY  PT_OUTTAB FROM LS_OUTTAB.
*  ENDLOOP.

ENDFORM.                    " SET_BUILD_DRDN



*&---------------------------------------------------------------------*
*&      Form  SET_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_dropdown .


*  DATA: lt_dropdown TYPE lvc_t_drop,
*        ls_dropdown TYPE lvc_s_drop.

  DATA: lt_dropdown TYPE lvc_t_drop,
        ls_dropdown TYPE lvc_s_drop.
  DATA: lt_dropalis TYPE lvc_t_dral,
        ls_dropalis TYPE lvc_s_dral.


  DATA: l_bsart TYPE ekko-bsart,
        l_batxt TYPE t161t-batxt.

**  SELECT * FROM T001W.
**    LS_DROPALIS-HANDLE = 1.
**    LS_DROPALIS-INT_VALUE = T001W-WERKS.
**    CONCATENATE T001W-WERKS ':' T001W-NAME2
**           INTO LS_DROPALIS-VALUE.
**    APPEND LS_DROPALIS TO LT_DROPALIS.
**    CLEAR LS_DROPALIS.
**  ENDSELECT.
*
**  SELECT a~bsart b~batxt INTO (l_bsart, l_batxt)
**    FROM ekko AS a INNER JOIN t161t AS b
**                   ON a~bsart = b~bsart  UP TO 10 ROWS.
*
*  CLEAR ls_dropalis.
*  ls_dropalis-handle = 4.
*  ls_dropalis-int_value = 'ZUS1'.
*  CONCATENATE 'ZUS1' ':' '11111'
*         INTO ls_dropalis-value.
*  APPEND ls_dropalis TO lt_dropalis.
*  CLEAR ls_dropalis.
*  ls_dropalis-handle = 4.
*  ls_dropalis-int_value = 'ZUS2'.
*  CONCATENATE 'ZUS2' ':' '22222'
*         INTO ls_dropalis-value.
*  APPEND ls_dropalis TO lt_dropalis.
*  CLEAR ls_dropalis.
*  ls_dropalis-handle = 4.
*  ls_dropalis-int_value = 'ZUS3'.
*  CONCATENATE 'ZUS3' ':' '33333'
*         INTO ls_dropalis-value.
*  APPEND ls_dropalis TO lt_dropalis.
*
*  CLEAR ls_dropalis.
*  ls_dropalis-handle = 2.
*  ls_dropalis-int_value = 'ZUS4'.
*  CONCATENATE 'ZUS4' ':' '44444'
*         INTO ls_dropalis-value.
*  APPEND ls_dropalis TO lt_dropalis.
*  CLEAR ls_dropalis.
*  ls_dropalis-handle = 2.
*  ls_dropalis-int_value = 'ZUS5'.
*  CONCATENATE 'ZUS5' ':' '55555'
*         INTO ls_dropalis-value.
*  APPEND ls_dropalis TO lt_dropalis.
*  CLEAR ls_dropalis.
*  ls_dropalis-handle = 2.
*  ls_dropalis-int_value = 'ZUS6'.
*  CONCATENATE 'ZUS6' ':' '66666'
*         INTO ls_dropalis-value.
*  APPEND ls_dropalis TO lt_dropalis.
**  ENDSELECT.
*  CLEAR ls_dropalis.
*  ls_dropalis-handle = 3.
*  ls_dropalis-int_value = 'ZUS7'.
*  CONCATENATE 'ZUS7' ':' '77777'
*         INTO ls_dropalis-value.
*  APPEND ls_dropalis TO lt_dropalis.
*  CLEAR ls_dropalis.
*  ls_dropalis-handle = 3.
*  ls_dropalis-int_value = 'ZUS8'.
*  CONCATENATE 'ZUS8' ':' '88888'
*         INTO ls_dropalis-value.
*  APPEND ls_dropalis TO lt_dropalis.
*  CLEAR ls_dropalis.
*  ls_dropalis-handle = 3.
*  ls_dropalis-int_value = 'ZUS9'.
*  CONCATENATE 'ZUS9' ':' '99999'
*         INTO ls_dropalis-value.
*  APPEND ls_dropalis TO lt_dropalis.
*
*  CALL METHOD g_grid1->set_drop_down_table
*    EXPORTING
*      it_drop_down       = lt_dropdown
*      it_drop_down_alias = lt_dropalis.

ENDFORM.                    " SET_DROPDOWN



*&---------------------------------------------------------------------*
*&      Form  CHANGE_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_row .

*  LOOP AT gt_selected_rows INTO gs_selected_row.
*    CLEAR it_data.
*    READ TABLE it_data INDEX gs_selected_row-row_id.
*
*  ENDLOOP.

  CLEAR: is_data, is_data_o.
  READ TABLE gt_selected_rows INTO gs_selected_row INDEX 1.
  READ TABLE it_data INDEX gs_selected_row-row_id INTO is_data.
  is_data_o = is_data.
  CASE g_proc.
    WHEN 'B'.  "Buyback change
      CLEAR: is_buyback-acl_no, is_buyback-vin_no, g_title, g_aclnoonly.
      is_buyback-acl_no = is_data-acl_no.
      is_buyback-vin_no = is_data-vin_no.
      g_title           = is_data-post_title.
      IF is_buyback-vin_no IS INITIAL.
        g_aclnoonly = 'X'.
      ENDIF.
      CALL SCREEN 217.
    WHEN 'L'.  "Legal change
      CLEAR: is_buyback-acl_no, is_buyback-vin_no, g_title, g_aclnoonly.
      is_buyback-acl_no = is_data-acl_no.
      is_buyback-vin_no = is_data-vin_no.
      g_lname           = is_data-last_name.
      g_fname           = is_data-first_name.
      g_title           = is_data-post_title.
      IF is_buyback-vin_no IS INITIAL.
        g_aclnoonly = 'X'.
      ENDIF.
      CALL SCREEN 227.
    WHEN 'C' OR 'R'.  "Claim change & Reclaim change
      CLEAR: g_title.

      ztda_posts-post_month = is_data-post_month.
      g_title               = is_data-post_title.
      CALL SCREEN 237.
    WHEN 'V'.  "Vendor Files change
      CLEAR: g_title.
      lfa1-name1 = is_data-meta_value.
      g_title    = is_data-post_title.
      CALL SCREEN 257.
  ENDCASE.

ENDFORM.                    " CHANGE_ROW



*&---------------------------------------------------------------------*
*&      Form  DELETE_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SELECTED_ROWS  text
*----------------------------------------------------------------------*
FORM delete_row TABLES pt_selected_rows.

  CLEAR: gv_msg,   g_answer.
***  gv_msg = 'Data delete confirm'.
  gv_msg = 'Delete Confirm'.
  PERFORM confirm_job  USING g_answer
                             text-m09.
  IF g_answer  =  c_1.
*
    CASE g_proc.
      WHEN 'B' OR 'L'. "Buyback data deletion & Legal data deletion
        LOOP AT pt_selected_rows INTO gs_selected_row.

          READ TABLE it_data INDEX gs_selected_row-row_id
                              INTO is_data.
          it_data-del = 'X'.
          MODIFY it_data INDEX gs_selected_row-row_id
                         TRANSPORTING del.
          PERFORM ztda_posts_delete.
        ENDLOOP.
        DELETE it_data WHERE del NE ' '.
      WHEN 'C' OR 'R'. "Claim data deletion & Reclaim data deletion
        LOOP AT pt_selected_rows INTO gs_selected_row.

          READ TABLE it_data INDEX gs_selected_row-row_id
                              INTO is_data.
          it_data-del = 'X'.
          MODIFY it_data INDEX gs_selected_row-row_id
                         TRANSPORTING del.
          PERFORM ztda_posts_delete.
        ENDLOOP.
        DELETE it_data WHERE del NE ' '.
      WHEN 'V'. "Vendor Files deletion
        LOOP AT pt_selected_rows INTO gs_selected_row.

          READ TABLE it_data INDEX gs_selected_row-row_id
                              INTO is_data.
          it_data-del = 'X'.
          MODIFY it_data INDEX gs_selected_row-row_id
                         TRANSPORTING del.
          PERFORM ztda_posts_delete.
        ENDLOOP.
        DELETE it_data WHERE del NE ' '.
    ENDCASE.
    MESSAGE s999 WITH text-m11.      "Data has been deleted successfully!
  ENDIF.

ENDFORM.                    " DELETE_ROW
