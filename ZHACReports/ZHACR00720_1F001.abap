*&---------------------------------------------------------------------*
*&  Include           ZHARC00700F001
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM start_of_selection.
  PERFORM data_processing.
ENDFORM.                    " START_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESSING
*&---------------------------------------------------------------------*
FORM data_processing.
  DATA lt_zbdctable LIKE zhacs0010 OCCURS 0 WITH HEADER LINE.

  PERFORM select_zbdctable
                      TABLES lt_zbdctable.

  PERFORM move_data_to_list
                      TABLES lt_zbdctable.

ENDFORM.                    " DATA_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  END_OF_SELECTION
*&---------------------------------------------------------------------*
FORM end_of_selection.
  CALL SCREEN 100.
ENDFORM.                    " END_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CONTROL1
*&---------------------------------------------------------------------*
FORM alv_cl_control1.

  PERFORM alv_cl_create_obj_dock_cont
              USING    1500
                       cl_gui_docking_container=>dock_at_top
              CHANGING g_docking_container1.
  PERFORM alv_cl_create_obj_alv_grid
              USING    g_docking_container1
              CHANGING g_alv_grid1.
  PERFORM alv_cl_variant1.
  PERFORM alv_cl_layout1.
  PERFORM alv_cl_register_edit_event USING g_alv_grid1.
  PERFORM alv_cl_exclude_fcode1.
  PERFORM alv_cl_create_obj_event_recvr1.
  PERFORM alv_cl_set_for_input USING g_alv_grid1.
* ###### #### ### ## ####.
  PERFORM alv_cl_cell_control1.
  PERFORM alv_cl_sort1.
  PERFORM alv_cl_fieldcatalog_merge1.
  PERFORM alv_cl_set_for_first_display1.

ENDFORM.                    " ALV_CL_CONTROL1
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CREATE_OBJ_DOCK_CONT
*&---------------------------------------------------------------------*
FORM alv_cl_create_obj_dock_cont
     USING    p_extension
              p_side
     CHANGING p_docking_container TYPE REF TO cl_gui_docking_container.

  CLEAR p_docking_container.

* Container for Custom Controls in the Screen Area
  DATA: l_repid TYPE sy-repid,
        l_dynnr TYPE sy-dynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.

  CREATE OBJECT p_docking_container
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      extension = p_extension
      side      = p_side.

ENDFORM.                    " ALV_CL_CREATE_OBJ_DOCK_CONT
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CREATE_OBJ_ALV_GRID
*&---------------------------------------------------------------------*
FORM alv_cl_create_obj_alv_grid
       USING    p_container
                CHANGING p_alv_grid TYPE REF TO cl_gui_alv_grid.

  CLEAR p_alv_grid.

* Create an instance of alv control
  CREATE OBJECT p_alv_grid
    EXPORTING
      i_parent = p_container.

ENDFORM.                    " ALV_CL_CREATE_OBJ_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_VARIANT1
*&---------------------------------------------------------------------*
FORM alv_cl_variant1.

  CLEAR gs_variant1.
* ABAP ######
  gs_variant1-report   = sy-repid.
  gs_variant1-handle   = 'LIS1'.
  gs_variant1-username = sy-uname.

ENDFORM.                    " ALV_CL_VARIANT1

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_LAYOUT1
*&---------------------------------------------------------------------*
FORM alv_cl_layout1.
  DATA: l_title TYPE lvc_title,
        c_money(18) TYPE c.

  CLEAR gs_layout1.

* Set a layout for the grid control
  gs_layout1-cwidth_opt = 'X'.              "### ###
  gs_layout1-sel_mode   = 'D'.
*  GS_LAYOUT1-TOTALS_BEF  = 'X'.
*  GS_LAYOUT1-NO_MERGING = 'X'.
*  GS_LAYOUT1-DETAILINIT = 'X'. "DISPLAY INITIAL VALUES ON DETAIL SCREEN
*  GS_LAYOUT1-NO_KEYFIX  = ' '.
*  GS_LAYOUT1-BOX_FNAME  = 'MARK'.
  gs_layout1-no_rowmark = ' '.
  gs_layout1-smalltitle = 'X'.
  gs_layout1-stylefname = 'CELLSTYL'.         "# ###
*  GS_LAYOUT1-INFO_FNAME = 'ROWSCOL'.         "# #
  gs_layout1-ctab_fname = 'CELLSCOL'.         "# #
  gs_layout1-zebra = 'X'.
  gs_layout1-detailtitl = 'TEST'.
*  CONCATENATE '('
*              G_SMR_FILTEREDLINE
*              '/'
*              G_SMR_TOTALLINE
*              ')'
*              INTO GS_ALV_LAYOUT1-GRID_TITLE.
*  READ TABLE GT_LIST INDEX 1.
*
*  CONCATENATE '#####:'
*              GT_LIST-WDATE+0(4)
*              '#'
*              GT_LIST-WDATE+4(2)
*              '#'
*              GT_LIST-WDATE+6(2)
*              '#'
*         INTO L_TITLE SEPARATED BY SPACE.
*
*  WRITE G_REFUND_AMT TO C_MONEY CURRENCY 'KRW'.
*  CONCATENATE L_TITLE
*             '#####:'
*             C_MONEY
*             '#'
*        INTO GS_LAYOUT1-GRID_TITLE
*             SEPARATED BY SPACE.
*

ENDFORM.                    " ALV_CL_LAYOUT1

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_REGISTER_EDIT_EVENT
*&---------------------------------------------------------------------*
FORM alv_cl_register_edit_event
                    USING p_alv_grid TYPE REF TO cl_gui_alv_grid.
* Edit  ### ##
  IF sy-batch IS INITIAL.
    CALL METHOD p_alv_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  ENDIF.
ENDFORM.                    " ALV_CL_REGISTER_EDIT_EVENT

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_EXCLUDE_FCODE1
*&---------------------------------------------------------------------*
FORM alv_cl_exclude_fcode1.

* ### ### #### ## ### ####.
  REFRESH gt_fcode1.
* FIELD-SYMBOLS : <TABLE> TYPE UI_FUNCTIONS.

*  DATA : LS_EXCLUDE   TYPE UI_FUNC.
*  DATA : L_TABLE_NAME LIKE FELD-NAME.

*  CONCATENATE 'PT_FCODE' '[]' INTO  L_TABLE_NAME.
*  ASSIGN     (L_TABLE_NAME)    TO <TABLE>.

  PERFORM alv_cl_append_exclude_function
        TABLES gt_fcode1 "<TABLE>
*        USING : cl_gui_alv_grid=>mc_fc_excl_all. " ** ## #### **
        USING :
                cl_gui_alv_grid=>mc_fc_loc_undo, " ####&LOCAL&UNDO
*                CL_GUI_ALV_GRID=>MC_FC_AUF,      " #### &AUF
*                CL_GUI_ALV_GRID=>MC_FC_AVERAGE,  " &AVERAGE
*                cl_gui_alv_grid=>mc_fc_back_classic,
*                CL_GUI_ALV_GRID=>MC_FC_CALL_ABC, " &ABC
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
*                CL_GUI_ALV_GRID=>MC_FC_DELETE_FILTER,
*                CL_GUI_ALV_GRID=>MC_FC_DESELECT_ALL,
*                cl_gui_alv_grid=>mc_fc_detail,
*                CL_GUI_ALV_GRID=>MC_FC_EXPCRDATA,
*                CL_GUI_ALV_GRID=>MC_FC_EXPCRDESIG,
*                CL_GUI_ALV_GRID=>MC_FC_EXPCRTEMPL,
*                cl_gui_alv_grid=>mc_fc_expmdb,
*                cl_gui_alv_grid=>mc_fc_extend,
**                cl_gui_alv_grid=>mc_fc_f4,
*                CL_GUI_ALV_GRID=>MC_FC_FILTER,
*                CL_GUI_ALV_GRID=>MC_FC_FIND,
*                cl_gui_alv_grid=>mc_fc_fix_columns,
                cl_gui_alv_grid=>mc_fc_graph,
*                CL_GUI_ALV_GRID=>MC_FC_HELP,
                cl_gui_alv_grid=>mc_fc_info,
*                CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
*                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,          " # ##
*                cl_gui_alv_grid=>mc_fc_html,
                cl_gui_alv_grid=>mc_fc_loc_copy_row,      " # ##.
                cl_gui_alv_grid=>mc_fc_loc_cut,           " ##.
                cl_gui_alv_grid=>mc_fc_loc_delete_row,    " ###.
                cl_gui_alv_grid=>mc_fc_loc_insert_row,    " ###.
                cl_gui_alv_grid=>mc_fc_loc_move_row,
                cl_gui_alv_grid=>mc_fc_loc_append_row,    " ####.
                cl_gui_alv_grid=>mc_fc_loc_paste,         " ####.
                cl_gui_alv_grid=>mc_fc_loc_paste_new_row, " ####.
*                cl_gui_alv_grid=>mc_fc_maintain_variant,
*                cl_gui_alv_grid=>mc_fc_maximum,
*                cl_gui_alv_grid=>mc_fc_minimum,
*                cl_gui_alv_grid=>mc_fc_pc_file,
*                CL_GUI_ALV_GRID=>MC_FC_PRINT,
*                CL_GUI_ALV_GRID=>MC_FC_PRINT_BACK,
*                CL_GUI_ALV_GRID=>MC_FC_PRINT_PREV,
                cl_gui_alv_grid=>mc_fc_refresh,
*                cl_gui_alv_grid=>mc_fc_reprep,
*                cl_gui_alv_grid=>mc_fc_save_variant,
*                CL_GUI_ALV_GRID=>MC_FC_SELECT_ALL.
*                cl_gui_alv_grid=>mc_fc_send,
*                cl_gui_alv_grid=>mc_fc_separator,
*                cl_gui_alv_grid=>mc_fc_sort,
*                cl_gui_alv_grid=>mc_fc_sort_asc,
*                cl_gui_alv_grid=>mc_fc_sort_dsc,
*                CL_GUI_ALV_GRID=>MC_FC_SUBTOT,
*                CL_GUI_ALV_GRID=>MC_MB_SUM,
*                CL_GUI_ALV_GRID=>MC_FC_SUM,
*                cl_gui_alv_grid=>mc_fc_to_office,
*                cl_gui_alv_grid=>mc_fc_to_rep_tree,
*                cl_gui_alv_grid=>mc_fc_unfix_columns,
*                CL_GUI_ALV_GRID=>MC_FC_VIEWS,
                cl_gui_alv_grid=>mc_fc_view_crystal.
*                CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
*                CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID.
*                cl_gui_alv_grid=>mc_fc_word_processor.

ENDFORM.                    " ALV_CL_EXCLUDE_FCODE1
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_APPEND_EXCLUDE_FUNCTION
*&---------------------------------------------------------------------*
FORM alv_cl_append_exclude_function
                              TABLES pt_fcode
                              USING  p_value.
  DATA : ls_exclude TYPE ui_func.
  ls_exclude = p_value.
  APPEND ls_exclude TO pt_fcode.
ENDFORM.                    " ALV_CL_APPEND_EXCLUDE_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CREATE_OBJ_EVENT_RECVR1
*&---------------------------------------------------------------------*
FORM alv_cl_create_obj_event_recvr1.

  CLEAR g_event_receiver1.

  CREATE OBJECT g_event_receiver1.
  SET HANDLER g_event_receiver1->handle_toolbar1       FOR g_alv_grid1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_DOUBLE_CLICK1 FOR G_ALV_GRID1.
  SET HANDLER g_event_receiver1->handle_user_command1  FOR g_alv_grid1.
  SET HANDLER g_event_receiver1->handle_data_changed1 FOR g_alv_grid1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_AFTER_USER_COMMAND1
*                                                      FOR G_ALV_GRID1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_BUTTON_CLICK1 FOR G_ALV_GRID1.

ENDFORM.                    "ALV_CL_CREATE_OBJ_EVENT_RECVR1
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SET_FOR_INPUT
*&---------------------------------------------------------------------*
FORM alv_cl_set_for_input
            USING p_alv_grid TYPE REF TO cl_gui_alv_grid.

* ### ## ## DISPLAY
* INPUT ## 1## ## ### ####
*            0## ## ####.
  CALL METHOD p_alv_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDFORM.                    " ALV_CL_SET_FOR_INPUT

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CELL_CONTROL1
*&---------------------------------------------------------------------*
FORM alv_cl_cell_control1.
  DATA: lt_cellstyl TYPE lvc_s_styl OCCURS 0 WITH HEADER LINE,
        lt_cellscol TYPE lvc_s_scol OCCURS 0 WITH HEADER LINE,
        l_tabix    TYPE sy-tabix.
* ## # ##
* ## ## ### ### ### ####
* ### ### #### ##.
*  CHECK SCREEN_MODE EQ C_CHANGE.

  LOOP AT gt_list.
    l_tabix = sy-tabix.

    CLEAR: gt_list-cellstyl,
           gt_list-cellscol.

*    IF GT_LIST-FBRST EQ C_FBRST_S OR
*       GT_LIST-FBRST EQ C_FBRST_X.
*      GT_LIST-MARK = SPACE.
*      PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
*         USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    ELSE.
*    PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
*       USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*    ENDIF.
    INSERT LINES OF lt_cellstyl INTO TABLE gt_list-cellstyl.
    INSERT LINES OF lt_cellscol INTO TABLE gt_list-cellscol.
    MODIFY gt_list INDEX l_tabix TRANSPORTING
                                      cellstyl
                                      cellscol.
    REFRESH: lt_cellstyl,
             lt_cellscol.
    CLEAR  : lt_cellstyl,
             lt_cellscol.
    CLEAR: gt_list.
  ENDLOOP.
ENDFORM.                    " ALV_CL_CELL_CONTROL1

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SORT1
*&---------------------------------------------------------------------*
FORM alv_cl_sort1.
  DATA: ls_sort LIKE lvc_s_sort.

*  REFRESH GT_SORT1.

*  LS_SORT-FIELDNAME = 'BELNR'.
*  LS_SORT-SPOS      = 1.
*  LS_SORT-GROUP     = ' '.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-SUBTOT    = ' '.
*  APPEND LS_SORT TO GT_SORT1.
*  CLEAR LS_SORT.

ENDFORM.                    " ALV_CL_SORT1

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_FIELDCATALOG_MERGE1
*&---------------------------------------------------------------------*
FORM alv_cl_fieldcatalog_merge1.

  DATA: l_tabix TYPE sy-tabix,
        ls_fdcat LIKE lvc_s_fcat.

  REFRESH gt_fdcat1.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_buffer_active    = 'X'
      i_bypassing_buffer = 'X'
      i_structure_name   = 'ZHACS0010'
    CHANGING
      ct_fieldcat        = gt_fdcat1[].

  LOOP AT gt_fdcat1
          INTO ls_fdcat.
    l_tabix = sy-tabix.
    ls_fdcat-key        = space.
    ls_fdcat-fix_column = space.

*   ######
    CASE ls_fdcat-fieldname.
      WHEN 'EXMON'.
        ls_fdcat-fix_column = 'X'.
    ENDCASE.
*   ######
    CASE ls_fdcat-fieldname.
      WHEN 'PARTYP' OR
           'PARMES' OR
           'FILTYP' OR
           'FILMES'.
        ls_fdcat-no_out = 'X'.
    ENDCASE.
*   ######
    CASE ls_fdcat-fieldname.
      WHEN 'TOTAX'.
        ls_fdcat-do_sum = 'X'.
    ENDCASE.
    CASE ls_fdcat-fieldname.
      WHEN 'TABNAME'.
        ls_fdcat-edit = 'X'.
    ENDCASE.
***    CASE LS_FDCAT-FIELDNAME.
***      WHEN 'TABNAME'.
***        LS_FDCAT-EDIT = 'X'.
***    ENDCASE.
**   ########
*    CASE LS_FDCAT-FIELDNAME.
*      WHEN 'MARK'.
*        LS_FDCAT-CHECKBOX = 'X'.
*    ENDCASE.
**   KEY####
*    CASE LS_FDCAT-FIELDNAME.
*      WHEN 'MARK'.
*        LS_FDCAT-EMPHASIZE = 'C000'.
*      WHEN 'IMCOD' OR
*           'IMRAN'.
*        LS_FDCAT-EMPHASIZE = 'C410'.
*    ENDCASE.

*   #####
    CASE ls_fdcat-fieldname.
      WHEN 'RETMSG'.
        ls_fdcat-scrtext_l    = 'AOBJ Msg type'.
        ls_fdcat-scrtext_m    = 'AOBJ Msg type'.
        ls_fdcat-scrtext_s    = 'AOBJ Msg type'.
        ls_fdcat-coltext      = 'AOBJ Msg type'.
      WHEN 'MESSAGE'.
        ls_fdcat-scrtext_l    = 'AOBJ Message'.
        ls_fdcat-scrtext_m    = 'AOBJ Message'.
        ls_fdcat-scrtext_s    = 'AOBJ Message'.
        ls_fdcat-coltext      = 'AOBJ Message'.
      WHEN 'CRETYP'.
        ls_fdcat-scrtext_l    = 'Program Msg type'.
        ls_fdcat-scrtext_m    = 'Program Msg type'.
        ls_fdcat-scrtext_s    = 'Program Msg type'.
        ls_fdcat-coltext      = 'Program Msg type'.
      WHEN 'CREMES'.
        ls_fdcat-scrtext_l    = 'Program Message'.
        ls_fdcat-scrtext_m    = 'Program Message'.
        ls_fdcat-scrtext_s    = 'Program Message'.
        ls_fdcat-coltext      = 'Program Message'.
      WHEN 'PARTYP'.
        ls_fdcat-scrtext_l    = 'Para Msg type'.
        ls_fdcat-scrtext_m    = 'Para Msg type'.
        ls_fdcat-scrtext_s    = 'Para Msg type'.
        ls_fdcat-coltext      = 'Para Msg type'.
      WHEN 'PARMES'.
        ls_fdcat-scrtext_l    = 'Para Message'.
        ls_fdcat-scrtext_m    = 'Para Message'.
        ls_fdcat-scrtext_s    = 'Para Message'.
        ls_fdcat-coltext      = 'Para Message'.
      WHEN 'VBELN'.
        ls_fdcat-scrtext_l    = '######'.
        ls_fdcat-scrtext_m    = '######'.
        ls_fdcat-scrtext_s    = '######'.
        ls_fdcat-coltext      = '######'.
    ENDCASE.


    MODIFY gt_fdcat1 FROM ls_fdcat INDEX l_tabix.
    CLEAR ls_fdcat.

  ENDLOOP.
ENDFORM.                    " ALV_CL_FIELDCATALOG_MERGE1
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SET_FOR_FIRST_DISPLAY1
*&---------------------------------------------------------------------*
FORM alv_cl_set_for_first_display1.

  CALL METHOD g_alv_grid1->set_table_for_first_display
    EXPORTING
*     I_STRUCTURE_NAME     = ' '
      is_layout            = gs_layout1
      is_variant           = gs_variant1
      i_save               = 'A'
      it_toolbar_excluding = gt_fcode1
    CHANGING
      it_fieldcatalog      = gt_fdcat1[]
      it_sort              = gt_sort1
      it_outtab            = gt_list[].

ENDFORM.                    " ALV_CL_SET_FOR_FIRST_DISPLAY1

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
FORM alv_cl_refresh_table_display
        USING p_alv_grid TYPE REF TO cl_gui_alv_grid
              p_rec_stable TYPE lvc_s_stbl.

  p_rec_stable-row = 'X'.
  p_rec_stable-col = 'X'.
  CALL METHOD p_alv_grid->refresh_table_display
    EXPORTING
      is_stable = p_rec_stable.
ENDFORM.                    " ALV_CL_REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CELL_DOUBLE_CLICK1
*&---------------------------------------------------------------------*
FORM alv_cl_cell_double_click1
                           USING
                              p_row TYPE lvc_s_row
                              p_column TYPE lvc_s_col.

*  DATA LT_LINE TYPE REF TO DATA.
*  FIELD-SYMBOLS: <GA_LIST> TYPE ANY,
*                 <GA_LIST_TMP> TYPE ANY.
*
**  CREATE DATA LT_LINE LIKE LINE OF <GT_LIST>.
**  ASSIGN LT_LINE->* TO <GA_LIST>.
*
*
*  CASE P_COLUMN-FIELDNAME.
*
*    WHEN 'BELNR_RLFE'.
*
*      READ TABLE GT_LIST INDEX P_ROW-INDEX.
*
*      IF SY-SUBRC EQ 0.
*
**        ASSIGN COMPONENT 'BELNR' OF STRUCTURE <GA_LIST> TO <GA_LIST_TMP>.
*        IF GT_LIST-BELNR_RLFE IS INITIAL.
*
*          MESSAGE S000 WITH
*            '####### ####.'.
*
*        ELSE.
*
*          SET PARAMETER ID 'BLN' FIELD GT_LIST-BELNR_RLFE.
*          SET PARAMETER ID 'BUK' FIELD C_BUKRS_1000.
*          SET PARAMETER ID 'GJR' FIELD GT_LIST-BAS_DT(4).
*
*          CALL TRANSACTION C_TCODE_FB03
*               AND SKIP FIRST SCREEN.
*
*        ENDIF.
*      ENDIF.
*
*    WHEN 'BELNR_AR'.
*
*      READ TABLE GT_LIST INDEX P_ROW-INDEX.
*
*      IF SY-SUBRC EQ 0.
*
*        IF GT_LIST-BELNR_AR IS INITIAL.
*
*          MESSAGE S000 WITH
*            '######### ####.'.
*
*        ELSE.
*
*          SET PARAMETER ID 'BLN' FIELD GT_LIST-BELNR_AR.
*          SET PARAMETER ID 'BUK' FIELD C_BUKRS_1000.
*          SET PARAMETER ID 'GJR' FIELD GT_LIST-BUDAT.
*
*          CALL TRANSACTION C_TCODE_FB03
*               AND SKIP FIRST SCREEN.
*
*        ENDIF.
*      ENDIF.
*
*    WHEN 'CNRT_NO'.
*
*      READ TABLE GT_LIST INDEX P_ROW-INDEX.
*
*      IF GT_LIST-RNT_TYP EQ 1.
*
*        SET PARAMETER ID 'CNRT1' FIELD GT_LIST-CNRT_NO.
*        CALL TRANSACTION 'ZRFIAM0142' .
*      ELSE.
*        SET PARAMETER ID 'CNRT2' FIELD GT_LIST-CNRT_NO.
*        CALL TRANSACTION 'ZRFIAM0072' .
*      ENDIF.
**    WHEN 'ANLN1'.
**
**      READ TABLE <GT_LIST> INTO <GA_LIST> INDEX P_ROW-INDEX.
**
**      IF SY-SUBRC EQ 0.
**        ASSIGN COMPONENT 'ANLN1' OF STRUCTURE <GA_LIST> TO <GA_LIST_TMP>.
**
**        IF <GA_LIST_TMP> IS INITIAL.
**
**          MESSAGE S000 WITH
**            '##### ####.'.
**
**        ELSE.
**
**          SET PARAMETER ID 'AN1' FIELD <GA_LIST_TMP>.
**          SET PARAMETER ID 'BUK' FIELD C_BUKRS_1000.
**          CALL TRANSACTION C_TCODE_AS03
**               AND SKIP FIRST SCREEN.
**
**        ENDIF.
**      ENDIF.
*
*    WHEN 'BELNR_GRAMT'.
*
*      READ TABLE GT_LIST INDEX P_ROW-INDEX.
*
*      IF SY-SUBRC EQ 0.
*
**          ASSIGN COMPONENT 'BELNR' OF STRUCTURE <GA_LIST> TO <GA_LIST_TMP>.
*        IF GT_LIST-BELNR_GRAMT IS INITIAL.
*
*          MESSAGE S000 WITH
*            '######## ####.'.
*
*        ELSE.
*
*          SET PARAMETER ID 'BLN' FIELD GT_LIST-BELNR_GRAMT.
*          SET PARAMETER ID 'BUK' FIELD C_BUKRS_1000.
*          SET PARAMETER ID 'GJR' FIELD GT_LIST-BUDAT(4).
*
*          CALL TRANSACTION C_TCODE_FB03
*               AND SKIP FIRST SCREEN.
*
*        ENDIF.
*      ENDIF.
*
*    WHEN 'BELNR_R'.
*
*      READ TABLE GT_LIST INDEX P_ROW-INDEX.
*
*      IF SY-SUBRC EQ 0.
*
**          ASSIGN COMPONENT 'BELNR' OF STRUCTURE <GA_LIST> TO <GA_LIST_TMP>.
*        IF GT_LIST-BELNR_R IS INITIAL.
*
*          MESSAGE S000 WITH
*            '########### ####.'.
*
*        ELSE.
*
*          SET PARAMETER ID 'BLN' FIELD GT_LIST-BELNR_R.
*          SET PARAMETER ID 'BUK' FIELD C_BUKRS_1000.
*          SET PARAMETER ID 'GJR' FIELD GT_LIST-GJAHR_R.
*
*          CALL TRANSACTION C_TCODE_FB03
*               AND SKIP FIRST SCREEN.
*
*        ENDIF.
*      ENDIF.
*
*  ENDCASE.

ENDFORM.                    " ALV_CL_CELL_DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZBDCTABLE
*&---------------------------------------------------------------------*
FORM select_zbdctable
                  TABLES pt_zbdctable STRUCTURE zhacs0010.

  SELECT
    tabname
    retmsg
    message
    partyp
    parmes
    cretyp
    cremes
    filtyp
    filmes
    FROM zbdctable
    INTO CORRESPONDING FIELDS OF TABLE pt_zbdctable
    WHERE ( retmsg NE 'S'
      OR cretyp NE 'S' ).


ENDFORM.                    " SELECT_ZBDCTABLE
*&---------------------------------------------------------------------*
*&      Form  MOVE_DATA_TO_LIST
*&---------------------------------------------------------------------*
FORM move_data_to_list
                    TABLES pt_zbdctable STRUCTURE zhacs0010.

  LOOP AT pt_zbdctable.
    MOVE-CORRESPONDING pt_zbdctable TO gt_list.
    APPEND gt_list.
    CLEAR gt_list.
  ENDLOOP.

ENDFORM.                    " MOVE_DATA_TO_LIST
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM call_transaction .

  DATA  : lt_rows TYPE lvc_t_row.
  DATA  : ls_rows TYPE lvc_s_row.

  CALL METHOD g_alv_grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  SORT lt_rows DESCENDING BY index.

  IF lt_rows[] IS INITIAL.
    MESSAGE s032.
    EXIT.
  ENDIF.

  LOOP AT lt_rows INTO ls_rows .

    READ TABLE gt_list INDEX ls_rows-index.

    PERFORM create_aobj
                    USING gt_list-tabname
                          ls_rows-index.

  ENDLOOP.
ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  CREATE_AOBJ
*&---------------------------------------------------------------------*
FORM create_aobj
              USING p_tabname
                    p_index.

  DATA:
    lt_bdcdata LIKE bdcdata OCCURS 10,
    lt_msgdata LIKE bdcmsgcoll OCCURS 1.
  DATA: l_tabname TYPE tabname.
  DATA: l_tabname_r TYPE tabname.
  DATA: l_ddtext  LIKE dd02t-ddtext.
***  DATA: L_LEN TYPE I.

  PERFORM select_dd02t
                  USING p_tabname
                  CHANGING l_ddtext.
***  L_LEN = CHARLEN( P_TABNAME ).
  l_tabname = p_tabname(10).
  CONCATENATE l_tabname '_R' INTO l_tabname_r.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPL0ARC'          '0100',
    ' '   'BDC_OKCODE'        '=NEWL'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPL0ARC'          '0101',
    ' '   'V_ARC_OBJ-OBJECT'  l_tabname,    "Object Name
    ' '   'V_ARC_OBJ-OBJTEXT' l_ddtext(50), "Text
    ' '   'V_ARC_OBJ-APPLIC'  'CA',         "Aplication Area
    ' '   'BDC_OKCODE'       '=ATAB'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLSVCM'          '0100',
    ' '   'BDC_CURSOR'        'VIMDYNFLDS-DYN_LINE(02)',
    ' '   'BDC_OKCODE'        '=DETA'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPL0ARC'          '0400',
    ' '   'BDC_OKCODE'       '=NEWL'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPL0ARC'          '0400',
    ' '   'V_ARC_DEF-SEQUENCE(01)'  '0',
    ' '   'V_ARC_DEF-SON(01)'  l_tabname,    "Object Name,
    ' '   'BDC_OKCODE'       '=ATAB'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLSVCM'          '0100',
    ' '   'BDC_CURSOR'        'VIMDYNFLDS-DYN_LINE(05)',
    ' '   'BDC_OKCODE'        '=DETA'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPL0ARC'          '0500',
    ' '   'BDC_OKCODE'        '=NEWL'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPL0ARC'          '0501',
    ' '   'V_ARC_USR-OBJECT'  l_tabname,    "Object Name,
    ' '   'V_ARC_USR-FILENAME' 'ZARCHIVE_FILENAME', "Logical File Path
    ' '   'V_ARC_USR-ARCH_SIZE' '100',       "File Size
*    ' '   'CREP'                'P1',      "Repository
    ' '   'BDC_OKCODE'        '=ATAB'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLSVCM'          '0100',
    ' '   'BDC_CURSOR'        'VIMDYNFLDS-DYN_LINE(01)',
    ' '   'BDC_OKCODE'        '=P+'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLSVCM'          '0100',
    ' '   'BDC_CURSOR'        'VIMDYNFLDS-DYN_LINE(03)',
    ' '   'BDC_OKCODE'        '=DETA'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPL0ARC'          '0850',
    ' '   'BDC_OKCODE'        '=NEWL'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPL0ARC'          '0850',
    ' '   'V_ARC_RPRG-READ_PRG(01)'  l_tabname_r,
    ' '   'BDC_OKCODE'        '=SAVE'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLSTRD'          '0100',
    ' '   'KO007-L_DEVCLASS'  'ZHAC',
*    ' '   'KO007-L_AUTHOR'    sy-uname,
    ' '   'BDC_OKCODE'        '=ADD'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
   'X'   'SAPLSTRD'               '0300',
   ' '   'BDC_OKCODE'             '=LOCK'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
   'X'   'SAPLSTRD'               '0300',
   ' '   'BDC_OKCODE'             '=LOCK'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
   'X'   'SAPL0ARC'               '0850',
   ' '   'BDC_OKCODE'             '=ENDE'.


  g_bdcmode = 'E'.
* CALL TRANSACTION
  PERFORM call_transaction_for_bdc
                                TABLES
                                  lt_bdcdata
                                  lt_msgdata
                                USING
                                  c_tcode_aobj.

* TRANSACTION RESULT
  PERFORM result_transaction_for_acr
                                 TABLES
                                   lt_msgdata
                                 USING
                                   p_tabname.

ENDFORM.                    " CREATE_AOBJ
*---------------------------------------------------------------------*
*       FORM APPEND_BDC_DATA
*---------------------------------------------------------------------*
FORM append_bdc_data
                  TABLES
                    pt_bdcdata STRUCTURE bdcdata
                  USING
                    p_dynbegin LIKE bdcdata-dynbegin
                    p_name
                    p_value.
  IF p_dynbegin = 'X'.
    pt_bdcdata-program  = p_name.
    pt_bdcdata-dynpro   = p_value.
    pt_bdcdata-dynbegin = p_dynbegin.

    APPEND pt_bdcdata.
    CLEAR pt_bdcdata.
  ELSE.
    pt_bdcdata-fnam = p_name.
    pt_bdcdata-fval = p_value.

    APPEND pt_bdcdata.
    CLEAR pt_bdcdata.
  ENDIF.
ENDFORM.                    " APPEND_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_FOR_BDC
*&---------------------------------------------------------------------*
FORM call_transaction_for_bdc
                          TABLES
                            pt_bdcdata
                            pt_msgdata
                          USING
                            p_tcode TYPE sy-tcode.
  DATA: ls_options LIKE ctu_params.
* OPTION
  ls_options-dismode = g_bdcmode.
  ls_options-updmode = 'S'.
  ls_options-defsize = 'X'.
  ls_options-racommit = 'X'.

  CALL TRANSACTION p_tcode USING    pt_bdcdata
                           OPTIONS  FROM ls_options
                           MESSAGES INTO pt_msgdata.

ENDFORM.                    " CALL_TRANSACTION_FOR_BDC
*&---------------------------------------------------------------------*
*&      Form  RESULT_TRANSACTION_FOR_ACR
*&---------------------------------------------------------------------*
FORM result_transaction_for_acr
                            TABLES
                              pt_msgdata STRUCTURE bdcmsgcoll
                            USING
                              p_tabname.

  DATA l_return LIKE zbdctable-retmsg.
  DATA l_retmsg LIKE zbdctable-message.

* ##### ##
  LOOP AT pt_msgdata
    WHERE
      msgtyp  EQ c_msgty_s AND
      msgspra EQ sy-langu  AND
      msgid   EQ 'SV'      AND
      ( msgnr EQ '004' OR
        msgnr EQ '018' ).

    l_return = c_msgty_s.  "##
    l_retmsg = 'Succeed'.
    CLEAR pt_msgdata.
    EXIT.
  ENDLOOP.

* ###### ###
  IF sy-subrc NE 0.

*   ##### ##
    LOOP AT pt_msgdata
      WHERE
        msgtyp  EQ c_msgty_e.  "####

      l_return = c_msgty_e.  "##

      PERFORM message_text_build
                              USING
                                pt_msgdata-msgid
                                pt_msgdata-msgnr
                                pt_msgdata-msgtyp
                                pt_msgdata-msgv1
                                pt_msgdata-msgv2
                                pt_msgdata-msgv3
                                pt_msgdata-msgv4
                              CHANGING
                                l_retmsg.
      CLEAR pt_msgdata.
      EXIT.
    ENDLOOP.

***    IF SY-SUBRC NE 0.
****    ###### ##
***
***      LOOP AT PT_MSGDATA
***        WHERE
***          MSGSPRA EQ SY-LANGU  AND
***          MSGID   EQ '00'      AND
***          ( MSGNR EQ '344' OR
***            MSGNR EQ '346' OR
***            MSGNR EQ '347' OR
***            MSGNR EQ '348' OR
***            MSGNR EQ '349' ).
***
***        PS_RETURN-RETCD = C_RETCD_E.
***
***        PERFORM MESSAGE_TEXT_BUILD
***                                USING
***                                  PT_MSGDATA-MSGID
***                                  PT_MSGDATA-MSGNR
***                                  PT_MSGDATA-MSGTYP
***                                  PT_MSGDATA-MSGV1
***                                  PT_MSGDATA-MSGV2
***                                  PT_MSGDATA-MSGV3
***                                  PT_MSGDATA-MSGV4
***                                CHANGING
***                                  PS_RETURN-RETMG.
***        CLEAR PT_MSGDATA.
***        EXIT.
***      ENDLOOP.
***
***      IF SY-SUBRC NE 0.
****     ########## ###
***
***        PS_RETURN-RETCD = C_RETCD_E.  "##
***        PS_RETURN-RETMG = C_UNKNOWN_ERROR.
***
***      ENDIF.
***    ENDIF.
  ENDIF.

  IF l_return IS NOT INITIAL.

    UPDATE zbdctable
       SET retmsg = l_return
           message = l_retmsg
     WHERE tabname = p_tabname.

    IF sy-subrc EQ 0.
      COMMIT WORK.
      gt_list-retmsg = l_return.
      gt_list-message = l_retmsg.
      MODIFY gt_list TRANSPORTING retmsg message WHERE tabname = p_tabname .
    ENDIF.
    CLEAR l_return.
  ENDIF.

ENDFORM.                    " RESULT_TRANSACTION_FOR_ACR
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
FORM message_text_build
                    USING
                      p_msgid
                      p_msgno
                      p_msgty
                      p_msgv1
                      p_msgv2
                      p_msgv3
                      p_msgv4
                    CHANGING
                      p_message.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = p_msgid
      msgnr               = p_msgno
      msgv1               = p_msgv1
      msgv2               = p_msgv2
      msgv3               = p_msgv3
      msgv4               = p_msgv4
    IMPORTING
      message_text_output = p_message.

ENDFORM.                    " MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
*&      Form  CREATE_PROGRAM
*&---------------------------------------------------------------------*
FORM create_program .

  DATA  : lt_rows TYPE lvc_t_row.
  DATA  : ls_rows TYPE lvc_s_row.

  CALL METHOD g_alv_grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  SORT lt_rows DESCENDING BY index.

  IF lt_rows[] IS INITIAL.
    MESSAGE s032.
    EXIT.
  ENDIF.

  LOOP AT lt_rows INTO ls_rows .

    READ TABLE gt_list INDEX ls_rows-index.

    IF gt_list-retmsg NE c_msgty_s.
      MESSAGE e000.
    ENDIF.
***    IF GT_LIST-PARTYP NE C_MSGTY_S.
***       MESSAGE E000.
***    ENDIF.

    PERFORM create_cbo_program
                          USING gt_list-tabname
                                ls_rows-index.
  ENDLOOP.

ENDFORM.                    " CREATE_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  CREATE_CBO_PROGRAM
*&---------------------------------------------------------------------*
FORM create_cbo_program
                    USING
                      p_tabname
                      p_index.

  DATA:
    lt_bdcdata LIKE bdcdata OCCURS 10,
    lt_msgdata LIKE bdcmsgcoll OCCURS 1.
  DATA: l_obj(30),
        l_tab(30),
        l_top(30),
        l_pro(30),
        l_del(30),
        l_rld(30),
        l_rea(30).

  PERFORM create_concatenate:
                        USING p_tabname '_T'
                        CHANGING l_top,
                        USING p_tabname '_W'
                        CHANGING l_pro,
                        USING p_tabname '_D'
                        CHANGING l_del,
                        USING p_tabname '_L'
                        CHANGING l_rld,
                        USING p_tabname '_R'
                        CHANGING l_rea.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archiving Object Create
*    'X'   'YHACR00800_1'     '1000',  "by AKM
    'X'   c_tcode_zhacr00800 '1000',
    ' '   'P_OBJ'            p_tabname(10),
    ' '   'P_TAB'            p_tabname,
    ' '   'P_TOP'            l_top,
    ' '   'P_PRO'            l_pro,
    ' '   'P_DEL'            l_del,
    ' '   'P_RLD'            l_rld,
    ' '   'P_REA'            l_rea,
    ' '   'BDC_OKCODE'     '=ONLI'.


***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****   Archiving Object Create
***    'X'   'ZHACR00800'       '1000',
***    ' '   'P_OBJ'            P_TABNAME,
***    ' '   'P_TAB'            P_TABNAME,
***    ' '   'P_TOP'            L_TOP,
***    ' '   'P_PRO'            L_PRO,
***    ' '   'P_DEL'            L_DEL,
***    ' '   'P_RLD'            L_RLD,
***    ' '   'P_REA'            L_REA,
***    ' '   'BDC_OKCODE'     '=ONLI'.

  g_bdcmode = 'E'.

* CALL TRANSACTION
  PERFORM call_transaction_for_bdc
                                TABLES
                                  lt_bdcdata
                                  lt_msgdata
                                USING
                                  c_tcode_zhacr00800.

* TRANSACTION RESULT
  PERFORM result_transaction_for_cre
                                 TABLES
                                   lt_msgdata
                                 USING
                                   p_tabname.


ENDFORM.                    " CREATE_CBO_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONCATENATE
*&---------------------------------------------------------------------*
FORM create_concatenate
                    USING p_tabname
                          p_value
                    CHANGING p_ret.

  CONCATENATE
        p_tabname
        p_value INTO p_ret.

ENDFORM.                    " CREATE_CONCATENATE
*&---------------------------------------------------------------------*
*&      Form  RESULT_TRANSACTION_FOR_ACR
*&---------------------------------------------------------------------*
FORM result_transaction_for_cre
                            TABLES
                              pt_msgdata STRUCTURE bdcmsgcoll
                            USING
                              p_tabname.

  DATA l_return LIKE zbdctable-retmsg.
  DATA l_retmsg LIKE zbdctable-message.

* ##### ##
  LOOP AT pt_msgdata
    WHERE
      msgtyp  EQ c_msgty_s AND
      msgspra EQ sy-langu  AND
      msgid   EQ 'EU'  AND
      msgnr EQ '000'.

    l_return = c_msgty_s.  "##
    l_retmsg = 'Succeed'.
    CLEAR pt_msgdata.
    EXIT.
  ENDLOOP.

* ###### ###
  IF sy-subrc NE 0.

*   ##### ##
    LOOP AT pt_msgdata
      WHERE
        msgtyp  EQ c_msgty_e.  "####

      l_return = c_msgty_e.  "##

      PERFORM message_text_build
                              USING
                                pt_msgdata-msgid
                                pt_msgdata-msgnr
                                pt_msgdata-msgtyp
                                pt_msgdata-msgv1
                                pt_msgdata-msgv2
                                pt_msgdata-msgv3
                                pt_msgdata-msgv4
                              CHANGING
                                l_retmsg.
      CLEAR pt_msgdata.
      EXIT.
    ENDLOOP.

***    IF SY-SUBRC NE 0.
****    ###### ##
***
***      LOOP AT PT_MSGDATA
***        WHERE
***          MSGSPRA EQ SY-LANGU  AND
***          MSGID   EQ '00'      AND
***          ( MSGNR EQ '344' OR
***            MSGNR EQ '346' OR
***            MSGNR EQ '347' OR
***            MSGNR EQ '348' OR
***            MSGNR EQ '349' ).
***
***        PS_RETURN-RETCD = C_RETCD_E.
***
***        PERFORM MESSAGE_TEXT_BUILD
***                                USING
***                                  PT_MSGDATA-MSGID
***                                  PT_MSGDATA-MSGNR
***                                  PT_MSGDATA-MSGTYP
***                                  PT_MSGDATA-MSGV1
***                                  PT_MSGDATA-MSGV2
***                                  PT_MSGDATA-MSGV3
***                                  PT_MSGDATA-MSGV4
***                                CHANGING
***                                  PS_RETURN-RETMG.
***        CLEAR PT_MSGDATA.
***        EXIT.
***      ENDLOOP.
***
***      IF SY-SUBRC NE 0.
****     ########## ###
***
***        PS_RETURN-RETCD = C_RETCD_E.  "##
***        PS_RETURN-RETMG = C_UNKNOWN_ERROR.
***
***      ENDIF.
***    ENDIF.
  ENDIF.

  IF l_return IS NOT INITIAL.

    UPDATE zbdctable
       SET cretyp = l_return
           cremes = l_retmsg
     WHERE tabname = p_tabname.

    IF sy-subrc EQ 0.
      COMMIT WORK.

      gt_list-cretyp = l_return.
      gt_list-cremes = l_retmsg.

      MODIFY gt_list TRANSPORTING cretyp cremes WHERE tabname = p_tabname .
    ENDIF.
    CLEAR l_return.
  ENDIF.

ENDFORM.                    " RESULT_TRANSACTION_FOR_CRE
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZHACT0010
*&---------------------------------------------------------------------*
FORM select_zhact0010  USING p_tabname.

  DATA l_object LIKE zhact0010-object.

  SELECT SINGLE
    object
    FROM zhact0010
    INTO l_object
   WHERE object EQ p_tabname.

  IF sy-subrc EQ 0.
    PERFORM update_zbdctable
                         USING p_tabname.
  ELSE.
    PERFORM create_zhacv0010
                         USING p_tabname.
  ENDIF.

ENDFORM.                    " SELECT_ZHACT0010
*&---------------------------------------------------------------------*
*&      Form  CREATE_ZHACV0010
*&---------------------------------------------------------------------*
FORM create_zhacv0010
                  USING p_tabname.

  DATA:
    lt_bdcdata LIKE bdcdata OCCURS 10,
    lt_msgdata LIKE bdcmsgcoll OCCURS 1.
  DATA: l_tabname TYPE tabname.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLSVIX'       '0210',
    ' '   'BDC_CURSOR'     'MARK_CHECKBOX(01)',
    ' '   'BDC_OKCODE'     '=AWAL'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLSVIX'           '0210',
    ' '   'BDC_CURSOR'   'MARK_CHECKBOX(01)',
    ' '   'BDC_OKCODE'         '=OKAY'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLSVIX'           '0100',
    ' '   'BDC_CURSOR'   'D0100_FIELD_TAB-LOWER_LIMIT(01)',
    ' '   'BDC_OKCODE'         '=OKAY'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLZBACG02'         '0001',
    ' '   'BDC_CURSOR'          'ZHACV0010-OBJECT(01)',
    ' '   'BDC_OKCODE'          '=AEND'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLZBACG02'         '0001',
    ' '   'BDC_CURSOR'          'ZHACV0010-LTEXT(01)',
    ' '   'BDC_OKCODE'          '=NEWL'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLZBACG02'         '0002',
    ' '   'BDC_CURSOR'          'ZHACV0010-SERIAL',
    ' '   'ZITTACV02-OBJECT'    p_tabname,
    ' '   'ZITTACV02-LGGBN'     'P',
    ' '   'ZITTACV02-SERIAL'    '1',
    ' '   'BDC_OKCODE'          '=NEXT'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
    'X'   'SAPLZBACG02'         '0002',
    ' '   'BDC_CURSOR'          'ZHACV0010-SERIAL',
    ' '   'ZITTACV02-OBJECT'    p_tabname,
    ' '   'ZITTACV02-LGGBN'     'S',
    ' '   'ZITTACV02-SERIAL'    '1',
    ' '   'BDC_OKCODE'          '=NEXT'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
   'X'   'SAPLZBACG02'         '0002',
   ' '   'BDC_CURSOR'          'ZHACV0010-SERIAL',
   ' '   'ZITTACV02-OBJECT'    p_tabname,
   ' '   'ZITTACV02-LGGBN'     'S',
   ' '   'ZITTACV02-SERIAL'    '2',
   ' '   'BDC_OKCODE'          '/00'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
  'X'   'SAPLZBACG02'         '0002',
  ' '   'BDC_CURSOR'          'ZHACV0010-OBJECT',
***    ' '   'ZITTACV02-OBJECT'    P_TABNAME,
***    ' '   'ZITTACV02-LGGBN'     'S',
***    ' '   'ZITTACV02-SERIAL'    '2',
  ' '   'BDC_OKCODE'          '=SAVE'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
  'X'   'SAPLZBACG02'         '0002',
  ' '   'BDC_CURSOR'          'ZHACV0010-LTEXT',
  ' '   'BDC_OKCODE'          '=UEBE'.

* CALL TRANSACTION
  PERFORM call_transaction_for_bdc_e
                                TABLES
                                  lt_bdcdata
                                  lt_msgdata
                                USING
                                  c_tcode_zhacv0010.

* TRANSACTION RESULT
  PERFORM result_transaction_for_par
                                 TABLES
                                   lt_msgdata
                                 USING
                                   p_tabname.

ENDFORM.                    " CREATE_ZHACV0010
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_FOR_BDC
*&---------------------------------------------------------------------*
FORM call_transaction_for_bdc_e
                            TABLES
                              pt_bdcdata
                              pt_msgdata
                            USING
                              p_tcode TYPE sy-tcode.
  DATA: ls_options LIKE ctu_params.
* OPTION
  ls_options-dismode = 'E'.
  ls_options-updmode = 'S'.
  ls_options-defsize = 'X'.
  ls_options-racommit = 'X'.

  CALL TRANSACTION p_tcode USING    pt_bdcdata
                           OPTIONS  FROM ls_options
                           MESSAGES INTO pt_msgdata.

ENDFORM.                    " CALL_TRANSACTION_FOR_BDC
*&---------------------------------------------------------------------*
*&      Form  CREATE_PARAMETER
*&---------------------------------------------------------------------*
FORM create_parameter .

  DATA  : lt_rows TYPE lvc_t_row.
  DATA  : ls_rows TYPE lvc_s_row.

  CALL METHOD g_alv_grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  SORT lt_rows DESCENDING BY index.

  IF lt_rows[] IS INITIAL.
    MESSAGE s032.
    EXIT.
  ENDIF.

  LOOP AT lt_rows INTO ls_rows .

    READ TABLE gt_list INDEX ls_rows-index.

    PERFORM select_zhact0010
                          USING gt_list-tabname.

  ENDLOOP.

ENDFORM.                    " CREATE_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  RESULT_TRANSACTION_FOR_ACR
*&---------------------------------------------------------------------*
FORM result_transaction_for_par
                            TABLES
                              pt_msgdata STRUCTURE bdcmsgcoll
                            USING
                              p_tabname.

  DATA l_return LIKE zbdctable-retmsg.
  DATA l_retmsg LIKE zbdctable-message.

* ##### ##
  LOOP AT pt_msgdata
    WHERE
      msgtyp  EQ c_msgty_s AND
      msgspra EQ sy-langu  AND
      msgid   EQ 'SV'  AND
      msgnr EQ '018'.

    l_return = c_msgty_s.  "##
    l_retmsg = 'Succeed'.
    CLEAR pt_msgdata.
    EXIT.
  ENDLOOP.

* ###### ###
  IF sy-subrc NE 0.

*   ##### ##
    LOOP AT pt_msgdata
      WHERE
        ( msgtyp  EQ c_msgty_e OR   "####
          msgtyp  EQ c_msgty_i ).
      l_return = c_msgty_e.  "##

      PERFORM message_text_build
                              USING
                                pt_msgdata-msgid
                                pt_msgdata-msgnr
                                pt_msgdata-msgtyp
                                pt_msgdata-msgv1
                                pt_msgdata-msgv2
                                pt_msgdata-msgv3
                                pt_msgdata-msgv4
                              CHANGING
                                l_retmsg.
      CLEAR pt_msgdata.
      EXIT.
    ENDLOOP.

***    IF SY-SUBRC NE 0.
****    ###### ##
***
***      LOOP AT PT_MSGDATA
***        WHERE
***          MSGSPRA EQ SY-LANGU  AND
***          MSGID   EQ '00'      AND
***          ( MSGNR EQ '344' OR
***            MSGNR EQ '346' OR
***            MSGNR EQ '347' OR
***            MSGNR EQ '348' OR
***            MSGNR EQ '349' ).
***
***        PS_RETURN-RETCD = C_RETCD_E.
***
***        PERFORM MESSAGE_TEXT_BUILD
***                                USING
***                                  PT_MSGDATA-MSGID
***                                  PT_MSGDATA-MSGNR
***                                  PT_MSGDATA-MSGTYP
***                                  PT_MSGDATA-MSGV1
***                                  PT_MSGDATA-MSGV2
***                                  PT_MSGDATA-MSGV3
***                                  PT_MSGDATA-MSGV4
***                                CHANGING
***                                  PS_RETURN-RETMG.
***        CLEAR PT_MSGDATA.
***        EXIT.
***      ENDLOOP.
***
***      IF SY-SUBRC NE 0.
****     ########## ###
***
***        PS_RETURN-RETCD = C_RETCD_E.  "##
***        PS_RETURN-RETMG = C_UNKNOWN_ERROR.
***
***      ENDIF.
***    ENDIF.
  ENDIF.

  IF l_return IS NOT INITIAL.

    UPDATE zbdctable
       SET partyp = l_return
           parmes = l_retmsg
     WHERE tabname = p_tabname.

    IF sy-subrc EQ 0.
      COMMIT WORK.

      gt_list-partyp = l_return.
      gt_list-parmes = l_retmsg.

      MODIFY gt_list TRANSPORTING partyp parmes WHERE tabname = p_tabname .
    ENDIF.
    CLEAR l_return.
  ENDIF.

ENDFORM.                    " RESULT_TRANSACTION_FOR_PAR
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZBDCTABLE
*&---------------------------------------------------------------------*
FORM update_zbdctable
                  USING p_tabname.

  UPDATE zbdctable
     SET partyp = c_msgty_s
   WHERE tabname = p_tabname.

  IF sy-subrc EQ 0.
    COMMIT WORK.

    gt_list-partyp = c_msgty_s.

    MODIFY gt_list TRANSPORTING partyp WHERE tabname = p_tabname .
  ENDIF.

ENDFORM.                    " UPDATE_ZBDCTABLE
*&---------------------------------------------------------------------*
*&      Form  SELECT_DD02T
*&---------------------------------------------------------------------*
FORM select_dd02t  USING    p_tabname
                   CHANGING p_ddtext.

  DATA: l_ddtext  LIKE dd02t-ddtext.

  SELECT SINGLE
    ddtext
    FROM dd02t
    INTO l_ddtext
   WHERE tabname EQ p_tabname
     AND ddlanguage EQ sy-langu.

  IF sy-subrc EQ 0.
    p_ddtext = l_ddtext.
  ENDIF.

ENDFORM.                    " SELECT_DD02T
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATALOGS
*&---------------------------------------------------------------------*
FORM create_field_catalogs .

  DATA  : lt_rows TYPE lvc_t_row.
  DATA  : ls_rows TYPE lvc_s_row.

  CALL METHOD g_alv_grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  SORT lt_rows DESCENDING BY index.

  IF lt_rows[] IS INITIAL.
    MESSAGE s032.
    EXIT.
  ENDIF.

  LOOP AT lt_rows INTO ls_rows .

    READ TABLE gt_list INDEX ls_rows-index.

    PERFORM create_bdc_field_catalogs
                                  USING gt_list-tabname.

  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATALOGS
*&---------------------------------------------------------------------*
*&      Form  CREATE_BDC_FIELD_CATALOGS
*&---------------------------------------------------------------------*
FORM create_bdc_field_catalogs
                            USING p_tabname.

  DATA:
     lt_bdcdata LIKE bdcdata OCCURS 10,
     lt_msgdata LIKE bdcmsgcoll OCCURS 1.
  DATA l_fieldcat TYPE tabname.
  DATA l_object TYPE tabname.
  DATA l_ddtext  LIKE dd02t-ddtext.
  DATA l_fldnr TYPE aind_fldnr.
  DATA l_last_flag.
  DATA l_arch_info TYPE aind_desc.
*** DATA : BEGIN OF LT_DD03L OCCURS 0,
***         TABNAME LIKE DD03L-TABNAME,
***         FIELDNAME LIKE DD03L-FIELDNAME,
***         POSITION LIKE DD03L-POSITION,
***         KEYFLAG LIKE DD03L-KEYFLAG,
***       END OF LT_DD03L.
  DATA : BEGIN OF lt_dd03l OCCURS 0,
          tabname LIKE zhact0020-tabname,
          fieldname LIKE zhact0020-fieldname,
          position LIKE zhact0020-serial,
          keyflag LIKE zhact0020-keyflag,
        END OF lt_dd03l.


  PERFORM select_dd02t
                    USING p_tabname
                    CHANGING l_ddtext.

***  PERFORM SELECT_DD03L
***                   TABLES LT_DD03L
***                   USING P_TABNAME.

  PERFORM select_zhact0020
                       TABLES lt_dd03l
                       USING p_tabname.

  SORT lt_dd03l BY position.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archive Information System: Central management
     'X'   'SAPLAINA'       '0100',
     ' '   'BDC_OKCODE'     '=CUST'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archive Retrieval Configurator
     'X'   'SAPLAS_ARC'     '1000',
     ' '   'BDC_OKCODE'     '=FCAT'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Change View "Field Catalogs": Overview
    'X'   'SAPLARIC'           '0150',
    ' '   'BDC_CURSOR'         'AIND_SV1S-ARCHINDEX(01)',
    ' '   'BDC_OKCODE'         '=NEWL'.

  IF p_tabname(1) EQ 'Z'.
    l_object = p_tabname(10).
    CONCATENATE l_object '_001' INTO l_fieldcat.
  ELSE.
    l_object = p_tabname(10).
    CONCATENATE 'Z' l_object '_001' INTO l_fieldcat.
  ENDIF.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   New Entries: Overview of Added Entries
    'X'   'SAPLARIC'                 '0150',
    ' '   'AIND_SV1S-ARCHINDEX(01)'  l_fieldcat,
    ' '   'AIND_SV1S-TEXT(01)'       l_ddtext,
    ' '   'AIND_SV1S-OBJECT(01)'     l_object,
    ' '   'AIND_SV1S-FILEFIELD(01)'  'K',
    ' '   'AIND_SV1S-OFFSFIELD(01)'  'D',
    ' '   'BDC_OKCODE'               '=ATAB'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLSVCM'          '0100',
    ' '   'BDC_CURSOR'        'VIMDYNFLDS-DYN_LINE(02)',
    ' '   'BDC_OKCODE'        '=DETA'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLARIC'           '0350',
    ' '   'BDC_CURSOR'         'VIMDYNFLDS-DYN_LINE(01)',
    ' '   'BDC_OKCODE'         '=NEWL'.

  LOOP AT lt_dd03l.

    AT LAST.
      l_last_flag = c_flag_x.
    ENDAT.

    ADD 1 TO l_fldnr.

    IF lt_dd03l-keyflag EQ c_flag_x.

      PERFORM append_bdc_data TABLES lt_bdcdata USING:
*       Change View "Field Selection":Overview
        'X'   'SAPLARIC'                 '0350',
        ' '   'AIND_SV3ES-FLDNR(01)'     l_fldnr,
        ' '   'AIND_SV3ES-FIELDNAME(01)' lt_dd03l-fieldname,
        ' '   'AIND_SV3ES-REFTAB(01)'    lt_dd03l-tabname,
        ' '   'AIND_SV3ES-REFFIELD(01)'  lt_dd03l-fieldname,
        ' '   'AIND_SV3ES-KEYFLAG(01)'   c_flag_x,
        ' '   'BDC_OKCODE'               '=NEXT'.

    ELSE.

      IF l_last_flag EQ c_flag_x.

        PERFORM append_bdc_data TABLES lt_bdcdata USING:
*         Change View "Field Selection":Overview
          'X'   'SAPLARIC'                 '0350',
          ' '   'AIND_SV3ES-FLDNR(01)'     l_fldnr,
          ' '   'AIND_SV3ES-FIELDNAME(01)' lt_dd03l-fieldname,
          ' '   'AIND_SV3ES-REFTAB(01)'    lt_dd03l-tabname,
          ' '   'AIND_SV3ES-REFFIELD(01)'  lt_dd03l-fieldname,
          ' '   'BDC_OKCODE'               '=SAVE'.

        CLEAR l_last_flag.

      ELSE.

        PERFORM append_bdc_data TABLES lt_bdcdata USING:
*         Change View "Field Selection":Overview
          'X'   'SAPLARIC'                 '0350',
          ' '   'AIND_SV3ES-FLDNR(01)'     l_fldnr,
          ' '   'AIND_SV3ES-FIELDNAME(01)' lt_dd03l-fieldname,
          ' '   'AIND_SV3ES-REFTAB(01)'    lt_dd03l-tabname,
          ' '   'AIND_SV3ES-REFFIELD(01)'  lt_dd03l-fieldname,
          ' '   'BDC_OKCODE'               '=NEXT'.

      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLARIC'                 '0350',
    ' '   'BDC_OKCODE'               '=ENDE'.


  IF p_tabname(1) EQ 'Z'.
    l_object = p_tabname(10).
    CONCATENATE l_object '_001' INTO l_arch_info.
  ELSE.
    l_object = p_tabname(10).
    CONCATENATE 'Z' l_object '_001' INTO l_arch_info.
  ENDIF.
  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archive Retrieval Configurator
    'X'   'SAPLAS_ARC'               '1000',
    ' '   'AIND_STR1-ARCHINDEX'      l_arch_info,
    ' '   'BDC_OKCODE'               '=NEWL'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archive Retrieval Configurator
    'X'   'SAPLAS_ARC'               '1010',
***    ' '   'AIND_STR1-ARCHINDEX'      L_ARCH_INFO,
    ' '   'AIND_STR1T-TEXT'          l_ddtext,
    ' '   'AIND_STR1-OBJECT'         l_object,
    ' '   'AIND_STR1-SKEY'           l_fieldcat,
    ' '   'BDC_OKCODE'               '=NEWL'.


***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
***
***    'X'   'SAPLAS_ARC'               '1000',
***    ' '   'BDC_OKCODE'               '/EENDE'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
***
***    'X'   'SAPLAINA'                 '0100',
***    ' '   'BDC_OKCODE'               '/E%EX'.

  g_bdcmode = 'E'.
* CALL TRANSACTION
  PERFORM call_transaction_for_bdc
                                TABLES
                                  lt_bdcdata
                                  lt_msgdata
                                USING
                                  c_tcode_sari.

* TRANSACTION RESULT
  PERFORM result_transaction_for_fica
                                 TABLES
                                   lt_msgdata
                                 USING
                                   p_tabname.

  REFRESH lt_dd03l.
  CLEAR: l_fieldcat,
         l_arch_info.

ENDFORM.                    " CREATE_BDC_FIELD_CATALOGS
*&---------------------------------------------------------------------*
*&      Form  SELECT_DD03L
*&---------------------------------------------------------------------*
FORM select_dd03l  TABLES   pt_dd03l
                   USING    p_tabname.

  SELECT
    tabname
    fieldname
    position
    keyflag
    FROM dd03l
    INTO CORRESPONDING FIELDS OF TABLE pt_dd03l
   WHERE tabname EQ p_tabname
***     AND KEYFLAG EQ C_FLAG_X
     AND position NE '0001'.

ENDFORM.                    " SELECT_DD03L
*&---------------------------------------------------------------------*
*&      Form  RESULT_TRANSACTION_FOR_FICA
*&---------------------------------------------------------------------*
FORM result_transaction_for_fica
                            TABLES
                              pt_msgdata STRUCTURE bdcmsgcoll
                            USING
                              p_tabname.

  DATA l_return LIKE zbdctable-retmsg.
  DATA l_retmsg LIKE zbdctable-message.

* ##### ##
  LOOP AT pt_msgdata
    WHERE
      msgtyp  EQ c_msgty_s AND
      msgspra EQ sy-langu  AND
      msgid   EQ 'SV'      AND
      ( msgnr EQ '004' OR
        msgnr EQ '018' ).

    l_return = c_msgty_s.  "##
    l_retmsg = 'Succeed'.
    CLEAR pt_msgdata.
    EXIT.
  ENDLOOP.

* ###### ###
  IF sy-subrc NE 0.

*   ##### ##
    LOOP AT pt_msgdata
      WHERE
        msgtyp  EQ c_msgty_e.  "####

      l_return = c_msgty_e.  "##

      PERFORM message_text_build
                              USING
                                pt_msgdata-msgid
                                pt_msgdata-msgnr
                                pt_msgdata-msgtyp
                                pt_msgdata-msgv1
                                pt_msgdata-msgv2
                                pt_msgdata-msgv3
                                pt_msgdata-msgv4
                              CHANGING
                                l_retmsg.
      CLEAR pt_msgdata.
      EXIT.
    ENDLOOP.

***    IF SY-SUBRC NE 0.
****    ###### ##
***
***      LOOP AT PT_MSGDATA
***        WHERE
***          MSGSPRA EQ SY-LANGU  AND
***          MSGID   EQ '00'      AND
***          ( MSGNR EQ '344' OR
***            MSGNR EQ '346' OR
***            MSGNR EQ '347' OR
***            MSGNR EQ '348' OR
***            MSGNR EQ '349' ).
***
***        PS_RETURN-RETCD = C_RETCD_E.
***
***        PERFORM MESSAGE_TEXT_BUILD
***                                USING
***                                  PT_MSGDATA-MSGID
***                                  PT_MSGDATA-MSGNR
***                                  PT_MSGDATA-MSGTYP
***                                  PT_MSGDATA-MSGV1
***                                  PT_MSGDATA-MSGV2
***                                  PT_MSGDATA-MSGV3
***                                  PT_MSGDATA-MSGV4
***                                CHANGING
***                                  PS_RETURN-RETMG.
***        CLEAR PT_MSGDATA.
***        EXIT.
***      ENDLOOP.
***
***      IF SY-SUBRC NE 0.
****     ########## ###
***
***        PS_RETURN-RETCD = C_RETCD_E.  "##
***        PS_RETURN-RETMG = C_UNKNOWN_ERROR.
***
***      ENDIF.
***    ENDIF.
  ENDIF.

  IF l_return IS NOT INITIAL.

    UPDATE zbdctable
       SET filtyp = l_return
           filmes = l_retmsg
     WHERE tabname = p_tabname.

    IF sy-subrc EQ 0.
      COMMIT WORK.
      gt_list-filtyp = l_return.
      gt_list-filmes = l_retmsg.
      MODIFY gt_list TRANSPORTING filtyp filmes WHERE tabname = p_tabname .
    ENDIF.
    CLEAR l_return.
  ENDIF.

ENDFORM.                    " RESULT_TRANSACTION_FOR_FICA
*&---------------------------------------------------------------------*
*&      Form  DEACTIVATE_INFOSTRUCTURE
*&---------------------------------------------------------------------*
FORM deactivate_infostructure .

  DATA  : lt_rows TYPE lvc_t_row.
  DATA  : ls_rows TYPE lvc_s_row.

  CALL METHOD g_alv_grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  SORT lt_rows DESCENDING BY index.

  IF lt_rows[] IS INITIAL.
    MESSAGE s032.
    EXIT.
  ENDIF.

  LOOP AT lt_rows INTO ls_rows .

    READ TABLE gt_list INDEX ls_rows-index.

    PERFORM deactivate_bdc_infostructure
                                    USING gt_list-tabname.

  ENDLOOP.

ENDFORM.                    " DEACTIVATE_INFOSTRUCTURE
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZHACT0020
*&---------------------------------------------------------------------*
FORM select_zhact0020
                TABLES pt_zhact0020
                USING p_tabname.

  SELECT
    tabname
    fieldname
    serial
    keyflag
    FROM zhact0020
    INTO CORRESPONDING FIELDS OF TABLE pt_zhact0020
   WHERE tabname EQ p_tabname.
***     AND KEYFLAG EQ C_FLAG_X
***     AND POSITION NE '0001'.

ENDFORM.                    " SELECT_ZHACT0020
*&---------------------------------------------------------------------*
*&      Form  DEACTIVATE_BDC_INFOSTRUCTURE
*&---------------------------------------------------------------------*
FORM deactivate_bdc_infostructure
                              USING p_tabname.

  DATA:
    lt_bdcdata LIKE bdcdata OCCURS 10,
    lt_msgdata LIKE bdcmsgcoll OCCURS 1.
  DATA l_object TYPE tabname.
  DATA l_arch_info TYPE aind_desc.


  IF p_tabname(1) EQ 'Z'.
    l_object = p_tabname(10).
    CONCATENATE l_object '_001' INTO l_arch_info.
  ELSE.
    l_object = p_tabname(10).
    CONCATENATE 'Z' l_object '_001' INTO l_arch_info.
  ENDIF.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archive Information System: Central management
    'X'   'SAPLAINA'       '0100',
    ' '   'BDC_OKCODE'     '=CUST'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLAS_ARC'           '1000',
    ' '   'BDC_CURSOR'           'AIND_STR1-ARCHINDEX',
    ' '   'AIND_STR1-ARCHINDEX'  l_arch_info,
    ' '   'BDC_OKCODE'           '=DACT'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPMSSY0'             '120',
    ' '   'BDC_OKCODE'           '=&ONT'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLAS_ARC'           '1000',
    ' '   'BDC_CURSOR'           'AIND_STR1-ARCHINDEX',
    ' '   'AIND_STR1-ARCHINDEX'  l_arch_info,
    ' '   'BDC_OKCODE'           '=TDEL'.

***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
***
***    'X'   'SAPLSPO1'             '0500',
***    ' '   'BDC_OKCODE'           '=OPT1'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLAS_ARC'           '1000',
    ' '   'BDC_CURSOR'           'AIND_STR1-ARCHINDEX',
    ' '   'AIND_STR1-ARCHINDEX'  l_arch_info,
    ' '   'BDC_OKCODE'           '=PDEL'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLAS_ARC'           '1000',
    ' '   'BDC_OKCODE'           '/EENDE'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLAINA'             '0100',
    ' '   'BDC_OKCODE'           '/E%EX'.

* CALL TRANSACTION
  PERFORM call_transaction_for_bdc
                                TABLES
                                  lt_bdcdata
                                  lt_msgdata
                                USING
                                  c_tcode_sari.

  CLEAR l_arch_info.
ENDFORM.                    " DEACTIVATE_BDC_INFOSTRUCTURE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_INFOSTRUCTURE
*&---------------------------------------------------------------------*
FORM modify_infostructure .

  DATA  : lt_rows TYPE lvc_t_row.
  DATA  : ls_rows TYPE lvc_s_row.

  CALL METHOD g_alv_grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  SORT lt_rows DESCENDING BY index.

  IF lt_rows[] IS INITIAL.
    MESSAGE s032.
    EXIT.
  ENDIF.

  LOOP AT lt_rows INTO ls_rows .

    READ TABLE gt_list INDEX ls_rows-index.

    PERFORM modify_bdc_infostructure
                                 USING gt_list-tabname.

  ENDLOOP.

ENDFORM.                    " MODIFY_INFOSTRUCTURE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_BDC_INFOSTRUCTURE
*&---------------------------------------------------------------------*
FORM modify_bdc_infostructure
                          USING p_tabname.

  DATA:
    lt_bdcdata LIKE bdcdata OCCURS 10,
    lt_msgdata LIKE bdcmsgcoll OCCURS 1.
  DATA l_fieldcat TYPE tabname.
  DATA l_object TYPE tabname.
  DATA l_ddtext  LIKE dd02t-ddtext.
  DATA l_fldnr TYPE aind_fldnr.
  DATA l_last_flag.
  DATA l_arch_info TYPE aind_desc.
  DATA : BEGIN OF lt_dd03l OCCURS 0,
           tabname LIKE zhact0020-tabname,
           fieldname LIKE zhact0020-fieldname,
           position LIKE zhact0020-serial,
           keyflag LIKE zhact0020-keyflag,
         END OF lt_dd03l.

  PERFORM select_dd02t
                    USING p_tabname
                    CHANGING l_ddtext.

***  PERFORM SELECT_DD03L
***                   TABLES LT_DD03L
***                   USING P_TABNAME.

  PERFORM select_zhact0020
                       TABLES lt_dd03l
                       USING p_tabname.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archive Information System: Central management
    'X'   'SAPLAINA'       '0100',
    ' '   'BDC_OKCODE'     '=CUST'.

  IF p_tabname(1) EQ 'Z'.
    l_object = p_tabname(10).
    CONCATENATE l_object '_001' INTO l_fieldcat.
  ELSE.
    l_object = p_tabname(10).
    CONCATENATE 'Z' l_object '_001' INTO l_fieldcat.
  ENDIF.
  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archive Retrieval Configurator
    'X'   'SAPLAS_ARC'         '1000',
    ' '   'AIND_STR1-ARCHINDEX' l_fieldcat,
    ' '   'BDC_OKCODE'         '=DELE'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archive Retrieval Configurator
    'X'   'SAPLSPO1'           '0500',
***    ' '   'BDC_SUBSCR'         'SAPLSPO1                                0501SUBSCREEN',
    ' '   'BDC_OKCODE'         '=OPT1'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPMSSY0'         '0120',
    ' '   'BDC_OKCODE'       '=&ONT'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLAS_ARC'         '1000',
    ' '   'AIND_STR1-ARCHINDEX' l_fieldcat,
    ' '   'BDC_OKCODE'         '=FCAT'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLARIC'           '0150',
    ' '   'BDC_CURSOR'         'AIND_SV1S-ARCHINDEX(01)',
    ' '   'BDC_OKCODE'         '=POSI'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLSPO4'           '0300',
    ' '   'BDC_CURSOR'         'SVALD-VALUE(01)',
    ' '   'SVALD-VALUE(01)'    l_fieldcat,
    ' '   'BDC_OKCODE'         '=FURT'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLARIC'           '0150',
    ' '   'BDC_CURSOR'         'AIND_SV1S-ARCHINDEX(01)',
    ' '   'VIM_MARKED(01)'     c_flag_x,
    ' '   'BDC_OKCODE'         '=DELE'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLARIC'           '0150',
    ' '   'BDC_CURSOR'         'AIND_SV1S-ARCHINDEX(01)',
    ' '   'BDC_OKCODE'         '=SAVE'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Change View "Field Catalogs": Overview
     'X'   'SAPLARIC'           '0150',
     ' '   'BDC_CURSOR'         'AIND_SV1S-ARCHINDEX(01)',
     ' '   'BDC_OKCODE'         '=NEWL'.

  IF p_tabname(1) EQ 'Z'.
    l_object = p_tabname(10).
    CONCATENATE l_object '_001' INTO l_fieldcat.
  ELSE.
    l_object = p_tabname(10).
    CONCATENATE 'Z' l_object '_001' INTO l_fieldcat.
  ENDIF.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   New Entries: Overview of Added Entries
    'X'   'SAPLARIC'                 '0150',
    ' '   'AIND_SV1S-ARCHINDEX(01)'  l_fieldcat,
    ' '   'AIND_SV1S-TEXT(01)'       l_ddtext,
    ' '   'AIND_SV1S-OBJECT(01)'     l_object,
    ' '   'AIND_SV1S-FILEFIELD(01)'  'K',
    ' '   'AIND_SV1S-OFFSFIELD(01)'  'D',
    ' '   'BDC_OKCODE'               '=ATAB'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLSVCM'          '0100',
    ' '   'BDC_CURSOR'        'VIMDYNFLDS-DYN_LINE(02)',
    ' '   'BDC_OKCODE'        '=DETA'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLARIC'           '0350',
    ' '   'BDC_CURSOR'         'VIMDYNFLDS-DYN_LINE(01)',
    ' '   'BDC_OKCODE'         '=NEWL'.

  LOOP AT lt_dd03l.

    AT LAST.
      l_last_flag = c_flag_x.
    ENDAT.

    ADD 1 TO l_fldnr.

    IF lt_dd03l-keyflag EQ c_flag_x.

      PERFORM append_bdc_data TABLES lt_bdcdata USING:
*       Change View "Field Selection":Overview
        'X'   'SAPLARIC'                 '0350',
        ' '   'AIND_SV3ES-FLDNR(01)'     l_fldnr,
        ' '   'AIND_SV3ES-FIELDNAME(01)' lt_dd03l-fieldname,
        ' '   'AIND_SV3ES-REFTAB(01)'    lt_dd03l-tabname,
        ' '   'AIND_SV3ES-REFFIELD(01)'  lt_dd03l-fieldname,
        ' '   'AIND_SV3ES-KEYFLAG(01)'   c_flag_x,
        ' '   'BDC_OKCODE'               '=NEXT'.

    ELSE.

      IF l_last_flag EQ c_flag_x.

        PERFORM append_bdc_data TABLES lt_bdcdata USING:
*         Change View "Field Selection":Overview
          'X'   'SAPLARIC'                 '0350',
          ' '   'AIND_SV3ES-FLDNR(01)'     l_fldnr,
          ' '   'AIND_SV3ES-FIELDNAME(01)' lt_dd03l-fieldname,
          ' '   'AIND_SV3ES-REFTAB(01)'    lt_dd03l-tabname,
          ' '   'AIND_SV3ES-REFFIELD(01)'  lt_dd03l-fieldname,
          ' '   'BDC_OKCODE'               '=SAVE'.

        CLEAR l_last_flag.

      ELSE.

        PERFORM append_bdc_data TABLES lt_bdcdata USING:
*         Change View "Field Selection":Overview
          'X'   'SAPLARIC'                 '0350',
          ' '   'AIND_SV3ES-FLDNR(01)'     l_fldnr,
          ' '   'AIND_SV3ES-FIELDNAME(01)' lt_dd03l-fieldname,
          ' '   'AIND_SV3ES-REFTAB(01)'    lt_dd03l-tabname,
          ' '   'AIND_SV3ES-REFFIELD(01)'  lt_dd03l-fieldname,
          ' '   'BDC_OKCODE'               '=NEXT'.

      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:

    'X'   'SAPLARIC'                 '0350',
    ' '   'BDC_OKCODE'               '=ENDE'.


  l_object = p_tabname(10).
  CONCATENATE l_object '_001' INTO l_arch_info.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archive Retrieval Configurator
    'X'   'SAPLAS_ARC'               '1000',
    ' '   'AIND_STR1-ARCHINDEX'      l_arch_info,
    ' '   'BDC_OKCODE'               '=NEWL'.

  PERFORM append_bdc_data TABLES lt_bdcdata USING:
*   Archive Retrieval Configurator
    'X'   'SAPLAS_ARC'               '1010',
***    ' '   'AIND_STR1-ARCHINDEX'      L_ARCH_INFO,
    ' '   'AIND_STR1T-TEXT'          l_ddtext,
    ' '   'AIND_STR1-OBJECT'         l_object,
    ' '   'AIND_STR1-SKEY'           l_fieldcat,
    ' '   'BDC_OKCODE'               '=NEWL'.


  g_bdcmode = 'E'.
* CALL TRANSACTION
  PERFORM call_transaction_for_bdc
                                TABLES
                                  lt_bdcdata
                                  lt_msgdata
                                USING
                                  c_tcode_sari.

ENDFORM.                    " MODIFY_BDC_INFOSTRUCTURE
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CELL_DATA_CHANGED1
*&---------------------------------------------------------------------*
FORM alv_cl_cell_data_changed1
                         USING    rr_data_changed
                         TYPE REF TO cl_alv_changed_data_protocol.

  DATA : ls_mod_cells TYPE lvc_s_modi.

* Internal Value Control Use
* ### ### ## ### ### ### # ### ####
* #### ### # #### ##
  DATA: l_tabix TYPE i.
  DATA: lt_delta_cells    TYPE lvc_t_modi,
        ls_delta_cells    TYPE lvc_s_modi.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.

    CHECK NOT ls_mod_cells-value IS INITIAL.

    CASE ls_mod_cells-fieldname.
      WHEN 'TABNAME'.
        READ TABLE gt_list INDEX ls_mod_cells-row_id.

        IF sy-subrc EQ 0.
          DATA la_list LIKE zbdctable.
          DATA l_tabname LIKE zbdctable-tabname.

          SELECT SINGLE
            tabname
            FROM zbdctable
            INTO l_tabname
           WHERE tabname EQ ls_mod_cells-value.

          IF sy-subrc EQ 0.
            MESSAGE s191 WITH ls_mod_cells-value DISPLAY LIKE 'E'.
            CLEAR ls_mod_cells-value.
          ELSE.
            IF gt_list-tabname IS INITIAL.
              la_list-tabname = ls_mod_cells-value.
              INSERT zbdctable FROM la_list.
              COMMIT WORK.
              CLEAR la_list.
            ELSE.
              DELETE FROM zbdctable WHERE tabname = gt_list-tabname.
              IF sy-subrc EQ 0.
                COMMIT WORK.
                la_list-tabname = ls_mod_cells-value.
                INSERT zbdctable FROM la_list.
                COMMIT WORK.
                CLEAR la_list.
              ENDIF.
            ENDIF.


          ENDIF.


          PERFORM modify_cell
                          USING rr_data_changed
                                ls_mod_cells.

        ENDIF.

    ENDCASE.

  ENDLOOP.


ENDFORM.                    " ALV_CL_CELL_DATA_CHANGED1
*&---------------------------------------------------------------------*
*&      Form  MODIFY_CELL
*&---------------------------------------------------------------------*
FORM modify_cell
             USING rr_data_changed TYPE REF TO cl_alv_changed_data_protocol
                   ls_mod_cells TYPE lvc_s_modi.

  CALL METHOD rr_data_changed->modify_cell
    EXPORTING
      i_row_id    = ls_mod_cells-row_id
      i_fieldname = ls_mod_cells-fieldname
      i_value     = ls_mod_cells-value.

ENDFORM.                    " MODIFY_CELL
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_TOOLBAR_PART1
*&---------------------------------------------------------------------*
FORM alv_cl_toolbar_part1
                USING r_object TYPE REF TO cl_alv_event_toolbar_set
                      r_interactive.

  DATA: ls_toolbar  TYPE stb_button.
  CLEAR ls_toolbar.
  MOVE 3                 TO ls_toolbar-butn_type.
  APPEND ls_toolbar      TO r_object->mt_toolbar.

  CLEAR ls_toolbar.
  MOVE 'ADD'            TO ls_toolbar-function.
  MOVE icon_insert_row TO ls_toolbar-icon.
  MOVE ''      TO ls_toolbar-quickinfo.
  MOVE ' '               TO ls_toolbar-disabled.
  MOVE ''      TO ls_toolbar-text.
  APPEND ls_toolbar TO r_object->mt_toolbar.
*
***  CLEAR LS_TOOLBAR.
***  MOVE 'DEL'            TO LS_TOOLBAR-FUNCTION.
***  MOVE ICON_DELETE_ROW   TO LS_TOOLBAR-ICON.
***  MOVE ''        TO LS_TOOLBAR-QUICKINFO.
***  MOVE ' '               TO LS_TOOLBAR-DISABLED.
***  MOVE ''        TO LS_TOOLBAR-TEXT.
***  APPEND LS_TOOLBAR TO R_OBJECT->MT_TOOLBAR.

ENDFORM.                    " ALV_CL_TOOLBAR_PART1
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_USER_COMMAND_PART1
*&---------------------------------------------------------------------*
FORM alv_cl_user_command_part1
                            USING    p_ucomm.

  CASE p_ucomm.
    WHEN 'ADD'.
      CLEAR gt_list.
      INSERT gt_list INDEX 1.
    WHEN 'DEL'.
      DATA  : lt_rows TYPE lvc_t_row.
      DATA  : ls_rows TYPE lvc_s_row.

      CALL METHOD g_alv_grid1->get_selected_rows
        IMPORTING
          et_index_rows = lt_rows.

      SORT lt_rows DESCENDING BY index.

      IF lt_rows[] IS INITIAL.
        MESSAGE s003 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      LOOP AT lt_rows INTO ls_rows .
        READ TABLE gt_list INDEX ls_rows-index.
        DELETE FROM zbdctable WHERE tabname = gt_list-tabname.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ENDIF.
        DELETE gt_list INDEX ls_rows-index.
      ENDLOOP.
  ENDCASE.
* ALV Refresh
  PERFORM alv_cl_refresh_table_display
                          USING  g_alv_grid1
                                 g_rec_stable1.
ENDFORM.                    " ALV_CL_USER_COMMAND_PART1
