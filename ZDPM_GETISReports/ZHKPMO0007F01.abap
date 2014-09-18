*&---------------------------------------------------------------------*
*&  Include           ZHKPMO0007F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA : lt_mpla LIKE TABLE OF mpla WITH HEADER LINE ,
         lt_mpos LIKE TABLE OF mpos WITH HEADER LINE ,
         lt_iloa LIKE TABLE OF iloa WITH HEADER LINE .

  DATA : lt_t399g LIKE TABLE OF t399g_t WITH HEADER LINE ,
         lt_T399W like table of T399W_T with header line .

  CLEAR : gt_data[], gt_data .

*.. Maintenance plan
  SELECT * INTO TABLE lt_mpla
    FROM mpla
   WHERE warpl IN s_warpl
     AND wptxt IN s_wptxt
     AND strat IN s_strat
     AND mptyp IN s_mptyp
     AND plan_sort IN s_sort .

  CHECK lt_mpla[] IS NOT INITIAL .

*.. Maintenance item
  SELECT * INTO TABLE lt_mpos
   FROM mpos
   FOR ALL ENTRIES IN lt_mpla
  WHERE warpl EQ lt_mpla-warpl .

*.. PM Object Location and Account Assignment
  IF lt_mpos[] IS NOT INITIAL .
    SELECT * INTO TABLE lt_iloa
      FROM iloa
      FOR ALL ENTRIES IN lt_mpos
     WHERE iloan EQ lt_mpos-iloan .
  ENDIF .

*... Descriptions for Maintenance Plan Sort Fields
  SELECT * INTO TABLE lt_t399g
    FROM t399g_t
    FOR ALL ENTRIES IN lt_mpla
   WHERE spras     EQ sy-langu
     AND plan_sort EQ lt_mpla-plan_sort .

  select * into table lt_T399W
    from T399W_T
   WHERE spras     EQ sy-langu .

  SORT lt_mpos BY warpl .
  SORT lt_iloa BY iloan .
  SORT lt_t399g BY plan_sort .
  sort lt_t399w by MPTYP .

  LOOP AT lt_mpla .

    CLEAR : lt_t399g , lt_t399w .
    READ  TABLE lt_t399g WITH KEY plan_sort = lt_mpla-plan_sort
                                  BINARY SEARCH .

    READ  TABLE lt_t399w WITH KEY MPTYP = lt_mpla-MPTYP
                                  BINARY SEARCH .

    READ TABLE lt_mpos WITH KEY warpl = lt_mpla-warpl
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      LOOP AT lt_mpos FROM sy-tabix .
        IF lt_mpos-warpl <> lt_mpla-warpl .
          EXIT .
        ENDIF .
        CLEAR gt_data .
        gt_data-warpl  =  lt_mpla-warpl .
        gt_data-wptxt  =  lt_mpla-wptxt .
        gt_data-strat  =  lt_mpla-strat .
        gt_data-abrho  =  lt_mpla-abrho .
        gt_data-mptyp  =  lt_mpla-mptyp .
        gt_data-mptxt  =  lt_t399w-txt .
        gt_data-hunit  =  lt_mpla-hunit .
        gt_data-stadt  =  lt_mpla-stadt .
        gt_data-plan_sort  =  lt_mpla-plan_sort .
        gt_data-txt    =  lt_t399g-txt  .
        gt_data-wapos  =  lt_mpos-wapos .
        gt_data-wppos  =  lt_mpos-wppos .
        gt_data-pstxt  =  lt_mpos-pstxt .
        gt_data-equnr  =  lt_mpos-equnr .
        gt_data-plnty  =  lt_mpos-plnty .
        gt_data-plnnr  =  lt_mpos-plnnr .
        gt_data-plnal  =  lt_mpos-plnal .
        gt_data-iwerk  =  lt_mpos-iwerk .
        gt_data-iloan  =  lt_mpos-iloan .
        gt_data-laufn  =  lt_mpos-laufn .
        gt_data-auart  =  lt_mpos-auart .
        gt_data-ilart  =  lt_mpos-ilart .
        gt_data-qmnum  =  lt_mpos-qmnum .
        gt_data-qmart  =  lt_mpos-qmart .
        READ TABLE lt_iloa WITH KEY iloan = lt_mpos-iloan
                                    BINARY SEARCH .
        IF sy-subrc = 0 .
          gt_data-beber = lt_iloa-beber .
        ENDIF .
        APPEND gt_data .
      ENDLOOP .
    ENDIF .

  ENDLOOP .

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .

  DATA : lt_0025 LIKE TABLE OF zhkpmt0025 WITH HEADER LINE .

  CHECK gt_data[] IS NOT INITIAL .

  SELECT * INTO TABLE lt_0025
    FROM zhkpmt0025
    FOR ALL ENTRIES IN gt_data
   WHERE warpl EQ gt_data-warpl
     AND wapos EQ gt_data-wapos .


  SORT lt_0025 BY warpl  wapos .

  LOOP AT gt_data .

*  '@08@'."  Green light; positive
*  '@09@'."  Yellow light; neutral
*  '@0A@'."  Red light; negative


    READ TABLE lt_0025 WITH KEY warpl = gt_data-warpl
                                wapos = gt_data-wapos
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      IF p_check = c_x .
        DELETE gt_data . CONTINUE .
      ENDIF .
      gt_data-zpmty = lt_0025-zpmty .
      gt_data-icon  = '@08@'."  Green light; positive
    ELSE .
      gt_data-icon  = '@09@'."  Yellow light; neutral
    ENDIF .

    MODIFY gt_data .
  ENDLOOP .


ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM create_container_object  USING    p_sy_dynnr.
  CREATE OBJECT g_alv_doc_mdat
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      side      = g_alv_doc_mdat->dock_at_left
      extension = 2700. "1500.

  CREATE OBJECT g_grid_mdat
    EXPORTING
      i_parent = g_alv_doc_mdat.

  CREATE OBJECT g_event_mdat.
  CREATE OBJECT g_event_item.

ENDFORM.                    " CREATE_CONTAINER_OBJECT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid  USING    p_dynnr.

*celltab
  st_lay-stylefname = 'CELLTAB'.
*  st_lay-ctab_fname = 'ALV_COLOR'.
*columns
*  st_lay-edit = 'X'.
*  ST_LAY-CWIDTH_OPT = 'X'.
  st_lay-zebra      = 'X'.

*  st_lay-box_fname  = 'MARK'.
*      ST_LAY-SEL_MODE = 'A'.
*      st_lay-grid_title  = g_title.

*      ST_LAY-GRID_TITLE  = G_TITLE.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_field_catalog  USING    p_dynnr.

  DATA: l_tabname  TYPE tabname ,
        l_tabix    TYPE sytabix .

  l_tabname = 'ZHKPMS0060' .

  CLEAR: gt_field[].
  SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_bypassing_buffer = 'X'
      i_buffer_active    = 'X'
      i_structure_name   = l_tabname
    CHANGING
      ct_fieldcat        = gt_field.

  LOOP AT gt_field INTO st_field.
    l_tabix = sy-tabix .
    CASE st_field-fieldname.
      WHEN 'ICON'.
        st_field-reptext   = 'Status' .
        st_field-scrtext_m = 'Status' .
        st_field-coltext   = 'Status' .
        st_field-key       = 'X'.
        st_field-outputlen = 4 .
        MODIFY gt_field FROM st_field.

      WHEN 'WARPL'.
        st_field-reptext   = 'MntPlan' .
        st_field-scrtext_m = 'MntPlan'.
        st_field-coltext   = 'MntPlan'.
        st_field-key       = 'X'.
*        st_field-outputlen = 2 .
*        st_field-just      = c_c .
*        st_field-no_out    = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'WPTXT' .
        st_field-reptext   = 'MntPlan desc.' .
        st_field-scrtext_m = 'MntPlan desc.' .
        st_field-coltext   = 'MntPlan desc.' .
        st_field-key       = 'X'.
        st_field-edit      = ' '.
        MODIFY gt_field FROM st_field.

      WHEN 'STRAT'.
        st_field-reptext   = 'Strategy' .
        st_field-scrtext_m = 'Strategy' .
        st_field-coltext   = 'Strategy' .
        st_field-key       = 'X'.
        st_field-edit      = ' '.
        st_field-hotspot   = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'ABRHO'.
        st_field-reptext   = 'SP' .
        st_field-scrtext_m = 'SP' .
        st_field-coltext   = 'SP' .
        st_field-key       = 'X'.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'MPTYP'.
        st_field-reptext   = 'Mplan Cat.' .
        st_field-scrtext_m = 'Mplan Cat.' .
        st_field-coltext   = 'Mplan Cat.' .
        st_field-key       = ' '.
        MODIFY gt_field FROM st_field.

      WHEN 'MPTXT'.
        st_field-reptext   = 'Mplan Cat.Text' .
        st_field-scrtext_m = 'Mplan Cat.Text' .
        st_field-coltext   = 'Mplan Cat.Text' .
        st_field-key       = ' '.
        st_field-outputlen = 20 .
        MODIFY gt_field FROM st_field.

      WHEN 'HUNIT'.
        st_field-reptext   = 'Unit' .
        st_field-scrtext_m = 'Unit' .
        st_field-coltext   = 'Unit' .
        st_field-key       = ' '.
*        st_field-outputlen = 10 .
        MODIFY gt_field FROM st_field.
      WHEN 'STADT'.
        st_field-reptext   = 'Cycle Start' .
        st_field-scrtext_m = 'Cycle Start' .
        st_field-coltext   = 'Cycle Start' .
        st_field-key       = ' '.
        MODIFY gt_field FROM st_field.
      WHEN 'PLAN_SORT'.
        st_field-reptext   = 'Sort' .
        st_field-scrtext_m = 'Sort' .
        st_field-coltext   = 'Sort' ..
        st_field-key       = ' '.
        st_field-edit      = ' '.
        MODIFY gt_field FROM st_field.
      WHEN 'TXT'.
        st_field-reptext   = 'Text' .
        st_field-scrtext_m = 'Text' ..
        st_field-coltext   = 'Text' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'WAPOS'.
        st_field-reptext   = 'MntItem' .
        st_field-scrtext_m = 'MntItem' .
        st_field-coltext   = 'MntItem' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'WPPOS'.
        st_field-reptext   = 'Item'.
        st_field-scrtext_m = 'Item'.
        st_field-coltext   = 'Item'.
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'PSTXT'.
        st_field-reptext   = 'MntItem desc.' .
        st_field-scrtext_m = 'MntItem desc.' .
        st_field-coltext   = 'MntItem desc.' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'EQUNR' .
        st_field-reptext   = 'Equipment' .
        st_field-scrtext_m = 'Equipment' .
        st_field-coltext   = 'Equipment' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'PLNTY'.
        st_field-reptext   = 'TLType' .
        st_field-scrtext_m = 'TLType' .
        st_field-coltext   = 'TLType' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.
      WHEN 'PLNNR' .

        st_field-reptext   = 'Group' .
        st_field-scrtext_m = 'Group' .
        st_field-coltext   = 'Group' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'PLNAL' .
        st_field-reptext   = 'GrC' .
        st_field-scrtext_m = 'GrC' .
        st_field-coltext   = 'GrC' .
        st_field-key       = ' '.
        st_field-edit      = ' '.
        st_field-checkbox  = ' ' .
        st_field-just      = ' ' .
        MODIFY gt_field FROM st_field.
      WHEN 'IWERK' .
        st_field-reptext   = 'Plant' .
        st_field-scrtext_m = 'Plant' .
        st_field-coltext   = 'Plant' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'ILOAN' .
        st_field-reptext   = 'Loc/Acc' .
        st_field-scrtext_m = 'Loc/Acc' .
        st_field-coltext   = 'Loc/Acc' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'LAUFN' .
        st_field-reptext   = 'Last ord.' .
        st_field-scrtext_m = 'Last ord.' .
        st_field-coltext   = 'Last ord.' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'AUART' .
        st_field-reptext   = 'OrdType' .
        st_field-scrtext_m = 'OrdType' .
        st_field-coltext   = 'OrdType' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'ILART' .
        st_field-reptext   = 'MAT' .
        st_field-scrtext_m = 'MAT' .
        st_field-coltext   = 'MAT' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'QMNUM' .
        st_field-reptext   = 'Noti' .
        st_field-scrtext_m = 'Noti' .
        st_field-coltext   = 'Noti' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'QMART' .
        st_field-reptext   = 'NotiType' .
        st_field-scrtext_m = 'NotiType' .
        st_field-coltext   = 'NotiType' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'BEBER' .
        st_field-reptext   = 'Shop' .
        st_field-scrtext_m = 'Shop' .
        st_field-coltext   = 'Shop' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'ZPMTY' .
        st_field-reptext   = 'PMTYpe' .
        st_field-scrtext_m = 'PMTYpe' .
        st_field-coltext   = 'PMTYpe' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-drdn_hndl = '1'.
        st_field-outputlen = 14 .
        MODIFY gt_field FROM st_field.

      WHEN 'MESG' .
        st_field-reptext   = 'message' .
        st_field-scrtext_m = 'message' .
        st_field-coltext   = 'message' .
        st_field-key       = ' '.
        st_field-outputlen = 30 .
        MODIFY gt_field FROM st_field.
      WHEN OTHERS .
        DELETE gt_field .
    ENDCASE .
  ENDLOOP.


ENDFORM.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  EXCLUDING_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM excluding_functions  USING pa_dynnr.
  DATA ls_exclude TYPE ui_func.


  CLEAR: gt_exclude, gt_exclude[].
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO gt_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO gt_exclude.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUBTOT.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUM.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO gt_exclude.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FULL.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_SOFT.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.

**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_FILTER .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_FIND .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.

**  IF PA_DYNNR <> '0400'.
**    LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUBTOT .
**    APPEND LS_EXCLUDE TO IT_EXCLUDE.
**    LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUM .
**    APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  ENDIF.

**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_MAXIMUM .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_MINIMUM .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_AVERAGE .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.

ENDFORM.                    " EXCLUDING_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv  USING    p_dynnr.

* Define a drop down table.
  PERFORM set_drdn_table.

  CALL METHOD g_grid_mdat->set_table_for_first_display
    EXPORTING      "
      it_toolbar_excluding = gt_exclude
      is_layout            = st_lay
      i_save               = 'x'
    CHANGING
      it_fieldcatalog      = gt_field[]
      it_outtab            = gt_data[].

* Set editable cells to ready for input initially
  CALL METHOD g_grid_mdat->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assign_event  USING    p_dynnr.

*  SET HANDLER g_event_item->hotspot_click_item   FOR g_grid_mdat.
*  SET HANDLER G_EVENT_ITEM->DOUBLE_CLICK         FOR G_GRID_MDAT.
*  SET HANDLER g_event_item->toolbar_item         FOR g_grid_mdat.
*  SET HANDLER g_event_item->user_command_item    FOR g_grid_mdat .
*  SET HANDLER G_EVENT_ITEM->DATA_CHANGED         FOR G_GRID_MDAT .

  CALL METHOD g_grid_mdat->set_toolbar_interactive.

  CALL METHOD g_grid_mdat->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.                    " ASSIGN_EVENT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_data  USING p_dynnr.

  DATA: ls_stable TYPE lvc_s_stbl.

  ls_stable-row = 'x'.
  ls_stable-col = 'x'.

  CALL METHOD g_grid_mdat->refresh_table_display
    EXPORTING
      is_stable = ls_stable.

ENDFORM.                    " REFRESH_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_DRDN_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_drdn_table .

  DATA : lt_dd07l LIKE TABLE OF dd07l WITH HEADER LINE .
  DATA: lt_dropdown TYPE lvc_t_drop,
        ls_dropdown TYPE lvc_s_drop.

  SELECT * INTO TABLE lt_dd07l
    FROM dd07l
   WHERE domname = 'ZCPMSORTL' .

  CHECK sy-subrc = 0 .

  LOOP AT lt_dd07l .
* First listbox (handle '1').
    ls_dropdown-handle = '1'.
    ls_dropdown-value = lt_dd07l-domvalue_l.
    APPEND ls_dropdown TO lt_dropdown.
  ENDLOOP .

  CALL METHOD g_grid_mdat->set_drop_down_table
    EXPORTING
      it_drop_down = lt_dropdown.

ENDFORM.                    " SET_DRDN_TABLE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_save .

  DATA : l_answer .
  DATA : l_valid ,
         l_error .
  DATA : ls_0025 LIKE zhkpmt0025 .


  CALL METHOD g_grid_mdat->check_changed_data
    IMPORTING
      e_valid = l_valid.

  REFRESH g_rows_t.
  CALL METHOD g_grid_mdat->get_selected_rows
    IMPORTING
      et_index_rows = g_rows_t.
  IF g_rows_t[] IS INITIAL.
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF.

  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_data INDEX g_rows_s-index.
    IF sy-subrc = 0 AND gt_data-zpmty IS NOT INITIAL .

      UPDATE zhkpmt0025
         SET zpmty = gt_data-zpmty
             aedat = sy-datum
             aezet = sy-uzeit
             aenam = sy-uname
       WHERE warpl = gt_data-warpl
         AND wapos = gt_data-wapos  .
      IF sy-subrc <> 0 .
        CLEAR ls_0025 .
        ls_0025-warpl = gt_data-warpl .
        ls_0025-wapos = gt_data-wapos .
        ls_0025-zpmty = gt_data-zpmty .
        ls_0025-erdat = sy-datum .
        ls_0025-erzet = sy-uzeit .
        ls_0025-ernam = sy-uname .
        ls_0025-aedat = sy-datum .
        ls_0025-aezet = sy-uzeit .
        ls_0025-aenam = sy-uname .
        INSERT zhkpmt0025 FROM ls_0025 .
      ENDIF .
      gt_data-icon  = '@08@'."  Green light; positive
      MODIFY gt_data INDEX g_rows_s-index.
    ENDIF .

  ENDLOOP .

  MESSAGE s000 WITH 'process is complete' .
ENDFORM.                    " PROCESS_SAVE
