*&---------------------------------------------------------------------*
*&  Include           ZACOU154_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_initial .
*... SET DEFAULT VALUE.
  IF p_vstel IS INITIAL. "Shipping point
    p_vstel = 'P200'.
  ENDIF.

  IF p_bdate IS INITIAL. "Base date
    p_bdate = sy-datum.
  ENDIF.
ENDFORM.                    " SET_INITIAL
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA: ls_makt TYPE makt,
        lt_makt TYPE TABLE OF makt,
        ls_vbfa TYPE vbfa,
        lt_vbfa TYPE TABLE OF vbfa.

  DATA: ls_record LIKE LINE OF gt_record,
        lt_record LIKE TABLE OF ls_record.

  DATA: l_cno TYPE p.

  FIELD-SYMBOLS <fs> LIKE gs_record. "STRUCTURE gs_record DEFAULT gs_record .

*  IF s_matnr[] IS INITIAL.
*    MESSAGE s000 DISPLAY LIKE 'E' WITH text-m03.
*    g_sflag = 'X'.
*  ELSE.

  CLEAR: gs_s_matnr, gt_s_matnr[].

  gs_s_matnr-matnr    = p_matnr1.
  gs_s_matnr-quantity = p_quan1.
  APPEND gs_s_matnr TO gt_s_matnr.

  IF p_matnr2 IS NOT INITIAL.
    gs_s_matnr-matnr    = p_matnr2.
    gs_s_matnr-quantity = p_quan2.
    APPEND gs_s_matnr TO gt_s_matnr.
  ENDIF.

  IF p_matnr3 IS NOT INITIAL.
    gs_s_matnr-matnr    = p_matnr3.
    gs_s_matnr-quantity = p_quan3.
    APPEND gs_s_matnr TO gt_s_matnr.
  ENDIF.

  SORT gt_s_matnr BY matnr.

  CLEAR: g_sflag, gs_record, gt_record[].

*... Set select period.
  g_endda = p_bdate.  "End date is input value.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = p_bdate
      days      = '00'
      months    = '00'
      signum    = '-'
      years     = '01'
    IMPORTING
      calc_date = g_begda.

  SELECT a~vbeln a~posnr a~matnr a~vrkme a~kwmeng a~erdat
    INTO CORRESPONDING FIELDS OF TABLE lt_record
    FROM vbap AS a INNER JOIN vbup AS b
      ON a~vbeln EQ b~vbeln
     AND a~posnr EQ b~posnr
      FOR ALL ENTRIES IN gt_s_matnr
   WHERE a~matnr EQ gt_s_matnr-matnr
     AND a~vstel EQ p_vstel
     AND a~erdat BETWEEN g_begda AND g_endda
     AND b~lfsta <> 'C'
* by I.G Moon 12/20/2012 {
     AND b~LFGSA <> 'C'.
* }

  SORT lt_record BY vbeln posnr erdat.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
    FROM vbfa
    FOR ALL ENTRIES IN lt_record
   WHERE vbelv   EQ lt_record-vbeln
     AND posnv   EQ lt_record-posnr
     AND vbtyp_n EQ 'J'.

  SORT lt_vbfa BY vbelv posnv.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_makt
    FROM makt
     FOR ALL ENTRIES IN lt_record
   WHERE matnr EQ lt_record-matnr
     AND spras EQ sy-langu.


  SORT lt_makt BY matnr.

  DATA: l_count  TYPE i,
        l_count2 TYPE i.
  LOOP AT gt_s_matnr INTO gs_s_matnr.
    l_count = 0.

    LOOP AT lt_record INTO ls_record WHERE matnr = gs_s_matnr-matnr.
      l_count = l_count + 1.
    ENDLOOP.

    gs_s_matnr-count = l_count.

    l_count2 = 1.

    LOOP AT lt_record INTO ls_record WHERE matnr = gs_s_matnr-matnr.
      READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_record-matnr.
      IF sy-subrc EQ 0.
        ls_record-maktx = ls_makt-maktx.
      ENDIF.

      LOOP AT lt_vbfa INTO ls_vbfa WHERE vbelv EQ ls_record-vbeln
                                     AND posnv EQ ls_record-posnr.
        ADD ls_vbfa-rfmng TO ls_record-rfmng.
      ENDLOOP.

      ls_record-open_no = ls_record-kwmeng - ls_record-rfmng.

      l_cno = l_cno + ls_record-open_no.
      IF l_cno >= gs_s_matnr-quantity.
        ls_record-post_no = gs_s_matnr-quantity.
        IF ls_record-post_no > ls_record-open_no.
          ls_record-icon = icon_red_light.
          ls_record-message = 'Posting quantity exceed open quantity!'.
        ENDIF.
        APPEND ls_record TO gt_record.
        CLEAR l_cno.
        EXIT.
      ELSE.
        IF gs_s_matnr-quantity >= ls_record-open_no.
          IF gs_s_matnr-count EQ 1.
            ls_record-post_no = gs_s_matnr-quantity.
            ls_record-icon = icon_red_light.
            ls_record-message = 'Posting quantity exceed open quantity!'.
          ELSE.
            IF l_count EQ l_count2.
              ls_record-post_no = gs_s_matnr-quantity.
              ls_record-icon = icon_red_light.
              ls_record-message = 'Posting quantity exceed open quantity!'.
            ELSE.
              ls_record-post_no = ls_record-open_no.
              gs_s_matnr-quantity = gs_s_matnr-quantity - ls_record-open_no.
            ENDIF.
          ENDIF.
        ENDIF.
        APPEND ls_record TO gt_record.
      ENDIF.
      l_count2 = l_count2 + 1.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_alv .
  CLEAR: gt_exclude.

  PERFORM build_layout_edit.       "Set Layout
  PERFORM build_variant.
  PERFORM build_fieldcat CHANGING gt_fieldcat. "Build fieldcat

  PERFORM alv_exclude_functions CHANGING gt_exclude.

  PERFORM alv_sort.

  IF g_grid IS INITIAL.
    PERFORM create_dynamic_container USING g_custom_container
                                           g_container.
    PERFORM create_dynamic_grid USING g_grid
                                      g_custom_container.
    PERFORM display_grid.

  ELSE.
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = gt_exclude
        is_layout            = gs_layout
        i_save               = 'A'
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = gt_fieldcat
        it_outtab            = gt_record[]
        it_sort              = gt_alv_sort[].

    CALL METHOD g_grid->refresh_table_display.
    CALL METHOD cl_gui_cfw=>flush.
  ENDIF.
ENDFORM.                    " CREATE_ALV
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT_EDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout_edit .
  CLEAR gs_layout.
  gs_layout-sel_mode   = 'A'.
*  gs_layout-box_fname  = 'BOX'.
  gs_layout-zebra      = 'X'.
  gs_layout-info_fname = 'LINECOLOR'.
*  gs_layout-edit = 'X'.
  gs_layout-cwidth_opt = 'A'.
  gs_layout-ctab_fname = 'TABCOLOR'.
  gs_layout-stylefname = 'CELLTAB'.
ENDFORM.                    " BUILD_LAYOUT_EDIT
*&---------------------------------------------------------------------*
*&      Form  BUILD_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_variant .
  CLEAR gs_variant.
  gs_variant-report   = g_repid = sy-repid.
  gs_variant-username = sy-uname.
ENDFORM.                    " BUILD_VARIANT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM build_fieldcat  CHANGING pt_fieldcat  TYPE lvc_t_fcat.

  DATA: l_pos TYPE i.

  FIELD-SYMBOLS <fcat> LIKE lvc_s_fcat.

  l_pos = 0.

  CLEAR: pt_fieldcat[].


  PERFORM alv_create_field_catalog USING:
     pt_fieldcat  'VBELN'    'Order no.'         'X' l_pos ' ',
     pt_fieldcat  'POSNR'    'Item no.'          'X' l_pos ' ',
     pt_fieldcat  'MATNR'    'Material no.'      'X' l_pos ' ',
     pt_fieldcat  'MAKTX'    'Material Text'     'X' l_pos ' ',
     pt_fieldcat  'KWMENG'   'Order Quantity'    ' ' l_pos ' ',
     pt_fieldcat  'RFMNG'    'Issue Quantity'    ' ' l_pos ' ',
     pt_fieldcat  'OPEN_NO'  'Open Quantity'     ' ' l_pos ' ',
     pt_fieldcat  'POST_NO'  'Post Quantity'     ' ' l_pos ' ',
     pt_fieldcat  'VRKME'    'Unit'              ' ' l_pos ' ',
     pt_fieldcat  'ERDAT'    'Created date'      ' ' l_pos ' ',
     pt_fieldcat  'ICON'     'Status'            ' ' l_pos ' ',
     pt_fieldcat  'DOC_NUM'  'Delivery Doc.'     ' ' l_pos ' ',
     pt_fieldcat  'MESSAGE'  'Message'           ' ' l_pos ' '.
  LOOP AT pt_fieldcat ASSIGNING <fcat>.
    CASE <fcat>-fieldname.
      WHEN 'VBELN' OR 'DOC_NUM'.
        <fcat>-hotspot = 'X'.
      WHEN 'KWMENG' OR 'RFMNG'.
        <fcat>-qfieldname = 'VRKME'.
        <fcat>-do_sum = 'X'.
      WHEN 'OPEN_NO' OR 'POST_NO'.
*        IF p_radio2 EQ 'X'.
*          <fcat>-edit = 'X'.
*        ENDIF.
        <fcat>-just = 'R'.
        <fcat>-do_sum = 'X'.
      WHEN 'ICON'.
        <fcat>-just = 'C'.
      WHEN 'MESSAGE'.
        <fcat>-outputlen = '50'.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ALV_CREATE_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_FIELDCAT  text
*      -->P_LS_FCAT  text
*      -->P_0297   text
*      -->P_0298   text
*      -->P_0299   text
*      -->P_L_POS  text
*      -->P_0301   text
*----------------------------------------------------------------------*
FORM alv_create_field_catalog  USING   pt_fcat      TYPE lvc_t_fcat
*                                       ps_fcat      TYPE lvc_s_fcat
                                       p_fieldname  TYPE slis_fieldname
                                       p_fieldtext
                                       p_key
                                       p_pos TYPE i
                                       p_edit.

  DATA: ls_fcat TYPE lvc_s_fcat.

  ls_fcat-col_pos      = p_pos + 1.
  ls_fcat-row_pos      = 1.
  ls_fcat-fieldname    = p_fieldname.

  ls_fcat-scrtext_s    = p_fieldtext.
  ls_fcat-scrtext_m    = p_fieldtext.
  ls_fcat-scrtext_l    = p_fieldtext.
  ls_fcat-reptext      = p_fieldtext.

  ls_fcat-key          = p_key.
  ls_fcat-edit         = p_edit.


  APPEND ls_fcat TO pt_fcat. CLEAR : ls_fcat.

ENDFORM.                    " ALV_CREATE_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  ALV_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_EXCLUDE  text
*----------------------------------------------------------------------*
FORM alv_exclude_functions CHANGING pt_exclude TYPE ui_functions.

  DATA : ls_exclude TYPE ui_func.

  CLEAR : pt_exclude, pt_exclude[].

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sum.
  APPEND ls_exclude TO pt_exclude.

ENDFORM.                    " ALV_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_CUSTOM_CONTAINER  text
*      -->P_G_CONTAINER  text
*----------------------------------------------------------------------*
FORM create_dynamic_container USING us_container TYPE REF TO cl_gui_custom_container
                                    us_container_name.

  CREATE OBJECT us_container
    EXPORTING
      container_name              = us_container_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

ENDFORM.                    " CREATE_DYNAMIC_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_GRID  text
*      -->P_G_CUSTOM_CONTAINER  text
*----------------------------------------------------------------------*
FORM create_dynamic_grid USING us_grid TYPE REF TO cl_gui_alv_grid
                               us_cont.

*-  Create an instance of alv control
  CREATE OBJECT us_grid
    EXPORTING
      i_parent = us_cont.

ENDFORM.                    " CREATE_DYNAMIC_GRID
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_grid .
  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.


  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT g_event_receiver
    EXPORTING
      i_repid = sy-repid.

  SET HANDLER g_event_receiver->handle_data_changed  FOR g_grid.
  SET HANDLER g_event_receiver->handle_hotspot_click  FOR g_grid.

  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = gt_exclude
      is_layout            = gs_layout
      i_save               = 'A'
      is_variant           = gs_variant
    CHANGING
      it_fieldcatalog      = gt_fieldcat
      it_outtab            = gt_record[]
      it_sort              = gt_alv_sort[].

  CALL METHOD g_grid->refresh_table_display.
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.                    " DISPLAY_GRID

*&---------------------------------------------------------------------*
*&      Form  alv_handle_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM alv_handle_data_changed USING pr_data_changed
                             TYPE REF TO cl_alv_changed_data_protocol.

  FIELD-SYMBOLS <record> LIKE gs_record.

  DATA: ls_mod_cell TYPE lvc_s_modi,
        l_value1(20) TYPE c,
        l_value2(15) TYPE p DECIMALS 3.

  DATA: l_flag(1) TYPE c.

  CLEAR: l_flag.

  LOOP AT pr_data_changed->mt_good_cells INTO ls_mod_cell.

    CONDENSE ls_mod_cell-value.

    l_value1 = ls_mod_cell-value.

    READ TABLE gt_record ASSIGNING <record> INDEX ls_mod_cell-row_id.

    l_value2 = l_value1.

    IF l_value2 > ( <record>-kwmeng - <record>-rfmng ).
      MESSAGE s000 DISPLAY LIKE 'E' WITH text-m02.
      l_flag = 'X'.
      <record>-open_no = l_value1.
    ENDIF.
  ENDLOOP.

  CHECK l_flag <> 'X'.
  CALL METHOD g_grid->refresh_table_display.
ENDFORM.                    "alv_handle_data_changed

*&---------------------------------------------------------------------*
*&      Form  hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW_ID     text
*      -->P_COLUMN_ID  text
*----------------------------------------------------------------------*
FORM hotspot_click USING p_row_id p_column_id.
  FIELD-SYMBOLS <record> LIKE gs_record.

  READ TABLE gt_record ASSIGNING <record> INDEX p_row_id.

  CASE p_column_id.
    WHEN 'VBELN'.
      SET PARAMETER ID 'AUN' FIELD <record>-vbeln.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    WHEN 'DOC_NUM'.
      SET PARAMETER ID 'VL' FIELD <record>-doc_num.
      CALL TRANSACTION 'VL03' AND SKIP FIRST SCREEN.
  ENDCASE.
ENDFORM.                    "hotspot_click
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_GRID  text
*      -->P_LT_ROWS[]  text
*----------------------------------------------------------------------*
FORM get_selected_row  USING    p_grid TYPE REF TO cl_gui_alv_grid
                              pt_rows.

  DATA: lt_rowid TYPE lvc_t_roid.

  CALL METHOD p_grid->get_selected_rows
    IMPORTING
      et_index_rows = pt_rows
      et_row_no     = lt_rowid.

ENDFORM.                    " GET_SELECTED_ROW
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data .
  DATA: l_cnt     TYPE i,
        lt_rows   TYPE lvc_t_row,
        ls_rows   TYPE lvc_s_row.

  DATA: ls_techn_control  TYPE  bapideliciouscontrol.

  DATA: ls_return TYPE bapiret2,
        lt_return TYPE TABLE OF bapiret2.

  DATA: ls_vbak TYPE vbak,
        lt_vbak TYPE TABLE OF vbak,
        ls_vbap TYPE vbap,
        lt_vbap TYPE TABLE OF vbap.

  DATA: ls_sitem TYPE bapidlvreftosalesorder,
        lt_sitem TYPE TABLE OF bapidlvreftosalesorder.

  DATA: ls_citem TYPE bapidlvitemcreated,
        lt_citem TYPE TABLE OF bapidlvitemcreated.

  FIELD-SYMBOLS <record> LIKE gs_record.

*... Get selected row.
  PERFORM get_selected_row USING g_grid
                                 lt_rows[].

  IF lines( lt_rows ) > 0.
    CLEAR: ls_sitem, lt_sitem[].

    LOOP AT lt_rows INTO ls_rows WHERE index > 0.
      READ TABLE gt_record ASSIGNING <record> INDEX ls_rows-index.

      IF <record>-icon EQ  icon_red_light.
        MESSAGE s000 DISPLAY LIKE 'E' WITH text-m04.
        RETURN.
      ENDIF.

      IF <record>-vbeln IS NOT INITIAL.
        SELECT SINGLE * INTO ls_vbak
               FROM vbak
               WHERE vbeln = <record>-vbeln.

        SELECT SINGLE * INTO ls_vbap
               FROM vbap
               WHERE vbeln = <record>-vbeln
                 AND posnr = <record>-posnr.

        ls_sitem-ref_doc  = ls_vbap-vbeln.
        ls_sitem-ref_item = ls_vbap-posnr.
        ls_sitem-dlv_qty  = <record>-post_no.
        ls_sitem-sales_unit  = ls_vbap-vrkme.
        APPEND ls_sitem TO lt_sitem.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
      TABLES
        sales_order_items = lt_sitem
        created_items     = lt_citem
        return            = lt_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      LOOP AT lt_rows INTO ls_rows.
        READ TABLE gt_record ASSIGNING <record> INDEX ls_rows-index.
        <record>-icon = icon_red_light.
        <record>-message = ls_return-message.
      ENDLOOP.
    ELSE.
      LOOP AT lt_rows INTO ls_rows.
        READ TABLE gt_record ASSIGNING <record> INDEX ls_rows-index.
        <record>-icon = icon_green_light.

        READ TABLE lt_citem INTO ls_citem INDEX 1.
        <record>-doc_num = ls_citem-deliv_numb.
      ENDLOOP.
    ENDIF.
  ELSE.
    MESSAGE s000 DISPLAY LIKE 'E' WITH text-m01.
    EXIT.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_sort .

  CLEAR: gs_alv_sort, gt_alv_sort[].

  gs_alv_sort-spos = 1.
  gs_alv_sort-fieldname = 'MATNR'.
  gs_alv_sort-up = 'X'.
  APPEND gs_alv_sort TO gt_alv_sort.

  gs_alv_sort-spos = 2.
  gs_alv_sort-fieldname = 'MAKTX'.
  gs_alv_sort-up = 'X'.
  APPEND gs_alv_sort TO gt_alv_sort.
ENDFORM.                    " ALV_SORT
