*&---------------------------------------------------------------------*
*&  Include           ZQMR_COR_SCRAP_MATERIAL_F01
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
*... SET DEFAULT VALUE
  IF s_ccode  IS INITIAL.
    s_ccode-option = 'EQ'.
    s_ccode-sign   = 'I'.
    s_ccode-low    = 'QM02'.
    APPEND s_ccode.
  ENDIF.

  IF s_inact IS INITIAL.
    s_inact-option = 'NE'.
    s_inact-sign   = 'I'.
    s_inact-low    = 'X'.
    APPEND s_inact .
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

  DATA: l_cursor1  TYPE cursor,
        l_cursor2  TYPE cursor,
        l_maxsize1 TYPE  rsmaxsize,
        l_maxsize2 TYPE  rsmaxsize.

  DATA: lt_jcds TYPE TABLE OF jcds,
        ls_jcds TYPE jcds.

  DATA: lt_qmel TYPE TABLE OF qmel,
        ls_qmel TYPE qmel.

  DATA: ls_makt TYPE makt,
        lt_makt LIKE TABLE OF ls_makt.

  OPEN CURSOR WITH HOLD l_cursor1 FOR
  SELECT * FROM jcds
          WHERE stat    IN s_stat
            AND chgnr   IN s_chgnr
            AND udate   IN s_udate
            AND utime   IN s_utime
            AND cdtcode IN s_ccode
            AND inact   IN s_inact
            AND chind   IN s_chind.

  FETCH NEXT CURSOR l_cursor1
             APPENDING CORRESPONDING FIELDS
             OF TABLE  lt_jcds
             PACKAGE SIZE l_maxsize1.

  IF sy-subrc <> 0.
    CLOSE CURSOR l_cursor1.
  ENDIF.

  SORT lt_jcds BY objnr.

  CHECK lt_jcds[] IS NOT INITIAL.

  OPEN CURSOR WITH HOLD l_cursor2 FOR
  SELECT * FROM qmel FOR ALL ENTRIES IN lt_jcds
   WHERE qmnum EQ lt_jcds-objnr+2(12)
** Furong on 07/26/12
     and qmart = 'Q3'.
** End

  FETCH NEXT CURSOR l_cursor2
             APPENDING CORRESPONDING FIELDS
             OF TABLE  lt_qmel
             PACKAGE SIZE l_maxsize2.

  IF sy-subrc <> 0.
    CLOSE CURSOR l_cursor2.
  ENDIF.

  SORT lt_qmel BY qmnum.

  SELECT matnr maktx INTO CORRESPONDING FIELDS OF TABLE lt_makt
    FROM makt
     FOR ALL ENTRIES IN lt_qmel
   WHERE matnr EQ  lt_qmel-matnr.

  SORT lt_makt BY matnr.

  LOOP AT lt_jcds INTO ls_jcds.
    READ TABLE lt_qmel INTO ls_qmel WITH KEY qmnum = ls_jcds-objnr+2(12)
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_jcds TO gs_record.
      MOVE-CORRESPONDING ls_qmel TO gs_record.

      READ TABLE lt_makt INTO ls_makt WITH KEY matnr = gs_record-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_record-maktx = ls_makt-maktx.
      ENDIF.

      APPEND gs_record TO gt_record.
      CLEAR: gs_record, ls_qmel.
    ENDIF.
  ENDLOOP.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_qmel
*    FROM qmel
*    FOR ALL ENTRIES IN lt_jcds
*    WHERE qmnum EQ lt_jcds-objnr+2(12).
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
*  gs_layout-ctab_fname = 'TABCOLOR'.
*  gs_layout-stylefname = 'CELLTAB'.
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
     pt_fieldcat  'QMNUM'      'notif. Number'          'X' l_pos ' ',
     pt_fieldcat  'MATNR'      'Material no.'           'X' l_pos ' ',
     pt_fieldcat  'MAKTX'      'Material Text'          'X' l_pos ' ',
     pt_fieldcat  'RKMNG'      'Comp. Quantity'         'X' l_pos ' ',
     pt_fieldcat  'MGEIN'      'UoM'                    ' ' l_pos ' ',
     pt_fieldcat  'LIFNUM'     'Vendor'                 ' ' l_pos ' ',
     pt_fieldcat  'ERNAM'      'Created by'             ' ' l_pos ' ',
     pt_fieldcat  'QMDAT'      'Date Created'           ' ' l_pos ' ',
     pt_fieldcat  'MZEIT'      'Time'                   ' ' l_pos ' ',
     pt_fieldcat  'MAWERK'     'Plant.'                 ' ' l_pos ' ',
     pt_fieldcat  'STAT'       'Object status'          ' ' l_pos ' ',
     pt_fieldcat  'UDATE'      'Creation date of the change document' ' ' l_pos ' ',
     pt_fieldcat  'UTIME'      'Time changed'        ' ' l_pos ' ',
     pt_fieldcat  'CDTCODE'    'Transaction in which a change was made'   ' ' l_pos ' '.

  LOOP AT pt_fieldcat ASSIGNING <fcat>.
    CASE <fcat>-fieldname.
      WHEN 'QMNUM'.
        <fcat>-no_zero = 'X'.
      WHEN 'RFMNG'.
        <fcat>-qfieldname = 'MGEIN'.
        <fcat>-do_sum = 'X'.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ALV_CREATE_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_FIELDCAT  text
*      -->P_0408   text
*      -->P_0409   text
*      -->P_0410   text
*      -->P_L_POS  text
*      -->P_0412   text
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
FORM alv_exclude_functions  CHANGING pt_exclude TYPE ui_functions.

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
*&      Form  ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_sort .
  CLEAR: gs_alv_sort, gt_alv_sort[].
ENDFORM.                    " ALV_SORT

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
FORM create_dynamic_grid  USING us_grid TYPE REF TO cl_gui_alv_grid
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
*  CALL METHOD g_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*
*  CALL METHOD g_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*  CREATE OBJECT g_event_receiver
*    EXPORTING
*      i_repid = sy-repid.
*
*  SET HANDLER g_event_receiver->handle_data_changed  FOR g_grid.
*  SET HANDLER g_event_receiver->handle_hotspot_click  FOR g_grid.

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
