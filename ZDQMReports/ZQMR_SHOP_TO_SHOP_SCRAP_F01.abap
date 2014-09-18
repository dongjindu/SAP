*&---------------------------------------------------------------------*
*&  Include           ZQM_SHOP_TO_SHOP_SCRAP_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  POV_QMGRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pov_qmgrp .
  DATA: ls_value TYPE zsqm_qmgrp,
        lt_value LIKE TABLE OF ls_value.
  DATA: ls_return    TYPE ddshretval,
        lt_return    TYPE TABLE OF ddshretval.

  ls_value-qmgrp = 'MXB*'.
  APPEND ls_value TO lt_value.

  ls_value-qmgrp = 'MXE*'.
  APPEND ls_value TO lt_value.

  ls_value-qmgrp = 'MXG*'.
  APPEND ls_value TO lt_value.

  ls_value-qmgrp = 'MXP*'.
  APPEND ls_value TO lt_value.

  ls_value-qmgrp = 'MXS*'.
  APPEND ls_value TO lt_value.

  ls_value-qmgrp = 'MXT*'.
  APPEND ls_value TO lt_value.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'QMGRP'
      window_title    = 'Code Group - Coding'
      value_org       = 'S'
    TABLES
      value_tab       = lt_value
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  READ TABLE lt_return INTO ls_return INDEX 1.

  IF sy-subrc EQ 0.
    s_qmgrp-low = ls_return-fieldval.
    s_qmgrp-sign = 'I'.
    s_qmgrp-option = 'CP'.
    APPEND s_qmgrp.
  ENDIF.

ENDFORM.                    " POV_QMGRP
*&---------------------------------------------------------------------*
*&      Form  SET_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_initial .
  IF s_qmkat[] IS INITIAL.
    s_qmkat-low    ='X'.
    s_qmkat-sign   = 'I'.
    s_qmkat-option = 'EQ'.
    APPEND s_qmkat.
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
  DATA: lran_qmgrp LIKE LINE OF gran_qmgrp.

  DATA: ls_makt TYPE makt,
        lt_makt LIKE TABLE OF ls_makt.

  DATA: ls_qpgt TYPE qpgt,
        lt_qpgt LIKE TABLE OF ls_qpgt.

  DATA: ls_qpct TYPE qpct,
        lt_qpct LIKE TABLE OF ls_qpct.

  SORT s_qmgrp  BY low.

  DELETE ADJACENT DUPLICATES FROM s_qmgrp COMPARING low.

  SELECT codegruppe AS low INTO CORRESPONDING FIELDS OF TABLE gran_qmgrp
    FROM qpgt
   WHERE katalogart IN s_qmkat
     AND codegruppe IN s_qmgrp.

  SORT gran_qmgrp BY low.

  lran_qmgrp-sign = 'I'.
  lran_qmgrp-option = 'EQ'.

  MODIFY gran_qmgrp FROM lran_qmgrp TRANSPORTING sign option
   WHERE low <> ''.

  LOOP AT s_qmgrp.
    CASE s_qmgrp-low.
      WHEN 'MXB*'.
        add_fecod: '2001', '2004', '2005', '2006', '2007',
                   '2008', '2010', '2012'.
      WHEN 'MXE*'.
        add_fecod: '5004', '5005', '5006', '5007', '5008',
                   '5010', '7004', '7005', '7006', '7007',
                   '7008', '7010'.
      WHEN 'MXG*'.

      WHEN 'MXP*'.
        add_fecod: '3004', '3005', '3006', '3007', '3008',
                   '3010', '3011', '3012', '3013', '3014',
                   '3016'.
      WHEN 'MXS*'.
        add_fecod: '1001', '1002', '1003', '1004', '1005',
                   '1006', '1007', '1008', '1010', '1011',
                   '1012', '1013', '1014', '1015', '1016',
                   '1017', '1018', '1019', '1021', '1022',
                   '1023', '1026', '1027', '1028', '1029',
                   '1030'.
      WHEN 'MXT*'.
        add_fecod: '4005', '4006', '4007', '4008', '4010',
                   '4011', '4013'.
    ENDCASE.
  ENDLOOP.

  SELECT a~qmnum a~qmart a~rkmng a~qmgrp a~qmkat a~qmdat a~mawerk a~mzeit  a~matnr a~mgein
         a~lifnum a~ernam
         b~fecod b~fegrp b~fekat b~fever b~qmnum
*         c~code c~codegruppe c~katalogart c~kurztext AS kurztext2 c~version
*         d~kurztext AS kurztext1
*         d~kurztext AS kurztext1 e~maktx
    INTO CORRESPONDING FIELDS OF TABLE gt_record
    FROM qmel AS a INNER JOIN qmfe AS b
      ON a~qmnum = b~qmnum
*   INNER JOIN qpct AS c
*      ON c~katalogart = b~fekat
*     AND c~codegruppe = b~fegrp
*     AND c~code = b~fecod
*     AND c~version = b~fever
*   INNER JOIN qpgt AS d
*      ON b~fekat = d~katalogart
*    AND b~fegrp  = d~codegruppe
*   INNER JOIN makt AS e
*      ON a~matnr = e~matnr
   WHERE a~ernam  IN s_ernam
     AND a~lifnum IN s_lifnum
     AND a~matnr  IN s_matnr
     AND a~mawerk IN s_mawerk
     AND a~qmart  IN s_qmart
     AND a~qmdat  IN s_qmdat
     AND a~qmgrp  IN gran_qmgrp
     AND a~qmkat  IN s_qmkat
     AND a~qmnum  IN s_qmnum
     AND b~fecod  IN gran_fecod
     AND b~fegrp  IN s_fegrp.

  CHECK lines( gt_record ) > 0.

  SELECT matnr maktx INTO CORRESPONDING FIELDS OF TABLE lt_makt
    FROM makt
     FOR ALL ENTRIES IN gt_record
   WHERE matnr EQ  gt_record-matnr.

  SORT lt_makt BY matnr.

  SELECT katalogart codegruppe kurztext INTO CORRESPONDING FIELDS OF TABLE lt_qpgt
    FROM qpgt
     FOR ALL ENTRIES IN gt_record
   WHERE katalogart = gt_record-fekat
     AND codegruppe = gt_record-fegrp.

  SORT lt_qpgt BY katalogart codegruppe.

  SELECT katalogart codegruppe code version kurztext
    INTO CORRESPONDING FIELDS OF TABLE lt_qpct
    FROM qpct
     FOR ALL ENTRIES IN gt_record
   WHERE katalogart = gt_record-fekat
     AND codegruppe = gt_record-fegrp
     AND code       = gt_record-fecod
     AND version    = gt_record-fever.

  SORT lt_qpct BY katalogart codegruppe code version.

  LOOP AT gt_record INTO gs_record.
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = gs_record-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_record-maktx = ls_makt-maktx.
    ENDIF.

    READ TABLE lt_qpgt INTO ls_qpgt WITH KEY katalogart = gs_record-fekat
                                             codegruppe = gs_record-fegrp
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_record-kurztext1 = ls_qpgt-kurztext.
    ENDIF.

    READ TABLE lt_qpct INTO ls_qpct WITH KEY katalogart = gs_record-fekat
                                             codegruppe = gs_record-fegrp
                                             code       = gs_record-fecod
                                             version    = gs_record-fever
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_record-code        = ls_qpct-code.
      gs_record-codegruppe  = ls_qpct-codegruppe.
      gs_record-katalogart  = ls_qpct-katalogart.
      gs_record-kurztext2   = ls_qpct-kurztext.
      gs_record-version     = ls_qpct-version.
    ENDIF.

    MODIFY gt_record FROM gs_record.
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
FORM build_fieldcat CHANGING pt_fieldcat  TYPE lvc_t_fcat.

  DATA: l_pos TYPE i.

  FIELD-SYMBOLS <fcat> LIKE lvc_s_fcat.

  l_pos = 0.

  CLEAR: pt_fieldcat[].


  PERFORM alv_create_field_catalog USING:
     pt_fieldcat  'QMNUM'      'notif. Number'          'X' l_pos ' ',
     pt_fieldcat  'MAWERK'     'Plant.'                 'X' l_pos ' ',
     pt_fieldcat  'MATNR'      'Material no.'           'X' l_pos ' ',
     pt_fieldcat  'MAKTX'      'Material Text'          'X' l_pos ' ',
     pt_fieldcat  'RKMNG'      'Comp. Quantity'         'X' l_pos ' ',
     pt_fieldcat  'MGEIN'      'UoM'                    ' ' l_pos ' ',
     pt_fieldcat  'LIFNUM'     'Vendor'                 ' ' l_pos ' ',
     pt_fieldcat  'ERNAM'      'Created by'             ' ' l_pos ' ',
     pt_fieldcat  'QMDAT'      'Date Created'           ' ' l_pos ' ',
     pt_fieldcat  'MZEIT'      'Time'                   ' ' l_pos ' ',
     pt_fieldcat  'QMKAT'      'Catalog Type'           ' ' l_pos ' ',
     pt_fieldcat  'QMGRP'      'Dept Created'           ' ' l_pos ' ',
     pt_fieldcat  'FEKAT'      'Cat Type defect'        ' ' l_pos ' ',
     pt_fieldcat  'FEGRP'      'Error Dept GRP'         ' ' l_pos ' ',
     pt_fieldcat  'FECOD'      'Problem Code'           ' ' l_pos ' ',
     pt_fieldcat  'KURZTEXT1'  'Description Code Text'  ' ' l_pos ' ',
     pt_fieldcat  'KURZTEXT2'  'Short Text for Code'    ' ' l_pos ' '.

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
*      -->P_0598   text
*      -->P_0599   text
*      -->P_0600   text
*      -->P_L_POS  text
*      -->P_0602   text
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

*  gs_alv_sort-spos = 1.
*  gs_alv_sort-fieldname = 'MATNR'.
*  gs_alv_sort-up = 'X'.
*  APPEND gs_alv_sort TO gt_alv_sort.
*
*  gs_alv_sort-spos = 2.
*  gs_alv_sort-fieldname = 'MAKTX'.
*  gs_alv_sort-up = 'X'.
*  APPEND gs_alv_sort TO gt_alv_sort.
ENDFORM.                    " ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_CUSTOM_CONTAINER  text
*      -->P_G_CONTAINER  text
*----------------------------------------------------------------------*
FORM create_dynamic_container   USING us_container TYPE REF TO cl_gui_custom_container
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
