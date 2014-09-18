*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200' WITH sy-datum sy-uzeit.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM exclude_tb_functions.
    PERFORM build_field_catalog USING 'IT_OUTPUT'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    wa_stbl-row = 'X'.
    wa_stbl-col = 'X'.
    CALL METHOD alv_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stbl.
  ENDIF.
  IF  grid_container_tot IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object_tot.
    PERFORM set_attributes_alv_grid_tot.
    PERFORM exclude_tb_functions_tot.
    PERFORM build_field_catalog_tot USING 'IT_TOTAL'.
    PERFORM assign_itab_to_alv_total.
  ELSE.
    wa_stbl-row = 'X'.
    CALL METHOD alv_grid_tot->refresh_table_display
      EXPORTING
        is_stable = wa_stbl.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  DATA:   w_repid LIKE sy-repid.
  CREATE OBJECT grid_container
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = grid_container
      i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  IF sy-tcode = 'ZAPP_ASMP_PIR'.
    wa_is_layout-edit       = 'X'.      "/Edit Mode Enable
  ELSE.
    wa_is_layout-edit       = ' '.
  ENDIF.
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = ' '.   "/optimizes the column width
*  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.   "Row Color
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  wa_is_layout-stylefname = 'CELLTAB'.
  wa_is_layout-ctab_fname = 'CELLCOLOR'. "Cell Color
*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.

*
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'WERKS'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.
  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8),
        l_cn(2) TYPE n.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MAKTX'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'MATNR_S'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'NO_OUT'      'X',
                                  ' ' 'COLTEXT'     'Material',
                                   'E' 'OUTPUTLEN'   '18',

                                 'S' 'DESC'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Element',
                                  'E' 'OUTPUTLEN'   '9'.


  WRITE w_d-1 TO l_datum MM/DD/YY.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                     'S' 'MTD'        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
                                     ' ' 'COLTEXT'     'Total Req.',
                                     ' ' 'DECIMALS_O'  '0',
                                     ' ' 'KEY'         'X',
                                     ' ' 'NO_ZERO'     'X',
                                     'E' 'OUTPUTLEN'   '6',



                                     'S' 'D-1'        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
*                                     ' ' 'COLTEXT'     L_DATUM(5),

                                     ' ' 'COLTEXT'     'D-1 G/I',
                                     ' ' 'DECIMALS_O'  '0',
                                     ' ' 'NO_ZERO'     'X',
                                     'E' 'OUTPUTLEN'   '6',

                                     'S' 'DUE_IN'        ' ',
                                     ' ' 'COLTEXT'     'N-S/O',
                                     ' ' 'DECIMALS_O'  '0',
                                     ' ' 'NO_ZERO'     'X',
                                     'E' 'OUTPUTLEN'   '6',

                                     'S' 'PAST_DUE'        ' ',
                                     ' ' 'COLTEXT'     'Past Due',
                                     ' ' 'DECIMALS_O'  '0',
                                     ' ' 'NO_ZERO'     'X',
                                     'E' 'OUTPUTLEN'   '6'.


  LOOP AT it_day.
    CONCATENATE 'QTYD_' it_day-seq INTO l_rqty.
    WRITE it_day-datum TO l_datum MM/DD/YY.

    PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                   'S' l_rqty        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
                                   ' ' 'COLTEXT'     l_datum(5),
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '6'.
    CLEAR: l_rqty.
  ENDLOOP.

  LOOP AT it_week.
    CONCATENATE 'QTYW_' it_week-seq INTO l_rqty.
    WRITE it_week-datum TO l_datum MM/DD/YY.

    PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                   'S' l_rqty        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
                                   ' ' 'COLTEXT'     l_datum(5),
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '6'.
    CLEAR: l_rqty.
  ENDLOOP.
ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).
  FIELD-SYMBOLS <fs>.

  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.
    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.
    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

** ENTER
  CALL METHOD alv_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  CALL METHOD alv_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->handle_data_changed FOR alv_grid.


  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = alv_grid.

  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
      it_toolbar_excluding = it_exclude
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_output[].
*               it_sort          = it_sort[].


ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_tb_functions.
  DATA ls_exclude TYPE ui_func.

* Row manipulation
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO it_exclude.

*  Sort buttons
  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_asc.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc.
  APPEND ls_exclude TO it_exclude.
**  This excludes all buttons
*  LS_EXCLUDE = '&EXCLALLFC'.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT_TOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object_tot.
  DATA:   w_repid LIKE sy-repid.
  CREATE OBJECT grid_container_tot
    EXPORTING
      container_name              = wa_custom_control_tot
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid_tot
    EXPORTING
      i_parent      = grid_container_tot
      i_appl_events = 'X'.


ENDFORM.                    " CREATE_CONTAINER_N_OBJECT_TOT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_TOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid_tot.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_tot_layout-edit       = ' '.      "/Edit Mode Enable
  wa_tot_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_tot_layout-language   = sy-langu. "/Language Key
  wa_tot_layout-cwidth_opt = ' '.   "/optimizes the column width
  wa_tot_layout-info_fname = 'IF'.
  wa_tot_layout-ctab_fname = 'CELLCOLOR'.
*//-- Set Variant Structure
  wa_tot_variant-report       = sy-repid.
  wa_tot_variant-username     = sy-uname.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_TOT
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv_total.
  CALL METHOD alv_grid_tot->set_table_for_first_display
    EXPORTING
      is_layout            = wa_tot_layout
      i_save               = wa_tot_save
      is_variant           = wa_tot_variant
*     i_default            = space
      it_toolbar_excluding = it_exclude_tot
    CHANGING
      it_fieldcatalog      = it_fieldcat_tot[]
      it_outtab            = it_total[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV_TOTAL
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_TOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0044   text
*----------------------------------------------------------------------*
FORM build_field_catalog_tot USING p_itab.

  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8),
        l_cn(2) TYPE n.

  CLEAR: it_fieldcat_tot,  it_fieldcat_tot[],
         it_fname_tot, it_fname_tot[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fname_tot.

  PERFORM setting_fieldcat_tot TABLES it_fieldcat_tot USING :
                                  'S' 'MAKTX'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     '   ',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'MATNR_S'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'NO_OUT'      'X',
                                  ' ' 'COLTEXT'     '  ',
                                   'E' 'OUTPUTLEN'   '18',

                                  'S' 'DESC'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Element',
                                  'E' 'OUTPUTLEN'   '9'.

  WRITE w_d-1 TO l_datum MM/DD/YY.
  PERFORM setting_fieldcat_tot TABLES it_fieldcat_tot USING :

                                     'S' 'MTD'        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
                                     ' ' 'COLTEXT'     'Total Req.',
                                     ' ' 'DECIMALS_O'  '0',
                                     ' ' 'KEY'         'X',
                                     ' ' 'NO_ZERO'     'X',
                                     'E' 'OUTPUTLEN'   '6',


                                     'S' 'D-1'        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
*                                     ' ' 'COLTEXT'     L_DATUM(5),

                                     ' ' 'COLTEXT'     'D-1 G/I',
                                     ' ' 'DECIMALS_O'  '0',
                                     ' ' 'NO_ZERO'     'X',
                                     'E' 'OUTPUTLEN'   '6',

                                     'S' 'DUE_IN'        ' ',
                                     ' ' 'COLTEXT'     'N-S/O',
                                     ' ' 'DECIMALS_O'  '0',
                                     ' ' 'NO_ZERO'     'X',
                                     'E' 'OUTPUTLEN'   '6',

                                     'S' 'PAST_DUE'        ' ',
                                     ' ' 'COLTEXT'     'Past Due',
                                     ' ' 'DECIMALS_O'  '0',
                                     ' ' 'NO_ZERO'     'X',
                                     'E' 'OUTPUTLEN'   '6'.



  LOOP AT it_day.
    CONCATENATE 'QTYD_' it_day-seq INTO l_rqty.
    WRITE it_day-datum TO l_datum MM/DD/YY.

    PERFORM setting_fieldcat_tot TABLES it_fieldcat_tot USING :

                                   'S' l_rqty        ' ',
                                   ' ' 'COLTEXT'     l_datum(5),
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '6'.
    CLEAR: l_rqty.
  ENDLOOP.

  LOOP AT it_week.
    CONCATENATE 'QTYW_' it_week-seq INTO l_rqty.
    WRITE it_week-datum TO l_datum MM/DD/YY.

    PERFORM setting_fieldcat_tot TABLES it_fieldcat_tot USING :

                                   'S' l_rqty        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
                                   ' ' 'COLTEXT'     l_datum(5),
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '6'.
    CLEAR: l_rqty.
  ENDLOOP.


ENDFORM.                    " BUILD_FIELD_CATALOG_TOT
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT_tot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT_TOT  text
*      -->P_0759   text
*      -->P_0760   text
*      -->P_0761   text
*----------------------------------------------------------------------*
FORM setting_fieldcat_tot TABLES   p_fieldcat STRUCTURE it_fieldcat
                           USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fname_tot INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " SETTING_FIELDCAT_tot

*---------------------------------------------------------------------*
*       FORM EXCLUDE_TB_FUNCTIONS_tot                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM exclude_tb_functions_tot.
  DATA ls_exclude TYPE ui_func.

**  This excludes all buttons
  ls_exclude = '&EXCLALLFC'.
  APPEND ls_exclude TO it_exclude_tot.
ENDFORM.                    " EXCLUDE_TB_FUNCTIONS_tot
*&---------------------------------------------------------------------*
*&      Module  INIT_PRDT_GROUP  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_data OUTPUT.
  IF w_flag IS INITIAL.
    w_flag = 'X'.
    PERFORM set_data.
    PERFORM set_weeks.
    PERFORM set_days.
    PERFORM make_dropdown_list_box.
    PERFORM make_dropdown_list_box_plant.
  ENDIF.
ENDMODULE.                 " INIT_PRDT_GROUP  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_dropdown_list_box.
  CLEAR : xlist[] , xvalue.

  SELECT MATNR AS KEY
    INTO CORRESPONDING FIELDS OF TABLE xLIST
    FROM MARA
   WHERE MTART = 'PROD'
     AND MATNR LIKE '%PNL'
     and MATNR not LIKE 'MV%'.

*  xvalue-text = 'AS-PNL'.
*  xvalue-key  = 'AS-PNL'.
*  APPEND xvalue TO xlist .
**
*  xvalue-text = 'MP-PNL'.
*  xvalue-key  = 'MP-PNL'.
*  APPEND xvalue TO xlist .

  PERFORM list_box_function USING 'W_PRDT'.
  READ TABLE xlist INTO xvalue  INDEX 1.
  w_prdt = xvalue-key.

ENDFORM.                    " make_dropdown_list_box

*---------------------------------------------------------------------*
*       FORM list_box_function                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_LIST_NAME                                                   *
*---------------------------------------------------------------------*
FORM list_box_function USING   p_list_name .
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = p_list_name  " list box
      values          = xlist
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
ENDFORM.                    " list_box_function
*&---------------------------------------------------------------------*
*&      Form  MAKE_DROPDOWN_LIST_BOX_PLANT
*&---------------------------------------------------------------------*
FORM make_dropdown_list_box_plant .
  DATA: lt_werks LIKE TABLE OF t001w WITH HEADER LINE.

  CLEAR : xlist[] , xvalue.

  SELECT * INTO TABLE lt_werks
  FROM t001w
  WHERE WERKS LIKE 'P%'.

  LOOP AT lt_werks.
    xvalue-text = lt_werks-name1.
    xvalue-key  = lt_werks-werks.
    APPEND xvalue TO xlist .
  ENDLOOP.

  PERFORM list_box_function USING 'W_WERKS'.
  if w_werks is INITIAL.
    READ TABLE xlist INTO xvalue  INDEX 1.
    w_werks = xvalue-key.
  endif.
ENDFORM.                    " MAKE_DROPDOWN_LIST_BOX_PLANT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.

*  AUTHORITY-CHECK OBJECT 'Z:PP_ASMP_PC'
*                  ID 'ACTVT' FIELD '01'.
  select single * from agr_users
  where agr_name = 'Z:PP_ASMP_PC'
    and uname    = sy-uname
    and to_dat   = '99991231'.

  if sy-subrc <> 0.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'G1'.
        SCREEN-INVISIBLE = 1.
        SCREEN-ACTIVE    = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    CALL METHOD ALV_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.

  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
