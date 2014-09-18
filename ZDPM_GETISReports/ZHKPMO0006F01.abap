*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0006F01
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
  DATA : l_sttxt LIKE rihafvr-sttxt ,
         l_flag .

  CLEAR : gt_data[] .

*. Get row data : Equipment master data
  SELECT ui~eqtyp
         ui~equnr
         kt~eqktx
         ui~eqart
         ui~groes
         ui~inbdt
         ui~answt
         ui~waers
         ui~ansdt
         ui~herst
         ui~herld
         ui~typbz
         ui~baujj
         ui~baumm
         ui~objnr
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM equi AS ui INNER JOIN eqkt AS kt
                 ON kt~equnr = ui~equnr
                AND kt~spras = sy-langu
   WHERE ui~equnr IN s_equnr
     AND ui~eqtyp IN s_eqtyp
     AND kt~eqktx IN s_eqktx .


  LOOP AT gt_data .

    CLEAR : l_sttxt ,
            l_flag .

    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        flg_user_stat    = 'X'
        objnr            = gt_data-objnr
        only_active      = ' '
        spras            = sy-langu
      IMPORTING
        line             = l_sttxt
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    PERFORM read_status USING : l_sttxt
                                'DLFL'
                                l_flag ,
                                l_sttxt
                                'INAC'
                                l_flag .
    IF l_flag = c_x .
      DELETE gt_data .
    ENDIF .

  ENDLOOP .

  CHECK gt_data[] IS NOT INITIAL .
*.
  DATA : lt_equz LIKE TABLE OF equz WITH HEADER LINE ,
         lt_iloa LIKE TABLE OF iloa WITH HEADER LINE .

  DATA : lt_t001k LIKE TABLE OF t001k WITH HEADER LINE .

  SELECT * INTO TABLE lt_t001k
    FROM t001k .
  SORT lt_t001k BY bwkey .

*.. Equipment time segment
  SELECT equnr iloan
    INTO CORRESPONDING FIELDS OF TABLE lt_equz
    FROM equz
    FOR ALL ENTRIES IN gt_data
   WHERE equnr EQ gt_data-equnr
     AND datbi EQ '99991231' .

*.. PM Object Location and Account Assignment
  IF lt_equz[] IS NOT INITIAL .
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_iloa
      FROM iloa
      FOR ALL ENTRIES IN lt_equz
     WHERE iloan EQ lt_equz-iloan
       AND swerk IN s_werks
       AND beber IN s_beber .
  ENDIF .

  SORT lt_equz BY equnr .
  SORT lt_iloa BY iloan .

  LOOP AT gt_data .

    READ TABLE lt_equz WITH KEY equnr = gt_data-equnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .

      READ TABLE lt_iloa WITH KEY iloan = lt_equz-iloan
                                  BINARY SEARCH .
      IF sy-subrc = 0 .
        gt_data-tplnr = lt_iloa-tplnr .
        gt_data-swerk = lt_iloa-swerk .
        gt_data-stort = lt_iloa-stort .
        gt_data-beber = lt_iloa-beber .
        gt_data-anlnr = lt_iloa-anlnr .
        gt_data-anlun = lt_iloa-anlun .
      ELSE .
        DELETE gt_data . CONTINUE .
      ENDIF .

    ELSE .
      DELETE gt_data . CONTINUE .
    ENDIF .

    READ TABLE lt_t001k WITH KEY bwkey = gt_data-swerk
                                 BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-bukrs = lt_t001k-bukrs .
    ENDIF .

    MODIFY gt_data .
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

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_status USING    p_sttxt
                           p_at
                           p_flag.

  SEARCH p_sttxt FOR p_at .
  IF sy-subrc = 0 .
    p_flag = c_x .
  ENDIF .

ENDFORM.                    " READ_STATUS
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_container_object  USING    p_dynnr.

  CREATE OBJECT g_alv_doc_mdat
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      side      = g_alv_doc_mdat->dock_at_left
      extension = 2700.  "1500.

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

  l_tabname = 'ZHKPMS0010' .

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
      WHEN 'EQTYP'.
        st_field-reptext   = 'C'.
        st_field-scrtext_m = 'C'.
        st_field-coltext   = 'C'.
        st_field-key       = 'X'.
        st_field-outputlen = 2 .
*        st_field-just      = c_c .
*        st_field-no_out    = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'EQUNR' .
        st_field-reptext   = 'Equipment' .
        st_field-scrtext_m = 'Equipment' .
        st_field-coltext   = 'Equipment' .
        st_field-key       = 'X'.
        st_field-edit      = ' '.
        MODIFY gt_field FROM st_field.

      WHEN 'EQKTX'.
        st_field-reptext   = 'Description' .
        st_field-scrtext_m = 'Description' .
        st_field-coltext   = 'Description' .
        st_field-key       = ' '.
        st_field-edit      = ' '.
        MODIFY gt_field FROM st_field.

      WHEN 'TPLNR'.
        st_field-reptext   = 'Functional Loc.'.
        st_field-scrtext_m = 'Functional Loc.'.
        st_field-coltext   = 'Functional Loc.'.
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'SWERK'.
        st_field-reptext   = 'Plant'.
        st_field-scrtext_m = 'Plant'.
        st_field-coltext   = 'Plant'.
        st_field-key       = ' '.
        MODIFY gt_field FROM st_field.
      WHEN 'STORT'.
        st_field-reptext   = 'Location'.
        st_field-scrtext_m = 'Location'.
        st_field-coltext   = 'Location'.
        st_field-key       = ' '.
*        st_field-outputlen = 10 .
        MODIFY gt_field FROM st_field.
      WHEN 'BEBER'.
        st_field-reptext   = 'P/S' .
        st_field-scrtext_m = 'P/S' .
        st_field-coltext   = 'P/S' .
        st_field-key       = ' '.
        MODIFY gt_field FROM st_field.
      WHEN 'EQART'.
        st_field-reptext   = 'Object Type'.
        st_field-scrtext_m = 'Object Type'.
        st_field-coltext   = 'Object Type'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'ANLNR'.
        st_field-reptext   = 'Asset'.
        st_field-scrtext_m = 'Asset'.
        st_field-coltext   = 'Asset'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'ANLUN'.
        st_field-reptext   = 'AsubNo'.
        st_field-scrtext_m = 'AsubNo'.
        st_field-coltext   = 'AsubNo'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'GROES'.
        st_field-reptext   = 'Size/dimension'.
        st_field-scrtext_m = 'Size/dimension'.
        st_field-coltext   = 'Size/dimension'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'INBDT'.
        st_field-reptext   = 'Start-Up'.
        st_field-scrtext_m = 'Start-Up'.
        st_field-coltext   = 'Start-Up'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'ANSWT'.
        st_field-reptext   = 'Acquisition Value'.
        st_field-scrtext_m = 'Acquisition Value'.
        st_field-coltext   = 'Acquisition Value'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'WAERS' .
        st_field-reptext   = 'CrCy'.
        st_field-scrtext_m = 'CrCy'.
        st_field-coltext   = 'CrCy'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'ANSDT'.
        st_field-reptext   = 'Acq Date'.
        st_field-scrtext_m = 'Acq Date'.
        st_field-coltext   = 'Acq Date'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'HERST' .

        st_field-reptext   = 'Manufacturer'.
        st_field-scrtext_m = 'Manufacturer'.
        st_field-coltext   = 'Manufacturer'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'HERLD' .
        st_field-reptext   = 'Mctry'.
        st_field-scrtext_m = 'Mctry'.
        st_field-coltext   = 'Mctry'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'TYPBZ' .
        st_field-reptext   = 'Model Number'.
        st_field-scrtext_m = 'Model Number'.
        st_field-coltext   = 'Model Number'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'BAUJJ' .
        st_field-reptext   = 'ConY'.
        st_field-scrtext_m = 'ConY'.
        st_field-coltext   = 'ConY'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'BAUMM'.
        st_field-reptext   = 'conm'.
        st_field-scrtext_m = 'conm'.
        st_field-coltext   = 'conm'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
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

  SET HANDLER g_event_item->hotspot_click_item   FOR g_grid_mdat.
*  SET HANDLER G_EVENT_ITEM->DOUBLE_CLICK         FOR G_GRID_MDAT.
*  SET HANDLER G_EVENT_ITEM->TOOLBAR_ITEM         FOR G_GRID_MDAT.
*  SET HANDLER G_EVENT_ITEM->USER_COMMAND_ITEM    FOR G_GRID_MDAT .
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
*&      Form  PROCESS_DATA_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data_0100 .

  DATA : l_valid ,
         l_error .
  DATA : l_check_error .


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
    IF sy-subrc = 0 .

      PERFORM call_bdc .

      MODIFY gt_data INDEX g_rows_s-index.
    ENDIF .

  ENDLOOP .

ENDFORM.                    " PROCESS_DATA_0100
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc .
  DATA : lt_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE .
  DATA : l_answt(10) ,
         l_inbdt(10) ,
         l_ansdt(10) .

  CLEAR : gt_bdctab[] , gt_bdctab ,
          gt_msgtab[] , gt_msgtab .

  WRITE gt_data-answt TO l_answt CURRENCY gt_data-waers .

  IF gt_data-inbdt > '11111111' .
    WRITE gt_data-inbdt TO l_inbdt .
  ENDIF .
  IF gt_data-ansdt > '11111111' .
    WRITE gt_data-ansdt TO l_ansdt .
  ENDIF .

*. Set BDC
  PERFORM dynpro USING:
     'X' 'SAPMIEQ0'             '0100' ,
     ' ' 'BDC_OKCODE'           '/00' ,
     ' ' 'RM63E-EQUNR'          gt_data-equnr.

  PERFORM dynpro USING:
     'X' 'SAPMIEQ0'             '0101' .

  IF gt_data-bukrs = 'HA01' OR "HMMC
     gt_data-bukrs = 'K201' OR "KMMG
     gt_data-bukrs = 'KA01' .  "KMS
    PERFORM dynpro USING:
       ' ' 'BDC_OKCODE'           '=T\04' .
  ELSE .
    PERFORM dynpro USING:
       ' ' 'BDC_OKCODE'           '=T\03' .
  ENDIF .

  PERFORM dynpro USING:
     ' ' 'ITOB-EQART'           gt_data-eqart ,
*       ' ' 'ITOB-BRGEW'
*       ' ' 'ITOB-GEWEI'
     ' ' 'ITOB-GROES'           gt_data-groes ,
     ' ' 'ITOB-INBDT'           l_inbdt ,
     ' ' 'ITOB-ANSWT'           l_answt ,
     ' ' 'ITOB-WAERS'           gt_data-waers ,
     ' ' 'ITOB-HERST'           gt_data-herst ,
     ' ' 'ITOB-HERLD'           gt_data-herld ,
     ' ' 'ITOB-ANSDT'           l_ansdt ,
     ' ' 'ITOB-TYPBZ'           gt_data-typbz ,
     ' ' 'ITOB-BAUJJ'           gt_data-baujj ,
     ' ' 'ITOB-BAUMM'           gt_data-baumm ,

     'X' 'SAPMIEQ0'             '0101' ,
     ' ' 'BDC_OKCODE'           'BU' ,
     ' ' 'ITOB-ANLNR'           gt_data-anlnr ,
     ' ' 'ITOB-ANLUN'           gt_data-anlun .


  MOVE  : ' ' TO ctu_params-nobinpt,
          's' TO ctu_params-updmode,
          'x' TO ctu_params-defsize,
          c_n TO ctu_params-dismode.

  CALL TRANSACTION 'IE02' USING         gt_bdctab
                          OPTIONS FROM  ctu_params
                          MESSAGES INTO gt_msgtab.


  DESCRIBE TABLE gt_msgtab LINES sy-tabix.
  READ TABLE gt_msgtab INDEX sy-tabix.


*  '@08@'."  Green light; positive
*  '@09@'."  Yellow light; neutral
*  '@0A@'."  Red light; negative

  IF gt_msgtab-msgtyp EQ 'S'. " AND
*    ( gt_msgtab-msgnr  BETWEEN '100' AND '100' ) .
    gt_data-icon = '@08@' .
    CLEAR gt_data-mesg .
  ELSE.

    lt_return-id     = gt_msgtab-msgid .
    lt_return-number = gt_msgtab-msgnr .
    APPEND lt_return .
    CALL FUNCTION 'COM_READ_MESSAGE_TEXTS'
      EXPORTING
        iv_language = sy-langu
      CHANGING
        ct_return   = lt_return[].

    READ TABLE lt_return INDEX 1.
    gt_data-icon = '@0A@' .
    gt_data-mesg = lt_return-message.

  ENDIF .



ENDFORM.                    " CALL_BDC
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dynpro  USING    p_dynbegin
                      p_name
                      p_value.

  IF p_dynbegin EQ c_x .
    CLEAR gt_bdctab.
    MOVE : p_name  TO gt_bdctab-program,
           p_value TO gt_bdctab-dynpro,
           c_x   TO gt_bdctab-dynbegin.
    APPEND gt_bdctab.

  ELSE.
    CLEAR gt_bdctab.
    MOVE : p_name  TO gt_bdctab-fnam,
           p_value TO gt_bdctab-fval.
    APPEND gt_bdctab.

  ENDIF.

ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .

  st_functxt-icon_id     = icon_export.
  st_functxt-quickinfo   = 'Excel Upload '.
  st_functxt-icon_text   = 'Excel Upload '.
  sscrfields-functxt_01  = st_functxt.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_screen .

  CLEAR g_excel .

  CASE sy-ucomm .
    WHEN 'FC01'.

      PERFORM data_upload.

  ENDCASE .

ENDFORM.                    " SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  DATA_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_upload .

  DATA: l_filename LIKE rlgrap-filename.

  PERFORM get_filename  CHANGING l_filename.

  IF l_filename IS NOT INITIAL.
    g_filename = l_filename.
  ELSE .
    EXIT .
  ENDIF.

  PERFORM check_file_exist.

  PERFORM display_status USING text-m52.

  TRANSLATE l_filename TO UPPER CASE.

  PERFORM upload_excel_file TABLES gt_upload
                             USING l_filename
                                   '1'
                                   '1'.

  IF gt_upload[] IS NOT INITIAL.

    PERFORM move_to_list.
    g_excel = c_x.

    DESCRIBE TABLE gt_data LINES sy-ffile .
    MESSAGE s001 WITH sy-ffile 'has been viewed.' .

    CALL SCREEN 0100 .

  ELSE.
    MESSAGE e000 WITH text-m53.
  ENDIF.

ENDFORM.                    " DATA_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_filename  CHANGING p_file.
*
  DATA : l_title        TYPE string,
         l_mask         TYPE string,
         l_def_path     TYPE string,
         lt_files       TYPE filetable,
         ls_filename    TYPE file_table,
         l_rc           TYPE i VALUE 1,
         l_filter       TYPE string.

  CALL METHOD cl_gui_frontend_services=>directory_get_current
    CHANGING
      current_directory = l_def_path.

  CONCATENATE 'Excel (*.xls)|*.xls;*.xlsx|'
*              'Excel (*.xlsx)|*.xlsx|'
              'All Files (*.*)|*.*|'
         INTO l_mask.

  l_title  = 'File selection'.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = l_title
      default_filename  = space
      file_filter       = l_mask
      initial_directory = l_def_path
    CHANGING
      file_table        = lt_files
      rc                = l_rc.

  READ TABLE lt_files INTO ls_filename INDEX 1.

  p_file = ls_filename .
*

ENDFORM.                    " GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  CHECK_FILE_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_file_exist .

  DATA: l_return         TYPE i,
         l_wdy_mime_path  TYPE wdy_mime_path.

  l_wdy_mime_path = g_filename.

*  l_return = zcl_mm_excel_n=>check_filename( l_wdy_mime_path ).
*
*  IF l_return IS NOT INITIAL.
*    MESSAGE e000 WITH text-e23.
*  ENDIF.


ENDFORM.                    " CHECK_FILE_EXIST
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_status  USING    p_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 10
      text       = p_text
    EXCEPTIONS
      OTHERS     = 1.

ENDFORM.                    " DISPLAY_STATUS
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_EXCEL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload_excel_file  TABLES   pt_upload STRUCTURE  gt_upload
                        USING    p_filename
                                 p_value_1
                                 p_value_2.

  CLEAR : gt_upload[] .
  CALL FUNCTION 'Z_HKPM_EXCEL_UPLOAD'
    EXPORTING
      filename     = p_filename
      itab         = 'IT_UPLOAD'
      begin_line   = p_value_1
    TABLES
      outab        = pt_upload
    EXCEPTIONS
      upload_error = 1
      OTHERS       = 2.

  IF sy-subrc NE 0.
    MESSAGE e000 WITH text-e24.
  ENDIF.

ENDFORM.                    " UPLOAD_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  MOVE_TO_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_to_list .

  DATA : ls_usr01 LIKE usr01 .
  DATA : l_inbdt(10) .


  CLEAR : gt_data[], gt_data .


  SELECT SINGLE * INTO ls_usr01
    FROM usr01
   WHERE bname EQ sy-uname .

  LOOP AT gt_upload .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gt_upload-equnr
      IMPORTING
        output = gt_upload-equnr.

    REPLACE ALL OCCURRENCES OF ',' IN gt_upload-answt WITH  space .

    REPLACE ALL OCCURRENCES OF ',' IN gt_upload-inbdt WITH  space .
    REPLACE ALL OCCURRENCES OF '.' IN gt_upload-inbdt WITH  space .
    REPLACE ALL OCCURRENCES OF '/' IN gt_upload-inbdt WITH  space .
    REPLACE ALL OCCURRENCES OF '-' IN gt_upload-inbdt WITH  space .

    REPLACE ALL OCCURRENCES OF ',' IN gt_upload-ansdt WITH  space .
    REPLACE ALL OCCURRENCES OF '.' IN gt_upload-ansdt WITH  space .
    REPLACE ALL OCCURRENCES OF '/' IN gt_upload-ansdt WITH  space .
    REPLACE ALL OCCURRENCES OF '-' IN gt_upload-ansdt WITH  space .

    CLEAR : l_inbdt .
    PERFORM get_date_format_conversion USING gt_upload-inbdt
                                             ls_usr01-datfm
                                     CHANGING l_inbdt .
    IF l_inbdt IS NOT INITIAL .
      gt_upload-inbdt = l_inbdt .
    ENDIF .

    CLEAR : l_inbdt .
    PERFORM get_date_format_conversion USING gt_upload-ansdt
                                             ls_usr01-datfm
                                     CHANGING l_inbdt .
    IF l_inbdt IS NOT INITIAL .
      gt_upload-ansdt = l_inbdt .
    ENDIF .

    MODIFY gt_upload .
  ENDLOOP .
**.


  DATA : l_sttxt LIKE rihafvr-sttxt ,
         l_flag .

  CLEAR : gt_data[] .

*. Get row data : Equipment master data
  SELECT ui~eqtyp
         ui~equnr
         kt~eqktx
         ui~eqart
         ui~groes
         ui~inbdt
         ui~answt
         ui~waers
         ui~ansdt
         ui~herst
         ui~herld
         ui~typbz
         ui~baujj
         ui~baumm
         ui~objnr
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM equi AS ui INNER JOIN eqkt AS kt
                 ON kt~equnr = ui~equnr
                AND kt~spras = sy-langu
    FOR ALL ENTRIES IN gt_upload
   WHERE ui~equnr EQ gt_upload-equnr .

  LOOP AT gt_data .

    CLEAR : l_sttxt ,
            l_flag .

    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        flg_user_stat    = 'X'
        objnr            = gt_data-objnr
        only_active      = ' '
        spras            = sy-langu
      IMPORTING
        line             = l_sttxt
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    PERFORM read_status USING : l_sttxt
                                'DLFL'
                                l_flag ,
                                l_sttxt
                                'INAC'
                                l_flag .
    IF l_flag = c_x .
      DELETE gt_data .
    ENDIF .

  ENDLOOP .

  CHECK gt_data[] IS NOT INITIAL .
*.
  DATA : lt_equz LIKE TABLE OF equz WITH HEADER LINE ,
         lt_iloa LIKE TABLE OF iloa WITH HEADER LINE .

  DATA : lt_t001k LIKE TABLE OF t001k WITH HEADER LINE .

  SELECT * INTO TABLE lt_t001k
    FROM t001k .
  SORT lt_t001k BY bwkey .

*.. Equipment time segment
  SELECT equnr iloan
    INTO CORRESPONDING FIELDS OF TABLE lt_equz
    FROM equz
    FOR ALL ENTRIES IN gt_data
   WHERE equnr EQ gt_data-equnr .

*.. PM Object Location and Account Assignment
  IF lt_equz[] IS NOT INITIAL .
    SELECT iloan
           tplnr swerk stort beber
      INTO CORRESPONDING FIELDS OF TABLE lt_iloa
      FROM iloa
      FOR ALL ENTRIES IN lt_equz
     WHERE iloan EQ lt_equz-iloan .
  ENDIF .

  SORT lt_equz BY equnr .
  SORT lt_iloa BY iloan .
  SORT gt_upload BY equnr .

  LOOP AT gt_data .

    READ TABLE lt_equz WITH KEY equnr = gt_data-equnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .

      READ TABLE lt_iloa WITH KEY iloan = lt_equz-iloan
                                  BINARY SEARCH .
      IF sy-subrc = 0 .
        gt_data-tplnr = lt_iloa-tplnr .
        gt_data-swerk = lt_iloa-swerk .
        gt_data-stort = lt_iloa-stort .
        gt_data-beber = lt_iloa-beber .
      ENDIF .

    ENDIF .

    READ TABLE gt_upload WITH KEY equnr = gt_data-equnr
                                  BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-eqart = gt_upload-eqart .
      gt_data-anlnr = gt_upload-anlnr .
      gt_data-anlun = gt_upload-anlun .
      gt_data-groes = gt_upload-groes .
      gt_data-inbdt = gt_upload-inbdt .
      gt_data-answt = gt_upload-answt .
      gt_data-waers = gt_upload-waers .
      gt_data-ansdt = gt_upload-ansdt .
      gt_data-herst = gt_upload-herst .
      gt_data-herld = gt_upload-herld .
      gt_data-typbz = gt_upload-typbz .
      gt_data-baujj = gt_upload-baujj .
      gt_data-baumm = gt_upload-baumm .
    ENDIF .

    READ TABLE lt_t001k WITH KEY bwkey = gt_data-swerk
                                 BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-bukrs = lt_t001k-bukrs .
    ENDIF .

    MODIFY gt_data .
  ENDLOOP .

ENDFORM.                    " MOVE_TO_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_DATE_FORMAT_CONVERSION
*----------------------------------------------------------------------*
FORM get_date_format_conversion  USING    p_date
                                          p_datfm
                                 CHANGING p_indate .

  CASE p_datfm .
    WHEN '1' . "DD.MM.YYYY
      p_indate+0(4) = p_date+4(4) .
      p_indate+4(2) = p_date+2(2) .
      p_indate+6(2) = p_date+0(2) .

    WHEN '2' OR '3' . "MM/DD/YYYY
      p_indate+0(4) = p_date+4(4) .
      p_indate+4(2) = p_date+0(2) .
      p_indate+6(2) = p_date+2(2) .

    WHEN '5' OR '6' OR '7' . "YYYY.MM.DD
      p_indate = p_date .

  ENDCASE .

ENDFORM.                    " GET_DATE_FORMAT_CONVERSION
