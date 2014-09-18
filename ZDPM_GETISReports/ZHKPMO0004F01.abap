*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0004F01
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
  DATA : lt_mbew LIKE TABLE OF mbew WITH HEADER LINE .
  DATA : lt_t001k LIKE TABLE OF t001k WITH HEADER LINE ,
         lt_t001  LIKE TABLE OF t001  WITH HEADER LINE .
  CLEAR : gt_data[], gt_data .

*. Get row data : Material master data
  SELECT rd~werks
         rd~lgort
         rd~matnr
         kt~maktx
         ra~meins
         ra~wrkst
         ra~matkl
         rd~labst
         ra~mtart
         rd~lminb
         rd~lbstf
         rd~lgpbe
         rc~maabc
         rd~lvorm
         ra~mhdrz
         ra~zeinr
         ra~ferth
         ra~mfrpn
         ra~pstat
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM mard AS rd INNER JOIN marc AS rc
                 ON rd~matnr = rc~matnr
                AND rd~werks = rc~werks
                    INNER JOIN mara AS ra
                 ON rd~matnr = ra~matnr
                    INNER JOIN makt AS kt
                 ON kt~matnr = rd~matnr
                AND kt~spras = sy-langu
   WHERE rd~werks IN s_werks
     AND rd~lgort IN s_lgort
     AND rd~matnr IN s_matnr
     AND rd~lvorm EQ space
     AND ra~mtart IN s_mtart .

  CHECK gt_data[] IS NOT INITIAL .

*.. T001K
  SELECT * INTO TABLE lt_t001k
   FROM t001k .

  SORT lt_t001k BY bwkey .

  SELECT * INTO TABLE lt_t001
    FROM t001 .

*.. Material Valuation
  SELECT * INTO TABLE lt_mbew
    FROM mbew
    FOR ALL ENTRIES IN gt_data
  WHERE matnr EQ gt_data-matnr
    AND bwkey EQ gt_data-werks .

  SORT lt_mbew BY matnr bwkey .
  SORT lt_t001 BY bukrs .

  LOOP AT gt_data .

    READ TABLE lt_mbew WITH KEY matnr = gt_data-matnr
                                bwkey = gt_data-werks
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-verpr = lt_mbew-verpr .
      gt_data-peinh = lt_mbew-peinh .
    ENDIF .

    IF gt_data-mtart = 'UNBW' .
      gt_data-mhdrz = gt_data-labst .
    ENDIF .

    READ TABLE lt_t001k WITH KEY bwkey = gt_data-werks
                                 BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-bukrs = lt_t001k-bukrs .
    ENDIF .

    READ TABLE lt_t001 WITH KEY bukrs = gt_data-bukrs .
    IF sy-subrc = 0 .
      gt_data-waers = lt_t001-waers .
    ENDIF .

    IF gt_data-bukrs = 'HA08' .
      gt_data-zeinr = gt_data-ferth .
    ENDIF .

    IF gt_data-bukrs = 'H201'  .
      gt_data-wrkst = gt_data-mfrpn .
    ENDIF .

      SEARCH gt_data-pstat FOR 'D' .
      IF sy-subrc <> 0 .
        gt_data-mesg = 'Change impossible "Maintain MRP View" and then try again' .
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

  l_tabname = 'ZHKPMS0020' .

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
      WHEN 'WERKS'.
        st_field-reptext   = 'Plant' .
        st_field-scrtext_m = 'Plant'.
        st_field-coltext   = 'Plant'.
        st_field-key       = 'X'.
        st_field-outputlen = 2 .
*        st_field-just      = c_c .
*        st_field-no_out    = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'LGORT' .
        st_field-reptext   = 'Sloc' .
        st_field-scrtext_m = 'Sloc' .
        st_field-coltext   = 'Sloc' .
        st_field-key       = 'X'.
        st_field-edit      = ' '.
        MODIFY gt_field FROM st_field.

      WHEN 'MATNR'.
        st_field-reptext   = 'Material' .
        st_field-scrtext_m = 'Material' .
        st_field-coltext   = 'Material' .
        st_field-key       = 'X'.
        st_field-edit      = ' '.
        st_field-hotspot   = 'X' .
        MODIFY gt_field FROM st_field.

      WHEN 'MAKTX'.
        st_field-reptext   = 'Description' .
        st_field-scrtext_m = 'Description' .
        st_field-coltext   = 'Description' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'MEINS'.
        st_field-reptext   = 'UOM' .
        st_field-scrtext_m = 'UOM' .
        st_field-coltext   = 'UOM' .
        st_field-key       = ' '.
        MODIFY gt_field FROM st_field.
      WHEN 'WRKST'.
        st_field-reptext   = 'SPEC' .
        st_field-scrtext_m = 'SPEC' .
        st_field-coltext   = 'SPEC' .
        st_field-key       = ' '.
*        st_field-outputlen = 10 .
        MODIFY gt_field FROM st_field.
      WHEN 'MATKL'.
        st_field-reptext   = 'MATGr' .
        st_field-scrtext_m = 'MATGr' .
        st_field-coltext   = 'MATGr' .
        st_field-key       = ' '.
        MODIFY gt_field FROM st_field.
      WHEN 'VERPR'.
        st_field-reptext   = 'Price' .
        st_field-scrtext_m = 'Price' .
        st_field-coltext   = 'Price' ..
        st_field-key       = ' '.
        st_field-edit      = ' '.
        MODIFY gt_field FROM st_field.
      WHEN 'WAERS'.
        st_field-reptext   = 'CrCv' .
        st_field-scrtext_m = 'CrCv' ..
        st_field-coltext   = 'CrCv' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'LABST'.
        st_field-reptext   = 'Stock' .
        st_field-scrtext_m = 'Stock' .
        st_field-coltext   = 'Stock' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'MTART'.
        st_field-reptext   = 'Mat Type'.
        st_field-scrtext_m = 'Mat Type'.
        st_field-coltext   = 'Mat Type'.
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'LMINB'.
        st_field-reptext   = 'Reorder point(MIN)' .
        st_field-scrtext_m = 'Reorder point(MIN)' .
        st_field-coltext   = 'Reorder point(MIN)' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'LBSTF' .
        st_field-reptext   = 'Reple Qty(Max)' .
        st_field-scrtext_m = 'Reple Qty(Max)' .
        st_field-coltext   = 'Reple Qty(Max)' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'LGPBE'.
        st_field-reptext   = 'BIN' .
        st_field-scrtext_m = 'BIN' .
        st_field-coltext   = 'BIN' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'MAABC' .

        st_field-reptext   = 'ABC' .
        st_field-scrtext_m = 'ABC' .
        st_field-coltext   = 'ABC' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'LVORM' .
        st_field-reptext   = 'Delete' .
        st_field-scrtext_m = 'Delete' .
        st_field-coltext   = 'Delete' .
        st_field-key       = ' '.
        st_field-edit      = ' '.
        st_field-checkbox  = c_x .
        st_field-just      = 'R' .
        MODIFY gt_field FROM st_field.
      WHEN 'MHDRZ' .
        st_field-reptext   = 'UNBW Stock' .
        st_field-scrtext_m = 'UNBW Stock' .
        st_field-coltext   = 'UNBW Stock' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'ZEINR' .
        st_field-reptext   = 'Maker' .
        st_field-scrtext_m = 'Maker' .
        st_field-coltext   = 'Maker' .
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
  SET HANDLER g_event_item->toolbar_item         FOR g_grid_mdat.
  SET HANDLER g_event_item->user_command_item    FOR g_grid_mdat .
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
*&      Form  PROCESS_COMM_A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_comm_a .

  DATA : l_answer .
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

  PERFORM answer_and_question USING text-q01
                                    text-q02
                                    ''
                              CHANGING l_answer .
  CHECK l_answer EQ c_j .


  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_data INDEX g_rows_s-index.
    IF sy-subrc = 0 AND gt_data-dupkey IS INITIAL .

      PERFORM call_bdc_mmab .

      MODIFY gt_data INDEX g_rows_s-index.
    ENDIF .

  ENDLOOP .

ENDFORM.                    " PROCESS_COMM_A
*&---------------------------------------------------------------------*
*&      Form  ANSWER_AND_QUESTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM answer_and_question  USING  p_qustion1
                                 p_qustion2
                                 p_title
                         CHANGING p_answer .
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1 = p_qustion1
      textline2 = p_qustion2
      titel     = p_title
    IMPORTING
      answer    = p_answer.

ENDFORM.                    " ANSWER_AND_QUESTION
*&---------------------------------------------------------------------*
*&      Form  GET_BAPI_DATA_0010
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bapi_data_0010 .

  DATA: ls_headdata             TYPE bapimathead,
        ls_clientdata           TYPE bapi_mara,
        ls_plantdata            TYPE bapi_marc,
        ls_forecastparameters   TYPE bapi_mpop,
        ls_planningdata         TYPE bapi_mpgd,
        ls_storagelocationdata  TYPE bapi_mard,
        ls_valuationdata        TYPE bapi_mbew,
        ls_warehousenumberdata  TYPE bapi_mlgn,
        ls_salesdata            TYPE bapi_mvke,
        ls_storagetypedata      TYPE bapi_mlgt,
        ls_clientdatax          TYPE bapi_marax,
        ls_plantdatax           TYPE bapi_marcx,
        ls_forecastparametersx  TYPE bapi_mpopx,
        ls_planningdatax        TYPE bapi_mpgdx,
        ls_storagelocationdatax	TYPE bapi_mardx,
        ls_valuationdatax       TYPE bapi_mbewx,
        ls_warehousenumberdatax TYPE bapi_mlgnx,
        ls_salesdatax           TYPE bapi_mvkex,
        ls_storagetypedatax     TYPE bapi_mlgtx.

  DATA: ls_return               TYPE bapiret2.

  DATA: lt_materialdescription  TYPE TABLE OF bapi_makt       WITH HEADER LINE,
        lt_unitsofmeasure       TYPE TABLE OF bapi_marm       WITH HEADER LINE,
        lt_unitsofmeasurex      TYPE TABLE OF bapi_marmx      WITH HEADER LINE,
        lt_internationalartnos  TYPE TABLE OF bapi_mean       WITH HEADER LINE,
        lt_materiallongtext     TYPE TABLE OF bapi_mltx       WITH HEADER LINE,
        lt_taxclassifications   TYPE TABLE OF bapi_mlan       WITH HEADER LINE,
        lt_returnmessages       TYPE TABLE OF bapi_matreturn2 WITH HEADER LINE,
        lt_prtdata              TYPE TABLE OF bapi_mfhm       WITH HEADER LINE,
        lt_prtdatax             TYPE TABLE OF bapi_mfhmx      WITH HEADER LINE,
        lt_extensionin          TYPE TABLE OF bapiparex       WITH HEADER LINE,
        lt_extensioninx         TYPE TABLE OF bapiparexx      WITH HEADER LINE.

  DATA: ls_bapi_te_mara         TYPE bapi_te_mara,  "BAPI_TE_MARA,
        ls_bapi_te_marax        TYPE bapi_te_marax, "BAPI_TE_MARAX.
        l_valuepart             TYPE recaextdata.

  ls_headdata-material        = gt_data-matnr .
  ls_headdata-basic_view      = c_x . "MARA
  ls_headdata-storage_view    = c_x . "MARC
  ls_headdata-mrp_view        = c_x . "MARD

  "------------------------------------------------------------------*
  "BAPI header MARA

  IF gt_data-bukrs = 'HA08' .
    ls_clientdata-prod_memo   = gt_data-zeinr .
    ls_clientdatax-prod_memo  = c_x .
  ELSE .
    ls_clientdata-document    = gt_data-zeinr .
    ls_clientdatax-document   = c_x .
  ENDIF .

  ls_clientdata-minremlife          = gt_data-mhdrz .
  ls_clientdatax-minremlife         = c_x .
  "------------------------------------------------------------------*
  "BAPI header MARD
  ls_storagelocationdata-plant      = gt_data-werks .     "
  ls_storagelocationdata-stge_loc   = gt_data-lgort .     "
  ls_storagelocationdata-reorder_pt = gt_data-lminb .
  ls_storagelocationdata-repl_qty   = gt_data-lbstf .
  ls_storagelocationdata-stge_bin   = gt_data-lgpbe .

  ls_storagelocationdatax-plant      = gt_data-werks.     "
  ls_storagelocationdatax-stge_loc   = gt_data-lgort.     "
  ls_storagelocationdatax-reorder_pt = c_x .
  ls_storagelocationdatax-repl_qty   = c_x  .
  ls_storagelocationdatax-stge_bin   = c_x  .

  "------------------------------------------------------------------*
  "BAPI header MARC
  ls_plantdata-plant       = gt_data-werks .
  ls_plantdata-abc_id      = gt_data-maabc .

  ls_plantdatax-plant      = gt_data-werks .
  ls_plantdatax-abc_id     = c_x .

  "------------------------------------------------------------------*
  "BABI
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata             = ls_headdata
      clientdata           = ls_clientdata
      clientdatax          = ls_clientdatax
      plantdata            = ls_plantdata
      plantdatax           = ls_plantdatax
      storagelocationdata  = ls_storagelocationdata
      storagelocationdatax = ls_storagelocationdatax
    IMPORTING
      return               = ls_return.

*  '@08@'."  Green light; positive
*  '@09@'."  Yellow light; neutral
*  '@0A@'."  Red light; negative

  IF ls_return-type EQ c_e.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    gt_data-icon = '@0A@' .
    gt_data-mesg = ls_return-message.

  ELSE .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CLEAR : gt_data-mesg .
    gt_data-icon = '@08@' .

  ENDIF .

ENDFORM.                    " GET_BAPI_DATA_0010
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_MMAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_mmab .

  DATA : lt_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE .

  CLEAR : gt_bdctab[] , gt_bdctab ,
          gt_msgtab[] , gt_msgtab .

*. Set BDC
  PERFORM dynpro USING:
     'X' 'SAPMM03Z'             '0100' ,
     ' ' 'BDC_OKCODE'           '=GOON' ,
     ' ' 'RM03Z-MATNR'          gt_data-matnr ,
     ' ' 'RM03Z-NMTAR'          gt_data-mtart ,

     'X' 'SAPMM03Z'             '0100' ,
     ' ' 'BDC_OKCODE'           '=LOS' ,

     'X' 'SAPMM03Z'             '0300' ,
     ' ' 'BDC_OKCODE'           '=ENTR' .


  MOVE  : ' ' TO ctu_params-nobinpt,
          'S' TO ctu_params-updmode,
          'X' TO ctu_params-defsize,
          c_n TO ctu_params-dismode.

  CALL TRANSACTION 'MMAM' USING         gt_bdctab
                          OPTIONS FROM  ctu_params
                          MESSAGES INTO gt_msgtab.

  COMMIT WORK .

  DESCRIBE TABLE gt_msgtab LINES sy-tabix.
  READ TABLE gt_msgtab INDEX sy-tabix.


*  '@08@'."  Green light; positive
*  '@09@'."  Yellow light; neutral
*  '@0A@'."  Red light; negative

  DATA : l_mtart LIKE mara-mtart .

  SELECT SINGLE mtart INTO l_mtart
    FROM mara
   WHERE matnr EQ gt_data-matnr.

  IF l_mtart = gt_data-mtart .
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

ENDFORM.                    " CALL_BDC_MMAB
*&---------------------------------------------------------------------*
*&      Form  process_comm_c
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_comm_c .
  DATA : ls_mara LIKE mara .

  DATA : l_answer .
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

  PERFORM answer_and_question USING text-q04
                                    text-q02
                                    ''
                              CHANGING l_answer .
  CHECK l_answer EQ c_j .


  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_data INDEX g_rows_s-index.
    IF sy-subrc = 0 AND gt_data-dupkey IS INITIAL .

* Check of MARC View
      SELECT SINGLE * INTO ls_mara
        FROM mara
       WHERE matnr = gt_data-matnr .

      SEARCH ls_mara-pstat FOR 'D' .
      IF sy-subrc <> 0 .
        gt_data-icon = '@0A@' .
        gt_data-mesg = 'Change impossible "Maintain MRP View" and then try again' .
        MODIFY gt_data INDEX g_rows_s-index. CONTINUE .
      ENDIF .

      PERFORM get_bapi_data_0010 .

      MODIFY gt_data INDEX g_rows_s-index.
    ENDIF .

  ENDLOOP .

ENDFORM.                    " process_comm_c
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMM_B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_comm_b .
  DATA : l_answer .
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

  PERFORM answer_and_question USING text-q03
                                    text-q02
                                    ''
                              CHANGING l_answer .
  CHECK l_answer EQ c_j .


  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_data INDEX g_rows_s-index.
    IF sy-subrc = 0 AND gt_data-dupkey IS INITIAL .

      PERFORM call_bdc_mm06 .

      MODIFY gt_data INDEX g_rows_s-index.
    ENDIF .

  ENDLOOP .

ENDFORM.                    " PROCESS_COMM_B
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_MM06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_mm06 .

  DATA : lt_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE .

  CLEAR : gt_bdctab[] , gt_bdctab ,
          gt_msgtab[] , gt_msgtab .

*. Set BDC
  PERFORM dynpro USING:
     'X' 'SAPMM03G'             '0100' ,
     ' ' 'BDC_OKCODE'           '/00' ,
     ' ' 'RM03G-MATNR'          gt_data-matnr ,
     ' ' 'RM03G-WERKS'          gt_data-werks ,
     ' ' 'RM03G-LGORT'          gt_data-lgort ,

     'X' 'SAPMM03G'             '0111' ,
     ' ' 'BDC_OKCODE'           '=BU' ,
     ' ' 'RM03G-LVOWK'          ' ' ,
     ' ' 'RM03G-LVOLG'          'X' .


  MOVE  : ' ' TO ctu_params-nobinpt,
          'S' TO ctu_params-updmode,
          'X' TO ctu_params-defsize,
          c_n TO ctu_params-dismode.

  CALL TRANSACTION 'MM06' USING         gt_bdctab
                          OPTIONS FROM  ctu_params
                          MESSAGES INTO gt_msgtab.


  DESCRIBE TABLE gt_msgtab LINES sy-tabix.
  READ TABLE gt_msgtab INDEX sy-tabix.


*  '@08@'."  Green light; positive
*  '@09@'."  Yellow light; neutral
*  '@0A@'."  Red light; negative


  IF gt_msgtab-msgtyp EQ 'E' .

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

  ELSE.

    gt_data-lvorm = c_x .
    gt_data-icon = '@08@' .
    CLEAR gt_data-mesg .

  ENDIF .

ENDFORM.                    " CALL_BDC_MM06
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM event_toolbar_item  USING p_e_object TYPE REF TO cl_alv_event_toolbar_set
                          p_e_interactive TYPE char1.

  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-function  = 'L_MIGO'.
  ls_toolbar-icon      = ' '.
  ls_toolbar-disabled  = space.
  ls_toolbar-text      = 'Goods receipt'. .
*  LS_TOOLBAR-QUICKINFO = '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-function  = 'L_MB1A'.
  ls_toolbar-icon      = ' '.
  ls_toolbar-disabled  = space.
  ls_toolbar-text      = 'Goods Issue'. .
*  LS_TOOLBAR-QUICKINFO = '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-function  = 'L_MB1B'.
  ls_toolbar-icon      = ' '.
  ls_toolbar-disabled  = space.
  ls_toolbar-text      = 'Stock transfer'. .
*  LS_TOOLBAR-QUICKINFO = '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.


  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-function  = 'PMO001'.
  ls_toolbar-icon      = ' '.
  ls_toolbar-disabled  = space.
  ls_toolbar-text      = 'Create PR'. .
*  LS_TOOLBAR-QUICKINFO = '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.


  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-function  = 'L_MI01'.
  ls_toolbar-icon      = ' '.
  ls_toolbar-disabled  = space.
  ls_toolbar-text      = 'Physical Inventory'. .
*  LS_TOOLBAR-QUICKINFO = '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-function  = 'L_ME5A'.
  ls_toolbar-icon      = ' '.
  ls_toolbar-disabled  = space.
  ls_toolbar-text      = 'PR List Display'. .
*  LS_TOOLBAR-QUICKINFO = '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-function  = 'L_ME2M'.
  ls_toolbar-icon      = ' '.
  ls_toolbar-disabled  = space.
  ls_toolbar-text      = 'PO List Display'. .
*  LS_TOOLBAR-QUICKINFO = '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

ENDFORM.                    " EVENT_TOOLBAR_ITEM
*&---------------------------------------------------------------------*
*&      Form  EVENT_UCOMM_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_ucomm_item  USING p_e_ucomm.

  CASE p_e_ucomm.
    WHEN 'L_MIGO'.
      CALL TRANSACTION 'MIGO' .

    WHEN 'L_MB1A'.
      CALL TRANSACTION 'MB1A' .

    WHEN 'PMO001'.
      CALL TRANSACTION 'ZHKPMO0001' .

    WHEN 'L_MB1B'.
      CALL TRANSACTION 'MB1B' .
    WHEN 'L_MI01'.
      CALL TRANSACTION 'MI01' .
    WHEN 'L_ME5A'.
      CALL TRANSACTION 'ME5A' .
    WHEN 'L_ME2M'.
      CALL TRANSACTION 'ME2M' .

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " EVENT_UCOMM_ITEM
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

  CLEAR : gt_data[] , gt_data, gt_upload[], gt_upload .

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

  DELETE gt_upload WHERE matnr = space .

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
FORM display_status   USING    p_text.

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

  DATA : lt_mard LIKE TABLE OF zhkpms0020 WITH HEADER LINE .
  DATA : lt_mbew LIKE TABLE OF mbew       WITH HEADER LINE .

  CLEAR : gt_data[], gt_data .

  LOOP AT gt_upload .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gt_upload-matnr
      IMPORTING
        output = gt_upload-matnr.

    REPLACE ALL OCCURRENCES OF ',' IN gt_upload-lminb WITH  space .
    REPLACE ALL OCCURRENCES OF ',' IN gt_upload-lbstf WITH  space .
    REPLACE ALL OCCURRENCES OF ',' IN gt_upload-lgpbe WITH  space .

    MODIFY gt_upload .
  ENDLOOP .

*. Get row data : Material master data
  SELECT rd~werks
         rd~lgort
         rd~matnr
         kt~maktx
         ra~meins
         ra~wrkst
         ra~matkl
         rd~labst
         ra~mtart
         rd~lminb
         rd~lbstf
         rd~lgpbe
         rc~maabc
         rd~lvorm
         ra~mhdrz
         ra~pstat
    INTO CORRESPONDING FIELDS OF TABLE lt_mard
    FROM mard AS rd INNER JOIN marc AS rc
                 ON rd~matnr = rc~matnr
                AND rd~werks = rc~werks
                    INNER JOIN mara AS ra
                 ON rd~matnr = ra~matnr
                    INNER JOIN makt AS kt
                 ON kt~matnr = rd~matnr
                AND kt~spras = sy-langu
    FOR ALL ENTRIES IN gt_upload
   WHERE rd~werks EQ gt_upload-werks
     AND rd~lgort EQ gt_upload-lgort
     AND rd~lvorm EQ space
     AND rd~matnr EQ gt_upload-matnr .

*.. Material Valuation
  SELECT * INTO TABLE lt_mbew
    FROM mbew
    FOR ALL ENTRIES IN gt_upload
  WHERE matnr EQ gt_upload-matnr
    AND bwkey EQ gt_upload-werks .

  SORT lt_mbew BY matnr bwkey .

  SORT lt_mard BY werks lgort matnr .

  LOOP AT gt_upload .

    CLEAR : gt_data .
    READ TABLE lt_mard WITH KEY werks = gt_upload-werks
                                lgort = gt_upload-lgort
                                matnr = gt_upload-matnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      MOVE-CORRESPONDING lt_mard TO gt_data .
    ELSE .
      CONTINUE .
    ENDIF .

    gt_data-mtart = gt_upload-mtart .
    gt_data-lminb = gt_upload-lminb .
    gt_data-lbstf = gt_upload-lbstf .
    gt_data-lgpbe = gt_upload-lgpbe .
    gt_data-maabc = gt_upload-maabc .
    gt_data-mhdrz = gt_upload-mhdrz .
    gt_data-zeinr = gt_upload-zeinr .

    READ TABLE lt_mbew WITH KEY matnr = gt_data-matnr
                                bwkey = gt_data-werks
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-verpr = lt_mbew-verpr .
      gt_data-peinh = lt_mbew-peinh .
    ENDIF .

    APPEND gt_data .

  ENDLOOP .

* check out : Material Number are duplicated
  DATA : lt_data LIKE gt_data OCCURS 0 WITH HEADER LINE .
  DATA : l_count TYPE i .

*  '@08@'."  Green light; positive
*  '@09@'."  Yellow light; neutral
*  '@0A@'."  Red light; negative

  lt_data[] = gt_data[] .
  SORT lt_data BY werks lgort matnr .

  LOOP AT gt_data .

    CLEAR : l_count .
    READ TABLE lt_data WITH KEY  werks = gt_data-werks
                                 lgort = gt_data-lgort
                                 matnr = gt_data-matnr
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      LOOP AT lt_data FROM sy-tabix .
        IF lt_data-matnr <> gt_data-matnr .
          EXIT .
        ENDIF .
        l_count = l_count + 1 .
      ENDLOOP .
    ENDIF .

    IF l_count > 1 .
      gt_data-icon = '@0A@'."  Red light; negative
      gt_data-mesg = 'Material Number are duplicated' .
      gt_data-dupkey = 'X' .
    ENDIF .

      SEARCH gt_data-pstat FOR 'D' .
      IF sy-subrc <> 0 .
        gt_data-mesg = 'Change impossible "Maintain MRP View" and then try again' .
      ENDIF .

     MODIFY gt_data .
  ENDLOOP .

ENDFORM.                    " MOVE_TO_LIST
*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_hotspot_click_item  USING    p_row_id
                                        p_column_id.



  READ TABLE gt_data    INDEX p_row_id.

  CHECK sy-subrc = 0 .

  CASE p_column_id.

    WHEN 'MATNR'.
      SET PARAMETER ID 'MAT'  FIELD gt_data-matnr.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

  ENDCASE.

ENDFORM.                    " EVENT_HOTSPOT_CLICK_ITEM
