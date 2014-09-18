*&---------------------------------------------------------------------*
*&  Include           ZFIA_FTA_POSTING_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET TITLEBAR  '100'.
  SET PF-STATUS '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'POST'.
      IF it_summ_data[] IS INITIAL.
        MESSAGE i000 WITH 'Already posting '.
      ELSE.
        PERFORM post_process_loop.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.

  __process 'Read BSIS...' '30'.                           "30%

  PERFORM get_table_data.
  PERFORM balance_data_move.
  PERFORM data_move_summ.
  PERFORM posting_data_check.

  __process 'Prepare Screen...' '98'.                      "98%

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
FORM create_field_category USING mode_edit.
ENDFORM.                    " CREATE_FIELD_CATEGORY

*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
FORM sort_build USING ft_sort TYPE lvc_t_sort.



ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       Event of changed data
*----------------------------------------------------------------------*
*      -->RR_DATA_CHANGED  Log is Visible
*----------------------------------------------------------------------*
FORM data_changed USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_cells     TYPE lvc_s_modi,
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE.

  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = stable.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
FORM set_lvc_layout.
  CLEAR gs_layo.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
FORM exclude_functions.

  PERFORM append_exclude_functions
           TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  EVENT_RECEIVER_PROCESS
*&---------------------------------------------------------------------*
FORM event_receiver_process .

  CREATE OBJECT g_event_receiver.
  SET :
    HANDLER g_event_receiver->handle_data_changed  FOR g_grid,
    HANDLER g_event_receiver->handle_double_click  FOR g_grid,
    HANDLER g_event_receiver->handle_hotspot_click FOR g_grid.

ENDFORM.                    " EVENT_RECEIVER_PROCESS
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.

  IF g_custom_container IS INITIAL.

    PERFORM create_and_init_alv.

  ENDIF.
*   Display alv grid
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = gs_layo
      it_toolbar_excluding = gt_exclude
      i_save               = gc_var_save
      is_variant           = gs_variant
    CHANGING
      it_outtab            = it_data[]   "gt_out[]
      it_fieldcatalog      = gt_fcat[]
      it_sort              = gt_sort[].

  __focus g_grid.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color.
*  CLEAR: GS_SPECIALCOL, GT_SPECIALCOL[], GT_OUT-TABCOLOR[].
*
*  DEFINE __COLOR.
*    GS_SPECIALCOL-FIELDNAME = &1 .
*    GS_SPECIALCOL-COLOR-COL = &2 .
*    GS_SPECIALCOL-COLOR-INT = &3 .
*    APPEND GS_SPECIALCOL TO GT_SPECIALCOL .
*  END-OF-DEFINITION.
*
** COLOR
*  __COLOR : 'RMENGE' '6' 0,
*            'DTYUPS' '5' 0,
*            'DMBTR'  '7' 0.
*  GT_OUT-TABCOLOR[] = GT_SPECIALCOL[].
*  MODIFY GT_OUT TRANSPORTING TABCOLOR WHERE TABCOLOR IS INITIAL.
ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv.
  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = stable.
ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.
*  CLEAR GV_INDEX.
*  GV_INDEX = E_ROW-INDEX.
*
*  READ TABLE GT_OUT INDEX GV_INDEX.
*  IF SY-SUBRC = 0.
*    IF E_COLUMN = 'BELUM'.
*      CHECK GT_OUT-BELUM NE SPACE.
*      SET PARAMETER ID : 'RBN'  FIELD GT_OUT-BELUM,
*                         'GJR'  FIELD P_DATE(4).
*      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
*    ENDIF.
*    IF E_COLUMN = 'MATNR'.
*      CHECK GT_OUT-MATNR NE SPACE.
*      SET PARAMETER ID 'MAT'  FIELD GT_OUT-MATNR.
*      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
*    ENDIF.
*    IF E_COLUMN = 'EBELN'.
*      CHECK GT_OUT-EBELN NE SPACE.
*      SET PARAMETER ID 'BES'  FIELD GT_OUT-EBELN.
*      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
*    ENDIF.
*  ENDIF.
*
*  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS EXPORTING CONTROL = G_GRID.
*
ENDFORM.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  create_and_init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv.

*   Create object
  PERFORM create_object.

*   Exclude toolbar
  PERFORM exclude_functions.

  PERFORM event_receiver_process.

  PERFORM grid_fieldcat.
  PERFORM build_sort_field.
  PERFORM set_lvc_layout.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " create_and_init_alv
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS

*&---------------------------------------------------------------------*
*&      Form  get_data_from_dkbz_dkpo
*&---------------------------------------------------------------------*
FORM get_row_data_06.

ENDFORM.                    "get_row_data_06
*&---------------------------------------------------------------------*
*&      Form  CALC_DATA_06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_data.


ENDFORM.                    " CALC_DATA_06
*&---------------------------------------------------------------------*
*&      Form  GET_ROW_DATA_EKBZ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data_ekbz_06_08.


ENDFORM.                    " GET_ROW_DATA_EKBZ
*&---------------------------------------------------------------------*
*&      Form  REFINE_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refine_itab_06_08.

ENDFORM.                    " REFINE_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_gt_out.


ENDFORM.                    " GET_GT_OUT
**&---------------------------------------------------------------------*
**&      Form  create_calc_tab
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_GT_CALC  text
**----------------------------------------------------------------------*
*FORM create_calc_tab TABLES p_gt_calc STRUCTURE gt_calc
*                      USING  p_$key
*                             tempq
*                             dtyamt .
*
*ENDFORM.                    " create_calc_tab
**&---------------------------------------------------------------------*
**&      Form  REDUCE_MATNR_SUM
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_ITAB_FOR_MATNR_SUM  text
**      -->P_M__IDX  text
**      -->P_TEMPQ  text
**----------------------------------------------------------------------*
*FORM reduce_matnr_sum TABLES   p_itab_for_matnr_sum STRUCTURE
*                                                  itab_for_matnr_sum
*                      USING    p_m_idx
*                               p_tempq.
*
*
*ENDFORM.                    " REDUCE_MATNR_SUM
**&---------------------------------------------------------------------*
**&      Form  CREATE_CALC_TAB_NONE_PO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_GT_CALC  text
**      -->P_ENDIF  text
**----------------------------------------------------------------------*
*FORM create_none_po   TABLES p_gt_calc STRUCTURE gt_calc
*                      USING  p_$key
*                             tempq
*                             dtyamt .
*
*
*ENDFORM.                    " CREATE_CALC_TAB_NONE_PO
**&---------------------------------------------------------------------*
**&      Form  SELECT_ROW_BY_SUBTOTAL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_PT_ROWS  text
**----------------------------------------------------------------------*
*FORM select_row_by_subtotal TABLES p_pt_rows
*                                   STRUCTURE lvc_s_row.
*
*  DATA: tmpgrp TYPE lvc_t_grpl, " For subtotal Selection .
*       $tmpgrp TYPE lvc_s_grpl.
*
*  CALL METHOD g_grid->get_subtotals
*    IMPORTING
*      et_grouplevels = tmpgrp.
*
** Selected Row by row selection ( Sub total )
*  LOOP AT p_pt_rows WHERE NOT rowtype IS INITIAL.
*    READ TABLE tmpgrp INDEX p_pt_rows-index INTO $tmpgrp.
*    CHECK sy-subrc EQ 0 .
*
*    LOOP AT gt_out FROM $tmpgrp-index_from
*                     TO $tmpgrp-index_to.
*      gt_out-chk = true .
*      MODIFY gt_out.
*    ENDLOOP.
*
*  ENDLOOP.
*
*ENDFORM.                    " SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*&      Form  set_filed_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XCOLOR  text
*      -->P_GT_OUT_TABCOLOR  text
*      -->P_1564   text
*      -->P_1565   text
*----------------------------------------------------------------------*
*FORM set_filed_color TABLES   p_gt_out STRUCTURE gt_out
*                      USING   p_fieldname
*                              p_color.
*  DATA: xcolor TYPE slis_specialcol_alv.
*  CLEAR xcolor.
*  xcolor-fieldname = p_fieldname.
*  xcolor-color-col = p_color.
*  xcolor-color-int = '0'. "Intensified on/off
*  xcolor-color-inv = '0'.
*  APPEND xcolor TO p_gt_out-tabcolor.
*ENDFORM.                    " set_filed_color
**&---------------------------------------------------------------------*
**&      Form  WRITE_OFF_ITEM
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_P_IT_INV  text
**      <--P_ITEM_CNT  text
**----------------------------------------------------------------------*
*FORM write_off_item TABLES   p_it_inv STRUCTURE it_inv
*                    USING    $gross_amount
*                             $grs_dist_amt
*                    CHANGING $gl_doc_cnt .
*
*ENDFORM.                    " WRITE_OFF_ITEM
*&---------------------------------------------------------------------*
*&      Form  LOAD_SAVED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM load_saved_data.
*
*
*ENDFORM.                    " LOAD_SAVED_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_FTZ_IF_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
**----------------------------------------------------------------------*
*FORM update_ftz_if_item.
*
*  UPDATE ztmm_duty_it
*     SET cl_doc_no = *ztcou124-belum
*   WHERE entno = *ztcou124-entno
*     AND matnr = *ztcou124-matnr.
*
*ENDFORM.                    " UPDATE_FTZ_IF_TABLE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_FTZ_IF_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM update_ftz_if_header USING p_flag.
*  UPDATE ztmm_duty_hd
*     SET clear_doc = p_flag
*   WHERE entno = *ztcou124-entno.
*ENDFORM.                    " UPDATE_FTZ_IF_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_ROW_DATA_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_row_data_01.
*
*ENDFORM.                    " GET_ROW_DATA_01
*&---------------------------------------------------------------------*
*&      Form  GET_ROW_DATA_08
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_row_data_08.
*
*ENDFORM.                    " GET_ROW_DATA_08
*&---------------------------------------------------------------------*
*&      Form  GET_TABLE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_table_data.

  DATA gjahr TYPE  gjahr.

  CLEAR: it_ztmm_duty_hd, it_ztmm_duty_hd[].
  CLEAR: it_ztmm_duty_it, it_ztmm_duty_it[].

* select head
  SELECT *
     INTO  CORRESPONDING FIELDS OF TABLE it_ztmm_duty_hd
     FROM  ztmm_duty_hd
     WHERE ( entno = p_entn11
        OR   entno = p_entn21
        OR   entno = p_entn31 ).
  SORT it_ztmm_duty_hd BY entno.

  READ TABLE it_ztmm_duty_hd INDEX 1.
  CHECK sy-subrc = 0.
* select Item Table
  SELECT *
     INTO  CORRESPONDING FIELDS OF TABLE it_ztmm_duty_it
     FROM  ztmm_duty_it
     FOR ALL ENTRIES IN it_ztmm_duty_hd
     WHERE entno    =  it_ztmm_duty_hd-entno
       AND duty_amt <> 0.
  SORT it_ztmm_duty_it BY entno seq.

  LOOP AT it_ztmm_duty_it.
    CLEAR: it_data.
    it_data-icon        =  ''.
    it_data-entno       =  it_ztmm_duty_it-entno.
    it_data-seq         =  it_ztmm_duty_it-seq.
    it_data-ebeln       =  it_ztmm_duty_it-ebeln.
    it_data-duty_amt    =  it_ztmm_duty_it-duty_amt.
* Vendor Account Number
    CLEAR: ekko.
    SELECT SINGLE lifnr
      INTO ekko-lifnr
      FROM ekko
     WHERE ebeln = it_ztmm_duty_it-ebeln.
* Country Key
    CLEAR: lfa1.
    SELECT SINGLE land1
      INTO it_data-land1
      FROM lfa1
     WHERE lifnr = ekko-lifnr.
    IF it_data-land1 = 'KR'.
      it_data-acct11    =  p_ven1.
      it_data-acct12    =  p_expact.
    ELSE.
* Selection screen 1. Assignment
      IF p_entn11         =  it_ztmm_duty_it-entno.
        it_data-acct11    =  p_acct11.
        it_data-text11    =  p_text11.
        it_data-acct12    =  p_acct12.
        it_data-amt012    =  p_amt012.
*        it_data-text12    =  p_text12.
      ENDIF.
* Selection screen 2. Assignment
      IF p_entn21         =  it_ztmm_duty_it-entno.
        it_data-acct11    =  p_acct21.
        it_data-text11    =  p_text21.
        it_data-acct12    =  p_acct22.
        it_data-amt012    =  p_amt022.
*        it_data-text12    =  p_text22.
      ENDIF.
* Selection screen 3. Assignment
      IF p_entn31         =  it_ztmm_duty_it-entno.
        it_data-acct11    =  p_acct31.
        it_data-text11    =  p_text31.
        it_data-acct12    =  p_acct32.
        it_data-amt012    =  p_amt032.
*        it_data-text12    =  p_text32.
      ENDIF.
    ENDIF.
* Parking Document Number
    CLEAR: it_ztmm_duty_hd.
    READ TABLE it_ztmm_duty_hd WITH KEY entno = it_ztmm_duty_it-entno.
    IF sy-uname = 'T00266'.
      it_ztmm_duty_hd-belnr = ''.
    ENDIF.
    IF it_ztmm_duty_hd-belnr = ''.
      it_data-icon = ''.
    ELSE.
      it_data-icon = icon_led_green.
    ENDIF.
    it_data-belnr = it_ztmm_duty_hd-belnr.

    APPEND it_data.
  ENDLOOP.
* Screen count display
  DESCRIBE TABLE it_data LINES p_data_cnt.
  p_cnt  = p_data_cnt.

ENDFORM.                    " GET_TABLE_DATA.
*&---------------------------------------------------------------------*
*&      Form  GET_ROW_DATA_EKBZ_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_row_data_ekbz_01.
*
*ENDFORM.                    " GET_ROW_DATA_EKBZ_01
**&---------------------------------------------------------------------*
**&      Form  COLLECT_ITAB
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_LT_ITAB  text
**----------------------------------------------------------------------*
*FORM collect_itab TABLES p_lt_itab STRUCTURE it_row_tab.
*
*  DATA : flag(1).
*
*  LOOP AT it_row_tab.
*    MOVE it_row_tab TO p_lt_itab .
*    IF p_lt_itab-shkzg EQ 'S'. " is Debit ?
*      p_lt_itab-bmenge = -1 * p_lt_itab-bmenge.
*      p_lt_itab-dmbtr  = -1 * p_lt_itab-dmbtr.
*    ENDIF.
*    CLEAR : p_lt_itab-shkzg,p_lt_itab-bewtp,p_lt_itab-accup.
*    COLLECT p_lt_itab.
*  ENDLOOP.
*
*  DELETE p_lt_itab WHERE bmenge <= 0.
*
*ENDFORM.                    " COLLECT_ITAB
*&---------------------------------------------------------------------*
*&      Form  REFINE_ITAB_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM refine_itab_01.
*
*ENDFORM.                    " REFINE_ITAB_01
*&---------------------------------------------------------------------*
*&      Form  CHECK_PARTIAL_POST_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM check_partial_post_01 TABLES p_gt_calc STRUCTURE gt_calc .
*
*
*ENDFORM.                    " CHECK_PARTIAL_POST_01
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_plant.
*
*ENDFORM.                    " GET_PLANT
*&---------------------------------------------------------------------*
*&      Form  get_po_one_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GR_MATNR  text
*----------------------------------------------------------------------*
*FORM get_po_one_time TABLES $gr_matnr STRUCTURE gr_matnr.
*
*
*ENDFORM.                    " get_po_one_time
*&---------------------------------------------------------------------*
*&      Form  get_po_several_times
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GR_MATNR  text
*----------------------------------------------------------------------*
*FORM get_po_several_times TABLES $gr_matnr STRUCTURE gr_matnr.
*
*
*
*ENDFORM.                    " get_po_several_times
**&---------------------------------------------------------------------*
**&      Form  UNIT_CONVERT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_FROM_U  text
**      -->P_TO_U  text
**      -->P_QTY1  text
**      <--P_RESULT_QTY  text
**----------------------------------------------------------------------*
*FORM unit_convert USING    p_to_u
*                  CHANGING p_result_qty
*                           p_from_u.
*
*  DATA $p_result_qty LIKE i_bsis-menge.
*
*  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
*    EXPORTING
*      input                = p_result_qty
*      unit_in              = p_from_u
*      unit_out             = p_to_u
*    IMPORTING
*      output               = $p_result_qty
*    EXCEPTIONS
*      conversion_not_found = 1
*      division_by_zero     = 2
*      input_invalid        = 3
*      output_invalid       = 4
*      overflow             = 5
*      type_invalid         = 6
*      units_missing        = 7
*      unit_in_not_found    = 8
*      unit_out_not_found   = 9.
*
*  IF sy-subrc <> 0.
**           ALT UoM
*    DATA : l_umrez_f TYPE umrez,
*           l_umrez_t TYPE umrez.
*
*    CLEAR : l_umrez_f,l_umrez_t.
*
**    SELECT SINGLE UMREZ INTO :
**              L_UMREZ_F FROM MARM
**             WHERE MATNR = I_BSIS-MATNR
**             AND MEINH = P_FROM_U,
**
**              L_UMREZ_T FROM MARM
**             WHERE MATNR = I_BSIS-MATNR
**             AND MEINH = P_TO_U.
**
*    SELECT SINGLE umrez umren INTO :
*              (l_umrez_f, l_umrez_t) FROM marm
*             WHERE matnr = i_bsis-matnr
*             AND meinh = p_from_u.
*
*    IF l_umrez_f <> 0 AND  l_umrez_t <> 0.
*      $p_result_qty = p_result_qty * ( l_umrez_f / l_umrez_t ).
*      p_result_qty = $p_result_qty.
*      p_from_u = p_to_u.
*    ELSE.
** error
*      p_result_qty = 0.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " UNIT_CONVERT
**&---------------------------------------------------------------------*
**&      Form  UNIT_CONVERT_BATCH
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_I_BSIS  text
**      -->P_P_LT_ITAB  text
**----------------------------------------------------------------------*
*FORM unit_convert_batch TABLES p_i_bsis STRUCTURE i_bsis
*                               p_lt_itab STRUCTURE it_row_tab .
*
*  SORT p_lt_itab BY matnr ebeln ebelp.
*
*  LOOP AT p_i_bsis .
*    $ix = sy-tabix.
*    READ TABLE p_lt_itab WITH KEY matnr = p_i_bsis-matnr BINARY SEARCH.
*    CHECK sy-subrc EQ 0.
*    IF i_bsis-uom NE space AND i_bsis-uom NE p_lt_itab-meins.
*      PERFORM unit_convert USING p_lt_itab-meins
*                        CHANGING p_i_bsis-menge
*                                 i_bsis-uom.
*      IF p_i_bsis-menge NE 0 AND i_bsis-uom NE space.
*        MODIFY i_bsis INDEX $ix TRANSPORTING uom menge.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " UNIT_CONVERT_BATCH
*&---------------------------------------------------------------------*
*&      Form  GET_DST_AMT_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dst_amt_01.

ENDFORM.                    " GET_DST_AMT_01
*&---------------------------------------------------------------------*
*&      Form  GRID_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grid_fieldcat .

  DATA : l_datum(08),
         l_fcat  TYPE lvc_s_fcat.
  DATA : lv_pos  TYPE i.

  sy-datum = sy-datum + 1.
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.
*
  CLEAR   gt_fcat.
  REFRESH gt_fcat.
*                                                                                                            X
  PERFORM insert_gt_field USING '' 'ICON'     lv_pos '' 'Icon'             'M' '10' '' ''       'C01' ''  '' '' ''  '' '' ''  ''.
  PERFORM insert_gt_field USING '' 'ENTNO'    lv_pos '' 'Entry No'         'M' '10' 'X' ''      'C01' ''  '' '' ''  '' '' ''  ''.
  PERFORM insert_gt_field USING '' 'SEQ'      lv_pos '' 'Serial'           'M' '05' 'X' ''      'C01' ''  '' '' ''  '' '' ''  ''.
  PERFORM insert_gt_field USING '' 'DUTY_AMT' lv_pos '' 'Duty AMT'         'M' '10' ''  'LAND1' ''    ''  '' 'X' '' '' '' 'CURR'  ''.
  PERFORM insert_gt_field USING '' 'LAND1'    lv_pos '' 'Country Key'      'M' '15' ''  ''      ''    ''  '' '' ''  '' '' ''  ''.
  PERFORM insert_gt_field USING '' 'EBELN'    lv_pos '' 'Doc Number'       'M' '10' ''  ''      ''    ''  '' '' ''  '' '' ''  ''.
  PERFORM insert_gt_field USING '' 'ACCT11'   lv_pos '' '(Screen)Accrual Acct'
                                                                           'M' '15' ''  ''      'C01' ''  '' '' ''  '' '' ''  ''.
  PERFORM insert_gt_field USING '' 'TEXT11'   lv_pos '' '(Screen)Text'     'M' '15' ''  ''      'C01' ''  '' '' ''  '' '' ''  ''.
  PERFORM insert_gt_field USING '' 'ACCT12'   lv_pos '' '(Screen)Duty MPF' 'M' '15' ''  ''      'C01' ''  '' '' ''  '' '' ''  ''.
  PERFORM insert_gt_field USING '' 'AMT012'   lv_pos '' '(Screen)MPF Amt'  'M' '15' ''  'LAND1' 'C01' ''  '' '' ''  '' '' 'CURR'  ''.
*  PERFORM insert_gt_field USING '' 'TEXT12'   lv_pos '' '(Screen)Text'     'M' '15' ''  ''      'C01' ''  '' '' ''  '' '' ''  ''.
  PERFORM insert_gt_field USING '' 'BELNR'    lv_pos '' 'Park Number'      'M' '10' ''  ''      ''    ''  '' '' 'X' '' '' ''  ''.

ENDFORM.                    " GRID_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  insert_gt_field
*&---------------------------------------------------------------------*
FORM insert_gt_field  USING       p_2961
                                  p_2962
                                  p_2963
                                  p_2964
                                  p_2965
                                  p_2966
                                  p_2967
                                  p_2968
                                  p_2969
                                  p_2970
                                  p_2971
                                  p_2972
                                  p_2973
                                  p_2974
                                  p_2975
                                  p_2976
                                  p_2977
                                  p_2978.

  ADD 1 TO p_2963.
  CLEAR: gs_fcat.

  gs_fcat-icon       = p_2961.
  gs_fcat-fieldname  = p_2962.
  gs_fcat-col_pos    = p_2963.
  gs_fcat-row_pos    = p_2964.
  gs_fcat-coltext    = p_2965.
  gs_fcat-selddictxt = p_2966.
  gs_fcat-outputlen  = p_2967.
  gs_fcat-fix_column = p_2968.
  gs_fcat-currency   = p_2969.
  gs_fcat-emphasize  = p_2970.
  gs_fcat-just       = p_2971.
  gs_fcat-fix_column = p_2972.
  gs_fcat-do_sum     = p_2973.
  gs_fcat-hotspot    = p_2974.
  gs_fcat-decimals_o = p_2975.
  gs_fcat-col_opt    = p_2976.
  gs_fcat-datatype   = p_2977.        " Data type
  gs_fcat-no_out     = p_2978.

  APPEND gs_fcat TO gt_fcat.

*  append: gt_fcat.
*  clear : gt_fcat.

ENDFORM.                    " insert_gt_field
*&---------------------------------------------------------------------*
*&      Form  build_sort_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort_field .

  CLEAR: gs_sort, gt_sort. REFRESH:  gt_sort.

  gs_sort-spos      = '1'.
  gs_sort-fieldname = 'ENTNO'.
  gs_sort-up        = 'X'.
  gs_sort-down      = ' '.
  gs_sort-subtot    = 'X'.
  APPEND gs_sort TO gt_sort.

  gs_sort-spos      = '2'.
  gs_sort-fieldname = 'SEQ'.
  gs_sort-up        = 'X'.
  gs_sort-down      = ' '.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO gt_sort.

ENDFORM.                    " build_sort_field
*&---------------------------------------------------------------------*
*&      Form  POST_PROCESS_LOOP
*&---------------------------------------------------------------------*
FORM post_process_loop .

  DATA: lw_diff LIKE it_summ_data-amt012,
        lw_claim_amount_c(13),
        lw_line TYPE i,
        l_webre LIKE ekpo-webre,
        l_check(3) TYPE c,
        L_new_clear(3) type c,
        l_text(30).

  CLEAR: it_bdc, it_bdc[].
  CLEAR: headerdata.
  CLEAR: it_summ_data_temp, it_summ_data_temp[].
  CLEAR: w_loop_cnt, l_check.
  L_new_clear = 'NEW'.

  PERFORM data_post_field_move.

  SELECT SINGLE waers INTO w_waers FROM t001
    WHERE bukrs = p_bukrs.

  SORT it_summ_data BY no entno.
  it_summ_data_temp[] = it_summ_data[].
  DESCRIBE TABLE it_summ_data LINES w_bdc_tot.
** f-43
* "KR" Select
  READ TABLE it_summ_data INDEX 1.
* not "CN" Select
  READ TABLE it_summ_data_temp INDEX 2.
*1
  PERFORM bdc_dynpro  USING 'SAPMF05A'     '0100'.
  PERFORM bdc_field   USING 'BDC_CURSOR'   'RF05A-NEWKO'.
  PERFORM bdc_field   USING 'BDC_OKCODE'   '/00'.
  PERFORM bdc_field   USING 'BKPF-BLDAT'    w_docdt.        "01/17/2012
  PERFORM bdc_field   USING 'BKPF-BLART'   'KR'.             "KR
  PERFORM bdc_field   USING 'BKPF-BUKRS'    p_bukrs.        "H201
  PERFORM bdc_field   USING 'BKPF-BUDAT'    w_posdt.        "02/10/2012
  PERFORM bdc_field   USING 'BKPF-MONAT'    w_posdt+0(2).    "02
  PERFORM bdc_field   USING 'BKPF-WAERS'    w_waers.         "US
  PERFORM bdc_field   USING 'RF05A-NEWBS'   '31'.            "31
  PERFORM bdc_field   USING 'BKPF-XBLNR'    p_refer.         "1912033020  :  Reference
  lw_claim_amount_c = abs( p_ven1 ).
  PERFORM bdc_field   USING 'RF05A-NEWKO'   lw_claim_amount_c.         "500187
*2
  PERFORM bdc_dynpro  USING 'SAPMF05A'     '0302'.
  PERFORM bdc_field   USING 'BDC_CURSOR'   'RF05A-NEWKO'.
  PERFORM bdc_field   USING 'BDC_OKCODE'   '/00'.
  lw_claim_amount_c = abs( w_total_amt ).
  PERFORM bdc_field   USING 'BSEG-WRBTR'    lw_claim_amount_c. "10,000
  PERFORM bdc_field   USING 'BSEG-ZFBDT'    w_docdt.        "01/17/2012
  PERFORM bdc_field   USING 'RF05A-NEWBS'  '40'.            "40
  lw_claim_amount_c = abs( p_expact ).
  PERFORM bdc_field   USING 'RF05A-NEWKO'   lw_claim_amount_c.       "532160
*
  LOOP AT it_summ_data.
    w_tabix    = sy-tabix + 1.
    w_loop_cnt = w_loop_cnt + 1.
    READ TABLE it_summ_data_temp INDEX  w_tabix.
*3 end
    IF w_bdc_tot = w_tabix.
      PERFORM bdc_dynpro  USING 'SAPMF05A'    '0300'.
      PERFORM bdc_field   USING 'BDC_CURSOR'  'RF05A-NEWKO'.
      PERFORM bdc_field   USING 'BDC_OKCODE'  '/00'.
      lw_claim_amount_c = abs( it_summ_data-duty_amt ).
      PERFORM bdc_field   USING 'BSEG-WRBTR'   lw_claim_amount_c. "7466.3
      IF p_acct22 = it_summ_data_temp-acct11
      OR p_acct32 = it_summ_data_temp-acct11.
        PERFORM bdc_field   USING 'BSEG-SGTXT'   ''.
      ELSE.
        PERFORM bdc_field   USING 'BSEG-ZUONR'   it_summ_data-entno.    "309-0110118-5
        PERFORM bdc_field   USING 'BSEG-SGTXT'   it_summ_data-text11.   "06
      ENDIF.
      PERFORM bdc_field   USING 'RF05A-NEWBS'  '40'.
      lw_claim_amount_c = abs( it_summ_data_temp-acct11 ).
      PERFORM bdc_field   USING 'RF05A-NEWKO'  lw_claim_amount_c. "532165
*4
      w_tabix   = w_bdc_tot.
      PERFORM bdc_dynpro  USING 'SAPLKACB'   '0002'.
      PERFORM bdc_field   USING 'BDC_OKCODE' '=ENTE'.
*
      if L_new_clear = 'NEW'.
        IF p_acct12 = it_summ_data_temp-acct11
        OR p_acct22 = it_summ_data_temp-acct11
        OR p_acct32 = it_summ_data_temp-acct11.
        PERFORM bdc_dynpro  USING 'SAPLKEAK'     '0300'.
        PERFORM bdc_field   USING 'BDC_CURSOR'   'RKEAK-FIELD(15)'.
        PERFORM bdc_field   USING 'BDC_OKCODE'   '=WEIT'.
        PERFORM bdc_field   USING 'RKEAK-FIELD(15)'  'US'.
*
        PERFORM bdc_dynpro  USING 'SAPLKACB'   '0002'.
        PERFORM bdc_field   USING 'BDC_CURSOR' 'DKACB-ERGOKONT'.
        PERFORM bdc_field   USING 'BDC_OKCODE' '=ENTE'.
        L_new_clear = 'Skip'.
      endif.
      endif.
*5
      PERFORM bdc_dynpro  USING 'SAPMF05A'     '0300'.
      PERFORM bdc_field   USING 'BDC_OKCODE'   '/00'.
      lw_claim_amount_c = abs( it_summ_data_temp-duty_amt ).
      PERFORM bdc_field   USING 'BSEG-WRBTR'   lw_claim_amount_c.    "485
    ELSE.
*6
      PERFORM bdc_dynpro  USING 'SAPMF05A'    '0300'.
      PERFORM bdc_field   USING 'BDC_CURSOR'  'RF05A-NEWKO'.
      PERFORM bdc_field   USING 'BDC_OKCODE'  '/00'.
      lw_claim_amount_c = abs( it_summ_data-duty_amt ).
      PERFORM bdc_field   USING 'BSEG-WRBTR'   lw_claim_amount_c. "7466.3
      IF l_check = 'END'.
        PERFORM bdc_field   USING 'BSEG-SGTXT'   ''.
      ELSE.
        IF p_acct12 = it_summ_data_temp-acct11
        OR p_acct22 = it_summ_data_temp-acct11
        OR p_acct32 = it_summ_data_temp-acct11.
          PERFORM bdc_field   USING 'BSEG-ZUONR'   it_summ_data-entno.    "309-0110118-5
          PERFORM bdc_field   USING 'BSEG-SGTXT'   it_summ_data-text11.   "06
          l_check = 'END'.
        ELSE.
        ENDIF.
      ENDIF.
      PERFORM bdc_field   USING 'RF05A-NEWBS'  '40'.
      lw_claim_amount_c = abs( it_summ_data_temp-acct11 ).
      PERFORM bdc_field   USING 'RF05A-NEWKO'  lw_claim_amount_c. "532165

      IF p_acct12 = it_summ_data_temp-acct11
      OR p_acct22 = it_summ_data_temp-acct11
      OR p_acct32 = it_summ_data_temp-acct11.
*7
        PERFORM bdc_dynpro  USING 'SAPLKACB'   '0002'.
        PERFORM bdc_field   USING 'BDC_OKCODE' '=ENTE'.
        if L_new_clear = 'NEW'.
          PERFORM bdc_dynpro  USING 'SAPLKEAK'     '0300'.
          PERFORM bdc_field   USING 'BDC_CURSOR'   'RKEAK-FIELD(15)'.
          PERFORM bdc_field   USING 'BDC_OKCODE'   '=WEIT'.
          PERFORM bdc_field   USING 'RKEAK-FIELD(15)'  'US'.
*
          PERFORM bdc_dynpro  USING 'SAPLKACB'   '0002'.
          PERFORM bdc_field   USING 'BDC_CURSOR' 'DKACB-ERGOKONT'.
          PERFORM bdc_field   USING 'BDC_OKCODE' '=ENTE'.
          L_new_clear = 'Skip'.
        endif.

*8
        PERFORM bdc_dynpro  USING 'SAPMF05A'     '0300'.
        PERFORM bdc_field   USING 'BDC_OKCODE'   '/00'.
        lw_claim_amount_c = abs( it_summ_data_temp-duty_amt ).
        PERFORM bdc_field   USING 'BSEG-WRBTR'   lw_claim_amount_c.    "485
      ENDIF.
    ENDIF.
*9
    PERFORM bdc_dynpro  USING 'SAPLKACB'     '0002'.
    PERFORM bdc_field   USING 'BDC_OKCODE'   '=COBL_XERGO'.
    PERFORM bdc_field   USING 'DKACB-XERGO'   'X'.

*10
    PERFORM bdc_dynpro  USING 'SAPLKEAK'     '0300'.
    PERFORM bdc_field   USING 'BDC_CURSOR'   'RKEAK-FIELD(15)'.
    PERFORM bdc_field   USING 'BDC_OKCODE'   '=WEIT'.
    PERFORM bdc_field   USING 'RKEAK-FIELD(15)'  'US'.
*11
    PERFORM bdc_dynpro  USING 'SAPLKACB'   '0002'.
    PERFORM bdc_field   USING 'BDC_CURSOR' 'DKACB-ERGOKONT'.
    PERFORM bdc_field   USING 'BDC_OKCODE' '=ENTE'.
    L_new_clear = 'Skip'.
    IF w_bdc_tot = w_loop_cnt + 1.
      EXIT.
    ENDIF.
  ENDLOOP.
*12
  PERFORM bdc_dynpro  USING 'SAPMF05A'    '0300'.
  PERFORM bdc_field   USING 'BDC_OKCODE'  '=AB'.
  lw_claim_amount_c = abs( it_summ_data_temp-duty_amt ).
  PERFORM bdc_field   USING 'BSEG-WRBTR'   lw_claim_amount_c.    "485
  PERFORM bdc_field   USING 'DKACB-FMORE'  'X'.

  PERFORM bdc_dynpro  USING 'SAPLKACB'     '0002'.
  PERFORM bdc_field   USING 'BDC_OKCODE'   '=ENTE'.

  PERFORM bdc_dynpro      USING 'SAPMF05A'     '0700'.
  PERFORM bdc_field       USING 'BDC_CURSOR'   'RF05A-NEWBS'.
  PERFORM bdc_field       USING 'BDC_OKCODE'   '=BU'.

  CALL TRANSACTION 'F-43' USING it_bdc
                          MODE p_bdcmod
                          UPDATE 'S'
                          MESSAGES INTO it_mess.

  READ TABLE it_mess WITH KEY msgtyp = 'E'.
  IF sy-subrc EQ 0.
    READ TABLE it_summ_data INDEX 1.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt1  = 'Error for parking Memo (Scrap)'
        txt2  = it_mess-msgv1
*       txt3  = it_summ_data-entno
*       txt4  =                         it_mess-msgv2
      .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    CLEAR: it_mess.
    READ TABLE it_mess WITH KEY msgtyp   = 'S' dynumb = '0700'.
    IF sy-subrc <> 0.
      READ TABLE it_mess WITH KEY msgtyp = 'S' dynumb = '0312'.
    ENDIF.

* Parking document number
    LOOP AT it_summ_data.
*   screen update
      LOOP AT it_data WHERE entno = it_summ_data-entno.
        IF it_mess-msgv1 = ''.
          it_data-icon  = icon_led_red.
        ELSE.
          it_data-icon  = icon_led_green.
          it_data-belnr = it_mess-msgv1.    "invoicedocnumber.
        ENDIF.
        MODIFY it_data TRANSPORTING icon belnr.
      ENDLOOP.
*   D/B Table update
      SELECT SINGLE *
        INTO ztmm_duty_hd
        FROM ztmm_duty_hd
       WHERE entno = it_summ_data-entno.
      IF sy-subrc = 0.
        ztmm_duty_hd-belnr = it_mess-msgv1.  "invoicedocnumber.
        UPDATE ztmm_duty_hd FROM ztmm_duty_hd.
      ENDIF.
    ENDLOOP.
    IF it_mess-msgv1 = ''.
      CONCATENATE 'Error for parking Memo (Scrap)' ' ' INTO l_text.
    ELSE.
      CONCATENATE it_mess-msgv1 ' was created' INTO l_text.
    ENDIF.
    MESSAGE s000 WITH l_text.
  ENDIF.

ENDFORM.                    " POST_PROCESS_LOOP
*&---------------------------------------------------------------------*
*&      Form  data_move_summ
*&---------------------------------------------------------------------*
FORM data_move_summ.

  CLEAR: it_summ_data,         it_summ_data[].
  CLEAR: it_summ_data_temp,    it_summ_data_temp[].
  CLEAR: w_total_amt.

* Parking data move
  LOOP AT it_data WHERE belnr = ''.
    CLEAR: it_summ_data.
    it_summ_data-entno     =  it_data-entno.     "Entry No.
    it_summ_data-duty_amt  =  it_data-duty_amt.  "amt
    it_summ_data-land1     =  it_data-land1.     "KR
    it_summ_data-acct11    =  it_data-acct11.    "Accrual Acct
    it_summ_data-text11    =  it_data-text11.    "text
    it_summ_data-acct12    =  it_data-acct12.    "Duty MPF
    it_summ_data-amt012    =  0.                 "MPF Amt
* "KR"
    IF it_data-land1     = 'KR'.
      it_summ_data-no    = '1_KR'.
      it_summ_data-entno = ''.
* "space"
    ELSEIF it_data-land1  = ''.
      it_summ_data-no    = '2_XX'.
      it_summ_data-entno = ''.
* "CN"
    ELSE.
      it_summ_data-no    = '3_CN'.
    ENDIF.
    IF it_data-land1     = 'KR'
    OR it_data-land1    = ''.
      it_summ_data-acct11    =  p_expact.
    ENDIF.
    w_total_amt   = w_total_amt  + it_data-duty_amt.
    COLLECT it_summ_data.
  ENDLOOP.

  LOOP AT it_summ_data WHERE no = '3_CN'.
*   Selection screen 1. Assignment
    IF p_entn11              =  it_summ_data-entno.
      it_summ_data-amt012    =  p_amt012.
    ENDIF.
*   Selection screen 2. Assignment
    IF p_entn21              =  it_summ_data-entno.
      it_summ_data-amt012    =  p_amt022.
    ENDIF.
*   Selection screen 3. Assignment
    IF p_entn31              =  it_summ_data-entno.
      it_summ_data-amt012    =  p_amt032.
    ENDIF.
    MODIFY it_summ_data TRANSPORTING amt012.
  ENDLOOP.

  CHECK NOT it_summ_data[] IS INITIAL.

* "CN" Zero process
  PERFORM zero_cn_check_data.
* merge
  CLEAR: it_summ_data_temp, it_summ_data_temp[].
  LOOP AT it_summ_data WHERE no = '3_CN'.
    CLEAR: it_summ_data_temp.
    it_summ_data_temp-entno     =  it_summ_data-entno.
    it_summ_data_temp-acct11    =  it_summ_data-acct12.
    it_summ_data_temp-duty_amt  =  it_summ_data-amt012.
    it_summ_data_temp-no        =  it_summ_data-no.
    it_summ_data_temp-land1     =  it_summ_data-land1.
    it_summ_data_temp-text11    =  it_summ_data-text11.
    APPEND it_summ_data_temp.
  ENDLOOP.
  LOOP AT it_summ_data_temp.
    MOVE-CORRESPONDING it_summ_data_temp TO it_summ_data.
    APPEND it_summ_data.
  ENDLOOP.


* total(screen)
  w_total_amt    = w_total_amt
                 + p_amt012 + p_amt022 + p_amt032.

ENDFORM.                    " data_move_summ
*&---------------------------------------------------------------------*
*&      Form  SCREEN_SELECT_CHECK
*&---------------------------------------------------------------------*
FORM screen_select_check .

  w_check = ''.

  IF  p_acct21 = ''
  AND p_entn21 = ''
  AND p_text21 = ''
  AND p_acct22 = ''
  AND p_amt022 = 0.
*  AND p_text22 = ''.
  ELSE.
    IF p_acct21 = ''
    OR p_entn21 = ''
    OR p_text21 = ''
    OR p_acct22 = ''
    OR p_amt022 = 0.
*    OR p_text22 = ''.
      MESSAGE i000 WITH 'no.2 all required entry fields'.
      w_check = '2'.
      EXIT.
    ENDIF.
  ENDIF.

  IF  p_acct31 = ''
  AND p_entn31 = ''
  AND p_text31 = ''
  AND p_acct32 = ''
  AND p_amt032 = 0.
*  AND p_text32 = ''.
  ELSE.
    IF p_acct31 = ''
    OR p_entn31 = ''
    OR p_text31 = ''
    OR p_acct32 = ''
    OR p_amt032 = 0.
*    OR p_text32 = ''.
      MESSAGE i000 WITH 'no.3 all required entry fields'.
      w_check = '3'.
      EXIT.
    ENDIF.
  ENDIF.

  CHECK  w_check = ''.

  IF p_text21 = ''
  OR p_text21 = '01'
  OR p_text21 = '06'
  OR p_text21 = '08'.
  ELSE.
    MESSAGE i000 WITH 'Field check "Text = (01, 06, 08)"'.
    w_check = '2'.
    EXIT.
  ENDIF.
  IF p_text31 = ''
  OR p_text31 = '01'
  OR p_text31 = '06'
  OR p_text31 = '08'.
  ELSE.
    MESSAGE i000 WITH 'Field check "Text = (01, 06, 08)"'.
    w_check = '3'.
    EXIT.
  ENDIF.

ENDFORM.                    " SCREEN_SELECT_CHECK
*&---------------------------------------------------------------------*
*&      Form  DATA_POST_FIELD_MOVE
*&---------------------------------------------------------------------*
FORM data_post_field_move .

  CONCATENATE p_docdt+4(2) '/' p_docdt+6(2) '/' p_docdt+0(4) INTO w_docdt.
  CONCATENATE p_posdt+4(2) '/' p_posdt+6(2) '/' p_posdt+0(4) INTO w_posdt.

ENDFORM.                    " DATA_POST_FIELD_MOVE
*---------------------------------------------------------------------*
*       FORM BDC_DYNPRO                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PROGRAM                                                       *
*  -->  DYNPRO                                                        *
*---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR it_bdc.
  it_bdc-program  = program.
  it_bdc-dynpro   = dynpro.
  it_bdc-dynbegin = 'X'.
  APPEND it_bdc.
ENDFORM.                    "bdc_dynpro

*---------------------------------------------------------------------*
*       FORM BDC_FIELD                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAM                                                          *
*  -->  FVAL                                                          *
*---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR it_bdc.
  it_bdc-fnam = fnam.
  it_bdc-fval = fval.
  APPEND it_bdc.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  BALANCE_DATA_MOVE
*&---------------------------------------------------------------------*
FORM balance_data_move .

  CLEAR: it_balance_data, it_balance_data[].
* Balance table insert
  LOOP AT it_data.
    it_balance_data-entno    = it_data-entno.
    it_balance_data-duty_amt = it_data-duty_amt.
    COLLECT it_balance_data.
  ENDLOOP.
* balance data --> high amount
  SORT it_data DESCENDING BY entno land1  duty_amt.
  LOOP AT it_balance_data.
    CLEAR: it_ztmm_duty_hd.
    READ TABLE it_ztmm_duty_hd WITH KEY entno = it_balance_data-entno.
    LOOP AT it_data WHERE entno = it_balance_data-entno
                      AND land1 = 'KR'.
      it_data-duty_amt =   it_data-duty_amt
                       + ( it_ztmm_duty_hd-duty_amt
                       -   it_balance_data-duty_amt ).
      MODIFY it_data TRANSPORTING duty_amt.
      EXIT.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " BALANCE_DATA_MOVEedloop
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM hotspot_click  USING    p_e_row_id
                             p_e_column_id.

  READ TABLE it_data INDEX p_e_row_id.

  IF sy-subrc = 0.
    CASE p_e_column_id.
      WHEN 'BELNR'.
        SET PARAMETER ID 'BUK' FIELD  p_bukrs.
        SET PARAMETER ID 'BLN' FIELD  it_data-belnr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*          CLEAR R_UCOMM.
    ENDCASE.
  ENDIF.

ENDFORM.                    " HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  ZERO_CN_CHECK_DATA
*&---------------------------------------------------------------------*
FORM zero_cn_check_data .

* Selection screen 1. Assignment
  IF p_entn11 <> ''.
    READ TABLE it_summ_data WITH KEY no    = '3_CN'
                                     entno =  p_entn11.
    IF sy-subrc <> 0.
      CLEAR: it_summ_data.
      it_summ_data-entno      =  p_entn11.
*      it_summ_data-acct11     =  p_acct11.
*      it_summ_data-text11     =  p_text11.
      it_summ_data-acct12     =  p_acct12.
      it_summ_data-amt012     =  p_amt012.
      it_summ_data-no         = '3_CN'.
      APPEND it_summ_data.
    ENDIF.
  ENDIF.
* Selection screen 2. Assignment
  IF p_entn21 <> ''.
    READ TABLE it_summ_data WITH KEY no    = '3_CN'
                                     entno =  p_entn21.
    IF sy-subrc <> 0.
      CLEAR: it_summ_data.
      it_summ_data-entno      =  p_entn21.
*      it_summ_data-acct11     =  p_acct21.
*      it_summ_data-text11     =  p_text21.
      it_summ_data-acct12     =  p_acct22.
      it_summ_data-amt012     =  p_amt022.
      it_summ_data-no         = '3_CN'.
      APPEND it_summ_data.
    ENDIF.
  ENDIF.
* Selection screen 3. Assignment
  IF p_entn31 <> ''.
    READ TABLE it_summ_data WITH KEY no    = '3_CN'
                                     entno =  p_entn31.
    IF sy-subrc <> 0.
      CLEAR: it_summ_data.
      it_summ_data-entno      =  p_entn31.
*      it_summ_data-acct11     =  p_acct31.
*      it_summ_data-text11     =  p_text31.
      it_summ_data-acct12     =  p_acct32.
      it_summ_data-amt012     =  p_amt032.
      it_summ_data-no         = '3_CN'.
      APPEND it_summ_data.
    ENDIF.
  ENDIF.

ENDFORM.                    " ZERO_CN_CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  POSTING_DATA_CHECK
*&---------------------------------------------------------------------*
FORM posting_data_check .

  DELETE it_summ_data WHERE duty_amt = 0.
  LOOP AT it_summ_data WHERE land1  = ''
                          OR text11 = ''.
    IF  it_summ_data-land1  = ''.
      it_summ_data-land1  = 'CN'.
    ENDIF.
    IF it_summ_data-text11 = ''.
      it_summ_data-text11 = '06'.
    ENDIF.
    MODIFY it_summ_data TRANSPORTING land1 text11.
  ENDLOOP.

ENDFORM.                    " POSTING_DATA_CHECK
