*&---------------------------------------------------------------------*
*& Program ID     : ZPMR0160
*& Profram Name   : Min/Max Management.
*& Created by     : Yang
*& Created on     : 25.08.2008
*& Development ID :
*& Reference Pgm. :
*& Description    : Min/Max Management
*&
*& Modification Log
*&=====================================================================*
*& Date        Developer      Request ID      Description
*& 26.08.2008                                 first dev.
*&---------------------------------------------------------------------*

REPORT zpmr0160        NO STANDARD PAGE HEADING
                                 LINE-SIZE 300
                                 MESSAGE-ID zmmm.
*======================================================================*
*    Global Data Declaration.
*======================================================================*

*-------------------------------------------------------------------*
*   INCLUDE
*-------------------------------------------------------------------*
INCLUDE zpmr0160_top.
INCLUDE zpmr0160_def.
INCLUDE zpmr0160_cls.
INCLUDE zpmr0160_f01.

*--------------------------------------------------------------------*
*   INITIALIZATION                                                   *
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init_data.
*** set Options: save variants userspecific or general
*  g_save  =  'A'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST                                 *
*----------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
*  PERFORM f4help_layout CHANGING p_vari.
*
*--------------------------------------------------------------------*
*   AT SELECTION SCREEN                                              *
*--------------------------------------------------------------------*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgort-low.
  PERFORM help_request_s_lgort.


AT SELECTION-SCREEN ON BLOCK b1.
  PERFORM read_text USING 'LGORT'
                          s_lgort-low
                 CHANGING t_fing.

*--------------------------------------------------------------------*
*   START-OF-SELECTION                                               *
*--------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM progress_indicator.
  PERFORM query_data.

*  CHECK NOT g_alv1_t[] IS INITIAL.
  CHECK g_err IS INITIAL.
  CALL SCREEN 100.

*--------------------------------------------------------------------*
*   END-OF-SELECTION                                                 *
*--------------------------------------------------------------------*
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  query_data
*&---------------------------------------------------------------------*
FORM query_data .

  CLEAR : g_itab_t,  g_itab_t[],
          g_itab2_t, g_itab2_t[],
          g_detail_t, g_detail_t[],
          g_err,
          g_check, g_cond, g_cond[].

  SELECT SINGLE pernr ename
         INTO (p_pernr, t_pernr)
         FROM pa0001
       WHERE pernr = p_pernr
         AND endda >= sy-datum
         AND begda <= sy-datum.

  IF sy-subrc NE 0.
    MESSAGE i999(zmmm) WITH text-m31.
    g_err  = 'X'.
    EXIT.
  ENDIF.

  CASE 'X'.
    WHEN p_r10.  " All

    WHEN p_r20. " Min/Max Maintained
      CONCATENATE 'AND' ' A~LMINB  NE ''' '0' ''''   INTO g_cond-text.
      APPEND g_cond.
      CONCATENATE 'AND' ' A~LBSTF  NE ''' '0' ''''   INTO g_cond-text.
      APPEND g_cond.
    WHEN p_r30. " P/R suggested
      CONCATENATE 'AND' ' A~LMINB  NE ''' '0' ''''   INTO g_cond-text.
      APPEND g_cond.
      CONCATENATE 'AND' ' A~LBSTF  NE ''' '0' ''''   INTO g_cond-text.
      APPEND g_cond.

  ENDCASE.

  READ TABLE g_cond INDEX 1.
  MOVE '   ' TO g_cond-text(3).
  MODIFY g_cond INDEX 1.

** Fuorng on 03/30/12
*  SELECT a~matnr a~labst a~lminb
*         a~lbstf b~wrkst b~meins a~lgpbe
*     INTO CORRESPONDING FIELDS OF TABLE g_itab2_t
*       FROM mard AS a INNER JOIN mara AS b
*          ON a~matnr EQ b~matnr
*        WHERE a~werks IN s_werks
*          AND a~lgort IN s_lgort
*          AND a~matnr IN s_matnr
*          AND b~mtart EQ 'ERSA'
*          AND (g_cond).

  SELECT a~matnr a~labst a~lminb
          a~lbstf b~wrkst b~meins a~lgpbe
          mfrpn mfrnr c~maabc       "Added Maabc on 3.11.2014
      INTO CORRESPONDING FIELDS OF TABLE g_itab2_t
        FROM mard AS a INNER JOIN mara AS b
           ON a~matnr EQ b~matnr
                       INNER JOIN marc AS c
           ON a~matnr = c~matnr
          AND a~werks = c~werks
         WHERE a~werks IN s_werks
           AND a~lgort IN s_lgort
           AND a~matnr IN s_matnr
           AND a~lvorm = ' '
           AND b~mtart EQ 'ERSA'
           AND (g_cond).

** End on 03/30/12

  SORT : g_itab2_t.

  LOOP AT g_itab2_t.
    MOVE-CORRESPONDING g_itab2_t TO g_itab_t.
    g_itab_t-lgort = s_lgort-low.
** Fuorng on 03/30/12
    SELECT SINGLE name1 FROM lfa1 INTO g_itab_t-name1
       WHERE lifnr = g_itab_t-mfrnr.
** End on 03/30/12

    COLLECT g_itab_t. CLEAR g_itab_t.
  ENDLOOP.

  PERFORM read_open_pr_data.
  PERFORM read_open_po_data.

  PERFORM data_sub_query.

ENDFORM.                    " query_data

*&---------------------------------------------------------------------*
*&      Form  data_sub_query
*&---------------------------------------------------------------------*
FORM data_sub_query .

  CLEAR : g_alv1_t, g_alv1_t[].

  LOOP AT g_itab_t.
    MOVE-CORRESPONDING  g_itab_t  TO  g_alv1_t.

    READ TABLE it_preq_t WITH KEY matnr = g_itab_t-matnr.
    IF sy-subrc EQ 0.
      g_alv1_t-menge_pr = it_preq_t-menge.
    ENDIF.

    READ TABLE it_poqty_t WITH KEY matnr = g_itab_t-matnr.
    IF sy-subrc EQ 0.
      g_alv1_t-menge_po = it_poqty_t-opnqt.
    ENDIF.

** Fuorng on 04/11/12
*    g_alv1_t-zreqty = g_alv1_t-lbstf - g_alv1_t-labst
*                 - g_alv1_t-menge_pr - g_alv1_t-menge_po.
*   g_alv1_t-zreqty = g_alv1_t-LMINB - g_alv1_t-labst
*                 - g_alv1_t-menge_pr - g_alv1_t-menge_po.
    IF g_alv1_t-labst =< g_alv1_t-lminb.
      g_alv1_t-zreqty = g_alv1_t-lbstf - g_alv1_t-labst
                    - g_alv1_t-menge_pr - g_alv1_t-menge_po.
    ENDIF.
** End on 04/11/12

    IF g_alv1_t-zreqty < 0.
      g_alv1_t-zreqty = 0.
    ENDIF.

    IF p_r30 EQ 'X' AND  g_alv1_t-zreqty = 0.
      CONTINUE.
    ENDIF.

    PERFORM read_text   USING   'MATNR'
                                 g_itab_t-matnr
                        CHANGING g_alv1_t-maktx.

    APPEND  g_alv1_t.
    CLEAR  g_alv1_t.
  ENDLOOP.

ENDFORM.                    " data_sub_query

*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " status_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  create_control  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_control OUTPUT.
** Detail list.
  IF  container_spliter IS INITIAL.
*/ Create the splitter container
    PERFORM create_splitter.

    PERFORM create_grid1.
*/ GRID LIST2
    PERFORM create_grid2.
  ENDIF.

ENDMODULE.                 " create_control  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit_command  INPUT
*&---------------------------------------------------------------------*
MODULE exit_command INPUT.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.                 " exit_command  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  IF NOT grid1 IS INITIAL.
    CALL METHOD grid1->check_changed_data .
  ENDIF.

  IF NOT grid2 IS INITIAL.
    CALL METHOD grid2->check_changed_data .
  ENDIF.

  CASE  sy-ucomm.
    WHEN  'REFS'.
      PERFORM query_data.
  ENDCASE.

ENDMODULE.                 " user_command_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  create_docking
*&---------------------------------------------------------------------*
FORM create_docking .

  CREATE OBJECT docking
    EXPORTING
      repid = 'ZPMR0160'
      dynnr = '0100'
      side  = docking->dock_at_top.

ENDFORM.                    " create_docking

*&---------------------------------------------------------------------*
*&      Form  create_grid1
*&---------------------------------------------------------------------*
FORM create_grid1 .

  CREATE OBJECT grid1
    EXPORTING
      i_parent = container_0.

  PERFORM set_field_catalogs_grid1.

  PERFORM set_cell_attribute.

*/ LAYOUT SETTING
  PERFORM set_option_grid1.

*// Tool Bar
  PERFORM exclude_of_toolbar_button
                        USING 'gt_tbar_excluding1'.

  CALL METHOD grid1->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*// 'gs_layout' must at least contain the report-id to allow
  g_repid = sy-repid.
*// 2.At least field REPORT of this structure has to be filled!
  alv_variant-report = g_repid.

*/ ALV DATA
  CALL METHOD grid1->set_table_for_first_display
    EXPORTING
      is_layout            = gs_layout2
      it_toolbar_excluding = gt_tbar_excluding1
      i_save               = 'A'   "
      i_default            = 'X'   " �
*  is_variant �
      is_variant           = alv_variant
    CHANGING
      it_outtab            = g_alv1_t[]
      it_fieldcatalog      = gt_lvc1[].


* register f4 for field CLASS
  PERFORM register_events.

* Set editable cells to ready for input initially
  CALL METHOD grid1->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDFORM.                    " create_grid1

*&---------------------------------------------------------------------*
*&      Form  create_splitter
*&---------------------------------------------------------------------*
FORM create_splitter .

  DATA : lv_value TYPE i.

  CREATE OBJECT container_spliter
    EXPORTING
      container_name = 'CC01'.

  CREATE OBJECT splitter
    EXPORTING
      parent  = container_spliter
      rows    = 2
      columns = 1.

  CALL METHOD splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = container_0.

  CALL METHOD splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = container_1.

ENDFORM.                    " create_splitter
*&---------------------------------------------------------------------*
*&      Form  create_grid2
*&---------------------------------------------------------------------*
FORM create_grid2 .
  DATA: lt_f4 TYPE lvc_t_f4 WITH HEADER LINE.

  CREATE OBJECT grid2
    EXPORTING
      i_parent = container_1.

  PERFORM set_field_catalogs_grid2.

  PERFORM set_cell_attribute_2.

  CALL METHOD grid2->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

** LAYOUT SETTING
  PERFORM set_option_grid2.

*// 'gs_layout' must at least contain the report-id to allow
  g_repid = sy-repid.
*// 2.At least field REPORT of this structure has to be filled!
  IF p_r10 EQ 'X'.
    alv_variant-report = 'ZPMR0160_2'.
  ELSE.
    alv_variant-report = 'ZPMR0160_3'.
  ENDIF.

*/ ALV DATA
  CALL METHOD grid2->set_table_for_first_display
    EXPORTING
      is_layout            = gs_layout
      it_toolbar_excluding = gt_tbar_excluding2
      i_save               = 'A'   "
      is_variant           = alv_variant  " display
    CHANGING
      it_outtab            = g_alv2_t[]
      it_fieldcatalog      = gt_lvc2[]
      it_sort              = gt_sort[].


  CLEAR lt_f4.
  lt_f4-fieldname = 'AUFNR'.
  lt_f4-register = 'X'.
  lt_f4-getbefore = 'X'.
  lt_f4-chngeafter = space.
  INSERT TABLE lt_f4.

  CLEAR lt_f4.
  lt_f4-fieldname = 'DT_CAT'.
  lt_f4-register = 'X'.
  lt_f4-getbefore = 'X'.
  lt_f4-chngeafter = space.
  INSERT TABLE lt_f4.

  CALL METHOD grid2->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

*/ ALV
  CREATE OBJECT event_receiver.

  SET HANDLER event_receiver->handle_user_command2           FOR grid2.
  SET HANDLER event_receiver->handle_hotspot_click2          FOR grid2.
  SET HANDLER event_receiver->handle_toolbar2                FOR grid2.
  SET HANDLER event_receiver->handle_double_click2           FOR grid2.
  SET HANDLER event_receiver->handle_data_changed_finished2  FOR grid2.
  SET HANDLER event_receiver->handle_data_changed2           FOR grid2.
  SET HANDLER event_receiver->handle_onf4_2                  FOR grid2.

  CALL METHOD grid2->set_toolbar_interactive.

ENDFORM.                    " create_grid2

*&---------------------------------------------------------------------*
*&      Form  create_grid3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_grid3 .

ENDFORM.                    " create_grid3
*&---------------------------------------------------------------------*
*&      Form  set_field_catalogs_GRID3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_catalogs_grid3 .


  REFRESH: gt_lvc3[].
  PERFORM fill_field_catalogs_3
          USING:
          'S' 'FIELDNAME'   'VBELN',
          ' ' 'COLTEXT'     '주문번호',
          ' ' 'JUST'        'L',
          ' ' 'KEY'         'X',
          ' ' 'HOTSPOT'     'X',
          ' ' 'CONVEXIT'    'ALPHA',
          'E' 'OUTPUTLEN'   '10',

          'S' 'FIELDNAME'   'POSNR',
          ' ' 'COLTEXT'     '품목번호',
          ' ' 'JUST'        'L',
          ' ' 'KEY'         'X',
          'E' 'OUTPUTLEN'   '08'.

ENDFORM.                    " set_field_catalogs_GRID3
*&---------------------------------------------------------------------*
*&      Form  set_option_GRID3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_option_grid3 .

  CLEAR: gs_layout.
* 1. LAYOUT OPTION
  gs_layout-sel_mode   = 'D'.
*  gs_layout-no_rowmark = 'X'.
  gs_layout-zebra      = 'X'.
  gs_layout-cwidth_opt = 'X'.
  gs_layout-no_toolbar = ' '.
  gs_layout-grid_title = ' '.

* 2. EXCLUDING MENU
  CLEAR :   gt_tbar_excluding3[].
  APPEND : '&LOCAL&CUT'           TO gt_tbar_excluding3,
           '&LOCAL&COPY'          TO gt_tbar_excluding3,
           '&LOCAL&PASTE'         TO gt_tbar_excluding3,
           '&LOCAL&PASTE_NEW_ROW' TO gt_tbar_excluding3,
           '&LOCAL&UNDO'          TO gt_tbar_excluding3,
           '&LOCAL&APPEND'        TO gt_tbar_excluding3,
           '&LOCAL&INSERT_ROW'    TO gt_tbar_excluding3,
           '&LOCAL&DELETE_ROW'    TO gt_tbar_excluding3,
           '&LOCAL&COPY_ROW'      TO gt_tbar_excluding3,
           '&DETAIL'              TO gt_tbar_excluding3,
           '&FIND'                TO gt_tbar_excluding3,
           '&CRBATCH'             TO gt_tbar_excluding3,
           '&MB_VIEW'             TO gt_tbar_excluding3,
           '&MB_EXPORT'           TO gt_tbar_excluding3,
           '&GRAPH'               TO gt_tbar_excluding3,
           '&INFO'                TO gt_tbar_excluding3,
           '&REFRESH'             TO gt_tbar_excluding3.

ENDFORM.                    " set_option_GRID3
*&---------------------------------------------------------------------*
*&      Form  fill_field_catalogs_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0793   text
*      -->P_0794   text
*      -->P_0795   text
*----------------------------------------------------------------------*
FORM fill_field_catalogs_3  USING    p_gub  p_fname  p_value.

  IF p_gub = 'S'.

    CLEAR gs_lvc3.

  ENDIF.

  DATA l_fname(40).
  FIELD-SYMBOLS <fs> TYPE any.
  CONCATENATE 'GS_LVC3-' p_fname INTO l_fname.

  ASSIGN (l_fname) TO <fs>.
  <fs> = p_value.

  IF p_gub = 'E'.

    APPEND gs_lvc3 TO gt_lvc3.

  ENDIF.

ENDFORM.                    " fill_field_catalogs_3

*&---------------------------------------------------------------------*
*&      Form  set_field_catalogs_GRID1
*&---------------------------------------------------------------------*
FORM set_field_catalogs_grid1 .
  DATA: l_datum(08).

*-added on 3.11.2014 Victor
  sy-datum = sy-datum + 1.
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.

**
  REFRESH : gt_lvc1[].

  IF p_r30 EQ 'X'.

    PERFORM fill_field_catalogs
            USING:
            'S' 'FIELDNAME'   'BOX',
            ' ' 'COLTEXT'     'Select',
            ' ' 'JUST'        'L',
            ' ' 'KEY'         'X',
            ' ' 'CHECKBOX'    'X',
            ' ' 'EDIT'        'X',
*            ' ' 'NO_OUT'      'X',
            'E' 'OUTPUTLEN'   '1'.

  ENDIF.


  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = c_st_nm
    CHANGING
      ct_fieldcat      = gt_lvc1.

  PERFORM change TABLES gt_lvc1.



ENDFORM.                    " set_field_catalogs_GRID1

*&---------------------------------------------------------------------*
*&      Form  set_option_GRID1
*&---------------------------------------------------------------------*
FORM set_option_grid1 .

  CLEAR : gs_layout2.

* 1. LAYOUT OPTION
  gs_layout2-sel_mode   = 'D'.
*  gs_layout-no_rowmark = 'X'.
  gs_layout2-zebra      = 'X'.
  gs_layout2-cwidth_opt = 'X'.
  gs_layout2-stylefname = 'CELLTAB'.
  gs_layout2-ctab_fname = 'F_COL'.

ENDFORM.                    " set_option_GRID1

*&---------------------------------------------------------------------*
*&      Form  exclude_of_toolbar_button
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0756   text
*----------------------------------------------------------------------*
FORM exclude_of_toolbar_button  USING    p_tabname..
  DATA : l_tab_name LIKE feld-name.

  FIELD-SYMBOLS : <table> TYPE ui_functions.

  CONCATENATE p_tabname '[]' INTO  l_tab_name.
  ASSIGN     (l_tab_name)    TO <table>.

*// 제거할 버튼 지정
  PERFORM add_exclude_toolbar_button
         TABLES <table>
*        USING : cl_gui_alv_grid=>mc_fc_excl_all. " ** 툴바 모두제거 **
        USING : cl_gui_alv_grid=>mc_fc_loc_undo, " 실행취소&LOCAL&UNDO
                cl_gui_alv_grid=>mc_fc_auf,      " 소계확장 &AUF
                cl_gui_alv_grid=>mc_fc_average,  " &AVERAGE
*                cl_gui_alv_grid=>mc_fc_back_classic,
*                cl_gui_alv_grid=>mc_fc_call_abc, " &ABC
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
*                cl_gui_alv_grid=>mc_fc_delete_filter,
*                cl_gui_alv_grid=>mc_fc_deselect_all,
*                cl_gui_alv_grid=>mc_fc_detail,
*                cl_gui_alv_grid=>mc_fc_expcrdata,
*                cl_gui_alv_grid=>mc_fc_expcrdesig,
*                cl_gui_alv_grid=>mc_fc_expcrtempl,
*                cl_gui_alv_grid=>mc_fc_expmdb,
*                cl_gui_alv_grid=>mc_fc_extend,
*                cl_gui_alv_grid=>mc_fc_f4,
*                cl_gui_alv_grid=>mc_fc_filter,
*                cl_gui_alv_grid=>mc_fc_find,
*                cl_gui_alv_grid=>mc_fc_fix_columns,
                cl_gui_alv_grid=>mc_fc_graph,
*                cl_gui_alv_grid=>mc_fc_help,
                cl_gui_alv_grid=>mc_fc_info,
*                cl_gui_alv_grid=>mc_fc_load_variant,
                cl_gui_alv_grid=>mc_fc_loc_copy,          " 행 카피.
*                cl_gui_alv_grid=>mc_fc_html,
                cl_gui_alv_grid=>mc_fc_loc_copy_row,      " 행 카피.
                cl_gui_alv_grid=>mc_fc_loc_cut,           " 가위.
                cl_gui_alv_grid=>mc_fc_loc_delete_row,    " 행삭제.
                cl_gui_alv_grid=>mc_fc_loc_insert_row,    " 행삽입.
                cl_gui_alv_grid=>mc_fc_loc_move_row,
                cl_gui_alv_grid=>mc_fc_loc_append_row,    " 라인생성.
                cl_gui_alv_grid=>mc_fc_loc_paste,         " 겹쳐쓰기.
                cl_gui_alv_grid=>mc_fc_loc_paste_new_row, " 겹쳐쓰기.
*                cl_gui_alv_grid=>mc_fc_maintain_variant,
*                cl_gui_alv_grid=>mc_fc_maximum,
*                cl_gui_alv_grid=>mc_fc_minimum,
*                cl_gui_alv_grid=>mc_fc_pc_file,
*                cl_gui_alv_grid=>mc_fc_print,
*                cl_gui_alv_grid=>mc_fc_print_back,
*                cl_gui_alv_grid=>mc_fc_print_prev,
                cl_gui_alv_grid=>mc_fc_refresh.
*                cl_gui_alv_grid=>mc_fc_reprep,
*                cl_gui_alv_grid=>mc_fc_save_variant,
*                cl_gui_alv_grid=>mc_fc_select_all,
*                cl_gui_alv_grid=>mc_fc_send,
*                cl_gui_alv_grid=>mc_fc_separator,
*                cl_gui_alv_grid=>mc_fc_sort,
*                cl_gui_alv_grid=>mc_fc_sort_asc,
*                cl_gui_alv_grid=>mc_fc_sort_dsc,
*                cl_gui_alv_grid=>mc_fc_subtot,
*                cl_gui_alv_grid=>mc_mb_sum,
*                cl_gui_alv_grid=>mc_fc_sum.
*                cl_gui_alv_grid=>mc_fc_to_office,
*                cl_gui_alv_grid=>mc_fc_to_rep_tree,
*                cl_gui_alv_grid=>mc_fc_unfix_columns,
*                cl_gui_alv_grid=>mc_fc_views,
*                cl_gui_alv_grid=>mc_fc_view_crystal,
*                cl_gui_alv_grid=>mc_fc_view_excel,
*                cl_gui_alv_grid=>mc_fc_view_grid,
*                cl_gui_alv_grid=>mc_fc_word_processor.

ENDFORM.                    " exclude_of_toolbar_button

*&---------------------------------------------------------------------*
*&      Form  add_exclude_toolbar_button
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_exclude_toolbar_button  TABLES   p_table
                                 USING    p_value.

  DATA: l_exclude TYPE ui_func.

  l_exclude = p_value.
  APPEND l_exclude TO p_table. "결과적으로 GT_EXCLUDE에 제거할 버튼 저장

ENDFORM.                    " add_exclude_toolbar_button

*&---------------------------------------------------------------------*
*&      Form  fill_field_catalogs
*&---------------------------------------------------------------------*
FORM fill_field_catalogs  USING    p_gub  p_fname  p_value.

** 'S' -> Structure
** 'E' -> Structure

  IF  p_gub  =  'S'.
    CLEAR  gs_lvc1.
  ENDIF.

**
  DATA l_fname(40).
  FIELD-SYMBOLS <fs> TYPE any.
  CONCATENATE 'GS_LVC1-' p_fname INTO l_fname.

  ASSIGN (l_fname) TO <fs>.
  <fs> = p_value.

  IF  p_gub  =  'E'.
    APPEND gs_lvc1 TO gt_lvc1.
  ENDIF.

ENDFORM.                    " fill_field_catalogs

*&---------------------------------------------------------------------*
*&      Form  set_field_catalogs_GRID2
*&---------------------------------------------------------------------*
FORM set_field_catalogs_grid2 .

  DATA : l_st_nm  LIKE dd02l-tabname.

  REFRESH : gt_lvc2[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = c_st_nm_2
    CHANGING
      ct_fieldcat      = gt_lvc2.

  PERFORM change_2 TABLES gt_lvc2.

ENDFORM.                    " set_field_catalogs_GRID2

*&---------------------------------------------------------------------*
*&      Form  set_option_GRID2
*&---------------------------------------------------------------------*
FORM set_option_grid2 .

  CLEAR : gs_layout.

* 1. LAYOUT OPTION - 라인 선택 가능
  gs_layout-sel_mode   = 'D'.
*  gs_layout-no_rowmark = 'X'.
  gs_layout-zebra      = 'X'.
*  gs_layout-cwidth_opt = 'X'.
  gs_layout-stylefname = 'CELLTAB'.
  gs_layout-ctab_fname = 'F_COL'.
  gs_layout-grid_title = ' '.

* 2. EXCLUDING MENU
  CLEAR :   gt_tbar_excluding2[].
  APPEND : '&REFRESH'             TO gt_tbar_excluding2,
           '&LOCAL&CUT'           TO gt_tbar_excluding2,
           '&LOCAL&COPY'          TO gt_tbar_excluding2,
           '&LOCAL&PASTE'         TO gt_tbar_excluding2,
           '&LOCAL&PASTE_NEW_ROW' TO gt_tbar_excluding2,
           '&LOCAL&UNDO'          TO gt_tbar_excluding2,
           '&LOCAL&APPEND'        TO gt_tbar_excluding2,
           '&LOCAL&INSERT_ROW'    TO gt_tbar_excluding2,
           '&LOCAL&DELETE_ROW'    TO gt_tbar_excluding2,
           '&LOCAL&COPY_ROW'      TO gt_tbar_excluding2,
           '&DETAIL'              TO gt_tbar_excluding2.

ENDFORM.                    " set_option_GRID2

*&---------------------------------------------------------------------*
*&      Form  fill_field_catalogs_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0544   text
*      -->P_0545   text
*      -->P_0546   text
*----------------------------------------------------------------------*
FORM fill_field_catalogs_2  USING    p_gub  p_fname  p_value.

*- 'S' -> Structure에
*- 'E' -> Structure의

  IF p_gub = 'S'.

    CLEAR gs_lvc2.

  ENDIF.

  DATA l_fname(40).
  FIELD-SYMBOLS <fs> TYPE any.
  CONCATENATE 'GS_LVC2-' p_fname INTO l_fname.

  ASSIGN (l_fname) TO <fs>.
  <fs> = p_value.

  IF p_gub = 'E'.

    APPEND gs_lvc2 TO gt_lvc2.

  ENDIF.

ENDFORM.                    " fill_field_catalogs_2

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_GRID2
*&---------------------------------------------------------------------*
FORM get_data_grid2.

  DATA : l_tabix LIKE sy-tabix,
         lv_menge LIKE lips-lfimg,
         lv_gbsta LIKE vbup-gbsta,
         lv_flg.

  READ TABLE g_alv1_t INTO g_alv1_s INDEX gv_index.

  CHECK sy-subrc = 0.

  g_alv1_s-box = 'X'.

  MODIFY g_alv1_t FROM g_alv1_s
                  TRANSPORTING box
                WHERE matnr = g_alv1_s-matnr.

  MOVE-CORRESPONDING g_alv1_s TO g_alv2_s.

  SELECT SINGLE verpr peinh
         INTO (g_alv2_s-verpr, g_alv2_s-peinh)
         FROM mbew
         WHERE matnr = g_alv1_s-matnr
           AND bwkey IN s_werks.

  g_alv2_s-waers = 'USD'.
  g_alv2_s-lfdat = sy-datum.

  SELECT SINGLE ekgrp
         INTO g_alv2_s-ekgrp
         FROM marc
         WHERE matnr = g_alv1_s-matnr
           AND werks IN s_werks.

  APPEND g_alv2_s TO g_alv2_t.

ENDFORM.                    " GET_DATA_GRID2
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_GRID2
*&---------------------------------------------------------------------*
FORM get_data_grid2_2.

  DATA : l_tabix LIKE sy-tabix,
         lv_menge LIKE lips-lfimg,
         lv_gbsta LIKE vbup-gbsta,
         lv_flg.

  READ TABLE g_alv1_t INTO g_alv1_s INDEX gv_index.

  CHECK sy-subrc = 0.

  DELETE g_alv2_t WHERE matnr = g_alv1_s-matnr.


*  SORT g_alv2_t BY idnrk.


ENDFORM.                    " GET_DATA_GRID2


*&---------------------------------------------------------------------*
*&      Form  build_sort_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_sort_field .
*// Sort 필드 설정
  CLEAR : gt_sort, gt_sort[].

*  gs_sort-fieldname = 'VBELN'.
*  gs_sort-spos      = '1'.
*  gs_sort-up        = 'X'.
**  gs_sort-subtot    = 'X'.   "SUBTOTAL 집계
*  APPEND gs_sort TO gt_sort.
*
ENDFORM.                    " build_sort_field


*&---------------------------------------------------------------------*
*&      Form  change
*&---------------------------------------------------------------------*
FORM change TABLES  pt_fieldcat TYPE lvc_t_fcat.

  DATA:l_fieldcat_s LIKE pt_fieldcat,
        l_name(3).

  LOOP AT pt_fieldcat INTO l_fieldcat_s.

    CASE l_fieldcat_s-fieldname.
      WHEN 'LMINB'.
        l_fieldcat_s-coltext = 'Min'.
      WHEN 'LBSTF'.
        l_fieldcat_s-coltext = 'Max'.
      WHEN 'MATNR'.
        l_fieldcat_s-hotspot = 'X'.
      WHEN 'MENGE_PO'.
        l_fieldcat_s-coltext = 'P/O Qty'.
*        l_fieldcat_s-hotspot = 'X'.
      WHEN 'MENGE_PR'.
        l_fieldcat_s-coltext = 'P/R Qty'.
*        l_fieldcat_s-hotspot = 'X'.
*        l_fieldcat_s-f4availabl = 'X'.
** On 03/29/12
      WHEN 'LABST'.
        l_fieldcat_s-coltext = 'Available Stock'.
** End
    ENDCASE.

    MODIFY pt_fieldcat FROM l_fieldcat_s INDEX sy-tabix.

  ENDLOOP.

ENDFORM.                    " change

*&---------------------------------------------------------------------*
*&      Form  change
*&---------------------------------------------------------------------*
FORM change_2 TABLES  pt_fieldcat TYPE lvc_t_fcat.

  DATA:l_fieldcat_s LIKE pt_fieldcat,
        l_name(3).

  LOOP AT pt_fieldcat INTO l_fieldcat_s.

    CASE l_fieldcat_s-fieldname.
      WHEN 'LFDAT' OR
           'WAERS' OR
           'EKGRP'.
        l_fieldcat_s-edit = 'X'.
        l_fieldcat_s-f4availabl = 'X'.
      WHEN 'AUFNR'.
        l_fieldcat_s-no_zero = 'X'.
        l_fieldcat_s-edit = 'X'.
    ENDCASE.

    MODIFY pt_fieldcat FROM l_fieldcat_s INDEX sy-tabix.

  ENDLOOP.

ENDFORM.                    " change

*&---------------------------------------------------------------------*
*&      Form  read_open_order
*&---------------------------------------------------------------------*
*       Read Open Orders
*----------------------------------------------------------------------*
FORM read_open_po_data.
*--> Message ausgeben
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      text = text-odr.

  CHECK NOT g_itab_t[] IS INITIAL.

  CLEAR : it_poqty, it_poqty[],
          it_poqty_t, it_poqty_t[].

  SELECT p~matnr p~werks p~meins
         p~menge
    INTO CORRESPONDING FIELDS OF TABLE it_poqty
    FROM ekko AS k INNER JOIN ekpo AS p
                      ON k~ebeln = p~ebeln
                   INNER JOIN eket AS t
                      ON p~ebeln = t~ebeln
                     AND p~ebelp = t~ebelp
     FOR ALL ENTRIES IN g_itab_t
    WHERE p~matnr = g_itab_t-matnr
      AND p~werks = s_werks-low
      AND p~loekz = space
      AND p~elikz = space
      AND p~lgort = s_lgort-low
      AND k~bstyp IN ('F','L').

  LOOP AT it_poqty.
    SELECT *
      FROM eket
     WHERE ebeln EQ it_poqty-ebeln
       AND ebelp EQ it_poqty-ebelp.
      it_poqty-wemng = it_poqty-wemng + eket-wemng.
    ENDSELECT.
    IF it_poqty-bstae IS INITIAL.
      it_poqty-opnqt = it_poqty-menge - it_poqty-wemng.
    ELSE.
*---- Notified quantity
      SELECT SUM( menge )
        INTO it_poqty-ameng
        FROM ekes
       WHERE ebeln = it_poqty-ebeln
         AND ebelp = it_poqty-ebelp
         AND ebtyp = 'LA'.
      it_poqty-opnqt = it_poqty-menge - it_poqty-ameng.
    ENDIF.
    IF it_poqty-opnqt < 0.
      CLEAR it_poqty-opnqt.
    ENDIF.
    MODIFY it_poqty.
  ENDLOOP.


  LOOP AT it_poqty.
    MOVE-CORRESPONDING it_poqty TO it_poqty_t.
    CLEAR : it_poqty_t-ebeln,
            it_poqty_t-ebelp,
            it_poqty_t-bstae.
    COLLECT it_poqty_t. CLEAR it_poqty_t.
  ENDLOOP.


ENDFORM.                    " read_open_order
