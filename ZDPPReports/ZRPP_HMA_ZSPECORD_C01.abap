*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_C01                                        *
*----------------------------------------------------------------------*
INCLUDE zrpp_common_alvc.
TYPE-POOLS : rsds.

*---------------------------------------------------------------------*
*FORM  CREATE_OBJECT
*---------------------------------------------------------------------*
*TEXT :
*---------------------------------------------------------------------*
FORM p1000_create_object .

  IF g_docking_container IS INITIAL.
    CREATE OBJECT g_docking_container
      EXPORTING
        repid     = gv_repid
        dynnr     = '0100'
        side      = cl_gui_docking_container=>dock_at_bottom
        extension = 2000.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = g_docking_container.

    gs_o_layout-report = g_repid_c = gv_repid .

    CLEAR : gs_layout.
*--< Commented by Victor 06.27.2011
*    PERFORM set_input_con
*            USING g_grid ' ' '0'.

*   DATA MODIFY EVENT
    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*-->

    PERFORM set_sort_c USING:
                  '1' 'STATUS'  'X' '' '' '' '' '',

                  '4' 'WO_SER'  'X' '' '' '' '' '',
                  '5' 'NATION'  'X' '' '' '' '' ''.
*                  '4' ''  'X' '' '' '' '' '',
*                  '5' 'WERKS'  'X' '' '' '' '' '',
*                  '6' 'WERKS'  'X' '' '' '' '' ''.

*    gs_layout-stylefname = 'H_STYLE'.
    gs_layout-zebra      = 'X'.
    gs_layout-sel_mode   = 'A'.
*    ROW COLOR
*    GS_LAYOUT-INFO_FNAME = 'COLOR'.
*    CELL COLOR
*    GS_LAYOUT-CTAB_FNAME = 'TABCOLOR'.
**   BOX
*    gs_layout-box_fname  = 'MARK'.
    gs_layout-cwidth_opt = 'X'.
*    gs_layout-edit = 'X'.

* Title
*    GS_LAYOUT-GRID_TITLE = TEXT-TT1 .


*---Exclude Icon
*    APPEND cl_gui_alv_grid=>mc_fc_excl_all TO gt_excl_func.
    APPEND     cl_gui_alv_grid=>mc_fc_loc_copy_row TO gt_excl_func.
    APPEND     cl_gui_alv_grid=>mc_fc_loc_cut TO gt_excl_func.
    APPEND     cl_gui_alv_grid=>mc_fc_loc_delete_row TO gt_excl_func.
    APPEND     cl_gui_alv_grid=>mc_fc_loc_insert_row TO gt_excl_func.
    APPEND     cl_gui_alv_grid=>mc_fc_loc_move_row TO gt_excl_func.
    APPEND     cl_gui_alv_grid=>mc_fc_loc_append_row TO gt_excl_func.
    APPEND     cl_gui_alv_grid=>mc_fc_loc_paste      TO gt_excl_func.
    APPEND     cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO gt_excl_func.
    APPEND     cl_gui_alv_grid=>mc_fc_loc_copy TO gt_excl_func.

    PERFORM p1010_set_grid_events
                          USING g_grid
                                  'X'.

    PERFORM get_filedcat_alv  USING gt_fieldcat[].
    PERFORM call_grid_display_old.
*    PERFORM CALL_GRID_DISPLAY  TABLES GT_DATA[]  USING  G_GRID.

  ELSE.

*--< commented by Victor 06.29.2011
*    CALL METHOD g_grid->set_frontend_fieldcatalog
*      EXPORTING
*        it_fieldcatalog = gt_fieldcat[].
*
*    CALL METHOD g_grid->set_frontend_layout
*      EXPORTING
*        is_layout = gs_layout.
*-->

  ENDIF.

ENDFORM.                    "CREATE_OBJECT

*&---------------------------------------------------------------------*
*&      Form  P1010_SET_GRID_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_GRID  text
*      -->P_0097   text
*----------------------------------------------------------------------*
FORM p1010_set_grid_events
  USING p_grid TYPE REF TO cl_gui_alv_grid
           p_toolbar.

  DATA : p_object TYPE REF TO cl_alv_event_toolbar_set,
         p_er_data_changed TYPE REF TO cl_alv_changed_data_protocol,
         ps_row_no     TYPE lvc_s_roid,
         pr_event_data TYPE REF TO cl_alv_event_data,
         pt_bad_cells  TYPE lvc_t_modi.

  CREATE OBJECT g_events.

*****  이벤트 핸들러 등록
*_DOUBLE CLICK
  SET HANDLER g_events->double_click FOR p_grid.
*  PERFORM EVENT_DOUBLE_CLICK
*          USING '' '' ''.

* HOTSPOT
  SET HANDLER g_events->hotspot_click FOR p_grid.

*_DATA CHANGED
  SET HANDLER g_events->data_changed FOR p_grid.
  PERFORM event_data_changed
          USING p_er_data_changed '' '' '' ''.

*_DATA CHANGED FINISHED
  SET HANDLER g_events->data_changed_finished FOR p_grid.
  PERFORM event_data_changed_finis
          USING ''.

  SET HANDLER g_events->print_top_of_page FOR p_grid.

  CHECK NOT p_toolbar IS INITIAL.
  SET HANDLER g_events->user_command FOR p_grid.
  PERFORM  event_ucomm
          USING ''
                '' .

  SET HANDLER g_events->toolbar FOR p_grid.
  PERFORM  event_toolbar
          USING p_object
                '' ''.

ENDFORM.                    " P1010_SET_GRID_EVENTS

*&---------------------------------------------------------------------*
*&      Form  EVENT_UCOMM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0243   text
*      -->P_0244   text
*----------------------------------------------------------------------*
FORM event_ucomm   USING   e_ucomm LIKE sy-ucomm
                                                  p_check.
**---------------------------------------------------------------
*  CHECK P_CHECK EQ 'X'.
**
**---------------------------------------------------------------
*  CASE E_UCOMM.
**___재전송
*    WHEN '&CS03'.
*      PERFORM P3000_CALL_CS03.
*  ENDCASE.

ENDFORM.                    " P1020_EVENT_UCOMM
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OBJECT  text
*      -->P_0254   text
*      -->P_0255   text
*----------------------------------------------------------------------*
FORM event_toolbar
   USING  e_object TYPE REF TO cl_alv_event_toolbar_set
               e_interactive TYPE c
               p_check.

*---------------------------------------------------------------
  CHECK p_check EQ 'X' .

*---------------------------------------------------------------
*_변경일때만 추가 버튼 삽임
*  CHECK S_CHANGE IS NOT INITIAL.

  DATA : ls_toolbar  TYPE stb_button.

*_SET : BUTTON TYPE - SEPARATOR
  CLEAR : ls_toolbar.
*  LS_TOOLBAR-BUTN_TYPE = 3.
*  APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

*  LS_TOOLBAR-FUNCTION = '&MMBE'.
*  LS_TOOLBAR-ICON = ICON_BIW_REPORT.
*  LS_TOOLBAR-QUICKINFO = '재고조회'.
*  LS_TOOLBAR-TEXT = ' 재고 조회'.
*  APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

ENDFORM.                    " P1030_EVENT_TOOLBAR
*&--------------------------------------------------------------------*
*&      Form  EVENT_DOUBLE_CLICK
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_0201   text
*      -->P_0202   text
*      -->P_0203   text
*---------------------------------------------------------------------*
FORM event_double_click  USING p_row
                               p_column
                               p_row_no.
  DATA: l_ucomm TYPE sy-ucomm.
  DATA: l_save_ucomm   TYPE sy-ucomm.
  DATA: lflg_refresh(1) TYPE c.
  DATA: lflg_exit(1) TYPE c.
  DATA: ls_stable TYPE lvc_s_stbl.

  l_ucomm = '&ETA'.
*  L_UCOMM =  GS_LAYOUT-F2CODE.
  CALL METHOD g_grid->set_function_code
    CHANGING c_ucomm = l_ucomm.


ENDFORM.                    " P1040_EVENT_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0225   text
*----------------------------------------------------------------------*
FORM event_hotspot_click  USING e_row_id e_column_id.

  READ TABLE gt_data INDEX e_row_id .
*  CASE E_COLUMN_ID .
*    WHEN 'LIFNR'.
*      SET PARAMETER ID 'LIF' FIELD GT_DATA-LIFNR.
*      SET PARAMETER ID 'EKO' FIELD GT_DATA-EKORG.
*      CALL TRANSACTION 'MK03' AND SKIP FIRST SCREEN.
*  ENDCASE.

ENDFORM.                    " P1060_EVENT_DATA_CHANGED_FINIS

*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ER_DATA_CHANGED  text
*      -->P_0213   text
*      -->P_0214   text
*      -->P_0215   text
*      -->P_0216   text
*----------------------------------------------------------------------*
FORM event_data_changed  USING    p_data_changed
                                        value(p_0213)
                                        value(p_0214)
                                        value(p_0215)
                                        value(p_0216).

ENDFORM.                    " P1050_EVENT_DATA_CHANGED

*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED_FINIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0225   text
*----------------------------------------------------------------------*
FORM event_data_changed_finis  USING    value(p_0225).

ENDFORM.                    " P1060_EVENT_DATA_CHANGED_FINIS

*&---------------------------------------------------------------------*
*&      Form  GET_FILEDCAT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM get_filedcat_alv  USING gt_fieldcat TYPE lvc_t_fcat.

  DATA : ls_fieldcat TYPE lvc_s_fcat.
  DATA : lt_fieldcat TYPE slis_t_fieldcat_alv,
         l_fieldcat  TYPE slis_fieldcat_alv.

  CLEAR : lt_fieldcat ,lt_fieldcat[].

  DATA: l_datum(08).

  sy-datum = sy-datum + 1.            "Victor 06.24.2011
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name         = gv_repid  "'ZRPP_HMA_ZPODER'
            i_internal_tabname     = 'GT_DATA'
            i_bypassing_buffer     = 'X'
            i_inclname             = gv_repid  "'ZRPP_HMA_ZPODER'
       CHANGING
            ct_fieldcat            = lt_fieldcat[]
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.

  CLEAR : gt_fieldcat , gt_fieldcat[].

  LOOP AT lt_fieldcat INTO l_fieldcat.
    CLEAR : ls_fieldcat.
    MOVE-CORRESPONDING l_fieldcat TO ls_fieldcat.
    ls_fieldcat-key       = space.


    CASE ls_fieldcat-fieldname.
      WHEN 'STATUS'.
        ls_fieldcat-seltext = 'Status'.
        ls_fieldcat-coltext = 'Status'.
        ls_fieldcat-icon = 'X'.
      WHEN 'SELECT'.
        ls_fieldcat-seltext = 'Select'.
        ls_fieldcat-coltext = 'Select'.
        ls_fieldcat-checkbox = 'X'.
        ls_fieldcat-edit = 'X'.
      WHEN 'DESC'.
        ls_fieldcat-seltext = 'Text'.
        ls_fieldcat-coltext = 'Text'.
      WHEN 'CHECK'.
        ls_fieldcat-no_out = 'X'.
      WHEN 'WO_SER'     .
        ls_fieldcat-coltext = text-f01.
        ls_fieldcat-seltext = text-f01.
        ls_fieldcat-emphasize = 'C500'.
        ls_fieldcat-fix_column = 'X'.
        ls_fieldcat-key = 'X'.
      WHEN 'NATION'     .
        ls_fieldcat-coltext = text-f02.
        ls_fieldcat-seltext = text-f02.
        ls_fieldcat-emphasize = 'C500'.
        ls_fieldcat-fix_column = 'X'.
        ls_fieldcat-key = 'X'.
      WHEN 'DEALER'     .
        ls_fieldcat-coltext = text-f03.
        ls_fieldcat-seltext = text-f03.
        ls_fieldcat-emphasize = 'C500'.
        ls_fieldcat-fix_column = 'X'.
        ls_fieldcat-key = 'X'.
      WHEN 'EXTC'       .
        ls_fieldcat-coltext = text-f04.
        ls_fieldcat-seltext = text-f04.
        ls_fieldcat-emphasize = 'C500'.
        ls_fieldcat-fix_column = 'X'.
        ls_fieldcat-key = 'X'.
      WHEN 'INTC'       .
        ls_fieldcat-coltext = text-f05.
        ls_fieldcat-seltext = text-f05.
        ls_fieldcat-emphasize = 'C500'.
        ls_fieldcat-fix_column = 'X'.
        ls_fieldcat-key = 'X'.

      WHEN 'MOYE'       .
        ls_fieldcat-coltext = text-f06.
        ls_fieldcat-seltext = text-f06.
      WHEN 'BMDL'       .
        ls_fieldcat-coltext = text-f07.
        ls_fieldcat-seltext = text-f07.
      WHEN 'OCNN'       .
        ls_fieldcat-coltext = text-f08.
        ls_fieldcat-seltext = text-f08.
      WHEN 'VERS'       .
        ls_fieldcat-coltext = text-f09.
        ls_fieldcat-seltext = text-f09.
      WHEN 'GRADE'       .
        ls_fieldcat-coltext = 'Grade'.
        ls_fieldcat-seltext = 'Grade'.
        ls_fieldcat-no_out = 'X'.
      WHEN '_IOQTY'.
        ls_fieldcat-coltext = text-f10.
        ls_fieldcat-seltext = text-f10.
        ls_fieldcat-col_pos = 14.
        ls_fieldcat-do_sum = 'X'.
        ls_fieldcat-quantity = 'EA'.
      WHEN '_MOQTY'     .
        ls_fieldcat-coltext = text-f11.
        ls_fieldcat-seltext = text-f11.
        ls_fieldcat-col_pos = 15.
        ls_fieldcat-do_sum = 'X'.
        ls_fieldcat-quantity = 'EA'.

      WHEN 'INITQTY'    .
        ls_fieldcat-coltext = text-f10.
        ls_fieldcat-seltext = text-f10.
        ls_fieldcat-datatype = 'INT'.
        ls_fieldcat-no_out = 'X'.
      WHEN 'MODQTY'     .
        ls_fieldcat-coltext = text-f11.
        ls_fieldcat-seltext = text-f11.
        ls_fieldcat-datatype = 'INT'.
        ls_fieldcat-no_out = 'X'.
      WHEN '_CRT_DATE'.
        ls_fieldcat-coltext = text-f12.
        ls_fieldcat-seltext = text-f12.
        ls_fieldcat-col_pos = 16.
*        ls_fieldcat-do_sum = ''.
      WHEN 'CRT_DATE'   .
        ls_fieldcat-no_out = 'X'.
        ls_fieldcat-datatype = 'DATUM'.
      WHEN '_CHG_DATE'   .
        ls_fieldcat-coltext = text-f13.
        ls_fieldcat-seltext = text-f13.
        ls_fieldcat-col_pos = 17.
*        ls_fieldcat-do_sum = ''.
      WHEN 'CHG_DATE'   .
        ls_fieldcat-no_out = 'X'.
        ls_fieldcat-coltext = text-f13.
        ls_fieldcat-seltext = text-f13.
        ls_fieldcat-datatype = 'DATUM'.
      WHEN 'DEST'       .
        ls_fieldcat-coltext = text-f14.
        ls_fieldcat-seltext = text-f14.
      WHEN 'CLSR'       .
        ls_fieldcat-coltext = text-f15.
        ls_fieldcat-seltext = text-f15.
      WHEN 'FLET'       .
        ls_fieldcat-coltext = text-f16.
        ls_fieldcat-seltext = text-f16.
      WHEN 'LCNT'       .
        ls_fieldcat-coltext = text-f17.
        ls_fieldcat-seltext = text-f17.
      WHEN 'LCNO'       .
        ls_fieldcat-coltext = text-f18.
        ls_fieldcat-seltext = text-f18.
      WHEN 'S219'       .
        ls_fieldcat-coltext = text-f19.
        ls_fieldcat-seltext = text-f19.

    ENDCASE.

    ls_fieldcat-reptext   = l_fieldcat-seltext_s.
    ls_fieldcat-ref_table = l_fieldcat-ref_tabname.


    APPEND ls_fieldcat TO gt_fieldcat.

  ENDLOOP.

ENDFORM.                    " GET_FILEDCAT_ALV
*&---------------------------------------------------------------------*
*&      Form  P1200_CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_POS[]  text
*      -->P_G_GRID  text
*----------------------------------------------------------------------*
FORM p1200_call_grid_display  TABLES   p_table
                          USING    p_grid TYPE REF TO cl_gui_alv_grid  .

  CALL METHOD p_grid->set_table_for_first_display
    EXPORTING
      i_save               = 'A'
      i_default            = 'X'
      is_layout            = gs_layout
      i_structure_name     = 'GT_DATA'
      is_variant           = gs_o_layout
      it_toolbar_excluding = gt_excl_func
    CHANGING
      it_fieldcatalog      = gt_fieldcat
      it_sort              = gt_sort
      it_outtab            = p_table[].

ENDFORM.                    " P1200_CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY_OLD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_DATA[]  text
*----------------------------------------------------------------------*
FORM call_grid_display_old .
  DATA: l_struct    LIKE dd02l-tabname.

  l_struct = 'GT_DATA'.
*-----> SET OBJECT
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      i_structure_name              = l_struct
      is_variant                    = gs_o_layout
      i_save                        = 'A'
      it_toolbar_excluding          = gt_excl_func
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = gt_data[]
      it_fieldcatalog               = gt_fieldcat[]
      it_sort                       = gt_sort[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4 .

ENDFORM.                    " CALL_GRID_DISPLAY_OLD

*---------------------------------------------------------------------*
*       FORM REFRESH                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM refresh.
  DATA:
    l_reportid LIKE rsvar-report,
    l_texpr    TYPE rsds_texpr,
    l_trange   TYPE rsds_trange.

  DATA BEGIN OF l_sel_tab OCCURS 1.
          INCLUDE STRUCTURE rsparams.
  DATA END OF l_sel_tab.

  l_reportid = sy-repid.
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
       EXPORTING
            curr_report     = l_reportid
       TABLES
            selection_table = l_sel_tab.

  CALL FUNCTION 'RS_REFRESH_FROM_DYNAMICAL_SEL'
       EXPORTING
            curr_report        = l_reportid
            mode_write_or_move = 'M'
       IMPORTING
            p_trange           = l_trange
       EXCEPTIONS
            not_found          = 1.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_EX'
         EXPORTING
              field_ranges = l_trange
         IMPORTING
              expressions  = l_texpr.
    IF p_submit IS INITIAL.
      LOOP AT l_sel_tab WHERE selname = 'P_SUBMIT'.
        l_sel_tab-low = 'X'.
        MODIFY l_sel_tab.
      ENDLOOP.
      SUBMIT (l_reportid) WITH SELECTION-TABLE l_sel_tab
                        WITH FREE SELECTIONS l_texpr AND RETURN.
      LEAVE LIST-PROCESSING.

    ELSE.
      SUBMIT (l_reportid) WITH SELECTION-TABLE l_sel_tab
                          WITH FREE SELECTIONS l_texpr.
    ENDIF.
  ELSE.
    IF p_submit IS INITIAL.
      LOOP AT l_sel_tab WHERE selname = 'P_SUBMIT'.
        l_sel_tab-low = 'X'.
        MODIFY l_sel_tab.
      ENDLOOP.
      SUBMIT (l_reportid) WITH SELECTION-TABLE l_sel_tab AND RETURN.
      LEAVE LIST-PROCESSING.
    ELSE.
      SUBMIT (l_reportid) WITH SELECTION-TABLE l_sel_tab.
    ENDIF.
  ENDIF.
ENDFORM.                               " REFRESH
