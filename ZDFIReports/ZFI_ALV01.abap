*&---------------------------------------------------------------------*
*&  Include           ZFI_ALV01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Program Name        : ZFI_ALV
*& Program Ttitle      : ALV include
*& Created by          :
*& Created on          : 2011.08.24
*& Description         :
*&---------------------------------------------------------------------*
* Change History
*----------------------------------------------------------------------*
* Mod. # |Date       |Developer |Description(Reason)
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Include           ZFI_ALV                                          *
*&---------------------------------------------------------------------*
*  for ALV Layout   Include File.

type-pools: slis.

constants : c_status        type slis_formname value 'STATUS',
            c_status1       type slis_formname value 'STATUS1',
            c_user_command  type slis_formname value 'USER_COMMAND',
            c_user_command1 type slis_formname value 'USER_COMMAND1',
            c_top_of_page   type slis_formname value 'TOP-OF-PAGE',
            c_top_of_page1  type slis_formname value 'TOP-OF-PAGE1',
            c_data_changed  type slis_formname value 'DATA_CHANGED',

            c_c_container1(30)  value 'G_C_CONTAINER1',
            c_c_container2(30)  value 'G_C_CONTAINER2',

            c_sp_group_required type  lvc_s_fcat-sp_group value 'MAND'.

data : gs_fieldcat         type slis_fieldcat_alv,
       gt_fieldcat         type slis_t_fieldcat_alv,
       gs_layout           type slis_layout_alv,
       gs_keyinfo          type slis_keyinfo_alv,
       gt_sp_group         type slis_t_sp_group_alv,
       gt_events           type slis_t_event,
       gs_events           type slis_alv_event,
       gt_event_exit       type slis_t_event_exit,
       gs_event_exit       type slis_event_exit,
       gs_sort             type slis_sortinfo_alv,
       gt_sort             type slis_t_sortinfo_alv,
       gt_top_of_page      type slis_t_listheader,
       gs_top_of_page      type slis_listheader,
       gs_print            type slis_print_alv,
       gt_excluding        type slis_t_extab,
       g_grid_title        type lvc_title.
*
data : gs_layout_lvc       type lvc_s_layo,
       gs_gsetting         type lvc_s_glay,
       gs_print_lvc        type lvc_s_prnt,
       gs_fieldcat_lvc     type lvc_s_fcat,
       gt_fieldcat_lvc     type lvc_t_fcat,
       gs_sort_lvc         type lvc_s_sort,
       gt_sort_lvc         type lvc_t_sort,
       gs_modi_cell        type lvc_s_modi,
       gt_modi_cell        type lvc_t_modi,
       gs_tb_exclude       type ui_func,
       gt_tb_exclude       type ui_functions,
       gs_dropdown         type lvc_s_drop,
       gt_dropdown         type lvc_t_drop,
       gs_dropdowna        type lvc_s_dral,
       gt_dropdowna        type lvc_t_dral,
       gt_scoltab          type lvc_t_scol,
       gs_scoltab          type lvc_s_scol,
       gt_celltab          type lvc_t_styl,
       gs_celltab          type lvc_s_styl,
       gt_sels             type lvc_t_row,
       gs_sels             type lvc_s_row,
       gs_f4               type lvc_s_f4,
       gt_f4               type lvc_t_f4,
       gt_qinfo_lvc        type lvc_t_qinf,
       gs_stable           type lvc_s_stbl,
       gs_toolbar          type stb_button,
       gs_row              type lvc_s_row,
       gs_col              type lvc_s_col,
       gs_row_no           type lvc_s_roid.

data : gs_variant          like disvariant,
       g_syrepid           like sy-repid,
       g_cposition         type i,
       g_sposition         type i,
       g_return            type i.

*CLASS lcl_event_receiver_grid1 DEFINITION DEFERRED.
*DATA : event_receiver_grid1 TYPE REF TO lcl_event_receiver_grid1.

data : g_grid1              type ref to cl_gui_alv_grid,
       g_grid2              type ref to cl_gui_alv_grid,
       g_sender             type ref to cl_gui_alv_grid,

       g_docking1           type ref to cl_gui_docking_container,
       g_docking2           type ref to cl_gui_docking_container,

       g_document1          type ref to cl_dd_document.

data: g_c_container1   type ref to cl_gui_custom_container,
      g_c_container2   type ref to cl_gui_custom_container.


data: g_easy_splitter1  type ref to cl_gui_easy_splitter_container,
      g_easy_splitter2  type ref to cl_gui_easy_splitter_container,
      g_splitter1       type ref to cl_gui_splitter_container,
      g_splitter2       type ref to cl_gui_splitter_container,

      g_container1      type ref to cl_gui_container,
      g_container2      type ref to cl_gui_container,
      g_container3      type ref to cl_gui_container,
      g_container4      type ref to cl_gui_container.


*..F4 Possible Entry
field-symbols : <f4tab> type lvc_t_modi.
data : ls_f4     type ddshretval.

data : lt_values type table of seahlpres,
       lt_fields type table of dfies,
       ls_value  type seahlpres,
       ls_field  type dfies.

* for ALV Grid
DATA : GT_EXCLUDE   TYPE UI_FUNCTIONS.

*===========================================================*
CONSTANTS: GC_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME
                                   VALUE 'TOP_OF_PAGE',
           GC_VAR_SAVE       TYPE C VALUE  'A',
           GC_PF_STATUS_SET  TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
           GC_USER_COMMAND   TYPE SLIS_FORMNAME VALUE 'USER_COMMAND',
           GC_TVERS          TYPE CK_TVERS      VALUE '01'.

DATA: GT_LIST_TOP_OF_PAGE  TYPE SLIS_T_LISTHEADER,
      GT_LIST_TOP_OF_PAGE1 TYPE SLIS_T_LISTHEADER,
      GT_SPECIALCOL        TYPE SLIS_T_SPECIALCOL_ALV,
      GS_SPECIALCOL        TYPE SLIS_SPECIALCOL_ALV.

DATA: GV_DEFAULT(1)  TYPE C,
      GS_VARIANT1 LIKE DISVARIANT,
      GV_REPID    LIKE SY-REPID.

* for ALV Grid
DATA : GT_EXCLUDE1  TYPE UI_FUNCTIONS,
       CONTAINER    TYPE SCRFNAME VALUE 'G_CUSTOM_CONTAINER',
       CONTAINER1   TYPE SCRFNAME VALUE 'G_CUSTOM_CONTAINER1',
       GS_FCAT      TYPE LVC_S_FCAT,
       GT_FCAT      TYPE LVC_T_FCAT,
       GS_LAYO      TYPE LVC_S_LAYO,
       GS_FCAT1     TYPE LVC_S_FCAT,
       GT_FCAT1     TYPE LVC_T_FCAT,
       GS_LAYO1     TYPE LVC_S_LAYO,
       GS_SORT_ALV  TYPE SLIS_SORTINFO_ALV,
       GT_SORT_ALV  TYPE SLIS_T_SORTINFO_ALV.

DATA : OK_CODE      TYPE SY-UCOMM,
       SAVE_OK_CODE TYPE SY-UCOMM.

* Define internal tables &sstructures for Possible Entry
DATA : GS_VALUES TYPE SEAHLPRES,
       GT_FIELDS TYPE TABLE OF DFIES WITH HEADER LINE,
       GT_VALUES TYPE TABLE OF SEAHLPRES WITH HEADER LINE,
       GS_FIELDS TYPE DFIES,
       LS_MODI   TYPE LVC_S_MODI.

* reference to custom container: neccessary to bind ALV Control
CLASS CL_GUI_RESOURCES DEFINITION LOAD.
*CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA : G_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_GRID              TYPE REF TO CL_GUI_ALV_GRID,
       G_CUSTOM_CONTAINER1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: GT_ROW     TYPE LVC_T_ROW,
      GT_ROID    TYPE LVC_T_ROID.

* define internal table for BDC
DATA: GT_BDC TYPE TABLE OF BDCDATA    WITH HEADER LINE,
      GT_MSG TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      GS_OPT LIKE CTU_PARAMS.

* for possible entry
DATA: BEGIN OF DYNPFIELDS OCCURS 3.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS.

DATA: DYNAME         TYPE PROGNAME,
      DYNUMB         TYPE SYCHAR04,
      EXC_EXCTAB     TYPE SLIS_T_EXTAB,
      POPUP_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      F              TYPE SLIS_FIELDCAT_ALV,
      SELFIELD       TYPE SLIS_SELFIELD,
      EXITFIELD,
      COLOR_ACTIVE(3)  VALUE 'C50',
      TABIX LIKE SY-TABIX.

* possible entry for reason code
TYPES: BEGIN OF TY_ZTCOUM02,
         RGRP2 TYPE ZRGRP2,
         TEXT  TYPE ZRTEXT,
       END OF TY_ZTCOUM02.

TYPES: BEGIN OF TY_RSN,
         KZUST TYPE KZUST,
         TEXT  TYPE ZRTEXT,
       END OF TY_RSN.

DATA: GT_ZTCOUM02 TYPE TABLE OF TY_ZTCOUM02 WITH HEADER LINE,
      GT_RSN      TYPE TABLE OF TY_RSN      WITH HEADER LINE.

DATA: STABLE        TYPE LVC_S_STBL.

DEFINE __SET_REFRESH_MODE.
  STABLE-ROW = &1.
  STABLE-COL = &1.
END-OF-DEFINITION.


*&---------------------------------------------------------------------*
*&      Form  LVC_FIELDCATALOG_MERGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form lvc_fieldcatalog_merge  using    p_table
                             changing pt_fieldcat_lvc type lvc_t_fcat.

  data : l_table  like  dd02l-tabname.

  l_table = p_table.
  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
*     I_BUFFER_ACTIVE              =
      i_structure_name             = l_table
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_BYPASSING_BUFFER           =
*     I_INTERNAL_TABNAME           =
    changing
      ct_fieldcat                  = pt_fieldcat_lvc
    exceptions
      inconsistent_interface       = 1
      program_error                = 2
      others                       = 3.

  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                    " LVC_FIELDCATALOG_MERGE
*&---------------------------------------------------------------------*
*&      Form  create_field_cat_lvc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_field_cat_lvc using cposition
                                fieldname
                                reptext
                                outputlen
                                datatype.
  clear gs_fieldcat_lvc.
  if cposition  is initial.
    g_cposition = g_cposition + 1.
    gs_fieldcat_lvc-col_pos = g_cposition.
  else.
    gs_fieldcat_lvc-col_pos = cposition.
  endif.
  gs_fieldcat_lvc-fieldname = fieldname.
  gs_fieldcat_lvc-reptext   = reptext.
  gs_fieldcat_lvc-datatype  = datatype.
  gs_fieldcat_lvc-outputlen = outputlen.

  append gs_fieldcat_lvc  to gt_fieldcat_lvc.
endform.                    "create_field_cat_lvc
*&---------------------------------------------------------------------*
*&      Form  create_sort_lvc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_sort_lvc  using sposition
                            fieldname
                            down
                            subtot
                            level.
  clear gs_sort_lvc.

  if sposition  is initial.
    add 1 to g_sposition.
    gs_sort_lvc-spos  = g_sposition.
  else.
    gs_sort_lvc-spos  = sposition.
  endif.

  gs_sort_lvc-fieldname = fieldname.

  if down is initial.
    gs_sort_lvc-up  = 'X'.
  else.
    gs_sort_lvc-down  = 'X'.
  endif.

  gs_sort_lvc-subtot  = subtot.
  gs_sort_lvc-level   = level.

  append gs_sort_lvc  to gt_sort_lvc.

endform.                    "create_sort_lvc
*&---------------------------------------------------------------------*
*&      Form  exclude_tb_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form exclude_tb_functions  using   pt_exclude type ui_functions.

  clear pt_exclude[].
  clear gs_tb_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_check.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_auf.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_average.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_back_classic.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_abc.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_chain.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_crbatch.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_crweb.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_call_line_ITEMs.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_master_data.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_more.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_call_report.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_xint.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_call_xxl.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_col_invisible.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_col_optimize.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_current_variant.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_data_save.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_delete_filter.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_deselect_all.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_detail.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
*  -------------------------"[??? ?? ?? ?? ?? ???]
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_excl_all.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
*------------------------------------------------------
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_expcrdata.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_expcrdesig.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_expcrtempl.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_expmdb.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_extend.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_f4.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_filter.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_find.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_fix_columns.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_graph.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_help.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_info.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_load_variant.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_loc_copy.
* APPEND GS_TB_EXCLUDE TO pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_html.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_maintain_variant.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_maximum.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_minimum.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_print.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_print_back.
*  APPEND GS_TB_EXCLUDE TO pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_print_prev.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_reprep.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_save_variant.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_select_all.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_send.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_separator.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_sort.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_sort_asc.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_sort_dsc.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_subtot.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_sum.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_to_office.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_to_rep_tree.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_unfix_columns.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_views.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_view_crystal.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_view_excel.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_view_grid.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_word_processor.
  append gs_tb_exclude to pt_exclude.

endform.                    " exclude_tb_functions
*&---------------------------------------------------------------------*
*&      Form  refresh_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_GRID    text
*      -->P_REFRESH  text
*----------------------------------------------------------------------*
form refresh_grid using pr_grid type ref to cl_gui_alv_grid
                        p_refresh .

  clear : gs_stable.
  gs_stable-row = p_refresh.
  gs_stable-col = p_refresh.

  call method pr_grid->refresh_table_display
    exporting
      is_stable      = gs_stable
      i_soft_refresh = 'X'.

endform.                    "refresh_grid
*&---------------------------------------------------------------------*
*&      Form  SET_CURRENT_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_GRID1  text
*      -->P_L_ROW  text
*----------------------------------------------------------------------*
form set_current_cell  using    pr_grid type ref to cl_gui_alv_grid
                                p_row type i.
  check pr_grid is bound.

  data: ls_row_id type  lvc_s_row,
        ls_col_id type  lvc_s_col,
        ls_row_no type  lvc_s_roid.

  call method pr_grid->get_current_cell
    importing
      es_row_id = ls_row_id
      es_col_id = ls_col_id
      es_row_no = ls_row_no.

  refresh gt_fieldcat_lvc.
  call method pr_grid->get_frontend_fieldcatalog
    importing
      et_fieldcatalog = gt_fieldcat_lvc.

  sort gt_fieldcat_lvc  by col_pos.
  read table gt_fieldcat_lvc  with key  no_out  = ''  into gs_fieldcat_lvc.

  ls_row_id-index     = p_row.
  ls_col_id-fieldname = gs_fieldcat_lvc-fieldname.
  ls_row_no-row_id    = p_row.

  call method pr_grid->set_current_cell_via_id
    exporting
      is_row_id    = ls_row_id
      is_column_id = ls_col_id
      is_row_no    = ls_row_no.

endform.                    " SET_CURRENT_CELL
*&---------------------------------------------------------------------*
*&      Form  GET_SELECT_ROW_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_GRID1  text
*----------------------------------------------------------------------*
form get_select_row_grid  using pr_grid  type ref to cl_gui_alv_grid.

  clear : gt_sels[], gs_sels.

  check pr_grid is bound.

  call method pr_grid->get_selected_rows
    importing
      et_index_rows = gt_sels.

endform.                    " GET_SELECT_ROW_GRID
*&---------------------------------------------------------------------*
*&      Form  GET_FIELD_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_field_text  using    ps_fieldcat_lvc structure lvc_s_fcat
                     changing p_text.

  data: l_fldnm   type dd03l-tabname.
  field-symbols <lfs>.

  clear p_text.

  if ps_fieldcat_lvc-ref_table is initial or ps_fieldcat_lvc-ref_field  is initial.
    l_fldnm = 'COLTEXT'.

  else.
    case ps_fieldcat_lvc-colddictxt.
      when  ''  or  'R'.
        l_fldnm = 'REPTEXT'.
      when  'S' or  'M' or 'L'.
        concatenate 'SCRTEXT_'  ps_fieldcat_lvc-colddictxt  into l_fldnm.
      when others.
        l_fldnm = 'REPTEXT'.
    endcase.
  endif.

  assign component l_fldnm  of structure ps_fieldcat_lvc  to <lfs>.
  if <lfs> is initial.
    l_fldnm = 'REPTEXT'.
    assign component l_fldnm  of structure ps_fieldcat_lvc  to <lfs>.
  endif.

  p_text  = <lfs>.

  unassign <lfs>.
endform.                    " GET_FIELD_TEXT
