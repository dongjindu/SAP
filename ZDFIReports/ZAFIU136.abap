************************************************************************
* Program Name      : ZAFIU136
* Author            : IG.Moon
* Creation Date     : 9/15/2009
* Specifications By : Michael Yoon
* Description       : Maintain the AR master for Investment
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
report zafiu136 message-id zmco.
include zacoui00.
include <icon>.                        " icon
*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
* Tables
tables :                                                    "ztfiu131,
         ztim001,ztim002,ztim003,sscrfields,imak,zsaru131, ztfiu131,
         bpja,imavz,imav,ania.

types: begin of ty_row_tab.
        include structure zsaru131.
*TYPES: icon TYPE icon_d,
*       icon2 TYPE icon_d,
*       status(5),
*       status_desc(30),
*       msg(50),
*       chk(1),
*       flag1,
*       flag2.
types: end of ty_row_tab.
types: begin of ty_out.
include  type ty_row_tab.
types   celltab  type lvc_t_styl.
types   tabcolor type slis_t_specialcol_alv.
types: end of ty_out.
data  gv_chk.

* for BAPI ///////////////////////////////////////////////////////////
data : wa_master        like   bapiappreqmaster,
       wa_master_x      like   bapiappreqmasterx,
       wa_user_fields   like   bapiapprequser,
       wa_user_fields_x like   bapiapprequserx,
       wa_variant       like   bapiappreqvarnt,
       wa_plan_total    like   bapiappreqplantotal,
       wa_externalnumber like  bapi_appreq_id-appreq ,
       wa_appropriat  like bapi_appreq_id-appreqvrnt.

data :
 it_org_units      like bapiappreqorgunit occurs 0 with header line,
 it_division       like bapiappreqdivision occurs 0 with header line,
 it_invest_reason  like bapiappreqinvreason occurs 0 with header line,
 it_partner        like  bapiappreqpartner occurs 0 with header line,
 it_variant        like bapiappreqvarntassign occurs 0 with header line,
 it_plan_year      like bapiappreqplanyear occurs 0 with header line,
 it_material_grp   like bapiappreqmatgroup  occurs 0 with header line,
 it_version        like bapiappreqvarntassign occurs 0 with header line,
* it_assets_equis   LIKE bapiappreqasset OCCURS 0 WITH HEADER LINE,

 i_msg             like bapiret2 occurs 0 with header line,
 g_msg            like bapiret2 occurs 0 with header line,

 it_org_units_x like bapiappreqchangex occurs 0 with header line,
 it_division_x like bapiappreqchangex occurs 0 with header line,
* it_material_grp_x LIKE bapiappreqchangex OCCURS 0 WITH HEADER LINE,
 it_invest_reason_x like bapiappreqchangex occurs 0 with header line,
 it_partner_x like bapiappreqpartnerx occurs 0 with header line.

* ////////////////////////////////////////////////////////////////////

data: g_error(1),g_import,
      g_repid  like sy-repid,
      g_ix     like sy-tabix,
      gv_index like sy-tabix.
data  : it_row_tab type table of ty_row_tab with header line,
        gt_out     type table of ty_out     with header line.
data  $gt_out like gt_out occurs 0 with header line.

data  gt_cbowbs  like ztfiu131 occurs 0 with header line.
data  gt_prjcode like ztim008  occurs 0 with header line.

data: it_status like bapiappreqstatus occurs 0 with header line.
data: it_user_status like bapiappreqstatus occurs 0 with header line.
data: it_varnt_status like bapiappreqstatusvarnt occurs 0
        with header line.
data: it_varnt_user_status like bapiappreqstatusvarnt occurs 0 with
        header line.

data $objnr type onr00.
data: g_kokrs like tka02-kokrs.
data:
    lt_ania  like rania occurs 0 with header line,
    lt_anib  like standard table of ranib
                  initial size 10 with header line,
    lt_anib1 like standard table of ranib
                  initial size 10 with header line.

data: i_imakpa like imakpa occurs 0 with header line.

data: g_row_id type lvc_s_modi-row_id,
      g_row_fieldname type lvc_s_modi-fieldname.

define __process.
  perform show_progress using &1 &2.
end-of-definition.
define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.
define __focus.
  call method cl_gui_control=>set_focus
    exporting
      control = &1.
end-of-definition.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
class lcl_event_receiver definition.

  public section.

    types: begin of ztfiu131_k,
              posnr   type ima_posnr,
              versi   type coversi,
           end of ztfiu131_k.

    types: ztfiu131_key   type standard table of ztfiu131_k,
           ztfiu131_table type standard table of ztfiu131.

    methods:
      handle_data_changed
         for event data_changed of cl_gui_alv_grid
             importing er_data_changed,
                       get_deleted_rows
             exporting
                       deleted_rows type ztfiu131_table,

      refresh_delta_tables.

    methods:
    handle_double_click for event double_click of cl_gui_alv_grid
            importing e_row
                      e_column
                      es_row_no.

  private section.
    data deleted_rows type standard table of ztfiu131.

* This flag is set if any error occured in one of the
* following methods:
    data: error_in_data type c.
    methods:
      update_delta_tables
         importing
            pr_data_changed type ref to cl_alv_changed_data_protocol.


endclass.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

* Setting for Change data
  method handle_data_changed.

* remember deleted lines for saving
*    CALL METHOD update_delta_tables( er_data_changed ).

    perform data_changed using er_data_changed.
  endmethod.                    " handle_data_changed

  method get_deleted_rows.
    deleted_rows = me->deleted_rows.
  endmethod.                    "get_deleted_rows

  method refresh_delta_tables.
    clear me->deleted_rows[].
  endmethod.                    "refresh_delta_tables

  method update_delta_tables.
    data: l_del_row type lvc_s_moce,
          ls_ztfiu131 type ztfiu131,
          ls_outtab like line of gt_out.

    loop at pr_data_changed->mt_deleted_rows into l_del_row.
      read table gt_out into ls_outtab index l_del_row-row_id.
      if sy-subrc ne 0.
        message i000(0k) with text-e01. "Internal error
      else.
        move-corresponding ls_outtab to ls_ztfiu131.
        append ls_ztfiu131 to deleted_rows.
      endif.
    endloop.
  endmethod.                    "update_delta_tables

  method handle_double_click.
    perform double_click using e_row
                               e_column
                               es_row_no.
  endmethod.                    " handle_double_click

endclass.                   " LCL_EVENT_RECEIVER Implementation

data g_event_receiver  type ref to lcl_event_receiver.

data begin of field_tab occurs 0.
        include structure sval.
data end of field_tab.

************************************************************************
data  : flag_data_changed,
        info(80).
data: begin of ftab occurs 10,
        fcode(6),
      end of ftab.
****************************** constants *******************************
constants:  false value ' ',
            true  value 'X'.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
parameters: p_bukrs like bkpf-bukrs memory id buk obligatory,
            p_gjahr like imak-gjahr default sy-datum(4) obligatory,
            p_mvers like imav-mvers,
            p_abp   type im_stratflg.

selection-screen begin of block bl with frame title text-001.

select-options: s_posnr  for imak-posnr,
                s_ivart  for imak-ivart,
                s_werks  for imak-werks,
                s_vkostl for imak-vkostl.

parameters   p_wovr as checkbox.
selection-screen end of block bl.

selection-screen begin of block bl2 with frame title text-001.

parameters p_test no-display.

selection-screen end of block bl2.

* Layout
selection-screen begin of block b4 with frame title text-01s.
parameter p_vari type slis_vari.
selection-screen end of block b4.

parameters  p_call no-display.
parameters  p_sync  as checkbox.
parameters  p_oldyr as checkbox.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.

*  sy-title =   '[IM] Register AR master for Investment '.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.
  perform get_default.
  perform get_data.

  perform get_data_att.

  check g_error eq false.

  perform move_out.
  perform set_output .

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.


*&---------------------------------------------------------------------*
*&      Form  data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_data_changed  text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
form data_changed using rr_data_changed
                        type ref to cl_alv_changed_data_protocol.

  flag_data_changed = true.

  data: ls_mod_cells type lvc_s_modi,
        ls_cells     type lvc_s_modi,
        lt_values type table of bapi_char_values with header line.

  loop at rr_data_changed->mt_good_cells into ls_mod_cells.
    read table gt_out index ls_mod_cells-row_id.
    if sy-subrc = 0.
      call method rr_data_changed->modify_cell
        exporting
          i_row_id    = ls_mod_cells-row_id
          i_fieldname = ls_mod_cells-fieldname
          i_value     = ls_mod_cells-value.
    endif.

* 06/28/2013 - T00306 Start
    if not ls_mod_cells-row_id is initial.
      g_row_id = ls_mod_cells-row_id.
      g_row_fieldname = ls_mod_cells-fieldname.
    endif.
* 06/28/2013 - T00306 End
  endloop.

  __set_refresh_mode true.
  call method g_grid->refresh_table_display
    exporting
      is_stable = stable.

endform.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  move_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_out.

  __process 'Preparing output...' '95'.

  __cls gt_out.

  loop at it_row_tab.
    clear gt_out.
    move-corresponding it_row_tab to gt_out.

    append gt_out.
  endloop.
  perform get_status.

  perform apply_icon.

endform.                    " move_out
*&---------------------------------------------------------------------*
*&      Form  set_output
*&---------------------------------------------------------------------*
form set_output.

  check : g_error is initial.
  clear flag_data_changed.
  call screen 100.

endform.                    " set_output

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
form show_progress using    pf_text
                            value(pf_val).
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = pf_val
      text       = pf_text.

endform.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
module status_0100 output.
*  SET TITLEBAR '100'.
*   Exclude toolbar
  perform exclude_functions.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  exclude_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exclude_functions.

  perform append_exclude_functions
           tables gt_exclude[]
           using: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_delete_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
*                  cl_gui_alv_grid=>mc_fc_loc_copy,
*                  cl_gui_alv_grid=>mc_fc_loc_paste.

endform.                    " exclude_functions
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module display_alv_100 output.

  data: ls_row  type lvc_s_row,
        ls_col  type lvc_s_col.

  if g_custom_container is initial.
    perform create_and_init_alv.
*   Display alv grid
    call method g_grid->set_table_for_first_display
      exporting
        is_layout            = gs_layo
        it_toolbar_excluding = gt_exclude
        i_save               = gc_var_save
        is_variant           = gs_variant
      changing
        it_outtab            = gt_out[]
        it_fieldcatalog      = gt_fcat[]
        it_sort              = gt_sort[].
  else.

    if not g_row_id is initial.
      ls_row-index = g_row_id.
      ls_col-fieldname = g_row_fieldname.
*      message i000 with ls_row-index ls_col-fieldname.
      call method g_grid->set_focus
        exporting
          control = g_grid.
      call method g_grid->set_current_cell_via_id
        exporting
          is_row_id    = ls_row
          is_column_id = ls_col.
    endif.
*    call method g_grid->refresh_table_display.

    data : ls_stabl type lvc_s_stbl.

    ls_stabl-row = 'X'.
    ls_stabl-col = 'X'.
    g_grid->refresh_table_display( exporting
                                        is_stable      = ls_stabl
                                        i_soft_refresh = ' ' ).

  endif.
*  __focus g_grid.
  perform user_status.


endmodule.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_and_init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_and_init_alv.

*   Create object
  perform create_object.

*  Create Object to verify input values.
  create object g_event_receiver.
  set handler : g_event_receiver->handle_data_changed for g_grid,
                g_event_receiver->handle_double_click for g_grid.

*   Create field category
  perform create_field_category using false.

  call method g_grid->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  call method g_grid->set_ready_for_input
    exporting
      i_ready_for_input = 0.

*  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  perform set_lvc_layout.

*   Set colors
  perform set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

*   Define cell attribute
  perform build_cell_attr.



endform.                    " create_and_init_alv
*&---------------------------------------------------------------------*
*&      Form  create_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
form create_field_category using mode_edit.

  data: l_pos       type i.

  define __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    gs_fcat-no_zero = 'X'.
    append gs_fcat to gt_fcat.
  end-of-definition.

  __catalog :
      'X' 'VERSI'      'Version'                       3 'CHAR' '',
      'X' 'POSNR'      'App. req.'                    12 'CHAR' '',
      'X' 'TXT50'      'Description'                  50 'CHAR' '',
      ' ' 'ICON'       'St'                            4 'ICON' '',
      ' ' 'STATUS_DESC' 'Status'                      20 'CHAR' '',
      ' ' 'PRVAMT'    'PrvYr Pln'                    15 'CURR' '',
      ' ' 'CURAMT'    'ThisYr '                      15 'CURR' '',
      ' ' 'NXTAMT1'    'Y+1 Plan'                     15 'CURR' '',
      ' ' 'NXTAMT2'    'Y+2 Plan'                     15 'CURR' '',
      ' ' 'NXTAMT3'    'Y+3 Plan'                     15 'CURR' '',
      ' ' 'NXTAMT4'    'Y+4 Plan'                     15 'CURR' '',
      ' ' 'MONAMT'    'Month Tot'                    15 'CURR' '',
      ' ' 'VARNT'      'Variant'                       4 'CHAR' '',
      ' ' 'IVART'      'RqTyp'                         2 'CHAR' '',
*      ' ' 'WAERS'	  'Curr'                          5 'CUKY' '',
*      ' ' 'IZWEK'      'Inv.reason'                   2 'CHAR' '',
*      ' ' 'SIZECL'    'Scale'                         2 'CHAR' '',
*      ' ' 'WDATU'	  'Impl.start'                    8 'DATS' '',
*      ' ' 'USR09'      'End.Date'                      8 'DATS' '',
*      ' ' 'PRIORI'    'Priority'                      1 'CHAR' '',
*      ' ' 'AKOSTL'    'Req. CCtr'                    10  'CHAR' '',
      ' ' 'VKOSTL'    'Res. CCtr'                    10  'CHAR' '',
*      ' ' 'GDATU'	  'Pland. approval'               8  'DATS' '',
*      ' ' 'PARNR_VERA' 'Applicant'                    12 'CHAR' '',
*      ' ' 'USR03'      'Asset Class'                  10 'CHAR' '',
*      ' ' 'ANLKL'      'Asset class'                   8 'CHAR' '',
*      ' ' 'AKTIV'      'Capitaliz. date'              50 'CHAR' '',
*      ' ' 'WERKS'	  'Plant'                         4 'CHAR' '',
      ' ' 'MSG'        'remarks'                      50 'CHAR' '',
*      ' ' 'AUART'	  'Order type'                    4  'CHAR' '',
*      ' ' 'PROFIDPROJ'  'Project profile'             7  'CHAR' '',
*      ' ' 'PSPNR'	  'Proj. seq. no.'                8  'NUMC' '',
      ' ' 'GJAHR'	  'Appr.year'                     4 'NUMC' ''.
*      ' ' 'PRART'	  'Program type'                  4 'CHAR' '',
*      ' ' 'SCOPE'	  'Object class'                  2 'CHAR' '',
*      ' ' 'VBUKRS'    'Comp.code'                     4 'CHAR' '',
*      ' ' 'VKOKRS'    'COAr'                          4 'CHAR' '',
*      ' ' 'ABUKRS'    'Req.CoCd'                      4 'CHAR' '',
*      ' ' 'ERNAM'	  'Created by'                   12 'CHAR' '',
*      ' ' 'ERDAT'	  'Created on'                    8 'DATS' '',
*      ' ' 'AENAM'	  'Changed by'                   12 'CHAR' '',
*      ' ' 'AEDAT'	  'Changed on'                    8 'DATS' ''.


  loop at gt_fcat into gs_fcat.
    gs_fcat-ref_table = 'ZSARU131'.
    gs_fcat-ref_field = gs_fieldcat-fieldname.
    modify gt_fcat from gs_fcat.
  endloop.

endform.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
form sort_build using ft_sort type lvc_t_sort.
  define sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  end-of-definition.

  sort_tab :
      'POSNR'             ' ' 'X' '' 'X' '',
      'TXT50'             ' ' 'X' '' 'X' '',
      'VERSI'             ' ' 'X' '' 'X' '',
      'IVART'             ' ' 'X' '' 'X' ''.

endform.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  set_lvc_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_lvc_layout.

  clear gs_layo.
  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.
endform.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  set_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_color.
  data : $ix(3) type n,
         $mtxt(6).

  clear: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  define __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  end-of-definition.

  __color :
          'POSNR'  '1' 0,
          'TXT50'  '1' 0,
          'VERSI'  '1' 0,
          'VARNT'  '1' 0,
          'IVART'  '3' 0,
          'WAERS'  '3' 0,
          'AMOUNT' '3' 0,
          'IZWEK'  '3' 0,
          'ERNAM'  '3' 0,
          'ERDAT'  '3' 0,
          'AENAM'  '3' 0,
          'AEDAT'  '3' 0,
          'VKOKRS' '3' 0,
          'ABUKRS' '3' 0,
          'VBUKRS' '3' 0,
          'WERKS'  '3' 0,
          'SIZECL' '3' 0,
          'WDATU'  '3' 0,
          'USR09'  '3' 0,
          'PRIORI' '3' 0,
          'AKOSTL' '3' 0,
          'VKOSTL' '3' 0,
          'AUART'  '3' 0,
          'PROFIDPROJ'  '3' 0,
          'PSPNR'   '3' 0,
          'GDATU'   '3' 0,
          'GJAHR'   '3' 0,
          'PRART'   '3' 0,
          'SCOPE'   '3' 0,
          'PARNR_VERA' '3' 0,
          'USR03'  '3' 0.




  gt_out-tabcolor[] = gt_specialcol[].
  modify gt_out transporting tabcolor where tabcolor is initial.

endform.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  build_cell_attr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_cell_attr.

  data: lt_celltab type lvc_t_styl,
        ls_celltab type lvc_s_styl.

  clear lt_celltab.
  refresh lt_celltab.

  clear gs_fcat.

  loop at gt_fcat into gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    if ls_celltab-fieldname eq 'POSNR' or
        ls_celltab-fieldname eq 'MSG' or
        ls_celltab-fieldname cp 'ICON*' or
        ls_celltab-fieldname cp 'STAT*' or
        ls_celltab-fieldname eq 'VERSI' or
        ls_celltab-fieldname cp 'VARNT' or
        ls_celltab-fieldname cp 'IVART' or
        ls_celltab-fieldname cp 'WAERS' or
        ls_celltab-fieldname cp 'ERNAM' or
        ls_celltab-fieldname cp 'ERDAT' or
        ls_celltab-fieldname cp 'AENAM' or
        ls_celltab-fieldname cp 'AEDAT' or
        ls_celltab-fieldname cp 'AUART' or
        ls_celltab-fieldname cp 'PRART' or
        ls_celltab-fieldname cp 'SCOPE' or
        ls_celltab-fieldname cp 'VKOKRS' or
        ls_celltab-fieldname cp 'ABUKRS' or
        ls_celltab-fieldname cp 'VBUKRS'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    else.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    endif.
    insert ls_celltab into table lt_celltab.
  endloop.

  clear gt_out-celltab.
  insert lines of lt_celltab into table gt_out-celltab.
  modify gt_out transporting celltab where celltab is initial.
  perform build_cell_attr1_lock.

endform.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_cell_attr1_lock.

*  DATA: LT_CELLTAB TYPE LVC_T_STYL,
*        LS_CELLTAB TYPE LVC_S_STYL.
*
*  CLEAR LT_CELLTAB.
*  REFRESH LT_CELLTAB.
*
*  __CLS GT_OUT-CELLTAB.
*  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE FEVOR = SPACE.
*
*  CLEAR GS_FCAT.
*
*  LOOP AT GT_FCAT INTO GS_FCAT.
*    LS_CELLTAB-FIELDNAME = GS_FCAT1-FIELDNAME.
*    LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
*  ENDLOOP.
*
*  INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
*  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE FEVOR = SPACE.
*

endform.                    " BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*&      Form  USER_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form user_status.
  __cls ftab.

  if g_grid->is_ready_for_input( ) eq 0.
    ftab-fcode = 'INST'.
    append ftab.
    ftab-fcode = 'INS10'.
    append ftab.
    ftab-fcode = 'DELE'.
    append ftab.
  endif.
  if p_call eq true.
    ftab-fcode = 'C137'.
    append ftab.
  endif.

  set pf-status '100' excluding ftab.
endform.                    " user_status
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  clear : g_error.

  ok_code = sy-ucomm.
  clear sy-ucomm.
  case ok_code.

    when 'BACK' or 'CANC'.
      perform free_container.
      leave to screen 0.
    when 'EXIT'.
      leave program.

    when 'VARI'.
      perform vari_ar.
      perform update_gt_out.
      wait up to '1' seconds.
      perform get_status.
      perform apply_icon.
      perform refresh_alv.

    when 'CREATE'.
      perform create_ar.
      perform update_gt_out.
      wait up to '1' seconds.
      perform get_status.
      perform apply_icon.
      perform refresh_alv.

    when 'UPDATE'.
      perform update_ar.
      perform update_gt_out.
      perform apply_icon.
      perform refresh_alv.
    when 'APPRV'.
      perform apprv_ar.
      wait up to '1' seconds.
      perform get_status.
      perform apply_icon.
      perform refresh_alv.

    when 'REJECT'.
      perform apprv_ar.
      wait up to '1' seconds.
      perform get_status.
      perform apply_icon.
      perform refresh_alv.

*    when 'DELETE'.
*      perform delete_ar.
*      perform update_gt_out.
*      wait up to '1' seconds.
*      perform get_status.
*      perform apply_icon.
*      perform refresh_alv.

    when 'SAVE'.
      perform : save_data,
                refresh_alv.
      __focus g_grid.

    when 'C135'.
      if p_call eq true.
        leave program.
      else.
        submit zafiu135 with p_bukrs eq p_bukrs
                        with s_posnr in s_posnr
                        with p_pyear eq p_gjahr
*                        WITH s_ivart IN s_ivart
                        with p_call eq true
                        and return.
      endif.

    when 'C137'.
      if p_call eq true.
        leave program.
      else.
        ranges: r_posnr for ztfiu132-posnr.
*FIXME only selected POSNR ...
        submit zafiu137 with p_bukrs = p_bukrs
                        with p_ayear  eq p_gjahr
                        with s_posnr in s_posnr
*                        WITH s_ivart IN s_ivart
                        with p_call eq true
                        and return.
      endif.

    when 'SWITCH'.
      if sy-dynnr eq '0100'.
        perform switch_edit_mode.
      endif.
      __focus g_grid.

    when 'UPDAV'.
      perform update_arv.
      perform update_gt_out.
      perform apply_icon.
      perform refresh_alv.

*    when 'PROC1'.
*      check g_error ne true.
*      perform data_confirm.
*      check g_error ne true.
*      perform proc using false.
*      perform update_gt_out.
*      perform apply_icon.
*      perform refresh_alv.
*
*    when 'PROC2'.
*      check g_error ne true.
*      perform data_confirm.
*      check g_error ne true.
*      perform proc using true.
*      perform update_gt_out.
*      perform apply_icon.
*      perform refresh_alv.

  endcase.

endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  free_container
*&---------------------------------------------------------------------*
form free_container.

  if not g_event_receiver is initial.
    free g_event_receiver.
  endif.

  if not g_grid is initial.
    call method g_grid->free.
  endif.

  if not g_custom_container is initial.
    call method g_custom_container->free.
  endif.

  free : g_grid,g_custom_container.

  clear :  gs_layo,gt_exclude,gt_out[],gt_fcat[],gt_sort[].


endform.                    " FREE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  really?
*&---------------------------------------------------------------------*
form really?.
  data $exists(1).
  data l_answer(1).

  perform pop_up using
      'This will save the AR data!'
      'Do you really want to proceed?' ' '
                 changing l_answer.

  if l_answer ne 'J'.
    g_error = true.
    message s000 with 'Processing was canceled by user.'.
  endif.
endform.                    " REALLY?
*&---------------------------------------------------------------------*
*&      Form  pop_up
*&---------------------------------------------------------------------*
form pop_up using    p_text p_text2 p_canc
            changing p_answer.

  call function 'POPUP_TO_CONFIRM_STEP'
    exporting
      textline1      = p_text
      textline2      = p_text2
      titel          = 'Check!'
      cancel_display = p_canc
    importing
      answer         = p_answer.


endform.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
form save_data.

  data iztfiu131 like ztfiu131 occurs 0 with header line.
  data $cnt type i.
  data :$lines(20), $msg(40).

  __cls $gt_out.

  perform get_selected_rows tables $gt_out.

  read table $gt_out index 1.
  if sy-subrc ne 0.
    message i000 with 'Please select at least 1 line'.
    exit.
  endif.

  loop at $gt_out.

    move-corresponding $gt_out to iztfiu131.

    if  iztfiu131-posnr is initial.
      message i000 with 'Please enter the AR Nubmer.'.
      exit.
    else.
      iztfiu131-zuser = sy-uname.
      iztfiu131-zsdat = sy-datum.
      iztfiu131-zstim = sy-uzeit.

      iztfiu131-gjahr = p_gjahr.
      iztfiu131-gjahr = p_abp.
      clear iztfiu131-monamt.

      append iztfiu131.
    endif.

  endloop.

  describe table iztfiu131 lines $cnt.
  if $cnt > 0.
    modify ztfiu131 from table iztfiu131.
    commit work.
    write $cnt to $lines.
    concatenate 'Successfully Saved :' $lines into $msg.
    message s000 with $msg.
  endif.

endform.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
form refresh_alv.
  __set_refresh_mode true.
  call method g_grid->refresh_table_display
    exporting
      is_stable      = stable
      i_soft_refresh = 'X'.

endform.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
form switch_edit_mode.

  data: lt_idxrow type lvc_t_row,
        lt_rowno  type lvc_t_roid,
        ls_rowno  type lvc_s_roid.

  data answer.

*  call method g_grid->get_selected_rows
*    importing
*      et_index_rows = lt_idxrow
*      et_row_no     = it_rowno.
*
  if g_grid->is_ready_for_input( ) eq 0.
    call method g_grid->set_ready_for_input
      exporting
        i_ready_for_input = 1.

    perform info_text_set using true.

  else.
**    IF FLAG_DATA_CHANGED EQ TRUE.
**      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
**           EXPORTING
**                TEXTLINE1     = 'Data has not been saved yet.'
**                TEXTLINE2     = 'Do you want to continue anyway? '
**                TITEL         = 'Confirmation'
**                DEFAULTOPTION = 'N'
**           IMPORTING
**                ANSWER        = ANSWER.
**      CHECK ANSWER EQ 'J'.
**    ENDIF.
**    CLEAR FLAG_DATA_CHANGED.
    call method g_grid->set_ready_for_input
      exporting
        i_ready_for_input = 0.
    perform info_text_set using false.
  endif.

  perform build_cell_attr.
endform.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  info_text_set
*&---------------------------------------------------------------------*
form info_text_set using    p_true.
  if p_true eq true.
    info = text-015.
  else.
    info = text-015.
  endif.

endform.                    " INFO_TEXT_SET

*---------------------------------------------------------------------*
*       FORM get_selected_rows                                        *
*---------------------------------------------------------------------*
form get_selected_rows tables $gt_out structure gt_out.

  data: lt_rows type lvc_t_row with header line,
        lt_row_no type lvc_t_roid. "Numeric IDs of Selected Rows

  call method g_grid->get_selected_rows
    importing
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  call method cl_gui_cfw=>flush.

  if sy-subrc ne 0.
    message e000
    with 'Error founded during flushing of ALV Grid Control'.
    exit.
  endif.

  read table lt_rows index 1.
  if sy-subrc ne 0.
*    $gt_out[] = gt_out[].
  else.
    loop at lt_rows where rowtype is initial.
      read table gt_out index lt_rows-index.
      gt_out-chk = true .
      modify gt_out index lt_rows-index .
    endloop.
    loop at gt_out.
      check gt_out-chk eq true.
      $gt_out = gt_out.
      append $gt_out.
    endloop.
  endif.

  gt_out-chk = false .
  modify gt_out transporting chk where chk eq true.

endform.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  really_del?
*&---------------------------------------------------------------------*
form really_del?.

  data $exists(1).
  data l_answer(1).

  perform pop_up using
      'This will delete the data from table also. '
      'Do you really want to delete the selected data?' ' '
                 changing l_answer.

  if l_answer ne 'J'.
    g_error = true.
    message s000 with 'Processing was canceled by user.'.
  endif.

endform.                    " really_del?
*&---------------------------------------------------------------------*
*&      Form  delete_line
*&---------------------------------------------------------------------*
form delete_line.

  data iztfiu131 like ztfiu131 occurs 0 with header line.
  data $cnt type i.
  data :$lines(10), $msg(40).

  __cls $gt_out.

  perform get_selected_rows tables $gt_out.

  read table $gt_out index 1.
  if sy-subrc ne 0.
    message i000 with 'Please select at least 1 line'.
    exit.
  endif.

  perform really_del?.
  check g_error ne true.

  loop at $gt_out.

*    SELECT SINGLE * FROM ztfiu131
*     WHERE posnr EQ $gt_out-posnr.
*    IF sy-subrc EQ 0.
*      MOVE-CORRESPONDING $gt_out TO iztfiu131.
*      APPEND iztfiu131.
*    ENDIF.
*
    delete gt_out where posnr = $gt_out-posnr.

  endloop.

*  DESCRIBE TABLE iztfiu131 LINES $cnt.

  if $cnt > 0 and g_import eq false.
*    DELETE ztfiu131 FROM TABLE iztfiu131.
*    COMMIT WORK.
*
    write $cnt to $lines.
    concatenate 'Successfully Deleted :' $lines into $msg.
    message s000 with $msg.

  endif.

endform.                    " delete_line
*&---------------------------------------------------------------------*
*&      Form  insert_line
*&---------------------------------------------------------------------*
form insert_line.
  clear gt_out.
  append gt_out.
endform.                    " insert_line
*&---------------------------------------------------------------------*
*&      Form  double_click
*&---------------------------------------------------------------------*
form double_click using  e_row     type lvc_s_row
                         e_column  type lvc_s_col
                         es_row_no type lvc_s_roid.
  data $ima_posid type ima_posid.

  clear gv_index.
  gv_index = e_row-index.

  read table gt_out index gv_index.

  if sy-subrc eq 0.

    $ima_posid =  gt_out-posnr.

    set parameter id : 'IAG'  field $ima_posid, "gt_out-posnr,
                       'IAA'  field gt_out-ivart,
                       'IAF'  field $ima_posid.


    call transaction 'IMA3N' and skip first screen.
  endif.

  call method cl_gui_control=>set_focus
    exporting
      control = g_grid.


endform.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  apply_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form apply_icon.
  data $ix like sy-tabix.
  data $posnr like imak-posnr.

  loop at gt_out.
    $ix = sy-tabix.

    if gt_out-status eq 'DELE'.
      gt_out-flag1 = 'D'.
    endif.

    case gt_out-flag1.

      when ' '.
        gt_out-icon = icon_light_out.

      when 'C'.
        gt_out-icon = icon_yellow_light.
      when 'A'.
        gt_out-icon = icon_green_light.
      when 'E'.
        gt_out-icon = icon_red_light.

      when 'R'.
        gt_out-icon = icon_red_light.
      when 'F'.
        gt_out-icon = icon_red_light.
      when 'D'.
        gt_out-icon = icon_red_light.

*      WHEN 'X'.
*        gt_out-icon = icon_green_light.
*      WHEN 'E'.
*        gt_out-icon = icon_red_light.
*      WHEN 'N'.
    endcase.

    modify gt_out index $ix transporting icon.
  endloop.

endform.                    " apply_icon
*&---------------------------------------------------------------------*
*&      Form  get_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_status.
  data $ix type i.
  data $posnr like imak-posnr.
  data $flag.

*  sort gt_out by posnr versi varnt.
*  clear : gt_out-flag1,
*          gt_out-status,
*          gt_out-status_desc.
*
* MODIFY gt_out TRANSPORTING flag1 status status_desc WHERE chk EQ false
*.

  loop at gt_out.
    $ix = sy-tabix.

    select single posnr into $posnr from imak
       where posnr eq gt_out-posnr.

    if sy-subrc eq 0.
      gt_out-flag1 = true.

      __cls : it_status, it_user_status,
              it_varnt_status, it_varnt_user_status.

      perform get_ar_status using gt_out-posnr
                            changing gt_out-flag1
                                     gt_out-status
                                     gt_out-status_desc.

      modify gt_out index $ix
             transporting  flag1 status status_desc.

    endif.

  endloop.

endform.                    " get_status
*&---------------------------------------------------------------------*
*&      Form  GET_AR_STATUS
*&---------------------------------------------------------------------*
form get_ar_status  using    i_ar          like gt_out-posnr
                    changing o_flag1       like gt_out-flag1
                             o_status      like gt_out-status
                             o_status_desc like gt_out-status_desc.
  data: lv_ar          type bapi_appreq_id-appreq.
  data: lt_status like bapiappreqstatus occurs 0 with header line.

  lv_ar = i_ar.
*  call function 'BAPI_APPREQUEST_GETSTATUS'
*    exporting
*      externalnumber              = lv_ar
*      language                    = sy-langu
*    tables
*      apprequest_status           = lt_status
*     apprequest_user_status      = it_user_status
*     apprequestvarnt_status      = it_varnt_status
*     apprequestvarnt_user_status =            it_varnt_user_status.

  read table  lt_status  index  1.
  if sy-subrc = 0.
    case lt_status-text.
      when 'CRTD'.
        o_flag1 = 'C'.
      when 'REJD'.
        o_flag1 = 'J'.
      when 'DLFL'.
        o_flag1 = 'D'.
      when 'TECO' or 'CLSD'.
        o_flag1 = 'P'.
      when 'FAPP'.
        o_flag1 = 'A'.
      when others.
        o_flag1 = 'R'.
    endcase.

    o_status = it_status-status.
    o_status_desc = it_status-description.
  endif.

*  data: l_objnr type j_objnr.
*  select single objnr into l_objnr from imak where posnr = i_ar.
*  call function 'STATUS_READ'
*    exporting
*      objnr                  = l_objnr
**   IMPORTING
**     STONR                  =   "user status no.
**   TABLES
**     STATUS                 =
**   EXCEPTIONS
**     OBJECT_NOT_FOUND       = 1
**     OTHERS                 = 2
*            .
*  if sy-subrc = 0.
*
*  call function 'STATUS_PROPERTIES_GET'
*    exporting
**     BYPASS_BUFFER          = ' '
**     CLIENT                 = SY-MANDT
*      objnr                  =
*      status                 =
**   IMPORTING
**     ACTIVE                 =
**     DEACTIVATED            =
**     NEVER_ACTIVE           =
**   EXCEPTIONS
**     OBJECT_NOT_FOUND       = 1
**     OTHERS                 = 2
*            .
*  if sy-subrc <> 0.
** Implement suitable error handling here
*  endif.
*
**User status table: TJ30-STONR
**CRTD Created
**FAPP For approval
**APRV Approved
**MDLK Master data locked

endform.                    " GET_AR_STATUS

*&---------------------------------------------------------------------*
*&      Form  apprv_ar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form apprv_ar.

  data $cnt type i.
  data  aposnr  like bapi_appreq_id-appreq.
  data $ima_posid type ima_posid.
  data $ix type i.
  __cls $gt_out.

  perform get_selected_rows tables $gt_out.

  read table $gt_out index 1.
  if sy-subrc ne 0.
    message i000 with 'Please select at least 1 line'.
    exit.
  endif.

  describe table $gt_out lines $cnt.
  if $cnt > 0.

    loop at $gt_out.
*      SELECT SINGLE * FROM imak WHERE posnr EQ $gt_out-posnr.
*      IF sy-subrc NE 0.
*        MESSAGE i000 WITH $gt_out-posnr ' has not been created!'.
*        CONTINUE.
*      ENDIF.
*
      $ima_posid =  $gt_out-posnr.

      set parameter id 'IAF'  field $ima_posid.

      call transaction 'IMA2N' and skip first screen .

    endloop.

  endif.


endform.                    " apprv_ar
*&---------------------------------------------------------------------*
*&      Form  vari_ar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form vari_ar.

  data $cnt type i.
  data  aposnr  like bapi_appreq_id-appreq.
  data $ix type i.
  data retcode type c.

  __cls $gt_out.

  perform get_selected_rows tables $gt_out.

  read table $gt_out index 1.
  if sy-subrc ne 0.
    message i000 with 'Please select at least 1 line'.
    exit.
  endif.

  describe table $gt_out lines $cnt.
  if $cnt > 0.

    loop at $gt_out.
      $ix = sy-tabix.

      perform fill_bapi_data.

***---VARIANT_TO_VERSION.
*      MOVE $gt_out-gjahr  TO it_variant-appr_year.
*      MOVE '10'  TO it_variant-plan_version.
*      APPEND it_variant.
*
      aposnr = $gt_out-posnr.

      it_version-appr_year    = p_gjahr.
      it_version-plan_version = p_mvers.

      append it_version.

      call function 'BAPI_APPREQUEST_ADDVARIANT'
        exporting
          externalnumber     = aposnr
          variant            = wa_variant
          plan_total         = wa_plan_total
          test_run           = p_test
        tables
          variant_to_version = it_version
          plan_year          = it_plan_year
          return             = i_msg.

      perform check_bapi_message.

      modify $gt_out index $ix transporting msg flag1.

      call function 'BAPI_TRANSACTION_COMMIT'
        importing
          return = i_msg.


    endloop.


  endif.

endform.                    " vari_ar

*&---------------------------------------------------------------------*
*&      Form  create_ar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_ar.

  data $cnt type i.
  data  aposnr  like bapi_appreq_id-appreq.
  data $ix type i.
  __cls $gt_out.

  perform get_selected_rows tables $gt_out.

  read table $gt_out index 1.
  if sy-subrc ne 0.
    message i000 with 'Please select at least 1 line'.
    exit.
  endif.

  describe table $gt_out lines $cnt.
  if $cnt > 0.

    loop at $gt_out.
      if $gt_out-flag1 eq true.
        message i000 with $gt_out-posnr ' has been created already!'.
        continue.
      endif.
      $ix = sy-tabix.

      perform fill_bapi_data.

      aposnr = $gt_out-posnr.

      call function 'BAPI_APPREQUEST_CREATE'
        exporting
          appropriationrequest_in        = aposnr
          apprequest_type                = $gt_out-ivart
          controlling_area               = g_kokrs
          master_data                    = wa_master
          user_fields                    = wa_user_fields
          variant                        = wa_variant
          plan_total                     = wa_plan_total
          language                       = sy-langu
          test_run                       = p_test
        importing
          externalnumber                 = wa_externalnumber
          appropriationrequestvariantout = wa_appropriat
        tables
          org_units                      = it_org_units
          material_grp                   = it_material_grp
          investment_reason              = it_invest_reason
          partner                        = it_partner
          variant_to_version             = it_variant
          plan_year                      = it_plan_year
          return                         = i_msg.

      perform check_bapi_message.

      modify $gt_out index $ix transporting msg flag1.

      call function 'BAPI_TRANSACTION_COMMIT'
        importing
          return = i_msg.


    endloop.


  endif.

endform.                    " create_ar
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_data
*&---------------------------------------------------------------------*
form fill_bapi_data.
  data : wk_date like bapiappreqvarntassign-appr_year.
  data : wk_date1 like sy-datum.

  clear : wa_master, wa_user_fields, wa_variant, wa_plan_total.

*========= IMPORT VALUE
* Master_DATA
  wa_master-req_txt         = $gt_out-txt50.
  wa_master-req_comp_code   = p_bukrs.
  wa_master-rsp_comp_code   = p_bukrs.
  wa_master-plant           = $gt_out-werks.
  wa_master-scale           = $gt_out-sizecl.
  wa_master-priority        = $gt_out-priori.
  wa_master-rsp_cost_center = $gt_out-vkostl.
  wa_master-orig_appr_year  = $gt_out-gjahr.
  wa_master-desired_start   = $gt_out-wdatu.
  wa_master-objectclass     = $gt_out-scope.
  wa_master-object_currency = $gt_out-waers.
  wa_master-strat_indicator = $gt_out-stratflg.

*--User_fields
*  wa_user_fields-user00     =  $gt_out-zpjt.
  wa_user_fields-user02     =  $gt_out-usr02.
  wa_user_fields-user03     =  $gt_out-usr03. " Asset Class
  wa_user_fields-user09_date  =  $gt_out-usr09. "

*  wa_user_fields-user01     =  $gt_out-zpot.

*  perform make_user01 using wa_user_fields-user01.

*--Variant
  wa_variant-description        = 'Initial Plan'.
  wa_variant-assessmnt_crit     =  '0001' ." bwert.
  wk_date = $gt_out-gjahr + 1.
  concatenate wk_date '0101' into wk_date1.
  wa_variant-value_date   =  wk_date1.

*--PLAN TOTAL
*  wa_plan_total-overhead_costs   = $gt_out-amount.

  wa_plan_total-investment_costs = $gt_out-prvamt + $gt_out-curamt +
  $gt_out-nxtamt1 + $gt_out-nxtamt2 + $gt_out-nxtamt3 + $gt_out-nxtamt4.

*--- Table  ORG_UNITS
  __cls it_org_units.
  move  $gt_out-akostl to  it_org_units-req_cost_center.
  move '100'           to  it_org_units-percentage.
  append it_org_units.

*--it_invest_reason .
  __cls it_invest_reason.
  it_invest_reason-inv_reason = $gt_out-izwek.
  it_invest_reason-percentage = '100'.
  append it_invest_reason.

*--- IT_PARTNER.
  __cls it_partner.

  move 'I1'                to it_partner-partner_function.
  move $gt_out-parnr_vera  to it_partner-partner.
  append it_partner.

  clear  it_partner.
  move 'I4'                to it_partner-partner_function.
  move $gt_out-parnr_vera  to it_partner-partner.
  append it_partner.

*---VARIANT_TO_VERSION.
  __cls it_variant.
  move p_gjahr        to it_variant-appr_year.
  move p_mvers        to it_variant-plan_version.
  append it_variant.

**---VARIANT_TO_VERSION.
*  __cls it_variant.
*
*  MOVE $gt_out-gjahr  TO it_variant-appr_year.
*  MOVE $gt_out-VARNT  TO it_variant-plan_version.
*  APPEND it_variant.

*----IT_PLAN_YEAR
  __cls it_plan_year.
  wk_date = $gt_out-gjahr.                                  " - 1.


*FIXME

*  DO 9 TIMES.
  do 1 times.
    if sy-index = 1.
      move wk_date to it_plan_year-fiscal_year.
*      MOVE $gt_out-amount TO it_plan_year-overhead_costs.
      move $gt_out-curamt to it_plan_year-investment_costs.
*      MOVE it_data-ertrag2 TO it_plan_year-revenue.
    elseif sy-index = 2.
      move wk_date to it_plan_year-fiscal_year.
*      MOVE it_data-gemkos3 TO it_plan_year-overhead_costs.
*      MOVE it_data-invkos3 TO it_plan_year-investment_costs.
*      MOVE it_data-ertrag3 TO it_plan_year-revenue.
    elseif sy-index = 3.
      move wk_date to it_plan_year-fiscal_year.
*      MOVE it_data-gemkos4 TO it_plan_year-overhead_costs.
*      MOVE it_data-invkos4 TO it_plan_year-investment_costs.
*      MOVE it_data-ertrag4 TO it_plan_year-revenue.
    elseif sy-index = 4.
      move wk_date to it_plan_year-fiscal_year.
*      MOVE it_data-gemkos5 TO it_plan_year-overhead_costs.
*      MOVE it_data-invkos5 TO it_plan_year-investment_costs.
*      MOVE it_data-ertrag5 TO it_plan_year-revenue.
    elseif sy-index = 5.
      move wk_date to it_plan_year-fiscal_year.
*      MOVE it_data-gemkos6 TO it_plan_year-overhead_costs.
*      MOVE it_data-invkos6 TO it_plan_year-investment_costs.
*      MOVE it_data-ertrag6 TO it_plan_year-revenue.
    elseif sy-index = 6.
      move wk_date to it_plan_year-fiscal_year.
*      MOVE it_data-gemkos7 TO it_plan_year-overhead_costs.
*      MOVE it_data-invkos7 TO it_plan_year-investment_costs.
*      MOVE it_data-ertrag7 TO it_plan_year-revenue.
    elseif sy-index = 7.
      move wk_date to it_plan_year-fiscal_year.
*      MOVE it_data-gemkos8 TO it_plan_year-overhead_costs.
*      MOVE it_data-invkos8 TO it_plan_year-investment_costs.
*      MOVE it_data-ertrag8 TO it_plan_year-revenue.
    elseif sy-index = 8.
      move wk_date to it_plan_year-fiscal_year.
*      MOVE it_data-gemkos9 TO it_plan_year-overhead_costs.
*      MOVE it_data-invkos9 TO it_plan_year-investment_costs.
*      MOVE it_data-ertrag9 TO it_plan_year-revenue.
    elseif sy-index = 9.
      move wk_date to it_plan_year-fiscal_year.
*      MOVE it_data-gemkos10 TO it_plan_year-overhead_costs.
*      MOVE it_data-invkos10 TO it_plan_year-investment_costs.
*      MOVE it_data-ertrag10 TO it_plan_year-revenue.
    endif.
    append it_plan_year.
    clear it_plan_year.
*    wk_date = wk_date + 1.

  enddo.

endform.                    " fill_bapi_data

*---------------------------------------------------------------------*
*       FORM make_user01                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_USER01                                                      *
*---------------------------------------------------------------------*
*FORM make_user01 using p_user01.
*
*  concatenate $gt_out-zpjt '^' $gt_out-zpot '^' $gt_out-zcomp '^'
*  $gt_out-zinvt '^' $gt_out-zdivision '^' $gt_out-zstd1 '^'
*  $gt_out-zstd2 into p_user01.
*
*ENDFORM.                    " make_user01
*&---------------------------------------------------------------------*
*&      Form  check_bapi_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_bapi_message.

  read table i_msg with key type = 'E'." TRANSPORTING NO FIELDS.
  if sy-subrc ne 0.
    call function 'BAPI_TRANSACTION_COMMIT'.
    $gt_out-flag1 = true.
    $gt_out-msg = 'success'.
  else.
    $gt_out-msg = i_msg-message.
    $gt_out-flag1 = 'E'.
  endif.

  append lines of i_msg to g_msg .

endform.                    " check_bapi_message
*&---------------------------------------------------------------------*
*&      Form  delete_ar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_ar.
  data $cnt type i.
  data  aposnr  like bapi_appreq_id-appreq.
  data $ix type i.

  __cls $gt_out.

  perform get_selected_rows tables $gt_out.

  read table $gt_out index 1.
  if sy-subrc ne 0.
    message i000 with 'Please select at least 1 line'.
    exit.
  endif.

  describe table $gt_out lines $cnt.
  if $cnt > 0.

    loop at $gt_out.
      $ix = sy-tabix.
      if $gt_out-flag1 ne true.
        message i000 with $gt_out-posnr ' has not been created!'.
        continue.
      endif.

      aposnr = $gt_out-posnr.

      call function 'BAPI_APPREQUEST_DELETE'
        exporting
          externalnumber = aposnr
          test_run       = p_test
        tables
          return         = i_msg.

      perform check_bapi_message.

      if  $gt_out-flag1 ne 'E'.
        clear $gt_out-flag1.
      endif.

      modify $gt_out index $ix transporting msg flag1.

    endloop.

  endif.


endform.                    " delete_ar
*&---------------------------------------------------------------------*
*&      Form  update_ar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_ar.
  data $cnt type i.
  data  aposnr  like bapi_appreq_id-appreq.
  data $ix type i.
  data : old_akostl like imakpa-akostl,
         old_izwek like imakpi-izwek.

  __cls $gt_out.

  perform get_selected_rows tables $gt_out.

  read table $gt_out index 1.
  if sy-subrc ne 0.
    message i000 with 'Please select at least 1 line'.
    exit.
  endif.

  describe table $gt_out lines $cnt.
  if $cnt > 0.

    loop at $gt_out.
      select single * from imak where posnr eq $gt_out-posnr.
      if sy-subrc ne 0.
        message i000 with $gt_out-posnr ' has not been created!'.
        continue.
      endif.
      $ix = sy-tabix.
      perform fill_bapi_data.
      aposnr = $gt_out-posnr.

      __cls : it_org_units, it_invest_reason,
              it_partner, it_division.

      __cls : it_org_units_x, it_invest_reason_x,
              it_partner_x, it_division_x.

      wa_master_x-req_txt_x = true.
      wa_master_x-plant_x = true.
      wa_master_x-scale_x = true.
      wa_master_x-desired_start_x = true.
      wa_master_x-priority_x = true.
      wa_master_x-rsp_cost_center_x = true.
      wa_master_x-trading_partner_x = true.
      wa_master_x-appr_date_x  = true.
      wa_master_x-orig_appr_year_x = true.
      wa_master_x-location_x = true.
      wa_master_x-objectclass_x = true.
      wa_master_x-object_currency_x = true.

*      wa_user_fields_x-user00_x     =  true.
      wa_user_fields_x-user01_x     =  true.
      wa_user_fields_x-user02_x     =  true.
      wa_user_fields_x-user03_x     =  true.
      wa_user_fields_x-user09_date_x     =  true.

      loop at it_partner.
        it_partner_x-partner_function_x = true.
        it_partner_x-partner_x = true.
        append it_partner_x.
      endloop.

*      APPEND : it_org_units_x, it_division_x, it_invest_reason_x.

      select single akostl into old_akostl
           from imakpa where posnr eq $gt_out-posnr.

      select single izwek into old_izwek
           from imakpi where posnr eq $gt_out-posnr.

      if old_akostl ne $gt_out-akostl.
        it_org_units-req_cost_center = old_akostl.
        append it_org_units.
        it_org_units_x-flag_d_x = 'D'.
        append it_org_units_x.

        move  $gt_out-akostl to  it_org_units-req_cost_center.
        move '100'           to  it_org_units-percentage.
        append it_org_units.
        it_org_units_x-flag_d_x = 'X'.
        append it_org_units_x.
      endif.

      if old_izwek ne $gt_out-izwek.
        it_invest_reason-inv_reason = old_izwek.
        append it_invest_reason.
        it_invest_reason_x-flag_d_x = 'D'.
        append it_invest_reason_x.

        it_invest_reason-inv_reason = $gt_out-izwek.
        it_invest_reason-percentage = '100'.
        append it_invest_reason.
        it_invest_reason_x-flag_d_x = 'X'.
        append it_invest_reason_x.
      endif.

      __cls i_msg.

      call function 'BAPI_APPREQUEST_CHANGE'
        exporting
          externalnumber      = aposnr
          master_data         = wa_master
          master_data_x       = wa_master_x
          user_fields         = wa_user_fields
          user_fields_x       = wa_user_fields_x
          language            = sy-langu
          test_run            = p_test
        tables
          org_units           = it_org_units
          org_units_x         = it_org_units_x
          investment_reason   = it_invest_reason
          investment_reason_x = it_invest_reason_x
*         partner             = it_partner
*         partner_x           = it_partner_x
*         variant_to_version  = it_variant
*         plan_year           = it_plan_year
          return              = i_msg.

      perform check_bapi_message.

      modify $gt_out index $ix transporting msg flag1.

      if $gt_out-flag1 ne 'E'.
        call function 'BAPI_TRANSACTION_COMMIT'
          importing
            return = i_msg.
      endif.

    endloop.

  endif.

endform.                    " update_ar
*---------------------------------------------------------------------*
*       FORM update_gt_out                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form update_gt_out.
  data $ix type i.

  loop at $gt_out.
    read table gt_out with key posnr = $gt_out-posnr
                               versi = $gt_out-versi
                               varnt = $gt_out-varnt.
    if sy-subrc eq 0.
      $ix = sy-tabix.
      gt_out-msg = $gt_out-msg.
      gt_out-flag1 = $gt_out-flag1.
      gt_out-anlkl = $gt_out-anlkl.
      gt_out-aktiv = $gt_out-aktiv.
      modify gt_out index $ix transporting msg flag1 anlkl aktiv.
    endif.
  endloop.
endform.                    " update_gt_out

*---------------------------------------------------------------------*
*       FORM update_arv                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form update_arv.

  data $cnt type i.
  data  aposnr  like bapi_appreq_id-appreq.
  data $ix type i.
  __cls $gt_out.

  perform get_selected_rows tables $gt_out.

  read table $gt_out index 1.
  if sy-subrc ne 0.
    message i000 with 'Please select at least 1 line'.
    exit.
  endif.

  describe table $gt_out lines $cnt.
  if $cnt > 0.

    loop at $gt_out.
      select single * from imak where posnr eq $gt_out-posnr.
      if sy-subrc ne 0.
        message i000 with $gt_out-posnr ' has not been created!'.
        continue.
      endif.
      $ix = sy-tabix.
      perform fill_bapi_data.
      aposnr = $gt_out-posnr.

      call function 'BAPI_APPREQUEST_SETPLANVALUES'
        exporting
          externalnumber              = aposnr
          appropriationrequestvariant = $gt_out-varnt
          plan_total                  = wa_plan_total
*         TEST_RUN                    = ' '
        tables
          plan_year                   = it_plan_year
          return                      = i_msg.

      perform check_bapi_message.

      modify $gt_out index $ix transporting msg flag1.

      call function 'BAPI_TRANSACTION_COMMIT'
        importing
          return = i_msg.

    endloop.

  endif.

endform.                    " update_arv
*&---------------------------------------------------------------------*
*&      Form  data_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form data_confirm.

  data l_answer(1).

  perform pop_up using
      'Simulation Data will be created'
      'Do you want to proceed it?' ' '
                 changing l_answer.

  if l_answer ne 'J'.
    g_error = true.
    message s000 with 'Processing was canceled by user.'.
  endif.

endform.                    " data_confirm
**&---------------------------------------------------------------------*
**&      Form  proc
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_FALSE  text
**----------------------------------------------------------------------*
*form proc using p_reset.
*  data $ix type i.
*  data $cnt type i.
*  __cls $gt_out.
*
*  perform get_selected_rows tables $gt_out.
*
*  loop at $gt_out.
*
*    $ix = sy-tabix.
*
*    move $gt_out-objnr to $objnr.
*
*    if p_reset = 'X'.
*
*      perform reset_depr using $objnr.
*
*      call function 'DEPREC_SIMUL_MODIFIKATION'
*        tables
*          t_ania = lt_ania
*          t_anib = lt_anib.
*
*    else.
*
*      perform simulate_depr.
*
*      call function 'DEPREC_SIMUL_MODIFIKATION'
*        tables
*          t_ania = lt_ania
*          t_anib = lt_anib1.
*
*    endif.
*    if sy-subrc eq 0.
*      concatenate $gt_out-posnr ' is updated!' into $gt_out-msg.
*      modify $gt_out index $ix.
*    endif.
*  endloop.
*
*  loop at $gt_out.
*    $ix = sy-tabix.
*    clear : $gt_out-anlkl, $gt_out-aktiv.
*
*    select single b~anlkl b~aktiv
*         into ($gt_out-anlkl,
*               $gt_out-aktiv)
*              from anib as a
*       inner join ania as b
*         on  b~objnr eq a~objnr
*         and b~lfdnr eq a~lfdnr
*       where a~objnr eq $gt_out-objnr
*         and a~afabe eq '20'.
*
*    modify $gt_out index $ix transporting anlkl aktiv.
*
*  endloop.
*
*endform.                    " proc
**&---------------------------------------------------------------------*
**&      Form  reset_depr
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_GT_OUT_OBJNR  text
**----------------------------------------------------------------------*
*form reset_depr using f_objnr.
*  refresh: lt_ania, lt_anib.
*
*  select * into corresponding fields of table lt_ania
*      from ania where objnr = f_objnr.
*
*  select * into corresponding fields of table lt_anib
*      from anib where objnr = f_objnr.
*
*  if sy-subrc = 0.
*    lt_ania-kz    = 'D'.
*    lt_anib-kz    = 'D'.
*
*    modify lt_ania transporting kz where objnr = f_objnr.
*    modify lt_anib transporting kz where objnr = f_objnr.
*  endif.
*
*endform.                    " reset_depr
**&---------------------------------------------------------------------*
**&      Form  simulate_depr
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*form simulate_depr.
*  clear gv_chk.
*
*  data:
*    ls_rania like rania,
*    ls_anib  like ranib,
*    l_subrc  like sy-subrc,
*    l_nodep  like sy-subrc.
*
*  data:
*        i_objnr like  ania-objnr,
*        i_anlkl like  ania-anlkl,
*        i_aktiv like  ania-aktiv,
*        i_bukrs like  t001-bukrs,
*        i_kokrs like  tka01-kokrs,
*        i_ivpro like  taif1-ivpro,
*        i_repid like  sy-repid,
*        i_dynnr like  sy-dynnr,
*        i_aktyp like  t020-aktyp.
*
*  data: it_afasplit like afasplit occurs 0.
*
*  i_bukrs = $gt_out-vbukrs.
**asset class
*  call function 'CONVERSION_EXIT_ALPHA_INPUT'
*    exporting
*      input  = $gt_out-anlkl
*    importing
*      output = i_anlkl.
**cap.date
*  i_aktiv = $gt_out-aktiv.
*  i_objnr = $objnr.
*
**investment profile
*  select single ivpro into i_ivpro
*     from taprf
*     where anlkl_s = i_anlkl.
*  if sy-subrc = 0.
*    l_nodep = 0.
*  else.
** no depr.asset... delete..simulation data.
*    l_nodep = 1.
*  endif.
*
*  i_dynnr = sy-dynnr.
*  i_repid = sy-repid.
*
** controlling area
*  select single kokrs into i_kokrs
*    from tka02
*    where bukrs = i_bukrs.
*
*  clear: lt_anib, lt_anib1.
*  refresh: lt_anib, lt_anib1.
*
*  call function 'AIPS_SIMUL_CHECK'
*    exporting
*      i_kokrs             = i_kokrs
*      i_bukrs             = i_bukrs
*      i_anlkl             = i_anlkl
*      i_aktiv             = i_aktiv
*      i_objnr             = i_objnr
*      i_ivpro             = i_ivpro
*      i_dynnr             = i_dynnr
*      i_repid             = i_repid
*    importing
*      es_ania             = ls_rania
*      e_subrc             = l_subrc
*    tables
*      et_anib             = lt_anib
*    exceptions
*      kein_bukrs          = 1
*      bukrs_kein_am_bukrs = 2
*      perioden_nicht_def  = 3.
*
*  check l_subrc is initial.
*
*
**IOPAAA0YR0001 0005    |0000022001|     |100.00
**FUNCTION DEPREC_SIMUL_MODIFIKATION.
**
**      CON_INS           LIKE T020-AKTYP  VALUE 'I',
**                                      - Update
**      CON_UPD           LIKE T020-AKTYP  VALUE 'U',
**                                      - Update set XUNVL
**      CON_UPD_SET       LIKE T020-AKTYP  VALUE 'S',
**                                      - delete
**      CON_DEL           LIKE T020-AKTYP  VALUE 'D',
*
*  clear i_imakpa.
*  refresh i_imakpa.
*
*  select * into table i_imakpa
*    from imakpa where posnr = $gt_out-posnr.
*
*  clear lt_ania.
*  refresh lt_ania.
*  move-corresponding ls_rania to lt_ania.
*
**change user, date
*  lt_ania-ernam = sy-uname.
*  lt_ania-erdat = sy-datum.
*
** set insert/update/...
**  read table lt_anib into ls_anib index 1.
**  lt_ania-KZ    = ls_anib-KZ.
*  select single * from ania where objnr = i_objnr.
*
*  if sy-subrc = 0.
*    perform insert_appr_req using i_objnr.
*    perform delete_appr_req using i_objnr.
*
*  else.
*    perform insert_appr_req using i_objnr.
*  endif.
*
*endform.                    " update_depr
**&---------------------------------------------------------------------*
**&      Form  insert_appr_req
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_I_OBJNR  text
**----------------------------------------------------------------------*
*form insert_appr_req using p_objnr type j_objnr.
*  lt_ania-kz = 'I'.
*  perform append_lt_ania using 'I'.
*
*  lt_anib-lfdnr = lt_ania-lfdnr.
*  lt_anib-kz = 'I'.
*  perform append_lt_anib1 using p_objnr.
*
*endform.                    " INSERT_APPR_REQ
**&---------------------------------------------------------------------*
**&      Form  append_lt_ania
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_4740   text
**----------------------------------------------------------------------*
*form append_lt_ania using p_kz.
*  loop at i_imakpa.
*    if p_kz = 'I'.
*      if ania is initial.
*        clear lt_ania-lfdnr.
*      else.
*        lt_ania-lfdnr = ania-lfdnr + 1.
*      endif.
*
*    elseif p_kz = 'D'.
*      move-corresponding ania to lt_ania.
*    endif.
*
*    lt_ania-kostl = i_imakpa-akostl.
*    lt_ania-aufpr = i_imakpa-aproz.
*    append lt_ania.
*  endloop.
*
*endform.                    " APPEND_LT_ANIA
**&---------------------------------------------------------------------*
**&      Form  append_lt_anib1
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_P_OBJNR  text
**----------------------------------------------------------------------*
*form append_lt_anib1 using p_objnr type j_objnr.
*  modify lt_anib transporting lfdnr kz where objnr = p_objnr.
*
*  loop at lt_anib.
*    move-corresponding lt_anib to lt_anib1.
*    append lt_anib1.
*    clear lt_anib1.
*  endloop.
*
*endform.                    " APPEND_LT_ANIB1
**&---------------------------------------------------------------------*
**&      Form  delete_appr_req
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_I_OBJNR  text
**----------------------------------------------------------------------*
*form delete_appr_req using p_objnr type j_objnr.
*  lt_ania-kz = 'D'.
*  perform append_lt_ania using 'D'.
*
*  lt_anib-lfdnr = lt_ania-lfdnr.
*  lt_anib-kz = 'D'.
*  perform append_lt_anib1 using p_objnr.
*
*endform.                    " DELETE_APPR_REQ
*&---------------------------------------------------------------------*
*&      Form  GET_DEFAULT
*&---------------------------------------------------------------------*
form get_default .

  select single kokrs into g_kokrs from tka02 where bukrs = p_bukrs.

* if project is closed, then display mode for all fields
  select * from ztim008 into table gt_prjcode
             where zsta ne 'C'.

  sort gt_prjcode by zpro.

endform.                    " GET_DEFAULT
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.
  ranges: r_stratflg for imak-stratflg.
  data  : lt_ar_new type table of ty_row_tab with header line.
  __cls : it_row_tab.

  if p_abp = 'X'.
    r_stratflg-option = 'EQ'.
    r_stratflg-sign   = 'I'.
    r_stratflg-low = 'X'.
    append r_stratflg.
  endif.

* select current year appr.req
  select a~posnr a~stratflg
         b~txt50 a~ivart a~waers a~ernam a~erdat
         a~aenam a~aedat a~vkokrs a~abukrs a~vbukrs a~werks a~sizecl
         a~wdatu a~priori a~vkostl a~auart a~profidproj
         a~pspnr a~gdatu a~gjahr a~prart a~scope
         a~usr00 a~usr01 a~usr02 a~usr03 a~usr09

      into corresponding fields of table it_row_tab
      from imak as a inner join imakt as b
      on  b~posnr eq a~posnr
      and b~spras eq sy-langu
      where a~posnr  in s_posnr
        and a~gjahr    = p_gjahr
        and a~stratflg in r_stratflg
        and a~ivart  in s_ivart
        and a~werks  in s_werks
        and a~vkostl in s_vkostl.

* select prev years appr.req which is not yet closed.
  if p_abp = 'X' and p_oldyr = 'X'.
    select a~posnr a~stratflg
           b~txt50 a~ivart a~waers a~ernam a~erdat
           a~aenam a~aedat a~vkokrs a~abukrs a~vbukrs a~werks a~sizecl
           a~wdatu a~priori a~vkostl a~auart a~profidproj
           a~pspnr a~gdatu a~gjahr a~prart a~scope
           a~usr00 a~usr01 a~usr02 a~usr03 a~usr09

        into corresponding fields of it_row_tab
        from imak as a inner join imakt as b
        on  b~posnr eq a~posnr
        and b~spras eq sy-langu
        where a~posnr    in s_posnr
          and a~posnr    like 'P%'
          and a~gjahr    < p_gjahr
          and a~ivart  in s_ivart
*          and a~STRATFLG = p_abp  "need to include all previous requests
          and a~werks  in s_werks
          and a~vkostl in s_vkostl.

      perform get_ar_status using    it_row_tab-posnr
                            changing it_row_tab-flag1
                                     it_row_tab-status
                                     it_row_tab-status_desc.

* exclude locked AR - completed
      if it_row_tab-flag1 ca 'JDP'.
      else.
        append it_row_tab.
      endif.
    endselect.
  endif.


* store CBO if appr.req exist in SAP only
  select * into table gt_cbowbs from ztfiu131
     where gjahr    = p_gjahr
       and stratflg = p_abp
       and posnr    in s_posnr.

  sort gt_cbowbs by posnr.
  loop at it_row_tab.
    read table gt_cbowbs with key posnr = it_row_tab-posnr binary search.
    if sy-subrc <> 0.
      move-corresponding it_row_tab to ztfiu131.
      ztfiu131-zuser = sy-uname.
      ztfiu131-zsdat = sy-datum.
      ztfiu131-zstim = sy-uzeit.
      modify ztfiu131.
    endif.
  endloop.


  sort it_row_tab by posnr.
* get CBO data without SAP appr.req
  loop at gt_cbowbs.
    read table it_row_tab with key posnr = gt_cbowbs-posnr binary search.
    if sy-subrc <> 0.
      move-corresponding gt_cbowbs to it_row_tab.
      append it_row_tab.
    endif.
  endloop.


  if p_sync = 'X'.
    perform sync_ztfiu131.
  endif.


endform.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  sync_ztfiu131
*&---------------------------------------------------------------------*
form sync_ztfiu131 .
  data  : lt_ar_new type table of ty_row_tab with header line.
  data  : $ix   like sy-tabix.
  data  : $updt like imak-aedat.

  loop at gt_cbowbs.
    $ix = sy-tabix.

* check if CBO is outdated
    select single aedat into $updt from imak
       where posnr = gt_cbowbs-posnr.

    if sy-subrc = 0 and $updt > gt_cbowbs-aedat.
      delete gt_cbowbs index $ix.

      select a~posnr b~txt50 a~ivart a~waers a~ernam a~erdat
             a~aenam a~aedat a~vkokrs a~abukrs a~vbukrs a~werks a~sizecl
             a~wdatu a~priori a~vkostl a~auart a~profidproj
             a~pspnr a~gdatu a~gjahr a~prart a~scope
             a~usr00 a~usr01 a~usr02 a~usr03 a~usr09

          appending corresponding fields of table lt_ar_new
          from imak as a inner join imakt as b
          on  b~posnr eq a~posnr
          and b~spras eq sy-langu
          where a~posnr  = gt_cbowbs-posnr.

    endif.

  endloop.

  loop at lt_ar_new.
    move-corresponding lt_ar_new to gt_cbowbs.
    append gt_cbowbs.

    move-corresponding lt_ar_new to ztfiu131.

    ztfiu131-zuser = lt_ar_new-aenam.
    ztfiu131-zsdat = lt_ar_new-aedat.

    modify ztfiu131.
  endloop.


endform.                    " sync_ztfiu131
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_ATT
*&---------------------------------------------------------------------*
form get_data_att .
  data: l_idx like sy-tabix,
         $amt like ztfiu131-curamt,
         nxt1 type gjahr,
         nxt2 type gjahr,
         nxt3 type gjahr.
  nxt1 = p_gjahr + 1.
  nxt2 = p_gjahr + 2.
  nxt3 = p_gjahr + 3.

  loop at it_row_tab.
    l_idx = sy-tabix.

* monthly plan amount
    select sum( tot ) into it_row_tab-monamt
      from ztfiu132
      where posnr = it_row_tab-posnr
        and ayear = p_gjahr
        and VERSI = p_mvers.
    modify it_row_tab index l_idx transporting monamt.

* check other fields
    select single akostl into it_row_tab-akostl
         from imakpa where posnr eq it_row_tab-posnr.

    select single versi into it_row_tab-versi
         from imavz where gjahr eq p_gjahr "it_row_tab-gjahr
                      and posnr eq it_row_tab-posnr
                      and versi = p_mvers.
    if sy-subrc ne 0.
      if p_wovr eq true.
      else.
        continue.
      endif.
    else.
      if p_wovr eq true.
        continue.
      endif.
    endif.

    select single varnt into it_row_tab-varnt from imavz
              where gjahr eq p_gjahr "it_row_tab-gjahr
               and  posnr eq it_row_tab-posnr
               and  versi eq it_row_tab-versi.

    if sy-subrc eq 0.
      select single * from imav
           where posnr eq it_row_tab-posnr
             and varnt eq it_row_tab-varnt.

      if sy-subrc eq 0.
        it_row_tab-objnr = imav-objnr.
        select single parnr into it_row_tab-parnr_vera from ihpa
                          where objnr eq imav-objnr.

*FIXME
        select wtjhr into $amt
               from bpja
               where lednr eq '0001'
                 and objnr eq imav-objnr
                 and ( wrttp eq '39' or wrttp eq '40').
          if bpja-gjahr < p_gjahr.
            it_row_tab-prvamt = it_row_tab-prvamt + $amt.
          elseif bpja-gjahr = p_gjahr.
            it_row_tab-curamt = it_row_tab-curamt + $amt.
          elseif bpja-gjahr = nxt1.
            it_row_tab-nxtamt1 = it_row_tab-nxtamt1 + $amt.
          elseif bpja-gjahr = nxt2.
            it_row_tab-nxtamt2 = it_row_tab-nxtamt2 + $amt.
          elseif bpja-gjahr = nxt3.
            it_row_tab-nxtamt3 = it_row_tab-nxtamt3 + $amt.
          else.
            it_row_tab-nxtamt4 = it_row_tab-nxtamt4 + $amt.
          endif.
        endselect.

        select single b~anlkl b~aktiv
             into (it_row_tab-anlkl,
                   it_row_tab-aktiv)
                  from anib as a
           inner join ania as b
             on  b~objnr eq a~objnr
             and b~lfdnr eq a~lfdnr
           where a~objnr eq imav-objnr
             and a~afabe eq '20'.

      endif.

    endif.

    modify it_row_tab index l_idx.
  endloop.

endform.                    " GET_DATA_ATT
