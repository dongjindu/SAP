************************************************************************
* Program Name      : ZAFIU135
* Author            : IG.Moon
* Creation Date     : 9/15/2009
* Specifications By : Michael Yoon
* Description       : Maintain the AR master for Investment
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
report zafiu135 message-id zmco.
include zacoui00.
include <icon>.                        " icon
field-symbols: <fs> type table.    " Output table

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*

* Tables
tables : ztfiu131,  "to save appr.req - temporary before final creation

         ztim001,ztim002,ztim003,sscrfields,imak,
         bpja,imavz,imav.

types: begin of ty_row_tab.
        include structure zsaru131.
*TYPES:
*       msg(30),
*       chk(1).
types: end of ty_row_tab.

types: begin of ty_out.
include  type ty_row_tab.
types   celltab  type lvc_t_styl.
types   tabcolor type slis_t_specialcol_alv.
types: end of ty_out.

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

data : begin of it_f4 occurs 0,
       usr02 like taif9t-usr02,
       txt50 like taif9t-txt50,
      end of it_f4.

data g_line type i.

data :  gt_return  like ddshretval occurs 0 with header line.


* ////////////////////////////////////////////////////////////////////

data: g_error(1),g_import,
      g_repid  like sy-repid,
      g_ix     like sy-tabix,
      gv_index like sy-tabix.
data  : it_row_tab type table of ty_row_tab with header line,
        it_row_tab_org type table of ty_row_tab with header line,
        gt_out     type table of ty_out     with header line,
        gt_temp    type table of ty_out     with header line.

data  $gt_out like gt_out occurs 0 with header line.
data: it_status like bapiappreqstatus occurs 0 with header line.
data: it_user_status like bapiappreqstatus occurs 0 with header line.
data: it_varnt_status like bapiappreqstatusvarnt occurs 0
        with header line.
data: it_varnt_user_status like bapiappreqstatusvarnt occurs 0 with
        header line.

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
              varnt   type ima_varnt,
              amount  type dmbtr,
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

    methods : handle_onf4 for event onf4 of cl_gui_alv_grid
              importing sender
                e_fieldname
                e_fieldvalue
                es_row_no
                er_event_data
                et_bad_cells
                e_display.

  private section.
    data deleted_rows type standard table of ztfiu131.

* This flag is set if any error occured in one of the
* following methods:
    data: error_in_data type c.
    methods:
      update_delta_tables
         importing
            pr_data_changed type ref to cl_alv_changed_data_protocol.

    methods:
      get_cell_values
           importing
             row_id          type int4
             pr_data_changed type ref to cl_alv_changed_data_protocol
           exporting
             key             type ztfiu131_k.


endclass.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

* Setting for Change data
  method handle_data_changed.

* remember deleted lines for saving
    call method update_delta_tables( er_data_changed ).

    perform data_changed using er_data_changed.
  endmethod.                    " handle_data_changed

  method get_deleted_rows.
    deleted_rows = me->deleted_rows.
  endmethod.                    "get_deleted_rows

  method refresh_delta_tables.
    clear me->deleted_rows[].
  endmethod.                    "refresh_delta_tables

  method update_delta_tables.
    data: l_mod_row type lvc_s_moce,
          ls_ztfiu131 type ztfiu131,
          ls_outtab like line of gt_out,
          wa_outtab like line of gt_out,
          l_row_id like sy-tabix,
          ls_key type ztfiu131_k,
          l_ix type i.

    data: ls_mod_cells type lvc_s_modi.

    loop at pr_data_changed->mt_good_cells into l_mod_row .

      l_row_id = l_mod_row-row_id.
      assign pr_data_changed->mp_mod_rows->* to <fs>.
      loop at <fs> into ls_outtab.
*        ls_outtab-bukrs  = g_bukrs.
*        ls_outtab-zseri  = '0001'.
*        MODIFY <fs> FROM ls_outtab INDEX sy-tabix.
      endloop.

    endloop.

    loop at pr_data_changed->mt_good_cells into ls_mod_cells.endloop.

    if ls_mod_cells-fieldname ne 'POSNR'.
      loop at gt_out into wa_outtab.
        l_ix = sy-tabix.
        check wa_outtab-posnr eq ls_outtab-posnr.
        move-corresponding wa_outtab to ls_key.
        wa_outtab = ls_outtab.
        move-corresponding ls_key to wa_outtab.
        modify gt_out index l_ix from wa_outtab.
      endloop.
    endif.

  endmethod.                    "update_delta_tables

  method handle_double_click.
    perform double_click using e_row
                               e_column
                               es_row_no.
  endmethod.                    " handle_double_click

  method get_cell_values.
* get values of key cells of row ROW_ID

*    CALL METHOD pr_data_changed->get_cell_value
*          EXPORTING
*                 i_row_id    = row_id
*                 i_fieldname = 'BUKRS'
*               IMPORTING
*                 e_value = key-bukrs.
*
*    IF sy-subrc NE 0.
*      MESSAGE i000(0k) WITH text-e02.
*    ENDIF.

  endmethod.                    "get_cell_values

*- On help f4 - Search Help
  method handle_onf4.
    perform pro_on_f4 using sender
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display.

    er_event_data->m_event_handled = 'X'.
  endmethod.                    "HANDLE_ONF4

endclass.                   " LCL_EVENT_RECEIVER Implementation

data g_event_receiver  type ref to lcl_event_receiver.

************************************************************************
data  : flag_data_changed,
        info(80).
data: begin of ftab occurs 10,
        fcode(6),
      end of ftab.
****************************** constants *******************************
constants:  false value ' ',
            true  value 'X'.

field-symbols : <month>.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
parameters p_test no-display.
parameters: p_bukrs like bkpf-bukrs memory id buk obligatory,
            p_pyear like imak-gjahr default sy-datum(4) obligatory,
            p_abp   type im_stratflg.

selection-screen begin of block bl with frame title text-001.

select-options : s_posnr for ztfiu131-posnr.
*                 s_gjahr for ztfiu131-gjahr.
select-options :
                 s_ivart for imak-ivart,
                 s_werks for imak-werks,
                 s_vkostl for imak-vkostl.
selection-screen end of block bl.


selection-screen begin of block b2 with frame title text-002.

selection-screen end of block b2.

*SELECTION-SCREEN BEGIN OF BLOCK view-result WITH FRAME TITLE text-t03.
*SELECTION-SCREEN PUSHBUTTON  1(40) timpr USER-COMMAND timpr.
*SELECTION-SCREEN END OF BLOCK view-result.

selection-screen begin of block b4 with frame title text-01s.
parameter p_vari type slis_vari modif id hid.
selection-screen end of block b4.

parameters  p_call no-display.

*--check
selection-screen begin of block b5 with frame title text-022.
parameters : p_chk1  as checkbox default 'X' modif id hid,
             p_chk2  as checkbox default 'X' modif id hid,
             p_chk3  as checkbox default 'X' modif id hid,
             p_chk4  as checkbox default 'X' modif id hid.
parameters: p_zimc type char01 no-display.
selection-screen end of block b5.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.

  if p_zimc is initial.
    get parameter id 'ZIMC' field p_zimc.
    set parameter id 'ZIMC' field ' '.
*    p_zimc = g_zimc.
  endif.


*  sy-title =   '[IM] Maintain AR Master '.

  perform button_.


at selection-screen output.

  if p_zimc eq 'X'.
    loop at screen.
      if screen-group1 = 'HID'.
        screen-active = '0'.
        modify screen.
        continue.
      endif.
    endloop.
  endif.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen.


  data l_answer.
  data $msg(50).
  data $date(10).

  case sscrfields-ucomm.
    when 'TIMPR'.
      clear g_error.

      check g_error eq false.
      g_import = true.
      perform import_.

      check g_error eq false.

      perform move_out.
      perform set_output .

  endcase.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.

*  CLEAR g_import.
*  PERFORM get_data.
*  CHECK g_error EQ false.
*
*  PERFORM move_out.
*  PERFORM set_output .

  clear g_error.

  check g_error eq false.
  g_import = true.
  perform import_.

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

*---Select Condition.
  refresh : gt_temp.
  clear   : gt_temp.

  if p_chk1 = 'X'.
    loop at gt_out where flag1 = 'C'.
      move-corresponding gt_out to gt_temp.
      append gt_temp.
      clear  gt_temp.
    endloop.
  endif.
  if p_chk2 = 'X'.
    loop at gt_out where flag1 = 'A'.
      move-corresponding gt_out to gt_temp.
      append gt_temp.
      clear  gt_temp.
    endloop.
  endif.
  if p_chk3 = 'X'.
    loop at gt_out where flag1 = 'F'.
      move-corresponding gt_out to gt_temp.
      append gt_temp.
      clear  gt_temp.
    endloop.
  endif.
  if p_chk4 = 'X'.
    loop at gt_out where flag1 = 'R'.
      move-corresponding gt_out to gt_temp.
      append gt_temp.
      clear  gt_temp.
    endloop.
  endif.

  refresh : gt_out.
  clear   : gt_out.
  gt_out[] = gt_temp[].

  perform apply_icon.

  sort gt_out by posnr versi varnt.

endform.                    " move_out
*&---------------------------------------------------------------------*
*&      Form  set_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_output.

  check : g_error is initial.
  clear flag_data_changed.
  call screen 100.

endform.                    " set_output
*&---------------------------------------------------------------------*
*&      Form  button_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form button_.
*  WRITE:
*          icon_import AS ICON TO timpr,
*         'Import Data from SAP AR table (IMAK)' TO timpr+4(36).

endform.                    " button_
*&---------------------------------------------------------------------*
*&      Form  import_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form import_.
  ranges: r_stratflg for imak-stratflg.

  data $ix type i.
  data $objnr type onr00.
  types: begin of itab_type,
          word(10),
        end   of itab_type.
  data itab type standard table of itab_type with
          non-unique default key initial size 5.
  data  ls_itab like line of itab.


  __cls it_row_tab.

  if p_abp = 'X'.
    r_stratflg-option = 'EQ'.
    r_stratflg-sign   = 'I'.
    r_stratflg-low = 'X'.
    append r_stratflg.
  endif.

  select a~posnr b~txt50 a~ivart a~waers a~ernam a~erdat
         a~aenam a~aedat a~vkokrs a~abukrs a~vbukrs a~werks a~sizecl
         a~wdatu a~priori a~vkostl a~auart a~profidproj
         a~pspnr a~gdatu a~gjahr a~prart a~scope a~usr02 a~usr09
         a~usr01 a~usr00 a~usr03
  into corresponding fields of it_row_tab
  from imak as a inner join imakt as b
  on  b~posnr eq a~posnr
  and b~spras eq sy-langu
  where a~posnr in s_posnr
    and a~gjahr = p_pyear
    and a~stratflg in r_stratflg
*    AND a~gjahr EQ p_pyear
    and a~ivart in s_ivart
    and a~werks in s_werks
    and a~vkostl in s_vkostl.

    select single akostl into it_row_tab-akostl
         from imakpa where posnr eq it_row_tab-posnr.

    select single izwek into it_row_tab-izwek
         from imakpi where posnr eq it_row_tab-posnr.

    append it_row_tab.
    clear it_row_tab.
  endselect.


endform.                    " import_
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_&1  text
*      -->P_&2  text
*----------------------------------------------------------------------*
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
*       text
*----------------------------------------------------------------------*
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
    call method g_grid->refresh_table_display.
  endif.
  __focus g_grid.
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
  set handler : g_event_receiver->handle_onf4         for g_grid.

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

  perform set_f4_field.

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
    append gs_fcat to gt_fcat.
  end-of-definition.

  data : $ix(3) type n,
         $mtxt(6).

  __catalog :
      ' ' 'ICON'       'St'                            4 'ICON' '',
      ' ' 'STATUS_DESC' 'Status'                      20 'CHAR' '',
      'X' 'POSNR'      'App. req.'                    12 'CHAR' '',
      'X' 'TXT50'      'Description'                  50 'CHAR' '',
      ' ' 'IVART'      'RqTyp'                         2 'CHAR' '',
      ' ' 'WERKS'	  'Plant'                         4 'CHAR' '',
      ' ' 'AKOSTL'    'Req. CCtr'                    10  'CHAR' '',
      ' ' 'VKOSTL'    'Res. CCtr'                    10  'CHAR' '',
*      ' ' 'ZINVT'    'Inv.Category'                  1 'CHAR' '',
      ' ' 'IZWEK'      'Inv.reason'                   2 'CHAR' '',
      ' ' 'SIZECL'    'Div'                         2 'CHAR' '',

      ' ' 'USR00'     'Asset Class'                 10 'CHAR' '',
      ' ' 'USR02'      'Objective / Category'       10 'CHAR' '',
      ' ' 'USR03'      'IM-Class'                   10 'CHAR' '',
      ' ' 'WDATU'	  'Impl.start'                    8 'DATS' '',
      ' ' 'USR09'      'End.Date'                      8 'DATS' '',
*      ' ' 'USR02'      'Source'                        1 'CHAR' '',
      ' ' 'PARNR_VERA' 'Applicant'                    12 'CHAR' '',
** On 06/05/13 by Furong
*      ' ' 'ZPJT'    'Prj.Type'                      2 'CHAR' '',
*      ' ' 'ZPOT'    'Prj.Object'                    3 'CHAR' '',
*      ' ' 'ZCOMP'    'Comp.flg'                      1 'CHAR' '',
** End on 06/05/13
*6/12/13 Andy
*      ' ' 'ZDIVISION'  'Division'                      2 'CHAR' '',
*      ' ' 'ZSTD1'      'Std.Cls1'                      2 'CHAR' '',
*      ' ' 'ZSTD2'      'Std.Cls2'                      4 'CHAR' '',
      ' ' 'GJAHR'	  'Appr.year'                     4 'NUMC' '',
      ' ' 'PRIORI'    'Priority'                      1 'CHAR' '',
      ' ' 'GDATU'	  'Pland. approval'               8  'DATS' '',
      ' ' 'MSG'        'remarks'                      50 'CHAR' ''.

  loop at gt_fcat into gs_fcat.
    gs_fcat-ref_table = 'ZSARU131'.
    gs_fcat-ref_field = gs_fieldcat-fieldname.

    if gs_fcat-fieldname = 'USR00'.
      gs_fcat-f4availabl = 'X'.     "F4
    endif.

    if gs_fcat-fieldname = 'USR02'.
      gs_fcat-f4availabl = 'X'.
    endif.

    if gs_fcat-fieldname = 'USR03'.
      gs_fcat-f4availabl = 'X'.
    endif.

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
      'TXT50'             ' ' 'X' '' 'X' ''.

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
        'POSNR'      '1' 0,
        'TXT50'      '1' 0,
        'IVART'      '3' 0,
        'WERKS'	 '3' 0,
        'AKOSTL'     '3' 0,
        'VKOSTL'     '3' 0,
*        'ZINVT'  '3' 0,
        'IZWEK'      '3' 0,
        'USR03'      '3' 0,
        'PRIORI'     '3' 0,
        'SIZECL'     '3' 0,
        'WDATU'	     '3' 0,
        'USR09'      '3' 0,
        'USR00'      '3' 0,
        'USR02'      '3' 0,
        'PARNR_VERA' '3' 0,
*        'ZPJT'   '3' 0,
*        'ZPOT'   '3' 0,
*        'ZCOMP'   '3' 0,
*        'ZDIVISION'  '3' 0,
*        'ZSTD1'      '3' 0,
*        'ZSTD2'      '3' 0,
        'GJAHR'	 '3' 0,
        'GDATU'	 '3' 0,
        'WDATU'      '3' 0.


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
        ls_celltab-fieldname cp 'STATUS*'.

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
*** 06/27/2013 T00306 Start
    ftab-fcode = 'UPDATE'.
    append ftab.
    ftab-fcode = 'DELETE'.
    append ftab.
  else.
    ftab-fcode = 'REJECT'.
    append ftab.
    ftab-fcode = 'C136'.
    append ftab.
*** 06/27/2013 T00306 Start
  endif.

  if p_zimc eq 'X'.
    ftab-fcode = 'C136'.
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
  data answer.

  clear : g_error.

  ok_code = sy-ucomm.
  clear sy-ucomm.
  case ok_code.

    when 'BACK' or 'CANC'.
*      IF flag_data_changed EQ true.
*        CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
*             EXPORTING
*                  textline1     = 'Data has not been saved yet.'
*                  textline2     = 'Do you want to continue anyway? '
*                  titel         = 'Confirmation'
*                  defaultoption = 'N'
*             IMPORTING
*                  answer        = answer.
*        CHECK answer EQ 'J'.
*      ENDIF.
      clear flag_data_changed.
      perform free_container.
      leave to screen 0.
    when 'EXIT'.
      leave program.

    when 'INST'.
      perform : insert_line,
                refresh_alv.

    when 'INS10'.
      do 10 times.
        perform   insert_line.
      enddo.
      perform   refresh_alv.

    when 'DELE'.
      perform : delete_line,
                refresh_alv.

    when 'SAVE'.
      perform : save_data,
                refresh_alv.
      __focus g_grid.

    when 'SWITCH'.
      if sy-dynnr eq '0100'.
*** 06/27/2013 T00306 Start
        perform check_changed_data.
*** 06/27/2013 T00306 End
        perform switch_edit_mode.
      endif.
      __focus g_grid.

    when 'C136'.
      if p_call eq true.
        leave program.
      else.
        submit zafiu136 via selection-screen
                         and return.

*        SUBMIT zafiu136 WITH s_posnr IN s_posnr
*                        WITH p_pyear EQ p_pyear
*                        WITH s_ivart IN s_ivart
*                        WITH p_call EQ true
*                        AND RETURN.
      endif.

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
      perform get_status.
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

    when 'DELETE'.
      perform delete_ar.
      perform update_gt_out.
      wait up to '1' seconds.
      perform get_status.
      perform apply_icon.
      perform refresh_alv.


  endcase.


endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  free_container
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*       text
*----------------------------------------------------------------------*
*      -->P_1394   text
*      -->P_1395   text
*      -->P_1396   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_data.

  data iztfiu131 like ztfiu131 occurs 0 with header line.
  data $cnt type i.
  data :$lines(20), $msg(40).

*  __cls gt_out.
*
*  perform get_selected_rows tables gt_out.
  loop at gt_out.
    gt_out-chk = true.
    modify gt_out transporting chk where posnr = gt_out-posnr.
  endloop.
*  __cls gt_out.
*  perform get_selected_rows tables $gt_out.
*
*  read table $gt_out index 1.
*  if sy-subrc ne 0.
*    message i000 with 'Please select at least 1 line'.
*    exit.
*  endif.

  loop at gt_out.

    move-corresponding gt_out to iztfiu131.

    if  iztfiu131-posnr is initial.
      message i000 with 'Please enter the AR Nubmer.'.
      exit.
    else.
      iztfiu131-gjahr = p_pyear.  "original planned year
      iztfiu131-zuser = sy-uname.
      iztfiu131-zsdat = sy-datum.
      iztfiu131-zstim = sy-uzeit.
      append iztfiu131.
    endif.

  endloop.

  describe table iztfiu131 lines $cnt.
  if $cnt > 0.
*---save to table ??
    modify ztfiu131 from table iztfiu131.
    commit work.
    write $cnt to $lines.
    concatenate 'Successfully Saved :' $lines into $msg.
    message s000 with $msg.
    clear flag_data_changed.
  endif.

endform.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form refresh_alv.
  __set_refresh_mode true.
  call method g_grid->refresh_table_display
    exporting
      is_stable = stable.

endform.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form switch_edit_mode.
  data answer.
  data l_line type i.

  if g_grid->is_ready_for_input( ) eq 0.
    call method g_grid->set_ready_for_input
      exporting
        i_ready_for_input = 1.

    perform info_text_set using true.
  else.
*    IF FLAG_DATA_CHANGED EQ TRUE.
*      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
*           EXPORTING
*                TEXTLINE1     = 'Data has not been saved yet.'
*                TEXTLINE2     = 'Do you want to continue anyway? '
*                TITEL         = 'Confirmation'
*                DEFAULTOPTION = 'N'
*           IMPORTING
*                ANSWER        = ANSWER.
*      CHECK ANSWER EQ 'J'.
*    ENDIF.
    clear flag_data_changed.
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
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
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
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  $GT_OUT                                                       *
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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

  loop at $gt_out.

    delete gt_out where posnr = $gt_out-posnr.

  endloop.

endform.                    " delete_line
*&---------------------------------------------------------------------*
*&      Form  insert_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form insert_line.
  clear gt_out.

  gt_out-waers = 'USD'.
  gt_out-ernam = sy-uname.
  gt_out-erdat = sy-datum.
  gt_out-aenam = sy-uname.
  gt_out-aedat = sy-datum.
  gt_out-vkokrs = 'H201'.
  gt_out-abukrs = 'H201'.
  gt_out-vbukrs = 'H201'.
  gt_out-priori = '1'.
  gt_out-profidproj = '0000002'.
*  gt_out-scope = 'IV'.
*  gt_out-zcomp = 'H'.
*  gt_out-sizecl = '1'.

  append gt_out.

endform.                    " insert_line
*&---------------------------------------------------------------------*
*&      Form  double_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
form double_click using  e_row     type lvc_s_row
                         e_column  type lvc_s_col
                         es_row_no type lvc_s_roid.
  data $ima_posid type ima_posid.

  clear gv_index.
  gv_index = e_row-index.

  read table gt_out index gv_index.

  if sy-subrc eq 0.

    $ima_posid =  gt_out-posnr.

    set parameter id : 'GJR'  field gt_out-gjahr,
                       'IAG'  field $ima_posid, "gt_out-posnr,
                       'IAA'  field gt_out-ivart,
                       'KOS'  field gt_out-vkostl,
                       'IAF'  field $ima_posid.


    call transaction 'IMA3N' and skip first screen.
  endif.

  call method cl_gui_control=>set_focus
    exporting
      control = g_grid.

endform.                    " DOUBLE_CLICK
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
  data $appreq like bapi_appreq_id-appreq.
  data $posnr like imak-posnr.
  data $flag.

  sort gt_out by posnr versi varnt.

*  clear : gt_out-flag1,
*          gt_out-status,
*          gt_out-status_desc.
*
* MODIFY gt_out TRANSPORTING flag1 status status_desc WHERE chk EQ false
*.

  loop at gt_out.

    $ix = sy-tabix.
    at new posnr.
      $flag = true.
    endat.
    check $flag = true.
    clear $flag.

    select single posnr into $posnr from imak
    where posnr eq gt_out-posnr.
    if sy-subrc eq 0.
      gt_out-flag1 = true.

      $appreq = gt_out-posnr.
      __cls : it_status, it_user_status,
              it_varnt_status, it_varnt_user_status.

      call function 'BAPI_APPREQUEST_GETSTATUS'
        exporting
          externalnumber              = $appreq
          language                    = sy-langu
        tables
          apprequest_status           = it_status
          apprequest_user_status      = it_user_status
          apprequestvarnt_status      = it_varnt_status
          apprequestvarnt_user_status = it_varnt_user_status.

*      LOOP AT it_varnt_status.
*
*        gt_out-flag1 = 'X'.
*        gt_out-status = it_varnt_status-status.
*        gt_out-status_desc = it_varnt_status-description.
*
*        SELECT * FROM imavz
*                   WHERE gjahr EQ gt_out-gjahr
*                    AND  posnr EQ gt_out-posnr
*                    AND  varnt EQ it_varnt_status-appreqvrnt.
*
*          IF sy-subrc EQ 0.
*            gt_out-status = 'ASSGN'.
*            gt_out-status_desc = 'Assigned'.
*          ELSE.
**          gt_out-flag1 = 'N'.
*          ENDIF.
*          gt_out-varnt = imavz-varnt.
*
*          MODIFY gt_out TRANSPORTING  varnt flag1 status status_desc
*                  WHERE posnr EQ gt_out-posnr.
**                    AND versi EQ imavz-versi.
*
*        ENDSELECT.
*
*      ENDLOOP.

      read table it_status index 1.

      case it_status-text.
        when 'CRTD'.
          gt_out-flag1 = 'C'.
        when 'FAPP'.
          gt_out-flag1 = 'A'.
        when 'REJD'.
          gt_out-flag1 = 'R'.
        when 'APRV'.
          gt_out-flag1 = 'F'.
      endcase.

      gt_out-status = it_status-status.
      gt_out-status_desc = it_status-description.

      modify gt_out transporting  flag1 status status_desc
              where posnr eq gt_out-posnr.


    endif.

  endloop.


endform.                    " get_status
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

      __cls i_msg.

      call function 'BAPI_APPREQUEST_CREATE'
        exporting
          appropriationrequest_in        = aposnr
          apprequest_type                = $gt_out-ivart
          controlling_area               = $gt_out-vkokrs
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

      if $gt_out-flag1 ne 'E'.
        call function 'BAPI_TRANSACTION_COMMIT'
          importing
            return = i_msg.

*-      SAP object created, clean up WIP table
*        delete from ztfiu131 where posnr = $gt_out-posnr.
      endif.



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
  wa_master-req_comp_code   = $gt_out-abukrs.
  wa_master-rsp_comp_code   = $gt_out-vbukrs.
  wa_master-plant           = $gt_out-werks.
  wa_master-scale           = $gt_out-sizecl.  "WBS-division
  wa_master-priority        = $gt_out-priori.
  wa_master-rsp_cost_center = $gt_out-vkostl.
  wa_master-orig_appr_year  = $gt_out-gjahr.
  wa_master-desired_start   = $gt_out-wdatu.
  wa_master-objectclass     = $gt_out-scope.
  wa_master-object_currency = $gt_out-waers.
  wa_master-strat_indicator = $gt_out-stratflg.

*--User_fields
  wa_user_fields-user00     =  $gt_out-usr00.

*6/12/13 - Andy
  wa_user_fields-user02     =  $gt_out-usr02. "project obj

  wa_user_fields-user03      =  $gt_out-usr03. " Class
  wa_user_fields-user09_date =  $gt_out-usr09. "

*  wa_user_fields-user01     =  $gt_out-zpot.

*6/12/13 - Andy
*  perform make_user01 using wa_user_fields-user01.

*--Variant
  wa_variant-description        = 'Initial Plan'.
  wa_variant-assessmnt_crit     =  '0001' ." bwert.
  wk_date = $gt_out-gjahr + 1.
  concatenate wk_date '0101' into wk_date1.
  wa_variant-value_date   =  wk_date1.

*--PLAN TOTAL
*  wa_plan_total-overhead_costs   = $gt_out-amount.
  wa_plan_total-investment_costs = $gt_out-amount.

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
  move $gt_out-gjahr  to it_variant-appr_year.
*  MOVE $gt_out-versi   TO it_variant-plan_version.
  move 'IM' to it_variant-plan_version.
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
*  DO 9 TIMES.
  do 1 times.
    if sy-index = 1.
      move wk_date to it_plan_year-fiscal_year.
*      MOVE $gt_out-amount TO it_plan_year-overhead_costs.
      move $gt_out-amount to it_plan_year-investment_costs.
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
*&---------------------------------------------------------------------*
*&      Form  update_gt_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_gt_out.
  data $ix type i.

  loop at $gt_out.
    read table gt_out with key posnr = $gt_out-posnr
                               versi = $gt_out-versi.
    if sy-subrc eq 0.
      $ix = sy-tabix.
      gt_out-msg = $gt_out-msg.
      gt_out-flag1 = $gt_out-flag1.
      gt_out-status = $gt_out-status.
      gt_out-status_desc = $gt_out-status_desc.
      modify gt_out index $ix transporting status status_desc msg flag1.
    endif.
  endloop.
endform.                    " update_gt_out
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

      wa_master_x-req_txt_x =
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

      wa_user_fields_x-user00_x     =  true.
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
      select single * from imak where posnr eq $gt_out-posnr.
      if sy-subrc ne 0.
        message i000 with $gt_out-posnr ' has not been created!'.
        continue.
      endif.

      $ima_posid =  $gt_out-posnr.

      set parameter id 'IAF'  field $ima_posid.

      call transaction 'IMA2N' and skip first screen .

    endloop.

  endif.


endform.                    " apprv_ar
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

*      IF $gt_out-flag1 NE true.
*        MESSAGE i000 WITH $gt_out-posnr ' has not been created!'.
*        CONTINUE.
*      ENDIF.

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
        $gt_out-status = 'DELE'.
        $gt_out-status_desc = 'Deleted'.
      endif.

      modify $gt_out index $ix transporting status status_desc msg flag1
        .

    endloop.

  endif.

endform.                    " delete_ar
*&---------------------------------------------------------------------*
*&      Form  make_user01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM make_user01 using p_user01.
*
*  concatenate $gt_out-zpjt '^' $gt_out-zpot '^' $gt_out-zcomp '^'
*  $gt_out-zinvt '^' $gt_out-zdivision '^' $gt_out-zstd1 '^'
*  $gt_out-zstd2 into p_user01.
*
*ENDFORM.                    " make_user01
*&---------------------------------------------------------------------*
*&      Form  PRO_ON_F4
*&---------------------------------------------------------------------*
form pro_on_f4  using sender
                      e_fieldname
                      e_fieldvalue
                      es_row_no structure lvc_s_roid
                      er_event_data
                      et_bad_cells
                      e_display.

  data : selectfield   like  help_info-fieldname,
        it_fields     like  help_value occurs 1 with header line,
        select_value  like  help_info-fldvalue,
        ld_tabix      like  sy-tabix,
        ls_modi       type  lvc_s_modi,
        l_matnr       like  mara-matnr.

  field-symbols : <f4tab> type lvc_t_modi.
*  ASSIGN er_event_data->m_data->* TO <f4tab>.

  if  e_fieldname = 'USR00' or e_fieldname = 'USR02'
                            or e_fieldname = 'USR03'.

    if e_fieldname = 'USR02'.
      select *  into corresponding fields of table it_f4
      from taif9t
      where spras = sy-langu
        and usrnr = '1'.
    else.
      select *  into corresponding fields of table it_f4
      from taif9t
      where spras = sy-langu
        and usrnr = '2'.

      loop at it_f4.
        if e_fieldname = 'USR00'.
          if it_f4-usr02 > '9999'.
            delete it_f4.
          endif.
        else.
          if it_f4-usr02 <= '9999'.
            delete it_f4.
          endif.
        endif.
      endloop.
    endif.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'USR02'
        value_org       = 'S'
      tables
        value_tab       = it_f4
        return_tab      = gt_return
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.

    read table gt_return into gt_return index 1.
    if sy-subrc = 0.
*      ls_modi-row_id    = es_row_no-row_id.
*      ls_modi-fieldname = 'USR00'.
*      ls_modi-value     = gt_return-fieldval.
      if e_fieldname = 'USR00'.
        gt_out-usr00  = gt_return-fieldval.
        modify gt_out from gt_out index es_row_no-row_id
                                              transporting usr00.
      elseif e_fieldname = 'USR03'.
        gt_out-usr03  = gt_return-fieldval.
        modify gt_out from gt_out index es_row_no-row_id
                                              transporting usr03.
      elseif e_fieldname = 'USR02'.
        gt_out-usr02  = gt_return-fieldval.
        modify gt_out from gt_out index es_row_no-row_id
                                              transporting usr02.
      endif.

*      APPEND ls_modi TO <f4tab>.
    endif.

  endif.

endform.                    " PRO_ON_F4
*&---------------------------------------------------------------------*
*&      Form  SET_F4_FIELD
*&---------------------------------------------------------------------*
form set_f4_field .
  clear : gt_f4, gt_f4[].
*-- F4 FIELD register
*-- [Caution]- register with abc order
  gs_f4-fieldname  = 'USR00'.
  gs_f4-register   = 'X'.
  append gs_f4 to gt_f4.

  gs_f4-fieldname  = 'USR02'.
  gs_f4-register   = 'X'.
  append gs_f4 to gt_f4.

  gs_f4-fieldname  = 'USR03'.
  gs_f4-register   = 'X'.
  append gs_f4 to gt_f4.

  call method g_grid->register_f4_for_fields
    exporting
      it_f4 = gt_f4.
endform.                    " SET_F4_FIELD
*&---------------------------------------------------------------------*
*&      Form  CHECK_CHANGED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_changed_data.

  data l_valid type char01.
  data: l_error,
        l_answer.
  data: l_line type i,
        l_index type i.

  if g_grid->is_ready_for_input( ) eq 0.
    describe table gt_out lines g_line.
  else.
    describe table gt_out lines l_line.
    if l_line ne g_line.
      perform popup_to_confirm using 'SAVE' changing l_answer.
      if l_answer = '1'.
        perform : save_data,
                  refresh_alv.
        __focus g_grid.
      else.
        l_line = l_line - g_line.
        do l_line times.
          l_index = g_line + sy-index.
          delete gt_out index l_index.
        enddo.
        perform refresh_alv.
      endif.
    endif.
  endif.

  call method g_grid->check_changed_data
    importing
      e_valid = l_valid.

endform.                    " CHECK_CHANGED_DATA
*&---------------------------------------------------------------------*
*&      Form  popup_to_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_ANSWER   text
*----------------------------------------------------------------------*
form popup_to_confirm using    p_ucomm
                      changing p_answer.

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar              = 'Confirmation'
      text_question         = 'Do you want to save the data?'
      text_button_1         = 'Yes'
      icon_button_1         = ' '
      text_button_2         = 'No'
      icon_button_2         = ' '
      default_button        = '2'
      display_cancel_button = ' '
    importing
      answer                = p_answer
    exceptions
      text_not_found        = 1
      others                = 2.

endform.                    "popup_to_confirm
