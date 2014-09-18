************************************************************************
* Program Name      : ZACOU143 ( Copy Plan )
* Author            : IG.Moon
* Creation Date     : 4/10/2008
* Specifications By : Andy Choi
* Description       : For NAFTA purpose, overhead cost need to be
*                     calculated based on NAFTA criteria.
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
report zacou143 message-id zmco.
include zacoui00.

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
*Type-Pools
type-pools: kcde.

* Tables
tables : csks, coss, cosp, keko, tckh4 .

types: begin of ty_co.
        include structure zsco142.
types: end of ty_co.

types: begin of ty_row_tab.
        include structure zsco142.
types:
       icon type icon-id,
       msg(30),
       chk(1).
types: end of ty_row_tab.

types: begin of ty_out.
include  type ty_row_tab.
types   celltab  type lvc_t_styl.
types   tabcolor type slis_t_specialcol_alv.
types: end of ty_out.


* For BAPI FM
data : it_return         like standard table of bapiret2
                         with header line.

data : gt_return         like standard table of bapiret2
                         with header line.

data : wa_headerinfo     like bapiplnhdr.
data : begin of it_headerinfo occurs 0.
data :  key(70).
        include structure bapiplnhdr.
data : end of   it_headerinfo.
data : it_indexstructure like standard table of bapiacpstru
                         with header line.
data : it_coobject       like standard table of bapipcpobj
                         with header line.
data : it_pervalue       like standard table of bapipcpval
                         with header line.
data : it_totvalue       like standard table of bapipcptot
                         with header line.

data : it_contrl         like standard table of bapipcpctrl
                         with header line.

data : it_zsco_kp06_excel like standard table of zsco_kp06_excel
                          with header line .

data : begin of it_post  occurs 0.
data :  key(70).
        include structure  it_zsco_kp06_excel.
data : end of   it_post.

data : gv_delta, g_curk type waers.

data: g_error(1),
      g_repid  like sy-repid,
      g_ix     like sy-tabix,
      c_versn like coss-versn value '000'.

data  :  p_tvers like keko-tvers,
         p_elehk like tckh4-elehk .

data  : it_row_tab type table of ty_row_tab with header line,
        gt_out     type table of ty_out     with header line,
        gt_coss    type table of ty_co      with header line,
        gt_cosp    type table of ty_co      with header line.


data  $gt_out like gt_out occurs 0 with header line.

define __process.
  perform show_progress using &1 &2.
end-of-definition.
define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.
define __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
end-of-definition.
*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
class lcl_event_receiver definition.
  public section.

    types: begin of ztcou131_k,
              co_area   type kokrs,
              fisc_year type gjahr,
              version   type versn,
              kostl     type kostl,
              kstar     type kstar,
           end of ztcou131_k.

    types: ztcou131_key   type standard table of ztcou131_k,
           ztcou131_table type standard table of ztcou131.

    methods:
      handle_data_changed
         for event data_changed of cl_gui_alv_grid
             importing er_data_changed,
                       get_deleted_rows
             exporting
                       deleted_rows type ztcou131_table,

      refresh_delta_tables.

  private section.
    data deleted_rows type standard table of ztcou131.

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
    call method update_delta_tables( er_data_changed ).

    perform data_changed using er_data_changed.
  endmethod.                    " handle_data_changed

  method get_deleted_rows.
    deleted_rows = me->deleted_rows.
  endmethod.

  method refresh_delta_tables.
    clear me->deleted_rows[].
  endmethod.

  method update_delta_tables.
    data: l_del_row type lvc_s_moce,
          ls_ztcou131 type ztcou131,
          ls_outtab like line of gt_out.

    loop at pr_data_changed->mt_deleted_rows into l_del_row.
      read table gt_out into ls_outtab index l_del_row-row_id.
      if sy-subrc ne 0.
        message i000(0k) with text-e01. "Internal error
      else.
        move-corresponding ls_outtab to ls_ztcou131.
        append ls_ztcou131 to deleted_rows.
      endif.
    endloop.
  endmethod.

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
selection-screen begin of block bl1 with frame title text-001.

parameters : p_kokrs like tka01-kokrs obligatory memory id cac,
             p_year  type bdatj obligatory memory id bdtj,
             p_poper  type poper obligatory memory id popr,
             p_versn like cosl-versn default '600' no-display.

select-options s_kostl for csks-kostl.
select-options s_kstar for coss-kstar.

selection-screen end of block bl1.

*selection-screen begin of block bls with frame title text-003.
*parameters : p_upd   radiobutton group ra01,
*             p_dlt   radiobutton group ra01.
parameters : p_trun as checkbox.
*selection-screen end of block bls.

* Layout
selection-screen begin of block b4 with frame title text-01s.
parameter p_vari type slis_vari.
selection-screen end of block b4.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.
  p_tvers  = '04'.
  p_elehk = 'H1'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen output.
  perform modify_screen.

at selection-screen on value-request for p_vari.
  perform alv_variant_f4 changing p_vari.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.

  perform validate.
  check g_error eq false.

  perform get_data.
  check g_error eq false.

  perform move_out.
  perform set_output .

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.

* Log.
*  PERFORM DISPLAY_LOG.


*&---------------------------------------------------------------------*
*&      Form  POST_PL_CCTR_AT_CE
*&---------------------------------------------------------------------*
*       POST PLAN DATA using BAPI FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_pl_cctr_at_ce.
  sort it_post by key
                  costcenter
                  acttype
                  cost_elem.

  clear : it_headerinfo, it_headerinfo[].

  data $flag.

** Header Part / Init Data Containers
  loop at it_post.
    at new key.
      $flag = true.
    endat.
    check $flag eq true.
    clear $flag.
    perform fill_header_data.
    clear it_post.
  endloop.

  data $subrc.

  __cls gt_return.
** Fill Object List and Plan Values per Period
  loop at it_headerinfo.
*   Clear Str.
    perform clear_bapi_str.
*   Set Header Info.
    move-corresponding it_headerinfo to wa_headerinfo.
*   Set Value/Object
    loop at it_post where key = it_headerinfo-key.
*   Obj
      on change of  it_post-key
                or  it_post-costcenter
                or  it_post-acttype .
*     Index of Object Key
        it_indexstructure-object_index
             = it_indexstructure-object_index + 1 .

        clear it_coobject.
        it_coobject-object_index = it_indexstructure-object_index.

        it_coobject-costcenter   = it_post-costcenter.
        it_coobject-acttype      = it_post-acttype.
        append it_coobject.
        clear  it_coobject.
      endon.

*   Value.
*     Index of Value
      it_indexstructure-value_index
           = it_indexstructure-value_index + 1.

      clear it_pervalue.
      it_pervalue-value_index = it_indexstructure-value_index.
      it_pervalue-cost_elem   = it_post-cost_elem.
*     Set Value( Fixed  AMT. )
      perform set_fixed_val.
*     Append PerValues
      append it_pervalue.
      clear  it_pervalue.

*     Append Index
      append it_indexstructure.
      clear it_post.
    endloop.

** Call BAPI FM
    clear $subrc.
    perform call_post_fm changing $subrc.
    clear it_headerinfo.

* Commit
    if p_trun = 'X'.
      message s009(zmco) with '- TEST RUN'.
    else.
      wait up to 5 seconds .
      commit work and wait.
      message s009(zmco) with '- POST'.
    endif.

  endloop.

endform.                    " POST_PL_CCTR_AT_CE

*&---------------------------------------------------------------------*
*&      Form  CLEAR_BAPI_STR
*&---------------------------------------------------------------------*
*       Clear Str. (For BAPI)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form clear_bapi_str.

  __cls :  it_indexstructure,it_coobject,it_pervalue,
           it_totvalue, it_contrl.

  clear wa_headerinfo.

endform.                    " CLEAR_BAPI_STR

*&---------------------------------------------------------------------*
*&      Form  FILL_HEADER_DATA
*&---------------------------------------------------------------------*
*       Fill Header DATA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fill_header_data.
  clear it_headerinfo.
  move-corresponding it_post to it_headerinfo.
  it_headerinfo-period_from  = it_post-period.
  it_headerinfo-period_to    = it_post-period.
  append it_headerinfo.
  clear  it_headerinfo.
endform.                    " FILL_HEADER_DATA

*&---------------------------------------------------------------------*
*&      Form  SET_FIXED_VAL
*&---------------------------------------------------------------------*
*       Set Value( Fixed  AMT. )
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fixed_val.

*  IT_PERVALUE-FIX_VAL_PER&1  =  IT_COSP-WKF0&1.
*  IT_HEADERINFO-PERIOD_FROM  = IT_POST-PERIOD.
*  IT_HEADERINFO-PERIOD_TO    = IT_POST-PERIOD.

  field-symbols: <fs1> type any.
  data : lv_fix(40).
  data : lv_cnt(2)  type n.

* Period Counter : Set From-Period .
  clear lv_cnt.
  lv_cnt = it_post-period.

  clear lv_fix.
  concatenate 'IT_PERVALUE-FIX_VAL_PER' lv_cnt
         into lv_fix.
  assign (lv_fix) to <fs1>.
  <fs1> = it_post-fix_val_perxx.

endform.                    " SET_FIXED_VAL

*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       POSTING
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_post_fm changing p_subrc.

  clear : it_return, it_return[].

  if p_trun eq true.
    call function 'BAPI_COSTACTPLN_CHECKPRIMCOST'
         exporting
              headerinfo     = wa_headerinfo
              delta          = gv_delta
         tables
              indexstructure = it_indexstructure
              coobject       = it_coobject
              pervalue       = it_pervalue
              totvalue       = it_totvalue
              contrl         = it_contrl
              return         = it_return.

  else.
    call function 'BAPI_COSTACTPLN_POSTPRIMCOST'
         exporting
              headerinfo     = wa_headerinfo
              delta          = gv_delta
         tables
              indexstructure = it_indexstructure
              coobject       = it_coobject
              pervalue       = it_pervalue
              totvalue       = it_totvalue
              contrl         = it_contrl
              return         = it_return.
  endif.

* Check error
  clear  it_return.
  append lines of it_return to gt_return.

  loop at it_return  where type ca 'E'.
    p_subrc = 'E'.
    message id     it_return-id
            type   it_return-type
            number it_return-number
            with   it_return-message_v1
                   it_return-message_v2
                   it_return-message_v3
                   it_return-message_v4.
    clear it_return.
  endloop.

endform.                    " CALL_POST_FM

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       Log.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_log.

  data : lv_execnt like sy-tfill.
  data : lv_poscnt like sy-tfill.

  describe table it_zsco_kp06_excel lines lv_execnt.
  describe table it_post            lines lv_poscnt.

  write : / text-011 , lv_execnt.
  skip 1.
  write : / text-012 , lv_poscnt.
  skip 1.
  write : / text-013.

endform.                    " DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form modify_screen.

  loop at screen.
    if screen-group1 = 'BRT'.
      screen-intensified = 1.
      modify screen.
    endif.
  endloop.

endform.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
form show_progress using    pf_text
                            value(pf_val).
  call function 'SAPGUI_PROGRESS_INDICATOR'
       exporting
            percentage = pf_val
            text       = pf_text.

endform.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  CHECK_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<MONTH>  text
*----------------------------------------------------------------------*
form check_num changing n_value.
  data num(12) value ' 0123456789.'.

  replace : '"' with '' into n_value,
            '"' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value.
  condense n_value no-gaps.
  if n_value cn num. n_value = 0. endif.
endform.                    " CHECK_NUM
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
                exporting i_row_id    = ls_mod_cells-row_id
                          i_fieldname = ls_mod_cells-fieldname
                          i_value     = ls_mod_cells-value.
    endif.
  endloop.

  __set_refresh_mode true.
  call method g_grid->refresh_table_display
       exporting is_stable = stable.

endform.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT
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
    gt_out-trans_curr = 'C'.

    concatenate sy-uname  sy-datum  sy-repid
           into gt_out-doc_hdr_tx
           separated by '/'.

    append gt_out.
  endloop.

endform.                    " MOVE_OUT
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
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

endform.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set titlebar '100'.
*   Exclude toolbar
  perform exclude_functions.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
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
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

endform.                    " EXCLUDE_FUNCTIONS
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
         exporting is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         changing  it_outtab            = gt_out[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].
  else.
    call method g_grid->refresh_table_display.
  endif.
  __focus g_grid.
  perform user_status.

endmodule.                 " DISPLAY_ALV_100  OUTPUT
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

    when 'SAVE'.
      check sy-dynnr eq '0100'.
      perform really?.
      check g_error ne true.

      perform : apply_cca,
                refresh_alv.
      __focus g_grid.

    when 'SWITCH'.
      if sy-dynnr eq '0100'.
        perform switch_edit_mode.
      endif.
      __focus g_grid.

    when 'LOGV'.
      call screen '300'.
  endcase.


endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FREE_CONTAINER
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
*&      Form  REALLY?
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
      'This will change the CCA data!'
      'Do you really want to proceed?' ' '
                 changing l_answer.

  if l_answer ne 'J'.
    g_error = true.
    message s000 with 'Processing was canceled by user.'.
  endif.
endform.                    " REALLY?
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1886   text
*      -->P_1887   text
*      -->P_1888   text
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
       exporting is_stable = stable.

endform.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
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
  set handler : g_event_receiver->handle_data_changed for g_grid.

*   Create field category
  perform create_field_category using false.

  call method g_grid->register_edit_event
       exporting i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  call method g_grid->set_ready_for_input
     exporting
            i_ready_for_input = 0.

  perform sort_build using gt_sort[].

*   Setting for layout
  perform set_lvc_layout.

*   Set colors
  perform set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

*   Define cell attribute
  perform build_cell_attr.


endform.                    " CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
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
    'X'  'KOSTL'             'Cost.Cc'            10 'CHAR' '',
    'X'  'KSTAR'             'Cost.El'            10 'CHAR' '',
    ' '  'DOC_HDR_TX'        'Doc.header text'    30 'CHAR' '',
    ' '  'TRANS_CURR'        'T.C'                5 'CHAR' ''.

  do 12 times.
    $ix = sy-index.
    check $ix eq p_poper.
    concatenate 'WKG' $ix into $mtxt.
    __catalog :
      ' '  $mtxt    'Amount'                15  'DEC' '' .
    concatenate 'DIS' $ix into $mtxt.
    __catalog :
      ' '  $mtxt    'Amt.Dst'                15  'DEC' '' .
  enddo.

*  __catalog :
*    ' '  'ICON'    'flg'                3  'ICON' '',
*    ' '  'MSG'     'remarks'           30  'CHAR' ''.

  loop at gt_fcat into gs_fcat.
    if gs_fcat-fieldname cp 'WKG*' or gs_fcat-fieldname cp 'DIS*'.
      gs_fcat-just = 'R'.
      gs_fcat-no_zero = 'X'.
      gs_fcat-decimals_o = '2'.
    endif.
    gs_fcat-ref_table = 'ZSCO142'.
    gs_fcat-ref_field = gs_fieldcat-fieldname.

    modify gt_fcat from gs_fcat.
  endloop.

endform.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
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
      'KOSTL'             ' ' 'X' '' 'X' '',
      'KSTAR'             ' ' 'X' '' 'X' ''.

endform.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
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
*&      Form  SET_COLOR
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
            'KOSTL'            '1' 0,
            'KSTAR'            '1' 0,
            'DOC_HDR_TX'       '1' 0,
            'TRANS_CURR'       '1' 0.

  do 12 times.
    $ix = sy-index.
    check $ix eq p_poper.
    concatenate 'WKG' $ix into $mtxt.
    __color :
             $mtxt             '4' 0.
    concatenate 'DIS' $ix into $mtxt.
    __color :
             $mtxt             '3' 0.
  enddo.

  __color :
            'ICON'    '1' 0,
            'MSG'     '1' 0.

  gt_out-tabcolor[] = gt_specialcol[].
  modify gt_out transporting tabcolor where tabcolor is initial.

endform.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
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
    if ls_celltab-fieldname cp 'DIS*'
      or ls_celltab-fieldname eq 'DOC_HDR_TX'
      or ls_celltab-fieldname eq 'TRANS_CURR'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    else.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
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
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form switch_edit_mode.
  data answer.
  if g_grid->is_ready_for_input( ) eq 0.
    call method g_grid->set_ready_for_input
                     exporting i_ready_for_input = 1.

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
                     exporting i_ready_for_input = 0.
    perform info_text_set using false.
  endif.

  perform build_cell_attr.
endform.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
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
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0300 output.
  set pf-status 'ZLOG'.
  sy-title = 'Error log...'.
  suppress dialog.
  leave to list-processing and return to screen 0.
  perform error_list.
endmodule.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ERROR_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form error_list.

  loop at gt_return.
    write:/ gt_return-message_v1(20),
            gt_return-message_v2(10),
            gt_return-message(40).
  endloop.

endform.                    " ERROR_LIST
*&---------------------------------------------------------------------*
*&      Form  user_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form user_status.
  __cls ftab.

  if g_grid->is_ready_for_input( ) eq 1.
    ftab-fcode = 'SAVE'.
    append ftab.
    ftab-fcode = 'LOGV'.
    append ftab.
  endif.

  set pf-status '100' excluding ftab.
endform.                    " user_status
*&---------------------------------------------------------------------*
*&      Form  APPLY_CCA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form apply_cca.

  __cls $gt_out.

  data: lt_row   type lvc_t_row,
        ls_row   type lvc_s_row,
        lt_roid  type lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  data  lt_messages      like messages       occurs 0 with header line.
  data  $messages      like messages       occurs 0 with header line.


  clear: lv_cnt, lt_row[], lt_roid[].

  perform get_selected_rows tables $gt_out.

* Preparation for posting
  perform pre_for_posting tables $gt_out.
  perform post_pl_cctr_at_ce.

endform.                    " APPLY_CCA
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
form get_selected_rows tables $gt_out structure gt_out.

  data: lt_rows type lvc_t_row with header line,
        lt_row_no type lvc_t_roid. "Numeric IDs of Selected Rows

  call method g_grid->get_selected_rows
           importing et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  call method cl_gui_cfw=>flush.

  if sy-subrc ne 0.
    message e000
    with 'Error founded during flushing of ALV Grid Control'.
    exit.
  endif.

  read table lt_rows index 1.
  if sy-subrc ne 0.
    $gt_out[] = gt_out[].
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
*&      Form  validate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form validate.
  clear g_error.
  select single waers into g_curk from
      tka01 where kokrs eq p_kokrs.
  if sy-subrc ne 0.
    message s000 with 'Please enter the valid CO-Area!'.
    g_error = true.
  endif.

endform.                    " validate
*&---------------------------------------------------------------------*
*&      Form  pre_for_posting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
form pre_for_posting tables p_tab structure gt_out.
  data l_no(3) type n.

  data l_filename(40).
  data : wkgxxx like  gt_out-wkg012.

* Collect Data
  __cls it_post.
  loop at p_tab.
    clear   it_post.
    it_post-co_area         = p_kokrs.
    it_post-fisc_year       = p_year     .
    it_post-version         = p_versn    .
    it_post-doc_hdr_tx      = p_tab-doc_hdr_tx    .
    it_post-plan_currtype   = p_tab-trans_curr .       " ?
    it_post-costcenter      = p_tab-kostl    .
    it_post-acttype         = p_tab-acttype .     " ?
    it_post-cost_elem       = p_tab-kstar     .
    it_post-trans_curr      = p_tab-trans_curr    .    " ?

    do 12 times varying wkgxxx from p_tab-dis001 next
                                    p_tab-dis002.
      l_no = sy-index.

      check l_no eq p_poper.

      it_post-fix_val_perxx   = wkgxxx .
      it_post-period          = l_no     .
      concatenate p_kokrs
                  p_year
                  it_post-period
                  p_versn
                  it_post-doc_hdr_tx
                  it_post-plan_currtype
             into it_post-key.
      collect it_post.
    enddo.
  endloop.

* Sorting
  clear   it_post.
  sort it_post by key.

endform.                    " PRE_FOR_POSTING
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.

  __cls : gt_coss,gt_cosp.

  select
        a~objnr
        a~kstar
        a~twaer
        a~meinh
        sum( a~wkg001 ) as wkg001
        sum( a~wkg002 ) as wkg002
        sum( a~wkg003 ) as wkg003
        sum( a~wkg004 ) as wkg004
        sum( a~wkg005 ) as wkg005
        sum( a~wkg006 ) as wkg006
        sum( a~wkg007 ) as wkg007
        sum( a~wkg008 ) as wkg008
        sum( a~wkg009 ) as wkg009
        sum( a~wkg010 ) as wkg010
        sum( a~wkg011 ) as wkg011
        sum( a~wkg012 ) as wkg012
  from cosp as a
  join cskb as b
  on b~kokrs eq p_kokrs
  and b~kstar eq a~kstar
  and b~datbi eq '99991231'
          into corresponding fields of table gt_cosp
           where a~lednr = '00'
             and a~objnr like 'KS%'
             and a~kstar in s_kstar
             and a~gjahr = p_year
             and a~wrttp = '01'
             and a~versn = '000'
             and b~katyp = '01'
             group by a~objnr a~kstar a~twaer a~meinh.

  select
        a~objnr
        a~kstar
        a~twaer
        a~meinh
        sum( a~wkg001 ) as wkg001
        sum( a~wkg002 ) as wkg002
        sum( a~wkg003 ) as wkg003
        sum( a~wkg004 ) as wkg004
        sum( a~wkg005 ) as wkg005
        sum( a~wkg006 ) as wkg006
        sum( a~wkg007 ) as wkg007
        sum( a~wkg008 ) as wkg008
        sum( a~wkg009 ) as wkg009
        sum( a~wkg010 ) as wkg010
        sum( a~wkg011 ) as wkg011
        sum( a~wkg012 ) as wkg012
  from coss as a
  join cskb as b
  on b~kokrs eq p_kokrs
  and b~kstar eq a~kstar
  and b~datbi eq '99991231'
          into corresponding fields of table gt_coss
           where a~lednr = '00'
             and a~objnr like 'KS%'
             and a~kstar in s_kstar
             and a~gjahr = p_year
             and a~wrttp = '01'
             and a~versn = '000'
             and b~katyp = '01'
             group by a~objnr a~kstar a~twaer a~meinh.

  __cls it_row_tab.
  loop at gt_cosp.
    move-corresponding gt_cosp to it_row_tab.
    append it_row_tab.
  endloop.
  loop at gt_coss.
    move-corresponding gt_coss to it_row_tab.
    append it_row_tab.
  endloop.

  perform collect_co_data tables it_row_tab.

  data $ix type i.
  data: begin of g_t_rate occurs 0.
          include structure zco140_gen.
  data: end of g_t_rate.

  sort it_row_tab by kostl kstar.

  field-symbols : <fs_wkg>,  <fs_dis> .
  data : f_wkg(30),f_dis(30).

  concatenate : 'IT_ROW_TAB-WKG' p_poper into f_wkg,
                'IT_ROW_TAB-DIS' p_poper into f_dis.

  assign : (f_wkg) to <fs_wkg>,
           (f_dis) to <fs_dis>.

  loop at it_row_tab.
    $ix = sy-tabix.
    if <fs_wkg> eq 0.
      delete it_row_tab index $ix.
      continue.
    endif.

    <fs_dis> = 0.

    if it_row_tab-kostl in s_kostl.
      modify it_row_tab index $ix.
    else.
      delete it_row_tab index $ix.
    endif.

  endloop.

endform.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  collect_co_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_COSP  text
*----------------------------------------------------------------------*
form collect_co_data tables   p_gt_co structure it_row_tab.

  data   $gt_co    type table of ty_row_tab      with header line.

  loop at p_gt_co.
    call function 'OBJECT_KEY_GET_KS'
         exporting
              objnr       = p_gt_co-objnr
         importing
              kokrs       = p_gt_co-kokrs
              kostl       = p_gt_co-kostl
         exceptions
              not_found   = 1
              wrong_obart = 2
              others      = 3.
    if sy-subrc <> 0.
    endif.
    $gt_co = p_gt_co.
    clear $gt_co-objnr.
    collect $gt_co.
  endloop.

  __cls p_gt_co.

  p_gt_co[] = $gt_co[].

endform.                    " collect_co_data
