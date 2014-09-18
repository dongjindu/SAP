*----------------------------------------------------------------------
* Program ID        : ZACOU123
* Title             : [CO] Duty YE Reconciliation
* Created on        : 05/02/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Duty YE Reconciliation
*----------------------------------------------------------------------
report zacou123_new message-id zmco.
tables: mkpf, lips.
include : zacoui00, zacou123_new_top.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
selection-screen begin of block b1 with frame title text-001.
parameters:   p_bukrs  type bukrs obligatory memory id buk,
              p_kschl  type kschl obligatory
              memory id vks default 'ZOA1'.
select-options : s_matnr  for ekpo-matnr modif id mat memory id mat,
                 s_ebeln  for ekpo-ebeln memory id bes.
parameters     : p_x_mat  radiobutton group selm
                          default 'X'  user-command ucom,
                 p_e_mat  radiobutton group selm,
                 p_o_mat  radiobutton group selm.
selection-screen skip.
selection-screen begin of line.
parameters     : p_all  radiobutton group rad default 'X' user-command chk.
selection-screen comment 2(7) text-017 for field p_all.
selection-screen position 12.
parameters     : p_pat  radiobutton group rad.
selection-screen comment 14(7) text-018 for field p_pat.
selection-screen end of line.
*select-options : s_mblnr for mkpf-mblnr no-extension no intervals
*                                           modif id chk memory id mat,
*                 s_lfimg for lips-lfimg modif id chk.
*parameters     : p_year  type numc4 modif id chk.
selection-screen end of block b1.

* Load saved data
selection-screen begin of block b0 with frame title text-011.
parameters p_load as checkbox user-command ucom modif id lod.
select-options s_date for sy-datum modif id lod.
selection-screen end of block b0.

selection-screen begin of block b2 with frame title text-002.
parameters p_date like sy-datum obligatory default sy-datum.
** Furong on 03/05/14 requested by Mr. Hong '
** to allow user to change posting date in any case
*modif id exl.
** end on 03/05/14

parameters p_max(3)  type n default 10 modif id exl.
selection-screen end of block b2.

selection-screen begin of block b3 with frame title text-005.

selection-screen begin of line.
selection-screen comment 35(12)  text-x00 for field p_exl
                                 modif id exl.
parameters p_exl   radiobutton group radi default 'X'
                                 modif id exl.
selection-screen comment 55(21) text-x01
                                 modif id exl.
parameters p_txt     radiobutton group radi
                                 modif id exl.
selection-screen end   of line.

parameters: p_file  like rlgrap-filename obligatory
                    default 'c:\temp\import.xls'
                    modif id exl.

selection-screen begin of line.
parameters p_head as checkbox modif id exl default 'X'.
selection-screen comment 3(25) text-t07 modif id exl.
selection-screen comment 33(42) text-t04 modif id exl.
selection-screen end of line.

selection-screen end   of block b3.

* Layout
selection-screen begin of block b4 with frame title text-010.
parameter p_vari type slis_vari.
selection-screen end of block b4.

selection-screen begin of block b5 with frame title text-022.
parameter p_mx_r type i default '50' obligatory.
parameters p_nega as checkbox default ''.
selection-screen end of block b5.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen .
  case sscrfields-ucomm.
    when 'UCOM'.
      perform modify_screen.
  endcase.

at selection-screen output.
  perform modify_screen.

at selection-screen on value-request for p_file.
  perform browser changing p_file.

at selection-screen on value-request for p_vari.
  perform alv_variant_f4 changing p_vari.

at selection-screen on value-request for p_kschl.
  perform kschl_input_help changing p_kschl.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
start-of-selection.

  if p_load ne true .
* verify the options
    perform check_select_value.
    if p_o_mat ne true.
*     Upload file
      perform upload_file using p_file.
    endif.
*     Gather Row Data
*    if p_pat eq 'X'.
*      if s_mblnr[] is initial.
*        message s000 with 'Material Doc No is required.' display like 'E'.
*        stop.
*      elseif s_lfimg[] is initial.
*        message s000 with 'Quantity is required.' display like 'E'.
*        stop.
*      elseif p_year is initial.
*        message s000 with 'Year is required.' display like 'E'.
*        stop.
*      endif.
*    endif.
    perform get_data.
  else.
*     Load saved data
    perform load_saved_data.
  endif.
*     error then exit !!!
  check g_error eq space .
*     Call ALV screen
  call screen 100.

end-of-selection.
* nothing

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
module status_0100 output.
  set titlebar '100'.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
module user_command_0100 input.
  ok_code = sy-ucomm.
  clear sy-ucomm.
  case ok_code.
    when 'BACK' or 'CANC'.
      leave to screen 0.
    when 'EXIT'.
      leave program.
    when 'POST'.
      perform post_data.
      perform save_data.
  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
form get_data.
  clear g_error.
  __process '40'.

  perform get_row_data.
  perform calc_data.
  perform get_gt_out.
  __process '98'. "98%

endform.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
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
*    GS_FCAT-DO_SUM        = &7.
    append gs_fcat to gt_fcat.
  end-of-definition.
  data $excel_header(13).

  if p_o_mat eq true. " Not use excel
    $excel_header = 'Qty.into    ' .
  else.
    $excel_header = 'Excel       ' .
  endif.

  if p_pat eq 'X'.
    __catalog : ' '  'STATUS'   'Status'   24  'CHAR' ''.
    __catalog : 'X'  'DESC'     'Delivered'   24  'CHAR' ''.
  endif.

  __catalog :
          'X'  'MATNR'   'Material'         18  'CHAR' ''.

  if p_pat eq 'X'.
    __catalog : 'X'  'MBLNR'   'Material Document No.' 10  'CHAR' ''.
*    __catalog : 'X'  'MBLNR_R' 'Reference Document No.' 10  'CHAR' ''.
    __catalog : 'X'  'XBLNR'   'Inbound Delivery No.' 10  'CHAR' ''.
  __catalog :
          'X'  'BUDAT'   'Posting Date'         10  'DATS' ''.
    endif.
  __catalog : 'X'  'XMENGES' $excel_header      13  'CHAR' '',
            'X'  'EBELN'   'P/O No.'          10  'CHAR' '',
            'X'  'EBELP'   'Item No.'          5  'CHAR' '',
            ' '  'MEINS'   'UOM'               5  'CHAR' '',
            ' '  'PMENGE'  'P/O Qty.   '      13  'QUAN' '',
            ' '  'GMENGE'  'G/R Qty.   '      13  'QUAN' '',
            ' '  'BMENGE'  'Balance    '      13  'QUAN' '',
            ' '  'RMENGE'  'Reconciling'      13  'QUAN' '',
            ' '  'AMENGE'  'After'            13  'QUAN' '',
            ' '  'UPRICE'  'U/P'              13  'CHAR' '',
            ' '  'DMBTR'   'Rec.Amt'           13  'CURR' '',
            ' '  'BALAMT'  'Bal.Amt'           13  'CURR' '',
            ' '  'ICON'    ' '                 4  'ICON' '',
            ' '  'BELUM'   'Acct.doc.#'       10  'CHAR' '',
            ' '  'MSG'  'Remarks                             .'
            30  'CHAR' ''.

  loop at gt_fcat into gs_fcat.
    case gs_fcat-fieldname.
      when 'STATUS'.
        gs_fcat-just = 'C'.
        modify gt_fcat from gs_fcat.
      when 'XMENGES'.
        gs_fcat-just = 'R'.
        modify gt_fcat from gs_fcat.
      when 'BMENGE' or 'RMENGE' or 'PMENGE' or 'AMENGE'
                    or 'GMENGE' .
        gs_fcat-qfieldname = 'MEINS'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-ref_table = 'EKPO'.
        gs_fcat-just = 'R'.
        modify gt_fcat from gs_fcat.
      when 'DMBTR' or 'UPRICE' or 'DMBTR' or 'BALAMT'.
        gs_fcat-qfieldname = 'WAERS'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-ref_table = 'EKBZ'.
        gs_fcat-just = 'R'.
        modify gt_fcat from gs_fcat.
    endcase.
  endloop.

endform.                    " CREATE_FIELD_CATEGORY

*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
form sort_build using ft_sort type lvc_t_sort.

  define sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  end-of-definition.

  sort_tab :
*             'BUKRS'    '1' 'X' 'X' 'X',
*             'KSCHL'    '2' 'X' 'X' 'X',
             'MATNR'    '1' 'X' 'X' 'X',
             'XMENGES'  '2' 'X' 'X' 'X',
             'EBELN'    '3' 'X' 'X' 'X'.
*             'ICON'     ' ' 'X' 'X' 'X',
  if p_load eq true.
    sort_tab :
               'BELUM'    ' ' 'X' 'X' 'X'.
  endif.

endform.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       Event of changed data
*----------------------------------------------------------------------*
*      -->RR_DATA_CHANGED  Log is Visible
*----------------------------------------------------------------------*
form data_changed using rr_data_changed
                        type ref to cl_alv_changed_data_protocol.

  data: ls_mod_cells type lvc_s_modi,
        ls_cells     type lvc_s_modi,
        lt_values type table of bapi_char_values with header line.

  __set_refresh_mode true.
  call method g_grid->refresh_table_display
    exporting
      is_stable = stable.

endform.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save data to table ZTCOU105
*----------------------------------------------------------------------*
form save_data.

  __cls i_ztcou123.

* delete saved data
  delete from ztcou123
    where     pdate        eq    p_date
      and     belum        eq    space
      and     bukrs        eq    p_bukrs
      and     kschl        eq    p_kschl
      and     matnr        in    s_matnr
      and     ebeln        in    s_ebeln.

*  DELETE FROM ZTCOU123 CLIENT SPECIFIED WHERE mandt EQ sy-mandt.

  loop at gt_out where chk eq true.
    move-corresponding gt_out to *ztcou123.
     *ztcou123-pdate = p_date.
     *ztcou123-erdat = sy-datum.
     *ztcou123-ernam = sy-uname.
    append *ztcou123 to i_ztcou123.
  endloop.

  modify ztcou123 from table i_ztcou123.
  if sy-subrc ne 0.
    rollback work.
  else.
    commit work.
  endif.

  perform refresh_alv.
  __focus g_grid.
endform.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
form set_lvc_layout.
  clear gs_layo.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-ctab_fname = 'COLINFO'.
  gs_layo-stylefname = 'HANDLE_STYLE'.

endform.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
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
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_gt_out.

  data: lt_cellcolor type lvc_t_scol,
        ls_cellcolor type lvc_s_scol.

  data: l_shkzg like ekbz-shkko.

  data: ls_out_temp like gt_out.

  data: ls_rbkp type rbkp,
        l_belum type belnr_d.

  data: begin of lt_cou123 occurs 0,
          pdate type dats,
          bukrs type bukrs,
          kschl type kschl,
          matnr type matnr,
          ebeln type ebeln,
          belum type belnr_d,
          mblnr type mblnr,
          icon  type icon_d,
        end of lt_cou123.

  if not gt_calc[] is initial.
    select bukrs pdate kschl matnr
           ebeln belum icon  mblnr
      into corresponding fields of table lt_cou123
      from ztcou123
       for all entries in gt_calc
     where bukrs = gt_calc-bukrs
       and kschl = gt_calc-kschl
       and matnr = gt_calc-matnr
       and ebeln = gt_calc-ebeln.
    sort lt_cou123 by bukrs kschl matnr belum descending.
  endif.

  __cls gt_out .
  loop at gt_calc.
    move-corresponding gt_calc to gt_out.
    gt_out-dmbtr = gt_out-uprice * gt_out-rmenge.
    gt_out-balamt = gt_out-uprice * gt_out-bmenge.

*** 12/02/2013 T00306 Start
    if p_all eq true.
      append gt_out. clear gt_out.
      continue.
    endif.

    gt_out-status = icon_red_light.
* Check if posted data
    read table lt_cou123 with key matnr = gt_out-matnr
                                  ebeln = gt_out-ebeln
                                  belum = gt_out-mblnr binary search.
    if sy-subrc eq 0 and
       lt_cou123-icon eq icon_led_green.
      gt_out-mblnr_r = lt_cou123-mblnr.
    endif.

    if gt_out-rmenge eq 0.
      gt_out-icon = icon_led_inactive.
    endif.

    clear ls_cellcolor.

    clear l_belum.
    read table lt_cou123 with key matnr = gt_out-matnr
                                  ebeln = gt_out-ebeln
                                  icon  = icon_led_green.

    if lt_cou123-mblnr is initial.
      l_belum = lt_cou123-belum.
    endif.

    case gt_calc-lichn.
      when 'Y'.
        gt_out-desc = 'From Korea'.
        gt_out-status = icon_yellow_light.

      when 'N'.
        gt_out-desc = 'From Non Korea'.
        gt_out-status = icon_yellow_light.
    endcase.

* Check if already posted
    if l_belum is initial.
      loop at lt_cou123 from sy-tabix where matnr = gt_out-matnr
                                        and ebeln = gt_out-ebeln
                                        and mblnr = gt_out-mblnr
                                        and icon = icon_led_green.
        if gt_out-mblnr is initial.
        elseif gt_out-mblnr ne lt_cou123-mblnr.
          continue.
        endif.
        l_belum = lt_cou123-belum.
        exit.
      endloop.
    endif.

    if not l_belum is initial.
      call function 'MRM_RBKP_SINGLE_READ'
        exporting
          i_belnr         = lt_cou123-belum
          i_gjahr         = lt_cou123-pdate+0(4)
        importing
          e_rbkp          = ls_rbkp
        exceptions
          entry_not_found = 1.

      if sy-subrc eq 0.
        if ls_rbkp-stblg is initial.
*          clear gt_out-balamt.
          case gt_calc-lichn.
            when 'Y'.
              gt_out-desc = 'From Korea(already posted)'.
              gt_out-status = icon_yellow_light.

            when 'N'.
              gt_out-desc = 'Non From Korea(already posted)'.
              gt_out-status = icon_yellow_light.
          endcase.
          gt_out-status = icon_green_light.
          gt_out-belum  = lt_cou123-belum.
          clear ls_cellcolor-color-col.
        endif.
      endif.
    endif.

    if gt_out-balamt is initial.
      clear l_shkzg.
      select single arewr shkzg
        into (gt_out-balamt, l_shkzg)
        from ekbz
       where ebeln = gt_calc-ebeln
         and belnr = gt_out-mblnr
         and ebelp = gt_out-ebelp.

      if l_shkzg eq 'S' and not gt_out-balamt is initial.
        gt_out-balamt = -1 * gt_out-balamt.
      endif.
    endif.

    ls_cellcolor-color-col = '2'.
    ls_cellcolor-fname     = 'STATUS'.
    append ls_cellcolor to gt_out-colinfo.
*** 12/02/2013 T00306 End


    append gt_out. clear gt_out.
  endloop.
endform.                    " GET_GT_OUT
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
  endif.
  __focus g_grid.

endmodule.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_color.
endform.                    " SET_COLOR
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
*&      Form  DOUBLE_CLICK
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
  clear gv_index.
  gv_index = e_row-index.

  read table gt_out index gv_index.
  if sy-subrc = 0.
    if e_column = 'BELUM'.
      check gt_out-belum ne space.
      set parameter id : 'RBN'  field gt_out-belum,
                         'GJR'  field p_date(4).
      call transaction 'MIR4' and skip first screen.
    endif.
    if e_column = 'MATNR'.
      check gt_out-matnr ne space.
      set parameter id 'MAT'  field gt_out-matnr.
      call transaction 'MM03' and skip first screen.
    endif.
    if e_column = 'EBELN'.
      check gt_out-ebeln ne space.
      set parameter id 'BES'  field gt_out-ebeln.
      call transaction 'ME23N' and skip first screen.
    endif.
  endif.

  call method cl_gui_control=>set_focus
    exporting
      control = g_grid.

endform.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  create_and_init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_and_init_alv.

  data: lt_ftab type sy-ucomm occurs 0 with header line.

*   Create object
  perform create_object.

*   Exclude toolbar
  perform exclude_functions.

*  if p_pat eq 'X'.
*    append 'POST' to lt_ftab.
*    set pf-status '100' excluding lt_ftab.
*  else.
  if p_load eq true.
    set pf-status '100'. " EXCLUDING 'POST'.
  else.
    set pf-status '100'.
  endif.
*  endif.

*  Create Object to verify input values.
  create object g_event_receiver.
  set :
    handler g_event_receiver->handle_data_changed for g_grid,
    handler g_event_receiver->handle_double_click for g_grid.

*   Create field category
  perform :
      create_field_category using false,
      sort_build using gt_sort[],
      set_lvc_layout,
      set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

endform.                    " create_and_init_alv
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form show_progress using    pf_text
                            value(pf_val).
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = pf_val
      text       = pf_text.

endform.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form modify_screen.
  loop at screen.
    if screen-group1 = 'EXL'.
      screen-input = 1.
      screen-invisible = 0.
    endif.
    case 'X'.
      when p_load.
        if screen-group1 = 'EXL'.
          screen-input = 0.
          screen-invisible  = 1.
        endif.
      when p_o_mat.
        if screen-group1 = 'EXL'.
          screen-input = 0.
          screen-invisible  = 1.
        endif.
    endcase.
*    if p_pat eq 'X'.
*      if screen-group1 = 'CHK'.
*        screen-active = 1.
*      endif.
*    else.
*      if screen-group1 = 'CHK'.
*        screen-active = 0.
*      endif.
*    endif.
    modify screen.
  endloop.

endform.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form upload_file using filename.

  if p_file eq space.
    g_error = true.
    exit.
  endif.

  data: it_itab like standard table of alsmex_tabline with header line.
  field-symbols : <fs>.
  data : v_index type i.
  data : begin_row type i value 1.

  __process '10'.
  if p_head = true.
    add 1 to begin_row.
  endif.

  if p_txt ne true.
    call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      exporting
        filename                = filename
        i_begin_col             = 1
        i_begin_row             = begin_row
        i_end_col               = 4
        i_end_row               = 65535
      tables
        intern                  = it_itab
      exceptions
        inconsistent_parameters = 1
        upload_ole              = 2
        others                  = 3.

    if sy-subrc ne 0.
      message s000 with 'Could not find the file.'.
      stop.
    endif.

    __process '20'.

    if it_itab[] is initial.
      message s003(zz) with 'No Data was uploaded'.
      g_error = true .
      exit.
    else.
      sort it_itab by row col.
      loop at it_itab.
        move : it_itab-col to v_index.
        assign component v_index of structure i_articles to <fs>.
        move : it_itab-value to <fs>.
        at end of row.
          append i_articles.
        endat.
      endloop.
    endif.
  else.
    data cancel.
    call function 'UPLOAD'
      exporting
        filename            = filename
        filetype            = 'DAT'
      importing
        cancel              = cancel
      tables
        data_tab            = i_articles
      exceptions
        conversion_erro     = 1
        invalid_table_width = 2
        invalid_type        = 3.

    if not cancel is initial or sy-subrc ne 0.
      message s003(zz) with 'No Data was uploaded'.
      stop.
    endif.

  endif.


  __process '30'.
  __cls : gr_matnr.

  loop at i_articles.
    clear gr_matnr.
    gr_matnr-sign   = 'I'.
    gr_matnr-low    = i_articles-matnr.
    gr_matnr-option = 'EQ'.
    append gr_matnr.
    perform check_num changing :
              i_articles-menge,
              i_articles-iv_amt,
              i_articles-dt_amt.
    modify i_articles.
  endloop.

*  DELETE ADJACENT DUPLICATES FROM GT_MATNR.
  sort gr_matnr by low .
  delete adjacent duplicates from gr_matnr.

endform.                    " upload_file
*&---------------------------------------------------------------------*
*&      Form  get_data_from_dkbz_dkpo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_row_data.

* Plants
  perform get_plant.

  data $gr_matnr like gr_matnr occurs 0  with header line.

  __cls it_row_tab.

  if not s_matnr[] is initial.
    $gr_matnr[] = s_matnr[] .
* Get data from PO table
    perform get_po_one_time tables $gr_matnr.
  else.
    data max_range type i.
    data line1 like sy-tabix.
    describe table gr_matnr lines line1.
    if p_mx_r is initial.
      max_range = 50.
    else.
      max_range = p_mx_r.
    endif.

    if line1 < max_range.
      $gr_matnr[] = gr_matnr[].
      perform get_po_one_time tables $gr_matnr.
    else.
      data : $times type i,
             $ix like sy-tabix,
             $line like sy-tabix.

      $times = line1 / max_range.
      add 1 to $times.
      $ix = 1.

      do $times times.

        __cls $gr_matnr.
        $line = 1.

        loop at gr_matnr from $ix.
          $ix = sy-tabix.
          $gr_matnr = gr_matnr.
          append $gr_matnr.
          clear $gr_matnr.
          add 1 to $line.
          if $line > max_range.
            exit.
          endif.
        endloop.

        add 1 to $ix.

        if not $gr_matnr[] is initial.
          perform get_po_several_times tables $gr_matnr.
        endif.

        if $ix > line1.
          exit.
        endif.

      enddo.

    endif.

  endif.

  read table it_row_tab index 1.
  if sy-subrc <> 0.
    g_error = true.
    message s000 with 'Could not find data.'.
    exit.
  endif.

  __process '60'.
  sort it_row_tab by bukrs kschl matnr ebeln ebelp .

  perform refine_itab.
  __process '75'.

endform.                    " get_data_from_dkbz_dkpo
*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form browser changing filename.
  data: it_tfile type filetable ,
        gd_subrc type i.

  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      window_title      = 'Select File Name'
      default_extension = '*.*'
      default_filename  = '*.*'
      file_filter       = '*.*'
      initial_directory = 'c:\temp\'
*     MULTISELECTION    =
*     WITH_ENCODING     =
    changing
      file_table        = it_tfile
      rc                = gd_subrc.
*         USER_ACTION =
*         FILE_ENCODING =
*         EXCEPTIONS
*         FILE_OPEN_DIALOG_FAILED = 1
*         CNTL_ERROR = 2
*         ERROR_NO_GUI = 3
*         NOT_SUPPORTED_BY_GUI = 4
*         others = 5
  .
  if sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  else.
    read table it_tfile into filename index 1.
  endif.

endform.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  CHECK_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_UPLOAD_MENGE  text
*----------------------------------------------------------------------*
form check_num changing n_value.
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
*&      Form  CALC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_data.
  __process '75'.

  __cls gt_calc.
  sort i_articles by matnr .

* Fill the table with excel data
  loop at it_row_tab.
    move-corresponding it_row_tab to gt_calc.
    read table i_articles with key matnr = it_row_tab-matnr
                          binary search.
    if sy-subrc eq 0 .
      gt_calc-xmenge = i_articles-menge .
      write gt_calc-xmenge to gt_calc-xmenges unit gt_calc-meins.
      append gt_calc.clear gt_calc .
    else.
      gt_calc-xmenge = 0 .
      write gt_calc-xmenge to gt_calc-xmenges unit gt_calc-meins.
      append gt_calc.clear gt_calc .
    endif.

  endloop.

* For calculating the reconciling
  sort gt_calc by bukrs kschl matnr ebeln ebelp.
* For summing the balance data by Material
  sort itab_for_matnr_sum by matnr.
* Calculate
  perform get_recon_quantity tables gt_calc.

endform.                    " CALC_DATA
*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_data.

  data: lt_rows type lvc_t_row with header line,
        lt_row_no type lvc_t_roid. "Numeric IDs of Selected Rows
  data: l_line type i,
        w_repid like sy-repid.

  data: l_error.

****************************> No Important
  data : total_lines type i,
         t_lines1 type i,
         t_lines2 type i,
         count type i.
****************************> end

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
    message e000 with 'Please select a data to post at least 1.'.
    exit.
  endif.

  if p_pat eq true.
    loop at lt_rows where rowtype is initial.
      read table gt_out index lt_rows-index.
      if gt_out-status ne icon_yellow_light.
        l_error = 'X'.
        message s000 with 'Invalid data to Post' display like 'E'.
        exit.
      endif.
    endloop.
  endif.

  check l_error is initial.

  perform get_posting_data tables lt_rows
                                  lt_row_no .

*
  describe table :
          it_inv lines t_lines1,
          it_cre lines t_lines2.
  total_lines = t_lines1 + t_lines2.
*

  read table it_inv index 1.
  if sy-subrc eq 0.
    perform call_bapi_inv tables it_inv
                          using text-p01
                                'X' 'RI'
                                total_lines
                       changing count.
  endif.

  read table it_cre index 1.
  if sy-subrc eq 0.
    perform call_bapi_inv tables it_cre
                          using text-p01
                                ' ' 'RI'
                                total_lines
                       changing count.
  endif.

endform.                    " POST_DATA
*&---------------------------------------------------------------------*
*&      Form  CALC_UNIT_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_lt_itab_FOR_UNITPRICE[]  text
*----------------------------------------------------------------------*
form refine_itab.

  data lt_itab like it_row_tab occurs 0 with header line.
  data lt_itab_for_unitprice like it_row_tab occurs 0 with header line.
  data l_bmenge like itab_for_matnr_sum-bmenge.

**   Calculate the unit price using by 'DCGR'
*  loop at it_row_tab where bewtp eq 'F'.
  loop at it_row_tab where bewtp eq 'F'.

*    if p_pat eq true.
*    elseif p_all eq true and it_row_tab-bewtp eq 'F'.
*    else.
*      continue.
*    endif.

    move it_row_tab to lt_itab_for_unitprice .

    if lt_itab_for_unitprice-shkzg eq 'S'.
      lt_itab_for_unitprice-bmenge =
               -1 * lt_itab_for_unitprice-bmenge.
      lt_itab_for_unitprice-dmbtr =
               -1 * lt_itab_for_unitprice-dmbtr.
    endif.

    clear :  lt_itab_for_unitprice-pmenge,
             lt_itab_for_unitprice-uprice,
             lt_itab_for_unitprice-shkzg,
             lt_itab_for_unitprice-bewtp,
             lt_itab_for_unitprice-meins,
             lt_itab_for_unitprice-waers.
    collect lt_itab_for_unitprice.
  endloop.

  __process '65'.

  loop at lt_itab_for_unitprice.
    if p_all eq true.
      if lt_itab_for_unitprice-bmenge > 0.
        lt_itab_for_unitprice-uprice =
            lt_itab_for_unitprice-dmbtr / lt_itab_for_unitprice-bmenge.
        modify lt_itab_for_unitprice .
      endif.
    else.
      lt_itab_for_unitprice-uprice =
          lt_itab_for_unitprice-dmbtr / lt_itab_for_unitprice-bmenge.
      modify lt_itab_for_unitprice.
    endif.
  endloop.
  sort lt_itab_for_unitprice by matnr ebeln ebelp .

  __process '67'.

  loop at it_row_tab.
    move it_row_tab to lt_itab .
    if lt_itab-shkzg eq 'S'.
      lt_itab-bmenge = -1 * lt_itab-bmenge.
      lt_itab-dmbtr  = -1 * lt_itab-dmbtr.
    endif.
    clear : lt_itab-shkzg,lt_itab-bewtp,lt_itab-uprice .
*** 12/02/2013 - T00306 Start
    if p_pat eq true.
      append lt_itab.
    else.
      collect lt_itab.
    endif.
  endloop.

  __process '69'.

**   GET SUM by MATNR
  __cls itab_for_matnr_sum.
  if p_nega eq true.
    data $lt_itab like lt_itab occurs 0 with header line.
    $lt_itab[] = lt_itab[].
    delete $lt_itab where bmenge < 0.
*** 12/02/2013 - T00306 Start
    if p_pat eq true.
      loop at $lt_itab.
        move $lt_itab to itab_for_matnr_sum .
        if not $lt_itab-xblnr is initial.
          add $lt_itab-bmenge to l_bmenge.
        endif.
        at end of matnr.
          itab_for_matnr_sum-bmenge = l_bmenge.
          append itab_for_matnr_sum.
          clear l_bmenge.
        endat .
      endloop.
*** 12/02/2013 - T00306 End
    else.
      loop at $lt_itab.
        move $lt_itab to itab_for_matnr_sum .
        at end of matnr.
          sum.
          itab_for_matnr_sum-bmenge = $lt_itab-bmenge.
          append itab_for_matnr_sum.
        endat .
      endloop.
    endif.

*** 12/02/2013 - T00306 Start
  else.
    if p_pat eq true.
      loop at lt_itab.
        move lt_itab to itab_for_matnr_sum.
        if not lt_itab-xblnr is initial.
          add lt_itab-bmenge to l_bmenge.
        endif.
        at end of matnr.
          itab_for_matnr_sum-bmenge = l_bmenge.
          append itab_for_matnr_sum.
          clear l_bmenge.
        endat.
      endloop.
*** 12/02/2013 - T00306 End
    else.
      loop at lt_itab.
        move lt_itab to itab_for_matnr_sum .

        at end of matnr.
          sum.
          itab_for_matnr_sum-bmenge = lt_itab-bmenge.
          append itab_for_matnr_sum.
        endat.
      endloop.
    endif.
  endif.

  __process '67'.

  loop at lt_itab.
    read table it_row_tab with key
                          bukrs = lt_itab-bukrs
                          kschl = lt_itab-kschl
                          matnr = lt_itab-matnr
                          ebeln = lt_itab-ebeln
                          ebelp = lt_itab-ebelp
                          binary search.
    if sy-subrc eq 0.
      lt_itab-pmenge = it_row_tab-pmenge .
    endif.

    read table lt_itab_for_unitprice with key
                          bukrs = lt_itab-bukrs
                          kschl = lt_itab-kschl
                          matnr = lt_itab-matnr
                          ebeln = lt_itab-ebeln
                          ebelp = lt_itab-ebelp
                          mblnr = lt_itab-mblnr
                          binary search.
    if sy-subrc eq 0.

      lt_itab-uprice = lt_itab_for_unitprice-uprice.
*      if ( p_all eq true ) or
*         ( ( p_pat eq true ) and ( not lt_itab-lichn is initial ) ).
*      G/R quantity
      lt_itab-gmenge = lt_itab_for_unitprice-bmenge .
*      endif.
*      At this point LT_ITAB_FOR_UNITPRICE-BMENGE contains the
    endif.

    modify lt_itab.

  endloop.

  __cls it_row_tab.
  it_row_tab[] = lt_itab[] .

endform.                    "refine_itab
*&---------------------------------------------------------------------*
*&      Form  GET_POSTING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*----------------------------------------------------------------------*
form get_posting_data tables pt_rows structure lvc_s_row
                             pt_row_no structure lvc_s_roid.

  __cls: it_inv,it_cre .

  clear gt_out-chk.
  modify gt_out transporting chk where chk = 'X'.

* Selected Row by row selection
  loop at pt_rows where rowtype is initial.
    read table gt_out index pt_rows-index.
    check gt_out-rmenge ne 0 .
    gt_out-chk = true .
    modify gt_out index pt_rows-index .
  endloop.

  perform select_row_by_subtotal tables pt_rows .

  loop at gt_out where chk eq true.

    check : gt_out-rmenge ne 0 ,
            gt_out-belum is initial .

*   Post type 'X' = I/V , '' = credit memo
    if gt_out-post_type eq true.
      move-corresponding gt_out to it_inv.
      move gt_out-rmenge to it_inv-menge.
      move pt_rows-index to it_inv-index.
      append it_inv. clear it_inv.
    else.
      move-corresponding gt_out to it_cre.
      move gt_out-rmenge to it_cre-menge.
      move pt_rows-index to it_cre-index.
      append it_cre. clear it_cre.
    endif.
  endloop.

* grouping by max. posting item per material .

  total_doc_cnt = 0.
  current_doc_cnt = 0.

  perform grouping_by_mat tables :
            it_inv,
            it_cre.
endform.                    " GET_POSTING_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI_INV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_INV  text
*----------------------------------------------------------------------*
form call_bapi_inv tables p_it_inv structure it_inv
                   using prog_text
                         ind
                         iv_type
                         p_total_lines
                changing p_count.

  data : end_flag(1) ,
         item_cnt(4) type n,
         gross_amount like bapi_incinv_create_header-gross_amount.

  data : percentage type p,
        $prog_text(50),$current_cnt(10),$total_cnt(10),$text(30) .

  __cls itemdata.

  item_cnt = 0.

  loop at p_it_inv.

    add 1 to p_count.

    at end of group.
      end_flag = true.
*      SUM.
*      GROSS_AMOUNT = P_IT_INV-DMBTR.
    endat.

    perform fill_bapi_structure_item tables p_it_inv
                                     changing item_cnt .

    if end_flag eq true .
*     *******************************************> No important
      add 1 to current_doc_cnt.
      $current_cnt = current_doc_cnt.
      $total_cnt = total_doc_cnt.
      concatenate $current_cnt '/' $total_cnt
      into $text.
      condense $text .
      concatenate prog_text $text into $prog_text.
      percentage = p_count / p_total_lines * 100.
      perform show_progress using $prog_text percentage.
*     *******************************************>
      item_cnt = 0.
      perform fill_bapi_structure_header tables p_it_inv
                                          using gross_amount
                                                ind
                                                iv_type  .
*** 12/02/2013 - T00306 Start
      if p_pat eq true.
        perform post_bapi_invoice_partial  using p_it_inv-index
                                      prog_text
                                      ind.
      else.
        perform post_bapi_invoice using p_it_inv-index
                                        prog_text
                                        ind.
      endif.
*** 12/02/2013 - T00306 End

      clear end_flag .
    endif.

  endloop.

endform.                    " CALL_BAPI_INV
*&---------------------------------------------------------------------*
*&      Form  GET_RECON_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CALC  text
*----------------------------------------------------------------------*
form get_recon_quantity tables   p_gt_calc structure gt_calc .

  data : tempq    like ekbz-menge,  " Temp variable
         quantity like ekbz-menge,  " Temp variable
         flag(1),
         balance  like ekbz-menge,  " Sum by Material
         abs_qty  like ekbz-menge.  " ABS Value for Quantity

* Declare temp. Table for Credit Posting
* Need to be incleased from recent P/O in Credit case.
  data : lt_itab_for_cr like gt_calc occurs 0  with header line,
         lt_itab_for_iv like gt_calc occurs 0  with header line.

  __process '80'.

  clear flag.

  loop at p_gt_calc.
    tempq = p_gt_calc-xmenge. " Store quantity temporary
    at new matnr.
      flag = true .
      quantity =  tempq. " quantity from Excel
    endat .

    if  flag eq true .
      read table itab_for_matnr_sum with key
                          matnr = p_gt_calc-matnr
                          binary search.
      if sy-subrc eq 0.
        balance = itab_for_matnr_sum-bmenge.
        quantity =  quantity -  balance .
      endif.

      clear  flag .
    endif .

    if  quantity >= 0.
      move p_gt_calc to lt_itab_for_cr.
      append lt_itab_for_cr.
      clear lt_itab_for_cr.
    else.
      move p_gt_calc to lt_itab_for_iv.
      append lt_itab_for_iv.
      clear lt_itab_for_iv.
    endif.

  endloop.

  __process '85'.

* Start Calc for Credit Memo post
  sort lt_itab_for_cr by bukrs kschl matnr ascending
                                     ebeln descending.

  loop at lt_itab_for_cr.
    tempq = lt_itab_for_cr-xmenge. " Store quantity temporary
    at new matnr.
      flag = true .
      quantity =  tempq. " quantity from Excel
    endat .

    if  flag eq true .
      read table itab_for_matnr_sum with key
                          matnr = lt_itab_for_cr-matnr
                          binary search.
      if sy-subrc eq 0.
        balance = itab_for_matnr_sum-bmenge.
        quantity =  quantity -  balance .
      endif.
      clear  flag .
    endif .

    lt_itab_for_cr-rmenge =
          lt_itab_for_cr-gmenge - lt_itab_for_cr-bmenge.
***  at first time RMENGE has the value for in/decrease limit

***  Quantity must be greater then 1.
    if lt_itab_for_cr-rmenge > 0.
      lt_itab_for_cr-rmenge = lt_itab_for_cr-rmenge - 1.
    elseif lt_itab_for_cr-rmenge < 0.
      lt_itab_for_cr-rmenge = lt_itab_for_cr-rmenge + 1.
    endif.

    if  quantity ge 0.
***    if excel > SAP, credit memo posting *** Increase from old PO#

      if lt_itab_for_cr-rmenge <=  quantity.
*                                            " limited by this quantity.
        quantity =  quantity - lt_itab_for_cr-rmenge.
      else." The limit quantity is enough to increase.
        lt_itab_for_cr-rmenge =  quantity .
        quantity = 0 .
      endif.
    else. " End of Target Quantity is not Zero
      lt_itab_for_cr-rmenge = 0.
    endif.

* /////////////////////// added for clearing negative balance
    if p_nega eq true.
      if lt_itab_for_cr-bmenge < 0.
        lt_itab_for_cr-rmenge = lt_itab_for_cr-bmenge * -1 .
      endif.
    endif.

    lt_itab_for_cr-amenge =
        lt_itab_for_cr-rmenge + lt_itab_for_cr-bmenge .

*    LT_ITAB_FOR_CR-DMBTR =
*        LT_ITAB_FOR_CR-UPRICE * LT_ITAB_FOR_CR-RMENGE .

    lt_itab_for_cr-post_type = space.
    modify lt_itab_for_cr .
  endloop.

  __process '90'.

* Start Calc for Invoice post
  sort lt_itab_for_iv by bukrs kschl matnr ebeln ebelp.

  loop at lt_itab_for_iv.
*                                     " Store quantity temporary
    tempq = lt_itab_for_iv-xmenge.

    at new matnr.
      flag = true .
      quantity =  tempq. " quantity from Excel
    endat .

    if  flag eq true .
      read table itab_for_matnr_sum with key
                          matnr = lt_itab_for_iv-matnr
                          binary search.
      if sy-subrc eq 0.
        balance = itab_for_matnr_sum-bmenge.
        quantity =  quantity -  balance .
      endif.

      clear  flag .
    endif .

    lt_itab_for_iv-rmenge = 0.

    if lt_itab_for_iv-bmenge > 0 and  quantity lt 0.

***   if excel <= SAP, Invoice posting
***   Decrease Quantity from old PO#

      abs_qty = abs(  quantity ) .

      if  abs_qty <= lt_itab_for_iv-bmenge.
        lt_itab_for_iv-rmenge =  abs_qty .
        quantity = 0.               " Calc. end for this material
      else.
        lt_itab_for_iv-rmenge = lt_itab_for_iv-bmenge .
        quantity  = lt_itab_for_iv-bmenge +  quantity.
      endif .

      lt_itab_for_iv-rmenge = -1 * lt_itab_for_iv-rmenge.

    endif.

* /////////////////////// added for clearing negative balance
    if p_nega eq true.
      if lt_itab_for_cr-bmenge < 0.
        lt_itab_for_iv-rmenge = lt_itab_for_iv-bmenge * -1 .
      endif.
    endif.

    lt_itab_for_iv-amenge =
          lt_itab_for_iv-bmenge + lt_itab_for_iv-rmenge.

*    LT_ITAB_FOR_IV-DMBTR =
*           LT_ITAB_FOR_IV-UPRICE * LT_ITAB_FOR_IV-RMENGE.

    lt_itab_for_iv-post_type = 'X'.
    modify lt_itab_for_iv .
  endloop.

  __cls p_gt_calc.

  append lines of :
                lt_itab_for_cr to p_gt_calc,
                lt_itab_for_iv to p_gt_calc.

  __process '95'.

endform.                    " GET_RECON_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  fill_BAPI_STRUCTURE_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IT_INV  text
*----------------------------------------------------------------------*
form fill_bapi_structure_item tables p_it_inv structure it_inv
                            changing p_cnt .

  add 1 to p_cnt.

* FIll Item data
  select single * from ekpo
                 where ebeln = p_it_inv-ebeln
                   and ebelp = p_it_inv-ebelp.

  if sy-subrc eq 0 and ekpo-webre eq 'X'.
    select single * from ekbe
            where ebeln = p_it_inv-ebeln
             and  ebelp = p_it_inv-ebelp.
    if sy-subrc eq 0.
      move :  ekbe-belnr  to    itemdata-ref_doc,
              ekbe-gjahr  to    itemdata-ref_doc_year,
              ekbe-buzei  to    itemdata-ref_doc_it.
    endif.
  endif.

  itemdata-invoice_doc_item = p_cnt.
  itemdata-po_number = p_it_inv-ebeln.
  itemdata-po_item = p_it_inv-ebelp.
  itemdata-cond_type = 'ZOA1'.

  itemdata-tax_code = 'U0'.
* ITEMDATA-ITEM_AMOUNT = ABS( P_IT_INV-DMBTR ) .
  itemdata-item_amount = 0.
  itemdata-quantity    = abs( p_it_inv-menge ) .
  itemdata-po_unit     = p_it_inv-meins.
  itemdata-po_unit_iso = itemdata-po_unit.

  append itemdata. clear itemdata.


endform.                    " fill_BAPI_STRUCTURE_item
*&---------------------------------------------------------------------*
*&      Form  fill_BAPI_STRUCTURE_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IT_INV  text
*----------------------------------------------------------------------*
form fill_bapi_structure_header tables p_it_inv structure it_inv
                                 using p_gross_amount
                                       ind
                                       iv_type.
  data $gross_amount like bapi_incinv_create_header-gross_amount .
  clear headerdata .

  $gross_amount = abs( p_gross_amount ) .
  move : ind              to  headerdata-invoice_ind,
         iv_type          to  headerdata-doc_type,
         p_bukrs          to  headerdata-comp_code,
*        $GROSS_AMOUNT    TO  HEADERDATA-GROSS_AMOUNT,
         0    to  headerdata-gross_amount,
         p_it_inv-waers   to  headerdata-currency,
         p_date           to  headerdata-doc_date,
         p_date           to  headerdata-pstng_date,
         sy-uname         to  headerdata-person_ext,
         ''               to  headerdata-calc_tax_ind,
         'YE_REC'         to  headerdata-ref_doc_no.


endform.                    " fill_BAPI_STRUCTURE_header
*&---------------------------------------------------------------------*
*&      Form  post_bapi_invoice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_*BAPI_INCINV_CREATE_HEADER  text
*      <--P_COUNT  text
*      <--P_TOTAL_LINES  text
*----------------------------------------------------------------------*
form post_bapi_invoice  using     p_index
                                  p_prog_text
                                  p_ind.

* Call Invoice Posting BAPI

  call function 'BAPI_INCOMINGINVOICE_CREATE'
    exporting
      headerdata       = headerdata
    importing
      invoicedocnumber = invoicedocnumber
      fiscalyear       = fiscalyear
    tables
      itemdata         = itemdata
*     TAXDATA          = TAXDATA
      return           = xreturn.


  if not invoicedocnumber is initial.
    loop at itemdata.
      gt_out-status = icon_led_green.
      gt_out-icon = icon_led_green.
      gt_out-belum = invoicedocnumber .
      if p_ind eq true.
        gt_out-msg = 'I/V'.
      else.
        gt_out-msg = 'Credit Memo'.
      endif.
      modify gt_out transporting icon belum msg status
       where ebeln = itemdata-po_number and ebelp = itemdata-po_item.
    endloop.
    call function 'BAPI_TRANSACTION_COMMIT'.
  else.
    loop at xreturn where type = 'E'.

      call function 'MESSAGE_TEXT_BUILD'
        exporting
          msgid               = xreturn-id
          msgnr               = xreturn-number
          msgv1               = xreturn-message_v1
          msgv2               = xreturn-message_v2
          msgv3               = xreturn-message_v3
          msgv4               = xreturn-message_v4
        importing
          message_text_output = xreturn-message.
      exit.
    endloop.
    loop at itemdata.
      gt_out-msg =   xreturn-message.
      gt_out-icon = icon_led_yellow.
      clear gt_out-belum.
      modify gt_out transporting icon belum msg
       where ebeln = itemdata-po_number and ebelp = itemdata-po_item
             and chk eq true.
    endloop.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  endif.
  clear :headerdata.
  __cls : itemdata,xreturn .

endform.                    " post_bapi_invoice
*&---------------------------------------------------------------------*
*&      Form  post_bapi_invoice_partial
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INDEX      text
*      -->P_PROG_TEXT  text
*      -->P_IND        text
*----------------------------------------------------------------------*
form post_bapi_invoice_partial  using p_index
                                      p_prog_text
                                      p_ind.

  data: lt_itemdata type table of bapi_incinv_create_item with header line.
  data: l_tabix type stabix,
        l_seque type i.

* Call Invoice Posting BAPI
  loop at gt_out where chk eq 'X'.
    l_tabix = sy-tabix.
    l_seque = l_seque + 1.

    refresh: lt_itemdata.
    read table itemdata index l_seque.
    append itemdata to lt_itemdata.

    call function 'BAPI_INCOMINGINVOICE_CREATE'
      exporting
        headerdata       = headerdata
      importing
        invoicedocnumber = invoicedocnumber
        fiscalyear       = fiscalyear
      tables
        itemdata         = lt_itemdata
*       TAXDATA          = TAXDATA
        return           = xreturn.


    if not invoicedocnumber is initial.
      loop at itemdata.
        gt_out-status = icon_green_light.
        gt_out-icon = icon_led_green.
        gt_out-belum = invoicedocnumber .
        if p_ind eq true.
          gt_out-msg = 'I/V'.
        else.
          gt_out-msg = 'Credit Memo'.
        endif.
        modify gt_out index l_tabix.
      endloop.
      call function 'BAPI_TRANSACTION_COMMIT'.
    else.
      loop at xreturn where type = 'E'.

        call function 'MESSAGE_TEXT_BUILD'
          exporting
            msgid               = xreturn-id
            msgnr               = xreturn-number
            msgv1               = xreturn-message_v1
            msgv2               = xreturn-message_v2
            msgv3               = xreturn-message_v3
            msgv4               = xreturn-message_v4
          importing
            message_text_output = xreturn-message.
        exit.
      endloop.
      loop at itemdata.
        gt_out-msg =   xreturn-message.
        gt_out-icon = icon_led_yellow.
        clear gt_out-belum.
        modify gt_out index l_tabix.
      endloop.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
    endif.
    wait UP TO 5 SECONDS.
  endloop.
  clear :headerdata.
  __cls : itemdata,xreturn .

endform.                    "post_bapi_invoice_patial
*&---------------------------------------------------------------------*
*&      Form  GROUPING_BY_MAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_INV  text
*----------------------------------------------------------------------*
form grouping_by_mat tables p_it_cr_or_inv structure it_inv.

  data :  group_key type i, $group_key(10),
          max_line  type i,
          line_cnt  type i.

  max_line = p_max .
  group_key = 0.

  loop at p_it_cr_or_inv.
    at new matnr.
      line_cnt = 0 .
      add 1 to : group_key, total_doc_cnt.
    endat.
    if  line_cnt ge  max_line.
      line_cnt = 0 .
      add 1 to : group_key, total_doc_cnt.
    endif.
    p_it_cr_or_inv-$group = group_key.
    modify p_it_cr_or_inv .
    add 1 to  line_cnt .
  endloop.

  loop at p_it_cr_or_inv.
    p_it_cr_or_inv-group = p_it_cr_or_inv-$group .
    modify p_it_cr_or_inv .
  endloop.

endform.                    " GROUPING_BY_MAT
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_select_value.
  case 'X'.
    when p_e_mat.
      if s_matnr is initial.
**S> 08/04/11 Paul
        message s000 with
        'Please enter the selection range for materials.'.
**E<
        stop.
      endif.
  endcase.
endform.                    " CHECK_SELECT_VALUE
*&---------------------------------------------------------------------*
*&      Form  SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_ROW_NO  text
*----------------------------------------------------------------------*
form select_row_by_subtotal tables p_pt_rows
                                   structure lvc_s_row.

  data: tmpgrp type lvc_t_grpl, " For subtotal Selection .
       $tmpgrp type lvc_s_grpl.

  call method g_grid->get_subtotals
    importing
      et_grouplevels = tmpgrp.

* Selected Row by row selection ( Sub total )
  loop at p_pt_rows where not rowtype is initial.
    read table tmpgrp index p_pt_rows-index into $tmpgrp.
    check sy-subrc eq 0 .

    loop at gt_out from $tmpgrp-index_from
                     to $tmpgrp-index_to.
      gt_out-chk = true .
      modify gt_out .
    endloop.

  endloop.

endform.                    " SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*&      Form  LOAD_SAVED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form load_saved_data.

  __process '10'.

  __cls : gt_calc, gt_out .

* Get data from table
  select  *
  into corresponding fields of table gt_calc
  from    ztcou123
  where   pdate        in    s_date
  and     bukrs        eq    p_bukrs
  and     kschl        eq    p_kschl
  and     matnr        in    s_matnr
  and     ebeln        in    s_ebeln.

  if sy-subrc <> 0.
    g_error = true.
    message s000 with 'Could not find data.'.
    exit.
  endif.

*///////////////////// temp
  data $gt_calc like gt_calc occurs 0 with header line.
  data $ix like sy-tabix.
  data $flag(1).

  loop at gt_calc.
    $ix = sy-tabix.
    check gt_calc-icon eq icon_led_yellow.
    $gt_calc = gt_calc.
    append $gt_calc .
    delete gt_calc index $ix.
  endloop.

  sort $gt_calc by bukrs kschl matnr ebeln ebelp.

  loop at $gt_calc.
    at new ebelp.
      $flag = true.
    endat.
    check $flag eq true.
    gt_calc = $gt_calc.
    clear gt_calc-belum.
    append gt_calc.
    clear $flag.
  endloop.

*///////////////////// temp

  loop at gt_calc.
    write gt_calc-xmenge to gt_calc-xmenges unit gt_calc-meins.
    modify gt_calc.
    move-corresponding gt_calc to gt_out.
    append gt_out .
  endloop.

  __process '98'. "98%

endform.                    " LOAD_SAVED_DATA
*&---------------------------------------------------------------------*
*&      Form  KSCHL_INPUT_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_KSCHL  text
*----------------------------------------------------------------------*
form kschl_input_help changing p_p_kschl.
  data j like sy-index.
  clear : con_list.

  select kschl vtext
  into table con_list
  from t685t
  where spras = 'EN'
    and kvewe = 'A'
    and kappl = 'M'.

  sort con_list by kschl .

  help_field-tabname = 'T685T'.
  help_field-fieldname = 'KSCHL'.
  help_field-selectflag = 'X'.
  append help_field.

  help_field-tabname = 'T685T'.
  help_field-fieldname = 'VTEXT'.
  help_field-selectflag = ' '.
  append help_field.

  loop at con_list.
    help_value-value = con_list-kschl.
    append help_value.
    help_value-value = con_list-vtext.
    append help_value.
  endloop.

  perform value_help changing j.

  if j > 0.
    read table con_list index j.
    p_p_kschl = con_list-kschl.
  endif.

  dynpfields-fieldname  = 'KSCHL'.
  dynpfields-fieldvalue = con_list-kschl.
  append dynpfields.

  call function 'DYNP_VALUES_UPDATE'
    exporting
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    tables
      dynpfields = dynpfields.

  clear: dynpfields.
  refresh: con_list, help_field, help_vtab, help_value, dynpfields.
endform.                    " KSCHL_INPUT_HELP

*&---------------------------------------------------------------------*
*&      Form  VALUE_HELP
*&---------------------------------------------------------------------*
form value_help changing p_j.
  call function 'HELP_VALUES_GET_WITH_TABLE_EXT'
    exporting
      display       = ' '
    importing
      index         = p_j
    tables
      fields        = help_field
      select_values = help_vtab
      valuetab      = help_value.
endform.                               " VALUE_HELP
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_plant.
  __cls  : gt_plant, gr_bwkey.

* Get plant
  select bwkey into table gt_plant
    from t001k
   where bukrs = p_bukrs.

  loop at gt_plant.
    gr_bwkey-sign = 'I'.
    gr_bwkey-option = 'EQ'.
    gr_bwkey-low = gt_plant-bwkey.

    append gr_bwkey.
    clear gr_bwkey.
  endloop.


endform.                    " GET_PLANT
*&---------------------------------------------------------------------*
*&      Form  GET_PO_ONE_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GR_MATNR  text
*----------------------------------------------------------------------*
form get_po_one_time tables $gr_matnr structure gr_matnr.

  data: l_xblnr type xblnr1,
        l_tabix type stabix.
  ranges: r_date for sy-datum.

  data: begin of lt_temp occurs 0,
          vgbel type vgbel,
          lichn type lichn,
          mblnr type mblnr,
          menge type menge,
        end of lt_temp.

  case p_all.
    when 'X'.
      select  a~bukrs b~kschl a~matnr a~ebeln a~ebelp
              a~menge as pmenge
              sum( b~menge ) as bmenge
              sum( b~dmbtr ) as dmbtr
              b~shkzg b~bewtp a~meins b~waers b~lifnr a~werks
          into corresponding fields of table it_row_tab
          from       ekpo as a
          inner join ekbz as b
          on      b~ebeln      eq    a~ebeln
          and     b~ebelp      eq    a~ebelp
          where   a~matnr      in    $gr_matnr
          and     a~werks      in    gr_bwkey
          and     a~bukrs      eq    p_bukrs
          and     b~kschl      eq    p_kschl
          and     ( b~bewtp    eq    'M' or b~bewtp  eq 'F' )
          and     a~ebeln      in    s_ebeln
          group by a~matnr a~ebeln a~ebelp b~shkzg
                   a~bukrs b~kschl b~bewtp a~menge
                   a~brtwr a~meins b~waers b~lifnr a~werks.
    when others.

      select a~bukrs b~kschl a~matnr a~ebeln a~ebelp
             a~menge as pmenge
             b~menge as bmenge
             b~dmbtr
             b~shkzg b~bewtp a~meins b~waers b~lifnr a~werks
             b~belnr as mblnr
       into corresponding fields of table it_row_tab
       from ekpo as a inner join ekbz as b
                         on b~ebeln eq a~ebeln
                        and b~ebelp eq a~ebelp
      where  a~matnr in $gr_matnr
        and  a~werks in gr_bwkey
        and  a~bukrs eq p_bukrs
        and  b~kschl eq p_kschl
        and  ( b~bewtp eq 'M' or b~bewtp eq 'F' )
        and  a~ebeln in s_ebeln
        and  b~gjahr in r_date.

      loop at it_row_tab.
        l_tabix = sy-tabix.

        select single t1~lichn t2~xblnr
                     t2~budat
          into (it_row_tab-lichn, it_row_tab-xblnr,
                it_row_tab-budat)
          from lips as t1 inner join mkpf as t2
                             on t2~le_vbeln = t1~vbeln
         where t1~vgbel = it_row_tab-ebeln
           and t2~mblnr = it_row_tab-mblnr
           and t1~lfimg = it_row_tab-bmenge.

        if not s_date[] is INITIAL and
           not it_row_tab-budat in s_date.
           delete it_row_tab index l_tabix.
           clear: it_row_tab.
           CONTINUE.
        endif.
        modify it_row_tab index l_tabix.
          clear: it_row_tab.
      endloop.

      if sy-subrc ne 0.
        if sy-subrc <> 0.
          g_error = true.
          message s000 with 'Could not find data.' display like 'E'.
          exit.
        endif.
      endif.

  endcase.

*  endif.
*
endform.                    " GET_PO_ONE_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_PO_SEVERAL_TIMES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GR_MATNR  text
*----------------------------------------------------------------------*
form get_po_several_times tables $gr_matnr structure gr_matnr.

  data: begin of ls_refer,
          xblnr type xblnr1,
          lichn type lichn,
        end of ls_refer.

  data: l_xblnr type xblnr1.
  ranges: r_date for sy-datum.

  if p_pat ne 'X'.
    select  a~bukrs b~kschl a~matnr a~ebeln a~ebelp
            a~menge as pmenge
            sum( b~menge ) as bmenge
            sum( b~dmbtr ) as dmbtr
            b~shkzg b~bewtp a~meins b~waers b~lifnr a~werks
    appending corresponding fields of table it_row_tab
    from       ekpo as a
    inner join ekbz as b
    on      b~ebeln      eq    a~ebeln
    and     b~ebelp      eq    a~ebelp
    where   a~matnr      in    $gr_matnr
    and     a~werks      in    gr_bwkey
    and     a~bukrs      eq    p_bukrs
    and     b~kschl      eq    p_kschl
    and     ( b~bewtp    eq    'M' or b~bewtp  eq 'F' )
    and     a~ebeln      in    s_ebeln
    group by a~matnr a~ebeln a~ebelp b~shkzg
             a~bukrs b~kschl b~bewtp a~menge
             a~brtwr a~meins b~waers b~lifnr a~werks.
  else.

    select  a~bukrs b~kschl a~matnr a~ebeln a~ebelp
            a~menge as pmenge
            sum( b~menge ) as bmenge
            sum( b~dmbtr ) as dmbtr
            b~shkzg b~bewtp a~meins b~waers b~lifnr a~werks
    appending corresponding fields of table it_row_tab
    from       ekpo as a
    inner join ekbz as b
    on      b~ebeln      eq    a~ebeln
    and     b~ebelp      eq    a~ebelp
    where   a~matnr      in    $gr_matnr
    and     a~werks      in    gr_bwkey
    and     a~bukrs      eq    p_bukrs
    and     b~kschl      eq    p_kschl
    and     ( b~bewtp    eq    'M' or b~bewtp  eq 'F' )
    and     a~ebeln      in    s_ebeln
    group by a~matnr a~ebeln a~ebelp b~shkzg
             a~bukrs b~kschl b~bewtp a~menge
             a~brtwr a~meins b~waers b~lifnr a~werks b~menge.
  endif.

*  IF SY-SUBRC <> 0.
*    G_ERROR = TRUE.
*    MESSAGE S000 WITH 'Could not find data.'.
*    EXIT.
*  ENDIF.

endform.                    " GET_PO_SEVERAL_TIMES
