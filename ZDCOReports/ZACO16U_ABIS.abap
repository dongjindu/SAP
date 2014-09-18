************************************************************************
* Program Name      : ZACO16U_ABIS
* Author            : Hyung Jin Youn (Tuned by IG.MOON at 11/7/2007)
* Creation Date     : 04/11/2003
* Specifications By : Deok-Kie Lee
* Pattern           : Report 1-1
* Development Request No: UD1K903655
* Add documentation :
* Description       : Allocate the costs in Internal order- P001, E001
*                     to PCC by the rate of PCC cost -> Changed
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date       Developer       Request ID
* 16/04/2004 Hyung Jin Youn  UD1K909644
* Description
* Cost By Shop report requires the Additional Issue Data, so this
* program should be revised and changed to be fit to the change of
* Business Process - To PCC by the rate of PCC Quantity Base
* Date       Developer  Request     Description
* 06/06/06   Manju      UD1K920991  Rounding off changes
* 09/27/2012 Valerian   UD1K955607  Distribute difference amout
*                                   proportionaly by QTY
* 10/02/2012 Valerian   UD1K955611  Continue with next record if the
*                                   component is not found.
* 10/26/2012 Valerian   UD1K955712  Correct the rounding issue that
*                                   cause items being suppressed
* 11/12/2012 Valerian   UD1K955776  Correct other rounding issues.
************************************************************************
report zaco16u_abis message-id zmco.
tables : pkosa, ckmlmv001.
include zacoui00.
include <icon>.                        " icon
* For TOP include
include zaco16l_1top.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
parameters : p_kokrs like csks-kokrs memory id cac obligatory,
             p_gjahr like coep-gjahr memory id gjr default sy-datum(4)
                                                   obligatory,
             p_perio like coep-perio memory id bpe obligatory,
             p_versn like coep-versn  obligatory default '000',
             p_wrttp like coep-wrttp  obligatory default '04' .
parameters   p_conf  no-display. " not used
parameters   p_test  no-display. " not used

select-options :
*                 S_MTART FOR T134-MTART MEMORY ID MTA
*                         OBLIGATORY
*                         NO INTERVALS,
                 s_aufnr for aufk-aufnr memory id anr
                         obligatory
                         no intervals
                         matchcode object zsh_co_io,
                 s_kstar for cska-kstar memory id kat,
*                         obligatory
*                         no intervals,
                 s_matnr for coep-matnr.

selection-screen end of block bl1.
selection-screen begin of block bl2 with frame title text-002.

parameters :
    p_report no-display, " not used
    p_frpost no-display. " not used

parameters : p_post no-display. " not used
parameters   p_only  as checkbox.
parameters : p_bpst as checkbox.

selection-screen end of block bl2.

* Layout
selection-screen begin of block b4 with frame title text-01s.
parameter p_vari type slis_vari.
selection-screen end of block b4.

selection-screen begin of block view-result with frame title text-t03.
selection-screen pushbutton  1(24) vslt user-command vslt.
selection-screen end of block view-result.

*&----------------------------------------------------------------------

* For Sub-Routine
include zaco16l_f001.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.
  __cls s_aufnr.

  s_aufnr = 'IEQCE001'.
  append s_aufnr.
  s_aufnr = 'IEQCP001'.
  append s_aufnr.

**// Mod. By Hyung Jin Youn 2004.08.05
* Do not use Default Values
* KSTAR, I/O, Material Type
*  PERFORM SELECT_INIT.
**// End of Mod.
* Set Material Type for Semi-Finished product and FSC
  perform set_mtype_fk.

* 11/06/2007 by IG.MOON {
  perform default_.
* }
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen output.
* Check Post+Report/Report
*  PERFORM IND_POST_N_REPORT.

at selection-screen.
  case sscrfields-ucomm.
* screen
    when 'VSLT'.
      perform view_.
  endcase.

at selection-screen on value-request for p_vari.
  perform alv_variant_f4 changing p_vari.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.
  clear g_error.

* Controlling Area Information
  perform read_tka01.
  check g_error eq space.
*  if p_post = space.

* commentted by IG.MOON
*    PERFORM CONF_REPLACE_DATA.

  perform alpha_kstar.
  perform read_io_data.
  check g_error eq space.

  perform read_b_f_data.
  check g_error eq space.

  perform cal_cost_ratio_02.
  check g_error eq space.

  perform make_pcc_post_table.
  check g_error eq space.

  perform adjustment_data.
  check g_error eq space.

* disabled by IG.MOON {
*  clear p_frpost. " by IG.MOON
*  if p_frpost = 'X'.
*    perform update_data_to_table .
*  endif.
* }

* disabled by IG.MOON {
*  clear p_post. " by IG.MOON
*  if p_post = 'X'.
*    perform post_data.
*  endif.
* }

  perform get_saved_data.
  perform move_out.

  check g_error eq space.
  if p_bpst eq false.
    perform set_output .
  else.

    data : total_lines type i, count type i.

    __cls $gt_out.
    gt_out-chk = true.
    modify gt_out transporting chk where chk eq space.

    $gt_out[] = gt_out[].

    describe table $gt_out lines total_lines.
    loop at $gt_out.
      $gt_out-line_no = sy-tabix.
      modify $gt_out.
    endloop.

* Start BAPI logic.
    read table $gt_out index 1.
    if sy-subrc eq 0.
      perform call_bapi
                            using text-p01
                                  total_lines
                         changing count.

      perform delete_table.
      perform insert_table.
* Update Doc. No.
*  perform update_doc_no.

      gt_out-chk = space.
      modify gt_out transporting chk where chk eq true.


    endif.
  endif.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.

*  loop at gt_return.
*    write:/ gt_return-id,
*            gt_return-number,
*            gt_return-message_v1.
*
*  endloop.
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form default_.
  write:
          icon_biw_report_view as icon to vslt,
         'View saved data' to vslt+4(21).

endform.                    " DEFAULT_
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_output.
  call screen 100.

endform.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  move_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_out.

  data: begin of $aufnr occurs 0,
          aufnr   type aufnr,
        end   of $aufnr.

  data: begin of $it_aufnr occurs 0,
          aufnr       type aufnr,
          verid       type verid,
          fsc_matnr   type matnr,
        end   of $it_aufnr.

  perform progress_ind using '97'
                             'Preparing output...'.

  __cls gt_out.

  loop at it_post.
    $aufnr-aufnr = it_post-pcc_aufnr.
    append $aufnr.
  endloop.

  sort $aufnr.
  delete adjacent duplicates from $aufnr.

  loop at $aufnr.
    $it_aufnr-aufnr = $aufnr-aufnr.
    perform  read_verid_from_pcc_new using $aufnr-aufnr
                          changing $it_aufnr-verid $it_aufnr-fsc_matnr.
    append $it_aufnr.clear $it_aufnr.
  endloop.

  sort it_post by kstar io_aufnr pcc_aufnr.
  sort $it_aufnr by aufnr.
  sort it_saved by io_aufnr matnr.

  loop at it_post.
    clear gt_out.
    move-corresponding it_post to gt_out.
    read table $it_aufnr with key aufnr = it_post-pcc_aufnr
                                  binary search.
    check sy-subrc eq 0.
    move : $it_aufnr-verid to gt_out-verid,
           $it_aufnr-fsc_matnr to gt_out-fsc_matnr.
    gt_out-kokrs  = p_kokrs.
    gt_out-gjahr  = p_gjahr.
    gt_out-period = p_perio.
    gt_out-versn  = p_versn.

    if gt_out-belnr eq space.
      read table it_saved with key io_aufnr = it_post-io_aufnr
                                   matnr    =  it_post-matnr
                                   binary search.
      if sy-subrc eq 0.
        gt_out-belnr = it_saved-belnr.
      endif.
    endif.

    if p_only eq true and gt_out-belnr ne space.
      continue.
    endif.

    if gt_out-belnr ne space.
      gt_out-icon = icon_led_green.
    endif.
    append gt_out.
  endloop.

endform.                    " move_out
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

  __catalog :
    'X'  'KSTAR'    'Cost el.'         10  'CHAR' '',
    'X'  'MATNR'    'Material'         18  'CHAR' '',
    'X'  'STYPE'    'Source Type'       5  'CHAR' '',
    'X'  'IO_AUFNR'  'Order#'          12  'CHAR' '',
    'X'  'PCC_AUFNR'   'Order#'        12  'CHAR' '',
    ' '  'FSC_MATNR'   'FSC'           18  'CHAR' '',
    ' '  'CHG_WKGBTR'  'Amount'        15  'CURR' '',
    ' '  'MEINB'       'UOM'            3  'CHAR' '',
    ' '  'MBGBTR'      'Quantity'      12  'QUAN' '',
    ' '  'BELNR'       'Doc. Number'   10  'CHAR' '',
    ' '  'ICON'        'flg'            3  'ICON' '',
    ' '  'REMARKS'     'Remarks'       40  'CHAR' ''.

  loop at gt_fcat into gs_fcat.
    if gs_fcat-fieldname eq 'CHG_WKGBTR' or
        gs_fcat-fieldname eq 'RATE_CHILD' or
        gs_fcat-fieldname eq 'KSTAR_RATE'.
      gs_fcat-just = 'R'.
      gs_fcat-no_zero = 'X'.
    endif.

    if gs_fcat-fieldname eq 'CHG_WKGBTR'.
      gs_fcat-just = 'R'.
      gs_fcat-no_zero = 'X'.
      gs_fcat-cfieldname = 'WAERS'.
    endif.

    if gs_fcat-fieldname eq 'MBGBTR'.
      gs_fcat-just = 'R'.
      gs_fcat-no_zero = 'X'.
      gs_fcat-qfieldname = 'MEINB'.
    endif.

    gs_fcat-ref_table = 'ZTCO_ABISPOST'.
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
             'KSTAR'        ' ' 'X' 'X' 'X' '',
*             'WERKS'        ' ' 'X' 'X' 'X' '',
             'MATNR'        ' ' 'X' 'X' 'X' '',
             'STYPE'        ' ' 'X' 'X' 'X' '',
             'IO_AUFNR'     ' ' 'X' 'X' 'X' '',
             'PCC_AUFNR'    ' ' 'X' 'X' 'X' ''.

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
endform.                    " set_lvc_layout
*&---------------------------------------------------------------------*
*&      Form  data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_data_changed  text
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
*&      Form  user_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form user_status.
  __cls ftab.

*  if g_grid->is_ready_for_input( ) eq 1.
*    ftab-fcode = 'SAVE'.
*    append ftab.
*    ftab-fcode = 'LOGV'.
*    append ftab.
*  endif.

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
      leave to screen 0.
    when 'EXIT'.
      leave program.
    when 'SAVE'.
      check sy-dynnr eq '0100'.
      perform really? using
      'This will post new primary costs!'.
      check g_error ne true.
      perform : apply_data,
                refresh_alv.
      __focus g_grid.
    when 'REVE'.
      check sy-dynnr eq '0100'.
      perform really? using
      'This will reverse Docs you selected!'.
      check g_error ne true.
      perform : reverse_data,
                refresh_alv.
      __focus g_grid.
    when 'LOGV'.
      call screen '300'.
    when 'SAVT'.
      check sy-dynnr eq '0100'.
      perform really? using
      'This will change the data!'.
      check g_error ne true.
      perform save_to_table.

    when 'SWITCH'.
      if sy-dynnr eq '0100'.
        perform switch_edit_mode.
      endif.
      __focus g_grid.

  endcase.


endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  REALLY?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form really? using p_text.
  data $exists(1).
  data l_answer(1).

  perform pop_up using
      p_text
      'Do you really want to proceed?' ' '
                 changing l_answer.

  if l_answer ne 'J'.
    g_error = true.
    message s000 with 'Processing was canceled by user.'.
  endif.

endform.                    " REALLY?
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
*&      Form  APPLY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form apply_data.

  data: lt_rows type lvc_t_row with header line,
        lt_row_no type lvc_t_roid. "Numeric IDs of Selected Rows
  data: l_line type i.

****************************> No Important
  data : total_lines type i,
         count type i.
****************************> end

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
    message e000 with 'Please select a data.'.
    exit.
  endif.

  perform get_posting_data tables lt_rows
                                  lt_row_no .

* read total lines
  describe table $gt_out lines total_lines.
  if total_lines > 0.

* Start BAPI logic.
    read table $gt_out index 1.
    if sy-subrc eq 0.
      perform call_bapi
                            using text-p01
                                  total_lines
                         changing count.
    endif.

    perform delete_table.
    perform insert_table.
* Update Doc. No.
*  perform update_doc_no.
  else.
    message s001 with 'Please select the postable line(s).' .
  endif .

  gt_out-chk = space.
  modify gt_out transporting chk where chk eq true.

endform.                    " APPLY_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_POSTING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*      -->P_LT_ROW_NO  text
*----------------------------------------------------------------------*
form get_posting_data tables pt_rows structure lvc_s_row
                             pt_row_no structure lvc_s_roid.

  __cls $gt_out .

  clear gt_out-chk.
  modify gt_out transporting chk where chk = 'X'.

  data : $io_aufnr like gt_out-io_aufnr,
         $matnr like gt_out-matnr.

* Selected Row by row selection
*  loop at pt_rows where rowtype is initial.
*    read table gt_out index pt_rows-index.
*    gt_out-chk = true .
*    $io_aufnr = gt_out-io_aufnr.
*    $matnr = gt_out-matnr.
*    modify gt_out transporting chk where io_aufnr = $io_aufnr
*                                     and matnr = $matnr.
*  endloop.


  loop at pt_rows where rowtype is initial.
    read table gt_out index pt_rows-index.
    gt_out-chk = true .
    modify gt_out index pt_rows-index .
  endloop.

  perform select_row_by_subtotal tables pt_rows .

  loop at gt_out where chk eq true.
    check not gt_out-chg_wkgbtr is initial.
    if ok_code ne 'REVE'.
      check gt_out-belnr eq space.
    endif.
    move-corresponding gt_out to $gt_out.
    move sy-tabix to $gt_out-line_no.
    append $gt_out. clear $gt_out.
  endloop.

endform.                    " GET_POSTING_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_ROWS  text
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
      modify gt_out.
    endloop.

  endloop.

endform.                    " SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI_INV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_INV  text
*      -->P_TEXT_P01  text
*      -->P_1007   text
*      -->P_1008   text
*      -->P_TOTAL_LINES  text
*      <--P_COUNT  text
*----------------------------------------------------------------------*
form call_bapi
                   using prog_text
                         p_total_lines
                changing p_count.


* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $prog_text(50),$current_cnt(10),$total_cnt(10),$text(30) .

* The number of Item Records should not be greater than 200
  data : it_post_idx like sy-tabix.
  data : lv_mod      like sy-tabix.
  data : lv_div      like sy-tabix.
  data : lv_from_idx like sy-tabix,
         lv_to_idx   like sy-tabix,
         lv_skip.

  __cls it_post_fin.

  loop at $gt_out.
    move-corresponding $gt_out to it_post_fin.

    append it_post_fin.
  endloop.

* Make one document per 200 records
  sort it_post_fin by io_aufnr pcc_aufnr matnr kstar.

* Read the number of Index of "IT_POST"
  describe table it_post_fin lines it_post_idx.
  lv_div = it_post_idx  div  200.
  lv_mod = it_post_idx  mod  200.
  if lv_mod > 0.
    lv_div = lv_div + 1.
  endif.

* CALL POST FM
  do lv_div times.

*------------------------------------------------------------*
*     Not important
*------------------------------------------------------------*
    add 1 to current_doc_cnt.
    $current_cnt = current_doc_cnt.
    $total_cnt = lv_div.
    concatenate $current_cnt '/' $total_cnt
    into $text.
    condense $text .
    concatenate prog_text $text into $prog_text.
    percentage = current_doc_cnt / p_total_lines * 100.
    perform show_progress using $prog_text percentage.
*------------------------------------------------------------*

* Check Index of IT_POST
* Cal. MOD. DIV.
    lv_to_idx   =  sy-index * 200 .
    lv_from_idx =  lv_to_idx - 199.
* From
    clear lv_skip.
    clear it_post_fin. read table it_post_fin index lv_from_idx.
    if sy-subrc <> 0. lv_skip = 'X'. endif.
* TO
    clear it_post_fin. read table it_post_fin index lv_to_idx.
    if sy-subrc <> 0. lv_to_idx = lv_from_idx + lv_mod - 1 . endif.

    if lv_skip <> 'X'.
* Run Post FM
      perform call_post_fm_res using  lv_from_idx lv_to_idx .
    endif.
  enddo.

endform.                    " CALL_BAPI_INV
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$PROG_TEXT  text
*      -->P_PERCENTAGE  text
*----------------------------------------------------------------------*
form show_progress using    pf_text
                            value(pf_val).
  call function 'SAPGUI_PROGRESS_INDICATOR'
       exporting
            percentage = pf_val
            text       = pf_text.

endform.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  reverse_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form reverse_data.

  data: lt_rows type lvc_t_row with header line,
        lt_row_no type lvc_t_roid. "Numeric IDs of Selected Rows
  data: l_line type i.

****************************> No Important
  data : total_lines type i,
         count type i.
****************************> end

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
    message e000 with 'Please select a data.'.
    exit.
  endif.

  perform get_posting_data tables lt_rows
                                  lt_row_no .

* read total lines
  describe table $gt_out lines total_lines.

* Start BAPI logic.
  read table $gt_out index 1.
  if sy-subrc eq 0.
    perform call_bapi_rev
                          using text-p01
                                total_lines
                       changing count.
  endif.

*  PERFORM DELETE_TABLE.
*  PERFORM INSERT_TABLE.

  gt_out-chk = space.
  modify gt_out transporting chk where chk eq true.

endform.                    " reverse_data
*&---------------------------------------------------------------------*
*&      Form  call_bapi_rev
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_P01  text
*      -->P_TOTAL_LINES  text
*      <--P_COUNT  text
*----------------------------------------------------------------------*
form call_bapi_rev
                   using prog_text
                         p_total_lines
                changing p_count.


* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $prog_text(50),$current_cnt(10),$total_cnt(10),$text(30) .

* The number of Item Records should not be greater than 200
  data : it_post_idx like sy-tabix.
  data : lv_mod      like sy-tabix.
  data : lv_div      like sy-tabix.
  data : lv_from_idx like sy-tabix,
         lv_to_idx   like sy-tabix,
         lv_skip.

  __cls it_post_fin.

  loop at $gt_out.
    check not $gt_out-belnr is initial.
    move-corresponding $gt_out to it_post_fin.
    clear : it_post_fin-kstar,
            it_post_fin-matnr,
            it_post_fin-io_aufnr,
            it_post_fin-pcc_aufnr,
            it_post_fin-chg_wkgbtr,
            it_post_fin-waers,
            it_post_fin-stype.
    append it_post_fin.
  endloop.

  sort it_post_fin by belnr.

  delete adjacent duplicates from it_post_fin comparing belnr.

  data:
        it_return        like mesg occurs 0 with header line,
        lv_doc_no        type co_belnr,
        count         type sytabix,
        error         type c,
        flag          type c,
        sy_tabix      type sytabix.

  clear : total_doc_cnt, current_doc_cnt.

  loop at it_post_fin.
    at new belnr.
      flag = true.
    endat.
    check flag eq true.
    clear flag.
    add 1 to total_doc_cnt.
  endloop.

  if total_doc_cnt > 0.
  else.
    message s000 with 'No Doc for reverse was found!'.
    exit.
  endif.

  data $message type bapi_msg.
  data $error.


  loop at it_post_fin.

    at new belnr.
      flag = true.
    endat.
    check flag eq true.
    clear flag.

*------------------------------------------------------------*
*     Not important
*------------------------------------------------------------*
    add 1 to current_doc_cnt.
    $current_cnt = current_doc_cnt.
    $total_cnt = total_doc_cnt.
    concatenate $current_cnt '/' $total_cnt
    into $text.
    condense $text .
    concatenate prog_text $text into $prog_text.
    percentage = current_doc_cnt / p_total_lines * 100.
    perform show_progress using $prog_text percentage.
*------------------------------------------------------------*

    perform post_rev_data tables   it_post_fin
                                   it_return
                          changing lv_doc_no
                                   error.

    clear : $message, $error.
    read table it_return with key msgty = 'E'.
    if sy-subrc eq 0.
      $error = true.
      $message = it_return-text.
    endif.

    clear  it_return.
    loop at it_return.
      if it_return-msgty = 'S' and $error eq false.
        $message = it_return-text.
      endif.

      gt_return = it_return.
      append gt_return.
    endloop.

* Save Doc. No.
    if not lv_doc_no is initial and $error eq false.

      gt_out-belnr = ''.
      gt_out-remarks = $message.
      gt_out-icon = icon_led_green.

      modify gt_out transporting belnr
icon remarks  where belnr eq it_post_fin-belnr .

      $gt_out-belnr = ''.
      $gt_out-remarks = $message.
      $gt_out-icon = icon_led_green.

      modify $gt_out transporting belnr
icon remarks  where belnr eq it_post_fin-belnr .

      it_post_fin-belnr =   lv_doc_no.
      modify it_post_fin.
      clear  it_post_fin.
    else.
      gt_out-remarks = $message.
      gt_out-icon = icon_led_red.
      modify gt_out transporting belnr
icon remarks  where belnr eq it_post_fin-belnr .
      it_post_fin-belnr =   lv_doc_no.
      modify it_post_fin.
      clear  it_post_fin.
    endif.

  endloop.

endform.                    " CALL_BAPI_INV
*&---------------------------------------------------------------------*
*&      Form  fill_rev_doc_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_POST_FIN  text
*      <--P_REV_DOC_HEADER  text
*----------------------------------------------------------------------*
form fill_rev_doc_header using    p_it_post_fin structure it_post_fin
                         changing rev_doc_header like bapidochdrr.

  rev_doc_header-co_area = p_it_post_fin-kokrs.
*  REV_DOC_HEADER-DOCDATE = SY-DATUM.
*  REV_DOC_HEADER-POSTGDATE = SY-DATUM.

  rev_doc_header-rvrs_no  = p_it_post_fin-belnr.
  rev_doc_header-username = sy-uname.
*  REV_DOC_HEADER-OBJ_TYPE = P_IT_POST_FIN-STYPE.

* reference document number of the reversed document ( = the same as the
* original CATS-document, since CATS has only the additional STOKZ)
*  REV_DOC_HEADER-OBJ_KEY_R = P_IT_POST_FIN-BELNR.

endform.                    " fill_rev_doc_header
*&---------------------------------------------------------------------*
*&      Form  post_rev_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_CATSCO_HELP  text
*      -->P_RETURN  text
*      <--P_DOC_NO  text
*      <--P_ERROR  text
*----------------------------------------------------------------------*
form post_rev_data tables   p_it_post_fin structure it_post_fin
                            messages      structure mesg
                   changing doc_no
                            error type c.

  data: rev_doc_header type bapidochdrr,
        rev_doc_no     type table of bapidochdrr with header line,
        return         type table of bapiret2 with header line."102865

  free: messages.
  clear: doc_no, error, messages.

* Fill reversal document header
  perform fill_rev_doc_header using    p_it_post_fin
                              changing rev_doc_header.

* Call reversal BAPI
  call function 'BAPI_ACC_ACT_POSTINGS_REVERSE'
       exporting
            doc_header = rev_doc_header
       tables
            doc_no     = rev_doc_no
            return     = return.

  read table rev_doc_no index 1.

  if sy-subrc = 0 and not rev_doc_no-doc_no is initial.
    doc_no = rev_doc_no-doc_no.

    perform update_table using rev_doc_header-rvrs_no.

  else.
    error = 'X'.
  endif.

  loop at return.
    messages-msgty = return-type.
    messages-arbgb = return-id.
    messages-txtnr = return-number.
    messages-text  = return-message.
    messages-msgv1 = return-message_v1.
    messages-msgv2 = return-message_v2.
    messages-msgv3 = return-message_v3.
    messages-msgv4 = return-message_v4.
    messages-zeile = return-row.
    append messages.
  endloop.

endform.                               " POST_REV_DATA
*&---------------------------------------------------------------------*
*&      Form  delete_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_table.

  loop at gt_out where chk eq true..
    delete from ztco_abispost
                        where kokrs  = gt_out-kokrs
                          and gjahr  = gt_out-gjahr
                          and period = gt_out-period
                          and versn  = gt_out-versn
                          and matnr  = gt_out-matnr.
  endloop.
  commit work.

endform.                    " delete_table
*&---------------------------------------------------------------------*
*&      Form  insert_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form insert_table.
*  SORT $GT_OUT BY KSTAR IO_AUFNR PCC_AUFNR.
  loop at gt_out where chk eq true.
    clear ztco_abispost.
    move-corresponding gt_out to ztco_abispost.
* Period
    ztco_abispost-kokrs  = gt_out-kokrs.
    ztco_abispost-gjahr  = gt_out-gjahr.
    ztco_abispost-period = gt_out-period.
    ztco_abispost-versn  = gt_out-versn.
* Log
    ztco_abispost-erdat  = sy-datum.
    ztco_abispost-erzet  = sy-uzeit.
    ztco_abispost-ernam  = sy-uname.
    insert ztco_abispost.
    if sy-subrc <> 0.
      rollback work.
      message s045.
      g_error = true.
    endif.
  endloop.
* Commit Work
  commit work and wait.
  message s000 with 'Data has been saved successfully'.
endform.                    " insert_table
*&---------------------------------------------------------------------*
*&      Form  VIEW_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form view_.
  clear g_error.
  perform read_tka01.
  check g_error eq space.

  __cls it_post.
*
  select * into corresponding fields of table   it_post
           from ztco_abispost
          where kokrs  = p_kokrs
            and gjahr  = p_gjahr
            and period = p_perio
            and versn  = p_versn
            and matnr  in s_matnr
            and io_aufnr in s_aufnr.
*            and belnr = space.

  perform move_out.
  check g_error eq space.
  perform set_output .

endform.                    " VIEW_
*&---------------------------------------------------------------------*
*&      Form  save_to_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_to_table.
  data: lt_rows type lvc_t_row with header line,
        lt_row_no type lvc_t_roid. "Numeric IDs of Selected Rows
  data: l_line type i.

****************************> No Important
  data : total_lines type i,
         count type i.
****************************> end

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
    message e000 with 'Please select a data.'.
    exit.
  endif.

  perform get_posting_data tables lt_rows
                                  lt_row_no .

* read total lines
  describe table $gt_out lines total_lines.

* Start BAPI logic.
  read table $gt_out index 1.
  check sy-subrc eq 0.
  perform delete_table.
  perform insert_table.

  gt_out-chk = space.
  modify gt_out transporting chk where chk eq true.

endform.                    " save_to_table
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

    write: /
           gt_return-message.

  endloop.

endform.                    " ERROR_LIST
*&---------------------------------------------------------------------*
*&      Form  get_saved_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_saved_data.

  __cls it_saved.
*
  select * into corresponding fields of table   it_saved
           from ztco_abispost
          where kokrs  = p_kokrs
            and gjahr  = p_gjahr
            and period = p_perio
            and versn  = p_versn
            and matnr  in s_matnr
            and io_aufnr in s_aufnr.

endform.                    " get_saved_data
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
    set pf-status '100'.
  else.
    call method g_grid->set_ready_for_input
                     exporting i_ready_for_input = 0.
    set pf-status '100'.
  endif.

  perform build_cell_attr.

endform.                    " SWITCH_EDIT_MODE
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
    if   ls_celltab-fieldname = 'CHG_WKGBTR'
      or ls_celltab-fieldname = 'MBGBTR'.
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

  data: lt_celltab type lvc_t_styl,
        ls_celltab type lvc_s_styl.

  clear lt_celltab.
  refresh lt_celltab.

  __cls gt_out-celltab.
  modify gt_out transporting celltab where belnr ne space.

  clear gs_fcat.

  loop at gt_fcat into gs_fcat.
    ls_celltab-fieldname = gs_fcat1-fieldname.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert ls_celltab into table lt_celltab.
  endloop.

  insert lines of lt_celltab into table gt_out-celltab.
  modify gt_out transporting celltab where belnr ne space.

endform.                    " BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*&      Form  set_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_color.

  clear: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  define __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  end-of-definition.

  __color :
            'KSTAR'      '1' 0,
            'MATNR'      '1' 0,
            'STYPE'      '1' 0,
            'IO_AUFNR'   '1' 0,
            'PCC_AUFNR'  '1' 0,
            'FSC_MATNR'  '2' 0,
            'CHG_WKGBTR' '3' 0,
            'MBGBTR'     '3' 0,
            'BELNR'      '4' 0,
            'ICON'       '4' 0,
            'REMARKS'    '4' 0.

  gt_out-tabcolor[] = gt_specialcol[].
  modify gt_out transporting tabcolor where tabcolor is initial.

endform.                    " set_color
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_table using p_belnr.

  update ztco_abispost set belnr = space
                where kokrs = p_kokrs
                  and belnr = p_belnr.

endform.                    " update_table
