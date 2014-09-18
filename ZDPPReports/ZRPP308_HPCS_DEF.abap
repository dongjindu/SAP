************************************************************************
* Program Name      : ZRPP308_HPCS_DEF
* Author            : Kim Gil Hyun (Tonkey)
* Creation Date     : 2003.10.22.
* Specifications By : B. Choi
* Pattern           : 2.1
* Development Request No : Local
* Type :
* Addl Documentation:
* Description       : HPC DEFINITION                     *
* Modification Logs
* Date       Developer    RequestNo    Description
* 10/29/13   Furong       Block from running
************************************************************************
report  ZRPP308_HPCS_DEF   message-id zmpp .

tables: ztbm_abxhpcdt,  "Assembly work spec by HPCS(CODE 1 - 300)
        ztbm_abxhpcdt01.  "Assembly work spec by HPCS(CODE 301 - 600)

include <icon>.
include <list>.
*****// ALV //**********************************************************
type-pools: slis.
data: gt_fieldcat type slis_t_fieldcat_alv,
      gt_fc       type slis_t_fieldcat_alv,
      g_fieldcat_s like line of gt_fieldcat,
      gs_layout   type slis_layout_alv,
      gs_print    type slis_print_alv,
      gt_sort     type slis_t_sortinfo_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_header   type slis_t_listheader,
      gt_header1  type slis_t_listheader,
      gt_colinfo_table type slis_t_specialcol_alv. "line color.

* hierarchy(simple)
data : g_tabname_header       type slis_tabname,       "header part
       g_tabname_item         type slis_tabname,       "detail list
       gs_keyinfo             type slis_keyinfo_alv.   "relation key

* return
data : g_exit_caused_by_caller  type c,
       gs_exit_caused_by_user   type slis_exit_by_user.

data: col_pos type i,
      cnt     type i,
      g_save  type c,
      g_repid like sy-repid,
      g_variant like disvariant.
data: gt_extab type slis_t_extab with header line.

data: begin of it_data occurs 0.
        include structure zsbm_abxhpcdt.
data: end of it_data.

data: begin of it_display occurs 0,
        line like sy-tabix,
        row type i,
        key(20),
        inf(30),
        col001(4),
        col002(4),
        col003(4),
        col004(4),
        col005(4),
        col006(4),
        col007(4),
        col008(4),
        col009(4),
        col010(4),
        col011(4),
        col012(4),
        col013(4),
        col014(4),
        col015(4),
        col016(4),
        col017(4),
        col018(4),
        col019(4),
        col020(4),
        col021(4),
        col022(4),
        col023(4),
        col024(4),
        col025(4),
        col026(4),
        col027(4),
        col028(4),
        col029(4),
        col030(4),
        infno(4),
      end of it_display.

field-symbols: <it_data> type any,
               <it_display> type any.

data: begin of reason_tab occurs 0,
  code   like ztbm_abxhpcdt-FSCC,
*        text   LIKE lfa1-name1,
end of reason_tab.


data : alv_grid               type ref to cl_gui_alv_grid,
       gs_custom_container    type ref to cl_gui_custom_container,
       wa_container           type scrfname value 'CONTAINER'.


selection-screen  begin of block blk1 with frame title text-001.
select-options : p_fscc for ztbm_abxhpcdt-FSCC,  "
                 p_part for ztbm_abxhpcdt-pqbg,  "
                 p_extc for ztbm_abxhpcdt-cext,  "
                 p_intc for ztbm_abxhpcdt-cint.  "
selection-screen  end of block blk1.

************************************************************************
initialization.
************************************************************************
  g_repid = sy-repid.


************************************************************************
start-of-selection.
************************************************************************
* 10/29/13   Furong       Block from running
  MESSAGE e009 with 'The program has been blocked'.
*
  select *
    into corresponding fields of table it_data
    from ztbm_abxhpcdt as a
           inner join ztbm_abxhpcdt01 as b
           on a~FSCC = b~FSCC  and
              a~pqbg = b~pqbg  and
              a~cext = b~cext  and
              a~cint = b~cint
    where
              a~fscc in p_fscc and
              a~pqbg in p_part and
              a~cext in p_extc and
              a~cint in p_intc .

  perform make_display_data.


************************************************************************
end-of-selection.
************************************************************************
  perform  build_events.
  perform  build_fieldcat    using  'IT_DISPLAY'.
  perform  build_layout      using  'X'   space   space.
* ALV FUNCTION CALL
  perform  start_grid_viewer tables  it_display.


************************************************************************
at selection-screen output.
************************************************************************



************************************************************************
at selection-screen.
***********************************************************************

*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_events.

  constants : c_pss type slis_formname value 'PF_STATUS_SET',
              c_uc  type slis_formname value 'USER_COMMAND'.
*              c_top type slis_formname value 'TOP_OF_PAGE'.
  refresh gt_events.

  call function 'REUSE_ALV_EVENTS_GET'
       exporting
            i_list_type = 0
       importing
            et_events   = gt_events.

  perform modify_gt_events
          tables  gt_events
          using : slis_ev_pf_status_set c_pss,
                  slis_ev_user_command  c_uc.
*                  slis_ev_top_of_page   c_top.

endform.                    " build_events
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0048   text
*----------------------------------------------------------------------*
form build_fieldcat using p_intab.

  clear   : gt_fieldcat, gt_fc.
  refresh : gt_fieldcat, gt_fc.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       exporting
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       changing
            ct_fieldcat        = gt_fc.

* IT_DISPLAY
  perform setting_fieldcat tables gt_fieldcat using :

                                  'S' 'KEY'         ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '15',
                                  'E' 'SELTEXT_M'   'KEY',

                                  'S' 'INF'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'S',
                                  ' ' 'OUTPUTLEN'   '20',
                                  'E' 'SELTEXT_L'   'INFORMATION',

                                  'S' 'ROW'         ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'S',
                                  ' ' 'OUTPUTLEN'   '03',
                                  'E' 'SELTEXT_L'   'ROW'.

  data: l_col_n(03) type n,
        l_col_name_c(06) type c.
  do 30 times.
    l_col_n = l_col_n + 1.
    concatenate 'COL' l_col_n into l_col_name_c.
    perform setting_fieldcat tables gt_fieldcat using :
                                    'S' l_col_name_c    ' ',
                                    ' ' 'JUST'          'M',
                                    ' ' 'DDICTXT'       'L',
                                    ' ' 'OUTPUTLEN'     '04',
                                    ' ' 'FIX_COLUMN'    '  ',
                                    'E' 'SELTEXT_M'     l_col_n.

  enddo.

endform.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_0108   text
*      -->P_0109   text
*      -->P_0110   text
*----------------------------------------------------------------------*
form setting_fieldcat tables   p_fieldcat like gt_fieldcat
                      using    p_gubun
                               p_field
                               p_value.

  data : l_col(40).

  field-symbols <fs>.

* START - FIELD ATTRIBUTE SETTING
  if p_gubun = 'S'.
    clear : g_fieldcat_s.
    read table gt_fc into g_fieldcat_s
                     with key fieldname  = p_field.
    exit.
  endif.

  concatenate 'G_FIELDCAT_S-' p_field  into l_col.
  assign (l_col) to <fs>.
  move   p_value to <fs>.

* END - FIELD ATTRIBUTE SETTING
  if p_gubun = 'E'.
    add 1 to cnt.
    g_fieldcat_s-col_pos = cnt.
    append g_fieldcat_s to p_fieldcat.
  endif.

endform.                    " setting_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_layout using p_cb p_color p_sum.

  clear gs_layout.

  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  gs_layout-colwidth_optimize = ' '.
  gs_layout-default_item      = 'X'.
* check box
  if p_cb = 'X'.
    gs_layout-box_fieldname    = 'CHKBOX'.
  endif.
* line color
  if p_color = 'X'.
    gs_layout-coltab_fieldname = 'COLOR'.
  endif.
* sum
  if p_sum = 'X'.
    gs_layout-totals_text       = 'TOT'.
  endif.
*
endform.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*      -->P_SLIS_EV_PF_STATUS_SET  text
*      -->P_C_PSS  text
*----------------------------------------------------------------------*
form modify_gt_events tables p_events_t like gt_events
                      using  p_form p_value.

  data: ls_event type slis_alv_event.

  read table  p_events_t  with key  name = p_form
                          into ls_event.
  if sy-subrc eq 0.
*    move     p_value     to   ls_event-form.
    modify   p_events_t  from ls_event index sy-tabix.
  endif.

endform.                    " modify_gt_events

*--------------------------------------------------------------------*
form start_grid_viewer tables p_intab.

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
*            i_background_id    = 'ALV_BACKGROUND' "HEADER? ??
            i_callback_program = g_repid
            is_layout          = gs_layout
            it_fieldcat        = gt_fieldcat[]
*            IT_SORT            = GT_SORT[]
            i_save             = 'A'
            is_variant         = g_variant
            it_events          = gt_events[]
            is_print           = gs_print
*            it_list_commentary = gt_header
       importing
            e_exit_caused_by_caller = g_exit_caused_by_caller
            es_exit_caused_by_user  = gs_exit_caused_by_user
       tables
            t_outtab           = p_intab.

*  if gs_exit_caused_by_user cs 'X'.
*    set screen 0.
*  endif.

endform.                    " start_grid_viewer
*&---------------------------------------------------------------------*
*&      Form  make_display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_display_data.
  data: l_tabix like sy-tabix.
  loop at it_data.
    l_tabix = sy-tabix.
    perform setting_key using l_tabix.
    perform input_information using l_tabix.
    perform setting_code using l_tabix.
  endloop.
endform.                    " make_display_data
*&---------------------------------------------------------------------*
*&      Form  SETTING_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form setting_key using p_tabix.
*   1st line
  clear it_display.
  move p_tabix to it_display-line.
  move 1 to it_display-row.
  move 'FULL SPEC CODE' to it_display-key.
  append it_display.
*   2nd line
  clear it_display.
  move p_tabix to it_display-line.
  move 2 to it_display-row.
  move 'TYPE' to it_display-key.
  append it_display.
*   3rd line
  clear it_display.
  move p_tabix to it_display-line.
  move 3 to it_display-row.
  move 'EXTERNAL COLOR' to it_display-key.
  append it_display.
*   4th line
  clear it_display.
  move p_tabix to it_display-line.
  move 4 to it_display-row.
  move 'INTERNAL COLOR' to it_display-key.
  append it_display.
*   5th line
  clear it_display.
  move p_tabix to it_display-line.
  move 5 to it_display-row.
  move 'MODEL YEAR' to it_display-key.
  append it_display.
*   6th line
  clear it_display.
  move p_tabix to it_display-line.
  move 6 to it_display-row.
  move 'NATION' to it_display-key.
  append it_display.
*   7th line
  clear it_display.
  move p_tabix to it_display-line.
  move 7 to it_display-row.
  move 'MI' to it_display-key.
  append it_display.
*   8th line
  clear it_display.
  move p_tabix to it_display-line.
  move 8 to it_display-row.
  move 'OCN' to it_display-key.
  append it_display.
*   9th line
  clear it_display.
  move p_tabix to it_display-line.
  move 9 to it_display-row.
  move 'VERSION' to it_display-key.
  append it_display.
*   10th line
  clear it_display.
  move p_tabix to it_display-line.
  move 10 to it_display-row.
  move 'MATERIAL STATUS' to it_display-key.
  append it_display.
*   11th line
  move ' ' to it_display-key.
  move p_tabix to it_display-line.
  move 11 to it_display-row.
  append it_display.
*   12th line
  move p_tabix to it_display-line.
  move 12 to it_display-row.
  move ' ' to it_display-key.
  append it_display.
*   13th line
  move p_tabix to it_display-line.
  move 13 to it_display-row.
  move ' ' to it_display-key.
  append it_display.
*   14th line
  move p_tabix to it_display-line.
  move 14 to it_display-row.
  move ' ' to it_display-key.
  append it_display.
*   15th line
  move p_tabix to it_display-line.
  move 15 to it_display-row.
  move ' ' to it_display-key.
  append it_display.
*   16th line
  move p_tabix to it_display-line.
  move 16 to it_display-row.
  move ' ' to it_display-key.
  append it_display.
*   17th line
  move p_tabix to it_display-line.
  move 17 to it_display-row.
  move ' ' to it_display-key.
  append it_display.
*   18th line
  move p_tabix to it_display-line.
  move 18 to it_display-row.
  move ' ' to it_display-key.
  append it_display.
*   19th line
  move p_tabix to it_display-line.
  move 19 to it_display-row.
  move ' ' to it_display-key.
  append it_display.
*   20th line
  move p_tabix to it_display-line.
  move 20 to it_display-row.
  move ' ' to it_display-key.
  append it_display.

endform.                    " SETTING_KEY
*&---------------------------------------------------------------------*
*&      Form  input_information
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form input_information using p_tabix.
  read table it_display with key line = p_tabix  "WORK ORDER
                                 row = 1 .
  move it_data-FSCC to it_display-inf.
  modify it_display index sy-tabix.

  read table it_display with key line = p_tabix  "PART
                                 row = 2 .
  move it_data-pqbg to it_display-inf.
  modify it_display index sy-tabix.

  read table it_display with key line = p_tabix  "EXT.COLOR
                                 row = 3 .
  move it_data-cext to it_display-inf.
  modify it_display index sy-tabix.

  read table it_display with key line = p_tabix  "INT.COLOR
                                 row = 4 .
  move it_data-cint to it_display-inf.
  modify it_display index sy-tabix.

  read table it_display with key line = p_tabix  "YEAR
                                 row = 5 .
  move it_data-fscc+00(01) to it_display-inf.
  modify it_display index sy-tabix.

  read table it_display with key line = p_tabix  "NATION
                                 row = 6 .
  move it_data-fscc+01(03) to it_display-inf.
  modify it_display index sy-tabix.

  read table it_display with key line = p_tabix  "MI
                                 row = 7 .
  move it_data-FSCC+06(08) to it_display-inf.
  modify it_display index sy-tabix.

  read table it_display with key line = p_tabix  "OCN
                                 row = 8 .
  move it_data-FSCC+14(04) to it_display-inf.
  modify it_display index sy-tabix.

  read table it_display with key line = p_tabix  "VERSION
                                 row = 9 .
  move it_data-FSCC+18(03) to it_display-inf.
  modify it_display index sy-tabix.

  read table it_display with key line = p_tabix  "STATUS
                                 row = 10 .
  move it_data-stat to it_display-inf.
  modify it_display index sy-tabix.

endform.                    " input_information
*&---------------------------------------------------------------------*
*&      Form  setting_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TABIX  text
*----------------------------------------------------------------------*
form setting_code using    p_tabix.
  data: l_col_n(03) type n.
  data: l_row type i.
  clear: l_col_n, l_row.

* MAKE IT_DISPLAY'S 20 LINES PER ONE RECORD OF IT_DATA.
  do 20 times.
    l_row = l_row + 1.
    perform read_display using p_tabix
                               l_row
                               l_col_n.
  enddo.

endform.                    " setting_code
*&---------------------------------------------------------------------*
*&      Form  fill_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COL_N  text
*----------------------------------------------------------------------*
form fill_code using    p_col_n
                        p_tabix.
  data: l_data_name(12),
        l_display_name(17),
        l_col_30(03) type n.
  clear l_col_30.
  do 30 times.
    p_col_n = p_col_n + 1.
    l_col_30 = l_col_30 + 1.
    concatenate 'IT_DATA-C' p_col_n into l_data_name.
    concatenate 'IT_DISPLAY-COL' l_col_30 into l_display_name.
    assign (l_data_name) to <it_data>.
    assign (l_display_name) to <it_display>.
    <it_display> = <it_data>.
  enddo.
  modify it_display index p_tabix.
endform.                    " fill_code
*&---------------------------------------------------------------------*
*&      Form  READ_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TABIX  text
*      -->P_L_ROW  text
*      -->P_L_COL_N  text
*----------------------------------------------------------------------*
form read_display using    p_tabix
                           p_row
                           p_col_n.
  data: l_tabix like sy-tabix.
  read table it_display with key line = p_tabix
                                 row  = p_row .
  l_tabix = sy-tabix.
* FILL IT_DISPLAY'S 30 COLUMNS.
  perform fill_code using p_col_n
                          l_tabix.

endform.                    " READ_DISPLAY
