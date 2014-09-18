************************************************************************
* Program Name      : YRPP207R_BODY_INPUT_PLAN_OLD
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2003.12.10.
* Specifications By : B. Choi
* Development Request No : UD1K902277
* Addl Documentation:
* Description       : Body Input Plan.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 02/11/2004 Tonkey       UD1K907246   Because of The Changed Process,
*                                      It's Copied from old's ZRPP207R.
*                                      So, This Program may not be used
*
*
************************************************************************

report  yrpp207r_body_input_plan_old
                                 message-id zmpp .
tables: ztpp_dvrt2,     "DVRT - Vehicle Master
        ztpp_status,    "Status ID Mapping Between Legarcy and SAP
        zvpp_capacity,  "Capacity Planning for the Work Center
        equi .          "Equipment master data
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
*      gt_header1  TYPE slis_t_listheader,
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
        include structure ztpp_dvrt2.
data: end of it_data.

data: begin of it_display occurs 0,
        work_order(18),
        qty_b01 type i,
        qty_d_a type i,
        qty_d_p type i,
        qty_a01 type i,
        qty_a02 type i,
        qty_a03 type i,
        qty_a04 type i,
        qty_a05 type i,
        qty_a06 type i,
        qty_a07 type i,
        qty_a08 type i,
        qty_a09 type i,
        qty_a10 type i,
        qty_tot type i,
        qty_a20 type i,
        qty_a21 type i,
      end of it_display.
data: begin of it_display_t occurs 0,
        wo_header type mara-matnr,
        wo_color type mara-matnr,
        qty_b01 type i,
        qty_d_a type i,
        qty_d_p type i,
        qty_a01 type i,
        qty_a02 type i,
        qty_a03 type i,
        qty_a04 type i,
        qty_a05 type i,
        qty_a06 type i,
        qty_a07 type i,
        qty_a08 type i,
        qty_a09 type i,
        qty_a10 type i,
        qty_tot type i,
        qty_a20 type i,
        qty_a21 type i,
      end of it_display_t.
data: begin of it_workday occurs 0,
        seq type p,
        day type d,
      end of it_workday .
field-symbols: <it_data> type any,
               <it_display> type any.

data : alv_grid               type ref to cl_gui_alv_grid,
       gs_custom_container    type ref to cl_gui_custom_container,
       wa_container           type scrfname value 'CONTAINER'.


selection-screen  begin of block blk1 with frame title text-001.

select-options: s_model  for ztpp_dvrt2-modl ,     "Model Name
                s_serial for ztpp_dvrt2-body_ser . "Body Serial
selection-screen begin of line.
selection-screen comment 1(31) text-002 for field p_date.
parameters: p_date  like sy-datum obligatory.
*SELECTION-SCREEN COMMENT (02) text-003 .
*PARAMETERS: p_date_h  LIKE sy-datum OBLIGATORY.    " RP01's Date
selection-screen end   of line.
selection-screen  end of block blk1.

************************************************************************
initialization.
************************************************************************
  g_repid     = sy-repid.
  p_date    = sy-datum.
*  p_date_h    = sy-datum.

************************************************************************
at selection-screen.
************************************************************************
  data: l_flag .
  perform check_holiday using    p_date
                        changing l_flag   .
  if l_flag <> space.
    message e000 with 'The date isn''t a working day!!!'.
    exit.
  endif.
************************************************************************
start-of-selection.
************************************************************************
  perform search_initial_data.
  perform set_initial_qty .
  perform make_display_data.

************************************************************************
end-of-selection.
************************************************************************
  perform  build_events.
  perform  build_fieldcat    using  'IT_DISPLAY'.
  perform  build_layout      using  'X'   space   space.
  perform  build_comment     using  gt_header[].
* ALV FUNCTION CALL
  perform  start_grid_viewer tables  it_display.
*
***********************************************************************
top-of-page.
***********************************************************************
  perform top_of_page.

*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       Building Events For ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_events.
  constants : c_pss type slis_formname value 'PF_STATUS_SET',
              c_uc  type slis_formname value 'USER_COMMAND',
              c_top type slis_formname value 'TOP_OF_PAGE'.
  refresh gt_events.

  call function 'REUSE_ALV_EVENTS_GET'
       exporting
            i_list_type = 0
       importing
            et_events   = gt_events.

  perform modify_gt_events
          tables  gt_events
          using :
*            slis_ev_pf_status_set c_pss,
*            slis_ev_user_command  c_uc,
            slis_ev_top_of_page   c_top.


endform.                    " build_events
*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       Modification of Events For ALV
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
    move     p_value     to   ls_event-form.
    modify   p_events_t  from ls_event index sy-tabix.
  endif.

endform.                    " modify_gt_events
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       Building Field Categories For ALV
*----------------------------------------------------------------------*
*      -->P_0278   text
*----------------------------------------------------------------------*
form build_fieldcat using p_intab.
*ztpp_dvrt2.
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

                                  'S' 'WORK_ORDER'  ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '18',
                                  'E' 'SELTEXT_L'   'W/O No.',

                                  'S' 'QTY_B01'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D - 1',

                                  'S' 'QTY_D_A'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D - A',

                                  'S' 'QTY_D_P'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D - P',

                                  'S' 'QTY_A01'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D + 1',

                                  'S' 'QTY_A02'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D + 2',

                                  'S' 'QTY_A03'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D + 3',

                                  'S' 'QTY_A04'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D + 4',

                                  'S' 'QTY_A05'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D + 5',

                                  'S' 'QTY_A06'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D + 6',

                                  'S' 'QTY_A07'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D + 7',

                                  'S' 'QTY_A08'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D + 8',

                                  'S' 'QTY_A09'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D + 9',

                                  'S' 'QTY_A10'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D +10',

                                  'S' 'QTY_TOT'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'TOTAL',

                                  'S' 'QTY_A20'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D +20',

                                  'S' 'QTY_A21'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D +21'.

endform.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       Setting Field Categories For ALV
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_0386   text
*      -->P_0387   text
*      -->P_0388   text
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
*       Building Layout For ALV
*----------------------------------------------------------------------*
*      -->P_0278   text
*      -->P_SPACE  text
*      -->P_SPACE  text
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
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       Running GRID Viewer
*----------------------------------------------------------------------*
*      -->P_IT_DISPLAY  text
*----------------------------------------------------------------------*
form start_grid_viewer tables p_intab.

*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_bypassing_buffer       = 'X'
*            i_background_id          = 'ALV_BACKGROUND' "HEADER? ??
            i_callback_program       = g_repid
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_top_of_page   = 'TOP_OF_PAGE'
*            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
*            IT_SORT                  = GT_SORT[]
            i_save                   = 'A'
            is_variant               = g_variant
            it_events                = gt_events[]
            is_print                 = gs_print
            it_list_commentary       = gt_header
       importing
            e_exit_caused_by_caller  = g_exit_caused_by_caller
            es_exit_caused_by_user   = gs_exit_caused_by_user
       tables
            t_outtab                 = p_intab.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " start_grid_viewer
*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       Building Comments For ALV
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
form build_comment using    p_gt_header type slis_t_listheader.
  data: ls_line type slis_listheader,
*        L_MANAGER(50),
        l_date(50),
        l_list(50),
        l_dsnam like t024d-dsnam,
        l_h_dsnam like t024d-dsnam,
*        l_total(5) TYPE i,
*        l_succ(5) TYPE i,
        l_ldate(10),
        l_hdate(10).

* Title
  clear ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-001.
  append ls_line to p_gt_header.
* Date
  ls_line-typ = 'S'.
  concatenate 'AS OF :'
              p_date
    into ls_line-info separated by ' '.
  append ls_line to p_gt_header.
* User
  ls_line-typ  = 'A'.
  ls_line-key  = 'User: '.
  ls_line-info = sy-uname.
  append ls_line to p_gt_header.

* today
  ls_line-typ  = 'A'.
  ls_line-key  = 'Today : '.
  ls_line-info = sy-datum .
  append ls_line to p_gt_header.


endform.                    " build_comment
*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
form set_status using rt_extab type slis_t_extab.
  set pf-status 'STATUS' excluding rt_extab.
endform.                    "
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
form top_of_page.
  call function 'REUSE_ALV_COMMENTARY_WRITE'
       exporting
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
            it_list_commentary = gt_header.

endform.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  search_initial_data
*&---------------------------------------------------------------------*
*       Searching Initial Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form search_initial_data.
  data: l_date_s type sy-datum,
        l_date_e type sy-datum,
        l_date_t type d,
        l_date_l(15),
        l_date_h(15),
        l_num type n,
        l_lines type i,
        l_flag ,
        l_tabix type sy-tabix.
***************************************
* Fix Working Days(D-1's ~ D+21's) .
***************************************
  clear: it_workday, it_workday[] .
  l_date_s = p_date - 1 .
  do .
    clear l_flag .
    perform check_holiday using    l_date_s
                          changing l_flag   .
    if l_flag <> space.
      l_date_s = l_date_s - 1.
    else.
      l_date_t = l_date_s .
*     D-1's Date.
      it_workday-seq = -1.
      it_workday-day = l_date_t .
      append it_workday.
      exit .
    endif.
  enddo.
  do .
    l_tabix = sy-tabix - 1 .
    l_date_t = l_date_t + 1.
    perform calculate_next_date using 'B'
                                      l_date_t .
    it_workday-seq = l_tabix.
    it_workday-day = l_date_t .
    append it_workday .
*   D+21's Date.
    if l_tabix = 21 .
      l_date_e = l_date_t.
      exit .
    endif.
  enddo.

  concatenate l_date_s '0000000'
    into l_date_l .
  concatenate l_date_e '9999999'
    into l_date_h .
  select *
    into corresponding fields of table it_data
    from ztpp_dvrt2
    where modl     in s_model  and
          body_ser in s_serial and
          status   < '01'      and
          b between l_date_l and l_date_h.

endform.                    " search_initial_data
*&---------------------------------------------------------------------*
*&      Form  set_initial_qty
*&---------------------------------------------------------------------*
*       Setting Initial Quantities
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_initial_qty.
  data: l_num type p,
        l_rp_date_d type d,
        l_rp_date_n(08) type n,
        l_date_n(08) type n.
  sort it_data by work_order extc intc rp01.
  clear: it_display_t[].
  loop at it_data.
    clear it_display_t .
    move it_data-work_order to it_display_t-wo_header .
    concatenate it_data-work_order it_data-extc it_data-intc
      into it_display_t-wo_color .
    move it_data-rp01+00(08) to l_rp_date_d.
    read table it_workday with key day = l_rp_date_d.
    if sy-subrc <> 0.
      continue.
    endif.
    case it_workday-seq.
      when -1 .
        it_display_t-qty_b01 = 1.
        it_display_t-qty_tot = 1.
      when 0.
        if it_data-rp01+14(01) = 'A'.
          it_display_t-qty_d_a = 1.
          it_display_t-qty_tot = 1.
        elseif it_data-rp01+14(01) = 'P'.
          it_display_t-qty_d_p = 1.
          it_display_t-qty_tot = 1.
        endif.
      when 1.
        it_display_t-qty_a01 = 1.
        it_display_t-qty_tot = 1.
      when 2.
        it_display_t-qty_a02 = 1.
        it_display_t-qty_tot = 1.
      when 3.
        it_display_t-qty_a03 = 1.
        it_display_t-qty_tot = 1.
      when 4.
        it_display_t-qty_a04 = 1.
        it_display_t-qty_tot = 1.
      when 5.
        it_display_t-qty_a05 = 1.
        it_display_t-qty_tot = 1.
      when 6.
        it_display_t-qty_a06 = 1.
        it_display_t-qty_tot = 1.
      when 7.
        it_display_t-qty_a07 = 1.
        it_display_t-qty_tot = 1.
      when 8.
        it_display_t-qty_a08 = 1.
        it_display_t-qty_tot = 1.
      when 9.
        it_display_t-qty_a09 = 1.
        it_display_t-qty_tot = 1.
      when 10.
        it_display_t-qty_a10 = 1.
        it_display_t-qty_tot = 1.
      when 20.
        it_display_t-qty_a20 = 1.
        it_display_t-qty_tot = 1.
      when 21.
        it_display_t-qty_a21 = 1.
        it_display_t-qty_tot = 1.
      when others .
        it_display_t-qty_tot = 1.
    endcase.
    append it_display_t.
  endloop.

endform.                    " set_initial_qty
*&---------------------------------------------------------------------*
*&      Form  make_display_data
*&---------------------------------------------------------------------*
*       Modification of Data For Display
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_display_data.
  clear: it_display, it_display[].
  loop at it_display_t.
    at new wo_header.
      clear it_display.
      sum.
      move it_display_t-wo_header to it_display-work_order.
      move-corresponding it_display_t to it_display.
      append it_display.
    endat.
    at new wo_color.
      clear it_display.
      sum.
      move it_display_t-wo_color to it_display-work_order.
      move-corresponding it_display_t to it_display.
      append it_display.
    endat.
  endloop.
endform.                    " make_display_data
*&---------------------------------------------------------------------*
*&      Form  check_holiday
*&---------------------------------------------------------------------*
*       Checking Holiday
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      <--P_L_FLAG  text
*----------------------------------------------------------------------*
form check_holiday using    p_date
                   changing p_flag.
  data: lt_holiday type iscal_day occurs 0 .
  data: l_line type i.
  clear: lt_holiday, lt_holiday[].
  call function 'HOLIDAY_GET'
   exporting
*     HOLIDAY_CALENDAR                 = ' '
     factory_calendar                 = 'HM'
     date_from                        = p_date
     date_to                          = p_date
*   IMPORTING
*     YEAR_OF_VALID_FROM               =
*     YEAR_OF_VALID_TO                 =
*     RETURNCODE                       =
    tables
      holidays                         = lt_holiday
*   EXCEPTIONS
*     FACTORY_CALENDAR_NOT_FOUND       = 1
*     HOLIDAY_CALENDAR_NOT_FOUND       = 2
*     DATE_HAS_INVALID_FORMAT          = 3
*     DATE_INCONSISTENCY               = 4
*     OTHERS                           = 5
            .
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  describe table lt_holiday lines l_line.
  if l_line > 0 .
    p_flag = 'X'.
  endif.
endform.                    " check_holiday
*&---------------------------------------------------------------------*
*&      Form  calculate_next_date
*&---------------------------------------------------------------------*
*       Calculating The Next Working Day
*----------------------------------------------------------------------*
*      -->P_1687   text
*      -->P_L_DATE_T  text
*----------------------------------------------------------------------*
form calculate_next_date using    p_arbpl
                                  p_date_t.
  data: l_ident type zvpp_capacity-kalid .
  select single kalid  into l_ident
    from zvpp_capacity
   where arbpl = p_arbpl .

  call function 'DATE_CONVERT_TO_FACTORYDATE'
       exporting
            correct_option      = '+'
            date                = p_date_t
            factory_calendar_id = l_ident
       importing
            date                = p_date_t
       exceptions
            ca.
endform.                    " calculate_next_date
