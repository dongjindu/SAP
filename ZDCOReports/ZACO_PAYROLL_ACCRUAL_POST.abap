************************************************************************
* Program Name      : ZACO_PAYROLL_ACCRUAL_POST
* Author            : Furong Wang
* Creation Date     : 02/16/2007
* Specifications By : Andy Choi
* Pattern           :
* Development Request No : UD1K930748
* Addl Documentation:
* Description       : Payroll accrual posting (by month)
*
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 12/21/2008 Rakesh       UD1K942481   Change of incentive bonus payment

* input  - accrual time, reference payroll data
* output - ztco_payacc

report zaco_payroll_accrual_post  message-id zmco.

*tables:   coss, cosp,coep, csks.
tables: t001, tka01, ska1.

ranges: r_cc    for  csks-kostl.
ranges: r_ce    for  coss-kstar.
ranges: p_perbl for sy-datum.
data:   t_cehier   like sethier   occurs 0 with header line.
data:   t_cevalues like setvalues occurs 0 with header line.

data:   begin of list_tab  occurs 0.
        include structure pna_ccdata.
data:     seqno like ppoix-seqno.
data:     run_id type p_evnum.
data:     fld_name(10) type c.
DATA:     bukrs TYPE bukrs.
DATA:     io TYPE c.
data:   end of list_tab.

data: list_table like list_tab occurs 0 with header line.

data:   rspar like rsparams occurs 10 with header line.

ranges: r_wt    for t512w-lgart,
        r_zgart for ztco_mh_time-zgart.

ranges: r_lgart1 for t512w-lgart,
        r_lgart2 for t512w-lgart,
        r_lgart5 for t512w-lgart,
        r_lgartx for t512w-lgart.


data:   ok_code like sy-ucomm,
        save_ok like sy-ucomm.

data: begin of it_pay_ref occurs 0,
       permo  like t549q-permo,
       kostl  like list_table-kostl,
       empct  type zempct,        "Employee type

*      pernr  like list_table-pernr,
       hkont  like list_table-hkont,   "G/L
       lgart  type lgart,              "wage type
       zgart  type zgart,

       tpbpe   like list_table-betrg,  "rate
       betrg  like list_table-betrg,   "Amt

       ref_hr like ztco_mhad-anz01,
       acc_hr like ztco_mhad-anz01,
      end of it_pay_ref.

data: begin of it_pay_ref_tot occurs 0,
       kostl   like list_table-kostl,
       empct   type zempct,  "pa0001-persk,  "Employee Subgroup
       basic_r like list_table-betrg,   "basic pay
       gross_r like list_table-betrg,   "gross pay
       basic   like list_table-betrg,   "basic pay
       gross   like list_table-betrg,   "gross pay
       basic_p type indprz,
       gross_p type indprz,
     end of it_pay_ref_tot.

data: begin of it_acc_mhad occurs 0,
       kostl   like ztco_mhad-kostl,
       lgart   like ztco_mhad-lgart,   "time type
       lgart2  like ztco_mhad-lgart2,  "wage type
       empct   type zempct,            "empl.type
*      persk   like pa0001-persk,      "Employee Subgroup

       acc_hr  like ztco_mhad-anz01,
       secr_hr like ztco_mhad-anz01,                  "2nd shift reg 1.0
       seco_hr like ztco_mhad-anz01,                  "2nd shift ot 1.5
       secs_hr like ztco_mhad-anz01,                  "2nd shift ot 2.0

       ref_hr like ztco_mhad-anz01,   "no use
      end of it_acc_mhad.

* GL / ZGART
*data: it_gl    like ztco_gl_ttype occurs 0 with header line.

* LGART / ZGART / Stat.indicator
data: it_lgart like ztco_mh_time occurs 0 with header line.

*data: begin of it_data occurs 0,
*    permo like t549q-permo,
*    KOSTL like list_table-KOSTL,
*    PHKONT like list_table-HKONT,
*    BETRG like list_table-BETRG,
*    ref_hr like ztco_mhad-anz01,
*    acc_hr like ztco_mhad-anz01,
*    plog(50),
*    end of it_data.
data: it_data     like table of ztco_payacc with header line.
data: it_data_sel like table of ztco_payacc with header line.

data: begin of it_kostl occurs 0,
        kostl like it_data-kostl,
        end of it_kostl.

*data: begin of it_data_sel occurs 0,
*    KOKRS like ztco_mhad-KOKRS,
*    GJAHR like ztco_mhad-GJAHR,
*    PERID like ztco_mhad-PERID,
*    permo like t549q-permo,
*    KOSTL like list_table-KOSTL,
*    PHKONT like list_table-HKONT,
*    BETRG like list_table-BETRG,
*    ref_hr like ztco_mhad-anz01,
*    acc_hr like ztco_mhad-anz01,
*    plog(50),
*    end of it_data_sel.

data: obj_type like bapiache02-obj_type,
      obj_key like bapiache02-obj_key,
      obj_sys like bapiache02-obj_sys,

      documentheader  like bapiache08,
      accountgl       like bapiacgl08  occurs 0 with header line,
      currencyamount  like bapiaccr08  occurs 0 with header line,
      return          like bapiret2    occurs 0 with header line,
      extension1      like bapiextc    occurs 0 with header line,

*      t_edidd         LIKE edidd       OCCURS 0 WITH HEADER LINE,
      bapi_retn_info  like bapiret2    occurs 0 with header line.

*data: begin of it_ct_empl occurs 0,
*    KOSTL like list_table-KOSTL,
*    PERNR like list_table-PERNR,
*    lgart like ztco_mhad-lgart,
*    ref_hr like ztco_mhad-anz01,
*    acc_hr like ztco_mhad-anz01,
*    end of it_ct_empl.

data:  w_firstday like sy-datum,
       w_lastday  like sy-datum,
       w_acc_pend like sy-datum.

data: gw_pabrj like t549q-pabrj,
      gw_pabrp like t549q-pabrp.

data: g_set_benefit(20) type c.

data: p_cnt type i,
      w_tot like it_data-betrg,
      w_saved(1).

types:  begin of s_ee,
         pernr     like pa0001-pernr,
         persg     like pa0001-persg,  "Employee Group
         persk     like pa0001-persk,  "Employee Subgroup
         schkz     like pa0007-schkz,  "Work Schedule Rule
         begda     like pa0001-begda,
         endda     like pa0001-endda,
         ptext     like t501t-ptext,
        end of s_ee.

data:   it_ee   type standard table of s_ee with header line.

data:   begin of it_cc_grp occurs 0,
          grp1      like zsco_labor-grp1,
          grp2      like zsco_labor-grp1,
          grp3      like zsco_labor-grp1.
        include structure setvalues.
data:   end of it_cc_grp.
data:   begin of it_schkz occurs 0,
         schkz       like pa0007-schkz,
         ptext(10),
        end of it_schkz.
data:   begin of it_persk occurs 0,
          persk      like pa0001-persk,
          ptext(10),
        end of it_persk.
data:   begin of it_persg occurs 0,
          persg      like pa0001-persg,
          ptext(10),
        end of it_persg.

*-Start of +UD1K942481
data: begin of it_cosp occurs 0,
        objnr  like cosp-objnr ,
        gjahr  like cosp-gjahr ,
        kstar  like cosp-kstar ,
        wtg001 like cosp-wtg001,
        wtg002 like cosp-wtg002,
        wtg003 like cosp-wtg003,
        wtg004 like cosp-wtg004,
        wtg005 like cosp-wtg005,
        wtg006 like cosp-wtg006,
        wtg007 like cosp-wtg007,
        wtg008 like cosp-wtg008,
        wtg009 like cosp-wtg009,
        wtg010 like cosp-wtg010,
        wtg011 like cosp-wtg011,
        wtg012 like cosp-wtg012,
      end of it_cosp           .

data:  l_wrttp like cosp-wrttp value '04'  ,
       l_versn like cosp-versn value '000' ,
       l_vrgng like cosp-vrgng value 'COIN'.

*-End of +UD1K942481
data:   l_lines type i.

data: w_posted(1).

* ALV
data:   wc_control        type        scrfname value 'CC_ALV',
        wc_alv            type ref to cl_gui_alv_grid,
        wc_container      type ref to cl_gui_custom_container.

* CLASS DECLARATION
class   lcl_event_receiver definition deferred. "ALV EVENT HANDLE

data :  event_receiver type ref to lcl_event_receiver.

* INTERNAL TABLES FOR ALV GRID
data : it_fieldcat     type lvc_t_fcat ,
       it_fieldname    type slis_t_fieldcat_alv,
       it_sort         type lvc_t_sort.

* VARIABLES FOR ALV GRID
data : ws_layout type lvc_s_layo,
       w_variant   type disvariant,          "for parameter IS_VARIANT
       w_fieldname like line of it_fieldcat,
       w_repid     like sy-repid,
       w_cnt       type i,                   "Field count
       w_save      type c   value 'A'.   "for Parameter I_SAVE

*-Start of +UD1K942481
data: it_ztco     like ztco_payacc_at occurs 0 with header line,
      it_data_tmp like table of ztco_payacc with header line.

data: begin of it_ztco_tmp occurs 0    ,
        kstar like ztco_payacc_at-kstar,
      end of it_ztco_tmp               .
*-End of +UD1K942481

*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

data: w_container(100),
      w_control(100),
      w_alv(100),
      w_itab(100),
      w_structure like dd02l-tabname.
*
*constants: c_structure(100) value 'ZSCO_AALA_REPORT_'.
*
*constants: user(5) type          c value 'USR',
*           manager(5) type       c value 'MAN',
*           administrator(5) type c value 'ADM'.
* Manager's mode or Administrator mode
*data: e_mode(3) type c.

****************************************************************
* LOCAL CLASSES: EVEN HANDLING
****************************************************************
class lcl_event_receiver definition.
  public section.

    methods:
    handle_data_changed
       for event data_changed of cl_gui_alv_grid
            importing er_data_changed.

*        double_click   for event double_click
*                         of cl_gui_alv_grid
*                         importing e_row
*                                   e_column
*                                   es_row_no.
    methods:
     handle_toolbar
         for event toolbar of cl_gui_alv_grid
             importing e_object e_interactive,

     handle_user_command
         for event user_command of cl_gui_alv_grid
             importing e_ucomm,

     menu_button
          for event menu_button of cl_gui_alv_grid
             importing e_ucomm.

    data: error_in_data type c.

    methods:

    handle_double_click
        for event double_click of cl_gui_alv_grid
            importing e_row
                      e_column
                      es_row_no.
endclass.

****************************************************************
* LOCAL CLASSES:IMPLEMENTATION
****************************************************************
class lcl_event_receiver implementation.
  method handle_data_changed.

    data: ls_good type lvc_s_modi,
          lv_value type lvc_value,
          flag type c,
          lvc_t_row type lvc_t_row.

    data: ls_toolbar  type stb_button.



    error_in_data = space.
    loop at er_data_changed->mt_good_cells into ls_good.

      if ls_good-fieldname = 'BETRG'.
        call method er_data_changed->get_cell_value
         exporting
            i_row_id  = ls_good-row_id
            i_fieldname = ls_good-fieldname
         importing
            e_value =   lv_value.
        call method er_data_changed->modify_cell
                exporting
                     i_row_id = ls_good-row_id
                     i_fieldname = ls_good-fieldname
                     i_value     = lv_value.
      endif.
    endloop.
  endmethod.

  method handle_double_click.
    perform dbl_click using e_column-fieldname
                                 es_row_no-row_id.

  endmethod.                           "handle_double_click
  method  handle_toolbar.
    data: ls_toolbar  type stb_button.
    loop at e_object->mt_toolbar into ls_toolbar.

    endloop.

  endmethod.
  method  handle_user_command.
    case  e_ucomm.
      when others.
        write :/ 'I am here'.
    endcase.
  endmethod.
  method  menu_button.
    case  e_ucomm.
      when others.
        write :/ 'I am here'.
    endcase.

  endmethod.

endclass.

*end include


data: g_perid       like coss-perbl.

selection-screen begin of block b1 with frame title text-001.
parameters     p_kokrs       like tka01-kokrs obligatory default 'H201'.
parameters     p_gjahr       like coss-gjahr  obligatory memory id gjr.
parameters:    p_adate  like t549s-pdate obligatory
                                         memory id pstng_date.

*ref payroll data
* table: t549s,
* molga = 10 (usa), permo = 4 (bi-weekly), datid = 4 (posting date)
parameters:    p_pdate  like t549s-pdate obligatory.
*               p_payper like t549q-vabrp obligatory,
*               p_payyr  like t549q-vabrj obligatory.

selection-screen end of block b1.

selection-screen begin of block b3 with frame title text-100.
parameters: p_calc  radiobutton group rd,
            p_read  radiobutton group rd.
parameters: p_calb    as checkbox.
parameters: p_rever   as checkbox.
selection-screen end of block b3.

selection-screen begin of block b2 with frame title text-002.
parameters     p_ksgru       like rksb1-ksgru.
select-options p_kostl       for  it_data-kostl memory id kos.
selection-screen end of block b2.

*selection-screen begin of block b4 with frame title text-003.
*parameters: p_cyc    as checkbox default 'X',
*            p_ex_acc as checkbox default 'X'. "Exclude Accruals
*
*parameters: p_detail as checkbox.
*selection-screen end of block b4.


parameters:    p_accacc  like ska1-saknr default '216000'.
select-options: s_bnfgl for ska1-saknr.   "benefit from gross pay
*parameters:    p_shfreg  like list_table-hkont default '601190'.
*parameters:    p_shfot   like list_table-hkont default '601195'.
parameters:    p_holpy   like ska1-saknr default '601230'.
parameters:    p_vaca    like ska1-saknr default '601240'.
parameters:    p_leave   like ska1-saknr default '601250'.

parameters:    p_saknr   like ska1-saknr default '0000601180'.

*at selection-screen output.

*at selection-screen .

at selection-screen on value-request for p_pdate.
  perform get_help_pdate.

at selection-screen on value-request for p_adate.
  perform get_help_adate.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.
  data: gv_level type c.
  get parameter id 'ZCOLV1' field gv_level.

*----------------------------------------------------------------------*
start-of-selection.

  perform check_option.

  perform read_ca.
  check sy-subrc = 0.

  perform read_cc.
  perform read_ce.
  perform read_cc_group.
  perform set_wagetype.

  perform make_perrange_month.
  perform make_perrange_pay_period.
  check sy-subrc = 0.

  perform get_acc_pay_period.
  check p_pdate < w_acc_pend.

* perform get_gl_mapping.

  if p_read = 'X'.
    perform get_saved_data.
  else.
    perform get_acc_hours.
    perform get_pay_ref using '04'.  "bi-weekly

    perform calc_acc_amt.
    perform calc_acc_leave.

    perform calc_basic_gross.
    perform calc_benefits.

    perform fill_ref_info.

    perform modify_acc.  " +UD1K942481
  endif.

end-of-selection.

  if p_calc = 'X' and p_calb = 'X'.
    perform batch_save.
  else.
    call screen '100'.
  endif.

*&---------------------------------------------------------------------*
*&      Form  make_perrange_month
*&---------------------------------------------------------------------*
*      CONVERT THE PERIOD INTO DATE RANGE
*----------------------------------------------------------------------*
form make_perrange_month.
  data: l_date like sy-datum.
  data: i_perio type i.
  data: l_month(02).

  g_perid = p_adate+4(2).

  p_perbl-sign   = 'I'.
  p_perbl-option = 'BT'.
  l_month = p_adate+4(2).  "perid+1(2).
  concatenate p_gjahr l_month '01' into l_date.
  p_perbl-low = l_date.
  w_firstday = l_date.
  call function 'RP_LAST_DAY_OF_MONTHS'
       exporting
            day_in            = l_date
       importing
            last_day_of_month = p_perbl-high.
  w_lastday = p_perbl-high.
*  append p_perbl.
*  clear p_perbl.
endform.                    " make_perrange
*&---------------------------------------------------------------------*
*&      Form  READ_CC
*&---------------------------------------------------------------------*
*     Read the cost center if the input is cost center group
*----------------------------------------------------------------------*
form read_cc.
  data: t_setlist like setlist occurs 0 with header line.
  data: t_sethier like sethier occurs 0 with header line.
  data: t_setvalues like setvalues occurs 0 with header line.

* CHECK THE COST CENTER VALUE INPUT
  if not p_kostl is initial.
    r_cc[] = p_kostl[].
    exit.
  endif.

  check not p_ksgru is initial.

  call function 'G_SET_LIST_SELECT'
       exporting
            setclass      = '0101'
            shortname     = p_ksgru
            kokrs         = p_kokrs
            ktopl         = tka01-ktopl
       tables
            matching_sets = t_setlist.
  if t_setlist[] is initial.
    message e002(sy) with 'Cost Center group does not exist'.
    exit.
  else.
    read table t_setlist index 1.
  endif.

  call function 'G_SET_TREE_IMPORT'
       exporting
            setid                     = t_setlist-setname
       tables
            set_hierarchy             = t_sethier
            set_values                = t_setvalues
       exceptions
            set_not_found             = 1
            illegal_field_replacement = 2
            illegal_table_replacement = 3
            others                    = 4.

  if sy-subrc <> 0.
    message e002(sy) with 'Cost Center group does not exist'.
    exit.
  endif.
* TRANSFER THE VALUE TO CC RANGE.
  r_cc-sign = 'I'.
  r_cc-option = 'BT'.
  loop at t_setvalues.
    r_cc-low  = t_setvalues-from.
    r_cc-high = t_setvalues-to.
    append r_cc.
  endloop.
  clear r_cc.
endform.                    " READ_CC
*&---------------------------------------------------------------------*
*&      Form  READ_CC_GROUP
*&---------------------------------------------------------------------*
*       read the cost center group hierarchy of HMMA1
*----------------------------------------------------------------------*
form read_cc_group.
  data: t_setlist like setlist occurs 0 with header line.
  data: t_sethier like sethier occurs 0 with header line.
  data: t_setvalues like setvalues occurs 0 with header line.
  data: l_group like rksb1-ksgru.
  data: l_beg type i, l_end type i.

  l_group = 'HMMA1'.
  call function 'G_SET_LIST_SELECT'
       exporting
            setclass      = '0101'
            shortname     = l_group
            kokrs         = 'H201'
            ktopl         = 'HNA1'
       tables
            matching_sets = t_setlist.
  if t_setlist[] is initial.
    message e002(sy) with 'Cost Center group does not exist'.
    exit.
  else.
    read table t_setlist index 1.
  endif.

  call function 'G_SET_TREE_IMPORT'
       exporting
            setid                     = t_setlist-setname
       tables
            set_hierarchy             = t_sethier
            set_values                = t_setvalues
       exceptions
            set_not_found             = 1
            illegal_field_replacement = 2
            illegal_table_replacement = 3
            others                    = 4.

  if sy-subrc <> 0.
    message e002(sy) with 'Cost Center group does not exist'.
    exit.
  endif.
* TRANSFER THE VALUE TO GROUP TABLE
  l_beg = 1.
  loop at t_sethier.
    if t_sethier-level = '1'.
      clear: it_cc_grp.
      it_cc_grp-grp1 = t_sethier-shortname.
    endif.

    if t_sethier-level = '2'.
      it_cc_grp-grp2 = t_sethier-shortname.
    endif.

    if t_sethier-level = '3'.
      it_cc_grp-grp3 = t_sethier-shortname.
    endif.

    if t_sethier-vcount ne 0.
      l_end = l_beg + t_sethier-vcount - 1.
      loop at t_setvalues from l_beg to l_end.
        l_beg = l_beg + 1.
        move-corresponding t_setvalues to it_cc_grp.
        append it_cc_grp.
      endloop.
    endif.
  endloop.

endform.                    " READ_CC_GROUP

*&---------------------------------------------------------------------*
*&      Form  READ_CE
*&---------------------------------------------------------------------*
*   read the cost element group for report
*----------------------------------------------------------------------*
form read_ce.
  data: t_setlist like setlist occurs 0 with header line.
  data: t_sethier like sethier occurs 0 with header line.
  data: t_setvalues like setvalues occurs 0 with header line.
  data: l_indh like sy-tabix,
        l_indv like sy-tabix,
        l_shortname(20) type c.

*Benefit set-id
  concatenate p_kokrs '_HR' into l_shortname.
  concatenate p_kokrs '_HR_20' into g_set_benefit.

  call function 'G_SET_LIST_SELECT'
       exporting
            setclass      = '0102'
            shortname     = l_shortname
            kokrs         = p_kokrs
            ktopl         = tka01-ktopl
       tables
            matching_sets = t_setlist.
  if t_setlist[] is initial.
    message e002(sy) with 'Cost element group does not exist'.
    exit.
  else.
    read table t_setlist index 1.
  endif.

  call function 'G_SET_TREE_IMPORT'
       exporting
            setid                     = t_setlist-setname
       tables
            set_hierarchy             = t_sethier
            set_values                = t_setvalues
       exceptions
            set_not_found             = 1
            illegal_field_replacement = 2
            illegal_table_replacement = 3
            others                    = 4.

  if sy-subrc <> 0.
    message e002(sy) with 'Cost element group does not exist'.
    exit.
  endif.
* TRANSFER THE VALUE TO CE RANGE.
  r_ce-sign = 'I'.
  r_ce-option = 'BT'.
  loop at t_setvalues.
    r_ce-low  = t_setvalues-from.
    r_ce-high = t_setvalues-to.
    append r_ce.
  endloop.
  clear r_ce.
* SET THE NODE NAME
  l_indh = l_indv = 1.

  loop at t_sethier.
    if t_sethier-vcount ne 0.
      l_indh = l_indv + t_sethier-vcount - 1.
      loop at t_setvalues from l_indv to l_indh.
        l_indv = l_indv + 1.
        t_setvalues-lfieldname = t_sethier-setid+8(20).
        modify t_setvalues.
      endloop.
    else.
      continue.
    endif.
  endloop.

  t_cehier[] = t_sethier[].
  t_cevalues[] = t_setvalues[].
endform.                    " READ_CE
*&---------------------------------------------------------------------*
*&      Form  get_pay_ref
*&---------------------------------------------------------------------*
*   read the labor cost data by standard program
*----------------------------------------------------------------------*
form get_pay_ref using p_ptype.
  data: lt_pay_ref like it_pay_ref occurs 0 with header line.

  perform make_selection.

  submit zrco_rpcpcc00_01
     with selection-table rspar
     and return.
*  IMPORT THE RESULT
  clear: list_table[].
  import list_table[] from memory id 'LIST'.
  free memory.

  perform get_ee_master.

  loop at list_table.
    lt_pay_ref-permo = p_ptype.
    lt_pay_ref-kostl = list_table-kostl.

*'A' US-Salary, 'B' US-Wage, 'K' KR-Salary
*    IF py_times-persg = '9'.
*      py_times-empct = 'K'.
*    ELSEIF py_times-zeity = '1'.
*      py_times-empct = 'B'.
*    ELSE.
*      py_times-empct = 'A'.
*    ENDIF.
    read table it_ee with key pernr = list_table-pernr binary search.
    perform get_emp_categ(zacop01) using it_ee-persg it_ee-persk
                                   changing lt_pay_ref-empct.

    lt_pay_ref-hkont = list_table-hkont.

*    if list_table-anzhl <> 0.
    lt_pay_ref-lgart  = list_table-lgart.  "wage type
*    endif.

    lt_pay_ref-ref_hr  = list_table-anzhl.
    lt_pay_ref-betrg   = list_table-betrg.

    collect lt_pay_ref.
  endloop.

  loop at lt_pay_ref into it_pay_ref.
    if it_pay_ref-ref_hr <> 0.
      it_pay_ref-tpbpe = it_pay_ref-betrg / it_pay_ref-ref_hr.
    else.
*      clear it_pay_ref-lgart.
    endif.

    read table it_lgart with key lgart = it_pay_ref-lgart
                        binary search.
    if sy-subrc = 0.
      it_pay_ref-zgart = it_lgart-zgart.
    endif.

    collect it_pay_ref.    clear it_pay_ref.
  endloop.

endform.                    " get_pay_ref
*&---------------------------------------------------------------------*
*&      Form  MAKE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_selection.
  clear: rspar, rspar[].
*COMPANY CODE
  rspar-selname = 'P_BUKRS'. rspar-kind = 'P'.
  rspar-low  = p_kokrs.
  append rspar.
  clear: rspar.
*PERIOD
  rspar-selname = 'P_BUDAT'.
  rspar-kind = 'S'.
  rspar-sign   = 'I'.
  rspar-option = 'EQ'.
  rspar-low  = p_pdate.
  append rspar.
  clear: rspar.

*COST CENTER
  loop at r_cc.
    rspar-selname = 'P_KOSTL'.
    rspar-kind    = 'S'.
    rspar-sign    = r_cc-sign.
    rspar-option  = r_cc-option.
    rspar-low     = r_cc-low.
    rspar-high    = r_cc-high.
    append rspar.
    clear: rspar.
  endloop.
*GENEAL LEDGER(COST ELEMENT)
  loop at r_ce.
    rspar-selname = 'P_HKONT'.
    rspar-kind    = 'S'.
    rspar-sign    = r_ce-sign.
    rspar-option  = r_ce-option.
    rspar-low     = r_ce-low.
    rspar-high    = r_ce-high.
    append rspar.
    clear: rspar.
  endloop.
*WAGE TYPE RADIO BUTTON
  rspar-selname = 'WTYPE2'.
  rspar-kind = 'P'.
  rspar-low  = 'X'.
  append rspar.
  clear: rspar.
*WAGE TYPE
*  LOOP AT R_WT.
*    RSPAR-SELNAME = 'r_lgart'.
*    RSPAR-KIND    = 'P'.
*    RSPAR-LOW     = R_WT-LOW.
*    APPEND RSPAR.
*    CLEAR: RSPAR.
*  ENDLOOP.
*EXCLUDING Accrual
  rspar-selname = 'P_EX_ACC'.
  rspar-kind = 'P'.
  rspar-low  =  'X'.
  append rspar.
  clear: rspar.

* add employee temporary
*  rspar-selname = 'P_PERNR'.
*  rspar-kind    = 'S'.
*  rspar-sign    = p_pernr-sign.
*  rspar-option  = p_pernr-option.
*  rspar-low     = p_pernr-low.
*  rspar-high    = p_pernr-high.
*  append rspar.
*  clear: rspar.

endform.                    " MAKE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  SET_WAGETYPE
*&---------------------------------------------------------------------*
*       Set the wage type, but code did not use this now
*----------------------------------------------------------------------*
form set_wagetype.
  refresh r_wt. clear r_wt.
  clear : r_lgart1, r_lgart1[],r_lgart2, r_lgart2[],
          r_lgart5, r_lgart5[].

  r_zgart-option  = 'EQ'. r_zgart-sign    = 'I'.
  r_zgart-low = '1'.  append r_zgart.  "regular
  r_zgart-low = '2'.  append r_zgart.  "ot
  r_zgart-low = '5'.  append r_zgart.  "paid leave
  r_zgart-low = '9'.  append r_zgart.  "holiday
  r_zgart-low = 'X'.  append r_zgart.  "shift-premium

  r_lgart1-option = 'EQ'. r_lgart1-sign   = 'I'.
  r_lgart2-option = 'EQ'. r_lgart2-sign   = 'I'.
  r_lgart5-option = 'EQ'. r_lgart5-sign   = 'I'.
  r_lgartx-option = 'EQ'. r_lgart5-sign   = 'I'.
  r_wt-option     = 'EQ'. r_wt-sign       = 'I'.

  select * into table it_lgart
     from ztco_mh_time
     where zgart in r_zgart.
  sort it_lgart by lgart.

  loop at it_lgart.
    case it_lgart-zgart.
* Regular
      when '1'.
        r_lgart1-low = it_lgart-lgart. append r_lgart1.
        r_wt-low     = it_lgart-lgart. append r_wt.

* Over Time
      when '2'.
        r_lgart2-low = it_lgart-lgart. append r_lgart2.
        r_wt-low     = it_lgart-lgart. append r_wt.

* paid leave, holiday
      when '5' or '9'.
        r_lgart5-low = it_lgart-lgart. append r_lgart5.
        r_wt-low     = it_lgart-lgart. append r_wt.

      when 'X'.
        r_lgartx-low = it_lgart-lgart. append r_lgartx.
        r_wt-low     = it_lgart-lgart. append r_wt.
    endcase.

  endloop.

  clear: r_lgart1, r_lgart2, r_lgart5, r_lgartx.
endform.                    " SET_WAGETYPE
*&---------------------------------------------------------------------*
*&      Form  GET_EE_GROUP
*&---------------------------------------------------------------------*
*     read the emplyess information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form get_ee_group.
*  data: pt0007 like pa0007 occurs 0 with header line.
*  data: subrc  like sy-subrc.
*  data: wa_ee  type s_ee.
*  data: wa_eel type s_ee.
*  data: lt_ee  type standard table of s_ee.
*  data: lt_t501t like t501t occurs 0 with header line.
*  read table p_perbl index 1.
** READ THE EMPOLYEE GROUP AND SUBGROUP
*  select pernr persg persk
*   into corresponding fields of table it_ee
*   from pa0001
*   for all entries in list_table
*   where pernr = list_table-pernr and
*         begda le p_perbl-high     and
*         endda ge p_perbl-high     and
*         sprps ne 'X'.
*  sort it_ee by pernr.
*  delete adjacent duplicates from it_ee
*     comparing pernr.
*
** Check if terminated EE EXIST
*  loop at it_ee into wa_ee.
*    if wa_ee-persg = '5'.
**   READ THE PREVIOUS RECORD EE GROUP.
*      select pernr persg persk begda
*        into corresponding fields of table lt_ee
*        from pa0001
*        where pernr = wa_ee-pernr.
*      sort lt_ee by begda descending.
*      loop at lt_ee into wa_eel.
*        if wa_eel-persg ne '5'.
*          wa_ee-persg = wa_eel-persg.
*          modify it_ee from wa_ee.
*          exit.
*        endif.
*      endloop.
*      if wa_ee-persg ne '1' and
*         wa_ee-persg ne '9'.
*        wa_ee-persg = '1'.  "IF OTHER GROUP, CONSIDER AS GROUP '1'.
*        modify it_ee from wa_ee.
*      endif.
*    endif.
*  endloop.
*
** READ THE GROUP TEXT
*  select * into table lt_t501t
*    from t501t
*    for all entries in it_ee
*    where persg = it_ee-persg and
*          sprsl = 'EN'      .
*  loop at it_ee into wa_ee.
*    clear: lt_t501t.
*    read table lt_t501t with key persg = wa_ee-persg.
*    if sy-subrc eq 0.
*      wa_ee-ptext = lt_t501t-ptext.
*      modify it_ee from wa_ee.
*    endif.
*  endloop.
*
** READ EE WORK SCHEDULE RULE
*  loop at it_ee into wa_ee.
*    clear: pt0007, pt0007[].
*    call function 'HR_READ_INFOTYPE'
*         exporting
*              pernr     = wa_ee-pernr
*              infty     = '0007'
*              begda     = p_perbl-high
*              endda     = p_perbl-high
*         importing
*              subrc     = subrc
*         tables
*              infty_tab = pt0007.
*    if subrc ne 0.
*      message e000 with 'WORK SCHEDULE ERROR FOR' wa_ee-pernr.
*    else.
*      read table pt0007 index 1.
*      wa_ee-schkz = pt0007-schkz.
*      modify it_ee from wa_ee.
*    endif.
*  endloop.
*
*
*endform.                    " GET_EE_GROUP
" GET_SUPPORT_COST
*&---------------------------------------------------------------------*
*&      Form  DATA_SUMMARY
*&---------------------------------------------------------------------*
*       summarize the cost data by cost element EE group
*----------------------------------------------------------------------*
*form data_summary.
*  perform ce_summary.
*  perform group_summary.
*  perform get_text.
*endform.                    " DATA_SUMMARY

*&****ALV FORMS*********************************************************

*&---------------------------------------------------------------------*
*&      Form  DBL_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
form dbl_click using    p_e_column_fieldname
                        p_es_row_no_row_id.

endform.                    " DBL_CLICK
*&---------------------------------------------------------------------*
*&      Module  SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_alv output.

  if wc_container is initial.
    perform create_container .
    perform set_alv_layout .
    perform build_field_catalog .
    perform set_sort_total .
    perform start_alv_display.
    perform sssign_event.
  else.
    perform set_alv_layout .
    perform build_field_catalog .
    perform set_sort_total .
    perform refresh_display.
  endif.

endmodule.                 " SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_container.
  create object wc_container
         exporting container_name = 'CC_ALV'
         exceptions
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  endif.

  create object wc_alv
         exporting i_parent      = wc_container
                   i_appl_events = 'X'.

endform.                    " CREATE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_alv_layout.

  clear : ws_layout, w_variant.
*  WS_LAYOUT-ZEBRA           = 'X'.
  ws_layout-edit            = ' '.
  ws_layout-sel_mode        = 'A'.
  ws_layout-language        = sy-langu.
*  ws_layout-cwidth_opt      = 'X'.
  ws_layout-no_merging      = 'X'.
  ws_layout-no_keyfix       = 'X'.
*  ws_layout-ctab_fname      = 'CLRTB'.
  w_variant-report            = sy-repid.
  w_variant-username          = sy-uname.
* BUILD THE CELL COLOR
*  perform build_cell_color.

endform.                    " SET_ALV_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_cell_color.
*  data: ct type lvc_t_scol.
*  data: w_ct  like line of ct.
*  data: wa_dis type s_dis.
*
*  loop at it_dis into wa_dis.
*    w_ct-fname = 'TCOST'.
*    w_ct-color-col = '5'.
*    w_ct-color-int = '1'.
*    append w_ct to ct.
*    w_ct-fname = 'TOTCO'.
*    w_ct-color-col = '5'.
*    append w_ct to ct.
*    w_ct-fname = 'TOTBE'.
*    w_ct-color-col = '5'.
*    append w_ct to ct.
*    w_ct-fname = 'THOUR'.
*    w_ct-color-col = '5'.
*    append w_ct to ct.
*
*    wa_dis-clrtb = ct.
*    modify it_dis from wa_dis.
*  endloop.

endform.                    " BUILD_CELL_COLOR

*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_field_catalog.
  data: lw_itab type slis_tabname.
  data: wa_fc   like line of it_fieldcat.
  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  move: sy-repid to w_repid.
  lw_itab = 'ZTCO_PAYACC'.

  call function 'LVC_FIELDCATALOG_MERGE '
       exporting
            i_structure_name   = lw_itab
            i_bypassing_buffer = 'X'
       changing
            ct_fieldcat        = it_fieldcat.
* SET THE FIELD ATTRIBUTE
  loop at it_fieldcat into wa_fc.
*    IF wa_fc-fieldname = 'BETRG'.
    if wa_fc-fieldname <> 'POST_DOC'.
      wa_fc-edit = 'X'.
      modify it_fieldcat from wa_fc.
    endif.
  endloop.
endform.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SORT  text
*----------------------------------------------------------------------*
form set_sort_total .

  perform fill_sort_filed using:  '1' 'KOSTL' 'X' ' ' 'X',
                                  '2' 'FROCC' 'X' ' ' ' ',
                                  '3' 'TOCC'  'X' ' ' ' ',
                                  '4' 'GTEXT' 'X' ' ' ' ',
                                  '5' 'KTEXT' ' ' 'X' 'X',
                                  '6' 'KZTXT' 'X' ' ' ' '.
endform.                    " SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*&      Form  START_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
form start_alv_display.
  data: lw_dynnr   like   sy-dynnr.

  call method wc_alv->set_table_for_first_display
   exporting
             is_layout        = ws_layout
             i_save           = w_save
             is_variant       = w_variant
             i_default        = space
   changing  it_fieldcatalog  = it_fieldcat
             it_sort          = it_sort
             it_outtab        = it_data[].

* Enter---
  call method wc_alv->register_edit_event
                exporting
                   i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  call method wc_alv->register_edit_event
                exporting
                   i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  create object event_receiver.
  set handler event_receiver->handle_data_changed for wc_alv.
  set handler event_receiver->handle_double_click for wc_alv.

endform.                    " START_ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SSSIGN_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sssign_event.

endform.                    " SSSIGN_EVENT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'ZD1'.
  set titlebar 'ZD1'.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  save_ok = ok_code.
  case save_ok.
    when 'BACK' or 'CANC'.
      leave to screen 0.
    when 'EXIT'.
      leave program.
    when 'SAVE_CHG'.
      perform get_selected_rows.
      perform save_changes.
    when 'POST'.
      perform get_selected_rows.
      perform post_gl.
  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FILL_SORT_FILED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fill_sort_filed using p_spos
                           p_field
                           p_up
                           p_down
                           p_total.
  data: wa_sort like line of it_sort.

  wa_sort-spos = p_spos.
  wa_sort-fieldname  = p_field.
  wa_sort-up         = p_up.
  wa_sort-down       = p_down.
  wa_sort-subtot     = p_total.
  append wa_sort to it_sort.
endform.                    " FILL_SORT_FILED
*&---------------------------------------------------------------------*
*&      Form  make_test_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form make_test_data.
*  perform enter_test_data using : '0000022001' ' ' ' ' 'Active'
*                                  'Salary' 'Shift 1' '1' '200',
*                                  '0000022001' ' ' ' ' 'Active'
*                                  'Salary' 'Shift 2' '1' '200',
*                                  '0000022001' ' ' ' ' 'Active'
*                                  'Salary' 'Shift 3' '1' '200',
*                                  '0000022001' ' ' ' ' 'Active'
*                                  'Hourly' 'Shift 1' '1' '200'.
*
*
*endform.                    " make_test_data
*&---------------------------------------------------------------------*
*&      Form  enter_test_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2644   text
*      -->P_2645   text
*      -->P_2646   text
*      -->P_2647   text
*      -->P_2648   text
*      -->P_2649   text
*      -->P_2650   text
*      -->P_2651   text
*----------------------------------------------------------------------*
*form enter_test_data using    p_1 p_2 p_3 p_4 p_5 p_6 p_7 p_8.
*  wa_out-kostl = p_1.
*  wa_out-frocc = p_2.
*  wa_out-tocc = p_3.
*  wa_out-gtext = p_4.
*  wa_out-ktext = p_5.
*  wa_out-kztxt = p_6.
*  wa_out-zcunt = p_7.
*  wa_out-regul = p_8.
*  append wa_out to it_out.
*endform.                    " enter_test_data
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form refresh_display.
  call method wc_alv->refresh_table_display.
endform.                    " REFRESH_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  MAKE_OUT_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM MAKE_OUT_FORMAT.
*  DATA: WA_OUT1 TYPE S_OUT.
*  CLEAR: WA_OUT.
*  LOOP AT IT_OUT INTO WA_OUT1.
*    PERFORM INITAL_DIS_FORMAT USING:
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Standard',
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Shift 1',
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Shift 2',
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Shift 3',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Standard',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Shift 1',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Shift 2',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Shift 3',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Standard',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Shift 1',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Shift 2',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Shift 3'.
*
*  ENDLOOP.
** SORT
*  SORT IT_DIS BY KOSTL ASCENDING
*                 FROCC ASCENDING
*                 TOCC  ASCENDING
*                 GTEXT DESCENDING
*                 KTEXT DESCENDING
*                 KZTXT ASCENDING.
* DELETE ADJACENT DUPLICATES FROM IT_DIS
*    COMPARING KOSTL FROCC TOCC GTEXT KTEXT KZTXT.
*
*ENDFORM.                    " MAKE_OUT_FORMAT
*&---------------------------------------------------------------------*
*&      Form  INITAL_DIS_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_KOSTL  text
*      -->P_2756   text
*      -->P_2757   text
*      -->P_2758   text
*----------------------------------------------------------------------*
*FORM INITAL_DIS_FORMAT USING  P_1 P_2 P_3 P_4 .
*
*   WA_OUT-KOSTL = P_1.
*   WA_OUT-GTEXT = P_2.
*   WA_OUT-KTEXT = P_3.
*   WA_OUT-KZTXT = P_4.
*   APPEND WA_OUT TO IT_OUT2.
*ENDFORM.                    " INITAL_DIS_FORMAT
*&---------------------------------------------------------------------*
*&      Form  make_perrange_pay_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_perrange_pay_period.
  data: l_date like sy-datum.
  data: i_payper type i.

  clear: p_perbl, p_perbl[].
  p_perbl-sign   = 'I'.
  p_perbl-option = 'BT'.

  select single pabrj pabrp
   into (gw_pabrj, gw_pabrp)
    from t549s
    where molga = '10'    "usa
      and permo = '4'     "bi-weekly
      and datid = '4'     "post date
      and pdate = p_pdate.
  check sy-subrc = 0.

  select single begda endda
   into (p_perbl-low, p_perbl-high)
    from t549q
    where permo = '4'
      and pabrj = gw_pabrj    "p_payyr
      and pabrp = gw_pabrp.   "p_payper.

  append p_perbl.

  describe table p_perbl lines sy-index.
  if sy-index = 0.
    sy-subrc = 4.
  endif.
endform.                    " make_perrange_pay_period
*&---------------------------------------------------------------------*
*&      Form  get_acc_hours
*&---------------------------------------------------------------------*
form get_acc_hours.
  data: l_date like sy-datum value '99991231',
        l_anz_date like sy-datum,
        l_anz_date_last like sy-datum,
*       l_period like ztco_mhad-perid,
        l_day(2),
        l_text(30),
*       l_gjahr like ztco_mhad-gjahr,
        l_period_f like ztco_mhad-perid,
        l_gjahr_f like ztco_mhad-gjahr,
        l_period_t like ztco_mhad-perid,
        l_gjahr_t like ztco_mhad-gjahr,
        l_lgart like ztco_mhad-lgart.

  data: lt_mhad like table of ztco_mhad with header line.

  field-symbols : <fs>.

  l_gjahr_f = p_perbl-low+0(4).
  l_gjahr_t = p_perbl-high+0(4).
  l_period_f = p_perbl-low+4(2).
  l_period_t = p_perbl-high+4(2).

  select * into table lt_mhad
       from ztco_mhad as a
     where a~kokrs = p_kokrs
       and a~gjahr = p_gjahr
       and a~perid = g_perid
       and a~kostl in r_cc
*       and gjahr between l_gjahr_f and l_gjahr_t
*       and perid between l_period_f and l_period_t
       and lgart in r_zgart.

  sort lt_mhad by gjahr perid kostl pernr lgart.

  loop at lt_mhad.
    it_acc_mhad-kostl = lt_mhad-kostl.
    it_acc_mhad-empct = lt_mhad-empct.

*    read table it_ee with key pernr = lt_mhad-pernr binary search.
*    it_acc_mhad-persk  = it_ee-persk.

    it_acc_mhad-lgart  = lt_mhad-lgart.
    it_acc_mhad-lgart2 = lt_mhad-lgart2.  "wage type

** get acrual hrs
    l_anz_date = p_adate.
    l_anz_date_last = w_lastday.
    while l_anz_date <= l_anz_date_last.
      l_day = l_anz_date+6(2).

      concatenate 'LT_MHAD-ANZ' l_day into l_text.
      assign (l_text) to <fs>.
      it_acc_mhad-acc_hr = it_acc_mhad-acc_hr + <fs>.

*Hourly & 2nd shift
      if lt_mhad-anzsh > 1.  "it_ee-persk = 'U0'
        if lt_mhad-lgart = '1'.    " regular work
          it_acc_mhad-secr_hr = it_acc_mhad-secr_hr + <fs>.
        else.                      " ot / ...
          if lt_mhad-dtype = '1'.  " week day
            it_acc_mhad-seco_hr = it_acc_mhad-seco_hr + <fs>.
          else.
            it_acc_mhad-secs_hr = it_acc_mhad-secs_hr + <fs>.
          endif.
        endif.
      endif.

      l_anz_date = l_anz_date + 1.
    endwhile.

    collect it_acc_mhad. clear it_acc_mhad.
  endloop.



*
*
*  if p_detail = 'X' and gv_level = 'ADM'.
*
*    loop at it_pay_ref.
*      select single lgart into l_lgart
*        from ztco_gl_ttype
*        where hkont = it_pay_ref-phkont.
*** get reference hr
*      l_anz_date = p_perbl-low.
*      l_anz_date_last = p_perbl-high.
*      while l_anz_date < l_anz_date_last.
*        l_gjahr = l_anz_date+0(4).
*        l_period = l_anz_date+4(2).
*        l_day = l_anz_date+6(2).
*        read table lt_mhad with key gjahr = l_gjahr
*                                  perid = l_period
*                                  kostl = it_pay_ref-kostl
*                                  pernr = it_pay_ref-pernr
*                                  lgart = l_lgart.
*
*        concatenate 'LT_MHAD-ANZ' l_day into l_text.
*        assign (l_text) to <fs>.
*        it_pay_ref-ref_hr = it_pay_ref-ref_hr + <fs>.
*        l_anz_date = l_anz_date + 1.
*      endwhile.
*
*** get acrual hrs
*      l_anz_date = w_acc_start.
*      l_anz_date_last = w_lastday.
*
*      while l_anz_date < l_anz_date_last.
*        l_gjahr = l_anz_date+0(4).
*        l_period = l_anz_date+4(2).
*        l_day = l_anz_date+6(2).
*        read table lt_mhad with key gjahr = l_gjahr
*                                    perid = l_period
*                                    kostl = it_pay_ref-kostl
*                                    pernr = it_pay_ref-pernr
*                                    phkont = it_pay_ref-phkont.
*
*        concatenate 'LT_MHAD-ANZ' l_day into l_text.
*        assign (l_text) to <fs>.
*        it_pay_ref-acc_hr = it_pay_ref-acc_hr + <fs>.
*        l_anz_date = l_anz_date + 1.
*      endwhile.
*      if it_data-ref_hr <> 0.
*        it_pay_ref-betrg = it_pay_ref-betrg * ( it_pay_ref-acc_hr /
*                           it_pay_ref-ref_hr ).
*      endif.
*      modify it_pay_ref.
*      clear: it_pay_ref.
*    endloop.
*  else.
*** summary
*    loop at it_pay_ref.
*      move-corresponding it_pay_ref to it_data.
*      it_data-kokrs = p_kokrs.
*      it_data-gjahr = p_gjahr.
*      it_data-perid = p_perio.
*      collect it_data.
*      clear: it_pay_ref, it_data.
*    endloop.
*
*    sort it_data by kokrs gjahr perid permo kostl phkont.
*    loop at it_data.
*      select single lgart into l_lgart
*        from ztco_gl_ttype
*        where hkont = it_data-phkont.
*** get reference hr
*      l_anz_date = p_perbl-low.
*      l_anz_date_last = p_perbl-high.
*      while l_anz_date < l_anz_date_last.
*        l_gjahr = l_anz_date+0(4).
*        l_period = l_anz_date+4(2).
*        l_day = l_anz_date+6(2).
*        read table lt_mhad with key gjahr = l_gjahr
*                                  perid = l_period
*                                  kostl = it_data-kostl
*                                  lgart = l_lgart.
*
*        concatenate 'LT_MHAD-ANZ' l_day into l_text.
*        assign (l_text) to <fs>.
*        it_data-ref_hr = it_data-ref_hr + <fs>.
*        l_anz_date = l_anz_date + 1.
*      endwhile.
*
*** get acrual hrs
*      l_anz_date = w_acc_start.
*      l_anz_date_last = w_lastday.
*
*      while l_anz_date < l_anz_date_last.
*        l_gjahr = l_anz_date+0(4).
*        l_period = l_anz_date+4(2).
*        l_day = l_anz_date+6(2).
*        read table lt_mhad with key gjahr = l_gjahr
*                                    perid = l_period
*                                    kostl = it_data-kostl
*                                    phkont = it_data-phkont.
*
*        concatenate 'LT_MHAD-ANZ' l_day into l_text.
*        assign (l_text) to <fs>.
*        it_data-acc_hr = it_data-acc_hr + <fs>.
*        l_anz_date = l_anz_date + 1.
*      endwhile.
*
*      if it_data-ref_hr <> 0.
*        it_data-betrg = it_data-betrg * ( it_data-acc_hr /
*                            it_data-ref_hr ).
*      endif.
*      modify it_data.
*      clear: it_pay_ref.
*    endloop.
*
*  endif.
endform.                    " get_acc_hours
*&---------------------------------------------------------------------*
*&      Module  set_ALVobject  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_alvobject output.
  if wc_container is initial.
    perform create_container .
    perform set_alv_layout .
    perform build_field_catalog .
*    perform set_sort_total .
    perform start_alv_display.
    perform sssign_event.
  else.
    perform set_alv_layout .
    perform build_field_catalog .
    perform set_sort_total .
    perform refresh_display.
  endif.
endmodule.                 " set_ALVobject  OUTPUT
**&---------------------------------------------------------------------
*
**&      Form  CREATE_CONTAINER
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*form create_container.
*  create object wc_container
*         exporting container_name = 'CC_ALV'
*         exceptions
*          cntl_error = 1
*          cntl_system_error = 2
*          create_error = 3
*          lifetime_error = 4
*          lifetime_dynpro_dynpro_link = 5.
*
*  if sy-subrc ne 0.
*    w_repid = sy-repid.
*    call function 'POPUP_TO_INFORM'
*         exporting
*              titel = w_repid
*              txt2  = sy-subrc
*              txt1  = 'The control can not be created'.
*  endif.
*
*  create object wc_alv
*         exporting i_parent      = wc_container
*                   i_appl_events = 'X'.
*
*endform.                    " CREATE_CONTAINER
**&---------------------------------------------------------------------
*
**&      Form  SET_ALV_LAYOUT
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*form set_alv_layout.
*
*  clear : ws_layout, w_variant.
**  WS_LAYOUT-ZEBRA           = 'X'.
*  ws_layout-edit            = ' '.
*  ws_layout-sel_mode        = 'A'.
*  ws_layout-language        = sy-langu.
*  ws_layout-cwidth_opt      = 'X'.
*  ws_layout-no_merging      = 'X'.
*  ws_layout-no_keyfix       = 'X'.
*  ws_layout-ctab_fname      = 'CLRTB'.
*  w_variant-report            = sy-repid.
*  w_variant-username          = sy-uname.
** BUILD THE CELL COLOR
*  perform build_cell_color.
*
*endform.                    " SET_ALV_LAYOUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit input.

  case sy-ucomm.
    when 'EXIT' .
      clear: sy-ucomm.
      leave to screen 0.
  endcase.

endmodule.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_selected_rows.

  data: lt_rows type lvc_t_row with header line,
           lt_row_no type lvc_t_roid. "/Numeric IDs of Selected Rows
  data: l_line type i.
  refresh: it_data_sel, it_kostl.
  clear it_data_sel.
  call method wc_alv->get_selected_rows
           importing et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  call method cl_gui_cfw=>flush.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    exit.
  endif.
  read table lt_rows index 1.
  if sy-subrc ne 0.
    message e000(zz) with text-m12.
  else.
    loop at lt_rows where index ne 0.
      read table it_data index lt_rows-index.
      if sy-subrc eq 0.
        move-corresponding it_data to it_kostl.
        collect it_kostl.
      endif.
    endloop.
    loop at it_kostl.
      loop at it_data where kostl = it_kostl-kostl.
        it_data_sel = it_data.
        it_data_sel-mandt = sy-mandt.
        append it_data_sel.
      endloop.
    endloop.
  endif.
endform.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  post_changes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_changes.

  data: lt_temp like table of it_data_sel with header line.
  data: l_answer(1).

  perform check_gl_posted.

  if w_posted is initial.
    select * into table lt_temp
     from ztco_payacc
     for all entries in it_kostl
     where kokrs = p_kokrs
       and gjahr = p_gjahr
       and perid = g_perid
       and kostl = it_kostl-kostl.
    if not lt_temp[] is initial.
      call function 'POPUP_TO_CONFIRM'
        exporting
*   TITLEBAR                    = ' '
*   DIAGNOSE_OBJECT             = ' '
  text_question               = 'Do you want to overwrite existed data?'
         text_button_1               = 'Yes'
*   ICON_BUTTON_1               = ' '
         text_button_2               = 'No'
*   ICON_BUTTON_2               = ' '
*   DEFAULT_BUTTON              = '1'
*   DISPLAY_CANCEL_BUTTON       = 'X'
*   USERDEFINED_F1_HELP         = ' '
*   START_COLUMN                = 25
*   START_ROW                   = 6
*   POPUP_TYPE                  =
       importing
         answer                 = l_answer
* TABLES
*   PARAMETER                   =
* EXCEPTIONS
*   TEXT_NOT_FOUND              = 1
*   OTHERS                      = 2
                .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.

      if l_answer <> '1'.
        exit.
      endif.
    endif.

    delete ztco_payacc from table it_data_sel.
    insert ztco_payacc from table it_data_sel.
    if sy-subrc = 0.
      commit work.
      message i000 with 'Data has been saved'.
      w_saved = 'X'.
    else.
      rollback work.
      clear w_saved.
    endif.
  endif.
endform.                    " post_changes
*&---------------------------------------------------------------------*
*&      Form  get_saved_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_saved_data.
  data: lt_data like table of it_data with header line.
  select * into corresponding fields of table lt_data
    from ztco_payacc
    where kokrs   = p_kokrs
     and  gjahr   = p_gjahr
     and  perid =  g_perid
     and  kostl   in r_cc.
*     and  actual =  p_ex_acc.

  check sy-subrc eq 0.
  refresh it_data.
  loop at lt_data.
    move-corresponding lt_data to it_data.
    append it_data.
  endloop.

endform.                    " get_saved_data
*&---------------------------------------------------------------------*
*&      Form  check_gl_posted
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_gl_posted.
  data: lw_payacc like ztco_payacc,
        l_stblg like bkpf-stblg,
        l_stgrd like bkpf-stgrd.
  ranges: lr_cc    for  csks-kostl.

  clear: w_posted.
  refresh: lr_cc.
  lr_cc-option = 'EQ'.
  lr_cc-sign = 'I'.

  sort it_data_sel by kostl.

  loop at it_data_sel.
    at end of kostl.
      select single * into lw_payacc
       from ztco_payacc
       where kokrs = p_kokrs
         and gjahr = p_gjahr
         and perid = g_perid
         and kostl = it_data_sel-kostl
         and belnr <> ' '.

      if sy-subrc = 0.
        select single stblg stgrd into (l_stblg, l_stgrd)
         from bkpf
         where bukrs = p_kokrs
           and belnr = lw_payacc-belnr
           and gjahr = p_gjahr.

        if l_stblg = ' '.  "<> ' '
          w_posted = 'X'.
          message w000 with 'Document was Posted already -  '
                            it_data_sel-kostl.
          lr_cc-low = it_data_sel-kostl. append lr_cc.
          exit.
        endif.
      endif.

    endat.
  endloop.

  describe table lr_cc lines sy-index.
  check sy-index > 0.
  delete it_data_sel where kostl in lr_cc.

endform.                    " check_gl_posted
*&---------------------------------------------------------------------*
*&      Form  POST_GL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_gl.
*  data: l_rc(1).
  clear: w_posted.
  if p_read = 'X' or w_saved = 'X'.

    perform check_gl_posted.
    if w_posted is initial.
      sort it_data_sel by kostl empct hkont lgart.

      loop at it_data_sel.
        at new kostl.
          clear: p_cnt,w_tot.
          perform fill_bapi_header using '1'.
        endat.

        perform fill_bapi_line using '1'.

        at end of kostl.
          if w_tot > 0.
            perform fill_bapi_line_exp.
            perform call_bapi_post.
            perform update_log using it_data_sel-kostl '1'.

*-----not working to link reversal document...
            if sy-subrc = 0 and p_rever = 'X'.

              perform fill_bapi_header using '2'.
              perform fill_bapi_line_reversal.
              perform call_bapi_post.
              perform update_log using it_data_sel-kostl '2'.
            endif.
          endif.
        endat.
      endloop.
**** reverse posting
*      loop at it_data_sel.
*        at new kostl.
*          clear: p_cnt,w_tot.
*
*          refresh : accountgl, currencyamount, return, extension1.
*          perform fill_bapi_header using '2'.
*        endat.
*        perform fill_bapi_line using '2'.
*        at end of kostl.
*          if w_tot < 0.
*            perform fill_bapi_line_exp.
*            perform call_bapi_post.
*            perform update_log using it_data_sel-kostl '2'.
*          endif.
*        endat.
*      endloop.

    endif.
  else.
    message i000 with 'Please save the data before posting'.
  endif.

endform.                    " POST_GL
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fill_bapi_header using p_type..
  if p_type = '1'.
    refresh : accountgl, currencyamount, return, extension1.
  endif.

  clear : documentheader.

*Default: BKPFF: Actg doc.direct inpt
*IDOC reverse -> From original system
*  documentheader-obj_type       = 'Y001'. "Default:BKPFF
*  documentheader-obj_key        = 'CHOI'.


*  documentheader-obj_sys        = g_objsys.
  documentheader-username       = sy-uname.
  documentheader-comp_code      = p_kokrs.

*-+ by ig.moon 4/2/2008 {
*  documentheader-doc_type       = 'PY'.
  documentheader-doc_type       = 'AC'.
* }


*documentheader-ac_doc_no      = space.
  documentheader-doc_date       = w_lastday.
  documentheader-trans_date     = sy-datum.
  documentheader-header_txt     = it_data_sel-kostl.
  concatenate 'PY ACC/' gw_pabrj '-' gw_pabrp
         into documentheader-ref_doc_no.
  if p_type = '1'.
    documentheader-pstng_date     = w_lastday.
  else.
*    if obj_key is initial.
*       wait up to '0.5' seconds.
*    endif.
*    documentheader-obj_key_r      = obj_key.
    documentheader-pstng_date     = w_lastday + 1.
    documentheader-reason_rev     = '05'.
  endif.

  documentheader-fisc_year      = documentheader-pstng_date(4).
  documentheader-fis_period     = documentheader-pstng_date+4(2).

*lineitem currency
  currencyamount-currency    = t001-waers.

endform.                    " fill_bapi_header
*&---------------------------------------------------------------------*
*&      Form  FILL_BAPI_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fill_bapi_line using p_type.
  data: l_amt like bapiaccr08-amt_doccur,
        l_act like ska1-saknr,
        l_hr(10) type c,
        l_rt(08) type c.

*  UD1K940532 by IG.MOON 5/11/2007
*  if it_data_sel-betrg <> 0.
  if it_data_sel-acc_amt <> 0.
*  END
    if p_type = '1'.
      l_amt = + it_data_sel-acc_amt.
    else.
      l_amt = - it_data_sel-acc_amt.
    endif.
    l_hr = it_data_sel-acc_hr.
    l_rt = it_data_sel-tpbpe.
    concatenate l_hr '*' l_rt
           into accountgl-item_text.
    concatenate it_data_sel-empct '-' it_data_sel-lgart
           into accountgl-alloc_nmbr.

    p_cnt = p_cnt + 1.
    move: p_cnt        to accountgl-itemno_acc,
          p_cnt        to currencyamount-itemno_acc,
          it_data_sel-hkont to accountgl-gl_account,
          it_data_sel-kostl to accountgl-costcenter,
          l_amt        to currencyamount-amt_doccur.
    append: accountgl, currencyamount.

    w_tot = w_tot + l_amt.
* 2nd lineitem
*  p_cnt = p_cnt + 1.
*  l_amt = - l_amt.
*  MOVE: p_cnt        TO accountgl-itemno_acc,
*        p_cnt        TO currencyamount-itemno_acc,
*        l_act        TO accountgl-gl_account,
*        l_amt        TO currencyamount-amt_doccur.
*  APPEND: accountgl, currencyamount.
  endif.
endform.                    " FILL_BAPI_LINE
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_line_exp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fill_bapi_line_exp.

* change sign
  w_tot = - w_tot.

  accountgl-item_text  = documentheader-ref_doc_no.

  p_cnt = p_cnt + 1.
  move: p_cnt             to accountgl-itemno_acc,
        p_cnt             to currencyamount-itemno_acc,
        p_accacc          to accountgl-gl_account,
        it_data_sel-kostl to accountgl-alloc_nmbr,
        w_tot             to currencyamount-amt_doccur.

  clear: accountgl-costcenter.

  append: accountgl, currencyamount.


endform.                    " fill_bapi_line_exp
*&---------------------------------------------------------------------*
*&      Form  call_bapi_post
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_RC  text
*----------------------------------------------------------------------*
form call_bapi_post.

*  IF p_run = space.
*    CALL FUNCTION 'BAPI_ACC_GL_POSTING_CHECK'
*         EXPORTING
*              documentheader = documentheader
*         TABLES
*              accountgl      = accountgl
*              currencyamount = currencyamount
*              return         = return
*              extension1     = extension1.
*  ELSE.

  call function 'BAPI_ACC_GL_POSTING_POST'
       exporting
            documentheader = documentheader
       importing
            obj_type       = obj_type
            obj_key        = obj_key
            obj_sys        = obj_sys
       tables
            accountgl      = accountgl
            currencyamount = currencyamount
            return         = return
            extension1     = extension1.
*  ENDIF.

endform.                    " call_bapi_post
*&---------------------------------------------------------------------*
*&      Form  update_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_SEL_KOSTL  text
*----------------------------------------------------------------------*
form update_log using  p_kostl p_type.
  data: l_msg(200) type c.
  read table return with key type = 'E'.
  if sy-subrc = 0.
    if p_type = '1'.
      loop at it_data where kostl = p_kostl.
        it_data-message = return-message.
        modify it_data.
      endloop.
    endif.

    rollback work.
    clear l_msg.
    loop at return where type = 'E'.
      concatenate return-message '/' l_msg into l_msg.
    endloop.

    message i000 with l_msg.
    sy-subrc = 8.

* OK-posted
  else.
    if p_type = '1'.
      loop at it_data where kostl = p_kostl.
        it_data-belnr = obj_key(10).
        concatenate sy-datum '-' sy-uzeit into it_data-message.
        modify it_data transporting belnr message.
      endloop.
      update ztco_payacc set belnr   = obj_key(10)
                             message = it_data-message
                         where kokrs = p_kokrs
                           and gjahr = p_gjahr
                           and perid = g_perid
                           and kostl = it_data_sel-kostl.
    else.
      loop at it_data where kostl = p_kostl.
        it_data-stokz = 'X'.
        modify it_data transporting stokz.
      endloop.

      update ztco_payacc set stokz  = 'X'
                         where kokrs = p_kokrs
                           and gjahr = p_gjahr
                           and perid = g_perid
                           and kostl = it_data_sel-kostl.
    endif.

*+ by ig.moon 4/2/2008 {
call function 'BAPI_TRANSACTION_COMMIT'
 EXPORTING
   WAIT          = 'Z' .
* }

*reversal doc need commit.. original doc.
    commit work.
    message s000 with 'Document posted.' obj_key p_kostl.
    sy-subrc = 0.
  endif.

endform.                    " update_log
*&---------------------------------------------------------------------*
*&      Form  get_ee_master
*&---------------------------------------------------------------------*
form get_ee_master.
  data: begin of lt_pernr occurs 0,
          pernr  like pa0001-pernr,
        end of lt_pernr.
  refresh lt_pernr.
  loop at list_table.
    lt_pernr-pernr = list_table-pernr.
    append lt_pernr.
  endloop.
  sort lt_pernr.
  delete adjacent duplicates from lt_pernr.

  select pernr persg persk begda endda
   into corresponding fields of table it_ee
   from pa0001
   for all entries in lt_pernr
   where pernr = lt_pernr-pernr and
         begda <= p_perbl-high     and
         endda >= p_perbl-high     and
         sprps ne 'X'.


  sort it_ee by pernr.
  delete adjacent duplicates from it_ee
     comparing pernr.

endform.                    " get_ee_master
*&---------------------------------------------------------------------*
*&      Form  get_acc_pay_period.
*&---------------------------------------------------------------------*
form get_acc_pay_period.

** get accrual per period (date)
  select single endda into w_acc_pend
    from t549q
    where permo = '04'
      and begda = p_adate.

  if sy-subrc <> 0.
    message i000 with 'Error to determine accrual period'.
  endif.

endform.                    " get_acc_pay_period.
*&---------------------------------------------------------------------*
*&      Form  calc_acc_amt
*&---------------------------------------------------------------------*
form calc_acc_amt.

  sort it_pay_ref  by kostl empct hkont  lgart.
  sort it_acc_mhad by kostl empct lgart  lgart2.

*regular + OT
  loop at it_pay_ref where zgart ca '12X'.  "<> space.
    read table it_acc_mhad with key kostl  = it_pay_ref-kostl
                                    empct  = it_pay_ref-empct
                                    lgart  = it_pay_ref-zgart
                                    lgart2 = it_pay_ref-lgart
                           binary search.
    if sy-subrc = 0.
      clear it_data.
      move-corresponding it_pay_ref to it_data.
      it_data-kokrs = p_kokrs.
      it_data-gjahr = p_gjahr.
      it_data-perid = g_perid.

*regular + OT
      if it_acc_mhad-acc_hr > 0.
        it_data-acc_hr  = it_acc_mhad-acc_hr.
        it_data-acc_amt = it_pay_ref-tpbpe * it_acc_mhad-acc_hr.
        collect it_data.
      endif.

**shift premium + OT
*      if it_acc_mhad-secr_hr > 0.
*        it_data-hkont   = p_shfreg.
*        it_data-acc_hr  = it_acc_mhad-secr_hr.
*        it_data-acc_amt = 1 * it_acc_mhad-secr_hr.
*        collect it_data to lt_data.
*      endif.
*
*      if it_acc_mhad-seco_hr > 0.
*        it_data-hkont   = p_shfot.
*        it_data-acc_hr  = it_acc_mhad-seco_hr.
*        it_data-acc_amt = '1.5' * it_acc_mhad-seco_hr.
*        collect it_data to lt_data.
*      endif.

    endif.
  endloop.


endform.                    " calc_acc_amt
*&---------------------------------------------------------------------*
*&      Form  get_gl_mapping
*&---------------------------------------------------------------------*
*form get_gl_mapping.
*
**  select * into table it_gl
**     from ztco_gl_ttype
**     where kokrs = p_kokrs.
**
**  sort it_gl by hkont.
*
*endform.                    " get_gl_mapping
*&---------------------------------------------------------------------*
*&      Form  calc_benefits
*&---------------------------------------------------------------------*
form calc_benefits.
  data: l_node(30).

  sort it_pay_ref_tot by kostl empct.

  loop at it_pay_ref where ref_hr = 0.
*check benefit account
    perform get_cenodename using it_pay_ref-hkont l_node.
    check l_node(9) = g_set_benefit(9).

    read table it_pay_ref_tot with key kostl = it_pay_ref-kostl
                                       empct = it_pay_ref-empct
                              binary search.
    check sy-subrc = 0.

    clear it_data.
    move-corresponding it_pay_ref to it_data.
    it_data-kokrs = p_kokrs.
    it_data-gjahr = p_gjahr.
    it_data-perid = g_perid.

*   read table it_gl with key hkont = it_pay_ref-hkont binary search.
*   if it_gl-zusereg = 'X'.  "Gross
    if it_pay_ref-hkont in s_bnfgl.
      it_data-acc_amt = it_pay_ref-betrg * it_pay_ref_tot-gross_p / 100.
      it_data-tpbpe = it_pay_ref_tot-gross_p.
    else.
      it_data-acc_amt = it_pay_ref-betrg * it_pay_ref_tot-basic_p / 100.
      it_data-tpbpe = it_pay_ref_tot-basic_p.
    endif.

    if it_data-acc_amt > 0.
      collect it_data.
    endif.

  endloop.

endform.                    " calc_benefits
*&---------------------------------------------------------------------*
*&      Form  calc_basic_gross
*&---------------------------------------------------------------------*
form calc_basic_gross.

  loop at it_pay_ref.
    it_pay_ref_tot-kostl = it_pay_ref-kostl.
    it_pay_ref_tot-empct = it_pay_ref-empct.
    if it_pay_ref-zgart ca '159'.
      it_pay_ref_tot-gross_r = it_pay_ref-betrg.
      it_pay_ref_tot-basic_r = it_pay_ref-betrg.
    elseif it_pay_ref-zgart ca '2'.
      it_pay_ref_tot-gross_r = it_pay_ref-betrg.
    endif.
    collect it_pay_ref_tot. clear it_pay_ref_tot.
  endloop.

  loop at it_data.
    it_pay_ref_tot-kostl = it_data-kostl.
    it_pay_ref_tot-empct = it_data-empct.
    if it_data-zgart ca '159'.
      it_pay_ref_tot-basic = it_data-acc_amt.
      it_pay_ref_tot-gross = it_data-acc_amt.
    else.                "OT
      it_pay_ref_tot-gross = it_data-acc_amt.
    endif.
    collect it_pay_ref_tot. clear it_pay_ref_tot.
  endloop.

*-
  loop at it_pay_ref_tot.
    it_pay_ref_tot-basic_p = 100 * it_pay_ref_tot-basic /
                                 it_pay_ref_tot-basic_r.
    it_pay_ref_tot-gross_p = 100 * it_pay_ref_tot-gross /
                                 it_pay_ref_tot-gross_r.
    modify it_pay_ref_tot index sy-tabix transporting basic_p gross_p.
  endloop.

endform.                    " calc_basic_gross
*&---------------------------------------------------------------------*
*&      Form  fill_ref_info
*&---------------------------------------------------------------------*
form fill_ref_info.

  data: l_idx like sy-tabix.
  data: w_pay_ref like it_pay_ref.
  sort it_pay_ref by kostl empct hkont lgart.

  loop at it_data.
    l_idx = sy-tabix.

    read table it_pay_ref into w_pay_ref
        with key kostl = it_data-kostl
                 empct = it_data-empct
                 hkont = it_data-hkont
                 lgart = it_data-lgart  binary search.
    if sy-subrc <> 0.
      read table it_pay_ref into w_pay_ref
          with key kostl = it_data-kostl
                   empct = it_data-empct
                   hkont = it_data-hkont binary search.
    endif.
    check sy-subrc = 0.
    it_data-ref_hr = w_pay_ref-ref_hr.
    it_data-betrg  = w_pay_ref-betrg.

    if it_data-tpbpe = 0.
      it_data-tpbpe   = w_pay_ref-tpbpe. "rate
    endif.
    modify it_data index l_idx transporting ref_hr betrg tpbpe.
  endloop.

endform.                    " fill_ref_info
*&---------------------------------------------------------------------*
*&      Form  batch_save
*&---------------------------------------------------------------------*
form batch_save.

  delete from ztco_payacc
    where kokrs = p_kokrs
      and gjahr = p_gjahr
      and perid = g_perid.

  insert ztco_payacc from table it_data.
  if sy-subrc = 0.
    commit work.
    message i000 with 'Data has been saved'.
  else.
    rollback work.
    w_saved = 'X'.
  endif.

endform.                    " batch_save
*&---------------------------------------------------------------------*
*&      Form  check_option
*&---------------------------------------------------------------------*
form check_option.

  check p_calc = 'X' and p_calb = 'X'.

  select count( * ) into sy-index
   from ztco_payacc
   where kokrs = p_kokrs
     and gjahr = p_gjahr
     and perid = g_perid.

  if sy-dbcnt > 0.

    data: l_answer(1).

    call function 'POPUP_TO_CONFIRM'
         exporting
              text_question = 'Do you want to overwrite existed data?'
              text_button_1 = 'Yes'
              text_button_2 = 'No'
         importing
              answer        = l_answer.

    if l_answer <> '1'.
      message e000 with 'Cancelled job'.
    endif.

  endif.


endform.                    " check_option
*&---------------------------------------------------------------------*
*&      Form  calc_acc_leave
*&---------------------------------------------------------------------*
form calc_acc_leave.
  data: lt_data like it_data occurs 0 with header line.
  data: l_idx   like sy-tabix.

  refresh lt_data. clear lt_data.

  sort it_pay_ref  by kostl empct zgart  lgart  hkont.

*paid leave / holiday
  loop at it_acc_mhad where lgart ca '59'.

* commented by IG. Moon 5/2/2007
*    read table it_pay_ref with key kostl  = it_pay_ref-kostl
*                                   empct  = it_pay_ref-empct
*                                   zgart  = '1'
*                           binary search.

    read table it_pay_ref with key kostl  = it_acc_mhad-kostl
                                   empct  = it_acc_mhad-empct
                                   zgart  = '1'
                           binary search.

    check sy-subrc = 0.
    clear it_data.

    it_data-kostl = it_pay_ref-kostl.
    it_data-empct = it_pay_ref-empct.

    it_data-kokrs = p_kokrs.
    it_data-gjahr = p_gjahr.
    it_data-perid = g_perid.

    it_data-lgart = it_acc_mhad-lgart2.

    if it_acc_mhad-acc_hr > 0.
      it_data-acc_hr  = it_acc_mhad-acc_hr.
      it_data-acc_amt = it_pay_ref-tpbpe * it_acc_mhad-acc_hr.

      if it_acc_mhad-lgart = '9'.  "holiday
        it_data-zgart = '9'.
        it_data-hkont = p_holpy.
      else.
        it_data-zgart = '5'.
        case it_acc_mhad-lgart2.
          when '0310'.  it_data-hkont = p_vaca.
          when others.  it_data-hkont = p_leave.
        endcase.
      endif.
      collect it_data into lt_data.
    endif.

  endloop.

* fill rate
  loop at lt_data.
    l_idx = sy-tabix.
    read table it_pay_ref
        with key kostl  = it_pay_ref-kostl
                 empct  = it_pay_ref-empct
                 zgart  = '1'
             binary search.
    if sy-subrc = 0.
      lt_data-tpbpe  = it_pay_ref-tpbpe.
      modify lt_data index l_idx transporting tpbpe.
    endif.
  endloop.

  append lines of lt_data to it_data.

endform.                    " calc_acc_leave
*&---------------------------------------------------------------------*
*&      Form  read_ca
*&---------------------------------------------------------------------*
form read_ca.

  select single * from tka01 where kokrs = p_kokrs.
  select single * from t001  where bukrs = p_kokrs.

endform.                    " read_ca
*&---------------------------------------------------------------------*
*&      Form  GET_CENODENAME
*&---------------------------------------------------------------------*
*       FIND THE COST ELEMENT GROUP NODE NAME
*----------------------------------------------------------------------*
form get_cenodename using  f_hkont like pna_ccdata-hkont
                           f_node .

  loop at t_cevalues.
    if f_hkont ge t_cevalues-from and
       f_hkont le t_cevalues-to.

      f_node = t_cevalues-lfieldname.
      exit.

    endif.
  endloop.

endform.                    " GET_CENODENAME
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_line_reversal
*&---------------------------------------------------------------------*
form fill_bapi_line_reversal.

  loop at currencyamount.
    currencyamount-amt_doccur = - currencyamount-amt_doccur.
    modify currencyamount  transporting amt_doccur.
  endloop.

endform.                    " fill_bapi_line_reversal
*&---------------------------------------------------------------------*
*&      Form  get_help_adate
*&---------------------------------------------------------------------*
* last field -> input value
form get_help_adate.
  data: lt_549s like table of t549s with header line.
  data: begin of value_tab occurs 0,
          pabrj like t549q-pabrj,
          pabrp like t549q-pabrp,
          endda like t549q-endda,
          begda like t549q-begda,
        end of value_tab.

  ranges: lr_pdate for  t549s-pdate.
  refresh lr_pdate.
  lr_pdate-option = 'BT'.
  lr_pdate-sign   = 'I'.
  lr_pdate-low    = sy-datum - 120.
  lr_pdate-high   = sy-datum.
  append lr_pdate.

  select pabrj pabrp begda endda
    into corresponding fields of table value_tab
    from t549q
    where permo = '4'
      and begda in lr_pdate.

  delete adjacent duplicates from value_tab.

* Set F4 values for pdate

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
       exporting
            retfield        = 'S_MCODE'
            dynpprog        = 'DYNPPROG'  "sy-repid "l_dyname
            dynpnr          = '1000'
            dynprofield     = 'BEGDA'
            window_title    = 'Accrual starting date'
            value_org       = 'S'
       tables
            value_tab       = value_tab
       exceptions
            parameter_error = 1.

endform.                    " get_help_adate
*&---------------------------------------------------------------------*
*&      Form  GET_HELP_PDATE
*&---------------------------------------------------------------------*
form get_help_pdate.

  data: lt_549s like table of t549s with header line.
  data: begin of value_tab occurs 0,
          pabrj like t549s-pabrj,
          pabrp like t549s-pabrp,
          pdate like t549s-pdate,
        end of value_tab.

  ranges: lr_pdate for  t549s-pdate.
  refresh lr_pdate.
  lr_pdate-option = 'BT'.
  lr_pdate-sign   = 'I'.
  lr_pdate-low    = sy-datum - 120.
  lr_pdate-high   = sy-datum.
  append lr_pdate.

  select pabrj pabrp pdate
    into corresponding fields of table value_tab
    from t549s
    where molga = '10'    "usa
      and permo = '4'     "bi-weekly
      and datid = '4'     "post date
      and pdate in lr_pdate.

  delete adjacent duplicates from value_tab.

* Set F4 values for pdate

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
       exporting
            retfield        = 'S_MCODE'
            dynpprog        = 'DYNPPROG'  "sy-repid "l_dyname
            dynpnr          = '1000'
            dynprofield     = 'PDATE'
            window_title    = 'Posting Date'
            value_org       = 'S'
       tables
            value_tab       = value_tab
       exceptions
            parameter_error = 1.


endform.                    " GET_HELP_PDATE
*&---------------------------------------------------------------------*
*&      Form  modify_acc
*&---------------------------------------------------------------------*
*       Modify account 601180
*----------------------------------------------------------------------*
form modify_acc.
  data: l_period like ztco_payacc_at-perio,
        l_rate   like ztco_payacc_at-rate ,
        l_gjahr  like cosp-gjahr          ,
        l_objnr  like cosp-objnr          ,
        l_date(10)   type c               ,
        l_lednr      like cosp-lednr      ,
        l_wtg001     like cosp-wtg001     ,
        l_index      like sy-tabix        ,
        l_period1(3) type c               ,
        wa_data      like ztco_payacc     ,
        l_acc_amt    like ztco_payacc-acc_amt.


  data: begin of it_kostl occurs 0,
          kostl like csks-kostl   ,
        end of it_kostl           .

  if not p_kostl is initial.
    loop at p_kostl.
      it_kostl-kostl = p_kostl-low.
      append it_kostl.
      clear  it_kostl.
    endloop.
  else.
    loop at it_data.
      it_kostl-kostl = it_data-kostl.
      append it_kostl.
      clear  it_kostl.
    endloop.
  endif.
  sort it_kostl by kostl.

  delete adjacent duplicates from it_kostl comparing kostl.

  l_lednr = '00'.
  l_date = p_adate.
  l_gjahr = p_adate(4).
  concatenate 'KS' p_kokrs '%' into l_objnr.
  condense l_objnr no-gaps.
  concatenate l_date(4) '0' l_date+4(2) into l_period.

  l_period1 = l_period+4(3).
  select * from ztco_payacc_at
           into table it_ztco
           where kokrs = p_kokrs and
                 perio >= l_period.
  loop at it_ztco.
    it_ztco_tmp-kstar = it_ztco-kstar.
    append it_ztco_tmp.
  endloop.
  sort it_ztco_tmp by kstar.
  delete adjacent duplicates from it_ztco_tmp comparing kstar.

  select objnr gjahr kstar wtg001
         wtg002 wtg003 wtg004 wtg005
         wtg006 wtg007 wtg008 wtg009
         wtg010 wtg011 wtg012
         from cosp
         into table it_cosp
         for all entries in it_ztco_tmp
         where objnr like l_objnr        and
               lednr = l_lednr           and
               gjahr = p_gjahr           and
               wrttp = l_wrttp           and
               versn = l_versn           and
               kstar = it_ztco_tmp-kstar and
               vrgng = l_vrgng.
  sort it_ztco by kokrs perio kstar.
  it_data_tmp[] = it_data[].

  loop at it_kostl.

    read table it_data into wa_data with key kostl = it_kostl-kostl.
    if sy-subrc = 0.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
           exporting
                input  = wa_data-kostl
           importing
                output = wa_data-kostl.

      clear: l_rate,
             l_objnr.
      concatenate 'KS' p_kokrs wa_data-kostl into l_objnr.

      clear wa_data-acc_amt.

      loop at it_ztco_tmp.
        clear: l_wtg001, l_acc_amt, l_rate.
*-Get total amount from COSP table
        loop at it_cosp where objnr = l_objnr and
                              kstar = it_ztco_tmp-kstar.
          case l_period1.
            when '001'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001.
            when '002'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002.
            when '003'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002 +
                         it_cosp-wtg003.
            when '004'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002 +
                         it_cosp-wtg003 + it_cosp-wtg004.
            when '005'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002 +
                       it_cosp-wtg003 + it_cosp-wtg004 + it_cosp-wtg005.
            when '006'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002 +
                     it_cosp-wtg003 + it_cosp-wtg004 + it_cosp-wtg005 +
                         it_cosp-wtg006.
            when '007'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002 +
                     it_cosp-wtg003 + it_cosp-wtg004 + it_cosp-wtg005 +
                         it_cosp-wtg006 + it_cosp-wtg007.
            when '008'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002 +
                     it_cosp-wtg003 + it_cosp-wtg004 + it_cosp-wtg005 +
                       it_cosp-wtg006 + it_cosp-wtg007 + it_cosp-wtg008.
            when '009'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002 +
                     it_cosp-wtg003 + it_cosp-wtg004 + it_cosp-wtg005 +
                     it_cosp-wtg006 + it_cosp-wtg007 + it_cosp-wtg008 +
                         it_cosp-wtg009.
            when '010'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002 +
                     it_cosp-wtg003 + it_cosp-wtg004 + it_cosp-wtg005 +
                     it_cosp-wtg006 + it_cosp-wtg007 + it_cosp-wtg008 +
                         it_cosp-wtg009 + it_cosp-wtg010.
            when '011'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002 +
                     it_cosp-wtg003 + it_cosp-wtg004 + it_cosp-wtg005 +
                     it_cosp-wtg006 + it_cosp-wtg007 + it_cosp-wtg008 +
                       it_cosp-wtg009 + it_cosp-wtg010 + it_cosp-wtg011.
            when '012'.
              l_wtg001 = l_wtg001 + it_cosp-wtg001 + it_cosp-wtg002 +
                     it_cosp-wtg003 + it_cosp-wtg004 + it_cosp-wtg005 +
                     it_cosp-wtg006 + it_cosp-wtg007 + it_cosp-wtg008 +
                     it_cosp-wtg009 + it_cosp-wtg010 + it_cosp-wtg011 +
                         it_cosp-wtg012.
          endcase.
        endloop.    " LOOP AT it_cosp WHERE objnr = l_objnr AND

*-Get total amount calculate by this program
        loop at it_data_tmp where hkont = it_ztco_tmp-kstar and
                                  kokrs = wa_data-kokrs and
                                  gjahr = wa_data-gjahr and
                                  perid = wa_data-perid and
                                  kostl = wa_data-kostl.
          l_acc_amt = l_acc_amt + it_data_tmp-acc_amt.
        endloop.

*-Get rate from ZTCO_PAYACC)AT table

        loop at it_ztco where kstar = it_ztco_tmp-kstar.
          l_rate = l_rate + ( it_ztco-portion * it_ztco-rate ).
        endloop
        .

        wa_data-acc_amt = wa_data-acc_amt +
               ( ( ( l_wtg001 + l_acc_amt ) * l_rate ) / 10000 ).

      endloop.      " LOOP AT it_ztco_tmp.

      wa_data-hkont = p_saknr.
      clear: wa_data-zgart,   wa_data-lgart, wa_data-ref_hr,
             wa_data-betrg,   wa_data-tpbpe, wa_data-acc_hr,
             wa_data-message, wa_data-belnr, wa_data-stokz.
      append wa_data to it_data.
    endif.        " IF sy-subrc = 0.
  endloop.        " LOOP AT it_kostl.
endform.                    " modify_acc
