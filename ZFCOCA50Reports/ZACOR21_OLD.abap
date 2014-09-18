************************************************************************
* Program Name      : ZRCO_LABOR_COST
* Author            : Chris Li, Andy Choi
* Creation Date     : 03/15/2005
* Specifications By : Andy Choi
* Pattern           :
* Development Request No : UD1K914960
* Addl Documentation:
* Description       : This report generate the lobor cost analysis
*                     result for each cost center based on the actual
*                     cost posting and reglect the support and being
*                     support between cost centers.
*
* Modification Logs
* Date       Developer    RequestNo    Description

*Note 1012176 - Indexes for tables PPOIX and PPOPX
* RPCIPQ00
************************************************************************
* Date        Developer  Request          Description
* 04/02/2007  Manju      UD1K940237       Switch signs of Amount & Hour
*                                         for previous period
* 01/25/2012  Valerian   UD1K953792       Correct Korean Coordinator
*                                         Monthly Working Hour
************************************************************************
report zrco_labor_cost  message-id zmco.


*include zrco_labortop.
*----------------------------------------------------------------------*
*   INCLUDE ZRCO_LABORTOP                                              *
*----------------------------------------------------------------------*
tables:   coss, cosp,coep, csks,
          ztco_labor_cost,
          zsco_labor, pa0002.

ranges: r_cc    for  csks-kostl.
ranges: r_ce    for  coss-kstar.
ranges: p_perbl for sy-datum.
data:   gv_prev_period like s016-spmon.

data:   t_cehier   like sethier   occurs 0 with header line.
data:   t_cevalues like setvalues occurs 0 with header line.


*// == 2011.09.22 change by yn.kim for ECC6. standard changed. ==//*
*// == ORG P/G : RPCPCC0D -- itab => list_table.
DATA: BEGIN OF list_tab OCCURS 0.
        INCLUDE STRUCTURE pna_ccdata.
DATA:   seqno LIKE ppoix-seqno.
DATA:   run_id TYPE p_evnum.
DATA:   fld_name(10) TYPE c.
DATA:   bukrs TYPE bukrs.
DATA:   io TYPE c.
DATA: END OF list_tab.

***DATA:   BEGIN OF list_tab  OCCURS 0.
***        INCLUDE STRUCTURE pna_ccdata.
***DATA:     seqno LIKE ppoix-seqno.
***DATA:     run_id TYPE p_evnum.
***DATA:     fld_name(10) TYPE c.
***DATA:   END OF list_tab .
*// ======================== Changed end ========================//*


data: list_table like list_tab occurs 0 with header line.

data:   rspar like rsparams occurs 10 with header line.
ranges: r_wt for t512w-lgart.
data:   ok_code like sy-ucomm,
        save_ok like sy-ucomm.
types:  begin of s_ee,
         pernr     like pa0001-pernr,
         persg     like pa0001-persg,
         ptext     like t501t-ptext,
         persk     like pa0001-persk,
         schkz     like pa0007-schkz,
         begda     like pa0001-begda,
        end of s_ee.
types:  begin of s_out,
         grp1      like zsco_labor-grp1,
         grp2      like zsco_labor-grp1,
         grp3      like zsco_labor-grp1,
         kostl     like csks-kostl,
         frocc     like csks-kostl,
         tocc      like csks-kostl,

*         persg     like pa0001-persg,
*         gtext(10) type c,
*         persk     like pa0001-persk,
*         ktext(10) type c,
         empct     type zempct,            "empl.type

         schkz     like pa0007-schkz,
         kztxt(10) type c,

         zcunt     like pna_ccdata-emp_count,
         regul     like pna_ccdata-betrg,
         overt     like pna_ccdata-betrg,
         bonus     like pna_ccdata-betrg,
         zleav     like pna_ccdata-betrg,
         othco     like pna_ccdata-betrg,
         totco     like pna_ccdata-betrg,
         pensn     like pna_ccdata-betrg,
         health    like pna_ccdata-betrg,
         workc     like pna_ccdata-betrg,
         insur     like pna_ccdata-betrg,
         tax       like pna_ccdata-betrg,
         othbe     like pna_ccdata-betrg,
         totbe     like pna_ccdata-betrg,
         tcost     like pna_ccdata-betrg,
         tregu     like pna_ccdata-anzhl,
         tover     like pna_ccdata-anzhl,
         tothr     like pna_ccdata-anzhl,
         thour     like pna_ccdata-anzhl,
        end of s_out.
types:  begin of s_dis.
include type s_out.
types:    clrtb     type lvc_t_scol.
types:  end of s_dis.
data:   it_ee   type standard table of s_ee.
data:   it_out  type standard table of s_out.
data:   it_out_s  type standard table of s_out.
data:   wa_out  type s_out.
data:   it_dis  type standard table of s_dis.
data:   it_out1 type standard table of s_out.
data:   begin of it_covp occurs 0,
*           gjahr   like covp-gjahr,
*           perio   like covp-perio,
           kostl   like csks-kostl,
           objnr   like covp-objnr,
           parob1  like covp-parob1,
*           beknz   like covp-beknz,
*           sgtxt   like covp-sgtxt,
           kstar   like covp-kstar,
           meinb   like covp-meinb,
           wkgbtr  like covp-wkgbtr,
           mbgbtr  like covp-mbgbtr,
           pkost   like csks-kostl,
        end of it_covp.
data: begin of it_mha_sup  occurs 0,
       kostl   like ztco_mha-kostl,
       srkostl like ztco_mha-srkostl,
*       LGART   like ztco_mha-lgart,
       anzhl   like ztco_mha-anzhl,
     end of it_mha_sup.

data:   begin of it_catsco occurs 0,
          counter    like catsco-counter,
          stokz      like catsco-stokz,
          workdate   like catsco-workdate,
          catshours  like catsco-catshours,
          skostl     like catsco-skostl,
          lstar      like catsco-lstar,
          rkostl     like catsco-rkostl,
        end of it_catsco.
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
data:   l_lines type i.

data    c_mark     type c value 'X'.
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

constants: c_structure(100) value 'ZSCO_AALA_REPORT_'.

ranges: r_lgart1 for t512w-lgart,
        r_lgart2 for t512w-lgart,
        r_lgart3 for t512w-lgart.

constants: user(5) type          c value 'USR',
           manager(5) type       c value 'MAN',
           administrator(5) type c value 'ADM'.
* Manager's mode or Administrator mode
data: e_mode(3) type c.

* BEGIN OF UD1K953792
data: frdate type sy-datum,
      todate type sy-datum.
* END OF UD1K953792
****************************************************************
* LOCAL CLASSES: EVEN HANDLING
****************************************************************
class lcl_event_receiver definition.
  public section.
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
  method handle_double_click.
    perform dbl_click using e_column-fieldname
                                 es_row_no-row_id.

  endmethod.                           "handle_double_click
endclass.

*end include
tables: t512w.

selection-screen begin of block b1 with frame title text-001.
parameters     p_kokrs       like tka01-kokrs obligatory default 'H201'.
parameters     p_gjahr       like coss-gjahr  obligatory memory id gjr.
select-options p_perio       for  coss-perbl  obligatory no-extension  .
*arameters     p_versn       like coss-versn  default '0' obligatory   .
parameters :   p_read  radiobutton group rd,
               p_calc  radiobutton group rd,
               p_save  radiobutton group rd.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
selection-screen begin of line.
selection-screen comment 1(31) text-005 for field p_ksgru.
parameters     p_ksgru       like rksb1-ksgru.
parameters:    p_ok(2)   type c.
selection-screen end of line.
*temperary add employee
select-options p_kostl       for  csks-kostl .
select-options p_pernr       for  pa0002-pernr .
selection-screen end of block b2.

selection-screen begin of block b4 with frame title text-003.
parameters: p_cycle(6)    type c  default 'HCR101' no-display.
parameters  p_atype       like catsd-awart
                               default 'PE20' no-display.
** changed by Furong on 10/26/2006
parameters: p_cyc    as checkbox default 'X',
            p_ex_acc as checkbox default 'X'. "Exclude Accruals
** end of change
selection-screen end of block b4.

selection-screen begin of block b3 with frame title text-004.
parameters: p_acc_hr as checkbox default ' ', "Accrual from HR
            p_acc_co as checkbox default 'X'. "Accrual from CO
selection-screen end of block b3.


*
at selection-screen output.
  if p_ok = '22' and e_mode = 'ADM'.
    loop at screen.
      if screen-name  cs 'P_PERNR'.
        screen-input = 1.
        screen-invisible = 0.
        modify screen.
      elseif screen-name  cs 'P_OK'.
        screen-input = 0.
        screen-invisible = 1.
        modify screen.
      endif.
    endloop.
  else.
    loop at screen.
      if screen-name  cs 'P_PERNR'.
        screen-input = 0.
        screen-invisible = 1.
        modify screen.
      endif.
      if e_mode <> 'ADM' and screen-name  cs 'P_OK'.
        screen-input = 0.
        screen-invisible = 1.
        modify screen.
      endif.
    endloop.
  endif.

*AT SELECTION-SCREEN ON P_OK.
*  IF P_OK = '22'.
*    LOOP AT SCREEN.
*      IF screen-name  CS 'P_PERNR'.
*        screen-input = 1.
*        screen-invisible = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

at selection-screen .
  perform make_perrange.
  perform read_cc.
  perform read_ce.
  perform read_cc_group.
  perform set_wagetype.
  perform make_text.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.
  data: l_cousertype(3) type c.
  get parameter id 'ZCOLV1' field l_cousertype.
  e_mode = l_cousertype.

*----------------------------------------------------------------------*
start-of-selection.
*  check e_mode = manager or e_mode = administrator.
  if p_ex_acc = 'X'.
    clear: p_acc_hr, p_acc_co.
  endif.

  case c_mark .

    when p_read.
      perform read_old_data.
    when p_calc.
      perform do_calculation.
    when p_save.
      perform delete_old_data.
      perform do_calculation.
      perform save_result.

  endcase.

end-of-selection.
  call screen '100'.


*&---------------------------------------------------------------------*
*&      Form  make_perrange
*&---------------------------------------------------------------------*
*      CONVERT THE PERIOD INTO DATE RANGE
*----------------------------------------------------------------------*
form make_perrange.
  data: l_date like sy-datum.
  data: i_perio type i.
  data: l_month(02).

* calculation allowed for single period.
  if p_read = ' '.
    p_perio-high = p_perio-low.
  endif.

  loop at p_perio.
    p_perbl-sign   = 'I'.
    p_perbl-option = 'BT'.
    l_month = p_perio-low+1(2).
    concatenate p_gjahr l_month '01' into l_date.
    p_perbl-low = l_date.

    if not p_perio-high is initial.
      l_month = p_perio-high+1(2).
      concatenate p_gjahr l_month '01' into l_date.
    endif.

    call function 'RP_LAST_DAY_OF_MONTHS'
         exporting
              day_in            = l_date
         importing
              last_day_of_month = p_perbl-high.

    append p_perbl.

* BEGIN OF UD1K953792
    frdate = l_date.
    todate = p_perbl-high.
* END OF UD1K953792

    l_date = p_perbl-low - 1.
    gv_prev_period = l_date(6).

    clear p_perbl.
  endloop.

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
        l_indv like sy-tabix.

  call function 'G_SET_LIST_SELECT'
       exporting
            setclass      = '0102'
            shortname     = 'H201_HR'
            kokrs         = 'H201'
            ktopl         = 'HNA1'
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
*&      Form  GET_COST
*&---------------------------------------------------------------------*
*   read the labor cost data by standard program
*----------------------------------------------------------------------*
form get_cost.

  perform make_selection.

  submit zrco_rpcpcc00_01
     with selection-table rspar
     and return.
*  IMPORT THE RESULT

*// 2011.09.22 by yn.kim ==> structure [list_table] is changed for ECC6
  import list_table[] from memory id 'LIST'.
  free memory.
  describe table list_table lines l_lines.

  tables: zcolab02, pa0001, t528t, t513s, pa0008.
  data: i_zcolab02 like zcolab02 occurs 0 with header line,
        i_pa0001   like pa0001   occurs 0 with header line,
        i_pa0008   like pa0008   occurs 0 with header line,
        i_t528t    like t528t    occurs 0 with header line,
        i_t513s    like t513s    occurs 0 with header line.
  data: l_dats(8) type c.
  select * into table i_pa0001 from pa0001
     where bukrs =  p_kokrs
       and pernr in p_pernr
       order by pernr  endda.
*  SELECT * INTO TABLE i_t528t FROM t528t
*     WHERE sprsl = sy-langu AND otype = 'S' AND endda = '99991231'.
*  SELECT * INTO TABLE i_t513s FROM t513s
*     WHERE sprsl = sy-langu AND endda = '99991231'.
  select * into table i_pa0008 from pa0008
    where pernr in p_pernr
    order by pernr  endda.

  loop at list_table.

*   ANZHL       time
*   BETRG       amt
*   BETRG_D     amt debit
*   BETRG_C     MTD amt credit
*   MTD_ANZHL   MTD time
*   MTD_BETRG   MTD amt
*   MTD_BETRG_D YTD amt debit
*   MTD_BETRG_C YTD amt credit
*   YTD_ANZHL   YTD time
*   YTD_BETRG   YTD headcount
*   YTD_BETRG_D YTD headcount
*   YTD_BETRG_C YTD headcount
*   EMP_COUNT   Headcount

    move-corresponding list_table  to list_tab.
    clear list_tab-budat.
    collect list_tab.

* save to ztable for further analysis
*    MOVE-CORRESPONDING list_table  TO i_zcolab02.
*
*    LOOP AT i_pa0001 WHERE pernr = list_table-pernr
*                       AND endda >= list_table-budat.
*      EXIT.
*    ENDLOOP.
*    i_zcolab02-persg = i_pa0001-persg.
*    i_zcolab02-persk = i_pa0001-persk.
*
*    LOOP AT i_pa0008 WHERE pernr = list_table-pernr
*                       AND endda >= list_table-budat.
*      EXIT.
*    ENDLOOP.
*    i_zcolab02-trfar = i_pa0008-trfar.
*    i_zcolab02-trfgb = i_pa0008-trfgb.
*    i_zcolab02-trfgr = i_pa0008-trfgr. "grp
*    i_zcolab02-trfst = i_pa0008-trfst. "lev
*
*
*    i_zcolab02-kokrs = p_kokrs.
*    i_zcolab02-gjahr = p_gjahr.
*    l_dats = list_table-budat.
*    i_zcolab02-monat = l_dats+4(2).
*    COLLECT i_zcolab02.  CLEAR i_zcolab02.
  endloop.

*  delete from zcolab02 where kokrs = p_kokrs
*                         and gjahr = p_gjahr
*                         and monat = p_perio-low.
*  commit work.
*  insert zcolab02 from table i_zcolab02.

endform.                    " GET_COST
*&---------------------------------------------------------------------*
*&      Form  MAKE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_selection.
*COMPANY CODE
  rspar-selname = 'P_BUKRS'. rspar-kind = 'P'.
  rspar-low  = 'H201'.
  append rspar.
  clear: rspar.
*PERIOD
  loop at p_perbl.
    rspar-selname = 'P_BUDAT'.
    rspar-kind = 'S'.
    rspar-sign = p_perbl-sign.
    rspar-option = p_perbl-option.
    rspar-low  = p_perbl-low.
    rspar-high = p_perbl-high.
    append rspar.
    clear: rspar.
  endloop.
*COST CENTER
  loop at r_cc.
    rspar-selname = 'P_KOSTL'.
    rspar-kind    = 'S'.
    rspar-sign    = r_cc-sign.
    rspar-option  = r_cc-option.
    rspar-low      = r_cc-low.
    rspar-high   = r_cc-high.
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

*EXCLUDING ACCRUE VALUE
  rspar-selname = 'P_EX_ACC'.
  rspar-kind = 'P'.
*  rspar-low  = space.
* accrual from CO... no need to get data from HR
  if p_acc_hr = ' '.
    rspar-low  = 'X'.
  else.
    rspar-low  =  p_ex_acc.
  endif.

  append rspar.
  clear: rspar.

* add employee temporary
  rspar-selname = 'P_PERNR'.
  rspar-kind    = 'S'.
  rspar-sign    = p_pernr-sign.
  rspar-option  = p_pernr-option.
  rspar-low     = p_pernr-low.
  rspar-high    = p_pernr-high.
  append rspar.
  clear: rspar.



endform.                    " MAKE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  SET_WAGETYPE
*&---------------------------------------------------------------------*
*       Set the wage type, but code did not use this now
*----------------------------------------------------------------------*
form set_wagetype.
  refresh r_wt. clear r_wt.
  clear : r_lgart1, r_lgart1[],r_lgart2, r_lgart2[],
          r_lgart3, r_lgart3[].

  r_lgart1-option = 'EQ'. r_lgart1-sign   = 'I'.
  r_lgart2-option = 'EQ'. r_lgart2-sign   = 'I'.
  r_lgart3-option = 'EQ'. r_lgart3-sign   = 'I'.
  r_wt-option     = 'EQ'. r_wt-sign       = 'I'.

  tables: ztco_mh_time.
  select * from ztco_mh_time.

    case ztco_mh_time-zgart.
* Regular
      when '1'.
        r_lgart1-low = ztco_mh_time-lgart. append r_lgart1.
        r_wt-low = ztco_mh_time-lgart. append r_wt.

* Over Time
      when '2'.
        r_lgart2-low = ztco_mh_time-lgart. append r_lgart2.
        r_wt-low = ztco_mh_time-lgart. append r_wt.

* paid leave, holiday
      when '5' or '9'.
        r_lgart3-low = ztco_mh_time-lgart. append r_lgart3.
        r_wt-low = ztco_mh_time-lgart. append r_wt.
    endcase.

  endselect.

  clear: r_lgart1, r_lgart2, r_lgart3.
endform.                    " SET_WAGETYPE
*&---------------------------------------------------------------------*
*&      Form  GET_EE_GROUP
*&---------------------------------------------------------------------*
*     read the emplyess information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_ee_group.
  data: pt0007 like pa0007 occurs 0 with header line.
  data: subrc  like sy-subrc.
  data: wa_ee  type s_ee.
  data: wa_eel type s_ee.
  data: lt_ee  type standard table of s_ee.
  data: lt_t501t like t501t occurs 0 with header line.
  read table p_perbl index 1.
* READ THE EMPOLYEE GROUP AND SUBGROUP
  select pernr persg persk
   into corresponding fields of table it_ee
   from pa0001
   for all entries in list_table
   where pernr = list_table-pernr and
         begda le p_perbl-high     and
         endda ge p_perbl-high     and
         sprps ne 'X'.
  sort it_ee by pernr.
  delete adjacent duplicates from it_ee
     comparing pernr.

* Check if terminated EE EXIST
  loop at it_ee into wa_ee.
    if wa_ee-persg = '5'.
*   READ THE PREVIOUS RECORD EE GROUP.
      select pernr persg persk begda
        into corresponding fields of table lt_ee
        from pa0001
        where pernr = wa_ee-pernr.
      sort lt_ee by begda descending.
      loop at lt_ee into wa_eel.
        if wa_eel-persg ne '5'.
          wa_ee-persg = wa_eel-persg.
          modify it_ee from wa_ee.
          exit.
        endif.
      endloop.
      if wa_ee-persg ne '1' and
         wa_ee-persg ne '9'.
        wa_ee-persg = '1'.  "IF OTHER GROUP, CONSIDER AS GROUP '1'.
        modify it_ee from wa_ee.
      endif.
    endif.
  endloop.

* READ THE GROUP TEXT
  select * into table lt_t501t
    from t501t
    for all entries in it_ee
    where persg = it_ee-persg and
          sprsl = 'EN'      .
  loop at it_ee into wa_ee.
    clear: lt_t501t.
    read table lt_t501t with key persg = wa_ee-persg.
    if sy-subrc eq 0.
      wa_ee-ptext = lt_t501t-ptext.
      modify it_ee from wa_ee.
    endif.
  endloop.

* READ EE WORK SCHEDULE RULE
  loop at it_ee into wa_ee.
    clear: pt0007, pt0007[].
    call function 'HR_READ_INFOTYPE'
         exporting
              pernr     = wa_ee-pernr
              infty     = '0007'
              begda     = p_perbl-high
              endda     = p_perbl-high
         importing
              subrc     = subrc
         tables
              infty_tab = pt0007.
    if subrc ne 0.

      message E000 with 'WORK SCHEDULE ERROR FOR' wa_ee-pernr.

    else.
      read table pt0007 index 1.
      wa_ee-schkz = pt0007-schkz.
      modify it_ee from wa_ee.
    endif.
  endloop.

endform.                    " GET_EE_GROUP
" GET_SUPPORT_COST
*&---------------------------------------------------------------------*
*&      Form  DATA_SUMMARY
*&---------------------------------------------------------------------*
*       summarize the cost data by cost element EE group
*----------------------------------------------------------------------*
form data_summary.
  perform ce_summary.
  perform include_co_accrual.
  perform group_summary.
  perform get_text.
endform.                    " DATA_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  CE_SUMMARY
*&---------------------------------------------------------------------*
*       SUMMARIZE THE DATA FOR EACH EMPLOYEE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ce_summary.
  data: l_tabix like sy-tabix.
  data: wa_list like list_table.

  sort list_tab by pernr kostl hkont.
  clear: wa_out.

  loop at list_tab.
    l_tabix = sy-tabix + 1.

*   SUMMARIZE THE COST AND WORKING HOURS
    perform sal_time_sum using wa_out
                               list_tab-hkont
                               list_tab-betrg.

*   TIME
    if list_tab-lgart in r_lgart1.
      wa_out-tregu  = wa_out-tregu + list_tab-anzhl.
    elseif list_tab-lgart in r_lgart2.
      wa_out-tover  = wa_out-tover + list_tab-anzhl.
    elseif list_tab-lgart in r_lgart3.
      wa_out-tothr  = wa_out-tothr + list_tab-anzhl.
    endif.

*   CHECK IF THE NEXT RECORD IS FOR THE SAME EMPLOYEE
    clear: wa_list.
    read table list_tab into wa_list index l_tabix.
    if wa_list-pernr <> list_tab-pernr
    or wa_list-kostl <> list_tab-kostl.

      perform get_other_fields using wa_out list_tab.

* BEGIN OF UD1K953792
data: l_molga     type molga,
      l_seqnr     type pc261-seqnr,
      it_rgdir    type pc261 occurs 0 with header line,
      payroll_res type payus_result,
      inter       type payus_result-inter,
      wa_rt       type pc207.

    if wa_out-empct = 'K' and wa_out-tregu = 0.
      call function 'CU_READ_RGDIR'
        exporting
          persnr          = list_tab-pernr
        importing
          molga           = l_molga
        tables
          in_rgdir        = it_rgdir
        exceptions
          no_record_found = 1
          others          = 2.

      call function 'CD_READ_LAST'
        exporting
          begin_date      = frdate
          end_date        = todate
        importing
          out_seqnr       = l_seqnr
        tables
          rgdir           = it_rgdir
        exceptions
          no_record_found = 1
          others          = 2.

      if sy-subrc = 0.
         call function 'PYXX_READ_PAYROLL_RESULT'
          exporting
            clusterid                    = 'RU'
            employeenumber               = list_tab-pernr
            sequencenumber               = l_seqnr
            read_only_international      = ' '
          changing
            payroll_result               = payroll_res
          exceptions
            illegal_isocode_or_clusterid = 1
            error_generating_import      = 2
            import_mismatch_error        = 3
            subpool_dir_full             = 4
            no_read_authority            = 5
            no_record_found              = 6
            versions_do_not_match        = 7
            error_reading_archive        = 8
            error_reading_relid          = 9
            others                       = 10.

        inter = payroll_res-inter.
        read table inter-rt into wa_rt with key lgart = '/5UH'.
        if sy-subrc = 0.
          wa_out-tregu = wa_rt-anzhl.
        endif.
      endif.
    endif.
* END OF UD1K953792

*     "SUM THE SUBTOTAL
      wa_out-totco = wa_out-regul + wa_out-overt  "LABOR COST SUBTOTAL
                   + wa_out-bonus + wa_out-zleav
                   + wa_out-othco.
      wa_out-totbe = wa_out-pensn + wa_out-health
                   + wa_out-workc + wa_out-insur  "BENEFIT SUBTOTAL
                   + wa_out-tax   + wa_out-othbe.
      wa_out-thour = wa_out-tregu + wa_out-tover  "WORK HOUR SUBTOTAL
                   + wa_out-tothr.
      wa_out-tcost = wa_out-totco + wa_out-totbe. "TOTAL OF COST/BENEFIT
*     "SAVE THE SUMMARY RESULT FOR EACH EMPLOYEE

      append wa_out to it_out. clear: wa_out.
    endif.
  endloop.


endform.                    " CE_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  SAL_TIME_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_OUT  text
*      -->P_LIST_TABLE  text
*----------------------------------------------------------------------*
form sal_time_sum using    pa_out  structure wa_out
                           f_hkont like pna_ccdata-hkont
                           f_betrg like pna_ccdata-betrg.
*                          pa_list structure pna_ccdata.
  data: l_node(30).

*GET THE COST ELEMENT GROUP NODE NAME
  perform get_cenodename using f_hkont l_node.

*SUMMARIZE THE COST AND BENEFIT
  case l_node.
    when 'H201_HR_11'.  "REGULAR PAY
      pa_out-regul  = pa_out-regul + f_betrg.

    when 'H201_HR_12'.  "OVERTIME PAY
      pa_out-overt  = pa_out-overt + f_betrg.

    when 'H201_HR_13'.  "BONUS PAY
      pa_out-bonus  = pa_out-bonus + f_betrg.

    when 'H201_HR_14'.  "LEAVE PAY
      pa_out-zleav  = pa_out-zleav + f_betrg.

    when 'H201_HR_15'.  "OTHER PAY
      pa_out-othco  = pa_out-othco + f_betrg.

    when 'H201_HR_21'.  "PENSION & 401K
      pa_out-pensn  = pa_out-pensn + f_betrg.

    when 'H201_HR_22'.  "INSURANCE
      pa_out-health = pa_out-health + f_betrg.

    when 'H201_HR_23'.  "INSURANCE
      pa_out-workc  = pa_out-workc + f_betrg.

    when 'H201_HR_24'.  "INSURANCE
      pa_out-insur  = pa_out-insur + f_betrg.

    when 'H201_HR_25'.  "TAX
      pa_out-tax  = pa_out-tax + f_betrg.

    when 'H201_HR_26'.  "OTHERS
      pa_out-othbe  = pa_out-othbe + f_betrg.

    when others.

      pa_out-othbe  = pa_out-othbe + f_betrg.
  endcase.

*SUMMARIZE THE WORKING HOURS
*  case l_node.
*    when 'H201_HR_11'.  "REGULAR WORKING HOUR
*      pa_out-tregu  = pa_out-tregu + pa_list-anzhl.
*
*    when 'H201_HR_12'.  "OVERTIME WORKING HOUR
*      pa_out-tover  = pa_out-tover + pa_list-anzhl.
*
*    when others.        "OTHER WORKING HOUR
*      pa_out-tothr  = pa_out-tothr + pa_list-anzhl.
*  endcase.

endform.                    " SAL_TIME_SUM
*&---------------------------------------------------------------------*
*&      Form  GET_CENODENAME
*&---------------------------------------------------------------------*
*       FIND THE COST ELEMENT GROUP NODE NAME
*----------------------------------------------------------------------*
*      -->P_PA_LIST_HKONT  text
*----------------------------------------------------------------------*
form get_cenodename using  p_hkont like pna_ccdata-hkont
                           p_node .

  loop at t_cevalues.
    if p_hkont ge t_cevalues-from and
       p_hkont le t_cevalues-to.
      p_node = t_cevalues-lfieldname.
      exit.
    endif.
  endloop.

endform.                    " GET_CENODENAME
*&---------------------------------------------------------------------*
*&      Form  GET_OTHER_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_OUT  text
*      -->P_LIST_TABLE  text
*----------------------------------------------------------------------*
form get_other_fields using  pa_out  structure wa_out
                             pa_list structure pna_ccdata.
  data: wa_ee  type s_ee.

  pa_out-kostl  = pa_list-kostl.
*Headcount...
  pa_out-zcunt  = 1.

*GET THE EE GROUP/SUBGROUP/WORKING SCHEDULE RULE
  clear: wa_ee.
  read table it_ee into wa_ee with key pernr = pa_list-pernr
                   binary search.
  if sy-subrc eq 0.
    perform get_emp_categ(zacop01) using wa_ee-persg wa_ee-persk
                                   changing pa_out-empct.
    pa_out-schkz  = wa_ee-schkz.

  else.
    pa_out-empct = 'X'.

*    pa_out-persg  = wa_ee-persg.
*    pa_out-gtext  = wa_ee-ptext.
*    pa_out-persk  = wa_ee-persk.

    pa_out-schkz  = wa_ee-schkz.
  endif.
endform.                    " GET_OTHER_FIELDS
*&---------------------------------------------------------------------*
*&      Form  GROUP_SUMMARY
*&---------------------------------------------------------------------*
*       DATA SUMMARY BY
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form group_summary.

  clear: it_out1[], wa_out, it_out_s.
  it_out_s[] = it_out[].
  sort it_out by kostl empct schkz.  "persg persk schkz.
  loop at it_out into wa_out.
    collect wa_out into it_out1.
  endloop.
  clear it_out.
  it_out = it_out1.
  clear it_out1.

endform.                    " GROUP_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  GET_SUPPORT_INFO
*&---------------------------------------------------------------------*
*      GET THE SUPPORT AND BEING SUPPORT COST BT COST CENTERS
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form get_support_info.
  ranges: r_ce  for covp-kstar.
  data: l_cc    like csks-kostl.
  data: begin of lt_cc occurs 0,
          kostl    like csks-kostl,
          objnr    like csks-objnr,
        end of lt_cc.

* GET THE COST ELEMENT FOR REGULAR SALARY/WAGE
  loop at t_cevalues where lfieldname ca 'H201_HR'.
    r_ce-sign   = 'I'.
    r_ce-option = 'BT'.
    r_ce-low    = t_cevalues-from.
    r_ce-high   = t_cevalues-to.
    append r_ce.
  endloop.

*** UD1K940482 ( start )
*** New Logic for gathering the row data by IG.MOON 5/5/2007

perform gather_row_data.

*** UD1K940482 ( end )

*** Commented by IG.MOON 5/5/2007
*** UD1K940482 ( start )
***
****READ THE SUPPPORT RECORDS
***  select a~kostl  b~objnr  b~parob1
***         b~kstar  b~meinb
***         sum( b~wkgbtr ) sum( b~mbgbtr )
***    into table it_covp
***    from  csks as a inner join covp as b
***      on  a~objnr = b~objnr   and
***          a~kokrs = b~kokrs
***    where b~refbt  = 'K'       and
***          b~refbn  = 'SUPPORT' and
***          b~kokrs  = p_kokrs   and
***          b~vrgng  = 'KAMV'    and
***          b~perio  in p_perio  and
***          b~gjahr  eq p_gjahr  and
***          b~kstar  in r_ce     and
***        ( b~stflg  ne 'X'       or
***          b~stokz  ne 'X' )    and
***          a~kostl in r_cc
***     group by a~kostl b~objnr b~parob1 b~kstar b~meinb.
*** UD1K940482 ( end )

* DELETE THE RECORD THAT CYCLE IS NOT P_CYCLE
*  if 1 = 2.
*    loop at it_covp.
*      if it_covp-sgtxt cs p_cycle.
*      else.
*        delete it_covp.
*      endif.
*    endloop.
*  endif.

* GET THE PARTNER COST CENTER
  select kostl objnr into table lt_cc
   from csks
   for all entries in it_covp
   where objnr = it_covp-parob1.

* by IG. MOON 5/4/2007
*** UD1K940482
sort lt_cc by objnr .

  data: l_idx like sy-tabix.
  loop at it_covp.
    l_idx = sy-tabix.
    read table lt_cc with key objnr = it_covp-parob1 binary search .
    if sy-subrc eq 0.
      it_covp-pkost = lt_cc-kostl.
      modify it_covp index l_idx transporting pkost.
    endif.
  endloop.

*NO split regular / OT ; expense .. no split... Andy
  select kostl srkostl sum( anzhl )
       into table it_mha_sup
       from ztco_mha
       where kokrs = p_kokrs
         and gjahr = p_gjahr
         and perid in p_perio
         and kostl in r_cc
         and lgart = '3'
       group by kostl srkostl.

endform.                    " GET_SUPPORT_INFO
*&---------------------------------------------------------------------*
*&      Form  SUPPORT_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form support_summary.
  exit.

  data:  l_exist .
  data:  wa_out1 type s_out.
  data:  lt_out type standard table of s_out.
  clear: wa_out,
         it_out1.
* APPEND THE SUPPORT COST
*  loop at it_covp.
*    wa_out-kostl = it_covp-kostl.
*    if it_covp-beknz = 'S'.
*      wa_out-frocc = it_covp-pkost.
*    elseif it_covp-beknz = 'H'.
*      wa_out-tocc  = it_covp-pkost.
*    endif.
*    wa_out-regul = - it_covp-wkgbtr.
*    append wa_out to it_out1.
*    clear wa_out.
*  endloop.

* APPEND THE SUPPORT TIME
  loop at it_catsco.
    wa_out-kostl = it_catsco-skostl.
    wa_out-tocc  = it_catsco-rkostl.
    wa_out-tregu = it_catsco-catshours.
    append wa_out to it_out1.
    clear wa_out.
    wa_out-kostl = it_catsco-rkostl.
    wa_out-frocc  = it_catsco-skostl.
    wa_out-tregu = it_catsco-catshours.
    append wa_out to it_out1.
    clear wa_out.
  endloop.
* DELETE THE ENTRIES THAT NOT IN THE ENTERED CC
  loop at it_out1 into wa_out.
    clear: l_exist.
    loop at r_cc.
      if wa_out-kostl ge r_cc-low and
         wa_out-kostl le r_cc-high.
        l_exist = 'X'.
        exit.
      endif.
    endloop.
    if l_exist ne 'X'.
      delete table it_out1 from wa_out.
    endif.
  endloop.

* Sumarize for the same sender or receiver
  sort it_out1 by kostl frocc tocc.
  loop at it_out1 into wa_out.
    collect wa_out into lt_out.
  endloop.
* ATTACH THE SUPPORT RESULT TO THE OUTPT TABLE
  append lines of lt_out to it_out.

endform.                    " SUPPORT_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  FINAL_SUMMARY
*&---------------------------------------------------------------------*
form final_summary.
  data: wa_out1 type s_out.
  data: wa_dis  type s_dis.
  clear: it_out1, wa_out.
* for layout test. making some data in internal table
*   perform make_test_data.

* here need to be deleted end.

  sort it_out by  kostl ascending
                  frocc ascending
                  tocc  ascending
                  empct.
*                  gtext descending
*                  ktext descending
*                  kztxt ascending.

  loop at it_out into wa_out.
*    wa_out-persg = space.
*    wa_out-persk = space.
*    wa_out-schkz = space.
    collect wa_out into it_out1.
  endloop.

  clear: it_out.
* ATTACH THE COST CENTER GROUP
  loop at it_out1 into wa_out.
    perform find_cc_group using wa_out.
    modify it_out1 from wa_out.
  endloop.
*
  clear it_out.

* MOVE RESULT TO DISPLAY TABLE
  loop at it_out1 into wa_out.
    move-corresponding wa_out to wa_dis.
    append wa_dis to it_dis.
  endloop.

endform.                    " FINSAL SUMMARY

*&---------------------------------------------------------------------*
*&      Form  GET_SUPPORT_HOUR
*&---------------------------------------------------------------------*
*     read the support hours
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form get_support_hour.

  exit.
  data: lt_cats  like it_catsco occurs 0 with header line.
  select counter stokz workdate catshours
         skostl  lstar rkostl
   into  table lt_cats
   from  catsco
   where workdate in p_perbl and
         kokrs = 'H201'      and
       ( skostl in r_cc  or
         rkostl in r_cc  )   and
         lstar = 'MAN_HR'.
  if sy-subrc ne 0.
    exit.
  endif.
  it_catsco[] = lt_cats[] .
*  DELETE THE REVERSED DOC
  loop at lt_cats.
    read table it_catsco with key counter = lt_cats-counter
                                  stokz   = 'X'.
    if sy-subrc eq 0.
      delete lt_cats.
    endif.
  endloop.
  clear: it_catsco[], it_catsco.
  it_catsco[] = lt_cats[].
endform.                    " GET_SUPPORT_HOUR
*&---------------------------------------------------------------------*
*&      Form  MAKE_TEXT
*&---------------------------------------------------------------------*
*    Make the display text in report for employee group,subgroup and
*    working schedule rule name
*----------------------------------------------------------------------*
form make_text.
  perform make_persg_text using: '1' 'Active',
                                 '9' 'Inpatriate'.
  perform make_persk_text using: 'U0' 'Hourly',
                                 'U2' 'Salary',
                                 'U3' 'Salary',
                                 'U8' 'Contract'.

  tables: ztco_mh_ws.
  data: wstxt(3) type c,
        wskey(5) type c.
  select * from ztco_mh_ws.
    wskey+1(4) = ztco_mh_ws-schkz.
    concatenate 'WS' ztco_mh_ws-anzsh into wstxt.
    perform make_schkz_text using: wskey  wstxt.
  endselect.
*  perform make_schkz_text using: ' 1000' 'Standard',
*                                 ' 1001' 'Shift 1',
*                                 ' 1002' 'Shift 2',
*                                 ' 1003' 'Shift 3',
*                                 ' 2001' 'Shift 1',
*                                 ' 2002' 'Shift 2',
*                                 ' 2003' 'Shift 3',
*                                 ' 3001' 'Shift 1',
*                                 ' 3002' 'Shift 2',
*                                 ' 3003' 'Shift 3'.

endform.                    " MAKE_TEXT
*&---------------------------------------------------------------------*
*&      Form  MAKE_PERSK_TEXT
*&---------------------------------------------------------------------*
form make_persk_text using p_1 p_2.
  it_persk-persk = p_1.
  it_persk-ptext = p_2.
  append it_persk.
endform.
*&---------------------------------------------------------------------*
*&      Form  MAKE_PERSG_TEXT
*&---------------------------------------------------------------------*
form make_persg_text using p_1 p_2.
  it_persg-persg = p_1.
  it_persg-ptext = p_2.
  append it_persg.
endform.

*&---------------------------------------------------------------------*
*&      Form  MAKE_SCHKZ_TEXT
*&---------------------------------------------------------------------*
form make_schkz_text using p_1 p_2.
  it_schkz-schkz = p_1.
  it_schkz-ptext = p_2.
  append it_schkz.
endform.

*&---------------------------------------------------------------------*
*&      Form  GET_TEXT
*&---------------------------------------------------------------------*
form get_text .
  data: l_idx like sy-tabix.

  loop at it_out into wa_out.
    check wa_out-schkz <> space.

    l_idx = sy-tabix.

*ANDY - comment
** GET THE EE GROUP TEXT
** for terminated person, but we have cost for the period
** chang the person group to 'ACTIVE'
*    if wa_out-persg = '5'.
*      wa_out-persg = '1'.
*    endif.
*    clear: it_persg.
*    read table it_persg with key persg = wa_out-persg.
*    if sy-subrc eq 0.
*      wa_out-gtext = it_persg-ptext.
*    else.
*      message i000 with text-010.
*    endif.
** GET THE EE SUBGROUP TEXT
*    clear: it_persk.
*    read table it_persk with key persk = wa_out-persk.
*    if sy-subrc eq 0.
*      wa_out-ktext = it_persk-ptext.
*    else.
*      message e000 with text-010.
*    endif.

* GET THE EE WS RULE TEXT

    clear: it_schkz.
    read table it_schkz with key schkz = wa_out-schkz.
    if sy-subrc eq 0.
      wa_out-kztxt = it_schkz-ptext.
    else.
      write:/ '***Error in time wage type configuration: ',
              wa_out-schkz.
    endif.
    modify it_out index l_idx from wa_out transporting kztxt.
  endloop.


endform.
*&---------------------------------------------------------------------*
*&      Form  FIND_CC_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_OUT  text
*----------------------------------------------------------------------*
form find_cc_group using    pa_out structure wa_out.
  loop at it_cc_grp.
    if pa_out-kostl ge it_cc_grp-from and
       pa_out-kostl le it_cc_grp-to.
      pa_out-grp1 = it_cc_grp-grp1.
      pa_out-grp2 = it_cc_grp-grp2.
      pa_out-grp3 = it_cc_grp-grp3.
      exit.
    endif.
  endloop.
endform.                    " FIND_CC_GROUP




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
  ws_layout-cwidth_opt      = 'X'.
  ws_layout-no_merging      = 'X'.
  ws_layout-no_keyfix       = 'X'.
  ws_layout-ctab_fname      = 'CLRTB'.
  w_variant-report            = sy-repid.
  w_variant-username          = sy-uname.
* BUILD THE CELL COLOR
  perform build_cell_color.

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
  data: ct type lvc_t_scol.
  data: w_ct  like line of ct.
  data: wa_dis type s_dis.

  loop at it_dis into wa_dis.
    w_ct-fname = 'TCOST'.
    w_ct-color-col = '5'.
    w_ct-color-int = '1'.
    append w_ct to ct.
    w_ct-fname = 'TOTCO'.
    w_ct-color-col = '5'.
    append w_ct to ct.
    w_ct-fname = 'TOTBE'.
    w_ct-color-col = '5'.
    append w_ct to ct.
    w_ct-fname = 'THOUR'.
    w_ct-color-col = '5'.
    append w_ct to ct.

    wa_dis-clrtb = ct.
    modify it_dis from wa_dis.
  endloop.

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
  lw_itab = 'ZSCO_LABOR'.

  call function 'LVC_FIELDCATALOG_MERGE '
       exporting
            i_structure_name   = lw_itab
            i_bypassing_buffer = 'X'
       changing
            ct_fieldcat        = it_fieldcat.
* SET THE FIELD ATTRIBUTE
  loop at it_fieldcat into wa_fc.
    if wa_fc-fieldname = 'SCHKZ' or
       wa_fc-fieldname = 'PERSK' or
       wa_fc-fieldname = 'PERSG'.
      delete it_fieldcat index sy-tabix.
      continue.
    endif.
    if wa_fc-fieldname = 'KOSTL' or
       wa_fc-fieldname = 'FROCC' or
       wa_fc-fieldname = 'EMPCT' or
*       wa_fc-fieldname = 'GTEXT' or
*       wa_fc-fieldname = 'PERSK' or
       wa_fc-fieldname = 'SCHKZ' or
*       wa_fc-fieldname = 'PERSG' or
*       wa_fc-fieldname = 'KTEXT' or
       wa_fc-fieldname = 'GRP1'  or
       wa_fc-fieldname = 'GRP2'  or
       wa_fc-fieldname = 'GRP3'  or
       wa_fc-fieldname = 'KZTXT' or
       wa_fc-fieldname = 'TOCC'.
      wa_fc-key  = 'X'.
      modify it_fieldcat from wa_fc.
    else.
      wa_fc-do_sum  = 'X'.
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
                                  '4' 'EMPCT' 'X' ' ' ' ',
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

  w_structure = 'ZSCO_LABOR'.
  call method wc_alv->set_table_for_first_display
     exporting
               is_layout        = ws_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     changing  it_fieldcatalog  = it_fieldcat
               it_sort          = it_sort
               it_outtab        = it_dis.

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
  set pf-status 'STATUS100'.
  set titlebar 'TITLE100'.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  save_ok = ok_code.
  case save_ok.
    when 'BACK' or 'EXIT' or 'CANCEL'.
      leave to screen 0.
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
form make_test_data.
  perform enter_test_data using : '0000022001' ' ' ' ' 'Active'
                                  'Salary' 'Shift 1' '1' '200',
                                  '0000022001' ' ' ' ' 'Active'
                                  'Salary' 'Shift 2' '1' '200',
                                  '0000022001' ' ' ' ' 'Active'
                                  'Salary' 'Shift 3' '1' '200',
                                  '0000022001' ' ' ' ' 'Active'
                                  'Hourly' 'Shift 1' '1' '200'.


endform.                    " make_test_data
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
form enter_test_data using    p_1 p_2 p_3 p_4 p_5 p_6 p_7 p_8.
  wa_out-kostl = p_1.
  wa_out-frocc = p_2.
  wa_out-tocc = p_3.
*  wa_out-gtext = p_4.
*  wa_out-ktext = p_5.
  wa_out-kztxt = p_6.
  wa_out-zcunt = p_7.
  wa_out-regul = p_8.
  append wa_out to it_out.
endform.                    " enter_test_data
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
*&      Form  do_calculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form do_calculation.
* cost center is one : valid on end of month.
  perform get_cost.
  if l_lines ne 0.
    perform get_ee_group.
    perform data_summary.

*ANDY... FIX IT LATER
    if p_cyc = 'X'.
      perform get_support_info.

      if 1 = 2.
        perform get_support_hour.
        perform support_summary.
      endif.

      perform append_support_info.

    endif.

    perform final_summary.
  endif.

endform.                    " do_calculation
*&---------------------------------------------------------------------*
*&      Form  read_old_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_old_data.
  data: w_dis like line of it_dis.
  select * into corresponding fields of table it_out
    from ztco_labor_cost
    where kokrs   = p_kokrs
     and  gjahr   = p_gjahr
     and  periol in p_perio
"     between p_perio-low and p_perio-high
*    and  versn   = p_versn
     and  kostl   in r_cc
     and  accrl  =  p_ex_acc.

  check sy-subrc eq 0.

  loop at it_out into wa_out.
    move-corresponding wa_out to w_dis.
    append w_dis to it_dis.
  endloop.
endform.                    " read_old_data
*&---------------------------------------------------------------------*
*&      Form  delete_old_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_old_data.
  delete from ztco_labor_cost where kokrs  = p_kokrs
                                and gjahr  = p_gjahr
                                and periol in p_perio
                                and accrl  =  p_ex_acc.
*                               and perioh = p_perio-high.
endform.                    " delete_old_data
*&---------------------------------------------------------------------*
*&      Form  save_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_result.
  data: lt_out like ztco_labor_cost occurs 0 with header line.
  data: w_dis  like line of it_dis.
  data: w_out  like line of it_out.
  clear: w_out.
  loop at it_dis into w_dis.
* loop at it_out_s into w_out.
    move-corresponding w_dis to lt_out.
    lt_out-kokrs  = p_kokrs.
    lt_out-gjahr  = p_gjahr.
    lt_out-periol = p_perio-low.
** changed by Furong on 10/26/2006
    lt_out-accrl  = p_ex_acc.
** end of change
*    lt_out-perioh = p_perio-high.
*   lt_out-versn  = p_versn.
    append lt_out.
  endloop.
  check not lt_out is initial.

* modify ztco_labor_cost from table lt_out.
  insert ztco_labor_cost from table lt_out
         accepting duplicate keys.

  commit work and wait.
endform.                    " save_result
*&---------------------------------------------------------------------*
*&      Form  append_support_info
*&---------------------------------------------------------------------*
form append_support_info.
  loop at it_covp.
    clear: wa_out.

*   SUMMARIZE THE COST AND WORKING HOURS
    perform sal_time_sum using wa_out
                               it_covp-kstar
                               it_covp-wkgbtr.

    wa_out-kostl = it_covp-kostl.

    read table it_mha_sup with key kostl   = it_covp-kostl
                                   srkostl = it_covp-pkost.
    if sy-subrc = 0.
      wa_out-frocc  = it_covp-kostl.
      wa_out-tocc   = it_covp-pkost.
    else.
*      read table it_mha_sup with key kostl   = it_covp-pkost
*                                     srkostl = it_covp-kostl.
      wa_out-tocc   = it_covp-kostl.
      wa_out-frocc  = it_covp-pkost.
    endif.


*     "SUM THE SUBTOTAL
    wa_out-totco = wa_out-regul + wa_out-overt  "LABOR COST SUBTOTAL
                 + wa_out-bonus + wa_out-zleav
                 + wa_out-othco.
    wa_out-totbe = wa_out-pensn + wa_out-health
                 + wa_out-workc + wa_out-insur  "BENEFIT SUBTOTAL
                 + wa_out-tax   + wa_out-othbe.
    wa_out-thour = wa_out-tregu + wa_out-tover  "WORK HOUR SUBTOTAL
                 + wa_out-tothr.
    wa_out-tcost = wa_out-totco + wa_out-totbe. "TOTAL OF COST/BENEFIT

    collect wa_out into it_out.

  endloop.

* hours---
  data: l_idx like sy-tabix.
  loop at it_out into wa_out.
    check wa_out-frocc <> space.
    l_idx = sy-tabix.
    read table it_mha_sup with key kostl   = wa_out-frocc
                                   srkostl = wa_out-tocc.
    wa_out-thour = it_mha_sup-anzhl.
    modify it_out index l_idx from wa_out transporting thour.
  endloop.

endform.                    " append_support_info
*&---------------------------------------------------------------------*
*&      Form  include_co_accrual
*&---------------------------------------------------------------------*
form include_co_accrual.
  sort it_out by kostl empct schkz.

* CO accrual
  data: lt_co_acc like ztco_payacc occurs 0 with header line.

  if p_acc_co = 'X' and p_ex_acc = ' '.


*should be...yyyymm = S016-SPMON

    select * into table lt_co_acc
      from ztco_payacc
      where kokrs = p_kokrs
        and gjahr = gv_prev_period(4)
        and perid = gv_prev_period+4(2)
        and kostl in r_cc.

    select * appending table lt_co_acc
      from ztco_payacc
      where kokrs = p_kokrs
        and gjahr = p_gjahr
        and perid in p_perio
        and kostl in r_cc.

    loop at lt_co_acc.

*      read table it_out with key kostl = lt_co_acc-kostl
*                                 empct = lt_co_acc-empct
* Begin of changes  - UD1K940237
* Change the sign of Amount & HR for previous Period
      if  gv_prev_period(4) eq   lt_co_acc-gjahr and
          gv_prev_period+4(2) eq lt_co_acc-perid.
        lt_co_acc-acc_amt = lt_co_acc-acc_amt * -1.
        lt_co_acc-acc_hr  = lt_co_acc-acc_hr * -1.
      endif.

* End of changes - UD1K940237

      perform sal_time_sum using wa_out
                                 lt_co_acc-hkont
                                 lt_co_acc-acc_amt.
*   TIME
      if lt_co_acc-lgart in r_lgart1.
        wa_out-tregu  = wa_out-tregu + lt_co_acc-acc_hr.
      elseif lt_co_acc-lgart in r_lgart2.
        wa_out-tover  = wa_out-tover + lt_co_acc-acc_hr.
      elseif lt_co_acc-lgart in r_lgart3.
        wa_out-tothr  = wa_out-tothr + lt_co_acc-acc_hr.
      endif.

*     "SUM THE SUBTOTAL
      wa_out-totco = wa_out-regul + wa_out-overt  "LABOR COST SUBTOTAL
                   + wa_out-bonus + wa_out-zleav
                   + wa_out-othco.
      wa_out-totbe = wa_out-pensn + wa_out-health
                   + wa_out-workc + wa_out-insur  "BENEFIT SUBTOTAL
                   + wa_out-tax   + wa_out-othbe.
      wa_out-thour = wa_out-tregu + wa_out-tover  "WORK HOUR SUBTOTAL
                   + wa_out-tothr.
      wa_out-tcost = wa_out-totco + wa_out-totbe. "TOTAL OF COST/BENEFIT


      wa_out-kostl  = lt_co_acc-kostl.
      wa_out-empct  = lt_co_acc-empct.

      collect wa_out into it_out. clear: wa_out.
    endloop.
  endif.

endform.                    " include_co_accrual

*** UD1K940478 ( START )
*** by IG.MOON 5/5/2007
form gather_row_data.

clear it_covp.
refresh it_covp.

ranges lr_objnr for  csks-objnr.

data: begin of i_objnr occurs 0,
        objnr like csks-objnr,
        kostl like csks-kostl,
      end of i_objnr.

  select distinct objnr kostl into table i_objnr
    from  csks
    where kostl in r_cc .

  loop at i_objnr .
    lr_objnr-sign   = 'I'.
    lr_objnr-low    = i_objnr-objnr.
    lr_objnr-option = 'EQ'.
    append lr_objnr.
  endloop.

  select objnr  parob1
         kstar  meinb
         sum( wkgbtr ) as wkgbtr sum( mbgbtr ) as mbgbtr
    into corresponding fields of table it_covp
    from  covp
    where objnr in lr_objnr  and
          kstar  in r_ce     and
          gjahr  eq p_gjahr  and
          perio  in p_perio  and
          refbt  = 'K'       and
          refbn  = 'SUPPORT' and
          kokrs  = p_kokrs   and
          vrgng  = 'KAMV'    and
        ( stflg  ne 'X' and stokz  ne 'X' )
     group by objnr parob1 kstar meinb.

  sort i_objnr by objnr.
  loop at it_covp .
    read table i_objnr with key objnr = it_covp-objnr
                       binary search.
    if sy-subrc eq 0.
      it_covp-kostl = i_objnr-kostl .
      modify it_covp .
    endif.
  endloop.

endform.                    " GATHER_ROW_DATA
*** UD1K940478 ( END )
