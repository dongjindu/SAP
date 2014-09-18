************************************************************************
* Program Name      : ZACO03U_MH
* Author            : Andy Choi
* Specifications By : Andy Choi
*
* Maintain ztco_mhaatype table : select time wage type ranges
*  	PROD	1001	UES1	X	05/27/2004	14:28:19	9071483

* Date      Developer      Request       Description
* 06/06/06  Manju          UD1K920571    Program changes to capture
*                                        Delta time changes.
* 08/31/10  Valerian       UD1K949773    Refer to table ZTCO_MH_DWS
*                                        to get Shift Information
************************************************************************
report zaco03u_mh message-id zmco.


* RPTBAL00 - report
* RPTIME00 - eval
* CAT2     - recording
* CADO     - display time
*
* BAPI_CATIMESHEETMGR_INSERT
* Inserting records that generate attendances/absences and wage types
* for Human Resources (HR)
* Inserting records that trigger activity allocation for Controlling
* (CO)
* Inserting records that generate confirmations for Project System (PS)
* Inserting records that generate confirmations for Plant Maintenance
* (PM) or Customer Service (CS)
* Inserting records with information on external services for Materials
* Management (MM)
*  call function 'PM_CATS_CONFIRMATION_CREATE' in background task
*       exporting
*            cluster_no    = p_cluster_no
*            profile       = p_profile
*       tables
*            catsrecords   = p_cats_confirmation
*            extensionin   = p_cats_extensionin
*            longtext      = p_cats_longtext.


*----------------------------------------------------------------------*
*   Include Program
*----------------------------------------------------------------------*
* For Global Value in CO
*NCLUDE zlzgco_global_formto1.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** type-pools
type-pools: slis.

** Tables
tables : catsdb, ztco_mhaatype,
         ztco_mh_time,
         ztco_jc,  "job code mapping
         ztco_mh_ws,
         tka01, pa0007,t552a,
         ztco_mhat,
         ztco_mhad,
         pa0001,
         ztco_mha.

data: begin of i_pa7 occurs 0,
        pernr like pa0007-pernr,
        begda like pa0007-begda,
        endda like pa0007-endda,
        arbst like pa0007-arbst,  "Daily Working Hours
      end of i_pa7.

** Internal table
data : begin of py_times occurs 0,
         werks like t001p-werks, "PA
         btrtl like t001p-btrtl, "sub-area
         molga like t001p-molga, "country
         mofid like t001p-mofid, "Public Holiday Calendar
         persg like t503-persg,
         persk like t503-persk,
         zeity like t503-zeity,
         datum like sy-datum.
*        MOSID like t001p-mosid, "Personnel Subarea Grouping for WS
        include structure ztco_mha.
data :   lgart2   like ztco_mhad-lgart2.
*       dtype(1) TYPE c.
data : end of py_times.

data : it_time           like standard table of ztco_mha
                         with header line .
data : it_time_dt        like it_time occurs 0 with header line.

data : it_timet          like standard table of ztco_mhat
                         with header line .
data : it_timet_dt        like it_timet occurs 0 with header line.

data : it_timed          like standard table of ztco_mhad
                         with header line .
data : it_timed_dt        like it_timed occurs 0 with header line.
data : it_timed_b         like it_timed occurs 0 with header line.

* Begin of changes -
data : it_time_db           like standard table of ztco_mha
                         with header line .
data : it_timet_db          like standard table of ztco_mhat
                         with header line .
data : it_timed_db          like standard table of ztco_mhad
                         with header line .
* End of changes -

data: i_jobcd    like ztco_jc      occurs 0 with header line,
      i_timetype like ztco_mh_time occurs 0 with header line,
      i_wksch    like ztco_mh_ws   occurs 0 with header line,
      i_dwksch like ztco_mh_dws  occurs 0 with header line. "UD1K949773

* Emp.Data
data : begin of i_emp occurs 0,
         pernr like pa0001-pernr,
         hire  like sy-datum,
         fire  like sy-datum,
         kostl like pa0001-kostl,
         werks like pa0001-werks,  "PSA      -> PSG for WS (V_001P_N)
         btrtl like pa0001-btrtl,  "PSubArea -> PSG for WS (V_001P_N)
         mosid like t001p-mosid,   "PSG for WS
       end of i_emp.

data : gw_time  like it_time,
       gw_timet like it_timet,
       gw_timed like it_timed.

data : it_date           like standard table of casdayattr
                         with header line.
data : g_workdays   type i,
       g_totdays(2) type n.


data : g_weekday type wotnr,
       g_freeday type cind,
       g_holiday type cind.

data : begin of it_t552a  occurs 0,
         zeity   like t552a-zeity,
         mofid   like t552a-mofid,
         mosid   like t552a-mosid,
         schkz   like t552a-schkz,

         solst   like t552a-solst,
         tpr01   like t552a-tpr01,
         tpr02   like t552a-tpr02,
         tpr03   like t552a-tpr03,
         tpr04   like t552a-tpr04,
         tpr05   like t552a-tpr05,
         tpr06   like t552a-tpr06,
         tpr07   like t552a-tpr07,
         tpr08   like t552a-tpr08,
         tpr09   like t552a-tpr09,
         tpr10   like t552a-tpr10,
         tpr11   like t552a-tpr11,
         tpr12   like t552a-tpr12,
         tpr13   like t552a-tpr13,
         tpr14   like t552a-tpr14,
         tpr15   like t552a-tpr15,
         tpr16   like t552a-tpr16,
         tpr17   like t552a-tpr17,
         tpr18   like t552a-tpr18,
         tpr19   like t552a-tpr19,
         tpr20   like t552a-tpr20,
         tpr21   like t552a-tpr21,
         tpr22   like t552a-tpr22,
         tpr23   like t552a-tpr23,
         tpr24   like t552a-tpr24,
         tpr25   like t552a-tpr25,
         tpr26   like t552a-tpr26,
         tpr27   like t552a-tpr27,
         tpr28   like t552a-tpr28,
         tpr29   like t552a-tpr29,
         tpr30   like t552a-tpr30,
         tpr31   like t552a-tpr31,
      end of it_t552a.

data : begin of it_sch occurs 0,
        schkz     type schkn,
        zeity     type dzeity,
        mosid     type mosid,

        date      like sy-datum,
        freeday   type cind,  "non working 'X'
        shift(1)  type c,
       end of it_sch.

data: begin of i_wdays occurs 0,
         mosid like t552a-mosid,
         schkz like t552a-schkz,
         zeity like t552a-zeity,
         solst like t552a-solst,
         ttday type i,   "Total days
         works type i,   "Working days
         odays type i,   "ot work days

         wkday type i,   "week days
         saday type i,   "SAT days
         suday type i,   "SUN days
         hoday type i,   "HOL days

      end of i_wdays.

ranges : pnpkostl for pernr-kostl.

*data : begin of it_pern occurs 0,
*         pernr   type pernr_d,
*         kostl   type kostl,
*         emp_fte type pranz,
*       end of it_pern.

* For B/F Quan.
*DATA : it_tmp_cosl       LIKE STANDARD TABLE OF ztco_mhhrtrans
*                         WITH HEADER LINE .
* For A/A Type
data : it_ztco_mhaatype  like standard table of ztco_mhaatype
                         with header line .

* For DATA retrieval
data : begin of it_catsdb occurs 0,
          pernr     like catsdb-pernr,
          workdate  like catsdb-workdate,
          skostl    like catsdb-skostl  , "Sender CCtr
          lstar     like catsdb-lstar   ,
          rkostl    like catsdb-rkostl  , "Rec. CCtr
          awart     like catsdb-awart   ,
          unit      like catsdb-unit    ,
          status    like catsdb-status  ,
          catshours like catsdb-catshours.
data : end of   it_catsdb.


data : it_catsdb_item  like standard table of it_catsdb
                            with header line .
data : it_catsdb_sum   like standard table of it_catsdb
                            with header line .

data : it_catsdb_item3 like standard table of it_catsdb
                    with header line .

data : begin of it_cosl occurs 500.
        include structure zsco_cosl_key01.
        include structure zsco_cosl_lst01.
data : end of   it_cosl.

data : begin of i_t001p occurs 0,
         werks like t001p-werks, "PA
         btrtl like t001p-btrtl, "sub-area
         molga like t001p-molga, "country
         mofid like t001p-mofid, "Public Holiday Calendar
         mosid like t001p-mosid, "Personnel Subarea Grouping for WS
       end of i_t001p.
data : begin of i_t503 occurs 0,
         persg like t503-persg, "Employee Group
         persk like t503-persk, "Employee Subgroup
         zeity like t503-molga, "time type
       end of i_t503.

* For Getting Inactive MH
data : begin of it_inact occurs 0,
         pernr  like pa0000-pernr,
         date   type datum,
         anzhl  like ztco_mha-anzhl,
         dayhc  like ztco_mha-dayhc,
         massn  like pa0000-massn,
         massg  like pa0000-massg,

         kostl  like pa0001-kostl,
         stell  like pa0001-stell,
         persg  like pa0001-persg,
         persk  like pa0001-persk,
       end of it_inact.

data : begin of it_pa0000 occurs 0,
         pernr  like pa0000-pernr,
         endda  like pa0000-endda,
         begda  like pa0000-begda,
         massn  like pa0000-massn,
         massg  like pa0000-massg,
       end of it_pa0000.

data : begin of it_pa0001 occurs 0,
         pernr  like pa0001-pernr,
         begda  like pa0001-begda,
         endda  like pa0001-endda,
         kostl  like pa0001-kostl,
         stell  like pa0001-stell,
         persg  like pa0001-persg,
         persk  like pa0001-persk,
       end of it_pa0001.


data : begin of it_jc occurs 0,
         jobcd  like ztco_jc-jobcd,
         stell  like ztco_jc-stell,
       end of it_jc.

data : g_total_anzhl like ztco_mha-anzhl.

data: gv_msg(100).

** Range
ranges : r_perid    for ztco_mhhrtrans-perid.
ranges:  r_wt       for t512w-lgart.

** Data date/time
data: g_prv_date like sy-datum,
      g_prv_time like sy-uzeit.
data: g_datefr   like sy-datum,
      g_dateto   like sy-datum.

** For BAPI
data : it_costcenterlist like standard table of bapi0012_cclist
                         with header line.
data : it_return         like standard table of bapiret2
                         with header line.


* Globale Daten
include rptbal01.

*--- ALV
type-pools: slis.
data : w_fieldcat type slis_t_fieldcat_alv with header line,
       w_eventcat type slis_t_event with header line,
       w_selfield type slis_selfield,
       w_sortcat  type slis_t_sortinfo_alv with header line,
       w_col_pos  type i,
       w_program  like sy-repid,
       w_top_of_page type slis_t_listheader,
       w_line1 type slis_listheader.

data: g_repid     like sy-repid,
      gt_fieldcat type slis_t_fieldcat_alv,
*      gs_layout   type slis_layout_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_sorts    type slis_t_sortinfo_alv with header line,
      gs_prnt     type slis_print_alv.
*---- ALV


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
parameters : p_kokrs like csks-kokrs   memory   id cac obligatory,
             p_gjahr like anlp-gjahr   memory   id gjr obligatory.

selection-screen begin of line.
selection-screen comment 1(31) text-002.
*   From Period.
parameters: p_frper like rku01g-perbi obligatory.

selection-screen comment 52(05) text-003.
*   To Period.
parameters: p_toper like rku01g-perbi no-display. " OBLIGATORY.
selection-screen end of line.
parameter: p_run   as checkbox,
           p_cbo   as checkbox.

selection-screen end of block bl1.

parameter: p_delta as checkbox.

selection-screen begin of block bl2 with frame title text-001.
parameters : p_lstar like csla-lstar  default 'MAN_HR' no-display,
             p_ncoal like grpdynp-name_coall    default 'HMMA1'.
select-options : s_kostl for p0001-kostl.
parameters: p_mofid like t552a-mofid default 'U1' no-display.
selection-screen end of block bl2.

* For HR Time Sheet
selection-screen begin of block bl3 with frame title text-006.
select-options : s_status for catsdb-status     default '30'  "Approved.
                                                       no-display.
"OBLIGATORY.
selection-screen end of block bl3.

select-options : s_pernr    for p0001-pernr,
                 s_workdt   for catsdb-workdate no intervals.
*PARAMETERS: p_prdat LIKE sy-datum,
*            p_prtim LIKE sy-uzeit.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.
* Set Global ALV Parameter
  g_repid = sy-repid.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen.
* Check period range
  perform check_period_range.
* Searching for CCtr group
at selection-screen on value-request for p_ncoal.
  perform read_cctr_group.

at selection-screen output.
  authority-check object 'S_DEVELOP'
     id 'DEVCLASS' dummy
     id 'OBJTYPE'  field 'DEBUG'
     id 'OBJNAME'  dummy
     id 'P_GROUP'  dummy
     id 'ACTVT'    field '03'.
  if  sy-subrc <> 0.
    loop at screen.
      if screen-name  cs 'S_PERNR'.
        screen-input = 0.
        screen-invisible = 1.
        modify screen.
      endif.
    endloop.
  endif.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.
** Calculating Period Count
*  perform cal_per_count.
* Controlling Area Information
  perform read_tka01.
* Read CCtrs
  perform read_cctr.
* Preparation to select data
  perform read_basic_data.
  perform pre_to_select.

  if p_cbo = 'X'.
    select * into table it_time from ztco_mha
       where gjahr = p_gjahr
         and perid in r_perid
         and kostl in pnpkostl
         and pernr in s_pernr.
  else.
    if p_run   = 'X'.
      perform confirm_deletion.
    endif.

*non payroll-------------------
* Attendence/Absence Type
    perform read_catsdb_aatypes.
* Supporting/Supported.
    perform read_catsdb_supporting.

*Payroll-----------------------
    perform read_fr_time_eval.

    perform read_hire_fire.
    perform read_pa0000.
    perform collect_mh.
    perform calc_wdays.

*---collect inactive time
    perform collect_inactive_time.

    perform calc_emp_cnt.
    perform calc_avg_fte.

*---collect supporting time
*---issue: net MH ?????
    perform collect_supporting_time.
*---collect absence time
    perform collect_absence_time.


  endif.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.

  if p_delta = 'X'.
    perform capture_delta_changes.
    if p_run = space.
      it_time[] = it_time_dt[].
    endif.
  endif.

  if p_cbo = space and p_run   = 'X'.
* Do not Commit Work or Dequeue explicitly
* LUW will do
    if p_delta = 'X'.
      perform delta_update.
    else.
      perform delete_table.
      commit work.
      perform db_insert.
    endif.

  else.

* Preparation of ALV
    perform pre_report_adj.
* Call ALV LIST
    perform call_alv_list.
  endif.
*----------------------------------------------------------------------*
* Sub-Routine
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_RANGE
*&---------------------------------------------------------------------*
*       Check Period range
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_period_range.
*NO-DISPLAY / Only One period
  p_toper = p_frper.
*  IF P_FRPER > P_TOPER.
*    MESSAGE E031.
*  ENDIF.
endform.                    " CHECK_PERIOD_RANGE

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR_GROUP
*&---------------------------------------------------------------------*
*       Read CCtr Group (Search Help)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_cctr_group.
  call function 'K_GROUP_SELECT'
    exporting
*     BUTTONS                  = 'X'
      class                    = '0101'
*     CRUSER                   = '*'
      field_name               = space
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      searchfld_required       = ' '
*     SET                      = GV_CCGR_SETID
*     START_COLUMN             = 10
*     START_ROW                = 5
*     TABLE                    = 'CCSS'
*     TYPELIST                 = 'BS'
*     UPDUSER                  = '*'
*     KOKRS                    =
*     KTOPL                    =
    importing
*     CLASS_NAME               =
      set_name                 = p_ncoal
*     SET_TITLE                =
*     TABLE_NAME               =
*     SETID                    =
    exceptions
      no_set_picked            = 1
      others                   = 2.

* No error check for F4  SH
  if sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                    " READ_CCTR_GROUP

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_tka01.
  clear tka01.
  select single * from tka01
                 where kokrs = p_kokrs.
  if sy-subrc <> 0.
    message e038 with p_kokrs.
  endif.
endform.                    " Read_TKA01

*&---------------------------------------------------------------------*
*&      Form  PRE_TO_SELECT
*&---------------------------------------------------------------------*
*       Preparation to select data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pre_to_select.
  select * into corresponding fields of table i_t001p
     from t001p inner join t500p
       on t001p~werks = t500p~persa
     where t500p~bukrs = p_kokrs.
  select * into corresponding fields of table i_t503
     from t503.

* Get No working daily work scheduling
  tables: t550a.
  ranges: r_tprog for t550a-tprog.
  r_tprog-option = 'EQ'.
  r_tprog-sign   = 'I'.
  select * from t550a where sollz = 0.
    r_tprog-low = t550a-tprog.
    append r_tprog.
  endselect.

* Read A/A type
*  CLEAR : it_ztco_mhaatype, it_ztco_mhaatype.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_mhaatype
*           FROM ztco_mhaatype
*          WHERE selind = 'X'.
*
*  IF it_ztco_mhaatype[] IS INITIAL .
*    MESSAGE e042.
*  ENDIF.

* Convert Periods to date Range
  call function 'FIRST_DAY_IN_PERIOD_GET'
       exporting
            i_gjahr = p_gjahr
            i_periv = tka01-lmona
            i_poper = p_frper
       importing
            e_date  = g_datefr.

  call function 'LAST_DAY_IN_PERIOD_GET'
       exporting
            i_gjahr = p_gjahr
            i_periv = tka01-lmona
            i_poper = p_toper
       importing
            e_date  = g_dateto.

  describe table s_workdt lines sy-tabix.
  if sy-tabix = 0.
    s_workdt-low = g_datefr.
    s_workdt-high = g_dateto.
  else.
    if s_workdt-low < g_datefr.
      s_workdt-low = g_datefr.
    endif.

    if s_workdt-high is initial or s_workdt-high > g_dateto.
      s_workdt-high = g_dateto.
    endif.
  endif.

  s_workdt-sign   = 'I'.
  s_workdt-option = 'BT'.
  append s_workdt.

* Period Range
  clear : r_perid, r_perid[].
* Building Period Range
  r_perid-low    = p_frper.
  r_perid-high   = p_toper.
  r_perid-sign   = 'I'.
  r_perid-option = 'BT'.
  append r_perid.  clear  r_perid.


*FIXME - critical error!!!!
*holiday calendar - exit in T001P
  call function 'DAY_ATTRIBUTES_GET'
       exporting
            factory_calendar = 'HM'
            holiday_calendar = p_mofid
            date_from        = s_workdt-low
            date_to          = s_workdt-high
       tables
            day_attributes   = it_date.

* RPUWSH00   Revaluation of the Planned Working Time Infotype (0007)

* T-code: PT03
* Monthly Work Schedule
  select * from t552a
      for all entries in i_t001p
      where mofid = i_t001p-mofid
        and mosid = i_t001p-mosid
        and kjahr = p_gjahr
        and monat = p_toper.
*        and schkz = '2005'.
    move-corresponding t552a to it_t552a. append it_t552a.
  endselect.

  data: lstr(6) type c.
  concatenate p_gjahr p_frper+1(2) into lstr.
  field-symbols : <fs> type any.
  data : num(2) type n.
  data : f_field(14).
  data: l_idx like sy-tabix.

  loop at it_t552a.
    it_sch-schkz   = it_t552a-schkz  .
    it_sch-zeity   = it_t552a-zeity  .
    it_sch-mosid   = it_t552a-mosid  .

    num = 1.
    do 31 times.
      concatenate lstr num into it_sch-date.

      concatenate 'IT_T552A-TPR' num into f_field.
      assign (f_field) to <fs>.
      if <fs> eq space.
        exit.
      endif.
      if <fs> in r_tprog.  "'1008'.    "Day Off
        it_sch-freeday = 'X'.
      else.
        it_sch-freeday = ' '.
      endif.
      append it_sch.

* calc work days.
* 3 group + 2 shift; 10 hours; SAT/SUN
* 2 group + 2 shift
      clear i_wdays.
      move-corresponding it_sch  to i_wdays.
      read table it_date with key date = it_sch-date.

      i_wdays-ttday = 1.

      if it_date-holiday = 'X'.
        if it_sch-freeday = ' '.
          i_wdays-hoday = 1.
          i_wdays-works = 1.
        endif.
      else.
        if it_date-weekday < 6.     "Mo~Fr
          if it_sch-freeday = ' '.
            i_wdays-wkday = 1.
            i_wdays-works = 1.
          endif.
        elseif it_date-weekday = 6. "SAT
          if it_sch-freeday = ' '.
            i_wdays-saday = 1.
            i_wdays-works = 1.
          endif.
        else.                       "SUN
          if it_sch-freeday = ' '.
            i_wdays-suday = 1.
            i_wdays-works = 1.
          endif.
        endif.
      endif.
*         wdays TYPE i,   "week work days
*         odays TYPE i,   "ot work days

*         wkday TYPE i,   "week days
*         saday TYPE i,   "SAT days
*         suday TYPE i,   "SUN days
*         hoday TYPE i,   "HOL days

      collect i_wdays. clear i_wdays.

      clear: it_sch-freeday, it_sch-shift, it_sch-date.
      num = num + 1.
    enddo.

    read table i_wdays with key schkz   = it_t552a-schkz
                                zeity   = it_t552a-zeity
                                mosid   = it_t552a-mosid.
    l_idx = sy-tabix.
    i_wdays-solst = it_t552a-solst.
    modify i_wdays index l_idx.

  endloop.

  data: l_mon like t009b-bumon.
  l_mon = p_frper+1(2).
  call function 'NUMBER_OF_DAYS_PER_MONTH_GET'
       exporting
            par_month = l_mon
            par_year  = p_gjahr
       importing
            par_days  = g_totdays.

  clear g_workdays.
  loop at it_date where freeday = space.
    g_workdays = g_workdays + 1.
  endloop.


* WLB3_GET_NUMBER_OF_WORKDAYS

endform.                    " PRE_TO_SELECT

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_MHHRTRANS
*&---------------------------------------------------------------------*
*       Enqueue
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form enqueue_ztco_mhhrtrans.

  data : lv_perid like ztco_mhhrtrans-perid.

  lv_perid = p_frper.

  do 16 times .
    if lv_perid =< p_toper.
      call function 'ENQUEUE_EZCO_ZTCO_MHHRTR'
        exporting
          mode_ztco_mhhrtrans       = 'E'
          mandt                     = sy-mandt
          gjahr                     = p_gjahr
          perid                     = lv_perid
*         KOSTL                     =
*         LSTAR                     =
*         X_GJAHR                   = ' '
*         X_PERID                   = ' '
*         X_KOSTL                   = ' '
*         X_LSTAR                   = ' '
*         _SCOPE                    = '2'
*         _WAIT                     = ' '
*         _COLLECT                  = ' '
        exceptions
          foreign_lock              = 1
          system_failure            = 2
          others                    = 3.

      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.
* Period Counting
    lv_perid = lv_perid  + 1.

  enddo.
endform.                    " ENQUEUE_ZTCO_MHHRTRANS

*&---------------------------------------------------------------------*
*&      Form  confirm_deletion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form confirm_deletion.
* local Data definition
  data : lv_answer,
         lv_cnt type sy-dbcnt.
  data : lv_title(80).

* Begin of changes -  UD1K920571
*  SELECT COUNT( * ) INTO lv_cnt FROM ztco_mha
*           WHERE gjahr = p_gjahr
*             AND perid IN r_perid.
*  IF lv_cnt = 0.
*    SELECT COUNT( * ) INTO lv_cnt FROM ztco_mhat
*             WHERE gjahr = p_gjahr
*               AND perid IN r_perid.
*  ENDIF.
* End of changes  UD1K920571.

  if lv_cnt > 0.
    clear : lv_answer,  lv_title.
   concatenate 'All data will be lost ' p_gjahr '/' p_frper '/' p_toper
                                                          into lv_title.

    call function 'POPUP_TO_CONFIRM_STEP'
      exporting
*     DEFAULTOPTION        = 'Y'
        textline1            = lv_title
        textline2            = 'In Table - ZTCO_MHHRTRANS'
        titel                = 'Delete DATA in Table'
*     START_COLUMN         = 25
*     START_ROW            = 6
*     CANCEL_DISPLAY       = 'X'
      importing
        answer               = lv_answer.

    if  lv_answer <> 'J'.
      message e043.
    else.
*      perform delete_table.
    endif.
  endif.



endform.                    " confirm_deletion

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR
*&---------------------------------------------------------------------*
*       Read CCtrs for retrieval.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_cctr.
* Making an internal table for CCtr to select data
  data : lv_datum like sy-datum.
  concatenate p_gjahr p_frper+1(2) '01' into lv_datum.

  clear : it_costcenterlist, it_costcenterlist[],
          it_return,         it_return[].

  call function 'BAPI_COSTCENTER_GETLIST1'
       exporting
            controllingarea = p_kokrs
            date_from       = lv_datum
            costcentergroup = p_ncoal
       tables
            costcenterlist  = it_costcenterlist
            return          = it_return.
* Message
  perform dis_bapi_message.


* Cost center
  clear : pnpkostl, pnpkostl[].
  pnpkostl-sign   = 'I'.
  pnpkostl-option = 'EQ'.

  loop at it_costcenterlist.
    check it_costcenterlist-costcenter in s_kostl.
    pnpkostl-low    = it_costcenterlist-costcenter.
    append pnpkostl.
  endloop.

endform.                    " READ_CCTR

*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*       Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form dis_bapi_message.
  if not it_return[] is initial.
    loop at   it_return.
      message id     it_return-id
              type   it_return-type
              number it_return-number
              with   it_return-message_v1
                     it_return-message_v2
                     it_return-message_v3
                     it_return-message_v4.
    endloop.
  endif.
endform.                    " DIS_BAPI_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       CALL ALV LIST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_alv_list.
  g_repid = sy-repid.
  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       tables
            t_outtab           = it_time
       exceptions
            program_error      = 1
            others             = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " CALL_ALV_LIST

*---------------------------------------------------------------------*
*       FORM PF_STATUS                                                *
*---------------------------------------------------------------------*
*       PF_STATUS                                                     *
*---------------------------------------------------------------------*
form pf_status using  extab type slis_t_extab.
  set pf-status 'BALVLIST' excluding extab.
endform.

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       For User_command - AT User Command                            *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
form user_command using ucomm    like sy-ucomm
                        selfield type slis_selfield.
  case ucomm.
* Important part !
* For POST - DB Update
    when 'UPDA' .
*      PERFORM update.
  endcase.

endform.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
form basic_top_of_page.
*  write : / 'Controlling Area                         : '
*            , p_kokrs.
*  write : / 'Fiscal Year/Period/Version/Activity Type : '
*            , p_gjahr, '/', p_frper, '~', p_toper, '/', p_versn
*            , '/', p_lstar.
*  write : / 'Value Type/CO business transaction       : '
*            , p_wrttp, '/',  s_vrgng-low, '~',  s_vrgng-high.

endform.

*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pre_report_adj.

* Building Field Cat.
  clear : gt_fieldcat, gt_fieldcat[].
  perform field_setting tables gt_fieldcat using :
   'KOSTL'    'CC'           '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*  'ZEITY'    'ZEITY'        '01' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'JOBCD'    'JC'           '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'PERNR'    'PERNR'        '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'DTYPE'    'DT'           '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'LGART'    'WT'           '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'ANZSH'    'Shift'        '01' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'SCHKZ'    'WS'           '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'ANZHL'    'ANZHL'        '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'NETMH'    'NETMH'        '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'EMP_CNT'  'EMP_CNT'      '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'EMP_AVG'  'EMP_AVG'      '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'EMP_FTE'  'EMP_FTE'      '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
*  'EMP_DAY'  'EMP_DAY'      '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'DAYHC'    'DAYHC'        '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'WDAYS'    'WDAYS'        '10' ' ' 'R'  ' '  ' '  ' '  ' '  ' ',
   'SOLST'    'SOLST'        '10' ' ' 'R'  ' '  ' '  ' '  ' '  ' '.

endform.                    " PRE_REPORT_ADJ

*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
form field_setting tables p_fieldcat_t like gt_fieldcat using
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .
  data: ls_fieldcat type slis_fieldcat_alv.
  clear ls_fieldcat.

*  add 1 to gv_col_pos.

  ls_fieldcat-tabname    = 'IT_TIME'.

  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.
*  ls_fieldcat-col_pos    = gv_col_pos.

  append ls_fieldcat to gt_fieldcat.

endform.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  db_insert
*&---------------------------------------------------------------------*
form db_insert.
  data: l_it_time like it_time.
  l_it_time-erdat = sy-datum.
  l_it_time-erzet = sy-uzeit.
  l_it_time-ernam = sy-uname.

  modify it_time from l_it_time transporting erdat erzet ernam
         where kokrs eq p_kokrs and
               gjahr eq p_gjahr.

  insert ztco_mha  from table it_time.
  if sy-subrc = 0.
    insert ztco_mhat from table it_timet.

    if sy-subrc = 0.
      insert ztco_mhad from table it_timed.
    endif.
  endif.

*    MODIFY ztco_mha  FROM TABLE it_time.
*    IF sy-subrc = 0.
*      MODIFY ztco_mhat FROM TABLE it_timet.
*    ENDIF.
*    IF sy-subrc = 0.
*      MODIFY ztco_mhad FROM TABLE it_timed.
*    ENDIF.

  if sy-subrc <> 0.
    message e044.
  else.
    message s009 with 'Data Inserted'.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  delta_update
*&---------------------------------------------------------------------*
form delta_update.
  data: l_it_time like it_time.
  l_it_time-erdat = sy-datum.
  l_it_time-erzet = sy-uzeit.
  l_it_time-ernam = sy-uname.

  modify it_time_dt
         from l_it_time transporting erdat erzet ernam
         where kokrs eq p_kokrs and
               gjahr eq p_gjahr.

  modify ztco_mha  from table it_time_dt.
  if sy-subrc = 0.
    modify ztco_mhat from table it_timet_dt.
  endif.
  if sy-subrc = 0.
    modify ztco_mhad from table it_timed_dt.
  endif.

  if sy-subrc <> 0.
    message e044.
  else.
    message s009 with 'Delta Updated'.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  read_fr_time_eval
*&---------------------------------------------------------------------*
*       Read TIMESHEET data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_fr_time_eval.

* Ingnore A/A type
* HR source has no indicator to check if supportive hour or
* non-supportive hour

  data : pnpbegda like qppnp-begda,
         pnpendda like qppnp-endda,
         pnptimr6 like qppnp-timr6.
  data : sw_zl    like rptxxxxx-kr_feld3.

  data : it_l_rsparams like standard table of rsparams
                       with header line .
*
  call function 'RS_REFRESH_FROM_SELECTOPTIONS'
    exporting
      curr_report           = 'ZACO03U_HRMH'
*   IMPORTING
*     SP                    =
    tables
      selection_table       = it_l_rsparams
    exceptions
      not_found             = 1
      no_report             = 2
      others                = 3.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

* Put data
  pnptimr6 = 'X'.
  sw_zl    = 'X'.

  clear s_workdt.
  read table s_workdt index 1.
  pnpbegda = s_workdt-low.
  pnpendda = s_workdt-high.

  free memory id 'HRM'.
  submit zaco03u_hrmh
*    VIA SELECTION-SCREEN
    and return
    with selection-table it_l_rsparams
    with pnppernr  in s_pernr
    with pnptimr6 = pnptimr6
    with pnpbegda = pnpbegda
    with pnpendda = pnpendda
    with sw_zl    = sw_zl           "only time wage
    with lgart    in r_wt
    with pnpkostl in pnpkostl.


*<data_tab>
  import time_data_zes   = time_data_zes
         time_data_saldo = time_data_saldo
         time_data_zl    = time_data_zl
         data_tab        = data_tab
         from memory id 'HRM'.


** Re-org.
  data : lv_tabcol(30).
  data : lv_kostl(5) value 'KOSTL', lv_anzhl(5) value 'ANZHL',
         lv_pernr(5) value 'PERNR', "emp#
         lv_datum(5) value 'DATUM', "date
         lv_schkz(5) value 'SCHKZ', "shift
         lv_stell(5) value 'STELL', "job
         lv_lgart(5) value 'LGART', "wagetype
         lv_zeity(5) value 'ZEITY', "subgroup for workschedule
         lv_mosid(5) value 'MOSID', "Psub group for workschedule
         lv_werks(5) value 'WERKS',
         lv_persg(5) value 'PERSG',
         lv_persk(5) value 'PERSK',
         lv_btrtl(5) value 'BTRTL'.

  data : l_lgart    type lgart.

  field-symbols : <fstab> type table,
                  <fswa>  type any,
                  <fsfn>  type any,
                  <fsval> type any.

  concatenate data_tab '[' ']' into lv_tabcol.
  assign (lv_tabcol) to <fstab>.


  assign local copy of initial line of <fstab> to <fswa>.
* If no data found in HR tables
  if sy-subrc <> 0.
    message s000 with text-101.
    exit.
  endif .

  data: l_stell like pa0001-stell.

  loop at <fstab> assigning <fswa>.
* CCTR -KOSTL
    assign lv_kostl to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    py_times-kostl = <fsval> .
* MAN_HR -ANZHL
    assign lv_anzhl to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    py_times-anzhl = <fsval>.
* WageType
*1	Regular Working (weekday,weekend)
*2	OT
*5	Paid Leave
*9	Holiday
*A	Absence Time
*U	Unpaid Leave
    assign lv_lgart to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    l_lgart = <fsval> .
*    READ TABLE i_timetype WITH KEY lgart = l_lgart.
*    py_times-lgart  = i_timetype-zgart.
    py_times-lgart2 = l_lgart.

*HEADER
    assign lv_werks to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    py_times-werks = <fsval> .
    assign lv_persg to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    py_times-persg = <fsval> .
    assign lv_persk to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    py_times-persk = <fsval> .
    assign lv_btrtl to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    py_times-btrtl = <fsval> .


* work schedule rule
*  1000 - standard
*  1001 - admin 1st  1002 - admin 2nd
*  2001 - PP 1st,    2002 - PP 2nd
*  3001 - PM 1st,    3002 - PM 2nd  3003 - PM 3rd
* Refer T508A table...
    assign lv_schkz to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    py_times-schkz = <fsval> .

    read table i_t001p with key werks = py_times-werks
                                btrtl = py_times-btrtl.
    py_times-mofid = i_t001p-mofid.
    py_times-mosid = i_t001p-mosid.

* DATE
    assign lv_datum to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    py_times-datum = <fsval> .
* EMP#
    assign lv_pernr to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    py_times-pernr = <fsval> .

*'A' US-Salary, 'B' US-Wage, 'K' KR-Salary
*    IF py_times-persg = '9'.
*      py_times-empct = 'K'.
*    ELSEIF py_times-zeity = '1'.
*      py_times-empct = 'B'.
*    ELSE.
*      py_times-empct = 'A'.
*    ENDIF.
    perform get_emp_categ(zacop01) using py_times-persg py_times-persk
                                   changing py_times-empct.

* time type: 1 - hourly, 2 - salary
    read table i_t503 with key persg = py_times-persg
                              persk = py_times-persk.
    py_times-zeity = i_t503-zeity.

* job
    assign lv_stell to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    l_stell = <fsval>.
    read table i_jobcd with key stell = l_stell.
    if sy-subrc = 0.
      py_times-jobcd = i_jobcd-jobcd.
    else.
      py_times-jobcd = 'XX'.
    endif.

    collect py_times.
    clear <fswa>.

*Employee
    i_emp-pernr = py_times-pernr. append i_emp.

  endloop.

  delete adjacent duplicates from i_emp.
endform.                    " read_fr_time_eval
*&---------------------------------------------------------------------*
*&      Form  read_catsdb_aatypes
*&---------------------------------------------------------------------*
form read_catsdb_aatypes.

  ranges : r_awart for catsdb-awart.
  r_awart-sign   = 'I'.
  r_awart-option = 'EQ'.
  select * from ztco_mh_time
     where ( zgart = 'B' and flag  = 'X' )
        or   zgart = 'A'.
    r_awart-low = ztco_mh_time-lgart. append r_awart.
  endselect.

  if sy-subrc <> 0.
    message e026.
  endif.

* Select data (approved status: 30)
  select pernr
         workdate  skostl  lstar   rkostl
         awart     unit    status  catshours
         into corresponding fields of table it_catsdb_item
         from catsdb
        where awart    in r_awart
          and status   in s_status
          and workdate in s_workdt
          and pernr    in s_pernr.

  loop at it_catsdb_item.
    it_catsdb_sum-pernr     = it_catsdb_item-pernr.
    it_catsdb_sum-workdate  = it_catsdb_item-workdate.
    it_catsdb_sum-catshours = it_catsdb_item-catshours.
    collect it_catsdb_sum.
  endloop.

  sort it_catsdb_sum by pernr workdate.

*----deleted logic----
*date, time
*  g_cur_date = sy-datum.
*  g_cur_time = sy-uzeit.
*  SELECT SINGLE MAX( erdat ) MAX( erzet )
*           INTO (g_prv_date, g_prv_time)
*           FROM ztco_mha
*           WHERE gjahr = p_gjahr
*             AND perid IN r_perid.
*
*
*
** check index of table : Z02
*  CLEAR : it_catsdb_item, it_catsdb_item[].
*
*  IF NOT g_prv_date IS INITIAL.
*    SELECT workdate  skostl  lstar   rkostl
*           awart     unit    status  catshours
*           INTO CORRESPONDING FIELDS OF TABLE it_catsdb_item
*           FROM catsdb
*          WHERE status   IN s_status
*            AND workdate IN s_workdt
*            AND ersda >= g_prv_date
*            AND erstm >  g_prv_time.
**..old data don't have date/time stamp...
*  ELSE.
*    SELECT workdate  skostl  lstar   rkostl
*           awart     unit    status  catshours
*           INTO CORRESPONDING FIELDS OF TABLE it_catsdb_item
*           FROM catsdb
*          WHERE status   IN s_status
*            AND workdate IN s_workdt
*            AND ersda >= p_prdat
*            AND erstm >  p_prtim.
*  ENDIF.
endform.                    " read_catsdb_aatypes
*&---------------------------------------------------------------------*
*&      Form  read_catsdb_supporting
*&---------------------------------------------------------------------*
*       Cal. Supportive and Not-Supportive Working Hour
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_catsdb_supporting.
*FIXME
* need to collect ztco_mha information... later

* TimeSheet  (Summed)
*  clear it_tmp_catsdb.
*  sort  it_tmp_catsdb by gjahr perid kostl lstar.

  ranges : r_awart for catsdb-awart.
  r_awart-sign   = 'I'.
  r_awart-option = 'EQ'.
  select * from ztco_mh_time
     where zgart = '3'
       and flag  = 'X'.
    r_awart-low = ztco_mh_time-lgart. append r_awart.
  endselect.

  if sy-subrc <> 0.
    message e026.
  endif.

* Local Data definition

* Select data (approved status: 30)
  clear : it_catsdb_item3, it_catsdb_item3[].
  select pernr     workdate  skostl  lstar   rkostl
         awart     unit    status  catshours
         into corresponding fields of table it_catsdb_item3
         from catsdb
        where awart    in r_awart
          and status   in s_status
          and workdate in s_workdt
          and pernr    in s_pernr
          and catshours <> 0.

  if  it_catsdb_item3[] is initial.
*    MESSAGE E026.
  endif.


*FIXIT LATER - ANDY - start...
* Cal.
*  data : it_l_catadd like standard table of it_tmp_catsdb
*                     with header line .
*  loop at it_l_calwsig.
*    loop at it_tmp_catsdb where  kostl  = it_l_calwsig-kostl.
*      it_tmp_catsdb-actqty = it_tmp_catsdb-actqty
*                           + it_l_calwsig-catshours.
*      modify it_tmp_catsdb.
*      clear  it_tmp_catsdb.
*    endloop.
*    if sy-subrc <> 0.
*      move-corresponding it_l_calwsig to it_l_catadd.
*      it_l_catadd-gjahr = p_gjahr.
*      it_l_catadd-perid = p_frper.
*      it_l_catadd-lstar = p_lstar.
*      it_l_catadd-unit = 'STD'.
*      it_l_catadd-actqty = it_l_calwsig-catshours.
*      collect it_l_catadd.
*    endif.
*    clear it_l_catadd.
*    clear it_l_calwsig.
*  endloop.
*
*  loop at it_l_catadd.
*    clear  it_tmp_catsdb.
*    move-corresponding it_l_catadd to it_tmp_catsdb.
*    collect it_tmp_catsdb.
*    clear   it_tmp_catsdb.
*  endloop.
*  clear  it_tmp_catsdb.
*FIXIT LATER - ANDY - end.....



*deleted...logic
** Sender '-'
*  SORT it_catsdb_item3 BY SKOSTL .
*  LOOP AT IT_TMP_CATSDB.
*    LOOP AT it_catsdb_item3 WHERE  SKOSTL  = IT_TMP_CATSDB-KOSTL.
*      IT_TMP_CATSDB-ACTQTY = IT_TMP_CATSDB-ACTQTY
*                           - it_catsdb_item3-CATSHOURS.
*    ENDLOOP.
*    MODIFY IT_TMP_CATSDB.
*    CLEAR  IT_TMP_CATSDB.
*  ENDLOOP.
*
** Receiver '-'
*  SORT it_catsdb_item3 BY RKOSTL .
*  LOOP AT IT_TMP_CATSDB.
*    LOOP AT it_catsdb_item3 WHERE  RKOSTL  = IT_TMP_CATSDB-KOSTL.
*      IT_TMP_CATSDB-ACTQTY = IT_TMP_CATSDB-ACTQTY
*                           + it_catsdb_item3-CATSHOURS.
*    ENDLOOP.
*    MODIFY IT_TMP_CATSDB.
*    CLEAR  IT_TMP_CATSDB.
*  ENDLOOP.

endform.                    " read_catsdb_supporting

*
*&---------------------------------------------------------------------*
*&      Form  read_basic_data
*&---------------------------------------------------------------------*
*       Set the wage type, but code did not use this now
*----------------------------------------------------------------------*
form read_basic_data.
* Jobcode
  select * into table i_jobcd
     from ztco_jc.


  select * into table i_wksch
     from ztco_mh_ws
     where kokrs = p_kokrs.

  select * into table i_timetype
     from  ztco_mh_time.
  if sy-subrc <> 0.
    message e000 with 'Maintain Time Types'.
  endif.
  delete i_timetype where flag = 'X'.

  refresh r_wt. clear r_wt.
  r_wt-sign   = 'I'. r_wt-option = 'EQ'.

*exclude inactive.
  loop at i_timetype where zgart <> 'V'.
    r_wt-low  = i_timetype-lgart.  append r_wt.
  endloop.

* BEGIN OF UD1K949773
  select * into table i_dwksch
    from ztco_mh_dws.
* END OF UD1K949773
endform.                    " read_basic_data
*&---------------------------------------------------------------------*
*&      Form  get_weekday
*&---------------------------------------------------------------------*
*FORM get_weekday USING    f_datum.
**                 CHANGING f_weekday
**                          f_freeday
**                          f_holiday.
*
*  READ TABLE it_date WITH KEY date = f_datum.
*  f_weekday = it_date-weekday.
*  f_freeday = it_date-freeday.
*  f_holiday = it_date-holiday.
*
*ENDFORM.                    " get_weekday
*&---------------------------------------------------------------------*
*&      Form  calc_avg_fte
*&---------------------------------------------------------------------*
form calc_avg_fte.
  data: l_tabix like sy-tabix.
  data: l_arbst like pa0007-arbst,
        l_wdays like it_time-wdays.
*.. regular working, leave, holiday
  data: l_solst like t552a-solst,
        l_sols2 like t552a-solst.


* calculate FTE with regular working
  sort it_time by pernr
                  lgart.
*                 datum
*                  kostl.

* by ig.moon {
  sort i_wdays by mosid schkz.
* }

* REGULAR + OT + Supporting/Supported
  loop at it_time where lgart <= '3'.

    clear i_wdays.                                          "UD1K949773
    read table i_wdays with key mosid = it_time-mosid
                                schkz = it_time-schkz
* by ig.moon {
                                binary search.
* }

*                               zeity = it_time-zeity.  "FIXME!!!
    l_solst = i_wdays-solst.
    l_wdays = i_wdays-works.

    if l_solst = 0.
      it_time-emp_fte = 0.
    else.
      it_time-emp_fte = it_time-anzhl / l_solst.
    endif.

*consider only regulary working
    data: l_maxdh like it_time-dayhc.
    if it_time-wdays > 0.           "dtype = '1'.
      if l_wdays <> 0.                                      "UD1K949773
        if it_time-dayhc > it_time-wdays.
          it_time-emp_avg = it_time-wdays / l_wdays.
        else.
          it_time-emp_avg = it_time-dayhc / l_wdays.
        endif.
      endif.                                                "UD1K949773
    endif.

    it_time-solst   = l_solst.

*only update Headcnt
    clear: it_time-emp_cnt,
           it_time-dayhc,  "it_time-emp_day,
           it_time-anzhl,   it_time-netmh,
           it_time-wdays.
    collect it_time.
  endloop.

endform.                    " calc_avg_fte
*&---------------------------------------------------------------------*
*&      Form  read_hire_fire
*&---------------------------------------------------------------------*
form read_hire_fire.
  data: l_hire like sy-datum,
        l_fire like sy-datum.
  data: begin of phifi occurs 5.
          include structure phifi.
  data: end of phifi.
  data: error_table like standard table of rpbenerr.

  loop at i_emp.
    call function 'HR_CLM_GET_ENTRY_LEAVE_DATE'
         exporting
              pernr       = i_emp-pernr
              begda       = s_workdt-low
              endda       = s_workdt-high
         importing
              hire_date   = i_emp-hire
              fire_date   = i_emp-fire
         tables
              error_table = error_table
              phifi       = phifi.

*HR IT error
    if i_emp-fire is initial.
      read table phifi with key stat2 = '0'.
      call function 'RP_CALC_DATE_IN_INTERVAL'
           exporting
                date      = phifi-begda
                days      = 1
                months    = '00'
                signum    = '-'
                years     = '00'
           importing
                calc_date = i_emp-fire.

    endif.

    select * from pa0001
      where pernr = i_emp-pernr
        and endda >= s_workdt-low
      order by endda ascending.
      i_emp-werks = pa0001-werks.
      i_emp-btrtl = pa0001-btrtl.
      exit.
    endselect.
    if sy-subrc = 0.
*-----Personnel Subarea Grouping for Work Schedules
      read table i_t001p with key werks = i_emp-werks
                                  btrtl = i_emp-btrtl.
      i_emp-mosid = i_t001p-mosid.
    endif.
* Get Last CC
*  select single * kostl  into i_emp-kostl
*    from pa0001
*    where pernr = i_emp-pernr
*      and endda >= s_workdt-low.

    modify i_emp.
  endloop.

* work time
  select * into corresponding fields of table i_pa7
    from pa0007
    for all entries in i_emp
    where pernr = i_emp-pernr
      and endda >= s_workdt-low.

endform.                    " read_hire_fire
*&---------------------------------------------------------------------*
*&      Form  collect_mh
*&---------------------------------------------------------------------*
form collect_mh.

  data: l_dtype(1) type c,
        l_datum    like sy-datum.
  data: l_arbst like pa0007-arbst.

  sort py_times by pernr datum descending.

* by ig.moon {
  sort : i_emp by pernr,
         i_wksch by mosid schkz.
  sort i_wdays by mosid schkz.
* }

  loop at py_times.
*Check Fire Date again.
    read table i_emp with key pernr = py_times-pernr
* by ig.moon. {
    binary search.
* }
    check i_emp-fire > s_workdt-low.

    it_time-kokrs = p_kokrs.
    it_time-gjahr = p_gjahr.
    it_time-perid = p_frper.

* CCTR -KOSTL
    it_time-kostl = py_times-kostl.
* MAN_HR -ANZHL
    it_time-anzhl = py_times-anzhl.
* WageType
    it_time-lgart = py_times-lgart.

* EMP#
    it_time-pernr = py_times-pernr.

* commented by ig.moon {
*    read table i_emp with key pernr = it_time-pernr.
* }

* work schedule rule
    it_time-mosid = py_times-mosid.
    it_time-schkz = py_times-schkz.
*   it_time-zeity = py_times-zeity.  "FIXME

* subgroup / group -> category
    it_time-empct = py_times-empct.

* job
    it_time-jobcd = py_times-jobcd.

* BEGIN OF UD1K949773
    data: tprog type tprog.

    CALL FUNCTION 'Z_CO_GET_DWS_IG'
         EXPORTING
              schkz                          = py_times-schkz
              datum                          = py_times-datum
         IMPORTING
              tprog                          = tprog
         EXCEPTIONS
              not_found_work_schedule_rules  = 1
              invalid_date                   = 2
              not_found_period_work_schedule = 3
              OTHERS                         = 4.

    IF sy-subrc <> 0.
      tprog = py_times-schkz.
    ENDIF.

    it_time-schkz = tprog.

    loop at i_dwksch where mosid = it_time-mosid
                       and schkz = it_time-schkz
                       and zsdat <= py_times-datum
                       and zedat >= py_times-datum.

      if not i_dwksch-zshif is initial.
        it_time-anzsh = i_dwksch-zshif.
      endif.

      exit.
    endloop.

    if sy-subrc <> 0.
      message s000 with 'Check WS-Shift configuration'
                        it_time-mosid
                        it_time-schkz
                        py_times-datum.
    endif.

** Shift (default=1)
*    it_time-anzsh = 1.
*    read table i_wksch with key mosid = it_time-mosid
*                                schkz = it_time-schkz
** by ig.moon. {
*                                binary search.
** }
*    if sy-subrc <> 0.
*      message e000 with 'Check WS-Shift configuration'
*                        it_time-mosid
*                        it_time-schkz.
*    endif.
*    if sy-subrc = 0.
*      it_time-anzsh = i_wksch-anzsh.
*    endif.
* END OF UD1K949773

*get work days.
    clear: i_wdays.
    read table i_wdays with key mosid = it_time-mosid
                                schkz = it_time-schkz
* by ig.moon {
                                binary search.
* }
*                               zeity = it_time-zeity. "FIXME

*-----get STD working hour
    if i_wdays-works = 0.
* BEGIN OF UD1K949773
*     break-point.
*     message s000 with 'Check Monthly Work Schedule (T552A)'
*                       it_time-mosid
*                       it_time-schkz.
* END OF UD1K949773
    else.
      l_arbst = i_wdays-solst / i_wdays-works.
    endif.

* DATE
    l_datum = py_times-datum.
    read table it_date with key date = l_datum.
    if it_date-holiday = 'X'.
*      it_time-holmh = it_time-anzhl.
*      it_time-holhc = 1.
      l_dtype = '2'.
    else.
      if it_date-weekday < 6.    "Mo~Fr
        if it_date-freeday = 'X'.  "plant shut-down
*          it_time-holmh = it_time-anzhl.
*          it_time-holhc = 1.
          l_dtype = '2'.   "HMC
        else.
*          it_time-daymh = it_time-anzhl.
*          it_time-dayhc = 1.
          l_dtype = '1'.
        endif.
      elseif it_date-weekday = 6. "SAT
*        it_time-satmh = it_time-anzhl.
*        it_time-sathc = 1.
        l_dtype = '5'.
      else.                       "SUN
*        it_time-sunmh = it_time-anzhl.
*        it_time-sunhc = 1.
        l_dtype = '3'.
      endif.
    endif.
*---1=Wk working, 2=Wk OT, 5=SAT, 3=SUN
    it_time-dtype = l_dtype.

*-----------------------------------------------------------
*    LOOP AT i_pa7 WHERE pernr = it_time-pernr.
*      IF  py_times-datum >= i_pa7-begda
*      AND py_times-datum <= i_pa7-endda.
*        l_arbst = i_pa7-arbst.
*      ENDIF.
*    ENDLOOP.

    read table i_timetype with key lgart = py_times-lgart2.
    it_time-lgart  = i_timetype-zgart.

*WHAT IT IS???
*    IF l_dtype <> '1' AND i_timetype-zgart = '1'
*    AND l_arbst = 8.
*      it_time-lgart  = '2'.  "Extra Work
*    ENDIF.

*-----------------------------------------------------------
* Max = 1, WK hour / STD hour
* Regular working only
    if it_time-lgart = '1'.
      if it_time-anzhl >= l_arbst.
        it_time-dayhc = 1.
      else.
        it_time-dayhc = it_time-anzhl / l_arbst.
      endif.

*...Clear Weekday Overtime HC
      if l_dtype = '1'  and it_time-lgart = '2'.
        clear: it_time-dayhc.
      endif.
    endif.

* CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'

*-----------------------------------------------------------
* Date : valid from - ztco_mha
*FIXME!!!
*    READ TABLE it_time INTO gw_time
*      WITH KEY pernr = it_time-pernr
*               kostl = it_time-kostl.
*    IF sy-subrc = 0.
*      it_time-datum = gw_time-datum.
*    ELSE.
*      it_time-datum = l_datum.
*    ENDIF.
*-----------------------------------------------------------
* Net M/H
*supporting?????
*....Regular / OT
    if it_time-lgart = '1'.
      it_time-netmh = it_time-anzhl.

      read table it_catsdb_sum with key pernr    = it_time-pernr
                                        workdate = l_datum
                               binary search.
      if sy-subrc = 0.
        it_time-netmh = it_time-netmh - it_catsdb_sum-catshours.
      endif.
    endif.

*Break-Time; Hard Coding...
    if it_time-lgart ca '12'.
      if it_time-kostl(2) = 'MX'.
        perform calc_net_mh using    it_time-anzhl
                            changing it_time-netmh.
      else.
        it_time-netmh = it_time-anzhl.
      endif.
    endif.

* add to t/d
    perform add_to_time_sum_detail using l_datum
                                         py_times-lgart2.
    collect: it_time.
    clear: it_time.
  endloop.

endform.                    " collect_mh
*&---------------------------------------------------------------------*
*&      Form  delete_table
*&---------------------------------------------------------------------*
form delete_table.
  delete from ztco_mha
       where gjahr = p_gjahr
         and perid in r_perid
         and pernr in s_pernr.

  delete from ztco_mhat
       where gjahr = p_gjahr
         and perid in r_perid.


  delete from ztco_mhad
       where gjahr = p_gjahr
         and perid in r_perid
         and pernr in s_pernr.


  exit.
  delete from ztco_mha
       where gjahr = p_gjahr
         and perid in r_perid
         and kostl in pnpkostl.

  delete from ztco_mhat
       where gjahr = p_gjahr
         and perid in r_perid
         and kostl in pnpkostl.

  delete from ztco_mhad
       where gjahr = p_gjahr
         and perid in r_perid
         and kostl in pnpkostl.
endform.                    " delete_table
*&---------------------------------------------------------------------*
*&      Form  calc_wdays
*&---------------------------------------------------------------------*
form calc_wdays.
  data: l_tabix like sy-tabix.

* by ig.moon {
  sort i_wdays by mosid schkz.
* }

*working HC (exclude overtime)
  loop at it_time.
    l_tabix = sy-tabix.

    clear i_wdays.                                          "UD1K949773
    read table i_wdays with key mosid = it_time-mosid
                                schkz = it_time-schkz
                                binary search.
*                                zeity = it_time-zeity. "FIXME

* only production member has OT days.
    read table i_jobcd with key jobcd = it_time-jobcd.
    if i_jobcd-trfgr = space.
      clear: i_wdays-hoday, i_wdays-suday, i_wdays-saday.
    endif.

*Weekday + Normal Work
*Normal Work + OT
*    IF ( it_time-dtype = '1'  AND it_time-lgart = '1' )
*    OR ( it_time-dtype <> '1' AND it_time-lgart CA '12' ).
    if it_time-lgart = '1'.
      case it_time-dtype.
        when '1'.
          if i_wdays-wkday > 0.
*           it_time-emp_day = it_time-dayhc / i_wdays-wkday.
            it_time-wdays   = i_wdays-wkday.
          endif.
        when '2'.
          if i_wdays-hoday > 0.
*           it_time-emp_day = it_time-dayhc / i_wdays-hoday.
            it_time-wdays   = i_wdays-hoday.
          endif.
        when '3'.
          if i_wdays-suday > 0.
*           it_time-emp_day = it_time-dayhc / i_wdays-suday.
            it_time-wdays   = i_wdays-suday.
          endif.
        when '5'.
          if i_wdays-saday > 0.
*           it_time-emp_day = it_time-dayhc / i_wdays-saday.
            it_time-wdays   = i_wdays-saday.
          endif.
      endcase.
    endif.

*    it_time-wdays = i_wdays-wdays.
*    it_time-odays = i_wdays-odays.
    modify it_time index l_tabix.
  endloop.
endform.                    " calc_wdays
*&---------------------------------------------------------------------*
*&      Form  calc_emp_cnt
*&---------------------------------------------------------------------*
form calc_emp_cnt.
  data: l_tabix like sy-tabix.
  data : l_stat2 like pa0000-stat2.

  refresh it_pa0001.
  select pernr endda begda kostl stell persg persk
    appending corresponding fields of table it_pa0001
    from pa0001
    for all entries in i_emp
    where pernr = i_emp-pernr
      and endda >= g_dateto
      and begda <= g_dateto.
  delete adjacent duplicates from it_pa0001.
  sort it_pa0001 by pernr.

* Headcount - valid on end of month(working day)
  sort it_time by pernr lgart kostl.

  loop at i_emp.
    clear l_stat2.
*0	Withdrawn
*1	Inactive
*2	Retiree
*3	Active
    select single stat2 into l_stat2 from pa0000
      where pernr = i_emp-pernr
        and ( begda <= g_dateto and endda >= g_dateto  ).

    read table it_pa0001 with key pernr = i_emp-pernr binary search.
    if l_stat2 = '3'.   "Active
      read table it_time with key pernr = i_emp-pernr
                                  lgart = '1'
                                  kostl = it_pa0001-kostl
                         binary search.
      if sy-subrc = 0.
        l_tabix = sy-tabix.
        it_time-emp_cnt = 1.
        modify it_time index l_tabix.
      endif.
    elseif l_stat2 = '1'.   "Inactive
      read table it_time with key pernr = i_emp-pernr
                                  lgart = 'V'
                                  kostl = it_pa0001-kostl
                         binary search.
      if sy-subrc = 0.
        l_tabix = sy-tabix.
        it_time-emp_cnt = 1.
        modify it_time index l_tabix.
      else.
        concatenate i_emp-pernr ' is inactive, but no time' into gv_msg.
        perform show_warning.
      endif.
    endif.

  endloop.

endform.                    " calc_emp_cnt
*&---------------------------------------------------------------------*
*&      Form  capture_delta_changes
*&---------------------------------------------------------------------*
form capture_delta_changes.
* Select Data
  data:  l_index type i.
  select * from ztco_mha
               into table it_time_db
          where kokrs eq p_kokrs and
                gjahr eq p_gjahr and
                perid in r_perid and
                pernr in s_pernr and
                kostl in pnpkostl and
                chnge =  space.

  select * from ztco_mhat
               into table it_timet_db
          where kokrs eq p_kokrs and
                gjahr eq p_gjahr and
                perid in r_perid and
                chnge =  space.

  select * from ztco_mhad
               into table it_timed_db
         where kokrs eq p_kokrs and
                gjahr eq p_gjahr and
                perid in r_perid and
                kostl in pnpkostl and
                pernr in s_pernr  and
                chnge =  space.

* Check for Delta changes - ztco_mha (only Man hours)
  loop at it_time.
    l_index = sy-tabix.
    clear it_time_db.
    read table it_time_db with key kokrs =    it_time-kokrs
                                   gjahr =    it_time-gjahr
                                   perid =    it_time-perid
                                   pernr =    it_time-pernr
*                                  datum =    it_time-datum
                                   kostl =    it_time-kostl
                                   lgart =    it_time-lgart
                                   dtype =    it_time-dtype
                                   anzsh =    it_time-anzsh
                                   empct =    it_time-empct
                                   jobcd =    it_time-jobcd
                                   mosid =    it_time-mosid
                                   schkz =    it_time-schkz.
*                                  zeity =    it_time-zeity.

    if sy-subrc <> 0.
      it_time-chnge = '1'.
      append it_time to it_time_dt.
    elseif it_time-anzhl <> it_time_db-anzhl.
      it_time_dt = it_time.
      it_time_dt-chnge = '1'.
      it_time_dt-anzhl   =  it_time-anzhl   - it_time_db-anzhl.
      it_time_dt-netmh   =  it_time-netmh   - it_time_db-netmh.
      it_time_dt-emp_fte =  it_time-emp_fte - it_time_db-emp_fte.
      clear it_time_dt-emp_cnt.
      it_time_dt-emp_avg =  it_time-emp_avg - it_time_db-emp_avg.
*     it_time_dt-emp_day =  it_time-emp_day - it_time_db-emp_day.
      it_time_dt-dayhc   =  it_time-dayhc   - it_time_db-dayhc  .
      it_time_dt-wdays   =  it_time-wdays   - it_time_db-wdays.
      it_time_dt-solst   =  it_time-solst.
      append it_time_dt.
    endif.
  endloop.

* Check for Delta changes - ZTCO_MHAT (CC summary)
  loop at it_timet.
    l_index = sy-tabix.
    clear it_timet_db.
    read table it_timet_db with key kokrs =    it_timet-kokrs
                                    gjahr =    it_timet-gjahr
                                    perid =    it_timet-perid
                                    kostl =    it_timet-kostl
                                    lgart =    it_timet-lgart
                                    anzsh =    it_timet-anzsh
                                    empct =    it_timet-empct
                                    jobcd =    it_timet-jobcd.

    if sy-subrc <> 0.
      it_timet-chnge = '1'.
      append it_timet to it_timet_dt.
    elseif it_timet-anzhl <> it_timet_db-anzhl.
      it_timet_dt = it_timet.
      it_timet_dt-chnge = '1'.
      it_timet_dt-anzhl =  it_timet_dt-anzhl - it_timet_db-anzhl.
      it_timet_dt-netmh =  it_timet_dt-netmh - it_timet_db-netmh.
      append it_timet_dt.
    endif.
  endloop.

* Check for Delta changes - ZTCO_MHAD
  loop at it_timed.
    l_index = sy-tabix.
    clear it_timed_db.
    read table it_timed_db with key kokrs =    it_timed-kokrs
                                    gjahr =    it_timed-gjahr
                                    perid =    it_timed-perid
                                    kostl =    it_timed-kostl
                                    dtype =    it_timed-dtype
                                    lgart =    it_timed-lgart
                                    lgart2 =   it_timed-lgart2
                                    anzsh  =   it_timed-anzsh
                                    skostl =   it_timed-skostl
                                    empct  =   it_timed-empct
                                    jobcd  =   it_timed-jobcd
                                    pernr =    it_timed-pernr
*                                   mosid =    it_timed-mosid
                                    schkz =    it_timed-schkz.
*                                   zeity =    it_timed-zeity.
    if sy-subrc <> 0.
      it_timed-chnge = '1'.
      append it_timed to it_timed_dt.
    elseif it_timed-anzhl <> it_timed_db-anzhl.
      it_timed_dt = it_timed.
      it_timed-chnge = '1'.
      it_timed-anzhl =  it_timed-anzhl - it_timed_db-anzhl.
      append it_timed_dt.
    endif.
  endloop.

endform.                    " capture_delta_changes
*&---------------------------------------------------------------------*
*&      Form  add_to_time_sum_detail
*&---------------------------------------------------------------------*
form add_to_time_sum_detail  using f_datum like sy-datum
                                   f_lgart .

*////////////////////////////////////////
* marked by ig.moon
*////////////////////////////////////////


  move-corresponding it_time  to it_timet.
* + by ig.moon {
  it_timet-skostl = it_time-srkostl.
* }

  move-corresponding it_time  to it_timed.

* + by ig.moon {
  it_timed-skostl = it_time-srkostl.
* }

  it_timed-lgart2 = f_lgart.    "py_times-lgart2.

  case f_datum+6(2).
    when '01'. it_timed-anz01 = it_timed-anzhl.
    when '02'. it_timed-anz02 = it_timed-anzhl.
    when '03'. it_timed-anz03 = it_timed-anzhl.
    when '04'. it_timed-anz04 = it_timed-anzhl.
    when '05'. it_timed-anz05 = it_timed-anzhl.
    when '06'. it_timed-anz06 = it_timed-anzhl.
    when '07'. it_timed-anz07 = it_timed-anzhl.
    when '08'. it_timed-anz08 = it_timed-anzhl.
    when '09'. it_timed-anz09 = it_timed-anzhl.
    when '10'. it_timed-anz10 = it_timed-anzhl.
    when '11'. it_timed-anz11 = it_timed-anzhl.
    when '12'. it_timed-anz12 = it_timed-anzhl.
    when '13'. it_timed-anz13 = it_timed-anzhl.
    when '14'. it_timed-anz14 = it_timed-anzhl.
    when '15'. it_timed-anz15 = it_timed-anzhl.
    when '16'. it_timed-anz16 = it_timed-anzhl.
    when '17'. it_timed-anz17 = it_timed-anzhl.
    when '18'. it_timed-anz18 = it_timed-anzhl.
    when '19'. it_timed-anz19 = it_timed-anzhl.
    when '20'. it_timed-anz20 = it_timed-anzhl.
    when '21'. it_timed-anz21 = it_timed-anzhl.
    when '22'. it_timed-anz22 = it_timed-anzhl.
    when '23'. it_timed-anz23 = it_timed-anzhl.
    when '24'. it_timed-anz24 = it_timed-anzhl.
    when '25'. it_timed-anz25 = it_timed-anzhl.
    when '26'. it_timed-anz26 = it_timed-anzhl.
    when '27'. it_timed-anz27 = it_timed-anzhl.
    when '28'. it_timed-anz28 = it_timed-anzhl.
    when '29'. it_timed-anz29 = it_timed-anzhl.
    when '30'. it_timed-anz30 = it_timed-anzhl.
    when '31'. it_timed-anz31 = it_timed-anzhl.
  endcase.

  collect: it_timet, it_timed.

  if it_time-lgart = 'B'.
    collect it_timed into it_timed_b.
  endif.

  clear  : it_timet, it_timed.

endform.                    " add_to_time_sum_detail
*&---------------------------------------------------------------------*
*&      Form  collect_absence_time
*&---------------------------------------------------------------------*
form collect_absence_time.
* Collect to IT_TIME... tables.
* FIXME LATER... what if CC changed...
  data: w_time_info like it_time.
  loop at it_catsdb_item.
    read table it_time into w_time_info
          with key pernr = it_catsdb_item-pernr
                   lgart = '1'.               "from regular working
    if sy-subrc <> 0.
*---- error... FIXME
      concatenate 'ABS HR:' it_catsdb_item3-pernr
                  ' - no time recording' into gv_msg.
      perform show_warning.
    else.
      it_time-lgart = 'B'.
      it_time-anzhl = it_catsdb_item-catshours.
      clear: it_time-emp_fte, it_time-emp_cnt, it_time-emp_avg,
             it_time-dayhc,   "it_time-emp_day,
             it_time-wdays,   it_time-solst.
      collect it_time.

      perform add_to_time_sum_detail using it_catsdb_item-workdate
                                           it_catsdb_item-awart.

    endif.
  endloop.

endform.                    " collect_absence_time
*&---------------------------------------------------------------------*
*&      Form  collect_supporting_time
*&---------------------------------------------------------------------*
form collect_supporting_time.


  data : begin of it_l_calwsig occurs 0,
            kostl     like catsdb-skostl  , "CCtr
            catshours like catsdb-catshours.
  data : end of   it_l_calwsig.

  clear : it_l_calwsig, it_l_calwsig[].
  sort py_times by pernr datum.

  loop at it_catsdb_item3.
* Sender

    read table it_time with key pernr = it_catsdb_item3-pernr
                                lgart = '1'.
    if sy-subrc <> 0.
*FIXME...
      concatenate 'SUP HR:' it_catsdb_item3-pernr
                  ' - no time recording' into gv_msg.
      perform show_warning.
    else.
      read table py_times with key pernr = it_catsdb_item3-pernr
                                   datum = it_catsdb_item3-workdate.
      if sy-subrc <> 0.
        concatenate 'SUP HR:' it_catsdb_item3-pernr ' - no time on '
                    it_catsdb_item3-workdate into gv_msg.
        perform show_warning.
      endif.
      it_time-kostl   = py_times-kostl.
      it_time-srkostl = it_catsdb_item3-rkostl.

      it_time-lgart = '3'.
      it_time-anzhl = it_catsdb_item3-catshours.

      if it_time-kostl(2) = 'MX'.
        perform calc_net_mh using    it_time-anzhl
                            changing it_time-netmh.
      else.
        it_time-netmh = it_time-anzhl.
      endif.

      it_time-anzhl = - it_time-anzhl.
      it_time-netmh = - it_time-netmh.


      clear: it_time-emp_fte, it_time-emp_cnt, it_time-emp_avg,
             it_time-dayhc,   "it_time-emp_day,
             it_time-wdays,   it_time-solst.
      collect it_time.

      perform add_to_time_sum_detail using it_catsdb_item3-workdate
                                           it_catsdb_item3-awart.


* Receiver
      it_time-kostl = it_catsdb_item3-rkostl.
      it_time-srkostl = py_times-kostl.
      it_time-anzhl = - it_time-anzhl.
      it_time-netmh = - it_time-netmh.
      collect it_time.

      perform add_to_time_sum_detail using it_catsdb_item3-workdate
                                           it_catsdb_item3-awart.
    endif.
  endloop.

endform.                    " collect_supporting_time
*&---------------------------------------------------------------------*
*&      Form  calc_net_mh
*&---------------------------------------------------------------------*
form calc_net_mh using    f_anzhl
                 changing f_netmh.

  if f_anzhl <= 4.
    f_netmh = f_anzhl - 15 / 60.  "rest time
  elseif f_anzhl > 4 and f_anzhl <= 8.
    f_netmh = f_anzhl - 25 / 60.
  elseif f_anzhl > 8.
    f_netmh = f_anzhl - 25 / 60
            - ( f_anzhl - 8 ) * 5 / 60.
  endif.

  if f_netmh < 0.
    f_netmh = 0.
  endif.

endform.                    " calc_net_mh
*&---------------------------------------------------------------------*
*&      Form  collect_inactive_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form collect_inactive_time.

*  Select HR Master Record: Inactive data
  perform select_pa000.

  perform make_it_time.

endform.                    " collect_inactive_time
*&---------------------------------------------------------------------*
*&      Form  select_pa000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_pa000.

  data : l_date type datum,
         l_total_date type datum,
         l_work(1),
         l_times type i,
         l_total type i.

*  Get inactive employee
  select pernr endda begda massn massg stat2
    into corresponding fields of table it_pa0000
    from pa0000
    where ( ( begda between g_datefr and g_dateto ) or
            ( endda between g_datefr and g_dateto ) or
            ( begda < g_datefr and endda > g_dateto ) )
      and stat2 = '1'
      and pernr in s_pernr.

* adjust start, end period
  loop at it_pa0000.
    if it_pa0000-begda < g_datefr.
      it_pa0000-begda = g_datefr.
    endif.
    if it_pa0000-endda > g_dateto.
      it_pa0000-endda = g_dateto.
    endif.
    modify it_pa0000.
  endloop.

  l_total       = g_dateto+6(2).
  l_total_date  = g_datefr.
  clear g_total_anzhl.
*  Calcuate how many days in the monrh are work
  do l_total times.
    clear l_work.
    perform get_working_date_or_not using    l_total_date
                                    changing l_work.
* if the date is working day => Add 1 day  * 8HR
    if l_work = 'X'.
      g_total_anzhl  = g_total_anzhl +  8 .
    endif.
    l_total_date = l_total_date + 1.
  enddo.

*  Calcuate how many days(inactive) are work
  sort i_emp by pernr.
  loop at it_pa0000.
    read table i_emp with key pernr = it_pa0000-pernr binary search.
    if sy-subrc <> 0.
      i_emp-pernr = it_pa0000-pernr. append i_emp.
    endif.

    l_times = it_pa0000-endda - it_pa0000-begda + 1.
    l_date  = it_pa0000-begda .
    do l_times times.
      clear l_work.
      perform get_working_date_or_not using    l_date
                                      changing l_work.
* if the date is working day => Add 1 day  * 8HR
      if l_work = 'X'.
        it_inact-pernr  = it_pa0000-pernr.
        it_inact-massn  = it_pa0000-massn.
        it_inact-massg  = it_pa0000-massg.
        it_inact-date   = l_date.
        it_inact-anzhl  = 8.
        it_inact-dayhc  = 1.

        collect it_inact. clear it_inact.
      endif.
      l_date = l_date + 1.
    enddo.
    clear l_date.
  endloop.

  delete adjacent duplicates from i_emp.


  check not it_inact[] is initial.
  select pernr endda begda kostl stell persg persk
    into corresponding fields of table it_pa0001
    from pa0001
    for all entries in it_inact
    where pernr = it_inact-pernr
      and endda >= it_inact-date
      and begda <= it_inact-date.

  delete adjacent duplicates from it_pa0001.

  check not it_pa0001[] is initial.
  select jobcd stell
    into corresponding fields of table it_jc
    from ztco_jc
     for all entries in it_pa0001
    where stell = it_pa0001-stell .



endform.                    " select_pa000
*&---------------------------------------------------------------------*
*&      Form  get_working_Date_or_not
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      <--P_L_WORK  text
*----------------------------------------------------------------------*
form get_working_date_or_not using    p_date
                             changing p_work.

  data : l_work like scal-indicator.


  call function 'DATE_CONVERT_TO_FACTORYDATE'
       exporting
            date                 = p_date
            factory_calendar_id  = 'HM'
       importing
            workingday_indicator = l_work.
  if l_work = ''.
    p_work = 'X'.
  endif.

endform.                    " get_working_Date_or_not
*&---------------------------------------------------------------------*
*&      Form  make_it_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_it_time.
  data : l_lgart2 like ztco_mhad-lgart2.
  data : l_emp_avg like it_time-anzhl.

  sort it_pa0001 by pernr begda.

  clear it_time.
  loop at it_inact.
*   Pernr's Cost center, job code, emp_cat...
    clear it_pa0001 .
    loop at it_pa0001 where pernr = it_inact-pernr
                        and endda >= it_inact-date
                        and begda <= it_inact-date.
      exit.
    endloop.
    if sy-subrc = 0 .
      it_time-kokrs = p_kokrs.
      it_time-gjahr = p_gjahr.
      it_time-perid = p_frper.
      it_time-anzhl = it_inact-anzhl.
      it_time-dayhc = it_inact-dayhc.
      it_time-lgart = 'V'.
      it_time-pernr = it_inact-pernr.
      it_time-kostl = it_pa0001-kostl.
      clear it_jc.
      read table it_jc with key stell = it_pa0001-stell.
      it_time-jobcd = it_jc-jobcd.
*   Employee category
      perform get_emp_categ(zacop01) using it_pa0001-persg
                                           it_pa0001-persk
                                     changing it_time-empct.
      collect it_time.

      clear l_lgart2.
      concatenate it_inact-massn it_inact-massg into l_lgart2.
*   Detail data(ZTCO_MHAD)
      perform add_to_time_sum_detail using it_inact-date
                                           l_lgart2.
    endif.
  endloop.

endform.                    " make_it_time
*&---------------------------------------------------------------------*
*&      Form  read_pa0000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_pa0000.
*  Get inactive employee
  select pernr endda begda massn massg stat2
    into corresponding fields of table it_pa0000
    from pa0000
    where ( ( begda between g_datefr and g_dateto ) or
            ( endda between g_datefr and g_dateto ) )
      and stat2 = '1'
      and pernr in s_pernr.

endform.                    " read_pa0000
*&---------------------------------------------------------------------*
*&      Form  show_warning
*&---------------------------------------------------------------------*
form show_warning.
  call function 'FI_PROGRESS_INDICATOR'
    exporting
*      percentage          = p_%
      text                = gv_msg
*     MESSAGECLASS        = ' '
*     MESSAGENUMBER       = ' '
*     MESSAGEPAR1         = ' '
*     MESSAGEPAR2         = ' '
*     MESSAGEPAR3         = ' '
*     MESSAGEPAR4         = ' '
      .
endform.                    " show_warning










* back up by ig.moon *

**    loop at it_timet.
**      select single * from ztco_mhat
**      where kokrs eq it_timet-kokrs
**        and gjahr eq it_timet-gjahr
**        and perid eq it_timet-perid
**        and dtype eq it_timet-dtype
**        and kostl eq it_timet-kostl
**        and anzsh eq it_timet-anzsh
**        and lgart eq it_timet-lgart
**        and jobcd eq it_timet-jobcd
**        and empct eq it_timet-empct
**        and chnge eq it_timet-chnge.
**      if sy-subrc eq 0.
**
*** { ????????????????????????????????????
**        add : it_timet-anzhl to ztco_mhat-anzhl,
**              it_timet-netmh to ztco_mhat-netmh,
**              it_timet-daymh to ztco_mhat-daymh,
**              it_timet-satmh to ztco_mhat-satmh,
**              it_timet-sunmh to ztco_mhat-sunmh,
**              it_timet-holmh to ztco_mhat-holmh.
**
**        update ztco_mhat.
**
***        ztco_mhat-SKOSTL
**
*** }
**
***        break-point.
**
**      else.
**
**        ztco_mhat = it_timet.
**        insert ztco_mhat.
**
**      endif.
**    endloop.
