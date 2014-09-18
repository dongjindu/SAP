report zhr_401k_empr_contri_report2 line-size 250 .
* TCODE - ZER401K2
**********************************************************************
*Date        Developer    Request        Description
* 10/10/2007 IG.Moon     UD1K941341      Copy from
*                  zhr_401k_empr_contri_report
*                  and change logic to solve some urgent issues.
* 06/21/2011 Valerian    UD1K952165      Change the logic of benefit
*                                        calculation
* 10/18/2011 KDM                         REFRESH lt_rgdir. "" ADD KDM01
*                                        --> To solve a bad performance
***********************************************************************

tables : p0001,
         pa0001,
         pa0002,
         pa0000,
         p0000,
         pa0041,
         pa0169,
         pc261,
         t511p,sscrfields.

type-pools: slis.
data: in_rgdir like pc261 occurs 0 with header line,
      wa_rt like pc207 occurs 0 with header line,
      seqnr like pc261-seqnr,
      ws_401k type   pc207-betrg,
      ws_gsal type   pc207-betrg,
      ws_8387 type   pc207-betrg,
      ws_8337 type  pc207-betrg,
      ws_bc31 type  pc207-betrg,
      l_limit  type pc207-betrg,
      l_ee% type pa0169-eepct,
      ws_pernr type p0000-pernr,
      result type pay99_result.

* by ig.moon 5/5/2010 {
data  ws_8347 type  pc207-betrg.
* }

data: w_permo  like t549a-permo,   " Period parameters
      w_abkrt  like t549t-atext.   " Payroll Area Text

data : begin of it_output occurs 0,
        pernr like pa0001-pernr,
        perid like pa0002-perid,
        401e_date like p0041-begda,
        401er_date like p0041-begda,
        hire_date like p0041-begda,
        t_date   like p0000-begda,                          "UD1K940520
        fpper like pc261-fpper,
        fpbeg like pc261-fpbeg,
        fpend like pc261-fpend,
        gross_sal like pc207-betrg,
        401k_amt like pc207-betrg,
        ee% like pa0169-eepct,
        ee_401k like pc207-betrg,
        ee_401cu like pc207-betrg,
        er_paid like pc207-betrg,
        diff like pc207-betrg,
        diff1 like pc207-betrg,
* UD1K941143 - by IG.MOON 7/30/2007
* {
        er_paid_a like pc207-betrg, "used when user select prev.option
                                    " to keep the actual data
* }
        linecolor(4),
       end of it_output.

data : begin of wa_final occurs 0,
        pernr like pa0001-pernr,
        perid like pa0002-perid,
        gross_sal like pc207-betrg,
        401k_amt like pc207-betrg,
        ee_401k like pc207-betrg,
        ee_401cu like pc207-betrg,
        er_paid like pc207-betrg,
        diff like pc207-betrg,
        diff1 like pc207-betrg,
* UD1K941143 - by IG.MOON 7/30/2007
* {
        er_paid_a like pc207-betrg, "used when user select prev.option
                                    " to keep the actual data
* }
        linecolor(4),
        flag(1) type c,
       end of wa_final.

data : begin of it_final occurs 0,
        pernr like pa0001-pernr,
        perid like pa0002-perid,
        gross_sal like pc207-betrg,
        401k_amt like pc207-betrg,
        ee_401k like pc207-betrg,
        ee_401cu like pc207-betrg,
        er_paid like pc207-betrg,
        diff like pc207-betrg,
        diff1 like pc207-betrg,
* UD1K941143 - by IG.MOON 7/30/2007
* {
        er_paid_a like pc207-betrg, "used when user select prev.option
                                    " to keep the actual data
* }
        linecolor(4),
       end of it_final.

data : begin of it_sallimit occurs 0,
        pernr like pa0001-pernr,
        fpper like pc261-fpper,
        401k_amt like pc207-betrg,
        diff1 like pc207-betrg,
       end of it_sallimit.

data : begin of it_diff occurs 0,
 pernr like pa0001-pernr,
 diff1 like pc207-betrg,
end of it_diff.


data : begin of it_401ksum occurs 0,
        fpper like pc261-fpper,
        diff1 like pc207-betrg,
       end of it_401ksum.

data : begin of it_401ktemp occurs 0,
        fpper like pc261-fpper,
        diff1 like pc207-betrg,
       end of it_401ktemp.

data : ws_rgdir like line of in_rgdir,
       l_relid like  pcl2-relid,
       l_pernr like pc200-pernr,
       ll_pernr like p0000-pernr,
       l_code like p0041-dar01,
       l_code1 like p0041-dar01,
       t_date like p0041-dat01,
       401e_date like  p0041-dat01,
       401l_date like  p0041-dat01,
       hire_date like  p0041-dat01,
       l_edate  like pa0169-begda,
       l_edt1  like pa0169-begda,
       l_edate1(10) type c,
       l_edate2(10) type c
       .

* UD1K941341 - by IG.MOON 8/15/2007
* {
define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.

types: begin of ts_inper_directory_entry.
types:   ipend like pc261-ipend,
         iperm like pc261-iperm,
         inper like pc261-inper,
         inpty like pc261-inpty,
         inpid like pc261-inpid,
       end of ts_inper_directory_entry.
types: tt_inper_directory type ts_inper_directory_entry occurs 0.

data pt_output like it_output occurs 0 with header line.
data pt_diff like it_diff occurs 0 with header line.

ranges pnpabkrs for pc261-iabkrs.

constants:  false value ' ',
            true  value 'X'.

types: begin of tinper,
         iabkrs like pc261-iabkrs,
         iabkrs_txt like t549t-atext,
         iperm like pc261-iperm,
         iperm_txt like t549n-atext,
         inper like pc261-inper,
         ipend like pc261-ipend,
         inpty like pc261-inpty,
         inpid like pc261-inpid,
       end of tinper.

data: inper type tinper occurs 0 with header line.

types: begin of tfpper,
         abkrs like pc261-abkrs,
         abkrs_txt like t549t-atext,
         permo like pc261-permo,
         permo_txt like t549n-atext,
         fpper like pc261-fpper,
         paydt like pc261-paydt,
         payty like pc261-payty,
         payid like pc261-payid,
       end of tfpper.

data: fpper type tfpper occurs 0 with header line.

constants c_null_date(8) type c value '00000000'.
constants:
  begin of cd_c,                       "constants
*   void_true like pc261-void     value 'X',     "void    "XTWP40K001106
    reversal_true  type void      value 'R',   "reversal
    reversal_false type void      value ' ',   "valid
    void_true      type void      value 'V',   "void
    void_false     type void      value ' ',   "valid
    actual         type srtza     value 'A',   "actual result
    previous       type srtza     value 'P',   "previous result
    old            type srtza     value 'O',   "old result
    regular        type payty     value ' ',   "regular result
    bonus          type payty     value 'A',   "bonus   result
    correct        type payty     value 'B',   "correction result
    non_auth       type payty     value 'C',   "Non author. manual
  end of cd_c.

* }


data : g_exit_caused_by_caller  type c,
       gs_exit_caused_by_user   type slis_exit_by_user,
       c_red(4)   value 'C610',
       c_green(4) value 'C510',
       c_color(4) value 'C710'.


data : begin of it_tab occurs 0,
        pernr like p0000-pernr,
        begda like p0000-begda,     " UD1K940413
        perid like pa0002-perid,
       end of it_tab.

data : begin of it_no401k occurs 0,
        pernr like p0000-pernr,
       end of it_no401k.

data: gt_fieldcat  type slis_t_fieldcat_alv with header line,
      gt_fieldcat1 type slis_t_fieldcat_alv with header line,
      gt_fieldcat2 type slis_t_fieldcat_alv with header line,
      gt_fc       type slis_t_fieldcat_alv,
      gt_fc1       type slis_t_fieldcat_alv,
      gt_fc2       type slis_t_fieldcat_alv,
      g_fieldcat_s like line of gt_fieldcat,
      gs_layout   type slis_layout_alv,
      gs_print    type slis_print_alv,
      gt_sort     type slis_t_sortinfo_alv with header line,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event with header line,
      gt_header   type slis_t_listheader,
      g_repid     like sy-repid,
      gt_colinfo_table type slis_t_specialcol_alv. "line color.
data  g_error.

* Select_options
selection-screen begin of block b1 with frame title text-001.
select-options : s_pernr for p0000-pernr,
                 s_werks for pa0001-werks,
                 s_btrtl for pa0001-btrtl,
                 s_persg for pa0001-persg,
                 s_persk for pa0001-persk,
                 s_stat2 for pa0000-stat2.
selection-screen end of block b1  .
selection-screen begin of block b4 with frame title text-004.
parameters :   p_abkrs like  pa0001-abkrs obligatory default '11'.
select-options : s_begda   for p0001-begda,
                 s_fpper for    pc261-fpper.
selection-screen end of block b4.

parameters: p_abrpr  like t549q-pabrp modif id p1 no-display,
            p_abrjr  like t549q-pabrj modif id p1 no-display.


selection-screen begin of block b2 with frame title text-002.
parameters :
*P_ER type check  default 'X',
             p_r1 radiobutton group g1
             user-command ucom default 'X' ,"Show ALL
             p_r8 radiobutton group g1, "Show only 401K enrolled
             p_r9 radiobutton group g1, "All records with difference
             p_r7 radiobutton group g1,   " Show only difference
             p_r2 radiobutton group g1,   " Over Payment
             p_r3 radiobutton group g1,   " Under Payment
             p_r6 radiobutton group g1.   "Summary

* UD1K941129 by IG.MOON - data is came from actual or previous
* {
parameters p_two as checkbox modif id stw default 'X'.

**selection-screen begin of block b2_s with frame title text-007.
**parameters :
**     p_act radiobutton group g1s    user-command ucom
**     default 'X' , " Actual
**     p_prv radiobutton group g1s.   "Previous
**selection-screen end of block b2_s  .
SELECTION-SCREEN BEGIN OF BLOCK b2_s WITH FRAME TITLE TEXT-007.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_act RADIOBUTTON GROUP g1s DEFAULT 'X'. "Actual
SELECTION-SCREEN   COMMENT    3(27) text-h01.
PARAMETERS: p_prv RADIOBUTTON GROUP g1s.     "Previous
SELECTION-SCREEN   COMMENT    33(15) text-h02.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN  END   OF BLOCK b2_s.

* }

selection-screen end of block b2  .

selection-screen begin of block b3 with frame title text-003.
**parameters : p_r4 radiobutton group g2,
**            p_r5 radiobutton group g2 default 'X'.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_r4 RADIOBUTTON GROUP g2 . "
SELECTION-SCREEN   COMMENT    3(29) text-h03.
PARAMETERS: p_r5 RADIOBUTTON GROUP g2 DEFAULT 'X'.     "
SELECTION-SCREEN   COMMENT    35(15) text-h04.
SELECTION-SCREEN END OF LINE.

selection-screen end of block b3  .



initialization.
* Get Current Payroll Period
  perform get_payroll_period using p_abkrs
                          changing w_permo " s_begda-low s_begda-high
                                   w_abkrt p_abrpr p_abrjr.
  concatenate  p_abrjr p_abrpr into s_fpper-low.
  append s_fpper.
*  append s_begda.

at selection-screen .
  case sscrfields-ucomm.
    when 'UCOM'.
      perform modify_screen.
  endcase.

at selection-screen output.
  perform modify_screen.

start-of-selection.

* Select All Employee's who are active.
  perform select_valid_employees.

  check g_error eq space.
  call function 'REUSE_ALV_BLOCK_LIST_INIT'
       EXPORTING
            i_callback_program = 'ZHR_401K_EMPR_CONTRI_REPORT'.


* UD1K941341 - by IG.MOON {
* Select Payroll Cluster Data

*  perform read_payroll_data.
  if p_act eq 'X'.
    perform read_payroll_data.
  else.
    perform read_payroll_data_prv.
  endif.
* }

* Output Options / Formatting.
  perform output_options.

end-of-selection.
  check g_error eq space.

* Display Output
  perform display_output.


*&---------------------------------------------------------------------*
*&      Form  read_payroll_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_payroll_data.
  data : flag(1) type c,
         prev_fpper type  pc261-fpper,
         lv_molga type molga,
         lw_month  like  t5a4a-dlydy,
         $l_edate like l_edate.

  loop at it_tab.
    ll_pernr = it_tab-pernr .
* Read Termination date
    clear: l_code, t_date,401e_date, l_edate,
           hire_date.
* Display Termination date for all employee if exists
*    if s_STAT2-low eq '0'.             " UD1K940413
*      perform read_termination_date.
*    endif.


* Read 401K Eligibilty Date/Hire/Termination Date      "UD1K940451
    perform read_eligibilty_date.

* UD1K940484
* Begin of changes -* UD1K940484
* If 401K Eligibilty Date is maintained for an employee
* then reports show incorrect value. So show Exceptions
* report for the employee who are missing 401K eligibilty
* date ( Z9) and Skip those employee when RUn online
    if 401e_date is initial.
      it_no401k-pernr = it_tab-pernr.
      append it_no401k. clear it_no401k.
      continue.
    endif.

* End of changes - UD1K940484

* Add 12 Months to 401K Eligibity Date
* to determine date from which employee will
* receive 401K Employer contribution - UD1K940451

    call function 'HR_PSD_DATES_ADD_MONTHS'
      exporting
        v_date            = 401e_date
       v_months           = 12
     importing
       e_date             = l_edate
* EXCEPTIONS
*   NOT_POSITIVE       = 1
*   OTHERS             = 2

*lw_month = '12'.
*    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*      EXPORTING
*        DATE            =  401E_DATE
*        DAYS            = '00'
*        MONTHS          =  lw_month
*       SIGNUM          = '+'
*        YEARS           = '00'
*     IMPORTING
*       CALC_DATE       =  L_EDATE
                  .

    .

* Read Payroll Control Records
    clear lv_molga.
    call function 'CU_READ_RGDIR'
         EXPORTING
              persnr          = ll_pernr
         IMPORTING
              molga           = lv_molga
         TABLES
              in_rgdir        = in_rgdir
         EXCEPTIONS
              no_record_found = 1
              others          = 2.

    ws_pernr = ll_pernr.

* Delete payroll control records based on selection input
    if not s_begda[] is initial.
      delete in_rgdir where not paydt in s_begda. "System Year
    endif.
* Delete payroll control records based on selection input
    if  not  s_fpper[]  is initial.
      delete in_rgdir where not fpper in s_fpper. "Payroll Period
    endif.
* Delete payroll control records where payroll period is 000000
    delete in_rgdir where fpper eq '000000'.

** Delete voided payroll data.
    delete in_rgdir where voidr ne space.

    delete in_rgdir where srtza ne 'A'. "Active

* Cluster id for US
* Personnel Country Grouping
    clear l_relid.

    select single relid into l_relid
                  from t500l
                  where molga = lv_molga.
    if   l_relid is initial.
      l_relid = 'RU'.
    endif.

    loop at in_rgdir into ws_rgdir.
      seqnr = ws_rgdir-seqnr.
      l_pernr = ws_pernr.

* Read Payroll cluster Data for each payroll control record
      call function 'PYXX_READ_PAYROLL_RESULT'
           EXPORTING
                clusterid                    = l_relid
                employeenumber               = l_pernr
                sequencenumber               = seqnr
                read_only_international      = 'X'
           CHANGING
                payroll_result               = result
           EXCEPTIONS
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
      clear : ws_8387, ws_401k,ws_8337, ws_bc31, ws_gsal .

* by ig.moon 5/5/2010 {
      clear : ws_8347.
* }

      loop at result-inter-rt into wa_rt
      where ( lgart = '/102' or lgart = '8387'
              or  lgart = '8337' or lgart = 'BC31'
* By Furong On 01/31/14 (
              or  lgart = '8347'
* )
              or  lgart = '/101' ).

        if  wa_rt-lgart = '/101'.     "Gross Salary
          ws_gsal = wa_rt-betrg.
        endif.

        if  wa_rt-lgart = '/102'.                           "401K wages
          ws_401k = wa_rt-betrg.
        endif.
        if  wa_rt-lgart = '8387'.   " Employer paid
          ws_8387 = ws_8387 + wa_rt-betrg.
        endif.
        if wa_rt-lgart =  '8337'.   " Employee paid
          ws_8337 = ws_8337 + wa_rt-betrg.
        endif.
        if  wa_rt-lgart =  'BC31'.  "401C contribution
          ws_bc31 = ws_bc31 + wa_rt-betrg.
        endif.

* by ig.moon 5/5/2010 {
        if  wa_rt-lgart =  '8347'.  "401C was changed to 8347 12/14/2009
          ws_8347 = ws_8347 + wa_rt-betrg.
        endif.
* }
      endloop.

* by ig.moon 5/5/2010 {
      if  ws_8347 <> 0.
        ws_bc31 = ws_8347.
      endif.
* }

*      if  ws_8387 > 0.       " Employee contribution > 0
      clear : pa0169, l_ee%.
      if  ws_8337 <> 0 .      " Employer Contribution

* Read Employee contribution

*        call function 'ZHR_READ_PA0169'
*             exporting
*                  pernr = it_tab-pernr
*                  subty = '401K'
*                  begda = ws_rgdir-fpbeg
*                  endda = ws_rgdir-fpend
*             importing
*                  eepct = it_output-ee%.
*
        perform get_pa0169 using it_tab-pernr
                                 '401K'
                                 ws_rgdir-fpbeg
                                 ws_rgdir-fpend
                         changing it_output-ee%.

      else.
*        call function 'ZHR_READ_PA0169'
*             exporting
*                  pernr = it_tab-pernr
*                  subty = '401K'
*                  begda = ws_rgdir-fpbeg
*                  endda = ws_rgdir-fpend
*             importing
*                  eepct = l_ee%.

        perform get_pa0169 using it_tab-pernr
                                 '401K'
                                 ws_rgdir-fpbeg
                                 ws_rgdir-fpend
                         changing l_ee%.

        clear it_output-ee%.
      endif.

* it_output-diff = it_output-401K_AMT * ( 60 / 100 ) * ( 3
*/ 100 ).

* Populate output internal Table.
      it_output-pernr = it_tab-pernr.
      it_output-perid = it_tab-perid.
      it_output-fpper = ws_rgdir-fpper.   "Pay period
      it_output-gross_sal  =    ws_gsal.   " Gross Salary
      it_output-401k_amt = ws_401k.                         "401K Wages
      it_output-er_paid  = ws_8387.       "ER paid
      it_output-ee_401k  = ws_8337 * -1.  "EE paid UD1K940463
      it_output-ee_401cu = ws_bc31 * -1.  "401C contriUD1K940463
      it_output-fpbeg = ws_rgdir-fpbeg.   "Payroll begin date
      it_output-fpend = ws_rgdir-fpend.   "payroll end date

* Employee Contribution is < 3% then Apply that percentage

      if it_output-ee% is initial.
        message s000(zmco) with '#' it_tab-pernr
        ': has 0% rate in period of ' ws_rgdir-fpper .
      endif.

* BEGIN OF UD1K952165
      if ws_rgdir-fpbeg < '20061218'.
        if it_output-ee% < 3.
          it_output-diff = it_output-401k_amt * ( 60 / 100 ) *
                           ( it_output-ee% / 100 ).
        else.
          it_output-diff = it_output-401k_amt * ( 60 / 100 ) *
                           ( 3 / 100 ).
        endif.


      elseif ws_rgdir-fpbeg >= '20061218' and
             ws_rgdir-fpbeg  < '20100419'.

        if it_output-ee% < 4.
          it_output-diff = it_output-401k_amt * ( 60 / 100 ) *
                           ( it_output-ee% / 100 ).
        else.
          it_output-diff = it_output-401k_amt * ( 60 / 100 ) *
                           ( 4 / 100 ).
        endif.


      elseif ws_rgdir-fpbeg >= '20100419' and
             ws_rgdir-fpbeg  < '20110321'.

        if it_output-ee% < 4.
          it_output-diff = it_output-401k_amt * ( 80 / 100 ) *
                           ( it_output-ee% / 100 ).
        else.
          it_output-diff = it_output-401k_amt * ( 80 / 100 ) *
                           ( 4 / 100 ).
        endif.


      else.
        if it_output-ee% < 4.
          it_output-diff = it_output-401k_amt * ( 100 / 100 ) *
                           ( it_output-ee% / 100 ).
        else.
          it_output-diff = it_output-401k_amt * ( 100 / 100 ) *
                           ( 4 / 100 ).
        endif.
      endif.

*      if  it_output-ee% < 3.
*
*        if ws_rgdir-fpbeg < '20100419'.
*          it_output-diff =
*           it_output-401k_amt *  ( 60 / 100 ) * ( it_output-ee% / 100 )
*.
*        else.
*          it_output-diff =
*          it_output-401k_amt *  ( 80 / 100 ) * ( it_output-ee% / 100 ).
*        endif.
*
*      elseif ws_rgdir-fpbeg <= '20061217' .   " 2004 - 2006 Pay period
*       it_output-diff = it_output-401k_amt * ( 60 / 100 ) * ( 3 / 100 )
*.
*      elseif  it_output-ee% > 4.          "2007 ER % is 4
*        if ws_rgdir-fpbeg < '20100419'.
*       it_output-diff = it_output-401k_amt * ( 60 / 100 ) * ( 4 / 100 )
*.
*        else.
*       it_output-diff = it_output-401k_amt * ( 80 / 100 ) * ( 4 / 100 )
*.
*        endif.
*      else.
*        if ws_rgdir-fpbeg < '20100419'.
*          it_output-diff = it_output-401k_amt * ( 60 / 100 ) * (
*   it_output-ee% / 100 ).
*        else.
*          it_output-diff = it_output-401k_amt * ( 80 / 100 ) * (
*   it_output-ee% / 100 ).
*        endif.
*      endif.
* END OF UD1K952165

* Actual 401K Employee contribution starts from next payperiod
* after employee qualifies.( In other words, Technically even if
*employee is eligbile for 401K employer contribution in Payroll period
* 12. System calculates 401K employer contribution from payperiod 13.)

* Show difference value only  after Employee 401K eligibility period
* starts ( which 1 Year after joinning date or Z9 value from infotype
* 41)

* UD1K941172 - by IG.MOON 7/31/2007 {
*     if   it_output-fpbeg > l_edate .                      "UD1K940451
      $l_edate  = l_edate + 1.
      if   it_output-fpbeg > l_edate
          or ( $l_edate between it_output-fpbeg and it_output-fpend ) .
* }
        it_output-diff1 = it_output-er_paid - it_output-diff.

* BEGIN OF UD1K952165
        if it_output-diff1 > '0.01' or it_output-diff1 < '-0.01'.
          it_output-linecolor = c_red.
        endif.
* END OF UD1K952165

* if there is no Employee contribution then don't
* Show Difference PER JT ( Exceptions)
*        if it_output-EE_401K eq 0.                          "UD1K940451
* per JT show difference even if there is no employer contribution
*         clear   it_output-diff1.                "UD1K940502
*        endif.
*           if it_output-EE_401K eq 0 and l_ee% ne it_output-EE%.
*             it_output-EE% = l_ee%.
*           endif.
      else.
        clear it_output-diff.
      endif.



* Check for Manual adjustment / Pro rate calculation when employee
*reaches 401K limit - hightlight such records in Red color
      clear l_limit.
      l_limit  = ( it_output-401k_amt * it_output-ee% ) / 100.
      l_limit = l_limit .
*        if   l_limit <>  it_output-EE_401K and flag eq 'X'.
      if l_limit <>  it_output-ee_401k.
        move : c_red   to it_output-linecolor.
      endif.


* Collect data  to display only records which have differnec
*      it_sallimit-pernr = it_tab-pernr.
*      it_sallimit-FPPER = ws_rgdir-FPPER+0(4).
*      it_sallimit-diff1 = it_output-diff1.
*      collect it_sallimit.
*      if s_STAT2-low eq '0'.             " UD1K940413
      it_output-t_date = t_date . " UD1K940413
*      endif .                            " UD1K940413
      it_output-401e_date = 401e_date.
      it_output-hire_date = hire_date.   "Hire Date/UD1K940516
      it_output-401er_date =   l_edate + 1 .
      if  it_output-401er_date < '20040501'.
        it_output-401er_date = '20040501'.
      endif.

* For Employee who do have 169 record
      if it_output-ee% eq  0 and it_output-fpbeg > '20040501'.
        move : c_color  to it_output-linecolor.
      endif.

*if a  employees has  one difference then show all records for that
*employee
      if  p_r9 eq 'X' and it_output-diff1 <> 0.
        it_diff-pernr =  it_output-pernr.
        it_diff-diff1 =  it_output-diff1.
        collect it_diff.
      endif.

      append  it_output.
      clear it_output.
*      endif.
*      endif.
    endloop.


* retain only records which have difference
*    loop at it_sallimit.
*      if   it_sallimit-diff1 eq 0.
*        delete it_output  where  pernr =  it_sallimit-pernr and
*                                fpper+0(4) = it_sallimit-FPPER.
*      endif.
*
*    endloop.
*    refresh it_sallimit. clear it_sallimit.
  endloop.

endform.                    " read_payroll_data
*&---------------------------------------------------------------------*
*&      Form  select_valid_employees
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_valid_employees.

  clear g_error.

* 401K Plan enrolled Option is choosen
  if p_r8   eq 'X' .

    select  a~pernr
            b~begda                                         "UD1K940413
            c~perid
             into table it_tab from ( pa0001 as a inner join
                                           pa0000 as b on
                                         a~pernr = b~pernr
                                         inner join pa0002 as c
                                         on a~pernr = c~pernr )
             where  a~pernr in s_pernr and
                    a~begda <= sy-datum and
                    a~endda >= sy-datum and
                    a~abkrs eq  p_abkrs and
                    a~werks in  s_werks and
                    a~btrtl in  s_btrtl  and
                    a~persg in  s_persg  and
                    a~persk in  s_persk and
                    b~begda <= sy-datum and
                    b~endda >= sy-datum and
                    b~stat2 in s_stat2  and
                    a~pernr in ( select pernr from pa0169
                               where pa0169~pernr = a~pernr ) and
                    c~begda <= sy-datum and
                    c~endda >= sy-datum .
    .
* For All others
  else.
    select  a~pernr
            b~begda                                         "UD1K940413
            c~perid
             into table it_tab from ( pa0001 as a inner join
                                           pa0000 as b on
                                         a~pernr = b~pernr
                                         inner join pa0002 as c
                                        on a~pernr = c~pernr )
             where  a~pernr in s_pernr and
                    a~begda <= sy-datum and
                    a~endda >= sy-datum and
                    a~abkrs eq  p_abkrs and
                    a~werks in  s_werks and
                    a~btrtl in  s_btrtl  and
                    a~persg in  s_persg  and
                    a~persk in  s_persk and
                    b~begda <= sy-datum and
                    b~endda >= sy-datum and
                    b~stat2 in s_stat2  and
                    c~begda <= sy-datum and
                    c~endda >= sy-datum .
    .
  endif.

endform.                    " select_valid_employees
*&---------------------------------------------------------------------*
*&      Form  display_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_output.

  g_repid = sy-repid.

  if p_r6 eq 'X'.             " Summary Option is choosen
    perform build_fieldcat  using  'IT_FINAL'.
    perform build_layout  using 'X' 'X' space.
    perform build_comment     using  gt_header[].
    perform display_grid.
  else.
    perform build_fieldcat  using  'IT_OUTPUT'.
    perform build_layout  using 'X' 'X' space.
    perform build_comment     using  gt_header[].
    perform display_grid.
  endif.


* Works only in the Background
* Display Employee with NO 401K Elibility Date
* Exception Report for
* Employee without 401K Eligibilty Date and Fiscal year
* wise break up of differences
*  if not it_no401K[] is initial and sy-BATCH eq 'X'.
  if  sy-batch eq 'X'.
    perform display_exception_report.
  endif.
endform.                    " display_output
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0420   text
*----------------------------------------------------------------------*
form build_fieldcat  using p_intab type slis_tabname.

  data: gs_fieldcat like line of gt_fieldcat.
  clear   : gt_fieldcat, gt_fc.
  refresh : gt_fieldcat, gt_fc.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name         = g_repid
            i_internal_tabname     = p_intab
            i_inclname             = g_repid
       CHANGING
            ct_fieldcat            = gt_fc
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            others                 = 3.

  gt_fieldcat[] = gt_fc[].

  if p_r6 eq 'X' .
* Summary Option is choosen
    loop at gt_fieldcat.
      case gt_fieldcat-fieldname.
        when 'PERNR'.
          gt_fieldcat-seltext_l = 'Team Member'.
          gt_fieldcat-seltext_m = 'Team Member'.
          gt_fieldcat-seltext_s = 'Team Member'.
          gt_fieldcat-key = 'X' .
          gt_fieldcat-key_sel = 'X'.

        when 'PERID'.
          gt_fieldcat-seltext_l = 'SSN'.
          gt_fieldcat-seltext_m = 'SSN'.
          gt_fieldcat-seltext_s = 'SSN'.
          gt_fieldcat-reptext_ddic = 'SSN'.
          gt_fieldcat-key = 'X' .
          gt_fieldcat-key_sel = 'X'.

        when 'GROSS_SAL'.
          gt_fieldcat-seltext_l = 'Gross Salary'.
          gt_fieldcat-seltext_m = 'Gross Salary'.
          gt_fieldcat-seltext_s = 'Gross Salary'.
          gt_fieldcat-do_sum  = 'X'.

        when '401K_AMT'.
          gt_fieldcat-seltext_l = '401K Wages'.
          gt_fieldcat-seltext_m = '401K Wages'.
          gt_fieldcat-seltext_s = '401K Wages'.
          gt_fieldcat-do_sum  = 'X'.
        when 'ER_PAID'.
          gt_fieldcat-seltext_l = 'EMPR. Paid'.
          gt_fieldcat-seltext_m = 'EMPR. Paid'.
          gt_fieldcat-seltext_s = 'EMPR. Paid'.
          gt_fieldcat-reptext_ddic = 'ER.Paid'.
          gt_fieldcat-do_sum  = 'X'.

        when 'DIFF'.
          gt_fieldcat-seltext_l = 'Corr. Amt.'.
          gt_fieldcat-seltext_m = 'Corr. Amt.'.
          gt_fieldcat-seltext_s = 'Corr. Amt.'.
          gt_fieldcat-reptext_ddic = 'Corr. Amt.'.
          gt_fieldcat-do_sum  = 'X'.
        when 'DIFF1'.
          gt_fieldcat-seltext_l = 'Difference'.
          gt_fieldcat-seltext_m = 'Difference'.
          gt_fieldcat-seltext_s = 'Difference'.
          gt_fieldcat-reptext_ddic = 'Difference'.
          gt_fieldcat-do_sum  = 'X'.

        when 'EE_401K' .
          gt_fieldcat-seltext_l = 'EE 401K'.
          gt_fieldcat-seltext_m = 'EE 401K'.
          gt_fieldcat-seltext_s = 'EE 401K'.
          gt_fieldcat-reptext_ddic = 'EE 401K'.
          gt_fieldcat-do_sum  = 'X'.

        when 'EE_401CU'.
          gt_fieldcat-seltext_l = 'EE 401CU'.
          gt_fieldcat-seltext_m = 'EE 401CU'.
          gt_fieldcat-seltext_s = 'EE 401CU'.
          gt_fieldcat-reptext_ddic = 'EE 401CU'.
          gt_fieldcat-do_sum  = 'X'.


* UD1K941143
* {
        when  'ER_PAID_A'.
*          if p_act eq 'X'.
          gt_fieldcat-no_out = 'X'.
*          else.
*            gt_fieldcat-seltext_l = '(A)EMPR.Paid'.
*            gt_fieldcat-seltext_m = '(A)EMPR.Paid'.
*            gt_fieldcat-seltext_s = '(A)EMPR.Paid'.
*            gt_fieldcat-reptext_ddic = '(A)ER.Paid'.
*            gt_fieldcat-do_sum  = 'X'.
*          endif.
* }

*        when 'BEGDA'.
*          if s_STAT2-low eq '0'.
*            gt_fieldcat-seltext_l = 'Last Day'.
*            gt_fieldcat-seltext_M = 'Last Day'.
*            gt_fieldcat-seltext_S = 'Last Day'.
*            gt_fieldcat-REPTEXT_DDIC = 'Last Day'.
*            gt_fieldcat-do_sum  = 'X'.
*          else.
*          delete  gt_fieldcat  where fieldname eq gt_fieldcat-fieldname
*.
*          endif.
      endcase.
      modify gt_fieldcat transporting no_out
      seltext_l seltext_m seltext_s
      reptext_ddic key key_sel do_sum .

    endloop.

  else.
* Other Than Summary Option
    loop at gt_fieldcat.
      case gt_fieldcat-fieldname.
        when 'PERNR'.
          gt_fieldcat-seltext_l = 'Team Member'.
          gt_fieldcat-seltext_m = 'Team Member'.
          gt_fieldcat-seltext_s = 'Team Member'.
          gt_fieldcat-key = 'X' .
          gt_fieldcat-key_sel = 'X'.

        when 'PERID'.
          gt_fieldcat-seltext_l = 'SSN'.
          gt_fieldcat-seltext_m = 'SSN'.
          gt_fieldcat-seltext_s = 'SSN'.
          gt_fieldcat-reptext_ddic = 'SSN'.
          gt_fieldcat-key = 'X' .
          gt_fieldcat-key_sel = 'X'.


        when 'FPPER'.
          gt_fieldcat-seltext_l = 'Pay Period'.
          gt_fieldcat-seltext_m = 'Pay Period'.
          gt_fieldcat-seltext_s = 'Pay Period'.
          gt_fieldcat-key = 'X' .

        when 'GROSS_SAL'.
          gt_fieldcat-seltext_l = 'Gross Salary'.
          gt_fieldcat-seltext_m = 'Gross Salary'.
          gt_fieldcat-seltext_s = 'Gross Salary'.
          gt_fieldcat-do_sum  = 'X'.

        when '401K_AMT'.
          gt_fieldcat-seltext_l = '401K Wages'.
          gt_fieldcat-seltext_m = '401K Wages'.
          gt_fieldcat-seltext_s = '401K Wages'.
          gt_fieldcat-do_sum  = 'X'.
        when 'EE%'.
          gt_fieldcat-seltext_l = 'EE%'.
          gt_fieldcat-seltext_m = 'EE%'.
          gt_fieldcat-seltext_s = 'EE%'.
          gt_fieldcat-reptext_ddic = 'EE%'.

        when 'ER_PAID'.
          gt_fieldcat-seltext_l = 'EMPR. Paid'.
          gt_fieldcat-seltext_m = 'EMPR. Paid'.
          gt_fieldcat-seltext_s = 'EMPR. Paid'.
          gt_fieldcat-reptext_ddic = 'ER.Paid'.
          gt_fieldcat-do_sum  = 'X'.

        when 'DIFF'.
          gt_fieldcat-seltext_l = 'Corr. Amt.'.
          gt_fieldcat-seltext_m = 'Corr. Amt.'.
          gt_fieldcat-seltext_s = 'Corr. Amt.'.
          gt_fieldcat-reptext_ddic = 'Corr. Amt.'.
          gt_fieldcat-do_sum  = 'X'.
        when 'DIFF1'.
          gt_fieldcat-seltext_l = 'Difference'.
          gt_fieldcat-seltext_m = 'Difference'.
          gt_fieldcat-seltext_s = 'Difference'.
          gt_fieldcat-reptext_ddic = 'Difference'.
          gt_fieldcat-do_sum  = 'X'.
        when 'FPBEG' .
          gt_fieldcat-seltext_l = 'Begin Date.'.
          gt_fieldcat-seltext_m = 'Begin Date'.
          gt_fieldcat-seltext_s = 'Begin Date'.

        when 'FPEND'.
          gt_fieldcat-seltext_l = 'End Date'.
          gt_fieldcat-seltext_m = 'End Date'.
          gt_fieldcat-seltext_s = 'End Date'.
        when 'EE_401K' .
          gt_fieldcat-seltext_l = 'EE 401K'.
          gt_fieldcat-seltext_m = 'EE 401K'.
          gt_fieldcat-seltext_s = 'EE 401K'.
          gt_fieldcat-reptext_ddic = 'EE 401K'.
          gt_fieldcat-do_sum  = 'X'.

        when 'EE_401CU'.
          gt_fieldcat-seltext_l = 'EE 401CU'.
          gt_fieldcat-seltext_m = 'EE 401CU'.
          gt_fieldcat-seltext_s = 'EE 401CU'.
          gt_fieldcat-reptext_ddic = 'EE 401CU'.
          gt_fieldcat-do_sum  = 'X'.


* begin of changes -  UD1K940413
        when 'T_DATE'.
*          if s_STAT2-low eq '0'.  "Only for withdrawn
          gt_fieldcat-seltext_l = 'Withdrawn Date'.
          gt_fieldcat-seltext_m = 'Withdrawn Date'.
          gt_fieldcat-seltext_s = 'Withdrawn Date'.
          gt_fieldcat-reptext_ddic = 'Withdrawn Date'.
          gt_fieldcat-key = 'X' .
          gt_fieldcat-key_sel = 'X'.
*           gt_fieldcat-do_sum  = 'X'.
*          else.
*          delete  gt_fieldcat  where fieldname eq gt_fieldcat-fieldname
          .
*            clear gt_fieldcat.
*            continue.
*          endif.
* end of changes -  UD1K940413

        when  '401E_DATE'.
          gt_fieldcat-seltext_l = '401K EDATE'.
          gt_fieldcat-seltext_m = '401K EDATE'.
          gt_fieldcat-seltext_s = '401K EDATE'.
          gt_fieldcat-reptext_ddic = '401K EDATE'.
          gt_fieldcat-key = 'X' .
          gt_fieldcat-key_sel = 'X'.

        when '401ER_DATE'.
          gt_fieldcat-seltext_l = '401K ERDATE'.
          gt_fieldcat-seltext_m = '401K ERDATE'.
          gt_fieldcat-seltext_s = '401K ERDATE'.
          gt_fieldcat-reptext_ddic = '401K ERDATE'.
          gt_fieldcat-key = 'X' .
          gt_fieldcat-key_sel = 'X'.

        when  'HIRE_DATE'.
          gt_fieldcat-seltext_l = 'HIRE DATE'.
          gt_fieldcat-seltext_m = 'HIRE DATE'.
          gt_fieldcat-seltext_s = 'HIRE DATE'.
          gt_fieldcat-reptext_ddic = 'HIRE DATE'.
          gt_fieldcat-key = 'X' .
          gt_fieldcat-key_sel = 'X'.

* UD1K941143
* {
*          if p_act eq 'X'.
        when  'ER_PAID_A'.
          gt_fieldcat-no_out = 'X'.
*          else.
*            gt_fieldcat-seltext_l = '(A)EMPR.Paid'.
*            gt_fieldcat-seltext_m = '(A)EMPR.Paid'.
*            gt_fieldcat-seltext_s = '(A)EMPR.Paid'.
*            gt_fieldcat-reptext_ddic = '(A)ER.Paid'.
*            gt_fieldcat-do_sum  = 'X'.
*          endif.
* }

      endcase.
      modify gt_fieldcat transporting no_out
      seltext_l seltext_m seltext_s
      reptext_ddic key key_sel do_sum .

    endloop.
  endif.
endform.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0478   text
*      -->P_0479   text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
form build_layout using  p_cb p_color p_sum  .
  clear gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  gs_layout-colwidth_optimize = 'X'.                        "UD1K940912
*  gs_layout-default_item      = 'X'.
  gs_layout-list_append  = 'X'.


endform.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
form build_comment using    p_gt_header type slis_t_listheader.
  data: ls_line  type slis_listheader,
         ls_color type slis_specialcol_alv,
         l_date(50).
  data: l_text(70) type c.
  data: i_lines(5).
  data: i_count(5).

  clear ls_line.
  ls_line-typ  = 'S'.
  ls_line-info = '401K Contribution Report'.
  append ls_line to p_gt_header.

endform.                    " build_comment
*&---------------------------------------------------------------------*
*&      Form  display_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_grid.
*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************

* if Summary Option is choosen.
  if p_r6 eq 'X' .

* Sort fields.
    clear gt_sort[].
    gt_sort-fieldname = 'PERNR'.
    gt_sort-up = 'X'.
    append gt_sort.
    if p_r4 eq 'X'.
      gt_sort-subtot = 'X'.
      append gt_sort.
    endif.

    it_final[] = wa_final[].
    sort it_final by pernr.

* Layout
    move : 'LINECOLOR' to gs_layout-info_fieldname.
*         'X'         TO gs_layout-colwidth_optimize,
*         'MARK'      TO gs_layout-box_fieldname.


    if sy-batch eq 'X' .      " Background


      call function 'REUSE_ALV_BLOCK_LIST_APPEND'
           EXPORTING
                is_layout   = gs_layout
                it_fieldcat = gt_fieldcat[]
                i_tabname   = 'IT_OUTPUT'
                it_events   = gt_events[]
                it_sort     = gt_sort[]
                i_text      = ' '
           TABLES
                t_outtab    = it_final.

    else.

      call function 'REUSE_ALV_GRID_DISPLAY'
           exporting
                i_bypassing_buffer       = 'X'
                i_callback_program       = g_repid
*            i_callback_pf_status_set = 'SET_STATUS'
                i_callback_user_command  = 'USER_COMMAND'
                it_fieldcat              = gt_fieldcat[]
                i_save                   = 'A'
                it_events                = gt_events[]
                is_print                 = gs_print
                it_sort                  = gt_sort[]
                is_layout                = gs_layout
           importing
                e_exit_caused_by_caller  = g_exit_caused_by_caller
                es_exit_caused_by_user   = gs_exit_caused_by_user
           tables
                t_outtab                 = it_final.
    endif.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  else.
* Sort fields.
    clear gt_sort[].
    gt_sort-fieldname = 'PERNR'.
    gt_sort-up = 'X'.
    append gt_sort.
    gt_sort-fieldname = 'PERID'.
    gt_sort-up = 'X'.
    append gt_sort.
    gt_sort-fieldname = '401E_DATE'.
    gt_sort-up = 'X'.
    append gt_sort.
    gt_sort-fieldname = 'HIRE_DATE'.
    gt_sort-up = 'X'.
    append gt_sort.
    gt_sort-fieldname = 'T_DATE'.
    gt_sort-up = 'X'.
    append gt_sort.
    gt_sort-fieldname = '401ER_DATE'.
    gt_sort-up = 'X'.
    append gt_sort.
    if p_r4 eq 'X'.
      gt_sort-subtot = 'X'.
      append gt_sort.
    else.
      gt_sort-fieldname = 'FPPER'.
      gt_sort-up = 'X'.
      append gt_sort.
    endif.


* Layout
    move : 'LINECOLOR' to gs_layout-info_fieldname.
*         'X'         TO gs_layout-colwidth_optimize,
*         'MARK'      TO gs_layout-box_fieldname.

    if sy-batch eq 'X' .      " Background


      call function 'REUSE_ALV_BLOCK_LIST_APPEND'
           EXPORTING
                is_layout   = gs_layout
                it_fieldcat = gt_fieldcat[]
                i_tabname   = 'IT_OUTPUT'
                it_events   = gt_events[]
                it_sort     = gt_sort[]
                i_text      = ' '
           TABLES
                t_outtab    = it_output.

    else.
      call function 'REUSE_ALV_GRID_DISPLAY'
           EXPORTING
                i_bypassing_buffer      = 'X'
                i_callback_program      = g_repid
                i_callback_user_command = 'USER_COMMAND'
                it_fieldcat             = gt_fieldcat[]
                i_save                  = 'A'
                it_events               = gt_events[]
                is_print                = gs_print
                it_sort                 = gt_sort[]
                is_layout               = gs_layout
           IMPORTING
                e_exit_caused_by_caller = g_exit_caused_by_caller
                es_exit_caused_by_user  = gs_exit_caused_by_user
           TABLES
                t_outtab                = it_output.
    endif.


    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.
endform.                    " display_grid
*&---------------------------------------------------------------------*
*&      Form  get_payroll_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_ABKRS  text
*      <--P_W_PERMO  text
*      <--P_P_BEGCA  text
*      <--P_P_ENDCA  text
*      <--P_W_ABKRT  text
*      <--P_P_ABRPC  text
*      <--P_P_ABRJC  text
*----------------------------------------------------------------------*
form get_payroll_period using v_abkrs
                     changing v_permo " v_begda v_endda
                              v_abkrt v_pabrp v_pabrj.

  call function 'PA03_PERIODDATES_GET'
       EXPORTING
            f_abkrs               = v_abkrs
       IMPORTING
            f_permo               = v_permo
*            f_current_begda       = v_begda
*            f_current_endda       = v_endda
            f_abkrs_text          = v_abkrt
       CHANGING
            f_current_period      = v_pabrp
            f_current_year        = v_pabrj
       EXCEPTIONS
            pcr_does_not_exist    = 1
            abkrs_does_not_exist  = 2
            period_does_not_exist = 3
            others                = 4.
  if sy-subrc <> 0.
*      message e003 with v_pabrp v_pabrj.
  endif.


endform.                    " get_payroll_period
*&---------------------------------------------------------------------*
*&      Form  read_termination_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_termination_date.
*  select single * from PA0041
*            where  PERNR = it_tab-pernr and
*                   begda <=  sy-datum and
*                   endda >= sy-datum .
*  if sy-subrc eq 0.
** Z3 can be in any of 12 fields (Dar01 - Dar12 )
*    DO 12 TIMES VARYING l_code FROM pa0041-dar01 NEXT pa0041-dar02
*                VARYING l_date FROM pa0041-dat01 NEXT pa0041-dat02 .
*      IF l_code = 'Z3'.
*        l_date = l_date.
*        EXIT.
*      ENDIF.
*    ENDDO.
*  endif.
endform.                    " read_termination_date
*&---------------------------------------------------------------------*
*&      Form  read_eligibilty_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_eligibilty_date.
  data :  serv_date like  p0041-dat01.

  clear hire_date.
  select single * from pa0041
            where  pernr = it_tab-pernr and
                   begda <=  sy-datum and
                   endda >= sy-datum .
  if sy-subrc eq 0.
* Z3 can be in any of 12 fields (Dar01 - Dar12 )
    do 12 times varying l_code1 from pa0041-dar01 next pa0041-dar02
               varying 401l_date from pa0041-dat01 next pa0041-dat02
.

      if l_code1 = 'Z1'.
        hire_date = 401l_date.                              "UD1K940516
      endif.

      if l_code1 = 'Z9'.
        401e_date = 401l_date.
*        EXIT.
      endif.
      if l_code1 = 'Z3'.                                    "UD1K940520
        t_date = 401l_date.
      endif.

    enddo.
  endif.
endform.                    " read_eligibilty_date
*&---------------------------------------------------------------------*
*&      Form  display_exception_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_exception_report.
  data ls_print type slis_print_alv.

* Show 401K Summary Fiscal Year wise.


  perform build_fieldcat2  using  'IT_401KSUM'.

* Page Heading
  clear : gt_events[], gt_sort[].

  gt_events-name = 'TOP_OF_PAGE'.
  gt_events-form = 'TOP_OF_PAGE401K_SUM'.
  append gt_events.

  sort it_401ksum by fpper.

  call function 'REUSE_ALV_BLOCK_LIST_APPEND'
       EXPORTING
            is_layout   = gs_layout
            it_fieldcat = gt_fieldcat2[]
            i_tabname   = 'IT_401KSUM'
            it_events   = gt_events[]
            it_sort     = gt_sort[]
            i_text      = 'Employees 401K Summary'
       TABLES
            t_outtab    = it_401ksum[].



* Show Employees without 401K Plan
* Field catalog
  perform build_fieldcat1  using  'IT_NO401K'.

* Page Heading
  clear : gt_events[], gt_sort[].

  gt_events-name = 'TOP_OF_PAGE'.
  gt_events-form = 'TOP_OF_PAGE401K'.
  append gt_events.


  call function 'REUSE_ALV_BLOCK_LIST_APPEND'
       EXPORTING
            is_layout   = gs_layout
            it_fieldcat = gt_fieldcat1[]
            i_tabname   = 'IT_NO401K'
            it_events   = gt_events[]
            it_sort     = gt_sort[]
            i_text      = 'Employees without 401K Eligibilty Date'
       TABLES
            t_outtab    = it_no401k[].


  ls_print-no_print_selinfos = 'X'. " Display no selection infos
  ls_print-no_print_listinfos = 'X'. " Display no listinfos
  ls_print-reserve_lines = 2. " Lines reserved for end of page


  call function 'REUSE_ALV_BLOCK_LIST_DISPLAY'
       EXPORTING
            i_interface_check = ' '
            is_print          = ls_print
       EXCEPTIONS
            program_error     = 1
            others            = 2.

  .
endform.                    " display_exception_report

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE401K                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form top_of_page401k.
  data: ld_text type slis_text40.
  ld_text = 'Employees without 401K Eligibility Date'..
  write: / ld_text.
endform.

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE401K_SUM                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form top_of_page401k_sum.
  data: ld_text type slis_text40.
  ld_text = '401K Difference Fiscal Year wise'..
  write: / ld_text.
endform.


*&---------------------------------------------------------------------*
*&      Form  build_fieldcat1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2184   text
*----------------------------------------------------------------------*
form build_fieldcat1 using p_intab type slis_tabname.


  clear   : gt_fieldcat1, gt_fc1.
  refresh : gt_fieldcat1, gt_fc1.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name         = g_repid
            i_internal_tabname     = p_intab
            i_inclname             = g_repid
       CHANGING
            ct_fieldcat            = gt_fc1
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            others                 = 3.

  gt_fieldcat1[] = gt_fc1[].


endform.                    " build_fieldcat1


*---------------------------------------------------------------------*
*       FORM build_fieldcat2                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_INTAB                                                       *
*---------------------------------------------------------------------*
form build_fieldcat2 using p_intab type slis_tabname.


  clear   : gt_fieldcat2, gt_fc2.
  refresh : gt_fieldcat2, gt_fc2.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name         = g_repid
            i_internal_tabname     = p_intab
            i_inclname             = g_repid
       CHANGING
            ct_fieldcat            = gt_fc2
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            others                 = 3.

  gt_fieldcat2[] = gt_fc2[].


endform.                    " build_fieldcat1
*&---------------------------------------------------------------------*
*&      Form  output_options
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form output_options.

  data : l_cnt type i.

* if Overpayment option is chosen
* Retain records with only postive difference
  if p_r2 eq 'X'.
    delete it_output where diff1 <= 0.
    loop at it_output.
      it_401ksum-fpper = it_output-fpper+0(4).
      it_401ksum-diff1 = it_output-diff1.
      collect it_401ksum.
    endloop.
  endif.

* if under payment option is chosen
* Retain records with only negative difference
  if p_r3 eq 'X'.
    delete it_output where diff1 >= 0.
    loop at it_output.
      it_401ksum-fpper = it_output-fpper+0(4).
      it_401ksum-diff1 = it_output-diff1.
      collect it_401ksum.
    endloop.
  endif.

  if p_r1  eq 'X'.
*    delete it_output where diff1 = 0.
  endif.


*if a  employees has  one difference then show all records for that
*employee
  if  p_r9 eq 'X' .
    describe table it_diff lines l_cnt.
    if l_cnt > 0.
      sort it_diff by pernr.
      loop at it_output.

        at new pernr.
          read table it_diff with key pernr = it_output-pernr
                                         binary search.
          if  sy-subrc ne 0.
            delete it_output where pernr = it_output-pernr.
          endif.
        endat.
      endloop.
    else.
      delete  it_output where pernr <> '0000000' .
    endif.
  endif.
* Show records that have differences only
  if p_r7  eq 'X'.
    delete it_output where diff1 = 0.

* Over Payment
    loop at it_output where diff1  > 0.
      it_401ktemp-fpper = it_output-fpper+0(4).
      it_401ktemp-diff1 = it_output-diff1.
      collect it_401ktemp.
    endloop.
    append lines of it_401ktemp to it_401ksum.
* Under Payment
    refresh it_401ktemp[]. clear it_401ktemp.

    loop at it_output where diff1  < 0.
      it_401ktemp-fpper = it_output-fpper+0(4).
      it_401ktemp-diff1 = it_output-diff1.
      collect it_401ktemp.
    endloop.
    append lines of it_401ktemp to it_401ksum.

  endif.

* Summary Option is choosen
  if  p_r6 eq 'X'.
    if p_two eq 'X'.
* Get All over Payments and SUM it
      loop at it_output where diff1 >  0.
        move-corresponding it_output to wa_final.
        wa_final-flag = 'O'.
        wa_final-linecolor = ''.
        collect wa_final.
      endloop.
* Get All Under Payments and SUM it
      loop at it_output where diff1 <  0.
        move-corresponding it_output to wa_final.
        wa_final-flag = 'U'.
        wa_final-linecolor = ''.
        collect wa_final.
      endloop.
    else.

* UD1K941129 by IG.MOON
* {
      loop at it_output where diff1 <>  0 .
        move-corresponding it_output to wa_final.
        wa_final-flag = ''.
        wa_final-linecolor = ''.
        collect wa_final.
      endloop.
* }
    endif.
  endif.

  sort it_output  by pernr  fpper.
endform.                    " output_options
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
    if screen-group1 = 'STW'.
      screen-invisible = 1.
    endif.
    case 'X'.
      when p_r6.
        if screen-group1 = 'STW'.
          screen-invisible = 0.
        endif.
    endcase.
    modify screen.
  endloop.
endform.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  get_prev_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IN_RGDIR  text
*----------------------------------------------------------------------*
form get_prev_data tables   p_in_rgdir structure pc261.

* Temporary internal for storing 'O' records.
  data $in_rgdir like pc261 occurs 0 with header line.

* Move 'O' data into temporary int. table.
  loop at p_in_rgdir where srtza eq 'O'.
    $in_rgdir = p_in_rgdir.
    append $in_rgdir.
  endloop.

  check not $in_rgdir[] is initial.

* Delete 'O' data from original int. table
  delete p_in_rgdir where srtza ne 'P' and srtza ne 'A' .
  clear p_in_rgdir.

* check with 'P' data in original if 'O' data is exists.

  data : $inper type iperi,
         $ix like sy-tabix.

* find out the latest in-period on OLD.
*  perform get_latest_in_period tables $in_rgdir
*                             changing $inper.

  check not $inper  is initial.

* Only work with the latest old recoreds.
  delete $in_rgdir where inper ne $inper.

* sort temp. int. table
  sort $in_rgdir by fpper.

  loop at p_in_rgdir where srtza eq 'P'.
    $ix = sy-tabix.
    read table $in_rgdir with key fpper = p_in_rgdir-fpper.
    if sy-subrc eq 0.
      p_in_rgdir = $in_rgdir.
      modify p_in_rgdir index $ix.
    endif.
  endloop.

endform.                    " get_prev_data
*&---------------------------------------------------------------------*
*&      Form  get_latest_in_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$IN_RGDIR  text
*----------------------------------------------------------------------*
form get_latest_in_period tables   p_$in_rgdir structure pc261
                                   p_rgdir structure pc261.

  data $in_rgdir like pc261 occurs 0 with header line.
  $in_rgdir[] = p_$in_rgdir[].
  sort $in_rgdir by inper descending.
  read table $in_rgdir index 1.
  if sy-subrc eq 0.
    p_rgdir = $in_rgdir-inper.
  endif.

endform.                    " get_latest_in_period
*&---------------------------------------------------------------------*
*&      Form  read_payroll_data_prv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_payroll_data_prv.
  data : flag(1) type c,
         prev_fpper type  pc261-fpper,
         lv_molga type molga,
         lw_month  like  t5a4a-dlydy,
         $l_edate like l_edate.

  data ip_beg like sy-datum.
  data ip_end like sy-datum.
  data fp_beg like sy-datum.
  data fp_end like sy-datum.
  data ip_beg_ref like sy-datum.
  data ip_end_ref like sy-datum.
  data fp_beg_ref like sy-datum.
  data fp_end_ref like sy-datum.

  data $it_output like it_output occurs 0 with header line.
  data lt_inper_directory type tt_inper_directory.
  data ls_inper_directory_entry type ts_inper_directory_entry.
  data lt_tweaked_evp type hrpy_tt_rgdir.
  data lt_evp_related_records type hrpy_tt_rgdir.
  data lt_eval_tab  type pay_t_eval_period.
  field-symbols <eval_wa> type pay_eval_period.
  data lv_evp_lines_number type i.
  data $fpper like pc261-fpper.
  data $year(4).
  data $str(2) type n.
  data: lt_rgdir type hrpy_tt_rgdir,
        ls_rgdir type hrpy_tt_rgdir with header line.


  $year = s_fpper-low(4).

  loop at it_tab.
    ll_pernr = it_tab-pernr .

* Read Termination date
    clear: l_code, t_date,401e_date, l_edate,
           hire_date.

* Read 401K Eligibilty Date/Hire/Termination Date      "UD1K940451
    perform read_eligibilty_date.

* UD1K940484
* Begin of changes -* UD1K940484
* If 401K Eligibilty Date is maintained for an employee
* then reports show incorrect value. So show Exceptions
* report for the employee who are missing 401K eligibilty
* date ( Z9) and Skip those employee when RUn online
    if 401e_date is initial.
      it_no401k-pernr = it_tab-pernr.
      append it_no401k. clear it_no401k.
      continue.
    endif.

* End of changes - UD1K940484

* Add 12 Months to 401K Eligibity Date
* to determine date from which employee will
* receive 401K Employer contribution - UD1K940451

    call function 'HR_PSD_DATES_ADD_MONTHS'
         EXPORTING
              v_date   = 401e_date
              v_months = 12
         IMPORTING
              e_date   = l_edate.

* EXCEPTIONS
*   NOT_POSITIVE       = 1
*   OTHERS             = 2

* Read Payroll Control Records
    clear lv_molga.
    call function 'CU_READ_RGDIR'
         EXPORTING
              persnr          = ll_pernr
         IMPORTING
              molga           = lv_molga
         TABLES
              in_rgdir        = in_rgdir
         EXCEPTIONS
              no_record_found = 1
              others          = 2.

    ws_pernr = ll_pernr.

* Cluster id for US
* Personnel Country Grouping
    clear l_relid.

    select single relid into l_relid
                  from t500l
                  where molga = lv_molga.
    if   l_relid is initial.
      l_relid = 'RU'.
    endif.

    __cls : pt_output, pt_diff.

    do 26 times.
      $str = sy-index.
      concatenate $year $str into $fpper.
      check $fpper in s_fpper.
      __cls : lt_tweaked_evp, lt_inper_directory, lt_eval_tab.
      perform check_abkrs_pabrp_pabrj using
                            $fpper
                            p_abkrs
                            changing ip_beg ip_end.


      perform fill_inper_dir using 'X' ' ' ' '
          ip_beg ip_end ' ' ' ' in_rgdir[]
        changing lt_inper_directory[].


*//  2011.08.15  insert. ======================== //*
      REFRESH lt_rgdir.   "" ADD KDM01(10/18/2011)
      loop at in_rgdir.
        move-corresponding in_rgdir to ls_rgdir.
        append ls_rgdir to lt_rgdir.
        clear:  ls_rgdir.
      endloop.
*//  ============================================ //*

      loop at lt_inper_directory into ls_inper_directory_entry.
        clear lt_evp_related_records. refresh lt_evp_related_records.
*// 2011.08.10 change by kimyn for ecc6.0
        lt_eval_tab = cl_hr_cd_manager=>eval_periods(
                    imp_inpty      = ls_inper_directory_entry-inpty
                    imp_inper      = ls_inper_directory_entry-inper
                    imp_iperm      = ls_inper_directory_entry-iperm
                    imp_iabkrs     = pnpabkrs[]
                    imp_bondt      = ls_inper_directory_entry-ipend
                    imp_inpid      = ls_inper_directory_entry-inpid
                    imp_rgdir      = lt_rgdir
                    imp_all_of_run = false ).

***        loop at lt_eval_tab assigning <eval_wa>.
***          append lines of <eval_wa>-evp to lt_evp_related_records.
***        endloop.

***        call function 'CD_EVAL_PERIODS'
***             EXPORTING
***                  bonus_date      = ls_inper_directory_entry-ipend
***                  inper_modif     = ls_inper_directory_entry-iperm
***                  inper           = ls_inper_directory_entry-inper
***                  pay_type        = ls_inper_directory_entry-inpty
***                  pay_ident       = ls_inper_directory_entry-inpid
***             IMPORTING
***                  result          = lt_eval_tab
***             TABLES
***                  rgdir           = in_rgdir
***                  iabkrs          = pnpabkrs[]
***             EXCEPTIONS
***                  no_record_found = 1
***                  others          = 2.
***        if sy-subrc eq 2. exit. endif.
        if sy-subrc eq 0.
          loop at lt_eval_tab assigning <eval_wa>.
            append lines of <eval_wa>-evp to lt_evp_related_records.
          endloop.

          perform filter_tweaked_rgdir using 'X' ' '
                    '18000101' ip_end ' ' ' '
                    ls_inper_directory_entry
                  changing lt_evp_related_records[] lt_tweaked_evp[].
        endif.
      endloop.

* at this point all periods to process are in LT_EVP: go ahead
      describe table lt_tweaked_evp lines lv_evp_lines_number.

      check lv_evp_lines_number gt 0.

      sort lt_tweaked_evp by fpend descending seqnr ascending.

      perform process_evp   using lt_tweaked_evp[]
                                  lv_molga
                                  ll_pernr
                                  l_relid
                                  $fpper.

    enddo.
    append lines of : pt_output to it_output,
                      pt_diff to it_diff.
  endloop.

endform.                    " read_payroll_data_prv
*&---------------------------------------------------------------------*
*&      Form  fill_inper_dir
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3767   text
*      -->P_3768   text
*      -->P_3769   text
*      -->P_IP_BEG  text
*      -->P_IP_END  text
*      -->P_3772   text
*      -->P_3773   text
*      -->P_IN_RGDIR[]  text
*      <--P_LT_INPER_DIRECTORY[]  text
*----------------------------------------------------------------------*
form fill_inper_dir using
                  value(ix_ypernodt)
                  value(ix_yoc)
                  value(ix_for_view)
                  value(iv_ip_beg) type d
                  value(iv_ip_end) type d
                  value(iv_payty) type pc261-payty
                  value(iv_payid) type pc261-payid
                  it_rgdir type hrpy_tt_rgdir
         changing ot_inper_directory type tt_inper_directory.

  data ls_rgdir like line of it_rgdir.
  data ls_inper_directory like line of ot_inper_directory.

  if ix_ypernodt eq 'X'."""""""""""""""""""""""""""""""""""""""""""
    if ix_yoc eq space .
      clear iv_payty. clear iv_payid.
    endif.
    if ix_for_view eq 'X'.
      loop at it_rgdir into ls_rgdir
          where ipend between iv_ip_beg and iv_ip_end
            and void eq space
            and abkrs in pnpabkrs
            and payty eq iv_payty
            and payid eq iv_payid.
        move-corresponding ls_rgdir to ls_inper_directory.
        collect ls_inper_directory into ot_inper_directory.
      endloop.
    else."in-view
      loop at it_rgdir into ls_rgdir
          where ipend between iv_ip_beg and iv_ip_end
            and void eq space
            and iabkrs in pnpabkrs
            and ( inpty eq iv_payty and inpid eq iv_payid ).
        move-corresponding ls_rgdir to ls_inper_directory.
        collect ls_inper_directory into ot_inper_directory.
      endloop.
    endif."in-view or for-view
  else.      "ix_ypernodt eq false."""""""""""""""""""""""""""""""""
    if ix_for_view eq 'X'.
      loop at it_rgdir into ls_rgdir
          where ipend between iv_ip_beg and iv_ip_end
            and void eq space
            and abkrs in pnpabkrs.        "4 per4mance
        move-corresponding ls_rgdir to ls_inper_directory.
        collect ls_inper_directory into ot_inper_directory.
      endloop.
    else."in-view
      loop at it_rgdir into ls_rgdir
          where ipend between iv_ip_beg and iv_ip_end
            and void eq space
            and iabkrs in pnpabkrs.
        move-corresponding ls_rgdir to ls_inper_directory.
        collect ls_inper_directory into ot_inper_directory.
      endloop.
    endif."in-view or for-view
  endif.    "ix_ypernodt = ?"""""""""""""""""""""""""""""""""""""""""
endform.                    " fill_inper_dir
*&---------------------------------------------------------------------*
*&      Form  check_abkrs_pabrp_pabrj
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$FPPER  text
*      -->P_P_ABKRS  text
*      <--P_IP_BEG  text
*      <--P_IP_END  text
*----------------------------------------------------------------------*
form check_abkrs_pabrp_pabrj using
        p_period
        value(iv_abkr_cal)
        changing p_iv_fp_beg p_iv_fp_end.

  data lv_pnpxabkr type abkrs.
  data lv_permo like t549a-permo.
  data ov_abrp_cal like t569v-pabrp.
  data ov_abrj_cal like t569v-pabrj.

  data $p_iv_fp_beg like sy-datum.
  data $p_iv_fp_end like sy-datum.

  lv_pnpxabkr = iv_abkr_cal. "the types are different.

  ov_abrp_cal = p_period+4(2).
  ov_abrj_cal = p_period(4).

  call function 'PA03_PERIODDATES_GET'
       EXPORTING
            f_abkrs               = lv_pnpxabkr
       IMPORTING
            f_permo               = lv_permo
            f_current_begda       = $p_iv_fp_beg
            f_current_endda       = $p_iv_fp_end
       CHANGING
            f_current_period      = ov_abrp_cal
            f_current_year        = ov_abrj_cal
       EXCEPTIONS
            pcr_does_not_exist    = 1
            abkrs_does_not_exist  = 2
            period_does_not_exist = 3
            others                = 4.

  if not $p_iv_fp_beg is initial.
    p_iv_fp_beg = $p_iv_fp_beg.
  endif.
  if not $p_iv_fp_end is initial.
    p_iv_fp_end = $p_iv_fp_end.
  endif.

endform.                    " check_abkrs_pabrp_pabrj
*---------------------------------------------------------------------*
*       FORM filter_tweaked_rgdir                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(IX_YPERNODT)                                            *
*  -->  VALUE(IX_YOC)                                                 *
*  -->  VALUE(IV_FP_BEG)                                              *
*  -->  VALUE(IV_FP_END)                                              *
*  -->  VALUE(IV_PAYTY)                                               *
*  -->  VALUE(IV_PAYID)                                               *
*  -->  VALUE(IS_INPER_DIRECTORY_ENTRY)                               *
*  -->  IT_RGDIR                                                      *
*  -->  OT_RGDIR                                                      *
*---------------------------------------------------------------------*
form filter_tweaked_rgdir using
         value(ix_ypernodt)
         value(ix_yoc)
         value(iv_fp_beg) type d
         value(iv_fp_end) type d
         value(iv_payty) type pc261-payty
         value(iv_payid) type pc261-payid
         value(is_inper_directory_entry) type ts_inper_directory_entry
       changing it_rgdir type hrpy_tt_rgdir
         ot_rgdir type hrpy_tt_rgdir.
* see forms FILTER_INPER_DIR and CORE_PROC for explanations.
  data ls_rgdir like line of it_rgdir.
  sort it_rgdir by ipend descending.
  if ix_ypernodt eq 'X'."""""""""""""""""""""""""""""""""""""""""""
    if ix_yoc eq space .
      clear iv_payty. clear iv_payid.
    endif.
    loop at it_rgdir into ls_rgdir
        where void eq space.
      if ls_rgdir-srtza ne cd_c-actual.
        move-corresponding is_inper_directory_entry to ls_rgdir.
      endif.
      append ls_rgdir to ot_rgdir.
    endloop.
  else.      "ix_ypernodt eq space."""""""""""""""""""""""""""""""""
    loop at it_rgdir into ls_rgdir
        where void eq space.
      if ls_rgdir-srtza ne cd_c-actual.
        move-corresponding is_inper_directory_entry to ls_rgdir.
      endif.
      append ls_rgdir to ot_rgdir.
    endloop.
    loop at it_rgdir into ls_rgdir
        where void eq space.
      if ls_rgdir-srtza ne cd_c-actual.
        move-corresponding is_inper_directory_entry to ls_rgdir.
      endif.
      append ls_rgdir to ot_rgdir.
    endloop.
  endif.    "ix_ypernodt = ?"""""""""""""""""""""""""""""""""""""""""
endform.                    " filter_tweaked_rgdir
*&---------------------------------------------------------------------*
*&      Form  process_evp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_TWEAKED_EVP[]  text
*      -->P_LV_MOLGA  text
*      -->P_PERNR_PERNR  text
*      -->P_LV_RELID  text
*      -->P_IX_YCOMPPER  text
*      -->P_IX_REF_PER  text
*      -->P_IX_ARC_READ  text
*----------------------------------------------------------------------*
form process_evp  using it_evp type hrpy_tt_rgdir
                        value(iv_molga) type molga
                        value(iv_pernr) like pernr-pernr
                        value(iv_relid) like t500l-relid
                        p_fpper.

  data ls_evp like line of it_evp.
  data ls_result type pay99_result.
  data ls_lastwpbp like line of ls_result-inter-wpbp.
  data ls_wpbp like line of ls_result-inter-wpbp.
  data lx_retro type xfeld.

  data : flag(1) type c,
         prev_fpper type  pc261-fpper,
         lv_molga type molga,
         lw_month  like  t5a4a-dlydy,
         $l_edate like l_edate.

  data $pt_output like pt_output occurs 0 with header line.
  data $$pt_output like pt_output occurs 1 with header line.

  loop at it_evp into ws_rgdir.

    call function 'PYXX_READ_PAYROLL_RESULT'
         exporting
              clusterid                    = iv_relid
              employeenumber               = iv_pernr
              sequencenumber               = ws_rgdir-seqnr
*             READ_ONLY_BUFFER             = ' '
              read_only_international      = 'X'
         changing
              payroll_result               = result
         exceptions
              illegal_isocode_or_clusterid = 1
              error_generating_import      = 2
              import_mismatch_error        = 3
              subpool_dir_full             = 4
              no_read_authority            = 5
              no_record_found              = 6
              versions_do_not_match        = 7
              others                       = 8.

    loop at result-inter-wpbp into ls_lastwpbp. endloop.
    loop at result-inter-wpbp[] into ls_wpbp.  endloop.

    call function 'CD_RETROCALC_PERIOD'
         EXPORTING
              entry = ws_rgdir
         IMPORTING
              calcd = lx_retro.

    clear : ws_8387, ws_401k,ws_8337, ws_bc31, ws_gsal .

* by ig.moon 5/5/2010 {
    clear : ws_8347.
* }

    loop at result-inter-rt into wa_rt
    where ( lgart = '/102' or lgart = '8387'
            or  lgart = '8337' or lgart = 'BC31'
* By Furong On 01/31/14 (
            or  lgart = '8347'
* )
            or  lgart = '/101' ).

      if  wa_rt-lgart = '/101'.     "Gross Salary
        ws_gsal = wa_rt-betrg.
      endif.

      if  wa_rt-lgart = '/102'.                             "401K wages
        ws_401k = wa_rt-betrg.
      endif.
      if  wa_rt-lgart = '8387'.   " Employer paid
        ws_8387 = ws_8387 + wa_rt-betrg.
      endif.
      if wa_rt-lgart =  '8337'.   " Employee paid
        ws_8337 = ws_8337 + wa_rt-betrg.
      endif.
      if  wa_rt-lgart =  'BC31'.  "401C contribution
        ws_bc31 = ws_bc31 + wa_rt-betrg.
      endif.

* by ig.moon 5/5/2010 {
      if  wa_rt-lgart =  '8347'.  "401C was changed to 8347 12/14/2009
        ws_8347 = ws_8347 + wa_rt-betrg.
      endif.
* }
    endloop.

* by ig.moon 5/5/2010 {
    if  ws_8347 <> 0.
      ws_bc31 = ws_8347.
    endif.
* }

* Populate output internal Table.
    pt_output-pernr = it_tab-pernr.
    pt_output-perid = it_tab-perid.
    pt_output-fpper = ws_rgdir-fpper.   "Pay period
    pt_output-gross_sal  =    ws_gsal.   " Gross Salary
    pt_output-401k_amt = ws_401k.                           "401K Wages
    pt_output-er_paid  = ws_8387.       "ER paid
    pt_output-ee_401k  = ws_8337 * -1.  "EE paid UD1K940463
    pt_output-ee_401cu = ws_bc31 * -1.  "401C contriUD1K940463
    pt_output-fpbeg = ws_rgdir-fpbeg.   "Payroll begin date
    pt_output-fpend = ws_rgdir-fpend.   "payroll end date

    if ws_rgdir-srtza ne 'A'.
      pt_output-gross_sal = - pt_output-gross_sal.
      pt_output-401k_amt = - pt_output-401k_amt.
      pt_output-ee_401k = - pt_output-ee_401k.
      pt_output-ee_401cu = - pt_output-ee_401cu.
      pt_output-er_paid = - pt_output-er_paid.
      pt_output-diff = - pt_output-diff.
      pt_output-diff1 = - pt_output-diff1.
    endif.

    pt_output-t_date = t_date . " UD1K940413
    pt_output-401e_date = 401e_date.
    pt_output-hire_date = hire_date.   "Hire Date/UD1K940516
    pt_output-401er_date =   l_edate + 1 .
    if  pt_output-401er_date < '20040501'.
      pt_output-401er_date = '20040501'.
    endif.

    if pt_output-fpper ne p_fpper.
      $pt_output = pt_output.
      clear : $pt_output-fpper,$pt_output-fpbeg,$pt_output-fpend,
              $pt_output-linecolor.
      collect $pt_output.
      clear : pt_output, $pt_output.
    else.
      clear pt_output-linecolor.
      collect pt_output.
      clear pt_output.
    endif.

  endloop.                 "it_evp

  data $ix like sy-tabix.

  read table pt_output with key  pernr = iv_pernr
                                 fpper = p_fpper .

  if sy-subrc eq 0.
    $ix = sy-tabix.
    $$pt_output = pt_output.
    read table $pt_output index 1.
    if sy-subrc eq 0.
      pt_output = $pt_output.
      move :
            $$pt_output-fpper to pt_output-fpper,
            $$pt_output-fpbeg to pt_output-fpbeg,
            $$pt_output-fpend to pt_output-fpend.
      collect pt_output.
    endif.
  endif.

  loop at pt_output where pernr = iv_pernr and fpper = p_fpper.
    $ix = sy-tabix.
    clear : pa0169, l_ee%.
    ws_8387 = pt_output-er_paid.
    ws_8337 = pt_output-ee_401k.

    if  ws_8337 <> 0 .      " Employer Contribution
      call function 'ZHR_READ_PA0169'
           EXPORTING
                pernr = pt_output-pernr
                subty = '401K'
                begda = pt_output-fpbeg
                endda = pt_output-fpend
           IMPORTING
                eepct = pt_output-ee%.

    else.
      call function 'ZHR_READ_PA0169'
           EXPORTING
                pernr = pt_output-pernr
                subty = '401K'
                begda = pt_output-fpbeg
                endda = pt_output-fpend
           IMPORTING
                eepct = l_ee%.
      clear pt_output-ee%.
    endif.

* Employee Contribution is < 3% then Apply that percentage

    if  pt_output-ee% < 3.

      if pt_output-fpbeg < '20100419'.
        pt_output-diff =
        pt_output-401k_amt *  ( 60 / 100 ) * ( pt_output-ee% / 100 ).
      else.
        pt_output-diff =
        pt_output-401k_amt *  ( 80 / 100 ) * ( pt_output-ee% / 100 ).
      endif.

    elseif pt_output-fpbeg <= '20061217' .   " 2004 - 2006 Pay period
      pt_output-diff = pt_output-401k_amt * ( 60 / 100 ) * ( 3 / 100 ).
    elseif  pt_output-ee% > 4.          "2007 ER % is 4
      if pt_output-fpbeg < '20100419'.
       pt_output-diff = pt_output-401k_amt * ( 60 / 100 ) * ( 4 / 100 ).
      else.
       pt_output-diff = pt_output-401k_amt * ( 80 / 100 ) * ( 4 / 100 ).
      endif.
    else.
      if pt_output-fpbeg < '20100419'.
        pt_output-diff = pt_output-401k_amt * ( 60 / 100 )
            * ( pt_output-ee% / 100 ).
      else.
        pt_output-diff = pt_output-401k_amt * ( 80 / 100 )
              * ( pt_output-ee% / 100 ).
      endif.
    endif.
    $l_edate  = l_edate + 1.
    if   pt_output-fpbeg > l_edate
        or ( $l_edate between pt_output-fpbeg and pt_output-fpend ) .
      pt_output-diff1 = pt_output-er_paid - pt_output-diff.
    else.
      clear pt_output-diff.
    endif.

    clear l_limit.
    l_limit  = ( pt_output-401k_amt * pt_output-ee% ) / 100.
    l_limit = l_limit .
    if l_limit <>  pt_output-ee_401k.
      move : c_red   to pt_output-linecolor.
    endif.

* For Employee who do have 169 record
    if pt_output-ee% eq  0 and pt_output-fpbeg > '20040501'.
      move : c_color  to pt_output-linecolor.
    endif.

    if  p_r9 eq 'X' and pt_output-diff1 <> 0.
      pt_diff-pernr =  pt_output-pernr.
      pt_diff-diff1 =  pt_output-diff1.
      collect pt_diff.
    endif.
    modify pt_output index $ix transporting ee% diff diff1 linecolor.
    clear pt_output.

  endloop.

endform.                              "process_evp
*&---------------------------------------------------------------------*
*&      Form  get_pa0169
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_PERNR  text
*      -->P_1408   text
*      -->P_WS_RGDIR_FPBEG  text
*      -->P_WS_RGDIR_FPEND  text
*      <--P_IT_OUTPUT_EE%  text
*----------------------------------------------------------------------*
FORM get_pa0169 USING    PERNR
                         VALUE(SUBTY)
                         BEGDA
                         ENDDA
                CHANGING EEPCT.

  data : begin of it_Pa0169 occurs 0,
          pernr like pa0169-pernr,
          EEPCT like pa0169-EEPCT,
          BEGDA like pa0169-BEGDA,
          endda like pa0169-endda,
         end of it_pa0169.

  data l_cnt type  i.

  select pernr EEPCT BEGDA endda
  into table it_Pa0169
                         from PA0169
                         where PERNR =  PERNR and
                               SUBTY =  '401K' and
                               BAREA = '10' and
                               endda >= BEGDA.
  sort   it_Pa0169  by pernr  BEGDA   .


  loop at it_pa0169.
    if BEGDA  between it_pa0169-begda and it_pa0169-endda.
* if 2 employee contributions records are maintained in
* the same pay period then pick the last record
      EEPCT  =  it_pa0169-EEPCT.
      exit.
    elseif ENDDA  between it_pa0169-begda and it_pa0169-endda.
      EEPCT  =  it_pa0169-EEPCT.
      exit.
    endif.
  endloop.

ENDFORM.                    " get_pa0169
