REPORT zcolabor1 NO STANDARD PAGE HEADING.
*
* logic by Andy
* program by Andy
*
* CO labor cost plan
*
TABLES: pa0001, pa0008, t510, t500p,
        cskt, pa0000,
        ztco_jc,
        zcolab1,
        zsco_lab02.

PARAMETERS: p_bukrs LIKE bkpf-bukrs MEMORY ID buk.

*arameters: p_gjahr like bkpf-gjahr default sy-datum(4),
*           p_monat like bkpf-monat default sy-datum+4(2).
PARAMETERS: p_datum LIKE sy-datum   DEFAULT sy-datum,
            p_max(2) TYPE n DEFAULT 25,
            p_inc(2) TYPE n DEFAULT  5 NO-DISPLAY.

PARAMETERS: p_pitm1(6) TYPE c DEFAULT 'A100' NO-DISPLAY,
            p_pitm2(6) TYPE c DEFAULT 'A150' NO-DISPLAY.

SELECT-OPTIONS: r_pernr  FOR pa0001-pernr,
                r_kostl  FOR pa0001-kostl,
                s_PERSG  for pa0001-persg,
                s_PERSK  for pa0001-persk.

*SELECT-OPTIONS: s_massn1 for pa0000-massn,
*                s_massn2 for pa0000-massn.


CONSTANTS:
 c_eg1(1)   TYPE c VALUE   'A',"US-Salary
 c_eg2(1)   TYPE c VALUE   'B',"US-Wage
 c_eg3(1)   TYPE c VALUE   'K'."KR-Salary


DATA: BEGIN OF i_pa0000 OCCURS 0,
        pernr LIKE pa0001-pernr,
        massn LIKE pa0000-massn,
      END OF i_pa0000.

DATA: BEGIN OF i_pa0001 OCCURS 0,
        pernr LIKE pa0001-pernr,
        werks LIKE pa0001-werks,
        persg LIKE pa0001-persg,
        persk LIKE pa0001-persk,
        kostl LIKE pa0001-kostl,
        stell LIKE pa0001-stell,
        abkrs LIKE pa0001-abkrs,
        categ(1) TYPE c,   "(K)orean, (S)alary, (W)age
      END OF i_pa0001.

*...
*        zcolab2,
*        zthr_pcp00,
*        zthr_pcp02,
*        zthr_pcp05,
*        zshr_pcp00.

DATA: i_jobcd LIKE ztco_jc OCCURS 0 WITH HEADER LINE.
*... internal tables
DATA: BEGIN OF itab OCCURS 0,
       bukrs  TYPE bukrs, "Company Code
*      zvern  TYPE zvern, "Version
*       gjahr  TYPE gjahr, "Fiscal year
*       month  TYPE month, "Month
       yyyymm(6),
       zpitm(6) TYPE c,   "Plan Item
       zkey(2)  TYPE c,   "Empl.Grp.Cate
       kostl  TYPE kostl, "Cost Center
*       persg  type persg, "Employee Group
*       persk  type persk, "Employee Subgroup
       jobcd  TYPE char2, "Job Code
       zsnrt(2) TYPE n,   "Seniority
       pranz  TYPE pranz, "HR payroll: Number
*      maxbt  TYPE maxbt, "HR Payroll: Amount
      END OF itab.

DATA: BEGIN OF it_cpy00 OCCURS 0.
        INCLUDE STRUCTURE itab.
DATA:   maxbt  TYPE maxbt. "HR Payroll: Amount
DATA: END OF it_cpy00.

DATA: it_pcp00 LIKE itab OCCURS 0 WITH HEADER LINE,
*      it_cpy00 LIKE itab OCCURS 0 WITH HEADER LINE,
      it_pcpxx LIKE zsco_lab02 OCCURS 0 WITH HEADER LINE.
DATA : it_pcp LIKE it_pcp00 OCCURS 0 .
*data : it_del like zthr_pcp02 occurs 0 with header line.

*DATA: BEGIN OF it_calsl OCCURS 0,   " monthly salary
*      persg    LIKE pa0001-persg,
*      persk    LIKE pa0001-persk,
*      kostl    LIKE pa0001-kostl,
*      stell    LIKE pa0001-stell,
*      zsnrt    LIKE zcolab1-zsnrt,
*      znumb    LIKE zcolab1-znumb,
*      hramt    LIKE zcolab1-hramt,
*      yramt    LIKE zcolab1-yramt,
*      zamt     LIKE zcolab1-zamt,
*      END OF it_calsl.

*data : begin of it_code occurs 0,
*       1300 like zthr_pcp02-zval1,
*       1301 like zthr_pcp02-zval1,
*       1302 like zthr_pcp02-zval1,
*
*       1320 like zthr_pcp02-zval1,
*       1321 like zthr_pcp02-zval1,
*       1322 like zthr_pcp02-zval1,
*       1330 like zthr_pcp02-zval1,
*       1340 like zthr_pcp02-zval1,
*       1350 like zthr_pcp02-zval1,
*       1360 like zthr_pcp02-zval1,
*       1370 like zthr_pcp02-zval1,
*       1380 like zthr_pcp02-zval1,
*       1390 like zthr_pcp02-zval1,
*       1400 like zthr_pcp02-zval1,
*       1401 like zthr_pcp02-zval2,
*       1410 like zthr_pcp02-zval1,
*       1411 like zthr_pcp02-zval2,
*       1430 like zthr_pcp02-zval1,
*       1500 like zthr_pcp02-zval1,
*       1501 like zthr_pcp02-zval2,
*       end of it_code.

*DATA: BEGIN OF it_1175 OCCURS 1,
*      zcode LIKE zthr_pcp02-zcode,
*      zval1 TYPE p DECIMALS 6,
*      END OF it_1175.

*DATA: BEGIN OF it_versn OCCURS 1,
*      zvern    LIKE zthr_pcp03-zvern.
*DATA: END OF it_versn.

DATA: it_field     LIKE help_value OCCURS 1 WITH HEADER LINE,
      dynpfields   LIKE STANDARD TABLE OF dynpread WITH HEADER LINE.

DATA: BEGIN OF it_persa OCCURS 1,
      werks    LIKE t500p-persa,
      name1    LIKE t500p-name1.
DATA: END OF it_persa.

DATA: BEGIN OF it_kostl OCCURS 1,
      zkost    LIKE cskt-kostl,
      zktxt    LIKE cskt-ktext.
DATA: END OF it_kostl.

DATA : BEGIN OF it_fpper OCCURS 0,
      fpper LIKE  pc261-fpper,
      betrg LIKE  pc207-betrg,
      xetrg LIKE  pc207-betrg,
      END OF it_fpper.

DATA : BEGIN OF it_extr OCCURS 0,
       zseqn LIKE zthr_pcp05-zseqn,
       rate  TYPE p DECIMALS 2,
       zextr LIKE zthr_pcp05-zextr,
       END OF it_extr.

DATA: it_payrt     TYPE hrpay99_tab_of_results,
      wa_payrt     TYPE pay99_result,
      wa_reslt     TYPE pc207,
      p_type(4).

RANGES: r_month    FOR wa_reslt-lgart,
        r_wekly    FOR wa_reslt-lgart,
        r_hramt    FOR wa_reslt-lgart,
        r_bonus    FOR wa_reslt-lgart,
        r_jobcode  FOR pa0001-stell,
        r_jobpaid  FOR pa0001-stell,
        r_mang     FOR zcolab1-zobjc,
        r_w401     FOR wa_reslt-lgart,
        r_persg    FOR p0001-persg,
        r_otcode   FOR pa0001-stell,
        r_1170     FOR zcolab1-zcost,
        r_1180     FOR zcolab1-zobjc.

DATA : BEGIN OF it_wage OCCURS 0,
       val1 LIKE zthr_pcp02-zval1,
       val2 LIKE zthr_pcp02-zval2,
       val3 LIKE zthr_pcp02-zval3,
       END  OF it_wage.
*... variants
DATA: w_zmons       LIKE zcolab1-zmons,
      w_werks      LIKE pa0001-werks,
      w_name1      LIKE t500p-name1,
      w_inrate     TYPE p DECIMALS 2.

DATA: w_titl1(60),
      w_titl2(60),
      w_titl3(60),
      w_titl4(60),
      w_titl5(60),
      w_titl6(60),
      w_titl7(60),
      w_titl8(60),
      w_titl9(60),
      w_titl10(60),
      w_titl11(60),
      w_titla(60),
      w_titlb(60),
      w_titlc(60),
      w_titld(60).

DATA: w_fname      LIKE  help_info-fieldname,
      w_tabix      LIKE  sy-tabix,
      w_fldvl      LIKE  help_info-fldvalue.

DATA: g_this_year_fr      LIKE sy-datum,       " closing date
      g_this_year_to      LIKE sy-datum.       " closing date
DATA: w_ptext(50),
      w_betrg      LIKE wa_reslt-betrg,
*      g_hramt      TYPE PC207,     " HOURLY WAGE
      g_hramt      LIKE bseg-dmbtr,
      g_yramt      LIKE bseg-dmbtr,
      w_bonus      LIKE bseg-dmbtr,     " BONUS WAGE
      w_401k       LIKE bseg-dmbtr,
      w_nhce       LIKE bseg-dmbtr,
      w_anhce      LIKE bseg-dmbtr,
      w_total      LIKE bseg-dmbtr,
      w_wrkmo      TYPE i,
      w_wrkdy      TYPE i,
      w_event      TYPE c.

*DATA: w_increse_rate1 TYPE p DECIMALS 1,
DATA : w_increse_rate1(20)," TYPE p DECIMALS 1,
      w_increse_rate2 TYPE p DECIMALS 1,
      w_increse_rate3 TYPE p DECIMALS 1.
* Attendance increase
DATA: w_attpay      LIKE zsco_lab02-zhouy,
      w_att_rate TYPE p DECIMALS 3.
RANGES      r_attjo FOR zcolab1-zobjc.
* GRID data
DATA: it_fcat             TYPE lvc_t_fcat.

DATA: ok_code LIKE sy-ucomm,
      gt_sflight TYPE TABLE OF sflight,
      g_container TYPE scrfname VALUE 'CCONTROL',
      grid1  TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container.




*perform make_basic_data.

* HR-GL ASSIGN
DATA:
  BEGIN OF coll_tab OCCURS 1,
    molga LIKE t500l-molga,
    lgart LIKE t512w-lgart,
    seqno LIKE t52el-seqno,
    koart LIKE t52ek-koart,
    sign  LIKE t52el-sign,
    spprc LIKE t52el-spprc,
    symko LIKE t52ek-symko,
    process LIKE acct_det_bf-process,
    momag LIKE t52em-momag,
    bukrs LIKE hrca_company-comp_code,
    dakat LIKE t5d7o-dakat,
    dart  LIKE t5d7c-dart,
    udart LIKE t5d7e-udart,
    shkz(1),                             "debit / credit
    acct LIKE acct_det_bf-gl_account,
  END OF coll_tab.


*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.
DATA:  g_repid      LIKE sy-repid.

CONSTANTS : c_status_set   TYPE slis_formname
                           VALUE 'PF_STATUS_SET',
            c_user_command TYPE slis_formname
                           VALUE 'USER_COMMAND',
            c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
            c_top_of_list  TYPE slis_formname VALUE 'TOP_OF_LIST',
            c_end_of_list  TYPE slis_formname VALUE 'END_OF_LIST'.
*---- ALV

Initialization.
*  s_massn1-option = 'I'.
*  s_massn1-option = 'EQ'.
*  s_massn1-low = 'Z5'. append s_massn1.
*  s_massn1-low = 'Z7'. append s_massn1.
*  s_massn1-low = 'Z8'. append s_massn1.
*  s_massn1-low = 'ZE'. append s_massn1.
*  s_massn1-low = 'ZI'. append s_massn1.
*  s_massn1-low = 'ZW'. append s_massn1.
*  s_massn1-low = 'ZX'. append s_massn1.
*  s_massn1-low = 'ZY'. append s_massn1.
*
*  s_massn2[] = s_massn1[]
*  s_massn2-option = 'I'.
*  s_massn2-option = 'EQ'.
*  s_massn2-low = 'ZH'. append s_massn2.

*
*Z0   Hiring
*Z1   Quick Hire (mini-master)
*Z2   Transfer/Relocation
*Z3   Organizational Reassignment
*Z4   Change in Pay
*Z5   Termination
*Z6   Rehire
*Z7   Retirement
*Z8   Retirement (outsourced)
*Z9   Inpatriation
*ZA   Payroll Hire Part 2
*ZB   LOA Active Status with Pay
*ZC   LOA Inactive Status
*ZD   Return from LOA
*ZE   End of Inpat Assignment
*ZF   Hire Applicant
*ZG   Change in Personal Data
*ZH   Expatriation
*ZI   End of Expat Assignment
*ZT   Country Reassignment
*ZW   Voluntary Termination
*ZX   Involuntary Termination
*ZY   Death


*******************
START-OF-SELECTION.
*******************
  g_this_year_fr = g_this_year_to = p_datum(4).
  g_this_year_fr+4(4) = '0101'.
  g_this_year_to+4(4) = '1231'.


  PERFORM get_hr_gl_mapping.
  PERFORM show_progress USING 10.

  PERFORM get_wage_types.
  PERFORM get_ee_org.

*FIXME.....hardcoding...
  PERFORM job_code_selection_check.
  PERFORM job_paid-leave-up-to-manager.

  PERFORM show_progress USING 20.

  PERFORM get_hr_and_calculate.
  PERFORM show_progress USING 60.

*****************
END-OF-SELECTION.
*****************
  PERFORM calc_avg_wage.
  PERFORM show_progress USING 90.

* ANDY
* perform display_out.
  WRITE:/ 'SHIFT+PF1 - Download'.

*&---------------------------------------------------------------------*
*DELETE FROM zcolab1
*           WHERE zyear = p_zyear
*             AND zvern = p_vern .
*  delete from zcolab2
*             where zyear = p_gjahr.
*            AND zvers = p_vern .
*COMMIT WORK.
*INSERT zcolab1 FROM TABLE it_pcp00.
*  INSERT zcolab2 FROM TABLE it_pcpxx.
*

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
AT PF13.
  PERFORM data_download.


************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
FORM user_command USING p_ucomm    LIKE sy-ucomm
                        p_selfield TYPE slis_selfield.
  CASE p_ucomm.
    WHEN 'PF13'.
    WHEN '&IC1'.
      PERFORM data_download.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  APPEND_PERNR_INFO_TABLE
*&---------------------------------------------------------------------*
FORM append_pernr_info_table USING f_begdt.
* calculate annual holiday
  DATA : f_ahday(8) TYPE p DECIMALS 2,
         l_cday     TYPE i.

  IF pa0001-stell IN r_jobpaid .
    l_cday = it_cpy00-zsnrt / 12 + 15.
  ELSE.
    l_cday = it_cpy00-zsnrt / 12 + 10.
  ENDIF.
  f_ahday = l_cday / 12.


  it_pcpxx-zyear = p_datum(4).
  it_pcpxx-pernr = pa0001-pernr.
  it_pcpxx-zcost = pa0001-kostl.
  it_pcpxx-zperg = pa0001-persg.
  it_pcpxx-zsubg = pa0001-persk.
  it_pcpxx-zobjc = pa0001-stell.

  it_pcpxx-zsnrt = it_cpy00-zsnrt.

  it_pcpxx-ansal = it_cpy00-maxbt.
* it_pcpxx-zhouy = it_cpy00-hramt.
  it_pcpxx-bonus = w_bonus.
  it_pcpxx-z401k = w_401k.
  it_pcpxx-znhce = ( w_nhce / w_wrkmo ) * 26.
  it_pcpxx-zanhce = w_anhce.
  it_pcpxx-ahday = f_ahday.  "Annual Holiday
  it_pcpxx-begda = f_begdt.

  it_pcpxx-erdat = sy-datum.
  it_pcpxx-erzet = sy-uzeit.
  it_pcpxx-ernam = sy-uname.
  it_pcpxx-zpera = pa0001-werks.
*
  APPEND it_pcpxx. CLEAR it_pcpxx.
ENDFORM.                    " APPEND_PERNR_INFO_TABLE

*&---------------------------------------------------------------------*
*&      Form  get_wage_master_SALER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wage_master USING f_hramt TYPE maxbt.

*1     Active Team Member
*2     Retiree
*4     Intern
*5     Terminated
*7     Contractor
*8     Expatriate
*9     Inpatriate
*A     Co-Op
*B     Temporary
*
*U0    Hourly
*U2    Salary Exempt
*U3    Salary Non-Exempt
*U4    Executives
*U6    Non-Payroll w/ben
*U7    Non-Payroll w/o ben
*U8    Contractor
*UA    Non-Payroll
*UB    Intern
*UC    Co-Op

* PA0015 - additinoal pay(vacation payout, ...)
* PA0014 - recurring/deduction

* Hourly rate * 2080 hr = Annual Amount

  CLEAR pa0008.
  SELECT SINGLE trfar trfgb trfgr trfst ansal bet01
                ind01 stvor lga01
            INTO (pa0008-trfar, pa0008-trfgb, pa0008-trfgr,
                  pa0008-trfst, pa0008-ansal, pa0008-bet01,
                  pa0008-ind01, pa0008-stvor, pa0008-lga01)
                 FROM pa0008
                 WHERE pernr = pa0001-pernr
                  AND endda = '99991231' .
*FIXME  pa0008-bet01 => monthly pay.

* Korean monthly Pay...
  IF i_pa0001-categ = c_eg3.     " pa0008-bet01 : monthly pay
    f_hramt = pa0008-ansal.
  ELSEIF i_pa0001-categ = c_eg1. " pa0008-bet01 : bi-weekly pay
    f_hramt = pa0008-ansal.
  ELSE.

*11	Bi-Weekly
*13	Monthly
*14	Non-Payroll

    IF pa0001-abkrs = '11' AND pa0008-bet01 <> 0.
      f_hramt = pa0008-bet01.  "Hourly Rate

    ELSEIF pa0008-ind01 = 'I' AND pa0008-bet01 = 0.
* if indirect valuation, no entry : PA0008-IND01, STVOR next increase
* 0007 infotype is used
*Employment percent       100.00
*Daily working hours        8.00
*Weekly working hours      40.00
*Monthly working hrs      173.33
*Annual working hours    2080.00
*Weekly workdays            5.00
*
* T511 table... wagetype(pa0008),

      f_hramt = 0.

* not need to valuate, use wage table directly
      IF sy-subrc = 999.
        PERFORM get_indirect_wage USING pa0001-pernr
                                        f_hramt.
      ENDIF.
    ELSE.
      f_hramt = pa0008-ansal.  "Annual...
    ENDIF.

  ENDIF.

ENDFORM.                    " get_wage_master_SALER
*&---------------------------------------------------------------------*
*&      Form  begin_end_day_cal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_RGDIR_FPBEG  text
*      -->P_LS_RGDIR_FPEND  text
*      <--P_L_BDATE  text
*      <--P_L_EDATE  text
*----------------------------------------------------------------------*
FORM begin_end_day_cal USING    p_fpbeg
                                p_fpend
                       CHANGING p_bdate
                                p_edate.

*   DATE CACULATION
  IF p_bdate IS INITIAL.
    p_bdate = p_fpbeg.
  ENDIF.

  IF p_edate IS INITIAL.
    p_edate = p_fpend.
  ENDIF.

  IF p_bdate >  p_fpbeg.
    p_bdate = p_fpbeg.
  ENDIF.

  IF p_edate < p_fpend.
    p_edate = p_fpend.
  ENDIF.

ENDFORM.                    " begin_end_day_cal
*&---------------------------------------------------------------------*
*&      Form  data_delete_history
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form data_delete_history.
*
*  delete from zcolab1 where zyear = p_zyear
*                          and zvern = p_vern .
*
*  delete from zcolab2 where zyear = p_zyear
*                           and zvern = p_vern .
*
*  delete from zthr_pcp05 where zyear = p_zyear
*                           and zvern = p_vern .
*
*  delete from zthr_pcp06 where zyear = p_zyear
*                           and zvern = p_vern .
*
*  delete from zthr_pcp07 where zyear = p_zyear
*                           and zvern = p_vern .
*
*  delete from zthr_pcp08 where zyear = p_zyear
*                           and zvern = p_vern .
*
*  delete from zthr_ahc01 where zyear = p_zyear
*                           and zvern = p_vern .
*
*endform.                    " data_delete_history
*&---------------------------------------------------------------------*
*&      Form  GET_401K_WAGE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_401k_wage_type.


*  SELECT zval1 zval2 INTO (zthr_pcp02-zval1, zthr_pcp02-zval2)
*    FROM zthr_pcp02 WHERE zmodl = '02'
*                      AND zgrup = '1220'
*                      AND zval2 = '601310'.
*
*    r_w401-low = zthr_pcp02-zval1.
*    APPEND r_w401. CLEAR r_w401.
*
*  ENDSELECT.



ENDFORM.                    " GET_401K_WAGE_TYPE
*&---------------------------------------------------------------------*
*&      Form  get_wage_types
*&---------------------------------------------------------------------*
FORM get_wage_types.
* get wage type by G/L account
  CLEAR: r_month, r_wekly, r_hramt, r_bonus.
  REFRESH: r_month, r_wekly, r_hramt, r_bonus.
*  CLEAR: it_wage[], it_wage.

  r_month-sign = r_wekly-sign = r_hramt-sign = r_bonus-sign =
  r_w401-sign = 'I'.
  r_month-option = r_wekly-option = r_hramt-option =
  r_bonus-option = r_w401-option = 'EQ'.


  LOOP AT coll_tab.
    CASE coll_tab-acct.
      WHEN '0000601100'.  "Korean
        IF coll_tab-momag = '9'.
          r_month-low = coll_tab-lgart. APPEND r_month.
        ENDIF.
      WHEN '0000601110'.
        IF coll_tab-momag = '1'.
          r_wekly-low = coll_tab-lgart. APPEND r_wekly.
        ENDIF.
      WHEN '0000601120'. r_hramt-low = coll_tab-lgart. APPEND r_hramt.
      WHEN '0000601200'. r_bonus-low = coll_tab-lgart. APPEND r_bonus.
      WHEN '0000601360'. r_w401-low  = coll_tab-lgart. APPEND r_w401.
    ENDCASE.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM r_month.
  DELETE ADJACENT DUPLICATES FROM r_wekly.
  DELETE ADJACENT DUPLICATES FROM r_hramt.
  DELETE ADJACENT DUPLICATES FROM r_bonus.
  DELETE ADJACENT DUPLICATES FROM r_w401.

*  r_w401-sign   = 'I'.
*  r_w401-option = 'EQ'.
*  r_w401-low    = '8387'.
*  append r_w401. clear r_w401.


*FIXME
*  CLEAR zthr_pcp02.
*  SELECT zval1 zval2 zval3
*    INTO (zthr_pcp02-zval1, zthr_pcp02-zval2, zthr_pcp02-zval3)
*    FROM zthr_pcp02 WHERE zmodl = '02'
*                      AND zgrup = '1220'
*                      AND zval2 IN ('601100', '601110', '601120',
*                                    '601200', '601310').
*
*    CASE zthr_pcp02-zval2.
*      WHEN '601100'.
*        r_month-low = zthr_pcp02-zval1.
*        APPEND r_month. CLEAR r_month.
*      WHEN '601110'.
*        r_wekly-low = zthr_pcp02-zval1.
*        APPEND r_wekly. CLEAR r_wekly.
*      WHEN '601120'.
*        r_hramt-low = zthr_pcp02-zval1.
*        APPEND r_hramt. CLEAR r_hramt.
*      WHEN '601200'.
*        r_bonus-low = zthr_pcp02-zval1.
*        APPEND r_bonus. CLEAR r_bonus.
*      WHEN '601360'.
*        r_w401-low = zthr_pcp02-zval1.
*        APPEND r_w401. CLEAR r_w401.
*    ENDCASE.
*
*    MOVE : zthr_pcp02-zval1 TO it_wage-val1,
*           zthr_pcp02-zval2 TO it_wage-val2,
*           zthr_pcp02-zval3 TO it_wage-val3.
*    APPEND it_wage . CLEAR it_wage.
*
*  ENDSELECT.
*... test data
*  g_this_year_fr = '20030101'.
*  g_this_year_to = '20031231'.
ENDFORM.                    " get_wage_types
*&---------------------------------------------------------------------*
*&      Form  get_ee_org
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ee_org.
  CLEAR : r_persg[], r_persg.

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_pa0000
*    FROM pa0000
*    WHERE pernr = pa0001-pernr
*      AND stat2 <> '0'.  "Withdraw

*Except Retire...
  r_persg-sign   = 'I'.
  r_persg-option = 'EQ'.
  r_persg-low    = '1'.  APPEND r_persg.
  r_persg-low    = '9'.  APPEND r_persg.

  CLEAR pa0001.
  SELECT pernr werks persg persk kostl stell abkrs
    INTO (pa0001-pernr, pa0001-werks, pa0001-persg,
          pa0001-persk, pa0001-kostl, pa0001-stell,
          pa0001-abkrs)
    FROM pa0001 WHERE endda = '99991231'
                  AND pernr IN r_pernr
                  AND kostl IN r_kostl
                  AND persg IN r_persg
                  AND PERSG in s_persg
                  AND PERSK in s_persk.

    MOVE-CORRESPONDING pa0001 TO i_pa0001.

    PERFORM get_emp_categ  USING    pa0001-persg pa0001-persk
                           CHANGING i_pa0001-categ.

    APPEND i_pa0001.
  ENDSELECT.

  data: l_massn like pa0000-massn.
  loop at i_pa0001.
    select * from pa0000
      where pernr = i_pa0001-pernr
        and begda <= p_datum
      order by begda descending.
      exit.
    endselect.
    if pa0000-stat2 = '0'.
*    l_MASSN   = pa0000-MASSN.
*    if l_massn in s_massn2.
**    if  ( L_MASSN = 'Z5' or l_MASSN = 'Z7'
**       or l_MASSN = 'Z8' or l_MASSN = 'ZE' or l_MASSN = 'ZI' ).

      delete i_pa0001 index sy-tabix.
    endif.
  endloop.

  SELECT * INTO TABLE i_jobcd
     FROM ztco_jc.

*  SELECT zval1 zval2 INTO (zthr_pcp02-zval1, zthr_pcp02-zval2)
*    FROM zthr_pcp02 WHERE zmodl = '02'
*                      AND zgrup = '1230' .
*    r_persg-low = zthr_pcp02-zval1.
*    APPEND r_persg. CLEAR r_persg.
*  ENDSELECT.

ENDFORM.                    " get_ee_org
*&---------------------------------------------------------------------*
*&      Form  JOB_CODE_SELECTION
*&---------------------------------------------------------------------*
FORM job_code_selection_check.

  CLEAR :  r_jobcode[],  r_jobcode.
  r_jobcode-sign   = 'I'.
  r_jobcode-option = 'EQ'.

  r_jobcode-low = '90002147'.  APPEND r_jobcode. "President & CEO
  r_jobcode-low = '90000661'.  APPEND r_jobcode. "Executive Vice Presi
  r_jobcode-low = '90000659'.  APPEND r_jobcode. "Vice President
  r_jobcode-low = '90000658'.  APPEND r_jobcode. "Senior Director
  r_jobcode-low = '90000040'.  APPEND r_jobcode. "Director
  r_jobcode-low = '90000657'.  APPEND r_jobcode. "Senior Manager
  r_jobcode-low = '10001145'.  APPEND r_jobcode. "Project Leader
  r_jobcode-low = '90000269'.  APPEND r_jobcode. "Manager
  r_jobcode-low = '10001144'.  APPEND r_jobcode. "Project Coordinator
  r_jobcode-low = '90000656'.  APPEND r_jobcode. "Assistant Manager
  r_jobcode-low = '10001143'.  APPEND r_jobcode. "Senior Consultant
  r_jobcode-low = '90000312'.  APPEND r_jobcode. "Specialist-Exempt
  r_jobcode-low = '90000047'.  APPEND r_jobcode. "Engineer-Exempt
  r_jobcode-low = '10001142'.  APPEND r_jobcode. "Consultant
  r_jobcode-low = '10001141'.  APPEND r_jobcode. "Technical Support
  r_jobcode-low = '90000039'.  APPEND r_jobcode. "Assistant
  r_jobcode-low = '10000968'.  APPEND r_jobcode. "Group Leader


ENDFORM.                    " JOB_CODE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  JOB_Paid-Leave-Up-to-Manager
*&---------------------------------------------------------------------*
FORM job_paid-leave-up-to-manager.
  CLEAR :  r_jobpaid[],  r_jobpaid.

  r_jobpaid-sign   = 'I'.
  r_jobpaid-option = 'EQ'.
  r_jobpaid-low = '90002147'. APPEND r_jobpaid. "President & CEO
  r_jobpaid-low = '90000661'. APPEND r_jobpaid. "Executive Vice Presiden
  r_jobpaid-low = '90000659'. APPEND r_jobpaid. "Vice President
  r_jobpaid-low = '90000658'. APPEND r_jobpaid. "Senior Director
  r_jobpaid-low = '90000040'. APPEND r_jobpaid. "Director
  r_jobpaid-low = '90000657'. APPEND r_jobpaid. "Senior Manager
  r_jobpaid-low = '10001145'. APPEND r_jobpaid. "Project Leader
ENDFORM.                    " JOB_Paid-Leave-Up-to-Manager
*&---------------------------------------------------------------------*
*&      Form  new_costcenter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form new_costcenter tables lt_pcp00 structure it_pcp00.
*  data : lt_pcp02 like zthr_pcp02 ,
*         lt_pcp00_temp like lt_pcp00 occurs 0 with header line,
*         wt_pcp00 like lt_pcp00.
*  data : f_num(2) type n.
*
*  clear :lt_pcp02,lt_pcp00.
*  clear f_num.
*
*  select * into  lt_pcp02  from zthr_pcp02
*    where zmodl eq '02'
*      and zgrup eq '1260'.
*    if sy-subrc = 0.
*      condense lt_pcp02-zrmrk no-gaps.
*      f_num = 1.
*      do 40 times.
*        search lt_pcp02-zrmrk for f_num.
*        if sy-subrc = 0.
*          move f_num to  lt_pcp00-zmons.
*          perform new_data_append tables lt_pcp00
*                                  using  lt_pcp02 p_zyear p_vern.
*          f_num = f_num + 1.
*        else.
*          f_num = f_num + 1.
*        endif.
*      enddo.
*    endif.
*  endselect.
*
*  lt_pcp00_temp[] = lt_pcp00[].
*  refresh lt_pcp00.
*  loop at lt_pcp00_temp.
*    move-corresponding lt_pcp00_temp to wt_pcp00.
*    read table lt_pcp00 with key zyear = wt_pcp00-zyear
*                                 zmons = wt_pcp00-zmons
*                                 zvern = wt_pcp00-zvern
*                                 zcost = wt_pcp00-zcost
**                                zpera = wt_pcp00-zpera
*                                 zperg = wt_pcp00-zperg
*                                 zsubg = wt_pcp00-zsubg
*                                 zobjc = wt_pcp00-zobjc
*                                 zsnrt = wt_pcp00-zsnrt.
*    if sy-subrc <> 0.
*      insert wt_pcp00 into table lt_pcp00.
*    else.
*      wt_pcp00-zhedc =  wt_pcp00-zhedc + lt_pcp00-zhedc.
*      lt_pcp00-zhedc =  wt_pcp00-zhedc.
*
*      modify lt_pcp00 transporting  zhedc
*       where zyear = wt_pcp00-zyear
*         and zmons = wt_pcp00-zmons
*         and zvern = wt_pcp00-zvern
*         and zcost = wt_pcp00-zcost
*         and zpera = wt_pcp00-zpera
*         and zperg = wt_pcp00-zperg
*         and zsubg = wt_pcp00-zsubg
*         and zobjc = wt_pcp00-zobjc
*         and zsenr = wt_pcp00-zsenr.
*    endif.
*  endloop.
*
*endform.                    " new_costcenter
*&---------------------------------------------------------------------*
*&      Form  new_data_append
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form new_data_append tables lt_pcp00 structure it_pcp00
*                     using  lt_pcp02 like zthr_pcp02
*                            p_zyear like p_zyear
*                            p_vern  like p_vern.
*
*  data : l_act01  like it_pcp00-act01,
*         l_mthly  like it_pcp00-mthly,
*         l_hramt  like it_pcp00-hramt,
*         l_ancur  like it_pcp00-ancur value 'USD',
*         l_header like  lt_pcp00-zhedc.
*
*  clear l_header.
*  lt_pcp00-zyear = p_zyear.
*  lt_pcp00-zvern = p_vern.
*  lt_pcp00-zcost = lt_pcp02-zctxt.
*  lt_pcp00-zpera = lt_pcp02-zval2(4).
*  lt_pcp00-zperg = lt_pcp02-zval2+5(1).
*  lt_pcp00-zsubg = lt_pcp02-zval2+7(2).
*  lt_pcp00-zobjc = lt_pcp02-zval4.
*  lt_pcp00-zsenr = '1'.
*  lt_pcp00-zhedc = lt_pcp02-zval2+10(2).
*
**FIXME
**
**President & CEO            27601.71  221916.00
**Executive Vice President   25258.47  215433.00
**Vice President             21390.60  182844.00
**Senior Director            14539.46  125470.00
**Director                   8509.40   076821.00
**Senior Manager             7830.30   079651.40
**Manager                    6692.74   066366.56
**Director                   18377.72  170818.10
**Senior Manager             10781.82  299907.75
**Project Leader             14187.66  160000.00
**Manager                    8816.75   088334.12
**Project Coordinator        10278.95  115000.00
**Assistant Manager          5633.33   070167.38
**Senior Consultant          5593.52   075442.86
**Specialist-Exempt          4746.01   047602.67
**Engineer-Exempt            4746.01   047602.67
**Consultant                 4530.07   058166.67
**Technical Support          3250.00   035204.08
**Assistant                  2367.42   027346.15
**Group Leader               4671.33   055880.00
**Team Leader                2628.89   036400.00
**Maintenance Team Member    3521.04   039104.00
**Production Team Member     2572.74   029577.60
*
**  CLEAR : zthr_pcp02.
**  SELECT SINGLE zval1 zval5 INTO
**              (zthr_pcp02-zval1, zthr_pcp02-zval5)
**    FROM zthr_pcp02 WHERE zmodl = '02'
**                      AND zgrup = '1250'
**                      AND zval2 = lt_pcp00-zperg
**                      AND zval4 = lt_pcp00-zobjc.
**
**  MOVE : zthr_pcp02-zval1 TO    l_act01,    " monthly
**         zthr_pcp02-zval5 TO    l_mthly .   " annaly
*
*  l_act01 = l_act01 .
*  l_hramt = l_mthly / 2080  .
*
*  move : l_hramt to lt_pcp00-hramt ,
*         l_act01 to lt_pcp00-mthly ,
*         l_act01 to lt_pcp00-omthly ,
*         l_ancur to lt_pcp00-ancur.
*  lt_pcp00-ansal =   l_mthly.
*
*  if lt_pcp00-zperg = '9' and lt_pcp00-zsubg = 'U2'.
*    lt_pcp00-act01 = lt_pcp00-mthly * lt_pcp00-zhedc .
*  elseif ( ( lt_pcp00-zperg = '1' and lt_pcp00-zsubg = 'U2' ) or
*           ( lt_pcp00-zperg = '1' and lt_pcp00-zsubg = 'U3' ) ).
*    lt_pcp00-act02 = lt_pcp00-mthly * lt_pcp00-zhedc .
*  else.
*    lt_pcp00-act03 = lt_pcp00-mthly * lt_pcp00-zhedc .
*  endif.
*
*  lt_pcp00-erdat = sy-datum.
*  get time.
*  lt_pcp00-erzet = sy-uzeit.
*  lt_pcp00-ernam = sy-uname.
*  append lt_pcp00.clear lt_pcp00.
*
*endform.                    " new_data_append
*&---------------------------------------------------------------------*
*&      Form  original_hire_date
*&---------------------------------------------------------------------*
FORM original_hire_date  USING f_begdt LIKE sy-datum.
  DATA : l_end      LIKE sy-datum,
         l_no_year  TYPE i,
         l_no_month TYPE i,
         l_begin   LIKE sy-datum.
  DATA : l_entry   LIKE hida OCCURS 1 WITH HEADER LINE.
  DATA : l_code LIKE p0041-dar01,
         l_date LIKE p0041-dat01.

  TABLES: pa0041.

  CLEAR: l_begin.
  CALL FUNCTION 'HR_ENTRY_DATE'
       EXPORTING
            persnr      = pa0001-pernr
       IMPORTING
            entrydate   = f_begdt
       TABLES
            entry_dates = l_entry.

  l_begin = f_begdt.

*Check Original Hire Date
  SELECT SINGLE *
                FROM pa0041
                WHERE pernr = pa0001-pernr
                  AND endda = '99991231' .

  DO 12 TIMES VARYING l_code FROM pa0041-dar01 NEXT pa0041-dar02
              VARYING l_date FROM pa0041-dat01 NEXT pa0041-dat02 .
    IF l_code = 'Z1'.
      l_begin = l_date.
      EXIT.
    ENDIF.
  ENDDO.

*  CONCATENATE p_xyear '1231' INTO l_end.
  l_end = sy-datum.
  l_end+6(2) = '15'.

* Defferience day mont year
  CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
       EXPORTING
            beg_da   = l_begin
            end_da   = l_end
       IMPORTING
            no_year  = l_no_year
            no_month = l_no_month.

  IF i_pa0001-categ = c_eg2.
    it_cpy00-zsnrt = l_no_year * 12 + l_no_month.
    IF it_cpy00-zsnrt >= p_max.
      it_cpy00-zsnrt = p_max.
    ENDIF.

    IF it_cpy00-zsnrt = 0.
      it_cpy00-zsnrt = 1.
    ENDIF.
  ELSE.
    it_cpy00-zsnrt = 0.
  ENDIF.
ENDFORM.                    " original_hire_date
*&---------------------------------------------------------------------*
*&      Form  PAY_INCRESE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pay_increse_rate.
* increase rate payment
*
  DATA : l_zval1 TYPE p DECIMALS 2.
  CLEAR : l_zval1, w_inrate.

* Junuary to December for Salaried

  w_inrate = '0.04'.

*  CLEAR zthr_pcp02.
*  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
*    FROM zthr_pcp02 WHERE zmodl = '2'
*                      AND zgrup = '1060'
*                      AND zcode = '10020'.
*  MOVE zthr_pcp02-zval1 TO l_zval1.
*  IF l_zval1 < 1.
*    l_zval1 = l_zval1 + 1.
*  ENDIF.
*
*  MOVE : l_zval1 TO w_inrate.

ENDFORM.                    " PAY_INCRESE_RATE
*&---------------------------------------------------------------------*
*&      Form  POP_UP_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pop_up_message  USING p_text
                     CHANGING p_answer.
  DATA : answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1 = p_text
            textline2 = p_text
            titel     = 'Personal cost plan Confirm Box'
       IMPORTING
            answer    = answer.
  MOVE : answer TO p_answer .
ENDFORM.                    " POP_UP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  read_payroll_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T500L_RELID  text
*      -->P_PA0001_PERNR  text
*      -->P_LS_RGDIR_SEQNR  text
*----------------------------------------------------------------------*
FORM read_payroll_result USING    p_relid
                                  p_pernr
                                  p_seqnr
                                  p_srtza
                                  p_fpper
                                  p_fpbeg.

  DATA : lt_result TYPE pay99_result.
  DATA : ls_rt TYPE pc207.
  DATA : ls_crt LIKE LINE OF lt_result-inter-crt.

*  pay result get
  CLEAR : lt_result.

  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
       EXPORTING
            clusterid                    = p_relid
            employeenumber               = p_pernr
            sequencenumber               = p_seqnr
            read_only_international      = 'X'
       CHANGING
            payroll_result               = lt_result
       EXCEPTIONS
            illegal_isocode_or_clusterid = 1
            error_generating_import      = 2
            import_mismatch_error        = 3
            subpool_dir_full             = 4
            no_read_authority            = 5
            no_record_found              = 6
            versions_do_not_match        = 7
            OTHERS                       = 8.

*... G/L account - 601100
  IF i_pa0001-categ = c_eg3.
    IF p_srtza EQ 'A'.
      w_wrkmo = w_wrkmo + 1.
    ENDIF.

    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_month.
      READ TABLE it_wage WITH KEY val1 = ls_rt-lgart
                                  val2 = '601100' .
      CHECK it_wage-val3 > p_fpbeg .

      w_betrg = w_betrg + ls_rt-betrg.

*A	current result
*P	previous result
*O	old result
      IF p_srtza EQ 'A'.
        MOVE :         ls_rt-betrg    TO it_fpper-betrg.
      ELSE.
        ls_rt-betrg = ls_rt-betrg * -1.
      ENDIF.
      MOVE : p_fpper        TO it_fpper-fpper,
             ls_rt-betrg    TO it_fpper-xetrg.
      COLLECT it_fpper. CLEAR it_fpper.

    ENDLOOP.

*... G/L account - 601110
  ELSEIF i_pa0001-categ = c_eg1.

    IF p_srtza EQ 'A'.
      w_wrkmo = w_wrkmo + 1.
    ENDIF.

*    total wage amount
    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_wekly.

      READ TABLE it_wage WITH KEY val1 = ls_rt-lgart
                                  val2 = '601110' .
      CHECK it_wage-val3 > p_fpbeg .

      w_betrg = w_betrg + wa_reslt-betrg.

      IF p_srtza EQ 'A'.
        MOVE :         ls_rt-betrg    TO it_fpper-betrg.
      ELSE.
        ls_rt-betrg = ls_rt-betrg * -1.
      ENDIF.
      MOVE : p_fpper        TO it_fpper-fpper,
             ls_rt-betrg    TO it_fpper-xetrg.
      COLLECT it_fpper. CLEAR it_fpper.

    ENDLOOP.
*Gross amount
*    LOOP AT lt_result-inter-crt INTO ls_crt WHERE lgart = '/102'.
    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart = '/102'.

      CHECK w_wrkmo <> 0 .
*      w_nhce =  ls_crt-betrg / w_wrkmo .
*      w_nhce = w_nhce * 26 .
      w_nhce = w_nhce + ls_rt-betrg .
    ENDLOOP.
*Actual amount
    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_w401.
      CHECK w_wrkmo <> 0 .
      READ TABLE lt_result-inter-rt INTO ls_rt WITH KEY  lgart = '/102'.
      IF sy-subrc = 0.
        w_anhce =  ls_rt-betrg .
        w_anhce = w_anhce + w_anhce .
      ENDIF.
    ENDLOOP.

*Actual 401k amount
    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_w401.
*      W_401K = W_401K + LS_RT-BETRG.
*      w_401k = w_401k + ABS( ls_rt-betrg ).
      w_401k = w_401k + ls_rt-betrg.
    ENDLOOP.

*... G/L account - 601120
  ELSE.
    IF p_srtza EQ 'A'.
      w_wrkmo = w_wrkmo + 1.
    ENDIF.

    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_hramt.

      READ TABLE it_wage WITH KEY val1 = ls_rt-lgart
                                  val2 = '601120' .
      CHECK it_wage-val3 > p_fpbeg .

      IF p_srtza EQ 'A'.
        MOVE :         ls_rt-betrg    TO it_fpper-betrg.
      ELSE.
        ls_rt-betrg = ls_rt-betrg * -1.
      ENDIF.
      MOVE : p_fpper        TO it_fpper-fpper,
             ls_rt-betrg    TO it_fpper-xetrg.
      COLLECT it_fpper. CLEAR it_fpper.

      w_betrg = w_betrg + wa_reslt-betrg.
    ENDLOOP.

    LOOP AT lt_result-inter-rt INTO ls_crt WHERE lgart = '/102'.
      CHECK w_wrkmo <> 0 .
*      w_nhce =  ls_crt-betrg / w_wrkmo .
*      w_nhce = w_nhce * 26 .
      w_nhce = w_nhce +  ls_rt-betrg .
    ENDLOOP.
*Actual amount
    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_w401.
      CHECK w_wrkmo <> 0 .
      READ TABLE lt_result-inter-rt INTO ls_rt WITH KEY  lgart = '/102'.
      IF sy-subrc = 0.
        w_anhce =  ls_rt-betrg .
        w_anhce = w_anhce + w_anhce .
      ENDIF.
    ENDLOOP.

    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_w401.
      w_401k = w_401k + ls_rt-betrg.
*      w_401k = w_401k + ABS( ls_rt-betrg ).
    ENDLOOP.

  ENDIF.
*... G/L account - 601200
  LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_bonus.
*    W_BONUS = W_BONUS + WA_RESLT-BETRG.
    w_bonus = w_bonus + ls_rt-betrg.
  ENDLOOP.


ENDFORM.                    " read_payroll_result
*&---------------------------------------------------------------------*
*&      Form  salery_retroactive
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salery_retroactive.
  DATA : lx LIKE it_fpper .

  CLEAR : lx.

  LOOP AT it_fpper .

    IF it_fpper-betrg <> it_fpper-xetrg .
      lx-xetrg = it_fpper-xetrg.
    ENDIF.
    lx-betrg  = lx-betrg + it_fpper-betrg - lx-xetrg.
    CLEAR lx-xetrg.

  ENDLOOP.

  w_betrg = lx-betrg.


ENDFORM.                    " salery_retroactive
*&---------------------------------------------------------------------*
*&      Form  get_hr_and_calculate
*&---------------------------------------------------------------------*
FORM get_hr_and_calculate.
  DATA: l_begdt   LIKE sy-datum.

  LOOP AT i_pa0001.
    CLEAR pa0001.
    MOVE-CORRESPONDING i_pa0001 TO pa0001.

* check termination
    READ TABLE i_pa0000 WITH KEY pernr = pa0001-pernr.
    CHECK sy-subrc NE 0.

    it_cpy00-bukrs = p_bukrs.
    concatenate p_datum(4) p_datum+4(2) into it_cpy00-yyyymm.

*   it_cpy00-gjahr = p_datum(4).
*   it_cpy00-month = p_monat.
*   it_cpy00-zvern = p_vern.
*   it_cpy00-zpera = pa0001-werks.
    it_cpy00-kostl = pa0001-kostl.
*    it_cpy00-persg = pa0001-persg. "Employee Group
*    it_cpy00-persk = pa0001-persk. "Employee Subgroup
    CONCATENATE 'E'  i_pa0001-categ INTO  it_cpy00-zkey.

* be hired Date
    PERFORM original_hire_date USING l_begdt.

* Headcount add
    it_cpy00-pranz = 1.

* get hourly, salary...
    PERFORM get_wage_master  USING it_cpy00-maxbt.

* get job code
    READ TABLE i_jobcd WITH KEY stell = i_pa0001-stell
                                trfgr = pa0008-trfgr.
    IF sy-subrc <> 0.
      READ TABLE i_jobcd WITH KEY stell = i_pa0001-stell.
    ENDIF.
    IF sy-subrc = 0.
      it_cpy00-jobcd = i_jobcd-jobcd.
    ELSE.
      it_cpy00-jobcd = 'XX'.
    ENDIF.

* Account number (601100;Salary - Korean)
* Account number (601110;Salary - Local)
* Account number (601120;Wage - Biweekly)
*ACTUAL...???
* PERFORM calcurate_monthly_salary_zz USING    t_begda.
    PERFORM append_pernr_info_table USING l_begdt.

    COLLECT it_cpy00. CLEAR it_cpy00.
  ENDLOOP.

ENDFORM.                    " get_hr_and_calculate
*&---------------------------------------------------------------------*
*&      Form  calc_avg_wage
*&---------------------------------------------------------------------*
FORM calc_avg_wage.
  DATA: l_pranz LIKE itab-pranz.

  LOOP AT it_cpy00.
*    move-corresponding it_cpy00 to it_pcp00.
*    it_pcp00-maxbt = it_cpy00-maxbt / it_cpy00-pranz.
*    append it_pcp00.
*    move-corresponding it_pcp00 to itab.

    l_pranz = it_cpy00-maxbt / it_cpy00-pranz.
    MOVE-CORRESPONDING it_cpy00 TO itab.
*   itab-month = p_datum+4(2).

    itab-zpitm = p_pitm1.
    APPEND itab.

* except wage worker...
    IF itab-zkey <> 'EB'.
      itab-pranz = l_pranz.
      itab-zpitm = p_pitm2.
      APPEND itab.
    ENDIF.

    CLEAR: it_pcp00, itab.
  ENDLOOP.

***10	H1	H4	1	MAINTMLD	01		12/31/9999
*	03/28/2005
**	20.12
***10	H1	H4	1	MAINTMLD	02		12/31/9999
*	03/28/2005
**	21.54
***10	H1	H4	1	MAINTMLD	03		12/31/9999
*	03/28/2005
**	22.96
***10	H1	H4	1	MAINTMLD	04		12/31/9999
*	03/28/2005
**	24.38
***10	H1	H4	1	MAINTMLD	05		12/31/9999
*	03/28/2005
**	25.79
***10	H1	H4	1	PRODTMLD	01		12/31/9999
*	03/28/2005
**	15.46
***10	H1	H4	1	PRODTMLD	02		12/31/9999
*	03/28/2005
**	17.27
***10	H1	H4	1	PRODTMLD	03		12/31/9999
*	03/28/2005
**	19.08
***10	H1	H4	1	PRODTMLD	04		12/31/9999
*	03/28/2005
**	20.89
***10	H1	H4	1	PRODTMLD	05		12/31/9999
*	03/28/2005
**	22.70
***10	H1	H5	1	MAINTMMB	01		12/31/9999
*	03/28/2005
**	19.12
***10	H1	H5	1	MAINTMMB	02		12/31/9999
*	03/28/2005
**	20.54
***10	H1	H5	1	MAINTMMB	03		12/31/9999
*	03/28/2005
**	21.96
***10	H1	H5	1	MAINTMMB	04		12/31/9999
*	03/28/2005
**	23.38
***10	H1	H5	1	MAINTMMB	05		12/31/9999
*	03/28/2005
**	24.79
***10	H1	H5	1	PRODTMMB	01		12/31/9999
*	03/28/2005
**	14.46
***10	H1	H5	1	PRODTMMB	02		12/31/9999
*	03/28/2005
**	16.27
***10	H1	H5	1	PRODTMMB	03		12/31/9999
*	03/28/2005
**	18.08
***10	H1	H5	1	PRODTMMB	04		12/31/9999
*	03/28/2005
**	19.89
***10	H1	H5	1	PRODTMMB	05		12/31/9999
*	03/28/2005
**	21.70

  itab-bukrs = p_bukrs.
  itab-yyyymm = p_datum(6).
*  itab-gjahr = p_datum(4).
*  itab-month = p_datum+4(2).
  SELECT * FROM t510
     WHERE molga = t500p-molga
       AND trfar = 'H1'
       AND trfgb IN ('H4', 'H5')
       AND endda = '99991231'.

*    itab-maxbt = t510-betrg.    "Hourly
    itab-pranz = t510-betrg.    "Hourly

* 01~05 level * by 5 month
    itab-zsnrt = t510-trfst. "p_inc.                        "01~05
    itab-zpitm = p_pitm2.

    READ TABLE i_jobcd WITH KEY trfgr = t510-trfgr.
    IF sy-subrc = 0.
      itab-jobcd = i_jobcd-jobcd.
      APPEND itab.
    ENDIF.

  ENDSELECT.
ENDFORM.                    " calc_avg_wage
*&---------------------------------------------------------------------*
*& form get_hr_gl_mapping.
*&---------------------------------------------------------------------*
FORM get_hr_gl_mapping.
*RPDKON00
  DATA:
    coll_tab_copy LIKE coll_tab OCCURS 1 WITH HEADER LINE.

* source of informations about financial accounts       "XMU_2001/02/23
 DATA:                                                                 "
   BEGIN OF bukrs_dest_tab OCCURS 1,                                   "
     bukrs LIKE hrca_company-comp_code.                                "
     INCLUDE STRUCTURE bdbapidest.                                     "
 DATA: END OF bukrs_dest_tab.                                          "

  TABLES:
   t500l,
*  t500p,
    t500t,
    t52ek,
    t52ekt,
    t52ep,
    t52ez,
    t52el,
    t52em,
    t52emt,
    t512t,
    t5d7o,
    t5d7c,
    t5d7e,
    t5d7d,
    t5d7f,
    hrca_company.

* select countries
  SELECT * FROM t500l WHERE intca = 'US'.  "molga in p_molga.
    coll_tab-molga = t500l-molga.
    APPEND coll_tab.
  ENDSELECT.

* Merge wage type
  CLEAR coll_tab_copy. REFRESH coll_tab_copy.
  LOOP AT coll_tab.
    SELECT * FROM t52ez WHERE molga = coll_tab-molga
*                       and lgart in p_lgart                "
                        AND begda LE sy-datum
                        AND endda GE sy-datum.
      MOVE-CORRESPONDING coll_tab TO coll_tab_copy.
      coll_tab_copy-lgart = t52ez-lgart.
      SELECT * FROM t52el WHERE molga = coll_tab-molga
                          AND lgart = t52ez-lgart
*                         AND SPPRC = SPACE               "SH_AHRK062283
                          AND endda = t52ez-endda.
        coll_tab_copy-seqno = t52el-seqno.
        coll_tab_copy-symko = t52el-symko.
        coll_tab_copy-sign  = t52el-sign.
        coll_tab_copy-spprc = t52el-spprc.               "SH_AHRK048504

*       read KOART
        SELECT SINGLE * FROM t52ek WHERE symko = t52el-symko.
        IF sy-subrc EQ 0.
*         KOART
          coll_tab_copy-koart = t52ek-koart.
*         read process
          SELECT SINGLE * FROM t52ep WHERE koart = t52ek-koart.
          IF sy-subrc EQ 0.
            coll_tab_copy-process+0(2) = 'HR'.
            coll_tab_copy-process+2(1) = t52ep-kttyp.
          ELSE.
            coll_tab_copy-process = '???'(999).
          ENDIF.
          IF t52ek-u_momag = 'X'.
*           dependent on MOMAG
            SELECT * FROM t52em.
              coll_tab_copy-momag = t52em-momag.
              APPEND coll_tab_copy.
            ENDSELECT.
            IF sy-subrc NE 0.
*             dependent on MOMAGs but there are no MOMAGs
              CLEAR coll_tab_copy-momag.
              APPEND coll_tab_copy.
            ENDIF.
          ELSE.
*         independent on MOMAG
            CLEAR coll_tab_copy-momag.
            APPEND coll_tab_copy.
          ENDIF.
        ELSE.
*         symbolic account = SPACE or not available
          CLEAR coll_tab_copy-momag.
          APPEND coll_tab_copy.
        ENDIF.
      ENDSELECT.
      IF sy-subrc NE 0.
*       wage type is in T52EZ but no entries in T52EL
        CLEAR coll_tab_copy-symko.
        CLEAR coll_tab_copy-momag.
        APPEND coll_tab_copy.
      ENDIF.
    ENDSELECT.
    IF sy-subrc NE 0.
*     wage type is not in T52EZ --> not to be posted at all
      APPEND coll_tab TO coll_tab_copy.                  "XMU2001/10/19
*     CLEAR COLL_TAB_COPY-SYMKO.                         "XMU2001/10/19
*     CLEAR COLL_TAB_COPY-MOMAG.                         "XMU2001/10/19
*     APPEND COLL_TAB_COPY.                              "XMU2001/10/19
    ENDIF.
  ENDLOOP.
  CLEAR coll_tab. REFRESH coll_tab.
  coll_tab[] = coll_tab_copy[].

* Step 2
  DATA:
    BEGIN OF bukrs_tab OCCURS 1,
      molga LIKE t500l-molga,
      bukrs LIKE hrca_company-comp_code,
    END OF bukrs_tab.

* create BUKRS_TAB
  SELECT * FROM t500l.
    SELECT * FROM t500p WHERE molga = t500l-molga
                        AND   bukrs = p_bukrs.
      bukrs_tab-molga = t500p-molga.
      bukrs_tab-bukrs = t500p-bukrs.
      COLLECT bukrs_tab.
      bukrs_dest_tab-bukrs = t500p-bukrs.               "XMU_2001/02/23
      COLLECT bukrs_dest_tab.                           "XMU_2001/02/23
    ENDSELECT.
  ENDSELECT.

* Merge BUKRS
  CLEAR coll_tab_copy. REFRESH coll_tab_copy.
  LOOP AT coll_tab.
    MOVE-CORRESPONDING coll_tab TO coll_tab_copy.
    IF NOT coll_tab_copy-symko IS INITIAL.                "HB_AHRK027439
      LOOP AT bukrs_tab WHERE molga = coll_tab-molga.
        coll_tab_copy-bukrs = bukrs_tab-bukrs.
        APPEND coll_tab_copy.
      ENDLOOP.
    ELSE. "symko is initial -> don't read ccodes          "HB_AHRK027439
      APPEND coll_tab_copy.            "HB_AHRK027439
    ENDIF.                             "HB_AHRK027439
  ENDLOOP.
  CLEAR coll_tab. REFRESH coll_tab.
  coll_tab[] = coll_tab_copy[].


* Step 3
  DATA:
    l_modif LIKE hrpp_acct_det-add_modif,
    debit   LIKE acct_det_bf-gl_account,
    credit  LIKE acct_det_bf-gl_account.
  CLEAR coll_tab_copy. REFRESH coll_tab_copy.
  LOOP AT coll_tab.
    MOVE-CORRESPONDING coll_tab TO coll_tab_copy.
    IF NOT coll_tab_copy-symko IS INITIAL.                "HB_AHRK027439

      l_modif+0(2) = coll_tab-dart.
      l_modif+2(2) = coll_tab-udart.

      PERFORM get_account USING coll_tab-bukrs coll_tab-process
                                coll_tab-symko coll_tab-momag
                                l_modif debit credit.
      IF debit = credit.
        coll_tab_copy-shkz = space.
        coll_tab_copy-acct = debit.
        APPEND coll_tab_copy.
      ELSE.
        coll_tab_copy-shkz = 'S'(008).
        coll_tab_copy-acct = debit.
        APPEND coll_tab_copy.
        coll_tab_copy-shkz = 'H'(009).
        coll_tab_copy-acct = credit.
        APPEND coll_tab_copy.
      ENDIF.
    ELSE. "symko is initial -> don't read account         "HB_AHRK027439
      APPEND coll_tab_copy.            "HB_AHRK027439
    ENDIF.                             "HB_AHRK027439
  ENDLOOP.
  CLEAR coll_tab. REFRESH coll_tab.
  coll_tab[] = coll_tab_copy[].

ENDFORM.
*----------------------------------------------------------------------
*       FORM GET_ACCOUNT
*----------------------------------------------------------------------
FORM get_account USING value(companycode) LIKE acct_det_bf-comp_code
                       value(process) LIKE acct_det_bf-process
                       value(symb_acct) LIKE hrpp_acct_det-symb_acct
                       value(eg_acct_det) LIKE hrpp_acct_det-eg_acct_det
                       value(add_modif) LIKE hrpp_acct_det-add_modif
                       account_debit LIKE acct_det_bf-gl_account
                       account_credit LIKE acct_det_bf-gl_account.
  STATICS: BEGIN OF account_table OCCURS 10,
             companycode LIKE companycode,
             process LIKE process,
             symb_acct LIKE symb_acct,
             eg_acct_det LIKE eg_acct_det,
             add_modif LIKE add_modif,
             account_debit LIKE account_debit,
             account_credit LIKE account_credit,
           END OF account_table.
  DATA return_tab LIKE bapireturn1 OCCURS 0 WITH HEADER LINE.

  IF NOT symb_acct IS INITIAL.
    CLEAR account_table.
    account_table-companycode = companycode.
    account_table-process = process.
    account_table-symb_acct = symb_acct.
    account_table-eg_acct_det = eg_acct_det.
    account_table-add_modif = add_modif.
    READ TABLE account_table.
    IF sy-subrc <> 0.
      CALL FUNCTION 'HRPP_FI_ACCT_DET_HR'
           EXPORTING
                companycode       = companycode
                process           = process
                symb_acct         = symb_acct
                eg_acct_det       = eg_acct_det
                add_modif         = add_modif
           IMPORTING
                gl_account_debit  = account_table-account_debit
                gl_account_credit = account_table-account_credit
           TABLES
                return_tab        = return_tab.
*   reject if return_tab holds at least 1 error message
      READ TABLE return_tab WITH KEY type = 'E'.
      IF sy-subrc = 0.
        account_table-account_debit = '???'(999).
        account_table-account_credit = '???'(999).
      ELSE.
        IF account_table-account_debit = space.
          account_table-account_debit = '???'(999).
        ENDIF.
        IF account_table-account_credit = space.
          account_table-account_credit = '???'(999).
        ENDIF.
      ENDIF.
      APPEND account_table.
    ENDIF.
    account_debit = account_table-account_debit.
    account_credit = account_table-account_credit.
  ELSE.
*   in this case the buffering doesn't work in most situations
*   and returns a SUBRC = 0 e.g. when all fields are empty
    account_debit = '???'(999).
    account_credit = '???'(999).
  ENDIF.
ENDFORM.                               " GET_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
FORM show_progress USING    value(p_val).
  DATA: l_str(3) TYPE c.
  l_str = p_val.
  CONCATENATE l_str '% processing...........' INTO w_ptext.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = p_val
            text       = w_ptext.

ENDFORM.                    " show_progress
*&---------------------------------------------------------------------*
*&      Form  CALCURATE_MONTHLY_SALARY_zz
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_l_begdt  text
*----------------------------------------------------------------------*
FORM calcurate_monthly_salary_zz USING    t_begda.

  DATA : wa_diffday TYPE i.
  DATA : l_zval1     TYPE p DECIMALS 2.
  DATA : l_bdate TYPE d,
         l_edate TYPE d.

  DATA : ov_molga TYPE molga,
         it_rgdir TYPE hrpy_tt_rgdir.
  DATA : ls_rgdir LIKE LINE OF it_rgdir.
  DATA : lt_result TYPE pay99_result.
  DATA : ls_rt TYPE pc207.
  CLEAR:it_payrt.

****  pay result .
  CALL FUNCTION 'CA_CU_READ_RGDIR_NEW'
       EXPORTING
            persnr          = pa0001-pernr
       IMPORTING
            molga           = ov_molga
       TABLES
            cu_ca_rgdir     = it_rgdir
       EXCEPTIONS
            no_record_found = 1
            OTHERS          = 2.

  DELETE it_rgdir WHERE
    NOT ( fpend BETWEEN g_this_year_fr and g_this_year_to ) OR
    payty = 'A'.

* Personnel Country Grouping
  SELECT SINGLE relid INTO t500l-relid
                FROM t500l
                WHERE molga = ov_molga.


*
  CLEAR: w_betrg, w_wrkmo, g_hramt, w_bonus, w_401k ,
         w_total , g_yramt, w_nhce,w_anhce.
  CLEAR: l_bdate , l_edate.
  CLEAR : it_fpper, it_fpper[].

  LOOP AT it_rgdir INTO ls_rgdir.
    IF ls_rgdir-fpbeg <  t_begda AND  " middle entering
       ls_rgdir-fpend >  t_begda.
      CONTINUE.
    ELSEIF ls_rgdir-fpbeg < g_this_year_to AND
           ls_rgdir-fpbeg > g_this_year_to.
      CONTINUE.
    ELSEIF ls_rgdir-srtza EQ 'P'.
      CONTINUE.
    ELSE.

      CHECK ls_rgdir-fpbeg >= g_this_year_fr .
      PERFORM begin_end_day_cal USING  ls_rgdir-fpbeg
                                       ls_rgdir-fpend
                                CHANGING l_bdate
                                         l_edate .

      PERFORM read_payroll_result USING t500l-relid
                                        pa0001-pernr
                                        ls_rgdir-seqnr
                                        ls_rgdir-srtza
                                        ls_rgdir-fpper
                                        ls_rgdir-fpbeg.
    ENDIF.
  ENDLOOP.

  PERFORM salery_retroactive .
****** total amount caulation .


ENDFORM.                    " CALCURATE_MONTHLY_SALARY_zz
*&---------------------------------------------------------------------*
*&      Form  data_download
*&---------------------------------------------------------------------*
FORM data_download.
  DATA: f_file LIKE rlgrap-filename.
  f_file = 'c:\temp\lab.txt'.


  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename = f_file
*           item     = f_file
            filetype = 'DAT'
            silent   = 'X'
            filetype_no_show = 'X'
       TABLES
            data_tab = itab.

  WRITE:/ f_file, ' is created...'.

ENDFORM.                    " data_download
*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
FORM display_out.

  PERFORM field_setting TABLES gt_fieldcat USING :
   'ZPITM'     'PLAN ITM'       '06' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'ZKEY'      'KEY'            '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'MONTH'     'MON'            '02' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'KOSTL'     'CC'             '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
*   'PERSG'     'EG'             '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
*   'PERSK'     'ESG'            '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'JOBCD'     'JC'             '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'ZSNRT'     'SN'             '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'PRANZ'     'QTY'            '15' ' ' 'L'  ' '  ' '  '  ' ' '  'X'.
*  'MAXBT'     'HRAMT'          '22' ' ' 'R'  ' '  ' '  '  ' ' '  'X'.

  g_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program      = g_repid
            i_callback_user_command = 'USER_COMMAND'
            it_fieldcat             = gt_fieldcat
            i_save                  = 'A'
       TABLES
            t_outtab                = itab
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.

ENDFORM.                    " display_out
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
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

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
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

  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  get_indirect_wage
*&---------------------------------------------------------------------*
FORM get_indirect_wage USING    f_pernr  f_hramt.
  DATA: indbw_end_out TYPE d.
  DATA: BEGIN OF tbindbw OCCURS 20,                         "QEAK70329
          seqnr(3).                        "sequence number
*        LGART LIKE P0008-LGA01,          "wage type         "QNOK062433
*        OPKEN LIKE P0008-OPK01,          "OPKEN from T511   "QNOK062433
**       betrg like p0008-bet01,          "value             "QNOK062433
*        INDBW(1),                        "indicator ind. eval."K062433
*        ANZHL LIKE P0008-ANZ01,          "amount            "QNOK062433
*        MODNA,                           "because of ANSAL  "QNOK062433
*        MOD01,                           "because of ANSAL  "QNOK062433
          INCLUDE STRUCTURE ptbindbw.                       "QNOK062433
  DATA: END OF tbindbw.

  TABLES: t511.
  SELECT SINGLE * FROM t511
    WHERE molga = t500p-molga
      AND lgart = pa0008-lga01
      AND endda = '99991231'.
  MOVE-CORRESPONDING t511 TO tbindbw.
  tbindbw-indbw = 'I'. "Indirect
  CLEAR: tbindbw-mod01.
  tbindbw-waers = 'USD'.
  APPEND tbindbw.

  TABLES: p0001, p0007, p0008, p0230, p0304.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF p0001
     FROM pa0001 WHERE pernr = pa0001-pernr
                   AND endda = '99991231'.
  p0001-infty = '0001'.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF p0007
     FROM pa0007 WHERE pernr = pa0001-pernr
                   AND endda = '99991231'.
  p0007-infty = '0007'.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF p0008
     FROM pa0008 WHERE pernr = pa0001-pernr
                   AND endda = '99991231'.
  p0008-infty = '0008'.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF p0230
     FROM pa0230 WHERE pernr = pa0001-pernr
                   AND endda = '99991231'.


  CALL FUNCTION 'RP_EVALUATE_INDIRECTLY_P0008'
       EXPORTING
            ptclas    = 'A' "Master data and time data (pspar-tclas)
            ppernr    = pa0001-pernr
            pmolga    = t500p-molga
            pbegda    = p_datum        "valuation date
            pp0001    = p0001
            pp0007    = p0007
            pp0008    = p0008
            pp0230    = p0230
*           pp0304    = p0304
       IMPORTING
            pendda    = indbw_end_out
       TABLES
            ptbindbw  = tbindbw
       EXCEPTIONS
            error_at_indirect_evaluation = 1.

  READ TABLE tbindbw INDEX 1.
  f_hramt = tbindbw-betrg.
ENDFORM.
* get_indirect_wage
*&---------------------------------------------------------------------*
*&      Form  get_emp_categ
*&---------------------------------------------------------------------*
FORM get_emp_categ USING    f_persg
                            f_persk
                   CHANGING f_categ.

*parameters: p_eg1(1)   type c default 'A' no-display,  "US-Salary
*            p_eg2(1)   type c default 'B' no-display,  "US-Wage
*            p_eg3(1)   type c default 'K' no-display.  "KR-Salary

  IF ( f_persg = '9' AND f_persk = 'U2') or
* by ig.moon 10/21/2009 {
     ( f_persg = '4' AND f_persk = 'UE').
* }
    f_categ = c_eg3.
  ELSEIF ( ( f_persg = '1' AND f_persk = 'U2' ) OR
           ( f_persg = '1' AND f_persk = 'U3' ) or
* by ig.moon 10/21/2009 {
           ( f_persg = '4' AND f_persk = 'UD' ) ).
* }
    f_categ = c_eg1.
  ELSE.
    f_categ = c_eg2.
  ENDIF.

ENDFORM.                    " get_emp_categ
