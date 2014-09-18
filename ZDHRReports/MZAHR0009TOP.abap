*&---------------------------------------------------------------------*
*& Include MZAHR0009TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM sapmzahr0009 MESSAGE-ID zmhr.
*...
TABLES: pa0001, pa0008, t510, zthr_pcpxx, zthr_pcp00,zthr_pcp02,
        zthr_pcp05,t500p, cskt,zshr_pcp00.

*... internal tables
DATA: it_pcp00 LIKE zthr_pcp00 OCCURS 0 WITH HEADER LINE,
      it_cpy00 LIKE zthr_pcp00 OCCURS 0 WITH HEADER LINE,
       it_pcpxx LIKE zthr_pcpxx OCCURS 0 WITH HEADER LINE.
DATA : it_pcp LIKE zshr_pcp00 OCCURS 0 .
DATA : it_del LIKE zthr_pcp02 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_calsl OCCURS 0,   " monthly salary
      persg    LIKE pa0001-persg,
      persk    LIKE pa0001-persk,
      kostl    LIKE pa0001-kostl,
      stell    LIKE pa0001-stell,
      zsenr    LIKE zthr_pcp00-zsenr,
      zhedc    LIKE zthr_pcp00-zhedc,
      zsaly    LIKE zthr_pcp00-zsaly,
      act01    LIKE zthr_pcp00-act01,
      act02    LIKE zthr_pcp00-act02,
      act03    LIKE zthr_pcp00-act03,
      houry    LIKE zthr_pcp00-houry,
      ansal    LIKE zthr_pcp00-ansal.
DATA: END OF it_calsl.

DATA : BEGIN OF it_code OCCURS 0,
       1300 LIKE zthr_pcp02-zval1,
       1301 LIKE zthr_pcp02-zval1,
       1302 LIKE zthr_pcp02-zval1,

       1320 LIKE zthr_pcp02-zval1,
       1321 LIKE zthr_pcp02-zval1,
       1322 LIKE zthr_pcp02-zval1,
       1330 LIKE zthr_pcp02-zval1,
       1340 LIKE zthr_pcp02-zval1,
       1350 LIKE zthr_pcp02-zval1,
       1360 LIKE zthr_pcp02-zval1,
       1370 LIKE zthr_pcp02-zval1,
       1380 LIKE zthr_pcp02-zval1,
       1390 LIKE zthr_pcp02-zval1,
       1400 LIKE zthr_pcp02-zval1,
       1401 LIKE zthr_pcp02-zval2,
       1410 LIKE zthr_pcp02-zval1,
       1411 LIKE zthr_pcp02-zval2,
       1430 LIKE zthr_pcp02-zval1,
       1500 LIKE zthr_pcp02-zval1,
       1501 LIKE zthr_pcp02-zval2,
       END OF it_code.

DATA: BEGIN OF it_1175 OCCURS 1,
      zcode LIKE zthr_pcp02-zcode,
      zval1 TYPE p DECIMALS 6,
      END OF it_1175.

DATA: BEGIN OF it_years OCCURS 1,
      zyear    LIKE zthr_pcp03-zyear.
DATA: END OF it_years.

DATA: BEGIN OF it_versn OCCURS 1,
      zvers    LIKE zthr_pcp03-zvers.
DATA: END OF it_versn.

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
        r_houry    FOR wa_reslt-lgart,
        r_bonus    FOR wa_reslt-lgart,
        r_jobcode  FOR pa0001-stell,
        r_paid     FOR pa0001-stell,
        r_mang     FOR zthr_pcp00-zobjc,
        r_w401     FOR wa_reslt-lgart,
        r_persg    FOR p0001-persg,
        r_otcode   FOR pa0001-stell,
        r_1170     FOR zthr_pcp00-zcost,
        r_1180     FOR zthr_pcp00-zobjc.

DATA : BEGIN OF it_wage OCCURS 0,
       val1 LIKE zthr_pcp02-zval1,
       val2 LIKE zthr_pcp02-zval2,
       val3 LIKE zthr_pcp02-zval3,
       END  OF it_wage.
*... variants
DATA: w_zyear       LIKE zthr_pcp00-zyear,
      w_xyear       LIKE zthr_pcp00-zyear,
      w_zmons       LIKE zthr_pcp00-zmons,
      w_zvers       LIKE zthr_pcp00-zvers,
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

DATA: w_begda      LIKE sy-datum,       " closing date
      w_endda      LIKE sy-datum.       " closing date
DATA: w_ptext(50),
      w_betrg      LIKE wa_reslt-betrg,
*      W_ZHOUY      TYPE PC207,     " HOURLY WAGE
      w_zhouy      LIKE zthr_pcpxx-zhouy,     " HOURLY WAGE
      w_nhouy      LIKE zthr_pcpxx-nhouy,
      w_bonus      LIKE zthr_pcpxx-bonus,     " BONUS WAGE
      w_401k       LIKE zthr_pcpxx-z401k,
      w_nhce       LIKE zthr_pcpxx-znhce,
      w_anhce       LIKE zthr_pcpxx-znhce,
      w_total      LIKE zthr_pcpxx-zhouy,
      w_wrkmo      TYPE i,
      w_wrkdy      TYPE i,
      w_event      TYPE c,
      w_ansal      LIKE zthr_pcp00-ansal.
*DATA: w_increse_rate1 TYPE p DECIMALS 1,
DATA : w_increse_rate1(20)," TYPE p DECIMALS 1,
      w_increse_rate2 TYPE p DECIMALS 1,
      w_increse_rate3 TYPE p DECIMALS 1.
* Attendance increase
DATA: w_attpay      LIKE zthr_pcpxx-zhouy,
      w_att_rate TYPE p DECIMALS 3.
RANGES      r_attjo FOR zthr_pcp00-zobjc.
* GRID data
DATA: it_fcat             TYPE lvc_t_fcat.

DATA: ok_code LIKE sy-ucomm,
      gt_sflight TYPE TABLE OF sflight,
      g_container TYPE scrfname VALUE 'CCONTROL',
      grid1  TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container.
