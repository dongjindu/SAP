*&---------------------------------------------------------------------*
*&  Include           ZAHRU1001
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZAHRU1001
*&---------------------------------------------------------------------*
TABLES : pa0001,
         pa0002,
         pc261, *t549q ,t549a, t549s, HRP1001.
*         zshr9001, zshr9002.

DATA : BEGIN OF gt_temp OCCURS 0.
        INCLUDE STRUCTURE zthr0030.
*DATA : grptx LIKE zthr0029-grptx,
DATA :  lengh LIKE zthr0029-lengh.
DATA : END OF gt_temp.
DATA : BEGIN OF gt_result OCCURS 0.
        INCLUDE STRUCTURE ZSHR0240_EGREP.
DATA : END OF gt_result.

Data: gt_temp2 LIKE gt_temp OCCURS 0 WITH HEADER LINE,
      gt_tab    LIKE zshr0240_2 OCCURS 0 WITH HEADER LINE,
      in_rgdir LIKE pc261 OCCURS 0 WITH HEADER LINE,
      tp_rgdir LIKE pc261 OCCURS 0 WITH HEADER LINE,
      gt_0029    LIKE zthr0029 OCCURS 0 WITH HEADER LINE.
DATA : ws_rgdir LIKE LINE OF in_rgdir.

data:  gt_payroll TYPE hrpayus_tab_of_results, "HRPAY99_TAB_OF_RESULTS,
       lt_payroll LIKE LINE OF gt_payroll,
       gt_wpbp    LIKE pc205 OCCURS 0 WITH HEADER LINE,
       lt_wpbp    LIKE gt_wpbp,
       gt_rt      LIKE pc207 OCCURS 0 WITH HEADER LINE,
       gt_crt     LIKE pc208 OCCURS 0 WITH HEADER LINE,
       gt_bt      LIKE pc209 OCCURS 0 WITH HEADER LINE,
       gt_ddntk   LIKE pc23e OCCURS 0 WITH HEADER LINE,  "Deductions
       gt_accr    LIKE pc23g OCCURS 0 WITH HEADER LINE,  "Accruals
       g_tabix(3) TYPE n,
       fpper type FAPER,
       g_cnt(10),
       g_tabname(30),
       g_betrg(10),
       g_usrnm(80),  "Top Comment - User Name
       g_info1(30),  "Top Comment - date
       g_info2(30),  "Top Comment - date
       lv_begda LIKE sy-datum,
       lv_endda LIKE sy-datum,
       g_begda LIKE sy-datum,  "Begin Date
       g_endda LIKE sy-datum.  "End Date


FIELD-SYMBOLS : <fs_01> TYPE ANY, <fs_02> TYPE ANY.

TYPES: BEGIN OF ts_inper_directory_entry.
TYPES:   ipend LIKE pc261-ipend,
         iperm LIKE pc261-iperm,
         inper LIKE pc261-inper,
         inpty LIKE pc261-inpty,
         inpid LIKE pc261-inpid,
       END OF ts_inper_directory_entry.
TYPES: tt_inper_directory TYPE ts_inper_directory_entry OCCURS 0.

DATA payty_cal LIKE pc261-payty.
DATA lt_tweaked_evp TYPE hrpy_tt_rgdir.
DATA lt_evp_related_records TYPE hrpy_tt_rgdir.
DATA lt_rgdir TYPE hrpy_tt_rgdir.
DATA lt_inper_directory TYPE tt_inper_directory.
DATA ls_inper_directory_entry TYPE ts_inper_directory_entry.
DATA lv_evp_lines_number TYPE i.
DATA lv_relid LIKE t500l-relid.
DATA lt_eval_tab  TYPE pay_t_eval_period.
RANGES rg_abkrs FOR p0001-abkrs.
DATA iv_fp_end LIKE sy-datum.

FIELD-SYMBOLS <eval_wa> TYPE pay_eval_period.

DATA: BEGIN OF it_orgtx OCCURS 0,
        orgeh LIKE pa0001-orgeh,
        orgtx TYPE orgtx,
      END OF it_orgtx           .

DATA: BEGIN OF it_status OCCURS 0,
        pernr LIKE pa0000-pernr,
        begda LIKE pa0000-begda,
        massn LIKE pa0000-massn,
        massg LIKE pa0000-massg,
        stat2 LIKE pa0000-stat2,
      END OF it_status           .

DATA: BEGIN OF it_pernr OCCURS 0,
        pernr  LIKE pa0000-pernr,
*        nachn  TYPE nachn,
*        vorna  TYPE vorna,
*        perid  LIKE pa0002-perid,
        stat2  LIKE pa0000-stat2,
END   OF it_pernr.
data: ws_pernr LIKE LINE OF it_pernr.
DATA: w_permo  LIKE t549a-permo,   " Period parameters
      w_abkrt  LIKE t549t-atext,   " Payroll Area Text
      ws_status LIKE LINE OF it_status,
      lv_tabix TYPE sy-tabix.

DATA: v_pabrj LIKE *t549q-pabrj,
      v_pabrp LIKE *t549q-pabrp,
      l_relid LIKE  pcl2-relid.

data: w_date like sy-datum.

RANGES: objid_range FOR objec-objid.

DATA : lv_molga TYPE molga.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE u_break.
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.
DEFINE __define_not_important.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $current_cnt(10),$total_cnt(10),$text(100) .
  clear : total_doc_cnt,current_doc_cnt.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA: g_error(1).

DATA: old_abkrs TYPE abkrs.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16)  text-x02 MODIF ID opt.
SELECTION-SCREEN POSITION 28.
PARAMETERS p_op0   RADIOBUTTON GROUP radi USER-COMMAND radio.
SELECTION-SCREEN POSITION 33.
PARAMETERS p_date LIKE sy-datum. " pa0001-begda.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16)  text-x03 MODIF ID opt.
SELECTION-SCREEN POSITION 28.
PARAMETERS p_op1   RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_begda   FOR pa0001-begda no-extension.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN END OF BLOCK b4.

* Select_options
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS s_objid FOR HRP1001-objid NO INTERVALS.
*SELECT-OPTIONS s_pernr FOR pa0001-pernr no intervals.
PARAMETERS     p_abkrs LIKE  pa0001-abkrs OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1  .

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK bk0 WITH FRAME TITLE text-000.
PARAMETERS : p_lgafm LIKE zthr0036-lgafm                  "Form
                    MATCHCODE OBJECT zhr_0036 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bk0.

SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE text-001.
PARAMETERS : p_each  RADIOBUTTON GROUP rad0 DEFAULT 'X',  "Each Period
             p_total RADIOBUTTON GROUP rad0.              "Total Period
SELECTION-SCREEN END OF BLOCK bk1.


PARAMETERS: p_abrpr  LIKE t549q-pabrp MODIF ID p1 NO-DISPLAY,
            p_abrjr  LIKE t549q-pabrj MODIF ID p1 NO-DISPLAY.

PARAMETERS: ypernodt type xfeld NO-DISPLAY default space.
SELECT-OPTIONS s_pyty_c FOR payty_cal NO-DISPLAY.

RANGES : r_begda   FOR pa0001-begda,
         r_datum   FOR pa0001-begda.

SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE text-002.
PARAMETERS : p_kostl LIKE scmaper-month1 no-display,                 "CC. Code
             p_ltext LIKE scmaper-month1 no-display,                 "CC. Text
             p_orgtx LIKE scmaper-month1 no-display,                 "Org. Unit
             p_orx02 LIKE scmaper-month1 no-display,                 "Division
             p_orx03 LIKE scmaper-month1 no-display,                 "Sub. Division
             p_orx04 LIKE scmaper-month1 no-display,                 "Department
             p_orx05 LIKE scmaper-month1 no-display,                 "Section
             p_orx06 LIKE scmaper-month1 no-display,                 "WorkGroup
             p_orx07 LIKE scmaper-month1 no-display,                 "Group
             p_orx08 LIKE scmaper-month1 no-display,                 "Team
             p_pgtxt LIKE scmaper-month1 no-display,                 "EEG
             p_pktxt LIKE scmaper-month1 no-display,                 "ESG
             p_pernr LIKE scmaper-month1 DEFAULT '001' no-display,   "Pers. No.
             p_nachn LIKE scmaper-month1 DEFAULT '002' no-display,   "Name
             p_midnm LIKE scmaper-month1 no-display,                 "Middle Name
             p_tittx LIKE scmaper-month1 no-display,                 "Title
             p_apptx LIKE scmaper-month1 no-display,                 "Position Assignment
             p_msttx LIKE scmaper-month1 no-display,                 "Supervisor
*             p_perid LIKE scmaper-month1 no-display,                 "SSN
             p_statx LIKE scmaper-month1 no-display,                 "Employment Status
             p_gbdat LIKE scmaper-month1 no-display,                 "Birth Date
             p_hirdt LIKE scmaper-month1 no-display,                 "Entry Date
             p_trmdt LIKE scmaper-month1 no-display,                 "Termination Date
             p_abktx LIKE scmaper-month1 no-display,                 "Payroll Area
             p_STRAS LIKE scmaper-month1 no-display.                 "Address
SELECTION-SCREEN END OF BLOCK bk2.
