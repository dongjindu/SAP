*----------------------------------------------------------------------*
*   INCLUDE RPCPRRUD                                                   *
*----------------------------------------------------------------------*
* ERP2005
* =======
* SAM869832 08/10/2005 REC: Tax authority selection not working
*                      correctly
* SAM850091 06/01/2005 MSC: Payroll reconciliation shows incorrect
*                      values
*----------------------------------------------------------------------*

TABLES:  pme34.                         "Feature EVALC Structure

DATA: BEGIN OF trfrm_itab OCCURS 30,
        txfrm LIKE t51t8-txfrm,
        taxau LIKE t51t8-taxau,
        crnfm LIKE t51t8-crnfr,
        ctxau LIKE t51t8-taxau,
      END   OF trfrm_itab.

DATA: rp_langu LIKE sy-langu,          "User's Language
      retcode LIKE sy-subrc.

DATA: subty_str(40)  TYPE c,           "Return string from EVALC
      rp_curr(5)     TYPE c,           "User's Currency
      text_title_tmp(50) TYPE c,
      text_left_tmp(50)  TYPE c,
      text_right_tmp(50) TYPE c,
      cnt_x          TYPE i.           "Counter

DATA: pyarea_t       LIKE t549a-abkrs, "Previous Payroll Area
      co_code_t      LIKE pc205-bukrs, "Previous Company Code
      wgtyp          LIKE t512w-lgart, "Current Wage Type
      wgtyp_t        LIKE t512w-lgart, "Previous Wage Type
      lgart_txt      LIKE t512t-lgtxt, "Wage Type Description
      sel_rpt        LIKE pme34-repid, "Selected Report: DEDU0/ERNU0
      rc             LIKE sy-subrc.   "Return Code from EVALC

DATA: BEGIN OF sel_tab OCCURS 30.
        INCLUDE STRUCTURE pnpstringt.
DATA: END OF sel_tab.

DATA: sel_tab_2 LIKE sel_tab OCCURS 30 WITH HEADER LINE,
      sort_options LIKE sel_tab OCCURS 30 WITH HEADER LINE.

DATA: page_event,
      b_type.

* The variables below are used to pass information to the write FORMs
DATA: tt_code        LIKE pc205-bukrs, "PayArea/CCode
      tt_rt_betrg    LIKE pc207-betrg, "RT Value
      tt_crt_betrg_m LIKE pc207-betrg, "CRT (Mnth) Value
      tt_crt_betrg_q LIKE pc207-betrg, "CRT (Qtr) Value
      tt_crt_betrg_y LIKE pc207-betrg, "CRT (Yr) Value
      tt_ddntk_betrg LIKE pc23e-betrg, "DDNTK Value
      tt_arrrs_betrg LIKE pc22z-betrg. "ARRRS Value


DATA: BEGIN OF itab OCCURS 10,         "Results of Feature EVALC
        evalc(2),
      END OF itab.

DATA: BEGIN OF d_itab OCCURS 0,
        sort_value(10) TYPE c,
      END OF d_itab.

DATA: BEGIN OF rpt_form OCCURS 10,
        form LIKE t596a-appl,
        text LIKE t596b-text,
      END OF rpt_form.

DATA: BEGIN OF f4_fields OCCURS 2.      "Fields
        INCLUDE STRUCTURE help_value.
DATA:  END OF f4_fields.

DATA: BEGIN OF rpt_molga OCCURS 20.
        INCLUDE STRUCTURE t500t.
DATA: END OF rpt_molga.

DATA: tab_indx TYPE i.

DATA: index LIKE sy-tabix.

DATA: wgtyp_value LIKE help_info-fldvalue,
      form_value  LIKE help_info-fldvalue,
      text_value  LIKE help_info-fldvalue.

DATA: tax_forms LIKE rpt_form OCCURS 10 WITH HEADER LINE.

DATA: accumulations LIKE data_table OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF tab_taxty OCCURS 100,   "Evaluated Tax Types
        taxty       LIKE t5utt-taxty,
        ltext       LIKE t5utt-ltext,
      END   OF tab_taxty.

DATA: BEGIN OF tab_taxau OCCURS 100,   "Evaluated Tax Authorities
        taxau       LIKE t5utz-taxau,
        state       LIKE t5utz-state,
        ltext       LIKE t5utz-ltext,
      END   OF tab_taxau.

DATA: box_name LIKE t596h-sumlg,
      box_text LIKE t596h-sumtx.

DATA: prd_str(15) TYPE c,
      paydt_str(10) TYPE c,
      cntr TYPE i,
      total_betrg LIKE data_table-betrg,
      total_anzhl LIKE data_table-anzhl,
      sub_total_betrg LIKE data_table-betrg,
      sub_total_anzhl LIKE data_table-anzhl.

DATA: data_totals LIKE data_table OCCURS 0 WITH HEADER LINE.

DATA: tr_form_typ(4) TYPE c.

DATA: as_of_previous LIKE rgdir-paydt,
      as_of_current  LIKE rgdir-paydt.

DATA: save_pnpabkrs LIKE pnpabkrs OCCURS 0 WITH HEADER LINE.

*<SAM850091>
** Note 492427
*DATA: begin of save_rt occurs 0,
*        lgart like rt-lgart.
*DATA: end of save_rt.
*
** Note 0516833
*DATA: begin of wts_read occurs 0,
*        lgart like rt-lgart.
*DATA: end of wts_read.
*</SAM850091>

* Note 512368
DATA: save_begps LIKE pnpbegda,
      save_endps LIKE pnpendda.
DATA: BEGIN OF g_acct_type OCCURS 0,
        lgart LIKE t52el-lgart,
        endda LIKE t52el-endda,
        symko LIKE t52el-symko,
        koart LIKE t52ek-koart.
DATA: END OF g_acct_type.

*<SAM869832>
*DATA  g_fed_added TYPE c.
*</SAM869832>
