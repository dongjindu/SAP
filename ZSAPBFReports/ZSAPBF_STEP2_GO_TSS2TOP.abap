*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_STEP2_GO_TSS2TOP
*&---------------------------------------------------------------------*
*TYPE-POOLS: ppcpr.

CONSTANTS:
  gc_charx        TYPE c VALUE 'X',
  gc_chars        TYPE c VALUE 'S',
  gc_lowlimit     TYPE i VALUE 1,    "Min. NR of lines in mat. doc.
  gc_maxlimit     TYPE i VALUE 9999, "Max. NR of lines in mat. doc.
  gc_maxitems     TYPE i VALUE 500,  "Proposed NR of lines in doc.
  gc_min_sec      TYPE zsapbf_ppc_delay VALUE 600.
*  gc_ppc_typ_tss2 TYPE c VALUE '3'.  "Lock plant for Two Step TSS2
*
*TABLES: ppc_ord_inf. "DNE
**CDP2 Start
*TABLES: sscrfields.

*CDP2 End


DATA:
  gv_date        TYPE budat,
  gv_unam        TYPE ppc_confuname,
  gv_lognr       TYPE balognr,
  gs_parallel    TYPE zsapbf_ppc_parallel,
  gt_date_range  TYPE ppcpr_postdate_range,
  gt_unam_range  TYPE ppcpr_username_range,
  gs_seldate     LIKE LINE OF gt_date_range,
  gs_selunam     LIKE LINE OF gt_unam_range,
*  gt_plant_model TYPE zsapbf_tt_plant_model,
  gr_werks_range TYPE shp_werks_range_t.

DATA: gr_conftime TYPE zsapbf_tt_conftime_range.
*CDP2 Start
DATA: gs_button TYPE smp_dyntxt.
*DATA: gf_plant_received_jobs TYPE i VALUE 0.
*CDP2 End
*---------------------------------------------------------------------*
* Selection screen
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN  0001 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

SELECT-OPTIONS: so_date   FOR gv_date DEFAULT sy-datlo.
*----> Backflush entry Start date and time
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(50) text-004.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(20) FOR FIELD p_dtsta.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_dtsta TYPE zsapbf_ppc_stadate.
SELECTION-SCREEN COMMENT 52(4) FOR FIELD p_tista.
SELECTION-SCREEN POSITION 58.
PARAMETERS: p_tista TYPE zsapbf_ppc_statime.
SELECTION-SCREEN END OF LINE.

*----> Backflush entry end date and time
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(20) FOR FIELD p_dtend.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_dtend TYPE zsapbf_ppc_enddate.
SELECTION-SCREEN COMMENT 52(4) FOR FIELD p_tiend.
SELECTION-SCREEN POSITION 58.
PARAMETERS: p_tiend TYPE zsapbf_ppc_endtime.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_delay TYPE zsapbf_ppc_delay DEFAULT 600.

SELECT-OPTIONS: so_unam   FOR gv_unam DEFAULT sy-uname,
                so_werks  FOR ppc_ord_inf-plant.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:
  p_show_l  TYPE ppc_proto DEFAULT gc_charx,
  p_proto   TYPE ppc_proto DEFAULT gc_charx,
  p_limit   TYPE ppc_count DEFAULT gc_maxitems.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN END OF SCREEN 0001.



SELECTION-SCREEN BEGIN OF SCREEN 0002 AS SUBSCREEN.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_par  TYPE ppc_para AS CHECKBOX.
PARAMETERS: p_sgr  TYPE wsl_ctrl-serv_grp.
PARAMETERS: p_wps  TYPE zsapbf_ppc_wps_req DEFAULT 4.

PARAMETERS: p_del_hl TYPE ppc_mwdel_th DEFAULT gc_charx.
PARAMETERS: p_wttime TYPE ppc_mwwaittime DEFAULT 20.
*PARAMETERS: p_wtendt TYPE ppc_waitendtime DEFAULT 100.
PARAMETERS: p_retryc TYPE ppc_retry_com DEFAULT 3.
PARAMETERS: p_retrys TYPE ppc_retry_sys DEFAULT 3.
PARAMETERS: p_retryr TYPE ppc_retry_res DEFAULT 3.
*PARAMETERS: p_retrye TYPE ppc_retry_end DEFAULT 3.

SELECTION-SCREEN END OF BLOCK b3.

*CDP2 Start
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-013.
ENHANCEMENT-SECTION     TSS2 SPOTS ZSAPBF_TSS2_ENHANCEMENT STATIC .

*{ SAPCD08 2010-08-02 #625886
*PARAMETERS: p_meth1 TYPE zsapbf_meth_par_single RADIOBUTTON GROUP meth DEFAULT 'X' USER-COMMAND meth3,
*            p_meth2 TYPE zsapbf_meth_par_single RADIOBUTTON GROUP meth.
PARAMETERS: p_meth1 TYPE xfeld RADIOBUTTON GROUP meth DEFAULT 'X' USER-COMMAND meth3,
            p_meth2 TYPE xfeld RADIOBUTTON GROUP meth .
*} End of change
PARAMETERS: p_meth TYPE zsapbf_cust_meth-method MODIF ID bl1 AS LISTBOX VISIBLE LENGTH 100 USER-COMMAND meth.
END-ENHANCEMENT-SECTION.
INCLUDE zsapbf_conf_go_tss2_ui.                             "#EC *
SELECTION-SCREEN END   OF BLOCK b4.

SELECTION-SCREEN END OF SCREEN 0002.

SELECTION-SCREEN BEGIN OF TABBED BLOCK test1 FOR 22 LINES.

SELECTION-SCREEN TAB (40) tabs1 USER-COMMAND ucomm1
                  DEFAULT SCREEN 0001 .

SELECTION-SCREEN TAB (40) tabs2 USER-COMMAND ucomm2
                  DEFAULT SCREEN 0002 .

SELECTION-SCREEN END OF BLOCK test1.

*CDP2 End

SELECTION-SCREEN FUNCTION KEY 2.
*CDP2 End
