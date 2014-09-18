*&---------------------------------------------------------------------*
*& Include MZEMMGM01E_6007TOP                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
* For PF-STATUS and Titlebar
CLASS lcl_ps DEFINITION DEFERRED.
DATA: crv_ps TYPE REF TO lcl_ps.

* For OK code
DATA: ok_code LIKE sy-ucomm,  save_ok_code LIKE ok_code.

* For Return code
DATA: gv_subrc LIKE sy-subrc.

** Begin of Menu
* Itab & WA For FOR Dynamic Menu
DATA:  it_func TYPE STANDARD TABLE OF rsmpe-func.
DATA:  wa_func LIKE LINE OF it_func.
* Title
DATA: title(80).         " Title
** End of Menu

* Global structure for ZTMM_6007_01
TABLES: ztmm_6007_01.   "(Sub_Raw)
TABLES: ztmm_6007_02.   "(End_Sub)
DATA: gs_ztmm_6007_01 LIKE ztmm_6007_01.  "Sub_Raw
DATA: gs_ztmm_6007_02 LIKE ztmm_6007_02.  "End_Sub

**** Table Control
DATA visiblelines LIKE sy-loopc. " sy-loopc at PBO

CONTROLS: tc_9200 TYPE TABLEVIEW USING SCREEN '9200'.
DATA:     wa_tc_9200cols LIKE LINE OF tc_9200-cols.
" Workarea for Table Control structure COLS
** 9200 (Itab and WA Related to End_Sub)
TABLES: zsmm_6007_02.   "(End_Sub)
DATA: wa_zsmm_6007_02 LIKE zsmm_6007_02.  "End_Sub
DATA: it_zsmm_6007_02 LIKE TABLE OF wa_zsmm_6007_02.
DATA: gt_zsmm_6007_02 LIKE it_zsmm_6007_02.
DATA: gs_zsmm_6007_02 LIKE LINE OF gt_zsmm_6007_02.

DATA: gs_zvmm_6007_05 LIKE zvmm_6007_05.
DATA: gt_zvmm_6007_05 LIKE TABLE OF zvmm_6007_05.


* indices
DATA ix_check TYPE i.  "For check
