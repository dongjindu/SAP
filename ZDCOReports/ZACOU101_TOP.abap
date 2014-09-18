*----------------------------------------------------------------------*
*   INCLUDE ZACOU101_TOP                                               *
*----------------------------------------------------------------------*
*  Define Variant & tables & local class
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Define Variant & tables
*----------------------------------------------------------------------*
TABLES : ztcou100, kala, tck03 .


TYPES: BEGIN OF ty_ztcou100,
         kokrs  TYPE kokrs,
         kalka  TYPE ck_kalka,
         bdatj  TYPE bdatj,
         poper  TYPE poper,
         matnr  TYPE matnr,
         verid  TYPE verid,
         werks  TYPE werks_d,
         cnfg   TYPE zcnfg,
         stlan  TYPE stlan,
         crint  TYPE zcrint,
         crext  TYPE zcrext,
         sstat  TYPE icon_d,  " SELECT
         bstat  TYPE icon_d,  " BOM
         cstat  TYPE icon_d,  " COSTING
         rstat  TYPE zrstat,                                " 103,
         rstat1 TYPE icon_d,                                " 103
         lstat  TYPE zlstat,
         bwdat  TYPE ck_bwdat,
         aldat  TYPE ck_brdat,
        END OF  ty_ztcou100.

TYPES: BEGIN OF ty_out.
INCLUDE TYPE ty_ztcou100.
TYPES:   kalaid TYPE ck_kalaid,
         chk,
         err2(1),
         idx TYPE i,
         msg2 LIKE cfgnl-msglin,
         kadky type CK_KADKY,
         reg,
       END OF ty_out.

TYPES: BEGIN OF ty_keko,
         kalnr   TYPE ck_kalnr,
         matnr   TYPE matnr,
         verid   TYPE verid,
         kokrs   TYPE kokrs,
         kalka   TYPE ck_kalka,
         tvers   TYPE ck_tvers,
         kaladat TYPE ck_abdat,
         feh_sta TYPE ck_feh_sta,
         maxmsg  TYPE symsgty,
         kalaid  TYPE ck_kalaid,
         bwdat   TYPE ck_bwdat,
         aldat   TYPE ck_brdat,
       END OF ty_keko.

TYPES: BEGIN OF ty_103,
         artnr TYPE artnr,
       END OF ty_103.

TYPES: BEGIN OF ty_kala,
         kalaid TYPE ck_kalaid,
         kalka  TYPE kalka,
       END OF ty_kala.

TYPES: BEGIN OF ty_var,
         matnr TYPE matnr,
         cnfg  TYPE zcnfg,
       END OF ty_var.

TYPES: BEGIN OF ty_down,
         matnr TYPE matnr,
         cnfg  TYPE zcnfg,
       END OF ty_down.

TYPES: BEGIN OF ty_kalaid,
         kalaid TYPE ck_kalaid,
         matnr  TYPE matnr,
         cnfg   TYPE zcnfg,
       END OF ty_kalaid.

TYPES: BEGIN OF ty_stat,
         feh_stat TYPE ck_feh_sta,
       END OF ty_stat.

* Internal table for display
DATA: gt_ztcou100 TYPE TABLE OF ty_ztcou100 WITH HEADER LINE,
      gt_keko     TYPE TABLE OF ty_keko     WITH HEADER LINE,
      gt_103      TYPE TABLE OF ty_103      WITH HEADER LINE,
      gt_kala     TYPE TABLE OF ty_kala     WITH HEADER LINE,
      gt_out      TYPE TABLE OF ty_out      WITH HEADER LINE,
      gt_sel_f    TYPE TABLE OF ty_var      WITH HEADER LINE,
      gt_sel_m    TYPE TABLE OF ty_var      WITH HEADER LINE,
      gt_stat     TYPE TABLE OF ty_stat     WITH HEADER LINE.

RANGES r_matnr FOR mara-matnr.

DATA: gv_valdt TYPE sydatum,  "valuation date
      gv_cstdt TYPE sydatum,  "Costing run date
      gv_aldat TYPE sydatum,  "Qty date
      gv_tcnt TYPE i,
      gv_ccnt TYPE i,
      gv_rcnt TYPE i,
      gv_lcnt TYPE i,
      gv_mm02 TYPE i,
      gv_lock,
      gv_but  TYPE i VALUE 1,
      gv_klvar TYPE ck_klvar,
      gv_cnt_f TYPE i,
      gv_cnt_m TYPE i.

DATA messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click
               FOR EVENT hotspot_click OF cl_gui_alv_grid
               IMPORTING e_row_id
                         e_column_id.
ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
* Setting for hotspot click
  METHOD handle_hotspot_click.
    PERFORM hotspot_click
                 USING e_row_id e_column_id.
  ENDMETHOD.                    " handle_hotspot_click


ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.
