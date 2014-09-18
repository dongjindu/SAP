*----------------------------------------------------------------------*
*   INCLUDE ZACOU127_TOP                                               *
*----------------------------------------------------------------------*
TABLES : tka01,
         ztcou103, ztcou106 , bsis , ckmlmv011 ,sscrfields ,
        *ztcou127.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
END-OF-DEFINITION.

DATA NUM(12) VALUE ' 0123456789.'.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
****************************** Global Data *****************************
TYPES: BEGIN OF ty_t001k,
         bwkey TYPE bwkey,                " Plant
       END OF ty_t001k.

TYPES: BEGIN OF s_mats,
        kalnr    TYPE ck_kalnr,           " Cost est number
        matnr    TYPE matnr,              " Material
        werks    TYPE werks_d,            " Plant
        mtart    TYPE mtart,              " Material type
        matkl    TYPE matkl,              " Material class
        kalst    TYPE ck_kalst,           " Costing Level
        bklas    TYPE bklas,              " val class
        stprs    TYPE stprs,              " Standard price
        verpr    TYPE verpr,              " Moving average price
        peinh    TYPE ck_peinh_1,         " Price unit
        meins    TYPE meins,              " Base unit of measure
        zukumo   TYPE ck_zukum,           " GR
        ekkumo   TYPE ck_ekkum,           " PO GR
   END OF s_mats,
   ty_mats TYPE STANDARD TABLE OF s_mats WITH KEY kalnr.

TYPES: BEGIN OF ty_row_tab,
            artnr  LIKE ztcou127-artnr ,
          anpoper  LIKE ztcou127-poper ,
            upgvc  LIKE ztcou127-upgvc ,
            compn  LIKE ztcou127-compn ,
            lifnr  LIKE ztcou127-lifnr,
            ekgrp  LIKE ztcou127-ekgrp,
            kstar  LIKE ztcou127-kstar ,
            kzust(3),
            wertn  LIKE ztcou127-wertn,
            menge  LIKE ztcou127-menge,
            upgvc_t         LIKE makt-maktx,
            compn_t         LIKE makt-maktx,
            rsn_t           TYPE zrtext,
            sign   TYPE sign,
            meeht  TYPE meins,
            source(1),
            prd_qty LIKE ztcou127-prd_qty,
            $prd_qty(15).
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_bsis,
        $key(14),
         bukrs TYPE bukrs,                " Company code
         gjahr TYPE gjahr,                " Fiscal year
         monat TYPE monat,                " Fiscal period
         zuonr TYPE dzuonr,               " Assignment number
         belnr TYPE belnr_d,              " Accounting document number
         buzei TYPE buzei,                " Line item number
         dmbtr TYPE dmbtr,                " Amount in local currency
         bschl TYPE bschl,  "dr/cr
         $matnr type matnr,
       END OF ty_bsis.

TYPES: BEGIN OF ty_rv.
INCLUDE TYPE ty_bsis.
TYPES:   bwkey TYPE bwkey,                " Plant
         peinh TYPE peinh,                " Price unit
         ebeln TYPE ebeln,                " PO No.
         ebelp TYPE ebelp,                " PO Item No.
         menge TYPE menge_d,              " PO Qty.
         meins TYPE meins,                " Base unit of measure
         kschl TYPE kschl,                " Condition type
         lifnr TYPE lifnr,                " Vendor
       END OF ty_rv.

DATA: BEGIN OF it_ztcou103 OCCURS 0,
             kalka  LIKE ztcou103-kalka,
             artnr  LIKE ztcou103-artnr,
             upgvc  LIKE ztcou103-upgvc,
             compn  LIKE ztcou103-compn,
             werks  LIKE ztcou103-werks,
             kstar  LIKE ztcou103-kstar,
             typps  LIKE ztcou103-typps,
             menge  LIKE ztcou103-menge,
             stkkz  LIKE ztcou103-stkkz,
             gpreis LIKE ztcou103-gpreis,
             wertn  LIKE ztcou103-wertn,
             duty   LIKE ztcou103-duty,
             frg    LIKE ztcou103-frg,
             oth    LIKE ztcou103-oth,
             peinh  LIKE ztcou103-peinh,
             kalnr  LIKE ztcou103-kalnr,
             meeht  LIKE ztcou103-meeht,
      END OF it_ztcou103.

DATA: BEGIN OF it_ztcou106 OCCURS 0.
INCLUDE  TYPE ty_row_tab.
DATA:
            seq    LIKE ztcou106-seq   ,
            kzust1 LIKE ztcou106-kzust1,
            wertn1 LIKE ztcou106-wertn1,
            kzust2 LIKE ztcou106-kzust2,
            wertn2 LIKE ztcou106-wertn2,
            kzust3 LIKE ztcou106-kzust3,
            wertn3 LIKE ztcou106-wertn3,
            kpein  LIKE ztcou106-kpein .
DATA: END OF it_ztcou106.

DATA: BEGIN OF it_103 OCCURS 0,
             artnr  LIKE ztcou103-artnr,
             upgvc  LIKE ztcou103-upgvc,
             compn  LIKE ztcou103-compn,
             werks  LIKE ztcou103-werks,
             kalnr  LIKE ztcou103-kalnr,
END OF it_103.

DATA : BEGIN OF it_ckmlmv003 OCCURS 0,
         bwkey      LIKE ckmlmv001-bwkey,
         matnr      LIKE ckmlmv001-matnr,
         aufnr      LIKE ckmlmv013-aufnr,
         verid_nd   LIKE ckmlmv001-verid_nd,
         meinh      LIKE ckmlmv003-meinh,
         out_menge  LIKE ckmlmv003-out_menge,
       END OF  it_ckmlmv003.

DATA : BEGIN OF it_rv_sum OCCURS 0,
         kokrs TYPE kokrs,
         gjahr TYPE gjahr,                " Fiscal year
         poper TYPE poper,                " Fiscal period
         matnr TYPE matnr,
         lifnr TYPE lifnr,                " Vendor
         ekgrp TYPE ekgrp,
         menge TYPE menge_d,              " PO Qty.
         kzust TYPE kzust,
         chnge TYPE zchnge,               " Price
         meins TYPE meins,                " Base unit of measure
         $matnr type matnr,
        $flag(1),
       END OF  it_rv_sum.

DATA : BEGIN OF it_mat_txt  OCCURS 0,
         matnr      LIKE marc-matnr,
         werks      LIKE marc-werks,
         raube      LIKE mara-raube, "shop
         fevor      LIKE marc-fevor, "PP schedule
         mtart      LIKE mara-mtart,
         matkl      LIKE mara-matkl,
         bismt      LIKE mara-bismt,
         vspvb      LIKE marc-vspvb,
         prctr      LIKE marc-prctr,
         maktg      LIKE makt-maktg,
       END OF it_mat_txt.

DATA : BEGIN OF it_reason_txt  OCCURS 0,
          rgrp2 LIKE ztcoum02-rgrp2,
          text  LIKE ztcoum02-text,
       END OF it_reason_txt.

DATA : BEGIN OF it_material OCCURS 0,
         matnr TYPE matnr,
         mtype(1) TYPE c,
       END OF it_material.

DATA: BEGIN OF $it_row_tab OCCURS 0,
            artnr  LIKE ztcou127-artnr,
            lifnr  LIKE ztcou127-lifnr,
            wertn  LIKE ztcou127-wertn,
            menge  LIKE ztcou127-menge,
            meeht  TYPE meins.
DATA: END OF $it_row_tab.


* Type for ALV
TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES: waers LIKE tka01-waers.
TYPES: END OF ty_out.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE,
        gt_mat_reval   TYPE ty_mats               WITH HEADER LINE,
        gt_bsis  TYPE TABLE OF ty_bsis  WITH HEADER LINE,
        gt_rv    TYPE TABLE OF ty_rv    WITH HEADER LINE.

DATA: g_error(1),
      g_repid  LIKE sy-repid.

DATA: p_bukrs LIKE bsis-bukrs.

RANGES: r_bwkey FOR t001k-bwkey.           " Plant

DATA: gv_date_f TYPE sydatum,             " from date
      gv_info_f TYPE sydatum,             " from date (info)
      gv_date_t TYPE sydatum,             " to date
      gv_date3  TYPE sydatum,             " next end
      g_ix   LIKE sy-tabix.

INCLUDE <icon>.                        " icon
