*----------------------------------------------------------------------*
*   INCLUDE ZACOU109_TOP                                               *
*----------------------------------------------------------------------*
TABLES: ZTCOU103,                  " [CO] Cost Roll-Up
        SSCRFIELDS,
        MARA,                      " General Material Data
        KEKO,*MAST.
* Internal Table for UPG Description
TYPES: BEGIN OF TY_VC,
         UPGVC  TYPE MATNR,        " UPG
         UPGTXT TYPE MAKTG,        " UPG Text
       END OF TY_VC.

TYPES: BEGIN OF TY_VCIT,
         UPGVC  TYPE MATNR,        " UPG
         COMPN  TYPE IDNRK,        " Item
       END OF TY_VCIT.

* Internal Table for Cost review information
TYPES: BEGIN OF TY_COST,
         WERKS  TYPE WERKS_D,      " Plant
         ARTNR  TYPE ARTNR,        " Product
         UPGVC  TYPE MATNR,        " UPG
         KSTAR  TYPE KSTAR,        " Cost element
         COMPN  TYPE IDNRK,        " Item
         MENGE  TYPE MENGE_POS,    " Usage(Qty)
         GPREIS TYPE ZGPREIS,      " Unit Price
         WERTN  TYPE ZWERTN,       " Info-price
         DUTY   TYPE ZDUTY1,       " Duty price
         FRG    TYPE ZFRG1,        " Freight price
         OTH    TYPE ZOTH1,        " Others price
         PEINH  TYPE PEINH,
         MEEHT  TYPE MEINS,
         SPLNT  TYPE WERKQ,        " Supplying plant (source)
         BWDAT  TYPE CK_BWDAT,     " Valuation date of a cost estimate
         MAKTG  TYPE MAKTG,        " Item Description
         PROFL  TYPE PROFL,        " Src
         MATKL  TYPE MATKL,        " Mat Grp
         MTART  TYPE MTART,        " Material type
         EKGRP  TYPE EKGRP,        " Purchasing group
         BDATJ  TYPE BDATJ,
         POPER  TYPE POPER,
* UD1K941189 - by IG.MOON 8/1/2007 {
        $ARTNR  TYPE ARTNR,
         STKKZ  TYPE STKKZ,        " PM assembly indicator
         KALNR  TYPE CK_KALNR,
* }
* UD1K941566 - by IG.MOON 9/11/2007 {
         KALKA  TYPE CK_KALKA,
* }
       END OF TY_COST.

* Internal Table for FSC
TYPES: BEGIN OF TY_FSC,
         IDX(5),                         " Index
         ARTNR     TYPE ARTNR,           " Product
         MENGE(15) TYPE P DECIMALS 3,    " Usage(Qty)
         WERTN     TYPE ZWERTN,          " Price
       END OF TY_FSC.

* Internal Table for get usage of FSC
TYPES: BEGIN OF TY_MAT,
         ARTNR TYPE ARTNR,          " Product
         CNT   TYPE I,              " Index
       END OF TY_MAT.

* Internal Table for Costing Result
TYPES: BEGIN OF TY_102,
         MATNR   TYPE MATNR,        " Material
         WERKS   TYPE WERKS,        " Plant
         LIFNR   TYPE LIFNR,        " Vendor
         KZUST1  TYPE KZUST,        " Reason
         WERTN   TYPE ZWERTN,
         PEINH   TYPE PEINH,
         PMEHT   TYPE PMEHT,
         KALKA   TYPE CK_KALKA,
         BDATJ   TYPE BDATJ,
         POPER   TYPE POPER,
       END OF TY_102.

* Internal Table for STD&MAP
TYPES: BEGIN OF TY_STD,
         MATNR TYPE MATNR,          " Material
         BWKEY TYPE BWKEY,          " Valuation area
         STPRS TYPE CK_STPRS_1,     " Standard price
         PVPRS TYPE CK_PVPRS_1,     " Moving average price
* UD1K941189 - by IG.MOON 8/1/2007 {
         KALNR TYPE CK_KALNR,       " Cost est number
* }
       END OF TY_STD.

* Internal Table for display
TYPES: BEGIN OF TY_OUT,

* UD1K941189 - by IG.MOON 8/1/2007 {
         ARTNR   TYPE ARTNR,          " Product
* }
         IDX     TYPE I,            " Index
         MCNT    TYPE I,            " Count of qty base
         WCNT    TYPE I,            " Count of price base
         UPGVC   TYPE MATNR,        " UPG
         UPGTXT  TYPE MAKTG,        " UPG Text
         KSTAR   TYPE KSTAR,        " Cost element
         COMPN   TYPE IDNRK,        " Material
         MAKTG   TYPE MAKTG,        " Material Desc.
         PROFL   TYPE ADGE_PROFL,   " Source
         MATKL   TYPE MATKL,        " Material group
         EKGRP   TYPE EKGRP,        " Purchasing group
         LIFNR   TYPE LIFNR,        " Vendor
         WERTN   TYPE ZWERTN,       " Info-price
         PEINH   TYPE PEINH,        " Price unit
         PMEHT   TYPE PMEHT,        " Price quantity unit
         KZUST   TYPE KZUST,        " Reason
         VERPR   TYPE CK_PVPRS_1,   " Moving average price
         STPRS   TYPE CK_STPRS_1,   " Standard price

         MENGT   TYPE MENGE_POS,    " Total Qty
         WERTT   TYPE ZWERTN,       " Total Amt
         MEEHT   TYPE MEINS,                                " 103 UoM
         MAVG    TYPE ZWERTN,       " Average qty
         WAVG    TYPE ZWERTN,       " Average price
         MENGE1  TYPE MENGE_POS,    " Usage
         MENGE2  TYPE MENGE_POS,
         MENGE3  TYPE MENGE_POS,
         MENGE4  TYPE MENGE_POS,
         MENGE5  TYPE MENGE_POS,
         MENGE6  TYPE MENGE_POS,
         MENGE7  TYPE MENGE_POS,
         MENGE8  TYPE MENGE_POS,
         MENGE9  TYPE MENGE_POS,
         MENGE10 TYPE MENGE_POS,
         MENGE11 TYPE MENGE_POS,
         MENGE12 TYPE MENGE_POS,
         MENGE13 TYPE MENGE_POS,
         MENGE14 TYPE MENGE_POS,
         MENGE15 TYPE MENGE_POS,
         MENGE16 TYPE MENGE_POS,
         MENGE17 TYPE MENGE_POS,
         MENGE18 TYPE MENGE_POS,
         MENGE19 TYPE MENGE_POS,
         MENGE20 TYPE MENGE_POS,
         MENGE21 TYPE MENGE_POS,
         MENGE22 TYPE MENGE_POS,
         MENGE23 TYPE MENGE_POS,
         MENGE24 TYPE MENGE_POS,
         MENGE25 TYPE MENGE_POS,
         MENGE26 TYPE MENGE_POS,
         MENGE27 TYPE MENGE_POS,
         MENGE28 TYPE MENGE_POS,
         MENGE29 TYPE MENGE_POS,
         MENGE30 TYPE MENGE_POS,
         WERTN1  TYPE ZWERTN,       " Price
         WERTN2  TYPE ZWERTN,
         WERTN3  TYPE ZWERTN,
         WERTN4  TYPE ZWERTN,
         WERTN5  TYPE ZWERTN,
         WERTN6  TYPE ZWERTN,
         WERTN7  TYPE ZWERTN,
         WERTN8  TYPE ZWERTN,
         WERTN9  TYPE ZWERTN,
         WERTN10 TYPE ZWERTN,
         WERTN11 TYPE ZWERTN,
         WERTN12 TYPE ZWERTN,
         WERTN13 TYPE ZWERTN,
         WERTN14 TYPE ZWERTN,
         WERTN15 TYPE ZWERTN,
         WERTN16 TYPE ZWERTN,
         WERTN17 TYPE ZWERTN,
         WERTN18 TYPE ZWERTN,
         WERTN19 TYPE ZWERTN,
         WERTN20 TYPE ZWERTN,
         WERTN21 TYPE ZWERTN,
         WERTN22 TYPE ZWERTN,
         WERTN23 TYPE ZWERTN,
         WERTN24 TYPE ZWERTN,
         WERTN25 TYPE ZWERTN,
         WERTN26 TYPE ZWERTN,
         WERTN27 TYPE ZWERTN,
         WERTN28 TYPE ZWERTN,
         WERTN29 TYPE ZWERTN,
         WERTN30 TYPE ZWERTN,
         TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV,
* UD1K941189 - by IG.MOON 8/1/2007 {
         KALNR   TYPE CK_KALNR,
* }

         STKKZ  TYPE STKKZ,
         MATNR2  TYPE MATNR,                                "UD1K949919
         MATNR1  TYPE MATNR,                                "UD1K949919
         MAKTX   TYPE MAKTX,                                "UD1K949919
         ZCATX   TYPE ZCATX,                                "UD1K949919
       END OF TY_OUT.

* UD1K941189 - by IG.MOON 8/1/2007 {
DATA: BEGIN OF IT_KAL_MAT OCCURS 0,
          KALNR TYPE CK_KALNR,
          MATNR TYPE MATNR,
END OF IT_KAL_MAT.

* }

DATA: GT_VC   TYPE TABLE OF TY_VC   WITH HEADER LINE,
      GT_VCIT TYPE TABLE OF TY_VCIT WITH HEADER LINE,
      GT_102  TYPE TABLE OF TY_102  WITH HEADER LINE,
      GT_103  TYPE TABLE OF TY_COST WITH HEADER LINE,
      GT_STD  TYPE TABLE OF TY_STD  WITH HEADER LINE,
*     GT_COST TYPE TABLE OF TY_COST WITH HEADER LINE,
      GT_FSC  TYPE TABLE OF TY_FSC  WITH HEADER LINE,
      GT_MAT  TYPE TABLE OF TY_MAT  WITH HEADER LINE,
      GT_OUT  TYPE TABLE OF TY_OUT  WITH HEADER LINE,
      gv_fsc_count  TYPE I,              " Count of FSC
      GV_IDX  TYPE I,
      GV_MCNT TYPE I.

RANGES R_MATNR FOR ZTCOU103-ARTNR.
DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

DATA  G_ERROR(1).

*- U1 Start
DATA: GT_ZTCOU103  TYPE TABLE OF TY_COST WITH HEADER LINE.
*- U1 End
