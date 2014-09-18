*----------------------------------------------------------------------*
*   INCLUDE ZACOU106_TOP                                               *
*----------------------------------------------------------------------*
TABLES: ZTCOU104,     " [CO] Variance Analysis Code
        ZTCOU105,
        ZTCOU106,     " [CO] Calculate Variances
        ztcou104lock, *ztcou104lock.

TYPES: BEGIN OF      TY_ZTCOU102,
         BDATJ       TYPE BDATJ,             " Fiscal Year
         POPER       TYPE POPER,             " Period
         KALKA       TYPE CK_KALKA,          " Costing type
         MATNR       TYPE MATNR,             " Material
         WERKS       TYPE WERKS_D,           " Plant
         PROFL       TYPE PROFL,             " Source
         LIFNR       TYPE LIFNR,             " Vendor
         LAND1       TYPE LAND1,             " Country key
         WERTN       TYPE ZWERTN1,           " Net Price
         PMEHT       TYPE MEINS,             " UoM
         PEINH       TYPE PEINH,             " Price unit
         QTA	       TYPE ZQTA,           " Quota
         WERTN_V1    TYPE ZWERTN1,        " Net Price
         KZUST1	TYPE KZUST,         " Reason1
         WERTN1	TYPE ZWERTD,        " Diff$1
         KZUST2	TYPE KZUST,         " Reason2
         WERTN2	TYPE ZWERTD,        " Diff$2
         QTA_V2	TYPE ZQTA,           " Quota of 2nd vendor
         WERTN_V2	TYPE ZWERTN1,        " Net Price of 2nd vendor
         KZUST1_V2	TYPE KZUST,          " Reason1 of 2nd vendor
         WERTN1_V2	TYPE ZWERTD,         " Diff$1 of 2nd vendor
         KZUST2_V2	TYPE KZUST,          " Reason2 of 2nd vendor
         WERTN2_V2	TYPE ZWERTD,         " Diff$2 of 2nd vendor
         EKGRP      TYPE EKGRP,
         SOURCE(1)   TYPE C,              "source:102, Info
       END OF TY_ZTCOU102.

TYPES: BEGIN OF TY_ZTCOU103,
         BDATJ  TYPE BDATJ,                  " Fiscal Year
         POPER  TYPE POPER,                  " Period
         KALKA  TYPE CK_KALKA,               " Costing type
         ARTNR  TYPE ARTNR,                  " Product
         COMPN  TYPE IDNRK,                  " Component
         KSTAR  TYPE KSTAR,                  " Cost element
         UPGVC  TYPE ZUPGVC,                 " UPG
         MENGE  TYPE MENGE_POS,              " qty
         MEEHT  TYPE MEINS,                  " UoM
         GPREIS TYPE ZGPREIS,                " Price Unit
         WERTN  TYPE ZWERTN,                 " Price
         PEINH  TYPE CK_KPEINH,              " Unit of price
         SPLNT  TYPE WERKQ,                  " Supplying plant (source)
         BWDAT  TYPE CK_BWDAT,               " Valuation date
         STKKZ  TYPE STKKZ,                  " assembly indicator
         LOSGR  TYPE CK_LOSGR,
       END OF TY_ZTCOU103.

TYPES: BEGIN OF TY_ZTCOU116,
         KOKRS TYPE KOKRS,                   " Controling Area
         BDATJ TYPE BDATJ,                   " Fiscal Year
         LAND1 TYPE LAND1,                   " Country key
       END OF TY_ZTCOU116.

TYPES: BEGIN OF TY_GT_103A.
TYPES:   ID TYPE ZID1.                       " ID
INCLUDE TYPE TY_ZTCOU103.
TYPES: END OF TY_GT_103A.

TYPES: BEGIN OF TY_GT_103C,
         ID      TYPE ZID1,                  " ID
*         BDATJ   TYPE BDATJ,                 " Fiscal Year
*         POPER   TYPE POPER,                 " Period
         UPGVC   TYPE ZUPGVC,                " UPG
*         ARTNR   TYPE ARTNR,                 " Product
         COMPN   TYPE IDNRK,                 " Component
         KSTAR   TYPE KSTAR,                 " Cost element
       END OF TY_GT_103C.

TYPES: BEGIN OF TY_ZTCOU105,
         TMATNR TYPE MATNR,                  " To Material
         FMATNR TYPE MATNR,                  " From Material
       END OF TY_ZTCOU105.

TYPES: BEGIN OF TY_ALL,
         BDATJ   TYPE BDATJ,                 " Fiscal Year
         KALKA   TYPE CK_KALKA,              " Costing Type
         ID      TYPE ZID1,                  " ID
         UPGVC   TYPE ZUPGVC,                " UPG
         COMPN   TYPE IDNRK,                 " Component
         KSTAR   TYPE KSTAR,                 " Cost element
         KD,                                 " KD
         MIP,                                " MIP
         KZUST   TYPE KZUST,                 " Reason
         ARTNR   TYPE ARTNR,                 " Product
         PROFL   TYPE ADGE_PROFL,            " Source

         LIFNR   TYPE LIFNR,                 " Vendor
         MENGE   TYPE MENGE_POS,             " Qty
         MEEHT   TYPE MEINS,                 " UoM
         PMEHT   TYPE MEINS,                 " UoM(Price quantity unit)
         GPREIS  TYPE ZGPREIS,               " Unit Price
         KPEIN   TYPE KPEIN,                 " Unit
         WERTN   TYPE ZWERTN,                " Price

*        PPOPER  TYPE POPER,                 " Prv.Period
         PLIFNR  TYPE LIFNR,                 " Prv.Vendor
         PMENGE  TYPE MENGE_POS,             " Prv.Price qty
         PMEEHT  TYPE MEINS,                 " Prv.Price qty unit
         PPMEHT  TYPE MEINS,                 " Prv.UoM
         PGPREIS TYPE ZGPREIS,               " Prv.Price
         PKPEIN  TYPE KPEIN,                 " Unit
         PWERTN  TYPE ZWERTN,                " Prv.Price

         KZUST2  TYPE KZUST,                                " Reason2

        $FLAG(1),
        $COMPN   TYPE IDNRK,                 " Component
        $GPREIS  TYPE ZGPREIS,               " Unit Price
         EKGRP_C  TYPE  EKGRP,                 " Purchasing group
         EKGRP_P  TYPE  EKGRP,                 " Purchasing group
         OKZUST_C   TYPE  ZOKZUST,         " Original Reason in ZTCOU102
         OKZUST_P   TYPE  ZOKZUST,         " Original Reason in ZTCOU102
       END OF TY_ALL.

TYPES: BEGIN OF TY_A018,
         MATNR TYPE MATNR,                   " Material
         LIFNR TYPE LIFNR,                   " Vendor
         DATBI TYPE KODATBI,                 " End date
         DATAB TYPE KODATAB,                 " Start date
         KNUMH TYPE KNUMH,                   " Condition no.
         KBETR TYPE KBETR,            " Rate (condition amount)
         KPEIN TYPE KPEIN,            " Condition pricing unit
       END OF TY_A018.

DATA: GT_102 TYPE TABLE OF TY_ZTCOU102 WITH HEADER LINE,
      GT_103 TYPE TABLE OF TY_ZTCOU103 WITH HEADER LINE,

      GT_103A     TYPE TABLE OF TY_GT_103A  WITH HEADER LINE,
      GT_103C     TYPE TABLE OF TY_GT_103C  WITH HEADER LINE,
      GT_ALL      TYPE TABLE OF TY_ALL      WITH HEADER LINE,

      GT_ZTCOU105 TYPE TABLE OF TY_ZTCOU105 WITH HEADER LINE,
      GT_ZTCOU116 TYPE TABLE OF TY_ZTCOU116 WITH HEADER LINE,
      GT_OUT      TYPE TABLE OF ZTCOU106    WITH HEADER LINE,
      gt_outwk  TYPE TABLE OF ZTCOU106    WITH HEADER LINE,
      GT_A018     TYPE TABLE OF TY_A018     WITH HEADER LINE.

DATA: BEGIN OF GT_104 OCCURS 0,
        CYEAR   TYPE GJAHR,
        CPOPER  TYPE POPER,
        CFSC    TYPE MATNR,
        PYEAR   TYPE GJAHR,
        PPOPER  TYPE POPER,
        PFSC    TYPE MATNR,
        PKALKA  TYPE CK_KALKA.
        INCLUDE STRUCTURE ZTCOU104.
DATA: END OF GT_104.

DATA: GV_CNT(5),
      GV_RC  LIKE SY-SUBRC.

DATA: GV_PKALKA  TYPE CK_KALKA,       " Previous costing type
*      GV_PBDATJ  TYPE BDATJ,          " Previous Year
*      GV_PPOPER  TYPE POPER,          " Previous Period
*      GV_PFSC    TYPE ARTNR,          " Previous FSC

      GV_KALKA   TYPE CK_KALKA,       " Current costing type
      GV_POPER   TYPE POPER,          " Current Period
*      GV_BDATJ   TYPE BDATJ,          " Current Year
*      GV_FSC     TYPE ARTNR,          " Current FSC
      GV_MATNR   TYPE MATNR.          " for Alt. Material

CONSTANTS: C_EKORG       LIKE EKKO-EKORG  VALUE 'PU01'. " Purchase Org.
*CONSTANTS: C_KZUST_ROUND TYPE KZUST       VALUE 'ZER'.
*CONSTANTS: C_KZUST_ABP   TYPE KZUST       VALUE 'ZA'.
