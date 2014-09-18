*----------------------------------------------------------------------*
*   INCLUDE ZACOU111_TOP                                               *
*----------------------------------------------------------------------*
TABLES: MARA,       " General Material Data
        MLHD,       " Material Ledger Document: Header
        CKMLHD,     " Material Ledger: Header Record
        T001K,      " Valuation area
        BSIS,       " Accounting: Secondary Index for G/L Accounts
        KONP,       " Conditions (Item)
        CKMLMV011.  " Costing run: Object list

RANGES     R_KSCHL FOR KONP-KSCHL.             " Condition type
CONSTANTS: C_KSCHL TYPE KSCHA   VALUE 'PB00',  " Condition type
           C_WAERS TYPE WAERS   VALUE 'USD',   " Currency
           C_EKORG TYPE EKORG   VALUE 'PU01',  " Purchase Org.
           C_WERKS TYPE WERKS_D VALUE 'P001',  " Plant

*          Reason code
           C_GR(3)  VALUE 'Z1',                " GR variance
           C_IV(3)  VALUE 'Z3',                " Invoice Variance
           C_LDC(3) VALUE 'Z5',                " LDC
           C_IM(3)  VALUE 'Z4',                " Import Variance
           C_RV(3)  VALUE 'Z6',                " Revaluation Variance
           C_ABP(3) VALUE 'ZA',                " ABP Variance
           C_PO(3)  VALUE 'ZP',                " PO Variance
           C_EXC(3) VALUE 'Z8',                " EXCHANGE RATE DIFF
           C_STD(3) VALUE 'ZS',                " STD Variance
           C_OTH(3) VALUE 'ZX'.                " GR other variance

DATA: W_VARIANT TYPE DISVARIANT,          " Variant
      GV_YEAR   TYPE BDATJ,               " Fiscal Year
      GV_POPER  TYPE POPER,               " Period
      GV_DATE   TYPE SYDATUM,             " Costing Date
      GV_PAR,                             " Pararell Check
      GV_PEINH  TYPE PEINH.               " Price unit

DATA: GV_DATE_F TYPE SYDATUM,             " from date
      GV_INFO_F TYPE SYDATUM,             " from date (info)
      GV_DATE_T TYPE SYDATUM,             " to date
      GV_DATE3  TYPE SYDATUM.             " next end

* for get plant
TYPES: BEGIN OF TY_T001K,
         BWKEY TYPE BWKEY,                " Plant
       END OF TY_T001K.

* for info-prices
TYPES: BEGIN OF TY_INFO,
         MATNR  TYPE MATNR,               " Material
         KNUMH  TYPE KNUMH,               " Condition No.
         DATAB  TYPE KODATAB,             " Start date
         DATBI  TYPE KODATBI,             " End date
         LIFNR  TYPE ELIFN,               " Vendor
       END OF TY_INFO.

TYPES: BEGIN OF TY_A018,
         KNUMH TYPE KNUMH,                " Condition No.
         DATAB TYPE KODATAB,              " Start date
         DATBI TYPE KODATBI,              " End date
         MATNR TYPE MATNR,                " Material
         LIFNR TYPE ELIFN,                " Vendor
       END OF TY_A018.

TYPES: BEGIN OF TY_KNUMH,
         MATNR TYPE MATNR,                " Material
         LIFNR TYPE LIFNR,                " Vendor
         KNUMH TYPE KNUMH,                " Condition No.
         DATAB TYPE DATAB,                " Start Date
         DATBI TYPE DATBI,                " End Date
         EKGRP TYPE BKGRP,                " Pur.Grp
       END OF TY_KNUMH.

TYPES: BEGIN OF TY_CONDI,
         KNUMH TYPE KNUMH,                " Condition No.
         DATAB TYPE DATAB,                " Start date
         DATBI TYPE DATBI,                " End date
         KZUST TYPE KZUST,                " Reason
         KPEIN TYPE KPEIN,                " Condition pricing unit
         MEINS TYPE MEINS,                " Base unit of measure
         KBETR TYPE KBETR_KOND,           " Price
         MATNR TYPE MATNR,                " Material
         LIFNR TYPE LIFNR,                " Vendor
       END OF TY_CONDI.

* for GR & Invoice
TYPES: BEGIN OF TY_DOC,
        BELNR     TYPE BELNR_D,           " Accounting document number
        KJAHR     TYPE KJAHR,             " Fiscal year
        VGART     TYPE CK_VGART,          " Transaction type
        AWTYP     TYPE AWTYP,             " Ref Procedure
        AWREF     TYPE AWREF,             " Ref Doc
        BLDAT     TYPE BLDAT,             " document date
        KATEGORIE TYPE CKML_KATEGORIE,    " category
        PTYP      TYPE CK_PTYP_ORG,       " Original process category
        URZEILE   TYPE CK_URZEILE,        " MSEG; org.doc.item
        ZEILE     TYPE MBLPO ,            " MSEG; org.doc.item
        BUZEI     TYPE RBLGP,             " MSEG; org.doc.item
        MATNR     TYPE MATNR,             " Material
        BWKEY     TYPE BWKEY,             " Valuation area
        MEINS     TYPE MEINS,             " Unit
        KALNR     TYPE CK_KALNR,          " Cost est number
        BUKRS     TYPE BUKRS,             " Company Code
        PSART     TYPE CK_PSART,          " Item type in ML
        LIFNR     TYPE LIFNR,             " Vendor
        WAERS     TYPE WAERS,             " Currency
        SALK3     TYPE CK_SALK3D,         " GR Amount
        PEINH     TYPE PEINH,             " Price Unit
        BDATJ     TYPE BDATJ,             " Posting year
        POPER     TYPE POPER,             " Posting period
        MENGE     TYPE CK_FMENGE,         " Quantity
        FELDG     TYPE CK_FELDG,          " Field group
        PRDIF     TYPE CK_PRDIF,          " Price Diff.
        KRDIF     TYPE CK_KRDIF,          " Value of exchange rate
        POSDATE   TYPE BLDAT,             " Posting date
        REMARK(40),                       " Comment
        KSCHL     TYPE KSCHL,             " Condition
        EBELN     TYPE EBELN,             " Po No.
        EBELP     TYPE EBELP,             " Po Item No.
        CHK(1)      TYPE C,
     END OF TY_DOC.

* for ABP
TYPES: BEGIN OF TY_ABP,
         KOKRS TYPE KOKRS,                " Controlling area
         BDATJ TYPE BDATJ,                " Fiscal year
         POPER TYPE POPER,                " Period
         MATNR TYPE MATNR,                " Material
         LIFNR TYPE LIFNR,                " Vendor
         KZUST TYPE KZUST,                " Reason
         WERTN TYPE ZWERTN1,              " ABP price
         PMEHT TYPE PMEHT,                " UoM
         PEINH TYPE PEINH,                " Price unit
       END OF TY_ABP.

* for Revaluation
TYPES: BEGIN OF TY_BSIS,
         BUKRS TYPE BUKRS,                " Company code
         GJAHR TYPE GJAHR,                " Fiscal year
         MONAT TYPE MONAT,                " Fiscal period
         ZUONR TYPE DZUONR,               " Assignment number
         BELNR TYPE BELNR_D,              " Accounting document number
         BUZEI TYPE BUZEI,                " Line item number
         DMBTR TYPE DMBTR,                " Amount in local currency
         bschl type bschl,  "dr/cr
       END OF TY_BSIS.

TYPES: BEGIN OF TY_RV.
         INCLUDE TYPE TY_BSIS.
TYPES:   BWKEY TYPE BWKEY,                " Plant
         PEINH TYPE PEINH,                " Price unit
         EBELN TYPE EBELN,                " PO No.
         EBELP TYPE EBELP,                " PO Item No.
         MENGE TYPE MENGE_D,              " PO Qty.
         MEINS TYPE MEINS,                " Base unit of measure
         KSCHL TYPE KSCHL,                " Condition type
         LIFNR TYPE LIFNR,                " Vendor
       END OF TY_RV.

TYPES: BEGIN OF TY_EINA,
         MATNR TYPE MATNR,                " Mterial
         LIFNR TYPE LIFNR,                " Vendor
         EKGRP TYPE EKGRP,                " Purchase Org.
       END OF TY_EINA.

TYPES: BEGIN OF TY_OUT.
         INCLUDE STRUCTURE ZTCOU111.
TYPES: END OF TY_OUT.

TYPES: BEGIN OF S_MATS,
        KALNR    TYPE CK_KALNR,           " Cost est number
        MATNR    TYPE MATNR,              " Material
        WERKS    TYPE WERKS_D,            " Plant
        MTART    TYPE MTART,              " Material type
        MATKL    TYPE MATKL,              " Material class
        KALST    TYPE CK_KALST,           " Costing Level
        BKLAS    TYPE BKLAS,              " val class
        STPRS    TYPE STPRS,              " Standard price
        VERPR    TYPE VERPR,              " Moving average price
        PEINH    TYPE CK_PEINH_1,         " Price unit
        MEINS    TYPE MEINS,              " Base unit of measure
        ZUKUMO   TYPE CK_ZUKUM,           " GR
        EKKUMO   TYPE CK_EKKUM,           " PO GR
   END OF S_MATS,
   TY_MATS TYPE STANDARD TABLE OF S_MATS WITH KEY KALNR.

TYPES: BEGIN OF TY_105,
         TMATNR TYPE MATNR,             " to material
         FMATNR TYPE MATNR,             " from material
       END OF TY_105.

DATA: GT_MAT   TYPE TY_MATS           WITH HEADER LINE,
      GT_INFO  TYPE TABLE OF TY_INFO  WITH HEADER LINE,
      GT_105   TYPE TABLE OF TY_105   WITH HEADER LINE,
      GT_A018  TYPE TABLE OF TY_A018  WITH HEADER LINE,
      GT_MLDOC TYPE TABLE OF TY_DOC   WITH HEADER LINE,
      IT_MLDOC TYPE TABLE OF TY_DOC   WITH HEADER LINE,
      GT_BSIS  TYPE TABLE OF TY_BSIS  WITH HEADER LINE,
      GT_RV    TYPE TABLE OF TY_RV    WITH HEADER LINE,
      ITAB     TYPE TABLE OF TY_OUT   WITH HEADER LINE,
      GT_OUT   TYPE TABLE OF TY_OUT   WITH HEADER LINE,
      GT_DET   TYPE TABLE OF ZTCOU112 WITH HEADER LINE,
      GT_ABP   TYPE TABLE OF TY_ABP   WITH HEADER LINE,
      GT_KNUMH TYPE TABLE OF TY_KNUMH WITH HEADER LINE,
      GT_CONDI TYPE TABLE OF TY_CONDI WITH HEADER LINE,
      GT_EINA  TYPE TABLE OF TY_EINA  WITH HEADER LINE,
      GS_OUT   LIKE GT_OUT.

RANGES: R_BWKEY FOR T001K-BWKEY,           " Plant
        R_PTYP  FOR MLIT-PTYP.             " Process category
