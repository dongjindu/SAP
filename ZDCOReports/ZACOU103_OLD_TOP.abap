*----------------------------------------------------------------------*
*   INCLUDE ZACOU103_TOP                                               *
*----------------------------------------------------------------------*
TABLES: KEKO,      " Product Costing - Header Data
        CKIS,      " Items Unit Costing/Itemization Product Costing
        TKA01,     " Controlling Areas
        ZTCOU103.  " [CO] Cost Roll-Up

TYPES: BEGIN OF TY_KIS1,
         KALKA  TYPE CK_KALKA,        " Costing type
         KADKY  TYPE CK_KADKY,        " Costing date
         TVERS  TYPE CK_TVERS,        " Costing version
         BWVAR  TYPE CK_BWVAR,        " Valuation Variant in Costing
         POSNR  TYPE CK_POSNR,        " Unit costing line-item number
         TYPPS  TYPE TYPPS,           " Item category
         KSTAR  TYPE KSTAR,           " Cost element
         MATNR  TYPE MATNR,           " Material number
         GPREIS TYPE CK_KPT,          " Unit Price
         PEINH  TYPE CK_KPEINH,       " Price unit
         MENGE  TYPE MENGE_POS,       " Quantity
         MEEHT  TYPE MEINS,           " Base unit of measure
         UKALN  TYPE CK_UKALN,        " Cost estimate number
         STLAN  TYPE STLAN,           " BOM usage
         KALST  TYPE CK_KALST,        " Costing Level
         INDX   TYPE SYTABIX,         " Index
         STKKZ  TYPE STKKZ,           " PM assembly indicator
         MTART  TYPE MTART,           " Material type
         AUSSS  TYPE CS_E_AUSSS,      " Assembly Scrap
         KAUSF  TYPE CS_E_KAUSF,      " Component Scrap
         BAUSF  TYPE CS_E_BAUSD,      " Assembly Scrap Sued
         UPGVC  TYPE MATNR,           " UPG
         BWDAT  TYPE CK_BWDAT,        " Valuation date
       END OF TY_KIS1.

* Internal table for UPG
TYPES: BEGIN OF TY_UPG,
         IDNRK TYPE IDNRK,            " UPG
         MAKTX TYPE MAKTX,            " Description
      END OF TY_UPG.

TYPES: BEGIN OF TY_ITAB,
        KEY(60).
        INCLUDE STRUCTURE ZTCOU103.
TYPES:  POSNR TYPE SPOSN,             " BOM item number
        stgb  type ZSTGB,             " Str.Type
        CHK,
       END OF TY_ITAB.

TYPES: BEGIN OF TY_102,
         VER   TYPE ZVER1,            " BP Ver
         MATNR TYPE MATNR,            " Material number
         BKLAS TYPE BKLAS,            " Valuation class
         LIFNR TYPE LIFNR,            " Vendor
         PEINH TYPE PEINH,            " Price unit
         PMEHT TYPE PMEHT,            " Price quantity unit
         WERTN TYPE ZWERTN1,          " Net Price
         DUTY  TYPE ZDUTY1,           " Duty
         FRG   TYPE ZFRG1,            " Freight
         OTH   TYPE ZOTH1,            " Other
         STAT  TYPE ZSTAT1,           " Status(C, O, R)
       END OF TY_102.

TYPES: BEGIN OF TY_KEKO,
         KALNR TYPE CK_KALNR1,        " Cost estimate number
         WERKS TYPE WERKS_D,          " Plant
         BWDAT TYPE CK_BWDAT,         " Valuation date
         STLAN TYPE STLAN,            " BOM usage
         SOBES TYPE SOBES,            " Special procurement type
         SOWRK TYPE CK_SOWRK,         " Special procurements plant
         KALST TYPE CK_KALST,         " Costing Level
       END OF TY_KEKO.

DATA: BEGIN OF GT_BOM OCCURS 0,
         INDEX  LIKE STPOX-INDEX,     " Index
         STUFE  LIKE STPOX-STUFE,     " Level
         DISST  LIKE STPOX-DISST,     " low-level code
         IDNRK  LIKE STPOX-IDNRK,     " Object(Mat)
         POSNR  LIKE STPOX-POSNR,     " BOM item number
         HDNFO  LIKE STPOX-HDNFO,     " Indicator: header info record
         MTART  LIKE STPOX-MTART,     " mat type
         XCHAR  LIKE STPOX-XCHAR,     " batch-mgt
         DUMPS  LIKE STPOX-DUMPS,     " Phantom.
         STKKZ  LIKE STPOX-STKKZ,     " assemble ind
         SCHGT  LIKE STPOX-SCHGT,     " ind-bulk material
         MSTAE  LIKE STPOX-MSTAE,     " mat-status
         MMSTA  LIKE STPOX-MMSTA,     " mat-status(plant)
         MENGE  LIKE STPOX-MENGE,     " Qty
         AUSSS  LIKE STPOX-AUSSS,     " assembly scrap
         KAUSF  LIKE STPOX-KAUSF,     " component scrap
         BAUSF  LIKE STPOX-BAUSF,     " assembly scrap%
         MEINS  LIKE STPOX-MEINS,     " UoM
         SOBSL  LIKE STPOX-SOBSL,     " Special.Proc.
         RGEKZ  LIKE STPOX-RGEKZ,     " b/f ind.
         LGPRO  LIKE STPOX-LGPRO,     " S.Loc
         MATMK  LIKE STPOX-MATMK,     " Mat.Group
         POSTP  LIKE STPOX-POSTP,     " Type
         SORTF  LIKE STPOX-SORTF,     " SortString
         STAWN  LIKE STPOX-STAWN,     " Duty Code
         XTLTY  LIKE STPOX-XTLTY,     " BOM category (next level)
         XTLNR  LIKE STPOX-XTLNR,     " BOM no.
         EITM   LIKE STPOX-EITM,      " EndItem
         STGB   LIKE STPOX-STGB,      " Str.Type
         OJTXB  LIKE STPOX-OJTXB,     " description
         CHK,
      END OF GT_BOM.

TYPES : BEGIN OF TY_T030,
          BKLAS TYPE BKLAS,           " Valuation class
          KONTS TYPE SAKNR,           " G/L account number
        END OF TY_T030.

TYPES: BEGIN OF TY_CKIS,
         KALNR TYPE CK_KALNR,         " Cost est number
         KSTAR TYPE KSTAR,            " Cost element
         HRKFT TYPE HRKFT,            " Origin Group
         WERTN TYPE CK_KWT,           " Price
         UKALN TYPE CK_UKALN,     " Costing no. of entered cost estimate
       END OF TY_CKIS.

DATA: STB         TYPE TABLE OF STPOX    WITH HEADER LINE,
      F_CKIUSER   TYPE TABLE OF CKIUSER  WITH HEADER LINE,
      F_CKIUSER1  TYPE TABLE OF CKIUSER  WITH HEADER LINE,
      T_KIS1      TYPE TABLE OF TY_KIS1  WITH HEADER LINE,
      GT_KEKO     TYPE TABLE OF TY_KEKO  WITH HEADER LINE,
      GT_CKIS     TYPE TABLE OF TY_CKIS  WITH HEADER LINE,  " Component
      ITAB        TYPE TABLE OF TY_ITAB  WITH HEADER LINE,
      ITAB_ALL    TYPE TABLE OF ZTCOU103 WITH HEADER LINE,
      GT_102      TYPE TABLE OF TY_102   WITH HEADER LINE,
      V_ZTCOU103  TYPE TABLE OF ZTCOU103 WITH HEADER LINE,
      GT_100      TYPE TABLE OF ZTCOU100 WITH HEADER LINE,
      IT_T030     TYPE TABLE OF TY_T030  WITH HEADER LINE,
      W_UKALN     LIKE CKIS-UKALN,       " Cost est number
      W_WERTN     LIKE KIS1-WERTN,       " Price
      W_CKIS      LIKE GT_CKIS,
      W_UPG       LIKE GT_BOM.

DATA: GV_CHANGED,
      GV_DIFF   TYPE CK_KWT,              " Price Diff.
      GV_DDIFF  TYPE CK_KWT,              " Duty Diff.
      GV_FDIFF  TYPE CK_KWT,              " Freight Diff.
      GV_ODIFF  TYPE CK_KWT,              " Other Diff
      G_TXT(80).

data: begin of gt_missing occurs 0,
        matnr  type matnr,
      end of gt_missing.
