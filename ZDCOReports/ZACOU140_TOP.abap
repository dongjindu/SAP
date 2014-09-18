*----------------------------------------------------------------------*
*   INCLUDE ZACOU103_NEW_TOP
*----------------------------------------------------------------------*
TABLES: keko,      " Product Costing - Header Data
        ckis,      " Items Unit Costing/Itemization Product Costing
        tka01,     " Controlling Areas
        t001,
        ztco_ck11. "

TYPES: BEGIN OF ty_kis1,
         kalka  TYPE ck_kalka,        " Costing type
         kadky  TYPE ck_kadky,        " Costing date
         tvers  TYPE ck_tvers,        " Costing version
         bwvar  TYPE ck_bwvar,        " Valuation Variant in Costing
         posnr  TYPE ck_posnr,        " Unit costing line-item number
         typps  TYPE typps,           " Item category
         kstar  TYPE kstar,           " Cost element
         matnr  TYPE matnr,           " Material number
         gpreis TYPE ck_kpt,          " Unit Price
         peinh  TYPE ck_kpeinh,       " Price unit
         menge  TYPE menge_pos,       " Quantity
         meeht  TYPE meins,           " Base unit of measure
         ukaln  TYPE ck_ukaln,        " Cost estimate number
         stlan  TYPE stlan,           " BOM usage
         kalst  TYPE ck_kalst,        " Costing Level
         indx   TYPE sytabix,         " Index
         stkkz  TYPE stkkz,           " PM assembly indicator
         mtart  TYPE mtart,           " Material type
         matkl  type matkl,
         ausss  TYPE cs_e_ausss,      " Assembly Scrap
         kausf  TYPE cs_e_kausf,      " Component Scrap
         bausf  TYPE cs_e_bausd,      " Assembly Scrap Sued
         upgvc  TYPE matnr,           " UPG
         bwdat  TYPE ck_bwdat,        " Valuation date
         kalnr  TYPE ck_kalnr,        " cost est. no
         werks  LIKE ckis-werks,      " Plant
         baugr  LIKE ckis-baugr,      " assembly indicator
* UD1K941163 - by IG.MOON 7/31/2007 {
         wertn  LIKE ckis-wertn,      " Gross Amount
* }
* UD1K941492 by by IG.MOON 8/28/07  {
         verpr TYPE ck_pvprs_1,       " MAP price
*         verpr2 TYPE ck_pvprs_1,      " MAP price
         bwkey TYPE bwkey,
*}
         fevor TYPE fevor,             " production scheduler
*        $gpreis TYPE ck_kpt,          " Gross proce
*        $duty   TYPE ck_kpt,          " Duty
*        $frg    TYPE ck_kpt,          " Freight
*        $oth    TYPE ck_kpt,          " Other

        $flag(1),                     " for MAP price : No DCIN
       END OF ty_kis1.


* Internal table for UPG
TYPES: BEGIN OF ty_upg,
         idnrk TYPE idnrk,            " UPG
         maktx TYPE maktx,            " Description
      END OF ty_upg.

TYPES: BEGIN OF ty_itab,
        key(60).
        INCLUDE STRUCTURE ztco_ck11.
TYPES:  posnr TYPE sposn,             " BOM item number
        stgb  TYPE zstgb,             " Str.Type
        chk,
       END OF ty_itab.


TYPES: BEGIN OF ty_keko,
         kalnr TYPE ck_kalnr1,        " Cost estimate number
         werks TYPE werks_d,          " Plant
         bwdat TYPE ck_bwdat,         " Valuation date
         stlan TYPE stlan,            " BOM usage
         sobes TYPE sobes,            " Special procurement type
         sowrk TYPE ck_sowrk,         " Special procurements plant
         kalst TYPE ck_kalst,         " Costing Level
       END OF ty_keko.

DATA: BEGIN OF gt_bom OCCURS 0,
         index  LIKE stpox-index,     " Index
         stufe  LIKE stpox-stufe,     " Level
         disst  LIKE stpox-disst,     " low-level code
         idnrk  LIKE stpox-idnrk,     " Object(Mat)
         posnr  LIKE stpox-posnr,     " BOM item number
         hdnfo  LIKE stpox-hdnfo,     " Indicator: header info record
         mtart  LIKE stpox-mtart,     " mat type
         xchar  LIKE stpox-xchar,     " batch-mgt
         dumps  LIKE stpox-dumps,     " Phantom.
         stkkz  LIKE stpox-stkkz,     " assemble ind
         schgt  LIKE stpox-schgt,     " ind-bulk material
         mstae  LIKE stpox-mstae,     " mat-status
         mmsta  LIKE stpox-mmsta,     " mat-status(plant)
         menge  LIKE stpox-menge,     " Qty
         ausss  LIKE stpox-ausss,     " assembly scrap
         kausf  LIKE stpox-kausf,     " component scrap
         bausf  LIKE stpox-bausf,     " assembly scrap%
         meins  LIKE stpox-meins,     " UoM
         sobsl  LIKE stpox-sobsl,     " Special.Proc.
         rgekz  LIKE stpox-rgekz,     " b/f ind.
         lgpro  LIKE stpox-lgpro,     " S.Loc
         matmk  LIKE stpox-matmk,     " Mat.Group
         postp  LIKE stpox-postp,     " Type
         sortf  LIKE stpox-sortf,     " SortString
         stawn  LIKE stpox-stawn,     " Duty Code
         xtlty  LIKE stpox-xtlty,     " BOM category (next level)
         xtlnr  LIKE stpox-xtlnr,     " BOM no.
         eitm   LIKE stpox-eitm,      " EndItem
         stgb   LIKE stpox-stgb,      " Str.Type
         ojtxb  LIKE stpox-ojtxb,     " description
         upgn   LIKE stpox-upgn,      " user field - upg
         chk,
      END OF gt_bom.

TYPES : BEGIN OF ty_t030,
          bklas TYPE bklas,           " Valuation class
          konts TYPE saknr,           " G/L account number
        END OF ty_t030.

TYPES: BEGIN OF ty_ckis,
         kalnr TYPE ck_kalnr,         " Cost est number
         kstar TYPE kstar,            " Cost element
         hrkft TYPE hrkft,            " Origin Group
         wertn TYPE ck_kwt,           " Price
         ukaln TYPE ck_ukaln,     " Costing no. of entered cost estimate
         verpr TYPE ck_pvprs_1,
         matnr TYPE matnr,
       END OF ty_ckis.

* BY IG.MOON 10/11/2007 {
TYPES: BEGIN OF ty_ckiuser,
            bzobj TYPE ck_obj,
            kalnr TYPE ck_kalnr1,
            kalka TYPE ck_kalka,
            kadky TYPE ck_kadky,
            tvers TYPE ck_tvers,
            bwvar TYPE ck_bwvar,
            matnr TYPE matnr,
            werks TYPE werks_d,
            bwkey TYPE bwkey,
            bwtar TYPE bwtar_d,
            kokrs TYPE kokrs,
            kadat TYPE ck_kadky,
            aldat TYPE ck_brdat,
            verid TYPE verid,
            stlan TYPE stlan,
            stalt TYPE stalt,
            losgr TYPE ck_losgr,
            meins TYPE meins,
            disst TYPE disst,
            kalst TYPE ck_kalst,
            mgtyp TYPE ckml_mgtyp,
            bwdat TYPE ck_bwdat,
            poper TYPE poper,
            bdatj TYPE bdatj,
            btyp TYPE ckml_btyp,
            misch_verh TYPE ck_mischverh,
            bwvar_ba	TYPE ck_bwvar_ba,
            klvar TYPE ck_klvar,
            mkalk TYPE ck_mkalk,
            baltkz TYPE ck_balt_flag,
            kalaid TYPE ck_kalaid,
            hwaer  TYPE hwaer,

       END OF ty_ckiuser.
* }

DATA: stb         TYPE TABLE OF stpox    WITH HEADER LINE,

* BY IG.MOON 10/11/2007 {
*      F_CKIUSER   TYPE TABLE OF CKIUSER  WITH HEADER LINE,
*      F_MIXED     TYPE TABLE OF CKIUSER  WITH HEADER LINE,
*      F_CKIUSER1  TYPE TABLE OF CKIUSER  WITH HEADER LINE,
      f_ckiuser   TYPE TABLE OF ty_ckiuser  WITH HEADER LINE,
      f_mixed     TYPE TABLE OF ty_ckiuser  WITH HEADER LINE,
*      F_CKIUSER1  TYPE TABLE OF TY_CKIUSER  WITH HEADER LINE,
* }

      t_kis1      TYPE TABLE OF ty_kis1  WITH HEADER LINE,
      gt_keko     TYPE TABLE OF ty_keko  WITH HEADER LINE,
*      gt_ckis     TYPE TABLE OF ty_ckis  WITH HEADER LINE,  " Component
      itab        TYPE TABLE OF ty_itab  WITH HEADER LINE,
      itab_all    TYPE TABLE OF ZTCO_CK11 WITH HEADER LINE,
*     v_ztcou103  TYPE TABLE OF ty_103   WITH HEADER LINE,
*     v_db103     TYPE TABLE OF ty_103   WITH HEADER LINE,
      gt_100      TYPE TABLE OF ztcou100 WITH HEADER LINE,
      it_t030     TYPE TABLE OF ty_t030  WITH HEADER LINE,
      w_ukaln     LIKE ckis-ukaln,       " Cost est number
      w_wertn     LIKE kis1-wertn,       " Price
*      w_ckis      LIKE gt_ckis,
      w_upg       LIKE gt_bom.

DATA: BEGIN OF gt_locked OCCURS 0,
         artnr     LIKE ckmlmv001-matnr,
      END OF gt_locked.

DATA: gv_ratio TYPE p DECIMALS 4.
DATA: BEGIN OF gt_mixed OCCURS 0,
         werks       LIKE ckmlmv001-werks,
         matnr       LIKE ckmlmv001-matnr,
         btyp        LIKE ckmlmv001-btyp,
         kalnr       LIKE ckmlmv001-kalnr,
         misch_verh  LIKE ckmlmv003-misch_verh,
      END OF gt_mixed.


DATA: gv_changed,
      gv_diff   TYPE ck_kwt,              " Price Diff.
      gv_ddiff  TYPE ck_kwt,              " Duty Diff.
      gv_fdiff  TYPE ck_kwt,              " Freight Diff.
      gv_odiff  TYPE ck_kwt,              " Other Diff
      g_txt(80).

* by IG.MOON
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __e_exit.
  if g_error ne space.
    exit.
  endif.
END-OF-DEFINITION.

*TYPES: BEGIN OF TY_102,
*         VER   TYPE ZVER1,            " BP Ver
*         MATNR TYPE MATNR,            " Material number
*         BKLAS TYPE BKLAS,            " Valuation class
*         LIFNR TYPE LIFNR,            " Vendor
*         PEINH TYPE PEINH,            " Price unit
*         PMEHT TYPE PMEHT,            " Price quantity unit
*         WERTN TYPE ZWERTN1,          " Net Price
*         DUTY  TYPE ZDUTY1,           " Duty
*         FRG   TYPE ZFRG1,            " Freight
*         OTH   TYPE ZOTH1,            " Other
*         STAT  TYPE ZSTAT1,           " Status(C, O, R)
*       END OF TY_102.
*     GT_102      TYPE TABLE OF TY_102   WITH HEADER LINE,
*DATA : gt_102 LIKE ztcou102 OCCURS 0 WITH HEADER LINE,
*       gt_ztcou103 LIKE ztcou103 OCCURS 0 WITH HEADER LINE,
*       rt_ztcou103 LIKE ztcou103 OCCURS 0 WITH HEADER LINE.

** UD1K941492 by ig.moon 8/28/07
** {
*DATA ua_kalka LIKE keko-kalka.
*
DATA : start_date  TYPE sydatum,
       end_date  TYPE sydatum.


* }
