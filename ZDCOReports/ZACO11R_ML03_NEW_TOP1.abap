*----------------------------------------------------------------------*
*   INCLUDE ZACO11R_ML03_NEW_TOP                                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   Macro
*----------------------------------------------------------------------*
DEFINE DEF_VAL.
  DATA :
          &1_LBKUM LIKE MLCD-LBKUM,
          &1_SALK3 LIKE MLCD-SALK3,
          &1_RD    LIKE MLCD-SALK3.
END-OF-DEFINITION .

* To set Values from MLCD containers
DEFINE MLDR_SET_VAL_PROCCAT.
  IT_MLIT-&1_&2_LBKUM = &3-LBKUM.
  IT_MLIT-&1_&2_SALK3 = &3-SALK3.
  IT_MLIT-&1_&2_RD    =
                            &3-ESTPRD
                          + &3-ESTKDM
                          + &3-MSTPRD
                          + &3-MSTKDM
                          + &3-TPPRD.
END-OF-DEFINITION .

* By MVT GRP
DEFINE MLDR_SET_VAL_MVTGRP.
  IT_MLIT-&1_&2_LBKUM = &3-LBKUM * ( -1 ).
  IT_MLIT-&1_&2_SALK3 = &3-SALK3 * ( -1 ).
END-OF-DEFINITION .
* By Andy
DEFINE MLDR_SET_VAL_MVTGRP_R.
  IT_MLIT-&1_&2_LBKUM = &3-LBKUM.
  IT_MLIT-&1_&2_SALK3 = &3-SALK3.
END-OF-DEFINITION .

* For Cal. VKA
DEFINE MLDR_CAL_VKA.
* VKA.
*  it_mlit-vn_vka_&1
*   = it_mlit-vn_vka_&1
*  - ( it_mlit-vn_&2_&1 + it_mlit-vn_&3_&1 ).
* Begin of changes - UD1K918937
  IT_MLIT-VN_VKA_&1
   =  ( IT_MLIT-VN_VKA_&1 + IT_MLIT-VN_&2_&1 +
     IT_MLIT-VN_&3_&1 )
  - ( IT_MLIT-VN_&2_&1 + IT_MLIT-VN_&3_&1 ).
* End of changes - UD1K918937
END-OF-DEFINITION .


* For Substraction from VN ETC
DEFINE MLDR_SUB_VN_ETC.
*  it_mlit-vn_etc_&1
*               = it_mlit-vn_etc_&1
*               - it_mlit-vn_6665_&1
*               - it_mlit-vn_5857_&1
*               - it_mlit-vn_5655_&1
*               - it_mlit-vn_6059_&1
*               - it_mlit-vn_6463_&1.
* Begin of changes - UD1K918937
  IT_MLIT-VN_ETC_&1
               = ( IT_MLIT-VN_ETC_&1
               + IT_MLIT-VN_OR66_&1
               + IT_MLIT-VN_OR58_&1
               + IT_MLIT-VN_CC56_&1
               + IT_MLIT-VN_CC60_&1
               + IT_MLIT-VN_CC64_&1 )
               -
               (  IT_MLIT-VN_OR66_&1
                + IT_MLIT-VN_OR58_&1
                + IT_MLIT-VN_CC56_&1
                + IT_MLIT-VN_CC60_&1
                + IT_MLIT-VN_CC64_&1 ).


* End of changes - UD1K918937
END-OF-DEFINITION .


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Type-Pools
TYPE-POOLS : CKMV0, SLIS.
TYPE-POOLS : ZCOT1.

** Tables
TABLES : CKMLPP, CKMLCR, CKMLHD, CKMLCT.
TABLES : MACKU, MARC, MARA.
TABLES : MLCD_KEY, MLCD, MLCR, MLPP,
         MLHD, MLIT,MBEWH.
TABLES : T001W, MBEW.
TABLES : ZTCO_MLIT, ZTCO_MLI2, ZTCO_MLII.
TABLES : ZVCO_MLXXV.

**  Internal Tables
* For MLCD
DATA : IT_KALNR	  TYPE CKMV0_MATOBJ_TBL WITH HEADER LINE.
DATA : BEGIN OF IT_MLCD OCCURS 1000,
*       INCLUDE STRUCTURE mlcd.
        KALNR   LIKE MLCD-KALNR ,
        BDATJ   LIKE MLCD-BDATJ ,
        POPER   LIKE MLCD-POPER ,
*        UNTPER  like MLCD-UNTPER,
        CATEG(7),
        PTYP    LIKE MLCD-PTYP  ,
        BVALT   LIKE MLCD-BVALT ,
        CURTP   LIKE MLCD-CURTP ,
        OBJECT  LIKE MARA-MATNR,  "partner material
        LBKUM   LIKE MLCD-LBKUM ,
        MEINS   LIKE MLCD-MEINS ,
        SALK3   LIKE MLCD-SALK3 ,
        ESTPRD  LIKE MLCD-ESTPRD,
        ESTKDM  LIKE MLCD-ESTKDM,
        MSTPRD  LIKE MLCD-MSTPRD,
        MSTKDM  LIKE MLCD-MSTKDM,
        WAERS   LIKE MLCD-WAERS ,
        TPPRD   LIKE MLCD-TPPRD ,
       END OF   IT_MLCD.
* For Display
DATA : BEGIN OF IT_MLIT OCCURS 500,
        BUKRS LIKE T001K-BUKRS,
        BDATJ LIKE MLCD-BDATJ,
        POPER LIKE MLCD-POPER,
        MTART LIKE MARA-MTART,
        MATNR LIKE CKMLHD-MATNR,
        BWKEY LIKE CKMLHD-BWKEY,
        BWTAR LIKE CKMLHD-BWTAR,
        MEINS LIKE MLCD-MEINS,
        WAERS LIKE MLCD-WAERS.
*Value Part
DEF_VAL AB.  "AB	Beginning inventory
DEF_VAL PC.  "PC	Price changes
DEF_VAL ZU.  "ZU	Receipts
DEF_VAL ZV.  "B+	Inv Diff 712
DEF_VAL ZO.  "ZO	Other receipts
DEF_VAL BU.  "BU     Stock trf
DEF_VAL BM.                                                 "BM     M2M
DEF_VAL VP.  "VP "DC     debit/credit
DEF_VAL ND.  "ND	Not distributed
DEF_VAL KB.  "KB	Cumulative inventory

DEF_VAL VN.  "VN	Normal Consumption
DEF_VAL VO.  "VO	consumption order
DEF_VAL VK.  "VK     consumption cc
DEF_VAL VV.  "V+     Inv diff 711
DEF_VAL VU.  "VU     stock transfer
DEF_VAL VM.                                                 "VM  M2M
DEF_VAL NI.  "NC	Not allocated/Included
DEF_VAL EB.  "EB	Ending inventory

*DEF_VAL ZU_VP.
DEF_VAL ZU_BB.
DEF_VAL ZU_BF.
DEF_VAL ZU_BL.
DEF_VAL ZU_BUBS.
DEF_VAL ZU_BUBM.
DEF_VAL VN_CC60.
DEF_VAL ZU_FRE.
DEF_VAL ZU_ETC.

*DEF_VAL VN_VKA.
DEF_VAL VN_VF.
DEF_VAL VN_VL.
DEF_VAL VN_COB.
DEF_VAL VN_VK.
*DEF_VAL VN_OR.
DEF_VAL VN_VUBS.
DEF_VAL VN_VUBM.
DEF_VAL VN_VKA16.
DEF_VAL VN_VKA20.
DEF_VAL VN_OR66.
DEF_VAL VN_CC99.
DEF_VAL VN_OR58.
*DEF_VAL ZU_PC.
*DEF_VAL VN_VKA99.

DATA :  BKLAS LIKE MBEW-BKLAS,
        MAKTG LIKE MAKT-MAKTG,
        KALNR LIKE MLCD-KALNR,
        ERDAT TYPE ERDAT,
        ERZET TYPE ERZET,
        ERNAM TYPE ERNAM,
       END OF IT_MLIT.

DATA IT_MLIT1 LIKE IT_MLIT OCCURS 0 WITH HEADER LINE.

* For Detail Report
*DATA : it_mlit  LIKE STANDARD TABLE OF ztco_mlit
*                          WITH HEADER LINE
*                          INITIAL SIZE 3000.
*DATA : it_mlit LIKE STANDARD TABLE OF ztco_mlit
*                          WITH HEADER LINE
*                          INITIAL SIZE 3000.

* For ZVCO_MLXXV - Data by MVTGRP
*MLHD
*MLIT
*MLPP
*MLCR
*MLPPF
DATA : BEGIN OF IT_ZVCO_MLXXV OCCURS 1000,
         BELNR       LIKE MLCR-BELNR     ,
         KJAHR       LIKE MLCR-KJAHR     ,
         POSNR       LIKE MLCR-POSNR     ,
         BDATJ       LIKE MLCR-BDATJ     ,
         POPER       LIKE MLCR-POPER     ,

         AWREF       LIKE MLHD-AWREF     ,
         AWORG       LIKE MLHD-AWORG     ,

         MATNR       LIKE MLIT-MATNR     ,
         BWKEY       LIKE MLIT-BWKEY     ,

         KALNR       LIKE MLIT-KALNR     ,
         MEINS       LIKE MLIT-MEINS     ,
         WAERS       LIKE MLCR-WAERS     ,

*         PSART       LIKE MLIT-PSART     , "Item type
*         MLAST       LIKE MLIT-MLAST     , "2/3
*         STORNO      LIKE MLHD-STORNO    , "Ind:reversal
*         STATUS      LIKE MLPP-STATUS    , "period status
*         XABRERR     LIKE MLIT-XABRERR   , "Ind:Err last time
         BEWARTGRP   LIKE MLIT-BEWARTGRP , "MvType Grp
         KATEGORIE   LIKE MLIT-KATEGORIE , "Category in ML
         PTYP        LIKE MLIT-PTYP      , "Original process cat.
*         PTYP_KAT    LIKE MLIT-PTYP_KAT  , "Process cat.det.
*         PTYP_BVALT  LIKE MLIT-PTYP_BVALT, "procurement alt.
*         PTYP_PROC   LIKE MLIT-PTYP_PROC , "the process
*         FELDG       LIKE MLPPF-FELDG    , "Field Group
         LBKUM       LIKE MLPP-LBKUM     ,
         SALK3       LIKE MLCR-SALK3     ,

       END OF   IT_ZVCO_MLXXV.


DATA : BEGIN OF IT_DETAIL OCCURS 0,
         KJAHR       LIKE MLCR-KJAHR     ,
         POPER       LIKE MLCR-POPER     ,
         BELNR       LIKE MLCR-BELNR     ,
         POSNR       LIKE MLCR-POSNR     ,
         KALNR       LIKE MLIT-KALNR     ,
         MATNR       LIKE MLIT-MATNR     ,
         AWREF       LIKE MLHD-AWREF     ,
         AWORG       LIKE MLHD-AWORG     ,
         MJAHR       LIKE MSEG-MJAHR     ,
         PTYP        LIKE MLIT-PTYP      , " Original process cat.
       END OF IT_DETAIL.

DATA : BEGIN OF IT_MSEG OCCURS 0,
         MBLNR       LIKE MSEG-MBLNR     ,
         ZEILE       LIKE MSEG-ZEILE     ,
         BWART       LIKE MSEG-BWART,
         MATNR       LIKE MSEG-MATNR     ,
         SAKTO       LIKE MSEG-SAKTO     ,
         KOSTL       LIKE MSEG-KOSTL     ,
         AUFNR       LIKE MSEG-AUFNR     ,
         DMBTR       LIKE MSEG-DMBTR     ,
         MENGE       LIKE MSEG-MENGE     ,
         MEINS       LIKE MSEG-MEINS     ,
       END OF IT_MSEG.


**** FOR Detail & clearing acc : Begin
DATA : IT_ZTCO_MLII LIKE ZTCO_MLII OCCURS 0 WITH HEADER LINE.
DATA : IT_ZTCO_MLI2 LIKE ZTCO_MLI2 OCCURS 0 WITH HEADER LINE.
DATA : IT_MLI2_TEMP LIKE ZTCO_MLI2 OCCURS 0 WITH HEADER LINE.


DATA : BEGIN OF IT_CLEAR OCCURS 0,
        AWKEY   LIKE BKPF-AWKEY,
        MBLNR   LIKE MSEG-MBLNR,
        BDATJ   LIKE MSEG-MJAHR.
DATA : END OF   IT_CLEAR.

DATA : BEGIN OF IT_BKPF OCCURS 0,
        GJAHR   LIKE BKPF-GJAHR,
        AWKEY   LIKE BKPF-AWKEY,
        BLART   LIKE BKPF-BLART,
        BELNR   LIKE BKPF-BELNR.
DATA : END OF   IT_BKPF.

DATA : BEGIN OF IT_BSAS_TEMP OCCURS 0,
        GJAHR   LIKE BSAS-GJAHR,
        BELNR   LIKE BSAS-BELNR,
        AUGDT   LIKE BSAS-AUGDT,
        AUGBL   LIKE BSAS-AUGBL,
        BLART   LIKE BSAS-BLART.
DATA : END OF   IT_BSAS_TEMP.

DATA : BEGIN OF IT_BSAS OCCURS 0,
        BUKRS   LIKE BSAS-BUKRS,
        GJAHR   LIKE BSAS-GJAHR,
        BELNR   LIKE BSAS-BELNR,
        AUGDT   LIKE BSAS-AUGDT,
        AUGBL   LIKE BSAS-AUGBL,
        BSCHL   LIKE BSAS-BSCHL,
        BLART   LIKE BSAS-BLART,
        BUZEI   LIKE BSAS-BUZEI.
DATA : END OF   IT_BSAS.

DATA : BEGIN OF IT_BSEG OCCURS 0,
        GJAHR   LIKE BSEG-GJAHR,
        BELNR   LIKE BSEG-BELNR,
        DMBTR   LIKE BSEG-DMBTR,
        HKONT   LIKE BSEG-HKONT,
        BUZEI   LIKE BSEG-BUZEI,
        KOSTL   LIKE BSEG-KOSTL,
        AUFNR   LIKE BSEG-AUFNR.
DATA : END OF   IT_BSEG.

**** FOR Detail & clearing acc : End

DATA : BEGIN OF IT_T001W OCCURS 0.
        INCLUDE STRUCTURE T001W.
DATA : END OF   IT_T001W.

DATA : P_POPER LIKE MLCD_KEY-POPER.

* Find BADY BOY
TYPES:
  BEGIN OF S_MATS,
     KALNR TYPE CKMLHD-KALNR,
     MATNR TYPE CKMLHD-MATNR,
     BWKEY TYPE CKMLHD-BWKEY,
     BWTAR TYPE CKMLHD-BWTAR,
*     sobkz TYPE ckmlhd-sobkz,
*     vbeln TYPE ckmlhd-vbeln,
*     posnr TYPE ckmlhd-posnr,
*     pspnr TYPE ckmlhd-pspnr,
     MTART TYPE MARA-MTART,
     MATKL TYPE MARA-MATKL,
     SPART TYPE MARA-SPART,
*    prctr TYPE marc-prctr,
     MEINS TYPE MARA-MEINS,
     BKLAS TYPE MBEW-BKLAS,   "val class
     LFGJA TYPE MBEW-LFGJA,
     LFMON TYPE MBEW-LFMON,
     MAKTG TYPE MAKT-MAKTG,
     STPRS TYPE MBEW-STPRS,
     VERPR TYPE MBEW-VERPR,

     SALK3 TYPE MBEW-SALK3,
*    lbkum TYPE mbew-lbkum,
     STATUS   LIKE CKMLPP-STATUS,    "ML status
     ABKUMO   LIKE CKMLPP-ABKUMO,    "Begin
     UMKUMO   LIKE CKMLPP-UMKUMO,    "Prev Posting
     ZUKUMO   LIKE CKMLPP-ZUKUMO,    "GR
     VNKUMO   LIKE CKMLPP-VNKUMO,    "GI
     LBKUM    LIKE CKMLPP-LBKUM ,    "End
     EKKUMO   LIKE CKMLPP-EKKUMO,    "PO GR

     ABSALK3  LIKE CKMLCR-ABSALK3,
     ABPRD_O  LIKE CKMLCR-ABPRD_O,
     ABKDM_O  LIKE CKMLCR-ABKDM_O,
     ABPRD_MO LIKE CKMLCR-ABPRD_MO,
     ABKDM_MO LIKE CKMLCR-ABKDM_MO,

     VPPRD_O  LIKE CKMLCR-VPPRD_O,
     ZUPRD_O  LIKE CKMLCR-ZUPRD_O,
     ZUKDM_O  LIKE CKMLCR-ZUKDM_O,
     VPKDM_O  LIKE CKMLCR-VPKDM_O,

     ZUPRD_MO  LIKE CKMLCR-ZUPRD_MO,
     ZUKDM_MO  LIKE CKMLCR-ZUKDM_MO,

     VNPRD_EA  LIKE CKMLCR-VNPRD_EA,
     VNKDM_EA  LIKE CKMLCR-VNKDM_EA,
     EBPRD_EA  LIKE CKMLCR-EBPRD_EA,
     EBKDM_EA  LIKE CKMLCR-EBKDM_EA,
     VNPRD_MA  LIKE CKMLCR-VNPRD_MA,
     VNKDM_MA  LIKE CKMLCR-VNKDM_MA,
     EBPRD_MA  LIKE CKMLCR-EBPRD_MA,
     EBKDM_MA  LIKE CKMLCR-EBKDM_MA,
   END OF S_MATS,

  TY_MATS TYPE STANDARD TABLE OF S_MATS WITH KEY KALNR,

    BEGIN OF S_NDI,
       KALNR TYPE CKMLHD-KALNR,
       BDATJ TYPE CKMLPP-BDATJ,
       POPER TYPE CKMLPP-POPER,
       UNTPER TYPE CKMLPP-UNTPER,
       CURTP TYPE CKMLCR-CURTP,
       MATNR TYPE CKMLHD-MATNR,
       BWKEY TYPE CKMLHD-BWKEY,
       BWTAR TYPE CKMLHD-BWTAR,
       VBELN TYPE CKMLHD-VBELN,
       POSNR TYPE CKMLHD-POSNR,
       PSPNR TYPE CKMLHD-PSPNR,
       POS_TYPE(3),                 "NDI, NIN
       BKLAS TYPE MBEW-BKLAS,
       MTART TYPE MARA-MTART,
       MATKL TYPE MARA-MATKL,
       SPART TYPE MARA-SPART,
*       prctr TYPE marc-prctr,
       MEINS TYPE CKMLPP-MEINS,
       STATUS TYPE CKMLPP-STATUS,
       LBKUM TYPE CKMLPP-LBKUM,
       MENGE TYPE KKB_ML_MENGE,
       PBPOPO TYPE CKMLPP-PBPOPO,
       SALK3 TYPE CKMLCR-SALK3,
       WERT TYPE KKB_ML_BEWER,
       STPRS TYPE CKMLCR-STPRS,
       PVPRS TYPE CKMLCR-PVPRS,
       PEINH TYPE CKMLCR-PEINH,
       WAERS TYPE CKMLCR-WAERS,
       PBPRD_O TYPE CKMLCR-PBPRD_O,
       PBKDM_O TYPE CKMLCR-PBKDM_O,
       ESTPRD TYPE CKML_ESTPRD,
       ESTKDM TYPE CKML_ESTKDM,
       MSTPRD TYPE CKML_MSTPRD,
       MSTKDM TYPE CKML_MSTKDM,
       ESTDIF TYPE CK_SINGLELEVEL_DIF,
       MSTDIF TYPE CK_MULTILEVEL_DIF,
       PRDIF TYPE CK_SUM_PRDIF,
       KRDIF TYPE CK_SUM_KRDIF,
       SUMDIF TYPE CK_SUM_DIF,
       COLOR(3) TYPE C,
     END OF S_NDI,
     TY_OUT TYPE STANDARD TABLE OF S_NDI WITH KEY KALNR.

TYPES: BEGIN OF TY_MVT,
         KALNR TYPE CK_KALNR,
         CATEG TYPE CKML_CATEG,
         PTYP  TYPE CK_PTYP_BVALT,
       END OF TY_MVT.

DATA: T_MATS TYPE TY_MATS  WITH HEADER LINE,

      T_CKMLPP TYPE STANDARD TABLE OF CKMLPP
               WITH KEY KALNR BDATJ POPER
               WITH HEADER LINE,
      T_CKMLCR TYPE STANDARD TABLE OF CKMLCR
               WITH KEY KALNR BDATJ POPER CURTP
               WITH HEADER LINE,
      T_MLCD TYPE STANDARD TABLE OF MLCD
               WITH KEY KALNR BDATJ POPER UNTPER CATEG PTYP BVALT CURTP
               WITH HEADER LINE,
      T_MLCD_NOT_ALLOC TYPE STANDARD TABLE OF MLCD
               WITH KEY KALNR BDATJ POPER UNTPER CATEG PTYP BVALT CURTP
               WITH HEADER LINE,
      T_BAD  TYPE TY_OUT   WITH HEADER LINE,
      T_MVT  TYPE TABLE OF TY_MVT WITH HEADER LINE.

DATA : IT_LOG LIKE ZTCO_BATCH_LOG OCCURS 0 WITH HEADER LINE.

DATA: GV_DATE1 TYPE SYDATUM,
      GV_DATE2 TYPE SYDATUM.

RANGES: R_KALNR FOR ZTCO_MLIT-KALNR.
* For Summary
*DATA : it_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE.
*DATA : it_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE.
*DATA : t_out TYPE zcot1_it_out WITH HEADER LINE .

*--- ALV
TYPE-POOLS: SLIS.
DATA : W_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       W_EVENTCAT TYPE SLIS_T_EVENT WITH HEADER LINE,
       W_SELFIELD TYPE SLIS_SELFIELD,
       W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
       W_COL_POS  TYPE I,
       W_PROGRAM  LIKE SY-REPID,
       W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
       W_LINE1 TYPE SLIS_LISTHEADER.

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_FIELDCAT          TYPE SLIS_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GT_SP_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GT_SORTS    TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      GS_PRNT     TYPE SLIS_PRINT_ALV,
      G_REPID     LIKE SY-REPID,
      G_PF_STATUS_SET  TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
      G_USER_COMMAND   TYPE SLIS_FORMNAME VALUE 'USER_COMMAND',
      GS_VARIANT       LIKE DISVARIANT,
      G_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME
                                   VALUE 'TOP_OF_PAGE'.

*---- ALV

DATA: BEGIN OF I_PROC_KALNR OCCURS 0,
        WERKS LIKE CKMLMV001-WERKS,
        MATNR LIKE CKMLMV001-MATNR,
        BWTAR LIKE CKMLMV001-BWTAR,
        PROCK LIKE CKMLMV001-PROC_KALNR,
        BTYP  LIKE CKMLMV001-BTYP,  "bf-production, bb-procurement
        KALNR LIKE CKMLHD-KALNR,
      END OF I_PROC_KALNR.
