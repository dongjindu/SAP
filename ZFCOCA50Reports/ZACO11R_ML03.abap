***********************************************************************
* Program Name      : ZACO11R_ML03
* Author            : Manjunath Venkatesh
* Creation Date     : 02/03/2006
* Specifications By : Andy Choi
* Pattern           : Report 1-1
* Add documentation : Copy of ZACO11R_ML03_NEW
* Description       : ML report for detail
*                     ML data should be gathered as MVT Group and
*                     Process Category
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date      Developer   Request ID    Description
* 01/17/05  Manjunath   UD1K919229    Program changes to Add Ending STD
*                                     and VAR values from MBEWH & MBEW
************************************************************************
REPORT ZACO11R_ML03 MESSAGE-ID ZMCO.


*----------------------------------------------------------------------*
*   INCLUDE ZACO11R_ML03_NEW_TOP                                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   Macro
*----------------------------------------------------------------------*
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
END-OF-DEFINITION.

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
  IT_MLIT-VN_VKA_&1
   =  ( IT_MLIT-VN_VKA_&1 + IT_MLIT-VN_&2_&1 +
     IT_MLIT-VN_&3_&1 )
  - ( IT_MLIT-VN_&2_&1 + IT_MLIT-VN_&3_&1 ).
END-OF-DEFINITION .


* For Substraction from VN ETC
DEFINE MLDR_SUB_VN_ETC.
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
END-OF-DEFINITION .

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
* Type-Pools
TYPE-POOLS : CKMV0, SLIS, ZCOT1.

* Tables
TABLES: CKMLPP,
        CKMLCR,
        CKMLHD,
        CKMLCT,
        MACKU,
        MARC,
        MARA,
        MLCD_KEY,
        MLCD,
        MLCR,
        MLPP,
        MLHD,
        MLIT,
        MBEW,
        ZTCO_MLIT,
        ZTCO_MLI2,
        ZVCO_MLXXV.

*  Internal Tables
*  For MLCD
DATA IT_KALNR TYPE CKMV0_MATOBJ_TBL WITH HEADER LINE.

DATA: BEGIN OF IT_MLCD OCCURS 1000,
        KALNR   LIKE MLCD-KALNR,
        BDATJ   LIKE MLCD-BDATJ,
        POPER   LIKE MLCD-POPER,
        CATEG(7),
        PTYP    LIKE MLCD-PTYP,
        BVALT   LIKE MLCD-BVALT,
        CURTP   LIKE MLCD-CURTP,
        OBJECT  LIKE MARA-MATNR,
        LBKUM   LIKE MLCD-LBKUM,
        MEINS   LIKE MLCD-MEINS,
        SALK3   LIKE MLCD-SALK3,
        ESTPRD  LIKE MLCD-ESTPRD,
        ESTKDM  LIKE MLCD-ESTKDM,
        MSTPRD  LIKE MLCD-MSTPRD,
        MSTKDM  LIKE MLCD-MSTKDM,
        WAERS   LIKE MLCD-WAERS,
        TPPRD   LIKE MLCD-TPPRD,
      END OF   IT_MLCD.

* For Display
DATA: IT_MLIT  TYPE TABLE OF ZTCO_MLIT WITH HEADER LINE.

* For ZVCO_MLXXV - Data by MVTGRP
* MLHD
* MLIT
* MLPP
* MLCR
* MLPPF

DATA: BEGIN OF IT_ZVCO_MLXXV OCCURS 1000,
        BELNR       LIKE MLHD-BELNR,
        KJAHR       LIKE MLHD-KJAHR,
        AWREF       LIKE MLHD-AWREF,
        AWORG       LIKE MLHD-AWORG,
        URZEILE     LIKE MLIT-URZEILE,     " Line Item No.
        POSNR       LIKE MLIT-POSNR,
        MATNR       LIKE MLIT-MATNR,
        BWKEY       LIKE MLIT-BWKEY,
        KALNR       LIKE MLIT-KALNR,
        MEINS       LIKE MLIT-MEINS,
        BEWARTGRP   LIKE MLIT-BEWARTGRP,   " MvType Grp
        KATEGORIE   LIKE MLIT-KATEGORIE,   " Category in ML
        PTYP        LIKE MLIT-PTYP,        " Original process cat.
*       PERART      TYPE CK_PER_ART,       " VM-previous,LF-current
*        FELDG       LIKE MLPPF-FELDG,
        LBKUM       LIKE MLPP-LBKUM,
        SALK3       LIKE MLCR-SALK3,
        CHK(1)      TYPE C,
      END OF   IT_ZVCO_MLXXV.


DATA: BEGIN OF IT_DETAIL OCCURS 0,
        KJAHR       LIKE MLCR-KJAHR,
        POPER       LIKE MLCR-POPER,
        BELNR       LIKE MLCR-BELNR,
        POSNR       LIKE MLCR-POSNR,
        KALNR       LIKE MLIT-KALNR,
        BWKEY       LIKE MLIT-BWKEY,
        MATNR       LIKE MLIT-MATNR,
        BEWARTGRP   LIKE MLIT-BEWARTGRP,   " MvType Grp
        AWREF       LIKE MLHD-AWREF,   " Doc. No.
        AWORG       LIKE MLHD-AWORG,   " Year
        URZEILE     LIKE MLIT-URZEILE, " Line Item No.
        PTYP        LIKE MLIT-PTYP,    " Original process cat.
        MJAHR       LIKE MSEG-MJAHR,   " Year for matching MSEG
        ZEILE       LIKE MSEG-ZEILE,   " Line Item No. for matching MSEG
      END OF IT_DETAIL.

DATA: BEGIN OF IT_MSEG OCCURS 0,
        MBLNR       LIKE MSEG-MBLNR,
        MJAHR       LIKE MSEG-MJAHR,
        ZEILE       LIKE MSEG-ZEILE,
        SHKZG       LIKE MSEG-SHKZG,
        BWART       LIKE MSEG-BWART,
        MATNR       LIKE MSEG-MATNR,
        SAKTO       LIKE MSEG-SAKTO,
        KOSTL       LIKE MSEG-KOSTL,
        AUFNR       LIKE MSEG-AUFNR,
        DMBTR       LIKE MSEG-DMBTR,
        MENGE       LIKE MSEG-MENGE,
        MEINS       LIKE MSEG-MEINS,
      END OF IT_MSEG.

* FOR Detail & clearing acc : Begin
DATA: IT_ZTCO_MLI2 LIKE ZTCO_MLI2 OCCURS 0 WITH HEADER LINE,
      IT_MLI2_TEMP LIKE ZTCO_MLI2 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_CLEAR OCCURS 0,
       AWKEY   LIKE BKPF-AWKEY,
       MBLNR   LIKE MSEG-MBLNR,
       BDATJ   LIKE MSEG-MJAHR.
DATA: END OF   IT_CLEAR.

DATA: BEGIN OF IT_BKPF OCCURS 0,
       BUKRS   LIKE BKPF-BUKRS,
       GJAHR   LIKE BKPF-GJAHR,
       AWKEY   LIKE BKPF-AWKEY,
       BLART   LIKE BKPF-BLART,
       BELNR   LIKE BKPF-BELNR.
DATA: END OF   IT_BKPF.

DATA: BEGIN OF IT_BSAS_TEMP OCCURS 0,
        BUKRS   LIKE BKPF-BUKRS,
        GJAHR   LIKE BSAS-GJAHR,
        BELNR   LIKE BSAS-BELNR,
        AUGDT   LIKE BSAS-AUGDT,
        AUGBL   LIKE BSAS-AUGBL,
        BLART   LIKE BSAS-BLART.
DATA: END OF    IT_BSAS_TEMP.

DATA: BEGIN OF IT_BSAS OCCURS 0,
        BUKRS   LIKE BSAS-BUKRS,
        GJAHR   LIKE BSAS-GJAHR,
        BELNR   LIKE BSAS-BELNR,
        AUGDT   LIKE BSAS-AUGDT,
        AUGBL   LIKE BSAS-AUGBL,
        BSCHL   LIKE BSAS-BSCHL,
        BLART   LIKE BSAS-BLART,
        BUZEI   LIKE BSAS-BUZEI.
DATA: END OF   IT_BSAS.

DATA: BEGIN OF IT_BSEG OCCURS 0,
       GJAHR   LIKE BSEG-GJAHR,
       BELNR   LIKE BSEG-BELNR,
       DMBTR   LIKE BSEG-DMBTR,
       HKONT   LIKE BSEG-HKONT,
       BUZEI   LIKE BSEG-BUZEI,
       KOSTL   LIKE BSEG-KOSTL,
       AUFNR   LIKE BSEG-AUFNR.
DATA: END OF   IT_BSEG.

* FOR Detail & clearing acc : End
DATA : BEGIN OF IT_T001W OCCURS 0.
        INCLUDE STRUCTURE T001W.
DATA : END OF   IT_T001W.

DATA P_POPER LIKE MLCD_KEY-POPER.

* Find BADY BOY
TYPES: BEGIN OF S_MATS,
        KALNR    TYPE CKMLHD-KALNR,
        BKLAS    TYPE MBEW-BKLAS,      " val class
        MATNR    TYPE CKMLHD-MATNR,
        BWKEY    TYPE CKMLHD-BWKEY,
        BWTAR    TYPE CKMLHD-BWTAR,
        MTART    TYPE MARA-MTART,
        MATKL    TYPE MARA-MATKL,
        MAKTG    TYPE MAKT-MAKTG,
        STPRS    TYPE MBEW-STPRS,
        VERPR    TYPE MBEW-VERPR,
        SALK3    TYPE MBEW-SALK3,
        MEINS    TYPE CKMLPP-MEINS,
        STATUS   TYPE CK_MLSTAT,       "  ML status
        ABKUMO   TYPE CK_ABKUM,        " Begin
        UMKUMO   TYPE CKMLPP-UMKUMO,   " Prev Posting
        ZUKUMO   TYPE CKMLPP-ZUKUMO,   " GR
        VNKUMO   TYPE CKMLPP-VNKUMO,   " GI
        LBKUM    TYPE CKMLPP-LBKUM ,   " End
        EKKUMO   TYPE CKMLPP-EKKUMO,   " PO GR
        PEINH    LIKE CKMLCR-PEINH,
        ABSALK3  TYPE CKMLCR-ABSALK3,
        ABPRD_O  TYPE CKMLCR-ABPRD_O,
        ABKDM_O  TYPE CKMLCR-ABKDM_O,
        ABPRD_MO TYPE CKMLCR-ABPRD_MO,
        ABKDM_MO TYPE CKMLCR-ABKDM_MO,
        VPPRD_O  TYPE CKMLCR-VPPRD_O,
        ZUPRD_O  TYPE CKMLCR-ZUPRD_O,
        ZUKDM_O  TYPE CKMLCR-ZUKDM_O,
        VPKDM_O  TYPE CKMLCR-VPKDM_O,
        ZUPRD_MO TYPE CKMLCR-ZUPRD_MO,
        ZUKDM_MO TYPE CKMLCR-ZUKDM_MO,
        VNPRD_EA TYPE CKMLCR-VNPRD_EA,
        VNKDM_EA TYPE CKMLCR-VNKDM_EA,
        EBPRD_EA TYPE CKMLCR-EBPRD_EA,
        EBKDM_EA TYPE CKMLCR-EBKDM_EA,
        VNPRD_MA TYPE CKMLCR-VNPRD_MA,
        VNKDM_MA TYPE CKMLCR-VNKDM_MA,
        EBPRD_MA TYPE CKMLCR-EBPRD_MA,
        EBKDM_MA TYPE CKMLCR-EBKDM_MA,
   END OF S_MATS.

TYPES TY_MATS TYPE STANDARD TABLE OF S_MATS WITH KEY KALNR.

TYPES: BEGIN OF S_NDI,
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
       END OF S_NDI.

TYPES TY_OUT TYPE STANDARD TABLE OF S_NDI WITH KEY KALNR.

TYPES: BEGIN OF TY_MVT,
         KALNR TYPE CK_KALNR,
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

*DATA IT_LOG LIKE ZTCO_BATCH_LOG OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF I_PROC_KALNR OCCURS 0,
        WERKS LIKE CKMLMV001-WERKS,
        MATNR LIKE CKMLMV001-MATNR,
        BWTAR LIKE CKMLMV001-BWTAR,
        PROCK LIKE CKMLMV001-PROC_KALNR,
        BTYP  LIKE CKMLMV001-BTYP,  "bf-production, bb-procurement
        KALNR LIKE CKMLHD-KALNR,
      END OF I_PROC_KALNR.

DATA: GV_DATE1 TYPE SYDATUM,  "start
      GV_DATE2 TYPE SYDATUM,  "end
      GV_DATE3 TYPE SYDATUM.  "next end

RANGES R_KALNR FOR ZTCO_MLIT-KALNR.

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
*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_KOKRS LIKE CSKS-KOKRS     MEMORY ID CAC,
             P_BDATJ LIKE MLCD_KEY-BDATJ MEMORY ID BDTJ.
SELECT-OPTIONS :  S_POPER FOR MLCD_KEY-POPER NO-EXTENSION
                          MEMORY ID POPR.
PARAMETER:    P_CURTP LIKE MLCD-CURTP     DEFAULT '10' MODIF ID DIS,
              P_DB AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS : S_MATNR FOR MACKU-MATNR MEMORY ID MAT,
                 S_MTART FOR MACKU-MTART,
                 S_BWKEY FOR MBEW-BWKEY  MEMORY ID BWK,
                 S_BKLAS FOR MBEW-BKLAS.
SELECTION-SCREEN END OF BLOCK BL2.

SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-003.
PARAMETERS : P_UP AS CHECKBOX                  MODIF ID DIS,
             P_CLEAR AS CHECKBOX DEFAULT 'X'   MODIF ID DIS,
             P_SAKNR    LIKE BSEG-HKONT DEFAULT '0000137090' NO-DISPLAY.

SELECTION-SCREEN END OF BLOCK BL3.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK BL4 WITH FRAME TITLE TEXT-004.
PARAMETERS P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK BL4.
*for other issue of vehicle
PARAMETERS: P_BATCH(1) TYPE C  DEFAULT ' ' NO-DISPLAY.

PARAMETERS : P_GI_OTH AS CHECKBOX,
             P_GI_PHY AS CHECKBOX,
             P_GI_OSD AS CHECKBOX.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
* Check For Update-Option
  PERFORM CHK_UP_OPT.

AT SELECTION-SCREEN.
* Check Future Date
  PERFORM CHK_FT_DATE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

INITIALIZATION.
  S_BKLAS-OPTION = 'EQ'.
  S_BKLAS-SIGN   = 'E'.
  S_BKLAS-LOW    = '3040'.
  APPEND S_BKLAS.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA P_PERAB TYPE CO_PERAB.

* imported from parallel program
  IMPORT  P_KOKRS  FROM MEMORY ID 'ML03_KOKRS'.
  IMPORT  P_BDATJ  FROM MEMORY ID 'ML03_BDATJ'.
  IMPORT  P_PERAB  FROM MEMORY ID 'ML03_PERAB'.
  IMPORT  P_CURTP  FROM MEMORY ID 'ML03_CURTP'.
  IMPORT  P_BATCH  FROM MEMORY ID 'ML03_BATCH'.

  IF P_BATCH = 'X'.
    P_UP = 'X'.
    P_DB = ' '.
  ENDIF.

  CLEAR T_MATS.
  REFRESH T_MATS.
  IMPORT T_MATS = T_MATS FROM MEMORY ID 'ML03_MATS'.

  IF NOT T_MATS[] IS INITIAL.
    CLEAR P_DB.
    S_POPER-SIGN   = 'I'.    S_POPER-OPTION = 'EQ'.
    S_POPER-LOW    = P_PERAB.
    APPEND S_POPER.
  ENDIF.

* imported from parallel program
  PERFORM CHECK_OBLIGATORY_FIELD.

  READ TABLE S_POPER INDEX 1.
  IF S_POPER-HIGH IS INITIAL. S_POPER-HIGH = S_POPER-LOW. ENDIF.
  P_POPER = S_POPER-LOW.

  PERFORM GET_DATE.

  CLEAR IT_MLIT.
  REFRESH IT_MLIT.

  IF P_DB = 'X'.
    PERFORM GET_DATA_FROM_BUFFER.
  ELSE.
    IF T_MATS[] IS INITIAL.
      PERFORM GET_MATERIALS_FROM_CLOSING.
    ELSE.
      PERFORM GET_MATERIALS_FROM_PARALLEL.

*      LOOP AT T_MATS.
*        PERFORM MAKE_BATCH_LOG USING '2'.
*      ENDLOOP.
    ENDIF.

    SORT T_MATS BY KALNR.

    PERFORM GET_PROC_KALNR.
    PERFORM READ_MLCD.
    PERFORM FIND_BAD_BOYS.
    PERFORM MERGE_MLCD_DATA.
    PERFORM DELETE_BLANK_RECORDS.

*   Read Material Data / Unit / Curr.
    PERFORM FILL_MATERIAL_INFO.

*   Update
    PERFORM UPDATE_ZTCO_MLIT.

  ENDIF.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF P_BATCH <> 'X'.

    IF P_GI_OTH = 'X'.
      DELETE IT_MLIT WHERE VN_CC99_LBKUM = 0.
    ENDIF.
    IF P_GI_PHY = 'X'.
      DELETE IT_MLIT WHERE VN_CC60_LBKUM = 0.
    ENDIF.
    IF P_GI_OSD = 'X'.
      DELETE IT_MLIT WHERE VN_OR58_LBKUM = 0
                       AND VN_OR66_LBKUM = 0.
    ENDIF.

    PERFORM CALL_ALV_LIST.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  MLDR_READ_DATA_BY_MVTGRP
*&---------------------------------------------------------------------*
*       Read Data By MVTGRP
*----------------------------------------------------------------------*
FORM MLDR_READ_DATA_BY_MVTGRP.
  RANGES: R_PTYP      FOR MLIT-PTYP.
  RANGES: R_KJAHR     FOR MLIT-KJAHR.
  RANGES: R_BEWARTGRP FOR MLIT-BEWARTGRP.

  R_PTYP-SIGN   = 'I'.
  R_PTYP-OPTION = 'EQ'.
  R_PTYP-LOW    = 'B+'.     APPEND R_PTYP.
  R_PTYP-LOW    = 'VKA'.    APPEND R_PTYP.
  R_PTYP-LOW    = 'V+'.     APPEND R_PTYP.
  R_PTYP-LOW    = 'VK'.     APPEND R_PTYP.
  R_PTYP-LOW    = 'VEAU'.   APPEND R_PTYP.
  R_PTYP-LOW    = 'VA'.     APPEND R_PTYP.
  R_PTYP-LOW    = 'VP'.     APPEND R_PTYP.
*
  R_KJAHR-SIGN   = 'I'.
  R_KJAHR-OPTION = 'EQ'.
  R_KJAHR-LOW    = P_BDATJ. APPEND R_KJAHR.
  R_KJAHR-LOW    = P_BDATJ - 1.
  APPEND R_KJAHR.

  R_BEWARTGRP-SIGN   = 'I'.
  R_BEWARTGRP-OPTION = 'EQ'.
  R_BEWARTGRP-LOW    = '16'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '15'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '20'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '19'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '66'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '65'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '58'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '57'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '60'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '59'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '64'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '63'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '56'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '55'.  APPEND R_BEWARTGRP.

  R_BEWARTGRP-LOW    = '52'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = '51'.  APPEND R_BEWARTGRP.
  R_BEWARTGRP-LOW    = 'XX'.  APPEND R_BEWARTGRP.

* ANDY
* ML document has system date,
* 1 MLCR = 1 or 2 MLPP (cross period)
* need to check MLPPF (field group <> 'UMO')

  CLEAR : IT_ZVCO_MLXXV, IT_ZVCO_MLXXV[].

  IF NOT T_MVT[] IS INITIAL.
    REFRESH IT_ZVCO_MLXXV.

    SELECT MLCR~BELNR MLCR~KJAHR
           MLHD~AWREF MLHD~AWORG MLIT~URZEILE
           MLCR~POSNR MLIT~MATNR MLIT~BWKEY
           MLIT~KALNR MLIT~MEINS
           MLIT~BEWARTGRP MLIT~KATEGORIE MLIT~PTYP
*          MLPPF~FELDG
           MLPP~LBKUM MLCR~SALK3
       INTO TABLE IT_ZVCO_MLXXV
       FROM MLHD
         INNER JOIN MKPF
                 ON MLHD~AWREF =  MKPF~MBLNR
                AND MLHD~AWORG =  MKPF~MJAHR
         INNER JOIN MLIT
            ON MLIT~BELNR =  MLHD~BELNR
           AND MLIT~KJAHR =  MLHD~KJAHR
         INNER JOIN MLPP
            ON MLPP~BELNR =  MLHD~BELNR
           AND MLPP~KJAHR =  MLHD~KJAHR
           AND MLPP~POSNR =  MLIT~POSNR
*         left join mlppf
*            ON MLPPF~BELNR =  MLPP~BELNR
*           AND MLPPF~KJAHR =  MLPP~KJAHR
*           AND MLPPF~POSNR =  MLPP~POSNR
*           AND MLPPF~BDATJ =  MLPP~BDATJ
*           AND MLPPF~POPER =  MLPP~POPER
         INNER JOIN MLCR
            ON MLCR~BELNR =  MLPP~BELNR
           AND MLCR~KJAHR =  MLPP~KJAHR
           AND MLCR~POSNR =  MLPP~POSNR
           AND MLCR~BDATJ =  MLPP~BDATJ
           AND MLCR~POPER =  MLPP~POPER
*       FOR ALL ENTRIES IN T_MVT
       WHERE ( MLHD~CPUDT >= GV_DATE1
         AND   MLHD~CPUDT <  GV_DATE3 )
         AND MLHD~VGART  = 'UP'
         AND MLHD~AWTYP  = 'MKPF'
         AND MLHD~GLVOR <> 'RMWE'
         AND ( MKPF~BUDAT >= GV_DATE1 AND MKPF~BUDAT <= GV_DATE2 )
*        AND MLIT~KALNR = T_MVT-KALNR      " INDEX
         AND MLIT~PSART = 'UP'             " ML update
         AND MLIT~PTYP      IN R_PTYP
         AND MLIT~BEWARTGRP IN R_BEWARTGRP
         AND MLPP~BDATJ  = P_BDATJ
         AND MLPP~POPER  = P_POPER
*        AND MLPP~PERART = 'LF'
         AND MLCR~CURTP  = P_CURTP.

    SORT T_MVT BY KALNR.
    LOOP AT T_MVT.
      IT_ZVCO_MLXXV-CHK = 'X'.
      MODIFY IT_ZVCO_MLXXV TRANSPORTING CHK WHERE KALNR = T_MVT-KALNR.
    ENDLOOP.

    SORT IT_ZVCO_MLXXV BY CHK.
    DELETE IT_ZVCO_MLXXV WHERE CHK IS INITIAL.

*OLD LOGIC
*    IF 1 = 2.
*    DATA: T_MLHD      LIKE IT_ZVCO_MLXXV OCCURS 100000 WITH HEADER LINE
*.
*
*      SELECT MLHD~BELNR MLHD~KJAHR
*             MLHD~AWREF MLHD~AWORG
*         INTO CORRESPONDING FIELDS OF TABLE T_MLHD
*         FROM MLHD
*           INNER JOIN MKPF
*                   ON MLHD~AWREF =  MKPF~MBLNR
*                  AND MLHD~AWORG =  MKPF~MJAHR
*            WHERE ( MLHD~CPUDT >= GV_DATE1 AND MLHD~CPUDT <  GV_DATE3 )
*                AND MLHD~VGART  = 'UP'
*                AND MLHD~AWTYP  = 'MKPF'
*                AND MLHD~GLVOR <> 'RMWE'
*              AND ( MKPF~BUDAT >= GV_DATE1 AND MKPF~BUDAT <= GV_DATE2 )
*.
*
*      SELECT MLIT~BELNR MLIT~KJAHR MLIT~POSNR MLIT~URZEILE
*             MLIT~MATNR MLIT~BWKEY MLIT~KALNR
*             MLIT~MEINS MLIT~BEWARTGRP MLIT~KATEGORIE MLIT~PTYP
*             MLPP~LBKUM MLCR~SALK3
*          INTO CORRESPONDING FIELDS OF TABLE IT_ZVCO_MLXXV
*          FROM MLIT
*             INNER JOIN MLPP
*                     ON MLPP~BELNR =  MLIT~BELNR
*                    AND MLPP~KJAHR =  MLIT~KJAHR
*                    AND MLPP~POSNR =  MLIT~POSNR
*                  INNER JOIN MLCR
*                     ON MLCR~BELNR =  MLPP~BELNR
*                    AND MLCR~KJAHR =  MLPP~KJAHR
*                    AND MLCR~POSNR =  MLPP~POSNR
*                    AND MLCR~BDATJ =  MLPP~BDATJ
*                    AND MLCR~POPER =  MLPP~POPER
*          FOR ALL ENTRIES IN T_MLHD
*             WHERE MLIT~BELNR =  T_MLHD-BELNR
*               AND MLIT~KJAHR =  T_MLHD-KJAHR
*               AND MLIT~PSART = 'UP'             " ML update
*               AND MLIT~PTYP      IN R_PTYP
*               AND MLIT~BEWARTGRP IN R_BEWARTGRP
*               AND MLPP~BDATJ  = P_BDATJ
*               AND MLPP~POPER  = P_POPER
**              AND MLPP~PERART = 'LF'
*               AND MLCR~CURTP  = P_CURTP.
*
*      SORT T_MVT BY KALNR.
*      LOOP AT T_MVT.
*        IT_ZVCO_MLXXV-CHK = 'X'.
*        MODIFY IT_ZVCO_MLXXV TRANSPORTING CHK WHERE KALNR = T_MVT-KALNR
*.
*      ENDLOOP.
*
*
*      DELETE IT_ZVCO_MLXXV WHERE CHK IS INITIAL.
*      SORT T_MLHD BY BELNR KJAHR POSNR.
*
*      DATA L_IDX TYPE SY-TABIX.
*
*      LOOP AT IT_ZVCO_MLXXV.
*        CLEAR L_IDX.
*        L_IDX = SY-TABIX.
*
*        READ TABLE T_MLHD WITH KEY BELNR = IT_ZVCO_MLXXV-BELNR
*                                   KJAHR = IT_ZVCO_MLXXV-KJAHR
*                                   POSNR = IT_ZVCO_MLXXV-POSNR
*                          BINARY SEARCH.
*        IF SY-SUBRC <> 0.
*          BREAK-POINT.
*        ELSE.
*          IT_ZVCO_MLXXV-AWREF = T_MLHD-AWREF.
*          IT_ZVCO_MLXXV-AWORG = T_MLHD-AWORG.
*          MODIFY IT_ZVCO_MLXXV INDEX L_IDX TRANSPORTING AWREF AWORG.
*        ENDIF.
*
*      ENDLOOP.
*    ENDIF.

* --outer join
*    delete it_zvco_mlxxv where feldg = 'UMO'.

    LOOP AT IT_ZVCO_MLXXV.
*     Key Part
      CLEAR IT_MLIT.
      IT_MLIT-KALNR = IT_ZVCO_MLXXV-KALNR.

*     Set Value By MVTGRP.
      PERFORM MLDR_SET_VAL_BY_MVTGRP.

*     Collect
      COLLECT IT_MLIT.
      CLEAR: IT_MLIT, IT_ZVCO_MLXXV.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " MLDR_READ_DATA_BY_MVTGRP
*&---------------------------------------------------------------------*
*&      Form  MLDR_SET_VAL_BY_MVTGRP
*&---------------------------------------------------------------------*
*       Set Values By MVT Grp.
*----------------------------------------------------------------------*
FORM MLDR_SET_VAL_BY_MVTGRP.
* There can NOT be PRD or ERD of data caused by Material Movement
* PRD/ERD is posted in different document without Material Movement
  CASE IT_ZVCO_MLXXV-KATEGORIE.
    WHEN 'ZU'.
*     Procurement
      IF IT_ZVCO_MLXXV-PTYP = 'B+'.
        CASE IT_ZVCO_MLXXV-BEWARTGRP.
          WHEN '60'.            " Phy
            PERFORM SET_VAL_DETAIL USING 'VN_CC60'.
          WHEN '64' OR '63'.    " Free GR
            PERFORM SET_VAL_DETAIL USING 'ZU_FRE'.
          WHEN '52' OR '51'.    " Other procurement
            PERFORM SET_VAL_DETAIL USING 'ZU_ETC'.
          WHEN OTHERS.
            PERFORM SET_VAL_DETAIL USING 'ZU_ETC'.
        ENDCASE.
      ENDIF.

    WHEN 'VN'.
      CASE IT_ZVCO_MLXXV-PTYP.
*       Sales Order
        WHEN 'VKA'.
          CASE IT_ZVCO_MLXXV-BEWARTGRP.
            WHEN '16' OR '15'.    " Domestic export
              PERFORM SET_VAL_DETAIL USING 'VN_VKA16'.
            WHEN '20' OR '19'.    " Export
              PERFORM SET_VAL_DETAIL USING 'VN_VKA20'.
            WHEN '56' OR '55'.    " MSEG, BSEG  '137090'
              PERFORM SET_VAL_DETAIL USING 'VN_CC99'.
              PERFORM APPEND_IT_DETAIL.
          ENDCASE.

*       Consumption
        WHEN 'V+'.
          CASE IT_ZVCO_MLXXV-BEWARTGRP.
            WHEN '59'.
              PERFORM SET_VAL_DETAIL USING 'VN_CC60'.
            WHEN '16' OR '15'.
              PERFORM SET_VAL_DETAIL USING 'VN_VKA16'.
            WHEN OTHERS.
              PERFORM SET_VAL_DETAIL USING 'VN_CC99'.
              PERFORM APPEND_IT_DETAIL.
          ENDCASE.

*       MSEG Detail
        WHEN 'VK'.
*           MSEG(201)
          PERFORM APPEND_IT_DETAIL.

*       GI to Cost object Hiearchy
        WHEN 'VHP'.
          IF IT_ZVCO_MLXXV-BEWARTGRP = '58' OR
             IT_ZVCO_MLXXV-BEWARTGRP = '57' OR
             IT_ZVCO_MLXXV-BEWARTGRP = '66' OR
             IT_ZVCO_MLXXV-BEWARTGRP = '65'.
            PERFORM SET_VAL_DETAIL USING 'VN_COB'.
          ENDIF.

*       GI to Order
        WHEN 'VEAU'.
          CASE IT_ZVCO_MLXXV-BEWARTGRP.
            WHEN '58' OR '57'.
              PERFORM SET_VAL_DETAIL USING 'VN_OR58'.
            WHEN '66' OR '65'.
              PERFORM SET_VAL_DETAIL USING 'VN_OR66'.
            WHEN OTHERS.
              PERFORM SET_VAL_DETAIL USING 'VN_CC99'.
              PERFORM APPEND_IT_DETAIL.
          ENDCASE.

*       Fixed Asset
        WHEN 'VA'.
          IF IT_ZVCO_MLXXV-BEWARTGRP = '56' OR
             IT_ZVCO_MLXXV-BEWARTGRP = '55'.
*           MSEG (241) : VN_CC99
            PERFORM SET_VAL_DETAIL USING 'VN_CC99'.
            PERFORM APPEND_IT_DETAIL.
          ENDIF.

*       Project
        WHEN 'VP'.
          IF IT_ZVCO_MLXXV-BEWARTGRP = '56' OR
             IT_ZVCO_MLXXV-BEWARTGRP = '55'.
*           MSEG (201) : VN_CC99
            PERFORM SET_VAL_DETAIL USING 'VN_CC99'.
            PERFORM APPEND_IT_DETAIL.
          ENDIF.

      ENDCASE.

  ENDCASE.

ENDFORM.                    " MLDR_SET_VAL_BY_MVTGRP

*&---------------------------------------------------------------------*
*&      Form  DEL_ZTCO_MLIT
*&---------------------------------------------------------------------*
*       Delete data in table 'ZTCO_MLIT'
*----------------------------------------------------------------------*
FORM DEL_ZTCO_MLIT.
  CHECK P_UP = 'X'.

* local Data definition
  DATA: LV_ANSWER,
        LV_TITLE(80).

* message
  CLEAR : LV_ANSWER,  LV_TITLE.
  CONCATENATE 'All data will be lost  ' P_BDATJ '/' S_POPER
         INTO LV_TITLE.

  SELECT COUNT( * ) INTO SY-TABIX
    FROM ZTCO_MLIT
   WHERE BDATJ = P_BDATJ
     AND POPER = P_POPER.

  CHECK SY-TABIX > 0.

  IF P_BATCH <> 'X' .
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
       ID 'DEVCLASS' DUMMY
       ID 'OBJTYPE'  FIELD 'DEBUG'
       ID 'OBJNAME'  DUMMY
       ID 'P_GROUP'  DUMMY
       ID 'ACTVT'    FIELD '03'.

    IF SY-SUBRC <> 0.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
           EXPORTING
                TEXTLINE1 = LV_TITLE
                TEXTLINE2 = 'In Table - ZTCO_MLIT'
                TITEL     = 'Delete DATA in Table'
           IMPORTING
                ANSWER    = LV_ANSWER.

      IF  LV_ANSWER <> 'J'.
        MESSAGE E043.
      ENDIF.

    ENDIF.

  ENDIF.

* P_KOKRS  p_bdatj   P_POPER   P_CURTP
* whenever running this program
* All data - Period relative - should be deleted and replaced
* with new records
  DELETE FROM ZTCO_MLIT
         WHERE BDATJ = P_BDATJ
           AND POPER = P_POPER
           AND MATNR IN S_MATNR
           AND KALNR IN R_KALNR
           AND MTART IN S_MTART
           AND BWKEY IN S_BWKEY
           AND BKLAS IN S_BKLAS.

  DELETE FROM ZTCO_MLI2
         WHERE BDATJ = P_BDATJ
           AND POPER = P_POPER
           AND MATNR IN S_MATNR.

* No Check Subrc
  COMMIT WORK.

ENDFORM.                    " DEL_ZTCO_MLIT

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_MLIT
*&---------------------------------------------------------------------*
FORM UPDATE_ZTCO_MLIT.
  CHECK P_UP = 'X'.

* Delete data in table 'ZTCO_MLIT'.
  PERFORM DEL_ZTCO_MLIT.

  LOOP AT IT_ZTCO_MLI2.
    IT_ZTCO_MLI2-BUKRS = P_KOKRS.
    IT_ZTCO_MLI2-BDATJ = P_BDATJ.
    IT_ZTCO_MLI2-POPER = S_POPER-LOW.

    IT_ZTCO_MLI2-ERDAT = SY-DATUM.
    IT_ZTCO_MLI2-ERZET = SY-UZEIT.
    IT_ZTCO_MLI2-ERNAM = SY-UNAME.

    MODIFY IT_ZTCO_MLI2.  CLEAR IT_ZTCO_MLI2.
  ENDLOOP.

* Success
*FIXME
*  insert ZTCO_MLIT from table it_mlit.
  IF P_BATCH = 'X'.
    IF NOT IT_MLIT[] IS INITIAL.
      INSERT ZTCO_MLIT FROM TABLE IT_MLIT.
    ENDIF.

    IF NOT IT_ZTCO_MLI2[] IS INITIAL.
      INSERT ZTCO_MLI2 FROM TABLE IT_ZTCO_MLI2.
    ENDIF.

  ELSE.
    IF NOT IT_MLIT[] IS INITIAL.
      INSERT ZTCO_MLIT FROM TABLE IT_MLIT.

      IF SY-SUBRC <> 0.
        WRITE:/ 'error during insert(ZTCO_MLIT)'.
      ELSE.
        WRITE:/ 'Data is saved(ZTCO_MLIT)'.
      ENDIF.
    ELSE.
      WRITE:/ 'NO Data to  save(ZTCO_MLIT)'.
    ENDIF.

    IF NOT IT_ZTCO_MLI2[] IS INITIAL.
      INSERT ZTCO_MLI2 FROM TABLE IT_ZTCO_MLI2.

      IF SY-SUBRC <> 0.
        WRITE:/ 'error during insert(ZTCO_MLI2)'.
      ELSE.
        WRITE:/ 'Data is saved(ZTCO_MLI2)'.
      ENDIF.
    ELSE.
      WRITE:/ 'NO Data to  save(ZTCO_MLI2)'.
    ENDIF.
  ENDIF.

* for batch program.
  IF P_BATCH = 'X'.
    DATA : L_ARTNR(50),
           L_TEMP_ARTNR TYPE MATNR.
    DATA : BEGIN OF IT_TEMP_MLIT OCCURS 0 ,
           MATNR LIKE ZTCO_MLIT-MATNR,
           END OF IT_TEMP_MLIT.

    SELECT  MATNR INTO CORRESPONDING FIELDS OF TABLE  IT_TEMP_MLIT
        FROM ZTCO_MLIT
        FOR ALL ENTRIES IN T_MATS
       WHERE BUKRS = P_KOKRS
         AND BDATJ = P_BDATJ
         AND POPER = P_PERAB
         AND MATNR = T_MATS-MATNR.

    LOOP AT T_MATS.
      CLEAR  IT_MLIT.
      READ  TABLE IT_MLIT WITH KEY MATNR = T_MATS-MATNR.

      IF SY-SUBRC <> 0 .
*        PERFORM MAKE_BATCH_LOG USING 'D'.
      ELSE.
        CLEAR IT_TEMP_MLIT.
        READ TABLE IT_TEMP_MLIT WITH KEY MATNR = T_MATS-MATNR.

        IF SY-SUBRC = 0 .
*          PERFORM MAKE_BATCH_LOG USING 'F'.
        ELSE.
*          PERFORM MAKE_BATCH_LOG USING 'E'.
          CONCATENATE T_MATS-MATNR L_ARTNR INTO L_ARTNR
          SEPARATED BY SPACE.
        ENDIF.

      ENDIF.
    ENDLOOP.

    IF NOT L_ARTNR IS INITIAL.
      EXPORT  L_ARTNR TO  MEMORY ID 'ML03_ARTNR'.
    ENDIF.

  ENDIF.

ENDFORM.                    " UPDATE_ZTCO_MLIT
*&---------------------------------------------------------------------*
*&      Form  READ_MLCD
*&---------------------------------------------------------------------*
*       Read MLCD data
*----------------------------------------------------------------------*
FORM READ_MLCD.
  DATA: LS_MATS  TYPE S_MATS,
        LS_KALNR TYPE CKMV0_MATOBJ_STR.

  CLEAR   IT_KALNR.
  REFRESH IT_KALNR.

  LOOP AT T_MATS INTO LS_MATS.
    CLEAR LS_KALNR.
    LS_KALNR-KALNR = LS_MATS-KALNR.
    LS_KALNR-BWKEY = LS_MATS-BWKEY.
    APPEND LS_KALNR TO IT_KALNR.
  ENDLOOP.

* Read data
  CALL FUNCTION 'CKMCD_MLCD_READ'
       EXPORTING
            I_FROM_BDATJ      = P_BDATJ
            I_FROM_POPER      = P_POPER
            I_REFRESH_BUFFER  = 'X'
            I_ONLINE          = 'X'
       TABLES
            IT_KALNR          = IT_KALNR
            OT_MLCD           = T_MLCD
            OT_MLCD_NOT_ALLOC = T_MLCD_NOT_ALLOC
       EXCEPTIONS
            DATA_ERROR        = 1
            OTHERS            = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID   SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  SORT T_MLCD_NOT_ALLOC BY KALNR BDATJ POPER.

ENDFORM.                    " READ_MLCD
*&---------------------------------------------------------------------*
*&      Form  MERGE_MLCD_DATA
*&---------------------------------------------------------------------*
*       Merge to IT_MLCD
*----------------------------------------------------------------------*
FORM MERGE_MLCD_DATA.
  CLEAR : IT_MLCD, IT_MLCD[].
  CLEAR : IT_MLIT, IT_MLIT[].

* Ignore Process category for procurement alt. or consuption alt.
* Ignore Procurement alternative/process
* MLCD-PTYP MLCD-BVALT
  PERFORM ALLOCATED_DATA_MLCD.  " summary

* Not distributed
  PERFORM FILL_NDI_NIN.

* For Beginning Inv./Amt.
  PERFORM BEGINNING_INV_AMT.

* Calculate Values in the Category ' ' (SPACE)
  PERFORM CAL_SPACE_CAT.

* After PC, goods movement -> use new STD price.
  PERFORM READ_PREV_PST_VAR2.

* For  MVT Group; detail
  PERFORM MLDR_READ_DATA_BY_MVTGRP.
  PERFORM GET_FROM_MSEG.

* Calculate Cumulative / Ending -> update
  PERFORM CAL_CUMULATIVE_ENDING.

ENDFORM.                    " MERGE_MLCD_DATA

*&---------------------------------------------------------------------*
*&      Form  TRANS_VAL_TO_DIS
*&---------------------------------------------------------------------*
*       Transfering Value to Dis. Tab.
*----------------------------------------------------------------------*
FORM TRANS_VAL_TO_DIS.
  FIELD-SYMBOLS : <FSVAL> TYPE ANY.
  DATA  LV_FNAME(60).

  CLEAR IT_MLIT.

* Key Part
  IT_MLIT-KALNR = IT_MLCD-KALNR.

  IF IT_MLCD-CATEG <> 'PC' AND
     IT_MLCD-CATEG <> 'VP' AND
     IT_MLCD-CATEG <> 'ND' AND
     IT_MLCD-CATEG <> 'NI'.
* Qty.
    CLEAR LV_FNAME.
    CONCATENATE 'IT_MLIT' '-' IT_MLCD-CATEG '_LBKUM'
           INTO LV_FNAME.
    ASSIGN (LV_FNAME) TO <FSVAL>.

    <FSVAL> = IT_MLCD-LBKUM.

* Amt. / Valuated stock
    CLEAR LV_FNAME.
    CONCATENATE 'IT_MLIT' '-' IT_MLCD-CATEG '_SALK3'
           INTO LV_FNAME.
    ASSIGN (LV_FNAME) TO <FSVAL>.

    <FSVAL> = IT_MLCD-SALK3.
  ENDIF.

* Amt. / Price Difference (ERD+PRD In Category)
  CLEAR LV_FNAME.
  CONCATENATE 'IT_MLIT' '-' IT_MLCD-CATEG '_RD'
         INTO LV_FNAME.
  ASSIGN (LV_FNAME) TO <FSVAL>.

  <FSVAL> = IT_MLCD-ESTPRD
          + IT_MLCD-ESTKDM
          + IT_MLCD-MSTPRD
          + IT_MLCD-MSTKDM
          + IT_MLCD-TPPRD.

***ANDY
  IF IT_MLCD-CATEG = 'PC'.
    IT_MLIT-AB_SALK3 = - IT_MLCD-SALK3.
    IT_MLIT-AB_RD    = + IT_MLCD-SALK3.
    IT_MLIT-PC_SALK3 = + IT_MLCD-SALK3.
    IT_MLIT-PC_RD    = - IT_MLCD-SALK3.
  ENDIF.

* Append
  COLLECT IT_MLIT.
  CLEAR  IT_MLIT.

ENDFORM.                    " TRANS_VAL_TO_DIS

*&---------------------------------------------------------------------*
*&      Form  ALLOCATED_DATA_MLCD
*&---------------------------------------------------------------------*
*       For Allocated data / Unallocated Data
*----------------------------------------------------------------------*
FORM ALLOCATED_DATA_MLCD.
  DATA: L_OBJECT LIKE MARA-MATNR,
        L_CATEG(7).

  CLEAR T_MVT.
  REFRESH T_MVT.

* For Allocated Data
* Ending/Beginning Inv/Amt will be calculated later part
  LOOP AT T_MLCD WHERE CATEG <> 'AB'
                   AND CATEG <> 'EB'.
* Find object for GI
    CLEAR: IT_MLCD, L_OBJECT, L_CATEG.

    CASE T_MLCD-CATEG.
*     Receipts
      WHEN 'ZU'.
        CASE T_MLCD-PTYP.
*         Purchase: Purchase order
          WHEN 'BB' OR 'BBK'.
            L_CATEG = 'ZU_BB'.
*         Subcontracting: Subcontracting
          WHEN 'BL'.
            L_CATEG = 'ZU_BL'.
*         Production: Production
          WHEN 'BF'.
            L_CATEG = 'ZU_BF'.
*         Others: Procurement
          WHEN 'B+'.
            PERFORM APPEND_T_MVT.
*         Others: Sales order
          WHEN 'BKA'.
            L_CATEG = 'ZU_ETC'.
*         Others: Stock transfer
          WHEN 'BU'.
            READ TABLE T_MATS WITH KEY KALNR = T_MLCD-KALNR
                                   BINARY SEARCH.
            READ TABLE I_PROC_KALNR WITH KEY PROCK = T_MLCD-BVALT
                 BINARY SEARCH.
            IF SY-SUBRC <> 0.
              SELECT SINGLE WERKS MATNR BWTAR PROC_KALNR BTYP KALNR
                INTO I_PROC_KALNR
                FROM CKMLMV001
              WHERE PROC_KALNR = T_MLCD-BVALT.
            ENDIF.

            IF SY-SUBRC = 0 AND T_MATS-MATNR <> I_PROC_KALNR-MATNR.
              MOVE: I_PROC_KALNR-MATNR TO IT_MLCD-OBJECT.
              L_CATEG = 'ZU_BUBM'.
            ELSE.
              L_CATEG = 'ZU_BUBS'.
            ENDIF.

*           ...Material transfer posting
          WHEN 'BUBM'.
            L_CATEG = 'ZU_BUBM'.

*         Transfer posting-special stock: skip
          WHEN 'BUBS'.
            CLEAR T_MLCD-CATEG.

          WHEN OTHERS.
            CLEAR T_MLCD-CATEG.
        ENDCASE.

*     Consumption
      WHEN 'VN'.
*SIGN OF QTY,AMT
*        IF it_mlcd-categ = 'VN'.
        T_MLCD-LBKUM = - T_MLCD-LBKUM.
        T_MLCD-SALK3 = - T_MLCD-SALK3.
        T_MLCD-ESTPRD  = - T_MLCD-ESTPRD.
        T_MLCD-ESTKDM  = - T_MLCD-ESTKDM.
        T_MLCD-MSTPRD  = - T_MLCD-MSTPRD.
        T_MLCD-MSTKDM  = - T_MLCD-MSTKDM.
*        ENDIF.

        CASE T_MLCD-PTYP.
*         Sales
          WHEN 'VKA'.
            PERFORM APPEND_T_MVT.
*         Production
          WHEN 'VF'.
            L_CATEG = 'VN_VF'.
          WHEN  'VL'.
            L_CATEG = 'VN_VL'.
          WHEN 'V+'.
            PERFORM APPEND_T_MVT.
*         Others(CCTR GI)
          WHEN 'VK'.
            L_CATEG = 'VN_CC99'.
            PERFORM APPEND_T_MVT.
*         OS&D, Keyln(COB GI)
          WHEN 'VHP' OR 'VEAU'.
            PERFORM APPEND_T_MVT.
*         Others
          WHEN 'VA' OR 'VP'.
            PERFORM APPEND_T_MVT.
            L_CATEG = 'VK'.

          WHEN 'VU'.
            READ TABLE T_MATS WITH KEY KALNR = T_MLCD-KALNR
                                   BINARY SEARCH.
            READ TABLE I_PROC_KALNR WITH KEY PROCK = T_MLCD-BVALT
                 BINARY SEARCH.
            IF SY-SUBRC <> 0.
              SELECT SINGLE WERKS MATNR BWTAR PROC_KALNR BTYP KALNR
                INTO I_PROC_KALNR
                FROM CKMLMV001
              WHERE PROC_KALNR = T_MLCD-BVALT.
            ENDIF.

*           ...Material transfer posting
            IF SY-SUBRC = 0 AND T_MATS-MATNR <> I_PROC_KALNR-MATNR.
              MOVE: I_PROC_KALNR-MATNR TO IT_MLCD-OBJECT.
              L_CATEG = 'VN_VUBM'.                          " M2M
*           ...Stock transfer
            ELSE.
              L_CATEG = 'VN_VUBS'.
            ENDIF.

*          Transfer posting-special stock: skip
          WHEN 'VUBS'.
            CLEAR IT_MLCD-CATEG.
          WHEN OTHERS.
            CLEAR IT_MLCD-CATEG.
        ENDCASE.

      WHEN 'VP'.
        L_CATEG = 'VP'.
    ENDCASE.

* Transfer values
* Clear the values which are not useful.
    CLEAR : T_MLCD-MEINS, T_MLCD-WAERS.
    CLEAR : T_MLCD-BVALT, T_MLCD-PTYP.

    IF T_MLCD-CATEG <> SPACE.
      MOVE-CORRESPONDING T_MLCD TO IT_MLCD.

      IT_MLCD-CATEG = L_CATEG.
      COLLECT IT_MLCD.CLEAR IT_MLCD.
    ENDIF.
  ENDLOOP.

* Trasfer data to Display Tab.
  LOOP AT IT_MLCD.
    IF IT_MLCD-CATEG <> SPACE AND     " SPACE  Value
       IT_MLCD-CATEG <> 'AB' AND      " AB	    Beginning inventory
       IT_MLCD-CATEG <> 'EB'.         " EB	    Ending inventory
      PERFORM TRANS_VAL_TO_DIS.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ALLOCATED_DATA_MLCD

*&---------------------------------------------------------------------*
*&      Form  fill_material_info
*&---------------------------------------------------------------------*
*       Read Material Data / Unit / Curr.
*----------------------------------------------------------------------*
FORM FILL_MATERIAL_INFO.
* Read Material Information with Cost. Est. Number.
* read All data
* IT_KALNR

  CLEAR : MACKU, CKMLHD.

  LOOP AT IT_MLIT.
    CLEAR T_MATS.
    READ TABLE T_MATS WITH KEY
                      KALNR = IT_MLIT-KALNR
                      BINARY SEARCH.
    IF SY-SUBRC <> 0.
      WRITE:/ '***Fatal error...', IT_MLIT-KALNR.
    ELSE.
      MOVE : T_MATS-MATNR TO IT_MLIT-MATNR,
             T_MATS-BWKEY TO IT_MLIT-BWKEY,
             T_MATS-BWTAR TO IT_MLIT-BWTAR,
             T_MATS-MTART TO IT_MLIT-MTART,
             T_MATS-BKLAS TO IT_MLIT-BKLAS,
             T_MATS-MEINS TO IT_MLIT-MEINS.

      IT_MLIT-BUKRS = P_KOKRS.
      IT_MLIT-BDATJ = P_BDATJ.
      IT_MLIT-POPER = S_POPER-LOW.
      IT_MLIT-ERDAT = SY-DATUM.
      IT_MLIT-ERZET = SY-UZEIT.
      IT_MLIT-ERNAM = SY-UNAME.

      MODIFY IT_MLIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " fill_material_info

*&---------------------------------------------------------------------*
*&      Form  BEGINNING_INV_AMT
*&---------------------------------------------------------------------*
*       For Beginning Inv./Amt.
*----------------------------------------------------------------------*
FORM BEGINNING_INV_AMT.
  LOOP AT  T_MATS.
    CLEAR   IT_MLIT.
    IT_MLIT-KALNR = T_MATS-KALNR.

*   Beginning Qty + Previous Posting
    IT_MLIT-AB_LBKUM = T_MATS-ABKUMO.

*   Beginning Amt.
    IT_MLIT-AB_SALK3 = T_MATS-ABSALK3.

*   Beginning RD.
    IT_MLIT-AB_RD = T_MATS-ABPRD_O  + T_MATS-ABKDM_O
                   + T_MATS-ABPRD_MO + T_MATS-ABKDM_MO.

    COLLECT IT_MLIT.CLEAR IT_MLIT.
  ENDLOOP.

ENDFORM.                    " BEGINNING_INV_AMT
*&---------------------------------------------------------------------*
*&      Form  CAL_SPACE_CAT
*&---------------------------------------------------------------------*
*       Calculate Values in the Category ' ' (SPACE)
*----------------------------------------------------------------------*
FORM CAL_SPACE_CAT.
  DATA : IT_L_TMP_MLCD LIKE MLCD OCCURS 0 WITH HEADER LINE .

  CLEAR : IT_L_TMP_MLCD, IT_L_TMP_MLCD[].

* For Allocate (SAPCE)
  LOOP AT T_MLCD WHERE CATEG EQ SPACE  OR CATEG EQ 'AB'.
*   Clear the values which are not useful.
    CLEAR : T_MLCD-PTYP,  T_MLCD-BVALT.
    CLEAR : T_MLCD-MEINS, T_MLCD-WAERS.

*   --> AB
*   Transfer values
    CLEAR IT_L_TMP_MLCD.
    MOVE-CORRESPONDING T_MLCD TO IT_L_TMP_MLCD.
    IT_L_TMP_MLCD-CATEG = 'AB'.

    COLLECT IT_L_TMP_MLCD.CLEAR IT_L_TMP_MLCD.
    CLEAR T_MLCD.
  ENDLOOP.

* Put data into Disp. Tab.
  LOOP AT IT_L_TMP_MLCD.
    CLEAR IT_MLIT.
    IT_MLIT-KALNR = IT_L_TMP_MLCD-KALNR.

*   Qty.
    IT_MLIT-AB_LBKUM  = IT_L_TMP_MLCD-LBKUM.

*   Amt. / Valuated stock
    IT_MLIT-AB_SALK3  = IT_L_TMP_MLCD-SALK3.

*   Amt. / Price Difference (ERD+PRD In Category)
    IT_MLIT-AB_RD     = IT_L_TMP_MLCD-ESTPRD
                         + IT_L_TMP_MLCD-ESTKDM
                         + IT_L_TMP_MLCD-MSTPRD
                         + IT_L_TMP_MLCD-MSTKDM
                         + IT_L_TMP_MLCD-TPPRD.
    COLLECT IT_MLIT.
    CLEAR   IT_MLIT.
    CLEAR   IT_L_TMP_MLCD.
  ENDLOOP.

ENDFORM.                    " CAL_SPACE_CAT

*&---------------------------------------------------------------------*
*&      Form  SET_BEGINNING_DATA_FOR_NO_TR
*&---------------------------------------------------------------------*
* Data in MLCD could not be generated if any transaction
* is executied at specific period
* Set Beginning Inv. from Ending Inv. data of previous period.
*----------------------------------------------------------------------*
FORM SET_BEGINNING_DATA_FOR_NO_TR.
  DATA : IT_L_DIS LIKE STANDARD TABLE OF IT_MLIT
                  WITH HEADER LINE .

  CLEAR IT_MLIT.
  CLEAR IT_KALNR.
  CLEAR MBEW.

* If PAST period and it_mlit[] is initial. Do not proceed futher.
  CHECK  P_BDATJ => SY-DATUM(4)
    AND  P_POPER => SY-DATUM+4(2)
    AND  IT_MLIT[] IS INITIAL.


  SORT IT_MLIT BY KALNR.

  LOOP AT IT_KALNR.
    CLEAR IT_L_DIS.
    CLEAR IT_MLIT.
    READ TABLE  IT_MLIT WITH KEY KALNR = IT_KALNR-KALNR
                           BINARY SEARCH.
    IF SY-SUBRC <> 0.
*     MBEW
      CLEAR MBEW.
      SELECT SINGLE * FROM MBEW
                     WHERE MATNR = IT_KALNR-MATNR
                       AND BWKEY = IT_KALNR-BWKEY
                       AND BWTAR = IT_KALNR-BWTAR.
*     Set Data
      MOVE-CORRESPONDING MBEW TO IT_L_DIS.
*     Values
      IT_L_DIS-AB_LBKUM = MBEW-LBKUM.
      IT_L_DIS-AB_SALK3 = MBEW-SALK3.
      IT_L_DIS-KALNR = IT_KALNR-KALNR.

      APPEND IT_L_DIS.
      CLEAR  IT_L_DIS.
    ENDIF.
    CLEAR IT_KALNR.
  ENDLOOP.

* Add itab to Display-ITAB
  IF NOT IT_L_DIS[] IS INITIAL.
    APPEND LINES OF  IT_L_DIS  TO IT_MLIT.
  ENDIF.

ENDFORM.                    " SET_BEGINNING_DATA_FOR_NO_TR

*&---------------------------------------------------------------------*
*&      Form  CHK_FT_DATE
*&---------------------------------------------------------------------*
*       Check Future Period
*----------------------------------------------------------------------*
FORM CHK_FT_DATE.
  IF  P_BDATJ > SY-DATUM(4).
    MESSAGE E000 WITH TEXT-301 P_BDATJ P_POPER.
  ELSE.
*requested by andy
*changed by wskim,on 01/18/2005 : Program error
*-----Start
*    IF  P_POPER > SY-DATUM+4(2).
*      MESSAGE E000 WITH TEXT-301 p_bdatj P_POPER.
*    ENDIF.
*-----End
  ENDIF.

  IF P_DB IS INITIAL AND NOT S_POPER-HIGH IS INITIAL.
    MESSAGE E000 WITH TEXT-302 S_POPER-HIGH.
  ENDIF.

ENDFORM.                    " CHK_FT_DATE

*&---------------------------------------------------------------------*
*&      Form  CHK_UP_OPT
*&---------------------------------------------------------------------*
*       Chk. Update Option
*----------------------------------------------------------------------*
FORM CHK_UP_OPT.
* the authorization rule about CO is not defined yet.
* So, temporary, prevent running through update mode with T-code
  IF SY-TCODE = 'ZCOR11'.
    CLEAR  P_UP.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'DIS'.
        SCREEN-INVISIBLE = '1'.
        SCREEN-ACTIVE    = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.
ENDFORM.                    " CHK_UP_OPT

*&---------------------------------------------------------------------*
*&      Form  find_bad_boys
*&---------------------------------------------------------------------*
FORM FIND_BAD_BOYS.
* refer MLHELP_VALUE_FLOW_ANALYZER program
  DATA: LS_NDI_NDI TYPE S_NDI,
        LS_NDI_CUM TYPE S_NDI,
        LS_NDI_NIN TYPE S_NDI,
        LS_MATS TYPE S_MATS,
        LS_CKMLPP TYPE CKMLPP,
        LS_CKMLCR TYPE CKMLCR,
        LS_MLCD TYPE MLCD,
        LS_MLCD_NOT_ALLOC TYPE MLCD,
        L_COLOR(3) TYPE C,
        L_AB_MENGE LIKE MLCD-LBKUM,
        L_NIN TYPE BOOLE_D,
        L_KALNR_OLD LIKE MLCD-KALNR.

  CLEAR T_MATS.

  LOOP AT T_MATS INTO LS_MATS.
    MOVE-CORRESPONDING LS_MATS TO LS_CKMLPP.
    MOVE-CORRESPONDING LS_MATS TO LS_CKMLCR.

    CLEAR: LS_NDI_NDI, LS_NDI_CUM, LS_NDI_NIN.

    MOVE-CORRESPONDING LS_CKMLPP  TO LS_NDI_NDI.
    MOVE-CORRESPONDING LS_CKMLPP  TO LS_NDI_CUM.
    MOVE-CORRESPONDING LS_CKMLPP  TO LS_NDI_NIN.

    IF LS_CKMLPP-STATUS >= '40'.
      MOVE-CORRESPONDING LS_CKMLCR  TO LS_NDI_NDI.
      MOVE-CORRESPONDING LS_CKMLCR  TO LS_NDI_CUM.
      MOVE-CORRESPONDING LS_CKMLCR  TO LS_NDI_NIN.

      LS_NDI_CUM-ESTPRD = LS_MATS-ABPRD_O + LS_MATS-ZUPRD_O +
                          LS_MATS-VPPRD_O.

      LS_NDI_CUM-ESTKDM = LS_MATS-ABKDM_O + LS_MATS-ZUKDM_O +
                          LS_MATS-VPKDM_O.

      IF LS_CKMLPP-STATUS >= '50'.
        LS_NDI_CUM-MSTPRD = LS_MATS-ABPRD_MO + LS_MATS-ZUPRD_MO.
        LS_NDI_CUM-MSTKDM = LS_MATS-ABKDM_MO + LS_MATS-ZUKDM_MO.
      ELSE.
        LS_NDI_CUM-MSTPRD = LS_MATS-ABPRD_MO.
        LS_NDI_CUM-MSTKDM = LS_MATS-ABKDM_MO.
      ENDIF.

      LS_NDI_NDI-ESTPRD = LS_NDI_CUM-ESTPRD.
      LS_NDI_NDI-ESTKDM = LS_NDI_CUM-ESTKDM.
      LS_NDI_NDI-MSTPRD = LS_NDI_CUM-MSTPRD.
      LS_NDI_NDI-MSTKDM = LS_NDI_CUM-MSTKDM.

      LS_NDI_NDI-ESTPRD = LS_NDI_NDI-ESTPRD -
                          ( LS_MATS-VNPRD_EA + LS_MATS-EBPRD_EA ).

      LS_NDI_NDI-ESTKDM = LS_NDI_NDI-ESTKDM -
                          ( LS_MATS-VNKDM_EA + LS_MATS-EBKDM_EA ).

      LS_NDI_NDI-MSTPRD = LS_NDI_NDI-MSTPRD -
                          ( LS_MATS-VNPRD_MA + LS_MATS-EBPRD_MA ).

      LS_NDI_NDI-MSTKDM = LS_NDI_NDI-MSTKDM -
                          ( LS_MATS-VNKDM_MA + LS_MATS-EBKDM_MA ).

      LS_NDI_NDI-SUMDIF = LS_NDI_NDI-ESTPRD + LS_NDI_NDI-ESTKDM +
                          LS_NDI_NDI-MSTPRD + LS_NDI_NDI-MSTKDM.

      READ TABLE T_MLCD_NOT_ALLOC INTO LS_MLCD_NOT_ALLOC
                                   WITH KEY KALNR = LS_MATS-KALNR
                                            BDATJ = P_BDATJ
                                            POPER = P_POPER
                                           BINARY SEARCH.
      IF SY-SUBRC = 0.
        L_NIN = 'X'.
      ELSE.
        CLEAR L_NIN.
      ENDIF.

      IF NOT LS_NDI_NDI-SUMDIF IS INITIAL OR
         NOT L_NIN IS INITIAL.
        IF LS_NDI_NDI-KALNR <> L_KALNR_OLD.
          L_KALNR_OLD = LS_NDI_NDI-KALNR.
          IF L_COLOR = 'C21'.
            L_COLOR = 'C20'.
          ELSE.
            L_COLOR = 'C21'.
          ENDIF.
        ENDIF.

        MOVE-CORRESPONDING LS_MATS TO LS_NDI_CUM.
        MOVE-CORRESPONDING LS_MATS TO LS_NDI_NDI.
        MOVE-CORRESPONDING LS_MATS TO LS_NDI_NIN.

*       Not distributed
        IF NOT LS_NDI_NDI-SUMDIF IS INITIAL.
          LS_NDI_NDI-POS_TYPE = 'NDI'.
          LS_NDI_NDI-COLOR = L_COLOR.

          CLEAR: LS_NDI_NDI-MENGE, LS_NDI_NDI-WERT.
          LS_NDI_NDI-PRDIF = LS_NDI_NDI-ESTPRD + LS_NDI_NDI-MSTPRD.
          LS_NDI_NDI-KRDIF = LS_NDI_NDI-ESTKDM + LS_NDI_NDI-MSTKDM.
          LS_NDI_NDI-ESTDIF = LS_NDI_NDI-ESTPRD + LS_NDI_NDI-ESTKDM.
          LS_NDI_NDI-MSTDIF = LS_NDI_NDI-MSTPRD + LS_NDI_NDI-MSTKDM.

          APPEND LS_NDI_NDI TO T_BAD.
        ENDIF.

*       Not included
        IF NOT L_NIN IS INITIAL.
          LS_NDI_NIN-POS_TYPE = 'NIN'.
          LS_NDI_NIN-COLOR = L_COLOR.
          CLEAR: LS_NDI_NIN-MENGE, LS_NDI_NIN-WERT.
          LS_NDI_NIN-ESTPRD = LS_MLCD_NOT_ALLOC-ESTPRD.
          LS_NDI_NIN-ESTKDM = LS_MLCD_NOT_ALLOC-ESTKDM.
          LS_NDI_NIN-MSTPRD = LS_MLCD_NOT_ALLOC-MSTPRD.
          LS_NDI_NIN-MSTKDM = LS_MLCD_NOT_ALLOC-MSTKDM.
          LS_NDI_NIN-PRDIF = LS_NDI_NIN-ESTPRD + LS_NDI_NIN-MSTPRD.
          LS_NDI_NIN-KRDIF = LS_NDI_NIN-ESTKDM + LS_NDI_NIN-MSTKDM.
          LS_NDI_NIN-ESTDIF = LS_NDI_NIN-ESTPRD + LS_NDI_NIN-ESTKDM.
          LS_NDI_NIN-MSTDIF = LS_NDI_NIN-MSTPRD + LS_NDI_NIN-MSTKDM.

          LS_NDI_NIN-SUMDIF = LS_NDI_NIN-ESTPRD + LS_NDI_NIN-ESTKDM +
                                LS_NDI_NIN-MSTPRD + LS_NDI_NIN-MSTKDM.

          APPEND LS_NDI_NIN TO T_BAD.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " get_material_periods
*&---------------------------------------------------------------------*
*&      Form  fill_ndi_nin
*&---------------------------------------------------------------------*
FORM FILL_NDI_NIN.
* Not Include --> Consumption Diff.
  LOOP AT T_BAD.
    CLEAR IT_MLCD.

    IT_MLCD-KALNR  = T_BAD-KALNR.
    IT_MLCD-BDATJ  = T_BAD-BDATJ.
    IT_MLCD-POPER  = T_BAD-POPER.
    IT_MLCD-CURTP  = T_BAD-CURTP.

*NO change of sign!!! - by Andy Choi
    IF T_BAD-POS_TYPE = 'NDI'.
      IT_MLCD-CATEG = 'ND'.
      IT_MLCD-ESTPRD = - T_BAD-ESTPRD.
      IT_MLCD-ESTKDM = - T_BAD-ESTKDM.
      IT_MLCD-MSTPRD = - T_BAD-MSTPRD.
      IT_MLCD-MSTKDM = - T_BAD-MSTKDM.
    ELSE.
      IT_MLCD-CATEG = 'NI'.
      IT_MLCD-ESTPRD = - T_BAD-ESTPRD.
      IT_MLCD-ESTKDM = - T_BAD-ESTKDM.
      IT_MLCD-MSTPRD = - T_BAD-MSTPRD.
      IT_MLCD-MSTKDM = - T_BAD-MSTKDM.
    ENDIF.

    PERFORM TRANS_VAL_TO_DIS.

  ENDLOOP.

ENDFORM.                    " fill_ndi_nin
*&---------------------------------------------------------------------*
*&      Form  get_materials_from_closing
*&---------------------------------------------------------------------*
FORM GET_MATERIALS_FROM_CLOSING.
  TABLES MARV.

  TYPES: BEGIN OF TY_T001K,
           BWKEY TYPE BWKEY,
         END OF TY_T001K.

  DATA LT_T001K TYPE TABLE OF TY_T001K WITH HEADER LINE.

  RANGES: R_BWKEY FOR T001K-BWKEY.

  CLEAR:   LT_T001K, R_BWKEY.
  REFRESH: LT_T001K, R_BWKEY.

  R_BWKEY-SIGN   = 'I'.
  R_BWKEY-OPTION = 'EQ'.

  REFRESH T_MATS.

  SELECT BWKEY INTO TABLE LT_T001K
    FROM T001K
   WHERE BUKRS = P_KOKRS.

  IF S_BWKEY[] IS INITIAL.
    LOOP AT LT_T001K.
      R_BWKEY-LOW = LT_T001K-BWKEY.
      APPEND R_BWKEY.
    ENDLOOP.
  ELSE.
    LOOP AT LT_T001K WHERE BWKEY IN S_BWKEY.
      R_BWKEY-LOW = LT_T001K-BWKEY.
      APPEND R_BWKEY.
    ENDLOOP.
  ENDIF.

  CLEAR R_BWKEY.
*
  DATA: L_LFGJA TYPE LFGJA,
        L_LFMON TYPE LFMON,
        L_ABRECHDAT LIKE CKMLHD-ABRECHDAT.

  CLEAR:  L_LFGJA, L_LFMON, T_MATS.
  REFRESH T_MATS.

  DATA: L_DATE LIKE SY-DATUM.
  SELECT SINGLE LAST_DAY INTO L_DATE FROM CKMLRUNPERIOD
      INNER JOIN CKMLMV011
         ON  CKMLMV011~LAUFID = CKMLRUNPERIOD~RUN_ID
      WHERE GJAHR = P_BDATJ
        AND POPER = P_POPER
        AND BWKEY IN R_BWKEY.

  IF SY-SUBRC <> 0.

    SELECT MBEW~BWKEY MBEW~MATNR MBEW~BWTAR
           MAKT~MAKTG
           CKMLHD~KALNR
           MARA~MTART MARA~MATKL
           MBEW~BKLAS
           CKMLCR~STPRS
           CKMLCR~PVPRS AS VERPR
           CKMLPP~MEINS
           CKMLPP~STATUS
           CKMLPP~ABKUMO   CKMLPP~UMKUMO   CKMLPP~ZUKUMO
           CKMLPP~VNKUMO   CKMLPP~LBKUM    CKMLPP~EKKUMO
           CKMLCR~PEINH
           CKMLCR~ABSALK3  CKMLCR~ABPRD_O  CKMLCR~ABKDM_O
           CKMLCR~ABPRD_MO CKMLCR~ABKDM_MO
           CKMLCR~VPPRD_O  CKMLCR~ZUPRD_O
           CKMLCR~ZUKDM_O  CKMLCR~VPKDM_O
           CKMLCR~ZUPRD_MO CKMLCR~ZUKDM_MO
           CKMLCR~VNPRD_EA CKMLCR~VNKDM_EA
           CKMLCR~EBPRD_EA CKMLCR~EBKDM_EA
           CKMLCR~VNPRD_MA CKMLCR~VNKDM_MA
           CKMLCR~EBPRD_MA CKMLCR~EBKDM_MA
       INTO CORRESPONDING FIELDS OF TABLE T_MATS
             FROM ( MBEW
                    INNER JOIN MARA
                      ON  MBEW~MATNR = MARA~MATNR
                    INNER JOIN MAKT
                      ON  MAKT~MATNR = MARA~MATNR
                     AND  MAKT~SPRAS = SY-LANGU
                    INNER JOIN MARC
                      ON  MBEW~MATNR = MARC~MATNR
                      AND MBEW~BWKEY = MARC~WERKS
                    INNER JOIN CKMLCR
                      ON  CKMLCR~KALNR = MBEW~KALN1
                     AND  CKMLCR~BDATJ = P_BDATJ
                     AND  CKMLCR~POPER = P_POPER
                     AND  CKMLCR~CURTP = P_CURTP
                     AND  CKMLCR~UNTPER = SPACE
                    INNER JOIN CKMLPP
                      ON  CKMLPP~KALNR  = CKMLCR~KALNR
                     AND  CKMLPP~BDATJ  = CKMLCR~BDATJ
                     AND  CKMLPP~POPER  = CKMLCR~POPER
                     AND  CKMLPP~UNTPER = SPACE
                    INNER JOIN CKMLHD
                      ON  CKMLHD~KALNR = MBEW~KALN1 )
             WHERE MBEW~BWKEY IN R_BWKEY
               AND MBEW~BKLAS IN S_BKLAS
               AND MARA~MTART IN S_MTART
               AND MBEW~MATNR IN S_MATNR
               AND CKMLHD~ABRECHDAT <> L_ABRECHDAT
               AND (   CKMLPP~ZUKUMO <> 0 OR CKMLPP~VNKUMO <> 0
                    OR CKMLPP~ABKUMO <> 0 OR CKMLPP~UMKUMO <> 0
                    OR CKMLPP~LBKUM <> 0 )
      ORDER BY CKMLHD~KALNR.

    SORT T_MATS BY KALNR.
    PERFORM GET_VALUATION_CLASS.

  ELSE.
    SELECT MBEW~BWKEY MBEW~MATNR MBEW~BWTAR
           MAKT~MAKTG
           CKMLHD~KALNR
           CKMLMV011~MTART CKMLMV011~MATKL
           CKMLMV011~BKLAS AS VKLAS     "INCORRECT!!! - ANDY
           CKMLCR~STPRS
           CKMLCR~PVPRS AS VERPR
           CKMLPP~MEINS
           CKMLPP~STATUS
           CKMLPP~ABKUMO   CKMLPP~UMKUMO   CKMLPP~ZUKUMO
           CKMLPP~VNKUMO   CKMLPP~LBKUM    CKMLPP~EKKUMO
           CKMLCR~PEINH
           CKMLCR~ABSALK3  CKMLCR~ABPRD_O  CKMLCR~ABKDM_O
           CKMLCR~ABPRD_MO CKMLCR~ABKDM_MO
           CKMLCR~VPPRD_O  CKMLCR~ZUPRD_O
           CKMLCR~ZUKDM_O  CKMLCR~VPKDM_O
           CKMLCR~ZUPRD_MO CKMLCR~ZUKDM_MO
           CKMLCR~VNPRD_EA CKMLCR~VNKDM_EA
           CKMLCR~EBPRD_EA CKMLCR~EBKDM_EA
           CKMLCR~VNPRD_MA CKMLCR~VNKDM_MA
           CKMLCR~EBPRD_MA CKMLCR~EBKDM_MA
       INTO CORRESPONDING FIELDS OF TABLE T_MATS
             FROM ( MBEW
                    INNER JOIN MARA
                      ON  MBEW~MATNR = MARA~MATNR
                    INNER JOIN MAKT
                      ON  MAKT~MATNR = MARA~MATNR
                     AND  MAKT~SPRAS = SY-LANGU
                    INNER JOIN MARC
                      ON  MBEW~MATNR = MARC~MATNR
                      AND MBEW~BWKEY = MARC~WERKS
                    INNER JOIN CKMLMV011
                      ON  CKMLMV011~MATNR = MBEW~MATNR
                      AND CKMLMV011~BWKEY = MBEW~BWKEY
                    INNER JOIN CKMLRUNPERIOD
                      ON  CKMLMV011~LAUFID = CKMLRUNPERIOD~RUN_ID
                    INNER JOIN CKMLCR
                      ON  CKMLCR~KALNR  = CKMLMV011~KALNR
                     AND  CKMLCR~BDATJ  = CKMLRUNPERIOD~GJAHR
                     AND  CKMLCR~POPER  = CKMLRUNPERIOD~POPER
                     AND  CKMLCR~CURTP  = '10'
                     AND  CKMLCR~UNTPER = SPACE
                    INNER JOIN CKMLPP
                      ON  CKMLPP~KALNR  = CKMLCR~KALNR
                     AND  CKMLPP~BDATJ  = CKMLCR~BDATJ
                     AND  CKMLPP~POPER  = CKMLCR~POPER
                     AND  CKMLPP~UNTPER = SPACE
                    INNER JOIN CKMLHD
                      ON  CKMLMV011~KALNR = CKMLHD~KALNR )
             WHERE CKMLRUNPERIOD~GJAHR = P_BDATJ
               AND CKMLRUNPERIOD~POPER = P_POPER
               AND CKMLMV011~BWKEY IN R_BWKEY
*               and ckmlmv011~bklas in s_bklas
               AND CKMLMV011~MTART IN S_MTART
               AND CKMLMV011~MATNR IN S_MATNR
               AND CKMLHD~ABRECHDAT <> L_ABRECHDAT
               AND (   CKMLPP~ZUKUMO <> 0 OR CKMLPP~VNKUMO <> 0
                    OR CKMLPP~ABKUMO <> 0 OR CKMLPP~UMKUMO <> 0
                    OR CKMLPP~LBKUM <> 0 )
      ORDER BY CKMLHD~KALNR.

    PERFORM GET_EXTRA_DATA_MBEW.

    SORT T_MATS BY KALNR.
    DELETE ADJACENT DUPLICATES FROM T_MATS COMPARING KALNR.

    SORT T_MATS BY KALNR.
    PERFORM GET_VALUATION_CLASS.
  ENDIF.

ENDFORM.                    " get_materials_from_closing
*&---------------------------------------------------------------------*
*&      Form  get_data_from_buffer
*&---------------------------------------------------------------------*
FORM GET_DATA_FROM_BUFFER.
  IF S_POPER-HIGH IS INITIAL.
    S_POPER-HIGH = S_POPER-LOW.
  ENDIF.

  SELECT *
      FROM ZTCO_MLIT
     WHERE BUKRS = P_KOKRS
       AND BDATJ = P_BDATJ
*         and poper = p_poper
       AND POPER IN  S_POPER
       AND MATNR IN S_MATNR
       AND BKLAS IN S_BKLAS
       AND MTART IN S_MTART.

    IF S_POPER-LOW <> S_POPER-HIGH.
      IF   ZTCO_MLIT-POPER <> S_POPER-LOW.
        CLEAR: ZTCO_MLIT-AB_SALK3,
               ZTCO_MLIT-AB_RD,
               ZTCO_MLIT-AB_LBKUM.
      ENDIF.
      IF ZTCO_MLIT-POPER <> S_POPER-HIGH.
        CLEAR: ZTCO_MLIT-EB_SALK3,
               ZTCO_MLIT-EB_RD,
               ZTCO_MLIT-EB_LBKUM,
               ZTCO_MLIT-LBKUM,
               ZTCO_MLIT-SALK3,
               ZTCO_MLIT-VERPR.
      ENDIF.
    ENDIF.

    CLEAR: ZTCO_MLIT-POPER,
           ZTCO_MLIT-ERDAT,
           ZTCO_MLIT-ERNAM,
           ZTCO_MLIT-ERZET.

    MOVE-CORRESPONDING ZTCO_MLIT TO IT_MLIT.
    COLLECT IT_MLIT. CLEAR IT_MLIT.
  ENDSELECT.

ENDFORM.                    " get_data_from_buffer
*&---------------------------------------------------------------------*
*&      Form  call_alv_list
*&---------------------------------------------------------------------*
FORM CALL_ALV_LIST.
**  PERFORM FIELD_SETTING(ZCOGSREV) TABLES GT_FIELDCAT USING :
** 'MTART'          'Mtyp'        '04' ' ' 'L'  ' '  ' '  '  ' ' '  '
*,
*** 'BKLAS'           'V.Cl'        '04' ' ' 'L'  ' '  ' '  '  ' ' '  '
*,
** 'MATNR'            'Material'    '18' 'X' 'L'  ' '  ' '  '  ' ' '  '
*,
** 'BWKEY'            'Plant'       '04' ' ' 'L'  ' '  ' '  '  ' ' '  '
*,
** 'BWTAR'            'V.Typ'       '06' ' ' 'L'  ' '  ' '  '  ' ' '  '
*,
** 'KALNR'            'CostNo'      '12' ' ' 'L'  ' '  ' '  '  ' ' '  '
*,
** 'MEINS'            'UoM'         '04' ' ' 'L'  ' '  ' '  '  ' ' '  '
*,
**
**'AB_LBKUM'       ' BIG   '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS' ' ',
**'AB_SALK3'       '$BIG  S'    '16' ' ' 'R' ' ' ' ' ' ' '     '  ' ',
**'AB_RD'          '$BIG  V'    '16' ' ' 'R' ' ' ' ' ' ' '     '  ' ',
**
***'ZU_PC_LBKUM'    ' PChg'      '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
***'ZU_PC_SALK3'    '$Pchg S'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
***'ZU_PC_RD'       '$Pchg V'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**
**'ZU_BB_LBKUM'    ' GR MM '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'ZU_BB_SALK3'    '$GR MMS'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_BB_RD'       '$GR MMV'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_BF_LBKUM'    ' GR PP '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'ZU_BF_SALK3'    '$GR PPS'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_BF_RD'       '$GR PPV'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_BL_LBKUM'    ' GR OS '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'ZU_BL_SALK3'    '$GR OSS'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_BL_RD'       '$GR OSV'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_BUBS_LBKUM'  ' GR TRF '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'ZU_BUBS_SALK3'  '$GR TRFS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_BUBS_RD'     '$GR TRFV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_BUBM_LBKUM'  ' GR M2M'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'ZU_BUBM_SALK3'  '$GR M2MS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_BUBM_RD'     '$GR M2MV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_VP_LBKUM'    ' GR D/C'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'ZU_VP_SALK3'    '$GR D/CS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_VP_RD'       '$GR D/CV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_ETC_LBKUM'   ' GR ETC'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'ZU_ETC_SALK3'   '$GR ETC S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_ETC_RD'      '$GR ETC V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'ZU_ND_RD'       '$GR NoDst'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X'.
**
**'VN_NI_RD'       '$GI NoInc'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VKA16_LBKUM' ' GI SD  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_VKA16_SALK3' '$GI SD S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VKA16_RD'    '$GI SD V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VKA20_LBKUM' ' GI SE  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_VKA20_SALK3' '$GI SE S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VKA20_RD'    '$GI SE V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VKA99_LBKUM' ' GI SX  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_VKA99_SALK3' '$GI SX S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VKA99_RD'    '$GI SX V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VF_LBKUM'    ' GI PP  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_VF_SALK3'    '$GI PP S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VF_RD'       '$GI PP V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_COB_LBKUM'   ' GI COB  '  '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_COB_SALK3'   '$GI COB S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_COB_RD'      '$GI COB V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_OR66_LBKUM'  ' GI Key '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_OR66_SALK3'  '$GI Key S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_OR66_RD'     '$GI Key V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_OR58_LBKUM'  ' GI OSD  '  '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_OR58_SALK3'  '$GI OSD S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_OR58_RD'     '$GI OSD V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_CC60_LBKUM'  ' GI PhyIn ' '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_CC60_SALK3'  '$GI PhyInS' '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_CC60_RD'     '$GI PhyInV' '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_CC99_LBKUM'  ' GI Mis Q'  '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_CC99_SALK3'  '$GI Mis S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_CC99_RD'     '$GI Mis V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VUBS_LBKUM'  ' GI TRF'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_VUBS_SALK3'  '$GI TRFS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VUBS_RD'     '$GI TRFV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VUBM_LBKUM'  ' GI M2M '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'VN_VUBM_SALK3'  '$GI M2M S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'VN_VUBM_RD'     '$GI M2M V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*
**'EB_LBKUM'       ' End Q'     '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
**'EB_SALK3'       '$End S'     '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
**'EB_RD'          '$End V'     '16' ' ' 'R' ' ' ' ' ' ' '     '  'X'.

*
*
*  G_REPID = SY-REPID.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*       EXPORTING
*            I_CALLBACK_PROGRAM      = G_REPID
*            IT_FIELDCAT             = GT_FIELDCAT
*            I_CALLBACK_USER_COMMAND = G_USER_COMMAND
**            I_SAVE                  = 'A'
*       TABLES
*            T_OUTTAB                = IT_MLIT1
*       EXCEPTIONS
*            PROGRAM_ERROR           = 1
*            OTHERS                  = 2.

  CLEAR: GT_FIELDCAT, GS_LAYOUT, GT_EVENTS, GS_VARIANT,
         GT_FIELDCAT[], GT_EVENTS[].

  PERFORM BUILD_FIELD_CATEGORY USING:
    'MTART'          'Mtyp'      4   'CHAR',
    'MATNR'          'Material'  18  'CHAR',
    'BWKEY'          'Plant'     4   'CHAR',
    'BWTAR'          'V.Typ'     6   'CHAR',
    'KALNR'          'CostNo'    12  'CHAR',
    'BKLAS'          'ValCl'     4   'CHAR',
    'MEINS'          'UoM'       4   'CHAR',

    'AB_LBKUM'       ' BIG   '   16  'QUAN',
    'AB_SALK3'       '$BIG  S'   16  'CURR',
    'AB_RD'          '$BIG  V'   16  'CURR',

    'PC_SALK3'       '$Pchg S'  16  'CURR',
    'PC_RD'          '$Pchg V'  16  'CURR',

    'ZU_BB_LBKUM'    ' GR MM '   16  'QUAN',
    'ZU_BB_SALK3'    '$GR MMS'   16  'CURR',
    'ZU_BB_RD'       '$GR MMV'   16  'CURR',

    'ZU_BF_LBKUM'    ' GR PP '   16  'QUAN',
    'ZU_BF_SALK3'    '$GR PPS'   16  'CURR',
    'ZU_BF_RD'       '$GR PPV'   16  'CURR',

    'ZU_BL_LBKUM'    ' GR OS '   16  'QUAN',
    'ZU_BL_SALK3'    '$GR OSS'   16  'CURR',
    'ZU_BL_RD'       '$GR OSV'   16  'CURR',

    'ZU_BUBS_LBKUM'  ' GR TRF '   16  'QUAN',
    'ZU_BUBS_SALK3'  '$GR TRFS'   16  'CURR',
    'ZU_BUBS_RD'     '$GR TRFV'   16  'CURR',

    'ZU_BUBM_LBKUM'  ' GR M2M'    16  'QUAN',
    'ZU_BUBM_SALK3'  '$GR M2MS'   16  'CURR',
    'ZU_BUBM_RD'     '$GR M2MV'   16  'CURR',

*    'VP_LBKUM'       ' GR D/C'    16  'QUAN',
*    'VP_SALK3'       '$GR D/CS'   16  'CURR',
    'VP_RD'          '$GR D/CV'   16  'CURR',

    'ZU_ETC_LBKUM'   ' GR ETC'    16  'QUAN',
    'ZU_ETC_SALK3'   '$GR ETC S'  16  'CURR',
    'ZU_ETC_RD'      '$GR ETC V'  16  'CURR',

    'VN_VKA16_LBKUM' ' GI SD  '   16  'QUAN',
    'VN_VKA16_SALK3' '$GI SD S'   16  'CURR',
    'VN_VKA16_RD'    '$GI SD V'   16  'CURR',

    'VN_VKA20_LBKUM' ' GI SE  '   16   'QUAN',
    'VN_VKA20_SALK3' '$GI SE S'   16   'CURR',
    'VN_VKA20_RD'    '$GI SE V'   16   'CURR',

    'VN_VF_LBKUM'    ' GI PP  '   16   'QUAN',
    'VN_VF_SALK3'    '$GI PP S'   16   'CURR',
    'VN_VF_RD'       '$GI PP V'   16   'CURR',

    'VN_VL_LBKUM'    ' GI OS '   16  'QUAN',
    'VN_VL_SALK3'    '$GI OSS'   16  'CURR',
    'VN_VL_RD'       '$GI OSV'   16  'CURR',

    'VN_COB_LBKUM'   ' GI COB  '  16   'QUAN',
    'VN_COB_SALK3'   '$GI COB S'  16   'CURR',
    'VN_COB_RD'      '$GI COB V'  16   'CURR',

    'VN_OR66_LBKUM'  ' GI Key '   16   'QUAN',
    'VN_OR66_SALK3'  '$GI Key S'  16   'CURR',
    'VN_OR66_RD'     '$GI Key V'  16   'CURR',

    'VN_OR58_LBKUM'  ' GI OSD  '  16   'QUAN',
    'VN_OR58_SALK3'  '$GI OSD S'  16   'CURR',
    'VN_OR58_RD'     '$GI OSD V'  16   'CURR',

    'VN_CC99_LBKUM'  ' GI Mis Q'  16   'QUAN',
    'VN_CC99_SALK3'  '$GI Mis S'  16   'CURR',
    'VN_CC99_RD'     '$GI Mis V'  16   'CURR',

    'VN_VUBS_LBKUM'  ' GI TRF'    16   'QUAN',
    'VN_VUBS_SALK3'  '$GI TRFS'   16   'CURR',
    'VN_VUBS_RD'     '$GI TRFV'   16   'CURR',

    'VN_VUBM_LBKUM'  ' GI M2M '   16   'QUAN',
    'VN_VUBM_SALK3'  '$GI M2M S'  16   'CURR',
    'VN_VUBM_RD'     '$GI M2M V'  16   'CURR',

    'ND_RD'          '$GI NoDst'  16  'CURR',

    'EB_LBKUM'       ' End Q'     16   'QUAN',
    'EB_SALK3'       '$End S'     16   'CURR',
    'EB_RD'          '$End V'     16   'CURR'.

  GS_FIELDCAT-JUST = 'L'.
  MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING JUST
     WHERE DATATYPE = 'CHAR'.

  GS_FIELDCAT-JUST = 'R'.
  GS_FIELDCAT-DO_SUM = 'X'.
  MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING JUST DO_SUM
     WHERE DATATYPE = 'CURR' OR DATATYPE = 'QUAN'.

  CLEAR: GS_FIELDCAT-JUST, GS_FIELDCAT-DO_SUM.
  GS_FIELDCAT-QFIELDNAME = 'MEINS'.
  MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING QFIELDNAME
     WHERE DATATYPE = 'QUAN'.

* Set layout
  CLEAR GS_LAYOUT.
  GS_LAYOUT-ZEBRA             = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

* Set callback program
  G_REPID = GS_VARIANT-REPORT = SY-REPID.

* Set variant
  GS_VARIANT-VARIANT = P_VARI.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM      = G_REPID
            IS_LAYOUT               = GS_LAYOUT
            IT_FIELDCAT             = GT_FIELDCAT[]
            I_CALLBACK_USER_COMMAND = G_USER_COMMAND
            IT_EVENTS               = GT_EVENTS[]
            I_SAVE                  = 'A'
            IS_VARIANT              = GS_VARIANT
       TABLES
            T_OUTTAB                = IT_MLIT.


ENDFORM.                    " call_alv_list
*&---------------------------------------------------------------------*
*&      Form  make_itab_kalnr
*&---------------------------------------------------------------------*
FORM MAKE_ITAB_DETAIL.
  MOVE-CORRESPONDING IT_ZVCO_MLXXV TO IT_DETAIL.
  IT_DETAIL-MJAHR = IT_ZVCO_MLXXV-AWORG.
  IT_DETAIL-ZEILE = IT_ZVCO_MLXXV-URZEILE.

  COLLECT IT_DETAIL.
  CLEAR IT_DETAIL.

ENDFORM.                    " make_itab_kalnr
*&---------------------------------------------------------------------*
*&      Form  get_from_mseg
*&---------------------------------------------------------------------*
FORM GET_FROM_MSEG.
  DATA: L_FDATE TYPE DATUM,
        L_TDATE TYPE DATUM.

  CHECK NOT IT_DETAIL[] IS INITIAL.

  SORT IT_DETAIL BY AWREF MJAHR ZEILE.
  SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_MSEG
    FROM MSEG
     FOR ALL ENTRIES IN IT_DETAIL
   WHERE MBLNR = IT_DETAIL-AWREF
     AND MJAHR = IT_DETAIL-MJAHR
     AND ZEILE = IT_DETAIL-ZEILE.

  LOOP AT IT_MSEG.
    CLEAR T_MATS.
    READ TABLE T_MATS WITH KEY MATNR = IT_MSEG-MATNR.

    CLEAR IT_DETAIL.
    READ TABLE IT_DETAIL WITH KEY AWREF = IT_MSEG-MBLNR
                                  MJAHR = IT_MSEG-MJAHR
                                  ZEILE = IT_MSEG-ZEILE
                              BINARY SEARCH.
    READ TABLE T_MLCD WITH KEY KALNR = IT_DETAIL-KALNR.

    PERFORM MOVE_IT_ZTCO_MLI2.
    PERFORM GET_BKPF_DOC.

    IF IT_ZTCO_MLI2-SAKNR = P_SAKNR AND P_CLEAR = 'X'.
      PERFORM MAKE_TAB_FOR_BKPF USING IT_ZTCO_MLI2-MBLNR
                                      IT_ZTCO_MLI2-BDATJ.
    ENDIF.

    APPEND IT_ZTCO_MLI2.
    CLEAR IT_ZTCO_MLI2.
  ENDLOOP.

* for clearing acc : '137090'.
* previous data (not cleared : 137090)
  IF P_CLEAR = 'X' AND NOT IT_CLEAR[] IS INITIAL.

    PERFORM MODIFY_CLEAR_ACC.

  ENDIF.

ENDFORM.       " get_from_mseg
*&---------------------------------------------------------------------*
*&      Form  move_it_ztco_mli2
*&---------------------------------------------------------------------*
FORM MOVE_IT_ZTCO_MLI2.
  IT_ZTCO_MLI2-BDATJ     = IT_DETAIL-KJAHR.
  IT_ZTCO_MLI2-POPER     = IT_DETAIL-POPER.
  IT_ZTCO_MLI2-KALNR     = IT_DETAIL-KALNR.
  IT_ZTCO_MLI2-MBLNR     = IT_MSEG-MBLNR.
  IT_ZTCO_MLI2-ZEILE     = IT_MSEG-ZEILE.
  IT_ZTCO_MLI2-MATNR     = IT_MSEG-MATNR.
  IT_ZTCO_MLI2-BKLAS     = T_MATS-BKLAS.
  IT_ZTCO_MLI2-SAKNR     = IT_MSEG-SAKTO.
  IT_ZTCO_MLI2-KOSTL     = IT_MSEG-KOSTL.
  IT_ZTCO_MLI2-AUFNR     = IT_MSEG-AUFNR.
  IT_ZTCO_MLI2-MEINS     = IT_MSEG-MEINS.

  IF IT_MSEG-SHKZG = 'S'.
    IT_ZTCO_MLI2-DMBTR     = IT_MSEG-DMBTR.
    IT_ZTCO_MLI2-MENGE     = IT_MSEG-MENGE.
  ELSE.
    IT_ZTCO_MLI2-DMBTR     = - IT_MSEG-DMBTR.
    IT_ZTCO_MLI2-MENGE     = - IT_MSEG-MENGE.
  ENDIF.

ENDFORM.                    " move_it_ztco_mli2
*&---------------------------------------------------------------------*
*&      Form  get_clearing_doc
*&---------------------------------------------------------------------*
FORM GET_CLEARING_DOC.
  IT_ZTCO_MLI2-BDATJ     = IT_DETAIL-KJAHR.
  IT_ZTCO_MLI2-POPER     = IT_DETAIL-POPER.
  IT_ZTCO_MLI2-KALNR     = IT_DETAIL-KALNR.
  IT_ZTCO_MLI2-MBLNR     = IT_MSEG-MBLNR.
*  CONCATENATE
ENDFORM.                    " get_clearing_doc
*&---------------------------------------------------------------------*
*&      Form  make_tab_for_bkpf
*&---------------------------------------------------------------------*
FORM MAKE_TAB_FOR_BKPF USING P_MBLNR
                             P_BDATJ.

  CONCATENATE P_MBLNR P_BDATJ INTO IT_CLEAR-AWKEY .
  IT_CLEAR-MBLNR = P_MBLNR.
  IT_CLEAR-BDATJ = P_BDATJ.
  APPEND IT_CLEAR. CLEAR IT_CLEAR.

ENDFORM.                    " make_tab_for_bkpf
*&---------------------------------------------------------------------*
*&      Form  MODIFY_CLEAR_ACC
*&---------------------------------------------------------------------*
FORM MODIFY_CLEAR_ACC.
  DATA : L_FDATE TYPE DATUM,
         L_TDATE TYPE DATUM.

  DATA : L_BELNR LIKE BSEG-BELNR,
         L_BUZEI LIKE BSEG-BUZEI.

  CHECK NOT IT_CLEAR[] IS INITIAL.

  SELECT BUKRS GJAHR BELNR AWKEY BLART
    INTO CORRESPONDING FIELDS OF TABLE IT_BKPF
    FROM BKPF
     FOR ALL ENTRIES IN IT_CLEAR
   WHERE AWTYP = 'MKPF'
     AND AWKEY = IT_CLEAR-AWKEY.

  CHECK NOT IT_BKPF[] IS INITIAL.

  CONCATENATE P_BDATJ '0101' INTO L_FDATE.
  CONCATENATE P_BDATJ '1231' INTO L_TDATE.

  SELECT BUKRS GJAHR BELNR AUGDT AUGBL BLART
    INTO CORRESPONDING FIELDS OF TABLE IT_BSAS_TEMP
    FROM BSAS
     FOR ALL ENTRIES IN IT_BKPF
   WHERE BUKRS = IT_BKPF-BUKRS
     AND HKONT = P_SAKNR
     AND GJAHR = IT_BKPF-GJAHR
     AND BELNR = IT_BKPF-BELNR
     AND AUGDT BETWEEN L_FDATE AND L_TDATE.

  CHECK NOT IT_BSAS_TEMP[] IS INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BSAS
    FROM BSAS
     FOR ALL ENTRIES IN IT_BSAS_TEMP
   WHERE BUKRS  =  IT_BSAS_TEMP-BUKRS
     AND HKONT  =  P_SAKNR
     AND AUGDT  =  IT_BSAS_TEMP-AUGDT
     AND AUGBL  =  IT_BSAS_TEMP-AUGBL
     AND BLART  <> IT_BSAS_TEMP-BLART.

*  CHECK NOT it_bsas[] IS INITIAL.
  SELECT GJAHR BELNR HKONT KOSTL AUFNR BUZEI DMBTR
    INTO CORRESPONDING FIELDS OF TABLE IT_BSEG
    FROM BSEG
     FOR ALL ENTRIES IN IT_BSAS
   WHERE BUKRS  = IT_BSAS-BUKRS
     AND BELNR  = IT_BSAS-BELNR
     AND GJAHR  = IT_BSAS-GJAHR
     AND HKONT <> P_SAKNR.

  DATA: L_IDX TYPE SYTABIX.
  LOOP AT IT_ZTCO_MLI2 WHERE SAKNR = P_SAKNR.
    L_IDX = SY-TABIX.

    CLEAR IT_CLEAR.
    READ TABLE IT_CLEAR WITH KEY  MBLNR = IT_ZTCO_MLI2-MBLNR
                                  BDATJ = IT_ZTCO_MLI2-BDATJ.
    CHECK SY-SUBRC = 0 .
    CLEAR IT_BKPF.
    READ TABLE IT_BKPF WITH KEY AWKEY = IT_CLEAR-AWKEY.
    CHECK SY-SUBRC = 0 .
    CLEAR IT_BSAS_TEMP.
    READ TABLE IT_BSAS_TEMP WITH KEY GJAHR = IT_BKPF-GJAHR
                                     BELNR = IT_BKPF-BELNR.
    CHECK SY-SUBRC = 0 .
    CLEAR IT_BSAS.
    READ TABLE IT_BSAS WITH KEY AUGDT  = IT_BSAS_TEMP-AUGDT
                                AUGBL  = IT_BSAS_TEMP-AUGBL.

    CHECK SY-SUBRC = 0 .

    READ TABLE IT_BSEG WITH KEY BELNR  = IT_BSAS-BELNR
                                GJAHR  = IT_BSAS-GJAHR.

    IT_ZTCO_MLI2-AUGDT = IT_BSAS-AUGDT.
    IT_ZTCO_MLI2-AUGBL = IT_BSAS-AUGBL.

    IT_ZTCO_MLI2-SAKNT = IT_BSEG-HKONT.
    MODIFY IT_ZTCO_MLI2 INDEX L_IDX TRANSPORTING AUGDT AUGBL SAKNT.

*    LOOP AT it_bseg WHERE belnr = it_bsas-belnr
*                      AND dmbtr = it_ztco_mli2-dmbtr.
*      CLEAR : l_belnr, l_buzei.
*      SELECT SINGLE belnr buzei INTO (l_belnr, l_buzei)
*         FROM ztco_mli2
*        WHERE belnr = it_bseg-belnr
*          AND buzei = it_bseg-buzei
*          AND bdatj = it_ztco_mli2-bdatj
*          AND mblnr <> it_ztco_mli2-mblnr
*          AND zeile <> it_ztco_mli2-zeile.
*      IF sy-subrc <> 0 .
*        it_ztco_mli2-saknr = it_bseg-hkont.
*        it_ztco_mli2-belnr = it_bseg-belnr.
*        it_ztco_mli2-buzei = it_bseg-buzei.
*        it_ztco_mli2-kostl = it_bseg-kostl.
*        it_ztco_mli2-aufnr = it_bseg-aufnr.
*        MODIFY it_ztco_mli2. CLEAR it_ztco_mli2.
*        DELETE it_bseg.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " MODIFY_CLEAR_ACC
*&---------------------------------------------------------------------*
*&      Form  get_materials_from_PARALLEL
*&---------------------------------------------------------------------*
FORM GET_MATERIALS_FROM_PARALLEL.
  LOOP AT T_MATS.
    S_MATNR-SIGN       = 'I' .
    S_MATNR-OPTION     = 'EQ'.
    S_MATNR-LOW        = T_MATS-MATNR.
    APPEND S_MATNR.

    R_KALNR-SIGN       = 'I' .
    R_KALNR-OPTION     = 'EQ'.
    R_KALNR-LOW        = T_MATS-KALNR.
    APPEND R_KALNR.
  ENDLOOP.

ENDFORM.                    " get_materials_from_PARALLEL
*&---------------------------------------------------------------------*
*&      Form  GET_MATS_FORM_MBEW
*&---------------------------------------------------------------------*
*       Obsolete function
*----------------------------------------------------------------------*
*FORM GET_MATS_FORM_MBEW.
**MBEW - current
**MBEWH - past... but blank period...
*  DATA: L_MBEWH LIKE MBEWH.
*  LOOP AT T_MATS.
**old data... get from history
*    IF T_MATS-LFGJA = P_BDATJ AND T_MATS-LFMON > P_POPER
*    OR T_MATS-LFGJA > P_BDATJ.
*
*      SELECT * INTO L_MBEWH
*       FROM MBEWH
*          WHERE MATNR = T_MATS-MATNR
*            AND BWKEY = T_MATS-BWKEY
*            AND BWTAR = T_MATS-BWTAR
*           AND LFGJA  = P_BDATJ
*           AND LFMON <= P_POPER
*       ORDER BY LFGJA DESCENDING
*                LFMON DESCENDING.
*        EXIT.
*      ENDSELECT.
*      IF SY-SUBRC <> 0.
*        SELECT * INTO L_MBEWH
*         FROM MBEWH
*            WHERE MATNR = T_MATS-MATNR
*              AND BWKEY = T_MATS-BWKEY
*              AND BWTAR = T_MATS-BWTAR
*             AND LFGJA < P_BDATJ
*         ORDER BY LFGJA DESCENDING
*                  LFMON DESCENDING.
*          EXIT.
*        ENDSELECT.
*      ENDIF.
*
*      T_MATS-BKLAS = L_MBEWH-BKLAS.
*      T_MATS-STPRS = L_MBEWH-STPRS.
*      T_MATS-VERPR = L_MBEWH-VERPR.
*      MODIFY T_MATS.
*    ENDIF.
*
**delete
*    IF NOT T_MATS-BKLAS IN S_BKLAS.
*      DELETE T_MATS.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " GET_MATS_FORM_MBEW
*&---------------------------------------------------------------------*
*&      Form  make_batch_log
*&---------------------------------------------------------------------*
FORM MAKE_BATCH_LOG USING P_FLAG.
*  CHECK P_BATCH = 'X'.
*
*  IF P_FLAG = 'D'.
*    DELETE FROM ZTCO_BATCH_LOG WHERE REPID =  'ML03'
*                                 AND KOKRS = P_KOKRS
*                                 AND BDATJ = P_BDATJ
*                                 AND POPER = P_PERAB
*                                 AND MATNR = T_MATS-MATNR.
*
*  ELSE.
*    CLEAR : IT_LOG, IT_LOG[].
*    IT_LOG-KOKRS = P_KOKRS.
*    IT_LOG-BDATJ = P_BDATJ.
*    IT_LOG-POPER = P_PERAB.
*    IT_LOG-MATNR = T_MATS-MATNR.
*    IT_LOG-REPID = 'ML03'.
*    IT_LOG-FLAG  = P_FLAG.
*
*    APPEND IT_LOG. CLEAR IT_LOG.
*    MODIFY ZTCO_BATCH_LOG FROM TABLE IT_LOG.
*  ENDIF.
*
*  COMMIT WORK.

ENDFORM.                    " make_batch_log
*&---------------------------------------------------------------------*
*&      Form  check_obligatory_field
*&---------------------------------------------------------------------*
FORM CHECK_OBLIGATORY_FIELD.
  IF P_KOKRS IS INITIAL.
    MESSAGE S000 WITH 'Please input Controlling area. '.
  ENDIF.

  IF P_BDATJ IS INITIAL.
    MESSAGE S000 WITH 'Please input Year. '.
  ENDIF.

  IF S_POPER[] IS INITIAL.
    MESSAGE S000 WITH 'Please input Period. '.
  ENDIF.

  IF P_CURTP IS INITIAL.
    MESSAGE S000 WITH 'Please input Crcy type/val.view. '.
  ENDIF.

ENDFORM.                    " check_obligatory_field
*&---------------------------------------------------------------------*
*&      Form  get_extra_data_mbew
*&---------------------------------------------------------------------*
FORM GET_EXTRA_DATA_MBEW.
  TYPES: BEGIN OF TY_T134,
           KKREF TYPE KKREF,
         END OF TY_T134.

  TYPES: BEGIN OF TY_BKLAS,
           BKLAS TYPE BKLAS,
         END OF TY_BKLAS.

  DATA: LT_T134   TYPE TABLE OF TY_T134  WITH HEADER LINE,
        LT_BKLAS  TYPE TABLE OF TY_BKLAS WITH HEADER LINE.
*
  IF S_BKLAS[] IS INITIAL.
    REFRESH: LT_BKLAS, LT_T134.
    CLEAR  : LT_BKLAS, LT_T134.

    SELECT KKREF INTO TABLE LT_T134
      FROM T134
     WHERE MTART IN S_MTART
       AND KKREF <> SPACE.

    SELECT BKLAS INTO TABLE LT_BKLAS
      FROM T025
      FOR ALL ENTRIES IN LT_T134
     WHERE BKLAS IN S_BKLAS
       AND KKREF = LT_T134-KKREF.

    S_BKLAS-OPTION = 'EQ'. S_BKLAS-SIGN = 'I'.

    LOOP AT LT_BKLAS.
      S_BKLAS-LOW = LT_BKLAS-BKLAS.
      APPEND S_BKLAS.
    ENDLOOP.
    CLEAR S_BKLAS.
  ENDIF.

  DATA: T_MATS2 LIKE T_MATS OCCURS 0 WITH HEADER LINE.
  DATA: T_MATS3 LIKE T_MATS OCCURS 0 WITH HEADER LINE.
  SELECT
*        MBEWH~BKLAS
         CKMLHD~BWKEY CKMLHD~MATNR CKMLHD~BWTAR
         CKMLHD~KALNR
         MAKT~MAKTG
         MARA~MTART MARA~MATKL
         CKMLCR~STPRS
         CKMLCR~PVPRS AS VERPR
         CKMLPP~MEINS
         CKMLPP~STATUS
         CKMLPP~ABKUMO CKMLPP~UMKUMO CKMLPP~ZUKUMO
         CKMLPP~VNKUMO CKMLPP~LBKUM  CKMLPP~EKKUMO
         CKMLCR~PEINH
         CKMLCR~ABSALK3  CKMLCR~ABPRD_O  CKMLCR~ABKDM_O
         CKMLCR~ABPRD_MO CKMLCR~ABKDM_MO
         CKMLCR~VPPRD_O  CKMLCR~ZUPRD_O
         CKMLCR~ZUKDM_O  CKMLCR~VPKDM_O
         CKMLCR~ZUPRD_MO CKMLCR~ZUKDM_MO
         CKMLCR~VNPRD_EA CKMLCR~VNKDM_EA
         CKMLCR~EBPRD_EA CKMLCR~EBKDM_EA
         CKMLCR~VNPRD_MA CKMLCR~VNKDM_MA
         CKMLCR~EBPRD_MA CKMLCR~EBKDM_MA
     INTO CORRESPONDING FIELDS OF TABLE T_MATS2
           FROM   CKMLPP
                  INNER JOIN CKMLHD
                    ON  CKMLPP~KALNR = CKMLHD~KALNR
                  INNER JOIN MARA
                    ON  MARA~MATNR = CKMLHD~MATNR
                  INNER JOIN MAKT
                    ON  MAKT~MATNR = CKMLHD~MATNR
                   AND  MAKT~SPRAS = SY-LANGU
                  INNER JOIN CKMLCR
                    ON  CKMLCR~KALNR  = CKMLHD~KALNR
                   AND  CKMLCR~BDATJ  = P_BDATJ
                   AND  CKMLCR~POPER  = P_POPER
                   AND  CKMLCR~CURTP  = '10'
                   AND  CKMLCR~UNTPER = SPACE
             WHERE CKMLPP~BDATJ  = P_BDATJ
               AND CKMLPP~POPER  = P_POPER
               AND CKMLPP~KALNR  = CKMLHD~KALNR
               AND CKMLHD~MATNR IN S_MATNR
               AND CKMLHD~BWKEY IN S_BWKEY
*               AND MBEWH~LFGJA = P_BDATJ
*               AND MBEWH~LFMON = P_POPER
*               AND MBEWH~BKLAS IN S_BKLAS
*               and ckmlhd~kalnr <> t_mats-kalnr
               AND (   CKMLPP~ZUKUMO <> 0 OR CKMLPP~VNKUMO <> 0
                    OR CKMLPP~ABKUMO <> 0 OR CKMLPP~UMKUMO <> 0
                    OR CKMLPP~LBKUM <> 0 ).
*   ORDER BY CKMLHD~KALNR.

  SORT T_MATS BY KALNR.
  LOOP AT T_MATS2.
    READ TABLE T_MATS WITH KEY KALNR = T_MATS2-KALNR BINARY SEARCH.
    IF SY-SUBRC <> 0 AND T_MATS2-MTART IN S_MTART.
      APPEND T_MATS2 TO T_MATS3.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF T_MATS3 TO T_MATS.

ENDFORM.                    " get_extra_data_mbew
*&---------------------------------------------------------------------*
*&      Form  delete_blank_records
*&---------------------------------------------------------------------*
FORM DELETE_BLANK_RECORDS.
  DELETE IT_MLIT
    WHERE AB_LBKUM          = 0
      AND AB_SALK3          = 0
      AND AB_RD             = 0
      AND PC_LBKUM       = 0
      AND PC_SALK3       = 0
      AND PC_RD          = 0
      AND ZU_BB_LBKUM       = 0
      AND ZU_BB_SALK3       = 0
      AND ZU_BB_RD          = 0
      AND ZU_BL_LBKUM       = 0
      AND ZU_BL_SALK3       = 0
      AND ZU_BL_RD          = 0
      AND ZU_BF_LBKUM       = 0
      AND ZU_BF_SALK3       = 0
      AND ZU_BF_RD          = 0
      AND ZU_BUBS_LBKUM     = 0
      AND ZU_BUBS_SALK3     = 0
      AND ZU_BUBS_RD        = 0
      AND ZU_BUBM_LBKUM     = 0
      AND ZU_BUBM_SALK3     = 0
      AND ZU_BUBM_RD        = 0
      AND ZU_FRE_LBKUM       = 0
      AND ZU_FRE_SALK3       = 0
      AND ZU_FRE_RD          = 0
      AND ZU_ETC_LBKUM       = 0
      AND ZU_ETC_SALK3       = 0
      AND ZU_ETC_RD          = 0
      AND VP_RD          = 0
      AND VN_VKA16_LBKUM    = 0
      AND VN_VKA16_SALK3    = 0
      AND VN_VKA16_RD       = 0
      AND VN_VKA20_LBKUM    = 0
      AND VN_VKA20_SALK3    = 0
      AND VN_VKA20_RD       = 0
      AND VN_VF_LBKUM       = 0
      AND VN_VF_SALK3       = 0
      AND VN_VF_RD          = 0
      AND VN_VL_LBKUM       = 0
      AND VN_VL_SALK3       = 0
      AND VN_VL_RD          = 0
      AND VN_CC60_LBKUM     = 0
      AND VN_CC60_SALK3     = 0
      AND VN_CC60_RD        = 0
      AND VN_CC99_LBKUM     = 0
      AND VN_CC99_SALK3     = 0
      AND VN_CC99_RD        = 0
      AND VN_COB_LBKUM     = 0
      AND VN_COB_SALK3     = 0
      AND VN_COB_RD        = 0
      AND VN_OR58_LBKUM     = 0
      AND VN_OR58_SALK3     = 0
      AND VN_OR58_RD        = 0
      AND VN_OR66_LBKUM     = 0
      AND VN_OR66_SALK3     = 0
      AND VN_OR66_RD        = 0
      AND VN_VUBS_LBKUM     = 0
      AND VN_VUBS_SALK3     = 0
      AND VN_VUBS_RD        = 0
      AND VN_VUBM_LBKUM     = 0
      AND VN_VUBM_SALK3     = 0
      AND VN_VUBM_RD        = 0
      AND NI_RD        = 0
      AND EB_LBKUM          = 0
      AND EB_SALK3          = 0
      AND EB_RD             = 0.
ENDFORM.                    " delete_blank_records
*&---------------------------------------------------------------------*
*&      Form  cal_cumulative_ending
*&---------------------------------------------------------------------*
FORM CAL_CUMULATIVE_ENDING.
* Cumulative Inventory = Sum of Begining inventory + Receipts +
*                        not distributed.
  LOOP AT IT_MLIT.
*   Cumulative Qty
    IT_MLIT-KB_LBKUM = IT_MLIT-AB_LBKUM
                        + IT_MLIT-ZU_BB_LBKUM
                        + IT_MLIT-ZU_BL_LBKUM
                        + IT_MLIT-ZU_BF_LBKUM
                        + IT_MLIT-VN_CC60_LBKUM
                        + IT_MLIT-ZU_FRE_LBKUM
                        + IT_MLIT-ZU_ETC_LBKUM
                        + IT_MLIT-ZU_BUBS_LBKUM
                        + IT_MLIT-ZU_BUBM_LBKUM.

*   Cumulative Value (PC -> AB)
    IT_MLIT-KB_SALK3 =   IT_MLIT-AB_SALK3
                        + IT_MLIT-PC_SALK3
                        + IT_MLIT-ZU_BB_SALK3
                        + IT_MLIT-ZU_BL_SALK3
                        + IT_MLIT-ZU_BF_SALK3
                        + IT_MLIT-VN_CC60_SALK3
                        + IT_MLIT-ZU_FRE_SALK3
                        + IT_MLIT-ZU_ETC_SALK3
                        + IT_MLIT-ZU_BUBS_SALK3
                        + IT_MLIT-ZU_BUBM_SALK3.

*   Cumulative Price Difference (PC -> AB)
    IT_MLIT-KB_RD =   IT_MLIT-AB_RD
                     + IT_MLIT-PC_RD
                     + IT_MLIT-ZU_BB_RD
                     + IT_MLIT-ZU_BL_RD
                     + IT_MLIT-ZU_BF_RD
                     + IT_MLIT-VN_CC60_RD
                     + IT_MLIT-ZU_FRE_RD
                     + IT_MLIT-ZU_ETC_RD
                     + IT_MLIT-ZU_BUBS_RD
                     + IT_MLIT-ZU_BUBM_RD
                     + IT_MLIT-VP_RD
                     + IT_MLIT-ND_RD.
*   Ending.
    IT_MLIT-EB_LBKUM = IT_MLIT-KB_LBKUM
                        + IT_MLIT-VN_VKA16_LBKUM
                        + IT_MLIT-VN_VKA20_LBKUM
                        + IT_MLIT-VN_VF_LBKUM
                        + IT_MLIT-VN_VL_LBKUM
                        + IT_MLIT-VN_CC60_LBKUM
                        + IT_MLIT-VN_COB_LBKUM
                        + IT_MLIT-VN_OR58_LBKUM
                        + IT_MLIT-VN_OR66_LBKUM
                        + IT_MLIT-VN_CC99_LBKUM
                        + IT_MLIT-VN_VUBS_LBKUM
                        + IT_MLIT-VN_VUBM_LBKUM.
*   Ending Amt.
    IT_MLIT-EB_SALK3 =  IT_MLIT-KB_SALK3
                        + IT_MLIT-VN_VKA16_SALK3
                        + IT_MLIT-VN_VKA20_SALK3
                        + IT_MLIT-VN_VF_SALK3
                        + IT_MLIT-VN_VL_SALK3
                        + IT_MLIT-VN_CC60_SALK3
                        + IT_MLIT-VN_COB_SALK3
                        + IT_MLIT-VN_OR58_SALK3
                        + IT_MLIT-VN_OR66_SALK3
                        + IT_MLIT-VN_CC99_SALK3
                        + IT_MLIT-VN_VUBS_SALK3
                        + IT_MLIT-VN_VUBM_SALK3.
* not closed...
    READ TABLE T_MATS WITH KEY KALNR = IT_MLIT-KALNR.
    IF T_MATS-STATUS <> '70'.
      IT_MLIT-ND_RD    = IT_MLIT-ND_RD - IT_MLIT-KB_RD.
    ELSE.
*   Ending - RD
      IT_MLIT-EB_RD    = IT_MLIT-KB_RD
                          + IT_MLIT-NI_RD
                          + IT_MLIT-VN_VKA16_RD
                          + IT_MLIT-VN_VKA20_RD
                          + IT_MLIT-VN_VF_RD
                          + IT_MLIT-VN_VL_RD
                          + IT_MLIT-VN_CC60_RD
                          + IT_MLIT-VN_COB_RD
                          + IT_MLIT-VN_OR58_RD
                          + IT_MLIT-VN_OR66_RD
                          + IT_MLIT-VN_CC99_RD
                          + IT_MLIT-VN_VUBS_RD
                          + IT_MLIT-VN_VUBM_RD.
    ENDIF.

    MODIFY IT_MLIT.
  ENDLOOP.

ENDFORM.                    " cal_cumulative_ending
*&---------------------------------------------------------------------*
*&      Form  read_prev_pst_var2
*&---------------------------------------------------------------------*
FORM READ_PREV_PST_VAR2.
  DATA: BEGIN OF LT_MATS_PREV OCCURS 0,
          KALNR LIKE CKMLCR-KALNR,
          STPRS LIKE CKMLCR-STPRS,
          PEINH LIKE CKMLCR-PEINH,
          SALK3 LIKE CKMLCR-SALK3,
        END OF LT_MATS_PREV,

        L_PRYR LIKE MLCD_KEY-BDATJ,
        L_PRMN LIKE MLCD_KEY-POPER.

  CLEAR: L_PRMN, L_PRYR.

  L_PRMN = P_POPER - 1.

  IF L_PRMN = 0.
    L_PRMN = 12.
    L_PRYR = P_BDATJ - 1.
  ELSE.
    L_PRYR = P_BDATJ.
  ENDIF.

  IF NOT T_MATS[] IS INITIAL.
    CLEAR LT_MATS_PREV.
    REFRESH LT_MATS_PREV.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_MATS_PREV
        FROM CKMLCR
        FOR ALL ENTRIES IN T_MATS
        WHERE KALNR = T_MATS-KALNR
          AND BDATJ = L_PRYR
          AND POPER = L_PRMN.

    SORT LT_MATS_PREV BY KALNR.
  ENDIF.

  DATA: L_DIFF  LIKE CKMLCR-SALK3,
        W_MLSUM LIKE IT_MLIT.

* only price changed...
  LOOP AT IT_MLIT INTO W_MLSUM.
    READ TABLE LT_MATS_PREV WITH KEY KALNR = W_MLSUM-KALNR
         BINARY SEARCH.

    READ TABLE LT_MATS_PREV WITH KEY KALNR = W_MLSUM-KALNR
         BINARY SEARCH.

    IF SY-SUBRC = 0.
      CLEAR L_DIFF.
      L_DIFF = W_MLSUM-AB_LBKUM * ( T_MATS-STPRS / T_MATS-PEINH -
                              LT_MATS_PREV-STPRS / LT_MATS_PREV-PEINH ).

      IT_MLIT-KALNR = W_MLSUM-KALNR.
      IT_MLIT-AB_SALK3 = - L_DIFF.
      IT_MLIT-AB_RD    = + L_DIFF.
      IT_MLIT-PC_SALK3 = + L_DIFF.
      IT_MLIT-PC_RD    = - L_DIFF.

      COLLECT IT_MLIT. CLEAR IT_MLIT.
    ENDIF.
  ENDLOOP.

*zero inventory... clear (simplified method)
  LOOP AT IT_MLIT WHERE AB_LBKUM = 0.
    IT_MLIT-AB_SALK3 = IT_MLIT-AB_SALK3 + IT_MLIT-PC_SALK3.
    IT_MLIT-PC_SALK3 = 0.
    MODIFY IT_MLIT INDEX SY-TABIX.
  ENDLOOP.

ENDFORM.                    " read_prev_pst_var2
*&---------------------------------------------------------------------*
*&      Form  get_proc_kalnr
*&---------------------------------------------------------------------*
FORM GET_PROC_KALNR.
  CLEAR I_PROC_KALNR.
  REFRESH I_PROC_KALNR.

  SELECT WERKS MATNR BWTAR PROC_KALNR BTYP KALNR
    INTO TABLE I_PROC_KALNR
    FROM CKMLMV001
     FOR ALL ENTRIES IN T_MATS
   WHERE BWKEY = T_MATS-BWKEY
     AND MATNR = T_MATS-MATNR
     AND BWTAR = T_MATS-BWTAR.

  SORT I_PROC_KALNR BY PROCK.

ENDFORM.                    " get_proc_kalnr
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                  RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN '&IC1'.                             "dobbleclick
* CKM3
      IF  IT_MLIT-MATNR <> SPACE.  "rs_selfield-value NE space.
        SET PARAMETER ID 'MAT'  FIELD  IT_MLIT-MATNR.
        SET PARAMETER ID 'WRK'  FIELD  IT_MLIT-BWKEY.
        SET PARAMETER ID 'POPR' FIELD  IT_MLIT-POPER.
        SET PARAMETER ID 'BDTJ' FIELD  IT_MLIT-BDATJ.

        CALL TRANSACTION 'CKM3' AND SKIP FIRST SCREEN.
        CLEAR R_UCOMM.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Build field catalog for ALV list
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATEGORY USING P_FIELDNAME TYPE SLIS_FIELDNAME
                                P_TEXT      TYPE SCRTEXT_L
                                P_LEN       TYPE OUTPUTLEN
                                P_TYPE      TYPE DATATYPE_D.

  CLEAR GS_FIELDCAT.

  GS_FIELDCAT-FIELDNAME = P_FIELDNAME.
  GS_FIELDCAT-SELTEXT_L = P_TEXT.
  GS_FIELDCAT-OUTPUTLEN = P_LEN.
  GS_FIELDCAT-DATATYPE  = P_TYPE.

  APPEND GS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " BUILD_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  APPEND_T_MVT
*&---------------------------------------------------------------------*
FORM APPEND_T_MVT.
  T_MVT-KALNR = T_MLCD-KALNR.
  COLLECT T_MVT.
  CLEAR T_MVT.

ENDFORM.                    " APPEND_T_MVT
*&---------------------------------------------------------------------*
*&      Form  SET_VAL_DETAIL
*&---------------------------------------------------------------------*
FORM SET_VAL_DETAIL USING  P_CATEG.
  FIELD-SYMBOLS  <FS> TYPE ANY.
  DATA L_FNAME(60).

* Qty.
  CLEAR L_FNAME.
  CONCATENATE 'IT_MLIT' '-' P_CATEG '_LBKUM'
         INTO L_FNAME.
  ASSIGN (L_FNAME) TO <FS>.

  IF NOT IT_ZVCO_MLXXV-LBKUM IS INITIAL OR
     IT_ZVCO_MLXXV-LBKUM <> 0.
    <FS> = IT_ZVCO_MLXXV-LBKUM.
  ENDIF.

* Amt. / Valuated stock
  CLEAR L_FNAME.
  CONCATENATE 'IT_MLIT' '-' P_CATEG '_SALK3'
         INTO L_FNAME.
  ASSIGN (L_FNAME) TO <FS>.

  IF NOT IT_ZVCO_MLXXV-SALK3 IS INITIAL OR
     IT_ZVCO_MLXXV-SALK3 <> 0.
    <FS> = IT_ZVCO_MLXXV-SALK3.
  ENDIF.

ENDFORM.                    " SET_VAL_DETAIL
*&---------------------------------------------------------------------*
*&      Form  GET_DATE
*&---------------------------------------------------------------------*
FORM GET_DATE.
  DATA L_DATE(8).

  CLEAR: L_DATE, GV_DATE1, GV_DATE2, GV_DATE3.

  CONCATENATE P_POPER+1(2) '01'  P_BDATJ  INTO L_DATE.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
       EXPORTING
            INPUT  = L_DATE
       IMPORTING
            OUTPUT = GV_DATE1.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = GV_DATE1
       IMPORTING
            LAST_DAY_OF_MONTH = GV_DATE2.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = GV_DATE1
            DAYS      = '00'
            MONTHS    = '01'
            SIGNUM    = '+'
            YEARS     = '00'
       IMPORTING
            CALC_DATE = GV_DATE3.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = GV_DATE3
       IMPORTING
            LAST_DAY_OF_MONTH = GV_DATE3.


ENDFORM.                    " GET_DATE
*&---------------------------------------------------------------------*
*&      Form  APPEND_IT_DETAIL
*&---------------------------------------------------------------------*
*       Append Data to Internal table IT_DETAIL for VK_CC99
*----------------------------------------------------------------------*
FORM APPEND_IT_DETAIL.
  MOVE-CORRESPONDING IT_ZVCO_MLXXV TO IT_DETAIL.
  IT_DETAIL-MJAHR = IT_ZVCO_MLXXV-AWORG.
  IT_DETAIL-ZEILE = IT_ZVCO_MLXXV-URZEILE.

  APPEND IT_DETAIL.
  CLEAR IT_DETAIL.

ENDFORM.                    " APPEND_IT_DETAIL
*&---------------------------------------------------------------------*
*&      Form  GET_BKPF_DOC
*&---------------------------------------------------------------------*
FORM GET_BKPF_DOC.
  DATA: L_AWKEY LIKE BKPF-AWKEY.
  CONCATENATE IT_ZTCO_MLI2-MBLNR IT_ZTCO_MLI2-BDATJ INTO L_AWKEY.
  SELECT SINGLE BELNR INTO IT_ZTCO_MLI2-BELNR
     FROM BKPF
     WHERE AWTYP = 'MKPF'
       AND AWKEY = L_AWKEY.
ENDFORM.                    " GET_BKPF_DOC
*&---------------------------------------------------------------------*
*&      Form  get_valuation_class
*&---------------------------------------------------------------------*
FORM GET_VALUATION_CLASS.
* CKMLMV011 - valuation class ; incorrect
* MBEWH, MBEW only...
  TABLES: MBEWH.
  DATA: LW_MBEWH LIKE MBEWH.
  DATA: LT_MBEWH LIKE MBEWH OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE LT_MBEWH
       FROM MBEWH
       FOR ALL ENTRIES IN T_MATS
       WHERE MATNR = T_MATS-MATNR
         AND BWKEY = T_MATS-BWKEY
         AND LFGJA = P_BDATJ
         AND LFMON = P_POPER.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE LT_MBEWH
       FROM MBEW
       FOR ALL ENTRIES IN T_MATS
       WHERE MATNR = T_MATS-MATNR
         AND BWKEY = T_MATS-BWKEY
         AND LFGJA = P_BDATJ
         AND LFMON <= P_POPER.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE LT_MBEWH
       FROM MBEW
       FOR ALL ENTRIES IN T_MATS
       WHERE MATNR = T_MATS-MATNR
         AND BWKEY = T_MATS-BWKEY
         AND LFGJA < P_BDATJ.
  SORT LT_MBEWH BY MATNR BWKEY BWTAR.

  DATA: L_IDX TYPE I.
  LOOP AT T_MATS.
    L_IDX = SY-TABIX.

    CLEAR: LW_MBEWH.
    READ TABLE LT_MBEWH INTO LW_MBEWH
                        WITH KEY MATNR = T_MATS-MATNR
                                 BWKEY = T_MATS-BWKEY
                                 BWTAR = T_MATS-BWTAR
                             BINARY SEARCH.
    IF SY-SUBRC <> 0.
      SELECT * FROM MBEWH
           WHERE LFGJA = P_BDATJ
             AND LFMON < P_POPER
             AND MATNR = T_MATS-MATNR
             AND BWKEY = T_MATS-BWKEY
             ORDER BY LFMON DESCENDING.
        LW_MBEWH = MBEWH. EXIT.
      ENDSELECT.
      IF SY-SUBRC <> 0.
        SELECT * FROM MBEWH
             WHERE LFGJA < P_BDATJ
               AND MATNR = T_MATS-MATNR
               AND BWKEY = T_MATS-BWKEY
               ORDER BY LFGJA LFMON DESCENDING.
          LW_MBEWH = MBEWH. EXIT.
        ENDSELECT.
      ENDIF.
    ENDIF.


    CHECK LW_MBEWH-BKLAS IN S_BKLAS.
    T_MATS-BKLAS = LW_MBEWH-BKLAS.
    MODIFY T_MATS INDEX L_IDX TRANSPORTING BKLAS.
  ENDLOOP.

  SORT T_MATS BY BKLAS.
  DELETE T_MATS WHERE BKLAS IS INITIAL.

  SORT T_MATS BY KALNR.
ENDFORM.                    " get_valuation_class
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_VARI  text
*----------------------------------------------------------------------*
FORM ALV_VARIANT_F4 CHANGING P_VARI.
  DATA: RS_VARIANT LIKE DISVARIANT,
        LV_NOF4 TYPE C.

  CLEAR LV_NOF4.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'PA_VARI'.
      IF SCREEN-INPUT = 0.
        LV_NOF4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR RS_VARIANT.
  RS_VARIANT-REPORT   = SY-REPID.
  RS_VARIANT-USERNAME = SY-UNAME.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT = RS_VARIANT
            I_SAVE     = 'A'
       IMPORTING
            ES_VARIANT = RS_VARIANT
       EXCEPTIONS
            OTHERS     = 1.

  IF SY-SUBRC = 0 AND LV_NOF4 = SPACE.
    P_VARI = RS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.                    " ALV_VARIANT_F4
