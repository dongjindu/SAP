*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM01001                                                  *
*&---------------------------------------------------------------------*
*&  Program : Customs Clearance variable Define                        *
*&  Created on : Na Hyun Joo                                           *
*&  Created by : 2003.10.08                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*>> ALV¿ë.
TYPE-POOLS: SLIS.

DATA: G_REPID   LIKE SY-REPID.
DATA  G_SAVE(1) TYPE C.

DATA: MARKFIELD,
      W_SELECTED_LINES  TYPE P.

DATA  G_VARIANT LIKE DISVARIANT.
DATA  G_USER_COMMAND   TYPE SLIS_FORMNAME VALUE 'P2000_ALV_COMMAND'.
DATA  G_STATUS         TYPE SLIS_FORMNAME VALUE 'P2000_ALV_PF_STATUS'.

CONTROLS TABSTRIP    TYPE TABSTRIP.
CONTROLS: TC_0103_1    TYPE TABLEVIEW USING SCREEN 0103,
          TC_0103_2    TYPE TABLEVIEW USING SCREEN 0103,
          TC_1103_1    TYPE TABLEVIEW USING SCREEN 1103,
          TC_1103_2    TYPE TABLEVIEW USING SCREEN 1103,
          TC_2103_1    TYPE TABLEVIEW USING SCREEN 2103,
          TC_2103_2    TYPE TABLEVIEW USING SCREEN 2103.

*-----------------------------------------------------------------------
* Menu Status Variable define
*-----------------------------------------------------------------------
DATA : C_CD(25)      VALUE 'Entry/Immediate Delivery',
       C_CC(13)      VALUE 'Entry Summary',
       C_TR(14)      VALUE 'Delivery Order'.

*-----------------------------------------------------------------------
* Title Text Define
*-----------------------------------------------------------------------
DATA : W_CREATE(6)        TYPE     C     VALUE   'Create',
       W_CHANGE(6)        TYPE     C     VALUE   'Change',
       W_DISPLAY(7)       TYPE     C     VALUE   'Display',
       W_OPEN(08)         TYPE     C     VALUE   'Openning',
       W_REQUEST(6)       TYPE     C     VALUE   'Create',
       W_COST(10)         TYPE     C     VALUE   'Cost Entry',
       W_STAUTS(13)       TYPE     C     VALUE   'Status Change'.

DATA : W_FIRST_SCR0144    VALUE    'Y',
       W_ZFCOTM_NM(20)    TYPE     C,
       W_ZFCUT_NM(20)     TYPE     C,
       W_ZFMATGB_NM(18)   TYPE     C,
       W_ZFSHNO           LIKE     ZTBLIT-ZFSHNO,
       W_PSPNR            LIKE     PRPS-PSPNR,
       W_FIRST_SCR0200    VALUE    'Y'.

DATA : IT_SELECTED LIKE ZSREQHD OCCURS 50 WITH HEADER LINE.
DATA : IT_COST_SELECTED LIKE ZSBLCST OCCURS 50 WITH HEADER LINE.

DATA : IT_ZSBSEG_POST LIKE ZSBSEG OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSBSEG      LIKE ZSBSEG OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSBSEG_TMP  LIKE ZSBSEG OCCURS 10 WITH HEADER LINE.

"--------------------------------------------------------
" Internal Table for Delivery Order Creaation
"--------------------------------------------------------
DATA : IT_DOHD        LIKE ZSDOHD OCCURS 50 WITH HEADER LINE.
DATA : IT_DOIT        LIKE ZSDOIT OCCURS 50 WITH HEADER LINE.
DATA : IT_DOIT_SEL    LIKE ZSDOIT OCCURS 50 WITH HEADER LINE.
DATA : IT_DOHD_APPEND LIKE ZSDOHD OCCURS 50 WITH HEADER LINE.
DATA : IT_DOIT_APPEND LIKE ZSDOIT OCCURS 50 WITH HEADER LINE.
DATA : BEGIN OF IT_ZFTRNO OCCURS 0,
         ZFTRNO LIKE ZTTRHD-ZFTRNO,
       END OF IT_ZFTRNO.

DATA : OUT_TEXT(70)  TYPE C,
       LINE1(3)      TYPE N.

DATA : LOOPLINES LIKE SY-LOOPC.

DATA : W_KSCHL        LIKE KONP-KSCHL,
       W_HM_MIN       LIKE ZTIMIMG08-ZFMNAMT,
       W_HM_MAX       LIKE ZTIMIMG08-ZFMXAMT,
       W_MP_MIN       LIKE ZTIMIMG08-ZFMNAMT,
       W_MP_MAX       LIKE ZTIMIMG08-ZFMXAMT,
       W_HMF          LIKE ZTIMIMG08-ZFMXAMT,
       W_MPF          LIKE ZTIMIMG08-ZFMNAMT,
       W_CURRENT_LINE TYPE I,
       W_TOT_DUTY     LIKE ZTIDRUS-ZFDUTY,
       W_TOT_FEE      LIKE ZTIDRUS-ZFOTFE,
       WT_ZFTRCO      LIKE LFA1-NAME1,
       W_EBELP        LIKE EKPO-EBELP,
       WT_BUKRS(30)   TYPE C,
       WT_PLANT(30)   TYPE C,
       W_SRCH(2)      TYPE C,
       WL_TRAID       LIKE LIKP-TRAID.
