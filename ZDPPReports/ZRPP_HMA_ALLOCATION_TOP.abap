*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_TOP                                        *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : ZTPP_WOSUM, ZTSD_UM, ZTPP_VM, MSEG, AUSP,ZTPP_PMT07JB_A.
**-------------------------------------------------------------------*
**  TYPE DEFINE
**-------------------------------------------------------------------*
TYPES : BEGIN OF T_DATA ,
        UZEIT      LIKE SY-UZEIT,
        WO_NATION  LIKE ZTSD_UM-WO_NATION  ,
        MODEL_CODE LIKE ZTSD_UM-MODEL_CODE ,
        BMDL       LIKE ZTPP_MODEL_CONV-BMDL,
        WO_DEALER1 LIKE ZTSD_UM-WO_DEALER1 ,
        DTEXT      TYPE CHAR10,
        FRQTY      LIKE MSEG-MENGE, "Forecast Quantity
        PLQTY      LIKE MSEG-MENGE, "Plan Quantity
        SQQTY      LIKE MSEG-MENGE, "Seq  Quantity
        WLQTY      LIKE MSEG-MENGE, "Weld Qtuantity
        PTQTY      LIKE MSEG-MENGE, "Paint Quantity
        GAQTY      LIKE MSEG-MENGE, "GA Quantity
        SFQTY      LIKE MSEG-MENGE, "Sign off Quantity
        MMQTY      LIKE MSEG-MENGE, " -MG Quantity
        PMQTY      LIKE MSEG-MENGE, " +MG Quantity
        END OF T_DATA.

TYPES : BEGIN OF T_KEY,
          WO_SERIAL	  LIKE  ZTSD_UM-WO_SERIAL,
          WO_NATION   LIKE  ZTSD_UM-WO_NATION,
          WO_DEALER   LIKE  ZTSD_UM-WO_DEALER,
          WO_EXTC     LIKE  ZTSD_UM-WO_EXTC  ,
          WO_INTC     LIKE  ZTSD_UM-WO_INTC  ,
          WO_DEALER1  LIKE  ZTSD_UM-WO_DEALER1,
          MODEL_CODE  LIKE  ZTSD_UM-MODEL_CODE ,
          MODL        LIKE  ZTPP_PMT07JB_A-MODL,
          BODY_NO     LIKE  ZTSD_UM-BODY_NO    ,
          OBJEK       LIKE  AUSP-OBJEK,
          STATUS      LIKE  ZTPP_VM-RP_CSTATUS,
*          STATUS      LIKE  AUSP-ATWRT,
          MENGE       LIKE  MSEG-MENGE,
        END OF T_KEY.

TYPES : BEGIN OF T_PLAN,
          DIST LIKE  ZTPP_PMT07JB_A-DIST,
          WO_NATION  LIKE ZTSD_UM-WO_NATION,
          MODL LIKE  ZTPP_PMT07JB_A-MODL,
          GUBB LIKE  ZTPP_PMT07JB_A-GUBB,
          PQTY LIKE  ZTPP_PMT07JB_A-PQTY,

        END OF T_PLAN.
**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*

DATA : BEGIN OF GT_DATA OCCURS 0 ,
        UZEIT      LIKE SY-UZEIT,
        WO_NATION  LIKE ZTSD_UM-WO_NATION  ,
        MODEL_CODE LIKE ZTSD_UM-MODEL_CODE ,
        BMDL       LIKE ZTPP_MODEL_CONV-BMDL,
        WO_DEALER1 LIKE ZTSD_UM-WO_DEALER1 ,
        DTEXT      TYPE CHAR10,
        FRQTY      LIKE MSEG-MENGE, "Forecast Quantity
        PLQTY      LIKE MSEG-MENGE, "Plan Quantity
        SQQTY      LIKE MSEG-MENGE, "Seq  Quantity
        WLQTY      LIKE MSEG-MENGE, "Weld Qtuantity
        PTQTY      LIKE MSEG-MENGE, "Paint Quantity
        GAQTY      LIKE MSEG-MENGE, "GA Quantity
        SFQTY      LIKE MSEG-MENGE, "Sign off Quantity
        MMQTY      LIKE MSEG-MENGE, " -MG Quantity
        PMQTY      LIKE MSEG-MENGE. " +MG Quantity
DATA : END OF GT_DATA.

DATA : GS_DATA LIKE LINE OF GT_DATA.
*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*
DATA : GV_FLAG(1).

DATA: OK_CODE LIKE SY-UCOMM.
DATA : GV_TITLE(40).

DATA : GV_REPID LIKE SY-REPID ,
       GV_NEW(1),
       GV_DOCNUM LIKE EDIDC-DOCNUM.
