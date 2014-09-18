*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_TOP                                        *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : ZTPP_WOSUM, AUSP, CABN, EDP13, ZTSD_UM, ZTPP_VM.
**-------------------------------------------------------------------*
**  TYPE DEFINE
**-------------------------------------------------------------------*


**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*
*Field
*Work Serial
*Nation
*Dealer
*Ext
*Int
*Urgency
*Sum Qty


DATA : BEGIN OF GT_DATA OCCURS 0 ,
*  INCLUDE STRUCTURE ZTSD_UM.
    WO_SERIAL  LIKE ZTSD_UM-WO_SERIAL  ,
    WO_NATION  LIKE ZTSD_UM-WO_NATION  ,
    WO_DEALER  LIKE ZTSD_UM-WO_DEALER  ,
    WO_EXTC    LIKE ZTSD_UM-WO_EXTC    ,
    WO_INTC    LIKE ZTSD_UM-WO_INTC    ,
    URGENCY    LIKE ZTSD_UM-URGENCY    ,
    SUMQTY     LIKE ZTPP_WOSUM-MODQTY .
*    MODEL_CODE LIKE ZTSD_UM-MODEL_CODE ,
*    BODY_NO    LIKE ZTSD_UM-BODY_NO   .
DATA : END OF GT_DATA.
*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: OK_CODE LIKE SY-UCOMM.

DATA : GV_REPID LIKE SY-REPID ,
       GV_NEW(1),
       GV_DOCNUM LIKE EDIDC-DOCNUM.
