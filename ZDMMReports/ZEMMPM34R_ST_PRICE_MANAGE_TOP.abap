************************************************************************
* Program Name   : ZEMMPM34R_ST_PRICE_MANAGE
* Created by     : Min-su Park
* Created on     : 2003.10.16.
* Pattern        :
* Description    :  Manage Standard Price for Purchase Material
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.17.     Min-su Park    UD1K901873     Initial Coding
************************************************************************
*&---------------------------------------------------------------------*
*& Include ZRACO99_STD_PRICE_MANAGE_TOP                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  ZEMMPM34R_ST_PRICE_MANAGE MESSAGE-ID ZMMM .
*ALV
TYPE-POOLS: SLIS.

DATA:   WA_EVENTS      TYPE SLIS_T_EVENT,
        W_REPID LIKE SY-REPID           ,
        IT_FIELDCAT TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        W_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
        WA_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
*LOG Internal Table
DATA:   BEGIN OF IT_LOG OCCURS 0.
          INCLUDE STRUCTURE ZTMM_LOG.
DATA:   END OF IT_LOG.
*Internal Tables
DATA : BEGIN OF IT_NEW_MAT OCCURS 0.
        INCLUDE STRUCTURE ZVMM_NEW_SPRICE.
DATA : END OF IT_NEW_MAT.
DATA : IT_MARA  LIKE MARA   OCCURS 0 WITH HEADER LINE.
DATA : IT_A017  LIKE A017   OCCURS 0 WITH HEADER LINE.
DATA : IT_STPOX LIKE TABLE OF STPOX  WITH HEADER LINE.
DATA : IT_ANALY LIKE ZTMM_ANALY OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_INFO OCCURS 0                 ,
        WERKS LIKE ZVMM_INFORECORD-WERKS         ,
       END OF IT_INFO.
DATA : BEGIN OF IT_BPLAN OCCURS 0        .
        INCLUDE STRUCTURE ZVMM_MAT_MASTER.
DATA : END OF IT_BPLAN.
DATA : BEGIN OF IT_ERROR OCCURS 0 .
        INCLUDE STRUCTURE ZTMM_SPE.
DATA : EFFPR LIKE EINE-EFFPR      ,
       END OF IT_ERROR.
*             EKORG LIKE EINE-EKORG, "PO organization
*             WERKS LIKE MARC-WERKS, "Plant
*             MATNR LIKE MARA-MATNR, "Material
*             PROFL LIKE MARA-PROFL, "Profile
*             ETYPE                , "Error Type
*             TNETPR LIKE ZTMM_SPE-TNETPR,
*             NETPR  LIKE ZTMM_SPE-NETPR ,
*             WAERS  LIKE ZTMM_SPE-WAERS ,
**             KZUST LIKE KONH-KZUST, "Reason Code
*             EFFPR LIKE EINE-EFFPR, "Price
*       END OF IT_ERROR.
*BDC_DATA
DATA : BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF IT_BDC.
DATA : BEGIN OF IT_MESSAGE OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF IT_MESSAGE.

*General Vaiable
DATA : W_MARK  VALUE 'X'       ,
       W_EFFPR LIKE EINE-EFFPR ,
       W_STAWN LIKE MARC-STAWN .

*Table Control
CONTROLS TC_ERROR TYPE TABLEVIEW USING SCREEN 100.
DATA : OK_CODE LIKE SY-UCOMM,
       FCODE   LIKE SY-UCOMM.
*Selection Condition
TABLES : ZTMM_SPE.
SELECT-OPTIONS : S_MATNR FOR ZTMM_SPE-MATNR.
PARAMETERS : P_DATE LIKE SY-DATUM.
SELECT-OPTIONS : S_EKORG FOR ZTMM_SPE-EKORG NO-DISPLAY.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       R1 RADIOBUTTON GROUP RADI DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(25) TEXT-002 FOR FIELD R1.
SELECTION-SCREEN POSITION 34.
PARAMETERS       R2 RADIOBUTTON GROUP RADI.
SELECTION-SCREEN POSITION 35.
SELECTION-SCREEN COMMENT 36(25) TEXT-003 FOR FIELD R1.
SELECTION-SCREEN END OF LINE.
