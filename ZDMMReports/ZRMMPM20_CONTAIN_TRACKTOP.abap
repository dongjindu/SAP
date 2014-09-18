************************************************************************
* Program Name : ZRMMPM20_CONTAIN_TRACK
* Created by   : Min-su Park
* Created on   : 2003.11.19.
* Pattern      :
* Description  : Container Tracking
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.19.     Min-su Park    UD1K901873     Initial Coding
***********************************************************************
*&---------------------------------------------------------------------*
*& Include ZRMMPM20_CONTAIN_TRACKTOP                                   *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  ZRMMPM20_CONTAIN_TRACK        .

TYPE-POOLS: SLIS.
TABLES : LIKP, LIPS.

*ALV Definition.
DATA:   WA_EVENTS      TYPE SLIS_T_EVENT,
        W_REPID LIKE SY-REPID                           ,
        WA_FIELDCAT TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        W_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
        WA_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
        WA_layo   type slis_layout_alv            .

*ALV Definition02.
DATA:   WA_EVENTS02      TYPE SLIS_T_EVENT,
        W_REPID02 LIKE SY-REPID                           ,
        WA_FIELDCAT02 TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        W_FORMNAME_TOP_OF_PAGE02 TYPE SLIS_FORMNAME VALUE
        'TOP_OF_PAGE02',
        WA_LIST_TOP_OF_PAGE02 TYPE SLIS_T_LISTHEADER.
*ALV Definition03.
DATA:   WA_EVENTS03      TYPE SLIS_T_EVENT,
        W_REPID03 LIKE SY-REPID                           ,
        WA_FIELDCAT03 TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        W_FORMNAME_TOP_OF_PAGE03 TYPE SLIS_FORMNAME VALUE
        'TOP_OF_PAGE03',
        WA_LIST_TOP_OF_PAGE03 TYPE SLIS_T_LISTHEADER.

*Internal Table
*[1]Tracking basic
DATA : BEGIN OF IT_TRACK OCCURS 0,
            VSART LIKE LIKP-VSART,      "Shipping Type
            BEZEI LIKE T173T-BEZEI,     "Shipping type description
            VGBEL LIKE LIPS-VGBEL,      "Order No.
            TRAID LIKE LIKP-TRAID,      "Container No.
           ZFBLNO LIKE ZTBL-ZFBLNO,     "BL NO
            LFIMG LIKE LIPS-LFIMG,      "Case QTY.
            MEINS LIKE LIPS-MEINS,
            ZFETD LIKE ZTBL-ZFETD,      "ETD Pusan
            ZFETA LIKE ZTBL-ZFETA,      "ETA Mobile
           ZFRETA LIKE ZTBL-ZFRETA,     "Arrival Date
            ZFEDT LIKE ZTIDSUS-ZFEDT, "C/CLEAR
        PASS_DATE LIKE ZTMM_CONTAINER-PASS_DATE, "Arrival
            LGPLA LIKE ZTMM_CONTAINER-LGPLA, "Location
            BDATU LIKE LAGP-BDATU,      "Unpack Date
       LEAVE_DATE LIKE ZTMM_CONTAINER-LEAVE_DATE, "Retrun date
             HD_D(05)            ,      "Holding Days
             HD_I TYPE I         ,
             LGTYP LIKE ZTMM_CONTAINER-LGTYP,
             LGBER LIKE ZTMM_CONTAINER-LGBER,
        END OF IT_TRACK.
*[2]Tracking Containers by Case No.
DATA : BEGIN OF IT_TRACK02 OCCURS 0,
             KDMAT LIKE LIPS-KDMAT , "CASE NO
             MATNR LIKE LIPS-MATNR , "PART NO
             MAKTX LIKE MAKT-MAKTX , "PART NAME
*            LFIMG LIKE LIPS-LFIMG , "BOX QTY ?
             LFIMG LIKE LIPS-LFIMG ,  "TOTAL PCS
             MEINS LIKE LIPS-MEINS ,
             LGPLA LIKE LEIN-LGPLA , "Storage bin
             PLPOS LIKE LEIN-PLPOS , "Storage bin Position
             LOCAT(13)             , "Bin + Position
         PASS_DATE LIKE ZTMM_CONTAINER-PASS_DATE , "Receiving date
       END OF IT_TRACK02.
*[3]Tracking Containers by Part No.
DATA : BEGIN OF IT_TRACK03 OCCURS 0,
            BOX   TYPE CHAR1     ,
            VGBEL LIKE LIPS-VGBEL,      "Order No.
            TRAID LIKE LIKP-TRAID,      "Container No.
            KDMAT LIKE LIPS-KDMAT,      "CASE NO
            LFIMG LIKE LIPS-LFIMG,      "Case QTY.
            MEINS LIKE LIPS-MEINS,
            ZFETD LIKE ZTBL-ZFETD,      "ETD Pusan
            ZFETA LIKE ZTBL-ZFETA,      "ETA Mobile
            ZFEDT LIKE ZTIDSUS-ZFEDT,   "C/CLEAR
        PASS_DATE LIKE ZTMM_CONTAINER-PASS_DATE, "Arrival
            LGPLA LIKE ZTMM_CONTAINER-LGPLA, "Location
       END OF IT_TRACK03.

*Select Condition
SELECT-OPTIONS :
       S_VGBEL FOR LIPS-VGBEL ,
       S_MATNR FOR LIPS-MATNR ,
       S_TRAID FOR LIKP-TRAID ,
       S_KDMAT   FOR LIPS-KDMAT .
PARAMETERS : P_DATE LIKE SY-DATUM DEFAULT SY-DATUM.
