*----------------------------------------------------------------------*
*   INCLUDE ZAPP102L_PP_LOG_MANAGEMENT_TOP                             *
*----------------------------------------------------------------------*
PROGRAM ZAPP102M_PP_LOG_MANAGEMENT  NO STANDARD PAGE HEADING
                             LINE-SIZE 1023
                             MESSAGE-ID ZMPP.
INCLUDE <ICON> .
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ZTPP_IF_STATUS . "Interface Status


*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_SCREEN OCCURS 0 ,
       CHK ,
       SIGNAL(4)   TYPE  C ,
       TOTAL       TYPE  P ,
       SUCCESS     TYPE  P ,
       ERROR       TYPE  P .
INCLUDE STRUCTURE ZTPP_IF_STATUS .
DATA: END OF IT_SCREEN .

DATA: BEGIN OF IT_MENU   OCCURS 0,
        FCODE            LIKE SY-UCOMM,
      END OF IT_MENU.

*----> Detailed List Internal table
DATA: IT_ZTPPER       LIKE  TABLE OF ZTPPER     WITH HEADER LINE ,
      IT_ZTPPES       LIKE  TABLE OF ZTPPES     WITH HEADER LINE ,
      IT_ZTPPEP       LIKE  TABLE OF ZTPPEP     WITH HEADER LINE ,
      IT_ZTPPPR       LIKE  TABLE OF ZTPPPR     WITH HEADER LINE ,
      IT_ZTPPPS_BLK   LIKE  TABLE OF ZTPPPS_BLK WITH HEADER LINE ,
      IT_ZTPPPS_DIE   LIKE  TABLE OF ZTPPPS_DIE WITH HEADER LINE ,
      IT_ZTPPPS_PNL   LIKE  TABLE OF ZTPPPS_PNL WITH HEADER LINE ,
      IT_ZTPPPP       LIKE  TABLE OF ZTPPPP     WITH HEADER LINE ,
      IT_ZTPPPB       LIKE  TABLE OF ZTPPPB     WITH HEADER LINE ,
      IT_ZTPPVR       LIKE  TABLE OF ZTPPVR     WITH HEADER LINE ,
      IT_ZTPPVS       LIKE  TABLE OF ZTPPVS     WITH HEADER LINE ,
      IT_ZTPPVP       LIKE  TABLE OF ZTPPVP     WITH HEADER LINE .

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : WA_SCREEN    LIKE  IT_SCREEN .

RANGES : R_DATUM    FOR  SY-DATUM .
CONTROLS : TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

DATA: OK_CODE       TYPE  SY-UCOMM,
      SAVE_OK_CODE  TYPE  SY-UCOMM.

*-----> Checkbox
DATA: CHK.

*-----> SET/GET CURSOR HANDLING VARIABLE
DATA: WA_TXT9000(40) TYPE  C,         "SET/GET CURSOR FIELD
      WA_TXT9100(40) TYPE  C,
      WA_LINE9000    TYPE  SY-INDEX,  "SET/GET CURSOR LINE
      WA_LINE9100    TYPE  SY-INDEX,
      WA_LINE        TYPE  SY-INDEX.

DATA: WA_SIGNAL(5) ,
      WA_SUCCESS     TYPE  P,
      WA_ERROR       TYPE  P,
      WA_TOTAL       TYPE  P.

*-----> ALV GRID Setting
DATA : ALV_GRID               TYPE REF TO CL_GUI_ALV_GRID,
       GS_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       WA_CONTAINER           TYPE SCRFNAME VALUE 'CONTAINER'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : GS_VARIANT        TYPE DISVARIANT ,  "Display Variant
       GS_LAYOUT         TYPE LVC_S_LAYO ,  "Layout
       GS_PRINT          TYPE LVC_S_PRNT ,  "Print control
       GT_SPECIAL_GROUPS TYPE LVC_T_SGRP ,  "Field groups
       GT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS , "Exclu Toolbar Std FUNC
       GT_HEADER         TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
       GT_FIELDCAT       TYPE LVC_T_FCAT ,  "Field Catalog
       GT_SORT           TYPE LVC_T_SORT ,  "Sort criteria
       GT_FILTER         TYPE LVC_T_FILT .  "Filter criteria
DATA : WA_FIELDCAT     TYPE LVC_S_FCAT.

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK        VALUE 'X'.
