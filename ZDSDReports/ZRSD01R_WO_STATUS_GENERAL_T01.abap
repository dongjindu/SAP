*----------------------------------------------------------------------*
*   INCLUDE ZRSD01R_WO_STATUS_GENERAL_T01                              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   TABLES
*----------------------------------------------------------------------*
TABLES : ZSSD_WO_GENERAL, "W/O STATUS GENERAL
         ZTPP_WOSUM.      "ERP_WO QTY SUMMARY
*----------------------------------------------------------------------*
*   INTERNAL TABLE
*----------------------------------------------------------------------*
DATA : ITAB1 TYPE STANDARD TABLE OF ZSSD_WO_GENERAL WITH NON-UNIQUE
                 DEFAULT KEY INITIAL SIZE 100,
       T_ITAB1 TYPE ZSSD_WO_GENERAL.

*----------------------------------------------------------------------*
*   VARIABLES
*----------------------------------------------------------------------*
DATA : W_CNT TYPE I,
       W_GI_DATE LIKE SY-DATUM,
       W_IV_DATE LIKE SY-DATUM.

DATA : P_EX LIKE ZSSD_STOCK_OV2-EXTC,
       P_IT LIKE ZSSD_STOCK_OV2-INTC.

DATA : OK_CODE(4),
       SAVE_OK_CODE(4).
DATA : ALV_GRID1       TYPE REF TO CL_GUI_ALV_GRID,
       ALV_GRID2       TYPE REF TO CL_GUI_ALV_GRID,
       CONTAINER1      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       CONTAINER2      TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : GS_VARIANT1     TYPE DISVARIANT.
DATA : GS_VARIANT2     TYPE DISVARIANT.
DATA : GS_LAYOUT TYPE LVC_S_LAYO .
DATA : GT_HEADER       TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
       GT_FIELDCAT_LVC TYPE LVC_T_FCAT WITH HEADER LINE.

*----------------------------------------------------------------------*
*      CLASS DEFINE
*----------------------------------------------------------------------*
*CLASS LCL_APPLICATION DEFINITION DEFERRED.
*
*DATA : LT_ROWS TYPE LVC_T_ROW.
*DATA : LS_SELECTED_LINE TYPE LVC_S_ROW,
*       LF_ROW_INDEX TYPE LVC_INDEX.
*DATA:  G_APPLICATION  TYPE REF TO LCL_APPLICATION.
*----------------------------------------------------------------------*
*      SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: S_WO_SER FOR ZTPP_WOSUM-WO_SER  ,
                S_NATION FOR ZTPP_WOSUM-NATION  ,
                S_DEALER FOR ZTPP_WOSUM-DEALER  ,
                S_EXTC   FOR ZTPP_WOSUM-EXTC    ,
                S_INTC   FOR ZTPP_WOSUM-INTC    .
*PARAMETERS    : P_MON(4) .
SELECTION-SCREEN END OF BLOCK B1.
