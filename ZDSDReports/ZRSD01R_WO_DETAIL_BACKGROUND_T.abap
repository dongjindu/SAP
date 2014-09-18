*----------------------------------------------------------------------*
*   INCLUDE ZRSD01R_WO_DETAIL_BACKGROUND_T                             *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   TABLES
*----------------------------------------------------------------------*
TABLES : BAPI_SELECTED_OBJECTS,
         BAPI_SELECTION_CRITERIONS,
         BAPI_OBJECT_VALUES,
         AUSP,
         CABN,
         ZSSD_WO_DETAIL_BACKGROUND,  "W/O DETAIL BACKGROUND
         ZTPP_WOSUM.             "ERP_WO QTY SUMMARY

TYPE-POOLS : SLIS.
*----------------------------------------------------------------------*
*   INTERNAL TABLE
*----------------------------------------------------------------------*
DATA : ITAB1 TYPE STANDARD TABLE OF ZSSD_WO_DETAIL_BACKGROUND
           WITH NON-UNIQUE DEFAULT KEY INITIAL SIZE 100,
       T_ITAB1 TYPE ZSSD_WO_DETAIL_BACKGROUND.

DATA: BEGIN OF LTAB1 OCCURS 0.
        INCLUDE STRUCTURE ZSSD_WO_DETAIL_BACKGROUND.
DATA:   RP14DQ  LIKE ZTPP_WOSUM-RP14DQ.
DATA:   RP16DQ  LIKE ZTPP_WOSUM-RP16DQ.
DATA: END OF LTAB1.

DATA: BEGIN OF LTAB3 OCCURS 0.
        INCLUDE STRUCTURE ZSSD_WO_DETAIL_BACKGROUND.
DATA: END OF LTAB3.

DATA: BEGIN OF LTAB4 OCCURS 0,
          ATNAM LIKE CABN-ATNAM,
          ATINN LIKE CABN-ATINN,
      END OF LTAB4.

DATA: SO LIKE BAPI_SELECTED_OBJECTS OCCURS 0 WITH HEADER LINE,
      SC LIKE BAPI_SELECTION_CRITERIONS OCCURS 0 WITH HEADER LINE,
      OC LIKE BAPI_OBJECT_VALUES OCCURS 0 WITH HEADER LINE,
      RETURN LIKE BAPIRETURN1.

DATA: T_COLINFO_TABLE TYPE SLIS_T_SPECIALCOL_ALV WITH HEADER LINE.
DATA: GS_LAYOUT2 TYPE SLIS_LAYOUT_ALV.
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
SELECT-OPTIONS: S_ATINN  FOR AUSP-ATINN NO-DISPLAY.
PARAMETERS    : P_MON    LIKE SY-DATUM+2(4) DEFAULT SY-DATUM+2(4)
                         OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.
