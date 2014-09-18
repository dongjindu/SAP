*----------------------------------------------------------------------*
*   INCLUDE ZASD04R_STOCK_OVERVIEW_T01                                 *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   TABLES
*----------------------------------------------------------------------*
TABLES : T001W,
         T001L,
         ZSSD_STOCK_OV1, "STOCK OVERVIEW SUBTOTAL
         ZSSD_STOCK_OV2, "STOCK OVERVIEW DETAIL
         MSKA,  "Sales Order Stock
         VBAK,  "Sales Document: Header Data
         VBAP.  "Sales Document: Item Data

*----------------------------------------------------------------------*
*   INTERNAL TABLE
*----------------------------------------------------------------------*
DATA : ITAB1 TYPE STANDARD TABLE OF ZSSD_STOCK_OV1 WITH NON-UNIQUE
                 DEFAULT KEY INITIAL SIZE 100,
       T_ITAB1 TYPE ZSSD_STOCK_OV1.

DATA : ITAB2 TYPE STANDARD TABLE OF ZSSD_STOCK_OV2 WITH NON-UNIQUE
                 DEFAULT KEY INITIAL SIZE 100,
       T_ITAB2 TYPE ZSSD_STOCK_OV2.

DATA: BEGIN OF LTAB OCCURS 0.
        INCLUDE STRUCTURE ZSSD_STOCK_OV2.
DATA:   CUOBJ LIKE VBAP-CUOBJ.
DATA: END OF LTAB.


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

CLASS LCL_APPLICATION DEFINITION DEFERRED.

DATA : LT_ROWS TYPE LVC_T_ROW.
DATA : LS_SELECTED_LINE TYPE LVC_S_ROW,
       LF_ROW_INDEX TYPE LVC_INDEX.
DATA:  G_APPLICATION  TYPE REF TO LCL_APPLICATION.
*----------------------------------------------------------------------*
*      SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*SELECTION-SCREEN SKIP 1.
PARAMETERS : P_WERKS LIKE T001W-WERKS DEFAULT 'P001'.
SELECT-OPTIONS: S_LGORT FOR T001L-LGORT.
SELECTION-SCREEN END OF BLOCK B1.
