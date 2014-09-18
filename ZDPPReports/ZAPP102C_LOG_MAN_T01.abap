*----------------------------------------------------------------------*
*   INCLUDE ZASD04R_STOCK_OVERVIEW_T01                                 *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   TABLES
*----------------------------------------------------------------------*
TABLES : INDX,
         ZTPP_PP_LOG_HEAD, "PP Module - Processing Log Header Table
         ZTPP_PP_LOG_DETA, "PP Module - Processing Log Detail Table
         ZSPP_APP102.      "PP Module - Log Management

*----------------------------------------------------------------------*
*   INTERNAL TABLE
*----------------------------------------------------------------------*
DATA : ITAB1 TYPE STANDARD TABLE OF ZSPP_APP102 WITH NON-UNIQUE
                 DEFAULT KEY INITIAL SIZE 100,
       T_ITAB1 TYPE ZSPP_APP102.

DATA: BEGIN OF LTAB OCCURS 0.
        INCLUDE STRUCTURE ZSPP_APP102.
DATA: END OF LTAB.

DATA: BEGIN OF LTAB1 OCCURS 0.
        INCLUDE STRUCTURE ZTPP_PP_LOG_DETA.
DATA: END OF LTAB1.

DATA: VARIANT1 LIKE INDX-SRTFD VALUE 'ZAPP102'.
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
SELECT-OPTIONS: S_LOGKEY    FOR ZTPP_PP_LOG_HEAD-LOGKEY,
                S_PROG      FOR ZTPP_PP_LOG_HEAD-PROGRAMM,
                S_LTYPE     FOR ZTPP_PP_LOG_HEAD-LOGTYPE,
                S_JTYPE     FOR ZTPP_PP_LOG_HEAD-JOBTYPE,
                S_LSTEP     FOR ZTPP_PP_LOG_HEAD-LOGSTEP,
                S_MSG       FOR ZTPP_PP_LOG_HEAD-MSG,
                S_LDATE     FOR ZTPP_PP_LOG_HEAD-LDATE,
                S_LTIME     FOR ZTPP_PP_LOG_HEAD-LTIME,
                S_LUSER     FOR ZTPP_PP_LOG_HEAD-LUSER.

SELECTION-SCREEN END OF BLOCK B1.
