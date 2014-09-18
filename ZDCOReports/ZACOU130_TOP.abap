*----------------------------------------------------------------------*
*   INCLUDE ZACOU128_TOP                                               *
*----------------------------------------------------------------------*
TYPE-POOLS M60VT .

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : T001W,     "Plants/Branches
         MARA,      "General Material Data
         MARC,      "Plant Data for Material
         MAST,      "Material to BOM Link
         MAPL,      "Assignment of Task Lists to Materials
         PLKO.      "Task list - header

TABLES : KSSK,      "Allocation Table: Object to Class
         KSML,      "Characteristics of a Class
         INOB.      "Link between Internal Number and Object

TABLES : BAPISITEMR, "Communication fields:indep. reqmts item data table
         CM60R.      "Common work area for planned indep. req functions

TABLES : TKA01, PLAF,
         ZTCO_MHV, CRHD, ZTCOU129, ZSCOU129, *ZTCOU129,
         SSCRFIELDS.

INCLUDE <ICON>.                        " icon
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

DEFINE __PROCESS.
  PERFORM SHOW_PROGRESS USING &1 &2.
END-OF-DEFINITION.

DEFINE __MESSAGE.
  CALL FUNCTION 'POPUP_TO_INFORM'
       EXPORTING
            TITEL = &1
            TXT1  = &2
            TXT2  = SY-SUBRC.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  FALSE VALUE ' ',
            TRUE  VALUE 'X'.

****************************** Global Data *****************************
DATA: G_ERROR(1),
      G_REPID  LIKE SY-REPID,
      G_IX     LIKE SY-TABIX.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TYPES BEGIN OF TY_TOTAL.
        INCLUDE STRUCTURE RM60PLVP.
TYPES:  STATUS TYPE C,
      END OF TY_TOTAL.

DATA : IT_KSML   LIKE TABLE OF KSML  WITH HEADER LINE.

*-----> Upload file
DATA : BEGIN OF IT_FILE   OCCURS 0,
         WERKS    TYPE  T001W-WERKS,   "Plant
         MATNR    TYPE  MARA-MATNR,    "Material
         M01(15) TYPE  C,
         M02(15) TYPE  C,
         M03(15) TYPE  C,
         M04(15) TYPE  C,
         M05(15) TYPE  C,
         M06(15) TYPE  C,
         M07(15) TYPE  C,
         M08(15) TYPE  C,
         M09(15) TYPE  C,
         M10(15) TYPE  C,
         M11(15) TYPE  C,
         M12(15) TYPE  C.
DATA : END OF IT_FILE.

*-----> HEADER MATNR
DATA : BEGIN OF IT_HEADMATNR OCCURS 0,
         WERKS   TYPE  WERKS_D,   "PLANT
         VERSB   TYPE  VERSB,     "VERSION
         PBDNR   TYPE  PBDNR,   "REQUIREMENT PLAN No
         MATNR   TYPE  MATNR.   "MATERIAL No
DATA : END OF IT_HEADMATNR.

*-----> HEADER(DAILY ITEM)
DATA : BEGIN OF IT_HEADITEM OCCURS 0,
         WERKS   TYPE  WERKS_D,   "PLANT
         VERSB   TYPE  VERSB,             "VERSION
         PBDNR   TYPE  PBDNR,   "REQUIREMENT PLAN No
         MATNR   TYPE  MATNR,   "MATERIAL No
         PDATU   TYPE  PDATU,   "DATE
         PLNMG   TYPE  PLNMG,   "QTY
         PVER    TYPE  VERID.    "PROD VERSION
DATA : END OF IT_HEADITEM.

*-----> ITEM LINE(COLOR ITEM)
DATA : BEGIN OF IT_ITEM OCCURS 0,
         WERKS   TYPE  WERKS_D,   "PLANT
         VERSB   LIKE  PBIM-VERSB,             "VERSION
         PBDNR   TYPE  PBDNR,   "REQUIREMENT PLAN No
         MATNR   TYPE  MATNR,   "MATERIAL No
         PDATU   TYPE  PDATU,   "DATE
         COGUB   TYPE  ZCOGUB,   "EXT/INT GUBUN
         INEXC   TYPE  ZINEXC,   "INT-COLOR
         PLNMG   TYPE  PLNMG,   "QTY
         PVER    TYPE  VERID,    "PROD VERSION
         GUBB    TYPE  ZGUBB_A,   " A:DAY, B:WEEK, C:MONTH
       END OF IT_ITEM.

*-----> ERROR TABLE
DATA : BEGIN OF IT_ERROR OCCURS 0,
         PBDNR   LIKE  ZTPP_PMT07JB_C-PBDNR,   "REQUIREMENT PLAN No
         MATNR   TYPE MATNR,
         MSGTY   LIKE  SY-MSGTY,               "STATUS
         MSG     LIKE  CFGNL-MSGLIN.           "MESSAGE
DATA : END OF IT_ERROR.

*-----> SUCCESS TABLE
DATA : BEGIN OF IT_SUCCESS OCCURS 0,
         PBDNR   LIKE  ZTPP_PMT07JB_C-PBDNR.  "REQUIREMENT PLAN No
DATA : END OF IT_SUCCESS.

*-----> Working date of Month
DATA : BEGIN OF IT_WORKDATE OCCURS 0,
         WERKS   TYPE  T001W-WERKS,
         SPMON   TYPE  SPMON,
         DATUM   TYPE  DATUM.
DATA : END OF IT_WORKDATE.

*-----> Uploaded plant
DATA : BEGIN OF IT_WERKS OCCURS 0,
         WERKS   TYPE  T001W-WERKS.
DATA : END OF IT_WERKS.

*-----> Configure variant of FSC
DATA : BEGIN OF IT_SYVAL OCCURS 0,
         ATINN  TYPE  V_IBIN_SYVAL-ATINN,
         ATWRT  TYPE  V_IBIN_SYVAL-ATWRT.
DATA : END OF IT_SYVAL.

DATA : BEGIN OF IT_PLAF OCCURS 0,
         WERKS   TYPE  WERKS_D,
         MATNR   TYPE  MATNR,
         PEDTR   TYPE  PEDTR,
         GSMNG   TYPE  GSMNG,
         VERID   TYPE  VERID,
       END OF IT_PLAF.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : WA_LINE_IX        LIKE  SY-TABIX,
       WA_SUCCESS_IX     LIKE  SY-TABIX,    "Count of SUCCESS FOR PBDNR
       ERROR_IX       LIKE  SY-TABIX.    "Count of ERROR FOR LINE

DATA : WA_PBDNR          LIKE  PBIM-PBDNR.  "Req.plan number
DATA : WA_FABKL          LIKE  T001W-FABKL. "Factory calendar key

*-----> variable for Checking configure Variant
DATA : WA_OBJNR         TYPE V_IBINR-OBJNR,
       WA_IN_RECNO      TYPE V_IBINR-IN_RECNO.

DATA : WA_BEDAE         LIKE   T459U-BEDAE. "Requirements type

RANGES : R_PDATU  FOR  ZTPP_PMT07JB_C-PDATU.

FIELD-SYMBOLS : <MONTH>.
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BAPI)
*----------------------------------------------------------------------*
*-----> BAPI INPUT/OUTPUT TABLE
DATA : IT_BAPISSHDIN   LIKE TABLE OF BAPISSHDIN  WITH HEADER LINE,
       IT_BAPISCHARR   LIKE TABLE OF BAPISCHARR  WITH HEADER LINE,
       IT_BAPIRETURN   LIKE TABLE OF BAPIRETURN1 WITH HEADER LINE.

DATA : BEGIN OF IT_COLOR OCCURS 0.
        INCLUDE STRUCTURE RM60CUVT.
DATA : VTNAM    LIKE  CONF_OUT-ATNAM.
DATA : END OF IT_COLOR.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BDC DATA)
*----------------------------------------------------------------------*
DATA : IT_BDCDATA     LIKE TABLE OF BDCDATA  WITH HEADER LINE.
DATA : WA_OPTION_DS   LIKE CTU_PARAMS.   "BDC OPTION

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK    VALUE 'X',
           C_MONTH   VALUE 'M',
           C_KEYDATE LIKE   SY-DATUM  VALUE '99991231',  "KEY DATE
           C_REQTY56 LIKE   T459U-BEDAE  VALUE 'VSE',    "REQ TYPE ST:56
           C_REQTYOT LIKE   T459U-BEDAE  VALUE 'VSF'.    "REQ TYPE ST:OT

TYPES: BEGIN OF TY_ROW_TAB.
INCLUDE TYPE ZSCOU130.
TYPES: END OF TY_ROW_TAB.

TYPES: BEGIN OF TY_OUT.
INCLUDE  TYPE TY_ROW_TAB.
TYPES   CHK(1).
TYPES   FLAG(1).
TYPES   CELLTAB  TYPE LVC_T_STYL.
TYPES   TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV.
TYPES: END OF TY_OUT.
DATA  : IT_ROW_TAB TYPE TABLE OF TY_ROW_TAB WITH HEADER LINE,
        GT_OUT     TYPE TABLE OF TY_OUT     WITH HEADER LINE.

DATA: G_STARTDT LIKE SY-DATUM.

DATA: BEGIN OF FTAB OCCURS 10,
        FCODE(6),
      END OF FTAB.

DATA: BEGIN OF IT_SEL OCCURS 0.
        INCLUDE STRUCTURE IT_ROW_TAB.
DATA:  LINE_NO LIKE SY-TABIX,
      END OF IT_SEL.

DATA  C_VERSB   LIKE   PBIM-VERSB.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ZTCOU130_K,
               WERKS TYPE WERKS_D,
               MATNR TYPE MATNR,
           END OF ZTCOU130_K.

    TYPES: ZTCOU130_KEY   TYPE STANDARD TABLE OF ZTCOU130_K,
           ZTCOU130_TABLE TYPE STANDARD TABLE OF ZSCOU130.

    METHODS:
      HANDLE_DATA_CHANGED
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
             IMPORTING ER_DATA_CHANGED,
                       GET_DELETED_ROWS
             EXPORTING
                       DELETED_ROWS TYPE ZTCOU130_TABLE,

      REFRESH_DELTA_TABLES.

  PRIVATE SECTION.
    DATA DELETED_ROWS TYPE STANDARD TABLE OF ZSCOU130.

* This flag is set if any error occured in one of the
* following methods:
    DATA: ERROR_IN_DATA TYPE C.
    METHODS:
      UPDATE_DELTA_TABLES
         IMPORTING
            PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

* Setting for Change data
  METHOD HANDLE_DATA_CHANGED.

* remember deleted lines for saving
    CALL METHOD UPDATE_DELTA_TABLES( ER_DATA_CHANGED ).

    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.                    " handle_data_changed

  METHOD GET_DELETED_ROWS.
    DELETED_ROWS = ME->DELETED_ROWS.
  ENDMETHOD.

  METHOD REFRESH_DELTA_TABLES.
    CLEAR ME->DELETED_ROWS[].
  ENDMETHOD.

  METHOD UPDATE_DELTA_TABLES.
    DATA: L_DEL_ROW TYPE LVC_S_MOCE,
          LS_ZTCOU130 TYPE ZSCOU130,
          LS_OUTTAB LIKE LINE OF GT_OUT.

    LOOP AT PR_DATA_CHANGED->MT_DELETED_ROWS INTO L_DEL_ROW.
      READ TABLE GT_OUT INTO LS_OUTTAB INDEX L_DEL_ROW-ROW_ID.
      IF SY-SUBRC NE 0.
        MESSAGE I000(0K) WITH TEXT-E01. "Internal error
      ELSE.
        MOVE-CORRESPONDING LS_OUTTAB TO LS_ZTCOU130.
        APPEND LS_ZTCOU130 TO DELETED_ROWS.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA G_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

************************************************************************
DATA  : FLAG_DATA_CHANGED,
        INFO(80).

DEFINE __FOCUS.
  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
      EXPORTING
        CONTROL = &1 .
END-OF-DEFINITION.

RANGES R_BWKEY FOR T001K-BWKEY.
