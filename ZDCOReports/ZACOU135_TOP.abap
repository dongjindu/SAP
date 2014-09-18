*----------------------------------------------------------------------
*   INCLUDE ZACOU128_TOP                                               *
*----------------------------------------------------------------------*
INCLUDE ZACOUI00.
TABLES : MARC,SSCRFIELDS,ZTCOU135,*ZTCOU135 .

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

DEFINE __FOCUS.
  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
      EXPORTING
        CONTROL = &1 .
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  FALSE VALUE ' ',
            TRUE  VALUE 'X'.

DATA: G_ERROR(1),
      G_REPID  LIKE SY-REPID,
      G_IX     LIKE SY-TABIX.

DATA  G_PROC_TYPE(3).

****************************** Global Data *****************************
FIELD-SYMBOLS: <F_FIELD>, <F_FIELD2>.

DATA: G_STARTDT LIKE SY-DATUM.

TYPES: BEGIN OF TY_ROW_TAB.
        INCLUDE STRUCTURE ZTCOU135.
TYPES:
       TXT TYPE TXT_FEVOR,
       ICON TYPE ICON-ID,
       CHK(1).
TYPES: END OF TY_ROW_TAB.

TYPES: BEGIN OF TY_OUT.
INCLUDE  TYPE TY_ROW_TAB.
TYPES   REMARKS(30).
TYPES   OK?(1).
TYPES   CELLTAB  TYPE LVC_T_STYL.
TYPES   TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV.
TYPES: END OF TY_OUT.

DATA: BEGIN OF IT_PLAF_MARC OCCURS 0,
       PLNUM LIKE PLAF-PLNUM,
       WERKS LIKE PLAF-PLWRK,
       MATNR LIKE PLAF-MATNR,
       GSMNG LIKE PLAF-GSMNG,
       PEDTR LIKE PLAF-PEDTR,
       SAUFT LIKE MARC-SAUFT,
       FEVOR LIKE MARC-FEVOR,
       MTART LIKE MARA-MTART,
      END   OF IT_PLAF_MARC.

DATA: BEGIN OF IT_BOM_ROOT OCCURS 0,
       WERKS LIKE PLAF-PLWRK,
       FEVOR LIKE MARC-FEVOR,
       MATNR LIKE PLAF-MATNR,
       MTART LIKE MARA-MTART,
       SAUFT LIKE MARC-SAUFT,
      END   OF IT_BOM_ROOT.

DATA  GT_MESSAGES      LIKE MESSAGES       OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_PLAF OCCURS 0,
       PERIO(6),
       WERKS LIKE PLAF-PLWRK,
       MATNR LIKE MARA-MATNR,
       FEVOR LIKE MARC-FEVOR,
       SAUFT LIKE MARC-SAUFT,
       MTART LIKE MARA-MTART,
       GSMNG LIKE PLAF-GSMNG,
       $FLAG(1),
       END OF IT_PLAF.

DATA : BEGIN OF IT_MI OCCURS 0,
       PLNNR LIKE PLKO-PLNNR,
       MATNR LIKE MARA-MATNR,
       END OF IT_MI.

DATA : BEGIN OF IT_MIP OCCURS 0,
       MATNR LIKE MARA-MATNR,
       SAUFT LIKE MARC-SAUFT,
       END OF IT_MIP.

DATA : BEGIN OF IT_PLPO_TEMP OCCURS 0,
       PLNTY  LIKE PLPO-PLNTY,
       PLNNR  LIKE PLPO-PLNNR,
       PLNKN  LIKE PLPO-PLNKN,
       ZAEHL  LIKE PLPO-ZAEHL,
       DATUV  LIKE PLPO-DATUV,
       ARBID  LIKE PLPO-ARBID,
       AENNR  LIKE PLPO-AENNR,
       VORNR  LIKE PLPO-VORNR,
       WERKS  LIKE PLPO-WERKS,
       LAR01  LIKE PLPO-LAR01,
       VGE01  LIKE PLPO-VGE01,
       VGW01  LIKE PLPO-VGW01,
       LAR02  LIKE PLPO-LAR02,
       VGE02  LIKE PLPO-VGE02,
       VGW02  LIKE PLPO-VGW02,
       LAR03  LIKE PLPO-LAR03,
       VGE03  LIKE PLPO-VGE03,
       VGW03  LIKE PLPO-VGW03,
       ZGR03  LIKE PLPO-ZGR03,
       END OF IT_PLPO_TEMP.

DATA : BEGIN OF IT_PLPO OCCURS 0,
        WERKS   LIKE PLPO-WERKS,
        MATNR   LIKE MARA-MATNR,
        PLNTY   LIKE PLPO-PLNTY,
        TYPE(1),
        ARBID   LIKE PLPO-ARBID,
        VGW01   LIKE PLPO-VGW01,    "Set
        VGW02   LIKE PLPO-VGW02,    "Machine
        VGW03   LIKE PLPO-VGW03,    "Labor
        VGE01   LIKE PLPO-VGE01,    "Unit Set
        VGE02   LIKE PLPO-VGE02,    "Unit Machine
        VGE03   LIKE PLPO-VGE03,    "Unit Labor
        ZGR03   LIKE PLPO-ZGR03,
        PLNNR   LIKE PLPO-PLNNR,
        PLNAL   LIKE MAPL-PLNAL,
        AUFAK(6),
      END OF IT_PLPO.

DATA : BEGIN OF IT_RATE OCCURS 0,
       MATNR LIKE MARA-MATNR,
       END OF IT_RATE.

DATA : BEGIN OF IT_PRODUCT OCCURS 0,
       MATNR LIKE MARA-MATNR,
       END OF IT_PRODUCT.

DATA : BEGIN OF IT_CRHD OCCURS 0,
       OBJID LIKE CRHD-OBJID,
       ARBPL LIKE CRHD-ARBPL,
       END OF IT_CRHD.

DATA : BEGIN OF ITAB OCCURS 0,
       TYPE(1),
       WERKS LIKE PLAF-PLWRK,
       FEVOR LIKE MARC-FEVOR,
       MATNR LIKE MARA-MATNR,
       KOSTL TYPE KOSTL,
       PERIO(6),
       ENG_MH LIKE IT_PLAF-GSMNG,
       CC_RATE TYPE P DECIMALS 6,
       ABP_MH LIKE IT_PLAF-GSMNG,
       VGW01   LIKE PLPO-VGW01,    "Set
       VGW02   LIKE PLPO-VGW02,    "Machine
       VGE01   LIKE PLPO-VGE01,    "Unit set
       VGE02  LIKE PLPO-VGE02,     "Unit machine
       VGE03   LIKE PLPO-VGE03,    "Unit Mh
       MTART LIKE MARA-MTART,
       AUFAK(6),
       END OF ITAB .

DATA : BEGIN OF TMPT OCCURS 0,
         PERIO(6),
         KOSTL TYPE KOSTL,
         ARBPL  LIKE CRHD-ARBPL,
         ENG_MH LIKE IT_PLAF-GSMNG,
       END OF TMPT .

DATA : BEGIN OF IT_FEVOR_TXT  OCCURS 0,
        WERKS      LIKE MARC-WERKS,
        FEVOR      LIKE MARC-FEVOR,
        TXT        LIKE T024F-TXT,
      END OF IT_FEVOR_TXT.

DATA  : IT_ROW_TAB TYPE TABLE OF TY_ROW_TAB WITH HEADER LINE,
        GT_OUT     TYPE TABLE OF TY_OUT     WITH HEADER LINE.

* temp table for selected rows
DATA  $GT_OUT LIKE GT_OUT OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
*     BOM header data structure for read
DATA: BEGIN OF IT_TSTK2 OCCURS 0.
        INCLUDE STRUCTURE STKO_API02.
DATA: END OF IT_TSTK2.

*     BOM items table
DATA: BEGIN OF IT_TSTP3 OCCURS 0.
        INCLUDE STRUCTURE STPO_API03.
DATA: END OF IT_TSTP3.

*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ZTCOU135_K,
               WERKS TYPE WERKS_D,
               FEVOR TYPE FEVOR,
           END OF ZTCOU135_K.

    TYPES: ZTCOU135_KEY   TYPE STANDARD TABLE OF ZTCOU135_K,
           ZTCOU135_TABLE TYPE STANDARD TABLE OF ZTCOU135.

    METHODS:
      HANDLE_DATA_CHANGED
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
             IMPORTING ER_DATA_CHANGED,
                       GET_DELETED_ROWS
             EXPORTING
                       DELETED_ROWS TYPE ZTCOU135_TABLE,

      REFRESH_DELTA_TABLES.

  PRIVATE SECTION.
    DATA DELETED_ROWS TYPE STANDARD TABLE OF ZTCOU135.

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
          LS_ZTCOU135 TYPE ZTCOU135,
          LS_OUTTAB LIKE LINE OF GT_OUT.

    LOOP AT PR_DATA_CHANGED->MT_DELETED_ROWS INTO L_DEL_ROW.
      READ TABLE GT_OUT INTO LS_OUTTAB INDEX L_DEL_ROW-ROW_ID.
      IF SY-SUBRC NE 0.
        MESSAGE I000(0K) WITH TEXT-E01. "Internal error
      ELSE.
        MOVE-CORRESPONDING LS_OUTTAB TO LS_ZTCOU135.
        APPEND LS_ZTCOU135 TO DELETED_ROWS.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA G_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

************************************************************************
DATA  : FLAG_DATA_CHANGED,
        INFO(80).

* FOR bdc
DATA: BEGIN OF WA_RESULT,
        MATNR LIKE MARA-MATNR,
        WERKS LIKE MARC-WERKS,
        PLNNR LIKE PLKOD-PLNNR,          "Group
        PLNAL LIKE PLKOD-PLNAL,          "Group counter
        DATUV(10) TYPE C,                "Valid from date
        MESSA(100) TYPE C,
      END OF WA_RESULT.

DATA: WA_BDCDATA LIKE BDCDATA,
      WA_OPT     LIKE CTU_PARAMS,
      WA_MSG LIKE BDCMSGCOLL,
      IT_MSG LIKE TABLE OF WA_MSG,
      IT_RESULT LIKE TABLE OF WA_RESULT,
      IT_BDCDATA LIKE TABLE OF WA_BDCDATA.

DATA: BEGIN OF FTAB OCCURS 10,
        FCODE(6),
      END OF FTAB.
