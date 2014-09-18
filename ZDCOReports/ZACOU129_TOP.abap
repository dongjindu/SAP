*----------------------------------------------------------------------*
*   INCLUDE ZACOU128_TOP                                               *
*----------------------------------------------------------------------*
TABLES : TKA01,TC31A,*TC31A,ZTCOU128,*ZTCOU128,
         ZTCO_MHV, MARA, CRHD, ZTCOU129, ZSCOU129, *ZTCOU129,
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

DATA: G_ERROR(1),
      G_REPID  LIKE SY-REPID,
      G_IX     LIKE SY-TABIX.

****************************** Global Data *****************************
TYPES: BEGIN OF TY_ROW_TAB.
INCLUDE TYPE ZSCOU129.
TYPES   $HR_MH  TYPE ZHR_MH.
TYPES   $TP_MH  TYPE ZTP_MH.
TYPES   RELEASED(1).
TYPES: END OF TY_ROW_TAB.

TYPES: BEGIN OF TY_OUT.
INCLUDE  TYPE TY_ROW_TAB.
TYPES : CHK(1),
        FLAG(1),
        ICONR TYPE ICON_D.
TYPES   CELLTAB  TYPE LVC_T_STYL.
TYPES   TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV.
TYPES: END OF TY_OUT.
DATA  : IT_ROW_TAB TYPE TABLE OF TY_ROW_TAB WITH HEADER LINE,
        GT_OUT     TYPE TABLE OF TY_OUT     WITH HEADER LINE.

DATA: BEGIN OF IT_ARTNR OCCURS 0,
             MATNR  LIKE MARC-MATNR,
END OF IT_ARTNR.

DATA : BEGIN OF IT_PRODQTY OCCURS 0,
         BWKEY      LIKE CKMLMV001-BWKEY,
         MATNR      LIKE CKMLMV001-MATNR,
         PERIO      LIKE CKMLMV003-PERIO,
         VERID_ND   LIKE CKMLMV001-VERID_ND,
         MEINH      LIKE CKMLMV003-MEINH,
         OUT_MENGE  LIKE CKMLMV003-OUT_MENGE,
        $MATNR      LIKE CKMLMV001-MATNR,
       END OF  IT_PRODQTY.

DATA: G_STARTDT LIKE SY-DATUM.

DATA : BEGIN OF IT_MARA OCCURS 0,
       MTART LIKE MARA-MTART,
       MATNR LIKE MARA-MATNR,
       END OF IT_MARA.

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

DATA : BEGIN OF IT_MARC OCCURS 0,
       WERKS LIKE MARC-WERKS,
       MATNR LIKE MARC-MATNR,
       SAUFT LIKE MARC-SAUFT,
       END OF IT_MARC.

DATA : BEGIN OF IT_MI OCCURS 0,
       PLNNR LIKE PLKO-PLNNR,
       MATNR LIKE MARA-MATNR,
       END OF IT_MI.

DATA : BEGIN OF IT_MIP OCCURS 0,
       MATNR LIKE MARA-MATNR,
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
      END OF IT_PLPO.

DATA : BEGIN OF ITAB OCCURS 0,
       TYPE(1),
       MATNR LIKE MARA-MATNR,
       KOSTL TYPE KOSTL,
       PERIO(6),
       ENG_MH  TYPE GSMNG,
       CC_RATE TYPE P DECIMALS 6,
       PP_MH TYPE  GSMNG,
       VGW01   LIKE PLPO-VGW01,    "Set
       VGW02   LIKE PLPO-VGW02,    "Machine
       VGE01   LIKE PLPO-VGE01,    "Unit set
       VGE02  LIKE PLPO-VGE02,     "Unit machine
       VGE03   LIKE PLPO-VGE03,    "Unit Mh
      $PP_MH(15),
       END OF ITAB .

DATA : BEGIN OF TMPT OCCURS 0,
         PERIO(6),
         KOSTL TYPE KOSTL,
         ARBPL  LIKE CRHD-ARBPL,
         ENG_MH TYPE GSMNG,
       END OF TMPT .

DATA : BEGIN OF IT_ACT_HR_MH OCCURS 0,
         POPER     TYPE POPER,             " Period
         ARTNR     TYPE ARTNR,             " Product
         SHOP      TYPE ZZSHOP,            " Shop
         KOSTL     TYPE KOSTL,             " Cost center
         OS_DIR    LIKE ZSCOU129-TP_MH,
         FINAL_MH  LIKE ZSCOU129-HR_MH,
       END OF IT_ACT_HR_MH.

DATA : BEGIN OF $KOSTL OCCURS 0,
         KOSTL     LIKE CSKS-KOSTL,
       END   OF  $KOSTL.

DATA : BEGIN OF $KOSTL_ABTEI OCCURS 0,
         KOSTL     LIKE CSKS-KOSTL,
         ABTEI     LIKE CSKS-ABTEI,
       END   OF  $KOSTL_ABTEI.

DATA : G_VAL_CNT(2) TYPE N.

DATA: BEGIN OF FTAB OCCURS 10,
        FCODE(6),
      END OF FTAB.

DATA: BEGIN OF IT_SEL OCCURS 0.
        INCLUDE STRUCTURE IT_ROW_TAB.
DATA:  LINE_NO LIKE SY-TABIX,
      END OF IT_SEL.

************************************************************************
*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ZTCOU129_K,
               KOKRS TYPE KOKRS,
               PLNYR TYPE BDATJ,
               KOSTL TYPE KOSTL,
               MATNR TYPE MATNR,
           END OF ZTCOU129_K.

    TYPES: ZTCOU129_KEY   TYPE STANDARD TABLE OF ZTCOU129_K,
           ZTCOU129_TABLE TYPE STANDARD TABLE OF ZTCOU129.

    METHODS:
      HANDLE_DATA_CHANGED
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
             IMPORTING ER_DATA_CHANGED,
                       GET_DELETED_ROWS
             EXPORTING
                       DELETED_ROWS TYPE ZTCOU129_TABLE,

      REFRESH_DELTA_TABLES.

  PRIVATE SECTION.
    DATA DELETED_ROWS TYPE STANDARD TABLE OF ZTCOU129.

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
          LS_ZTCOU129 TYPE ZTCOU129,
          LS_OUTTAB LIKE LINE OF GT_OUT.

    LOOP AT PR_DATA_CHANGED->MT_DELETED_ROWS INTO L_DEL_ROW.
      READ TABLE GT_OUT INTO LS_OUTTAB INDEX L_DEL_ROW-ROW_ID.
      IF SY-SUBRC NE 0.
        MESSAGE I000(0K) WITH TEXT-E01. "Internal error
      ELSE.
        MOVE-CORRESPONDING LS_OUTTAB TO LS_ZTCOU129.
        APPEND LS_ZTCOU129 TO DELETED_ROWS.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA G_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.
DATA  : FLAG_DATA_CHANGED,
        INFO(80).

DEFINE __FOCUS.
  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
      EXPORTING
        CONTROL = &1 .
END-OF-DEFINITION.

RANGES R_BWKEY FOR T001K-BWKEY.
