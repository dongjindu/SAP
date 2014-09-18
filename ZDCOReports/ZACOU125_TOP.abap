*----------------------------------------------------------------------*
*   INCLUDE ZACOU123_TOP                                               *
*----------------------------------------------------------------------*
TABLES: EKBZ, EKPO, EKBE ,SSCRFIELDS,
        ZTMM_IF_PRICE .


*---------------------------------------------------------------------*
* Data
*---------------------------------------------------------------------*

TYPE-POOLS: TRUXS.

* internal table for uploaded file
DATA: BEGIN OF I_ARTICLES OCCURS 0,
            MATNR(18),		" Material #
            LIFNR(10),		" Vendor #
            APP_D(8),		" Approval Date	
            PRICE(13),
            PUM_N(8),           " Approved document #
            RESN_C(3),          " Reason Code
            PRICE_UNIT(5),
            ZP16(13),
            ZP17(13),
            ZP18(13),
            FRA1(13),
            ZOTH(13),
            ZOTI(13),
      END OF I_ARTICLES.

RANGES GR_MATNR FOR EKPO-MATNR.

DATA : BEGIN OF IT_ROW_TAB OCCURS 0,
            MATNR      LIKE   ZTMM_IF_PRICE-MATNR,
            LIFNR      LIKE   ZTMM_IF_PRICE-LIFNR,
            ZSEQ       LIKE   ZTMM_IF_PRICE-ZSEQ,
            INTF_D     LIKE   ZTMM_IF_PRICE-INTF_D,
            INTF_TIME  LIKE   ZTMM_IF_PRICE-INTF_TIME,
            APP_D      LIKE   ZTMM_IF_PRICE-APP_D,
            PRICE      LIKE   ZTMM_IF_PRICE-PRICE,
            RESN_C     LIKE   ZTMM_IF_PRICE-RESN_C,
            PUM_N      LIKE   ZTMM_IF_PRICE-PUM_N,
            PRICE_UNIT LIKE   ZTMM_IF_PRICE-PRICE_UNIT,
            ZP16       LIKE   ZTMM_IF_PRICE-ZP16,
            ZP17       LIKE   ZTMM_IF_PRICE-ZP17,
            ZP18       LIKE   ZTMM_IF_PRICE-ZP18,
            FRA1       LIKE   ZTMM_IF_PRICE-FRA1,
            ZOTH       LIKE   ZTMM_IF_PRICE-ZOTH,
            ZOTI       LIKE   ZTMM_IF_PRICE-ZOTI,
       END   OF  IT_ROW_TAB.

*DATA   IT_ROW_TAB LIKE ZTMM_IF_PRICE OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF TY_CALC.
INCLUDE  STRUCTURE ZTMM_IF_PRICE.
TYPES   DATE_F LIKE SY-DATUM.
TYPES   DATE_T LIKE SY-DATUM.
TYPES   END OF TY_CALC.

TYPES: BEGIN OF TY_OUT.
INCLUDE  STRUCTURE ZTMM_IF_PRICE.
TYPES   DATE_F LIKE SY-DATUM.
TYPES   DATE_T LIKE SY-DATUM.
TYPES   TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV.
TYPES:   CHK(1),
        WAERS LIKE T001-WAERS,
        MSG(80),
        $IX LIKE SY-TABIX.
TYPES: END OF TY_OUT.

DATA: GT_CALC       TYPE TABLE OF TY_CALC      WITH HEADER LINE,
      GT_OUT        TYPE TABLE OF TY_OUT      WITH HEADER LINE,
      IT_ITAB       TYPE TABLE OF TY_OUT      WITH HEADER LINE.
* ITAB for SUM infor. by balance QTY
DATA  ITAB_FOR_MATNR_SUM LIKE IT_ROW_TAB OCCURS 0 WITH HEADER LINE.

*DATA: BEGIN OF it_itab OCCURS 0.
*        INCLUDE STRUCTURE a018.
*DATA  index like sy-tabix.
*DATA: END OF it_itab.

* Total Doc. Count to be created.
DATA  : TOTAL_DOC_CNT TYPE I,
        CURRENT_DOC_CNT TYPE I.


* Value List for Condition Type
DATA: BEGIN OF CON_LIST OCCURS 0,
          KSCHL LIKE T685T-KSCHL,
          VTEXT LIKE T685T-VTEXT,
      END OF CON_LIST.

DATA: BEGIN OF HELP_FIELD OCCURS 0.
        INCLUDE STRUCTURE HELP_VALUE.
DATA: END OF HELP_FIELD.

DATA: BEGIN OF HELP_VTAB OCCURS 0.
        INCLUDE STRUCTURE HELP_VTAB.
DATA: END OF HELP_VTAB.

DATA: BEGIN OF HELP_VALUE OCCURS 0,
      VALUE LIKE HELP_VTAB-VALUE,
      END OF HELP_VALUE.

*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:
      HANDLE_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
              IMPORTING ER_DATA_CHANGED,

      HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
              IMPORTING E_ROW
                        E_COLUMN
                        ES_ROW_NO.

ENDCLASS.

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

* Setting for Change data
  METHOD HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.                    " handle_data_changed

* Double Click
  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM DOUBLE_CLICK USING E_ROW
                               E_COLUMN
                               ES_ROW_NO.
  ENDMETHOD.                    " handle_double_click

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA G_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

*----------------------------------------------------------------------*
* Others
*----------------------------------------------------------------------*
DATA:
      GV_INDEX      TYPE I,
      G_ERROR(1),
      NUM(12) VALUE ' 0123456789.'.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

DEFINE __FOCUS.
  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
      EXPORTING
        CONTROL = &1 .
END-OF-DEFINITION.

DEFINE __PROCESS.
  PERFORM SHOW_PROGRESS USING TEXT-S01 &1.
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
