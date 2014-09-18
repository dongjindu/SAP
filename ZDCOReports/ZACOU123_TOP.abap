*----------------------------------------------------------------------*
*   INCLUDE ZACOU123_TOP                                               *
*----------------------------------------------------------------------*
TABLES: EKBZ, EKPO, EKBE ,SSCRFIELDS,
        *ZTCOU123, ZTCOU123.

*---------------------------------------------------------------------*
* Data
*---------------------------------------------------------------------*

TYPE-POOLS: TRUXS.
* internal table for uploaded file
DATA: BEGIN OF I_ARTICLES OCCURS 0,
         MATNR(18),
         MENGE(13),
         IV_AMT(13),   " I/V Amount
         DT_AMT(13),   " Duty Amount
      END OF I_ARTICLES.

RANGES GR_MATNR FOR EKPO-MATNR.

DATA : BEGIN OF IT_ROW_TAB OCCURS 0,
         BUKRS           LIKE   EKPO-BUKRS,
         KSCHL           LIKE   EKBZ-KSCHL,
         MATNR           LIKE   EKPO-MATNR,
         EBELN           LIKE   EKPO-EBELN,
         EBELP           LIKE   EKPO-EBELP,
         PMENGE          LIKE   EKPO-MENGE, "P/O Qty
         GMENGE          LIKE   EKPO-MENGE, "G/R Qty
         BMENGE          LIKE   EKBZ-MENGE, "Balance Qty
         TMENGE          LIKE   EKBZ-MENGE, "Bal.Qty Total by Mat
         DMBTR           LIKE   EKBZ-DMBTR, "Amount
         UPRICE          TYPE   P DECIMALS 5,
         SHKZG           LIKE   EKBZ-SHKZG,
         BEWTP           LIKE   EKBZ-BEWTP,
         MEINS           LIKE   EKPO-MEINS,
         WAERS           LIKE   EKBZ-WAERS,
         LIFNR           LIKE   EKBZ-LIFNR,
         WERKS           LIKE   EKPO-WERKS,
       END   OF  IT_ROW_TAB.

TYPES: BEGIN OF TY_CALC,
         BUKRS           LIKE   EKPO-BUKRS,
         KSCHL           LIKE   EKBZ-KSCHL,
         MATNR           LIKE   EKPO-MATNR,
         EBELN           LIKE   EKPO-EBELN,
         EBELP           LIKE   EKPO-EBELP,
         XMENGE          LIKE   EKBZ-MENGE, " Qty from Excel
         XMENGES(13), " Qty from Excel with string format
         PMENGE          LIKE   EKBZ-MENGE, " Qty from PO
         GMENGE          LIKE   EKPO-MENGE, " G/R Qty
         BMENGE          LIKE   EKBZ-MENGE, " Qty Balance
         RMENGE          LIKE   EKBZ-MENGE, " Qty reconciling
         AMENGE          LIKE   EKBZ-MENGE, " Qty after reconciling
         UPRICE          TYPE   P DECIMALS 5,
         DMBTR           LIKE   EKBZ-DMBTR, " Amount
         MEINS           LIKE   EKPO-MEINS,
         WAERS           LIKE   EKBZ-WAERS,
         LIFNR           LIKE   EKBZ-LIFNR,
         WERKS           LIKE   EKPO-WERKS,
         POST_TYPE(1),   " Post type '' = credit memo, 'X' = I/V
         BELUM LIKE MSEG-BELUM,
         ICON  TYPE ICON_D,
         MSG   LIKE ZFI_AUTO_INVOICE-MSG,
       END OF TY_CALC.

TYPES: BEGIN OF TY_PLANT,
         BWKEY TYPE BWKEY,
       END OF TY_PLANT.

TYPES: BEGIN OF TY_OUT.
INCLUDE  STRUCTURE ZTCOU123.
TYPES : XMENGES(13),
        BALAMT TYPE DMBTR,
        TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV,
        CHK(1),
        HANDLE_STYLE TYPE LVC_T_STYL. "FOR DISABLE
TYPES: END OF TY_OUT.

DATA: GT_CALC       TYPE TABLE OF TY_CALC      WITH HEADER LINE,
      GT_OUT        TYPE TABLE OF TY_OUT      WITH HEADER LINE,
      GT_PLANT      TYPE TABLE OF TY_PLANT    WITH HEADER LINE.

* ITAB for SUM infor. by balance QTY
DATA  ITAB_FOR_MATNR_SUM LIKE IT_ROW_TAB OCCURS 0 WITH HEADER LINE.

* I/V Item
DATA: BEGIN OF IT_INV OCCURS 0,
      GROUP(10),
      MATNR LIKE EKPO-MATNR,
      EBELN LIKE EKBZ-EBELN,
      EBELP LIKE EKBZ-EBELP,
      LIFNR LIKE EKBZ-LIFNR,
      LFBNR LIKE EKBE-LFBNR,
      LFPOS LIKE EKBE-LFPOS,
      BUDAT LIKE EKBZ-BUDAT,
      MAKTX LIKE MAKT-MAKTX,
      MENGE LIKE EKPO-MENGE,
      MEINS LIKE EKPO-MEINS,
      WAERS LIKE EKBZ-WAERS,
      DMBTR LIKE EKBZ-DMBTR,
      AMOUNT LIKE EKBZ-DMBTR,
      WERKS LIKE EKPO-WERKS,
      INDEX LIKE SY-INDEX,
      $GROUP(10),
      $IND(1),
      END OF IT_INV.
DATA  IT_CRE LIKE IT_INV OCCURS 0 WITH HEADER LINE.

* BAPI
DATA : HEADERDATA        LIKE    BAPI_INCINV_CREATE_HEADER,
       I_INVOICE         LIKE    RBKP-XRECH,
       I_CREDITMEMO      LIKE    RBKP-XRECH,
       INVOICEDOCNUMBER  LIKE    BAPI_INCINV_FLD-INV_DOC_NO,
       FISCALYEAR        LIKE    BAPI_INCINV_FLD-FISC_YEAR.
DATA : I_ZTCOU123        LIKE    ZTCOU123 OCCURS 0.

DATA:   BEGIN OF ITEMDATA OCCURS 0.
        INCLUDE STRUCTURE   BAPI_INCINV_CREATE_ITEM.
DATA:   END   OF ITEMDATA.

DATA:   BEGIN OF TAXDATA OCCURS 0.
        INCLUDE STRUCTURE   BAPI_INCINV_CREATE_TAX.
DATA:   END   OF TAXDATA.

DATA:   BEGIN OF RETURN OCCURS 0.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA:   BEGIN OF XRETURN OCCURS 0.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF XRETURN.

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
* Define local class
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

RANGES : GR_BWKEY FOR T001W-BWKEY.

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
