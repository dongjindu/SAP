*----------------------------------------------------------------------*
*   INCLUDE ZACOU127_TOP                                               *
*----------------------------------------------------------------------*
TABLES : SSCRFIELDS ,
         MARA,      " General Material Data
         MLGT,      " Material Data for Each Storage Type
         PKHD,      " Control Cycle
         ZTMM_MAST, " Supply to Line Master Table
         MLGN,      " Material Data for Each Warehouse Number
         MARC,      " Plant Data for Material
         MARD,      " Storage Location Data for Material
         ZTMM_EAI_MAT_MST, *ZTMM_EAI_MAT_MST.
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
TYPES: BEGIN OF TY_ROW_TAB.
INCLUDE TYPE ZTMM_EAI_MAT_MST.
TYPES: END OF TY_ROW_TAB.

* Type for ALV
TYPES: BEGIN OF TY_OUT.
INCLUDE  TYPE TY_ROW_TAB.
TYPES : CHKBOX(1),
        ICON TYPE ICON_D,
        TABCOLOR  TYPE SLIS_T_SPECIALCOL_ALV.
TYPES: END OF TY_OUT.

DATA  : IT_ROW_TAB TYPE TABLE OF TY_ROW_TAB WITH HEADER LINE,
       $IT_ROW_TAB TYPE TABLE OF TY_ROW_TAB WITH HEADER LINE,
        GT_OUT     TYPE TABLE OF TY_OUT     WITH HEADER LINE,
        GT_SEL     TYPE TABLE OF TY_OUT     WITH HEADER LINE.
DATA: G_ERROR(1),
      G_REPID  LIKE SY-REPID.

DATA: BEGIN OF IT_MATNR OCCURS 0,
        MATNR   LIKE   MARD-MATNR,
        WERKS   LIKE   MARD-WERKS,
      END   OF IT_MATNR.

DATA: BEGIN OF IT_MARD OCCURS 0,
        MATNR   LIKE   MARD-MATNR,
        WERKS   LIKE   MARD-WERKS,
        LGORT   LIKE   MARD-LGORT,
      END   OF IT_MARD.

DATA: BEGIN OF IT_PKHD OCCURS 0,
        PKNUM   LIKE   PKHD-PKNUM,
        MATNR   LIKE   PKHD-MATNR,
        WERKS   LIKE   PKHD-WERKS,
        LGTYP   LIKE   PKHD-LGTYP,
        LGPLA   LIKE   PKHD-LGPLA,
        PRVBE   LIKE   PKHD-PRVBE,
      END   OF IT_PKHD.

RANGES R_PLANT FOR T001K-BWKEY.          " Plant

INCLUDE <ICON>.                        " icon
