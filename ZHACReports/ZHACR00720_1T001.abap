*&---------------------------------------------------------------------*
*&  Include           ZHARC00700T001
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& RANGES
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& FIELD-SYMBOLS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& MACRO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& MACRO SET_RANGE
*&---------------------------------------------------------------------*
DEFINE SET_RANGE.
  &1-SIGN = &2.
  &1-OPTION = &3.
  &1-LOW = &4.
  &1-HIGH = &5.

  APPEND &1.
  CLEAR &1.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS: C_FLAG_X TYPE CHAR1 VALUE 'X'.
CONSTANTS: C_TCODE_AOBJ LIKE SY-TCODE VALUE 'AOBJ',
***           C_TCODE_ZHACR00800 LIKE SY-TCODE VALUE 'ZHACR00800',
           C_TCODE_ZHACR00800 LIKE SY-TCODE VALUE 'ZHACR00800_1',
           C_TCODE_ZHACV0010 LIKE SY-TCODE VALUE 'ZHACV0010',
           C_TCODE_SARI LIKE SY-TCODE VALUE 'SARI'.


CONSTANTS: C_MSGTY_S TYPE BAPI_MTYPE VALUE 'S',
           C_MSGTY_E TYPE BAPI_MTYPE VALUE 'E',
           C_MSGTY_I TYPE BAPI_MTYPE VALUE 'I'.
*&---------------------------------------------------------------------*
*& GLOBAL VARIABLES
*&---------------------------------------------------------------------*
DATA: OK_CODE TYPE SY-UCOMM,
      OKCODE  TYPE SY-UCOMM.
DATA: G_LIST_TOTALLINE(15).
DATA: G_BDCMODE TYPE CHAR1 VALUE 'N'.
*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA: BEGIN OF GT_LIST OCCURS 0.
        INCLUDE STRUCTURE ZHACS0010.
DATA:   CELLSTYL TYPE LVC_T_STYL,
        CELLSCOL TYPE LVC_T_SCOL,
      END OF GT_LIST.


*&---------------------------------------------------------------------*
* Class DATA FOR ALV
*&---------------------------------------------------------------------*
DATA: G_DOCKING_CONTAINER1 TYPE REF TO CL_GUI_DOCKING_CONTAINER ,
      G_ALV_GRID1          TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAYOUT1           TYPE LVC_S_LAYO,
      GS_VARIANT1          TYPE DISVARIANT,       "Variant
      G_REC_STABLE1        TYPE LVC_S_STBL,       "#####
      GT_SORT1             TYPE LVC_T_SORT,
      GT_FDCAT1            TYPE LVC_T_FCAT,
      GT_FCODE1            TYPE UI_FUNCTIONS.     "#### ##

*&---------------------------------------------------------------------*
*& SELECTION SCREEN LAYOUT
*&---------------------------------------------------------------------*
