*&---------------------------------------------------------------------*
*&  Include           ZHACR01000T001
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*
TABLES: ARCH_OBJ, ADMI_RUN.
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
           C_TCODE_ZHACR00800 LIKE SY-TCODE VALUE 'YHACR00800_1',
           C_TCODE_ZHACV0010 LIKE SY-TCODE VALUE 'ZHACV0010',
           C_TCODE_SARA LIKE SY-TCODE VALUE 'SARA',
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
DATA: G_DISP_STATUS.
*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA: BEGIN OF GA_LIST OCCURS 0,
        APPLIC LIKE ZHACT0060-ZAPPLI,
        OBJECT LIKE ADMI_RUN-OBJECT,
        DOCUMENT LIKE ADMI_RUN-DOCUMENT,
        OBJTEXT LIKE ARCH_TXT-OBJTEXT,
        STATUS LIKE ADMI_RUN-STATUS,
        USER_NAME LIKE ADMI_RUN-USER_NAME,
        CREAT_DATE LIKE ADMI_RUN-CREAT_DATE,
        CREAT_TIME LIKE ADMI_RUN-CREAT_TIME,
        COMMENTS LIKE ADMI_RUN-COMMENTS,
        VARIANTWRI LIKE ADMI_RUN-VARIANTWRI,
        DB_SPACE_W LIKE ADMI_STATS-DB_SPACE_W,
        DB_INDEX_W LIKE ADMI_STATS-DB_INDEX_W,
        DB_CLUST_W LIKE ADMI_STATS-DB_CLUST_W,
        DB_STRUC_W LIKE ADMI_STATS-DB_STRUC_W,
        FILE_HEAD LIKE ADMI_STATS-FILE_HEAD,
        FILE_DATA LIKE ADMI_STATS-FILE_DATA,
        FILE_SIZE LIKE ADMI_FILES-FILE_SIZE,
        SAVEMONTHS LIKE ZHACT0060-SAVEMONTHS,
      END OF GA_LIST.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA: BEGIN OF GT_LIST_LEFT OCCURS 0.
        INCLUDE STRUCTURE ZHACS0020.
DATA:   ZSEQ LIKE ZHACT0080-ZSEQ.
DATA:   CELLSTYL TYPE LVC_T_STYL,
        CELLSCOL TYPE LVC_T_SCOL,
      END OF GT_LIST_LEFT.
DATA: BEGIN OF GT_LIST_RIGHT OCCURS 0.
        INCLUDE STRUCTURE ZHACS0030.
DATA:   CELLSTYL TYPE LVC_T_STYL,
        CELLSCOL TYPE LVC_T_SCOL,
      END OF GT_LIST_RIGHT.

*&---------------------------------------------------------------------*
* Class DATA FOR ALV
*&---------------------------------------------------------------------*
DATA:
      G_CUSTOM_CONTAINER1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CONTAIN_LEFT        TYPE REF TO CL_GUI_CONTAINER,
      G_CONTAIN_RIGHT       TYPE REF TO CL_GUI_CONTAINER,
      G_DOCKING_CONTAINER1 TYPE REF TO CL_GUI_DOCKING_CONTAINER ,
      G_ALV_GRID1          TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAYOUT1           TYPE LVC_S_LAYO,
      GS_VARIANT1          TYPE DISVARIANT,       "Variant
      G_REC_STABLE1        TYPE LVC_S_STBL,       "#####
      GT_SORT1             TYPE LVC_T_SORT,
      GT_FDCAT1            TYPE LVC_T_FCAT,
      GT_FCODE1            TYPE UI_FUNCTIONS.     "#### ##
DATA : G_SPLITTER         TYPE REF TO CL_GUI_SPLITTER_CONTAINER.


DATA:
*G_CUSTOM_CONTAINER2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
     G_DOCKING_CONTAINER2 TYPE REF TO CL_GUI_DOCKING_CONTAINER ,
      G_ALV_GRID2          TYPE REF TO CL_GUI_ALV_GRID,
      GS_ALV_LAYOUT2           TYPE LVC_S_LAYO,
      GS_ALV_VARIANT2          TYPE DISVARIANT,       "Variant
      G_REC_STABLE2        TYPE LVC_S_STBL,       "#####
      GT_SORT2             TYPE LVC_T_SORT,
      GT_FDCAT2            TYPE LVC_T_FCAT,
      GT_FCODE2            TYPE UI_FUNCTIONS.

*&---------------------------------------------------------------------*
*& SELECTION SCREEN LAYOUT
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 110 AS SUBSCREEN.

SELECT-OPTIONS: S_APPLIC FOR ARCH_OBJ-APPLIC,
                S_OBJECT FOR ARCH_OBJ-OBJECT,
                S_NAME FOR ADMI_RUN-USER_NAME,
                S_DATE FOR ADMI_RUN-CREAT_DATE.

SELECTION-SCREEN END OF SCREEN 110.
