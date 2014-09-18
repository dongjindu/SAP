**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : ZTPP_WOSUM, AUSP.
**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*
*
*DATA : BEGIN OF GT_ITEM OCCURS 0 ,
*DIST TYPE CHAR5 ,
*PACK TYPE CHAR6,
*MCCD TYPE CHAR12,
*GRDE TYPE CHAR1,
*OCCN TYPE CHAR4,
*EXCL TYPE CHAR3,
*INCL TYPE CHAR3,
*WKNO TYPE CHAR14,
*PLNT TYPE CHAR3,
*REFE TYPE CHAR11,
*VQTY(4) TYPE N,
*CQTY(4) TYPE N,
*OCHD LIKE SY-DATUM,
*CRDT LIKE SY-DATUM,
*FLAG TYPE CHAR1,
*SNDT LIKE SY-DATUM
*
*.
*DATA : END OF GT_ITEM.
DATA      : GT_ITEM LIKE TABLE OF ZSPP_GCC_OSR WITH HEADER LINE.
CONSTANTS : C_DEST(10) VALUE 'WMHR01'.   "Interface Destination.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: OK_CODE LIKE SY-UCOMM,
      GV_REPID LIKE SY-REPID.

DATA : GV_DATUM LIKE SY-DATUM.

CONSTANTS : C_PLNT VALUE '5N'.
