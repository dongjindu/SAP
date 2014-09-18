*&---------------------------------------------------------------------*
*& Include MZAHR0004TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
PROGRAM SAPMZAHR0004 MESSAGE-ID ZMHR.
*
TABLES: ZTHR_1001,        " EE working report - Org. unit maintenance
        HRP1000,          " Infotype 1000 DB Table
        HRP1001.          " Infotype 1001 DB Table

CONTROLS: TC9000 TYPE TABLEVIEW USING SCREEN 9000.

*... internal tables
DATA: BEGIN OF IT_H1001 OCCURS 0.
      INCLUDE STRUCTURE ZTHR_1001.
DATA: CHKBX,
      END OF IT_H1001.

DATA: IT_D1001 LIKE ZTHR_1001 OCCURS 0 WITH HEADER LINE.

DATA: DYNPFIELDS LIKE STANDARD TABLE OF DYNPREAD WITH HEADER LINE.

DATA: W_OTYPE    LIKE OBJEC-OTYPE,    " object type
      W_WEGID    LIKE GDSTR-WEGID,    " evaluation path
      W_PLVAR    LIKE OBJEC-PLVAR,    " plan version
      W_ORGEH    LIKE PA0001-ORGEH,
      W_ZTYPE    LIKE ZTHR_1001-ZTYPE.

DATA: W_SHORT    LIKE HRP1000-SHORT,
      W_STEXT    LIKE HRP1000-STEXT.

DATA: W_MODES       TYPE C VALUE 'D',
      W_ZINIT.
