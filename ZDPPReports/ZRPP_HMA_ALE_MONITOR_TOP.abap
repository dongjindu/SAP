*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_TOP                                        *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : EDIDC, TEDTT, EDIMSGT.



**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*
DATA: D_TAB TYPE REF TO DATA,
      LT_FCAT TYPE TABLE OF LVC_S_FCAT,
      LS_FCAT LIKE LINE OF LT_FCAT,
*        LS_ORI LIKE LINE OF LT_ORI,
      LV_TIME(2) TYPE C,
      LV_LINE(2) TYPE C.

FIELD-SYMBOLS : <NEW_TAB> TYPE TABLE.

DATA : T_FIELD(50).
FIELD-SYMBOLS : <STATUS> .

*** Data References
DATA: NEW_LINE TYPE REF TO DATA.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: OK_CODE LIKE SY-UCOMM,
      GV_REPID LIKE SY-REPID,
      GT_DATA LIKE TABLE OF EDIDC WITH HEADER LINE.
DATA MAX_DISPLAY TYPE I VALUE 300.
DATA: OWN_SYS TYPE REF TO BD_LS_MON.
DATA AUD_IDOC_TAB TYPE BDMON_DISP1.
