*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_TOP                                        *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : ZTPP_WOSUM, AUSP, CABN, EDP13.

**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*
*
DATA : BEGIN OF GT_DATA OCCURS 0 .
INCLUDE STRUCTURE ZTPP_KSBOHMM .
DATA : END OF GT_DATA,
*       GT_DATA LIKE TABLE OF ZTPP_KSBOHMM_IF WITH HEADER LINE,
       GT_KSBOHMM LIKE TABLE OF ZTPP_KSBOHMM_IF WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: OK_CODE LIKE SY-UCOMM.

DATA : GV_REPID LIKE SY-REPID ,
       GV_NEW(1),
       GV_DOCNUM LIKE EDIDC-DOCNUM.
