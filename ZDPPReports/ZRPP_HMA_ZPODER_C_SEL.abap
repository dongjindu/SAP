
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_SEL                                        *
*----------------------------------------------------------------------*
*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_CREDAT   FOR  EDIDC-CREDAT.
PARAMETERS : P_MESTYP LIKE EDIDC-MESTYP DEFAULT 'ZPODER_MST' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
