
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_SEL                                        *
*----------------------------------------------------------------------*
*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF SCREEN 2000 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-BAT.
PARAMETERS : P_BATCH AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_DATUM LIKE SY-DATUM DEFAULT SY-DATUM.
SELECT-OPTIONS : S_UZEIT FOR SY-UZEIT NO-EXTENSION .
SELECTION-SCREEN SKIP 1.

PARAMETERS : P_SAVE AS CHECKBOX ,
             P_SUBMIT NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B2.
*SELECTION-SCREEN END OF SCREEN 2000.
*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
