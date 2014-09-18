
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_SEL                                        *
*----------------------------------------------------------------------*
*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF SCREEN 2000 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : P_WOSER FOR ZTPP_WOSUM-WO_SER
                                   DEFAULT 'E*' OPTION CP SIGN I,
                 S_NATION FOR ZTPP_WOSUM-NATION
                                   DEFAULT 'B28' OPTION EQ SIGN I,
                 S_DEALER FOR ZTPP_WOSUM-DEALER
                                   DEFAULT 'A*'  OPTION CP SIGN I,
                 P_DATUM FOR SY-DATUM OBLIGATORY DEFAULT SY-DATUM .


PARAMETERS : P_SUBMIT NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.

*SELECTION-SCREEN END OF SCREEN 2000.
*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
