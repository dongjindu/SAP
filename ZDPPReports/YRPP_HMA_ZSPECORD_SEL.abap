
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_SEL                                        *
*----------------------------------------------------------------------*
*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : p_woser FOR ztpp_wosum-wo_ser
                                   DEFAULT 'E*Z*' OPTION CP SIGN I,
*                 s_nation FOR ztpp_wosum-nation
*                                   DEFAULT 'B28' OPTION EQ SIGN I,
                 s_dealer FOR ztpp_wosum-dealer
                                   DEFAULT 'A*'  OPTION CP SIGN I,
                 p_datum FOR sy-datum  DEFAULT sy-datum .
PARAMETER :p_submit NO-DISPLAY DEFAULT 'X'.
*PARAMETER : P_CHECK  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(5) text-t03.
PARAMETER : p_ra1 RADIOBUTTON GROUP ra  DEFAULT 'X' USER-COMMAND ura.
SELECTION-SCREEN COMMENT 15(5) text-t04.
PARAMETER : p_ra2 RADIOBUTTON GROUP ra .
SELECTION-SCREEN COMMENT 30(5) text-t05.
PARAMETER : p_ra3 RADIOBUTTON GROUP ra.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
