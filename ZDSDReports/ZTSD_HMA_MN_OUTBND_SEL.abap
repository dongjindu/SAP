*----------------------------------------------------------------------*
*   INCLUDE ZTSD_HMA_MN_OUTBND_SEL                                     *
*----------------------------------------------------------------------*
*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS : r_f1 RADIOBUTTON GROUP r2.
SELECTION-SCREEN COMMENT 5(10) text-c01.
PARAMETERS : r_f2 RADIOBUTTON GROUP r2.
SELECTION-SCREEN COMMENT 20(8) text-c02.
PARAMETERS : r_f3 RADIOBUTTON GROUP r2.
SELECTION-SCREEN COMMENT 35(10) text-c03.
PARAMETERS : r_f4 RADIOBUTTON GROUP r2.
SELECTION-SCREEN COMMENT 50(10) text-c04.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
PARAMETERS : r_batch AS CHECKBOX .
SELECTION-SCREEN BEGIN OF BLOCK ibox WITH FRAME .
SELECT-OPTIONS : s_dest FOR ztppvr-p_dest_code  NO INTERVALS.
SELECT-OPTIONS : s_vbeln FOR vbrk-vbeln NO INTERVALS .
SELECTION-SCREEN END OF BLOCK ibox.


SELECTION-SCREEN END OF BLOCK b1.

*-------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                           *
*-------------------------------------------------------------------*
