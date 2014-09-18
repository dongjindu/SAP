
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_SEL                                        *
*----------------------------------------------------------------------*
*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_DATUM   FOR  SY-DATUM default sy-datum,
                 S_UPTIM  for edidc-updtim,
                 S_MESTYP FOR EDIDC-MESTYP,
                 S_SNDPRN FOR EDIDC-SNDPRN,
                 S_STATUS FOR EDIDC-STATUS,
                 s_dirct  for edidc-direct,
                 s_docnm  for edidc-docnum.

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
