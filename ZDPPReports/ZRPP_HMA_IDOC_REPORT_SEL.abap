
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_SEL                                        *
*----------------------------------------------------------------------*
*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_WOSER FOR ZTPP_WOSUM-WO_SER,
                 S_DATUM FOR SY-DATUM OBLIGATORY DEFAULT SY-DATUM ,
                 S_DOCNUM FOR EDIDC-DOCNUM NO-DISPLAY.
PARAMETERS : P_MSTYP LIKE EDIDC-MESTYP OBLIGATORY ,
             P_DIRECT LIKE EDIDC-DIRECT DEFAULT '2',
             P_RCVPRN LIKE EDIDC-RCVPRN DEFAULT 'UD1300',
             P_SNDPRN LIKE EDIDC-SNDPRN DEFAULT 'NDECLNT850'.

PARAMETER :P_SUBMIT NO-DISPLAY DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
