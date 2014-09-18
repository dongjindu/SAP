*&---------------------------------------------------------------------*
*& Report  ZRPP_HMA_ZPODER                                             *
*&---------------------------------------------------------------------*
*& Program:                                                            *
*& Type   :                                                            *
*& Author :                                                            *
*& Title  :                                                            *
*&---------------------------------------------------------------------*
*& Requested by:        Daniel Kim                                     *
*&---------------------------------------------------------------------*
*  MODIFICATION LOG
************************************************************************
*  DATE      Developer      RequestNo.      Description
*  29/10/10  sjlee                          Init.
************************************************************************

REPORT  ZRPP_HMA_ZPODER   MESSAGE-ID ZMPP. .

*---------------------------------------------------------------------*
*  INCLUDE
*---------------------------------------------------------------------*
INCLUDE ZRSD_NOBODY_URGENCY_TOP.
INCLUDE ZRSD_NOBODY_URGENCY_SEL.
INCLUDE ZRSD_NOBODY_URGENCY_F01.
INCLUDE ZRSD_NOBODY_URGENCY_O01.
INCLUDE ZRSD_NOBODY_URGENCY_I01.
INCLUDE ZRSD_NOBODY_URGENCY_C01.

*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
  GV_REPID = SY-REPID.
*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM P1000_START_PROGRESSBAR USING 5.
  PERFORM P2000_GET_DATA.
  IF GT_DATA[] IS INITIAL .
  ENDIF.

END-OF-SELECTION.

  CALL SCREEN 0100.
