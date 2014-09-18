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
INCLUDE ZRPP_HMA_OPENSPEC_TOP.
INCLUDE ZRPP_HMA_OPENSPEC_SEL.
INCLUDE ZRPP_HMA_OPENSPEC_F01.
INCLUDE ZRPP_HMA_OPENSPEC_O01.
INCLUDE ZRPP_HMA_OPENSPEC_I01.
INCLUDE ZRPP_HMA_OPENSPEC_C01.
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
