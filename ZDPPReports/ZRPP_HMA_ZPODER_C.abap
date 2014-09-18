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

REPORT  ZRPP_HMA_ZPODER    .

*---------------------------------------------------------------------*
*  INCLUDE
*---------------------------------------------------------------------*
INCLUDE ZRPP_HMA_ZPODER_C_TOP.
*INCLUDE ZRPP_HMA_ZPODER_TOP.

INCLUDE ZRPP_HMA_ZPODER_C_SEL.
*INCLUDE ZRPP_HMA_ZPODER_SEL.
INCLUDE ZRPP_HMA_ZPODER_C_C01.
*INCLUDE ZRPP_HMA_ZPODER_C01.
INCLUDE ZRPP_HMA_ZPODER_C_F01.
*INCLUDE ZRPP_HMA_ZPODER_F01.
INCLUDE ZRPP_HMA_ZPODER_C_O01.
*INCLUDE ZRPP_HMA_ZPODER_O01.
INCLUDE ZRPP_HMA_ZPODER_C_I01.
*INCLUDE ZRPP_HMA_ZPODER_I01.



*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
GV_REPID = SY-REPID.
*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM P2000_GET_DATA.
  IF GT_DATA[] IS INITIAL .
  ENDIF.

END-OF-SELECTION.

  CALL SCREEN 0100.
