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

REPORT  ZRPP_HMA_IDOC_REPORT  MESSAGE-ID ZMPP. .

*---------------------------------------------------------------------*
*  INCLUDE
*---------------------------------------------------------------------*
INCLUDE ZRPP_HMA_IDOC_REPORT_TOP.

INCLUDE ZRPP_HMA_IDOC_REPORT_SEL.
INCLUDE ZRPP_HMA_IDOC_REPORT_C01.
INCLUDE ZRPP_HMA_IDOC_REPORT_F01.
INCLUDE ZRPP_HMA_IDOC_REPORT_O01.
INCLUDE ZRPP_HMA_IDOC_REPORT_I01.



*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
GV_REPID = SY-CPROG.
*G_REPID = SY-REPID.
*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM P2000_GET_DATA.
  IF <INTAB> IS INITIAL .
  ENDIF.

END-OF-SELECTION.

  CALL SCREEN 0100.
