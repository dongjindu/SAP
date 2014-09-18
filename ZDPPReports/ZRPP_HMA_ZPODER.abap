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
REPORT  ZRPP_HMA_ZPODER   MESSAGE-ID ZM_HMA                 .

INCLUDE ZRPP_HMA_ZPODER_TOP.
INCLUDE ZRPP_HMA_ZPODER_SEL.
INCLUDE ZRPP_HMA_ZPODER_C01.
INCLUDE ZRPP_HMA_ZPODER_F01.
INCLUDE ZRPP_HMA_ZPODER_O01.
INCLUDE ZRPP_HMA_ZPODER_I01.


*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
  GV_REPID = SY-REPID.
*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM P1000_START_PROGRESSBAR USING TEXT-P01 '5'.
  PERFORM P2000_GET_DATA.

  IF GT_DATA[] IS INITIAL.
    MESSAGE S000 .
    EXIT.
  ENDIF.

  DATA : LINE TYPE I.
  CLEAR : LINE.

  SORT GT_DATA BY UPDDAT DOCNUM.

  LOOP AT GT_DATA.
    AT NEW DOCNUM.
      LINE = LINE  + 1 .
    ENDAT.
    GT_DATA-NO = LINE.
    MODIFY GT_DATA.
  ENDLOOP.
  SORT GT_DATA BY UPDDAT DOCNUM DESCENDING WO_SER .
  PERFORM P1000_START_PROGRESSBAR USING TEXT-P01 '100'.




END-OF-SELECTION.

  CALL SCREEN 0100.
