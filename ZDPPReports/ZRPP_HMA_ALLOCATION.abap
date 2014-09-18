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
INCLUDE ZRPP_HMA_ALLOCATION_TOP.
INCLUDE ZRPP_HMA_ALLOCATION_SEL.
INCLUDE ZRPP_HMA_ALLOCATION_F01.
INCLUDE ZRPP_HMA_ALLOCATION_O01.
INCLUDE ZRPP_HMA_ALLOCATION_I01.
INCLUDE ZRPP_HMA_ALLOCATION_C01.
*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
  GV_REPID = SY-REPID.
  CLEAR : GV_FLAG.
*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM P1000_START_PROGRESSBAR USING 5.

  IF P_BATCH EQ 'X'.
    PERFORM P2000_GET_DATA.
    PERFORM P2200_SAVE_CBO.
  ELSE.
    PERFORM P2100_CHECK_DATE.
    IF GV_NEW EQ 'X' .
      PERFORM P2000_GET_DATA.
    ELSE.
      PERFORM P2000_GET_CBO_DATA.
    ENDIF.
    IF P_SAVE EQ 'X'.
      PERFORM P2200_SAVE_CBO.
    ENDIF.
    CALL SCREEN 0100.
  ENDIF.
  PERFORM P1000_START_PROGRESSBAR USING 100.
END-OF-SELECTION.
