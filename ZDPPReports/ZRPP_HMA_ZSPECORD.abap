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
*  06/24/11  Victor                         Modify Alv
*  02/27/14  Victor                         Modified for HMM(Mexico)
************************************************************************

REPORT  zrpp_hma_zpoder   MESSAGE-ID zmpp.

*---------------------------------------------------------------------*
*  INCLUDE
*---------------------------------------------------------------------*
INCLUDE zrpp_hma_zspecord_top.

INCLUDE zrpp_hma_zspecord_sel.
INCLUDE zrpp_hma_zspecord_f01.
INCLUDE zrpp_hma_zspecord_o01.
INCLUDE zrpp_hma_zspecord_i01.
INCLUDE zrpp_hma_zspecord_c01.


*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
  gv_repid = sy-repid.

*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM get_nation.

*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM p2000_get_data.
  PERFORM show_progress     USING 'Data gathering...' '90'.
  IF gt_data[] IS INITIAL .
    MESSAGE s001 WITH text-t01.
    EXIT.
  ELSE .
    MESSAGE s001 WITH 'Selected '.
  ENDIF.

END-OF-SELECTION.
  PERFORM show_progress     USING 'Data gathering...' '100'.
  CALL SCREEN 0100.
