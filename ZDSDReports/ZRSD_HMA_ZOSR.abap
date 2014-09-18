*&---------------------------------------------------------------------*
*& Report  ZRPP_HMA_ZPODER                                             *
*&---------------------------------------------------------------------*
*& Program:                                                            *
*& Type   :                                                            *
*& Author :                                                            *
*& Title  :  [HMA]Order Status Report(OSR)  Interface                  *
*&---------------------------------------------------------------------*
*& Requested by:        Daniel Kim                                     *
*&---------------------------------------------------------------------*
*  MODIFICATION LOG
************************************************************************
*  DATE      Developer      RequestNo.      Description
*  29/10/10   sjlee                          Init.
*  02/09/2011 Victor        Selection logic / Screen
*  03/04/2014 Victor        Added HMM(B20)/Removed hard coding
***********************************************************************
REPORT  ZRSD_HMA_ZOSR MESSAGE-ID zm_hma.

*---------------------------------------------------------------------*
*  INCLUDE
*---------------------------------------------------------------------*
INCLUDE zrsd_hma_zosr_top.
INCLUDE zrsd_hma_zosr_sel.
INCLUDE zrsd_hma_zosr_f01.
INCLUDE zrsd_hma_zosr_c01.

*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
  gv_repid = sy-repid.

*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

  perform get_ale_destination.

  PERFORM check_program.

  IF p_input EQ 'X'.
    PERFORM p1000_start_progressbar USING
    'Run Input Plan Process...' '5'.
    PERFORM call_input_plan.
    PERFORM p1000_start_progressbar USING
    'End Input Plan Process...' '100'.
  ENDIF.

  IF p_vin   EQ 'X'.
    PERFORM p1000_start_progressbar USING
    'Run Vehicle Plan Process...' '5'.
    PERFORM call_vehc_plan.
    PERFORM p1000_start_progressbar USING
    'End Vehicle Plan Process...' '100'.
  ENDIF.

  PERFORM p2000_get_data.

  PERFORM p1000_start_progressbar USING
                                  'Create Order Status Report' '40'.
  PERFORM p2100_mod_data.

** Furong on 03/28/12 add estiamted sign-off date
  perform assign_signoff_date.
** end on 03/28/12

  IF p_delta EQ 'X' AND p_miss IS INITIAL.  "Victor
    PERFORM p1000_start_progressbar USING 'Create Delta Data...' '10'.
    PERFORM p2200_get_delta.
    PERFORM p1000_start_progressbar USING 'Create Delta Data...' '100'.
  ENDIF.


  LOOP AT it_osr.
    MOVE-CORRESPONDING it_osr TO gt_item.
    gt_item-dt01_a = it_osr-dt01.
    gt_item-dt06_a = it_osr-dt06.
    gt_item-dt07_a = it_osr-dt07.
    gt_item-dt08_a = it_osr-dt08.
    gt_item-dt09_a = it_osr-dt09.
    gt_item-dt10_a = it_osr-dt10.
    gt_item-dt11_a = it_osr-dt11.
    gt_item-dt12_a = it_osr-dt12.
    gt_item-dt13_a = it_osr-dt13.
    gt_item-dt14_a = it_osr-dt14.
    gt_item-dt15_a = it_osr-dt15.
    gt_item-crdt_a = it_osr-crdt.
    gt_item-ipdd_a = it_osr-ipdd.
    gt_item-lpdd_a = it_osr-lpdd.
    gt_item-chdt_a = it_osr-chdt.
    APPEND gt_item.
  ENDLOOP.

*  GT_ITEM[] = IT_OSR[].
  IF NOT gt_item[] IS INITIAL.
    IF p_send EQ 'X'.
      PERFORM send_data.
    ENDIF.
    PERFORM 1000_display_data.
  ELSE.
    MESSAGE s000.
  ENDIF.

END-OF-SELECTION.
