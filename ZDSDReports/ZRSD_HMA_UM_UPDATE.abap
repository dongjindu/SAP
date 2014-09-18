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
*  04/28/2014 Victor            Initial Zvin added in the screen
************************************************************************

REPORT  zrpp_hma_um_update MESSAGE-ID zm_hma.

*---------------------------------------------------------------------*
*  INCLUDE
*---------------------------------------------------------------------*
INCLUDE zrsd_hma_um_update_top.
INCLUDE zrsd_hma_um_update_sel.
INCLUDE zrsd_hma_um_update_f01.
INCLUDE zrsd_hma_um_update_c01.

*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
  gv_repid = sy-repid.
  CLEAR : gv_message.
*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
** Furong on 08/28/12 for checking multi-run
  PERFORM check_multi_run.
  PERFORM check_screen. "04.28.2014 Victor

  CHECK w_flag IS INITIAL.
** End on 08/28/21
  PERFORM p1000_start_progressbar USING text-p01 '10'.
  PERFORM p2000_get_data.

  IF NOT gt_item[] IS INITIAL.
    IF p_view EQ 'X'.
      PERFORM 1000_display_data.
    ENDIF.
    MESSAGE s001 WITH gv_message.
  ELSE.

*-- ZTSD_UM update
    IF p_update EQ 'X'. "no I/F
      CALL FUNCTION 'Z_FPP_HMA_UPDATE_UM'
        TABLES
          return = gt_message.
    ELSE.
      MESSAGE s000.
    ENDIF.

  ENDIF.


*  fill vin

  IF p_fillv EQ 'X'.

    DATA : lv_status LIKE bapireturn-type .

    PERFORM p1000_start_progressbar USING 'Run Fill Vin..........' '70'.

    CALL FUNCTION 'Z_FPP_HMA_FILLVIN'
      EXPORTING
        i_zvin_init = p_zvin
        i_nation    = p_nation
      IMPORTING
        status      = lv_status
        message     = gv_message.

    CONCATENATE lv_status gv_message INTO gv_message.

** Furong on 08/28/12 for checking multi-run
    PERFORM unlock_prog.
** End on 08/28/21
  ENDIF.

  PERFORM p1000_start_progressbar USING 'End Processing........' '100'.

  MESSAGE s001 WITH gv_message.

END-OF-SELECTION.
