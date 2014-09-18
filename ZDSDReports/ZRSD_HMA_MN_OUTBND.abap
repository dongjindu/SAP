*&---------------------------------------------------------------------*
*& Report  ZRSD_HMA_MN_OUTBND                                          *
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
*  12/15/10  sjlee                          Init.
************************************************************************
REPORT  zrsd_hma_mn_outbnd     MESSAGE-ID zmpp                      .

TABLES : ztppvr.

INCLUDE : ztsd_hma_mn_outbnd_top,
          ztsd_hma_mn_outbnd_sel,
          ztsd_hma_mn_outbnd_f01.

*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
*  GV_REPID = SY-REPID.
*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM p1000_start_progressbar USING 5 .
  PERFORM p2000_get_data.
  PERFORM p1000_start_progressbar USING 90 .

END-OF-SELECTION.
