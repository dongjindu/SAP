*&--------------------------------------------------------------------*
*& Author                 : JIPARK
*& Creation Date          : 11/20/2003
*& Specification By       : JIPARK
*& Pattern                : Report 1-8
*& Development Request No : UD1K904355
*& Addl documentation     :
*& Description  : budget to cash conversion
*&
*& Modification Log
*&--------------------------------------------------------------------*
*& Date        Developer      Request ID      Description             *
*&--------------------------------------------------------------------*
*& 2011.09.28   YN.Kim        UP1K920005      ECC.6 Upgrade           *
*&--------------------------------------------------------------------*
REPORT  zcfit01  MESSAGE-ID zmfi NO STANDARD PAGE HEADING
                            LINE-SIZE 129 LINE-COUNT 80.

  include zcfit01_t01.
  include zcfit01_f01.

*------------------------------------------------------------------*
* INITIALIZATION                                                   *
*------------------------------------------------------------------*
INITIALIZATION.

  PERFORM  init_screen.

***********************************************************************
* START-OF-SELECTION
***********************************************************************
START-OF-SELECTION.

  PERFORM get_target_fm.
  PERFORM get_fm_detail.
  PERFORM convert_to_cm.

***********************************************************************
* END-OF-SELECTION
***********************************************************************
END-OF-SELECTION.

  perform display_alv.
*
