*&--------------------------------------------------------------------*
*& Author                 : JIPARK
*& Creation Date          : 11/12/2003
*& Specification By       : JIPARK
*& Pattern                : Report 1-3
*& Development Request No : UD1K904030
*& Addl documentation     :
*& Description  : freeze, finalize cash flow plan
*&
*& Modification Log
*&--------------------------------------------------------------------*
*& Date        Developer      Request ID      Description             *
*&--------------------------------------------------------------------*
*& 2011.09.30   YN.Kim        UP1K920005      ECC.6 Upgrade           *
*&--------------------------------------------------------------------*
REPORT zcfit03 MESSAGE-ID zmfi NO STANDARD PAGE HEADING
                               LINE-SIZE 140. " LINE-COUNT 80.

  include zcfit03_t01.
  include zcfit03_f01.

*------------------------------------------------------------------*
* INITIALIZATION                                                   *
*------------------------------------------------------------------*
INITIALIZATION.

  PERFORM  init_screen.

***********************************************************************
* START-OF-SELECTION
***********************************************************************
START-OF-SELECTION.

  PERFORM  get_t001.
  perform  select_data.

***********************************************************************
* END-OF-SELECTION
***********************************************************************
END-OF-SELECTION.

  perform display_alv.
*
