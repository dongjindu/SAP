*&--------------------------------------------------------------------*
*& Author                 : JIPARK                                    *
*& Creation Date          : 15/01/2004                                *
*& Specification By       : Andy Choi                                 *
*& Pattern                : Report 1-14                               *
*& Development Request No : UD1K905959                                *
*& Addl documentation     :                                           *
*& Description  : Transfer Planned Sales Order                        *
*&                                                                    *
*& Modification Log                                                   *
*&--------------------------------------------------------------------*
*& Date        Developer      Request ID      Description             *
*& 09.26.2011  yn.kim         UP1K920005      ECC6. Upgrade           *
*&--------------------------------------------------------------------*
report zrfit14 message-id zmfi no standard page heading.
                                           "LINE-SIZE 140.


  include zrfit14_t01.
  include zrfit14_f01.

*&---------------------------------------------------------------------*
*& INITIALIZATION                                                      *
*&---------------------------------------------------------------------*
INITIALIZATION.

  perform  init_screen.

***********************************************************************
* START-OF-SELECTION
***********************************************************************
start-of-selection.

  perform select_data.
  perform create_data.

***********************************************************************
* END-OF-SELECTION
***********************************************************************
end-of-selection.

  perform  display_alv.
*
