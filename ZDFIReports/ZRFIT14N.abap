*&--------------------------------------------------------------------*
*& Author                 : KDM                                       *
*& Creation Date          : 11/02/2011                                *
*& Specification By       : Shin SangJun                              *
*& Pattern                : Report 1-14                               *
*& Development Request No :                                           *
*& Addl documentation     :                                           *
*& Description  : Transfer Planned Sales Order                        *
*&                                                                    *
*& Modification Log                                                   *
*&--------------------------------------------------------------------*
*& Date        Developer      Request ID      Description             *
*&--------------------------------------------------------------------*
report zrfit14n message-id zmfi no standard page heading.
                                           "LINE-SIZE 140.

INCLUDE ZRFIT14N_T01.
INCLUDE ZRFIT14N_F01.

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
