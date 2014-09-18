*------------------------------------------------------------------*
* Report        : ZTRR01200                                        *
* Create Date   : 2011/08/17                                       *
* Create By     : T00193                                           *
* Title         : Investment code by Planning group                *
*                                                                  *
* Description   : Get all line item                                *
*------------------------------------------------------------------*
* Change History                                                   *
* Date           Developer              Description                *
* 2011.08.17     YN.Kim                 Create                     *
*------------------------------------------------------------------*
REPORT  ztrr01200 MESSAGE-ID zmfi.


 include ZTRR01200_t01.
 include ZTRR01200_f01.

*------------------------------------------------------------------*
* INITIALIZATION                                                   *
*------------------------------------------------------------------*
INITIALIZATION.

  perform  init_screen.

*------------------------------------------------------------------*
* TOP-OF-PAGE                                                      *
*------------------------------------------------------------------*
TOP-OF-PAGE.
*------------------------------------------------------------------*
* START-OF-SELECTION                                               *
*------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_order.
  PERFORM select_text.
  PERFORM select_data.

*------------------------------------------------------------------*
* END-OF-SELECTION                                                 *
*------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM display_list.
*
