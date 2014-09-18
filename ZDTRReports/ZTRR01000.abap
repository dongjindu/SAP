*------------------------------------------------------------------*
* Report        : ZTRR01000                                        *
* Create Date   : 2011.08.17                                       *
* Create By     : T00193                                           *
* Title         : Cash Position                                    *
*                                                                  *
* Description   : Cash Postion                                     *
*------------------------------------------------------------------*
* Change History                                                   *
* Date           Developer              Description                *
* 2011.08.17     YN.Kim                 Create                  *
*------------------------------------------------------------------*
REPORT  ztrr01000  NO STANDARD PAGE HEADING MESSAGE-ID zmfi
                                            LINE-SIZE 135.


  include ztrr01000_t01.
  include ztrr01000_f01.

*------------------------------------------------------------------*
* INITIALIZATION                                                   *
*------------------------------------------------------------------*
INITIALIZATION.
  GET PARAMETER ID 'BUK' FIELD p_bukrs.
  IF p_bukrs IS INITIAL .
    p_bukrs = 'H201'.
  ENDIF.

  perform  init_screen.

*------------------------------------------------------------------*
* TOP-OF-PAGE                                                      *
*------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM top_of_page.

*------------------------------------------------------------------*
* START-OF-SELECTION                                               *
*------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.
  PERFORM make_it_list.

*------------------------------------------------------------------*
* END-OF-SELECTION                                               *
*------------------------------------------------------------------*

  IF p_rb1 = 'X'.
    PERFORM write_list.
  ELSE.
    PERFORM display_list.
  ENDIF.
*
