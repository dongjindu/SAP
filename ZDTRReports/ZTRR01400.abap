*------------------------------------------------------------------*
* Report        : ZTRR01400                                        *
* Create Date   : 2011.08.17                                       *
* Create By     : yn.kim                                           *
* Title         : Cash Flow Actual List by G/L                     *
*                                                                  *
* Description   : Cash Flow Actual List by G/L                     *
*------------------------------------------------------------------*
* Change History                                                   *
* Date           Developer              Description                *
* 2011.09.22     yn.kim                                            *
*------------------------------------------------------------------*
*------------------------------------------------------------------*
REPORT  ztrr01400 MESSAGE-ID zmfi.


  include ztrr01400_t01.
  include ztrr01400_f01.

*------------------------------------------------------------------*
* INITIALIZATION                                                   *
*------------------------------------------------------------------*
INITIALIZATION.

  perform  init_screen.

*------------------------------------------------------------------*
* START-OF-SELECTION                                               *
*------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.
  PERFORM make_it_list.

*------------------------------------------------------------------*
* END-OF-SELECTION                                                 *
*------------------------------------------------------------------*
END-OF-SELECTION.

  IF p_memory = ''.
    PERFORM display_list.
  ELSE.
    perform export_list.
  ENDIF.
*
