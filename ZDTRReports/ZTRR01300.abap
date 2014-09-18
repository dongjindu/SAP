*------------------------------------------------------------------*
* Report        : ZTRR01300                                        *
* Create Date   : 2011.08.17                                       *
* Create By     : yn.kim                                           *
* Title         : Cash Flow Actual List by Planning Group          *
*                                                                  *
* Description   : Cash Flow Actual List by Planning Group          *
*------------------------------------------------------------------*
* Change History                                                   *
* Date           Developer              Description                *
* 2011.09.21     yn.kim                 Create                     *
*------------------------------------------------------------------*
REPORT  ztrr01300 MESSAGE-ID zmfi.

  include ztrr01300_t01.
  include ztrr01300_f01.

*------------------------------------------------------------------*
* INITIALIZATION                                                   *
*------------------------------------------------------------------*
INITIALIZATION.

*// screen initial Setting
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
    perform export_list.     "Data export.
  ENDIF.
*
