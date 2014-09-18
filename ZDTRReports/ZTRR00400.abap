*------------------------------------------------------------------*
* Report        : ZTRR00400                                        *
* Create Date   : 2011.08.17                                       *
* Create By     : yn.kim                                           *
* Title         : Cash Planning By Monthly                         *
*                                                                  *
* Description   : Cash Planning By Monthly                         *
*------------------------------------------------------------------*
* Change History                                                   *
* Date           Developer              Description                *
* 2011.09.22     yn.kim                 Create                     *
*------------------------------------------------------------------*
REPORT  ztrr00400  MESSAGE-ID zmfi.

  INCLUDE ztrr00400top.
  INCLUDE ztrr00400cls.
  INCLUDE ztrr00400f01.

*------------------------------------------------------------------*
* INITIALIZATION                                                   *
*------------------------------------------------------------------*
INITIALIZATION.

*// screen initial Setting
  PERFORM  init_screen.

*------------------------------------------------------------------*
* START-OF-SELECTION.                                              *
*------------------------------------------------------------------*
START-OF-SELECTION.

*// if display ==> Plan Version  SEQNO -> Required field.
*// Display(CBO Table data display)
  IF  p_r2 = 'X' AND p_seqno IS INITIAL.
    MESSAGE s031 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM select_data.
  PERFORM make_tree_data.

*------------------------------------------------------------------*
* END-OF-SELECTION.                                              *
*------------------------------------------------------------------*
END-OF-SELECTION.

  CALL SCREEN 9000.
*
