*------------------------------------------------------------------*
* Report        : ZTRR00500                                        *
* Create Date   : 2011.08.17                                       *
* Create By     : T00193                                           *
* Title         : Cash Planning By Daily                           *
*                                                                  *
* Description   : Cash Planning By Daily                           *
*------------------------------------------------------------------*
* Change History                                                   *
* Date           Developer              Description                *
* 2011.08.17     YN.Kim                 Create                     *
*------------------------------------------------------------------*
REPORT  ztrr00500  NO STANDARD PAGE HEADING
                          MESSAGE-ID zmfi.
*                          LINE-SIZE 135.


  INCLUDE ztrr00500top.
  INCLUDE ztrr00500cls.
  INCLUDE ztrr00500f01.

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

  PERFORM get_date.
  PERFORM select_data.
  PERFORM make_tree_data.

*------------------------------------------------------------------*
* END-OF-SELECTION.                                              *
*------------------------------------------------------------------*
END-OF-SELECTION.

  CALL SCREEN 9000.
*
