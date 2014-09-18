*&---------------------------------------------------------------------*
* Report        : ZTRR00700                                            *
* Create Date   : 2011/08/17                                           *
* Create By     : yn.Kim                                               *
* Title         : Cash Flow ( Plan/Actual ) By Day                     *
*                                                                      *
* Description   : Cash Flow ( Plan/Actual ) By Day                     *
*&---------------------------------------------------------------------*
* Change History                                                       *
* Date           Developer              Description                    *
* 2011.09.26     yn.Kim                 Test/Modify                    *
*&---------------------------------------------------------------------*
REPORT  ztrr00700  MESSAGE-ID zmfi.

  INCLUDE ztrr00700top.
  INCLUDE ztrr00700cls.
  INCLUDE ztrr00700f01.

*&---------------------------------------------------------------------*
* INITIALIZATION                                                       *
*&---------------------------------------------------------------------*
INITIALIZATION.

  PERFORM  init_screen.


*&---------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_date.
  PERFORM select_data.          "ACTUAL DATA/PLAN DATA
  PERFORM make_tree_data.       "GET HIERARCHY.

*&---------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  CALL SCREEN 9000.
