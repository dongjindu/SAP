*&---------------------------------------------------------------------*
* Report        : ZTRR00900                                            *
* Create Date   : 2011/08/17                                           *
* Create By     : yn.Kim                                               *
* Title         : Cash Flow Actual By G/L Account                      *
*                                                                      *
* Description   : Cash Flow Actual By G/L Account                      *
*&---------------------------------------------------------------------*
* Change History                                                       *
* Date           Developer              Description                    *
* 2011.09.26     yn.Kim                 Test/Modify                    *
*&---------------------------------------------------------------------*
REPORT  ztrr00900  MESSAGE-ID zmfi.

  INCLUDE ztrr00900top.
  INCLUDE ztrr00900cls.
  INCLUDE ztrr00900f01.

*&---------------------------------------------------------------------*
* INITIALIZATION                                                       *
*&---------------------------------------------------------------------*
INITIALIZATION.

  PERFORM  init_screen.

*&---------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.
  PERFORM make_tree_data.

*&---------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  CALL SCREEN 9000.
*
