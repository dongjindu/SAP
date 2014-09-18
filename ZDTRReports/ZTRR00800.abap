*------------------------------------------------------------------*
* Report        : ZTRR00800                                        *
* Create Date   : 2011/08/17                                       *
* Create By     : yn.kim                                           *
* Title         : Cash Flow ( Plan/Actual ) By Month               *
*                                                                  *
* Description   : Cash Flow ( Plan/Actual ) By Month               *
*------------------------------------------------------------------*
* Change History                                                   *
* Date           Developer              Description                *
* 2011.09.21     yn.kim                                            *
*------------------------------------------------------------------*
REPORT  ZTRR00800  MESSAGE-ID ZMFI.


  INCLUDE ZTRR00800TOP.
  INCLUDE ZTRR00800CLS.
  INCLUDE ZTRR00800F01.

*------------------------------------------------------------------*
* INITIALIZATION                                                   *
*------------------------------------------------------------------*
INITIALIZATION.

  perform  init_proc.

*------------------------------------------------------------------*
* START-OF-SELECTION.                                              *
*------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM GET_DATE.
  PERFORM SELECT_DATA.           "ACTUAL DATA/PLAN DATA
  PERFORM MAKE_TREE_DATA.        "GET HIERARCHY.

*------------------------------------------------------------------*
* END-OF-SELECTION.                                              *
*------------------------------------------------------------------*
END-OF-SELECTION.

  CALL SCREEN 9000.
*
