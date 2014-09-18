*----------------------------------------------------------------------*
*   INCLUDE MZAHR0010O01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'PS9000'.
  SET TITLEBAR '900'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_value OUTPUT.
  IF w_zyear = '' OR w_zvers = ''.
    CLEAR zthr_pcp02.

    SELECT SINGLE * FROM zthr_pcp02
        WHERE zmodl = '02'
          AND zgrup = '1030'
          AND zval4 <> ''.
    MOVE : zthr_pcp02-zval2 TO w_zyear,
           zthr_pcp02-zval1 TO w_zvers.
  ENDIF.

ENDMODULE.                 " INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  screen_modify  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_modify OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 EQ 'P1'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " screen_modify  OUTPUT
