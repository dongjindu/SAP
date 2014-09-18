*----------------------------------------------------------------------*
*   INCLUDE MZAHR0008O01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'PS9000'.
  SET TITLEBAR '900'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
  IF W_FLAGS = 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1'.
        SCREEN-INPUT = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE OUTPUT.
  IF W_ZYEAR = '' OR W_ZVERS = ''.
    CLEAR ZTHR_PCP02.

    SELECT SINGLE * FROM ZTHR_PCP02
        WHERE ZMODL = '02'
          AND ZGRUP = '1030'
          AND ZVAL4 <> ''.
    MOVE : ZTHR_PCP02-ZVAL2 TO W_ZYEAR,
           ZTHR_PCP02-ZVAL1 TO W_ZVERS.
  ENDIF.

ENDMODULE.                 " INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_DATA_SELECTION  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_DATA_SELECTION OUTPUT.
    IF W_FLAGS = ''.
      PERFORM SELECT_DATA.
    ENDIF.
ENDMODULE.                 " INITIAL_DATA_SELECTION  OUTPUT
