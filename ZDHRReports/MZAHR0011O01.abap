*----------------------------------------------------------------------*
***INCLUDE MZAHR0011O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'PS9000'.
  SET TITLEBAR '900'.

ENDMODULE.                 " STATUS_9000  OUTPUT
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
