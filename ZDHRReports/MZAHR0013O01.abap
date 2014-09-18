*----------------------------------------------------------------------*
***INCLUDE MZAHR0013O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.

  SET PF-STATUS 'PS9000'.
  SET TITLEBAR '9000'.

  IF W_ZYEAR IS INITIAL.
    W_ZYEAR = SY-DATUM(4).
  ENDIF.
  IF W_WERKS IS INITIAL.
    W_WERKS = '1010'.
    SELECT SINGLE NAME1 INTO W_NAME1
    FROM T500P WHERE MOLGA = '10'.

  ENDIF.
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
