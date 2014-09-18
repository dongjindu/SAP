*----------------------------------------------------------------------*
***INCLUDE ZASD03L_VEN_WC_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'ISD14R_9000'.

  CHECK OK_CODE = 'CHAN'.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'KY'.
    SCREEN-INPUT = 0.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " STATUS_9000  OUTPUT
