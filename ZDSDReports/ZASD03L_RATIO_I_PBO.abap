*----------------------------------------------------------------------*
***INCLUDE ZASD03L_RATIO_I_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'ASD03R_9000'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 CA ZTSD_RATIO_I-ZGROP.
      SCREEN-INPUT = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
