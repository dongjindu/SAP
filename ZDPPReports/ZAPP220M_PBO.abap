*----------------------------------------------------------------------*
***INCLUDE ZAPP220M_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR  '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  initial_value_set  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_value_set OUTPUT.

  MOVE  'HMMA'     TO   is219-comp.
  IF is219-name219 IS INITIAL.
    MOVE  '001'      TO   is219-name219.
  ENDIF.

ENDMODULE.                 " initial_value_set  OUTPUT
