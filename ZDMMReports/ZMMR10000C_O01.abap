*----------------------------------------------------------------------*
*   INCLUDE ZMMR10000C_O01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_2000 OUTPUT.
  SET PF-STATUS '2000'.
  SET TITLEBAR '2000'.
ENDMODULE.                             " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  init_2000_screen
*&---------------------------------------------------------------------*
MODULE init_2000_screen OUTPUT.
  PERFORM init_2000_container.
  PERFORM init_2000_alvtree.
  PERFORM init_2000_alvgrid.
  PERFORM init_2000_txtedit.
ENDMODULE.                    "INIT_2000_SCREEN OUTPUT
