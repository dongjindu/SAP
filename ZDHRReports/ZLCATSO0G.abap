* XZQP9CK126055 06.02.01 Hinweis 380118
* 4.6C
*eject
*&---------------------------------------------------------------------*
*&      Module  D2000_GLOBAL_SETTINGS  OUTPUT
*&---------------------------------------------------------------------*
*       set global values                                              *
*----------------------------------------------------------------------*
MODULE d2000_global_settings OUTPUT.
* keep the number of columns on screen.
* KEEP_SCOLS = SY-SCOLS.
*  PERFORM refresh_table_control.  " Hinweis 380118 "XCF del note 443364
  PERFORM set_tc_width CHANGING keep_scols.


ENDMODULE.                             " D2000_GLOBAL_SETTINGS  OUTPUT
