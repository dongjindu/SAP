*----------------------------------------------------------------------*
***INCLUDE ZXMPKO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  pbo_screen  OUTPUT
*&---------------------------------------------------------------------*
*       PBO
*----------------------------------------------------------------------*
MODULE pbo_screen OUTPUT.
  PERFORM move_field_value_pbo.
ENDMODULE.                 " pbo_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_header_attribute  OUTPUT
*&---------------------------------------------------------------------*
*       Modify Header Fields Attribute
*----------------------------------------------------------------------*
MODULE modify_header_attribute OUTPUT.
  PERFORM modify_header_attribute USING gf_mode.
ENDMODULE.                 " modify_header_attribute  OUTPUT
