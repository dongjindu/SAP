*----------------------------------------------------------------------*
*   INCLUDE ZXMG0U11                                                   *
*----------------------------------------------------------------------*
* Authored by BSBAE
* This program is for adjusting material number display.

DATA: lv_matnr(40). CLEAR: lv_matnr.
DATA: lv_strlen TYPE i. "String Length of Material Display.

CLEAR: lv_strlen.
lv_strlen = strlen( matnr ).
IF lv_strlen = 40.
  CHECK matnr(22) = '0000000000000000000000'.
  MOVE matnr+32(8) TO lv_matnr.
  CLEAR: matnr.
  MOVE lv_matnr TO matnr.
ENDIF.
