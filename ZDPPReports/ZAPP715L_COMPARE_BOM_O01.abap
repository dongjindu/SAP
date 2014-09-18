*----------------------------------------------------------------------*
*   INCLUDE ZAPP715L_COMPARE_BOM_O01                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS OUTPUT.
  CASE SY-DYNNR.
    WHEN '9000'.
      SET PF-STATUS '9000'.
      SET TITLEBAR '9000'.
    WHEN '9100'.
      SET PF-STATUS '9000'.
      SET TITLEBAR '9100'.
  ENDCASE.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ICON_EXTENSION  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_ICON_EXTENSION OUTPUT.
  PERFORM SET_ICON_9000.
ENDMODULE.                 " SET_ICON_EXTENSION  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHANGE_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHANGE_DISPLAY OUTPUT.
  PERFORM CHANGE_DISPLAY.

ENDMODULE.                 " CHANGE_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHANGE_DISPLAY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHANGE_DISPLAY_DATA OUTPUT.
  PERFORM CHANGE_DISPLAY_DATA.
ENDMODULE.                 " CHANGE_DISPLAY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_DATA OUTPUT.
  PERFORM DISPLAY_DATA.

ENDMODULE.                 " DISPLAY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CURSOR_FIELD OUTPUT.
  CASE SY-DYNNR.
    WHEN '9000'.
      SET CURSOR FIELD WA_TXT9000 LINE WA_LINE9000.
    WHEN '9100'.
      SET CURSOR FIELD WA_TXT9100 LINE WA_LINE9100.
  ENDCASE.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT
