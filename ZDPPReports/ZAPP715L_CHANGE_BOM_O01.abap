*----------------------------------------------------------------------*
*   INCLUDE ZAPP715L_CHANGE_BOM_O01                                    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR '9000'.
  wa_matnr  = p_matnr.
  wa_verid  = p_verid.
ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_5000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5000 OUTPUT.
  SET PF-STATUS '5000'.
  SET TITLEBAR '5000'.
ENDMODULE.                 " STATUS_5000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_cursor_field OUTPUT.
  SET CURSOR FIELD wa_txt9000 LINE wa_line9000.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_display OUTPUT.
  IF wa_dis1 = 'X' AND wa_dis2 = 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'DIS' .
         SCREEN-INPUT  = 1     .
         MODIFY SCREEN         .
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'DIS' .
         SCREEN-INPUT  = 0     .
         MODIFY SCREEN         .
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " SET_DISPLAY  OUTPUT
