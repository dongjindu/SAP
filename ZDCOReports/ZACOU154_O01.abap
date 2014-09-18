*&---------------------------------------------------------------------*
*&  Include           ZACOU154_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
*... BUTTON CONTROL
  DATA: lt_fcode TYPE TABLE OF sy-ucomm.
  CLEAR: lt_fcode[].

  IF p_radio1 EQ 'X'.
    APPEND 'SAVE' TO lt_fcode.
  ENDIF.

  SET PF-STATUS '0100' EXCLUDING lt_fcode.
  SET TITLEBAR '0100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_grid OUTPUT.
* alv
  PERFORM create_alv.
ENDMODULE.                 " ALV_GRID  OUTPUT
