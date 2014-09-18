*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_O01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: ucomm TYPE TABLE OF sy-ucomm,
        tmp_maktx LIKE makt-maktx,
        lv_title(40),
        lv_line TYPE i.

  lv_title  = wa_ale_dest-comp.

  DESCRIBE TABLE gt_data LINES lv_line.

  REFRESH ucomm.
  SET PF-STATUS 'S100' EXCLUDING ucomm.
  SET TITLEBAR  'T100' WITH lv_title lv_line.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_object OUTPUT.
  PERFORM p1000_create_object.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
