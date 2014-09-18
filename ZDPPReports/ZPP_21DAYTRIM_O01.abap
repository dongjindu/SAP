*----------------------------------------------------------------------*
*   INCLUDE ZSJ_TEST029_O01                                            *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: UCOMM TYPE TABLE OF SY-UCOMM,
        TMP_MAKTX LIKE MAKT-MAKTX,
        LINE TYPE I.

  DATA : LV_DATUM(20).

  DESCRIBE TABLE <ITAB> LINES LINE.



  REFRESH UCOMM.
  SET PF-STATUS 'S100' EXCLUDING UCOMM.
  SET TITLEBAR  'T100' WITH LINE.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.
  PERFORM P1000_CREATE_OBJECT.

ENDMODULE.                 " CREATE_OBJECT  OUTPUT
