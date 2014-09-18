*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_O01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: UCOMM TYPE TABLE OF SY-UCOMM,
        TMP_MAKTX LIKE MAKT-MAKTX.

  DATA : LV_DATUM(20).

  CONCATENATE S_CREDAT-LOW S_CREDAT-HIGH INTO LV_DATUM
  SEPARATED BY SPACE.

  REFRESH UCOMM.
  SET PF-STATUS 'S100' EXCLUDING UCOMM.
  SET TITLEBAR  'T100' WITH LV_DATUM.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.
  PERFORM P1000_CREATE_OBJECT.

ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'S200'.
  SET TITLEBAR 'T200'.

ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT_0200 OUTPUT.
  PERFORM P1000_CREATE_OBJECT_200.
  PERFORM REFRESH_DISPLAY_200.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
