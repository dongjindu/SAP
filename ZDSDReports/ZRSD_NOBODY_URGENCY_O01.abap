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

  DATA : LV_TITLE(40), LV_LINE TYPE I.

  WRITE : SY-DATUM MM/DD/YYYY TO LV_TITLE.

  CONCATENATE LV_TITLE S_URGNC-LOW '' INTO LV_TITLE
  SEPARATED BY SPACE.

  DESCRIBE TABLE GT_DATA LINES LV_LINE.


  REFRESH UCOMM.
  SET PF-STATUS 'S100' EXCLUDING UCOMM.
  SET TITLEBAR  'T100' WITH LV_TITLE LV_LINE.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.
  PERFORM P1000_CREATE_OBJECT.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
