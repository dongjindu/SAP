*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0001O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA : LT_FCODE TYPE TABLE OF SY-UCOMM.

  SET PF-STATUS 'G0100' EXCLUDING LT_FCODE .
  SET TITLEBAR 'T0100' .

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_ALV_OBJECT OUTPUT.

  IF G_ALV_DOC_MDAT IS INITIAL.
    PERFORM CREATE_CONTAINER_OBJECT    USING SY-DYNNR.
    PERFORM SET_ATTRIBUTES_ALV_GRID    USING SY-DYNNR.
    PERFORM BUILD_FIELD_CATALOG        USING SY-DYNNR.
    PERFORM EXCLUDING_FUNCTIONS        USING SY-DYNNR.
    PERFORM ASSIGN_ITAB_TO_ALV         USING SY-DYNNR.
    PERFORM ASSIGN_EVENT               USING SY-DYNNR.
  ELSE .
    PERFORM REFRESH_DATA               USING SY-DYNNR.
  ENDIF .

ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'G0200'.
  SET TITLEBAR 'T0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
