*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_O01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: UCOMM TYPE TABLE OF SY-UCOMM WITH HEADER LINE,
        TMP_MAKTX LIKE MAKT-MAKTX.

  DATA : LV_TITLE(40),
         LV_TIME(40),
         LV_LINE TYPE I.

*
*  CONCATENATE S_URGNC-LOW '' INTO LV_TITLE
*  SEPARATED BY SPACE.

  WRITE : P_DATUM MM/DD/YYYY TO LV_TITLE.
  IF S_UZEIT-LOW IS INITIAL.
    IF GV_NEW EQ 'X'.
      WRITE : SY-UZEIT USING EDIT MASK '__:__:__'   TO LV_TIME.
    ELSE .
      LV_TIME = GV_TITLE.
    ENDIF.
  ELSE .
    WRITE : S_UZEIT-LOW USING EDIT MASK '__:__:__'   TO LV_TIME.
  ENDIF.

  DESCRIBE TABLE GT_DATA LINES LV_LINE.
  CONCATENATE LV_TITLE LV_TIME INTO LV_TITLE SEPARATED BY SPACE.

  IF NOT S_UZEIT-HIGH IS INITIAL.
    WRITE : S_UZEIT-HIGH USING EDIT MASK '__:__:__'   TO LV_TIME.
    CONCATENATE LV_TITLE '~' LV_TIME INTO LV_TITLE  SEPARATED BY SPACE.
  ENDIF.

  REFRESH UCOMM.
  IF P_SAVE EQ 'X'.
    UCOMM = 'SAVE'. APPEND UCOMM.
  ENDIF.
  SET PF-STATUS 'S100' EXCLUDING UCOMM.
  SET TITLEBAR  'T100' WITH LV_TITLE.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.
  PERFORM P1000_CREATE_OBJECT.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
