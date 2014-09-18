*----------------------------------------------------------------------*
***INCLUDE MZAQM01_INSP_SCHEDO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------*
MODULE SET_CURSOR_FIELD OUTPUT.
  SET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_0100 OUTPUT.

  CHECK ST_DIST-ISIR = C_MARK.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'REG'.
    SCREEN-ACTIVE = 0.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_0100  OUTPUT
*&------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.

  SET PF-STATUS '0200'.
  SET TITLEBAR  '0200' WITH WA_MODE.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_0200 OUTPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  READ TABLE IT_ZSQM_INSP_SCH_ITEM_F INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_ZSQM_INSP_SCH_ITEM_F TO ZSQM_INSP_SCH_ITEM_F.
  ELSE.
    CLEAR ZSQM_INSP_SCH_ITEM_F.
  ENDIF.

  WA_CON_LINES = SY-LOOPC.

ENDMODULE.                 " TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_0200  OUTPUT
*&-----------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_0200 OUTPUT.
  IF ZSQM_INSP_SCH_ITEM_F IS INITIAL.
    LOOP AT SCREEN.
      SCREEN-INPUT = 0.
*    SCREEN-ACTIVE = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF ST_DIST-REGU = C_MARK.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'REG'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF WA_STATUS = C_SAVED.
    LOOP AT SCREEN.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " MODIFY_SCREEN_TABLE_0200  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_0200  OUTPUT
*&-----------------------------------------------------------------*
MODULE TABLE_CONTROL_LINES_0200 OUTPUT.
*--- Move the number of Internal Table Records to TABLE CONTROL-LINES
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.                "not headerline

  DESCRIBE TABLE IT_ZSQM_INSP_SCH_ITEM_F LINES <TC>-LINES.

ENDMODULE.                 " TABLE_CONTROL_LINES_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_0300 OUTPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  READ TABLE IT_ZSQM_INSP_SCH_ITEM_F INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_ZSQM_INSP_SCH_ITEM_F TO ZSQM_INSP_SCH_ITEM_F.
  ELSE.
    CLEAR ZSQM_INSP_SCH_ITEM_F.
  ENDIF.

  WA_CON_LINES = SY-LOOPC.

ENDMODULE.                 " TABLE_CONTROL_OUTPUT_0300  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_0300  OUTPUT
*&-----------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_0300 OUTPUT.
  IF ZSQM_INSP_SCH_ITEM_F IS INITIAL         OR
     ZSQM_INSP_SCH_ITEM_F-STATUS = C_RELEASE.
    LOOP AT SCREEN.
      SCREEN-INPUT = 0.
*      SCREEN-ACTIVE = 0.
      MODIFY SCREEN.
    ENDLOOP.
    EXIT.
  ENDIF.

  IF ZSQM_INSP_SCH_ITEM_F-STATUS = C_DONTUSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP4 NE 'DON'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CASE WA_MODE.
    WHEN C_CHANGE.
      IF ST_DIST-REGU = C_MARK.
        LOOP AT SCREEN.
          IF SCREEN-GROUP1 = 'REG'.
            SCREEN-INPUT = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    WHEN C_DISPLAY.
      LOOP AT SCREEN.
        IF SCREEN-GROUP4 NE 'CHG'.
          SCREEN-INPUT = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " MODIFY_SCREEN_TABLE_0300  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_0300  OUTPUT
*&-----------------------------------------------------------------*
MODULE TABLE_CONTROL_LINES_0300 OUTPUT.
*--- Move the number of Internal Table Records to TABLE CONTROL-LINES
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.                "not headerline

  DESCRIBE TABLE IT_ZSQM_INSP_SCH_ITEM_F LINES <TC>-LINES.

ENDMODULE.                 " TABLE_CONTROL_LINES_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS '0300'.
  SET TITLEBAR  '0300' WITH WA_MODE.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_0300  OUTPUT
*&-----------------------------------------------------------------*
MODULE MODIFY_SCREEN_0300 OUTPUT.
  CHECK WA_MODE = C_CHANGE            AND
        ZSQM_INSP_SCH_HDR-STATUS NE  C_RELEASE.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'CHG'.
      SCREEN-INPUT = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_0300  OUTPUT
