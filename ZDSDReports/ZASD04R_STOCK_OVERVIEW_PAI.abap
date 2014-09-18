*----------------------------------------------------------------------*
*   INCLUDE ZASD04R_STOCK_OVERVIEW_PAI                                 *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SY-DYNNR.
    WHEN '9000'.
      CASE SAVE_OK_CODE.
        WHEN 'BACK'.
          CLEAR: P_WERKS, LTAB, S_LGORT.
          REFRESH: LTAB, ITAB1, ITAB2, S_LGORT.
          SET SCREEN 0.
        WHEN 'EXIT'.
          CLEAR: P_WERKS, LTAB, S_LGORT.
          REFRESH: LTAB, ITAB1, ITAB2, S_LGORT.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          CLEAR: P_WERKS, LTAB, S_LGORT.
          REFRESH: LTAB, ITAB1, ITAB2, S_LGORT.
          SET SCREEN 0.
      ENDCASE.
    WHEN '9100'.
      CASE SAVE_OK_CODE.
        WHEN 'BACK'.
          CLEAR: P_EX, P_IT.
          REFRESH: ITAB2.
          SET SCREEN 9000.
        WHEN 'EXIT'.
          CLEAR: P_EX, P_IT.
          REFRESH: ITAB2.
          SET SCREEN 9000.
        WHEN 'CANC'.
          CLEAR: P_EX, P_IT.
          REFRESH: ITAB2.
          SET SCREEN 9000.
      ENDCASE.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

*  SAVE_OK_CODE = OK_CODE.
*
*  CASE SAVE_OK_CODE.
*    WHEN 'PICK'.
*      REFRESH LT_ROWS. CLEAR LT_ROWS.
*      CALL METHOD ALV_GRID1->GET_SELECTED_ROWS
*               IMPORTING ET_INDEX_ROWS = LT_ROWS.
*      IF SY-SUBRC NE 0.
*        MESSAGE E000 WITH 'NOTHING'.
*      ELSE.
*        READ TABLE LT_ROWS INTO LS_SELECTED_LINE INDEX 1.
*        IF SY-SUBRC NE 0.
*          MESSAGE E000 WITH 'NO DATA'.
*        ELSE.
*          CALL SCREEN 9100.
*        ENDIF.
*      ENDIF.
*  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
