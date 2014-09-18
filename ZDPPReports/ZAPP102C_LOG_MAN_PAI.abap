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
*          CLEAR:  LTAB.
*          REFRESH: LTAB, ITAB1, ITAB2, S_LGORT.
          SET SCREEN 0.
        WHEN 'EXIT'.
*          CLEAR:  LTAB.
*          REFRESH: LTAB, ITAB1, ITAB2, S_LGORT.
          LEAVE PROGRAM.
        WHEN 'CANC'.
*          CLEAR: P_WERKS, LTAB, S_LGORT.
*          REFRESH: LTAB, ITAB1, ITAB2, S_LGORT.
          SET SCREEN 0.
      ENDCASE.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
