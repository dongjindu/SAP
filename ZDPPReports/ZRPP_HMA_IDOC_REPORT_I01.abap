*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_I01                                        *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* MODULE  USER_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
* TEXT :
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA LV_CNT TYPE P.
  DATA LV_ANS(1).
  CLEAR OK_CODE.
  OK_CODE = SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'PROC'.
*      READ TABLE GT_DATA INDEX 1.
*      IF GT_DATA-STATUS IS INITIAL.
*        PERFORM POPUP_TO_CONFIRM USING  'S'
*                                          'Confirm'
*                                          TEXT-M01
*                                          TEXT-M02
*                                          TEXT-M03
*                                 CHANGING LV_ANS.
*
*        CHECK LV_ANS EQ 'J'.
*        DATA : LT_EDIDD LIKE TABLE OF EDIDD WITH HEADER LINE,
*               LT_EDIDC LIKE TABLE OF EDIDC.
*        LOOP AT GT_DATA.
*          LT_EDIDD-SEGNAM = C_ZSPSEG.
*          LT_EDIDD-SDATA  = GT_DATA.
*
*          APPEND LT_EDIDD.
*          CLEAR: GT_DATA , LT_EDIDD.
*
*        ENDLOOP.
*
*        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*             EXPORTING
*                  TEXT   = 'IDOC Generating....'
*             EXCEPTIONS
*                  OTHERS = 1.
*
*        PERFORM  P3000_SEND_IDOC TABLES LT_EDIDD LT_EDIDC.
*        PERFORM  REFRESH_DISPLAY.
*      ELSE.
*        MESSAGE i001 WITH
*        'Already sended.'.
*      ENDIF.
  ENDCASE.

ENDMODULE.                 " user_command_0100  INPUT

*----------------------------------------------------------------------*
* MODULE  EXIT  INPUT
*----------------------------------------------------------------------*
* TEXT :
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
*  PERFORM P1110_DESTROY_OBJECT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " exit  INPUT
