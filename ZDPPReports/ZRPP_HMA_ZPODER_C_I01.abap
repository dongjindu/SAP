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
      READ TABLE GT_DATA INDEX 1 .
      CHECK GT_DATA-STATUS IS INITIAL.
      PERFORM POPUP_TO_CONFIRM_1 USING  'S'
                                        'Confirm'
                                        TEXT-M01
                                        ' '
                                        ''
                               CHANGING LV_ANS.
      CHECK LV_ANS NE 'N'.
      PERFORM P3000_PO_PROC.
    WHEN 'REJC'.
      READ TABLE GT_DATA INDEX 1 .
      CHECK GT_DATA-STATUS IS INITIAL.
      PERFORM POPUP_TO_CONFIRM_1 USING  'S'
                                    'Confirm'
                                        TEXT-M01
                                    ' '
                                    ''
                           CHANGING LV_ANS.
      CHECK LV_ANS NE 'N'.
      PERFORM P3100_PO_REJC.
    WHEN 'ZVIN'.
      READ TABLE GT_DATA INDEX 1 .
      CHECK GT_DATA-STATUS EQ '@5D@'.
      PERFORM POPUP_TO_CONFIRM_1 USING  'S'
                                        'Confirm'
                                        TEXT-M01
                                        ' '
                                        '' CHANGING LV_ANS.

      .
      CHECK LV_ANS NE 'N'.
      PERFORM P3200_ZVIN_SAVE.

    WHEN 'ZVIN_DEL'.

      DELETE FROM ZTSD_UM WHERE WO_SERIAL NE ''.

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
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
      LEAVE TO SCREEN 0.
   ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
