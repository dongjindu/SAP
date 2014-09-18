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
