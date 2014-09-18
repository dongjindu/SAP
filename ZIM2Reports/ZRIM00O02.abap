*&---------------------------------------------------------------------*
*& INCLUDE ZRIM00O02 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 Amend Main PBO MODULE Include                *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.11                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1106  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1106 OUTPUT.

* DESCRIBE TABLE IT_ZSMLCSG7G  LINES G_PARAM_LINE.   " LINE 수 GET
* TC_1106-LINES = G_PARAM_LINE.                     " LINE 수 정?
  G_PARAM_LINE = TC_1106-TOP_LINE.
  TC_1106-LINES = G_PARAM_LINE + 6.              " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR1106  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1106_MARK_SCR1106  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_1106_MARK_SCR1106 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_1106-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_1106_MARK_SCR1106  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC1106_SCR1106  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC1106_SCR1106 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_1106-CURRENT_LINE GT TC_1106-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSMLCAMNARR INDEX TC_1106-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING IT_ZSMLCAMNARR TO ZSMLCAMNARR.     " DATA MOVE
     MOVE: IT_ZSMLCAMNARR-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC1106_SCR1106  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1114  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1114 OUTPUT.

* DESCRIBE TABLE IT_ZSMLCSG7G  LINES G_PARAM_LINE.   " LINE 수 GET
* TC_0103_1-LINES = G_PARAM_LINE.                     " LINE 수 정?
  G_PARAM_LINE = TC_1114-TOP_LINE.
  TC_1114-LINES = G_PARAM_LINE + 4.              " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR1114  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1114_MARK_SCR1114  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_1114_MARK_SCR1114 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_1114-CURRENT_LINE <= G_PARAM_LINE.
     W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
     CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_1114_MARK_SCR1114  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC1114_SCR1114  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC1114_SCR1114 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_1114-CURRENT_LINE GT TC_1114-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
*  READ TABLE IT_ZSLLCOF   INDEX TC_1114-CURRENT_LINE.
  READ TABLE IT_ZSLLCAMSGOF  INDEX TC_1114-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING IT_ZSLLCAMSGOF   TO ZSLLCOF.     " DATA MOVE
     MOVE: IT_ZSLLCAMSGOF-ZFSGOF         TO ZSLLCOF-ZFOFFER.
     MOVE: IT_ZSLLCAMSGOF-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC1114_SCR1114  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LLCOF_GET_LINE_SCR1114  INPUT
*&---------------------------------------------------------------------*
MODULE LLCOF_GET_LINE_SCR1114 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1114-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " LLCOF_GET_LINE_SCR1114  INPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1199  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1199 OUTPUT.

  DESCRIBE TABLE IT_ZSAMDLIST LINES G_PARAM_LINE.   " LINE 수 GET
  TC_1199-LINES = G_PARAM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR1199  OUTPUT
