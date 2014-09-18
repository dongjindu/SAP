*----------------------------------------------------------------------*
*   INCLUDE ZRIM10O02                                                  *
*----------------------------------------------------------------------*
*&  프로그램명 : [Include] 수입 수송 Main PBO MODULE(2)                *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.09.24                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  TC_0103_ENABLE_SET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0103_ENABLE_SET_SCR0103 OUTPUT.
*  PERFORM   P2000_MENGE_MODE_SET.
  PERFORM P2000_SCR_MODE_SET.
ENDMODULE.                 " TC_0103_ENABLE_SET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0104_ENABLE_SET_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0104_ENABLE_SET_SCR0104 OUTPUT.
*  PERFORM   P2000_MENGE_MODE_SET.
  PERFORM P2000_SCR_MODE_SET.
ENDMODULE.                 " TC_0104_ENABLE_SET_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0103 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSTRIT  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0103-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0103-TOP_LINE.
    TC_0103-LINES = G_PARAM_LINE + 14.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0104 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSTRCST  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0104-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0104-TOP_LINE.
    TC_0104-LINES = G_PARAM_LINE + 14.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0023  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0023 OUTPUT.
  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSTRCSTIT  LINES G_PARAM_LINE. " LINE 수 GET
    TC_0023-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0023-TOP_LINE.
    TC_0023-LINES = G_PARAM_LINE + 14.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0023  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET1_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET1_SCR0104 OUTPUT.
    SORT IT_ZTTRCSTIT BY ZFSEQ ZFITSEQ.
    DESCRIBE TABLE IT_ZTTRCSTIT  LINES G_PARAM_LINE. " LINE 수 GET
    TC_0104_1-LINES = G_PARAM_LINE.                   " LINE 수 정의.

ENDMODULE.                 " TOTAL_LINE_GET1_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET2_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET2_SCR0104 OUTPUT.

  CLEAR G_PARAM_LINE.
  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBSEG  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0104_2-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0104_2-TOP_LINE.
    TC_0104_2-LINES = G_PARAM_LINE + 14.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET2_SCR0104  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0103_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0103_SCR0103 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0103-CURRENT_LINE GT TC_0103-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSTRIT   INDEX TC_0103-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSTRIT   TO ZSTRIT.       " DATA MOVE
    MOVE: IT_ZSTRIT-ZFMARK         TO W_ROW_MARK.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0103_SCR0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0023_SCR0023  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0023_SCR0023 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0023-CURRENT_LINE GT TC_0023-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSTRCSTIT   INDEX TC_0023-CURRENT_LINE.
  IF SY-SUBRC = 0.                                    " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSTRCSTIT   TO ZSTRCSTIT.      " DATA MOVE
    MOVE: IT_ZSTRCSTIT-ZFMARK         TO W_ROW_MARK1.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0023_SCR0023  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0104_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0104_SCR0104 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0104-CURRENT_LINE GT TC_0104-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSTRCST   INDEX TC_0104-CURRENT_LINE.
  IF SY-SUBRC = 0.                                    " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSTRCST   TO ZSTRCST.      " DATA MOVE
    MOVE: IT_ZSTRCST-ZFMARK         TO W_ROW_MARK.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0104_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0104_1_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0104_1_SCR0104 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0104_1-CURRENT_LINE GT TC_0104_1-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZTTRCSTIT   INDEX TC_0104_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                    " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZTTRCSTIT   TO ZTTRCSTIT.   " DATA MOVE
  ENDIF.

ENDMODULE.                 " IT_TO_TC0104_1_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0104_2_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0104_2_SCR0104 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0104_2-CURRENT_LINE GT TC_0104_2-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBSEG   INDEX TC_0104_2-CURRENT_LINE.
  IF SY-SUBRC = 0.                                    " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBSEG   TO ZSBSEG.      " DATA MOVE
    MOVE: IT_ZSBSEG-ZFMARK         TO W_ROW_MARK2.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK2.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0104_2_SCR0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0020 OUTPUT.

  SET TITLEBAR 'POPU'.
  SET PF-STATUS 'POPU'.

  IF OPTION = '1'.
    SET CURSOR FIELD 'SPOP-OPTION1'.
  ELSE.
    SET CURSOR FIELD 'SPOP-OPTION2'.
  ENDIF.


ENDMODULE.                 " SET_STATUS_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0020 OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'SPOP-OPTION_CAN'.
      IF CANCEL_OPTION = SPACE.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE'.                   "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0021  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0021 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0021  OUTPUT
