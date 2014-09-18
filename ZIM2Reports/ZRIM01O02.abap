*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM01O02                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입 B/L 관련 PBO MODULE Include                      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.21                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC7110_SCR7110  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC7110_SCR7110 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_7110-CURRENT_LINE GT TC_7110-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBLUGC   INDEX TC_7110-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBLUGC   TO ZSBLUGC.     " DATA MOVE
    MOVE  IT_ZSBLUGC-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC7110_SCR7110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR3111  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR3111 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSIVIT  LINES G_PARAM_LINE.   " LINE  GET
    TC_3111-LINES = G_PARAM_LINE.                   " LINE  Define.
  ELSE.
    G_PARAM_LINE = TC_3111-TOP_LINE.
    TC_3111-LINES = G_PARAM_LINE + 4.               " LINE  Define.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR3111  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_3110_MARK_SCR3110  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_3110_MARK_SCR3110 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_3111-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_3110_MARK_SCR3110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3110_SCR3110  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC3110_SCR3110 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSIVIT   INDEX TC_3111-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE ZTIV-ZFIVAMC             TO IT_ZSIVIT-ZFIVAMC.
    MOVE-CORRESPONDING IT_ZSIVIT  TO ZSIVIT.          " DATA MOVE
    MOVE IT_ZSIVIT-ZFMARK         TO W_ROW_MARK.      " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC3110_SCR3110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR5710  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR5710 OUTPUT.

  DESCRIBE TABLE IT_ZSIVIT LINES LINE.
  TC_5710-LINES = LINE.

ENDMODULE.                 " TOTAL_LINE_GET_SCR5710  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_5710_MARK_SCR5710  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_5710_MARK_SCR5710 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_5710-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_5710_MARK_SCR5710  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC5710_SCR5710  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC5710_SCR5710 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIVIT   INDEX TC_5710-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVIT   TO ZSIVIT.      " DATA MOVE
    MOVE: IT_ZSIVIT-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.
ENDMODULE.                 " IT_TO_TC5710_SCR5710  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCRCOM_5710  OUTPUT
*&---------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCRCOM_5710 OUTPUT.

  LOOP AT SCREEN.
    CASE SY-TCODE.
      WHEN 'ZIM57'.                        " Invoice 금액 확?
        IF SCREEN-GROUP1 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      WHEN 'ZIM34'.                        " Invoice 상태 변?
        IF SCREEN-GROUP1 = 'IO2'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      WHEN OTHERS.
        SCREEN-INPUT   = '0'.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " OPERATION_MODE_SET_SCRCOM_5710  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR6710  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR6710 OUTPUT.

  G_PARAM_LINE = TC_6710-TOP_LINE.
  TC_6710-LINES = G_PARAM_LINE + 9.                 " LINE 수 정?
  IF NOT ( MSEG-MBLNR IS INITIAL ) OR NOT ( MSEG-MJAHR IS INITIAL ).
    DESCRIBE TABLE IT_ZSCUCLIVIT LINES LINE.
    TC_6710-LINES = LINE.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR6710  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6710_MARK_SCR6710  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_6710_MARK_SCR6710 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_6710-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_6710_MARK_SCR6710  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC6710_SCR6710  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC6710_SCR6710 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
* IF TC_6710-CURRENT_LINE GT TC_6710-LINES.
*    EXIT FROM STEP-LOOP.
* ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSCUCLIVIT   INDEX TC_6710-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE ZTCUCLIV-ZFIVAMC            TO IT_ZSCUCLIVIT-ZFIVAMC.
    MOVE-CORRESPONDING IT_ZSCUCLIVIT TO ZSCUCLIVIT.     " DATA MOVE
    MOVE: IT_ZSCUCLIVIT-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC6710_SCR6710  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR6210  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR6210 OUTPUT.

  IF TABSTRIP-ACTIVETAB IS INITIAL.
    TABSTRIP-ACTIVETAB = 'COMM1'.
    DYNPRO = '6212'.
  ENDIF.
  IF OK-CODE = 'COMM1'.
    TABSTRIP-ACTIVETAB = 'COMM1'.
    DYNPRO = '6212'.
  ENDIF.
  IF OK-CODE = 'COMM2'.
    TABSTRIP-ACTIVETAB = 'COMM2'.
    DYNPRO = '6214'.
  ENDIF.
  IF OK-CODE = 'IDRHS'.
    TABSTRIP-ACTIVETAB = 'IDRHS'.
    DYNPRO = '6216'.
  ENDIF.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR6210  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DYNSUB_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE DYNSUB_CLEAR_SCRCOM OUTPUT.

  CLEAR : PROGRAM, DYNSUB, DYNPRO.

ENDMODULE.                 " DYNSUB_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR6216  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR6216 OUTPUT.

  DESCRIBE TABLE IT_ZSIDRHS LINES LINE.
  TC_6216-LINES = LINE.                             " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR6216  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6216_MARK_SCR6216  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_6216_MARK_SCR6216 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_6216-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_6216_MARK_SCR6216  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC6216_SCR6216  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC6216_SCR6216 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
* IF TC_6216-CURRENT_LINE GT TC_6216-LINES.
*    EXIT FROM STEP-LOOP.
* ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIDRHS  INDEX TC_6216-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDRHS  TO ZSIDRHS.     " DATA MOVE
    MOVE: IT_ZSIDRHS-ZFMARK        TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC6216_SCR6216  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR6217  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR6217 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR6217  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR6218  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR6218 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR6218  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR6218  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR6218 OUTPUT.

  DESCRIBE TABLE IT_ZSIDRHSD_S LINES LINE.
  TC_6218-LINES = LINE + 10.                   " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR6218  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC6218_SCR6218  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC6218_SCR6218 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSIDRHSD_S INDEX TC_6218-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDRHSD_S TO ZSIDRHSD.    " DATA MOVE
    MOVE: IT_ZSIDRHSD_S-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC6218_SCR6218  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR6219  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR6219 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR6219  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR6219  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR6219 OUTPUT.

  G_PARAM_LINE = TC_6219-TOP_LINE.
  TC_6219-LINES = G_PARAM_LINE + 9.                 " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR6219  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6219_MARK_SCR6219  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_6219_MARK_SCR6219 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_6219-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_6219_MARK_SCR6219  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC6219_SCR6219  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC6219_SCR6219 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSIDRHSL_S INDEX TC_6219-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDRHSL_S TO ZSIDRHSL.    " DATA MOVE
    MOVE: IT_ZSIDRHSL_S-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC6219_SCR6219  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR6212  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR6212 OUTPUT.

  DESCRIBE TABLE IT_ZSIDRHSD LINES LINE.
  TC_6212-LINES = LINE.                             " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR6212  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6212_MARK_SCR6212  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_6212_MARK_SCR6212 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_6212-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_6212_MARK_SCR6212  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC6212_SCR6212  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC6212_SCR6212 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSIDRHSD INDEX TC_6212-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDRHSD TO ZSIDRHSD.    " DATA MOVE
    MOVE: IT_ZSIDRHSD-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC6212_SCR6212  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6410_MARK_SCR6410  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_6410_MARK_SCR6410 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_6410-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_6410_MARK_SCR6410  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR6410  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR6410 OUTPUT.

*  DESCRIBE TABLE IT_ZSIDRCR LINES LINE.
  LINE = TC_3111-TOP_LINE.
  TC_6410-LINES = LINE + 10.

ENDMODULE.                 " TOTAL_LINE_GET_SCR6410  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC6410_SCR6410  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC6410_SCR6410 OUTPUT.
  W_LOOPLINES = SY-LOOPC.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIDRCR  INDEX TC_6410-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDRCR  TO ZSIDRCR.     " DATA MOVE
    MOVE: IT_ZSIDRCR-ZFMARK        TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC6410_SCR6410  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR6412  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR6412 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR6412  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR6412  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR6412 OUTPUT.

  DESCRIBE TABLE IT_ZSIDRCRIT_S LINES LINE.
  TC_6412-LINES = LINE + 10.          " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR6412  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_6412_MARK_SCR6412  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_6412_MARK_SCR6412 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_6412-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_6412_MARK_SCR6412  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC6412_SCR6412  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC6412_SCR6412 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIDRCRIT_S INDEX TC_6412-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDRCRIT_S TO ZSIDRCRIT.    " DATA MOVE
    MOVE: IT_ZSIDRCRIT_S-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC6412_SCR6412  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR6610  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR6610 OUTPUT.

  G_PARAM_LINE = TC_6610-TOP_LINE.
  TC_6610-LINES = G_PARAM_LINE + 18.              " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR6610  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR8410  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR8410 OUTPUT.

  G_PARAM_LINE = TC_8410-TOP_LINE.
  TC_8410-LINES = G_PARAM_LINE + 10.                " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR8410  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_8410_MARK_SCR8410  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_8410_MARK_SCR8410 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_8410-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_8410_MARK_SCR8410  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC8410_SCR8410  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC8410_SCR8410 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_8410-CURRENT_LINE GT TC_8410-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSVTIVIT   INDEX TC_8410-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSVTIVIT   TO ZSVTIVIT.     " DATA MOVE
    MOVE: IT_ZSVTIVIT-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC8410_SCR8410  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR8512  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR8512 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR8512  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR8514  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR8514 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR8514  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR8514  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR8514 OUTPUT.

  DESCRIBE TABLE IT_ZSVTSG3 LINES LINE.
  TC_8514-LINES = LINE.                             " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR8514  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_8514_MARK_SCR8514  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_8514_MARK_SCR8514 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
     AND TC_8514-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_8514_MARK_SCR8514  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC8514_SCR8514  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC8514_SCR8514 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSVTSG3   INDEX TC_8514-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSVTSG3   TO ZSVTSG3.      " DATA MOVE
    MOVE: IT_ZSVTSG3-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC8514_SCR8514  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR8710  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR8710 OUTPUT.

  DESCRIBE TABLE IT_ZSREDSG1 LINES LINE.
  TC_8710-LINES = LINE.                             " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR8710  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC8710_SCR8710  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC8710_SCR8710 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSREDSG1 INDEX TC_8710-CURRENT_LINE.
  IF SY-SUBRC = 0.                                  " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSREDSG1 TO ZSREDSG1.    " DATA MOVE
    MOVE: IT_ZSREDSG1-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC8710_SCR8710  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR8712  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR8712 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR8712  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_7110_ENABLE_SET_SCR7110  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_7110_ENABLE_SET_SCR7110 OUTPUT.

  PERFORM P2000_SCR_MODE_SET.

ENDMODULE.                 " TC_7110_ENABLE_SET_SCR7110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_3110_ENABLE_SET_SCR3110  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_3110_ENABLE_SET_SCR3110 OUTPUT.

  PERFORM P2000_SCR_MODE_SET.
  EXIT.

ENDMODULE.                 " TC_3110_ENABLE_SET_SCR3110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCR3100  OUTPUT
*&---------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCR3100 OUTPUT.

  IF ZTIV-ZFPOYN = 'N'.
    CASE W_STATUS.
      WHEN C_REQ_C OR C_REQ_U.              " 생성, 변?
        LOOP AT SCREEN.
          IF SCREEN-NAME(12) EQ 'ZTIV-ZFIVAMC'.
            SCREEN-INPUT   = '1'.         " 무환의 경우 통?
          ENDIF.
          IF SCREEN-NAME(12) EQ 'ZTIV-ZFPKCHG'.
            SCREEN-INPUT   = '0'.
          ENDIF.
          IF SCREEN-NAME(12) EQ 'ZTIV-ZFHDCHG'.
            SCREEN-INPUT   = '0'.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " OPERATION_MODE_SET_SCR3100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_RADIO_SCR3100  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_RADIO_SCR3100 OUTPUT.
*-----------------------------------------------------------------------
* CALL Transaction일 경우, EXIT
  IF SY-CALLD EQ 'X'.
    EXIT.
  ENDIF.
*-----------------------------------------------------------------------

  CLEAR : RADIO_HBL, RADIO_BLN.
* RADIO_HBL = 'X'.

  CLEAR : RADIO_PO, RADIO_LC, RADIO_RQ, RADIO_NPO.
* RADIO_PO = 'X'.

ENDMODULE.                 " SET_RADIO_SCR3100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR7410  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR7410 OUTPUT.

  IF TABSTRIP-ACTIVETAB IS INITIAL.
    TABSTRIP-ACTIVETAB = 'COMM1'.
    DYNPRO = '7412'.
  ENDIF.
  IF OK-CODE = 'COMM1'.
    TABSTRIP-ACTIVETAB = 'COMM1'.
    DYNPRO = '7412'.
  ENDIF.
  IF OK-CODE = 'COMM2'.
    TABSTRIP-ACTIVETAB = 'COMM2'.
    DYNPRO = '7414'.
  ENDIF.
  IF OK-CODE = 'IDSHS'.
    TABSTRIP-ACTIVETAB = 'IDSHS'.
    DYNPRO = '7416'.
  ENDIF.
  IF OK-CODE = 'CST'.
    TABSTRIP-ACTIVETAB = 'CST'.
    IF ZTIMIMG00-ZFPSMS EQ '1'.
      DYNPRO = '7420'.
    ELSE.
    ENDIF.
  ENDIF.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR7410  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR7412  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR7412 OUTPUT.

  DESCRIBE TABLE IT_ZSIDSHSD LINES LINE.
  TC_7412-LINES = LINE.                             " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR7412  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC7412_SCR7412  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC7412_SCR7412 OUTPUT.

  W_LOOPLINES = SY-LOOPC.
* LINE의 유효성 여부 검증.2000-10-23 강석봉 추가 작업.
  IF TC_7412-CURRENT_LINE GT TC_7412-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.

  READ TABLE IT_ZSIDSHSD INDEX TC_7412-CURRENT_LINE.
  IF SY-SUBRC = 0.                                  " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDSHSD TO ZSIDSHSD.    " DATA MOVE
    MOVE: IT_ZSIDSHSD-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC7412_SCR7412  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR7416  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR7416 OUTPUT.

  DESCRIBE TABLE IT_ZSIDSHS LINES LINE.
  TC_7416-LINES = LINE.                             " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR7416  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR7417  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR7417 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR7417  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR7418  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR7418 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR7418  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR7418  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR7418 OUTPUT.

  DESCRIBE TABLE IT_ZSIDSHSD_S LINES LINE.
  TC_7418-LINES = LINE.                             " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR7418  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC7418_SCR7418  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC7418_SCR7418 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSIDSHSD_S INDEX TC_7418-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDSHSD_S TO ZSIDSHSD.    " DATA MOVE
    MOVE: IT_ZSIDSHSD_S-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC7418_SCR7418  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR7419  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR7419 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR7419  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR7419  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR7419 OUTPUT.

  G_PARAM_LINE = TC_7419-TOP_LINE.
  TC_7419-LINES = G_PARAM_LINE + 9.                 " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR7419  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC7419_SCR7419  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC7419_SCR7419 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSIDSHSL_S INDEX TC_7419-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDSHSL_S TO ZSIDSHSL.    " DATA MOVE
    MOVE: IT_ZSIDSHSL_S-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC7419_SCR7419  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR7420  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR7420 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR7420  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE_SCR7110  OUTPUT
*&---------------------------------------------------------------------*
MODULE INITIAL_VALUE_SCR7110 OUTPUT.

  MOVE 'PC'     TO  ZTBLUG-ZFPKCNM.
  MOVE 'KG'     TO  ZTBLUG-ZFTOWTM.

ENDMODULE.                 " INITIAL_VALUE_SCR7110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR7420  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR7420 OUTPUT.

  G_PARAM_LINE = TC_7420-TOP_LINE.
  TC_7420-LINES = G_PARAM_LINE + 5.                 " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR7420  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC7420_SCR7420  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC7420_SCR7420 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSCUCLCST INDEX TC_7420-CURRENT_LINE.
  IF SY-SUBRC = 0.                                  " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSCUCLCST TO ZSCUCLCST.    " DATA MOVE
    MOVE: IT_ZSCUCLCST-ZFMARK       TO W_ROW_MARK.  " MARK SET
  ENDIF.
  MOVE  'KRW'    TO   ZSCUCLCST-ZFKRW.

ENDMODULE.                 " IT_TO_TC7420_SCR7420  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC6610_SCR6610  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC6610_SCR6610 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSIDRDTU   INDEX TC_6610-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDRDTU  TO ZSIDRDTU.   " DATA MOVE
    MOVE IT_ZSIDRDTU-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC6610_SCR6610  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DEFAULT_VALUE_SCR6210  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_DEFAULT_VALUE_SCR6210 OUTPUT.

*  2000.07.03 김연?
*  IF ZTIDR-ZFINRC IS INITIAL.
*     MOVE '012' TO ZTIDR-ZFINRC.
*  ENDIF.
*  IF ZTIDR-ZFINRCD IS INITIAL.
*     MOVE '10' TO ZTIDR-ZFINRCD.
*  ENDIF.

ENDMODULE.                 " SET_DEFAULT_VALUE_SCR6210  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR8212  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR8212 OUTPUT.

  DESCRIBE TABLE IT_ZSPMTIV LINES LINE.
  TC_8212-LINES = LINE.

ENDMODULE.                 " TOTAL_LINE_GET_SCR8212  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC8212_SCR8212  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC8212_SCR8212 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSPMTIV   INDEX TC_8212-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSPMTIV  TO ZSPMTIV.          " DATA MOVE
    MOVE IT_ZSPMTIV-ZFMARK         TO W_ROW_MARK.      " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC8212_SCR8212  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_SUB_SCREEN_SCR8210  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SUB_SCREEN_SCR8210 OUTPUT.

  IF TABSTRIP-ACTIVETAB IS INITIAL.
    TABSTRIP-ACTIVETAB = 'COMM1'.
    DYNPRO = '8212'.
  ENDIF.
  IF OK-CODE = 'COMM1'.
    TABSTRIP-ACTIVETAB = 'COMM1'.
    DYNPRO = '8212'.
  ENDIF.
  IF OK-CODE = 'COMM2'.
    TABSTRIP-ACTIVETAB = 'COMM2'.
    DYNPRO = '8214'.
  ENDIF.
  PROGRAM = SY-CPROG.
  DYNSUB = DYNPRO.

ENDMODULE.                 " SET_SUB_SCREEN_SCR8210  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR8220  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR8220 OUTPUT.

  DESCRIBE TABLE IT_ZSPMTIV LINES LINE.
  TC_8220-LINES = LINE.

ENDMODULE.                 " TOTAL_LINE_GET_SCR8220  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC8220_SCR8220  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC8220_SCR8220 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSPMTIV   INDEX TC_8220-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSPMTIV  TO ZSPMTIV.          " DATA MOVE
    MOVE IT_ZSPMTIV-ZFMARK         TO W_ROW_MARK.      " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC8220_SCR8220  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR66101  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR66101 OUTPUT.

  DESCRIBE TABLE IT_ZSIDRCRIT LINES LINE.
  TC_66101-LINES = LINE.

ENDMODULE.                 " TOTAL_LINE_GET_SCR66101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC66101_SCR6610  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC66101_SCR6610 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSIDRCRIT   INDEX TC_66101-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIDRCRIT  TO ZSIDRCRIT.   " DATA MOVE
  ENDIF.

ENDMODULE.                 " IT_TO_TC66101_SCR6610  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INI_VALUE_FOR_SCR0105  OUTPUT
*&---------------------------------------------------------------------*
MODULE INI_VALUE_FOR_SCR0105 OUTPUT.

  SELECT SINGLE *
    FROM ZTBLUG
  WHERE ZFHBLNO = ZTBL-ZFHBLNO.
  IF SY-SUBRC = 0.
    MOVE '긴급보운 Yes' TO W_TXT1.
  ELSE.
    CLEAR W_TXT1.
  ENDIF.

ENDMODULE.                 " INI_VALUE_FOR_SCR0105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR9911  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR9911 OUTPUT.

  DESCRIBE TABLE IT_ZSMSHD LINES G_PARAM_LINE.      " LINE 수 GET
  TC_9911-LINES = G_PARAM_LINE.                     " LINE 수 정?
ENDMODULE.                 " TOTAL_LINE_GET_SCR9911  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR9911  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR9911 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_9911-CURRENT_LINE GT TC_9911-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSMSHD INDEX TC_9911-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING  IT_ZSMSHD  TO  ZSMSHD.
    MOVE : IT_ZSMSHD-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR9911  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR9912_1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR9912_1 OUTPUT.
  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSMSIT LINES G_PARAM_LINE.      " LINE 수 GET
    TC_9912_1-LINES = G_PARAM_LINE.                    " LINE 수 정?
  ELSE.
    TC_9912_1-LINES = TC_9912_1-TOP_LINE + 2.         " LINE 수 정?
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR9912_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR9912_2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR9912_2 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSMSCST LINES G_PARAM_LINE.      " LINE 수 GET
    TC_9912_2-LINES = G_PARAM_LINE.                    " LINE 수 정?
  ELSE.
    TC_9912_2-LINES = TC_9912_2-TOP_LINE + 3.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR9912_2  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR9912_1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR9912_1 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_9912_1-CURRENT_LINE GT TC_9912_1-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSMSIT INDEX TC_9912_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING  IT_ZSMSIT  TO  ZSMSIT.
    MOVE : IT_ZSMSIT-ZFMARK        TO  W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR9912_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR9912_2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FILL_TC_SCR9912_2 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_9912_2-CURRENT_LINE GT TC_9912_2-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSMSCST INDEX TC_9912_2-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS
    MOVE-CORRESPONDING  IT_ZSMSCST  TO  ZSMSCST.
    MOVE : IT_ZSMSCST-ZFMARK        TO  W_ROW_MARK1.  " MARK SET
  ENDIF.
  MOVE 'KRW' TO ZSMSCST-ZFKRW.

ENDMODULE.                 " FILL_TC_SCR9912_2  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR8210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR8210 OUTPUT.

  DESCRIBE TABLE IT_ZSPMTIV LINES LINE.
  TC_8210-LINES = LINE.

ENDMODULE.                 " TOTAL_LINE_GET_SCR8210  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC8210_SCR8210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC8210_SCR8210 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
  READ TABLE IT_ZSPMTIV   INDEX TC_8210-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSPMTIV  TO ZSPMTIV.          " DATA MOVE
    MOVE IT_ZSPMTIV-ZFMARK         TO W_ROW_MARK.      " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC8210_SCR8210  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR3115  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR3115 OUTPUT.

  DESCRIBE TABLE IT_ZSIVHST  LINES LINE.
  TC_3115-LINES = LINE.

ENDMODULE.                 " TOTAL_LINE_GET_SCR3115  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3115_SCR3115  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC3115_SCR3115 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_3115-CURRENT_LINE GT TC_3115-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.

  READ TABLE IT_ZSIVHST   INDEX TC_3115-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVHST  TO ZSIVHST.          " DATA MOVE
    MOVE IT_ZSIVHST-ZFMARK         TO W_ROW_MARK.      " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC3115_SCR3115  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IMG_CHECK  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IMG_CHECK OUTPUT.
  SELECT  SINGLE *  FROM  ZTIMIMG00.
  IF ZTIMIMG00-ZFIMPATH EQ '3'.
    MESSAGE  S564. LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.                 " IMG_CHECK  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUPPRESS_DIALOG_SCR0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUPPRESS_DIALOG_SCR0050 OUTPUT.
  SUPPRESS DIALOG.
ENDMODULE.                 " SET_SUPPRESS_DIALOG_SCR0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR8510  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR8510 OUTPUT.

  DESCRIBE TABLE IT_ZSVTSG3 LINES LINE.
  TC_8510-LINES = LINE.                             " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR8510  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC8510_SCR8510  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC8510_SCR8510 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSVTSG3   INDEX TC_8510-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSVTSG3   TO ZSVTSG3.      " DATA MOVE
    MOVE: IT_ZSVTSG3-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC8510_SCR8510  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_IMG_FILED_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_IMG_FILED_SCRCOM OUTPUT.

  CASE SY-DYNNR.
    WHEN 0810.       ASSIGN   W_0810_FIRST   TO   <FS_F>.
    WHEN 0820.       ASSIGN   W_0820_FIRST   TO   <FS_F>.
    WHEN 0830.       ASSIGN   W_0830_FIRST   TO   <FS_F>.
    WHEN 9910.       ASSIGN   W_9910_FIRST   TO   <FS_F>.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  IF <FS_F> EQ 'Y'.
    SELECT SINGLE * FROM ZTIMIMG00.
    IF SY-SUBRC EQ 0.
      IF SY-DYNNR EQ 9910.       ">모선관리.
        IF ZTIMIMG00-ZFCGYN NE 'X'.
          MESSAGE S623.
          LEAVE PROGRAM.
        ENDIF.
      ELSE.
        IF ZTIMIMG00-ZFCGYN NE 'X'.
          MESSAGE S622.
          LEAVE PROGRAM.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE S963.
      LEAVE PROGRAM.
    ENDIF.
    <FS_F> = 'N'.
  ENDIF.


ENDMODULE.                 " CHECK_IMG_FILED_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_INIT_DATA_SCR3101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_INIT_DATA_SCR3101 OUTPUT.

  IF W_3101_FIRST = 'N'.
    CLEAR : ZSIV, ZSREQHD.
    MOVE : 'Y'          TO      ZSIV-ZFPOYN,
           'X'          TO      ZSIV-ZFCSTYN,
           'Y'          TO      W_3100_FIRST,
           'C'          TO      ZSIV-ZFCLCD.
    SELECT SINGLE * FROM ZTIMIMG00.
    IF SY-SUBRC NE 0.
      MESSAGE S963.
      LEAVE TO SCREEN 0.
    ENDIF.
    IF ZTIMIMG00-ZFCSTMD IS INITIAL.
      MESSAGE S986.
      LEAVE TO SCREEN 0.
    ELSE.
      IF ZTIMIMG00-ZFCSTMD EQ 'S'.
        CLEAR : ZSIV-ZFCSTYN.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SET_INIT_DATA_SCR3101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_INIT_VALUE_SCR3501  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_INIT_VALUE_SCR3501 OUTPUT.

  IF W_3501_FIRST EQ 'N'.
    CLEAR : ZSCIVHD-ZFCIVNO, ZSCIVHD-ZFTRIPLE.
    ZSCIVHD-ZFPOYN  = 'Y'.
    ZSCIVHD-ZFPRPYN = 'N'.
    W_3501_FIRST = 'Y'.
  ENDIF.

ENDMODULE.                 " SET_INIT_VALUE_SCR3501  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0021  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0021 OUTPUT.

  DESCRIBE TABLE IT_ZSIVHSTIT LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0021-LINES = G_PARAM_LINE.                   " LINE 수 정의.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0021  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0021_SCR0021  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0021_SCR0021 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0021-CURRENT_LINE GT TC_0021-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIVHSTIT   INDEX TC_0021-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVHSTIT   TO ZSIVHSTIT. " DATA MOVE
    MOVE: IT_ZSIVHSTIT-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0021_SCR0021  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0031_SCR0031  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0031_SCR0031 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0031-CURRENT_LINE GT TC_0031-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIVHST   INDEX TC_0031-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVHST   TO ZSIVHST. " DATA MOVE
    MOVE ZTIV-BUKRS                 TO ZTIV-BUKRS.
    MOVE IT_ZSIVHST-ZFMARK          TO W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0031_SCR0031  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BASIC_SETTING  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_BASIC_SETTING OUTPUT.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE S963.
    LEAVE TO SCREEN 0.
  ENDIF.

  IF SY-TCODE EQ 'ZIM221' OR SY-TCODE EQ 'ZIM222'.
    IF W_FIRST_SCR0200 EQ 'Y'.
      IF ZTIMIMG00-BLSTYN NE 'X'.
        MESSAGE S573.
        LEAVE TO SCREEN 0.
      ENDIF.
      W_FIRST_SCR0200 = 'N'.
    ENDIF.
  ELSEIF SY-TCODE EQ 'ZIM223'.
    IF W_FIRST_SCR0200 EQ 'Y'.
      IF ZTIMIMG00-BLCSTMD NE 'X'.
        MESSAGE S573.
        LEAVE TO SCREEN 0.
      ENDIF.
      W_FIRST_SCR0200 = 'N'.
    ENDIF.
  ELSEIF SY-TCODE EQ 'ZIM21' AND SY-CALLD EQ 'X'.
    GET PARAMETER ID 'ZPHBLNO' FIELD ZSREQHD-ZFHBLNO.
    GET PARAMETER ID 'ZPREQNO' FIELD ZSREQHD-ZFREQNO.

    CLEAR : RADIO_PO, RADIO_LC, RADIO_RQ, RADIO_NP.
    MOVE : 'X'  TO  RADIO_RQ.
  ENDIF.

ENDMODULE.                 " CHECK_BASIC_SETTING  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0112  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0112 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBLIT  LINES G_PARAM_LINE.   " LINE  GET
    TC_0112-LINES = G_PARAM_LINE.                   " LINE  Define.
  ELSE.
    G_PARAM_LINE = TC_0112-TOP_LINE.
    TC_0112-LINES = G_PARAM_LINE + 15.              " LINE  Define.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0112  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0112_SCR0112  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0112_SCR0112 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE Varify Check
  IF TC_0112-CURRENT_LINE GT TC_0112-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line )
  READ TABLE IT_ZSBLIT   INDEX TC_0112-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBLIT   TO ZSBLIT.        " DATA MOVE
    MOVE: IT_ZSBLIT-ZFMARK         TO W_ROW_MARK.    " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0112_SCR0112  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0112  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0112 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0112-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0112  INPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0113  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0113 OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBLIT  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0113-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_0113-TOP_LINE.
    TC_0113-LINES = G_PARAM_LINE + 14.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0113  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0113_SCR0113  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0113_SCR0113 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0113-CURRENT_LINE GT TC_0113-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBLIT   INDEX TC_0113-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBLIT   TO ZSBLIT.       " DATA MOVE
    MOVE: IT_ZSBLIT-ZFMARK         TO W_ROW_MARK.   " MARK SET
  ELSE.
    CLEAR : W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0113_SCR0113  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0060  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0060 OUTPUT.

  DESCRIBE TABLE IT_ZSBSEG LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0060-LINES = G_PARAM_LINE.                   " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0060  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0070 OUTPUT.

  DESCRIBE TABLE IT_ZSBSEG_TMP LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0070-LINES = G_PARAM_LINE.                   " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0060_SCR0060  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0060_SCR0060 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0060-CURRENT_LINE GT TC_0060-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSBSEG   INDEX TC_0060-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBSEG   TO ZSBSEG.     " DATA MOVE
    MOVE: IT_ZSBSEG-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0060_SCR0060  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0070_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0070_SCR0070 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0070-CURRENT_LINE GT TC_0070-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* 품목 Internal Table Read ( Line별 )
  READ TABLE IT_ZSBSEG_TMP   INDEX TC_0070-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSBSEG_TMP   TO ZSBSEG.     " DATA MOVE
    MOVE: IT_ZSBSEG_TMP-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0060_SCR0060  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SUPPRESS_DIALOG_SCR8213  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SUPPRESS_DIALOG_SCR8213 OUTPUT.

  SUPPRESS DIALOG.

ENDMODULE.                 " SET_SUPPRESS_DIALOG_SCR8213  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0070 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

  SET CURSOR FIELD 'ZTBKPF-BLDAT'.

*   IF SY-DYNNR NE '3118'.
*      IF OPTION = '1'.
*         SET CURSOR FIELD 'SPOP-OPTION1'.
*      ELSE.
*         SET CURSOR FIELD 'SPOP-OPTION2'.
*      ENDIF.
*   ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_COLUMN_TREE_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_COLUMN_TREE_SCR0070 OUTPUT.

  IF G_TREE IS INITIAL.
* The Tree Control has not been created yet.
* Create a Tree Control and insert nodes into it.
    PERFORM P2000_CREATE_BLCOST_TREE.
  ENDIF.

ENDMODULE.                 " SET_COLUMN_TREE_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0071  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0071 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0071  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_COLUMN_TREE_SCR0071  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_COLUMN_TREE_SCR0071 OUTPUT.

  IF G_TREE IS INITIAL.
    PERFORM P2000_CREATE_POST_MESSAGE_TREE.
  ENDIF.


ENDMODULE.                 " SET_COLUMN_TREE_SCR0071  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCR0070 OUTPUT.

  SELECT SINGLE * FROM ZTIMIMG00.

  LOOP AT SCREEN.
    IF ZTIMIMG00-ZFAVPT NE 'X'.
       IF SCREEN-NAME EQ 'ZSBKPF-HKONT' OR
          SCREEN-NAME EQ 'ZSBKPF-ZFADVPT'.
          SCREEN-INVISIBLE = '1'.
          SCREEN-INPUT     = '0'.
       ENDIF.
       IF ZTIMIMG00-BLCALYN EQ 'X'.
          IF SCREEN-NAME EQ 'ZSBKPF-WMWST'.
             SCREEN-INPUT   = '1'.
          ENDIF.
       ENDIF.
    ENDIF.
    IF IT_ZSBKPF-ZFCSTGRP EQ '005'.
       IF SCREEN-NAME EQ 'ZSBKPF-WMWST'.
          SCREEN-INPUT = '0'.
       ENDIF.
    ENDIF.
    IF ZTIMIMG00-ZFBPLK IS INITIAL.
       IF SCREEN-NAME EQ 'ZSBKPF-BUPLA'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
       ENDIF.
    ENDIF.
    IF ZTIMIMG00-ZFBALK IS INITIAL.
       IF SCREEN-NAME EQ 'ZSBKPF-GSBER'.
          SCREEN-INPUT = '0'.
          SCREEN-INVISIBLE = '1'.
       ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


ENDMODULE.                 " OPERATION_MODE_SET_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_OPEN_BANK_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_OPEN_BANK_SCR0070 INPUT.

* IF ZSBKPF-LIFNR IS INITIAL.
*     MESSAGE W977(ZIM) WITH 'Input Vendor Code!'.
*     EXIT.
*  ENDIF.

  REFRESH : IT_OPBN_HELP.
  SELECT SINGLE * FROM LFA1
  WHERE  LIFNR    EQ   ZSBKPF-LIFNR.
  IF LFA1-LNRZA   IS   INITIAL.
     SELECT * FROM LFZA WHERE LIFNR EQ ZSBKPF-LIFNR.
        CLEAR : LFA1.
        SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LFZA-EMPFK.
        MOVE : LFZA-EMPFK      TO   IT_OPBN_HELP-LIFNR,
               LFA1-NAME1      TO   IT_OPBN_HELP-NAME1,
               LFA1-ORT01      TO   IT_OPBN_HELP-ORT01.
        APPEND IT_OPBN_HELP.
     ENDSELECT.
  ELSE.
     SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ  LFA1-LNRZA.
     MOVE : LFA1-LIFNR      TO   IT_OPBN_HELP-LIFNR,
            LFA1-NAME1      TO   IT_OPBN_HELP-NAME1,
            LFA1-ORT01      TO   IT_OPBN_HELP-ORT01.
     APPEND IT_OPBN_HELP.
  ENDIF.

  DESCRIBE  TABLE  IT_OPBN_HELP  LINES  W_LINE.
  IF W_LINE EQ 0.
    MESSAGE S406.  EXIT.
  ENDIF.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.

  WINDOW_TITLE = 'Alternative Payee'.
  CONCATENATE WINDOW_TITLE 'Help' INTO WINDOW_TITLE
              SEPARATED BY SPACE.

*  IF W_STATUS EQ C_REQ_D.
*     W_DISPLAY = 'X'.
*  ELSE.
*     CLEAR: W_DISPLAY.
*  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
               RETFIELD        = 'LIFNR'
               DYNPPROG        = DYNPROG
               DYNPNR          = DYNNR
               DYNPROFIELD     = 'ZSBKPF-ZFOPBN'
               WINDOW_TITLE    = WINDOW_TITLE
               VALUE_ORG       = 'S'
*               DISPLAY         = 'X'
       TABLES
               VALUE_TAB       = IT_OPBN_HELP
       EXCEPTIONS
               PARAMETER_ERROR = 1
               NO_VALUES_FOUND = 2
               OTHERS          = 3.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDMODULE.                 " HELP_OPEN_BANK_SCR0070  INPUT
