*&---------------------------------------------------------------------*
*& INCLUDE MZIM04O01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : L/C 원장관리 PBO MODULE Include                       *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.25                                            *
*&  적용회사PJT: Poong-San                                             *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_SCRCOM  OUTPUT
*&   DESC : GUI STATUS 및 GUI TITLE BAR를 SET하는 공통 MODULE
*&---------------------------------------------------------------------*
MODULE PF_STATUS_SCRCOM OUTPUT.
  REFRESH : IT_EXCL.              " Inactive Function용 Internal Table

*-----------------------------------------------------------------------
* GUI TITLE TEXT SETTING
*-----------------------------------------------------------------------
  PERFORM   P2000_SET_GUI_TEXT.

*-----------------------------------------------------------------------
* PF-STATUS Setting
*-----------------------------------------------------------------------
  PERFORM    P2000_SET_PF_STATUS.

*-----------------------------------------------------------------------
* PF-STATUS상의 Inactive Function Setting ( TRANSACTION 별 )
*-----------------------------------------------------------------------
  PERFORM    P2000_SET_STATUS_TCODE_DISABLE.

*-----------------------------------------------------------------------
* 화면별, 업무별 GUI TITLE SETTING( SCREEN 별 )
*-----------------------------------------------------------------------
  PERFORM    P2000_SET_STATUS_SCR_DISABLE.

  IF SY-DYNNR NE '0100'.
    MOVE 'COPY' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 화폐변경.
  ENDIF.

  CASE SY-DYNNR.
    WHEN '0100'.
    WHEN '0101'.
  ENDCASE.

  SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.

ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCRCOM OUTPUT.

  PERFORM P2000_SCR_MODE_SET.

ENDMODULE.                 " OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR101A  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR101A OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBSEG  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_101A-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_101A-TOP_LINE.
    TC_101A-LINES = G_PARAM_LINE + 5.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR101A  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.
  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

  IF SY-DYNNR NE '3118'.
    IF OPTION = '1'.
      SET CURSOR FIELD 'SPOP-OPTION1'.
    ELSE.
      SET CURSOR FIELD 'SPOP-OPTION2'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZTPMTHD-HKONT'.
      SCREEN-INPUT     = '0'.
      SCREEN-INVISIBLE = '1'.
    ENDIF.
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

ENDMODULE.                 " MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_101A_MARK_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_101A_MARK_SCR0101 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
    AND TC_101A-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " TC_101A_MARK_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC101_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC101_SCR0101 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_101A-CURRENT_LINE GT TC_101A-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSBSEG      INDEX TC_101A-CURRENT_LINE.

  IF SY-SUBRC = 0.                                    " Read Success.
    SELECT SINGLE *
             FROM ZTBKPF
            WHERE BUKRS = IT_ZSBSEG-BUKRS
              AND BELNR = IT_ZSBSEG-BELNR
              AND GJAHR = IT_ZSBSEG-GJAHR.

    MOVE-CORRESPONDING IT_ZSBSEG TO ZSBSEG.      " Data Move.
    READ TABLE IT_ZSIMIMG08
            WITH KEY  ZFCDTY   =   '003'
                      ZFCD     =   IT_ZSBSEG-ZFCD.
    MOVE: IT_ZSIMIMG08-ZFCDNM    TO ZSBSEG-ZFCDNM.

    PERFORM P2000_AMT_MAKE USING IT_ZSBSEG-WRBTR
                                 ZTBKPF-WAERS
                           CHANGING ZSBSEG-SGTXT.
    MOVE: IT_ZSBSEG-ZFMARK       TO W_ROW_MARK.  " Mark Set.
  ENDIF.

ENDMODULE.                 " IT_TO_TC101_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_101B_MARK_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_101B_MARK_SCR0101 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
    AND TC_101B-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK1 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK1.
  ENDIF.

ENDMODULE.                 " TC_101B_MARK_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC101B_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC101B_SCR0101 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_101B-CURRENT_LINE GT TC_101B-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSBSEG1      INDEX TC_101B-CURRENT_LINE.
  IF SY-SUBRC = 0.                                    " Read Success.
    SELECT SINGLE *
             FROM ZTBKPF
            WHERE BUKRS = IT_ZSBSEG1-BUKRS
              AND BELNR = IT_ZSBSEG1-BELNR
              AND GJAHR = IT_ZSBSEG1-GJAHR.

    MOVE-CORRESPONDING IT_ZSBSEG1      TO ZSBSEG2.      " Data Move.
    IF IT_ZSBSEG1-CPUDT IS INITIAL.
      MOVE ZTBKPF-UDAT TO ZSBSEG2-CPUDT.
    ENDIF.
    PERFORM P2000_AMT_MAKE    USING IT_ZSBSEG1-WRBTR
                                    ZTBKPF-WAERS
                           CHANGING ZSBSEG2-SGTXT.

    MOVE: IT_ZSBSEG-ZFMARK             TO W_ROW_MARK1.  " Mark Set.
  ENDIF.

ENDMODULE.                 " IT_TO_TC101B_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR101B  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR101B OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSBSEG1  LINES G_PARAM_LINE.   " LINE 수 GET
    TC_101B-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_101B-TOP_LINE.
    TC_101B-LINES = G_PARAM_LINE + 5.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR101B  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_101C_MARK_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_101C_MARK_SCR0101 OUTPUT.

  IF OK-CODE = 'MKA1'                  " mark all
    AND TC_101C-CURRENT_LINE <= G_PARAM_LINE.
    W_ROW_MARK2 = 'X'.
  ENDIF.
  IF OK-CODE = 'MKL1'.                " delete all marks
    CLEAR W_ROW_MARK2.
  ENDIF.

ENDMODULE.                 " TC_101C_MARK_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC101C_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC101C_SCR0101 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_101C-CURRENT_LINE GT TC_101C-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_ZSPMTHD      INDEX TC_101C-CURRENT_LINE.
  IF SY-SUBRC = 0.                                    " Read Success.
    MOVE-CORRESPONDING IT_ZSPMTHD      TO ZSPMTHD.      " Data Move.
    MOVE: IT_ZSPMTHD-ZFMARK             TO W_ROW_MARK2.  " Mark Set.
  ENDIF.

ENDMODULE.                 " IT_TO_TC101C_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR101C  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR101C OUTPUT.

  IF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSPMTHD  LINES G_PARAM_LINE.  " LINE 수 GET
    TC_101C-LINES = G_PARAM_LINE.                   " LINE 수 정의.
  ELSE.
    G_PARAM_LINE = TC_101C-TOP_LINE.
    TC_101C-LINES = G_PARAM_LINE + 5.              " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR101C  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

  MOVE OK-CODE TO W_OK_CODE.
  OK-CODE = ''.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
