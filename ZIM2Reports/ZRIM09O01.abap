*----------------------------------------------------------------------*
*   INCLUDE ZRIM09O01                                                  *
*----------------------------------------------------------------------*
*&  프로그램명 : 보세창고 출고 관련 PBO MODULE Include                 *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.20                                            *
*&  적용회사PJT: LG 화학                                               *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_SCRCOM  OUTPUT
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
*   PERFORM    P2000_SET_STATUS_TCODE_DISABLE.

*-----------------------------------------------------------------------
* 화면별, 업무별 GUI TITLE SETTING( SCREEN 별 )
*-----------------------------------------------------------------------
   PERFORM    P2000_SET_STATUS_SCR_DISABLE.

*-----------------------------------------------------------------------
* PF-STATUS SETTING
*-----------------------------------------------------------------------
   SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.

ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

   MOVE OK-CODE TO W_OK_CODE.
   CLEAR : OK-CODE.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
MODULE D0014_STATUS_SCR0014 OUTPUT.

  IF W_STATUS_CHK = 'D'.
     SET PF-STATUS 'STDLISA'.
  ELSE.
     SET PF-STATUS 'STDLISW'.
  ENDIF.

  INCLUDE = 'CCHBL'.                 ">통관요청 조회.
  CASE INCLUDE.
     WHEN 'CCHBL' OR 'CCBL'.
        SET TITLEBAR 'POPU' WITH
                      'Clearance request duplication List by B/L'.
     WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0101 OUTPUT.

  DESCRIBE TABLE IT_ZSBWIT      LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0101-LINES = G_PARAM_LINE.                       " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCRCOM OUTPUT.

  PERFORM P2000_SCR_MODE_SET.

ENDMODULE.                 " OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0101_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0101_SCR0101 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSBWIT   INDEX TC_0101-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING IT_ZSBWIT   TO ZSBWIT.      " DATA MOVE
     MOVE: IT_ZSBWIT-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0101_SCR0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU'.

   IF OPTION = '1'.
      SET CURSOR FIELD 'SPOP-OPTION1'.
   ELSE.
      SET CURSOR FIELD 'SPOP-OPTION2'.
   ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.

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

ENDMODULE.                 " MODIFY_SCREEN_SCR0001  OUTPUT
