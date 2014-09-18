*&---------------------------------------------------------------------*
*& INCLUDE ZRIM08O01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 기납증   Main PBO MODULE Include                      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

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
*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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

*>> 전기 기능 막음.
   IF ZTTAXBKHD-ZFPOSYN EQ 'Y'.
      MOVE 'POST' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 전기.
   ELSE.
      MOVE 'DCDE' TO IT_EXCL-FCODE.     APPEND IT_EXCL. " 전기취소.
      MOVE 'FB03' TO IT_EXCL-FCODE.     APPEND IT_EXCL. " 전기조회..
      MOVE 'ZIMY3' TO IT_EXCL-FCODE.    APPEND IT_EXCL. " 전기조회..
   ENDIF.

*-----------------------------------------------------------------------
* PF-STATUS SETTING
*-----------------------------------------------------------------------
   SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.

ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0014_STATUS_SCR0014 OUTPUT.

  IF W_STATUS_CHK = 'D'.
     SET PF-STATUS 'STDLISA'.
  ELSE.
     SET PF-STATUS 'STDLISW'.
  ENDIF.

  CASE INCLUDE.
     WHEN 'INCREATE' OR 'INCREAT1'.
        SET TITLEBAR 'POPU' WITH 'Partial reference LIST'.
     WHEN 'LCCHANGE' OR 'LCDISPLY' OR 'LCDISPL1'.
        SET TITLEBAR 'POPU' WITH 'Duplicate import request LIST'.
     WHEN 'TAXBKPO' OR 'TAXBKLC' OR 'TAXBKRQ'.
        SET TITLEBAR 'POPU' WITH 'Duplicate CTM LIST'.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH 'Message status'.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_IMG_CHECK_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE CHECK_IMG_CHECK_SCR0100 OUTPUT.

   IF W_FIRST_0100 EQ 'Y'.
      SELECT SINGLE * FROM ZTIMIMG00.
      IF SY-SUBRC NE 0.
         MESSAGE S963.
         LEAVE TO SCREEN 0.
      ENDIF.
      IF ZTIMIMG00-TAXBKYN NE 'X'.
         MESSAGE S628.
         LEAVE TO SCREEN 0.
      ENDIF.
      W_FIRST_0100 = 'N'.
   ENDIF.

ENDMODULE.                 " CHECK_IMG_CHECK_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0110 OUTPUT.

  IF W_STATUS EQ C_REQ_C OR W_STATUS EQ C_REQ_U.
    G_PARAM_LINE = TC_0110-TOP_LINE.
    TC_0110-LINES = G_PARAM_LINE + 06.               " LINE 수 정의.
  ELSEIF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSTAXBKIT   LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0110-LINES = G_PARAM_LINE.                    " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0110_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0110_SCR0110 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0110-CURRENT_LINE GT TC_0110-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSTAXBKIT INDEX TC_0110-CURRENT_LINE.

  IF SY-SUBRC = 0.                                    " READ SUCCESS..
     MOVE-CORRESPONDING IT_ZSTAXBKIT   TO ZSTAXBKIT.  " DATA MOVE
     MOVE: IT_ZSTAXBKIT-ZFMARK         TO W_ROW_MARK.    " MARK SET
     MODIFY IT_ZSTAXBKIT  INDEX TC_0110-CURRENT_LINE.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0110_SCR0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

   MOVE OK-CODE TO W_OK_CODE.
   CLEAR : OK-CODE.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCRCOM OUTPUT.

  LOOP AT SCREEN.
    CASE W_STATUS.
      WHEN C_REQ_C OR C_REQ_U.              " 생성, 변?
        IF SCREEN-GROUP1 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
        IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
      WHEN C_REQ_D.
        IF SCREEN-GROUP1 = 'I'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    IF SCREEN-NAME EQ 'W_ROW_MARK'.
       SCREEN-INPUT   = '1'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " OPERATION_MODE_SET_SCRCOM  OUTPUT
