*----------------------------------------------------------------------*
* INCLUDE ZRIM03O01 .
*----------------------------------------------------------------------*
*&  프로그램명 : 수입관세/부가세등록 Main PBO MODULE Include           *
*&      작성자 : 이 채 경 INFOLINK Ltd.                                *
*&      작성일 : 2001.06.10                                            *
*&  적용회사PJT:  LG 화학                                              *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  SET_INIT_FIELD_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_INIT_FIELD_SCR0100 OUTPUT.

  IF W_FIRST_SCR0100  EQ 'Y'.

     MOVE 'N' TO W_EDIT_CHECK.

*>> IMG 유지보수.
     SELECT SINGLE * FROM ZTIMIMG00.

     IF SY-SUBRC NE 0.
        MESSAGE S961.
        LEAVE TO SCREEN 0.
     ELSE.
        IF ZTIMIMG00-ZFPSMS NE '2'.
           MESSAGE S573.
           LEAVE TO SCREEN 0.
        ENDIF.
     ENDIF.
**>> IMG 비용계정코드.
*     SELECT SINGLE * FROM ZTIMIMG11.
*     IF SY-SUBRC NE 0.
*        MESSAGE S987.
*        LEAVE TO SCREEN 0.
*     ENDIF.
*>> STATUS FIELD SET.
     CASE SY-TCODE.
        WHEN 'ZIMC5'.   "생성.
           MOVE : C_REQ_C   TO   W_STATUS.
        WHEN 'ZIMC6'.   "변경.
           MOVE : C_REQ_U   TO   W_STATUS.
        WHEN 'ZIMC7'.   "조회.
           MOVE : C_REQ_D   TO   W_STATUS.
        WHEN OTHERS.
     ENDCASE.

*>> INITIALIZE.
     PERFORM P2000_DATA_INITIALIZE.
*>>
     CASE SY-TCODE.
        WHEN 'ZIMC5'.
           PERFORM   P2000_SET_CREATE_FIELD_VALUE.
        WHEN 'ZIMC6' OR 'ZIMC7'.   ">변경 및 조회일 경우...
           GET PARAMETER ID 'ZPCCNO' FIELD ZSCCHD-ZFCCNO.
           GET PARAMETER ID 'BUK'    FIELD ZSCCHD-BUKRS.
           GET PARAMETER ID 'GJR'    FIELD ZSCCHD-GJAHR.

           ZTCCHD-BUKRS = ZSCCHD-BUKRS.
           IF NOT ZTCCHD-BUKRS IS INITIAL.
*>> COMMPANY CODE CHECK.
              PERFORM  P1000_GET_COMPANY_CODE USING ZTCCHD-BUKRS.
           ENDIF.

           IF ZSCCHD-GJAHR IS INITIAL." OR ZSCCHD-GJAHR EQ '0000' OR
*              ZSCCHD-GJAHR EQ ' 000'  OR ZSCCHD-GJAHR EQ '  00' OR
*              ZSCCHD-GJAHR EQ '   0'.
              ZSCCHD-GJAHR = SY-DATUM(4).
           ENDIF.

           IF ZSCCHD-ZFCCNO IS INITIAL.
              PERFORM   P2000_SET_CREATE_FIELD_VALUE.
              EXIT.
           ENDIF.
*> CHARGE DOCUMENT READ.
           PERFORM   P1000_GET_CHARGE_DOCUMENT  USING   'S'.
           IF W_READ_ERROR = 'N'.
              PERFORM   P2000_BALANCE_CALCULATE.
           ENDIF.

        WHEN OTHERS.

     ENDCASE.

     MOVE : 'N'               TO W_FIRST_SCR0100,
            ZTCCHD-WAERS      TO RF05A-UBAZW.
  ENDIF.

ENDMODULE.                 " SET_INIT_FIELD_SCR0100  OUTPUT
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
*-----------------------------------------------------------------------
   PERFORM    P2000_SET_STATUS_TCODE_DISABLE.

*-----------------------------------------------------------------------
* PF-STATUS SETTING
*-----------------------------------------------------------------------
   SET PF-STATUS W_PFSTAT EXCLUDING IT_EXCL.

ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0100_ENABLE_SET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE TC_0100_ENABLE_SET_SCR0100 OUTPUT.

  LOOP AT SCREEN.
    CASE W_STATUS.
       WHEN C_REQ_C OR C_REQ_U.              " 생성, 변?
         IF SCREEN-GROUP1 = 'IO'.   SCREEN-INPUT   = '1'.
         ELSE.                      SCREEN-INPUT   = '0'.
         ENDIF.
         IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
        WHEN  C_ADD_U.                       " 추가변?
          IF SCREEN-GROUP2 = 'IO'.   SCREEN-INPUT   = '1'.
          ELSE.                      SCREEN-INPUT   = '0'.
          ENDIF.
          IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.   ENDIF.
       WHEN C_REQ_D OR C_ADD_D OR C_OPEN_D.  " 조회, 추가조회, 확정조?
         IF SCREEN-GROUP1 = 'I'.    SCREEN-INPUT   = '1'.
         ELSE.                      SCREEN-INPUT   = '0'.
         ENDIF.
       WHEN OTHERS.
    ENDCASE.
    IF W_STATUS EQ C_REQ_U AND
      ( SCREEN-NAME EQ 'ZTCCHD-BUKRS').
        SCREEN-INPUT = '0'.
    ENDIF.
*    READ TABLE IT_ZSCCIT INDEX 1.
*    IF ZTCCHD-ZFCSTGRP EQ '003' AND IT_ZSCCIT-ZFCD EQ '1AB'.
*       SCREEN-INPUT     = '0'.
*    ENDIF.
*> 세금자동계산.
* MKIM 막음 FROM
*      IF  ZTCCHD-XMWST EQ 'X' AND
*          SCREEN-NAME  EQ 'ZTCCHD-WMWST'.
*          SCREEN-INPUT     = '0'.
*          SCREEN-INVISIBLE = '1'.
*      ENDIF.
* MKIM 막음 TO
*>
      IF ZTCCHD-WAERS EQ T001-WAERS AND
       ( SCREEN-NAME EQ 'ZTCCHD-DMBTR' OR
         SCREEN-NAME EQ 'ZTCCHD-KURSF' OR
         SCREEN-NAME EQ 'ZTCCHD-WWERT' ).
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
      ENDIF.
*      IF ZTCCHD-ZFPOSYN NE 'Y' AND
*       ( SCREEN-NAME EQ 'ZTCCHD-ZFFYVT' OR
*         SCREEN-NAME EQ 'ZTCCHD-ZFVTAN' OR
*         SCREEN-NAME EQ 'ZTCCHD-ZFFYCC' OR
*         SCREEN-NAME EQ 'ZTCCHD-ZFCCAN' ).
*          SCREEN-INPUT     = '0'.
*          SCREEN-INVISIBLE = '1'.
*      ENDIF.

      IF ZTCCHD-BELNR1 IS INITIAL AND
       ( SCREEN-NAME EQ 'ZTCCHD-BELNR1' OR
         SCREEN-NAME EQ 'ZTCCHD-GJAHR1' ).
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
      ENDIF.

      IF ZTCCHD-BELNR2 IS INITIAL AND
       ( SCREEN-NAME EQ 'ZTCCHD-BELNR2' OR
         SCREEN-NAME EQ 'ZTCCHD-GJAHR2' ).
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
      ENDIF.
      IF ZTCCHD-ZFCCNO IS INITIAL AND
       ( SCREEN-NAME EQ 'ZTCCHD-ZFCCNO').
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
      ENDIF.


*      IF ZTCCHD-ZFPOSYN NE 'Y' AND ZTCCHD-ZFCCAN IS INITIAL AND
      IF ZTCCHD-ZFFYCC IS INITIAL AND
       ( SCREEN-NAME EQ 'ZTCCHD-ZFFYCC' OR  " 관세 전표연도.
         SCREEN-NAME EQ 'ZTCCHD-ZFCCAN').   " 관세 전표번호.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
      ENDIF.

      IF ZTCCHD-ZFVTAN IS INITIAL AND
       ( SCREEN-NAME EQ 'ZTCCHD-ZFFYVT' OR  " 부가세 전표연도.
         SCREEN-NAME EQ 'ZTCCHD-ZFVTAN' ).  " 부가세 전표번호.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
      ENDIF.

      IF SCREEN-NAME EQ 'W_ROW_MARK'.
         SCREEN-INPUT   =  '1'.
      ENDIF.

      MODIFY SCREEN.
   ENDLOOP.

ENDMODULE.                 " TC_0100_ENABLE_SET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0100 OUTPUT.

  IF W_STATUS EQ C_REQ_C OR W_STATUS EQ C_REQ_U.
    G_PARAM_LINE = TC_0100-TOP_LINE.
    TC_0100-LINES = G_PARAM_LINE + 06.               " LINE 수 정의.
  ELSEIF W_STATUS EQ C_REQ_D.
    DESCRIBE TABLE IT_ZSCCIT   LINES G_PARAM_LINE.   " LINE 수 GET
    TC_0100-LINES = G_PARAM_LINE.                    " LINE 수 정의.
  ENDIF.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0100_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE IT_TO_TC0100_SCR0100 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0100-CURRENT_LINE GT TC_0100-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSCCIT INDEX TC_0100-CURRENT_LINE.
  IF SY-SUBRC = 0.                                  " READ SUCCESS..
     MOVE-CORRESPONDING IT_ZSCCIT   TO ZSCCIT.      " DATA MOVE
     MOVE: IT_ZSCCIT-ZFMARK         TO W_ROW_MARK.  " MARK SET
     MOVE ZTCCHD-MWSKZ   TO ZSCCIT-MWSKZ.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0100_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

  MOVE OK-CODE TO W_OK_CODE.
  CLEAR : OK-CODE.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
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
*&      Module  D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0014_STATUS_SCR0014 OUTPUT.
  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH 'Message status'.
     WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0014_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_SCR110  OUTPUT
*&---------------------------------------------------------------------*
MODULE PF_STATUS_SCR110 OUTPUT.

  SET PF-STATUS '110'.

  CASE INCLUDE.
     WHEN '110'.
        SET TITLEBAR '110' WITH 'Accounting history'.
     WHEN OTHERS.
  ENDCASE.
  SUPPRESS DIALOG.

ENDMODULE.                 " PF_STATUS_SCR110  OUTPUT
