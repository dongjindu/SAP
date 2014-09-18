*&---------------------------------------------------------------------*
*& INCLUDE ZRZIMGI01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입시스템 Configuration 관리 PAI Module Inculde      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.26                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.
  IF ( SY-TCODE EQ 'ZIMGC3' AND SY-DYNNR EQ '9300' ) OR
     ( SY-TCODE EQ 'ZIMGA1' AND SY-DYNNR EQ '9400' ) OR
     ( SY-TCODE EQ 'ZIMC1'  AND SY-DYNNR EQ '8100' ) OR
     ( SY-TCODE EQ 'ZIMG02' AND SY-DYNNR EQ '0020' ) OR
     ( SY-TCODE EQ 'ZIMG05' AND SY-DYNNR EQ '0020' ) OR
     ( SY-TCODE EQ 'ZIMG14' AND SY-DYNNR EQ '0020' ) OR
     ( SY-TCODE EQ 'ZIMG08' AND SY-DYNNR EQ '0080' ) OR     "JSY020916
       SY-TCODE EQ 'ZIMGM'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  IF W_STATUS EQ 'D'.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSEIF W_STATUS EQ 'C' OR W_STATUS EQ 'I'.
*     PERFORM P2000_SET_MODIFY_CHECK.
    PERFORM P2000_SET_MESSAGE USING  SY-UCOMM.
  ENDIF.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0100 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0100-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0100_UPDATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE TC_0100_UPDATE_SCR0100 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG01 INDEX TC_0100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

* IF ( ZTIMIMG01-KTOKK IS INITIAL ) AND ( ZTIMIMG01-ZTERM IS INITIAL ).
  IF ZSIMIMG01-ZTERM   IS INITIAL AND
     ZSIMIMG01-BSTYP   IS INITIAL AND
     ZSIMIMG01-BSART   IS INITIAL AND
     ZSIMIMG01-ZFAPLDT IS INITIAL.
    MOVE 'D' TO W_DEL_MARK.
  ENDIF.

  MOVE-CORRESPONDING ZSIMIMG01 TO IT_ZSIMIMG01.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG01-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG01-LOEKZ.
  ENDIF.

  MOVE : W_ROW_MARK      TO IT_ZSIMIMG01-ZFMARK.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG01 INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG01.
  ENDIF.

ENDMODULE.                 " TC_0100_UPDATE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_VENDOR_ACCOUNT_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_VENDOR_ACCOUNT_SCR0100 INPUT.
  CLEAR : V_T077K-TXT30.

* CALL FUNCTION 'ZIM_GET_VENDOR_ACC_GRP'
*       EXPORTING
*           KTOKK  =   ZTIMIMG01-KTOKK
*       IMPORTING
*           TXT30  =   V_T077K-TXT30
*       EXCEPTIONS
*           KEY_INCOMPLETE = 0
*           NOT_FOUND      = 1
*           NOT_FOUND_TEXT = 2.
*
*     CASE SY-SUBRC.
*        WHEN 1.
**           MESSAGE E023 WITH IT_TC_0100-KTOKK.
*           MESSAGE E046(F2) WITH IT_TC_0100-KTOKK 'T077K'.
*        WHEN 2.
*           MESSAGE I046(F2) WITH IT_TC_0100-KTOKK 'T077Y'.
*        WHEN OTHERS.
*     ENDCASE.

ENDMODULE.                 " GET_VENDOR_ACCOUNT_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_PAY_TERM_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_PAY_TERM_SCR0100 INPUT.
  CLEAR : V_T052-TEXT1.

  CALL FUNCTION 'ZIM_GET_TERM_OF_PAYMENT'
       EXPORTING
            ZTERM          = ZSIMIMG01-ZTERM
       IMPORTING
            TEXT1          = V_T052-TEXT1
       EXCEPTIONS
            KEY_INCOMPLETE = 0
            NOT_FOUND      = 1
            NOT_FOUND_TEXT = 2.

  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE E024     WITH ZSIMIMG01-ZTERM  'T052'.
    WHEN 2.
*        MESSAGE I024     WITH ZTIMIMG01-ZTERM  'TVZBT'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " GET_PAY_TERM_SCR0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0100 INPUT.

  CASE OK-CODE.
    WHEN 'CHDC'.               " Change document
      PERFORM P2000_CHANGE_DOCUMENT.
    WHEN 'EXIT' OR 'BACK'.     " EXIT or BACK
      IF W_STATUS EQ 'C' OR W_STATUS EQ 'I'.
        PERFORM P2000_SET_MODIFY_CHECK   USING   W_GUBUN.
        IF W_GUBUN EQ 'Y'.
          PERFORM P2000_SAVE_PROCESS.
          IF ANTWORT NE 'C'.
            PERFORM 2000_BACK_SCREEN_DEFINE.
          ENDIF.
        ELSE.
          PERFORM 2000_BACK_SCREEN_DEFINE.
        ENDIF.
      ELSE.
        CLEAR OK-CODE.
        PERFORM 2000_BACK_SCREEN_DEFINE.
      ENDIF.
    WHEN 'INS1'.
      PERFORM  P2000_INSERT_LINE.
    WHEN 'DEL1'.
      PERFORM  P2000_DELETE_LINE.
    WHEN 'NEWL'.               " NEW ENTRY
      W_OLD_STATUS = W_STATUS.
      PERFORM P2000_SET_LOCK_MODE    USING    'L'.
      MOVE 'I' TO W_STATUS.
      PERFORM  P2000_IT_TAB_REFRESH.
*      SET SCREEN 2100. LEAVE TO SCREEN 2100.
    WHEN 'SAVE' OR 'ANZG'.     " SAVE or CHANGE=>DISPLAY
      CLEAR : ANTWORT.
      W_OK_CODE = OK-CODE.
      PERFORM P2000_SET_MODIFY_CHECK  USING W_GUBUN.
      IF W_GUBUN EQ 'Y'.
        PERFORM P2000_SAVE_PROCESS.
      ELSE.
        IF OK-CODE EQ 'SAVE'.  MESSAGE I973.   ENDIF.
        IF W_OK_CODE EQ 'ANZG'.
          PERFORM P2000_SET_LOCK_MODE    USING    'U'.
          MOVE 'D' TO W_STATUS.
          PERFORM  P1000_DATA_REREAD.    " DATA READ
        ENDIF.
      ENDIF.
    WHEN 'DELT'.               " Delete
      PERFORM P2000_DATA_DELETE.
    WHEN 'COPY'.
      PERFORM P2000_DATA_COPY.
    WHEN 'DELE' OR 'DELC'.     " DELETE mark or UNDELETE mark
      PERFORM P2000_SET_DEL_MARK.
    WHEN 'MKAL' OR 'MKLO'.
      PERFORM P2000_SET_ROW_MARK.
    WHEN 'AEND'.         " DISPLAY ==> CHANGE
      PERFORM P2000_AUTHORITY_CHECK USING W_OK_CODE.

      PERFORM P2000_SET_LOCK_MODE    USING    'L'.
      MOVE 'C' TO W_STATUS.
      PERFORM  P1000_DATA_REREAD.
    WHEN 'POSI'.                      " Position
      PERFORM P2000_CHANGE_POSITION.
    WHEN 'REFR'.
      PERFORM P2000_REFRESH_DATA.
    WHEN 'XLS'.
      PERFORM P2000_DOWNLOAD_DATA_AS_EXCEL.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

*  W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.


ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0200 INPUT.

  CASE OK-CODE.
    WHEN 'CHDC'.               " Change document
      PERFORM P2000_CHANGE_DOCUMENT.
    WHEN 'EXIT' OR 'BACK'.     " EXIT or BACK
      IF W_STATUS EQ 'C'.
        PERFORM P2000_SET_MODIFY_CHECK  USING  W_GUBUN.
        IF W_GUBUN EQ 'Y'.
          IF SY-TCODE EQ 'ZIMG02'.
            PERFORM P2000_SAVE_ZTIMIMGTX.
          ELSEIF SY-TCODE EQ 'ZIMG03'.
            PERFORM P2000_SAVE_ZTIMIMG00.
          ELSEIF SY-TCODE EQ 'ZIMG05'.
            PERFORM P2000_SAVE_ZTIMIMG11.
          ENDIF.
        ELSE.
          IF SY-TCODE EQ 'ZIMG02' OR
             SY-TCODE EQ 'ZIMG05'.
            PERFORM 2000_BACK_SCREEN_DEFINE.
          ELSE.
            SET SCREEN 0.  LEAVE SCREEN.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR OK-CODE.
        MOVE 'Y' TO W_FIRST_FLAG_0200.
        IF SY-TCODE EQ 'ZIMG02' OR
              SY-TCODE EQ 'ZIMG05'.
          PERFORM 2000_BACK_SCREEN_DEFINE.
        ELSE.
          SET SCREEN 0.  LEAVE SCREEN.
        ENDIF.
      ENDIF.
    WHEN 'AEND'.               " DISPLAY ==> CHANGE
      IF SY-TCODE EQ 'ZIMG02'.
        PERFORM   SET_LOCK_ZTIMIMGTX    USING    'L'.
      ELSEIF SY-TCODE EQ 'ZIMG05'.
        PERFORM   SET_LOCK_ZTIMIMG11    USING    'L'.
      ELSEIF SY-TCODE EQ 'ZIMG03'.
        PERFORM   SET_LOCK_ZTIMIMG00    USING    'L'.
      ENDIF.
      MOVE 'C' TO W_STATUS.
    WHEN 'ANZG' OR 'SAVE'.     " CHANGE  ==> DISPLAY
      CLEAR : ANTWORT.
      W_OK_CODE = OK-CODE.
*          IF SY-TCODE EQ 'ZIMG04'.  " Number Range Check
*             PERFORM   P2000_NUMBER_RANGE_CHECK  USING ''.
*          ENDIF.
      IF SY-TCODE EQ 'ZIMG02'.
        IF ZTIMIMGTX NE *ZTIMIMGTX.
          PERFORM P2000_SET_MESSAGE USING  OK-CODE.
        ELSE.
          IF OK-CODE EQ 'SAVE'.  MESSAGE I973. ENDIF.
        ENDIF.
      ELSEIF SY-TCODE EQ 'ZIMG03'.
        IF ZTIMIMG00 NE *ZTIMIMG00.
          PERFORM P2000_SET_MESSAGE USING  OK-CODE.
        ELSE.
          IF OK-CODE EQ 'SAVE'.  MESSAGE I973.   ENDIF.
        ENDIF.
      ELSEIF SY-TCODE EQ 'ZIMG05'.
        IF ZTIMIMG11 NE *ZTIMIMG11.
          PERFORM P2000_SET_MESSAGE USING  OK-CODE.
        ELSE.
          IF OK-CODE EQ 'SAVE'.  MESSAGE I973. ENDIF.
        ENDIF.
      ENDIF.

      IF ANTWORT EQ 'Y'.
*-----------------------------------------------------------------------
*   ZTIMIMG00 TABLE WRITE
*-----------------------------------------------------------------------
        IF SY-TCODE EQ 'ZIMG02'.
          PERFORM  P3000_WRITE_ZTIMIMGTX.
          PERFORM  SET_LOCK_ZTIMIMGTX    USING    'U'.
          MESSAGE  S953.
          PERFORM 2000_BACK_SCREEN_DEFINE.
        ELSEIF SY-TCODE EQ 'ZIMG03'.
          PERFORM  P3000_WRITE_ZTIMIMG00.
          PERFORM  SET_LOCK_ZTIMIMG00    USING    'U'.
          MESSAGE  S953.
          PERFORM 2000_BACK_SCREEN_DEFINE.
        ELSEIF SY-TCODE EQ 'ZIMG05'.
          PERFORM  P3000_WRITE_ZTIMIMG11.
          PERFORM  SET_LOCK_ZTIMIMG11    USING    'U'.
          MESSAGE  S953.
          PERFORM 2000_BACK_SCREEN_DEFINE.
        ENDIF.
      ENDIF.
      IF ANTWORT NE 'C'.
        IF SY-TCODE EQ 'ZIMG02'.
          PERFORM   SET_LOCK_ZTIMIMGTX    USING    'U'.
        ELSEIF SY-TCODE EQ 'ZIMG03'.
          PERFORM   SET_LOCK_ZTIMIMG00    USING    'U'.
        ELSEIF SY-TCODE EQ 'ZIMG05'.
          PERFORM   SET_LOCK_ZTIMIMG11    USING    'U'.
        ENDIF.
        IF W_OK_CODE EQ 'ANZG'.
          MOVE 'D' TO W_STATUS.
        ENDIF.
*-----------------------------------------------------------------------
*   ZTIMIMG00 TABLE SELECT
*-----------------------------------------------------------------------
        IF SY-TCODE NE 'ZIMG02'.
          PERFORM  P1000_READ_ZTIMIMGTX.
        ELSEIF SY-TCODE EQ 'ZIMG03'.
          PERFORM  P1000_READ_ZTIMIMG00.
        ELSEIF SY-TCODE EQ 'ZIMG05'.
          PERFORM  P1000_READ_ZTIMIMG11.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0800  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0800 INPUT.

  CASE OK-CODE.
*    WHEN 'CHDC'.               " Change document
*      PERFORM P2000_CHANGE_DOCUMENT.
    WHEN 'DEL1' .
      LOOP AT IT_ZSIMIMG20   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSIMIMG20   INDEX SY-TABIX.
      ENDLOOP.
      TC_0800-TOP_LINE = 1.

    WHEN 'INS1'.             " LINE INSET.
      W_COUNT = 0.
      LOOP AT IT_ZSIMIMG20   WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        ADD   1    TO    W_COUNT.
      ENDLOOP.
      CASE W_COUNT.
        WHEN 0.         MESSAGE S962.   EXIT.
        WHEN 1.
        WHEN OTHERS.    MESSAGE S965.   EXIT.
      ENDCASE.
      CLEAR : IT_ZSIMIMG20.
      INSERT INITIAL LINE INTO IT_ZSIMIMG20  INDEX  W_TABIX.

    WHEN 'EXIT' OR 'BACK'.     " EXIT or BACK
      IF W_STATUS EQ 'C'.
        PERFORM P2000_SET_MODIFY_CHECK  USING  W_GUBUN.
        IF W_GUBUN EQ 'Y'.
          PERFORM P2000_SAVE_ZTIMIMG20.
          PERFORM 2000_BACK_SCREEN_DEFINE.
        ELSE.
          PERFORM 2000_BACK_SCREEN_DEFINE.
        ENDIF.
      ELSE.
        CLEAR OK-CODE.
        PERFORM 2000_BACK_SCREEN_DEFINE.
      ENDIF.

    WHEN 'AEND'.               " DISPLAY ==> CHANGE
      PERFORM   SET_LOCK_ZTIMIMG20    USING    'L'.
      MOVE 'C' TO W_STATUS.

    WHEN 'ANZG' OR 'SAVE'.     " CHANGE  ==> DISPLAY
      CLEAR : ANTWORT.
      W_OK_CODE = OK-CODE.
      PERFORM P2000_SET_MODIFY_CHECK  USING  W_GUBUN.
      IF W_GUBUN EQ 'Y'.
        PERFORM P2000_SAVE_ZTIMIMG20.
      ELSE.
        IF OK-CODE EQ 'SAVE'.  MESSAGE I973. ENDIF.
        IF W_OK_CODE EQ 'ANZG'.
          MOVE 'D' TO W_STATUS.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0800  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR1200  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR1200 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1200-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR1200  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1200_UPDATE_SCR1200  INPUT
*&---------------------------------------------------------------------*
MODULE TC_1200_UPDATE_SCR1200 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG04 INDEX TC_1200-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

* IF ( ZTIMIMG01-KTOKK IS INITIAL ) AND ( ZTIMIMG01-ZTERM IS INITIAL ).
* IF ZTIMIMG04-ZTERM IS INITIAL.
*    MOVE 'D' TO W_DEL_MARK.
* ENDIF.

  MOVE-CORRESPONDING ZSIMIMG04 TO IT_ZSIMIMG04.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG04-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG04-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_ZSIMIMG04-ZFMARK.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSIMIMG04   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG04.
  ENDIF.

ENDMODULE.                 " TC_1200_UPDATE_SCR1200  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_PLANT_SCR1200  INPUT
*&---------------------------------------------------------------------*
MODULE GET_PLANT_SCR1200 INPUT.
  IF W_STATUS NE 'I'.   EXIT.    ENDIF.

  IF ZSIMIMG04-ZFWERKS IS INITIAL.
    MESSAGE E200.
  ENDIF.

  SELECT SINGLE * FROM T001W WHERE WERKS EQ ZSIMIMG04-ZFWERKS.
  IF SY-SUBRC NE 0.
    MESSAGE E201 WITH  ZSIMIMG04-ZFWERKS.
  ENDIF.
ENDMODULE.                 " GET_PLANT_SCR1200  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_MATGB_SCR1200  INPUT
*&---------------------------------------------------------------------*
MODULE GET_MATGB_SCR1200 INPUT.
  IF W_STATUS NE 'I'.   EXIT.    ENDIF.

  IF ZSIMIMG04-ZFMATGB IS INITIAL.
    MESSAGE E202.
  ENDIF.

  CASE ZSIMIMG04-ZFMATGB.
    WHEN '1' OR '2' OR '3' OR '4' OR '5'.
    WHEN OTHERS.
      MESSAGE E203 WITH ZSIMIMG04-ZFMATGB.
  ENDCASE.

ENDMODULE.                 " GET_MATGB_SCR1200  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_DATE_CHK_SCR1200  INPUT
*&---------------------------------------------------------------------*
MODULE INPUT_DATE_CHK_SCR1200 INPUT.

  IF W_STATUS NE 'I'.   EXIT.    ENDIF.

  IF ZSIMIMG04-ZFAPLDT IS INITIAL.
    MESSAGE E204.
  ENDIF.

ENDMODULE.                 " INPUT_DATE_CHK_SCR1200  INPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_RATE_CHK_SCR1200  INPUT
*&---------------------------------------------------------------------*
MODULE INPUT_RATE_CHK_SCR1200 INPUT.

  IF W_STATUS EQ 'D'.   EXIT.    ENDIF.

  IF ZSIMIMG04-ZFPLRTE IS INITIAL.
    MESSAGE E205.
  ENDIF.
ENDMODULE.                 " INPUT_RATE_CHK_SCR1200  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR1100 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1100-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1100_UPDATE_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE TC_1100_UPDATE_SCR1100 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG06 INDEX TC_1100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSIMIMG06 TO IT_ZSIMIMG06.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG06-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG06-LOEKZ.
  ENDIF.

  MOVE : W_ROW_MARK           TO IT_ZSIMIMG06-ZFMARK.

  IF IT_ZSIMIMG06-ZFAPLDT GT IT_ZSIMIMG06-ZFEXPDT.
    MESSAGE E280 WITH IT_ZSIMIMG06-ZFAPLDT IT_ZSIMIMG06-ZFEXPDT.
  ENDIF.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSIMIMG06   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG06.
  ENDIF.

ENDMODULE.                 " TC_1100_UPDATE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR1400  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR1400 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1400-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR1400  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR2400  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR2400 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_2400-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR2400  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1400_UPDATE_SCR1400  INPUT
*&---------------------------------------------------------------------*
MODULE TC_1400_UPDATE_SCR1400 INPUT.
*-----------------------------------------------------------------------
* Display MODE MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG07 INDEX TC_1400-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  W_OLD_SUBRC = SY-SUBRC.

  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSIMIMG07 TO IT_ZSIMIMG07.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG07-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG07-LOEKZ.
  ENDIF.

  MOVE : W_ROW_MARK      TO IT_ZSIMIMG07-ZFMARK.

  SELECT SINGLE *
           FROM ZTIMIMG10
          WHERE ZFCUT EQ IT_ZSIMIMG07-ZFCUT.
  IF SY-SUBRC EQ 0.
    SELECT SINGLE *
             FROM LFA1
            WHERE LIFNR = ZTIMIMG10-ZFVEN.
    MOVE LFA1-NAME1 TO IT_ZSIMIMG07-NAME1.
  ENDIF.

  IF  W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG07   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG07.
  ENDIF.

ENDMODULE.                 " TC_1400_UPDATE_SCR1400  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_2400_UPDATE_SCR2400  INPUT
*&---------------------------------------------------------------------*
MODULE TC_2400_UPDATE_SCR2400 INPUT.
*-----------------------------------------------------------------------
* Display MODE MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG24 INDEX TC_2400-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  W_OLD_SUBRC = SY-SUBRC.

  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSIMIMG24 TO IT_ZSIMIMG24.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG24-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG24-LOEKZ.
  ENDIF.

  MOVE : W_ROW_MARK      TO IT_ZSIMIMG24-ZFMARK.

  IF  W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG24   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG24.
  ENDIF.

ENDMODULE.                 " TC_2400_UPDATE_SCR2400  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9300  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9300 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE 'D' TO W_STATUS.
  W_ZFCDTY  =   ZTIMIMG08-ZFCDTY.
*-----------------------------------------------------------------------
* ZTIMIMG04 TABLE SELECT
*-----------------------------------------------------------------------
  PERFORM  P1000_READ_ZTIMIMG08.
  PERFORM  P2000_SET_REMARK_TEXT.

  ZSIMIMG08-ZFCDTY = ZTIMIMG08-ZFCDTY.
  TC_9301-TOP_LINE = 1.
  SET SCREEN 9301.  LEAVE SCREEN.
ENDMODULE.                 " USER_COMMAND_SCR9300  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR9301  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR9301 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_9301-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR9301  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_9301_UPDATE_SCR9301  INPUT
*&---------------------------------------------------------------------*
MODULE TC_9301_UPDATE_SCR9301 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG08 INDEX TC_9301-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSIMIMG08 TO IT_ZSIMIMG08.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG08-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG08-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_ZSIMIMG08-ZFMARK.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSIMIMG08   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG08.
  ENDIF.

ENDMODULE.                 " TC_9301_UPDATE_SCR9301  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR9100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR9100 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_9100-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_9301_UPDATE_SCR9100  INPUT
*&---------------------------------------------------------------------*
MODULE TC_9301_UPDATE_SCR9100 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG02 INDEX TC_9100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSIMIMG02 TO IT_ZSIMIMG02.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG02-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG02-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_ZSIMIMG02-ZFMARK.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG02   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG02.
  ENDIF.

ENDMODULE.                 " TC_9301_UPDATE_SCR9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR9200  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR9200 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_9200-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR9200  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_9200_UPDATE_SCR9200  INPUT
*&---------------------------------------------------------------------*
MODULE TC_9200_UPDATE_SCR9200 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG03 INDEX TC_9200-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG03-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG03-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_ZSIMIMG03-ZFMARK.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSIMIMG03   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG03.
  ENDIF.

ENDMODULE.                 " TC_9200_UPDATE_SCR9200  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR1500  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR1500 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1500-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR1500  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1500_UPDATE_SCR1500  INPUT
*&---------------------------------------------------------------------*
MODULE TC_1500_UPDATE_SCR1500 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG05 INDEX TC_1500-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  W_OLD_SUBRC = SY-SUBRC.

  CLEAR W_DEL_MARK.

*  MOVE-CORRESPONDING ZSIMIMG05 TO IT_ZSIMIMG05.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG05-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG05-LOEKZ.
  ENDIF.

  MOVE : W_ROW_MARK      TO IT_ZSIMIMG05-ZFMARK.

*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
*>>> 외화금?
* PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG05-NETPR
*                                            IT_ZSIMIMG05-WAERS.

*-----------------------------------------------------------------------

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG05   INDEX W_TABIX..
  ELSE.
    APPEND  IT_ZSIMIMG05.
  ENDIF.

ENDMODULE.                 " TC_1500_UPDATE_SCR1500  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1600_UPDATE_SCR1600  INPUT
*&---------------------------------------------------------------------*
MODULE TC_1600_UPDATE_SCR1600 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG09 INDEX TC_1600-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSIMIMG09 TO IT_ZSIMIMG09.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG09-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG09-LOEKZ.
  ENDIF.

  MOVE : W_ROW_MARK      TO IT_ZSIMIMG09-ZFMARK.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSIMIMG09   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG09.
  ENDIF.

ENDMODULE.                 " TC_1600_UPDATE_SCR1600  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR1600  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR1600 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1600-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR1600  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_ZTIMIMG00
*&---------------------------------------------------------------------*
FORM P2000_SAVE_ZTIMIMG00.
  W_OK_CODE = OK-CODE.
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
*-----------------------------------------------------------------------
* ZTIMIMG00 TABLE WRITE
*-----------------------------------------------------------------------
      PERFORM  P3000_WRITE_ZTIMIMG00.
      MESSAGE  S953.
      IF W_OK_CODE NE 'SAVE'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'N'.              " No...
      MESSAGE  S957.
      IF W_OK_CODE NE 'SAVE'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'C'.              " Cancel
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SAVE_ZTIMIMG00
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR1700  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR1700 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1700-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR1700  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1700_UPDATE_SCR1700  INPUT
*&---------------------------------------------------------------------*
MODULE TC_1700_UPDATE_SCR1700 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG17 INDEX TC_1700-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSIMIMG17 TO IT_ZSIMIMG17.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG17-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG17-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_ZSIMIMG17-ZFMARK.

*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal ) 00/08/25 김연중 추?
*-----------------------------------------------------------------------
*>>> 금?
  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG17-ZFMINBS
                                             IT_ZSIMIMG17-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG17-ZFMINPR
                                             IT_ZSIMIMG17-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG17-ZF45KLT
                                             IT_ZSIMIMG17-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG17-ZF100KLT
                                             IT_ZSIMIMG17-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG17-ZF1000KLT
                                             IT_ZSIMIMG17-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG17-ZF3000KLT
                                             IT_ZSIMIMG17-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG17-ZF3000KGE
                                             IT_ZSIMIMG17-WAERS.

*-----------------------------------------------------------------------

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSIMIMG17   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG17.
  ENDIF.

ENDMODULE.                 " TC_1700_UPDATE_SCR1700  INPUT
*&---------------------------------------------------------------------*
*&      Module  DATA_DOMAIN_CHECK_SCR9300  INPUT
*&---------------------------------------------------------------------*
MODULE DATA_DOMAIN_CHECK_SCR9300 INPUT.

  IF NOT ZTIMIMG08-ZFCDTY IS INITIAL.
    PERFORM   GET_DD07T_SELECT USING      'ZDCDTY'  ZTIMIMG08-ZFCDTY
                               CHANGING    DD07T-DDTEXT.
  ELSE.
    CLEAR : W_TEXT80.
    MESSAGE  E002(00).
  ENDIF.

ENDMODULE.                 " DATA_DOMAIN_CHECK_SCR9300  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_M_LC_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_M_LC_CHECK_SCR0200 INPUT.

  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'LC'.

ENDMODULE.                 " CHECK_M_LC_CHECK_SCR0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_L_LC_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_L_LC_CHECK_SCR0200 INPUT.

  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'LO'.

ENDMODULE.                 " CHECK_L_LC_CHECK_SCR0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_PURCH_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_PURCH_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'PU'.
ENDMODULE.                 " CHECK_PURCH_CHECK_SCR0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_TT_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_TT_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'TT'.
ENDMODULE.                 " CHECK_TT_CHECK_SCR0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_DA_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_DA_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'DA'.
ENDMODULE.                 " CHECK_DA_CHECK_SCR0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_OFF_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_OFF_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'OF'.
ENDMODULE.                 " CHECK_OFF_CHECK_SCR0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_BL_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_BL_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'BL'.
ENDMODULE.                 " CHECK_BL_CHECK_SCR0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_IV_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_IV_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'IV'.
ENDMODULE.                 " CHECK_IV_CHECK_SCR0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_VT_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_VT_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'VT'.
ENDMODULE.                 " CHECK_VT_CHECK_SCR0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_RED_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_RED_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'RE'.
ENDMODULE.                 " CHECK_RED_CHECK_SCR0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  IT_ZSIMIMG05_UPDATE_SCR1500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSIMIMG05_UPDATE_SCR1500 INPUT.
  READ TABLE IT_ZSIMIMG05 INDEX TC_1500-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  W_OLD_SUBRC = SY-SUBRC.

  MOVE-CORRESPONDING ZSIMIMG05 TO IT_ZSIMIMG05.
  IF IT_ZSIMIMG05-NETPR IS INITIAL.
    MESSAGE W167  WITH 'Net price'.
  ENDIF.
  IF IT_ZSIMIMG05-PEINH IS INITIAL.
    MESSAGE W167  WITH 'Price unit'.
    MOVE : 1       TO   IT_ZSIMIMG05-PEINH.
  ENDIF.
  IF IT_ZSIMIMG05-WAERS IS INITIAL.
    MESSAGE W167  WITH 'Currency key'.
    MOVE : 'KRW'   TO   IT_ZSIMIMG05-WAERS.
  ENDIF.

  IF IT_ZSIMIMG05-BPRME IS INITIAL.
    MESSAGE W167  WITH 'Net Price unit'.
    MOVE : 'KG'    TO   IT_ZSIMIMG05-BPRME.
  ENDIF.
*  IF ZFPORT
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
*>>> 외화금?
* PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG05-NETPR
*                                            IT_ZSIMIMG05-WAERS.
*-----------------------------------------------------------------------

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG05   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG05.
  ENDIF.

ENDMODULE.                 " IT_ZSIMIMG05_UPDATE_SCR1500  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_ZFVEN_NAME_SCR9100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZFVEN_NAME_SCR9100 INPUT.
  READ TABLE IT_ZSIMIMG02 INDEX TC_9100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSIMIMG02 TO IT_ZSIMIMG02.

  CLEAR : IT_ZSIMIMG02-NAME1.
  SELECT SINGLE NAME1 INTO IT_ZSIMIMG02-NAME1 FROM LFA1
                      WHERE LIFNR EQ IT_ZSIMIMG02-ZFVEN.

  MOVE : IT_ZSIMIMG02-NAME1 TO  ZSIMIMG02-NAME1.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG02   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG02.
  ENDIF.

ENDMODULE.                 " GET_ZFVEN_NAME_SCR9100  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_ZFBNAR_SCR9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ZFBNAR_SCR9200 INPUT.

  READ TABLE IT_ZSIMIMG03 INDEX TC_9200-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE : ZSIMIMG03-ZFBNAR(3)    TO    ZSIMIMG03-ZFCOTM,
         ZSIMIMG03-ZFBNAR+3(7)  TO    ZSIMIMG03-ZFBNARC.

  MOVE-CORRESPONDING ZSIMIMG03 TO IT_ZSIMIMG03.

  IF IT_ZSIMIMG03-ZFBNARCD IS INITIAL.
    MESSAGE E167 WITH '보세구역 내부코드'.
  ENDIF.

  IF W_STATUS NE 'I'.
    READ TABLE IT_ZSIMIMG03_ORG WITH KEY ZFBNAR = IT_ZSIMIMG03-ZFBNAR
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IF IT_ZSIMIMG03_ORG-ZFBNARCD NE IT_ZSIMIMG03-ZFBNARCD.
        PERFORM  P2000_BONDED_AREA_CHK
                 USING    IT_ZSIMIMG03_ORG-ZFBNARCD
                          W_TEXT40   W_CNT.
        IF W_CNT GT 0.
          MESSAGE E234 WITH IT_ZSIMIMG03_ORG-ZFBNARCD
                            W_TEXT40   W_CNT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT ZSIMIMG03-ZFCOTM IS INITIAL.
    PERFORM   GET_DD07T_SELECT USING      'ZDCOTM'  ZSIMIMG03-ZFCOTM
                               CHANGING   W_TEXT80.
    IF SY-SUBRC NE 0.
      MESSAGE W231 WITH ZSIMIMG03-ZFCOTM.
    ENDIF.
  ENDIF.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSIMIMG03   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG03.
  ENDIF.

ENDMODULE.                 " GET_ZFBNAR_SCR9200  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR1000  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR1000 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1000-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR1000  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_ZFVEN_NAME_SCR1000  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZFVEN_NAME_SCR1000 INPUT.

  READ TABLE IT_ZSIMIMG10 INDEX TC_1000-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSIMIMG10 TO IT_ZSIMIMG10.

  CLEAR : IT_ZSIMIMG10-NAME1.
  SELECT SINGLE NAME1 INTO IT_ZSIMIMG10-NAME1 FROM LFA1
                      WHERE LIFNR EQ IT_ZSIMIMG10-ZFVEN.

  MOVE : IT_ZSIMIMG10-NAME1 TO  ZSIMIMG10-NAME1.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG10   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG10.
  ENDIF.

ENDMODULE.                 " GET_ZFVEN_NAME_SCR1000  INPUT

*&---------------------------------------------------------------------*
*&      Module  TC_1000_UPDATE_SCR1000  INPUT
*&---------------------------------------------------------------------*
MODULE TC_1000_UPDATE_SCR1000 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG10 INDEX TC_1000-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSIMIMG10 TO IT_ZSIMIMG10.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG10-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG10-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_ZSIMIMG10-ZFMARK.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG10   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG10.
  ENDIF.

ENDMODULE.                 " TC_1000_UPDATE_SCR1000  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZFCD_MODIFY_CHECK_SCR1700  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZFCD_MODIFY_CHECK_SCR1700 INPUT.
  READ TABLE IT_ZSIMIMG17 INDEX TC_1700-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSIMIMG17 TO IT_ZSIMIMG17.

  IF IT_ZSIMIMG17-ZFCD IS INITIAL.
    MESSAGE E167 WITH 'B/L 비용코드'.
  ELSE.
    PERFORM  P2000_GET_ZTIMIMG08_TEXT     USING  '004'
                                                 IT_ZSIMIMG17-ZFCD
                                                 'E'
                                          CHANGING   W_TMP_TEXT.
  ENDIF.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSIMIMG17   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG17.
  ENDIF.
ENDMODULE.                 " ZFCD_MODIFY_CHECK_SCR1700  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZFCD5_CHECK_SCR9301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZFCD5_CHECK_SCR9301 INPUT.
  IF OK-CODE EQ 'DELT' AND
     W_ROW_MARK EQ 'X'.
    EXIT.
  ENDIF.

* 코드명은 입력필수.
  IF ZSIMIMG08-ZFCDNM IS INITIAL.
    PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFCDNM'
                                      DFIES-SCRTEXT_M   W_SUBRC.
    MESSAGE E167 WITH 'Code Description'.
  ENDIF.

* 수입거래구분에서는 유환여부 필수.
  IF ZSIMIMG08-ZFCDTY EQ '001'.
    IF  ZSIMIMG08-ZFCD1 IS INITIAL.
      PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFCD1'
                                        DFIES-SCRTEXT_M   W_SUBRC.
      MESSAGE E167 WITH 'Monetary Yes/No'.
    ENDIF.
  ENDIF.

* 선적항/도착항에서는 국가코드, TEXT 필수.
  IF ZSIMIMG08-ZFCDTY EQ '002'.

    IF ZSIMIMG08-ZFCD2 IS INITIAL.
      PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFCD2'
                                        DFIES-SCRTEXT_M   W_SUBRC.
      MESSAGE E167 WITH 'Country Code'.
    ENDIF.
    IF  ZSIMIMG08-ZFCD4 IS INITIAL.
      PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFCD4'
                                        DFIES-SCRTEXT_M   W_SUBRC.
      MESSAGE E167 WITH 'Loading Port/Arrival Port Text'.
    ENDIF.
  ENDIF.

  IF ZSIMIMG08-ZFCDTY EQ '003' OR   " Expense of L/C
     ZSIMIMG08-ZFCDTY EQ '004' OR   " Expense of Oversea
     ZSIMIMG08-ZFCDTY EQ '005' OR   " Expense of In-bonded
     ZSIMIMG08-ZFCDTY EQ '006' .    " Expense of Customs Clearance

    IF  ZSIMIMG08-ZFCD1 IS INITIAL.
      PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFCD1'
                                        DFIES-SCRTEXT_M   W_SUBRC.
      MESSAGE E167 WITH 'Delivery Cost Yes/No'.
    ENDIF.

    IF ZSIMIMG08-ZFCD5 IS INITIAL.
       PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFCD5'
                                         DFIES-SCRTEXT_M   W_SUBRC.
       MESSAGE E167 WITH 'Tax Code'.
    ENDIF.

    IF ZSIMIMG08-BLART IS INITIAL.
       PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIMIMG08' 'BLART'.
    ENDIF.

    CLEAR : T007A.
    SELECT SINGLE * FROM T007A
                    WHERE KALSM EQ 'TAXUS'
                    AND   MWSKZ EQ ZSIMIMG08-ZFCD5.
    IF SY-SUBRC NE 0.
      MESSAGE E244 WITH ZSIMIMG08-ZFCD5.
    ENDIF.

  ENDIF.

* 손보사 표준코드 및 식별자.
  IF ZSIMIMG08-ZFCDTY EQ '010'.
    IF NOT ZSIMIMG08-ZFCD5 IS INITIAL.
       PERFORM ALPHAFORMAT(SAPFF001) USING ZSIMIMG08-ZFCD5 W_LIFNR.

       SELECT SINGLE * FROM LFA1
              WHERE LIFNR  EQ  W_LIFNR.
       IF SY-SUBRC NE 0.
          PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFCD5'
                                            DFIES-SCRTEXT_M   W_SUBRC.
          MESSAGE E020 WITH ZSIMIMG08-ZFCD5.
       ENDIF.
    ENDIF.
  ENDIF.
*>> 보세구역/LOCATION MATCH..
  IF ZSIMIMG08-ZFCDTY EQ '011'.
    IF  ZSIMIMG08-ZFCD2 IS INITIAL.
      PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFCD2'
                                        DFIES-SCRTEXT_M   W_SUBRC.
      MESSAGE E167 WITH 'Bonded Area Code'.
    ENDIF.
    IF  ZSIMIMG08-ZFCD4 IS INITIAL.
      PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFCD4'
                                        DFIES-SCRTEXT_M   W_SUBRC.
      MESSAGE E167 WITH 'Storage Location'.
    ENDIF.
  ENDIF.
*>> 해상,항공운임 기본요율.  JSY 20021010 한수원.
  IF ZSIMIMG08-ZFCDTY EQ '016' OR
     ZSIMIMG08-ZFCDTY EQ '017'.
    IF  ZSIMIMG08-ZFMRATE IS INITIAL.
      PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFMRATE'
                                        DFIES-SCRTEXT_M   W_SUBRC.
      MESSAGE E167 WITH 'Rate'.
    ENDIF.
  ENDIF.
* 통관환율 관리용 통화.
  IF ZSIMIMG08-ZFCDTY EQ '998'.
    IF  ZSIMIMG08-ZFCD3 IS INITIAL.
      PERFORM P2000_NO_INPUT(SAPMZIM01) USING 'ZSIMIMG08' 'ZFCD3'
                                        DFIES-SCRTEXT_M   W_SUBRC.
      MESSAGE E167 WITH 'Sequence'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZFCD5_CHECK_SCR9301  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZFCD_GET_SCR9301  INPUT
*&---------------------------------------------------------------------*
MODULE ZFCD_GET_SCR9301 INPUT.

  IF ZTIMIMG08-ZFCDTY EQ '998'.     " 통관환율 (통화단위)
    IF NOT ZSIMIMG08-ZFCD IS INITIAL.
      SELECT SINGLE * FROM TCURT
                      WHERE WAERS EQ ZSIMIMG08-ZFCD
                      AND   SPRAS EQ SY-LANGU.
      IF SY-SUBRC EQ 0.
        IF ZSIMIMG08-ZFCDNM IS INITIAL.
          ZSIMIMG08-ZFCDNM = TCURT-LTEXT.
        ENDIF.
      ELSE.
        CLEAR : ZSIMIMG08-ZFCDNM.
        MESSAGE E277 WITH ZSIMIMG08-ZFCD.
      ENDIF.
      IF ZSIMIMG08-ZFCD3 IS INITIAL.
        MESSAGE E278.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZFCD_GET_SCR9301  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9400 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  W_STATUS = 'I'.
  TC_9401-TOP_LINE = 1.
  SET SCREEN 9401.  LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_SCR9400  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_TCURR_SCR9400  INPUT
*&---------------------------------------------------------------------*
MODULE GET_TCURR_SCR9400 INPUT.

  IF ZSREQHD-ZFDATE GT ZSREQHD-ZFEXPDT.
    MESSAGE E280 WITH ZSREQHD-ZFDATE ZSREQHD-ZFEXPDT.
  ENDIF.

  WRITE : ZSREQHD-ZFDATE   TO   W_CDATE.
  PERFORM P2000_DATE_CONVERT    USING  W_CDATE.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
       EXPORTING
            INPUT  = W_CDATE
       IMPORTING
            OUTPUT = W_GDATU.

  REFRESH : IT_ZSIMIMG06, IT_CURR, IT_TCURR.
  SELECT * INTO TABLE  IT_ZSIMIMG06 FROM ZTIMIMG06.

  SELECT ZFCD INTO CORRESPONDING FIELDS OF TABLE IT_CURR
                               FROM  ZTIMIMG08
                               WHERE ZFCDTY EQ '998'.

  IF SY-SUBRC NE 0.
    MESSAGE E279.
  ENDIF.

  LOOP AT IT_CURR.
    W_TABIX = SY-TABIX.

    MOVE : IT_CURR-ZFCD    TO IT_CURR-FCURR,
           ZSREQHD-ZFDATE  TO IT_CURR-ZFDATE,
           ZSREQHD-ZFEXPDT TO IT_CURR-ZFEXPDT.

    SELECT SINGLE LTEXT INTO IT_CURR-LTEXT FROM TCURT
                        WHERE WAERS    EQ    IT_CURR-FCURR
                        AND   SPRAS    EQ    SY-LANGU.

    MODIFY IT_CURR INDEX W_TABIX.
  ENDLOOP.

  SELECT * INTO TABLE IT_TCURR FROM TCURR FOR ALL ENTRIES IN IT_CURR
                               WHERE KURST EQ 'CC'
                               AND   FCURR EQ IT_CURR-FCURR
                               AND   TCURR EQ 'KRW'
                               AND   GDATU EQ W_GDATU.

  LOOP AT IT_CURR.
    W_TABIX = SY-TABIX.
    READ TABLE IT_TCURR WITH KEY FCURR = IT_CURR-FCURR.
    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING IT_TCURR  TO   IT_CURR.
    ENDIF.

    READ TABLE IT_ZSIMIMG06 WITH KEY WAERS    = IT_CURR-FCURR
                                     ZFAPLDT  = ZSREQHD-ZFDATE.
    IF SY-SUBRC EQ 0.
      MOVE : IT_ZSIMIMG06-ZFEXRT   TO    IT_CURR-ZFEXRT,
             IT_ZSIMIMG06-ZFEXPDT  TO    IT_CURR-ZFEXPDTO,
             IT_ZSIMIMG06-ERNAM    TO    IT_CURR-ERNAM,
             IT_ZSIMIMG06-CDAT     TO    IT_CURR-CDAT,
             IT_ZSIMIMG06-UNAM     TO    IT_CURR-UNAM,
             IT_ZSIMIMG06-UDAT     TO    IT_CURR-UDAT.
    ELSE.
      CLEAR : IT_CURR-ZFEXRT, IT_CURR-ZFEXPDTO.
      MOVE : SY-UNAME              TO    IT_CURR-ERNAM,
             SY-DATUM              TO    IT_CURR-CDAT,
             SY-UNAME              TO    IT_CURR-UNAM,
             SY-DATUM              TO    IT_CURR-UDAT.
    ENDIF.

    MODIFY IT_CURR INDEX W_TABIX.
  ENDLOOP.

  PERFORM   SET_V_CURR_LOCK      USING   'L'.

  PERFORM   SET_LOCK_ZTIMIMG06   USING   'L'.

ENDMODULE.                 " GET_TCURR_SCR9400  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR9401  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR9401 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_9401-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR9401  INPUT

*&---------------------------------------------------------------------*
*&      Module  TC_9401_UPDATE_SCR9401  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_9401_UPDATE_SCR9401 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_CURR INDEX TC_9401-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSTCURR TO IT_CURR.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_CURR   INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " TC_9401_UPDATE_SCR9401  INPUT

*&---------------------------------------------------------------------*
*&      Module  IT_CURR_UPDATE_SCR9401  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_CURR_UPDATE_SCR9401 INPUT.

  IF OK-CODE EQ 'MKAL' OR OK-CODE EQ 'MKLO'.  " 전체 선택/선택 해?
    EXIT.
  ENDIF.

  IF ZSTCURR-ZFMARK EQ 'X'.
    IF ZSTCURR-ZFDATE IS INITIAL.
      MESSAGE E167 WITH 'Valid from Date'.
    ENDIF.
    IF ZSTCURR-ZFEXPDT IS INITIAL.
      MESSAGE E167 WITH '종료일'.
    ENDIF.
    IF ZSTCURR-UKURS IS INITIAL.
      MESSAGE E283.
    ENDIF.
    IF ZSTCURR-ZFDATE GT ZSTCURR-ZFEXPDT.
      MESSAGE E280 WITH ZSTCURR-ZFDATE ZSTCURR-ZFEXPDT.
    ENDIF.
  ENDIF.
ENDMODULE.                 " IT_CURR_UPDATE_SCR9401  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_BDC_MODE_SCR9400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_BDC_MODE_SCR9400 INPUT.

  IF RADIO_BDCN EQ 'X'.
    DISP_MODE = 'N'.
  ELSEIF RADIO_BDCA EQ 'X'.
    DISP_MODE = 'A'.
  ELSEIF RADIO_BDCE EQ 'X'.
    DISP_MODE = 'E'.
  ENDIF.

ENDMODULE.                 " SET_BDC_MODE_SCR9400  INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG11
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG11.
* Clearing
  CLEAR: ZTIMIMG11, *ZTIMIMG11.
* Table Single-Select
  SELECT SINGLE * FROM ZTIMIMG11 WHERE BUKRS EQ W_BUKRS.
  IF SY-SUBRC NE 0.
    MOVE  W_BUKRS  TO  ZTIMIMG11-BUKRS.
  ELSE.
    MOVE-CORRESPONDING ZTIMIMG11  TO  *ZTIMIMG11.
    PERFORM P1000_VEN_GET_NAME.
  ENDIF.

ENDFORM.                    " P1000_READ_ZTIMIMG11

*&---------------------------------------------------------------------*
*&      Module  GET_PVEN_NAME_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_PVEN_NAME_SCR0500 INPUT.

  CLEAR : W_ZFPVEN1_NM, W_ZFPVEN2_NM, W_ZFPVEN3_NM, W_ZFPVEN4_NM,
          W_ZFPVEN5_NM, W_ZFPVEN6_NM, W_ZFPVEN7_NM.

  IF NOT ZTIMIMG11-ZFPVEN1 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE *
      FROM LFA1
     WHERE LIFNR = ZTIMIMG11-ZFPVEN1.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN1.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN1_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN2 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE *
      FROM LFA1
     WHERE LIFNR = ZTIMIMG11-ZFPVEN2.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN2.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN2_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN3 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE *
      FROM LFA1
     WHERE LIFNR = ZTIMIMG11-ZFPVEN3.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN3.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN3_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN4 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE *
      FROM LFA1
     WHERE LIFNR = ZTIMIMG11-ZFPVEN4.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN4.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN4_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN5 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE *
      FROM LFA1
     WHERE LIFNR = ZTIMIMG11-ZFPVEN5.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN5.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN5_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN6 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE *
      FROM LFA1
     WHERE LIFNR = ZTIMIMG11-ZFPVEN6.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN6.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN6_NM.
    ENDIF.
  ENDIF.
  IF NOT ZTIMIMG11-ZFPVEN7 IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE *
      FROM LFA1
     WHERE LIFNR = ZTIMIMG11-ZFPVEN7.
    IF SY-SUBRC NE 0.
      MESSAGE E023 WITH ZTIMIMG11-ZFPVEN7.
    ELSE.
      MOVE LFA1-NAME1        TO W_ZFPVEN7_NM.
    ENDIF.
  ENDIF.

ENDMODULE.                 " GET_PVEN_NAME_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0500  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0500 INPUT.

  CASE OK-CODE.
    WHEN 'CHDC'.               " Change document
      PERFORM P2000_CHANGE_DOCUMENT.
    WHEN 'EXIT' OR 'BACK'.     " EXIT or BACK
      IF W_STATUS EQ 'C'.
        PERFORM P2000_SET_MODIFY_CHECK  USING  W_GUBUN.
        IF W_GUBUN EQ 'Y'.
          PERFORM P2000_SAVE_ZTIMIMG11.
        ELSE.
          PERFORM 2000_BACK_SCREEN_DEFINE.
        ENDIF.
      ELSE.
        CLEAR OK-CODE.
        PERFORM 2000_BACK_SCREEN_DEFINE.
      ENDIF.
    WHEN 'AEND'.               " DISPLAY ==> CHANGE
      PERFORM   SET_LOCK_ZTIMIMG11    USING    'L'.
      MOVE 'C' TO W_STATUS.
    WHEN 'ANZG' OR 'SAVE'.     " CHANGE  ==> DISPLAY
      CLEAR : ANTWORT.
      W_OK_CODE = OK-CODE.
      IF SY-TCODE EQ 'ZIMG04'.  " Number Range Check
        PERFORM   P2000_NUMBER_RANGE_CHECK  USING ''.
      ENDIF.
      IF ZTIMIMG11 NE *ZTIMIMG11.
        PERFORM P2000_SET_MESSAGE USING  OK-CODE.
      ELSE.
        IF OK-CODE EQ 'SAVE'.  MESSAGE I973.   ENDIF.
      ENDIF.
      IF ANTWORT EQ 'Y'.
*-----------------------------------------------------------------------
* ZTIMIMG00 TABLE WRITE
*-----------------------------------------------------------------------
        PERFORM  P3000_WRITE_ZTIMIMG11.
        PERFORM  SET_LOCK_ZTIMIMG11    USING    'U'.
        MESSAGE  S953.
      ENDIF.
      IF ANTWORT NE 'C'.
        PERFORM   SET_LOCK_ZTIMIMG11    USING    'U'.
        IF W_OK_CODE EQ 'ANZG'.
          MOVE 'D' TO W_STATUS.
        ENDIF.
*-----------------------------------------------------------------------
* ZTIMIMG00 TABLE SELECT
*-----------------------------------------------------------------------
        PERFORM  P1000_READ_ZTIMIMG11.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_ZTIMIMG11
*&---------------------------------------------------------------------*
FORM P2000_SAVE_ZTIMIMG11.

  W_OK_CODE = OK-CODE.
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
*-----------------------------------------------------------------------
* ZTIMIMG00 TABLE WRITE
*-----------------------------------------------------------------------
      PERFORM  P3000_WRITE_ZTIMIMG11.
      MESSAGE  S953.
      IF W_OK_CODE NE 'SAVE'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'N'.              " No...
      MESSAGE  S957.
      IF W_OK_CODE NE 'SAVE'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'C'.              " Cancel
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SAVE_ZTIMIMG11

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG11
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG11.

  DATA: L_ZTIMIMG11   LIKE    ZTIMIMG11.

  SELECT SINGLE * INTO *ZTIMIMG11  FROM  ZTIMIMG11
  WHERE  BUKRS    EQ   ZTIMIMG11-BUKRS.

  IF SY-SUBRC EQ 0.
    UPDATE ZTIMIMG11.
* Change Document Object
    PERFORM   P3000_ZTIMIMG11_CHANGE_DOC        USING  'U'.
  ELSE.
* Change Document Object
    INSERT ZTIMIMG11.
    PERFORM   P3000_ZTIMIMG11_CHANGE_DOC        USING  'I'.
  ENDIF.

  IF SY-SUBRC NE 0.
    MESSAGE E952.
  ENDIF.

ENDFORM.                    " P3000_WRITE_ZTIMIMG11

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG11_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG11_CHANGE_DOC USING  UPD_CHNGIND.

  CONCATENATE SY-MANDT ZTIMIMG11-BUKRS INTO OBJECID.

  CALL FUNCTION 'ZTIMIMG11_WRITE_DOCUMENT'
      EXPORTING
           OBJECTID           =    OBJECID
           TCODE              =    SY-TCODE
           UTIME              =    SY-UZEIT
           UDATE              =    SY-DATUM
           USERNAME           =    SY-UNAME
           N_ZTIMIMG11        =    ZTIMIMG11
           O_ZTIMIMG11        =   *ZTIMIMG11
           UPD_ZTIMIMG11      =    UPD_CHNGIND
*            CDOC_UPD_OBJECT    =    UPD_CHNGIND
           OBJECT_CHANGE_INDICATOR = 'U'
           PLANNED_OR_REAL_CHANGES = ''
           NO_CHANGE_POINTERS      = ''
           UPD_ICDTXT_ZTIMIMG11    = ''
     TABLES
           ICDTXT_ZTIMIMG11   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG11_CHANGE_DOC

*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG11
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG11 USING PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG11'
         EXPORTING
              MANDT          = SY-MANDT
              BUKRS          = ZTIMIMG11-BUKRS
         EXCEPTIONS
              FOREIGN_LOCK   = 4
              SYSTEM_FAILURE = 8.
    CASE SY-SUBRC.
      WHEN  4.
        MESSAGE E513   WITH SY-UNAME  '비용계정'
                            SY-MANDT ''.
      WHEN  8.    MESSAGE E511.
    ENDCASE.

  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG11'
         EXPORTING
              MANDT = SY-MANDT
              BUKRS = ZTIMIMG11-BUKRS.
*        EXCEPTIONS
*                FOREIGN_LOCK       =     4
*                SYSTEM_FAILURE     =     8.
  ENDIF.


ENDFORM.                    " SET_LOCK_ZTIMIMG11
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIMIMG20
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIMIMG20.

*  CLEAR: IT_ZSIMIMG20, IT_ZSIMIMG20_ORG.
*  REFRESH : IT_ZSIMIMG20, IT_ZSIMIMG20_ORG.
** Table Single-Select
*  SELECT  * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG20
*         FROM  ZTIMIMG20
*        WHERE  BUKRS     EQ   ZTIMIMG20-BUKRS
*          AND  ZFAPLDT   EQ   ZTIMIMG20-ZFAPLDT.

*  APPEND LINES OF IT_ZSIMIMG20 TO IT_ZSIMIMG20_ORG.

** Clearing
*  CLEAR: ZTIMIMG20, *ZTIMIMG20.
*  READ TABLE IT_ZSIMIMG20 INDEX 1.
*  MOVE-CORRESPONDING IT_ZSIMIMG20 TO ZTIMIMG20.


  REFRESH : IT_ZSIMIMG20, IT_ZSIMIMG20_DEL.

* Table Multi-Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMIMG20
           FROM ZTIMIMG20.
*           WHERE BUKRS      EQ  ZTIMIMG20-BUKRS
*           AND   ZFAPLDT    EQ  ZTIMIMG20-ZFAPLDT.
*            ORDER BY ZTERM ZFAPLDT.

  IF SY-SUBRC NE 0.
    MESSAGE I967 WITH 'ZTIMIMG20'.
  ENDIF.

  IT_ZSIMIMG20_ORG[] = IT_ZSIMIMG20[].

ENDFORM.                    " P1000_READ_ZTIMIMG20
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_ZTIMIMG20
*&---------------------------------------------------------------------*
FORM P2000_SAVE_ZTIMIMG20.

  W_OK_CODE = OK-CODE.
  PERFORM P2000_SET_MESSAGE USING  OK-CODE.

  CASE ANTWORT.
    WHEN 'Y'.              " Yes...
*>> ZTIMIMG20 TABLE WRITE
      PERFORM  P3000_WRITE_ZTIMIMG20.
      MESSAGE  S953.
      PERFORM  SET_LOCK_ZTIMIMG20    USING    'U'.
      IF W_OK_CODE NE 'SAVE'.
        MOVE 'D' TO W_STATUS.
      ENDIF.
*>>   ZTIMIMG20 TABLE SELECT
      PERFORM  P1000_READ_ZTIMIMG20.
    WHEN 'N'.              " No...
      MESSAGE  S957.
      IF W_OK_CODE NE 'SAVE'.
        PERFORM 2000_BACK_SCREEN_DEFINE.
      ENDIF.
    WHEN 'C'.              " Cancel
      PERFORM   SET_LOCK_ZTIMIMG20    USING    'U'.
      IF W_OK_CODE EQ 'ANZG'.
        MOVE 'D' TO W_STATUS.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_SAVE_ZTIMIMG20
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTIMIMG20
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ZTIMIMG20.

  CLEAR : W_WAERS, W_ZFAPLDT, W_LAND1, W_ZFCD.

* KEY INSERT.
*  LOOP AT IT_ZSIMIMG20.
*    W_TABIX = SY-TABIX.
*    MOVE : ZTIMIMG20-BUKRS    TO  IT_ZSIMIMG20-BUKRS,
*           ZTIMIMG20-ZFAPLDT  TO  IT_ZSIMIMG20-ZFAPLDT.
*    MODIFY  IT_ZSIMIMG20 INDEX  W_TABIX.
*  ENDLOOP.

  LOOP AT IT_ZSIMIMG20_DEL.
    DELETE FROM ZTIMIMG20
           WHERE BUKRS   EQ IT_ZSIMIMG20_DEL-BUKRS
           AND   ZFAPLDT EQ IT_ZSIMIMG20_DEL-ZFAPLDT.

* Change Document..
    CLEAR : *ZTIMIMG20.
    MOVE-CORRESPONDING IT_ZSIMIMG20_DEL TO  ZTIMIMG20.
    PERFORM   P3000_ZTIMIMG20_CHANGE_DOC        USING  'D'.
  ENDLOOP.
  REFRESH : IT_ZSIMIMG20_DEL.

* Insert Data..
  LOOP AT IT_ZSIMIMG20.

    SELECT SINGLE * FROM  ZTIMIMG20
                    WHERE BUKRS    EQ IT_ZSIMIMG20-BUKRS
                    AND   ZFAPLDT  EQ IT_ZSIMIMG20-ZFAPLDT.

    IF SY-SUBRC EQ 0.
      IF NOT ( IT_ZSIMIMG20-ZFLCL     EQ ZTIMIMG20-ZFLCL    AND
               IT_ZSIMIMG20-ZF20FT    EQ ZTIMIMG20-ZF20FT   AND
               IT_ZSIMIMG20-ZF40FT    EQ ZTIMIMG20-ZF40FT   AND
               IT_ZSIMIMG20-ZFHQ      EQ ZTIMIMG20-ZFHQ      ).
        MOVE-CORRESPONDING ZTIMIMG20    TO *ZTIMIMG20.
        MOVE-CORRESPONDING IT_ZSIMIMG20 TO  ZTIMIMG20.
        MOVE : SY-UNAME         TO   ZTIMIMG20-UNAM,
               SY-DATUM         TO   ZTIMIMG20-UDAT.
        UPDATE ZTIMIMG20.
        IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.

* Change Document..
        PERFORM   P3000_ZTIMIMG20_CHANGE_DOC        USING  'U'.
      ENDIF.
    ELSE.
      IF IT_ZSIMIMG20-LOEKZ EQ 'X'.   CONTINUE.   ENDIF.
      MOVE-CORRESPONDING IT_ZSIMIMG20 TO  ZTIMIMG20.
      MOVE : SY-UNAME  TO   ZTIMIMG20-ERNAM,
             SY-DATUM  TO   ZTIMIMG20-CDAT,
             SY-UNAME  TO   ZTIMIMG20-UNAM,
             SY-DATUM  TO   ZTIMIMG20-UDAT.
      INSERT ZTIMIMG20.

      IF SY-SUBRC NE 0.   MESSAGE E952.   ENDIF.

* Change Document..
      CLEAR : *ZTIMIMG20.
      PERFORM   P3000_ZTIMIMG20_CHANGE_DOC        USING  'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_ZTIMIMG20
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIMIMG20_CHANGE_DOC
*&---------------------------------------------------------------------*
FORM P3000_ZTIMIMG20_CHANGE_DOC USING  UPD_CHNGIND.

  CONCATENATE SY-MANDT
              ZTIMIMG20-BUKRS
              ZTIMIMG20-ZFAPLDT INTO OBJECID.

  CALL FUNCTION 'ZTIMIMG20_WRITE_DOCUMENT'
    EXPORTING
          OBJECTID           =    OBJECID
          TCODE              =    SY-TCODE
          UTIME              =    SY-UZEIT
          UDATE              =    SY-DATUM
          USERNAME           =    SY-UNAME
          N_ZTIMIMG20        =    ZTIMIMG20
          O_ZTIMIMG20        =   *ZTIMIMG20
          UPD_ZTIMIMG20      =    UPD_CHNGIND
*           CDOC_UPD_OBJECT    =    UPD_CHNGIND
          OBJECT_CHANGE_INDICATOR = 'U'
          PLANNED_OR_REAL_CHANGES = ''
          NO_CHANGE_POINTERS      = ''
          UPD_ICDTXT_ZTIMIMG20    = ''
    TABLES
          ICDTXT_ZTIMIMG20   =    IT_CDTXT.

ENDFORM.                    " P3000_ZTIMIMG20_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  SET_LOCK_ZTIMIMG20
*&---------------------------------------------------------------------*
FORM SET_LOCK_ZTIMIMG20 USING PA_MODE.

  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG01'.
  ELSEIF PA_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG01'.
  ENDIF.

*  IF PA_MODE EQ 'L'.
**    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIMIMG20'
**         EXPORTING
**              MANDT          = SY-MANDT
**              ZFYEAR         = ZTIMIMG20-ZFYEAR
**              WERKS          = ZTIMIMG20-WERKS
**              ZFTRGB         = ZTIMIMG20-ZFTRGB
**         EXCEPTIONS
**              FOREIGN_LOCK   = 1
**              SYSTEM_FAILURE = 2
**              OTHERS         = 3.
**
*    CASE SY-SUBRC.
*      WHEN  1.
*        MESSAGE E513   WITH SY-UNAME  '수송요율'
*                            SY-MANDT ''.
*      WHEN  2.    MESSAGE E511.
*    ENDCASE.

*  ELSEIF PA_MODE EQ 'U'.
*
**    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIMIMG20'
**         EXPORTING
**              MANDT  = SY-MANDT
**              ZFYEAR = ZTIMIMG20-ZFYEAR
**              WERKS  = ZTIMIMG20-WERKS
**              ZFTRGB = ZTIMIMG20-ZFTRGB.

*  ENDIF.


ENDFORM.                    " SET_LOCK_ZTIMIMG20

*&---------------------------------------------------------------------*
*&      Module  CHECK_CONDITION_TYPE_SCR0500  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_CONDITION_TYPE_SCR0500 INPUT.

  IF NOT ZTIMIMG00-ZFKSCHL1 IS INITIAL.
    SELECT SINGLE * FROM T685
                    WHERE KVEWE EQ 'A'   "가격결정.
                    AND   KAPPL EQ 'M'   "어플리케이션.
                    AND   KSCHL EQ ZTIMIMG00-ZFKSCHL1.
    IF SY-SUBRC NE 0.
      MESSAGE E206(V1) WITH ZTIMIMG00-ZFKSCHL1 'A' 'M' ''.
    ENDIF.
  ENDIF.

  IF NOT ZTIMIMG00-ZFKSCHL2 IS INITIAL.
    SELECT SINGLE * FROM T685
                    WHERE KVEWE EQ 'A'   "가격결정.
                    AND   KAPPL EQ 'M'   "어플리케이션.
                    AND   KSCHL EQ ZTIMIMG00-ZFKSCHL2.
    IF SY-SUBRC NE 0.
      MESSAGE E206(V1) WITH ZTIMIMG00-ZFKSCHL2 'A' 'M' ''.
    ENDIF.
  ENDIF.

  IF NOT ZTIMIMG00-ZFKSCHL3 IS INITIAL.
    SELECT SINGLE * FROM T685
                    WHERE KVEWE EQ 'A'   "가격결정.
                    AND   KAPPL EQ 'M'   "어플리케이션.
                    AND   KSCHL EQ ZTIMIMG00-ZFKSCHL3.
    IF SY-SUBRC NE 0.
      MESSAGE E206(V1) WITH ZTIMIMG00-ZFKSCHL3 'A' 'M' ''.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_CONDITION_TYPE_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  KOMV_KSCHL1_SCR0500  INPUT
*&---------------------------------------------------------------------*
MODULE KOMV_KSCHL1_SCR0500 INPUT.

  PERFORM P2000_KOMV_KSCHL_HELP  USING   ZTIMIMG00-ZFKSCHL1
                                         '000000'.

ENDMODULE.                 " KOMV_KSCHL1_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  KOMV_KSCHL2_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KOMV_KSCHL2_SCR0500 INPUT.

  PERFORM P2000_KOMV_KSCHL_HELP  USING   ZTIMIMG00-ZFKSCHL2
                                         '000000'.

ENDMODULE.                 " KOMV_KSCHL2_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  KOMV_KSCHL3_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KOMV_KSCHL3_SCR0500 INPUT.

  PERFORM P2000_KOMV_KSCHL_HELP  USING   ZTIMIMG00-ZFKSCHL3
                                         '000010'.

ENDMODULE.                 " KOMV_KSCHL3_SCR0500  INPUT


*&---------------------------------------------------------------------*
*&      Module  CHECK_PAY_TERM_CODE_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_PAY_TERM_CODE_SCR0500 INPUT.

*  IF NOT ZTIMIMG11-ZTERM1 IS INITIAL.
*     SELECT * FROM T052 WHERE ZTERM EQ ZTIMIMG11-ZTERM1.
*        EXIT.
*     ENDSELECT.
*     IF SY-SUBRC NE 0.
*        MESSAGE E979 WITH ZTIMIMG11-ZTERM1.
*     ENDIF.
*  ENDIF.

*  IF NOT ZTIMIMG11-ZTERM2 IS INITIAL.
*     SELECT * FROM T052 WHERE ZTERM EQ ZTIMIMG11-ZTERM2.
*        EXIT.
*     ENDSELECT.
*     IF SY-SUBRC NE 0.
*        MESSAGE E979 WITH ZTIMIMG11-ZTERM2.
*     ENDIF.
*  ENDIF.
*
*  IF NOT ZTIMIMG11-ZTERM3 IS INITIAL.
*     SELECT * FROM T052 WHERE ZTERM EQ ZTIMIMG11-ZTERM3.
*        EXIT.
*     ENDSELECT.
*     IF SY-SUBRC NE 0.
*        MESSAGE E979 WITH ZTIMIMG11-ZTERM3.
*     ENDIF.
*  ENDIF.
*
*  IF NOT ZTIMIMG11-ZTERM4 IS INITIAL.
*     SELECT * FROM T052 WHERE ZTERM EQ ZTIMIMG11-ZTERM4.
*        EXIT.
*     ENDSELECT.
*     IF SY-SUBRC NE 0.
*        MESSAGE E979 WITH ZTIMIMG11-ZTERM4.
*     ENDIF.
*  ENDIF.

ENDMODULE.                 " CHECK_PAY_TERM_CODE_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_PAY1_SCR0500  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_PAY1_SCR0500 INPUT.

*  PERFORM  P2000_HELP_PAYMENT_TERM  USING  ZTIMIMG11-ZTERM1.

ENDMODULE.                 " HELP_PAY1_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_PAY2_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_PAY2_SCR0500 INPUT.

*  PERFORM  P2000_HELP_PAYMENT_TERM  USING  ZTIMIMG11-ZTERM2.

ENDMODULE.                 " HELP_PAY2_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_PAY3_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_PAY3_SCR0500 INPUT.

*  PERFORM  P2000_HELP_PAYMENT_TERM  USING  ZTIMIMG11-ZTERM3.

ENDMODULE.                 " HELP_PAY3_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_PAY4_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_PAY4_SCR0500 INPUT.

*  PERFORM  P2000_HELP_PAYMENT_TERM  USING  ZTIMIMG11-ZTERM4.

ENDMODULE.                 " HELP_PAY4_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_TBTKZ_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_TBTKZ_SCR0500 INPUT.

  PERFORM   P2000_TBTKZ_HELP     USING   ZTIMIMG00-TBTKZ.
  SET CURSOR FIELD 'ZTIMIMG00-TBTKZ'.
ENDMODULE.                 " HELP_TBTKZ_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_TBTKZ_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_TBTKZ_SCR0500 INPUT.

  IF NOT ZTIMIMG00-TBTKZ IS INITIAL.
    SELECT SINGLE * FROM T163B
                    WHERE BEWTP EQ ZTIMIMG00-TBTKZ.

    IF SY-SUBRC EQ 0.
      IF T163B-TBTKZ NE 'X'.
        MESSAGE E071(M8) WITH ZTIMIMG00-TBTKZ.
      ENDIF.
    ELSE.
      MESSAGE E071(M8) WITH ZTIMIMG00-TBTKZ.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_TBTKZ_SCR0500  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_CIV_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_CIV_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'CI'.
ENDMODULE.                 " CHECK_CIV_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_MSNO_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_MSNO_CHECK_SCR0200 INPUT.
  PERFORM   P2000_NUMBER_RANGE_CHECK  USING 'MS'.
ENDMODULE.                 " CHECK_MSNO_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR0600 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0600-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0600  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0600_UPDATE_SCR0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0600_UPDATE_SCR0600 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG12 INDEX TC_0600-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSIMIMG12 TO IT_ZSIMIMG12.

  CLEAR : W_BEZEI.
  SELECT SINGLE BEZEI INTO W_BEZEI FROM T618T
                      WHERE EXPVZ EQ IT_ZSIMIMG12-EXPVZ
                      AND   SPRAS EQ SY-LANGU.

  IF SY-SUBRC NE 0.
    MOVE '  '  TO  IT_ZSIMIMG12-BEZEI.
  ELSE.
    MOVE W_BEZEI TO IT_ZSIMIMG12-BEZEI.
  ENDIF.

  CLEAR W_DEL_MARK.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG12-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG12-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_ZSIMIMG12-ZFMARK.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG12   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG12.
  ENDIF.

ENDMODULE.                 " TC_0600_UPDATE_SCR0600  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LIFNR_SCR9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LIFNR_SCR9200 INPUT.
*>> 하역사명 DISPLAY.
  READ TABLE IT_ZSIMIMG03 INDEX TC_9200-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSIMIMG03 TO IT_ZSIMIMG03.

  CLEAR : IT_ZSIMIMG03-NAME1.
  SELECT SINGLE NAME1 INTO IT_ZSIMIMG03-NAME1 FROM LFA1
                      WHERE LIFNR EQ IT_ZSIMIMG03-LIFNR.

  MOVE : IT_ZSIMIMG03-NAME1 TO  ZSIMIMG03-NAME1.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG03   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG03.
  ENDIF.

ENDMODULE.                 " GET_LIFNR_SCR9200  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_POSTING_METHOD_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_POSTING_METHOD_SCR0200 INPUT.

  CHECK : W_STATUS NE C_REQ_D.

*>> Import Expense treating method..
  IF ZTIMIMG00-ZFCSTMD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG00' 'ZFCSTMD'.
  ENDIF.
*>> Charge Posting Method.
  IF ZTIMIMG00-ZFPSMS  IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG00' 'ZFPSMS'.
  ENDIF.

  CASE ZTIMIMG00-ZFCSTMD.
    WHEN 'S'.
      IF ZTIMIMG00-ZFPSMS NE '2'.
        MESSAGE E612
                WITH 'R/3 Condition Type' '비용 Group 별 Document'.
      ENDIF.
    WHEN 'I'.
      IF ZTIMIMG00-ZFPSMS NE '1'.
        MESSAGE E612
                WITH 'Infolink 제비용 배부' 'Charge Document'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
  IF ZTIMIMG00-ZFCSTMD EQ 'I'.
    IF NOT ZTIMIMG00-BLCSTMD IS INITIAL.
      MESSAGE E680 WITH '비용 Group 별 Document'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_POSTING_METHOD_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0020 INPUT.

  CLEAR  W_BUKRS.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'CHDC'.
      READ TABLE IT_T001 WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
        IF SY-TCODE EQ 'ZIMG02'.      ">EDI TEXT.
          MOVE : IT_T001-BUKRS TO ZTIMIMGTX-BUKRS,
                 IT_T001-BUTXT TO W_BUTXT.
          MOVE-CORRESPONDING IT_T001 TO T001.

        ELSEIF SY-TCODE EQ 'ZIMG05'.  ">비용처리.
          MOVE : IT_T001-BUKRS TO ZTIMIMG11-BUKRS,
                 IT_T001-BUTXT TO W_BUTXT.
          MOVE-CORRESPONDING IT_T001 TO T001.
        ELSEIF SY-TCODE EQ 'ZIMG14'.  ">통관수수료율.
          MOVE : IT_T001-BUKRS TO ZTIMIMG07-BUKRS,
                 IT_T001-BUTXT TO W_BUTXT.
          MOVE-CORRESPONDING IT_T001 TO T001.
          EXIT.
        ENDIF.
        PERFORM P2000_CHANGE_DOCUMENT.
      ELSE.
        MESSAGE S951.
      ENDIF.
    WHEN 'NEWL'.
      MOVE 'I' TO W_STATUS.
      IF SY-TCODE EQ 'ZIMG02'.
        CLEAR : ZTIMIMGTX, *ZTIMIMGTX, T001, W_BUTXT, T005.
        SET SCREEN 0200.  LEAVE SCREEN.
      ELSEIF SY-TCODE EQ 'ZIMG05'.
        CLEAR : ZTIMIMG11, *ZTIMIMG11, T001, W_BUTXT, T005.
        SET SCREEN 0500.  LEAVE SCREEN.
      ENDIF.
* 데이타 삭제..
    WHEN 'DELT'.
      READ TABLE IT_T001 WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
        PERFORM P2000_SET_MESSAGE USING  OK-CODE.
        CHECK ANTWORT EQ 'Y'.

        IF SY-TCODE EQ 'ZIMG02'.      ">EDI TEXT.
          SELECT SINGLE * FROM ZTIMIMGTX
                 WHERE    BUKRS EQ IT_T001-BUKRS.
          IF SY-SUBRC EQ 0.
            PERFORM   SET_LOCK_ZTIMIMGTX    USING    'L'.
            DELETE ZTIMIMGTX.
            IF SY-SUBRC EQ 0.
               *ZTIMIMGTX = ZTIMIMGTX.
              PERFORM   P3000_ZTIMIMGTX_CHANGE_DOC    USING  'D'.
            ELSE.
              MESSAGE S313.
            ENDIF.
            PERFORM   SET_LOCK_ZTIMIMGTX    USING    'U'.
          ELSE.
            MESSAGE E401(ZIM1)  WITH IT_T001-BUKRS.
          ENDIF.
        ELSEIF SY-TCODE EQ 'ZIMG05'.  ">비용처리.
          SELECT SINGLE * FROM ZTIMIMG11
                 WHERE    BUKRS EQ IT_T001-BUKRS.
          IF SY-SUBRC EQ 0.
            PERFORM   SET_LOCK_ZTIMIMG11    USING    'L'.
            DELETE ZTIMIMG11.
            IF SY-SUBRC EQ 0.
               *ZTIMIMG11 = ZTIMIMG11.
              PERFORM   P3000_ZTIMIMG11_CHANGE_DOC  USING  'D'.
            ELSE.
              MESSAGE S313.
            ENDIF.
            PERFORM   SET_LOCK_ZTIMIMG11    USING    'U'.
          ENDIF.
        ENDIF.
        W_FIRST_FLAG_0020 = 'Y'.
      ELSE.
        MESSAGE S951.
      ENDIF.

    WHEN 'DETA' OR 'COPY'.
      READ TABLE IT_T001 WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
        MOVE IT_T001-BUKRS  TO  W_BUKRS.
        IF SY-UCOMM EQ 'DETA'.
          MOVE 'D' TO W_STATUS.
        ELSE.
          MOVE 'I' TO W_STATUS.
        ENDIF.

        IF SY-TCODE EQ 'ZIMG02'.
          PERFORM  P1000_READ_ZTIMIMGTX.
          MOVE : IT_T001-BUTXT TO W_BUTXT.
          SELECT SINGLE * FROM T005
                 WHERE LAND1 EQ IT_T001-LAND1.
          IF SY-UCOMM EQ 'COPY'.
            CLEAR : ZTIMIMGTX-BUKRS, T001, W_BUTXT, T005.
          ENDIF.
          SET SCREEN 0200.  LEAVE SCREEN.
        ELSEIF SY-TCODE EQ 'ZIMG05'.
          PERFORM  P1000_READ_ZTIMIMG11.
          MOVE : IT_T001-BUTXT TO W_BUTXT.
          MOVE-CORRESPONDING IT_T001 TO T001.
          SELECT SINGLE * FROM T005
                 WHERE LAND1 EQ IT_T001-LAND1.
          IF SY-UCOMM EQ 'COPY'.
            CLEAR : ZTIMIMG11-BUKRS, T001, W_BUTXT, T005.
          ENDIF.
          SET SCREEN 0500.  LEAVE SCREEN.
        ENDIF.
      ELSE.
        MESSAGE S951.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.




*-----------------------------------------------------------------------
* ZTIMIMGTX TABLE SELECT
*-----------------------------------------------------------------------




ENDMODULE.                 " USER_COMMAND_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0080  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0080 INPUT.

  CLEAR : ZTIMIMG20, ZSIMIMG20.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT'.
      SET SCREEN 0.   LEAVE SCREEN.
*    WHEN 'CHDC'.
*      READ TABLE IT_TRPHD WITH KEY ZFMARK = 'X'.
*      IF SY-SUBRC EQ 0.
*        MOVE-CORRESPONDING IT_TRPHD TO ZTIMIMG20.
*        PERFORM P2000_CHANGE_DOCUMENT.
*      ELSE.
*        MESSAGE S951.
*      ENDIF.
    WHEN 'NEWL'.
      MOVE 'I' TO W_STATUS.
      CLEAR : ZTIMIMG20, IT_ZSIMIMG20.
      REFRESH : IT_ZSIMIMG20.
      SET SCREEN 0800.  LEAVE SCREEN.
* 데이타 삭제..
    WHEN 'DELT'.
      READ TABLE IT_TRPHD WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
        PERFORM P2000_SET_MESSAGE USING  OK-CODE.

        CHECK ANTWORT EQ 'Y'.

        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IMG20
                 FROM ZTIMIMG20
               WHERE   BUKRS   EQ IT_TRPHD-BUKRS
                 AND   ZFAPLDT EQ IT_TRPHD-ZFAPLDT.

        IF SY-SUBRC EQ 0.
          MOVE-CORRESPONDING IT_TRPHD TO ZTIMIMG20.
          PERFORM   SET_LOCK_ZTIMIMG20    USING    'L'.
          DELETE FROM ZTIMIMG20
               WHERE   BUKRS    EQ ZTIMIMG20-BUKRS
                 AND   ZFAPLDT  EQ ZTIMIMG20-ZFAPLDT.

          IF SY-SUBRC EQ 0.
            LOOP AT IT_IMG20.
              CLEAR : ZTIMIMG20, *ZTIMIMG20.
              ZTIMIMG20 = IT_IMG20.
               *ZTIMIMG20 = ZTIMIMG20.
              PERFORM   P3000_ZTIMIMG20_CHANGE_DOC  USING  'D'.
            ENDLOOP.
            W_FIRST_FLAG_0080 = 'Y'.
          ELSE.
            MESSAGE S313.
          ENDIF.
          PERFORM   SET_LOCK_ZTIMIMG20    USING    'U'.
        ENDIF.
        W_FIRST_FLAG_0020 = 'Y'.
      ELSE.
        MESSAGE S951.
      ENDIF.

    WHEN 'DETA' OR 'COPY'.
      READ TABLE IT_TRPHD WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
        CLEAR: ZTIMIMG20, *ZTIMIMG20.
        MOVE-CORRESPONDING IT_TRPHD  TO  ZTIMIMG20.
        IF SY-UCOMM EQ 'DETA'.
          MOVE 'D' TO W_STATUS.
        ELSE.
          MOVE 'I' TO W_STATUS.
        ENDIF.
        PERFORM P1000_READ_ZTIMIMG20.
        IF SY-UCOMM EQ 'COPY'.
          CLEAR : ZTIMIMG20, *ZTIMIMG20.
        ENDIF.
        SET SCREEN 0800.  LEAVE SCREEN.
      ELSE.
        MESSAGE S951.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_SCR0080  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0050 INPUT.

  CLEAR:  W_BUKRS, W_BUTXT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE 'D' TO W_STATUS.
  MOVE ZTIMIMG11-BUKRS  TO  W_BUKRS.
*-----------------------------------------------------------------------
* ZTIMIMGTX TABLE SELECT
*-----------------------------------------------------------------------
  PERFORM  P1000_READ_ZTIMIMG11.

  SET SCREEN 0500.  LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_SCR0050  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_EDI_PATH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_EDI_PATH INPUT.
  CHECK : W_STATUS NE C_REQ_D.

  IF ZTIMIMGTX-ZFEDIYN EQ 'X'.
    IF ZTIMIMGTX-ZFPATH IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMGTX' 'ZFPATH'.
    ENDIF.
    IF ZTIMIMGTX-ZFRECV IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMGTX' 'ZFRECV'.
    ENDIF.
    IF ZTIMIMGTX-ZFMAILID IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMGTX' 'ZFMAILID'.
    ENDIF.
    IF ZTIMIMGTX-HOST IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMGTX' 'HOST'.
    ENDIF.
    IF ZTIMIMGTX-ZFRBAK IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMGTX' 'ZFRBAK'.
    ENDIF.
    IF ZTIMIMGTX-UNAME IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMGTX' 'UNAME'.
    ENDIF.
    IF ZTIMIMGTX-PASSW IS INITIAL AND
       W_PASSW         IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMGTX' 'PASSW'.
    ENDIF.
    IF NOT W_PASSW  IS INITIAL.
      DESCRIBE FIELD W_PASSW LENGTH MI_PWD_LEN.

*-- FTP_CONNECT requires an encrypted password to work
      CALL 'AB_RFC_X_SCRAMBLE_STRING'
           ID 'SOURCE'      FIELD W_PASSW
           ID 'KEY'         FIELD MI_KEY
           ID 'SCR'         FIELD 'X'
           ID 'DESTINATION' FIELD ZTIMIMGTX-PASSW
           ID 'DSTLEN'      FIELD MI_PWD_LEN.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_EDI_PATH  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BANK_POSITION_SCR0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_BANK_POSITION_SCR0300 INPUT.
  CHECK : W_STATUS NE C_REQ_D.

  IF ZTIMIMG00-ZFBKYN EQ 'X'.
*      IF ZTIMIMG00-ZFUSDAM IS INITIAL.
*          PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG00' 'ZFUSDAM'.
*      ENDIF.
    IF ZTIMIMG00-LCBKYN IS INITIAL AND
       ZTIMIMG00-LOBKYN IS INITIAL AND
       ZTIMIMG00-PUBKYN IS INITIAL AND
       ZTIMIMG00-TTBKYN IS INITIAL AND
       ZTIMIMG00-DABKYN IS INITIAL AND
       ZTIMIMG00-DPBKYN IS INITIAL AND
       ZTIMIMG00-GSMBKYN IS INITIAL.
      MESSAGE E662.
    ENDIF.
*             ZTIMIMG00-LCBKCD,
*             ZTIMIMG00-LOBKCD,
*             ZTIMIMG00-PUBKCD,
*             ZTIMIMG00-TTBKCD,
*             ZTIMIMG00-DABKCD,
*             ZTIMIMG00-DPBKCD,
*             ZTIMIMG00-GSMBKCD.
  ELSE.
    IF NOT ( ZTIMIMG00-LCBKYN IS INITIAL AND
             ZTIMIMG00-LOBKYN IS INITIAL AND
             ZTIMIMG00-PUBKYN IS INITIAL AND
             ZTIMIMG00-TTBKYN IS INITIAL AND
             ZTIMIMG00-DABKYN IS INITIAL AND
             ZTIMIMG00-DPBKYN IS INITIAL AND
             ZTIMIMG00-GSMBKYN IS INITIAL ).
      MESSAGE W663.
      ZTIMIMG00-ZFBKYN = 'X'.
    ENDIF.

  ENDIF.

ENDMODULE.                 " CHECK_BANK_POSITION_SCR0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BUKRS_NAME_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_BUKRS_NAME_SCR0020 INPUT.

  CLEAR  W_BUTXT.

  IF ZTIMIMGTX-BUKRS NE SPACE.
    SELECT SINGLE BUTXT INTO W_BUTXT FROM T001
    WHERE  BUKRS  EQ    ZTIMIMGTX-BUKRS.
  ENDIF.

ENDMODULE.                 " GET_BUKRS_NAME_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BUKRS_NAME_SCR0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_BUKRS_NAME_SCR0050 INPUT.

  CLEAR  W_BUTXT.

  IF ZTIMIMG11-BUKRS NE SPACE.
    SELECT SINGLE BUTXT INTO W_BUTXT FROM T001
    WHERE  BUKRS  EQ    ZTIMIMG11-BUKRS.
  ENDIF.

ENDMODULE.                 " GET_BUKRS_NAME_SCR0050  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_FIX_EXCHANGE_RATE_METHOD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_FIX_EXCHANGE_RATE_METHOD INPUT.
  CHECK : W_STATUS NE C_REQ_D.

  IF ZTIMIMG00-ZFEXFIX EQ 'X'.
    IF ZTIMIMG00-ZFEXMTD IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG00' 'ZFEXMTD'.
    ENDIF.
  ELSE.
    IF NOT ZTIMIMG00-ZFEXMTD IS INITIAL.
      CLEAR : ZTIMIMG00-ZFEXMTD.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_FIX_EXCHANGE_RATE_METHOD  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_SCR9500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DATA_SCR9500 OUTPUT.

  IF W_FIRST_FLAG_9500 EQ 'Y'.
    CLEAR : G_NODE_KEY_OLD.

    IF SY-TCODE EQ 'ZIMGC4'.
      ZTIMIMG08-ZFCDTY = '001'.
    ELSEIF SY-TCODE EQ 'ZEXZ23'.
      ZTIMIMG08-ZFCDTY = '006'.
    ENDIF.

    G_NODE_KEY_NEW = ZTIMIMG08-ZFCDTY.
    W_ZFCDTY  =   ZTIMIMG08-ZFCDTY.
    MOVE 'D' TO W_STATUS.
    W_FIRST_FLAG_9500 = 'N'.

    REFRESH : IT_V_TVKN.
    SELECT ZFCDTY INTO IT_V_TVKN-NODE
           FROM   ZTIMIMG08
           GROUP BY ZFCDTY.

      IF SY-TCODE EQ 'ZIMGC4'.
        CASE IT_V_TVKN-NODE.
          WHEN '000' OR '001' OR '003' OR
               '004' OR '005' OR '006' OR
               '010' OR '012' OR '013' OR
               '997' OR '999' OR '014' OR
               '015' OR '016' OR '017'.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
      ELSEIF SY-TCODE EQ 'ZEXZ23'.
        CASE IT_V_TVKN-NODE.
          WHEN '006' OR
               '801' OR '802' OR '803' OR
               '804' OR '805' OR '806' OR '807' OR
               '808' OR '809' OR '810'.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
      ELSE.
        CONTINUE.
      ENDIF.

      PERFORM   GET_DD07T_SELECT
                       USING      'ZDCDTY'  IT_V_TVKN-NODE
                       CHANGING    IT_V_TVKN-TEXT.
      APPEND  IT_V_TVKN.
    ENDSELECT.

    SORT IT_V_TVKN BY NODE.
  ENDIF.

  IF G_NODE_KEY_NEW <> G_NODE_KEY_OLD.
     G_NODE_KEY_OLD   = G_NODE_KEY_NEW.
     ZTIMIMG08-ZFCDTY = G_NODE_KEY_NEW.
     W_ZFCDTY         = ZTIMIMG08-ZFCDTY.

     READ TABLE IT_V_TVKN WITH KEY NODE = W_ZFCDTY.
     DD07T-DDTEXT = IT_V_TVKN-TEXT.

     PERFORM  P1000_READ_ZTIMIMG08.
     PERFORM  P2000_SET_REMARK_TEXT.

    ZSIMIMG08-ZFCDTY = ZTIMIMG08-ZFCDTY.
  ENDIF.

ENDMODULE.                 " GET_DATA_SCR9500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_TREE_SCR9500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_TREE_SCR9500 INPUT.

*  IF G_EVENT EQ 'NODE_DOUBLE_CLICK'.
  PERFORM P2000_READ_NEW_NODE CHANGING G_NODE_KEY_NEW.
*  ENDIF.
  IF G_NODE_KEY_NEW IS INITIAL.
    IF SY-TCODE EQ 'ZIMGC4'.
      ZTIMIMG08-ZFCDTY = '001'.
    ELSEIF SY-TCODE EQ 'ZEXZ23'.
      ZTIMIMG08-ZFCDTY = '006'.
    ENDIF.
    G_NODE_KEY_NEW   = ZTIMIMG08-ZFCDTY.
    W_ZFCDTY         = ZTIMIMG08-ZFCDTY.
  ENDIF.

  CLEAR G_NODE_KEY_ALL.

  IF G_NODE_KEY_NEW <> G_NODE_KEY_OLD.
    IF W_STATUS EQ 'C' OR W_STATUS EQ 'I'.
      CLEAR : ANTWORT.
      PERFORM P2000_SET_MODIFY_CHECK  USING W_GUBUN.
      IF W_GUBUN EQ 'Y'.
        OK-CODE = 'ANZG'.
* MESSAGE
        W_OK_CODE = OK-CODE.
        PERFORM P2000_SET_MESSAGE USING  OK-CODE.
* POPUP SCREEN
        CASE ANTWORT.
          WHEN 'Y'.              " Yes
            PERFORM  P3000_DATABASE_MODIFY.
            MESSAGE  S953.
            IF W_STATUS EQ 'I'.
              W_STATUS = 'C'.
            ENDIF.
          WHEN 'N'.              " No...
            IF W_STATUS EQ 'I'.
              W_STATUS = 'C'.
            ENDIF.
          WHEN 'C'.              " Cancel
            IF SY-TCODE EQ 'ZIMGC4' OR
               SY-TCODE EQ 'ZEXZ23'.
              G_NODE_KEY_NEW = G_NODE_KEY_OLD.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ELSE.
        IF W_STATUS EQ 'I'.
          W_STATUS = 'C'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_TREE_SCR9500  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0010 INPUT.


  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN ''.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0020_UPDATE_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0020_UPDATE_SCR0020 INPUT.

  READ TABLE IT_T001 INDEX TC_0020-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING T001 TO IT_T001.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_T001-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_T001-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_T001-ZFMARK.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_T001   INDEX W_TABIX.
  ELSE.
    APPEND IT_T001.
  ENDIF.

ENDMODULE.                 " TC_0020_UPDATE_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0080_UPDATE_SCR0080  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0080_UPDATE_SCR0080 INPUT.

  READ TABLE IT_TRPHD INDEX TC_0080-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZTIMIMG20 TO IT_TRPHD.

*  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
*     MOVE : 'X'             TO W_DEL_MARK,
*            W_DEL_MARK      TO IT_T001-LOEKZ.
*  ENDIF.
*
*  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
*     MOVE : ' '             TO W_DEL_MARK,
*            ' '             TO IT_T001-LOEKZ.
*  ENDIF.
  MOVE : W_ROW_MARK      TO IT_TRPHD-ZFMARK.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_TRPHD   INDEX W_TABIX.
  ELSE.
    APPEND IT_TRPHD.
  ENDIF.

ENDMODULE.                 " TC_0080_UPDATE_SCR0080  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0800_UPDATE_SCR0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0800_UPDATE_SCR0800 INPUT.

*  IF NOT ZSIMIMG20- IS INITIAL.
*    CLEAR W_DEL_MARK.
*
*    MOVE-CORRESPONDING ZSIMIMG20 TO IT_ZSIMIMG20.
*
*    IF IT_ZSIMIMG20-ZFCFRATE IS INITIAL.
*      IT_ZSIMIMG20-ZFCFRATE = 1.
*    ENDIF.
*
*    IT_ZSIMIMG20-ZFNETPR = IT_ZSIMIMG20-NETPR * IT_ZSIMIMG20-ZFCFRATE.
**    IT_ZSIMIMG20-WAERS = 'KRW'.
*    MOVE : W_ROW_MARK      TO IT_ZSIMIMG20-ZFMARK.
*
*    CLEAR WA_ZSIMIMG20.
*    MOVE-CORRESPONDING IT_ZSIMIMG20 TO WA_ZSIMIMG20.
*    APPEND IT_ZSIMIMG20.
*    CLEAR : IT_ZSIMIMG20, ZSIMIMG20.
*  ENDIF.
ENDMODULE.                 " TC_0800_UPDATE_SCR0800  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ACCOUNT_CODE_SCR0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_ACCOUNT_CODE_SCR0500 INPUT.

  CHECK W_STATUS NE 'D'.

*  IF ZTIMIMG11-ZFIOCAC6 IS INITIAL.
*    SET CURSOR FIELD ZTIMIMG11-ZFIOCAC6.
*    MESSAGE W167 WITH 'Customs VAT Account No.'.
*  ENDIF.
*
*  IF ZTIMIMG11-ZFIOCAC19 IS INITIAL.
*    SET CURSOR FIELD ZTIMIMG11-ZFIOCAC19.
*    MESSAGE W167 WITH 'Contaner Tax Account No.'.
*  ENDIF.

ENDMODULE.                 " CHECK_ACCOUNT_CODE_SCR0500  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0002  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0002 INPUT.

  CASE SY-UCOMM.
    WHEN 'PICK'.
      READ TABLE IT_ZSIMIMG08 WITH KEY ZFCDTY = ZTIMIMG08-ZFCDTY
                                       ZFCD   = ZTIMIMG08-ZFCD.
      IF SY-SUBRC EQ 0.
        MOVE SY-TABIX  TO TOP_LINE.
        MOVE 'Y'       TO POSI_GB.
      ELSE.
        MOVE '1'       TO TOP_LINE.
        MOVE 'N'       TO POSI_GB.
      ENDIF.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'CANC'.
      MOVE '1'       TO TOP_LINE.
      MOVE 'N'       TO POSI_GB.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " MODULE GET_OK_CODE_SCR0002 INPUT.
*&---------------------------------------------------------------------*
*&      Module  CHECK_TRANS_PAYMENT_SCR0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_TRANS_PAYMENT_SCR0800 INPUT.

*  IF ZSIMIMG20-NETPR IS INITIAL.
*    MESSAGE E977 WITH '단가는 필수 항목입니다.'.
*  ENDIF.


ENDMODULE.                 " CHECK_TRANS_PAYMENT_SCR0800  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_TRANS_KEY_SCR0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_TRANS_KEY_SCR0800 INPUT.

*  IF ZTIMIMG20-ZFYEAR IS INITIAL.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG20' 'ZFYEAR'.
*  ENDIF.
*
*  IF ZTIMIMG20-WERKS IS INITIAL.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG20' 'WERKS'.
*  ENDIF.
*
*  IF ZTIMIMG20-ZFTRGB IS INITIAL.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG20' 'ZFTRGB'.
*  ENDIF.
*
*  IF W_STATUS EQ 'I'.
*    SELECT SINGLE * INTO W_ZTIMIMGTX
*           FROM ZTIMIMG20
*           WHERE ZFYEAR EQ ZTIMIMG20-ZFYEAR
*             AND WERKS  EQ ZTIMIMG20-WERKS
*             AND ZFTRGB EQ ZTIMIMG20-ZFTRGB.
*
*    IF SY-SUBRC EQ 0.
*      MESSAGE E977
*           WITH '헤더정보에 해당하는 데이타가 이미 존재합니다.'.
*    ENDIF.
*  ENDIF.

ENDMODULE.                 " CHECK_TRANS_KEY_SCR0800  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSIMIMG20_CLEAR_SCR0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSIMIMG20_CLEAR_SCR0800 INPUT.

  CLEAR : IT_ZSIMIMG20, ZSIMIMG20.
  REFRESH IT_ZSIMIMG20.

ENDMODULE.                 " IT_ZSIMIMG20_CLEAR_SCR0800  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_ITEM_SCR0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_ITEM_SCR0800 INPUT.

*  IF ZSIMIMG20-ZFWTTO   IS INITIAL AND
*     ZSIMIMG20-NETPR    IS INITIAL AND
*     ZSIMIMG20-ZFCFRATE IS INITIAL .
*    EXIT.
*  ENDIF.
*
*  IF ZSIMIMG20-ZFWTTO IS INITIAL
*     AND NOT ZSIMIMG20-NETPR IS INITIAL.
*    ZSIMIMG20-ZFWTTO = 999.
*  ENDIF.
*
**>> 중량 CHECK.
*  IF TC_0800-CURRENT_LINE > 1.
*    IF WA_ZSIMIMG20-ZFWTTO GE ZSIMIMG20-ZFWTTO.
*      MESSAGE E977 WITH '무게 순으로 정리되어 있지 않습니다.'.
*    ELSE.
*      ZSIMIMG20-ZFWTFR = WA_ZSIMIMG20-ZFWTTO.
*    ENDIF.
*  ENDIF.


ENDMODULE.                 " CHECK_ITEM_SCR0800  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CONFRATE_SCR0800  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_CONFRATE_SCR0800 INPUT.
*  IF NOT ZSIMIMG20-ZFCFRATE IS INITIAL.
*    IF ZSIMIMG20-ZFCFRATE GT 1.
*      MESSAGE E977 WITH '낙찰률은 1.0 보다 클 수 없습니다.'.
*    ENDIF.
*  ENDIF.
ENDMODULE.                 " CHECK_CONFRATE_SCR0800  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR2100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR2100 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_2100-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR2100  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_CHECK_SCR2100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FIELD_CHECK_SCR2100 INPUT.

   " 회사코드.
   IF ZTIMIMG21-BUKRS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG21' 'BUKRS'.
   ENDIF.

   " 적용일.
   IF ZTIMIMG21-ZFAPLDT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG21' 'ZFAPLDT'.
   ENDIF.

   " 외자구분.
   IF ZTIMIMG21-ZFRAGB IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG21' 'ZFRAGB'.
   ENDIF.

ENDMODULE.                 " FIELD_CHECK_SCR2100  INPUT
*&---------------------------------------------------------------------*
*&      Module  KEY_FIELD_CHECK_SCR2100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KEY_FIELD_CHECK_SCR2100 INPUT.

   " 코드구분.
   IF ZSIMIMG21-ZFCD  IS INITIAL.
      MESSAGE  E067(ZIM1).
   ENDIF.

   " 기본단위.
   IF ZSIMIMG21-MEINS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG21' 'MEINS'.
   ENDIF.

   " 통화코드.
   IF ZSIMIMG21-WAERS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG21' 'WAERS'.
   ENDIF.

ENDMODULE.                 " KEY_FIELD_CHECK_SCR2100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_2100_UPDATE_SCR2100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_2100_UPDATE_SCR2100 INPUT.

*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG21 INDEX TC_2100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSIMIMG21 TO IT_ZSIMIMG21.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG21-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG21-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_ZSIMIMG21-ZFMARK.

*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG21-ZFMINBS
                                             IT_ZSIMIMG21-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG21-ZF45KLT
                                             IT_ZSIMIMG21-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG21-ZF100KLT
                                             IT_ZSIMIMG21-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG21-ZF300KLT
                                             IT_ZSIMIMG21-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG21-ZF500KLT
                                             IT_ZSIMIMG21-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG21-ZF1000KLT
                                             IT_ZSIMIMG21-WAERS.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG21-ZF4000KLT
                                             IT_ZSIMIMG21-WAERS.

*-----------------------------------------------------------------------

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSIMIMG21   INDEX W_TABIX.
  ELSE.
    APPEND IT_ZSIMIMG21.
  ENDIF.

ENDMODULE.                 " TC_2100_UPDATE_SCR2100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0210_UPDATE_SCR0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0210_UPDATE_SCR0210 INPUT.

  READ TABLE IT_0210 INDEX TC_0210-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZTIMIMG21 TO IT_0210.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_0210-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_0210-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_0210-ZFMARK.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_0210   INDEX W_TABIX.
  ELSE.
    APPEND IT_0210.
  ENDIF.

ENDMODULE.                 " TC_0210_UPDATE_SCR0210  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0210 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'CHDC'.
    WHEN 'NEWL'.
      MOVE 'I' TO W_STATUS.
      CLEAR   : ZTIMIMG21, *ZTIMIMG21, IT_ZSIMIMG21.
      REFRESH : IT_ZSIMIMG21.
      SET SCREEN 2100. LEAVE TO SCREEN 2100.
* 데이타 삭제..
    WHEN 'DELT'.
      READ TABLE IT_0210 WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
         PERFORM P2000_SET_MESSAGE USING  OK-CODE.
         CHECK ANTWORT EQ 'Y'.

         SELECT * FROM  ZTIMIMG21
                  WHERE BUKRS    EQ  IT_0210-BUKRS
                  AND   ZFAPLDT  EQ  IT_0210-ZFAPLDT
                  AND   ZFRAGB   EQ  IT_0210-ZFRAGB.
            PERFORM   P3000_ZTIMIMG21_CHANGE_DOC    USING  'D'.
         ENDSELECT.

         DELETE FROM  ZTIMIMG21
                WHERE BUKRS      EQ  IT_0210-BUKRS
                AND   ZFAPLDT    EQ  IT_0210-ZFAPLDT
                AND   ZFRAGB     EQ  IT_0210-ZFRAGB.

         IF SY-SUBRC NE 0.
            ROLLBACK WORK.
         ELSE.
            MESSAGE S339.
         ENDIF.
      ENDIF.
    WHEN 'DETA' OR 'COPY'.
      READ TABLE IT_0210 WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
         MOVE : IT_0210-BUKRS   TO  ZTIMIMG21-BUKRS,
                IT_0210-ZFAPLDT TO  ZTIMIMG21-ZFAPLDT,
                IT_0210-ZFRAGB  TO  ZTIMIMG21-ZFRAGB.

         IF SY-UCOMM EQ 'DETA'.
           MOVE 'D' TO W_STATUS.
         ELSE.
           MOVE 'I' TO W_STATUS.
         ENDIF.

        PERFORM  P1000_READ_ZTIMIMG21.
        IF SY-UCOMM EQ 'COPY'.
           CLEAR : ZTIMIMG21-BUKRS, ZTIMIMG21-ZFAPLDT, ZTIMIMG21-ZFRAGB.
        ENDIF.
        SET SCREEN 2100.  LEAVE SCREEN.
      ELSE.
        MESSAGE S951.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0210  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0220_UPDATE_SCR0220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0220_UPDATE_SCR0220 INPUT.

  READ TABLE IT_0220 INDEX TC_0220-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZTIMIMG22 TO IT_0220.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_0220-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_0220-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_0220-ZFMARK.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_0220   INDEX W_TABIX.
  ELSE.
    APPEND IT_0220.
  ENDIF.

ENDMODULE.                 " TC_0220_UPDATE_SCR0220  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0220 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'CHDC'.
    WHEN 'NEWL'.
      MOVE 'I' TO W_STATUS.
      CLEAR   : ZTIMIMG22, *ZTIMIMG22, IT_ZSIMIMG22.
      REFRESH : IT_ZSIMIMG22.
      SET SCREEN 2200. LEAVE TO SCREEN 2200.
    WHEN 'DELT'.
      READ TABLE IT_0220 WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
         PERFORM P2000_SET_MESSAGE USING  OK-CODE.
         CHECK ANTWORT EQ 'Y'.

         SELECT * FROM  ZTIMIMG22
                  WHERE BUKRS    EQ  IT_0220-BUKRS
                  AND   ZFAPLDT  EQ  IT_0220-ZFAPLDT.
            PERFORM   P3000_ZTIMIMG22_CHANGE_DOC    USING  'D'.
         ENDSELECT.

         DELETE FROM  ZTIMIMG22
                WHERE BUKRS      EQ  IT_0220-BUKRS
                AND   ZFAPLDT    EQ  IT_0220-ZFAPLDT.

         IF SY-SUBRC NE 0.
            ROLLBACK WORK.
         ELSE.
            MESSAGE S339.
         ENDIF.
      ENDIF.
    WHEN 'DETA' OR 'COPY'.
      READ TABLE IT_0220 WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
         MOVE : IT_0220-BUKRS   TO  ZTIMIMG22-BUKRS,
                IT_0220-ZFAPLDT TO  ZTIMIMG22-ZFAPLDT.

         IF SY-UCOMM EQ 'DETA'.
           MOVE 'D' TO W_STATUS.
         ELSE.
           MOVE 'I' TO W_STATUS.
         ENDIF.

        PERFORM  P1000_READ_ZTIMIMG22.
        IF SY-UCOMM EQ 'COPY'.
           CLEAR : ZTIMIMG22-BUKRS, ZTIMIMG22-ZFAPLDT.
        ENDIF.
        SET SCREEN 2200.  LEAVE SCREEN.
      ELSE.
        MESSAGE S951.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0220  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR2200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR2200 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_2200-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR2200  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_CHECK_SCR2200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FIELD_CHECK_SCR2200 INPUT.

   " Company Code
   IF ZTIMIMG22-BUKRS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG22' 'BUKRS'.
   ENDIF.

   " Apply Date
   IF ZTIMIMG22-ZFAPLDT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG22' 'ZFAPLDT'.
   ENDIF.

ENDMODULE.                 " FIELD_CHECK_SCR2200  INPUT
*&---------------------------------------------------------------------*
*&      Module  KEY_FIELD_CHECK_SCR2200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KEY_FIELD_CHECK_SCR2200 INPUT.

   IF NOT ( ZSIMIMG22-ZF20MLB IS INITIAL AND
            ZSIMIMG22-ZF40MLB IS INITIAL AND
            ZSIMIMG22-ZFLCL   IS INITIAL ).
      IF ZSIMIMG22-ZFCD IS INITIAL.
         MESSAGE  E216(ZIM1).
      ENDIF.
   ENDIF.

ENDMODULE.                 " KEY_FIELD_CHECK_SCR2200  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_2200_UPDATE_SCR2200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_2200_UPDATE_SCR2200 INPUT.

*-----------------------------------------------------------------------
* Display MODE MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG22 INDEX TC_2200-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSIMIMG22 TO IT_ZSIMIMG22.

  IF IT_ZSIMIMG22-ZFCD IS INITIAL. EXIT. ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG22-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG22-LOEKZ.
  ENDIF.
  MOVE : W_ROW_MARK      TO IT_ZSIMIMG22-ZFMARK.

*-----------------------------------------------------------------------
* External ==> Internal
*-----------------------------------------------------------------------
  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG22-ZFLCL
                                             IT_ZSIMIMG22-ZFUSD.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG22-ZF20MLB
                                             IT_ZSIMIMG22-ZFUSD.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG22-ZF40MLB
                                             IT_ZSIMIMG22-ZFUSD.

  PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSIMIMG22-ZF45MLB
                                             IT_ZSIMIMG22-ZFUSD.
*-----------------------------------------------------------------------

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSIMIMG22   INDEX W_TABIX.
  ELSE.
    APPEND IT_ZSIMIMG22.
  ENDIF.

ENDMODULE.                 " TC_2200_UPDATE_SCR2200  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_CD_TO_POINT_SCR2200  INPUT
*&---------------------------------------------------------------------*
MODULE HELP_CD_TO_POINT_SCR2200 INPUT.

DATA : L_DISPLAY.

   REFRESH : IT_ZFCD_HELP.
   SELECT *
          FROM   ZTIMIMG08
          WHERE  ZFCDTY   EQ   '014'.
      MOVE : ZTIMIMG08-ZFCD   TO   IT_ZFCD_HELP-ZFCD,
             ZTIMIMG08-ZFCDNM TO   IT_ZFCD_HELP-ZFCDNM.
      APPEND IT_ZFCD_HELP.
   ENDSELECT.

   IF SY-SUBRC NE 0.
      MESSAGE S406.
      EXIT.
   ENDIF.

   DYNPROG = SY-REPID.
   DYNNR   = SY-DYNNR.

   WINDOW_TITLE = 'Loading Area of Ocean Freight'.
   CONCATENATE WINDOW_TITLE 'Code HELP' INTO WINDOW_TITLE
               SEPARATED BY SPACE.

   IF W_STATUS EQ C_REQ_D.
      L_DISPLAY = 'X'.
   ELSE.
      CLEAR: L_DISPLAY.
   ENDIF.

   CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
*                RETFIELD        = 'OTYPE'
                RETFIELD        = 'ZFCD'
                DYNPPROG        = DYNPROG
                DYNPNR          = DYNNR
                DYNPROFIELD     = 'ZSIMIMG22-ZFCD'
                WINDOW_TITLE    = WINDOW_TITLE
                VALUE_ORG       = 'S'
                DISPLAY         = L_DISPLAY
        TABLES
                VALUE_TAB       = IT_ZFCD_HELP
        EXCEPTIONS
                PARAMETER_ERROR = 1
                NO_VALUES_FOUND = 2
                OTHERS          = 3.

   IF SY-SUBRC <> 0.
      EXIT.
   ENDIF.

ENDMODULE.                 " HELP_CD_TO_POINT_SCR2200  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_CD_TO_POINT_SCR2100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_CD_TO_POINT_SCR2100 INPUT.

   REFRESH : IT_ZFCD_HELP.
   SELECT *
*          INTO CORRESPONDING FIELDS OF TABLE IT_BLSDP_HELP
          FROM   ZTIMIMG08
          WHERE  ZFCDTY   EQ   '015'.
      MOVE : ZTIMIMG08-ZFCD   TO   IT_ZFCD_HELP-ZFCD,
             ZTIMIMG08-ZFCDNM TO   IT_ZFCD_HELP-ZFCDNM.
      APPEND IT_ZFCD_HELP.
   ENDSELECT.

   IF SY-SUBRC NE 0.
      MESSAGE S406.
      EXIT.
   ENDIF.

   DYNPROG = SY-REPID.
   DYNNR   = SY-DYNNR.

   WINDOW_TITLE = 'Application Area of Air Freight'.
   CONCATENATE WINDOW_TITLE 'Code HELP' INTO WINDOW_TITLE
               SEPARATED BY SPACE.

   IF W_STATUS EQ C_REQ_D.
      L_DISPLAY = 'X'.
   ELSE.
      CLEAR: L_DISPLAY.
   ENDIF.

   CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
                RETFIELD        = 'ZFCD'
                DYNPPROG        = DYNPROG
                DYNPNR          = DYNNR
                DYNPROFIELD     = 'ZSIMIMG22-ZFCD'
                WINDOW_TITLE    = WINDOW_TITLE
                VALUE_ORG       = 'S'
                DISPLAY         = L_DISPLAY
        TABLES
                VALUE_TAB       = IT_ZFCD_HELP
        EXCEPTIONS
                PARAMETER_ERROR = 1
                NO_VALUES_FOUND = 2
                OTHERS          = 3.

   IF SY-SUBRC <> 0.
      EXIT.
   ENDIF.

ENDMODULE.                 " HELP_CD_TO_POINT_SCR2100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_NOMONEY_BIT_SCR0500  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_NOMONEY_BIT_SCR0500 INPUT.

   IF W_STATUS EQ 'D'. EXIT.  ENDIF.

   IF ZTIMIMG11-ZFNMPR IS INITIAL.
      SET CURSOR FIELD ZTIMIMG11-ZFNMPR.
      MESSAGE  E167  WITH 'Processing method for expense of reimports'.
   ENDIF.

   IF ZTIMIMG11-ZFNMPR EQ '1'.
      IF ZTIMIMG11-ZFIOCAC20 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC20.
         MESSAGE  E167  WITH 'Head office - (Sample)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC14 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC14.
         MESSAGE  E167  WITH 'Head office - (Compen.)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC15 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC15.
         MESSAGE  E167  WITH 'Head office - (replace)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC16 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC16.
         MESSAGE  E167  WITH 'Head office - (repair)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC17 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC17.
         MESSAGE  E167  WITH 'Head office - (S.B.)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC18 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC18.
         MESSAGE  E167  WITH 'Head office - (Others)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC30 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC30.
         MESSAGE  E167  WITH 'Plant - (Sample)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC24 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC24.
         MESSAGE  E167  WITH 'Plant - (replace)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC25 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC25.
         MESSAGE  E167  WITH 'Plant - (Compensation)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC26 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC26.
         MESSAGE  E167  WITH 'Plant - (repair)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC27 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC27.
         MESSAGE  E167  WITH 'Plant - (Ship.Back)'.
      ENDIF.
      IF ZTIMIMG11-ZFIOCAC28 IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFIOCAC28.
         MESSAGE  E167  WITH 'Plant - (Others)'.
      ENDIF.
   ENDIF.

ENDMODULE.                 " CHECK_NOMONEY_BIT_SCR0500  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_VENDOR_BIT_SCR0500  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_VENDOR_BIT_SCR0500 INPUT.

   IF W_STATUS  EQ  'D'.  EXIT.  ENDIF.

   IF NOT ZTIMIMG11-ZFVNCT IS INITIAL.
      IF ZTIMIMG11-ZFBOSE IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFBOSE.
         MESSAGE  E167  WITH 'Trucker'.
         EXIT.
      ENDIF.
      IF ZTIMIMG11-ZFHAYEK IS INITIAL.
         SET CURSOR FIELD ZTIMIMG11-ZFHAYEK.
         MESSAGE  E167  WITH 'Loading/Unloading Comp'.
         EXIT.
      ENDIF.
   ENDIF.

ENDMODULE.                 " CHECK_VENDOR_BIT_SCR0500  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INBOUND_DELIVERY  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INBOUND_DELIVERY INPUT.

 IF ZTIMIMG00-ZFIBYN IS INITIAL.
    CLEAR  :  ZTIMIMG00-ZFIBBD .
 ELSE.
   IF ZTIMIMG00-ZFIBBD IS INITIAL.
      MESSAGE E167 WITH 'Base date of inbound delivery'.
   ENDIF.
 ENDIF.
ENDMODULE.                 " CHECK_INBOUND_DELIVERY  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_FIX_BUKRS_METHOD  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_FIX_BUKRS_METHOD INPUT.

  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      IF ZTIMIMG00-ZFBUKRS IS INITIAL.
         MESSAGE  E543.
         EXIT.
      ENDIF.
   ENDIF.

ENDMODULE.                 " CHECK_FIX_BUKRS_METHOD  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INSCOMP_NAME_SCR0500  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INSCOMP_NAME_SCR0500 INPUT.

   CLEAR : W_INSCOMP_NM.
   SELECT SINGLE NAME1 INTO W_INSCOMP_NM
   FROM   LFA1
   WHERE  LIFNR EQ   ZTIMIMG11-ZFINSCP.

ENDMODULE.                 " CHECK_INSCOMP_NAME_SCR0500  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR2000  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR2000 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_2000-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR2000  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_2000_UPDATE_SCR2000  INPUT
*&---------------------------------------------------------------------*
MODULE TC_2000_UPDATE_SCR2000 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG20 INDEX TC_2000-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

* IF ( ZTIMIMG01-KTOKK IS INITIAL ) AND ( ZTIMIMG01-ZTERM IS INITIAL ).
  IF ZSIMIMG01-ZTERM   IS INITIAL AND
     ZSIMIMG01-BSTYP   IS INITIAL AND
     ZSIMIMG01-BSART   IS INITIAL AND
     ZSIMIMG01-ZFAPLDT IS INITIAL.
    MOVE 'D' TO W_DEL_MARK.
  ENDIF.

  MOVE-CORRESPONDING ZSIMIMG20 TO IT_ZSIMIMG20.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG20-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG20-LOEKZ.
  ENDIF.

  MOVE : W_ROW_MARK      TO IT_ZSIMIMG20-ZFMARK.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG20 INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG20.
  ENDIF.

ENDMODULE.                 " TC_2000_UPDATE_SCR2000  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_CHECK_SCR2000  INPUT
*&---------------------------------------------------------------------*
MODULE FIELD_CHECK_SCR2000 INPUT.

   IF ZSIMIMG20-BUKRS IS INITIAL.
      MOVE 'H201' TO ZSIMIMG20-BUKRS.
   ENDIF.

   SELECT SINGLE *
     FROM T001
    WHERE BUKRS = ZSIMIMG20-BUKRS.

   IF ZSIMIMG20-ZFUSD IS INITIAL.
      ZSIMIMG20-ZFUSD = T001-WAERS.
   ENDIF.

   IF ZSIMIMG20-ZFLCL  IS INITIAL AND
      ZSIMIMG20-ZF20FT IS INITIAL AND
      ZSIMIMG20-ZF40FT IS INITIAL.
      MESSAGE I977 WITH 'Input the inland freight net price.'.
   ENDIF.

ENDMODULE.                 " FIELD_CHECK_SCR2000  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_2300_UPDATE_SCR2300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_2300_UPDATE_SCR2300 INPUT.

  READ TABLE IT_ZSIEPORT INDEX TC_2300-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  W_OLD_SUBRC = SY-SUBRC.

  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZTIEPORT TO IT_ZSIEPORT.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIEPORT-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIEPORT-LOEKZ.
  ENDIF.

  MOVE : W_ROW_MARK      TO IT_ZSIEPORT-ZFMARK.

  IF  W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIEPORT   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIEPORT.
  ENDIF.

ENDMODULE.                 " TC_2300_UPDATE_SCR2300  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR2300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR2300 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_2300-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR2300  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSIMIMG07_AMOUNT_CHECK_SCR1  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSIMIMG07_CHECK_SCR1400 INPUT.
  IF ZSIMIMG07-ZFADAMT IS INITIAL.
    IF NOT ZSIMIMG07-ZFSTHS IS INITIAL.
      MESSAGE E463 WITH 'Add Amount'.
    ENDIF.
  ENDIF.
ENDMODULE.                 " IT_ZSIMIMG07_AMOUNT_CHECK_SCR1  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSIMIMG24_CHECK.
*&---------------------------------------------------------------------*
MODULE IT_ZSIMIMG24_CHECK INPUT.
  IF ZSIMIMG24-MATNR IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIMIMG24' 'MATNR'.
  ELSE.
    SELECT SINGLE *
             FROM MARA
            WHERE MATNR = ZSIMIMG24-MATNR.
    IF SY-SUBRC NE 0.
      MESSAGE E977 WITH 'Check the Part Number(Material Code)!'.
    ENDIF.
    SELECT SINGLE MAKTX
             INTO ZSIMIMG24-MAKTX
             FROM MAKT
            WHERE MATNR = ZSIMIMG24-MATNR
              AND SPRAS = SY-LANGU.
  ENDIF.

ENDMODULE.                 " IT_ZSIMIMG24_CHECK INPUT.
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR2500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR2500 INPUT.

  GET CURSOR LINE LINE FIELD F.
  LINE = TC_2500-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR2500  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_2500_UPDATE_SCR2500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_2500_UPDATE_SCR2500 INPUT.

*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSIMIMG23 INDEX TC_2500-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  W_OLD_SUBRC = SY-SUBRC.

  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZTIMIMG23 TO IT_ZSIMIMG23.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELE'.
    MOVE : 'X'             TO W_DEL_MARK,
           W_DEL_MARK      TO IT_ZSIMIMG23-LOEKZ.
  ENDIF.

  IF NOT ( W_ROW_MARK IS INITIAL ) AND OK-CODE = 'DELC'.
    MOVE : ' '             TO W_DEL_MARK,
           ' '             TO IT_ZSIMIMG23-LOEKZ.
  ENDIF.

  MOVE : W_ROW_MARK      TO IT_ZSIMIMG23-ZFMARK.

  IF  W_SY_SUBRC EQ 0.
    MODIFY IT_ZSIMIMG23   INDEX W_TABIX.
  ELSE.
    APPEND  IT_ZSIMIMG23.
  ENDIF.

ENDMODULE.                 " TC_2500_UPDATE_SCR2500  INPUT
