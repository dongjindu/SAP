*----------------------------------------------------------------------*
*   INCLUDE ZRIM09I01                                                  *
*----------------------------------------------------------------------*
*&  프로그램명 : 보세창고 출고관련 PAI MODULE Include                  *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.17                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  IF W_STATUS EQ 'D'.
     SET SCREEN 0. LEAVE SCREEN.
  ELSE.
*-----------------------------------------------------------------------
* 각 초기화면 Number 추가.
*-----------------------------------------------------------------------
     CASE SY-DYNNR.
          WHEN '0100' OR '0200' OR '0300'.
                SET SCREEN 0. LEAVE SCREEN.
          WHEN OTHERS.
                PERFORM P2000_SET_MESSAGE USING  SY-UCOMM.
     ENDCASE.
  ENDIF.


ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT

*&---------------------------------------------------------------------*
*&      Module  REQNO_CHECK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE REQNO_CHECK_SCRCOM INPUT.

  PERFORM OK_CODE_0100.
  PERFORM OK_CODE_BACK_EXIT.
  W_COUNT = 1.
  ANTWORT = 'N'.
  W_ERR_CHK = 'N'.
* 입력값 CHECK.
  IF ZSIV-ZFHBLNO IS INITIAL AND    " B/L NO를 입력하지 않을 경우.
     ZSIV-ZFBLNO  IS INITIAL.       " B/L관리번호가 입력하지 않을 경우.
     IF ZSIV-ZFIVNO  IS INITIAL.    " 관리번호가 입력하지 않을 경우.
        MESSAGE E411.
     ENDIF.
  ENDIF.

*>> House B/L 번호를 입력 했을 경우.
  IF NOT ZSIV-ZFHBLNO IS INITIAL. " B/L NO를 입력했을 경우.

        SELECT COUNT( * ) INTO W_LINE
                       FROM ZTBL
                       WHERE ZFHBLNO EQ ZSIV-ZFHBLNO.
        IF W_LINE EQ 0.
           MESSAGE E305 WITH ZSIV-ZFHBLNO.
        ENDIF.

        SELECT COUNT( * ) INTO  W_COUNT
                          FROM   ZVBL_IV
                          WHERE  ZFHBLNO EQ ZSIV-ZFHBLNO.
        CASE W_COUNT.
           WHEN 0.    MESSAGE E679 WITH ZSIV-ZFHBLNO.
           WHEN 1.
             SELECT ZFIVNO INTO ZSIV-ZFIVNO UP TO 1 ROWS
                            FROM ZVBL_IV
                            WHERE ZFHBLNO EQ ZSIV-ZFHBLNO.
             ENDSELECT.
           WHEN OTHERS.
              PERFORM P2000_CC_DOC_ITEM_SELECT.
              IF ANTWORT NE 'Y'.
                PERFORM P200_LEAV_SCREEN.
              ENDIF.
              PERFORM P2000_SEARCH_FIELD_MOVE.
        ENDCASE.
        IF SY-TCODE EQ 'ZIMBG1'.
           PERFORM P2000_CHECK_VALUES  USING ZSIV-ZFIVNO.
        ENDIF.
        PERFORM   P1000_READ_BW_DOC.
        EXIT.
   ENDIF.
   IF NOT ZSIV-ZFBLNO IS INITIAL.
        SELECT COUNT( * ) INTO  W_COUNT
                          FROM  ZTIV
                          WHERE ZFBLNO EQ ZSIV-ZFBLNO.
        CASE W_COUNT.
           WHEN 0.     MESSAGE E679 WITH ZSIV-ZFBLNO.
           WHEN 1.
              SELECT ZFIVNO INTO ZSIV-ZFIVNO UP TO 1 ROWS
                            FROM ZTIV
                            WHERE ZFBLNO EQ ZSIV-ZFBLNO.
              ENDSELECT.
           WHEN OTHERS.
              PERFORM P2000_CC_DOC_ITEM_SELECT_1.
              IF ANTWORT NE 'Y'.
                 PERFORM P200_LEAV_SCREEN.
              ENDIF.
              PERFORM P2000_SEARCH_FIELD_MOVE.

       ENDCASE.
       IF SY-TCODE EQ 'ZIMBG1'.
          PERFORM P2000_CHECK_VALUES  USING ZSIV-ZFIVNO.
       ENDIF.
       PERFORM   P1000_READ_BW_DOC.
       EXIT.
  ENDIF.
  IF NOT ZSIV-ZFIVNO  IS INITIAL.    " 요청번호가 입력 경우.
     ANTWORT = 'N'.
     W_COUNT = '1'.
     IF SY-TCODE EQ 'ZIMBG1' OR  SY-TCODE EQ 'ZIMBG2'.
        PERFORM P2000_CHECK_VALUES  USING ZSIV-ZFIVNO.
     ENDIF.
     PERFORM   P1000_READ_BW_DOC.
     EXIT.
  ENDIF.

 ENDMODULE.                 " REQNO_CHECK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
MODULE D0014_LIST_CHECK_SCR0014 INPUT.

* list-processing
  LEAVE TO LIST-PROCESSING.
* list Write
  PERFORM P2000_DATA_LISTING.

ENDMODULE.                 " D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0100 INPUT.

  IF W_ERR_CHK NE 'Y'.
     CASE SY-DYNNR.
       WHEN '0100'.
           MOVE C_REQ_C TO W_STATUS.
       WHEN '0200'.
           MOVE C_REQ_U TO W_STATUS.
       WHEN '0300'.
           MOVE C_REQ_D TO W_STATUS.
     ENDCASE.
     SET SCREEN 0101.  LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0101_UPDATE_SCR0101
*&---------------------------------------------------------------------*
MODULE TC_0101_UPDATE_SCR0101.

  READ TABLE IT_ZSBWIT  INDEX TC_0101-CURRENT_LINE.
  W_TABIX = SY-TABIX.
  W_SUBRC = SY-SUBRC.

  IF W_STATUS = C_REQ_D AND W_SUBRC EQ 0.
     MOVE W_ROW_MARK TO IT_ZSBWIT-ZFMARK.
     MODIFY IT_ZSBWIT INDEX TC_0101-CURRENT_LINE.
     EXIT.
  ENDIF.

* Internal Table Read
  MOVE-CORRESPONDING ZSBWIT  TO IT_ZSBWIT.

*>> 기출고 수량 계산 다른 출고번호.
  SELECT SUM( GIMENGE ) INTO W_BWMENGE2
     FROM  ZTBWIT
     WHERE ZFIVNO  EQ IT_ZSBWIT-ZFIVNO
       AND ZFIVDNO EQ IT_ZSBWIT-ZFIVDNO
       AND ZFGISEQ NE IT_ZSBWIT-ZFGISEQ.

*>> 출고할수있는 수량체크.
  W_BWMENGE1 = W_BWMENGE2 + IT_ZSBWIT-GIMENGE.
  IF  IT_ZSBWIT-CCMENGE < W_BWMENGE1.
      MESSAGE E674.
  ENDIF.
*>> 기출고 수량 수정.
*  IT_ZSBWIT-BWMENGE2 = W_BWMENGE2 + IT_ZSBWIT-GIMENGE.
*>>잔량 수정.
  IT_ZSBWIT-BWMENGE1 = IT_ZSBWIT-CCMENGE  -
                      ( IT_ZSBWIT-GIMENGE  +   W_BWMENGE2 ) .  " 잔량.
*>> 출고량입력.
*  IF IT_ZSBWIT-GIMENGE IS INITIAL.
*     IT_ZSBWIT-GIMENGE = IT_ZSBWIT-BWMENGE1.
*  ENDIF.
  MOVE W_ROW_MARK TO IT_ZSBWIT-ZFMARK.
  IF W_SUBRC EQ 0.
     MODIFY IT_ZSBWIT INDEX TC_0101-CURRENT_LINE.
  ENDIF.

ENDMODULE.                 " TC_0101_UPDATE_SCR0101.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0101  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0101 INPUT.

 CASE OK-CODE.
    WHEN 'DEL1'.
          DELETE IT_ZSBWIT  WHERE ZFMARK EQ 'X'.
    WHEN 'REF1'.
          PERFORM   P1000_READ_BW_DOC.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE' OR 'PRI'.
          PERFORM  P2000_EXIT_PROCESS.
    WHEN 'HIST'.      ">헤더변경사항.
         PERFORM  P2000_HEADER_CHANGE_DOC.
    WHEN 'HIIT'.      ">아이템변경사항.
         PERFORM  P2000_SELECT_ITEM.     " 선택된 아이템.
         PERFORM  P2000_ITEM_CHANGE_DOC.
    WHEN 'MKA1' OR 'MKL1'.
         IF OK-CODE EQ 'MKA1'.
            MOVE 'X'  TO W_ROW_MARK.
         ELSE.
            CLEAR : W_ROW_MARK.
         ENDIF.
         LOOP AT IT_ZSBWIT.
            IT_ZSBWIT-ZFMARK = W_ROW_MARK.
            MODIFY IT_ZSBWIT INDEX  SY-TABIX.
         ENDLOOP.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0101  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0101 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0101-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_FORWRD  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_FORWRD INPUT.

  IF  ZTBWHD-ZFTRCO IS INITIAL.
      MESSAGE E675.
  ENDIF.
  IF ZTBWHD-ZFSENDER IS INITIAL.
     MESSAGE E977 WITH 'Input sender name!'.
  ENDIF.

ENDMODULE.                 " CHECK_FORWRD  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.

  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  AFI_PROCESS_SCR0101  INPUT
*&---------------------------------------------------------------------*
MODULE AFI_PROCESS_SCR0101 INPUT.

  PERFORM OK_CODE_0100.
  PERFORM OK_CODE_BACK_EXIT.

ENDMODULE.                 " AFI_PROCESS_SCR0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PKCN_TOWT  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_PKCN_TOWT INPUT.

  IF ZTBWHD-ZFPKCN      IS INITIAL
     OR ZTBWHD-ZFPKCNM  IS INITIAL
     OR ZTBWHD-ZFTOWT   IS INITIAL
     OR ZTBWHD-ZFTOWTM  IS INITIAL.
     MESSAGE W977 WITH 'Input packing No and total weight!'.
  ENDIF.

ENDMODULE.                 " CHECK_PKCN_TOWT  INPUT
