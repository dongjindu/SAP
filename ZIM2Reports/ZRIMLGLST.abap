*&---------------------------------------------------------------------*
*& Report  ZRIMLGLST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : L/G 현황 List 조회                                    *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.01.03                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&  L/G 입수내역을 조회한다.
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
REPORT  ZRIMLGLST   MESSAGE-ID ZIM
                     LINE-SIZE 127
                     NO STANDARD PAGE HEADING.

TABLES : ZTBL,ZTLG,ZTREQHD,LFA1, ZTIMIMG00.

*-----------------------------------------------------------------------
* L/G 입수내역 리스트용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFBLNO     LIKE ZTBL-ZFBLNO,              "B/L DOCUMENT NO.
       ZFHBLNO    LIKE ZTBL-ZFHBLNO,             "HOUSE  B/L
       ZFMBLNO    LIKE ZTBL-ZFMBLNO,             "MASTER B/L
       ZFREBELN   LIKE ZTBL-ZFREBELN,            "대표 P/O No
       ZFFORD     LIKE ZTBL-ZFFORD,              "Forwarder
       NAME1(16)  TYPE C,                        "Forwarder NAME
       ZFCARNM    LIKE ZTBL-ZFCARNM,             "선기명.
       ZFETD      LIKE ZTBL-ZFETD,               "출항일(ETD)
       ZFETA      LIKE ZTBL-ZFETA,               "도착일(ETA)
       ZFSPRT     LIKE ZTBL-ZFSPRT,              "선적항.
       ZFAPRT     LIKE ZTBL-ZFAPRT,              "도착항.
       ZFLGAMT    LIKE ZTLG-ZFCIAM,              "L/G 금액.
       ZFLGAMC    LIKE ZTLG-ZFCIAMC,             "L/G 금액 통화.
       ZFEXRT     LIKE ZTBL-ZFEXRT,              "Exchange rate
       ZFTRCK     LIKE ZTBL-ZFTRCK,              "Trucker
       NAME2(16)  TYPE C,                        "Trucker NAME
       ZFLGSEQ    LIKE ZTLG-ZFLGSEQ,             "L/G 일련번호.
       ZFLGINO    LIKE ZTLG-ZFLGINO,             "L/G 번호.
       ZFDCNO     LIKE ZTLG-ZFDCNO,              "신용장번호.
       ZFREQNO    LIKE ZTLG-ZFREQNO,             "수입의뢰관리번호.
       ZFAPDT     LIKE ZTLG-ZFAPDT,              "신청일.
       ZFLGIDT    LIKE ZTLG-ZFLGIDT,             "발급일.
       ZFISBNM    LIKE ZTLG-ZFISBNM,             "L/G 발급은행.
       ZFOPBN     LIKE ZTREQHD-ZFOPBN,           "L/C 개설은행CODE.
       NAME3(16)  TYPE C.                        "L/C 개설은행명.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMLGLSTTOP.
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS     FOR ZTBL-BUKRS NO-EXTENSION
                                           NO INTERVALS,
                S_HBLNO   FOR ZTBL-ZFHBLNO,     "HOUSE B/L NO
                S_DCNO    FOR ZTLG-ZFDCNO,      "신용장 개설번호.
                S_LGINO   FOR ZTLG-ZFLGINO.     "L/G 번호.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS    : P_YES    AS CHECKBOX DEFAULT  SPACE,
                P_NO     AS CHECKBOX DEFAULT  'X'.
SELECTION-SCREEN END OF BLOCK B2.

* PARAMETER 초기값 Setting
INITIALIZATION.                          "초기값 SETTING
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.          "더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
  PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*  테이블 SELECT
  PERFORM   P1000_GET_IT_TAB           USING W_ERR_CHK.
  DESCRIBE TABLE IT_TAB LINES W_COUNT .
  IF W_COUNT = 0.
    MESSAGE S966.  EXIT.
  ENDIF.
* 레포트 관련 Text Table SELECT
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.         " SORT 선택.
      W_FIELD_NM = 'ZFBLHNO'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
* 전체 선택 및 선택해제.
    WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
    WHEN 'DISP'.          " B/L 조?
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_LG USING IT_SELECTED-ZFBLNO
                                    IT_SELECTED-ZFLGSEQ.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'REFR'.
* 테이블 SELECT
      PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
      PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
      PERFORM RESET_LIST.
    WHEN 'MANU' OR 'LGP'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_MANUAL_OPEN USING IT_SELECTED-ZFBLNO
                                        IT_SELECTED-ZFLGSEQ.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM82'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /53  ' [  L/G Document List Display  ] '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 108 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,(1) ' ',
            SY-VLINE,(36) 'HOUSE B/L No',
            SY-VLINE,(10) 'L/G SEQ',
            SY-VLINE,(10) 'App. Date',
            SY-VLINE,(16) 'L/G No',
            SY-VLINE,(35) 'Issuing Bank',
            SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE,(1) ' ',
            SY-VLINE,(36) 'L/C(Contract) No.',
            SY-VLINE,(10) 'REQ No',
            SY-VLINE,(10) 'Issue Date',
            SY-VLINE,(16) 'L/G Amount',
            SY-VLINE,(35) 'Openning Bank',
            SY-VLINE.
  WRITE : / SY-ULINE.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-ZFFORD
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    MOVE: LFA1-NAME1   TO   IT_TAB-NAME1.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-ZFTRCK
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    MOVE: LFA1-NAME1   TO   IT_TAB-NAME2.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-ZFOPBN
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    MOVE: LFA1-NAME1   TO   IT_TAB-NAME3.
    MODIFY  IT_TAB INDEX W_TABIX.
  ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'ZIM82'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIM82'.           " GUI TITLE SETTING..

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_TAB.
    W_LINE = W_LINE + 1.
    PERFORM P2000_PAGE_CHECK.
    PERFORM P3000_LINE_WRITE.

    AT LAST.
      PERFORM P3000_LAST_WRITE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

  IF W_LINE >= 53.
    WRITE : / SY-ULINE.
    W_PAGE = W_PAGE + 1.    W_LINE = 0.
    NEW-PAGE.
  ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE : / 'Total:', W_COUNT.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE:/   SY-VLINE, MARKFIELD AS CHECKBOX,
            SY-VLINE, (36) IT_TAB-ZFHBLNO,"24
            SY-VLINE, (10) IT_TAB-ZFLGSEQ,
            SY-VLINE, (10) IT_TAB-ZFAPDT ,
            SY-VLINE, (16) IT_TAB-ZFLGINO,
            SY-VLINE, (35) IT_TAB-ZFISBNM,
            SY-VLINE.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
* hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.

  WRITE:/    SY-VLINE,(1) ' ',
             SY-VLINE,(36) IT_TAB-ZFDCNO,
             SY-VLINE,(10) IT_TAB-ZFREQNO,"10
             SY-VLINE,(10) IT_TAB-ZFLGIDT,
             SY-VLINE,(16) IT_TAB-ZFLGAMT CURRENCY IT_TAB-ZFLGAMC,
             SY-VLINE,(35) IT_TAB-NAME3,
             SY-VLINE.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
* stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.
  WRITE : / SY-ULINE.
ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LG
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LG USING    P_ZFBLNO
                            P_ZFLGSEQ.
  SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPLGSEQ' FIELD P_ZFLGSEQ.
  EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
  EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.

  CALL TRANSACTION 'ZIM28' AND SKIP  FIRST SCREEN.

* 테이블 SELECT
  PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
  PERFORM RESET_LIST.
ENDFORM.                    " P2000_SHOW_LG
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_TAB USING W_ERR_CHK.

  W_ERR_CHK = 'N'.
  REFRESH : IT_TAB.
  SELECT * FROM ZTLG
           WHERE ZFHBLNO IN S_HBLNO AND
                 ZFLGINO IN S_LGINO AND
                 ZFDCNO  IN S_DCNO.
    CLEAR IT_TAB.
    MOVE-CORRESPONDING ZTLG TO IT_TAB.
    IF IT_TAB-ZFLGINO IS INITIAL.
      IF P_YES =  'X' AND  P_NO = ' '. CONTINUE. ENDIF.
      IF P_YES EQ ' ' AND  P_NO = ' '. CONTINUE. ENDIF.
    ELSE.
      IF P_YES EQ ' ' AND  P_NO = 'X'. CONTINUE. ENDIF.
      IF P_YES EQ ' ' AND  P_NO = ' '. CONTINUE. ENDIF.
    ENDIF.
    APPEND IT_TAB.
  ENDSELECT.
  IF SY-SUBRC NE 0.W_ERR_CHK = 'Y'.ENDIF.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
*>> B/L
    SELECT SINGLE *  FROM ZTBL
                     WHERE ZFBLNO = IT_TAB-ZFBLNO
                       AND BUKRS  IN S_BUKRS.
    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING ZTBL TO IT_TAB.
    ELSE.
      DELETE IT_TAB INDEX W_TABIX.
      CONTINUE.
    ENDIF.
*>> 수입의?
    SELECT SINGLE * FROM ZTREQHD
                 WHERE ZFREQNO = IT_TAB-ZFREQNO.
    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING ZTREQHD TO IT_TAB.
    ENDIF.
    MODIFY IT_TAB INDEX W_TABIX.

  ENDLOOP.

ENDFORM.                    " P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.
  DATA: INDEX   TYPE P,
        ZFBLNO  LIKE ZTLG-ZFBLNO,
        ZFLGSEQ LIKE ZTLG-ZFLGSEQ.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFBLNO   TO ZFBLNO,
         IT_TAB-ZFLGSEQ  TO ZFLGSEQ.
  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFBLNO  TO IT_SELECTED-ZFBLNO,
             IT_TAB-ZFLGSEQ TO IT_SELECTED-ZFLGSEQ.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFBLNO  TO IT_SELECTED-ZFBLNO,
             ZFLGSEQ TO IT_SELECTED-ZFLGSEQ.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ELSE.
      MESSAGE S962.
    ENDIF.
  ENDIF.
ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_MANUAL_OPEN
*&---------------------------------------------------------------------*
FORM P2000_MANUAL_OPEN USING    P_ZFBLNO
                                P_ZFLGSEQ.

  DATA: SELTAB     TYPE TABLE OF RSPARAMS,
        SELTAB_WA  LIKE LINE OF SELTAB.

  SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
  SET PARAMETER ID 'ZPLGSEQ' FIELD  P_ZFLGSEQ.

  EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
  EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.

  IF  SY-UCOMM EQ 'MANU'.
    CALL TRANSACTION 'ZIM29' AND SKIP  FIRST SCREEN.
  ELSE.

    SUBMIT ZRIMLGPRT
            WITH P_ZFBLNO EQ P_ZFBLNO
            WITH P_LGSEQ  EQ P_ZFLGSEQ
            AND RETURN.
  ENDIF.
ENDFORM.                    " P2000_MANUAL_OPEN
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> 회사코드 SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
