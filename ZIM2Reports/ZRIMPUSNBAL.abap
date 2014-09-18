*&---------------------------------------------------------------------*
*& Report  ZRIMPUSNBAL                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : The Usance Balance Status                             *
*&      작성자 : Chul-Woo Nam INFOLINK Ltd.                            *
*&      작성일 : 2003.10.23                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : The Usance Balance Status.
*&---------------------------------------------------------------------*
*&  Change His : Shin-Ho Na Modified at 2003.10.31
*&---------------------------------------------------------------------*
REPORT  ZRIMPUSNBAL MESSAGE-ID ZIM
                    LINE-SIZE 217
                    NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Declaration Part..
*-----------------------------------------------------------------------
TABLES :  BAPICURR,                            " BAPI에 대한 통화.
         *BAPICURR.                            " BAPI에 대한 통화.

DATA : DIGITS       TYPE I VALUE 20,
       W_CAL2       LIKE ZTPMTHD-ZFUSIT.       " 원화이자

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK         TYPE C,                    " 임시
       ZFPNNO       LIKE ZTPMTHD-ZFPNNO,       " Payment Notice 관리번호
       ZFPNBN       LIKE ZTPMTHD-ZFPNBN,       " 차입처
       W_ZFPNBN_NM(20) TYPE C,                 " 차입처명
       ZFOPNNO      LIKE ZTPMTHD-ZFOPNNO,      " L/C NO
       ZFHBLNO      LIKE ZTPMTHD-ZFHBLNO,      " BL NO
       BUDAT        LIKE ZTPMTHD-BUDAT,        " 발생일
       ZFPWDT       LIKE ZTPMTHD-ZFPWDT,       " 만기일
       ZFPYDT       LIKE ZTPMTHD-ZFPYDT,       " 결제완료
       ZFOPBN       LIKE ZTPMTHD-ZFOPBN,       " 개설은행
       ZFNTDT       LIKE ZTPMTHD-ZFNTDT,       " 통지일
       ZFDSDT       LIKE ZTPMTHD-ZFDSDT,       " 할인일
       ZFPYA        LIKE ZTPMTHD-ZFPYA,        " 반제여부
       ZFPNAM       LIKE ZTPMTHD-ZFPNAM,       " 외화원금
       ZFPNAMC      LIKE ZTPMTHD-ZFPNAMC,      " 외화원금 Currency
       ZFKRW        LIKE ZTPMTHD-ZFKRW,        " 원화통화
       ZFUSIT       LIKE ZTPMTHD-ZFUSIT,       " 외화이자
       ZFUSITC      LIKE ZTPMTHD-ZFUSITC,      " 외화이자 Currency
       ZFEXRT       LIKE ZTPMTHD-ZFEXRT,       " 환율
       FFACT        LIKE ZTPMTHD-FFACT,        " "원시" 통화단위환율
       ZFUSITR      LIKE ZTPMTHD-ZFUSITR,      " 이자율
       ZFMOA        LIKE ZTPMTHD-ZFMOA,        " 원화환산금액
       EBELN        LIKE ZTPMTHD-EBELN.        " PO No
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMPAYMTOP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모듈

*-----------------------------------------------------------------------
* Selection Screen.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  S_PNNO   FOR ZTPMTHD-ZFPNNO,  " Payment Notice 관리번호
                 S_PNBN   FOR ZTPMTHD-ZFPNBN,  " 통지은행
                 S_OPBN   FOR ZTPMTHD-ZFOPBN,  " 개설은행
                 S_OPNNO  FOR ZTPMTHD-ZFOPNNO, " L/C No
                 S_EBELN  FOR ZTPMTHD-EBELN,   " P/O No
                 S_NTDT   FOR ZTPMTHD-ZFNTDT,  " 통지일
                 S_PWDT   FOR ZTPMTHD-ZFPWDT,  " 만기일
                 S_PYDT   FOR ZTPMTHD-ZFPYDT,  " 결제완료일
                 S_DSDT   FOR ZTPMTHD-ZFDSDT.  " 할인일
SELECTION-SCREEN END OF BLOCK B1.

*SELECTION-SCREEN SKIP 1.                       " 1 LINE SKIP
*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
** Payment Notice Status
*SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(14) TEXT-002, POSITION 1.
*SELECTION-SCREEN : COMMENT 31(8) TEXT-021, POSITION 40.
*PARAMETERS : P_N    AS CHECKBOX.               " Not Yet
*SELECTION-SCREEN : COMMENT 44(8) TEXT-022, POSITION 53.
*PARAMETERS : P_C    AS CHECKBOX.               " Confirm
*SELECTION-SCREEN : COMMENT 57(8) TEXT-023, POSITION 66.
*PARAMETERS : P_P    AS CHECKBOX.               " Confirm
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.                                 " 초기값 SETTING
  PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 레포트 관련 Text Table SELECT
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  IF SY-LANGU EQ '3'.
    SET TITLEBAR 'ZIMR97' WITH 'Usance 잔액 현황'.
  ELSE.
    SET TITLEBAR 'ZIMR97' WITH 'The Usance Balance Status'.
  ENDIF.
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  IF SY-LANGU EQ '3'.
    WRITE :/75 '[ USANCE 잔액현황 ]'
                COLOR COL_HEADING INTENSIFIED OFF.
    WRITE :/ ' Date : ', SY-DATUM, 168 'Page : ', W_PAGE.
    WRITE :/ SY-ULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE :/ SY-VLINE, ' ',
              SY-VLINE NO-GAP, (10) '차입처'   NO-GAP CENTERED,
              SY-VLINE NO-GAP, (20) '차입처명' NO-GAP CENTERED,
              SY-VLINE NO-GAP, (15) 'L/C NO'   NO-GAP CENTERED,
              SY-VLINE NO-GAP, (15) 'B/L NO'   NO-GAP CENTERED,
              SY-VLINE NO-GAP, (10) '발생일'   NO-GAP CENTERED,
              SY-VLINE NO-GAP, (10) '만기일'   NO-GAP CENTERED,
              SY-VLINE NO-GAP, (35) '외    화' NO-GAP CENTERED,
              SY-VLINE NO-GAP, (16) '환율'     NO-GAP CENTERED,
              SY-VLINE NO-GAP, (39) '원    화' NO-GAP CENTERED,
              SY-VLINE NO-GAP, (10) 'P/O NO'   NO-GAP CENTERED,
              SY-VLINE NO-GAP, (10) '이자율'   NO-GAP CENTERED,
              SY-VLINE NO-GAP, (10) '비고'     NO-GAP CENTERED,
              SY-VLINE NO-GAP.
    WRITE : / SY-VLINE, ' ',
              SY-VLINE NO-GAP, (10) '  '       NO-GAP CENTERED,
              SY-VLINE NO-GAP, (20) '  '       NO-GAP CENTERED,
              SY-VLINE NO-GAP, (15) '  '       NO-GAP CENTERED,
              SY-VLINE NO-GAP, (15) '  '       NO-GAP CENTERED,
              SY-VLINE NO-GAP, (10) '  '       NO-GAP CENTERED,
              SY-VLINE NO-GAP, (10) '  '       NO-GAP CENTERED,
              SY-VLINE NO-GAP, (19) '원금'     NO-GAP CENTERED,
              SY-VLINE NO-GAP, (15) '이자'     NO-GAP CENTERED,
              SY-VLINE NO-GAP, (16) '  '       NO-GAP CENTERED,
              SY-VLINE NO-GAP, (23) '원금'     NO-GAP CENTERED,
              SY-VLINE NO-GAP, (15) '이자'     NO-GAP CENTERED,
              SY-VLINE NO-GAP, (10) '  '       NO-GAP CENTERED,
              SY-VLINE NO-GAP, (10) '  '       NO-GAP CENTERED,
              SY-VLINE NO-GAP, (10) '  '       NO-GAP CENTERED,
              SY-VLINE NO-GAP.
  ELSE.
    WRITE:/75 '[ Usance Balance Status ]'
               COLOR COL_HEADING INTENSIFIED OFF.
    WRITE:/ ' Date : ', SY-DATUM, 168 'Page : ', W_PAGE.
    WRITE:/ SY-ULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE:/ SY-VLINE, ' ',
            SY-VLINE NO-GAP, (10) 'Adv.Bank'           NO-GAP CENTERED,
            SY-VLINE NO-GAP, (20) 'Advising Bank Name' NO-GAP CENTERED,
            SY-VLINE NO-GAP, (15) 'L/C No.'            NO-GAP CENTERED,
            SY-VLINE NO-GAP, (15) 'B/L No.'            NO-GAP CENTERED,
            SY-VLINE NO-GAP, (10) 'Post Date'          NO-GAP CENTERED,
            SY-VLINE NO-GAP, (10) 'ExpiryDate'         NO-GAP CENTERED,
            SY-VLINE NO-GAP, (35) 'Foreign Currency'   NO-GAP CENTERED,
            SY-VLINE NO-GAP, (16) 'Exchang Ratio'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (39) 'Won Currency'       NO-GAP CENTERED,
            SY-VLINE NO-GAP, (10) 'P/O No.'            NO-GAP CENTERED,
            SY-VLINE NO-GAP, (10) 'Intr. Rate'         NO-GAP CENTERED,
            SY-VLINE NO-GAP, (10) 'Remark'             NO-GAP CENTERED,
            SY-VLINE NO-GAP.
    WRITE:/ SY-VLINE, ' ',
            SY-VLINE NO-GAP, (10) '  '                 NO-GAP CENTERED,
            SY-VLINE NO-GAP, (20) '  '                 NO-GAP CENTERED,
            SY-VLINE NO-GAP, (15) '  '                 NO-GAP CENTERED,
            SY-VLINE NO-GAP, (15) '  '                 NO-GAP CENTERED,
            SY-VLINE NO-GAP, (10) '  '                 NO-GAP CENTERED,
            SY-VLINE NO-GAP, (10) '  '                 NO-GAP CENTERED,
            SY-VLINE NO-GAP, (19) 'The Principal(For)' NO-GAP CENTERED,
            SY-VLINE NO-GAP, (15) 'Interest(For)'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (16) '  '                 NO-GAP CENTERED,
            SY-VLINE NO-GAP, (23) 'The Principal(Won)' NO-GAP CENTERED,
            SY-VLINE NO-GAP, (15) 'Interest(Won)'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (10) '  '                 NO-GAP CENTERED,
            SY-VLINE NO-GAP, (10) '  '                 NO-GAP CENTERED,
            SY-VLINE NO-GAP, (10) '  '                 NO-GAP CENTERED,
            SY-VLINE NO-GAP.
  ENDIF.
  ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  RANGES : R_ZFPYA  FOR ZTPMTHD-ZFPYA  OCCURS 2.
  MOVE 'N' TO W_ERR_CHK.
  REFRESH IT_TAB.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
           FROM ZTPMTHD
           WHERE ZFPNNO    IN S_PNNO
           AND   ZFPNBN    IN S_PNBN
           AND   ZFOPBN    IN S_OPBN
           AND   ZFOPNNO   IN S_OPNNO
           AND   EBELN     IN S_EBELN
           AND   ZFNTDT    IN S_NTDT
           AND   ZFPWDT    IN S_PWDT
           AND   ZFPYDT    IN S_PYDT
           AND   ZFDSDT    IN S_DSDT
           AND   ZFLCKN    EQ '2'.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.

    SELECT SINGLE NAME1 INTO IT_TAB-W_ZFPNBN_NM
           FROM LFA1
           WHERE LIFNR = IT_TAB-ZFPNBN.

    MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S738.
    EXIT.
  ENDIF.

ENDFORM.                           " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  SET PF-STATUS 'ZIMR97'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIMR97'.           " GUI TITLE  SETTING

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_TAB.
    W_LINE = W_LINE + 1.
    PERFORM P2000_PAGE_CHECK.
    PERFORM P3000_LINE_WRITE.
    AT LAST.
      PERFORM P3000_LAST_WRITE.
    ENDAT.
  ENDLOOP.

ENDFORM.                            " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.       " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                             " RESET_LIST
*----------------------------------------------------------------------*
* USER COMMAND.                                                        *
*----------------------------------------------------------------------*
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.         " SORT 선택
      W_FIELD_NM = 'ZFREQDT'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
    WHEN 'MKAL' OR 'MKLO'.
      PERFORM P2000_SELECT_RECORD USING SY-UCOMM.
    WHEN 'CHDC'.                " Payment Notice 변경.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_PMTHD USING IT_SELECTED-ZFPNNO.
        CALL TRANSACTION 'ZIMP3' AND SKIP FIRST SCREEN.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'DISP'.                " Payment Notice 조회.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_PMTHD USING IT_SELECTED-ZFPNNO.
        CALL TRANSACTION 'ZIMP4' AND SKIP FIRST SCREEN.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'REFR'.
      PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM RESET_LIST.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
    FORMAT RESET.
    IF SY-LANGU EQ '3'.
      WRITE : / '  총 ', W_COUNT, '건'.
    ELSE.
      WRITE : / 'Total', W_COUNT, 'Case'.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  CLEAR : W_CAL2.

*>> 처리 원화 금액 계산
  PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_TAB-ZFPNAM
                                          IT_TAB-ZFPNAMC
                                          IT_TAB-ZFMOA.

  IF IT_TAB-FFACT IS INITIAL.
     *BAPICURR-BAPICURR = IT_TAB-ZFEXRT * IT_TAB-ZFMOA.
  ELSE.
     *BAPICURR-BAPICURR = ( IT_TAB-ZFEXRT / IT_TAB-FFACT )
                                         * IT_TAB-ZFMOA.
  ENDIF.

  PERFORM SET_CURR_CONV_TO_INTERNAL USING *BAPICURR-BAPICURR 'KRW'.

  IF *BAPICURR-BAPICURR GT 9999999999999.
    MESSAGE W923 WITH *BAPICURR-BAPICURR.
    IT_TAB-ZFMOA = 0.
  ELSE.
    IT_TAB-ZFMOA = *BAPICURR-BAPICURR.
  ENDIF.

*>> 처리 원화 이자 계산
  PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_TAB-ZFUSIT
                                          IT_TAB-ZFUSITC
                                          W_CAL2.

  IF IT_TAB-FFACT IS INITIAL.
     *BAPICURR-BAPICURR = IT_TAB-ZFEXRT * W_CAL2.
  ELSE.
     *BAPICURR-BAPICURR = ( IT_TAB-ZFEXRT / IT_TAB-FFACT ) * W_CAL2.
  ENDIF.

  PERFORM SET_CURR_CONV_TO_INTERNAL USING *BAPICURR-BAPICURR 'KRW'.

  IF *BAPICURR-BAPICURR GT 9999999999999.
    MESSAGE W923 WITH *BAPICURR-BAPICURR.
    W_CAL2 = 0.
  ELSE.
    W_CAL2 = *BAPICURR-BAPICURR.
  ENDIF.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE : / SY-VLINE, MARKFIELD  AS CHECKBOX,
            SY-VLINE NO-GAP, (10) IT_TAB-ZFPNBN NO-GAP,
            SY-VLINE NO-GAP, (20) IT_TAB-W_ZFPNBN_NM NO-GAP,   "차입처명
            SY-VLINE NO-GAP, (15) IT_TAB-ZFOPNNO NO-GAP,       " L/C NO
            SY-VLINE NO-GAP, (15) IT_TAB-ZFHBLNO NO-GAP,
            SY-VLINE NO-GAP, (10) IT_TAB-BUDAT NO-GAP,         " 발생일
            SY-VLINE NO-GAP, (10) IT_TAB-ZFPWDT NO-GAP,
            SY-VLINE NO-GAP, (3)  IT_TAB-ZFPNAMC,
            (15) IT_TAB-ZFPNAM CURRENCY IT_TAB-ZFPNAMC NO-GAP,
            SY-VLINE NO-GAP, (15) IT_TAB-ZFUSIT NO-GAP,
            SY-VLINE NO-GAP, (10) IT_TAB-ZFEXRT, '/',
                             (3)  IT_TAB-FFACT NO-GAP,
            SY-VLINE NO-GAP, (3)  IT_TAB-ZFKRW,
            (19) IT_TAB-ZFMOA CURRENCY IT_TAB-ZFKRW NO-GAP,
            SY-VLINE NO-GAP, (15) W_CAL2 NO-GAP CURRENCY IT_TAB-ZFKRW,
            SY-VLINE NO-GAP, (10) IT_TAB-EBELN NO-GAP,
            SY-VLINE NO-GAP, (10) IT_TAB-ZFUSITR NO-GAP,
            SY-VLINE NO-GAP, (10) '  ' NO-GAP,
            SY-VLINE NO-GAP.

* Stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE :/ SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
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
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFPNNO  LIKE ZTPMTHD-ZFPNNO,
        ZFPYDT(10).

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFPNNO   TO ZFPNNO.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      READ LINE SY-INDEX FIELD VALUE  W_ZFPYDT.
      W_TABIX = SY-INDEX + 1.
      READ LINE W_TABIX FIELD VALUE  W_ZFPYDT.

      MOVE : W_LIST_INDEX    TO INDEX,
             IT_TAB-ZFPNNO   TO IT_SELECTED-ZFPNNO,
             W_ZFPYDT(4)     TO IT_SELECTED-ZFPYDT,
             W_ZFPYDT+5(2)   TO IT_SELECTED-ZFPYDT+4(2),
             W_ZFPYDT+8(2)   TO IT_SELECTED-ZFPYDT+6(2).

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_PMTHD
*&---------------------------------------------------------------------*
FORM P2000_SHOW_PMTHD USING    P_ZFPNNO.

  SET PARAMETER ID 'ZPPNNO'  FIELD P_ZFPNNO.
  EXPORT 'ZPPNNO'  TO MEMORY ID 'ZPPNNO'.

ENDFORM.                    " P2000_SHOW_PMTHD
*&---------------------------------------------------------------------*
*&      Form  SET_CURR_CONV_TO_EXTERNAL
*&---------------------------------------------------------------------*
FORM SET_CURR_CONV_TO_EXTERNAL USING    P_AMOUNT
                                        P_WAERS
                                        P_TO_AMOUNT.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
       EXPORTING
            CURRENCY        = P_WAERS
            AMOUNT_INTERNAL = P_AMOUNT
       IMPORTING
            AMOUNT_EXTERNAL = BAPICURR-BAPICURR.

  P_TO_AMOUNT = BAPICURR-BAPICURR.

ENDFORM.                    " SET_CURR_CONV_TO_EXTERNAL
*&---------------------------------------------------------------------*
*&      Form  SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------*
FORM SET_CURR_CONV_TO_INTERNAL USING    P_AMOUNT
                                        P_WAERS.

  BAPICURR-BAPICURR = P_AMOUNT.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
       EXPORTING
            CURRENCY             = P_WAERS
            AMOUNT_EXTERNAL      = BAPICURR-BAPICURR
            MAX_NUMBER_OF_DIGITS = DIGITS
       IMPORTING
            AMOUNT_INTERNAL      = P_AMOUNT
       EXCEPTIONS
            OTHERS               = 1.

ENDFORM.                    " SET_CURR_CONV_TO_INTERNAL
