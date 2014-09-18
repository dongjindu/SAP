*&---------------------------------------------------------------------*
*& Report            ZRIMTRPRNT                                        *
*&---------------------------------------------------------------------*
*&  프로그램명 : 발송증표 출력 LIST                                    *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.25                                            *
*&     적용회사: 한수원.
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

PROGRAM  ZRIMTRPRNT  MESSAGE-ID ZIM
                     LINE-SIZE 163
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------

INCLUDE   ZRIMTRPRNTTOP.
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모음.

*-----------------------------------------------------------------------
* Selection Screen .
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
*>> 검색조건
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS :    P_BUKRS  LIKE ZTBL-BUKRS DEFAULT 'KHNP'.
SELECT-OPTIONS: S_EBELN  FOR ZTTRHD-ZFREBELN
                       MATCHCODE OBJECT MEKK
                                            ,  " P/O NO.
                S_ERNAM  FOR ZTTRHD-ERNAM,     " 생성인.
                S_SENDER FOR ZTTRHD-ZFSENDER,  " 발송자.
                S_GIDT   FOR ZTTRHD-ZFGIDT,    " 출고일자.
                S_TRNO   FOR ZTTRHD-ZFTRNO,    " 출고번호.
                S_WERKS  FOR ZTTRHD-WERKS,     " 대표 수송처.
                S_TRCO   FOR ZTTRHD-ZFTRCO,    " 운송업체.
                S_TRGB   FOR ZTTRHD-ZFTRGB,    " 수송구분.
                S_DRMT   FOR ZTTRHD-ZFDRMT.    " 수송방법.
PARAMETERS :    P_ITEM  AS CHECKBOX.         "자재내역출력여부.
SELECTION-SCREEN END OF BLOCK B1.

*---------------------------------------------------------------------*
* EVENT INITIALIZATION.
*---------------------------------------------------------------------*
INITIALIZATION.                                 " 초기값 SETTING
  PERFORM   P2000_SET_PARAMETER.
  SET TITLEBAR 'TROK'.

*---------------------------------------------------------------------*
* EVENT TOP-OF-PAGE.
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*---------------------------------------------------------------------*
* EVENT START-OF-SELECTION.
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE .
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
  CLEAR : IT_TAB.
*-----------------------------------------------------------------------
* EVENT AT USER-COMMAND.
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.              " SORT 선택?
      IF IT_TAB-ZFTRNO IS INITIAL.
        MESSAGE S962.
      ELSE.
        W_FIELD_NM = 'ZFTRNO'.
        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
        PERFORM HANDLE_SORT TABLES  IT_TAB
                            USING   SY-UCOMM.
      ENDIF.
    WHEN 'MKAL' OR 'MKLO'.          " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.

    WHEN 'DISP'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S951.EXIT.
      ENDIF.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_TR USING IT_SELECTED-ZFTRNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.

*> 출력.
    WHEN 'PRPL'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S951.EXIT.
      ENDIF.
      IF W_SELECTED_LINES GE 1.
        PERFORM  P2000_PRINT_OUT.
      ENDIF.

    WHEN 'REFR'.
      PERFORM   P1000_READ_TEXT  USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM RESET_LIST.

    WHEN 'BAC1' OR 'EXIT' OR 'CANC'.    " 종료.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.
  CLEAR IT_TAB.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  P_ITEM = 'X'.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.

  WRITE : /54  '[ 발송증 출력 List ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 123 'Page : ', W_PAGE.

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : /  SY-VLINE NO-GAP, (03) ' '      CENTERED    NO-GAP,
             SY-VLINE NO-GAP, (10) '출고번호'           NO-GAP,
             SY-VLINE NO-GAP, (10) '출고일자'           NO-GAP,
             SY-VLINE NO-GAP, (10) '기한일'             NO-GAP,
             SY-VLINE NO-GAP, (30) '도착지(Plant)'      NO-GAP,
             SY-VLINE NO-GAP, (12) '발송자'             NO-GAP,
             SY-VLINE NO-GAP, (04) '구분'               NO-GAP,
             SY-VLINE NO-GAP, (04) '방법'               NO-GAP,
             SY-VLINE NO-GAP, (12) '출고처리상태'       NO-GAP,
             SY-VLINE NO-GAP, (46) '운송업체'           NO-GAP,
             SY-VLINE NO-GAP, (10) '대표 P/O'           NO-GAP,
             SY-VLINE.

  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
    WRITE : / SY-VLINE NO-GAP, (03) ' '                NO-GAP,
              SY-VLINE NO-GAP, (21) '자재번호'         NO-GAP,
              SY-VLINE NO-GAP, (41) '품명'             NO-GAP,
              SY-VLINE NO-GAP, (17) '통관수량'         NO-GAP,
              SY-VLINE NO-GAP, (17) '출고수량'         NO-GAP,
              SY-VLINE NO-GAP, (25) '도착지(Plant)'    NO-GAP,
              SY-VLINE NO-GAP, (20) 'Storage Location' NO-GAP,
              SY-VLINE NO-GAP, (10) '통관관리No'       NO-GAP,
              SY-VLINE.
  ENDIF.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    P_W_ERR_CHK.

  CLEAR  IT_TMP. REFRESH IT_TMP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TMP
           FROM ZTTRHD
          WHERE BUKRS EQ P_BUKRS
            AND ZFREBELN IN S_EBELN
            AND ERNAM    IN S_ERNAM
            AND ZFSENDER IN S_SENDER
            AND ZFGIDT   IN S_GIDT
            AND ZFTRNO   IN S_TRNO
            AND WERKS    IN S_WERKS
            AND ZFTRCO   IN S_TRCO
            AND ZFTRGB   IN S_TRGB
            AND ZFDRMT   IN S_DRMT.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'. MESSAGE S966. EXIT.
  ENDIF.

  CLEAR  IT_TAB. REFRESH IT_TAB.
  LOOP AT IT_TMP.

    IF IT_TMP-ZFGIYN EQ 'X'.
      CONTINUE.
    ENDIF.

*> 수송구분.
    IF NOT IT_TMP-ZFTRGB IS INITIAL.
      PERFORM   GET_DD07T_SELECT USING      'ZDTRGB'  IT_TMP-ZFTRGB
                                 CHANGING   IT_TMP-W_TRGB  W_SY_SUBRC.
    ENDIF.
*> 수송방법.
    IF NOT IT_TMP-ZFDRMT IS INITIAL.
      PERFORM   GET_DD07T_SELECT USING      'ZDDRMT'  IT_TMP-ZFDRMT
                                 CHANGING   IT_TMP-W_DRMT  W_SY_SUBRC.
    ENDIF.
*> 수송처.
    IF NOT IT_TMP-WERKS IS INITIAL.
      SELECT SINGLE NAME1 INTO IT_TMP-W_WERKS
                          FROM T001W
                         WHERE WERKS = IT_TMP-WERKS.
    ENDIF.
*> 운송업체.
    IF NOT IT_TMP-ZFTRCO IS INITIAL.
      PERFORM  P1000_GET_VENDOR   USING      IT_TMP-ZFTRCO
                                  CHANGING   IT_TMP-W_TRCO.
    ENDIF.

*> 출고상태.
    IF IT_TMP-ZFGIYN IS INITIAL.
      IT_TMP-W_GIYN = '출고처리대상'.
    ELSE.
      IT_TMP-W_GIYN = '출고처리완료'.
    ENDIF.


    MOVE-CORRESPONDING IT_TMP TO IT_TAB.
    APPEND IT_TAB.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.

  IF W_COUNT LE 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S966.
    EXIT.
  ENDIF.

  IF P_ITEM = 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TABIT
             FROM ZTTRIT
              FOR ALL ENTRIES IN IT_TAB
            WHERE ZFTRNO = IT_TAB-ZFTRNO.

    LOOP AT IT_TABIT.
      W_TABIX  = SY-TABIX.
*> 수송처.
      IF NOT IT_TABIT-WERKS IS INITIAL.
        SELECT SINGLE NAME1 INTO IT_TABIT-W_WERKS
                            FROM T001W
                           WHERE WERKS = IT_TABIT-WERKS.
      ENDIF.

*> Storage Location.
      IF NOT IT_TABIT-LGORT IS INITIAL.
        SELECT SINGLE LGOBE INTO IT_TABIT-W_LGORT
                            FROM T001L
                           WHERE WERKS = IT_TABIT-WERKS
                             AND LGORT = IT_TABIT-LGORT.
      ENDIF.

      MODIFY IT_TABIT INDEX W_TABIX.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " P1000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

  SET PF-STATUS 'TROK'.

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_TAB.
    W_LINE = W_LINE + 1.
    PERFORM P3000_LINE_WRITE.

    AT LAST.
      PERFORM P3000_LAST_WRITE.
    ENDAT.

  ENDLOOP.



ENDFORM.                    " P3000_DATA_WRITE
*&----------------------------------------------------------------------
*&      Form  RESET_LIST
*&----------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE .

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_VENDOR
*&---------------------------------------------------------------------*
FORM P1000_GET_VENDOR USING    P_LIFNR
                      CHANGING P_NAME1.
  DATA : L_TEXT(35).

  CLEAR : P_NAME1, W_LFA1.
  IF P_LIFNR IS INITIAL.
    EXIT.
  ENDIF.

* VENDOR MASTER SELECT( LFA1 )----------------------->
  CALL FUNCTION 'READ_LFA1'
    EXPORTING
      XLIFNR         = P_LIFNR
    IMPORTING
      XLFA1          = W_LFA1
    EXCEPTIONS
      KEY_INCOMPLETE = 01
      NOT_AUTHORIZED = 02
      NOT_FOUND      = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
  ENDCASE.
*   TRANSLATE W_LFA1 TO UPPER CASE.
  MOVE: W_LFA1-NAME1   TO   L_TEXT.
  TRANSLATE L_TEXT TO UPPER CASE.
  P_NAME1 = L_TEXT.

ENDFORM.                    " P1000_GET_VENDOR
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE : / SY-VLINE,  MARKFIELD AS CHECKBOX,
            SY-VLINE NO-GAP,  (10) IT_TAB-ZFTRNO      NO-GAP.
*>> 출고일 입력가능.
  IF IT_TAB-ZFGIYN IS INITIAL.
    WRITE IT_TAB-ZFGIDT TO W_ZFGIDT.
    WRITE : SY-VLINE NO-GAP,
            (10) W_ZFGIDT  INPUT ON   NO-GAP
                                 COLOR COL_POSITIVE INTENSIFIED ON.
  ELSEIF IT_TAB-ZFGIYN = 'X'.
    WRITE :  SY-VLINE NO-GAP,  (10) IT_TAB-ZFGIDT      NO-GAP.
  ENDIF.

  WRITE :  SY-VLINE NO-GAP,  (10) IT_TAB-ZFDRDT      NO-GAP,
           SY-VLINE NO-GAP,  (05) IT_TAB-WERKS       NO-GAP,
                             (25) IT_TAB-W_WERKS     NO-GAP,
           SY-VLINE NO-GAP,  (12) IT_TAB-ZFSENDER    NO-GAP,
           SY-VLINE NO-GAP,  (04) IT_TAB-W_TRGB      NO-GAP,
           SY-VLINE NO-GAP,  (04) IT_TAB-W_DRMT      NO-GAP.

  IF IT_TAB-ZFGIYN IS INITIAL.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ELSEIF IT_TAB-ZFGIYN = 'X'.
    FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
  ENDIF.
  WRITE :   SY-VLINE NO-GAP,  (12) IT_TAB-W_GIYN      NO-GAP.

  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE :   SY-VLINE NO-GAP,  (11) IT_TAB-ZFTRCO      NO-GAP,
                              (35) IT_TAB-W_TRCO      NO-GAP,
            SY-VLINE NO-GAP,  (10) IT_TAB-ZFREBELN    NO-GAP,
            SY-VLINE NO-GAP.

  FORMAT RESET.
* Stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  W_COUNT = W_COUNT + 1.

*> 자재내역.
  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

    LOOP AT IT_TABIT WHERE ZFTRNO = IT_TAB-ZFTRNO.
      WRITE : / SY-VLINE NO-GAP, (03) ' '                NO-GAP,
                SY-VLINE NO-GAP, (21) IT_TABIT-MATNR     NO-GAP,
                SY-VLINE NO-GAP, (41) IT_TABIT-TXZ01     NO-GAP,
                SY-VLINE NO-GAP, (17) IT_TABIT-CCMENGE   NO-GAP,
                SY-VLINE NO-GAP, (17) IT_TABIT-GIMENGE   NO-GAP,
                SY-VLINE NO-GAP, (05) IT_TABIT-WERKS     NO-GAP,
                                 (20) IT_TABIT-W_WERKS   NO-GAP,
                SY-VLINE NO-GAP, (05) IT_TABIT-LGORT     NO-GAP,
                                 (15) IT_TABIT-W_LGORT   NO-GAP,
                SY-VLINE NO-GAP, (10) IT_TABIT-ZFIVNO    NO-GAP,
                SY-VLINE NO-GAP.
*      HIDE : IT_TABIT.
    ENDLOOP.

  ENDIF.
  ULINE.
ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE : / '총', W_COUNT, '건'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  CLEAR   IT_SELECTED.
  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.
  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      READ LINE SY-INDEX FIELD VALUE  W_ZFGIDT.
      MOVE : IT_TAB-ZFTRNO   TO IT_SELECTED-ZFTRNO,
             W_ZFGIDT(4)     TO IT_SELECTED-ZFGIDT,
             W_ZFGIDT+5(2)   TO IT_SELECTED-ZFGIDT+4(2),
             W_ZFGIDT+8(2)   TO IT_SELECTED-ZFGIDT+6(2),
             IT_TAB-ZFDRDT   TO IT_SELECTED-ZFDRDT,
             IT_TAB-ZFGIYN   TO IT_SELECTED-ZFGIYN.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_TR
*&---------------------------------------------------------------------*
FORM P2000_SHOW_TR USING    P_ZFTRNO.

  SET PARAMETER ID 'BES'     FIELD ''.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD ''.
  SET PARAMETER ID 'ZPTRNO'  FIELD P_ZFTRNO.

  CALL TRANSACTION 'ZIMT3'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_TR
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE  USING VALUE(P_TITLE)
                                VALUE(P_QUESTION)
                                VALUE(P_BUTTON1)
                                VALUE(P_BUTTON2)
                                VALUE(P_DEFAULT)
                          CHANGING    P_ANSWER.

  CLEAR : P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = P_TITLE
      DIAGNOSE_OBJECT       = ''
      TEXT_QUESTION         = P_QUESTION
      TEXT_BUTTON_1         = P_BUTTON1
      TEXT_BUTTON_2         = P_BUTTON2
      DEFAULT_BUTTON        = P_DEFAULT
      DISPLAY_CANCEL_BUTTON = 'X'
      START_COLUMN          = 30
      START_ROW             = 8
    IMPORTING
      ANSWER                = P_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_PRINT_OUT
*&---------------------------------------------------------------------*
FORM P2000_PRINT_OUT .

  REFRESH : RG_SEL.
  CLEAR   : RG_SEL.

  LOOP AT IT_SELECTED.
    MOVE : 'I'                TO RG_SEL-SIGN,
           'EQ'               TO RG_SEL-OPTION,
           IT_SELECTED-ZFTRNO TO RG_SEL-LOW.
    APPEND RG_SEL.

  ENDLOOP.

  SUBMIT ZRIMTRSEND WITH S_ZFTRNO IN RG_SEL
                    AND RETURN.

ENDFORM.                    " P2000_PRINT_OUT
