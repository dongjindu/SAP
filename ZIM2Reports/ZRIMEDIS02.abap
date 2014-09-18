*&---------------------------------------------------------------------*
*& Report  ZRIMEDIS02                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : EDI Send(Matrix2B I/F용-L/G)                          *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2002.09.16                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
REPORT  ZRIMEDIS02  MESSAGE-ID ZIM
                     LINE-SIZE 113
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 수입의뢰 릴리즈용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_TAB OCCURS 0.
        INCLUDE STRUCTURE ZTLG.
DATA: ZFLGOD    LIKE ZTLGGOD-ZFLGOD,
      ZFGODS    LIKE ZTLGGOD-ZFGODS,
      ZFREBELN  LIKE ZTBL-ZFREBELN,
      ZFBLAMT   LIKE ZTBL-ZFBLAMT,
      ZFBLAMC   LIKE ZTBL-ZFBLAMC,
    END OF IT_TAB.

TABLES : ZTBL,               " Bill of Lading Header.
         ZTLG,               " Letter of Guarantee Header.
         ZTLGGOD,            " Letter of Guarantee Description of Goods.
         DD03D,              " Dynpro fields for table fields.
*         T024E,              " 구매조직.
*         T024,               " 구매그룹.
         LFA1,               " 구매처마스터 (일반섹션).
         TINC,               " 고객: 인도조건.
         EKPO,               " Purchasing Document Item.
         ZTIMIMGTX,          " EDI TEXT.
         ZTDHF1,             " 표준 EDI Flat Head.
         ZTCDF1,             " 전자문서번호 채번(EDI).
         ZTIMIMG03,          " 보세구역 코드.
         ZTIMIMG00.          " 수입시스템 Basic Configuration.

*-----------------------------------------------------------------------
* Select Record..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
       GUBUN      TYPE C,                     " 최초 구분.
       ZFREBELN   LIKE ZTBL-ZFREBELN,         " 구매문서.
       ZFBLNO     LIKE ZTLG-ZFBLNO,           " 수입의뢰 관리번호.
       ZFLGSEQ    LIKE ZTLG-ZFLGSEQ,          " Amend Seq.
       ZFLGOD     LIKE ZTLGGOD-ZFLGOD,        " 반복수 상품명세.
       ZFDOCST    LIKE ZTLG-ZFDOCST,          " 문서 상태.
       ZFEDIST    LIKE ZTLG-ZFEDIST,          " EDI 상태.
       ZFEDICK    LIKE ZTLG-ZFEDICK,          " EDI Check.
END OF IT_SELECTED.

*-----------------------------------------------------------------------
* Internal Table Define..
*-----------------------------------------------------------------------
DATA: IT_ZVREQ       LIKE  ZVREQHD_ST     OCCURS 0 WITH HEADER LINE.
DATA: IT_ZVOFF       LIKE  ZVREQ_OFF      OCCURS 0 WITH HEADER LINE.
DATA: BAPIMEPOITEM   LIKE  BAPIMEPOITEM   OCCURS 0 WITH HEADER LINE.
DATA: BAPIMEPOITEMX  LIKE  BAPIMEPOITEMX  OCCURS 0 WITH HEADER LINE.

*-----------------------------------------------------------------------
* Internal Table Define: IT_TAB_DOWN.
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB_DOWN OCCURS 0,
         W_EDI_RECORD(65535)  TYPE C,
       END   OF IT_TAB_DOWN.

DATA: BEGIN OF MTAB_DATA OCCURS 0,
      LINE(132)   TYPE C,
END OF MTAB_DATA.

*>>> Return Message 처리용..
DATA:   BEGIN OF RETURN OCCURS 0.                ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA : W_ERR_CHK(1)      TYPE C,                 " Error Check Bit..
       W_SELECTED_LINES  TYPE P,                 " 선택 Line Count..
       W_PAGE            TYPE I,                 " Page Counter..
       W_LINE            TYPE I,                 " 페이지당 Line Count..
       LINE(3)           TYPE N,                 " 페이지당 Line Count..
       W_COUNT           TYPE I,                 " 전체 Line Count..
       W_LIST_INDEX      LIKE SY-TABIX,          " List Index Number..
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " Field Value..
       W_TABIX          LIKE SY-TABIX,           " Table Index..
       W_UPDATE_CNT     TYPE I,                  " Update Sequence..
       W_BUTTON_ANSWER  TYPE C,                  " Pop Up Windows Ans..
       W_ITEM_CNT      LIKE SY-TABIX,            " 품목 Count..
       W_AMOUNT        LIKE ZTIV-ZFIVAMT,        " 수입의뢰 Amount..
       W_TOT_AMOUNT    LIKE ZTIV-ZFIVAMT,        " 수입의뢰 Amount..
       W_LOCAL_AMT     LIKE ZTIV-ZFIVAMT,        " USD 환산 Amount..
       W_EBELN         LIKE EKPO-EBELN,
       W_FILENAME      LIKE ZTDHF1-FILENAME,
       W_LFA1          LIKE LFA1,
       W_MENGE         LIKE ZTREQIT-MENGE,
       W_ZSREQIT       LIKE ZSREQIT,
       W_ZFHBLNO       LIKE ZTBL-ZFHBLNO,
       W_MAX_ZFAMDNO   LIKE ZTREQST-ZFAMDNO,
       MI_HANDLE       TYPE I.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMSORTCOM.                    " Report Sort를 위한 Include..
INCLUDE   ZRIMUTIL01.                     " Utility function 모음..
INCLUDE   ZRIMBDCCOM.                     " 수입의뢰 BDC 공통 Include..

* Variable Declaration for EDI..
DATA: W_OK_CODE         LIKE   SY-UCOMM,
      W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO,
      W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
      W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
      W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF.
DATA  W_ERR_MSG(100)    TYPE   C.

DATA  W_EDI_RECORD(65535).

DATA: BEGIN OF IT_EDIFILE OCCURS 0,
      W_RECORD          LIKE   W_EDI_RECORD,
      END OF IT_EDIFILE.

*-----------------------------------------------------------------------
* Selection Screen Clause.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                        " 2 Line Skip..
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS :    P_BUKRS   LIKE ZTLG-BUKRS       ">회사코드..
                          OBLIGATORY.
SELECT-OPTIONS:
                S_EBELN   FOR ZTBL-ZFREBELN,    " 구매문서.
                S_ZFBLNO  FOR ZTLG-ZFBLNO,      " B/L No..
                S_HBLNO   FOR ZTBL-ZFHBLNO.     " House B/L No..
PARAMETERS :    P_NAME    LIKE USR02-BNAME.     " 담당자..
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 1.                        " 1 Line Skip..
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS : P_NOOPEN   AS CHECKBOX.
PARAMETERS : P_OPEN     AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS : P_OK       AS CHECKBOX.
PARAMETERS : P_NOTOK    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B3.

SELECT-OPTIONS : S_EDIST  FOR ZTLG-ZFEDIST NO INTERVALS NO-DISPLAY.
SELECT-OPTIONS : S_EDICK  FOR ZTLG-ZFEDICK NO INTERVALS NO-DISPLAY.

* PARAMETER 초기값 Setting
INITIALIZATION.                        " 초기값 Setting..
  PERFORM   P2000_SET_PARAMETER.

* screen Selection
AT SELECTION-SCREEN ON P_BUKRS.
  PERFORM P2000_COMPANY_CODE_CHECK USING P_BUKRS.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.         " 해더 출력...

*-----------------------------------------------------------------------
* Start of Selection Clause.
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Import System Config Check
  PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.

* 파라메타 설정.
  PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Letter of Guarantee Select.
  PERFORM  P1000_READ_LG              USING   W_ERR_CHK.
* Display Erroe Message.
  IF W_ERR_CHK EQ 'Y'.  MESSAGE S966.  EXIT.  ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  W_OK_CODE = SY-UCOMM.
  CASE SY-UCOMM.
* SORT 선택?
    WHEN 'STUP' OR 'STDN'.             " SORT 선택시.
      W_FIELD_NM = 'ZFOPBN'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
* 전체 선택 및 선택해제.
    WHEN 'MKAL' OR 'MKLO'.             " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
    WHEN 'DISP'.                       " L/G Display.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_LG USING IT_SELECTED-ZFBLNO
                                    IT_SELECTED-ZFLGSEQ.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'FRGS' OR 'FRGR'.             " EDI File Create / Cancel.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES NE 0.
        PERFORM P2000_POPUP_MESSAGE.   " 메세지 박스.
        IF W_BUTTON_ANSWER EQ '1'.     " 확인일 경우.
          PERFORM P3000_DATA_UPDATE USING W_OK_CODE. " 데이타 반영.
          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.
    WHEN 'DOWN'.                       " FILE DOWNLOAD....
      PERFORM P3000_DOWNLOAD_EDI_FILE.
    WHEN 'REFR'.
* 구매의뢰 테이블 Select..
      PERFORM   P1000_READ_LG USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM RESET_LIST.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIME02L'.               " TITLE BAR
  P_NOOPEN = 'X'.                      " CREATE 대상.
  CLEAR : P_OPEN.                      " CREATE CANCLE 대상.

  P_OK = 'X'.                          " EDI CHECK BIT : OK
  CLEAR : P_NOTOK.                     " EDI CHECK BIT : NOT OK

  GET PARAMETER ID 'BUK'  FIELD  P_BUKRS.

  IF NOT P_BUKRS IS INITIAL.
    SELECT SINGLE * FROM ZTIMIMGTX
           WHERE   BUKRS  EQ  P_BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE S949 WITH P_BUKRS.
    ENDIF.
  ELSE.
  ENDIF.

ENDFORM.                               " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /45  '[ L/G 신청 EDI 송신 대상 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 93 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, '  '       NO-GAP,  SY-VLINE NO-GAP,
        (10)'B/L No.'            NO-GAP,  SY-VLINE NO-GAP,
         (6)'Seq.'               NO-GAP,  SY-VLINE NO-GAP,
        (10)' 구매문서 '         NO-GAP,  SY-VLINE NO-GAP,
        (35)'House B/L No.'      NO-GAP,  SY-VLINE NO-GAP,
        (24)'B/L Amount'         NO-GAP,  SY-VLINE NO-GAP,
         (8)'EDI 상태'           NO-GAP,  SY-VLINE NO-GAP,
         (8)'문서상태'           NO-GAP,  SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, '  '       NO-GAP,  SY-VLINE NO-GAP,
        (17)'전자문서번호'       NO-GAP,  SY-VLINE NO-GAP,
        (10)'수입의뢰No'         NO-GAP,  SY-VLINE NO-GAP,
        (35)'L/C 개설번호'       NO-GAP,  SY-VLINE NO-GAP,
        (42)'은행명'             NO-GAP,  SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                               " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.

  IF P_NOOPEN IS INITIAL AND P_OPEN IS INITIAL.
    W_ERR_CHK = 'Y'.   MESSAGE S351.   EXIT.
  ENDIF.

  IF P_OK IS INITIAL AND P_NOTOK IS INITIAL.
    W_ERR_CHK = 'Y'.   MESSAGE S352.   EXIT.
  ENDIF.

  IF P_NAME IS INITIAL.       P_NAME  =  '%'.      ENDIF.

*-----------------------------------------------------------------------
* EDI CREATE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_NOOPEN EQ 'X'.
    MOVE: 'I'      TO S_EDIST-SIGN,
          'EQ'     TO S_EDIST-OPTION,
          'N'      TO S_EDIST-LOW.
    APPEND S_EDIST.
  ENDIF.

*-----------------------------------------------------------------------
* EDI CALCLE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_OPEN EQ 'X'.
    MOVE: 'I'      TO S_EDIST-SIGN,
          'EQ'     TO S_EDIST-OPTION,
          'S'      TO S_EDIST-LOW.
    APPEND S_EDIST.
  ENDIF.

*-----------------------------------------------------------------------
* EDI CHECK BIT  SETTING
*-----------------------------------------------------------------------
  IF P_OK EQ 'X'.
    MOVE: 'I'      TO S_EDICK-SIGN,
          'EQ'     TO S_EDICK-OPTION,
          'O'      TO S_EDICK-LOW.
    APPEND S_EDICK.
  ENDIF.

*-----------------------------------------------------------------------
* EDI CALCLE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_NOTOK EQ 'X'.
    MOVE: 'I'      TO S_EDICK-SIGN,
          'EQ'     TO S_EDICK-OPTION,
          'X'      TO S_EDICK-LOW.
    APPEND S_EDICK.
  ENDIF.

ENDFORM.                               " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
FORM P2000_CONFIG_CHECK           USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
* Import System Configuration Select
  SELECT SINGLE * FROM ZTIMIMG00.

* Not Found
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE S961.   LEAVE TO SCREEN 0.
  ENDIF.

  SET PARAMETER ID 'BUK'  FIELD  P_BUKRS.

ENDFORM.                               " P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'ZIME02L'.             " GUI Status Setting..
  SET  TITLEBAR 'ZIME02L'.             " GUI Title Setting..

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_TAB.

    PERFORM P3000_LINE_WRITE.

    AT LAST.
      PERFORM P3000_LAST_WRITE.
    ENDAT.

  ENDLOOP.

ENDFORM.                               " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.         " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                               " RESET_LIST
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
    IF SY-SUBRC NE 0.   EXIT.   ENDIF. " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFBLNO   TO IT_SELECTED-ZFBLNO,
             IT_TAB-ZFLGSEQ  TO IT_SELECTED-ZFLGSEQ,
             IT_TAB-ZFDOCST  TO IT_SELECTED-ZFDOCST,
             IT_TAB-ZFEDICK  TO IT_SELECTED-ZFEDICK,
             IT_TAB-ZFEDIST  TO IT_SELECTED-ZFEDIST.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

* Error Check!!
  IF W_SELECTED_LINES EQ 0. MESSAGE S962. ENDIF.

ENDFORM.                               " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

  IF W_LINE >= 53.
    WRITE : / SY-ULINE.
    W_PAGE = W_PAGE + 1.    W_LINE = 0.
    NEW-PAGE.
  ENDIF.

ENDFORM.                               " P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE: / '총', W_COUNT, '건'.
  ENDIF.

ENDFORM.                               " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE,
          MARKFIELD  AS CHECKBOX,       SY-VLINE NO-GAP,
          IT_TAB-ZFBLNO NO-GAP,         SY-VLINE NO-GAP, " 개설신청일.
          IT_TAB-ZFLGSEQ,        SY-VLINE NO-GAP, " 구매문서.
          IT_TAB-ZFREBELN NO-GAP,       SY-VLINE NO-GAP, " currency
          IT_TAB-ZFHBLNO NO-GAP,
      (11)' '            NO-GAP,        SY-VLINE NO-GAP, " House B/L No.
          IT_TAB-ZFBLAMT CURRENCY IT_TAB-ZFBLAMC NO-GAP, " B/L Amount..
          IT_TAB-ZFBLAMC NO-GAP,        SY-VLINE NO-GAP. " B/L Currency.

  IF IT_TAB-ZFEDIST EQ 'N'.
    WRITE: '의뢰대상' NO-GAP,         SY-VLINE NO-GAP.
  ELSEIF IT_TAB-ZFEDIST EQ 'S'.
    WRITE: '기의뢰건' NO-GAP,         SY-VLINE NO-GAP.
  ENDIF.

  IF IT_TAB-ZFEDICK EQ 'X'.
    FORMAT COLOR COL_NEGATIVE INVERSE.
    WRITE: '송신불능' NO-GAP,          SY-VLINE NO-GAP.
    FORMAT RESET.
  ELSE.
    FORMAT COLOR COL_HEADING INVERSE.
    WRITE: '송신가능' NO-GAP,          SY-VLINE NO-GAP.
    FORMAT RESET.
  ENDIF.

* Hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE, ' ',             SY-VLINE NO-GAP,
      (17)IT_TAB-ZFDOCNO     NO-GAP, SY-VLINE NO-GAP,
          IT_TAB-ZFREQNO     NO-GAP, SY-VLINE NO-GAP, " 수입의뢰 No.
          IT_TAB-ZFDCNO      NO-GAP, SY-VLINE NO-GAP, " 신용장-승인번호.
*          IT_TAB-ZFISBNC,
          IT_TAB-ZFISBNM(42) NO-GAP, SY-VLINE NO-GAP. " 발급은행.

* stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.
ENDFORM.                               " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_UNRELEASE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_UNRELEASE_CHECK USING    P_ZFREQNO.
* Amend 존재여부 체?

* Invoice 체?

ENDFORM.                               " P2000_UNRELEASE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.
  DATA : TEXT100(100) TYPE  C.
  IF W_OK_CODE EQ 'FRGS'.
    TEXT100 = 'EDI FILE CREATE 작업을 계속 진행하시겠습니까?'.
  ELSEIF W_OK_CODE EQ 'FRGR'.
    TEXT100 = 'EDI FILE CANCLE 작업을 계속 진행하시겠습니까?'.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            TITLEBAR              = 'EDI FILE CREATE/CANCLE 확인'
            DIAGNOSE_OBJECT       = ''
            TEXT_QUESTION         = TEXT100
            TEXT_BUTTON_1         = '확    인'
            TEXT_BUTTON_2         = '아 니 오'
            DEFAULT_BUTTON        = '1'
            DISPLAY_CANCEL_BUTTON = 'X'
            START_COLUMN          = 30
            START_ROW             = 8
       IMPORTING
            ANSWER                = W_BUTTON_ANSWER.

ENDFORM.                               " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_DATA_UPDATE   USING   W_GUBUN.
*  DATA : L_REQTY   LIKE   ZTREQHD-ZFREQTY,
  DATA: L_RETURN  LIKE   SY-SUBRC,
        O_ZTLG    LIKE   ZTLG,
        L_COUNT   TYPE   I.

* 처리문서의 를 보관하기 위한 변수.
  DATA: L_DATNO   TYPE   I VALUE 0.
  DATA: L_DATCNO  TYPE   I VALUE 0.

  REFRESH : IT_EDIFILE.
  CLEAR : IT_EDIFILE, L_COUNT.
*  SORT IT_SELECTED BY ZFREQTY ZFREQNO ZFAMDNO.

  LOOP AT IT_SELECTED.
    W_TABIX = SY-TABIX.
*>>> 진행상태바..
    LINE = ( SY-TABIX / W_SELECTED_LINES ) * 100.
    OUT_TEXT = 'JOB PROGRESS %99999%%'.
    REPLACE '%99999%' WITH LINE INTO OUT_TEXT.
    PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE.
*>> EDI Bit Check..
    IF W_GUBUN EQ 'FRGS'.              " EDI CREATE
      IF IT_SELECTED-ZFEDICK EQ 'X'.
        MESSAGE I119 WITH IT_TAB-ZFBLNO IT_TAB-ZFLGSEQ.
        CONTINUE.
      ENDIF.
      IF IT_SELECTED-ZFDOCST NE 'N'.
        MESSAGE I104 WITH IT_TAB-ZFBLNO
                          IT_TAB-ZFLGSEQ IT_SELECTED-ZFDOCST.
        CONTINUE.
      ENDIF.
    ELSEIF W_GUBUN EQ 'FRGR'.          " EDI CANCLE
      IF IT_SELECTED-ZFDOCST NE 'R'.
        MESSAGE I104 WITH IT_SELECTED-ZFBLNO
                          IT_SELECTED-ZFLGSEQ IT_SELECTED-ZFDOCST.
        CONTINUE.
      ENDIF.
    ENDIF.

*>>> Letter of Guarantee Header, Description of Goods Select..
    SELECT SINGLE * FROM ZTLG
                    WHERE ZFBLNO  EQ IT_SELECTED-ZFBLNO
                    AND   ZFLGSEQ EQ IT_SELECTED-ZFLGSEQ.

    SELECT SINGLE * FROM ZTLGGOD
                    WHERE ZFBLNO  EQ IT_SELECTED-ZFBLNO
                    AND   ZFLGSEQ EQ IT_SELECTED-ZFLGSEQ
                    AND   ZFLGOD  EQ IT_SELECTED-ZFLGOD.

*>> 변경이력..
    O_ZTLG = ZTLG.
*>>  개설은행 조회.
    SELECT SINGLE * FROM LFA1
                    WHERE LIFNR   EQ ZTLG-ZFISBNC.
*>>>  EDI 식별자 조회.
    IF LFA1-BAHNS IS INITIAL.
      MESSAGE I274 WITH ZTLG-ZFISBNC.
      CONTINUE.
    ENDIF.

* Lock Check.
    PERFORM   P2000_LOCK_MODE_SET  USING    'L'
                                            IT_SELECTED-ZFBLNO
                                            IT_SELECTED-ZFLGSEQ
                                            L_RETURN.
    CHECK L_RETURN EQ 0.

*>>> EDI용 Field Create..
    IF W_GUBUN EQ 'FRGS' OR W_GUBUN EQ 'DOWN'.         " EDI Create.
      IF W_GUBUN EQ 'FRGS'.
        PERFORM   P3000_FILE_CREATE.
*>>> Matrix2B SAM-File Write Function..
        CALL FUNCTION 'ZIM_EDI_SAMFILE_WRITE'
             EXPORTING
                  ZFCDDOC = W_ZFCDDOC
                  BUKRS   = ZTLG-BUKRS
             IMPORTING
                  W_FILENAME  =  W_FILENAME
             TABLES
                  EDIFILE = IT_EDIFILE.
        L_DATNO = L_DATNO + 1.
        MESSAGE S251 WITH L_DATNO.
        REFRESH : IT_EDIFILE.

        PERFORM  P3000_FILE_TRANSFER.
        IF W_ERR_CHK EQ 'Y'.  CONTINUE.  ENDIF.

      ENDIF.
      IF W_GUBUN EQ 'DOWN'.
        PERFORM  P3000_FILE_CREATE.
        MOVE W_EDI_RECORD TO IT_TAB_DOWN.
        APPEND IT_TAB_DOWN.
        PERFORM P3000_TO_CLIENT_PC_DOWNLOAD.
      ENDIF.
    ELSE.
      CALL FUNCTION 'ZIM_EDI_SAMFILE_DELETE'
           EXPORTING
                ZFDHENO = ZTLG-ZFDOCNO.
      L_DATCNO = L_DATCNO + 1.
      MESSAGE S284 WITH L_DATCNO.
    ENDIF.

    ADD 1   TO    L_COUNT.             "---> 마지막을 알?

* 상태 변경.
*>>>>> 문서취소일 경우, 이전 EDI문서관리번호를 가지고 있기 위해.....
    IF W_GUBUN EQ 'FRGR'.
      MOVE ZTLG-ZFDOCNO  TO  W_ZFDHENO.
    ENDIF.

    MOVE : SY-UNAME    TO    ZTLG-UNAM,
           SY-DATUM    TO    ZTLG-UDAT,
           W_ZFDHENO   TO    ZTLG-ZFDOCNO.
    IF W_GUBUN EQ 'FRGS'.              " EDI CREATE
      MOVE : 'R'     TO    ZTLG-ZFDOCST,
             'S'     TO    ZTLG-ZFEDIST.
    ELSEIF W_GUBUN EQ 'FRGR'.          " EDI CANCLE
      MOVE : 'N'     TO    ZTLG-ZFDOCST,
             'N'     TO    ZTLG-ZFEDIST.
    ENDIF.
*>>> 변경...
    UPDATE ZTLG.

* CHANGE DOCUMENT
*    CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_STATUS'
*         EXPORTING
*              W_ZFREQNO = ZTLG-ZFBLNO
*              W_ZFAMDNO = ZTLG-ZFLGSEQ
*              N_ZTREQST = ZTLG
*              O_ZTREQST = O_ZTLG.

*>>> UNLOCK SETTTING.
    PERFORM   P2000_LOCK_MODE_SET  USING    'U'
                                             IT_SELECTED-ZFBLNO
                                             IT_SELECTED-ZFLGSEQ
                                             L_RETURN.

  ENDLOOP.

ENDFORM.                               " P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_LOCK_MODE_SET
*&---------------------------------------------------------------------*
FORM P2000_LOCK_MODE_SET USING    VALUE(P_MODE)
                                  VALUE(P_REQNO)
                                  VALUE(P_AMDNO)
                                  P_RETURN.
* Lock Check.
  IF P_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = P_REQNO
              ZFAMDNO = P_AMDNO
         EXCEPTIONS
              OTHERS  = 1.

    MOVE SY-SUBRC     TO     P_RETURN.
    IF SY-SUBRC NE 0.
      MESSAGE I510 WITH SY-MSGV1 'Import Document' P_REQNO P_AMDNO
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF P_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = P_REQNO
              ZFAMDNO = P_AMDNO.
  ENDIF.
ENDFORM.                               " P2000_LOCK_MODE_SET

*&---------------------------------------------------------------------*
*&      Form  P3000_FILE_CREATE
*&---------------------------------------------------------------------*
FORM P3000_FILE_CREATE.
  W_ZFCDDOC = 'APPLOG'.
*>>> Field Move..
  W_ZFDHSRO = LFA1-BAHNS.              " 식별자.
  W_LFA1-BAHNS = LFA1-BAHNS.
  W_ZFDHREF = ZTLG-ZFBLNO.         " 참조번호.
  MOVE '-'             TO W_ZFDHREF+10(1).
  MOVE ZTLG-ZFLGSEQ TO W_ZFDHREF+11(5).
  W_ZFDHENO = ZTLG-ZFDOCNO.         " 문서번호.

*>>> EDI 관리번호 Setting..
  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
      EXPORTING
           W_ZFCDDOC = W_ZFCDDOC
           W_ZFDHSRO = W_ZFDHSRO
           W_ZFDHREF = W_ZFDHREF
           W_BUKRS   = ZTLG-BUKRS
      CHANGING
           W_ZFDHENO = W_ZFDHENO
      EXCEPTIONS
           DB_ERROR  = 4
           NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

  CLEAR : W_EDI_RECORD.

*>>> EDI Data Create..
  CALL FUNCTION 'ZIM_MAT_APPLOG_EDI_DOC'
       EXPORTING
            W_ZFBLNO     = ZTLG-ZFBLNO
            W_ZFLGSEQ    = ZTLG-ZFLGSEQ
            W_ZFDHENO    = W_ZFDHENO
            W_BAHNS      = W_LFA1-BAHNS
       IMPORTING
            W_EDI_RECORD = W_EDI_RECORD
       EXCEPTIONS
            CREATE_ERROR = 4.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*>>> Internal Table Write..
  IT_EDIFILE-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE.

ENDFORM.                               " P3000_FILE_CREATE
*&---------------------------------------------------------------------*
*&      Form  P2000_COMPANY_CODE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_COMPANY_CODE_CHECK USING    P_BUKRS.

  IF NOT P_BUKRS IS INITIAL.
    SELECT SINGLE * FROM ZTIMIMGTX
           WHERE   BUKRS  EQ  P_BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE E949 WITH P_BUKRS.
    ELSE.
      IF ZTIMIMGTX-ZFEDIYN NE 'X'.
        MESSAGE E990 WITH P_BUKRS.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " P2000_COMPANY_CODE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_DOWNLOAD_EDI_FILE
*&---------------------------------------------------------------------*
FORM P3000_DOWNLOAD_EDI_FILE.

  PERFORM P2000_MULTI_SELECTION.
  IF W_SELECTED_LINES EQ 0.
    MESSAGE S766.
    EXIT.
  ELSE.
    LOOP AT IT_SELECTED.
      PERFORM P3000_DATA_UPDATE USING W_OK_CODE. " 데이타 반영.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " P3000_DOWNLOAD_EDI_FILE
*&---------------------------------------------------------------------*
*&      Form  P3000_TO_CLIENT_PC_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P3000_TO_CLIENT_PC_DOWNLOAD.
  DATA: WL_FILENAME LIKE RLGRAP-FILENAME.

  CONCATENATE 'C:\' W_ZFCDDOC SY-DATUM SY-UZEIT '.itf'
         INTO WL_FILENAME.
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME = WL_FILENAME
            FILETYPE = 'ASC'
       TABLES
            DATA_TAB = IT_TAB_DOWN.

  CLEAR:   W_EDI_RECORD,
           IT_TAB_DOWN.
  REFRESH: IT_TAB_DOWN.
ENDFORM.                               " P3000_TO_CLIENT_PC_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  P1000 Read L/G
*&---------------------------------------------------------------------*
FORM P1000_READ_LG   USING    W_ERR_CHK.

  SELECT * INTO TABLE IT_TAB
         FROM ZTLG
        WHERE ZFBLNO   IN S_ZFBLNO
          AND ZFEDIST  IN S_EDIST
          AND ZFEDICK  IN S_EDICK
          AND ZFDOCST  NE 'O'.
  IF SY-SUBRC NE 0.
    MOVE 'Y' TO W_ERR_CHK.
  ENDIF.

  LOOP AT IT_TAB.
    SELECT SINGLE *
             FROM ZTBL
            WHERE ZFBLNO = IT_TAB-ZFBLNO
              AND ZFHBLNO  IN S_HBLNO
              AND ZFREBELN IN S_EBELN.
    MOVE ZTBL-ZFBLNO    TO IT_TAB-ZFBLNO.
    MOVE ZTBL-ZFHBLNO   TO IT_TAB-ZFHBLNO.
    MOVE ZTBL-ZFBLAMT   TO IT_TAB-ZFBLAMT.
    MOVE ZTBL-ZFBLAMC   TO IT_TAB-ZFBLAMC.
    MOVE ZTBL-ZFREBELN  TO IT_TAB-ZFREBELN.
    MODIFY IT_TAB INDEX SY-TABIX.

  ENDLOOP.
ENDFORM.                               " P1000_READ_LG.
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LG
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LG USING    P_ZFBLNO
                            P_ZFLGSEQ.
  SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
  SET PARAMETER ID 'ZPBLNO'    FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPLGSEQ'   FIELD P_ZFLGSEQ.

  CALL TRANSACTION 'ZIM28' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_LG
*&---------------------------------------------------------------------*
*&      Form  P3000_FILE_TRANSFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FILE_TRANSFER .

*>> EDI SERVER로 FTP CONNECT.
   PERFORM  P2000_FTP_CONNECT.
   IF W_ERR_CHK  EQ  'Y'.  EXIT.  ENDIF.

*>> FILE TRANSFER.
   PERFORM  P2000_FTP_COMMAND.

*>> EDI SERVER FTP DISCONNECT.
   PERFORM  P2000_FTP_DISCONNECT.

ENDFORM.                    " P3000_FILE_TRANSFER

*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_CONNECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_CONNECT .

   PERFORM P2000_SHOW_BAR USING 'File Server에 로그인 중입니다...' 0.

DATA: mc_password(20) TYPE c ,
      mc_userid(20)   TYPE c ,
      mi_key          TYPE i VALUE 26101957,
      mi_pwd_len      TYPE i.

   DESCRIBE FIELD MC_PASSWORD LENGTH MI_PWD_LEN.
*
**-- FTP_CONNECT requires an encrypted password to work
   MC_PASSWORD = 'edi_int'.
   CALL 'AB_RFC_X_SCRAMBLE_STRING'
      ID 'SOURCE' FIELD MC_PASSWORD ID 'KEY'         FIELD MI_KEY
      ID 'SCR'    FIELD 'X'         ID 'DESTINATION' FIELD MC_PASSWORD
      ID 'DSTLEN' FIELD MI_PWD_LEN.

  CALL FUNCTION 'FTP_CONNECT'
       EXPORTING
           USER            = 'edi_int'
           PASSWORD        = MC_PASSWORD
           HOST            = '10.135.9.34'
           RFC_DESTINATION = 'SAPFTP'
      IMPORTING
           HANDLE          = MI_HANDLE
      EXCEPTIONS
           NOT_CONNECTED   = 1
           OTHERS          = 2.

   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S201(04) WITH ZTIMIMGTX-HOST.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S202(04) WITH ZTIMIMGTX-UNAME ZTIMIMGTX-HOST.
   ENDCASE.

ENDFORM.                    " P2000_FTP_CONNECT

*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_COMMAND .

DATA: L_STRLEN           TYPE I,
      L_START            TYPE I,
      L_END              TYPE I,
      L_LEN              TYPE I,
      L_POSITION1        TYPE I,
      L_POSITION2        TYPE I,
      L_FIRST_CHK        TYPE C VALUE 'N',
      L_COMMAND(90)      TYPE C.

   CONCATENATE 'cd' 'edi_send' INTO L_COMMAND SEPARATED BY SPACE.

*> INBOUND DIRECTORY로 이동.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = L_COMMAND
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

   CLEAR : L_COMMAND.
   CONCATENATE ZTIMIMGTX-ZFPATH '/' W_FILENAME INTO L_COMMAND.
   CONCATENATE 'put' L_COMMAND W_FILENAME
                     INTO L_COMMAND SEPARATED BY SPACE.

*> INBOUND DIRECTORY로 이동.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = L_COMMAND
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 0.
         DELETE DATASET  W_FILENAME.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

ENDFORM.                    " P2000_FTP_COMMAND

*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_DISCONNECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_DISCONNECT .

*> FTP DISCONNET.
   CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
            HANDLE   =   MI_HANDLE
      EXCEPTIONS
            OTHERS   =   1.

ENDFORM.                    " P2000_FTP_DISCONNECT
