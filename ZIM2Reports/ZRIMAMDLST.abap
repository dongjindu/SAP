*&---------------------------------------------------------------------*
*& Report  ZRIMAMDLST                                                  *
*&---------------------------------------------------------------------*
*&  Program : Import request AMEND status                              *
*&     Name : Lee Chae-Kyung INFOLINK Ltd.                           *
*&     Date : 2001.07.03                                               *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
* [Changed description]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMAMDLST   MESSAGE-ID ZIM
                     LINE-SIZE 119
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Import request INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFREQNO    LIKE ZTREQST-ZFREQNO,      " Import request No.
       STATUS(13),                           " Doc status.
       ZFDOCST    LIKE ZTREQST-ZFDOCST,      " Doc status.
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,      " AMEND No.
       ZFOPNNO    LIKE ZTREQST-ZFOPNNO,      " L/C No.
       ZFOPNDT    LIKE ZTREQST-ZFOPNDT,      " Changed date.
       ZFAPPDT    LIKE ZTREQST-ZFAPPDT,      " Change expected date.
       ZFOPAMT    LIKE ZTREQST-ZFOPAMT,      " Opening amount.
       CHANGEAM   LIKE ZTREQST-ZFOPAMT,      " Balance opening amount.
       WAERS      LIKE ZTREQST-WAERS,        " Currency
       BUHO(3),                              " Mark.
       TXZ01      LIKE ZTREQIT-TXZ01,        " Material description.
       MATNR      LIKE ZTREQIT-MATNR.        " Material No.
DATA : END OF IT_TAB.

DATA : BEGIN OF IT_TAB_DOWN OCCURS 0,
       ZFREQNO    LIKE ZTREQST-ZFREQNO,      " Import request No.
       STATUS(13),                           " Doc status.
       ZFDOCST    LIKE ZTREQST-ZFDOCST,      " Doc status.
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,      " AMEND No.
       ZFOPNNO    LIKE ZTREQST-ZFOPNNO,      " L/C No.
       ZFOPNDT    LIKE ZTREQST-ZFOPNDT,      " Changed date.
       ZFAPPDT    LIKE ZTREQST-ZFAPPDT,      " Change expected date.
       ZFOPAMT(18),                          " Opening amount.
       BUHO(3),                              " Mark.
       CHANGEAM(18),                         " Balance opening amount.
       WAERS      LIKE ZTREQST-WAERS,        " Currency.
       TXZ01      LIKE ZTREQIT-TXZ01,        " Material description.
       MATNR      LIKE ZTREQIT-MATNR.        " Material No.
DATA : END OF IT_TAB_DOWN.

*-----------------------------------------------------------------------
* Tables & variables Define
*-----------------------------------------------------------------------
TABLES : ZTREQHD,         " Import request Header
         ZTREQIT,         " Import request Item
         ZTREQST,         " Import request Status
         ZTIMIMG00.       " BASIC CONFIG.

*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
       ZFREQNO    LIKE ZTREQST-ZFREQNO,        " Import request No.
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO.        " Amend Seq.
DATA: END OF IT_SELECTED.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,               " Selection LINE COUNT
       W_PAGE            TYPE I,               " Page Counter
       W_LINE            TYPE I,               " Page LINE COUNT
       LINE(3)           TYPE N,               " Page LINE COUNT
       W_COUNT           TYPE I,               " Total COUNT
       W_ITCOUNT         TYPE C,               " ITEM COUNT
       W_OPNNO           LIKE ZTREQST-ZFOPNNO,
       W_REQNO           LIKE ZTREQST-ZFREQNO,
       W_TXZ01           LIKE ZTREQIT-TXZ01,     " Material description.
       W_MATNR           LIKE ZTREQIT-MATNR,     " Material No.
       W_MIN_ZFITMNO     LIKE ZTREQIT-ZFITMNO,   " Item No.
       OLDREQNO          LIKE ZTREQST-ZFREQNO,   " Imp request No.
       OLDLASTAM         LIKE ZTREQHD-ZFLASTAM,  " OLD Opening amount.
       OLDAMDNO          LIKE ZTREQST-ZFAMDNO,
       P_BUKRS           LIKE ZTREQHD-BUKRS.

INCLUDE   ZRIMSORTCOM.    " Include for report sort
INCLUDE   ZRIMUTIL01.     " Utility function

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                             NO INTERVALS,
                 S_RLDT    FOR ZTREQST-ZFOPNDT,  " Opening date.
                 S_APPDT   FOR ZTREQST-ZFAPPDT,  " Open expected date.
                 S_OPBN    FOR ZTREQHD-ZFOPBN,   " Opening bank.
                 S_MATGB   FOR ZTREQHD-ZFMATGB,  " Material type.
                 S_REQTY   FOR ZTREQHD-ZFREQTY,  " 수입의뢰 Type
                 S_WERKS   FOR ZTREQHD-ZFWERKS,  " Rep plant
                 S_EKORG   FOR ZTREQST-EKORG,    " Purch. Org.
                 S_EBELN   FOR ZTREQHD-EBELN,    " P/O Number
                 S_LIFNR   FOR ZTREQHD-LIFNR,    " Vendor
                 S_ZFBENI  FOR ZTREQHD-ZFBENI,   " Beneficiary
                 S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
                 S_REQNO   FOR ZTREQHD-ZFREQNO.  " Imp request No.
SELECTION-SCREEN END OF BLOCK B1.
* PARAMETER 초기값 Setting
INITIALIZATION.                          " Initial value SETTING
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 권한 검증 함수.
*   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
*   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*  테이블 SELECT
  PERFORM   P1000_GET_ZTREQST      USING   W_ERR_CHK.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
* SORT 선택.
    WHEN 'STUP' OR 'STDN'.         " SORT 선택.
      PERFORM RESET_LIST.
*         W_FIELD_NM = 'ZFREQNO'.
*         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
*         PERFORM HANDLE_SORT TABLES  IT_TAB
*                             USING   SY-UCOMM.
* 전체 선택 및 선택해제.
    WHEN 'MKAL' OR 'MKLO'.          " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
    WHEN 'DISP'.                    " L/C 조회.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_LC USING  IT_SELECTED-ZFREQNO
                                     IT_SELECTED-ZFAMDNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM P3000_CREATE_DOWNLOAD_FILE.
      PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'REFR'.
*  테이블 SELECT
      PERFORM P1000_GET_ZTREQST   USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM RESET_LIST.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR25'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /48  '[ Import request amend status ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /3 'Date : ', SY-DATUM, 100 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,  ' ',                       SY-VLINE,
                 (20)  'L  /  C     N O',         SY-VLINE,
                 (16)  'Change classification'CENTERED,     SY-VLINE,
                 (12)  'Change due date',              SY-VLINE,
                 (30)  'I t e m',           SY-VLINE,
                 (21)  'Opening amount'CENTERED,  SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE,  ' ',                       SY-VLINE,
                 (20)  'Import request No',        SY-VLINE,
                 (16)  'AMEND count'CENTERED,     SY-VLINE,
                 (12)  'Change date',              SY-VLINE,
                 (30)  'Itme code',           SY-VLINE,
                 (21)  'Balance'CENTERED,  SY-VLINE,
                                                / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
  AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
           ID 'ACTVT' FIELD '*'.

  IF SY-SUBRC NE 0.
    MESSAGE S960 WITH SY-UNAME 'Request Release Transaction'.
    W_ERR_CHK = 'Y'.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTREQHD
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTREQST   USING   W_ERR_CHK.

  REFRESH IT_TAB.
  SELECT *
    FROM  ZTREQST
    WHERE ZFOPNDT    IN     S_RLDT     " 개설일.
      AND ZFAPPDT    IN     S_APPDT    " 개설예정일.
      AND EKORG      IN     S_EKORG.

*>> SELECT 헤더 테이블.
    SELECT SINGLE *
      FROM ZTREQHD
     WHERE ZFOPBN  IN S_OPBN
       AND BUKRS   IN S_BUKRS
       AND ZFMATGB IN S_MATGB
       AND ZFREQTY IN S_REQTY
       AND ZFWERKS IN S_WERKS
       AND EBELN   IN S_EBELN
       AND LIFNR   IN S_LIFNR
       AND ZFBENI  IN S_ZFBENI
       AND ZFREQNO IN S_REQNO
       AND ZFREQNO = ZTREQST-ZFREQNO.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
    IF ZTREQST-ZFDOCST NE 'A'.
      IF ZTREQST-ZFAMDNO = '00000'.
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR IT_TAB.
    MOVE-CORRESPONDING   ZTREQST TO IT_TAB.
*>> 문서상태 text.
    CLEAR: DD07T,IT_TAB-STATUS.
    SELECT * FROM DD07T UP TO 1 ROWS
                        WHERE DOMNAME    EQ 'ZDDOCST'
                          AND DDLANGUAGE EQ SY-LANGU
                          AND AS4LOCAL   EQ 'A'
                          AND DOMVALUE_L EQ ZTREQST-ZFDOCST
                          ORDER BY AS4VERS DESCENDING.
    ENDSELECT.
    IT_TAB-STATUS = DD07T-DDTEXT.

*>> 차감금액 계산.
    IF IT_TAB-ZFAMDNO NE '00000'.
      OLDAMDNO = IT_TAB-ZFAMDNO - 1.
      SELECT SINGLE ZFOPAMT INTO OLDLASTAM
      FROM  ZTREQST
      WHERE ZFREQNO = IT_TAB-ZFREQNO
        AND ZFAMDNO = OLDAMDNO.
      IF SY-SUBRC EQ 0.
        IF     IT_TAB-ZFOPAMT > OLDLASTAM.
          IT_TAB-CHANGEAM = IT_TAB-ZFOPAMT - OLDLASTAM.
          IT_TAB-BUHO = '(+)'.
        ELSEIF IT_TAB-ZFOPAMT < OLDLASTAM.
          IT_TAB-CHANGEAM = OLDLASTAM - IT_TAB-ZFOPAMT.
          IT_TAB-BUHO = '(-)'.
        ELSE.
        ENDIF.
      ENDIF.
    ENDIF.

*>> SELECT ITEM.
    SELECT COUNT( DISTINCT ZFITMNO  ) INTO W_COUNT
      FROM ZTREQIT
     WHERE ZFREQNO = IT_TAB-ZFREQNO.
    SELECT MIN( ZFITMNO  ) INTO W_MIN_ZFITMNO
      FROM ZTREQIT
     WHERE ZFREQNO = IT_TAB-ZFREQNO.

    CLEAR: ZTREQIT,IT_TAB-TXZ01,IT_TAB-MATNR.
    SELECT SINGLE *
      FROM ZTREQIT
     WHERE ZFREQNO = IT_TAB-ZFREQNO
       AND ZFITMNO = W_MIN_ZFITMNO.

    MOVE: ZTREQIT-TXZ01 TO IT_TAB-TXZ01,
          ZTREQIT-MATNR TO IT_TAB-MATNR.

    IF W_COUNT > 1.
      W_ITCOUNT = W_COUNT - 1.
      CONCATENATE:
      IT_TAB-TXZ01  '외'  W_ITCOUNT '건'   INTO IT_TAB-TXZ01.
    ENDIF.
    APPEND IT_TAB.
  ENDSELECT.
  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0. MESSAGE S009. EXIT.ENDIF.

ENDFORM.                    " P1000_GET_ZTREQST
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'ZIMR25'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIMR25'.           " GUI TITLE SETTING..

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.  OLDREQNO = 0.
  IF SY-UCOMM EQ 'STUP'.
    SORT IT_TAB BY ZFREQNO   ZFAMDNO.
  ELSE.
    SORT IT_TAB BY ZFREQNO DESCENDING ZFAMDNO.
  ENDIF.

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
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFREQNO LIKE ZTREQST-ZFREQNO,
        ZFAMDNO LIKE ZTREQST-ZFAMDNO.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : IT_TAB-ZFREQNO  TO ZFREQNO,
         IT_TAB-ZFAMDNO  TO ZFAMDNO.
  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFREQNO  TO IT_SELECTED-ZFREQNO,
             IT_TAB-ZFAMDNO  TO IT_SELECTED-ZFAMDNO.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO,
             ZFAMDNO TO IT_SELECTED-ZFAMDNO.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ELSE.
      MESSAGE S951.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION

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

  FORMAT RESET.
  WRITE: / SY-ULINE.
  IF W_COUNT GT 0.
    WRITE : / 'Total', W_COUNT, 'records'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  IF IT_TAB-ZFREQNO NE OLDREQNO.
    MOVE: IT_TAB-ZFOPNNO TO W_OPNNO,
          IT_TAB-ZFREQNO TO W_REQNO,
          IT_TAB-TXZ01   TO W_TXZ01,
          IT_TAB-MATNR   TO W_MATNR.
  ENDIF.
  ON CHANGE OF IT_TAB-ZFREQNO.
    IF SY-TABIX NE 1.
      WRITE:/ SY-ULINE.
    ENDIF.
  ENDON.
  WRITE: / SY-VLINE, MARKFIELD  AS CHECKBOX,       SY-VLINE,
          (20) W_OPNNO,                            SY-VLINE,
          (13) IT_TAB-STATUS,(2)IT_TAB-ZFDOCST,    SY-VLINE,
          (12) IT_TAB-ZFAPPDT,                     SY-VLINE,
          (30) W_TXZ01,                            SY-VLINE,
           (3) IT_TAB-WAERS,
          (17) IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS,SY-VLINE.

* hide
  HIDE: IT_TAB.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE, ' ',                           SY-VLINE,
         (20) W_REQNO,                             SY-VLINE,
         (16) IT_TAB-ZFAMDNO,                      SY-VLINE,
         (12) IT_TAB-ZFOPNDT,                      SY-VLINE,
         (30) W_MATNR,                             SY-VLINE,
          (3) IT_TAB-BUHO RIGHT-JUSTIFIED,
         (17) IT_TAB-CHANGEAM CURRENCY IT_TAB-WAERS,SY-VLINE.

* hide
  HIDE: IT_TAB.
  W_COUNT = W_COUNT + 1.

*>> DATA RESET.
  CLEAR: W_OPNNO, W_REQNO,W_TXZ01,W_MATNR.
  MOVE IT_TAB-ZFREQNO TO OLDREQNO.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO P_ZFAMDNO.

  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
  SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
  EXPORT 'BES'           TO MEMORY ID 'BES'.
  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
  EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
  EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.

  IF P_ZFAMDNO = '00000'.
    CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
  ELSE.
    CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                    " P2000_SHOW_LC

*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM P3000_CREATE_DOWNLOAD_FILE.

  REFRESH IT_TAB_DOWN.
  LOOP AT IT_TAB.
    CLEAR IT_TAB_DOWN.
    MOVE-CORRESPONDING IT_TAB TO IT_TAB_DOWN.
    WRITE :IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS TO IT_TAB_DOWN-ZFOPAMT,
           IT_TAB-CHANGEAM CURRENCY IT_TAB-WAERS
                                              TO IT_TAB_DOWN-CHANGEAM.
    APPEND IT_TAB_DOWN.
  ENDLOOP.

ENDFORM.                    " P3000_CREATE_DOWNLOAD_FILE
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
