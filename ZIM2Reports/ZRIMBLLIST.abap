*&---------------------------------------------------------------------*
*& Report  ZRIMBLLIST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : B/L 입수내역 List                                     *
*&      작성자 :                                                       *
*&      작성일 : 2001.11.8                                             *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
REPORT  ZRIMBLLIST   MESSAGE-ID ZIM
                     LINE-SIZE 130
                     NO STANDARD PAGE HEADING.

TABLES : ZTBL.
*-----------------------------------------------------------------------
* B/L 입수내역 리스트용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
        ZFREBELN     LIKE ZTBL-ZFREBELN,           "대표 PO번호
        ZFBLNO       LIKE ZTBL-ZFBLNO,             "B/L관리번호
        ZFSHNO       LIKE ZTBL-ZFSHNO,             "선적차수
        ZFPOYN       LIKE ZTBL-ZFPOYN,             "유환여부
        ZFETD        LIKE ZTBL-ZFETD,              "ETD
        ZFCARNM      LIKE ZTBL-ZFCARNM,            "선명
        ZFSPRT       LIKE ZTBL-ZFSPRT,             "선적항
        EKGRP        LIKE ZTBL-EKGRP,              "구매그룹
        W_EKGRP(20)  TYPE C,
        LIFNR        LIKE ZTBL-LIFNR,              "Vendor
        W_LIFNR(30)  TYPE C,
        ZFHBLNO      LIKE ZTBL-ZFHBLNO,            "House B/L
        ZFRGDSR      LIKE ZTBL-ZFRGDSR,            "대표품명
        ZFETA        LIKE ZTBL-ZFETA,              "ETA
        ZFVIA        LIKE ZTBL-ZFVIA,              "VIA
        ZFAPRT       LIKE ZTBL-ZFAPRT,             "도착항
        ZFFORD       LIKE ZTBL-ZFFORD,             "선사
        W_ZFFORD(30) TYPE C,
        ZFBENI       LIKE ZTBL-ZFBENI,             "Beneficiary
        W_ZFBENI(30) TYPE C,
        ZFREQNO      LIKE ZTREQHD-ZFREQNO,         "수입의뢰관리번호.
        ZFREQTY      LIKE ZTREQHD-ZFREQTY,         "결제구분.
        ZFOPNNO      LIKE ZTREQHD-ZFOPNNO.         "신용장번호.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMPRELTOP.    " 구매 Released  Report Data Define용 Include
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모음

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTBL-BUKRS NO INTERVALS
                                            NO-EXTENSION,
                   S_EBELN   FOR ZTBL-ZFREBELN,
                   S_REQTY   FOR ZTREQHD-ZFREQTY,  " 결제구분.
                   S_HBLNO   FOR ZTBL-ZFHBLNO,     " House B/L No.
                   S_BLSDT   FOR ZTBL-ZFBLSDT      " B/L 송부일.
                             NO-EXTENSION,
                   S_BLADT   FOR ZTBL-ZFBLADT      " B/L 입수일.
                             NO-EXTENSION,
                   S_ZFBLST  FOR ZTBL-ZFBLST,      " 문서상태.
                   S_RPTTY   FOR ZTBL-ZFRPTTY,
                   S_EKGRP   FOR ZTBL-EKGRP,       " 구매그룹.
                   S_ZFTRCK  FOR ZTBL-ZFTRCK,      " TRUCKER
                   S_ETA     FOR ZTBL-ZFETA        " ETA
                             NO-EXTENSION,
                   S_SPRTC   FOR ZTBL-ZFSPRTC      " 선적항
                             NO INTERVALS.
   PARAMETERS :    P_VIA     LIKE ZTBL-ZFVIA.      " VIA
   SELECT-OPTIONS: S_FORD    FOR ZTBL-ZFFORD.      " Forwarder
   PARAMETERS :    P_POYN    LIKE ZTBL-ZFPOYN.     " 유환여부
   SELECT-OPTIONS: S_SHTY    FOR ZTBL-ZFSHTY,      " 해상운송구분
                   S_WERKS   FOR ZTBL-ZFWERKS.     " 대표 PLANT
SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P1000_SET_BUKRS.
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 헤더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 파라메타 설?
   PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 구매의뢰 테이블 SELECT
   PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

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
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
         W_FIELD_NM = 'ZFBLNO'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.

         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'DISP'.          " L/C 조?
         PERFORM P2000_MULTI_SELECTION.

         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P2000_SHOW_LC USING IT_SELECTED-ZFREQNO.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.

      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
* 구매의뢰 테이블 SELECT
           PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.

           IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
           PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.

           IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
           PERFORM RESET_LIST.
      WHEN OTHERS.
   ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM24'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /55  '[  B/L Receipt Detail ]' CENTERED
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /100 'Date : ', SY-DATUM.  ", 101 'Page : ', W_PAGE.

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,
            (20) 'P/O No'            CENTERED, SY-VLINE NO-GAP,
            (20) 'Money/Non-Money'   CENTERED, SY-VLINE NO-GAP,
            (10) 'E.T.D'             CENTERED, SY-VLINE NO-GAP,
            (15) 'Vessel name'       CENTERED, SY-VLINE NO-GAP,
            (15) 'Port of loading'   CENTERED, SY-VLINE NO-GAP,
            (15) 'Purchase group'    CENTERED, SY-VLINE NO-GAP,
            (20) 'Vendor'            CENTERED, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE : / SY-VLINE NO-GAP,
            (20) 'B/L No'            CENTERED, SY-VLINE NO-GAP,
            (20) 'Main Item'         CENTERED, SY-VLINE NO-GAP,
            (10) 'E.T.A'             CENTERED, SY-VLINE NO-GAP,
            (15) 'VIA'               CENTERED, SY-VLINE NO-GAP,
            (15) 'Port of arrival'   CENTERED, SY-VLINE NO-GAP,
            (15) 'Forwarder'         CENTERED, SY-VLINE NO-GAP,
            (20) 'Beneficiary'       CENTERED, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

   W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'B/L Doc transaction'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.
*
  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  IF P_VIA  IS INITIAL.  P_VIA  = '%'.   ENDIF.
  IF P_POYN IS INITIAL.  P_POYN = '%'.   ENDIF.

ENDFORM.                    " P2000_SET_SELETE_OPTION

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.

*      IT_TAB-ZFKRW = 'KRW'.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
      CLEAR : LFA1.
      CALL FUNCTION 'READ_LFA1'
           EXPORTING
                 XLIFNR          = IT_TAB-ZFFORD
           IMPORTING
                 XLFA1           = LFA1
           EXCEPTIONS
                 KEY_INCOMPLETE  = 01
                 NOT_AUTHORIZED  = 02
                 NOT_FOUND       = 03.

      MOVE: LFA1-NAME1   TO   IT_TAB-W_ZFFORD.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
      CLEAR : LFA1.
      CALL FUNCTION 'READ_LFA1'
           EXPORTING
                 XLIFNR          = IT_TAB-LIFNR
           IMPORTING
                 XLFA1           = LFA1
           EXCEPTIONS
                 KEY_INCOMPLETE  = 01
                 NOT_AUTHORIZED  = 02
                 NOT_FOUND       = 03.

      MOVE: LFA1-NAME1   TO   IT_TAB-W_LIFNR.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
      CLEAR : LFA1.
      CALL FUNCTION 'READ_LFA1'
           EXPORTING
                 XLIFNR          = IT_TAB-ZFBENI
           IMPORTING
                 XLFA1           = LFA1
           EXCEPTIONS
                 KEY_INCOMPLETE  = 01
                 NOT_AUTHORIZED  = 02
                 NOT_FOUND       = 03.

      MOVE: LFA1-NAME1   TO   IT_TAB-W_ZFBENI.

*-----------------------------------------------------------------------
* T024 SELECT( 구매그룹)
*-----------------------------------------------------------------------
      SELECT SINGLE EKNAM INTO IT_TAB-W_EKGRP
        FROM T024
       WHERE EKGRP = IT_TAB-EKGRP.

      MODIFY  IT_TAB INDEX W_TABIX.
   ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIM24'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM24'.           " GUI TITLE SETTING..

*   W_PAGE = 1.   W_LINE = 0.
    W_COUNT = 0.

   LOOP AT IT_TAB.
*      W_LINE = W_LINE + 1.
*      PERFORM P2000_PAGE_CHECK.
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
        ZFAMDNO LIKE ZTREQST-ZFAMDNO,
        ZFRLST1 LIKE ZTREQST-ZFRLST1,
        ZFRLST2 LIKE ZTREQST-ZFRLST2.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFBLNO   TO ZFREQNO.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
       MOVE : IT_TAB-ZFBLNO  TO IT_SELECTED-ZFREQNO.
*
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ELSE.
      MESSAGE S962.
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

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : /102 'Total', W_COUNT, 'case'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
  DATA: W_PO(20),
        W_DOM_TEXT(20).

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO INTO W_PO.

  "DOMAIN - 유환여부.
  PERFORM  GET_DD07T USING 'ZDPOYN' IT_TAB-ZFPOYN
                     CHANGING   W_DOM_TEXT.

  WRITE : / SY-VLINE NO-GAP,
            (20) W_PO                NO-ZERO, SY-VLINE NO-GAP,
            (20) W_DOM_TEXT         CENTERED, SY-VLINE NO-GAP,
            (10) IT_TAB-ZFETD       CENTERED, SY-VLINE NO-GAP,
            (15) IT_TAB-ZFCARNM             , SY-VLINE NO-GAP,
            (15) IT_TAB-ZFSPRT              , SY-VLINE NO-GAP,
            (15) IT_TAB-W_EKGRP             , SY-VLINE NO-GAP,
            (20) IT_TAB-W_LIFNR             , SY-VLINE NO-GAP.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

* hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  FORMAT RESET.
  WRITE : / SY-VLINE NO-GAP,
            (20) IT_TAB-ZFHBLNO             , SY-VLINE NO-GAP,
            (20) IT_TAB-ZFRGDSR             , SY-VLINE NO-GAP,
            (10) IT_TAB-ZFETA       CENTERED, SY-VLINE NO-GAP,
            (15) IT_TAB-ZFVIA               , SY-VLINE NO-GAP,
            (15) IT_TAB-ZFAPRT              , SY-VLINE NO-GAP,
            (15) IT_TAB-W_ZFFORD            , SY-VLINE NO-GAP,
            (20) IT_TAB-W_ZFBENI            , SY-VLINE NO-GAP.

* hide...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.
ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.
   SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
   SET PARAMETER ID 'ZPBLNO'    FIELD P_ZFREQNO.
   EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
   EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.

* JSY 주석처리 2003.04.01
* READ ZTIMIMG00.
*  SELECT SINGLE * FROM ZTIMIMG00.
*  IF ZTIMIMG00-BLSTYN EQ 'X'.
      CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
*  ELSE.
*     CALL TRANSACTION 'ZIM22' AND SKIP  FIRST SCREEN.
*  ENDIF.

* 구매의뢰 테이블 SELECT
   PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
   PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
   PERFORM RESET_LIST.
ENDFORM.                    " P2000_SHOW_LC

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_TAB USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting
  REFRESH : IT_TAB.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE IT_TAB  FROM ZTBL
                               WHERE ZFBLSDT    IN     S_BLSDT
                               AND   BUKRS      IN     S_BUKRS
                               AND   ZFBLSDT    NE     SPACE
                               AND   ZFBLADT    IN     S_BLADT
                               AND   ZFTRCK     IN     S_ZFTRCK
                               AND   ZFETA      IN     S_ETA
                               AND   ZFWERKS    IN     S_WERKS
                               AND   ZFSPRTC    IN     S_SPRTC
                               AND   ZFREBELN   IN     S_EBELN
                               AND   ZFHBLNO    IN     S_HBLNO
                               AND   EKGRP      IN     S_EKGRP
                               AND   ZFBLST     IN     S_ZFBLST
                               AND   ZFRPTTY    IN     S_RPTTY
                               AND   ZFVIA      LIKE   P_VIA
                               AND   ZFFORD     IN     S_FORD
                               AND   ZFPOYN     LIKE   P_POYN
                               AND   ZFSHTY     IN     S_SHTY
                               AND   ZFWERKS    IN     S_WERKS.

  IF SY-SUBRC NE 0.               " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S966.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_IT_TAB

*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
FORM GET_DD07T USING    P_DOMNAME
                               P_FIELD
                      CHANGING P_W_NAME.
  CLEAR : DD07T, P_W_NAME.

  IF P_FIELD IS INITIAL.   EXIT.   ENDIF.

  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ P_FIELD
                      ORDER BY AS4VERS DESCENDING.
    EXIT.
  ENDSELECT.

  P_W_NAME   = DD07T-DDTEXT.
  TRANSLATE P_W_NAME TO UPPER CASE.
ENDFORM.                    " GET_DD07T_SELECT
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
