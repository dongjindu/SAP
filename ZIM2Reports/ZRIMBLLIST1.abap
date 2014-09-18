*&---------------------------------------------------------------------*
*& Report  ZRIMBLLIST1                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : B/L 입수내역(영업반품)                                *
*&      작성자 : 맹 성 호                                              *
*&      작성일 : 2002.11.25                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 영업반품 건에 대한 B/L 입수내역을 조회.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMBLLIST   MESSAGE-ID ZIM
                     LINE-SIZE 130
                     NO STANDARD PAGE HEADING.

TABLES : ZTBL,
         KNA1.

*-----------------------------------------------------------------------
* B/L 입수내역 리스트용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
*        ZFREBELN     LIKE ZTBL-ZFREBELN,           " 대표 PO번호
        VBELN        LIKE ZTBL-VBELN,              " 영업문서 번호.
        ZFBLNO       LIKE ZTBL-ZFBLNO,             " B/L관리번호
        ZFSHNO       LIKE ZTBL-ZFSHNO,             " 선적차수
        ZFPOYN       LIKE ZTBL-ZFPOYN,             " 유환여부
        ZFPOTY       LIKE ZTBL-ZFPOTY,             " 무환종류.
        ZFETD        LIKE ZTBL-ZFETD,              " ETD
        ZFCARNM      LIKE ZTBL-ZFCARNM,            " 선명
        ZFSPRT       LIKE ZTBL-ZFSPRT,             " 선적항
*        EKGRP        LIKE ZTBL-EKGRP,              " 구매그룹
        VKORG        LIKE ZTBL-VKORG,              " 영업조직.
        W_VKORG(20)  TYPE C,
*        LIFNR        LIKE ZTBL-LIFNR,              " Vendor
        KUNNR        LIKE ZTBL-KUNNR,              " 판매처.
        W_KUNNR(30)  TYPE C,
        ZFHBLNO      LIKE ZTBL-ZFHBLNO,            " House B/L
        ZFRGDSR      LIKE ZTBL-ZFRGDSR,            " 대표품명
        ZFETA        LIKE ZTBL-ZFETA,              " ETA
        ZFVIA        LIKE ZTBL-ZFVIA,              " VIA
        ZFAPRT       LIKE ZTBL-ZFAPRT,             " 도착항
*        ZFFORD       LIKE ZTBL-ZFFORD,             " 선사
        VTWEG        LIKE ZTBL-VTWEG,              " 유통경로.
        W_VTWEG(30)  TYPE C,
        KUNWE        LIKE ZTBL-KUNWE,              " 인도처.
        W_KUNWE(30)  TYPE C.
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
                   S_VBELN   FOR ZTBL-VBELN,       " 영업문서.
                   S_KUNNR   FOR ZTBL-KUNNR,
                   S_KUNWE   FOR ZTBL-KUNWE,
                   S_BLSDT   FOR ZTBL-ZFBLSDT      " B/L 송부일
                             NO-EXTENSION,
                   S_BLADT   FOR ZTBL-ZFBLADT      " B/L 입수일
                             NO-EXTENSION,
                   S_ZFBLST  FOR ZTBL-ZFBLST,      " 문서상태.
                   S_RPTTY   FOR ZTBL-ZFRPTTY,
                   S_ZFTRCK  FOR ZTBL-ZFTRCK,      " TRUCKER
                   S_ETA     FOR ZTBL-ZFETA        " ETA
                             NO-EXTENSION,
                   S_SPRTC   FOR ZTBL-ZFSPRTC      " 선적항
                             NO INTERVALS.
   PARAMETERS :    P_VIA     LIKE ZTBL-ZFVIA.      " VIA
   PARAMETERS :    P_POYN    LIKE ZTBL-ZFPOYN      " 유환여부
                             DEFAULT 'N'.
   SELECT-OPTIONS: S_POTY    FOR ZTBL-ZFPOTY       " 무환종류.
                             NO-EXTENSION NO INTERVALS DEFAULT 'H',
                   S_SHTY    FOR ZTBL-ZFSHTY,      " 해상운송구분
                   S_WERKS   FOR ZTBL-ZFWERKS.     " 대표 PLANT
* 후속작업 진행상태.
*  SELECTION-SCREEN BEGIN OF LINE.
*     SELECTION-SCREEN : COMMENT 1(11) TEXT-003, POSITION 33.
*     PARAMETERS : P_YES  AS CHECKBOX.     " None
*  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 헤더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
* 2002.10.31 Nashinho Block with Comment..
*   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
*   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

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
         W_FIELD_NM = 'ZFBLHNO'.
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
  SET  TITLEBAR 'ZIM24A'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /45  '[  B/L 입수 내역(영업반품)  ]' CENTERED
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /105 'Date : ', SY-DATUM.  ", 101 'Page : ', W_PAGE.

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,
            (15) '영업문서 번호'  CENTERED, SY-VLINE NO-GAP,
            (25) '유.무환'        CENTERED, SY-VLINE NO-GAP,
            (10) 'ETD'            CENTERED, SY-VLINE NO-GAP,
            (15) '선명'           CENTERED, SY-VLINE NO-GAP,
            (15) '선적항'         CENTERED, SY-VLINE NO-GAP,
            (10) '영업조직'       CENTERED, SY-VLINE NO-GAP,
            (25) '판 매 처'       CENTERED, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE : / SY-VLINE NO-GAP,
            (15) 'House B/L No'   CENTERED, SY-VLINE NO-GAP,
            (25) '대표품명'       CENTERED, SY-VLINE NO-GAP,
            (10) 'ETA'            CENTERED, SY-VLINE NO-GAP,
            (15) 'VIA'            CENTERED, SY-VLINE NO-GAP,
            (15) '도착항'         CENTERED, SY-VLINE NO-GAP,
            (10) '유통경로'       CENTERED, SY-VLINE NO-GAP,
            (25) '인 도 처'       CENTERED, SY-VLINE NO-GAP.
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
*  IF P_YES  IS INITIAL.
*     MOVE  '%'   TO  P_YES.
*  ELSE.
*     MOVE  'N%'    TO  P_YES.
*  ENDIF.

ENDFORM.                    " P2000_SET_SELETE_OPTION

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.

*      IT_TAB-ZFKRW = 'KRW'.
*-----------------------------------------------------------------------
* CUSTOMER MASTER SELECT( KNA1 )
*-----------------------------------------------------------------------
      CALL FUNCTION 'READ_KNA1'
           EXPORTING
                 XKUNNR          = IT_TAB-KUNNR
           IMPORTING
                 XKNA1           = KNA1
           EXCEPTIONS
                 KEY_INCOMPLETE  = 01
                 NOT_AUTHORIZED  = 02
                 NOT_FOUND       = 03.

      MOVE: KNA1-NAME1   TO   IT_TAB-W_KUNNR.

*-----------------------------------------------------------------------
* CUSTOMER MASTER SELECT( KNA1 )
*-----------------------------------------------------------------------
      CALL FUNCTION 'READ_KNA1'
           EXPORTING
                 XKUNNR          = IT_TAB-KUNWE
           IMPORTING
                 XKNA1           = KNA1
           EXCEPTIONS
                 KEY_INCOMPLETE  = 01
                 NOT_AUTHORIZED  = 02
                 NOT_FOUND       = 03.

      MOVE: KNA1-NAME1   TO   IT_TAB-W_KUNWE.

*-----------------------------------------------------------------------
* TVKO SELECT( 영업조직 )
*-----------------------------------------------------------------------
      SELECT SINGLE VKORG INTO IT_TAB-W_VKORG
        FROM TVKO
       WHERE VKORG = IT_TAB-VKORG.

      MODIFY  IT_TAB INDEX W_TABIX.

*-----------------------------------------------------------------------
* TVTW SELECT( 유통경로 )
*-----------------------------------------------------------------------
      SELECT SINGLE VTWEG INTO IT_TAB-W_VTWEG
        FROM TVTW
       WHERE VTWEG = IT_TAB-VTWEG.

      MODIFY  IT_TAB INDEX W_TABIX.
   ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIM24'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM24A'.          " GUI TITLE SETTING..

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
     WRITE : /102 '총', W_COUNT, '건'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
  DATA: W_SO(15),
        W_DOM_TEXT(10).

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  CONCATENATE IT_TAB-VBELN '-' IT_TAB-ZFSHNO INTO W_SO.

  "DOMAIN - 유환여부.
  PERFORM  GET_DD07T USING 'ZDPOYN' IT_TAB-ZFPOYN
                     CHANGING   W_DOM_TEXT.

  WRITE : / SY-VLINE NO-GAP,
            (15) W_SO                       , SY-VLINE NO-GAP,
            (25) W_DOM_TEXT         CENTERED, SY-VLINE NO-GAP,
            (10) IT_TAB-ZFETD       CENTERED, SY-VLINE NO-GAP,
            (15) IT_TAB-ZFCARNM             , SY-VLINE NO-GAP,
            (15) IT_TAB-ZFSPRT              , SY-VLINE NO-GAP,
            (10) IT_TAB-W_VKORG             , SY-VLINE NO-GAP,
            (25) IT_TAB-W_KUNNR             , SY-VLINE NO-GAP.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

* hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  FORMAT RESET.
  WRITE : / SY-VLINE NO-GAP,
            (15) IT_TAB-ZFHBLNO             , SY-VLINE NO-GAP,
            (25) IT_TAB-ZFRGDSR             , SY-VLINE NO-GAP,
            (10) IT_TAB-ZFETA       CENTERED, SY-VLINE NO-GAP,
            (15) IT_TAB-ZFVIA               , SY-VLINE NO-GAP,
            (15) IT_TAB-ZFAPRT              , SY-VLINE NO-GAP,
            (10) IT_TAB-W_VTWEG            , SY-VLINE NO-GAP,
            (25) IT_TAB-W_KUNWE            , SY-VLINE NO-GAP.

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
* READ ZTIMIMG00.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF ZTIMIMG00-BLSTYN EQ 'X'.
      CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM22' AND SKIP  FIRST SCREEN.
   ENDIF.

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
                               AND   VBELN      IN     S_VBELN
                               AND   ZFBLST     IN     S_ZFBLST
                               AND   ZFRPTTY    IN     S_RPTTY
                               AND   ZFVIA      LIKE   P_VIA
*                               AND   VKORG     IN     S_FORD
*                               AND   ZFCAGTY    IN     S_CAGTY
                               AND   ZFPOYN     LIKE   P_POYN
                               AND   ZFPOTY     IN     S_POTY
                               AND   ZFSHTY     IN     S_SHTY
                               AND   ZFWERKS    IN     S_WERKS.
*                               AND   ZFSAMP     LIKE   P_YES.

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

*   TRANSLATE DD07T-DDTEXT TO UPPER CASE.
  P_W_NAME   = DD07T-DDTEXT.
  TRANSLATE P_W_NAME TO UPPER CASE.
ENDFORM.                    " GET_DD07T_SELECT
