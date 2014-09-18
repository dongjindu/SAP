*&---------------------------------------------------------------------*
*& Report  ZRIMINSCRTB                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : Insurance document Multi-Create Program               *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.04.12                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [변경내용] 2002.09.06 정승연.
*                - B/L기준 부보
*&---------------------------------------------------------------------*
REPORT  ZRIMINSCRTB   MESSAGE-ID ZIM
                     LINE-SIZE 129
                     NO STANDARD PAGE HEADING.

TABLES : ZTINSB.             " 보험 부보 TABLE

* INTERNAL TABLE이상*---------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK       TYPE C,                        " MARK
       UPDATE_CHK TYPE C,                        " DB Y/N.
       ZFMATGB    LIKE ZTBL-ZFMATGB,             " Material Type.
       ZFREBELN   LIKE ZTBL-ZFREBELN,            " Purchasing document
       ZFBLNO     LIKE ZTBL-ZFBLNO,              " B/L Document No.
       ZFHBLNO    LIKE ZTBL-ZFHBLNO,             " House B/L.
       ZFBLAMT(18) TYPE C,                       " Open Amount TEXT
       ZFBLAMC      LIKE ZTBL-ZFBLAMC,           " Currency
       EKGRP      LIKE ZTBL-EKGRP,               " Purchasing group
       ZFWERKS    LIKE ZTBL-ZFWERKS,             " Plant
       LIFNR      LIKE ZTBL-LIFNR,               " Vendor Code
       NAME1(17),                                " Name 1
       ZFBENI     LIKE ZTBL-ZFBENI,              " Beneficairy
       NAME2(17),                                " Name 1
       ZFCARC      TYPE ZTBL-ZFCARC,             " Loading Country
       ZFSPRT      TYPE ZTBL-ZFAPRT,             " Loading Port
       ZFAPPC      TYPE ZTBL-ZFAPPC,             " Arriving Country
       ZFAPRT      TYPE ZTBL-ZFAPRT,             " Arriving Port
       INCO1       LIKE ZTBL-INCO1,              " Incoterms
       ZFVIA       LIKE ZTBL-ZFVIA,              " VIA
       ZFETD       LIKE ZTBL-ZFETD,              " Expected shipping DT.
       ZFETA       LIKE ZTBL-ZFETA,              " Expected arriving DT.
       MATNM(9)    TYPE C.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Tables And Variance Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMINSCRTBTOP.
INCLUDE   ZRIMBDCCOM.     " 수입의뢰 BDC 공통 Include
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR ZTBL-BUKRS NO-EXTENSION
                                         NO INTERVALS,
                S_WERKS   FOR ZTBL-ZFWERKS,  " 대표 plant
                S_EBELN   FOR ZTBL-ZFREBELN,    " P/O Number
                S_ZFBENI  FOR ZTBL-ZFBENI,      " Benef.
                S_LIFNR   FOR ZTBL-LIFNR,       " Vendor.
                S_EKORG   FOR ZTBL-EKORG,       " Purch. Org.
                S_EKGRP   FOR ZTBL-EKGRP,       " Purch. Grp.
                S_HBLNO   FOR ZTBL-ZFHBLNO,     " House B/L
                S_BLNO    FOR ZTBL-ZFBLNO,      " B/L 관리번호.
                S_ETD     FOR ZTBL-ZFETD,       " 출항예정일.
                S_VIA     FOR ZTBL-ZFVIA.       " Via.
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
* EVENT AT SELECTION-SCREEN.
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.
  DISP_MODE = 'N'.

*-----------------------------------------------------------------------
* EVENT INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* EVENT TOP-OF-PAGE.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  IF SY-LANGU EQ '3'.
     PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ELSE.
     PERFORM   P3000_TITLE_WRITE_EN.
  ENDIF.

*-----------------------------------------------------------------------
* EVENT START OF SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함수.
  PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* B/L 테이블 SELECT.
  PERFORM   P1000_GET_ZTBL      USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 Text Table SELECT.
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write.
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
  CLEAR IT_TAB.
*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  IF IT_TAB-ZFBLNO IS INITIAL.
    MESSAGE S951.
  ELSE.
    CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
        W_FIELD_NM = 'ZFOPBN'.
        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
        PERFORM HANDLE_SORT TABLES  IT_TAB
                            USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해제.
        PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.

      WHEN 'DISP'.          " B/L 조회.
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES EQ 1.
          READ TABLE IT_SELECTED INDEX 1.
          PERFORM P2000_SHOW_BL USING IT_SELECTED-ZFBLNO.
        ELSEIF W_SELECTED_LINES GT 1.
          MESSAGE E965.
        ENDIF.
      WHEN 'CRTD'.          " insurance Document
        PERFORM P2000_MULTI_SELECTION.
        IF W_SELECTED_LINES GT 1.
           MESSAGE  E965.
           EXIT.
        ELSEIF W_SELECTED_LINES EQ 0.
           MESSAGE  E766.
           EXIT.
        ELSE.
           SET PARAMETER ID 'ZPHBLNO'  FIELD ''.
           SET PARAMETER ID 'ZPBLNO'   FIELD IT_SELECTED-ZFBLNO.
           CALL TRANSACTION 'ZIMB1' AND SKIP FIRST SCREEN.
        ENDIF.
        PERFORM  P1000_RESET_LIST.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
        PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
        PERFORM  P1000_RESET_LIST.
      WHEN OTHERS.
    ENDCASE.
    CLEAR IT_TAB.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM40'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ Insurance 생성 대상 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 90 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
*            '개설확정'    ,  SY-VLINE NO-GAP,
            (10) '구매문서'      CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (23) '개설 금액'     CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (04) 'Plnt'          CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (25) '선  적  지'    CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (03) 'Inc'           CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (32) 'Vendor'        CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (10) '선적일'        CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (9)  '자재구분'      CENTERED  NO-GAP,  SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
*            '자재납기'    ,  SY-VLINE NO-GAP,
            (10) 'B/L관리No'     CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (23) 'House B/L No.' CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (04) 'PGr'           CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (25) '도  착  지'    CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (03) 'VIA'           CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (32) 'Beneficiary'   CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (10) '도착일'        CENTERED  NO-GAP,  SY-VLINE NO-GAP,
            (9)  '     '         CENTERED  NO-GAP,  SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.

* 수입 CONFIG 관련 정보 GET.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFINMT NE '2'.
    MESSAGE  S111(ZIM1).
    W_ERR_CHK = 'Y'.   EXIT.
  ENDIF.

*-------< 2002.12.03 NHJ 주석처리 >------------------------------
*  IF DISP_MODE NE 'N'.            " BACKGROUND이 아닐 경우.
*    AUTHORITY-CHECK OBJECT 'ZM_BDC_MGT'
*            ID 'ACTVT' FIELD '*'.

*    IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'BDC 관리자'.
*      W_ERR_CHK = 'Y'.   EXIT.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTBL
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTBL  USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting
  CLEAR IT_BLTMP. REFRESH IT_BLTMP.
  SELECT * INTO TABLE IT_BLTMP FROM ZTBL
          WHERE BUKRS       IN   S_BUKRS
            AND ZFWERKS     IN   S_WERKS        " 대표 plant
            AND ZFREBELN    IN   S_EBELN        " P/O Number
            AND ZFBENI      IN   S_ZFBENI       " Benef.
            AND LIFNR       IN   S_LIFNR        " Vendor.
            AND EKORG       IN   S_EKORG        " Purch. Org.
            AND EKGRP       IN   S_EKGRP        " Purch. Grp.
            AND ZFHBLNO     IN   S_HBLNO        " House B/L
            AND ZFBLNO      IN   S_BLNO         " B/L 관리번호.
            AND ZFETD       IN   S_ETD          " 출항예정일.
            AND ZFVIA       IN   S_VIA          " Via.
            AND ( ZFINSYN EQ 'A'  OR    ZFINSYN EQ 'Z' )
            AND   ZFINSYN NE SPACE.

  IF SY-SUBRC NE 0.               " Not Found.
    W_ERR_CHK = 'Y'.  MESSAGE S966.  EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_ZTBL
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.
  CLEAR : IT_TAB.
  REFRESH : IT_TAB.

  SORT  IT_BLTMP BY ZFBLNO DESCENDING.

  LOOP AT IT_BLTMP.
    W_TABIX = SY-TABIX.
    READ TABLE IT_TAB WITH KEY ZFBLNO = IT_BLTMP-ZFBLNO.
    IF SY-SUBRC EQ 0.
      CONTINUE.
    ENDIF.

* 보험 문서 조회*-------------------------------------------------------

    SELECT COUNT( * ) INTO W_COUNT
                           FROM ZTINSB
                           WHERE ZFBLNO EQ IT_BLTMP-ZFBLNO.
    IF W_COUNT GT 0.    CONTINUE.     ENDIF.

    MOVE-CORRESPONDING IT_BLTMP  TO  IT_TAB.

* VENDOR MASTER SELECT( LFA1 )*-----------------------------------------
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-LIFNR
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    CASE SY-SUBRC.
      WHEN 01.     MESSAGE E022.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
    ENDCASE.
    MOVE: LFA1-NAME1   TO   IT_TAB-NAME1.

* Bene. MASTER SELECT( LFA1 )*------------------------------------------
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-ZFBENI
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    CASE SY-SUBRC.
      WHEN 01.     MESSAGE E022.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
    ENDCASE.
    MOVE: LFA1-NAME1   TO   IT_TAB-NAME2.

* 자재구분명 SET!
    SELECT SINGLE *   FROM DD07T
    WHERE  DOMNAME    EQ   'ZDMATGB'
    AND    DDLANGUAGE EQ   SY-LANGU
    AND    VALPOS     EQ   IT_TAB-ZFMATGB.
    MOVE   DD07T-DDTEXT    TO  IT_TAB-MATNM.


    APPEND  IT_TAB.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE < 1.
    W_ERR_CHK = 'Y'.  MESSAGE S009.    EXIT.
  ENDIF.

  SORT  IT_TAB  BY ZFBLNO .

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  READ TABLE IT_BLTMP INDEX 1.
  SET PF-STATUS 'ZIM06'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIM40'.           " GUI TITLE SETTING..

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
  IF SY-LANGU EQ '3'.
     PERFORM   P3000_TITLE_WRITE.
  ELSE.
     PERFORM   P3000_TITLE_WRITE_EN.
  ENDIF.
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFBLNO  TO IT_SELECTED-ZFBLNO,
             IT_TAB-ZFMATGB TO IT_SELECTED-ZFMATGB.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  W_ERR_CHK = 'N'.
  IF W_SELECTED_LINES EQ 0.
    MESSAGE S951.
    W_ERR_CHK = 'Y'.
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
    WRITE : / 'Total', W_COUNT, 'cases'.
  ENDIF.


ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE NO-GAP,
       (10) IT_TAB-ZFREBELN   NO-GAP,            " 구매문서번호.
            SY-VLINE NO-GAP,
       (05) IT_TAB-ZFBLAMC NO-GAP,              " currency
       (18) IT_TAB-ZFBLAMT  CURRENCY IT_TAB-ZFBLAMC NO-GAP,
            SY-VLINE NO-GAP,
       (04) IT_TAB-ZFWERKS NO-GAP,              " Payment Terms
            SY-VLINE NO-GAP,
       (05) IT_TAB-ZFCARC  NO-GAP,               " 선적국.
       (20) IT_TAB-ZFSPRT  NO-GAP,               " 선적항.
            SY-VLINE NO-GAP,
       (03) IT_TAB-INCO1   NO-GAP,               " Incoterms
            SY-VLINE NO-GAP,
       (12) IT_TAB-LIFNR   NO-GAP,               " Vendor.
       (20) IT_TAB-NAME1   NO-GAP,
            SY-VLINE NO-GAP,
       (10) IT_TAB-ZFETD   NO-GAP,              "선적일.
            SY-VLINE NO-GAP,
       (9)  IT_TAB-MATNM   NO-GAP,
            SY-VLINE NO-GAP.

*  WRITE : IT_TAB-NAME4 NO-GAP, SY-VLINE.         " 차입기관.

* hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE, ' ',
          SY-VLINE NO-GAP,
       (10) IT_TAB-ZFBLNO NO-GAP,               " B/L 관리번호.
            SY-VLINE NO-GAP,
       (23) IT_TAB-ZFHBLNO NO-GAP,              " House  B/L.
            SY-VLINE NO-GAP,
       (04) IT_TAB-EKGRP NO-GAP,                 " Purchasing Group
            SY-VLINE NO-GAP,
       (05) IT_TAB-ZFAPPC NO-GAP,              " 도착국.
       (20) IT_TAB-ZFAPRT  NO-GAP,             " 도착항.
            SY-VLINE NO-GAP,
       (03) IT_TAB-ZFVIA  NO-GAP,              " VIA
            SY-VLINE NO-GAP,
       (12) IT_TAB-ZFBENI  NO-GAP,             " Beneficiary
       (20) IT_TAB-NAME2   NO-GAP,
            SY-VLINE NO-GAP,
       (10) IT_TAB-ZFETA   NO-GAP,              "도착일.
            SY-VLINE NO-GAP,
        (9) '     '        NO-GAP,
            SY-VLINE NO-GAP.
* stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.
ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.

  CALL  FUNCTION  'POPUP_TO_CONFIRM'
        EXPORTING
            TITLEBAR        = '적하보험 생성 확인'
            DIAGNOSE_OBJECT = ''
            TEXT_QUESTION   =
                 '적하보험 생성작업을 계속 진행하시겠습니까?'
            TEXT_BUTTON_1   = '확    인'
            TEXT_BUTTON_2   = '아 니 오'
            DEFAULT_BUTTON  = '1'
            DISPLAY_CANCEL_BUTTON = 'X'
            START_COLUMN    = 30
            START_ROW       = 8
        IMPORTING
            ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFBLNO.
  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
  SET PARAMETER ID 'ZPBLNO' FIELD P_ZFBLNO.
  EXPORT 'BES'           TO MEMORY ID 'BES'.
  EXPORT 'ZPBLNO'       TO MEMORY ID 'ZPBLNO'.
  EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_BL
*&---------------------------------------------------------------------*
*&      Form  P1000_RESET_LIST
*&---------------------------------------------------------------------*
FORM P1000_RESET_LIST.
* 구매의뢰 테이블 SELECT
  PERFORM   P1000_GET_ZTBL      USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
* 레포트 관련 Text Table SELECT
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
  PERFORM RESET_LIST.

ENDFORM.                    " P1000_RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_BDC_DATA_MAKE
*&---------------------------------------------------------------------*
*&    부보생성 BDC
*&---------------------------------------------------------------------*
FORM P2000_BDC_DATA_MAKE.
  REFRESH : BDCDATA.

  LOOP AT IT_SELECTED.
    LINE = ( SY-TABIX / W_SELECTED_LINES ) * 100.
    OUT_TEXT = 'JOB PROGRESS %99999%%'.
    REPLACE '%99999%' WITH LINE INTO OUT_TEXT.
    PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE.

    REFRESH : BDCDATA.
* 초기화면 CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM01' '4100'.
* 초기화면 FIELD
    PERFORM P2000_DYNPRO USING :
         ' ' 'ZSREQHD-ZFHBLNO'   '',               " House B/L No.
         ' ' 'ZSREQHD-ZFBLNO' IT_SELECTED-ZFBLNO, " B/L 관리 No.
         ' ' 'BDC_OKCODE'      '/00'.               " ENTER
* 주화면   CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM01' '4101'.
* 저장.
    PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'SAVE'.
* 저장 확인 CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM01' '0001'.
* 저장.
    PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'YES'.
* CALL TRANSACTION
    PERFORM P2000_CALL_TRANSACTION  USING 'ZIMB1'
                                    CHANGING  W_SUBRC.
* 오류 발생시 LOG
    IF W_SUBRC NE 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_BDC_DATA_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_SEND_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SEND_MESSAGE.

  CALL  FUNCTION  'POPUP_TO_CONFIRM'
        EXPORTING
            TITLEBAR        = '보험부보 신청 확인'
            DIAGNOSE_OBJECT = ''
            TEXT_QUESTION   =
                 '적하보험 부보신청 작업을 계속 진행하시겠습니까?'
            TEXT_BUTTON_1   = '확    인'
            TEXT_BUTTON_2   = '아 니 오'
            DEFAULT_BUTTON  = '1'
            DISPLAY_CANCEL_BUTTON = 'X'
            START_COLUMN    = 30
            START_ROW       = 8
        IMPORTING
            ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_SEND_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_BDC_DATA_MAKE1
*&---------------------------------------------------------------------*
*      부보의뢰 BDC.
*----------------------------------------------------------------------*
FORM P2000_BDC_DATA_MAKE1.

  REFRESH : BDCDATA.

  LOOP AT IT_SELECTED.
    LINE = ( SY-TABIX / W_SELECTED_LINES ) * 100.
    OUT_TEXT = 'JOB PROGRESS %99999%%'.
    REPLACE '%99999%' WITH LINE INTO OUT_TEXT.
    PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE.

    REFRESH : BDCDATA.
* 초기화면 CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '4100'.
* 초기화면 FIELD
    PERFORM P2000_DYNPRO USING :
         ' ' 'ZSREQHD-ZFHBLNO'   '',               " House B/L.
         ' ' 'ZSREQHD-ZFBLNO' IT_SELECTED-ZFBLNO, " B/L NO.
         ' ' 'BDC_OKCODE'      '/00'.               " ENTER
* 주화면   CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '4101'.
* 저장.
    PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'LGIS'.
* 저장 확인 CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '0001'.
* 저장.
    PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'YES'.
* CALL TRANSACTION
    PERFORM P2000_CALL_TRANSACTION  USING 'ZIMB1'
                                    CHANGING  W_SUBRC.
* 오류 발생시 LOG
    IF W_SUBRC NE 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_BDC_DATA_MAKE1
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0100 OUTPUT.

   SET TITLEBAR 'POPU' WITH '포괄보험 Data Get!'.
*   SET PF-STATUS 'POPU'.  TEMP

ENDMODULE.                 " SET_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0100 INPUT.

  IF OK-CODE EQ 'CANC' OR OK-CODE EQ 'NO'.
     ANTWORT = 'N'.
     SET SCREEN 0.   LEAVE SCREEN.
  ELSEIF OK-CODE EQ 'YES'.
     ANTWORT = 'Y'.
     SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
     ANTWORT = 'N'.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE_EN
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE_EN.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ Insurance creation object ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 90 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',                        SY-VLINE  NO-GAP,
            (10) 'Pur Doc'                        CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (23) 'Openning amounts'               CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (04) 'Plnt'                           CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (25) 'Port of loading'                CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (03) 'Inc'                            CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (32) 'Vendor'                         CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (10) 'ETD'                            CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (9)  'Mat type'                       CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE : / SY-VLINE, ' ',                        SY-VLINE  NO-GAP,
            (10) 'B/L No'                         CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (23) 'House B/L No.'                  CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (04) 'PGr'                            CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (25) 'Port of discharge'              CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (03) 'VIA'                            CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (32) 'Beneficiary'                    CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (10) 'ETA'                            CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP,
            (9)  '     '                          CENTERED  NO-GAP,
                                                  SY-VLINE  NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE_EN
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
