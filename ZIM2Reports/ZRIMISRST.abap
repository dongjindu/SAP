*&---------------------------------------------------------------------*
*& Report  ZRIMISRST                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 적하보험 미발행 LIST                                  *
*&      작성자 : 맹성호 INFOLINK Ltd.                                  *
*&      작성일 : 2001.11.09                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMISRST  MESSAGE-ID ZIM
                   LINE-SIZE 142
                   NO STANDARD PAGE HEADING.


*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMISLSTTOP.

*TABLES : ZTINS,                                  " 보험 부보 TABLE
*         ZTINSRSP,                               " 보험 부보 REPONSE
*         ZTREQHD,                                " 수입의뢰 헤더.
*         ZTREQST,                                " 수입의뢰 상태.
*         ZTIMIMG00,
*         EKKO.                                   " 구매문서헤더.
*
*-----------------------------------------------------------------------
* INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFREQNO    LIKE ZTINS-ZFREQNO,            " 수입의뢰 번호.
       ZFINSEQ    LIKE ZTINS-ZFINSEQ,            " 보험부보 일련번호.
       ZFAMDNO    LIKE ZTINS-ZFAMDNO,            " Amend Seq.
       ZFTRANS    LIKE ZTINS-ZFTRANS,            " 운송방법.
       NAME2      TYPE C,
       ZFDOCST    LIKE ZTINS-ZFDOCST,            " 문서상태.
       ZFINSDT    LIKE ZTINS-ZFINSDT,            " 보험개시일.
       ERNAM      LIKE ZTINS-ERNAM,              " 보험생성인.
       CDAT       LIKE ZTINS-CDAT,               " 보험생성일.
       ZFRSTAW    LIKE ZTINS-ZFRSTAW,            " 대표품목 HS코드.
       ZFOPNNO    LIKE ZTREQHD-ZFOPNNO,          " L/C 승인번호.
       ZFMATGB    LIKE ZTREQHD-ZFMATGB,          " 자재구분.
       ZFLASTAM   LIKE ZTREQHD-ZFLASTAM ,        " 최종개설금액.
       ZFARCU     LIKE ZTREQHD-ZFARCU,           " 도착국가.
       ZFAPRT     LIKE ZTREQHD-ZFAPRT,           " 도착항.
       MAKTX      LIKE ZTREQHD-MAKTX,            " 품명.
       EBELN      LIKE ZTREQHD-EBELN,            " Purchasing document
       INCO1      LIKE ZTREQHD-INCO1,            " Incoterms
       ZFSHCU     LIKE ZTREQHD-ZFSHCU,           " 선적국가.
       ZFSPRT     LIKE ZTREQHD-ZFSPRT,           " 선적항.
       ZFWERKS    LIKE ZTREQHD-ZFWERKS,          " 플랜트.
       EKGRP      LIKE ZTREQST-EKGRP,            " 구매그룹.
       NAME1(16)  TYPE C,
       ZFUSD      LIKE ZTREQST-ZFUSD,            " USD Currency
       ZFOPNDT    LIKE ZTREQST-ZFOPNDT,          " 개설일(L/C DATE)
       WAERS      LIKE ZTREQST-WAERS.            " Currency
DATA : END OF IT_TAB.

DATA : IT_ZVREQ   LIKE ZVREQHD_ST OCCURS 0 WITH HEADER LINE.

INCLUDE   ZRIMSORTCOM.             " 수입의뢰 Report Sort를 위한 Include

*-----------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:  S_EBELN   FOR   ZTREQHD-EBELN,      " P/O No.
                 S_WERKS   FOR   ZTREQHD-ZFWERKS,    " 플랜트.
                 S_MATGB   FOR   ZTREQHD-ZFMATGB,    " 자재구분.
                 S_OPNNO   FOR   ZTREQHD-ZFOPNNO,    " L/C 번호.
                 S_TRANS   FOR   ZTINS-ZFTRANS,      " 운송방법.
                 S_REQNO   FOR   ZTINS-ZFREQNO,      " 수입의뢰 No.
                 S_AMDNO   FOR   ZTINS-ZFAMDNO,      " Amend Seq.
                 S_RSTAW   FOR   ZTINS-ZFRSTAW,      " HS 코드.
                 S_CDAT    FOR   ZTINS-CDAT,         " 보험생성일.
                 S_ERNAM   FOR   ZTINS-ERNAM.        " 보험생성인.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
PARAMETER S_REQDT LIKE  ZTINS-ZFINSDT.               " 부보희망일.
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.                                   " 초기값 SETTING
  PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                   " 헤더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION 절.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* RELEASE 여부 SET!
  PERFORM   P1000_GET_RELEASE_DATA.

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
    WHEN 'REFR'.
      PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
      PERFORM RESET_LIST.
    WHEN 'STUP' OR 'STDN'.                  " SORT 선택절.
      W_FIELD_NM = 'ZFREQNO'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
      PERFORM RESET_LIST.
    WHEN 'DSLC'.                            " L/C 조회, MANUAL CREATE
      PERFORM P2000_SHOW_LC  USING IT_TAB-ZFREQNO.
    WHEN 'DSINS'.
      PERFORM P2000_SHOW_INS USING IT_TAB-ZFREQNO
                                   IT_TAB-ZFINSEQ.
    WHEN 'DSPO'.                            " P/O 조회.
      PERFORM P2000_SHOW_PO  USING IT_TAB-EBELN.
    WHEN 'DOWN'.                            " FILE DOWNLOAD....
      PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.                " 종료.

    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM20'.                    " TITLE BAR
ENDFORM.                                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /51  '[ 적하보험 미발행 리스트 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 98 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : /  SY-VLINE,
             (10) '수입의뢰NO',          SY-VLINE,
             (18) '    의뢰금액  ',      SY-VLINE,
             (10) '부보개시일',          SY-VLINE,
             (18) '    구매오더번호',    SY-VLINE,
             (20) '   구 매 그 룹',      SY-VLINE,
             (21) '       자재구분',     SY-VLINE,
             (06) '도착국',              SY-VLINE,
             (14) '   도 착 항',         SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /  SY-VLINE,
             (10) 'Amend NO',            SY-VLINE,
             (18) '    운송방법  ',      SY-VLINE,
             (10) '개 설 일',            SY-VLINE,
             (18) '    품       명',     SY-VLINE,
             (20) '   플  랜  트',       SY-VLINE,
             (10) 'HS CODE', '|',
             (08) '인도조건',            SY-VLINE,
             (06) '선적국',              SY-VLINE,
             (14) '   선 적 항',         SY-VLINE.

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  REFRESH IT_TAB.

  SELECT *
    FROM ZTINS
   WHERE ZFREQNO  IN  S_REQNO
     AND ZFTRANS  IN  S_TRANS
     AND ZFAMDNO  IN  S_AMDNO
     AND ZFRSTAW  IN  S_RSTAW
     AND CDAT     IN  S_CDAT
     AND ERNAM    IN  S_ERNAM
     AND ( ZFDOCST = 'N' OR ZFDOCST = 'A' ).

    CLEAR IT_TAB.
    MOVE-CORRESPONDING ZTINS TO IT_TAB.

* 수입의뢰 DATA
    CLEAR ZTREQHD.
    SELECT SINGLE *
      FROM ZTREQHD
     WHERE ZFREQNO = IT_TAB-ZFREQNO.
    IF NOT ZTREQHD-EBELN    IN S_EBELN OR
       NOT ZTREQHD-ZFOPNNO  IN S_OPNNO OR
       NOT ZTREQHD-ZFWERKS  IN S_WERKS OR
       NOT ZTREQHD-ZFMATGB  IN S_MATGB.
      CONTINUE.
    ENDIF.

    MOVE : ZTREQHD-ZFLASTAM  TO IT_TAB-ZFLASTAM,
           ZTREQHD-EBELN     TO IT_TAB-EBELN,
           ZTREQHD-MAKTX     TO IT_TAB-MAKTX,
           ZTREQHD-ZFWERKS   TO IT_TAB-ZFWERKS,
           ZTREQHD-ZFMATGB   TO IT_TAB-ZFMATGB,
           ZTREQHD-INCO1     TO IT_TAB-INCO1,
           ZTREQHD-ZFARCU    TO IT_TAB-ZFARCU,
           ZTREQHD-ZFAPRT    TO IT_TAB-ZFAPRT,
           ZTREQHD-ZFSHCU    TO IT_TAB-ZFSHCU,
           ZTREQHD-ZFSPRT    TO IT_TAB-ZFSPRT.

    CLEAR ZTREQST.
    SELECT  SINGLE *
      FROM  ZTREQST
     WHERE  ZFAMDNO EQ IT_TAB-ZFAMDNO.

    MOVE : ZTREQST-EKGRP   TO IT_TAB-EKGRP,
           ZTREQST-ZFOPNDT TO IT_TAB-ZFOPNDT.

    APPEND IT_TAB.
  ENDSELECT.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0.
    MESSAGE S738.
  ENDIF.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'ZIM20'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIM20'.           " GUI TITLE SETTING

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
  PERFORM   P3000_TITLE_WRITE.                  " 헤더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                   " RESET_LIST
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
    WRITE : / '부보희망일 : ', S_REQDT,
            / '총', W_COUNT, '건'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

* 구매그룹명.
  SELECT SINGLE EKNAM INTO W_EKNAM FROM T024
         WHERE  EKGRP EQ   IT_TAB-EKGRP.

* 플랜트명.
  SELECT SINGLE NAME1 INTO W_WERKSNM FROM T001W
         WHERE  WERKS EQ   IT_TAB-ZFWERKS.

* 운송방법.
  CASE IT_TAB-ZFTRANS.
    WHEN 'A'.
      MOVE 'AIR  ' TO W_TRANS.
    WHEN 'O'.
      MOVE 'OCEAN' TO W_TRANS.
  ENDCASE.

* 자재구분.
  CASE IT_TAB-ZFMATGB.
    WHEN '1'.
      MOVE '수출용원자재'   TO  W_MATNM.
    WHEN '2'.
      MOVE 'Local       '   TO  W_MATNM.
    WHEN '3'.
      MOVE '내수용원자재'   TO  W_MATNM.
    WHEN '4'.
      MOVE '시설재      '   TO  W_MATNM.
    WHEN '5'.
      MOVE '상품        '   TO  W_MATNM.
    WHEN '6'.
      MOVE '제품        '   TO  W_MATNM.
  ENDCASE.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE,
          (10) IT_TAB-ZFREQNO,                         SY-VLINE,
          (03) IT_TAB-WAERS NO-GAP,
          (15) IT_TAB-ZFLASTAM  CURRENCY IT_TAB-WAERS, SY-VLINE,
          (10) IT_TAB-ZFINSDT,                         SY-VLINE,
          (18) IT_TAB-EBELN,                           SY-VLINE,
          (05) IT_TAB-EKGRP NO-GAP,
          (15) W_EKNAM,                                SY-VLINE,
          (21) W_MATNM  CENTERED,                      SY-VLINE,
          (06) IT_TAB-ZFARCU CENTERED,                 SY-VLINE,
          (14) IT_TAB-ZFAPRT,                          SY-VLINE.
* hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE,
          (10) IT_TAB-ZFAMDNO,             SY-VLINE,
          (18) W_TRANS CENTERED,           SY-VLINE,
          (10) IT_TAB-ZFOPNDT,             SY-VLINE,
          (18) IT_TAB-MAKTX,               SY-VLINE,
          (05) IT_TAB-ZFWERKS              NO-GAP,
          (15) W_WERKSNM,                  SY-VLINE,
          (10) IT_TAB-ZFRSTAW, '|',
          (08) IT_TAB-INCO1 CENTERED,      SY-VLINE,
          (06) IT_TAB-ZFSHCU CENTERED,     SY-VLINE,
          (14) IT_TAB-ZFSPRT,              SY-VLINE.

  WRITE:/ SY-ULINE.
* stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.

  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
  EXPORT 'BES'           TO MEMORY ID 'BES'.
  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
  EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.

  CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_LC

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_INS
*&---------------------------------------------------------------------*
FORM P2000_SHOW_INS USING     P_ZFREQNO
                              P_ZFINSEQ.

  SET PARAMETER ID 'BES'       FIELD ' '.
  SET PARAMETER ID 'ZPOPNNO'   FIELD ' '.
  SET PARAMETER ID 'ZPINSEQ'   FIELD P_ZFINSEQ.
  SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.

  CALL TRANSACTION 'ZIM43'    AND SKIP FIRST SCREEN.

ENDFORM.                                     " P2000_SHOW_INS

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_PO
*&---------------------------------------------------------------------*
FORM P2000_SHOW_PO USING    P_EBELN.

  SET PARAMETER ID 'ZPOPNNO'   FIELD ' '.
  SET PARAMETER ID 'ZPINSEQ'   FIELD ' '.
  SET PARAMETER ID 'ZPREQNO'   FIELD ' '.
  SET PARAMETER ID 'BES'       FIELD P_EBELN.

  CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_PO

*&---------------------------------------------------------------------*
*&      Form  P1000_RESET_LIST
*&---------------------------------------------------------------------*
FORM P1000_RESET_LIST.

  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
  PERFORM RESET_LIST.

ENDFORM.                    " P1000_RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_RELEASE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_RELEASE_DATA.

*  REFRESH : R_ZFRLST1.

*  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE E961.   EXIT.
  ENDIF.

ENDFORM.                       " P1000_GET_RELEASE_DATA
**----------------------------------------------------------------------
** 구매 릴리즈 사용 여?
**----------------------------------------------------------------------
*  CLEAR R_ZFRLST1.
*  IF  ZTIMIMG00-ZFRELYN1 EQ 'X'.
*    MOVE: 'I'      TO R_ZFRLST1-SIGN,
*          'EQ'     TO R_ZFRLST1-OPTION,
*          'R'      TO R_ZFRLST1-LOW.
*    APPEND R_ZFRLST1.
*  ELSE.
*    MOVE: 'I'      TO R_ZFRLST1-SIGN,
*          'EQ'     TO R_ZFRLST1-OPTION,
*          'N'      TO R_ZFRLST1-LOW.
*    APPEND R_ZFRLST1.
*  ENDIF.
*ENDFORM.                    " P1000_GET_RELEASE_DATA
