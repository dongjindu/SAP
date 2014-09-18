*&---------------------------------------------------------------------*
*& Report  ZRIMISNSB                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 적하보험 부보 신청                                    *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.09.05                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&   DESC.     : 미부보, 부보 현황을 동시에 조회 할 수 있고,
*&               미부보 건은 부보신청서를 출력하여 작성할 수 있다.
*&---------------------------------------------------------------------*
*& [변경내용] BL 기준의 부보.
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMISNSB  MESSAGE-ID ZIM
                   LINE-SIZE 188
                   NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFHBLNO      LIKE ZTBL-ZFHBLNO,      " House B/L NO.
       LIFNR        LIKE ZTBL-LIFNR,        " Vendor
       LIFNR_NM(20)      TYPE C,            " Vendor명.
       ZFBENI       LIKE ZTBL-ZFBENI,       " Beneficiary.
       ZFBENI_NM(20)     TYPE C,            " Beneficiary 명.
       ZFETD        LIKE ZTBL-ZFETD,        " 선적일(출항예정일).
       ZFREBELN     LIKE ZTBL-ZFREBELN,     " 대표P/O No.
       INCO1        LIKE ZTBL-INCO1,        " Incoterms.
       ZFBLNO       LIKE ZTINSB-ZFBLNO,     " B/L 관리번호.
       ZFINSEQ      LIKE ZTINSB-ZFINSEQ,     " 보험 SEQ.
       ZFDOCST      LIKE ZTINSB-ZFDOCST,     " 보험 Document Status.
       DOCST        LIKE DD07T-DDTEXT,
       ZFINSDT      LIKE ZTINSB-ZFINSDT,     " 보험개시일.
       ZFOPCD       LIKE ZTINSB-ZFOPCD,      " 보험회사.
       ZFINCOM_NM(20)    TYPE C,             " 보험회사명.
       ZFINNO       LIKE ZTINSB-ZFINNO,      " 보험증권번호.
       ZFIVAMT      LIKE ZTINSB-ZFIVAMT,     " Invoice Amount(보험가액).
       WAERS        LIKE ZTINSB-WAERS,       " Invoice Amount 통화.
       ZFRSTAW      LIKE ZTINSB-ZFRSTAW,     " 대표 HS.
       ZFCARC       LIKE ZTBL-ZFCARC,        " 선적국가코드.
       ZFSPRT       LIKE ZTBL-ZFSPRT,        " 선적항.
       ZFAPPC       LIKE ZTBL-ZFAPPC,        " 도착국가코드.
       ZFAPRT       LIKE ZTBL-ZFAPRT,        " 도착항.
       ZFCNCDNM     LIKE ZTINSBAGR-ZFCNCDNM, " 부보조건.
       ZFDSOG1      LIKE ZTINSBSG2-ZFDSOG1,  " 상품명세.
*      ZFLASTAM     LIKE ZTREQHD-ZFLASTAM,   " 개설금액.
       ZFKRWAMT     LIKE ZTINSB-ZFKRWAMT,    " 보험료(원).
       ZFKRW        LIKE ZTINSB-ZFKRW,       " 보험료(원) 화폐단위.
       ZFINAMT      LIKE ZTINSB-ZFINAMT,     " 보험료($).
       ZFINAMTC     LIKE ZTINSB-ZFINAMTC,    " 보험료통화.
       ZFINRT       LIKE ZTINSB-ZFINRT,      " 부보요율.
       ZFTRANS      LIKE ZTINSB-ZFTRANS,     " 운송구분.
       TRANS        LIKE DD07T-DDTEXT,       " 운송.
       ZFACDO       LIKE ZTBLCST-ZFACDO,     " 기표.
       LANDX        LIKE T005T-LANDX,        " 원산지.
*>> NCW Insert - 2003.12.09
       ZFISDT       LIKE ZTINSBRSP-ZFISDT,   "
       ZFETA        LIKE ZTBL-ZFETA,         "
       ZFOPNNO      LIKE ZTBL-ZFOPNNO,       "
       ZFBSINS      LIKE ZTINSB-ZFBSINS,     "
       ZTERM        LIKE EKKO-ZTERM.         "
*
DATA : END OF IT_TAB.

DATA : BEGIN OF IT_TAB_DOWN OCCURS 0,
       ZFHBLNO      LIKE ZTBL-ZFHBLNO,      " House B/L NO.
       LIFNR        LIKE ZTBL-LIFNR,        " Vendor
       LIFNR_NM(20)      TYPE C,            " Vendor명.
       ZFBENI       LIKE ZTBL-ZFBENI,       " Beneficiary.
       ZFBENI_NM(20)     TYPE C,            " Beneficiary 명.
       ZFETD        LIKE ZTBL-ZFETD,        " 선적일(출항예정일).
       ZFREBELN     LIKE ZTBL-ZFREBELN,     " 대표P/O No.
       INCO1        LIKE ZTBL-INCO1,        " Incoterms.
       ZFBLNO       LIKE ZTINSB-ZFBLNO,     " B/L 관리번호.
       ZFINSEQ      LIKE ZTINSB-ZFINSEQ,    " 보험 SEQ.
       ZFDOCST      LIKE ZTINSB-ZFDOCST,    " 보험 Document Status.
       ZFINSDT      LIKE ZTINSB-ZFINSDT,    " 보험개시일.
       ZFOPCD       LIKE ZTINSB-ZFOPCD,     " 보험회사.
       ZFINCOM_NM(20)    TYPE C,            " 보험회사명.
       ZFINNO       LIKE ZTINSB-ZFINNO,     " 보험증권번호.
       ZFIVAMT(14),                         " Invoice Amount(보험가액).
       WAERS        LIKE ZTINSB-WAERS,      " Invoice Amount 통화.
       ZFCARC       LIKE ZTBL-ZFCARC,       " 선적국가코드.
       ZFSPRT       LIKE ZTBL-ZFSPRT,       " 선적항.
       ZFAPPC       LIKE ZTBL-ZFAPPC,       " 도착국가코드.
       ZFAPRT       LIKE ZTBL-ZFAPRT,       " 도착항.
       ZFCNCDNM     LIKE ZTINSBAGR-ZFCNCDNM," 부보조건.
       ZFDSOG1      LIKE ZTINSBSG2-ZFDSOG1, " 상품명세.
*      ZFLASTAM     LIKE ZTREQHD-ZFLASTAM,  " 개설금액.
       ZFKRWAMT(14),                        " 보험료(원).
       ZFKRW        LIKE ZTINSB-ZFKRW,      " 보험료(원) 화폐단위.
       ZFINAMT(14),                         " 보험료($).
       ZFINAMTC     LIKE ZTINSB-ZFINAMTC,   " 보험료통화.
       ZFINRT       LIKE ZTINSB-ZFINRT,     " 부보요율.
       ZFTRANS      LIKE ZTINSB-ZFTRANS,    " 운송구분.
       ZFACDO       LIKE ZTBLCST-ZFACDO,    " 기표.
       LANDX        LIKE T005T-LANDX.       " 원산지.
DATA : END OF IT_TAB_DOWN.

DATA: BEGIN OF IT_TMP1 OCCURS 0.
        INCLUDE STRUCTURE IT_TAB.
DATA: END OF IT_TMP1.
*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------
INCLUDE   ZRIMISNSBTOP.
INCLUDE   ZRIMSORTCOM.    " Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " 기타유틸리티 모음.
*-----------------------------------------------------------------------
* Selection Screen.
*-----------------------------------------------------------------------

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  S_BUKRS   FOR  ZTBL-BUKRS NO-EXTENSION
                                            NO INTERVALS,
                 S_HBLNO     FOR ZTBL-ZFHBLNO,     " HBL.
                 S_LIFNR     FOR ZTBL-LIFNR,       " Vendor
                 S_BENI      FOR ZTBL-ZFBENI,      " Beneficiary.
                 S_REBELN    FOR ZTBL-ZFREBELN,    " P/O No.
                 S_INCO1     FOR ZTBL-INCO1,       " Incoterms.
                 S_BLNO      FOR ZTINSB-ZFBLNO,    " B/L관리번호.
                 S_INSDT     FOR ZTINSB-ZFINSDT,   " 보험개시일.
                 S_INCOM     FOR ZTINSB-ZFINCOM,   " 보험회사.
                 S_INNO      FOR ZTINSB-ZFINNO,    " 보험증권번호.
                 S_OPNO      FOR ZTINSB-ZFOPNO,   "포괄보험증권번호.
                 S_CDAT      FOR ZTINSB-CDAT,       " 보험생성일.
                 S_ERNAM     FOR ZTINSB-ERNAM.      " 보험생성인.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
* 보험부보 여부.
SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(14) TEXT-002.",POSITION 1.
SELECTION-SCREEN : COMMENT 33(2) TEXT-021, POSITION 36.
PARAMETERS : P_NO    AS CHECKBOX.              " No
SELECTION-SCREEN : COMMENT 48(3) TEXT-022, POSITION 52.
PARAMETERS : P_YES   AS CHECKBOX.              " Yes
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME.
PARAMETER P_REQDT LIKE  ZTINSB-ZFINSDT.               " 부보희망일.
SELECTION-SCREEN END OF BLOCK B3.

*---------------------------------------------------------------------*
* EVENT INITIALIZATION.
*---------------------------------------------------------------------*
INITIALIZATION.                                 " 초기값 SETTING
  PERFORM  P2000_SET_PARAMETER.
  PERFORM  P1000_SET_BUKRS.
  SET TITLEBAR 'ZIMR17'.
*---------------------------------------------------------------------*
* EVENT TOP-OF-PAGE.
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*---------------------------------------------------------------------*
* EVENT START-OF-SELECTION.
*---------------------------------------------------------------------*
START-OF-SELECTION.

* 레포트 관련 Text Table SELECT
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
  CLEAR : IT_TAB.
*-----------------------------------------------------------------------
* EVENT AT USER-COMMAND.
*-----------------------------------------------------------------------
AT USER-COMMAND.

  IF IT_TAB-ZFBLNO IS INITIAL.
    MESSAGE S962.
  ELSE.
    CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.              " SORT 선택?
        W_FIELD_NM = 'ZFHBLNO'.
        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
        PERFORM HANDLE_SORT TABLES  IT_TAB
                            USING   SY-UCOMM.
      WHEN 'DSIN'.                       " Insurance 조회.
        PERFORM P2000_SHOW_INS USING IT_TAB-ZFBLNO
                                     IT_TAB-ZFINSEQ.
      WHEN 'DSBL'.                       " B/L 조회.
        PERFORM P2000_SHOW_BL USING IT_TAB-ZFBLNO.

      WHEN 'DOWN'.          " FILE DOWNLOAD....
*      PERFORM P3000_CREATE_DOWNLOAD_FILE.
*        PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.    " 종료.
        LEAVE TO SCREEN 0.
      WHEN 'PRT'.                       " 적하보험부보 출력.
        PERFORM P2000_SHOW_ZTRED.

      WHEN OTHERS.
    ENDCASE.
    CLEAR IT_TAB.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_ZTRED
*&---------------------------------------------------------------------*
FORM P2000_SHOW_ZTRED.

*   EXPORT IT_TAB TO MEMORY ID 'ZPITSEL'.
*   CALL TRANSACTION 'ZIMR12' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_ZTRED

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  P_NO = 'X'.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.

  WRITE : /77  '[ Insuarance policy application List ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 169 'Page : ', W_PAGE.

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : /  SY-VLINE, (20) 'House B/L No.',
             SY-VLINE, (20) 'Vendor',
             SY-VLINE, (08) 'Incoterms',
             SY-VLINE, (10) 'ship.date',
             SY-VLINE, (30) 'Loading area',
             SY-VLINE, (19) 'Invoice amount',
             SY-VLINE, (15) 'HS code',
             SY-VLINE, (10) 'B/L Doc No',
             SY-VLINE, (10) 'Confirm Da',
             SY-VLINE, (15) 'Payment term',
             SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /  SY-VLINE, (20) 'Ins. policy Number',
             SY-VLINE, (20) 'Beneficiary',
             SY-VLINE, (08) ' ',
             SY-VLINE, (10) 'start DT',
             SY-VLINE, (30) 'Arrival area',
             SY-VLINE, (19) 'Premium(Local)',
             SY-VLINE, (15) 'Item Description',
             SY-VLINE, (10) 'Ins. Seq.',
             SY-VLINE, (10) 'Import dat',
             SY-VLINE, (15) 'LC No',
             SY-VLINE.
  FORMAT COLOR COL_TOTAL INTENSIFIED ON.
  WRITE : / SY-VLINE, (20) 'Purchase Order',
            SY-VLINE, (20) 'Insuarance Company',
            SY-VLINE, (08) 'Transp.',
            SY-VLINE, (10) 'Rate',
            SY-VLINE, (30) 'Document Status',
            SY-VLINE, (19) 'Premium',
            SY-VLINE, (15) 'Ins. Condition',
            SY-VLINE, (10) 'Cost Doc.',
            SY-VLINE, (10) '     ',
            SY-VLINE, (15) '     ',
            SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR : IT_TAB.
  REFRESH : IT_TAB.
  IF P_YES = ' ' AND P_NO = ' '.
    MESSAGE S977 WITH 'Select the Yes/No of insurance policy'.
    MOVE 'Y' TO W_ERR_CHK.
    EXIT.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TMP1
          FROM   ZTINSB AS I INNER JOIN ZTBL AS B
            ON   I~ZFBLNO = B~ZFBLNO
          WHERE  B~ZFHBLNO    IN  S_HBLNO
            AND  B~BUKRS      IN  S_BUKRS
            AND  B~LIFNR      IN  S_LIFNR
            AND  B~ZFBENI     IN  S_BENI
            AND  B~LIFNR      IN  S_LIFNR
            AND  B~INCO1      IN  S_INCO1
            AND  B~ZFINSYN    EQ  'A'
            AND  I~ZFBLNO     IN  S_BLNO
            AND  I~ZFINSDT    IN  S_INSDT
            AND  I~ZFINCOM    IN  S_INCOM
            AND  I~ZFINNO     IN  S_INNO
            AND  I~ZFOPNO     IN  S_OPNO
            AND  I~CDAT       IN  S_CDAT
            AND  I~ERNAM      IN  S_ERNAM .


  LOOP AT IT_TMP1.
    CLEAR : W_ZFINSEQ.

    CASE IT_TMP1-ZFDOCST.                       " 보험부보여부.
      WHEN 'O' OR 'A'.                            " 확정.
        IF P_YES = ' '.
          CONTINUE.
        ENDIF.
      WHEN OTHERS.                         " 기타.
        IF P_NO = ' '.
          CONTINUE.
        ENDIF.
    ENDCASE.

    CLEAR IT_TAB.
    MOVE-CORRESPONDING IT_TMP1  TO  IT_TAB.

*>> NCW Insert - 2003.12.09
* Confirmation date
    SELECT MAX( ZFINSEQ ) INTO W_ZFINSEQ
      FROM ZTINSBRSP
     WHERE ZFBLNO = IT_TMP1-ZFBLNO.

    SELECT SINGLE ZFISDT INTO IT_TAB-ZFISDT
      FROM ZTINSBRSP
     WHERE ZFBLNO  = IT_TMP1-ZFBLNO
       AND ZFINSEQ = W_ZFINSEQ.

* Insuring condition
*    SELECT SINGLE ZFBSINS INTO IT_TAB-ZFBSINS
*      FROM ZTINSB
*     WHERE ZFBLNO  = IT_TMP1-ZFBLNO
*       AND ZFINSEQ = W_ZFINSEQ.

* Payment condition
     SELECT SINGLE *
       FROM ZTBL
      WHERE ZFBLNO = IT_TMP1-ZFBLNO.

     SELECT SINGLE ZTERM INTO IT_TAB-ZTERM
       FROM ZTREQHD
      WHERE EBELN = ZTBL-ZFREBELN.
*<<
    SELECT MAX( ZFDSOG1 ) INTO IT_TAB-ZFDSOG1    "상품명세.
       FROM ZTINSBSG2
      WHERE ZFBLNO = IT_TMP1-ZFBLNO
        AND ZFINSEQ = IT_TMP1-ZFINSEQ.
*        SELECT MAX( ZFSHCUNM ) MAX( ZFARCUNM )
*           INTO (IT_TAB-ZFSHCUNM, IT_TAB-ZFARCUNM)    "선적지,도착지.
*           FROM ZTINSBSG3
*          WHERE ZFREQNO = ZTREQHD-ZFREQNO
*            AND ZFINSEQ = ZTINSB-ZFINSEQ
*            AND ZFAMDNO = IT_TAB-ZFAMDNO.
    SELECT MAX( ZFCNCDNM ) INTO IT_TAB-ZFCNCDNM  "부보 조건.
      FROM ZTINSBAGR
      WHERE ZFBLNO = IT_TMP1-ZFBLNO
        AND ZFINSEQ = IT_TMP1-ZFINSEQ.

    SELECT SINGLE NAME1 INTO IT_TAB-ZFINCOM_NM   "보험회사명.
      FROM LFA1
     WHERE LIFNR = IT_TMP1-ZFOPCD.

    SELECT SINGLE NAME1 INTO IT_TAB-LIFNR_NM     "Vendor명.
                FROM LFA1
          WHERE LIFNR = IT_TMP1-LIFNR.

    SELECT SINGLE NAME1 INTO IT_TAB-ZFBENI_NM    "Beneficiary명.
      FROM LFA1
     WHERE LIFNR = IT_TMP1-ZFBENI.

    SELECT MAX( ZFACDO ) INTO IT_TAB-ZFACDO      "기표.
       FROM ZTBLCST
      WHERE ZFBLNO = IT_TMP1-ZFBLNO
        AND ZFCSCD = '1AB'.

    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDDOCST'
                                         ZTINSB-ZFDOCST
                               CHANGING  IT_TAB-DOCST.
    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDTRANS'
                                         ZTINSB-ZFTRANS
                               CHANGING  IT_TAB-TRANS.

    APPEND IT_TAB.
    CLEAR IT_TAB.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0.
    W_ERR_CHK = 'Y'.  MESSAGE S738.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'ZIMR17'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIMR17'.           " GUI TITLE SETTING..

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  SORT IT_TAB BY ZFREBELN.

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
    WRITE: / SY-ULINE.
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
    WRITE : / SY-VLINE,
             'Total', W_COUNT, 'Case', (80) ' ',
             'Insuring request date : ', P_REQDT.
    WRITE AT 157 SY-VLINE.
    NEW-LINE.
    ULINE.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, (20) IT_TAB-ZFHBLNO,       " House B/L NO.
          SY-VLINE, (07) IT_TAB-LIFNR,         " Vendor
                    (12) IT_TAB-LIFNR_NM,      " Vendor명.
          SY-VLINE, (08) IT_TAB-INCO1,         "Incorterms.
          SY-VLINE, (10) IT_TAB-ZFETD,         " 선적일(출항예정일).
          SY-VLINE, (05) IT_TAB-ZFCARC NO-GAP, "선적국가코드.
                    (25) IT_TAB-ZFSPRT,        "선적항.
          SY-VLINE, (05) IT_TAB-WAERS,         " Invoice 금액.
                    (13) IT_TAB-ZFIVAMT  CURRENCY IT_TAB-WAERS,
          SY-VLINE, (15) IT_TAB-ZFRSTAW,       " 대표 HS.
          SY-VLINE, (10) IT_TAB-ZFBLNO,        " B/L 관리번호.
          SY-VLINE, (10) IT_TAB-ZFISDT,
          SY-VLINE, (15) IT_TAB-ZTERM,
          SY-VLINE.
* Hide
  HIDE : IT_TAB.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE : / SY-VLINE, (20) IT_TAB-ZFINNO,         "보험증권번호.
            SY-VLINE, (07) IT_TAB-ZFBENI,         "Beneficiary.
                      (12) IT_TAB-ZFBENI_NM,      "Beneficiary명.
            SY-VLINE, (08) ' ',
            SY-VLINE, (10) IT_TAB-ZFINSDT,        " 보험개시일.
            SY-VLINE, (05) IT_TAB-ZFAPPC NO-GAP,  "도착지국가코드.
                      (25) IT_TAB-ZFAPRT,        "도착항.
            SY-VLINE, (05) IT_TAB-ZFKRW,         "보험료원화
                      (13) IT_TAB-ZFKRWAMT CURRENCY IT_TAB-ZFKRW,
            SY-VLINE, (15) IT_TAB-ZFDSOG1,        " 상품명세.
            SY-VLINE, (10) IT_TAB-ZFINSEQ,     " 보험 SEQ.
            SY-VLINE, (10) IT_TAB-ZFETA,
            SY-VLINE, (15) IT_TAB-ZFOPNNO,
            SY-VLINE.
* Hide
  HIDE : IT_TAB.

  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE : / SY-VLINE, (20) IT_TAB-ZFREBELN,       " 대표P/O No.
            SY-VLINE, (07) IT_TAB-ZFOPCD,         " 보험회사.
                      (12) IT_TAB-ZFINCOM_NM,     " 보험회사명.
            SY-VLINE, (08) IT_TAB-TRANS,           " 운송구분.
            SY-VLINE, (10) IT_TAB-ZFINRT
                           RIGHT-JUSTIFIED,      " AIR 요율.
            SY-VLINE, (30) IT_TAB-DOCST,          " 문서상태.
            SY-VLINE, (05) IT_TAB-ZFINAMTC,        "보험료($).
                      (13) IT_TAB-ZFINAMT CURRENCY IT_TAB-ZFINAMTC,
            SY-VLINE, (15) IT_TAB-ZFCNCDNM,        "부보조건.
            SY-VLINE, (10) IT_TAB-ZFACDO,          "기표.
            SY-VLINE, (10) '       ',
            SY-VLINE, (15) '       ',
            SY-VLINE.

*hide.
  HIDE : IT_TAB.
  W_COUNT = W_COUNT + 1.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING   P_ZFBLNO .

  SET PARAMETER ID 'ZPHBLNO'  FIELD ' '.
  SET PARAMETER ID 'ZPBLNO'   FIELD P_ZFBLNO.

  EXPORT 'ZPHBLNO'   TO MEMORY ID 'ZPHBLNO'.
  EXPORT 'ZPBLNO'   TO MEMORY ID 'ZPBLNO'.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_INS
*&---------------------------------------------------------------------*
FORM P2000_SHOW_INS USING    P_ZFBLNO
                             P_ZFINSEQ.

  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPBLNO'    FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPINSEQ'   FIELD P_ZFINSEQ.

  EXPORT 'BES'       TO MEMORY ID 'BES'.
  EXPORT 'ZPOPNNO'   TO MEMORY ID 'ZPOPNNO'.
  EXPORT 'ZPBLNO'    TO MEMORY ID 'ZPBLNO'.
  EXPORT 'ZPINSEQ'   TO MEMORY ID 'ZPINSEQ'.


  CALL TRANSACTION 'ZIMB3' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_INS
*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM P3000_CREATE_DOWNLOAD_FILE.

  REFRESH IT_TAB_DOWN.
  LOOP AT IT_TAB.
    CLEAR IT_TAB_DOWN.

    MOVE-CORRESPONDING IT_TAB TO IT_TAB_DOWN.
    WRITE : IT_TAB-ZFIVAMT CURRENCY IT_TAB-WAERS TO IT_TAB_DOWN-ZFIVAMT,
            IT_TAB-ZFKRWAMT CURRENCY IT_TAB-ZFKRW TO
                                                  IT_TAB_DOWN-ZFKRWAMT,
            IT_TAB-ZFINAMT CURRENCY IT_TAB-ZFINAMTC TO
                                                  IT_TAB_DOWN-ZFINAMT.

    APPEND IT_TAB_DOWN.
  ENDLOOP.

ENDFORM.                    " P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00.
   SELECT SINGLE * FROM ZTIMIMG00.
*>> Company code SET.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
     MOVE: 'I'               TO S_BUKRS-SIGN,
           'EQ'              TO S_BUKRS-OPTION,
           ZTIMIMG00-ZFBUKRS TO S_BUKRS-LOW.
     APPEND S_BUKRS.
   ENDIF.

ENDFORM.                    " P1000_SET_BUKRS
