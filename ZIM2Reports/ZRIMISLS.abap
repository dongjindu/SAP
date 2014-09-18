*&---------------------------------------------------------------------*
*& Report  ZRIMISLS                                                    *
*&---------------------------------------------------------------------*
*&ABAP Name : ZRIMISLS                                                 *
*&Created by: 나신호 INFOLINK.Ltd                                      *
*&Created on: 07/26/2000                                               *
*&Version   : 1.0                                                      *
*&---------------------------------------------------------------------*
* 적하보험 부보 명?
* ZTINS : 수입의뢰번호 별로 AMAND 번호가 MAX 인 건.
* ZTREQST : ZTINS 수입의뢰번호 별로 개시일이 있는 MAX AMAND ?
*&---------------------------------------------------------------------*

REPORT  ZRIMISLS       NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 142.

TABLES :  WMTO_S.

DATA : BEGIN OF IT_TAB OCCURS 0,
               ZFREQNO     LIKE  ZTINS-ZFREQNO,
               ZFAMDNO     LIKE  ZTREQST-ZFAMDNO,
               ZFINSEQ     LIKE  ZTINS-ZFINSEQ,
               ZFOPAMT     LIKE  ZTREQST-ZFOPAMT,
               WAERS       LIKE  ZTREQST-WAERS,
               ZFOPNDT     LIKE  ZTREQST-ZFOPNDT,
               ZFMATGB     LIKE  ZTREQHD-ZFMATGB,
               ZFRSTAW     LIKE  ZTINS-ZFRSTAW,        " 대표 HS
               INCO1       LIKE  ZTREQHD-INCO1,
               ZFEXRT(12)  TYPE  C,
               ZFTAMI      TYPE  P DECIMALS 0,
               ZFIVAMT     LIKE  ZTINS-ZFIVAMT,
               ZFKRWAMT    LIKE  ZTINS-ZFKRWAMT,       " 보험금액 원?
               ZFKRW       LIKE  ZTINS-ZFKRW,          " Local Currency
               ZFINAMT     LIKE  ZTINS-ZFINAMT,        " 보험금?
               ZFINAMTC    LIKE  ZTINS-ZFINAMTC,       " CURR
               ZFSPRT      LIKE  ZTREQHD-ZFSPRT,
               ZFWERKS     LIKE  ZTREQHD-ZFWERKS,
               ZFOPNNO     LIKE  ZTREQHD-ZFOPNNO,
               ZFINSDT     LIKE  ZTINS-ZFINSDT,        " 보험개시?
               ZFINCD      LIKE  ZTINS-ZFINCD,         " 보험등?
               DAMBO(12)   TYPE  C,                    " 담보조?
               ZFTRANS     LIKE  ZTINS-ZFTRANS,        " VIA 운송방?
               ZFINRT(9)   TYPE  C,
               ZFINNO      LIKE  ZTINS-ZFINNO,         " 증권번?
               MAKTX       LIKE  ZTREQHD-MAKTX,
               ZFAPRT      LIKE  ZTREQHD-ZFAPRT.
DATA : END   OF IT_TAB.

DATA : BEGIN OF IT_TMP1  OCCURS 0,
               ZFREQNO   LIKE  ZTINS-ZFREQNO,
               ZFINSEQ   LIKE  ZTINS-ZFINSEQ,
               ZFAMDNO   LIKE  ZTINS-ZFAMDNO.
DATA : END   OF IT_TMP1.

DATA : BEGIN OF IT_INS OCCURS 0,
               ZFREQNO     LIKE  ZTINS-ZFREQNO,
               ZFAMDNO     LIKE  ZTINS-ZFAMDNO,
               ZFINSEQ     LIKE  ZTINS-ZFINSEQ,
               ZFINRT      LIKE  ZTINS-ZFINRT,
               ZFINCD      LIKE  ZTINS-ZFINCD,
               ZFTRANS     LIKE  ZTINS-ZFTRANS,
               ZFINNO      LIKE  ZTINS-ZFINNO,
               ZFIVAMT     LIKE  ZTINS-ZFIVAMT,
               ZFINAMT     LIKE  ZTINS-ZFINAMT,
               ZFINAMTC    LIKE  ZTINS-ZFINAMTC,
               ZFKRWAMT    LIKE  ZTINS-ZFKRWAMT,
               ZFKRW       LIKE  ZTINS-ZFKRW,
               WAERS       LIKE  ZTINS-WAERS,
               ZFINSDT     LIKE  ZTINS-ZFINSDT,
               ZFRSTAW     LIKE  ZTINS-ZFRSTAW.
DATA : END   OF IT_INS.

DATA : BEGIN OF IT_REQ  OCCURS 0,
               ZFREQNO     LIKE  ZTREQHD-ZFREQNO,
               ZFWERKS     LIKE  ZTREQHD-ZFWERKS,
               ZFOPNNO     LIKE  ZTREQHD-ZFOPNNO,
               ZFMATGB     LIKE  ZTREQHD-ZFMATGB,
               INCO1       LIKE  ZTREQHD-INCO1,
               ZFSPRT      LIKE  ZTREQHD-ZFSPRT,
               ZFAPRT      LIKE  ZTREQHD-ZFAPRT,
               MAKTX       LIKE  ZTREQHD-MAKTX,
               ZFOPAMT     LIKE  ZTREQST-ZFOPAMT,
               WAERS       LIKE  ZTREQST-WAERS,
               ZFOPNDT     LIKE  ZTREQST-ZFOPNDT.
DATA : END   OF IT_REQ.

DATA : W_ZFEXRT    LIKE  ZTINSRSP-ZFEXRT,
       W_ZFTAMI    TYPE  P DECIMALS 0,
       W_LOCAL     LIKE  ZTINS-ZFKRW,
       W_ZFKRWAMT  LIKE  ZTINS-ZFKRWAMT,
       W_MATGB     LIKE  ZTREQHD-ZFMATGB,
       W_SUBRC     LIKE  SY-SUBRC.

INCLUDE   ZRIMISLSTTOP.   " 변수 선언을 위한 Include.
INCLUDE   ZRIMSORTCOM.    " Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모음.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
   SELECT-OPTIONS: S_BUKRS    FOR ZTREQHD-BUKRS NO-EXTENSION
                                                NO INTERVALS,
                   S_WERKS    FOR ZTREQHD-ZFWERKS,
                   S_MATGB    FOR ZTREQHD-ZFMATGB,
                   S_WAERS    FOR ZTREQST-WAERS,
                   S_DOCST    FOR ZTINS-ZFDOCST
                                 OBLIGATORY,
                   S_INSDT    FOR ZTINS-ZFINSDT
                                 OBLIGATORY,
                   S_TRANS    FOR ZTINS-ZFTRANS.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM P1000_SET_BUKRS.
  PERFORM P1000_INITIALIZATION.
  SET  TITLEBAR  'TI1000'.               " GUI TITLE  SETTING

START-OF-SELECTION.
  PERFORM P1000_READ_DATA.
  IF W_SUBRC = 4.
     MESSAGE S191 WITH 'Import Request document'.  EXIT.
  ENDIF.

  PERFORM P1000_CHECK_DATA.
  IF IT_TAB[] IS INITIAL.
     MESSAGE S191 WITH 'Import Request document'.  EXIT.
  ENDIF.

  SET PF-STATUS 'PF1000'.
  SET TITLEBAR  'TI1000'.
  PERFORM P1000_WRITE_DATA.

TOP-OF-PAGE.
  IF SY-LANGU EQ '3'.
     PERFORM P1000_TOP_PAGE.
  ELSE.
     PERFORM P1000_TOP_PAGE_EN.
  ENDIF.
AT USER-COMMAND.

  CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택.
         W_FIELD_NM = 'ZFREQNO'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'DISP'.                   " 부보조회.
            PERFORM P2000_SHOW_INS USING  IT_TAB-ZFREQNO
                                          IT_TAB-ZFAMDNO
                                          IT_TAB-ZFINSEQ.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
*  테이블 SELECT
           PERFORM P1000_READ_DATA.
           PERFORM P1000_CHECK_DATA.
           PERFORM RESET_LIST.
      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_DATA.

  REFRESH : IT_TMP1, IT_INS, IT_REQ.

  CLEAR : W_SUBRC.
**  보험 MAX AMAND
  SELECT ZFREQNO  ZFINSEQ MAX( ZFAMDNO )
    INTO (IT_TMP1-ZFREQNO,IT_TMP1-ZFINSEQ,IT_TMP1-ZFAMDNO)
    FROM ZTINS
    WHERE ZFINSDT IN S_INSDT  GROUP BY ZFREQNO ZFINSEQ.

    APPEND IT_TMP1.  CLEAR IT_TMP1.
  ENDSELECT.

  IF SY-SUBRC <> 0.   W_SUBRC = 4.  EXIT.   ENDIF.

** 대상 보?
  SELECT ZFREQNO  ZFINRT  ZFINCD  ZFTRANS ZFINNO ZFINAMT ZFINAMTC
         ZFKRWAMT ZFINSDT ZFRSTAW ZFIVAMT WAERS  ZFKRW
    INTO CORRESPONDING FIELDS OF TABLE IT_INS
    FROM ZTINS FOR ALL ENTRIES IN IT_TMP1
   WHERE ZFREQNO = IT_TMP1-ZFREQNO
     AND ZFAMDNO = IT_TMP1-ZFAMDNO
     AND ZFINSEQ = IT_TMP1-ZFINSEQ
     AND ZFTRANS IN S_TRANS
     AND ZFDOCST IN S_DOCST.

  IF SY-SUBRC <> 0.   W_SUBRC = 4.  EXIT.   ENDIF.

** 수입 의뢰 상?
  CLEAR : IT_TMP1.   REFRESH : IT_TMP1.
  LOOP AT IT_INS.
    SELECT MAX( ZFAMDNO )  INTO IT_TMP1-ZFAMDNO
      FROM ZTREQST
      WHERE ZFREQNO = IT_INS-ZFREQNO.

    IT_TMP1-ZFREQNO = IT_INS-ZFREQNO.
    APPEND IT_TMP1.  CLEAR IT_TMP1.
  ENDLOOP.

** 수입 의뢰 상태 / HEAD
  SELECT A~ZFREQNO A~ZFWERKS A~ZFOPNNO    A~ZFMATGB
         A~INCO1   A~ZFSPRT  A~ZFAPRT     A~MAKTX  AS ZFRGDSR
         B~ZFOPAMT B~WAERS B~ZFOPNDT
   INTO (IT_REQ-ZFREQNO, IT_REQ-ZFWERKS, IT_REQ-ZFOPNNO, IT_REQ-ZFMATGB,
         IT_REQ-INCO1, IT_REQ-ZFSPRT, IT_REQ-ZFAPRT, IT_REQ-MAKTX,
         IT_REQ-ZFOPAMT, IT_REQ-WAERS, IT_REQ-ZFOPNDT)
    FROM ZTREQHD AS A INNER JOIN ZTREQST AS B
      ON A~ZFREQNO = B~ZFREQNO
      FOR ALL ENTRIES IN IT_TMP1
   WHERE A~ZFREQNO = IT_TMP1-ZFREQNO AND B~ZFAMDNO = IT_TMP1-ZFAMDNO
     AND A~ZFWERKS IN S_WERKS AND A~ZFMATGB IN S_MATGB
     AND B~WAERS IN S_WAERS
     AND A~BUKRS  IN S_BUKRS.
   APPEND IT_REQ.  CLEAR : IT_REQ.
  ENDSELECT.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_CHECK_DATA.

  REFRESH : IT_TAB.
  LOOP AT IT_INS.
    CLEAR : IT_REQ, W_ZFEXRT, W_ZFTAMI, IT_TAB.

    READ TABLE IT_REQ WITH KEY ZFREQNO = IT_INS-ZFREQNO.

    CHECK SY-SUBRC = 0.

    MOVE-CORRESPONDING IT_INS  TO  IT_TAB.
    MOVE-CORRESPONDING IT_REQ  TO  IT_TAB.
    IF IT_INS-ZFTRANS = 'A'.
       IT_TAB-DAMBO = 'All Risk Air'.
    ELSEIF IT_INS-ZFTRANS = 'O'.
       IT_TAB-DAMBO = 'All Risk'.
    ENDIF.

    SELECT SINGLE ZFEXRT ZFTAMI INTO (W_ZFEXRT, W_ZFTAMI) FROM ZTINSRSP
     WHERE ZFREQNO = IT_INS-ZFREQNO AND ZFAMDNO = IT_INS-ZFAMDNO.
    IF SY-SUBRC = 0 AND W_ZFEXRT <> 0.
       IT_TAB-ZFEXRT = W_ZFEXRT.
       IT_TAB-ZFTAMI = W_ZFTAMI * W_ZFEXRT.
    ELSE.
       IT_TAB-ZFEXRT = W_ZFEXRT.
       IT_TAB-ZFTAMI = 0.
    ENDIF.

    APPEND IT_TAB.   CLEAR IT_TAB.
  ENDLOOP.
ENDFORM.                    " P1000_CHECK_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_TOP_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_TOP_PAGE.
  SKIP 1.
  WRITE:/60 '   적 하  보 험  부 보  명 세   ' COLOR 1.
  WRITE:/140 'DATE :', SY-DATUM.
*  SKIP 1.
  WRITE :/80 'M:자재구분  Inc:Incoterms D:보험등급 VIA:운송방법',
         140 'PAGE :', SY-PAGNO.

  FORMAT COLOR 1 INTENSIFIED OFF.
  ULINE.
  WRITE:/(04) ' Seq.',
         (10) '수입의뢰NO',
         (16) 'L/C 개설금액  ' RIGHT-JUSTIFIED,
         (05) 'CURR',
         (10) 'L/C개설일',
         (01) 'M',
         (10) '대표 H/S',
         (03) 'INC',
         (12) ' 환    율',
*        (15) '보 험 금 액(\)  ' RIGHT-JUSTIFIED,
         (15) '송 장 금 액     ' RIGHT-JUSTIFIED,
         (15) '부 보 금 액(\)  ' RIGHT-JUSTIFIED,
         (15) '보 험 금 액  ' RIGHT-JUSTIFIED,
         (05) 'CURR',
         (26) '선  적  항'.
  WRITE:/6(05) 'Plant',
         (27) '신용장-승인번호',
         (10) '부보 일자',
         (01) 'D',
         (10) '담보조건',
         (03) 'VIA',
         (09) ' 보험요율',
         (25) '증 권 번 호',
         (30) '품    명',
         (26) '도  착  항'.
  ULINE.
ENDFORM.                    " P1000_TOP_PAGE

*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_WRITE_DATA.

  CLEAR : W_SUBRC, W_TABIX, W_ZFTAMI, W_ZFKRWAMT, W_LOCAL.
  SORT IT_TAB BY ZFREQNO.
  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    MOVE  IT_TAB-ZFKRW   TO  W_LOCAL.
    FORMAT RESET.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    NEW-LINE.
    WRITE : SY-VLINE NO-GAP,
           (04) W_TABIX RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
           (10) IT_TAB-ZFREQNO          NO-GAP, '-'  NO-GAP,
           (13) IT_TAB-ZFINSEQ          NO-GAP, SY-VLINE NO-GAP,
           (03) IT_TAB-INCO1            NO-GAP, SY-VLINE NO-GAP,
           (10) IT_TAB-ZFINSDT          NO-GAP, SY-VLINE NO-GAP,
           (12) IT_TAB-ZFEXRT           NO-GAP, SY-VLINE NO-GAP,
           (05) IT_TAB-WAERS            NO-GAP,
           (15) IT_TAB-ZFIVAMT CURRENCY IT_TAB-WAERS
                        RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
           (05) IT_TAB-ZFKRW            NO-GAP,
           (15) IT_TAB-ZFKRWAMT CURRENCY IT_TAB-ZFKRW
                        RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
           (40) IT_TAB-ZFINNO           NO-GAP, SY-VLINE NO-GAP.
    HIDE: IT_TAB.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    NEW-LINE.
    WRITE : SY-VLINE NO-GAP,
           (04) ' '                      NO-GAP, SY-VLINE NO-GAP,
           (24) IT_TAB-ZFOPNNO           NO-GAP, SY-VLINE NO-GAP,
           (03) IT_TAB-ZFTRANS  CENTERED NO-GAP, SY-VLINE NO-GAP,
           (10) IT_TAB-ZFRSTAW           NO-GAP, SY-VLINE NO-GAP,
           (12) IT_TAB-ZFINRT            NO-GAP, SY-VLINE NO-GAP,
           (20) ' '                      NO-GAP, SY-VLINE NO-GAP,
           (05) IT_TAB-ZFINAMTC          NO-GAP,
           (15) IT_TAB-ZFINAMT CURRENCY IT_TAB-ZFINAMTC
                         RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
           (40) IT_TAB-MAKTX             NO-GAP, SY-VLINE NO-GAP.
    HIDE: IT_TAB.
    ULINE.
    W_ZFKRWAMT   =   W_ZFKRWAMT   +  IT_TAB-ZFKRWAMT.
    AT LAST.
      FORMAT COLOR 3 INTENSIFIED OFF.
      WRITE:/ SY-VLINE NO-GAP,
             (20) '   Total sum',
           81(01)'('               NO-GAP,
             (03) W_LOCAL          NO-GAP,
             (01) ')'              NO-GAP,
             (15) W_ZFKRWAMT     CURRENCY W_LOCAL
                                 RIGHT-JUSTIFIED NO-GAP,
           142 SY-VLINE NO-GAP.
      ULINE.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " P1000_WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_INITIALIZATION.
  CONCATENATE SY-DATUM(6) '01' INTO S_INSDT-LOW.
  S_INSDT-HIGH = SY-DATUM.
  APPEND S_INSDT.

  S_DOCST-SIGN   = 'I'.
  S_DOCST-OPTION = 'EQ'.
  S_DOCST-LOW    = 'O'.
  S_DOCST-HIGH   = SPACE.
  APPEND S_DOCST.

  S_DOCST-SIGN   = 'I'.
  S_DOCST-OPTION = 'EQ'.
  S_DOCST-LOW    = 'A'.
  S_DOCST-HIGH   = SPACE.
  APPEND S_DOCST.


ENDFORM.                    " P1000_INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  P1000_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_DOWNLOAD.
  LOOP AT IT_TAB.
     WMTO_S-AMOUNT = IT_TAB-ZFOPAMT.
     CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
           EXPORTING
               CURRENCY  = IT_TAB-WAERS
               AMOUNT_INTERNAL = WMTO_S-AMOUNT
           IMPORTING
               AMOUNT_DISPLAY  = WMTO_S-AMOUNT
           EXCEPTIONS
               INTERNAL_ERROR = 1.
     IT_TAB-ZFOPAMT = WMTO_S-AMOUNT.

     WMTO_S-AMOUNT = IT_TAB-ZFKRWAMT.
     CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
           EXPORTING
               CURRENCY  = 'KRW'
               AMOUNT_INTERNAL = WMTO_S-AMOUNT
           IMPORTING
               AMOUNT_DISPLAY  = WMTO_S-AMOUNT
           EXCEPTIONS
               INTERNAL_ERROR = 1.
     IT_TAB-ZFKRWAMT = WMTO_S-AMOUNT.

     WMTO_S-AMOUNT = IT_TAB-ZFINAMT.
     CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
           EXPORTING
               CURRENCY  = IT_TAB-ZFINAMTC
               AMOUNT_INTERNAL = WMTO_S-AMOUNT
           IMPORTING
               AMOUNT_DISPLAY  = WMTO_S-AMOUNT
           EXCEPTIONS
               INTERNAL_ERROR = 1.
     IT_TAB-ZFINAMT = WMTO_S-AMOUNT.

     MODIFY IT_TAB.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
        EXPORTING
        FILENAME = 'C:\TEMP\TEMP.TXT'
        FILETYPE = 'DAT'
   TABLES
       DATA_TAB = IT_TAB.

ENDFORM.                    " P1000_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_INS
*&---------------------------------------------------------------------*
FORM P2000_SHOW_INS USING    P_ZFREQNO
                             P_ZFAMDNO
                             P_ZFINSEQ.

   SET PARAMETER ID 'ZPOPNNO'   FIELD ' '.
   SET PARAMETER ID 'BES'       FIELD ' '.
   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
   SET PARAMETER ID 'ZPINSEQ'   FIELD P_ZFINSEQ.

   EXPORT 'BES'           TO MEMORY ID 'BES'.
   EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
   EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
   EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.
   EXPORT 'ZPINSEQ'       TO MEMORY ID 'ZPINSEQ'.

   IF P_ZFAMDNO > '00000'.
      CALL TRANSACTION 'ZIM47' AND SKIP  FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM43' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_INS
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.

  IF SY-LANGU EQ '3'.
     PERFORM   P1000_TOP_PAGE.
  ELSE.
     PERFORM   P1000_TOP_PAGE_EN.
  ENDIF.

* 레포트 Write
  PERFORM   P1000_WRITE_DATA.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   TABLES : ZTIMIMG00.
   DATA   : P_BUKRS    LIKE  ZTINS-BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> Company code SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P1000_TOP_PAGE_EN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_TOP_PAGE_EN.

  SKIP 1.
  WRITE:/50 '   Insurance Policy List   ' COLOR 1.
  SKIP 2.
  WRITE:/ 'DATE :', SY-DATUM,
          95 'INC:Incoterms   VIA:Transpotation method'.

  ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : SY-VLINE NO-GAP,
         (04) 'Seq.'              CENTERED NO-GAP, SY-VLINE NO-GAP,
         (24) 'Import Request No' CENTERED NO-GAP, SY-VLINE NO-GAP,
         (03) 'INC'               CENTERED NO-GAP, SY-VLINE NO-GAP,
         (10) 'Effect. DT'        CENTERED NO-GAP, SY-VLINE NO-GAP,
         (12) 'Exchange Rate'     CENTERED NO-GAP, SY-VLINE NO-GAP,
         (20) 'Invoice amount'    CENTERED NO-GAP, SY-VLINE NO-GAP,
         (20) 'Premium(Local)'    CENTERED NO-GAP, SY-VLINE NO-GAP,
         (40) 'Policy No'         CENTERED NO-GAP, SY-VLINE NO-GAP.
  NEW-LINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : SY-VLINE NO-GAP,
         (04) ' '              CENTERED NO-GAP, SY-VLINE NO-GAP,
         (24) 'Approval No'    CENTERED NO-GAP, SY-VLINE NO-GAP,
         (03) 'VIA'            CENTERED NO-GAP, SY-VLINE NO-GAP,
         (10) 'Rep. H/S'       CENTERED NO-GAP, SY-VLINE NO-GAP,
         (12) 'Ins. Rate'      CENTERED NO-GAP, SY-VLINE NO-GAP,
         (20) ' '              CENTERED NO-GAP, SY-VLINE NO-GAP,
         (20) 'Premium'        CENTERED NO-GAP, SY-VLINE NO-GAP,
         (40) 'Item name'      CENTERED NO-GAP, SY-VLINE NO-GAP.
  ULINE.

ENDFORM.                    " P1000_TOP_PAGE_EN
