*&---------------------------------------------------------------------*
*& Report  ZRIMSKLOCRCT
*&---------------------------------------------------------------------*
*&  프로그램명 : 내국신용장 물품수령 증명서(인수증)                    *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.01.15                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     : 인수증을 프린터 하기위한 리스트 출력.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*


REPORT  ZRIMSKLOCRCT MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.

TABLES: ZTRED, ZTREDSG1,ZVREQHD_ST,ZTREQIT.
*-----------------------------------------------------------------------
* 물품수령 증명서 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFREQNO  LIKE    ZTREQIT-ZFREQNO,   "수입의뢰 관리번호.
       STAWN    LIKE    ZTREQIT-STAWN,     "HS CODE.
       ZFOPNDT  LIKE    ZVREQHD_ST-ZFOPNDT,"개설일.
       ZFREDNO  LIKE    ZTRED-ZFREDNO,     "인수증관리번호.
       ZFISNO   LIKE    ZTRED-ZFISNO,      "인수증 발급번호.
       EBELN    LIKE    ZTRED-EBELN,       "P/O 번호.
       ZFREVDT  LIKE    ZTRED-ZFREVDT,     "인수일.
       ZFISUDT  LIKE    ZTRED-ZFISUDT,     "발급일.
       ZFREXDT  LIKE    ZTRED-ZFREXDT,     "인수증 유효기일.
       ZFSCONM  LIKE    ZTRED-ZFSCONM,     "공급자 상호.
       ZFRCONM  LIKE    ZTRED-ZFRCONM,     "수령인 상호.
       ZFGDDT   LIKE    ZTRED-ZFGDDT,      "물품인도기일.
       ZFEXDT   LIKE    ZTRED-ZFEXDT,      "유효기일"
       ZFTXN4   LIKE    ZTRED-ZFTXN4,      "수령인 사업자등록 번호.
       ZFRCHNM  LIKE    ZTRED-ZFRCHNM,     "수령인 대표자명.
       ZFREAMF  LIKE    ZTRED-ZFREAMF,     "인수금액 외화.
       ZFREAMFC LIKE    ZTRED-ZFREAMFC,    "인수금액 외화 통화.
       ZFREAMK  LIKE    ZTRED-ZFREAMK,     "인수금액 원화.
       ZFKRW    LIKE    ZTRED-ZFKRW,       "원화 통화.
       ZFTOTAM  LIKE    ZTRED-ZFTOTAM,     "총금액.
       ZFTOTAMC LIKE    ZTRED-ZFTOTAMC,    "  통화.
       ZFETCD1  LIKE    ZTRED-ZFETCD1,     "기타조항.
       ZFLLCON  LIKE    ZTRED-ZFLLCON,     "LOCAL LC NO
       ZFOBNEID LIKE    ZTRED-ZFOBNEID,    "개설은행 BANK KEY
       ZFOBNM   LIKE    ZTRED-ZFOBNM,      "개설은행 식별자.
       ZFOBBR   LIKE    ZTRED-ZFOBBR,      "개설은행 지점명.
       ZFOPAMF  LIKE    ZTRED-ZFOPAMF,     "개설금액 외화.
       ZFOPAMFC LIKE    ZTRED-ZFOPAMFC,    "개설은행 통화.
       ZFREMK1  LIKE    ZTRED-ZFREMK1,     "참조사항.
       ZFREMK2  LIKE    ZTRED-ZFREMK2,     "참조사항.
       ZFREMK3  LIKE    ZTRED-ZFREMK3,     "참조사항.
       ZFREMK4  LIKE    ZTRED-ZFREMK4,     "참조사항.
       ZFREMK5  LIKE    ZTRED-ZFREMK5,     "참조사항.
       ZFDOCNO  LIKE    ZTRED-ZFDOCNO,     "전자문서 번호.
       MAKTX    LIKE    ZTREDSG1-MAKTX,    "품명.
       ZFLSG1   LIKE    ZTREDSG1-ZFLSG1,   "순번.
       ZFGOSD1  LIKE    ZTREDSG1-ZFGOSD1,  "규격.
       ZFQUN    LIKE    ZTREDSG1-ZFQUN,    "수량.
       ZFQUNM   LIKE    ZTREDSG1-ZFQUNM,   "수량단가.
       NETPR    LIKE    ZTREDSG1-NETPR,    "단가.
       ZFNETPRC LIKE    ZTREDSG1-ZFNETPRC, "단가 통화.
       ZFREAM   LIKE    ZTREDSG1-ZFREAM,   "인수금액.
       ZFREAMC  LIKE    ZTREDSG1-ZFREAMC.  "인수금액 통화.
DATA : END OF IT_TAB.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       LINE(3)           TYPE N,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME," 필드명.
       W_SUBRC           LIKE SY-UCOMM,
       W_TABIX           LIKE SY-TABIX.    " TABLE INDEX



*-----------------------------------------------------------------------
* Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 2.  " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
      PARAMETERS : P_REDNO  LIKE ZTRED-ZFREDNO
                   MEMORY ID  ZPREDNO.
SELECTION-SCREEN END OF BLOCK B1.

   INITIALIZATION.                    " 초기값 SETTING
   SET  TITLEBAR 'ZRIML'.          " TITLE BAR


*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

*  테이블 SELECT
   PERFORM   P1000_READ_DATA.
   IF SY-SUBRC NE 0.               " Not Found?
     MESSAGE S738.    EXIT.
   ENDIF.
* 레포트 Write
   PERFORM   P3000_DATA_WRITE.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_DATA.
** ZTRED
  REFRESH IT_TAB.
  SELECT *  FROM ZTRED
            WHERE   ZFREDNO = P_REDNO.
    MOVE-CORRESPONDING ZTRED TO IT_TAB.
    APPEND IT_TAB.
    CLEAR IT_TAB.
  ENDSELECT.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
**>> ZTRED
    SELECT SINGLE * FROM ZTREDSG1
                   WHERE ZFREDNO = IT_TAB-ZFREDNO .
    IF SY-SUBRC EQ 0.
      MOVE:   ZTREDSG1-MAKTX    TO IT_TAB-MAKTX,    "품명.
              ZTREDSG1-ZFLSG1   TO IT_TAB-ZFLSG1,   "순번.
              ZTREDSG1-ZFGOSD1  TO IT_TAB-ZFGOSD1,  "규격.
              ZTREDSG1-ZFQUN    TO IT_TAB-ZFQUN,    "수량.
              ZTREDSG1-ZFQUNM   TO IT_TAB-ZFQUNM,  "수량단가.
              ZTREDSG1-NETPR    TO IT_TAB-NETPR,    "단가.
              ZTREDSG1-ZFNETPRC TO IT_TAB-ZFNETPRC, "단가 통화.
              ZTREDSG1-ZFREAM   TO IT_TAB-ZFREAM,   "인수금액.
              ZTREDSG1-ZFREAMC  TO IT_TAB-ZFREAMC.  "인수금액 통화.
    ENDIF.

**>> ZVREQHD_ST
    SELECT SINGLE * FROM ZVREQHD_ST
                   WHERE EBELN = IT_TAB-EBELN .
    IF SY-SUBRC EQ 0.
       MOVE: ZVREQHD_ST-ZFOPNDT TO IT_TAB-ZFOPNDT,
             ZVREQHD_ST-ZFREQNO  TO IT_TAB-ZFREQNO .
    ENDIF.
**> ZTREQIT
    SELECT SINGLE * FROM ZTREQIT
                   WHERE ZFREQNO = IT_TAB-ZFREQNO .
    IF SY-SUBRC EQ 0.
       MOVE ZTREQIT-STAWN TO IT_TAB-STAWN.
    ENDIF.

    MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE .
*   SET PF-STATUS  'ZRIML'.
   SET TITLEBAR 'ZRIML'.          " TITLE BAR
   PERFORM P3000_LINE_WRITE.
ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

   SKIP 8.
   WRITE : 54 'SKC 주식회사'.
   WRITE : / SY-ULINE,120 SY-VLINE.
   WRITE : / SY-VLINE,40 ' ',120 SY-VLINE,
           / SY-VLINE,50 '내국신용장물품수령증명서',120 SY-VLINE,
           / SY-VLINE                              ,120 SY-VLINE,
           / SY-VLINE,50 '(No. ', 73 ')' ,120 SY-VLINE,
             SY-ULINE,                   120 SY-VLINE,
           / SY-VLINE,'',25 SY-VLINE, 120 SY-VLINE,
     / SY-VLINE,'    물품  공급자',25 SY-VLINE,
            30 IT_TAB-ZFSCONM,120 SY-VLINE,
          / SY-VLINE,'',25 SY-VLINE,   120 SY-VLINE,
            SY-ULINE,                  120 SY-VLINE,
          / SY-VLINE,'',25 SY-VLINE, 60 SY-VLINE,
                     '',85 SY-VLINE, 120 SY-VLINE,
          / SY-VLINE, '    물품인수일자',25 SY-VLINE,
                      30 IT_TAB-ZFREVDT,60 SY-VLINE,
                      61 '     물품인수금액',85 SY-VLINE,
                      90 IT_TAB-ZFREAMFC,
                      95 IT_TAB-ZFREAMF CURRENCY IT_TAB-ZFREAMFC,
                                           120 SY-VLINE,
         / SY-VLINE,'',25 SY-VLINE,   60 SY-VLINE,
                     '',85 SY-VLINE, 120 SY-VLINE,
           SY-ULINE,                       120 SY-VLINE,
         / SY-VLINE,'    인수품명세서',25 SY-VLINE,     120 SY-VLINE,
         / SY-VLINE,'           ',25 SY-VLINE,     120 SY-VLINE,
         / SY-VLINE,'           ',25 SY-VLINE,     120 SY-VLINE,
         / SY-VLINE,'           ',25 SY-VLINE,30 'HS Code:',
           43 IT_TAB-STAWN,                       120 SY-VLINE,
        / SY-VLINE,'           ',25 SY-VLINE, 30  IT_TAB-MAKTX,
                                                   120 SY-VLINE.
*>> MULTI DATA WRITE.
 PERFORM P3000_MULTIDATA.

 WRITE: / SY-VLINE,'           ',25 SY-VLINE,     120 SY-VLINE,
        / SY-VLINE,' ',25 SY-VLINE,26 SY-ULINE,   120 SY-VLINE,
        / SY-VLINE,' ',25 SY-VLINE,50 ' ',120 SY-VLINE,
        / SY-VLINE,' ',25 SY-VLINE,50 'TOTAL:',58 IT_TAB-ZFTOTAMC,
                       61 IT_TAB-ZFTOTAM CURRENCY IT_TAB-ZFTOTAMC,
                                                   120 SY-VLINE,
        / SY-VLINE,' ',25 SY-VLINE,50 ' ',120 SY-VLINE,
          SY-ULINE,                               120 SY-VLINE,
        / SY-VLINE,40'',     120 SY-VLINE,
        / SY-VLINE,50'관  련  내  국  신 용 장 ',     120 SY-VLINE,
        / SY-VLINE,40' ',     120 SY-VLINE,
          SY-ULINE,                              120 SY-VLINE,
        / SY-VLINE,'',25 SY-VLINE,26'',49 SY-VLINE,
                      52'',73 SY-VLINE,78 '',97 SY-VLINE,
                     104 '',120 SY-VLINE,
        / SY-VLINE,'    개 설 은 행',25 SY-VLINE,26'      L / C  No',
                     49 SY-VLINE, 52'    금      액',73 SY-VLINE,
                     78 ' 유 효 기 간',
                     97 SY-VLINE,
                    103 '인 도 기 일',120 SY-VLINE,
        / SY-VLINE,'',25 SY-VLINE,26'',49 SY-VLINE,
                     52'',73 SY-VLINE,78 '',97 SY-VLINE,
                    104 '',120 SY-VLINE,
          SY-ULINE,                              120 SY-VLINE,
       / SY-VLINE,'',25 SY-VLINE,26'',49 SY-VLINE,
                     52'',73 SY-VLINE,78 '',97 SY-VLINE,
                    104 '',120 SY-VLINE,
      / SY-VLINE, 6 IT_TAB-ZFOBNM,25 SY-VLINE,30 IT_TAB-ZFLLCON,
                     49 SY-VLINE, 52 IT_TAB-ZFOPAMFC,
                     55 IT_TAB-ZFOPAMF CURRENCY IT_TAB-ZFOPAMFC,
                     73 SY-VLINE,
                     80 IT_TAB-ZFEXDT,97 SY-VLINE,
                    103 IT_TAB-ZFGDDT,120 SY-VLINE,
      / SY-VLINE,'', 25 SY-VLINE,26'',49 SY-VLINE,
                     52'',73 SY-VLINE,78 '',97 SY-VLINE,
                   104 '',120 SY-VLINE,
       SY-ULINE,                              120 SY-VLINE,
     / SY-VLINE,' ',25 SY-VLINE, 26 '',120 SY-VLINE,
     / SY-VLINE,'    기       타',25 SY-VLINE, 50 'Local open DATE:',
                    68 IT_TAB-ZFOPNDT,        120 SY-VLINE,
     / SY-VLINE,' ',25 SY-VLINE, 26 '',       120 SY-VLINE,
       SY-ULINE,                              120 SY-VLINE,
     / SY-VLINE,40'                    ',     120 SY-VLINE,
     / SY-VLINE,40'                    ',     120 SY-VLINE,
/ SY-VLINE,40'위 물품을 틀림없이 수령하였음을 증명함.',120 SY-VLINE,
     / SY-VLINE,40'                    ',     120 SY-VLINE,
     / SY-VLINE,40'                    ',     120 SY-VLINE,
     / SY-VLINE,40'                    ',     120 SY-VLINE,
     / SY-VLINE,40'                    ',     120 SY-VLINE,
     / SY-VLINE,60'발급일자  :',80'년',90'월',100'일',120 SY-VLINE,
     / SY-VLINE,40'',     120 SY-VLINE,
     / SY-VLINE,60'물품수령인:',              120 SY-VLINE,
     / SY-VLINE,40'',                         120 SY-VLINE,
     / SY-VLINE,40'',                         120 SY-VLINE,
     / SY-VLINE,40'',                         120 SY-VLINE,
       SY-ULINE,                              120 SY-VLINE,
     / SY-VLINE,40'',                         120 SY-VLINE,
     / SY-VLINE,40'',                         120 SY-VLINE,
     / SY-VLINE,5'*유의사항.',                 120 SY-VLINE,
/ SY-VLINE,5 '1.물품수령증명서는 관련 세금계산서 건별로 대응하여',
                                '발급하여야 함.',120 SY-VLINE,
/ SY-VLINE,5 '  다만,내국신용장 조건에 따라 수출용원자재를 분할',
'공급받는',
'경우에는 매반월 또는 동일역월을 단위로 하는',120 SY-VLINE,
/ SY-VLINE,5 '  경우에 한하여 동 기간중 분할 공급시마다 교부된',
'세금계산서상의 공급가액을 일괄하여 물품수령증명서를',120 SY-VLINE ,
/ SY-VLINE,5 '  발급 할 수 있음.',120 SY-VLINE,
/ SY-VLINE,5'2.대기업이 중소기업으로 부터 물품을 인수하는 경우에는',
'공급자 발행세금계산서사의 발급일로 부터 10일 이',120 SY-VLINE ,
/ SY-VLINE,5'  내에 발급하여야 함.',120 SY-VLINE ,
/ SY-VLINE,5'3.물품수령 증명서의 물품명세는 관련 내국신용장의',
'물품명세와 일치하여야 함.',120 SY-VLINE,
/ SY-VLINE,5'4.물품수령 증명서상의 물품수령인의 서명 또는 인감은',
'관련내국신용장의 개설의뢰시 신고한 서명 또는 인감',120 SY-VLINE,
/ SY-VLINE,5'  (물품매도 확약서상의 서명또는 인감을 기준으로 함)과',
'일치하여야 함.',120 SY-VLINE,
/ SY-VLINE,5'5.물품수령 증명서상의 물품인수일자는 관련세금 계산서상의',
'공급일자를 모두 기재 하여야 함.',120 SY-VLINE,
/ SY-VLINE,40'',                            120 SY-VLINE,
/ SY-VLINE,40'',                            120 SY-VLINE,
 SY-ULINE,                                  120 SY-VLINE.
  SKIP 1.
   WRITE:/108 '제일제당 주식회사'.
   IF W_COUNT >= 6.
     PERFORM P3000_LAST_WRITE."별첨 문서.
   ENDIF.
ENDFORM.                    " P3000_LINE_WRITE


*&---------------------------------------------------------------------*
*&      Form  P3000_MULTIDATA
*&---------------------------------------------------------------------*
FORM P3000_MULTIDATA.

  W_COUNT = SY-TABIX.
  W_COUNT = 0.
  LOOP AT IT_TAB.
     W_COUNT = W_COUNT + 1.
     IF W_COUNT >= 6.
       MESSAGE S378. CONTINUE.
     ENDIF.
     WRITE: / SY-VLINE,'           ',25 SY-VLINE,30 IT_TAB-EBELN,
            40 IT_TAB-ZFGOSD1,60 IT_TAB-ZFQUN UNIT IT_TAB-ZFQUNM,
            75 IT_TAB-ZFQUNM,
            78 IT_TAB-ZFNETPRC,
            81 IT_TAB-NETPR  CURRENCY IT_TAB-ZFNETPRC,
            90 IT_TAB-ZFTOTAMC,
            95 IT_TAB-ZFTOTAM CURRENCY IT_TAB-ZFTOTAMC,
           120 SY-VLINE.
  ENDLOOP.
ENDFORM.                    " P3000_MULTIDATA

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LAST_WRITE.
  SKIP 5.
  WRITE:/40 '[ 별 첨  인 수 품  명 세 서 ]'.
  SKIP 3.
  WRITE:/  SY-ULINE,                                 120 SY-VLINE,
        /  SY-VLINE,'인수품명세서',25 SY-VLINE,      120 SY-VLINE,
        /  SY-VLINE,'            ',25 SY-VLINE,      120 SY-VLINE,
        /  SY-VLINE,'            ',25 SY-VLINE,      120 SY-VLINE,
        /  SY-VLINE,'            ',25 SY-VLINE,26 'HS Code:',
            36 IT_TAB-STAWN,                         120 SY-VLINE,
        /  SY-VLINE,'             ',25 SY-VLINE,26  IT_TAB-MAKTX,
                                                    120 SY-VLINE.
  W_LINE = SY-TABIX.
  W_LINE = 0.
LOOP AT IT_TAB.
  W_LINE = W_LINE + 1.

  IF W_LINE <= 5. "기존에 있는 데이타 생략.
    CONTINUE.
  ENDIF.
  WRITE: / SY-VLINE,'           ',25 SY-VLINE,26 IT_TAB-EBELN,
           40 IT_TAB-ZFGOSD1,55 IT_TAB-ZFQUN UNIT IT_TAB-ZFQUNM,
           70,IT_TAB-NETPR  CURRENCY IT_TAB-ZFNETPRC,
           85 IT_TAB-ZFREAMC,90 IT_TAB-ZFREAM CURRENCY IT_TAB-ZFREAMC,
                                                     120 SY-VLINE.
ENDLOOP.

  WRITE:/ SY-ULINE, 120 SY-VLINE.

ENDFORM.                    " P3000_LAST_WRITE
