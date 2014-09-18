*&---------------------------------------------------------------------*
*& Report  ZRIMLOCRCT1
*&---------------------------------------------------------------------*
*&  프로그램명 : 내국신용장 물품수령 증명서                            *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.01.15                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     : 인수증을 발행 하기위한 인수증 조회.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*


REPORT  ZRIMLOCRCT1  MESSAGE-ID ZIM
                     LINE-SIZE 116
                     NO STANDARD PAGE HEADING.
TABLES: ZTRED, ZTREDSG1,ZTREQST,ZTREQHD.
*-----------------------------------------------------------------------
* 물품수령 증명서 INTERNAL TABLE
*-----------------------------------------------------------------------

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFREDNO  LIKE    ZTRED-ZFREDNO,     "인수증관리번호.
       ZFGDNO   LIKE    ZTREDSG1-ZFGDNO,   "상품부호.
       MAKTX    LIKE    ZTREDSG1-MAKTX,    "품명.
       ZFLSG1   LIKE    ZTREDSG1-ZFLSG1,   "순번.
       ZFGOSD1  LIKE    ZTREDSG1-ZFGOSD1,  "규격.
       ZFQUN    LIKE    ZTREDSG1-ZFQUN,    "수량.
       ZFQUNM   LIKE    ZTREDSG1-ZFQUNM,   "수량단위.
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
       W_MAX_ZFREQNO     LIKE ZTREQHD-ZFREQNO,
       W_MAX_ZFAMDNO     LIKE ZTREQST-ZFAMDNO,
       W_SUBRC           LIKE SY-UCOMM,
       W_TABIX           LIKE SY-TABIX.    " TABLE INDEX

DATA  SUM_ZFQUN  LIKE   ZTREDSG1-ZFQUN.
DATA  SUM_ZFREAM LIKE   ZTREDSG1-ZFREAM.

*-----------------------------------------------------------------------
* Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 2.  " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
      PARAMETERS : P_REDNO  LIKE ZTRED-ZFREDNO.

SELECTION-SCREEN END OF BLOCK B1.

   INITIALIZATION.                    " 초기값 SETTING
   SET  TITLEBAR 'ZIML4'.          " TITLE BAR


*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

*  테이블 SELECT
   PERFORM   P1000_READ_DATA.
   DESCRIBE TABLE IT_TAB LINES W_LINE.
   IF W_LINE EQ 0.               " Not Found.
     MESSAGE S738.
     EXIT.
   ENDIF.
* 레포트 Write
   PERFORM   P3000_DATA_WRITE.

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA.

  REFRESH: IT_TAB.
  SELECT SINGLE *
    FROM ZTRED
   WHERE ZFREDNO = P_REDNO.

*>> SELECT LOCAL L/C 개설일.
  SELECT MAX( ZFREQNO )  INTO W_MAX_ZFREQNO
    FROM ZTREQHD
   WHERE EBELN = ZTRED-EBELN.

  SELECT MAX( ZFAMDNO ) INTO W_MAX_ZFAMDNO
    FROM ZTREQST
   WHERE ZFREQNO = W_MAX_ZFREQNO.

  SELECT SINGLE *
    FROM ZTREQST
   WHERE ZFREQNO = W_MAX_ZFREQNO
     AND ZFAMDNO = W_MAX_ZFAMDNO.

*>> SELECT ITEM

  SELECT * FROM ZTREDSG1
           WHERE ZFREDNO =  P_REDNO.
    CLEAR IT_TAB.
    MOVE-CORRESPONDING ZTREDSG1 TO IT_TAB.
    APPEND IT_TAB.
  ENDSELECT.
  CLEAR: SUM_ZFQUN, SUM_ZFREAM.
  SELECT SUM( ZFQUN ) INTO SUM_ZFQUN
         FROM ZTREDSG1
        WHERE ZFREDNO = P_REDNO.
  SELECT SUM( ZFREAM ) INTO SUM_ZFREAM
         FROM ZTREDSG1
        WHERE ZFREDNO = P_REDNO.


ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE .

   SET PF-STATUS 'ZIML4'.
   SET TITLEBAR  'ZIML4'.          " TITLE BAR
   PERFORM P3000_LINE_WRITE.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  SKIP 2.
  WRITE:/50 '내국신용장물품수령증명서'.
  SKIP 5.
  WRITE:/4 '발급번호     :', ZTRED-ZFISNO,
        /4 '발급일자     :', ZTRED-ZFISUDT,
        /4 '공급자       :', ZTRED-ZFSCONM,
        /4 '인수일자     :', ZTRED-ZFREVDT,
        /4 '인수금액     :', ZTRED-ZFREAMFC,
                             ZTRED-ZFREAMF CURRENCY ZTRED-ZFREAMFC
                             LEFT-JUSTIFIED,
                             ZTRED-ZFKRW,
                             ZTRED-ZFREAMK CURRENCY ZTRED-ZFKRW
                             LEFT-JUSTIFIED.
  IF NOT ZTRED-ZFREXDT IS INITIAL.
     WRITE:/4 '문서유효기일 :',ZTRED-ZFREXDT.
  ENDIF.
  IF  NOT ZTRED-ZFETCD1 IS INITIAL
   OR NOT ZTRED-ZFETCD2 IS INITIAL
   OR NOT ZTRED-ZFETCD3 IS INITIAL
   OR NOT ZTRED-ZFETCD4 IS INITIAL
   OR NOT ZTRED-ZFETCD5 IS INITIAL.

      WRITE: /4 '기타조건     :'.
      IF  NOT ZTRED-ZFETCD1 IS INITIAL.
          WRITE:19 ZTRED-ZFETCD1.
      ENDIF.
      IF  NOT ZTRED-ZFETCD3 IS INITIAL.
          WRITE: /19 ZTRED-ZFETCD2.
      ENDIF.
      IF  NOT ZTRED-ZFETCD3 IS INITIAL.
          WRITE: /19 ZTRED-ZFETCD3.
      ENDIF.
      IF  NOT ZTRED-ZFETCD4 IS INITIAL.
          WRITE: /19 ZTRED-ZFETCD4.
      ENDIF.
       IF  NOT ZTRED-ZFETCD5 IS INITIAL.
          WRITE: /19 ZTRED-ZFETCD5.
      ENDIF.

  ENDIF.
  SKIP 2.
*>> MULTI DATA WRITE.
  WRITE:/4 SY-ULINE,50 '<인수물품내역>',65 SY-ULINE.
  SKIP 1.
  PERFORM P3000_MULTIDATA.
  SKIP 2.
  WRITE:/4 SY-ULINE,46 '<관련 내국신용장 내역>',69 SY-ULINE.
  WRITE:/4 '개설은행     :',ZTRED-ZFOBNM.
  IF NOT ZTRED-ZFOBNEID IS INITIAL.
      WRITE:/4 '개설은행전자서명:',ZTRED-ZFOBNEID.
  ENDIF.
  WRITE:/4 '신용장 번호  :',ZTRED-ZFLLCON.

  IF NOT ZTRED-ZFOPAMFC IS INITIAL.
     WRITE: /4 '신용장 금액  :',ZTRED-ZFOPAMFC,
                               ZTRED-ZFOPAMF CURRENCY ZTRED-ZFOPAMFC
                               LEFT-JUSTIFIED.
  ENDIF.
  IF NOT ZTRED-ZFOPAMK IS INITIAL.
     IF NOT ZTRED-ZFOPAMFC IS INITIAL.SKIP 1. ENDIF.
     WRITE: ZTRED-ZFKRW,
            ZTRED-ZFOPAMK CURRENCY ZTRED-ZFKRW
                            LEFT-JUSTIFIED.
  ENDIF.
  WRITE:/4 '인도기일     :',ZTRED-ZFGDDT,
        /4 '유효기일     :',ZTRED-ZFEXDT.
  IF   NOT ZTRED-ZFREMK1 IS INITIAL
    OR NOT ZTRED-ZFREMK2 IS INITIAL
    OR NOT ZTRED-ZFREMK3 IS INITIAL
    OR NOT ZTRED-ZFREMK4 IS INITIAL.
    WRITE:/4 '참조사항     :'.
  ENDIF.
  IF NOT ZTRED-ZFREMK1 IS INITIAL.
    WRITE:19 ZTRED-ZFREMK1.
  ENDIF.
  IF NOT ZTRED-ZFREMK2 IS INITIAL.
    WRITE:/19 ZTRED-ZFREMK2.
  ENDIF.
  IF NOT ZTRED-ZFREMK3 IS INITIAL.
    WRITE:/19 ZTRED-ZFREMK3.
  ENDIF.
  IF NOT ZTRED-ZFREMK4 IS INITIAL.
    WRITE:/19 ZTRED-ZFREMK4.
  ENDIF.
  SKIP 2.
  WRITE:/4 SY-ULINE,50 '<물품 수령인>',65 SY-ULINE.
  SKIP 1.
  WRITE:/4 '기   관  명  :',ZTRED-ZFRCONM,
        /4 '대 표 자 명  :',ZTRED-ZFRCHNM,
        /4 '전 자 서 명  :',ZTRED-ZFTXN4.
  SKIP 2.
  WRITE:/4 SY-ULINE.
  WRITE:/4 SY-VLINE,'*유의사항.',116 SY-VLINE.
  WRITE:/4 SY-VLINE, 116 SY-VLINE.
  WRITE:/4 SY-VLINE,'1. 이 물품수령증명서의 발급일은 관련 내국신용장의',
  '물품 인도기일 이전이어야 합니다.', 116 SY-VLINE.
  WRITE:/4 SY-VLINE, 116 SY-VLINE.
  WRITE:/4 SY-VLINE,'2. 이 물품수령증명서는 공급자의 세금계산서',
  '발행일로부터 10일 이내에 발급하여야 합니다.', 116 SY-VLINE.
  WRITE:/4 SY-VLINE, 116 SY-VLINE.
  WRITE:/4 SY-VLINE,'3. 이 물품수령증명서의 물품명세는 관련',
  '내국신용장의 물품명세와 일치하여야 합니다.', 116 SY-VLINE.
  WRITE:/4 SY-VLINE, 116 SY-VLINE.
  WRITE:/4 SY-VLINE,'4. 이 물품수령증명서의 물품 수령인의 인감 또는',
  '서명은 관련 내국신용장의 개설의뢰시 신고한 인감 또는 서명',
  116 SY-VLINE.
  WRITE:/4 SY-VLINE,'   (물품매도확약서상의 인감 또는 서명을',
 '기준으로함)과 일치하여여 합니다.', 116 SY-VLINE.
  WRITE:/4 SY-VLINE, 116 SY-VLINE.
  WRITE:/4 SY-VLINE,'5. 이 물품수령증명서의 물품 인수일자는 공급자가',
  '교부한 세금계산서상의 공급 일자와 일치하여야 합니다.', 116 SY-VLINE.
  WRITE:/4 SY-VLINE,116 SY-VLINE.
  WRITE:/4 SY-ULINE.
  SKIP 1.
  WRITE:/4 SY-ULINE.
  WRITE:/4 SY-VLINE,'이 전자문서는  "무역업무자동화 촉진에관한법률" 에',
  '의거 전자문서교환방식으로 발행된 물품 수령증명서로서',116 SY-VLINE.
  WRITE:/4 SY-VLINE,'수혜업자는 동 법률 시행규정 제12조의',
  '별표2 제1장 제7조에서 정한 바에 따라 적색고무인을 날인하여야 합니다.'
  ,116 SY-VLINE.
  WRITE:/4 SY-VLINE,116 SY-VLINE.
  WRITE:/4 SY-ULINE.
  SKIP 1.
  IF W_COUNT >= 10.
      PERFORM P3000_LAST_WRITE."별첨 문서.
  ENDIF.
ENDFORM.                    " P3000_LINE_WRITE


*&---------------------------------------------------------------------*
*&      Form  P3000_MULTIDATA
*&---------------------------------------------------------------------*
FORM P3000_MULTIDATA.

  W_COUNT = 0.
  WRITE:/04 SY-ULINE.
  WRITE:/04 SY-VLINE,(45) '대표품명'CENTERED,
            SY-VLINE,(25) '수   량' CENTERED,
*           SY-VLINE,(20) '단가',
            SY-VLINE,(35) '금   액' CENTERED,116 SY-VLINE.
  WRITE:/04 SY-ULINE.

  LOOP AT IT_TAB.
     W_COUNT = W_COUNT + 1.
     IF W_COUNT >= 10.
       MESSAGE S656. CONTINUE.
     ENDIF.
*>> 2002.1.9 이채경 변경.
     WRITE:/4 SY-VLINE,(45)IT_TAB-MAKTX,
              SY-VLINE,53 SUM_ZFQUN UNIT IT_TAB-ZFQUNM,
                       76 IT_TAB-ZFQUNM,
              SY-VLINE,90 IT_TAB-ZFREAMC,
              95 SUM_ZFREAM CURRENCY IT_TAB-ZFREAMC,116 SY-VLINE.
     WRITE:/04 SY-ULINE.
*     WRITE:/4 SY-VLINE,(35)IT_TAB-MAKTX,
*            SY-VLINE,(16)IT_TAB-ZFQUN UNIT IT_TAB-ZFQUNM,
*                     (03)IT_TAB-ZFQUNM,
*            SY-VLINE,(20)IT_TAB-NETPR  CURRENCY IT_TAB-ZFNETPRC,
*            SY-VLINE,(03) IT_TAB-ZFREAMC,
*                     (20)IT_TAB-ZFREAM CURRENCY IT_TAB-ZFREAMC,
*            116 SY-VLINE.
*    WRITE:/04 SY-ULINE.
*    AT LAST.
*       WRITE:/4 SY-VLINE,'TOTAL',43 ZTRED-ZFTQUN UNIT ZTRED-ZFTQUNM,
*             61 ZTRED-ZFTQUNM,
*             90 ZTRED-ZFTOTAMC,
*             95 ZTRED-ZFTOTAM CURRENCY ZTRED-ZFTOTAMC,116 SY-VLINE.
*       WRITE:/04 SY-ULINE.
*    ENDAT.
     EXIT.
  ENDLOOP.

ENDFORM.                    " P3000_MULTIDATA

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  SKIP 5.
  WRITE:/40 '[ 별 첨  인 수 품  명 세 서]'.
  SKIP 3.
  WRITE:/04 SY-VLINE,(35)'품명/규격',
            SY-VLINE,(20) '수량',
            SY-VLINE,(20) '단가',
            SY-VLINE,(20) '금액',116 SY-VLINE.
  WRITE:/04 SY-ULINE.

  W_LINE = SY-TABIX.
  W_LINE = 0.
  LOOP AT IT_TAB.
     W_LINE = W_LINE + 1.
     IF W_LINE <= 9. "기존에 있는 데이타 생략.
       CONTINUE.
     ENDIF.
      WRITE:/4 SY-VLINE,(35)IT_TAB-MAKTX,
            SY-VLINE,(16)IT_TAB-ZFQUN UNIT IT_TAB-ZFQUNM,
                     (03)IT_TAB-ZFQUNM,
            SY-VLINE,(20)IT_TAB-NETPR  CURRENCY IT_TAB-ZFNETPRC,
            SY-VLINE,(03) IT_TAB-ZFREAMC,
                     (20)IT_TAB-ZFREAM CURRENCY IT_TAB-ZFREAMC,
            116 SY-VLINE.
    WRITE:/04 SY-ULINE.
  ENDLOOP.

  WRITE:/ SY-ULINE, 116 SY-VLINE.

ENDFORM.                    " P3000_LAST_WRITE
