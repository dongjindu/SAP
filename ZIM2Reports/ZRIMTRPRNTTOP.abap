*&---------------------------------------------------------------------*
*& Include ZRIMTRCONFTOP                                               *
*&---------------------------------------------------------------------*
*&  프로그램명 : 발송증 출력 LIST  DATA DEFINE                         *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.10.04                                            *
*&     적용회사: 한수원.
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
TABLES: ZTTRHD,ZTTRIT,LFA1, T001W.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,                 " 선택 LINE COUNT
       W_PAGE            TYPE I,                 " Page Counter
       W_LINE            TYPE I,                 " 페이지당 LINE COUNT
       LINE(3)           TYPE N,                 " 페이지당 LINE COUNT
       W_COUNT           TYPE I,                 " 전체 COUNT
       W_TABIX           LIKE SY-TABIX,
       W_ITCOUNT(3),                             " 품목 COUNT.
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드명.
       W_LIST_INDEX      LIKE SY-TABIX,
       W_LFA1            LIKE LFA1,
       W_BUTTON_ANSWER   TYPE C,
       W_UPDATE_CNT      TYPE I,
       W_ZFGIDT(10).                    "출고일자.

DATA:   BEGIN OF IT_TAB OCCURS 0,   ">> 내역.
        MARK       TYPE  C,                " MARK
        ZFTRNO     LIKE  ZTTRHD-ZFTRNO,    " 보세창고출고번호.
        ZFGIDT     LIKE  ZTTRHD-ZFGIDT,    " 출고일자.
        ZFDRDT     LIKE  ZTTRHD-ZFDRDT,    " 수송기한일.
        ZFTRGB     LIKE  ZTTRHD-ZFTRGB,    " 수송구분.
        W_TRGB(4)  TYPE  C,                " 수송구분명.
        ZFDRMT     LIKE  ZTTRHD-ZFDRMT,    " 수송방법.
        W_DRMT(4)  TYPE  C,                " 수송방법명.
        ZFREBELN   LIKE  ZTTRHD-ZFREBELN,  " 대표 P/O.
        WERKS      LIKE  ZTTRHD-WERKS,     " 대표 PLANT.
        W_WERKS    LIKE  T001W-NAME1,      " 수송처명.
        ZFTRCO     LIKE  ZTTRHD-ZFTRCO,    " 운송업체.
        W_TRCO     LIKE  LFA1-NAME1,       " 운송업체명.
        ZFSENDER   LIKE  ZTTRHD-ZFSENDER,  " 발송자.
        ZFGIYN     LIKE  ZTTRHD-ZFGIYN,    " 출고처리유무.
        W_GIYN(12) TYPE  C.
DATA:   END   OF IT_TAB.

DATA: IT_TMP LIKE TABLE OF IT_TAB WITH HEADER LINE.

DATA: BEGIN OF IT_TABIT OCCURS 0,
      ZFTRNO      LIKE  ZTTRIT-ZFTRNO,     " 보세창고출고번호.
      ZFTRIT      LIKE  ZTTRIT-ZFTRIT,     " ITEM NO.
      MATNR       LIKE  ZTTRIT-MATNR,      " 자재번호.
      TXZ01       LIKE  ZTTRIT-TXZ01,      " 품명.
      CCMENGE     LIKE  ZTTRIT-CCMENGE,    " 통관수량.
      GIMENGE     LIKE  ZTTRIT-GIMENGE,    " 출고수량.
      WERKS       LIKE  ZTTRIT-WERKS,      " 대표 PLANT.
      W_WERKS     LIKE  T001W-NAME1,       " 수송처명.
      LGORT       LIKE  ZTTRIT-LGORT,      " Storage location
      W_LGORT     LIKE  LFA1-NAME1,        " 운송업체명.
      ZFIVNO      LIKE  ZTTRIT-ZFIVNO.     " 통관관리번호.
DATA: END OF IT_TABIT.

DATA : BEGIN OF IT_SELECTED OCCURS 0,
       ZFTRNO     LIKE ZTTRHD-ZFTRNO,      " TRANS. DOCUMENT NO.
       ZFGIDT     LIKE ZTTRHD-ZFGIDT,      " 출고일자.
       ZFDRDT     LIKE ZTTRHD-ZFDRDT,      " 수송기한일.
       ZFGIYN     LIKE ZTTRHD-ZFGIYN.      " 처리여부.
DATA : END OF IT_SELECTED.

DATA : IT_TRHD LIKE TABLE OF ZTTRHD WITH HEADER LINE.

DATA: BEGIN OF RG_SEL OCCURS 10,
         SIGN(1),
         OPTION(2),
         LOW  LIKE ZTTRHD-ZFTRNO,
         HIGH LIKE ZTTRHD-ZFTRNO,
      END   OF RG_SEL.
