*----------------------------------------------------------------------*
*   INCLUDE ZRIMIDRLSTTOP                                              *
*----------------------------------------------------------------------*
*&  프로그램명 : 통관대장                                              *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.07                                            *
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
TABLES: ZTBL, ZTIDR, ZTIDS, ZTIMIMG10, LFA1, ZTBLINR_TMP, ZTCIVIT, EKET,
        ZTIMIMG02,   USR21, ADRP.

DATA : W_SEQ             TYPE I,
       W_ERR_CHK(1)      TYPE C,                 " ERROR CHECK
       W_SELECTED_LINES  TYPE P,                 " 선택 LINE COUNT
       W_PAGE            TYPE I,                 " Page Counter
       W_LINE            TYPE I,                 " 페이지당 LINE COUNT
       LINE(3)           TYPE N,                 " 페이지당 LINE COUNT
       W_COUNT           TYPE I,                 " 전체 COUNT
       W_TABIX           LIKE SY-TABIX,          " TABLE INDEX
       W_ITCOUNT(3),                             " 품목 COUNT.
       W_FIELD_NM        LIKE DD03D-FIELDNAME.   " 필드명.

DATA:   BEGIN OF IT_TAB OCCURS 0,        "> 내역.
        ZFBLNO    LIKE  ZTBL-ZFBLNO,     "> B/L 관리번호.
        ZFCCNO    LIKE  ZTBL-ZFCCNO,     "> 통관접수번호.
        ZFINRNO   LIKE  ZTIDR-ZFINRNO,   "> 반입번호.
        ZFREBELN  LIKE  ZTBL-ZFREBELN,   "> 계약번호.
        ZFSHNO    LIKE  ZTBL-ZFSHNO,     "> 선적차수.
        ZFIDRNO   LIKE  ZTIDS-ZFIDRNO,   "> 신고번호.
        ZFCARNM   LIKE  ZTIDR-ZFCARNM,   "> 선기명.
        ZFHBLNO   LIKE  ZTBL-ZFHBLNO,    "> HOUSE B/L.
        ZFMBLNO   LIKE  ZTBL-ZFMBLNO,    "> MASTER B/L.
        ZFIVAMK   LIKE  ZTCIVIT-ZFIVAMK, "> 송장금액.
        ZFPKCNT   LIKE  ZTIDR-ZFPKCNT,   "> 포장개수.
        PKCT      TYPE  I,               "> 포장수.
        ZFPKNM    LIKE  ZTIDR-ZFPKNM,    "> 포장단위.
        ZFTOWT    LIKE  ZTIDR-ZFTOWT,    "> 총중량.
        ZFTOWTM   LIKE  ZTIDR-ZFTOWTM,   "> 총중량 단위.
        ZFENDT    LIKE  ZTIDR-ZFENDT,    "> 입항일.
        ZFINDT    LIKE  ZTIDR-ZFINDT,    "> 반입일.
        ZFIDWDT   LIKE  ZTIDR-ZFIDWDT,   "> 신고일.
        ZFIDSDT   LIKE  ZTIDS-ZFIDSDT,   "> 접수일.
        ZFINRC    LIKE  ZTIDR-ZFINRC,    "> 신고지세관.
        IN_NM(20) TYPE  C,               "> 세관명.
        EINDT     LIKE  SY-DATUM,        "> 납기일.
        ZFCCCNAM  LIKE  ZTBL-ZFCCCNAM,   "> 통관담당자.
        PS_NM(20) TYPE  C.
DATA:   END   OF IT_TAB.
