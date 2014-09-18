*----------------------------------------------------------------------*
*   INCLUDE ZRIMTRSLSTTOP                                              *
*----------------------------------------------------------------------*
*&  프로그램명 : 수송 대상 LIST DATA DEFINE                            *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.09.16                                            *
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
TABLES: ZTBL,ZTIDS,ZTIMIMG10,LFA1.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,                 " 선택 LINE COUNT
       W_PAGE            TYPE I,                 " Page Counter
       W_LINE            TYPE I,                 " 페이지당 LINE COUNT
       LINE(3)           TYPE N,                 " 페이지당 LINE COUNT
       W_COUNT           TYPE I,                 " 전체 COUNT
       W_TABIX           LIKE SY-TABIX,
       W_ITCOUNT(3),                             " 품목 COUNT.
       W_FIELD_NM        LIKE DD03D-FIELDNAME.   " 필드명.

DATA:   BEGIN OF IT_TAB OCCURS 0,   ">> 내역.
        MARK      TYPE C,              " MARK
        ZFWERKS   LIKE ZTIDS-ZFWERKS,   " 대표 PLANT.
        W_WERKS    LIKE  T001W-NAME1,      " 수송처명.
        W_EBELN(15),
        ZFREBELN  LIKE  ZTIDS-ZFREBELN, " P/O NO.
        ZFSHNO    LIKE  ZTBL-ZFSHNO,    " 선적차수.
        ZFBLNO    LIKE  ZTIDS-ZFBLNO,
        ZFHBLNO   LIKE  ZTIDS-ZFHBLNO,
        ZFRSTAW   LIKE  ZTINSB-ZFRSTAW, " 대표 HS CODE!!!
        ZFPKCN    LIKE  ZTBL-ZFPKCN,    " 포장개수.
        ZFPKCNM   LIKE  ZTBL-ZFPKCNM,   " 포장개수단위.
        ZFNEWT    LIKE  ZTBL-ZFNEWT,    " 선적중량.
        ZFNEWTM   LIKE  ZTBL-ZFNEWTM,   " 선적중량단위.
        ZFINRC    LIKE  ZTIDS-ZFINRC,   " 세관
        DOMTEXT   LIKE  DD07T-DDTEXT,   " 세관명.
        ZFSTAMT   LIKE  ZTIDS-ZFSTAMT,  " 결제금액.
        ZFSTAMC   LIKE  ZTIDS-ZFSTAMC,  " 통화.
        ZFIDRNO   LIKE  ZTIDS-ZFIDRNO,  " 수입면허번호.
        ZFOPNNO   LIKE  ZTIDS-ZFOPNNO,  " L/C NO.
        ZFRGDSR   LIKE  ZTBL-ZFRGDSR,   " 대표품명.
        ZFCLSEQ   LIKE  ZTIDS-ZFCLSEQ,  " 통관순번.
        ZFIDSDT   LIKE  ZTIDS-ZFIDSDT,  " 수입면허일.
        ZFTOTAMT  LIKE  ZTIDS-ZFTOTAMT, " 총결제액.
        INCO1     LIKE  ZTIDS-INCO1,    " INCO.
        ZFIVNO    LIKE  ZTIDS-ZFIVNO.   " 통관입고요청관리번호.
DATA:   END   OF IT_TAB.

DATA: BEGIN OF IT_TMP OCCURS 0.
      INCLUDE STRUCTURE IT_TAB.
DATA: ZFIVDNO LIKE ZTIVIT-ZFIVDNO,
      CCMENGE LIKE ZTIVIT-CCMENGE.
DATA: END OF IT_TMP.

DATA: IT_TMP2 LIKE TABLE OF IT_TAB WITH HEADER LINE.

DATA: BEGIN OF IT_TRIT OCCURS 0,
      ZFIVNO  LIKE  ZTTRIT-ZFIVNO,
      ZFIVDNO LIKE  ZTTRIT-ZFIVDNO,
      GIMENGE LIKE  ZTTRIT-GIMENGE.
DATA: END OF IT_TRIT.


DATA: BEGIN OF IT_SELECTED OCCURS 0,
       GUBUN     TYPE C,                " 최초 구분.
       ZFIVNO    LIKE ZTIV-ZFIVNO,      " 통관입고 요청관리번호.
       ZFBLNO    LIKE  ZTIDS-ZFBLNO,    " B/L 관리번호.
       ZFHBLNO   LIKE  ZTIDS-ZFHBLNO,   " HBL.
       ZFCLSEQ   LIKE  ZTIDS-ZFCLSEQ,   " 통관순번.
       ZFPKCN    LIKE  ZTBL-ZFPKCN,     " 포장개수.
       ZFPKCNM   LIKE  ZTBL-ZFPKCNM,    " 포장개수단위.
       ZFNEWT    LIKE  ZTBL-ZFNEWT,     " 선적중량.
       ZFNEWTM   LIKE  ZTBL-ZFNEWTM.    " 선적중량단위.
DATA: END OF IT_SELECTED.


DATA : BEGIN OF IT_PK_COL OCCURS 0,
       ZFPKCN    TYPE I,    " 포장개수.
       ZFPKCNM   LIKE  ZTBL-ZFPKCNM.   " 포장개수단위.
DATA : END OF IT_PK_COL.

DATA : BEGIN OF IT_WT_COL OCCURS 0,
       ZFNEWT    LIKE  ZTBL-ZFNEWT,    " 선적중량.
       ZFNEWTM   LIKE  ZTBL-ZFNEWTM.   " 선적중량단위.
DATA : END OF IT_WT_COL.


DATA: BEGIN OF IT_TRS OCCURS 0,
       ZFIVNO    LIKE  ZTIV-ZFIVNO,     " 통관입고 요청관리번호.
       ZFBLNO    LIKE  ZTIDS-ZFBLNO,    " B/L 관리번호.
       ZFHBLNO   LIKE  ZTIDS-ZFHBLNO,    " HBL.
       ZFCLSEQ   LIKE  ZTIDS-ZFCLSEQ.   " 통관순번.
DATA : END OF IT_TRS.
