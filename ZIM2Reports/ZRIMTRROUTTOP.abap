*&---------------------------------------------------------------------*
*& Include ZRIMTRROUTTOP                                               *
*&---------------------------------------------------------------------*
*&  프로그램명 : 반출증(보세창고출고)  DATA DEFINE                     *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.05                                            *
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
         W_TRGB(4)         TYPE  C,                " 수송구분명.
         TEMP              TYPE  F.

  DATA: BEGIN OF IT_TAB OCCURS 0,
        ZFTRNO      LIKE  ZTTRIT-ZFTRNO,     " 보세창고출고번호.
        ZFTRIT      LIKE  ZTTRIT-ZFTRIT,     " ITEM NO.
        WERKS       LIKE  ZTTRIT-WERKS,      " PLANT.
        EBELN       LIKE  ZTTRIT-EBELN,      " P/O 번호.
        ZFSHNO      LIKE  ZTTRIT-ZFSHNO,     " 선적차수.
        ZFBLNO      LIKE  ZTTRIT-ZFBLNO,     " B/L NO.
        ZFHBLNO     LIKE  ZTBL-ZFHBLNO,      " House B/L NO.
        W_WERKS     LIKE  T001W-NAME1,       " 수송처명.
        MATNR       LIKE  ZTTRIT-MATNR,      " 자재번호.
        TXZ01       LIKE  ZTTRIT-TXZ01,      " 품명.
        GIMENGE     LIKE  ZTTRIT-GIMENGE,    " 출고수량.
        MEINS       LIKE  ZTTRIT-MEINS,      " 단위.
        ZFNEWT      LIKE  ZTBL-ZFNEWT,       " B/L 총중량.
        ZFNEWTM     LIKE  ZTBL-ZFNEWTM.      " 단위.
  DATA: END OF IT_TAB.

  DATA : BEGIN OF IT_TOT OCCURS 0,   " TOTAL.
         MEINS    LIKE ZTTRIT-MEINS,
         GIMENGE  LIKE ZTTRIT-GIMENGE.
  DATA : END OF IT_TOT.
