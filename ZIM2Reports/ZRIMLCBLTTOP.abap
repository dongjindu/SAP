*&---------------------------------------------------------------------
*& Report  ZRIMLCBL                                                   *
*&---------------------------------------------------------------------
*&  프로그램명 : L/C 잔량 리스트
*&      작성자 : 맹 성 호  INFOLINK Ltd.
*&      작성일 : 2002.12.03
*&---------------------------------------------------------------------
*&   DESC.     : L/C 개설된 금액에서 B/L을 입수한 이후의 잔량을 조회.
*&
*&---------------------------------------------------------------------
TABLES : ZTREQHD,                  " 수입의뢰 헤더.
         ZTREQST,                  " 수입의뢰 상태.
         ZTBL,                     " B/L Header
         ZTBLIT,                   " B/L Item
         ZTPMTHD,                  " Payment Notice Header
         LFA1.                     " Vendor Master
*-----------------------------------------------------------------------
* SELECT RECORD 절.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFHBLNO      LIKE ZTBL-ZFHBLNO,
      EBELN        LIKE ZTREQHD-EBELN,
      ZFOPNNO      LIKE ZTREQHD-ZFOPNNO,
END OF IT_SELECTED.

DATA : W_PROC_CNT        TYPE I,             " 처리건?
       W_LOOP_CNT        TYPE I,             " Loop Count
       SV_ZFOPNNO        LIKE ZTREQHD-ZFOPNNO,
       SV_ZFHBLNO        LIKE ZTBL-ZFHBLNO,
       SUM_ZFBLAMT       LIKE ZTBL-ZFBLAMT,
       TOT_ZFLASTAM      LIKE ZTREQHD-ZFLASTAM,
       TOT_ZFBLAMT       LIKE ZTBL-ZFBLAMT,
       TOT_W_LCBLAMT     LIKE ZTBL-ZFBLAMT.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_UPDATE_CNT      TYPE I,
       W_BUTTON_ANSWER   TYPE C.
