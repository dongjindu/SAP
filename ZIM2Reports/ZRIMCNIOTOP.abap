*----------------------------------------------------------------------*
*   INCLUDE ZRIMCNIOTOP                                                *
*----------------------------------------------------------------------*
*&  프로그램명 : Container 반/출입                                    *
*&      작성자 : 이석철 INFOLINK Ltd.                                  *
*&      작성일 : 2000.07.08                                            *
*&  적용회사PJT:
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

TABLES : ZTBL,          " Bill of Lading Header.
         ZTBLCON,       " B/L Container
         ZTIDS,         " 수입면허.
         LFA1,          " VENDOR MASTER.
         T001W.         " PLANT NAME.

*& INTERNAL TABLE 정의-------------------------------------------------*

DATA : IT_TAB      LIKE    ZTBL        OCCURS 0 WITH HEADER LINE.
DATA : IT_LFA1     LIKE    LFA1        OCCURS 0 WITH HEADER LINE.
DATA : IT_T001W    LIKE    T001W       OCCURS 0 WITH HEADER LINE.



DATA : W_TEM(13),
       W_ERR_CHK(1)      TYPE C,
       W_SEQ(2)          TYPE   P,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_MOD             TYPE I,             " 나머지 계산용.
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드명.
       W_TABIX           LIKE SY-TABIX.      " TABLE INDEX
