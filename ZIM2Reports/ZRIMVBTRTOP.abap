*----------------------------------------------------------------------*
*   INCLUDE ZRIMTIVSTOP                                                *
*----------------------------------------------------------------------*
*&  프로그램명 : 긴급 보세 운송 의뢰                                   *
*&      작성자 : 이석철 INFOLINK Ltd.
*&      작성일 : 2000.07.07                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

TABLES : ZTBLUG.          " 긴급 보세 운송 의?
*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFHBLNO     LIKE ZTBLUG-ZFHBLNO,          " House B/L No.
END OF IT_SELECTED.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX.      " TABLE INDEX
