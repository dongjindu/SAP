*----------------------------------------------------------------------*
*   INCLUDE ZRIMPAYMTOP                                                *
*----------------------------------------------------------------------*
*&  프로그램명 : Payment Notice 관리                                   *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.05.30                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     : Payment Notice 를 조회한다.
*&
*&---------------------------------------------------------------------*

TABLES : ZTPMTHD.           " Payment Notice Head
*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFPNNO     LIKE ZTPMTHD-ZFPNNO,    " Payment Notice 관리번?
      ZFPYDT     LIKE ZTPMTHD-ZFPYDT,
END OF IT_SELECTED.

DATA : BEGIN OF IT_ZTPMTHD OCCURS 0.
       INCLUDE STRUCTURE ZTPMTHD.
       DATA : LOCK     TYPE C VALUE 'N'.
DATA : END   OF IT_ZTPMTHD.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
*       W_ZFPYDT  LIKE ZTPMTHD-ZFPYDT,
       W_ZFPYDT(10),
       W_UPDATE_CNT      TYPE I,
       W_BUTTON_ANSWER   TYPE C.
