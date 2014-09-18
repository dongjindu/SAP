*----------------------------------------------------------------------*
*   INCLUDE ZRIMRECSTLSTTOP                                           *
*----------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰비용 회계처리 현황                            *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.09.26                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 수입의뢰비용을 조회한다.
*&
*&---------------------------------------------------------------------*
TABLES : ZTREQHD,         " 수입의뢰
         ZTRECST,         " 통관비용
         LFA1,            " Vendor Master
         ZTIMIMG00,       " 관리코드
         ZTIMIMG08.       " 관리코드
*-----------------------------------------------------------------------
* SELECT RECORD용
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
        ZFREQNO     LIKE ZTRECST-ZFREQNO,
        ZFFIYR      LIKE ZTRECST-ZFFIYR,
        ZFACDO      LIKE ZTRECST-ZFACDO,
END OF IT_SELECTED.

DATA : W_PROC_CNT        TYPE I,             " 처리건수
       W_LOOP_CNT        TYPE I,             " Loop Count
       SV_ZFVEN          LIKE ZTRECST-ZFVEN,
       SV_ZFFIYR         LIKE ZTRECST-ZFFIYR,
       SV_ZFACDO         LIKE ZTRECST-ZFACDO,
       SV_ZFCD3          LIKE ZTIMIMG08-ZFCD3,
       TOT_ZFCKAMT       LIKE ZTRECST-ZFCKAMT,
       SUM_ZFCKAMT       LIKE ZTRECST-ZFCKAMT.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드명
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_UPDATE_CNT      TYPE I,
       W_BUTTON_ANSWER   TYPE C.
