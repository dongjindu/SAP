*----------------------------------------------------------------------*
*   INCLUDE ZRIMBLCSTLSTTOP                                            *
*----------------------------------------------------------------------*
*&  프로그램명 : B/L 비용 회계처리 현황                                *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.09.26                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : B/L비용을 조회한다.
*&
*&---------------------------------------------------------------------*
TABLES : ZTBL,            " Bill of Lading
         ZTBLCST,       " 통관비?
         LFA1,            " Vendor Master
         ZTIMIMG00,       " 관리코?
         ZTIMIMG08,       " 관리코?
         BKPF.
*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
        ZFBLNO      LIKE ZTBLCST-ZFBLNO,
        ZFFIYR      LIKE ZTBLCST-ZFFIYR,
        ZFACDO      LIKE ZTBLCST-ZFACDO,
END OF IT_SELECTED.

DATA : W_PROC_CNT        TYPE I,             " 처리건?
       W_LOOP_CNT        TYPE I,             " Loop Count
       SV_ZFVEN          LIKE ZTBLCST-ZFVEN,
       SV_ZFPAY          LIKE ZTBLCST-ZFPAY,
       SV_ZFFIYR         LIKE ZTBLCST-ZFFIYR,
       SV_ZFACDO         LIKE ZTBLCST-ZFACDO,
       SV_ZFCSCD         LIKE ZTBLCST-ZFCSCD,
       SV_ZFWERKS        LIKE ZTBLCST-ZFWERKS,
       TOT_ZFCKAMT       LIKE ZTBLCST-ZFCKAMT,
       SUM_ZFCKAMT       LIKE ZTBLCST-ZFCKAMT.

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
