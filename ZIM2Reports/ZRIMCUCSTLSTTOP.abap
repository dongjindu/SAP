*----------------------------------------------------------------------*
*   INCLUDE ZRIMCUCSTLSTTOP                                           *
*----------------------------------------------------------------------*
*&  프로그램명 : 통관비용 회계처리 현황                                *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.09.19                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 통관비용을 조회한다.
*&
*&---------------------------------------------------------------------*
TABLES : ZTBL,            " Bill of Lading
         ZTCUCLCST,       " 통관비?
         ZTIDS,           " 수입면?
         LFA1,            " Vendor Master
         ZTIMIMG00,       " 관리코?
         ZTIMIMG08,       " 관리코?
         BKPF.
*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
        ZFBLNO      LIKE ZTCUCLCST-ZFBLNO,
        ZFCLSEQ     LIKE ZTCUCLCST-ZFCLSEQ,
        ZFFIYR      LIKE ZTCUCLCST-ZFFIYR,
        ZFACDO      LIKE ZTCUCLCST-ZFACDO,
END OF IT_SELECTED.

DATA : W_PROC_CNT        TYPE I,             " 처리건?
       W_LOOP_CNT        TYPE I,             " Loop Count
       SV_ZFVEN          LIKE ZTCUCLCST-ZFVEN,
       SV_ZFFIYR         LIKE ZTCUCLCST-ZFFIYR,
       SV_ZFACDO         LIKE ZTCUCLCST-ZFACDO,
       SV_ZFCSCD         LIKE ZTCUCLCST-ZFCSCD,
       TOT_ZFCAMT        LIKE ZTCUCLCST-ZFCAMT,
       SUM_ZFCAMT        LIKE ZTCUCLCST-ZFCAMT.

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
