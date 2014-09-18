*----------------------------------------------------------------------*
*   INCLUDE ZRIMBLCSTTOP                                               *
*----------------------------------------------------------------------*
*&  프로그램명 : B/L 비용 Posting                          *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.05.23                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     : B/L 비용을 조회하여 회계처리한?
*&
*&---------------------------------------------------------------------*
TABLES : ZTCUCLCST,       " 통관 비?
         ZTBL,            " Bill of Lading
         ZTIDS,           " 수입면?
         LFA1,            " Vendor Master
         ZTIMIMG08,       " 관리코?
         J_1BT001WV,      " Assign Branch to Plant
         ZVT001W,
         SPOP.     " POPUP_TO_CONFIRM_... function 모듈 팝업화면 필?


*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF    IT_SELECTED OCCURS 0,
      ZFVEN       LIKE ZTCUCLCST-ZFVEN,
      ZFPAY       LIKE ZTCUCLCST-ZFPAY,
      ZTERM       LIKE ZTCUCLCST-ZTERM,
      MWSKZ       LIKE ZTCUCLCST-MWSKZ,
      ZFWERKS     LIKE ZTCUCLCST-ZFWERKS,
      ZFCAMT      LIKE ZTCUCLCST-ZFCAMT,
END OF IT_SELECTED.
*-----------------------------------------------------------------------
* BDC 용 Table
*-----------------------------------------------------------------------
DATA:    BEGIN OF ZBDCDATA OCCURS 0.
         INCLUDE STRUCTURE BDCDATA.
DATA     END OF ZBDCDATA.

DATA : W_PROC_CNT        TYPE I,             " 처리건?
       W_LOOP_CNT        TYPE I,             " Loop Count
       SV_ZFVEN          LIKE ZTCUCLCST-ZFVEN,
       SV_ZFPAY          LIKE ZTCUCLCST-ZFPAY,
       SV_ZTERM          LIKE ZTCUCLCST-ZTERM,
       SV_MWSKZ          LIKE ZTCUCLCST-MWSKZ,
       SV_ZFWERKS        LIKE ZTCUCLCST-ZFWERKS,
       SUM_ZFCAMT        LIKE ZTCUCLCST-ZFCAMT,
       W_POSDT           LIKE SY-DATUM,
       W_DOCDT           LIKE SY-DATUM,
       ZFFIYR            LIKE ZTCUCLCST-ZFFIYR,
       ZFACDO            LIKE ZTCUCLCST-ZFACDO,
       RADIO_NONE(1)     TYPE C,
       RADIO_ALL(1)      TYPE C,
       RADIO_ERROR(1)    TYPE C,
       DISPMODE(1)       TYPE C,
       TEMP_WRBTR(16),
       TEMP_WMWST(16),
       W_WRBTR           LIKE ZTBLCST-ZFCKAMT,
       OK-CODE           LIKE SY-UCOMM,
       UMODE             VALUE 'S'.     " Async, Sync

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

DATA : W_J_1BT001WV    LIKE J_1BT001WV.
