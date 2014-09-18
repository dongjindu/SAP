*----------------------------------------------------------------------*
*   INCLUDE ZRIMCUCSTNTOP                                              *
*----------------------------------------------------------------------*
*&  프로그램명 : 관세/부가세 Posting - 무환                            *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.09.04                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     : 무환 관세/부가세를 조회하여 회계처리한?
*&
*&---------------------------------------------------------------------*
TABLES : ZTCUCLCST,       " 통관 비?
         ZTBL,            " Bill of Lading
         ZTIDS,           " 수입면?
         LFA1,            " Vendor Master
         ZTIMIMG08,       " 관리코?
         ZTIMIMG11,       " G/R, I/V, 비용처리 Configuration
         J_1BT001WV,      " Assign Branch to Plant
         ZVT001W,
         SPOP.     " POPUP_TO_CONFIRM_... function 모듈 팝업화면 필?


*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF    IT_SELECTED OCCURS 0,
      ZFBLNO      LIKE ZTIDS-ZFBLNO,
      ZFCLSEQ     LIKE ZTIDS-ZFCLSEQ,
      ZFCSQ       LIKE ZTCUCLCST-ZFCSQ,
      ZFVEN       LIKE ZTCUCLCST-ZFVEN,
      ZFPAY       LIKE ZTCUCLCST-ZFPAY,
      ZTERM       LIKE ZTCUCLCST-ZTERM,
      MWSKZ       LIKE ZTCUCLCST-MWSKZ,
      ZFWERKS     LIKE ZTCUCLCST-ZFWERKS,
      ZFCSCD      LIKE ZTCUCLCST-ZFCSCD,
      ZFOCDT      LIKE ZTCUCLCST-ZFOCDT,
      KOSTL       LIKE ZTBL-KOSTL,
      ZFCAMT      LIKE ZTCUCLCST-ZFCAMT,
      GRP_MARK(10)     TYPE   C,
END OF IT_SELECTED.
*-----------------------------------------------------------------------
* SELECT RECORD SUM
*-----------------------------------------------------------------------
DATA: BEGIN OF    IT_SELECTED_SUM OCCURS 0,
      ZFVEN       LIKE ZTCUCLCST-ZFVEN,
      ZFPAY       LIKE ZTCUCLCST-ZFPAY,
      ZTERM       LIKE ZTCUCLCST-ZTERM,
      MWSKZ       LIKE ZTCUCLCST-MWSKZ,
      ZFWERKS     LIKE ZTCUCLCST-ZFWERKS,
      ZFOCDT      LIKE ZTCUCLCST-ZFOCDT,
      KOSTL       LIKE ZTBL-KOSTL,
      ZFCAMT      LIKE ZTCUCLCST-ZFCAMT,
      ZFCAMT_1    LIKE ZTCUCLCST-ZFCAMT,
      ZFCAMT_2    LIKE ZTCUCLCST-ZFCAMT,
      GRP_MARK(10)     TYPE   C,
END OF IT_SELECTED_SUM.
*-----------------------------------------------------------------------
* BDC 용 Table
*-----------------------------------------------------------------------
DATA:    BEGIN OF ZBDCDATA OCCURS 0.
         INCLUDE STRUCTURE BDCDATA.
DATA     END OF ZBDCDATA.

DATA : W_PROC_CNT        TYPE I,             " 처리건?
       SV_ZFVEN          LIKE ZTCUCLCST-ZFVEN,
       SV_ZFPAY          LIKE ZTCUCLCST-ZFPAY,
       SV_ZTERM          LIKE ZTCUCLCST-ZTERM,
       SV_MWSKZ          LIKE ZTCUCLCST-MWSKZ,
       SV_ZFWERKS        LIKE ZTCUCLCST-ZFWERKS,
       SV_ZFOCDT         LIKE ZTCUCLCST-ZFOCDT,
       SV_KOSTL          LIKE ZTBL-KOSTL,
       W_GRP_MARK(10)    TYPE C,
       GRP_MARK_F(10)    TYPE C,
       GRP_MARK_T(10)    TYPE C,
       SUM_ZFCAMT        LIKE ZTCUCLCST-ZFCAMT,
       SUM_ZFCAMT_1      LIKE ZTCUCLCST-ZFCAMT,
       SUM_ZFCAMT_2      LIKE ZTCUCLCST-ZFCAMT,
       W_ZFCAMT          LIKE ZTCUCLCST-ZFCAMT,
       W_POSDT           LIKE SY-DATUM,
       W_DOCDT           LIKE SY-DATUM,
       W_ZFBDT           LIKE SY-DATUM,
       W_NEWKO(8)        TYPE C,
       ZFFIYR            LIKE ZTCUCLCST-ZFFIYR,
       ZFACDO            LIKE ZTCUCLCST-ZFACDO,
       RADIO_NONE(1)     TYPE C,
       RADIO_ALL(1)      TYPE C,
       RADIO_ERROR(1)    TYPE C,
       DISPMODE(1)       TYPE C,
       TEMP_WRBTR(16),
       TEMP_WMWST(16),
       TEMP_KOSTL(10),
       TEMP_AUFNR(12),
       TEMP_ZZWORK(10),
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
