*----------------------------------------------------------------------*
*   INCLUDE ZRIMCUCSTTOP                                              *
*----------------------------------------------------------------------*
*&  프로그램명 : 관세/부가세 Posting - 유환                            *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.05.23                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     : 유환 관세/부가세를 조회하여 회계처리한?
*&
*&---------------------------------------------------------------------*
TABLES : ZTCUCLCST,       " 통관 비?
         ZTBL,            " Bill of Lading
         BSEG,
         COBL,
         ZTIDS,           " 수입면?
         LFA1,            " Vendor Master
         ZTIMIMG00,       " 관리코?
         ZTIMIMG08,       " 관리코?
         ZTIMIMG11,       " G/R, I/V, 비용처리 Configuration
         J_1BT001WV,      " Assign Branch to Plant
         ZVT001W,
         SPOP.     " POPUP_TO_CONFIRM function 모듈 팝업화면 필?

*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF    IT_SELECTED OCCURS 0,
      ZFBLNO      LIKE ZTCUCLCST-ZFBLNO,
      ZFCLSEQ     LIKE ZTCUCLCST-ZFCLSEQ,
      ZFCSQ       LIKE ZTCUCLCST-ZFCSQ,
      ZFCSCD      LIKE ZTCUCLCST-ZFCSCD,
      BUKRS       LIKE ZTCUCLCST-BUKRS,
      ZFVEN       LIKE ZTCUCLCST-ZFVEN,
      ZFPAY       LIKE ZTCUCLCST-ZFPAY,
      ZTERM       LIKE ZTCUCLCST-ZTERM,
      MWSKZ       LIKE ZTCUCLCST-MWSKZ,
      ZFWERKS     LIKE ZTCUCLCST-ZFWERKS,
      ZFOCDT      LIKE ZTCUCLCST-ZFOCDT,
      ZFCAMT      LIKE ZTCUCLCST-ZFCAMT,
      ZFCAMT1     LIKE ZTCUCLCST-ZFCAMT,
      ZFCAMT2     LIKE ZTCUCLCST-ZFCAMT,
      GRP_MARK(10)    TYPE   C,
END OF IT_SELECTED.
*-----------------------------------------------------------------------
* SELECT RECORD SUM
*-----------------------------------------------------------------------
DATA: BEGIN OF    IT_SELECTED_SUM OCCURS 0,
      BUKRS       LIKE ZTCUCLCST-BUKRS,
      ZFVEN       LIKE ZTCUCLCST-ZFVEN,
      ZFPAY       LIKE ZTCUCLCST-ZFPAY,
      ZTERM       LIKE ZTCUCLCST-ZTERM,
      MWSKZ       LIKE ZTCUCLCST-MWSKZ,
      ZFWERKS     LIKE ZTCUCLCST-ZFWERKS,
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
*-----------------------------------------------------------------------
* 통관 비용관련 TABLE
*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_ZTCUCST OCCURS 0.
         INCLUDE STRUCTURE ZTCUCLCST.
DATA     END OF IT_ZTCUCST.
*-----------------------------------------------------------------------
* LOCK TABLE
*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_LOCKED OCCURS 0,
         ZFBLNO     LIKE   ZTIDS-ZFBLNO,
         ZFCLSEQ    LIKE   ZTIDS-ZFCLSEQ.
DATA     END OF IT_LOCKED.

*-----------------------------------------------------------------------
* ERROR 처리용 TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C.
DATA : END OF IT_ERR_LIST.

*-----------------------------------------------------------------------
* 비용관련 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_ZTIMIMG08 OCCURS 0.
         INCLUDE STRUCTURE ZTIMIMG08.
DATA     END OF IT_ZTIMIMG08.

DATA : W_PROC_CNT        TYPE I,             " 처리건?
       W_LOOP_CNT        TYPE I,             " Loop Count
       W_ERR_CNT         TYPE I,
       SV_ZFVEN          LIKE ZTCUCLCST-ZFVEN,
       SV_ZFPAY          LIKE ZTCUCLCST-ZFPAY,
       SV_ZTERM          LIKE ZTCUCLCST-ZTERM,
       SV_BUKRS          LIKE ZTCUCLCST-BUKRS,
       SV_MWSKZ          LIKE ZTCUCLCST-MWSKZ,
       SV_ZFOCDT         LIKE ZTCUCLCST-ZFOCDT,
       SV_ZFWERKS        LIKE ZTCUCLCST-ZFWERKS,
       W_GRP_MARK(10)    TYPE C,
       SUM_ZFCAMT        LIKE ZTCUCLCST-ZFCAMT,
       SUM_ZFCAMT1       LIKE ZTCUCLCST-ZFCAMT,
       SUM_ZFCAMT2       LIKE ZTCUCLCST-ZFCAMT,
       SV_ZFVEN_NM(20)   TYPE C,
       SV_ZFPAY_NM(20)   TYPE C,
       GRP_MARK_T(10)    TYPE C,
       W_POSDT           LIKE SY-DATUM,
       W_DOCDT           LIKE SY-DATUM,
       W_ZFBDT           LIKE SY-DATUM,
       ZFFIYR            LIKE ZTCUCLCST-ZFFIYR,
       ZFACDO            LIKE ZTCUCLCST-ZFACDO,
       RADIO_NONE(1)     TYPE C,
       RADIO_ALL(1)      TYPE C,
       RADIO_ERROR(1)    TYPE C,
       DISPMODE(1)       TYPE C,
       MARKFIELD         TYPE C,
       TEMP_WRBTR(16),
       TEMP_WMWST(16),
       W_WRBTR           LIKE ZTBLCST-ZFCKAMT,
       OK-CODE           LIKE SY-UCOMM.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       INCLUDE(8),
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_UPDATE_CNT      TYPE I,
       ANWORT(1),
       W_LOCK_CHK,
       W_SY_SUBRC        LIKE SY-SUBRC,
       W_MOD             TYPE I,
       W_BUTTON_ANSWER   TYPE C.

DATA : W_J_1BT001WV    LIKE J_1BT001WV.
DATA  SV_ZFBLNO        LIKE ZTBL-ZFBLNO.
DATA  SV_ZFCLSEQ       LIKE ZTCUCL-ZFCLSEQ.
