*&---------------------------------------------------------------------*
*&  INCLUDE ZRIMIVPRCTOP                                               *
*&---------------------------------------------------------------------*
*&  프로그램명 : Invoice Processing - 입고처리,                        *
*&                                    비용배분, I/V, G/R,              *
*&                                    금액확정, 금액조회, Status 변경  *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.21                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

TABLES : ZTIV,            " Invoice
         ZTIVIT,          " Invoice Item
         ZTBL,            " Bill of Lading
         ZTBLCST,         " B/L 비?
         ZTCUCL,          " 통?
         ZTCUCLIV,        " 통관 Invoice
         ZTREQHD,         " 수입의?
         ZTRECST,         " 수입의뢰비?
         ZTREQST,         " 수입의뢰상?
         ZTPMTHD,         " Payment Notice Head
         ZTPMTIV,         " Payment Notice Invoice
         TCURS,           " 환?
         ZTIMIMG00,       " 수입시스템 Basic Config
         ZTIMIMG01,       " Payment Term Configuration
         ZTIMIMG04,       " Planned Cost Rate
         ZTIMIMG11,       " G/R, I/V, 비용처리 Configuration
         LFBK,            " Vendor Master (Bank Details)
         EKPO,            " Purchasing Document Item
         T156,            " Movement Type
         T001L,           " Storage Locations
         SPOP.     " POPUP_TO_CONFIRM_... function 모듈 팝업화면 필?
*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFIVNO     LIKE ZTIV-ZFIVNO,           "Invoice 관리번?
END OF IT_SELECTED.

*-----------------------------------------------------------------------
* 비용배분용 집계 Table
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CST OCCURS 0,
      ZFIVNO      LIKE ZTIV-ZFIVNO,          " Invoice 관리번?
*     ZFREQNO     LIKE ZTIV-ZFREQNO,         " 수입의뢰 관리번?
      ZFBLNO      LIKE ZTIV-ZFBLNO,          " B/L 관리번?
      ZFCLSEQ     LIKE ZTCUCL-ZFCLSEQ,       " 통관순?
      ZFIVAMK     LIKE ZTIV-ZFIVAMK,         " Invoice 금액(원화)
      ZFLASTAMK   LIKE ZTREQHD-ZFLASTAM,     " L/C 개설금액(원화)
      ZFIVAMTSK   LIKE ZTIV-ZFIVAMT,         " B/L 금액(원화) I/V 합?
      ZFIDRAMK    LIKE ZTIV-ZFIVAMT,         " 통관(원화)금?
      ZFLCCSK     LIKE ZTRECST-ZFCAMT,       " 수입의뢰비?
      ZFBLCSK     LIKE ZTBLCST-ZFCAMT,       " B/L 비?
      ZFCRCSK     LIKE ZTCUCLCST-ZFCAMT,     " 통관비?
      ZFCUAMT     LIKE ZTCUCLCST-ZFCAMT,     " 관?
*     ZFPLRTE     LIKE ZTIV-ZFPLRTE,         " Planned Cost Rate
END OF IT_CST.

DATA : W_ZFOPNDT         LIKE ZTREQST-ZFOPNDT,      " 개설?
       W_ZFAPLDT         LIKE ZTIMIMG04-ZFAPLDT,    " 적용?
       W_ZFEXRT          LIKE ZTIV-ZFEXRT,          " 환?
       W_ZFIVDNO         LIKE ZTIVIT-ZFIVDNO,
       W_PROC_CNT        TYPE I,                    " 처리건?
       W_ZFCST           LIKE ZTIV-ZFPCST,          " 수입제비?
*      W_ZFIVAMP         LIKE ZTIV-ZFIVAMP,         " Invoice 처리금?
*      W_ZFIVPDT         LIKE ZTIV-ZFIVPDT,         " I/V Posting Date
*      W_ZFIVDDT         LIKE ZTIV-ZFIVDDT,         " I/V Document Date
*      W_ZFGRPDT         LIKE ZTIV-ZFGRPDT,         " G/R Posting Date
*      W_ZFGRDDT         LIKE ZTIV-ZFGRDDT,         " G/R Document Date
       W_BWARTWE         LIKE RM07M-BWARTWE,        " Movement Type
       W_LGORT           LIKE RM07M-LGORT,          " Storage Locaion
       W_ZFAMDNO         LIKE ZTREQST-ZFAMDNO,      " Amend Seq.
       W_WEPOS           LIKE EKPO-WEPOS,
       RADIO_NONE(1)     TYPE C,
       RADIO_ALL(1)      TYPE C,
       RADIO_ERROR(1)    TYPE C,
       DISPMODE(1)       TYPE C.
*-----------------------------------------------------------------------
* BDC 용 Table
*-----------------------------------------------------------------------
DATA:    BEGIN OF ZBDCDATA OCCURS 0.
         INCLUDE STRUCTURE BDCDATA.
DATA     END OF ZBDCDATA.

DATA : UMODE             VALUE 'S',     " Async, Sync
*      ZFGFDYR           LIKE ZTIV-ZFGFDYR,
*      ZFGFDNO           LIKE ZTIV-ZFGFDNO,
*      ZFMDYR            LIKE ZTIV-ZFMDYR,
*      ZFMDNO            LIKE ZTIV-ZFMDNO,
       W_ZFIVAMT         LIKE ZTIV-ZFIVAMT,
       W_EBELN           LIKE ZTREQHD-EBELN,
       W_WERKS           LIKE ZTREQHD-ZFWERKS,
       TEMP_KURSF(10),
       TEMP_WRBTR(16),
       TEMP_BKTXT        LIKE BKPF-BKTXT,
       TEMP_MENGE(17),
       TEMP_BLART(2),
       TEMP_FNAM         LIKE BDCDATA-FNAM,
       TEMP_XBLNR(16),
       TEMP_ERFMG(17),
       OK-CODE           LIKE SY-UCOMM,
       LOOP_CNT(2).
*-----------------------------------------------------------------------
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
