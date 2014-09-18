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

TABLES : ZTIV,            ">통관요청.
         *ZTIV,           ">통관요청 HEADER.
         ZTIVIT,          ">통관요청 Item
         ZTMSHD,          ">모선관리 HEADER.
         ZTBL,            ">Bill of Lading
         ZTBLCST,         ">B/L 비용.
         ZTCUCL,          ">통관.
         ZTCUCLIV,        ">통관 Invoice
         ZTREQHD,         ">수입의뢰.
         ZTRECST,         ">수입의뢰비용.
         ZTREQST,         ">수입의뢰상태/
         ZTPMTHD,         ">Payment Notice Head
         ZTPMTIV,         ">Payment Notice Invoice
         TCURS,           ">환율.
         ZTIDSUS,
         ZTIMIMG00,       ">수입시스템 Basic Config
         ZTIMIMG01,       ">Payment Term Configuration
         ZTIMIMG04,       ">Planned Cost Rate
         ZTIMIMG11,       ">G/R, I/V, 비용처리 Configuration
         LFBK,            ">Vendor Master (Bank Details)
         EKPO,            ">Purchasing Document Item
         EKET,            ">Scheduling Agreement Schedule Line
         T156,            ">Movement Type
         T001L,           ">Storage Locations
         SPOP.            ">POPUP_TO_CONFIRM_...
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
      ZFIVNO      LIKE ZTIV-ZFIVNO,          " Invoice 관리번호.
*      ZFREQNO     LIKE ZTIV-ZFREQNO,         " 수입의뢰 관리번호.
      ZFBLNO      LIKE ZTIV-ZFBLNO,          " B/L 관리번호.
      ZFCLSEQ     LIKE ZTCUCL-ZFCLSEQ,       " 통관순번.
      ZFIVAMK     LIKE ZTIV-ZFIVAMK,         " Invoice 금액(원화)
      ZFLASTAMK   LIKE ZTREQHD-ZFLASTAM,     " L/C 개설금액(원화)
      ZFIVAMTSK   LIKE ZTIV-ZFIVAMT,         " B/L 금액(원화) I/V 합?
      ZFIDRAMK    LIKE ZTIV-ZFIVAMT,         " 통관(원화)금?
      ZFLCCSK     LIKE ZTRECST-ZFCAMT,       " 수입의뢰비?
      ZFBLCSK     LIKE ZTBLCST-ZFCAMT,       " B/L 비용.
      ZFCRCSK     LIKE ZTCUCLCST-ZFCAMT,     " 통관비용.
      ZFCUAMT     LIKE ZTCUCLCST-ZFCAMT,     " 관세.
      ZFPLRTE     LIKE ZTIVIT-ZFPLRTE,       " Planned Cost Rate
END OF IT_CST.

DATA : W_ZFOPNDT         LIKE ZTREQST-ZFOPNDT,      " 개설?
       W_ZFAPLDT         LIKE ZTIMIMG04-ZFAPLDT,    " 적용?
       W_ZFEXRT          LIKE ZTIV-ZFEXRT,          " 환?
       W_PROC_CNT        TYPE I,                    " 처리건수.
       W_ERR_CNT         TYPE I,
       W_ZFCST           LIKE ZTIV-ZFPCST,          " 수입제비?
       W_ZFIVAMP         LIKE ZTIV-ZFIVAMT,         " Invoice 처리금액.
       W_ZFIVPDT         LIKE ZTIVHST-BUDAT,     " I/V Posting Date
       W_ZFIVDDT         LIKE ZTIVHST-BLDAT,     " I/V Document Date
       W_ZFGRPDT         LIKE ZTIVHST-BUDAT,     " G/R Posting Date
       W_ZFGRDDT         LIKE ZTIVHST-BLDAT,     " G/R Document Date
       W_ZFIDSDT         LIKE ZTIDS-ZFIDSDT,
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

DATA : W_LFA1            LIKE LFA1,
       W_LFA12           LIKE LFA1,
       W_EBELN           LIKE ZTREQHD-EBELN,
       W_MENGE           LIKE EKET-MENGE,
       W_WEMNG           LIKE EKET-WEMNG,
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
       F(20)             TYPE C,             " Field Name Alias
       LINE              TYPE I,
       W_BUTTON_ANSWER   TYPE C,
       W_LOOPLINES       LIKE SY-LOOPC,
       W_OLD_SUBRC       LIKE SY-SUBRC,
       W_ROW_MARK        TYPE C,
       G_PARAM_LINE      LIKE SY-TABIX.
DATA:  W_AMTTXT1(19),
       W_AMTTXT2(19),
       W_AMTLEN1 TYPE I,
       W_AMTLEN2 TYPE I.
