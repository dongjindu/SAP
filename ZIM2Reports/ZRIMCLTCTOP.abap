*----------------------------------------------------------------------*
*   INCLUDE ZRIMCLTCTOP                                                *
*----------------------------------------------------------------------*
*&  프로그램명 : 수입신고(과세통관) 자료 생성                          *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.03.07                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

TABLES : ZTBL,            " Bill of Lading
         ZTCUCLIV,        " 통관 Invoice
         ZTCUCLIVIT,      " 통관 Invoice Item
         ZTCUCL,          " 통?
         ZTIMIMGTX,
         ZTIDR,           " 수입신?
         ZTIDRHS,         " 수입신고 란사?
         ZTIDRHSD,        " 수입신고 규?
         ZTIDRHSL,        " 수입신고 요건확?
         ZTBLIT  ,
         ZTIMIMG01,       " Payment Term Configuration
         ZTIMIMG02,       " Payment Term Configuration
         ZTIMIMG00.       " 수입시스템 Basic Config
*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFIVNO     LIKE ZTCUCLIV-ZFIVNO,           "Invoice 관리번?
      ZFBLNO     LIKE ZTCUCLIV-ZFBLNO,           "B/L 관리번?
      ZFCUST     LIKE ZTCUCLIV-ZFCUST,           "통관상?
      ZFCLSEQ    LIKE ZTCUCLIV-ZFCLSEQ,        "통관순?
END OF IT_SELECTED.
*-----------------------------------------------------------------------
* 통관 Table Key
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CUCL OCCURS 0,
      ZFBLNO     LIKE ZTCUCL-ZFBLNO,         " B/L 관리번?
      ZFCLSEQ    LIKE ZTCUCL-ZFCLSEQ,        " 통관순?
END OF IT_CUCL.
*-----------------------------------------------------------------------
* 통관용 INVOICE INTERNAL TABLE.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IVIT OCCURS 0.
      INCLUDE STRUCTURE ZTCUCLIVIT.
DATA : END OF IT_IVIT.
*-----------------------------------------------------------------------
* 란/행 구성?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDRIT OCCURS 0,
      ZFBLNO     LIKE ZTCUCLIV-ZFBLNO,         " B/L 관리번?
*      ZFREQNO    LIKE ZTIV-ZFREQNO,            " 수입의뢰 관리번?
      ZFIVNO     LIKE ZTCUCLIVIT-ZFIVNO,       " Invoice 관리번?
      ZFIVDNO    LIKE ZTCUCLIVIT-ZFIVDNO,      " Invoice Item 일련번?
      MATNR      LIKE ZTCUCLIVIT-MATNR,        " Material number
      STAWN      LIKE ZTCUCLIVIT-STAWN,        " H/S Code
      MENGE      LIKE ZTCUCLIVIT-MENGE,        " Invoice 수?
      MEINS      LIKE ZTCUCLIVIT-MEINS,        " Base unit of measure
      NETPR      LIKE ZTCUCLIVIT-NETPR,        " Net price
      PEINH      LIKE ZTCUCLIVIT-PEINH,        " Price unit
      BPRME      LIKE ZTCUCLIVIT-BPRME,        " Order price unit
      TXZ01      LIKE ZTCUCLIVIT-TXZ01,        " Short Text
      ZFIVAMT    LIKE ZTCUCLIVIT-ZFIVAMT,      " Invoice 금?
      ZFIVAMC    LIKE ZTCUCLIVIT-ZFIVAMC,      " Invoice 금액 통?
      ZFCONO     LIKE ZTIDRHSD-ZFCONO,         " 란번?
      ZFRONO     LIKE ZTIDRHSD-ZFRONO,         " 규격(행)번?
END OF IT_IDRIT.

RANGES  R_TERM    FOR    ZTCUCLIV-ZFCUST    OCCURS 10.

DATA : W_PROC_CNT        TYPE I,             " 처리건?
       W_CRET_CNT        TYPE I,             " 생성건?
       W_MENGE           LIKE ZTCUCLIVIT-MENGE,
       W_ZFQNT           LIKE ZTIDRHS-ZFQNT,
       W_ZFBLNO          LIKE ZTCUCLIV-ZFBLNO,
       W_ZFCLSEQ         LIKE ZTCUCLIV-ZFCLSEQ.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_STAMT           LIKE ZTIDR-ZFSTAMT,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_UPDATE_CNT      TYPE I.
