*----------------------------------------------------------------------*
*   INCLUDE ZRIMCLICTOP                                                *
*----------------------------------------------------------------------*
*&  프로그램명 : 수입신고(수입통관) 자료 생성                          *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
TABLES : ZTBL,            " Bill of Lading
         ZTBLINOU,        " 보세운?
         ZTBLINR,         " 반입신?
         ZTREQHD,         " 수입의?
         ZTREQST,         " 수입의뢰 상?
         ZTIV,            " Invoice
         ZTIVIT,          " Invoice Item
         ZTCUCLIV,        " 통관 Invoice
         ZTCUCLIVIT,      " 통관 Invoice Item
         ZTCUCL,          " 통?
         ZTIMIMG06,       " 통관환?
         ZTIDR,           " 수입신?
         ZTIDRHS,         " 수입신고 란사?
         ZTIDRHSD,        " 수입신고 규?
         ZTIDRHSL,        " 수입신고 요건확?
         ZTREQIL,         " 수입추천 내?
         ZTIMIMG01,       " Payment Term Configuration
         ZTIMIMG00,       " 수입시스템 Basic Config
         EKKO.            " Purchasing Document Header
*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFIVNO     LIKE ZTIV-ZFIVNO,           "Invoice 관리번?
      ZFBLNO     LIKE ZTIV-ZFBLNO,           "B/L 관리번?
      ZFCUST     LIKE ZTIV-ZFCUST,           "통관상?
      ZFCLSEQ    LIKE ZTCUCL-ZFCLSEQ,        "통관순?
END OF IT_SELECTED.
*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED_TMP OCCURS 0,
      ZFIVNO     LIKE ZTIV-ZFIVNO,           "Invoice 관리번?
      ZFBLNO     LIKE ZTIV-ZFBLNO,           "B/L 관리번?
      ZFCUST     LIKE ZTIV-ZFCUST,           "통관상?
      ZFCLSEQ    LIKE ZTCUCL-ZFCLSEQ,        "통관순?
END OF IT_SELECTED_TMP.
*-----------------------------------------------------------------------
* 통관 Table Key
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CUCL OCCURS 0,
      ZFBLNO     LIKE ZTCUCL-ZFBLNO,         " B/L 관리번?
      ZFCLSEQ    LIKE ZTCUCL-ZFCLSEQ,        " 통관순?
END OF IT_CUCL.
*-----------------------------------------------------------------------
* 수입의뢰 Table Key
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_REQHD OCCURS 0,
      ZFREQNO    LIKE ZTREQHD-ZFREQNO,       " 수입의뢰 관리번?
END OF IT_REQHD.
*-----------------------------------------------------------------------
* 란/행 구성?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDRIT OCCURS 0,
      ZFBLNO     LIKE ZTCUCLIV-ZFBLNO,         " B/L 관리번?
      ZFREQNO    LIKE ZTREQHD-ZFREQNO,         " 수입의뢰 관리번?
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

DATA : W_ZFAPLDT         LIKE ZTIMIMG06-ZFAPLDT,
       W_PROC_CNT        TYPE I,             " 처리건?
       W_CRET_CNT        TYPE I,             " 생성건?
       W_LOOP_CNT        TYPE I,             " Loop Count
       W_MENGE           LIKE ZTCUCLIVIT-MENGE,
       W_ZFIVNO          LIKE ZTCUCLIV-ZFIVNO,
       W_ZFBTSEQ         LIKE ZTBLINR-ZFBTSEQ,
       W_ZFQNT           LIKE ZTIDRHS-ZFQNT,
       W_ZFLASTAM        LIKE ZTREQHD-ZFLASTAM,
       W_ZFCKAMT         LIKE ZTRECST-ZFCKAMT,
       W_ZFINAMT         LIKE ZTIDR-ZFINAMT,
       W_ZFDUAM          LIKE ZTIDR-ZFDUAM,
       W_ZFIVAMT         LIKE ZTCUCLIV-ZFIVAMT,
       W_ZFIVAMT_S       LIKE ZTIV-ZFIVAMT,
       W_ZFIVPKHD        LIKE ZTIV-ZFIVAMT.

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

DATA  W_SUBRC            LIKE SY-SUBRC.
