*----------------------------------------------------------------------*
*   INCLUDE ZRIMCLSCTOP                                                *
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
         ZTREQHD,         " 수입의?
         ZTREQST,         " 수입의뢰 상?
         ZTIV,            " Invoice
         ZTIVIT,          " Invoice Item
         ZTCUCLIV,        " 통관 Invoice
         ZTCUCLIVIT,      " 통관 Invoice Item
         ZTCUCL,          " 통?
         ZTIMIMG06,       " 통관환?
         ZTIMIMGTX,
         ZTIDR,           " 수입신?
         ZTIDRHS,         " 수입신고 란사?
         ZTIDRHSD,        " 수입신고 규?
         ZTIDRHSL,        " 수입신고 요건확?
         ZTREQIL,         " 수입추천 내?
         ZTIMIMG01,       " Payment Term Configuration
         ZTIMIMG00,       " 수입시스템 Basic Config
         ZTIMIMG08,
         ZTIMIMG10,
         ZTIMIMG03,
         LFA1,
         T001,
         EKKO.            " Purchasing Document Header
*-----------------------------------------------------------------------
* INVOICE TABLE INTERNAL TABLE.
*-----------------------------------------------------------------------
DATA:  BEGIN OF IT_TAB OCCURS 0,
       ZFIVNO    LIKE   ZTIV-ZFIVNO,
       ZFBLNO    LIKE   ZTIV-ZFBLNO,
       ZFHBLNO   LIKE   ZTBL-ZFHBLNO,
       ZFCUT     LIKE   ZTIDR-ZFCUT,         " 관세사.
       ZFCLSEQ   LIKE   ZTIDR-ZFCLSEQ,
       NAME1     LIKE   LFA1-NAME1,
       ZFIVAMT   LIKE   ZTIV-ZFIVAMT,
       ZFIVAMC   LIKE   ZTIV-ZFIVAMC,
       ZFIVAMK   LIKE   ZTIV-ZFIVAMK,
       ZFEXRT    LIKE   ZTIV-ZFEXRT,
       ZFPOYN    LIKE   ZTIV-ZFPOYN,
       POYN(07)  TYPE   C,
       LIFNR     LIKE   ZTIV-LIFNR,
       NAME2     LIKE   LFA1-NAME1,
       ZFPHVN    LIKE   ZTIV-ZFPHVN,
       ZFINRC    LIKE   ZTIDR-ZFINRC,        " 신고지 세관.
       INRC      LIKE   DD07T-DDTEXT,        " 신고지 세관.
       ZFINRCD   LIKE   ZTIDR-ZFINRCD,
       INRCD     LIKE   DD07T-DDTEXT,
       ZFCLCD    LIKE   ZTIV-ZFCLCD,         " 통관구분.
       ZFREBELN  LIKE   ZTBL-ZFREBELN,       " 대표 P/O NO.
       ZFRPTTY   LIKE   ZTBL-ZFRPTTY,        " 수입신고형태.
       ZFSHNO    LIKE   ZTBL-ZFSHNO,         " 선적차수.
       IMTRD     LIKE   ZTBL-IMTRD,          " 수입자구분.
       BUTXT     LIKE   T001-BUTXT,          " 회사이름.
       BUKRS     LIKE   ZTBL-BUKRS,          " 회사코드.
       RPTTY     LIKE   DD07T-DDTEXT,        " 수입신고형태.
       ZFPONC    LIKE   ZTIDR-ZFPONC,        " 수입거래구분.
       ZFCOCD    LIKE   ZTIDR-ZFCOCD,        " 징수형태.
       COCD      LIKE   DD07T-DDTEXT,        " 징수형태.
       INCO1     LIKE   ZTIDR-INCO1,         " INCOTERMS.
       ZFINAMT   LIKE   ZTIDR-ZFINAMT,       " 보험료.
       ZFTFA     LIKE   ZTIDR-ZFTFA,         " 선운임.
       ZFTFAC    LIKE   ZTIDR-ZFTFAC,        " 선운임.
       ZFBNARCD  LIKE   ZTIDR-ZFBNARCD,
       PONC      LIKE   ZTIMIMG08-ZFCDNM,    "
       ZFITKD    LIKE   ZTIDR-ZFITKD,        " 수입종류.
       ITKD      LIKE   DD07T-DDTEXT,        " 수입종류.
       ZFBNARM   LIKE   ZTIMIMG03-ZFBNARM,
       ZFCUST    LIKE   ZTIV-ZFCUST,
       CUST      LIKE   DD07T-DDTEXT,
       ZFGRST    LIKE   ZTIV-ZFGRST,
       GRST      LIKE   DD07T-DDTEXT,
       ZFCIVST   LIKE   ZTIV-ZFCIVST,
END OF IT_TAB.
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

RANGES  R_TERM1   FOR    ZTIV-ZFPOYN    OCCURS 10.
RANGES  R_TERM2   FOR    ZTIV-ZFCLCD    OCCURS 10.
RANGES  R_TERM3   FOR    ZTIV-ZFCUST    OCCURS 10.

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
       W_ZFIVPKHD        LIKE ZTIV-ZFIVAMT,
       W_ZFBLNO          LIKE ZTIV-ZFBLNO,
       W_ZFCLSEQ         LIKE ZTCUCL-ZFCLSEQ.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_UPDATE_CNT      TYPE I,
       W_BUTTON_ANSWER   TYPE C,
       W_NAME1(35)       TYPE C,
       W_GUBN            TYPE C,
       W_POYN(4)         TYPE C.
