&---------------------------------------------------------------------*
*&  INCLUDE ZRIMCDISTTOP                                               *
*&---------------------------------------------------------------------*
*&  프로그램명 : 자재별 부대비용 계산을 위한 Include                   *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.21                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

TABLES : ZTBL,            " Bill of Lading
         ZTBLCST,         " B/L 비?
         ZTCUCL,          " 통?
         ZTCUCLIV,        " 통관 Invoice
         ZTIV,            " Invoice
         ZTIVIT,          " Invoice Item
         ZTREQHD,         " 수입의?
         ZTRECST,         " 수입의뢰비?
         ZTREQST,         " 수입의뢰상?
         TCURS.           " 환?
*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFBLNO  LIKE ZTCUCL-ZFBLNO,            " B/L 관리번?
      ZFCLSEQ LIKE ZTCUCL-ZFCLSEQ,           " 통관순?
END OF IT_SELECTED.

*-----------------------------------------------------------------------
* 비용배분용 집계 Table
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CST OCCURS 0,
      ZFIVNO      LIKE ZTIV-ZFIVNO,          " Invoice 관리번?
      ZFREQNO     LIKE ZTIV-ZFREQNO,         " 수입의뢰 관리번?
      ZFBLNO      LIKE ZTIV-ZFBLNO,          " B/L 관리번?
      ZFCLSEQ     LIKE ZTCUCL-ZFCLSEQ,       " 통관순?
      ZFIVAMK     LIKE ZTIV-ZFIVAMK,         " Invoice 금액(원화)
      ZFLASTAMK   LIKE ZTREQHD-ZFLASTAM,     " L/C 개설금액(원화)
      ZFIVAMTSK   LIKE ZTIV-ZFIVAMT,         " B/L 금액(원화) I/V 합?
      ZFIDRAMK    LIKE ZTCUCL-ZFIDRAM,       " 수입신고금?
      ZFLCCSK     LIKE ZTRECST-ZFCAMT,       " 수입의뢰비?
      ZFBLCSK     LIKE ZTBLCST-ZFCAMT,       " B/L 비?
      ZFCRAMT     LIKE ZTCUCL-ZFCRAMT,       " 통관수수?
      ZFPLRTE     LIKE ZTIV-ZFPLRTE,         " Planned Cost Rate
*     ZFCUAMT     LIKE ZTCUCL-ZFCUAMT,       " 관?
END OF IT_CST.

DATA : W_ZFOPNDT  LIKE ZTREQST-ZFOPNDT,      " 개설?
       W_GDATU    LIKE TCURS-GDATU.          " 적용?

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_UPDATE_CNT      TYPE I,
