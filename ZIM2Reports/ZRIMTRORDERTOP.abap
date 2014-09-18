*&---------------------------------------------------------------------*
*& Include ZRIMTRORDETOP                                               *
*&---------------------------------------------------------------------*
*&  프로그램명 : 자재수송지시서(보세창고출고)  DATA DEFINE             *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.06                                            *
*&     적용회사: 한수원.
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
  TABLES: ZTTRHD,ZTTRIT, ZTBKPF, LFA1, T001W.

  DATA : MAX-LINE TYPE I VALUE 152.     " REPORT 넓이.

  DATA : W_ERR_CHK(1)      TYPE C,
         W_SELECTED_LINES  TYPE P,                 " 선택 LINE COUNT
         W_PAGE            TYPE I,                 " Page Counter
         W_LINE            TYPE I,                 " 페이지당 LINE COUNT
         LINE(3)           TYPE N,                 " 페이지당 LINE COUNT
         W_COUNT           TYPE I,                 " 전체 COUNT
         W_TABIX           LIKE SY-TABIX,
         W_ITCOUNT(3),                             " 품목 COUNT.
         W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드명.
         W_LIST_INDEX      LIKE SY-TABIX,
         W_LFA1            LIKE LFA1,
         W_TRGB(4)         TYPE  C,                " 수송구분명.
         TEMP              TYPE  F.

*>> 수송헤더내역.
 DATA : BEGIN OF ST_TAB_HD,
        ZFTRNO       LIKE   ZTTRHD-ZFTRNO,    " 수송지시번호.
        TXZ01        LIKE   ZTTRIT-TXZ01,     " 대표품명.
        W_ITM_CN     TYPE   I,                " 품목수.
        ZFTOWT       LIKE   ZTTRHD-ZFTOWT,    " 중량.
        ZFTOWTM      LIKE   ZTTRHD-ZFTOWTM,   " 단위.
        ZFGIDT       LIKE   ZTTRHD-ZFGIDT,    " 출고일.
        ZFDRDT       LIKE   ZTTRHD-ZFDRDT,    " 수송기한일.
        ZFDRMT       LIKE   ZTTRHD-ZFDRMT,    " 수송방법.
        W_DRMT(4)    TYPE   C,                " 수송방법명.
        ZFTRCO       LIKE   ZTTRHD-ZFTRCO,    " 운송업체.
        W_TRCO       LIKE   LFA1-NAME1,       " 운송업체명.
        ZFTRTERM     LIKE   ZTTRHD-ZFTRTERM,  " 작업조건.
        BUKRS        LIKE   ZTTRHD-BUKRS,     " 회사코드.
        BELNR        LIKE   ZTTRHD-BELNR,     " 비용문서번호.
        GJAHR        LIKE   ZTTRHD-GJAHR,     " 회계연도.
        AMOUNT       LIKE   ZTBKPF-WRBTR,     " 공급가액
        WMWST        LIKE   ZTBKPF-WMWST,     " 전표통화세액(부가세)
        WRBTR        LIKE   ZTBKPF-WRBTR,     " 전표통화금액(총액)
        WAERS        LIKE   ZTBKPF-WAERS,     " 전표통화.
        TRS_AMT      LIKE   ZTBSEG-WRBTR,     " 운반비.
        MAN_AMT      LIKE   ZTBSEG-WRBTR,     " 인건비.
        ETC_AMT      LIKE   ZTBSEG-WRBTR,     " 기타비용.
        ZFRMK1       LIKE   ZTTRHD-ZFRMK1,    " 비고1.
        ZFRMK2       LIKE   ZTTRHD-ZFRMK2,    " 비고2.
        ZFRMK3       LIKE   ZTTRHD-ZFRMK3,    " 비고3.
        ZFRMK4       LIKE   ZTTRHD-ZFRMK4,    " 비고4.
        ZFRMK5       LIKE   ZTTRHD-ZFRMK5.    " 비고5.
  DATA : END OF ST_TAB_HD.

  DATA : BEGIN OF IT_TAB_WERKS OCCURS 0,
         WERKS       LIKE  ZTTRIT-WERKS,      " PLANT.
         W_WERKS     LIKE  T001W-NAME1.       " 수송처명.
  DATA : END OF IT_TAB_WERKS.

  DATA: BEGIN OF IT_TAB_DTL OCCURS 0,
        ZFSEQ       LIKE  ZTTRCSTIT-ZFSEQ,
        ZFHBLNO     LIKE  ZTTRCST-ZFHBLNO,    " House B/L NO.
        ZFTRATE     LIKE  ZTTRCSTIT-ZFTRATE,  " 내역.
        ZFDTON      LIKE  ZTTRCSTIT-ZFDTON,   " 적용톤수.
        NETPR       LIKE  ZTTRCSTIT-NETPR,    " 단가.
        WAERS       LIKE  ZTTRCSTIT-WAERS,    " 통화.
        ZFTADD      LIKE  ZTTRCSTIT-ZFTADD,   " 운반비 할증.
        ZFTRAMT     LIKE  ZTTRCSTIT-ZFTRAMT,  " 운반비.
        ZFMADD      LIKE  ZTTRCSTIT-ZFMADD,   " 인건비 할증.
        ZFMAMT      LIKE  ZTTRCSTIT-ZFMAMT.   " 인건비.
  DATA: END OF IT_TAB_DTL.

  DATA : BEGIN OF IT_TAB_BSEG OCCURS 0,
         ZFCSTGRP   LIKE  ZTBSEG-ZFCSTGRP,    " 비용그룹.
         ZFCD       LIKE  ZTBSEG-ZFCD,
         ZFCDNM     LIKE  ZTIMIMG08-ZFCDNM,
         WRBTR      LIKE  ZTBSEG-WRBTR.
  DATA : END OF IT_TAB_BSEG.

  DATA : BEGIN OF IT_TAB_CST OCCURS 0,
         ZFSEQ      LIKE  ZTTRCSTIT-ZFSEQ,
         ZFHBLNO    LIKE  ZTTRCST-ZFHBLNO,
         ZFGARO     LIKE  ZTTRCST-ZFGARO,
         ZFSERO     LIKE  ZTTRCST-ZFSERO,
         ZFNOPI     LIKE  ZTTRCST-ZFNOPI,
         ZFRWET     LIKE  ZTTRCST-ZFRWET,
         ZFYTON     LIKE  ZTTRCST-ZFYTON,
         ZFWTON     LIKE  ZTTRCST-ZFWTON,
         ZFCTON     LIKE  ZTTRCST-ZFCTON.
  DATA : END OF IT_TAB_CST.

  DATA : W_TRS_TOT LIKE ZTBSEG-WRBTR,
         W_MAN_TOT LIKE ZTBSEG-WRBTR,
         W_ROW_TOT LIKE ZTBSEG-WRBTR,
         W_ETC_TOT LIKE ZTBSEG-WRBTR,
         W_GRD_TOT LIKE ZTBSEG-WRBTR,
         W_WAERS   LIKE ZTBKPF-WAERS.
