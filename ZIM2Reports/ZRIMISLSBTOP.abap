*----------------------------------------------------------------------*
*   INCLUDE ZRIMISLSBTOP                                               *
*----------------------------------------------------------------------*
*&  프로그램명 : BL기준 부보 관련 TOP                                  *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.09.04                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :BL기준 부보 관련 레포트를 위한 Include.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------
* 테이블.
*-----------------------------------------------------------------------
TABLES : ZTINSB,                       " 보험부보.
         ZTINSBSG2,                    " SEG2
         ZTINSBAGR,                    " AGR
         ZTINSBRSP,                    " 보험부보 Response
         ZTBL,                         " B/L
         ZTREQHD,                      " 수입의뢰 TABLE.
         ZTBLCST,                      " B/L 비용.
         ZTBLIT.                       " B/L ITEM.


*-----------------------------------------------------------------------
* SELECT RECORD 용.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
       ZFBLNO     LIKE ZTINSB-ZFBLNO,          " B/L 관리번호.
       ZFINSEQ    LIKE ZTINSB-ZFINSEQ,
       ZFINNO     LIKE ZTINSB-ZFINNO,
       ZFKRWAMT   LIKE ZTINSB-ZFKRWAMT,
       WERKS      LIKE ZTBL-ZFWERKS,
       BUPLA      LIKE ZTBKPF-BUPLA,
       BUKRS      LIKE ZTINSB-BUKRS,
       BELNR      LIKE ZTINSB-BELNR,
       ZFOPCD     LIKE ZTINSB-ZFOPCD,
       GJAHR      LIKE ZTINSB-GJAHR.
DATA: END OF IT_SELECTED.

DATA : BEGIN OF IT_LIFNR OCCURS 0,
       ZFOPCD     LIKE ZTINSB-ZFOPCD.
DATA : END   OF IT_LIFNR.

DATA : W_ERR_CHK(1)      TYPE C,                  " ERROR CHECK.
       W_SELECTED_LINES  TYPE P,                  " 선택 LINE COUNT
       W_PAGE            TYPE I,                  " Page Counter
       W_LINE            TYPE I,                  " 페이지당 LINE COUNT
       LINE(3)           TYPE N,                  " 페이지당 LINE COUNT
       W_COUNT           TYPE I,                  " 전체 COUNT
       L_COUNT           TYPE I,                  " 페지지당 LINE COUNT
       W_LCOUNT          TYPE I,                  " 보험사 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,           " TABLE INDEX
       W_FIELD_NM        LIKE DD03D-FIELDNAME,    " 필드명.
       W_TABIX           LIKE SY-TABIX,           " TABLE INDEX
       W_EKNAM           LIKE T024-EKNAM,         " 담당자.
       W_WERKSNM         LIKE T001W-NAME1,        " 플랜트명.
       W_TRANS           LIKE ZTINSB-ZFTRANS,     " 운송방법.
       W_MATNM(35)       TYPE C,                  " 자재명.
       W_MIN_LSG2        LIKE ZTINSBSG2-ZFLSG2,   " 최소 반복수.
       W_MIN_LAGR        LIKE ZTINSBAGR-ZFLAGR,   " 최대 반복수.
       OLD_ZFOPCD        LIKE ZTINSB-ZFOPCD,      " 보험회사CODE.
       OLD_ZFKRW         LIKE ZTINSB-ZFKRW,       " 소계금액통화.
       SUB_TOTALK        LIKE ZTINSB-ZFKRWAMT,    " 소계금액원화.
       W_ZFCSTGRP        LIKE ZTBKPF-ZFCSTGRP,    " 비용그룹코드.
       P_BUKRS           LIKE ZTBL-BUKRS.
