*----------------------------------------------------------------------*
*   INCLUDE ZRIMISLSTTOP                                               *
*----------------------------------------------------------------------*
*&  프로그램명 : 보험사별 보험료 조회                                  *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.02.15                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :보험사별 부보 현황조회를 위한 Include.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------
* 테이블.
*-----------------------------------------------------------------------
TABLES : ZTINS,                       " 보험부보.
         ZTINSSG2,                    " SEG2
         ZTINSAGR,                    " AGR
         ZTINSRSP,                    " 보험부보 Response
         ZTREQST,                     " 수입의뢰 상태(Status)
         ZTRECST,                     " 수입의뢰 비용.
         ZTREQHD.                     " 수입의뢰 헤더.

*-----------------------------------------------------------------------
* SELECT RECORD 용.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
       ZFREQNO    LIKE ZTINS-ZFREQNO,          " 수입의뢰 관리번호.
       ZFINSEQ    LIKE ZTINS-ZFINSEQ,
       INSAMDNO   LIKE ZTINS-ZFAMDNO,
       ZFAMDNO    LIKE ZTINS-ZFAMDNO,          " Amend Seq.
       ZFINNO     LIKE ZTINS-ZFINNO,
       ZFKRWAMT   LIKE ZTINS-ZFKRWAMT,
       WERKS      LIKE ZTREQHD-ZFWERKS,
       BUPLA      LIKE ZTBKPF-BUPLA,
       BUKRS      LIKE ZTINS-BUKRS,
       BELNR      LIKE ZTINS-BELNR,
       ZFOPCD     LIKE ZTINS-ZFOPCD,
       GJAHR      LIKE ZTINS-GJAHR.
DATA: END OF IT_SELECTED.


DATA : BEGIN OF IT_LIFNR OCCURS 0,
       ZFOPCD     LIKE ZTINS-ZFOPCD.
DATA : END   OF IT_LIFNR.


DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,                 " 선택 LINE COUNT
       W_PAGE            TYPE I,                 " Page Counter
       W_LINE            TYPE I,                 " 페이지당 LINE COUNT
       LINE(3)           TYPE N,                 " 페이지당 LINE COUNT
       W_COUNT           TYPE I,                 " 전체 COUNT
       L_COUNT           TYPE I,                 " 페지지당 LINE COUNT
       W_LCOUNT          TYPE I,                 " 보험사 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드명.
       W_TABIX           LIKE SY-TABIX,          " TABLE INDEX
       W_EKNAM           LIKE T024-EKNAM,
       W_WERKSNM         LIKE T001W-NAME1,
       W_TRANS           LIKE ZTREQHD-ZFTRANS,
       W_MATNM(35)       TYPE C,
       W_MIN_LSG2        LIKE ZTINSSG2-ZFLSG2,   " 최소 반복수.
       W_MIN_LAGR        LIKE ZTINSAGR-ZFLAGR,   " 최대 반복수.
       W_MAX_ZFAMDNO     LIKE ZTINS-ZFAMDNO,     " AEND 회수.
       W_MAX_ZFAMDNO_OLD LIKE ZTINS-ZFAMDNO,     " AEND 회수.
       OLD_ZFOPCD        LIKE ZTINS-ZFOPCD,     " 보험회사CODE.
       OLD_ZFKRW         LIKE ZTINS-ZFKRW,       " 소계금액통화.
       SUB_TOTALK        LIKE ZTINS-ZFKRWAMT,    " 소계금액원화.
       P_BUKRS           LIKE ZTINS-BUKRS.
