*----------------------------------------------------------------------*
*   INCLUDE ZRIMFLCLSTTOP                                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES 및 변수 DEFINE.                                               *
*----------------------------------------------------------------------*

*>>> TABLE DEFINE.
TABLES : ZTREQHD,                  "수입의뢰 해더.
         ZTREQIT,                  "수입의뢰 ITEM.
         ZTREQST,                  "수입의뢰 상태.
         T001W,                    "STANDARD-Plant
         EKPO.                     "STANDARD.

*>>> 변수 DEFINE.
DATA   : W_ERR_CHK TYPE C VALUE 'N',   "ERROR CHECK.
         W_LINE  TYPE I,               "IT_TAB LINE 수.
         W_LINE1 TYPE I,              "소계용 LINE 수.
         W_SUM   LIKE ZTREQHD-ZFLASTAM, "원화 소계용.
         W_TOTAL LIKE ZTREQHD-ZFLASTAM, "원화 전사용.
         W_WERKS LIKE EKPO-WERKS,     "사업장.
         W_PRI LIKE ZTREQIT-NETPR,    "단가.
         W_FOR LIKE ZTREQHD-ZFLASTAM, "외화.
         W_WON LIKE ZTREQHD-ZFLASTAM, "외화.
         W_MOD LIKE SY-TABIX,        "홀짝.
         W_TABIX LIKE SY-TABIX.

*>>> IT_TAB DEFINE.
DATA : BEGIN OF IT_TAB OCCURS 0,
         WERKS    LIKE EKPO-WERKS,      "EKPO 	플랜트
         EBELN    LIKE ZTREQHD-EBELN,   "EKKO 	P/O 주문번호
*>>KEY
         ZFREQNO  LIKE ZTREQHD-ZFREQNO,   "수입의뢰 관리번호
*>>필요FIELD 도출용.
         ZFOPNDT  LIKE ZTREQST-ZFOPNDT,   "개설일-외화,원화.
         KURSF    LIKE ZTREQHD-KURSF,	 "환율
         FFACT    LIKE ZTREQHD-FFACT,	 "원시 통화단위환율
         BPUMZ    LIKE EKPO-BPUMZ,
                      "오더가격단위를 오더단위로 환산하는 분자
         BPUMN    LIKE EKPO-BPUMN,        " 분모
         BPRME    LIKE ZTREQIT-BPRME,     "Order price unit
         MENGE    LIKE ZTREQIT-MENGE,     "수입의뢰 수량
         MEINS    LIKE ZTREQIT-MEINS,	 "T006	기본단위
         NETPR    LIKE ZTREQIT-NETPR,	 "단가
         PEINH    LIKE ZTREQIT-PEINH,	 "가격단위
         TXZ01    LIKE ZTREQIT-TXZ01,	 "내역(품명)
         LIFNR    LIKE ZTREQHD-LIFNR,     "구매처계정번
*>>LIST
         ZFDOCST  LIKE ZTREQST-ZFDOCST,   "문서 상태 'O'
         IMTRD    LIKE ZTREQHD-IMTRD,	 "수입자구분 'F'.
         LLIEF    LIKE ZTREQHD-LLIEF,	 "공급업체(OFFER CODE)
         INCO1    LIKE ZTREQHD-INCO1,	 "TINC인도조건(가격조건)
         WAERS    LIKE ZTREQHD-WAERS,	 "TCURC 통화키 (화패)
         MATNR    LIKE ZTREQHD-MATNR,	 "자재번호
         MAKTX    LIKE ZTREQHD-MAKTX,	 "자재내역
         EBELP    LIKE ZTREQIT-EBELP,	 "구매문서 품목번호
*>>MAKE FIELD-단가,외화,원화 계산용.
         EX_PRI   LIKE ZTREQIT-NETPR,     "단가용.
         EX_FOR   LIKE ZTREQHD-ZFLASTAM,  "외화용.
         EX_WON   LIKE ZTREQHD-ZFLASTAM.  "원화용.

DATA : END OF IT_TAB.


*>>> 소계용 IT_TAB DEFINE.
DATA : BEGIN OF IT_SUMMARY OCCURS 10,
         WAERS    LIKE    ZTREQHD-WAERS,
         EX_FOR   LIKE    ZTREQHD-ZFLASTAM,
         EX_WON   LIKE    ZTREQHD-ZFLASTAM.
DATA : END OF IT_SUMMARY.

*>>> 전사용 IT_TAB DEFINE.
DATA : BEGIN OF IT_TOTAL OCCURS 10,
         WAERS    LIKE    ZTREQHD-WAERS,
         EX_FOR   LIKE    ZTREQHD-ZFLASTAM,
         EX_WON   LIKE    ZTREQHD-ZFLASTAM.
DATA : END OF IT_TOTAL.

*>>> IT_TEMP DEFINE.
DATA : BEGIN OF IT_TEMP OCCURS 0,
*>>KEY
         ZFREQNO   LIKE ZTREQHD-ZFREQNO,  "수입의뢰 관리번호
*LIST
         ZFOPNDT   LIKE ZTREQST-ZFOPNDT,  "개설일-외화,원화.
         KURSF     LIKE ZTREQHD-KURSF,	 "환율
         FFACT     LIKE ZTREQHD-FFACT,	 "원시 통화단위환율
         ZFDOCST   LIKE ZTREQST-ZFDOCST,  "문서 상태 'O'
         IMTRD     LIKE ZTREQHD-IMTRD,	 "수입자구분 'F'.
         EBELN     LIKE ZTREQHD-EBELN,	 "구매(P/O 주문번호)
         LLIEF     LIKE ZTREQHD-LLIEF,	 "공급업체(OFFER CODE)
         INCO1     LIKE ZTREQHD-INCO1,	 "인도조건 (가격조건)
         WAERS     LIKE ZTREQHD-WAERS,	 "TCURC 통화키 (화패)
         MATNR     LIKE ZTREQHD-MATNR,	 "자재번호
         LIFNR    LIKE ZTREQHD-LIFNR,     "구매처계정번
         MAKTX     LIKE ZTREQHD-MAKTX.    "자재내역
DATA: END OF IT_TEMP.

*>>> IT_ITEM DEFINE.
DATA: BEGIN OF IT_ITEM OCCURS 0,
*>>KEY
        ZFREQNO   LIKE ZTREQHD-ZFREQNO,   "수입의뢰 관리번호
*>>LIST
        MENGE	    LIKE ZTREQIT-MENGE,      "수입의뢰 수량
        MEINS     LIKE ZTREQIT-MEINS,	"T006	기본단위
        NETPR     LIKE ZTREQIT-NETPR,	"단가
        PEINH     LIKE ZTREQIT-PEINH,	"가격단위
        TXZ01	    LIKE ZTREQIT-TXZ01,	"내역(품명)
        EBELP	    LIKE ZTREQIT-EBELP.	"구매문서EBELN와 EKPO-WERK
DATA: END OF IT_ITEM.
