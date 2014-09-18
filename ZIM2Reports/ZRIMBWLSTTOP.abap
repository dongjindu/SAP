*----------------------------------------------------------------------*
*   INCLUDE ZRIMBWLSTTOP                                              *
*----------------------------------------------------------------------*
*&  프로그램명 : 보세창고 출고                                    *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.25                                            *
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
TABLES: LFA1,
        ZTBWHD,ZTBWIT,ZTBL,
        T001W,ZTBLIT,
        ZTIMIMG00.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,                 " 선택 LINE COUNT
       W_PAGE            TYPE I,                 " Page Counter
       W_LINE            TYPE I,                 " 페이지당 LINE COUNT
       LINE(3)           TYPE N,                 " 페이지당 LINE COUNT
       W_COUNT           TYPE I,                 " 전체 COUNT
       W_TABIX           LIKE SY-TABIX,
       W_ITCOUNT(3),                             " 품목 COUNT.
       W_FIRST_CHECK     TYPE I,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드명.
       MARKFIELD,
       P_BUKRS           LIKE ZTBL-BUKRS.

*-----------------------------------------------------------------------
* INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_BWHD OCCURS 0,
       ZFIVNO   LIKE ZTBWHD-ZFIVNO,
       ZFGISEQ  LIKE ZTBWHD-ZFGISEQ,
       WAERS    LIKE ZTBWHD-WAERS,
       ZFBLNO   LIKE ZTBWHD-ZFBLNO,
       ZFHBLNO  LIKE ZTBL-ZFHBLNO,
       ZFCLSEQ  LIKE ZTBWHD-ZFCLSEQ,
       ZFREBELN LIKE ZTBWHD-ZFREBELN,
       W_EBELN(12),
       WERKS    LIKE ZTBWHD-WERKS,
       ZFSHNO   LIKE ZTBWHD-ZFSHNO,
       ZFIDSDT  LIKE ZTBWHD-ZFIDSDT,
       ZFTRCO   LIKE ZTBWHD-ZFTRCO,
       ZFSENDER LIKE ZTBWHD-ZFSENDER,
       ZFGIDT   LIKE ZTBWHD-ZFGIDT,
       ZFCARNO  LIKE ZTBWHD-ZFCARNO,
       ZFDRVNM  LIKE ZTBWHD-ZFDRVNM,
       ZFRMK1   LIKE ZTBWHD-ZFRMK1,
       ZFRMK2   LIKE ZTBWHD-ZFRMK2,
       ZFRMK3   LIKE ZTBWHD-ZFRMK3,
       ZFTOWT   LIKE ZTBWHD-ZFTOWT,
       ZFTOWTM  LIKE ZTBWHD-ZFTOWTM,
       ZFTOVL   LIKE ZTBWHD-ZFTOVL,
       ZFTOVLM  LIKE ZTBWHD-ZFTOVLM,
       NAME1    LIKE LFA1-NAME1.
DATA : END OF IT_BWHD.

DATA : BEGIN OF IT_BWIT OCCURS 0,
       ZFIVNO	  LIKE ZTBWIT-ZFIVNO,  " 통관요청/입고요청 관리번호
       ZFIVDNO  LIKE ZTBWIT-ZFIVDNO, " Invoice Item 일련번호
       ZFGISEQ  LIKE ZTBWIT-ZFGISEQ, " 보세창고출고 순번
       EBELN    LIKE ZTBWIT-EBELN,	   " 구매문서번호
       W_EBELN(15),
       ZFSHNO   LIKE ZTBWIT-ZFSHNO,  " 선적차수
       ZFREQNO  LIKE ZTBWIT-ZFREQNO, " 수입의뢰 관리번호
       ZFITMNO  LIKE ZTBWIT-ZFITMNO, " 수입문서 품목번호
       ZFBLNO	  LIKE ZTBWIT-ZFBLNO,  " B/L 관리번호
       ZFBLIT   LIKE ZTBWIT-ZFBLIT,  " B/L 품목번호
       ZFCGNO	  LIKE ZTBWIT-ZFCGNO,  " 하역관리번호
       ZFCGIT	  LIKE ZTBWIT-ZFCGIT,  " 하역자재순번
       MATNR	  LIKE ZTBWIT-MATNR,   " 자재번호
       GIMENGE  LIKE ZTBWIT-GIMENGE, " 보세창고 출고수량
       NETPR    LIKE ZTBWIT-NETPR,
       PEINH    LIKE ZTBWIT-PEINH,   " 가격단위
       BPRME    LIKE ZTBWIT-BPRME,   " 오더단위.
       MEINS	  LIKE ZTBWIT-MEINS,   " 기본단위
       TXZ01	  LIKE ZTBWIT-TXZ01.   " 내역
DATA : END OF IT_BWIT.

DATA: BEGIN OF IT_SELECTED OCCURS 0,
        ZFIVNO   LIKE ZTBWHD-ZFIVNO,
        ZFGISEQ  LIKE ZTBWHD-ZFGISEQ,
        ZFBLNO   LIKE ZTBWHD-ZFBLNO,
       ZFCLSEQ  LIKE ZTBWHD-ZFCLSEQ,
END OF IT_SELECTED.
