*&---------------------------------------------------------------------*
*& REPORT ZRIMMAT01                                                    *
*&---------------------------------------------------------------------*
*&  프로그램명 : 미착재고 현황                                         *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2001.01.26                                            *
*&---------------------------------------------------------------------*
*&   DESC. : 1. 수입의뢰건 별로 미착된 물품의 진행현황을 조회한다.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT ZRIMMATHIS NO STANDARD PAGE HEADING MESSAGE-ID ZIM.

*-----------------------------------------------------------------------
*  사용 TABLE DECLARE
*-----------------------------------------------------------------------
TABLES  : EKKO,                " ABAP Standard Header Table..
          EKPO,                " ABAP Standard Item Table..
          ZTREQHD,             " 수입의뢰 Header Table..
          ZTREQST,             " 수입의뢰 상태 Table..
          ZTREQIT,             " 수입의뢰 Item Table..
          ZTIV,                " Invoice Table..
          ZTIVIT,              " Invoice Item Table..
          ZTBL,                " B/L Table -ZTIVIT Table이 Item Table..
          LFA1,                " 거래처 Master Table..
          ZTCUCL,              " 통관 Table..
          ZTCUCLIV,            " 통관 Invoice Table..
          ZTCUCLIVIT,          " 통관 Invoice Item..
          ZTIDR,               " 수입신고 Table..
          ZTIDS,               " 수입면허 Table..
          EKET.                " 납기일 관련 Table..

*-----------------------------------------------------------------------
* INTERNAL TABLE & VARIABLE DECLARE
*--------------------------------------------------------------------

*------------------------------------------*
* P/O 번호 조회를 위한 INTERNAL TABLE 선언 *
*------------------------------------------*

DATA: BEGIN OF IT_PO OCCURS 1000,          " Internal Table IT_PO..
        EBELN    LIKE   EKKO-EBELN,        " P/O Header No..
        BSART    LIKE   EKKO-BSART,        " Purchasing Document Type..
        LOEKZ    LIKE   EKKO-LOEKZ,        " Deletion indicator..
        ERNAM    LIKE   EKKO-ERNAM,        " Creator..
        LIFNR    LIKE   EKKO-LIFNR,        " Vendor's Account No..
        ZTERM    LIKE   EKKO-ZTERM,        " Terms of Payment Key..
        EKORG    LIKE   EKKO-EKORG,        " Purchasing Organization..
        EKGRP    LIKE   EKKO-EKGRP,        " Purchasing Group..
        WAERS    LIKE   EKKO-WAERS,        " Current Key..
        BEDAT    LIKE   EKKO-BEDAT,        " Purchasing Document Data..
        LLIEF    LIKE   EKKO-LLIEF,        " Supplying Vensor..
        INCO1    LIKE   EKKO-INCO1,        " Incoterms (Part1)..
        LIFRE    LIKE   EKKO-LIFRE,        " Difference Invoicing Party.
        EBELP    LIKE   EKPO-EBELP,        " P/O Item No..
        TXZ01    LIKE   EKPO-TXZ01,        " Short Text..
        MATNR    LIKE   EKPO-MATNR,        " Material No..
        WERKS    LIKE   EKPO-WERKS,        " Plant..
        LGORT    LIKE   EKPO-LGORT,        " Storage Location..
        MENGE    LIKE   EKPO-MENGE,        " Purchase Order Quantity..
        MEINS    LIKE   EKPO-MEINS,        " Order Unit..
        BPRME    LIKE   EKPO-BPRME,        " Order Price Unit..
        NETPR    LIKE   EKPO-NETPR,        " Net Price..
        PEINH    LIKE   EKPO-PEINH,        " Price Unit..
        ELIKZ    LIKE   EKPO-ELIKZ,        " Delivery Comience Indicator

      END OF IT_PO.

*-----------------------------------------------*
* 수입의뢰 번호 조회를 위한 INTERNAL TABLE 선언 *
*-----------------------------------------------*

DATA: BEGIN OF IT_RN OCCURS 1000,          " REQNO조회위한IT선언.
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,  " 수입의뢰번호.
        ZFREQTY   LIKE   ZTREQHD-ZFREQTY,  " 수입의뢰 Type.
        EBELN     LIKE   ZTREQHD-EBELN,    " Purchasing Document Number.
        LIFNR     LIKE   ZTREQHD-LIFNR,    " Vendor's Account Number.
        LLIEF     LIKE   ZTREQHD-LLIEF,    " Supplying Vendor.
        ZFBENI    LIKE   ZTREQHD-ZFBENI,   " Different Invoicing Party.
        ZTERM     LIKE   ZTREQHD-ZTERM,    " Terms of Payment Key.
        INCO1     LIKE   ZTREQHD-INCO1,    " Incoterms (Part1).
        ZFLASTAM  LIKE   ZTREQHD-ZFLASTAM, " 최종개설금액.
        WAERS     LIKE   ZTREQHD-WAERS,    " Currency Key.
        ZFUSDAM   LIKE   ZTREQHD-ZFUSDAM,  " USD 환산금액.
        ZFMATGB   LIKE   ZTREQHD-ZFMATGB,  " 자재구분.
        ZFUSD     LIKE   ZTREQHD-ZFUSD,    " 미화통화.
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,  " 수입문서 품목번호.
        MATNR     LIKE   ZTREQIT-MATNR,    " Material Number.
        STAWN     LIKE   ZTREQIT-STAWN,    " Commodity Code.
        MENGE     LIKE   ZTREQIT-MENGE,    " 수입의뢰수량.
        MEINS     LIKE   ZTREQIT-MEINS,    " Base Unit of Measure.
        NETPR     LIKE   ZTREQIT-NETPR,    " Net Price.
        PEINH     LIKE   ZTREQIT-PEINH,    " Price Unit.
        BPRME     LIKE   ZTREQIT-BPRME,    " Order Price Unit.
        TXZ01     LIKE   ZTREQIT-TXZ01,    " Short Text.
        ZFAMDNO   LIKE   ZTREQST-ZFAMDNO,  " Amend Seq.
        ZFDOCST   LIKE   ZTREQST-ZFDOCST,  " 문서 상태.
        ZFRTNYN   LIKE   ZTREQST-ZFRTNYN,  " 접수 반려 여부.
        ZFRLST1   LIKE   ZTREQST-ZFRLST1,  " 의뢰 Release 상태.
        ZFRLST2   LIKE   ZTREQST-ZFRLST2,  " 개설 Release 상태.
        CDAT      LIKE   ZTREQST-CDAT,     " Created on.
        ZFREQDT   LIKE   ZTREQST-ZFREQDT,  " 요개설일자.
        ERNAM     LIKE   ZTREQST-ERNAM,    " Creater.
        EKORG     LIKE   ZTREQST-EKORG,    " Purch. Org.
        EKGRP     LIKE   ZTREQST-EKGRP,    " Purch. Grp.
        ZFOPNNO   LIKE   ZTREQST-ZFOPNNO,  " 신용장-승인번호.
        ZFOPAMT   LIKE   ZTREQST-ZFOPAMT,  " 개설 금액.
      END OF IT_RN.

*-----------------------------------------------*
* INVOICE 번호 조회를 위한 INTERNAL TABLE 선언. *
*-----------------------------------------------*

DATA: BEGIN OF IT_IV OCCURS 1000,
        ZFIVNO    LIKE   ZTIV-ZFIVNO,      " Invoice 관리번호.
*        ZFREQNO   LIKE   ZTIV-ZFREQNO,     " 수입의뢰 관리번호.
        ZFBLNO    LIKE   ZTIV-ZFBLNO,      " B/L 관리번호.
*        ZFCIVNO   LIKE   ZTIV-ZFCIVNO,     " Commercial Invoice No.
*        ZFIVPDT   LIKE   ZTIV-ZFIVPDT,     " I/V Posting Date.
*        ZFIVDDT   LIKE   ZTIV-ZFIVDDT,     " I/V Document Date.
*        ZFGRPDT   LIKE   ZTIV-ZFGRPDT,     " G/R Posting Date.
*        ZFGRDDT   LIKE   ZTIV-ZFGRDDT,     " G/R Document Date.
        ZFCUST    LIKE   ZTIV-ZFCUST,      " 통관상태.
        ZFCDST    LIKE   ZTIV-ZFCDST,      " 비용배부 상태.
*        ZFIVST    LIKE   ZTIV-ZFIVST,      " Invoice Verify 상태.
        ZFGRST    LIKE   ZTIV-ZFGRST,      " Good Receipt 상태.
*        ZFPAYYN   LIKE   ZTIV-ZFPAYYN,     " Payment 여부.
*        ZFGFDYR   LIKE   ZTIV-ZFGFDYR,     " 물대 회계전표연도.
*        ZFGFDNO   LIKE   ZTIV-ZFGFDNO,     " 물대 회계전표번호.
*        ZFCFDYR   LIKE   ZTIV-ZFCFDYR,     " 수입제비용 회계전표 연도.
*        ZFCFDNO   LIKE   ZTIV-ZFCFDNO,     " 수입제비용 회계전표 번호.
*        ZFMDYR    LIKE   ZTIV-ZFMDYR,      " 자재문서 연도.
*        ZFMDNO    LIKE   ZTIV-ZFMDNO,      " 자재문서 번호.
        ZFIVDNO   LIKE   ZTIVIT-ZFIVDNO,   " Invoice 일련번호.
        MATNR     LIKE   ZTIVIT-MATNR,     " Material Number.
*        MENGE     LIKE   ZTIVIT-MENGE,     " 수입의뢰 수량.
*        ZFPRQN    LIKE   ZTIVIT-ZFPRQN,    " Invoice 처리수?
        MEINS     LIKE   ZTIVIT-MEINS,     " Base Unit of Measure.
        NETPR     LIKE   ZTIVIT-NETPR,     " Net Price.
        PEINH     LIKE   ZTIVIT-PEINH,     " Price Unit.
        BPRME     LIKE   ZTIVIT-BPRME,     " Order Price Unit.
        TXZ01     LIKE   ZTIVIT-TXZ01,     " Short Text.
        ZFIVAMT   LIKE   ZTIVIT-ZFIVAMT,   " Invoice 금액.
*        ZFIVAMP   LIKE   ZTIVIT-ZFIVAMP,   " Invoice 처리금액.
        ZFIVAMC   LIKE   ZTIVIT-ZFIVAMC,   " Invoice 금액 통화.
        ZFIVAMK   LIKE   ZTIVIT-ZFIVAMK,   " Invoice 금액(원화).
      END OF IT_IV.

*-----------------------------------------------------------------------
* B/L 번호 조회를 위한 Internal Table Declaration.
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_BL OCCURS 1000,
        ZFBLNO    LIKE   ZTBL-ZFBLNO,      " B/L 관리 번호.
        KOSTL     LIKE   ZTBL-KOSTL,       " Cost Center.
        ZFHBLNO   LIKE   ZTBL-ZFHBLNO,     " House B/L No.
        ZFMBLNO   LIKE   ZTBL-ZFMBLNO,     " Master B/L No.
        ZFREBELN  LIKE   ZTBL-ZFREBELN,    " 대표 P/O No.
        EKORG     LIKE   ZTBL-EKORG,       " Purchasing Organization.
        EKGRP     LIKE   ZTBL-EKGRP,       " Purchasing Group.
        LIFNR     LIKE   ZTBL-LIFNR,       " Account No.
        ZFOPNNO   LIKE   ZTBL-ZFOPNNO,     " 신용장-승인번호.
        ZFETD     LIKE   ZTBL-ZFETD,       " 출항일(ETA).
        ZFETA     LIKE   ZTBL-ZFETA,       " 도착일(ETD).
        ZFRGDSR   LIKE   ZTBL-ZFRGDSR,     " 대표품명.
        ZFSPRTC   LIKE   ZTBL-ZFSPRTC,     " 선적항 코드.
        ZFSPRT    LIKE   ZTBL-ZFSPRT,      " 선적항.
        ZFAPPC    LIKE   ZTBL-ZFAPPC,      " 도착국가 코드.
        ZFAPRTC   LIKE   ZTBL-ZFAPRTC,     " 도착항 코드.
        ZFAPRT    LIKE   ZTBL-ZFAPRT,      " 도착항.
        ZFNEWT    LIKE   ZTBL-ZFNEWT,      " 순중량.
        ZFNEWTM   LIKE   ZTBL-ZFNEWTM,     " 순중량 단위.
        ZFTOWT    LIKE   ZTBL-ZFTOWT,      " 총중량.
        ZFBLAMT   LIKE   ZTBL-ZFBLAMT,     " B/L 금액.
        ZFBLAMC   LIKE   ZTBL-ZFBLAMC,     " B/L 금액 통화.
        ZFBLDT    LIKE   ZTBL-ZFBLDT,      " B/L 발행일.
        ZFBLADT   LIKE   ZTBL-ZFBLADT,     " B/L 입수일.
        ZFBLSDT   LIKE   ZTBL-ZFBLSDT,     " B/L 송부일.
      END OF IT_BL.

*-----------------------------------------------------------------------
* B/L별 Invoice 조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_TEMP OCCURS 1000,
        ZFBLNO    LIKE   ZTIV-ZFBLNO,
      END OF IT_TEMP.

*-----------------------------------------------------------------------
* 통관조회Invoice 조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_CUCLIV OCCURS 1000,
        ZFIVNO    LIKE   ZTCUCLIV-ZFIVNO,    " Invoice 관리번호.
        ZFBLNO    LIKE   ZTCUCLIV-ZFBLNO,    " B/L 관리번호.
        ZFCLSEQ   LIKE   ZTCUCLIV-ZFCLSEQ,   " 통관순번.
        ZFIVAMT   LIKE   ZTCUCLIV-ZFIVAMT,   " Invoice 금액.
        ZFIVAMC   LIKE   ZTCUCLIV-ZFIVAMC,   " Invoice 금액 통화.
        ZFIVAMK   LIKE   ZTCUCLIV-ZFIVAMK,   " Invoice 금액(원화)
        ZFRMK1    LIKE   ZTCUCLIV-ZFRMK1,    " 참조사항1.
        ZFRMK2    LIKE   ZTCUCLIV-ZFRMK2,    " 참조사항2.
        MJAHR     LIKE   ZTCUCLIV-MJAHR,     " 자재문서연도.
        MBLNR     LIKE   ZTCUCLIV-MBLNR,     " 자재문서번호.
        ZFPONC    LIKE   ZTCUCLIV-ZFPONC,    " 수입거래구분.
        ZFIVDNO   LIKE   ZTCUCLIVIT-ZFIVDNO, " Invoice Item 일련번호.
        MATNR     LIKE   ZTCUCLIVIT-MATNR,   " 자재번호.
        STAWN     LIKE   ZTCUCLIVIT-STAWN,   " 수입코드번호.
        MENGE     LIKE   ZTCUCLIVIT-MENGE,   " 수입의뢰수량.
        MEINS     LIKE   ZTCUCLIVIT-MEINS,   " 기본단위.
        NETPR     LIKE   ZTCUCLIVIT-NETPR,   " 단가.
        PEINH     LIKE   ZTCUCLIVIT-PEINH,   " 가격단위.
        BPRME     LIKE   ZTCUCLIVIT-BPRME,   " Order Price Unit.
        TXZ01     LIKE   ZTCUCLIVIT-TXZ01,   " 내역.

      END OF IT_CUCLIV.

*-----------------------------------------------------------------------
* 수입신고조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_CUCL OCCURS 1000.
      INCLUDE STRUCTURE ZTCUCL.
DATA  END OF IT_CUCL.

*-----------------------------------------------------------------------
* 수입신고조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_IDR OCCURS 1000.
      INCLUDE STRUCTURE ZTIDR.
DATA  END OF IT_IDR.

*-----------------------------------------------------------------------
* 수입면허조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_IDS OCCURS 1000.
        INCLUDE STRUCTURE ZTIDS.
DATA: END OF IT_IDS.

DATA: W_TABIX    LIKE SY-TABIX,
      W_BEWTP    LIKE EKBE-BEWTP,
      W_ERR_CHK  TYPE C,
      W_PAGE     TYPE I,
      COUNT      TYPE I.

*-----------------------------------------------------------------------
* HIDE VARIABLE.
*-----------------------------------------------------------------------

DATA: BEGIN OF DOCU,
        TYPE(2)   TYPE C,
        CODE      LIKE EKKO-EBELN,
        ITMNO     LIKE EKPO-EBELP,
        YEAR      LIKE BKPF-GJAHR,
      END OF DOCU.

*----------------------------------------------------*
* GOOD RECEIPT DATA 조회를 위한 INTERNAL TABLE 선언. *
*----------------------------------------------------*

DATA: BEGIN OF IT_GR OCCURS 0 ,
        ZFIVNO    LIKE   ZTIV-ZFIVNO,      " Invoice 관리번호.
*        ZFREQNO   LIKE   ZTIV-ZFREQNO,   " 수입의뢰 관리번호.
        ZFBLNO    LIKE   ZTIV-ZFBLNO,      " B/L 관리번호.

      END OF IT_GR.


*-----------------------------------------------------------------------
* 검색조건 WINDOW CREATE
*--------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_EBELN   FOR EKKO-EBELN,       " P/O No.
               S_MATNR   FOR EKPO-MATNR,       " 자재 No.
               S_REQNO   FOR ZTREQHD-ZFREQNO,  " 수입의뢰 No.
               S_LIFNR   FOR ZTREQHD-LIFNR,    " Vendor.
               S_MATGB   FOR ZTREQHD-ZFMATGB,  " 자재구분..
               S_REQTY   FOR ZTREQHD-ZFREQTY,  " 수입의뢰 Type
               S_EKORG   FOR ZTREQST-EKORG,    " Purch. Org.
               S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
               S_WERKS   FOR EKPO-WERKS.       " Plant
PARAMETERS :   P_NAME    LIKE USR02-BNAME.     " 담당자..

SELECT-OPTIONS:
               S_DOCST   FOR ZTREQST-ZFDOCST.  " 문서상태..

  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(14) TEXT-002, POSITION 33.
     PARAMETERS : P_YN    AS CHECKBOX.              " 생성대?
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
* TITLE BAR SETTING
*-----------------------------------------------------------------------
INITIALIZATION.
   SET TITLEBAR 'ZIMR40'.

*-----------------------------------------------------------------------
* SELECT 실행시....
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 수입의뢰 No.
   PERFORM P1000_READ_RN_DATA USING W_ERR_CHK.
   CHECK W_ERR_CHK NE 'Y'.

* Invoice No..
   PERFORM P1000_READ_IV_DATA.

* B/L Table Select..
   PERFORM P1000_READ_BL_DATA.

* 통관 Table Select..
   PERFORM P1000_READ_CUCL_DATA.

* 통관 Invoice Table Select..
   PERFORM P1000_READ_CUCLIV_DATA.

* 수입신고 Table Select..
   PERFORM P1000_READ_ZFIDR_DATA.

* 수입면허 Table Select..
   PERFORM P1000_READ_ZFIDS_DATA.

IF  P_YN = 'N%'.

* 미착품 조회시 완착품 DATA READ.
    PERFORM P1000_READ_GR_DATA.

* DATA READ 하여서 수입의뢰, BL정보, 통관정보에서 삭제.
    LOOP  AT  IT_GR.

* DATA READ 하여서 수입의뢰정보에서 수입의뢰 번호가 같은 자료 삭제.
*      READ  TABLE IT_RN
*      WITH KEY ZFREQNO = IT_GR-ZFREQNO.

      IF  SY-SUBRC  =  0 .
          W_TABIX   =  SY-TABIX.
          DELETE IT_RN INDEX W_TABIX.
      ENDIF.

      READ  TABLE IT_BL  WITH KEY ZFBLNO = IT_GR-ZFBLNO.

      IF  SY-SUBRC  =  0 .
          W_TABIX   =  SY-TABIX.
          DELETE IT_BL INDEX W_TABIX.
      ENDIF.


    ENDLOOP.

ENDIF.

* PO TABLE SELECT.
   PERFORM P1000_READ_PO_TABLE.

*-----------------------------------------------------------------------
* SELECT 실행후에....
*-----------------------------------------------------------------------
END-OF-SELECTION.

* Title Text Write.
   SET TITLEBAR 'ZIMR40'.
   SET PF-STATUS 'ZIMR40'.

* Sort P/O, Request No. Internal Table.
   PERFORM P2000_SORT_IT_DATA.

*-----------------------------------------------------------------------
* DEFINE 된 COMMAND 실행시.
*-----------------------------------------------------------------------
AT USER-COMMAND.

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
*       수입의뢰내역 & 수입의뢰 ITEM 내역을 조회한다.
*----------------------------------------------------------------------*
FORM P1000_READ_RN_DATA USING W_ERR_CHECK.

   DATA L_LINE_COUNT TYPE I.

   W_ERR_CHK = 'N'.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_RN
     FROM   ZTREQHD AS H INNER JOIN ZTREQIT AS I
     ON     H~ZFREQNO EQ I~ZFREQNO
     WHERE  H~ZFREQNO  IN  S_REQNO
     AND    H~EBELN    IN  S_EBELN
     AND    I~MATNR    IN  S_MATNR
     AND    H~LIFNR    IN  S_LIFNR
     AND    H~ZFMATGB  IN  S_MATGB
     AND    H~ZFREQTY  IN  S_REQTY
     AND    H~ZFWERKS  IN  S_WERKS.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

   CONCATENATE P_NAME '%' INTO P_NAME.

   LOOP AT IT_RN.

     W_TABIX = SY-TABIX.

     SELECT SINGLE ZFAMDNO ZFDOCST ZFRLST1 ZFRLST2 CDAT EKORG
                   EKGRP ZFOPNNO WAERS
              INTO (IT_RN-ZFAMDNO, IT_RN-ZFDOCST, IT_RN-ZFRLST1,
              IT_RN-ZFRLST2, IT_RN-CDAT, IT_RN-EKORG, IT_RN-EKGRP,
              IT_RN-ZFOPNNO, IT_RN-WAERS)
              FROM ZTREQST
              WHERE ZFREQNO EQ   IT_RN-ZFREQNO
              AND   EKORG   IN   S_EKORG
              AND   EKGRP   IN   S_EKGRP
              AND   ZFDOCST IN   S_DOCST
              AND   ERNAM   LIKE P_NAME
              AND   ZFAMDNO EQ ( SELECT MAX( ZFAMDNO ) FROM ZTREQST
                                 WHERE ZFREQNO EQ IT_RN-ZFREQNO ).
     IF SY-SUBRC = 0.
        MODIFY IT_RN INDEX W_TABIX.
     ELSE.
        DELETE IT_RN INDEX W_TABIX.
     ENDIF.

   ENDLOOP.

   DESCRIBE TABLE IT_RN LINES L_LINE_COUNT.
   IF L_LINE_COUNT = 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_RN_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_TABLE
*&---------------------------------------------------------------------*
* 수입의뢰건에 대해서만 PO TABLE SELECT.
*----------------------------------------------------------------------*
FORM P1000_READ_PO_TABLE.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_PO
     FROM   EKKO AS H INNER JOIN EKPO AS I
     ON     H~EBELN EQ I~EBELN
     FOR ALL ENTRIES IN IT_RN
     WHERE  H~EBELN EQ IT_RN-EBELN
     AND    I~EBELP EQ IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_PO_TABLE

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IV_DATA
*&---------------------------------------------------------------------*
*     조건에 해당하는 INVOICE TABLE SELECT.
*----------------------------------------------------------------------*
FORM P1000_READ_IV_DATA.

IF  P_YN = 'X'.
    P_YN = '%'.
ELSE.
    P_YN = 'N%'.
ENDIF.

*SELECT *
*     INTO CORRESPONDING FIELDS OF TABLE IT_IV
*     FROM   ZTIV AS H INNER JOIN ZTIVIT AS I
*     ON     H~ZFIVNO   EQ   I~ZFIVNO
*     WHERE  H~ZFREQNO  IN   S_REQNO
*     AND    I~MATNR    IN   S_MATNR
*     AND    H~ZFGRST   LIKE P_YN      .

ENDFORM.                    " P1000_READ_IV_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*       해당 하는 조건의 BL DATA SELECT.
*----------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BL
     FROM   ZTBL
     FOR ALL ENTRIES IN IT_IV
     WHERE  ZFBLNO EQ IT_IV-ZFBLNO.

ENDFORM.                    " P1000_READ_BL_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CUCL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_CUCL_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_CUCL FROM ZTIDS
     FOR ALL ENTRIES IN IT_CUCL
     WHERE ZFBLNO EQ IT_CUCL-ZFBLNO.

ENDFORM.                    " P1000_READ_CUCL_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CUCLIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_CUCLIV_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_CUCLIV
     FROM ZTCUCLIV AS H INNER JOIN ZTCUCLIVIT AS I
     ON     H~ZFIVNO EQ I~ZFIVNO.


ENDFORM.                    " P1000_READ_CUCLIV_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZFIDR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_ZFIDR_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_IDR FROM ZTIDR
     FOR ALL ENTRIES IN IT_IDR
     WHERE ZFBLNO EQ IT_IDR-ZFBLNO.

ENDFORM.                    " P1000_READ_ZFIDR_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZFIDS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_ZFIDS_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_IDS FROM ZTIDS
     FOR ALL ENTRIES IN IT_IDS
     WHERE ZFBLNO EQ IT_IDS-ZFBLNO.

ENDFORM.                    " P1000_READ_ZFIDS_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_IT_DATA
*&---------------------------------------------------------------------*
*       PO TABLE, 수입의뢰 TABLE 자재번호, PO NO, REQ NO 순으로.
*----------------------------------------------------------------------*
FORM P2000_SORT_IT_DATA.

   SORT IT_PO BY EBELP EBELN .
   SORT IT_RN BY ZFITMNO EBELN  ZFREQNO.

ENDFORM.                    " P2000_SORT_IT_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_GR_DATA
*&---------------------------------------------------------------------*
*       GOOD RECEIPT 된 상태의 자료 SELECT.
*----------------------------------------------------------------------*
FORM P1000_READ_GR_DATA.

*SELECT H~ZFIVNO  H~ZFREQNO  H~ZFBLNO
*     INTO CORRESPONDING FIELDS OF TABLE IT_GR
*     FROM   ZTIV AS H INNER JOIN ZTIVIT AS I
*     ON     H~ZFIVNO   EQ   I~ZFIVNO
*     WHERE  H~ZFREQNO  IN   S_REQNO
*     AND    I~MATNR    IN   S_MATNR
*     AND    H~ZFGRST   =    'Y'    .


ENDFORM.                    " P1000_READ_GR_DATA
