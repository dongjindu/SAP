*&---------------------------------------------------------------------*
*& Report ZRIMMATHIS                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입자재별 현황관리 PROGRAM                           *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2001.04.06                                            *
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*& 2001.07.03  나현주 ( 분할 입고 가능으로 인한 다중 입고자료 DISPLAY )
*&---------------------------------------------------------------------*
REPORT ZRIMMATHIS NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                     LINE-SIZE 143.

TABLES: EKKO,                " ABAP Standard Header Table..
        EKPO,                " ABAP Standard Item Table..
        ZTREQHD,             " 수입의뢰 Header Table..
        ZTREQIT,             " 수입의뢰 Item Table..
        ZTBL,                " B/L Table..
        ZTBLIT,              " B/L  Item Table..
        LFA1,                " 거래처 Master Table..
        ZTMSHD,              " 모선관리 Header Table..
        ZTCGHD,              " 하역 Header Table..
        ZTCGIT,              " 하역 자재 Table..
        ZTREQORJ,            " 수입의뢰 원산지 내역 Table..
        ZTIMIMG03,           " 보세구역 코드 Table..
        ZTIMIMG10,           " 관세사 관리..
        T005T,               " 국가이름 Table..
        T001L,               " 저장위치 Table..
        T001W,               " 플랜트/분기 Table..
        ZTCIVHD,             " Commercial Invoice Header..
        ZTCIVIT,             " Commercial Invoice Items..
        ZTIV,                " 통관요청/입고요청 Header..
        ZTIVIT,              " 통관요청/입고요청 Item Table..
        ZTCUCL,              " 통관 Table..
        ZTCUCLIV,            " 통관 Invoice Table..
        ZTCUCLIVIT,          " 통관 Invoice Item Table..
        ZTIDS,               " 수입면허 Table..
        ZTIDSHSD,
        ZTIDR,               " 수입신고 Table..
        ZTIVHSTIT.           " 입고요청 ITEM.

*------------------------------------------*
* P/O 번호 조회를 위한 INTERNAL TABLE 선언 *
*------------------------------------------*
DATA: BEGIN OF IT_PO OCCURS 1000,
        EBELN    LIKE   EKKO-EBELN,      " P/O Header No..
        LIFNR    LIKE   EKKO-LIFNR,      " Vendor's Account No..
        AEDAT    LIKE   EKKO-AEDAT,      " 레코드생성일..
        WAERS    LIKE   EKKO-WAERS,      " 통화키..
        EBELP    LIKE   EKPO-EBELP,      " P/O Item No..
        MATNR    LIKE   EKPO-MATNR,      " 자재번호..
        BUKRS    LIKE   EKPO-BUKRS,      " 회사코드..
        WERKS    LIKE   EKPO-WERKS,      " 플랜트..
        TXZ01    LIKE   EKPO-TXZ01,      " 내역..
        MENGE    LIKE   EKPO-MENGE,      " 구매오더수량..
        MEINS    LIKE   EKPO-MEINS,      " 오더단위..
        NETPR    LIKE   EKPO-NETPR,      " 구매문서의 단가 (문서통화).
        PEINH    LIKE   EKPO-PEINH,      " 가격단위..
        NAME1    LIKE   LFA1-NAME1,                         " 이름1..
      END OF IT_PO.

*-----------------------------------------------*
* 수입의뢰 번호 조회를 위한 INTERNAL TABLE 선언 *
*-----------------------------------------------*

DATA: BEGIN OF IT_RN OCCURS 1000,
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,  " 수입의뢰번호.
        ZFREQTY   LIKE   ZTREQHD-ZFREQTY,  " 수입의뢰 TYPE
        EBELN     LIKE   ZTREQHD-EBELN,    " Purchasing Document Number.
        EBELP     LIKE   ZTREQIT-EBELP,    " P/O Item Number.
        LIFNR     LIKE   ZTREQHD-LIFNR,    " Vendor's Account Number.
        WAERS     LIKE   ZTREQHD-WAERS,    " Currency Key.
        ZFOPNNO   LIKE   ZTREQHD-ZFOPNNO,  " 신용장-승인번호.
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,  " 수입문서 품목번호.
        MATNR     LIKE   ZTREQIT-MATNR,    " Material Number.
        MENGE     LIKE   ZTREQIT-MENGE,    " 수입의뢰수량.
        MEINS     LIKE   ZTREQIT-MEINS,    " Base Unit of Measure.
        NETPR     LIKE   ZTREQIT-NETPR,    " Net Price.
        PEINH     LIKE   ZTREQIT-PEINH,    " Price Unit.
        BPRME     LIKE   ZTREQIT-BPRME,    " Order Price Unit.
        ZFORIG    LIKE   ZTREQORJ-ZFORIG,  " 자재원산국..
        LANDX     LIKE   T005T-LANDX,      " 국가이름..
        END OF IT_RN.

*-----------------------------------------------------------------------
* B/L 번호 조회를 위한 Internal Table Declaration.
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_BL OCCURS 1000,
        ZFBLNO    LIKE   ZTBL-ZFBLNO,      " B/L 관리 번호..
        ZFMSNO    LIKE   ZTBL-ZFMSNO,      " 모선관리번호..
        ZFFORD    LIKE   ZTBL-ZFFORD,      " Forwarder..
        ZFAPRTC   LIKE   ZTBL-ZFAPRTC,     " 도착항 코드..
        ZFAPRT    LIKE   ZTBL-ZFAPRT,      " 도착항..
        ZFHBLNO   LIKE   ZTBL-ZFHBLNO,     " House B/L No..
        ZFREBELN  LIKE   ZTBL-ZFREBELN,    " 대표 P/O No..
        LIFNR     LIKE   ZTBL-LIFNR,       " Account No..
        ZFOPNNO   LIKE   ZTBL-ZFOPNNO,     " 신용장-승인번호.
        ZFETA     LIKE   ZTBL-ZFETA,       " 도착일(ETD)..
        ZFPOYN    LIKE   ZTBL-ZFPOYN,      " 유환여부..
        ZFRENT    LIKE   ZTBL-ZFRENT,      " 양도 B/L 여부..
        ZFBLIT    LIKE   ZTBLIT-ZFBLIT,    " B/L 품목번호..
        EBELN     LIKE   ZTBLIT-EBELN,     " 구매문서번호..
        EBELP     LIKE   ZTBLIT-EBELP,     " 구매문서 품목번호..
        ZFREQNO   LIKE   ZTBLIT-ZFREQNO,   " 수입의뢰 관리번호..
        ZFITMNO   LIKE   ZTBLIT-ZFITMNO,   " 수입문서 품목번호..
        MATNR     LIKE   ZTBLIT-MATNR,     " 자재번호..
        BLMENGE   LIKE   ZTBLIT-BLMENGE,   " B/L 수량..
        MEINS     LIKE   ZTBLIT-MEINS,     " 기본단위..
      END OF IT_BL.

*-----------------------------------------------------------------------
* 모선관리를 위한 Internal Table 선언..
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_MS OCCURS 1000,
        ZFMSNO    LIKE   ZTMSHD-ZFMSNO,    " 모선관리번호..
        ZFMSNM    LIKE   ZTMSHD-ZFMSNM,    " 모선명..
        ZFREQNO   LIKE   ZTMSIT-ZFREQNO,   " 수입의뢰관리번호..
        ZFSHSDF   LIKE   ZTMSHD-ZFSHSDF,   " 선적일(From)..
        ZFSHSDT   LIKE   ZTMSHD-ZFSHSDT,   " 선적일(To)..
      END OF IT_MS.

*-----------------------------------------------------------------------
*  Declaration of Internal Table for Commercial Invoice Data Reference.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CIV OCCURS 1000,
        ZFCIVRN   LIKE ZTCIVHD-ZFCIVRN,  " Commercial Invoice 관리번호..
        ZFCIVNO   LIKE ZTCIVHD-ZFCIVNO,  " Commercial Invoice Number..
        ZFMAVN    LIKE ZTCIVHD-ZFMAVN,   " 물대 거래처코드..
        ZFOPBN    LIKE ZTCIVHD-ZFOPBN,   " 개설은행 거래처코드..
        ZFCIVSQ   LIKE ZTCIVIT-ZFCIVSQ,  " Commercial Invoice 품목번호..
        EBELN     LIKE ZTCIVIT-EBELN,    " 구매문서번호..
        EBELP     LIKE ZTCIVIT-EBELP,    " 구매문서 품목번호..
        ZFREQNO   LIKE ZTCIVIT-ZFREQNO,  " 수입의뢰 관리번호..
        ZFITMNO   LIKE ZTCIVIT-ZFITMNO,  " 수입문서 품목번호..
        ZFBLNO    LIKE ZTCIVIT-ZFBLNO,   " B/L 관리번호..
        ZFBLIT    LIKE ZTCIVIT-ZFBLIT,   " B/L 품목번호..
        CMENGE    LIKE ZTCIVIT-CMENGE,   " Commercial Invoice 수량..
        MEINS     LIKE ZTCIVIT-MEINS,    " 기본단위..
      END OF IT_CIV.

*-----------------------------------------------------------------------
* 하역항 조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_CG OCCURS 1000,
        ZFCGNO    LIKE   ZTCGHD-ZFCGNO,    " 하역관리번호..
        ZFMSNO    LIKE   ZTCGHD-ZFMSNO,    " 모선관리번호..
        ZFETA     LIKE   ZTCGHD-ZFETA,     " 도착일(ETA)..
        ZFCGPT    LIKE   ZTCGHD-ZFCGPT,    " 하역항..
        ZFCGIT    LIKE   ZTCGIT-ZFCGIT,    " 하역자재순번..
        EBELN     LIKE   ZTCGIT-EBELN,     " 구매문서번호..
        EBELP     LIKE   ZTCGIT-EBELP,     " 구매문서 품목번호..
        ZFREQNO   LIKE   ZTCGIT-ZFREQNO,   " 수입의뢰 관리번호..
        ZFITMNO   LIKE   ZTCGIT-ZFITMNO,   " 수입문서 품목번호..
        ZFBLNO    LIKE   ZTCGIT-ZFBLNO,    " B/L 관리번호..
        ZFBLIT    LIKE   ZTCGIT-ZFBLIT,    " B/L 품목번호..
        MATNR     LIKE   ZTCGIT-MATNR,     " 자재번호..
        CGMENGE   LIKE   ZTCGIT-CGMENGE,   " 하역자재수량..
        MEINS     LIKE   ZTCGIT-MEINS,     " 기본단위..
        ZFBNARCD  LIKE   ZTCGIT-ZFBNARCD,  " 보세구역 내부관리코드..
      END OF IT_CG.

*-----------------------------------------------------------------------
* 거래처마스터 테이블 조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_LFA OCCURS 1000,
         LIFNR    LIKE   LFA1-LIFNR,
         NAME1    LIKE   LFA1-NAME1,
      END OF IT_LFA.

*-----------------------------------------------------------------------
* 보세구역코드 테이블 조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IMG03 OCCURS 1000.
        INCLUDE STRUCTURE ZTIMIMG03.
DATA  END OF IT_IMG03.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for Customer Clearence Data Reference..
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
* Declaration of Internal Talbe for Customs Clearance Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CUCL OCCURS 1000.
        INCLUDE STRUCTURE ZTCUCLIV.
DATA  END OF IT_CUCL.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for ZTIDR Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDR OCCURS 1000.
        INCLUDE STRUCTURE ZTIDR.
*DATA  zfivno  LIKE  ztidrhsd-zfivno.
DATA  ZFIVDNO LIKE  ZTIDRHSD-ZFIVDNO.
DATA  ZFQNT   LIKE  ZTIDRHSD-ZFQNT.
DATA  ZFQNTM  LIKE  ZTIDRHSD-ZFQNTM.
DATA  END OF IT_IDR.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for ZTIDS Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDS OCCURS 1000.
        INCLUDE STRUCTURE ZTIDS.
DATA  ZFQNT  LIKE  ZTIDSHSD-ZFQNT.
DATA  ZFQNTM LIKE  ZTIDSHSD-ZFQNTM.
DATA: END OF IT_IDS.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for T001L Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_T001L OCCURS 1000.
        INCLUDE STRUCTURE T001L.
DATA: END OF IT_T001L.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for T001W Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_T001W OCCURS 1000.
        INCLUDE STRUCTURE T001W.
DATA: END OF IT_T001W.

*-------------------------------------------------*
* 입고 DATA를 READ 하기 위한 INTERNAL TABLE 선언. *
*-------------------------------------------------*
DATA : BEGIN OF IT_IN OCCURS 1000,
       ZFIVNO    LIKE   ZTIVHSTIT-ZFIVNO,
       ZFIVHST   LIKE   ZTIVHSTIT-ZFIVHST,
       ZFGRST    LIKE   ZTIVHSTIT-ZFGRST,
       ZFIVDNO   LIKE   ZTIVHSTIT-ZFIVDNO,
       MATNR     LIKE   ZTIVHSTIT-MATNR,
       GRMENGE   LIKE   ZTIVHSTIT-GRMENGE,
       MEINS     LIKE   ZTIVHSTIT-MEINS,
       WERKS     LIKE   ZTIVHSTIT-WERKS,
       LGORT     LIKE   ZTIVHSTIT-LGORT,
       MBLNR     LIKE   ZTIVHST-MBLNR,
       MJAHR     LIKE   ZTIVHST-MJAHR,
       EBELN     LIKE   EKPO-EBELN,
       EBELP     LIKE   EKPO-EBELP,
       BUKRS     LIKE   ZTIV-BUKRS.
DATA : END OF IT_IN.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for ZTIVIT Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IVIT OCCURS 1000.
        INCLUDE STRUCTURE ZTIVIT.
DATA:   ZFCUST    LIKE   ZTIV-ZFCUST,
        ZFGRST    LIKE   ZTIV-ZFGRST.
DATA: END OF IT_IVIT.

*-----------------------------------------------------------------------
* Declaration of Variable for Table Usage..
*-----------------------------------------------------------------------
DATA: W_TABIX     LIKE SY-TABIX,
      W_BEWTP     LIKE EKBE-BEWTP,
      W_ERR_CHK   TYPE C,
      W_TRIPLE(5) TYPE C,
      W_PAGE      TYPE I.

*-----------------------------------------------------------------------
* Declaration of Variable for Instead of ON CHANGE OF Function..
*-----------------------------------------------------------------------
DATA: W_LOOP_CNT  TYPE I,
      TEMP_MATNR  LIKE EKPO-MATNR,         " 자재번호를 임시로 저장..
      TEMP_TXZ01  LIKE EKPO-TXZ01,         " 자재내역을 임시로 저장..
      TEMP_EBELN  LIKE EKPO-EBELN,         " P/O 번호를 임시로 저장..
      TEMP_EBELP  LIKE EKPO-EBELP,         " Item 번호를 임시로 저장..
      TEMP_REQNO  LIKE ZTREQHD-ZFREQNO,    " 수입의뢰관리번호 저장..
      TEMP_ITMNO  LIKE ZTREQIT-ZFITMNO,    " Item 번호를 임시로 저장..
      TEMP_BLNO   LIKE ZTBL-ZFBLNO,        " B/L 번호를 임시로 저장..
      TEMP_BLIT   LIKE ZTBLIT-ZFBLIT,      " B/L Item 번호 임시저장..
      TEMP_CGNO   LIKE ZTCGHD-ZFCGNO,      " 하역관리번호를 임시저장..
      TEMP_CGIT   LIKE ZTCGIT-ZFCGIT,      " 하역자재순번을 임시저장..
      TEMP_IVNO   LIKE ZTIVIT-ZFIVNO,      " 통관/입고요청관리번호 저장.
      TEMP_IVDNO  LIKE ZTIVIT-ZFIVDNO.    " Invoice Item 일련번호 저장..

*-----------------------------------------------------------------------
* HIDE VARIABLE.
*-----------------------------------------------------------------------
DATA: BEGIN OF DOCU,
        TYPE(2)   TYPE C,
        CODE      LIKE EKKO-EBELN,
        ITMNO     LIKE EKPO-EBELP,
        YEAR      LIKE BKPF-GJAHR,
      END OF DOCU.

*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_BUKRS   FOR ZTREQHD-BUKRS,
               S_EBELN   FOR EKKO-EBELN,       " P/O No.
               S_MATNR   FOR EKPO-MATNR,       " 자재 No.
               S_REQNO   FOR ZTREQHD-ZFREQNO,  " 수입의뢰 No.
               S_OPNNO   FOR ZTREQHD-ZFOPNNO,  " 신용장-승인번호.
               S_LIFNR   FOR ZTREQHD-LIFNR,    " Vendor.
               S_REQTY   FOR ZTREQHD-ZFREQTY,
               S_TRANS   FOR ZTREQHD-ZFTRANS,
               S_REQSD   FOR ZTREQHD-ZFREQSD,
               S_REQED   FOR ZTREQHD-ZFREQED,
               S_SHCU    FOR ZTREQHD-ZFSHCU.
* PARAMETERS :   P_TRIPLE  AS CHECKBOX.          " 삼국무역여?
SELECTION-SCREEN END OF BLOCK B2.

*.. 삼국무역 여?
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-R01.
  SELECTION-SCREEN : BEGIN OF LINE,
                     COMMENT 01(31) TEXT-R01.
    SELECTION-SCREEN : COMMENT 33(4) TEXT-R04.
    PARAMETERS : P_ALL RADIOBUTTON GROUP RDG.
    SELECTION-SCREEN : COMMENT 52(3) TEXT-R02.
    PARAMETERS : P_YES RADIOBUTTON GROUP RDG.
    SELECTION-SCREEN : COMMENT 70(3) TEXT-R03.
    PARAMETERS : P_NO  RADIOBUTTON GROUP RDG.
  SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.
  SET TITLEBAR 'TIT1'.
  MOVE 'X' TO P_ALL.
*-----------------------------------------------------------------------
* TOP-OF-PAGE.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM P3000_TITLE_WRITE.

*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 수입의뢰 No.
  PERFORM P1000_READ_RN_DATA USING W_ERR_CHK.
  CHECK W_ERR_CHK NE 'Y'.

* P/O Table Select..
  PERFORM P1000_READ_PO_DATA.

* 모선 Table Select..
  PERFORM P1000_READ_MS_DATA.

* B/L Table Select..
  PERFORM P1000_READ_BL_DATA.

* Commercial Invoice Table Select..
  PERFORM P1000_READ_CIV_DATA.

* 하역 Table Select..
  PERFORM P1000_READ_CG_DATA.

* 보세구역코드 Table Select..
  PERFORM P1000_READ_IMG03_DATA.

* 구매처 Table Select..
  PERFORM P1000_READ_LFA_DATA.

* 통관 Table Select..
  PERFORM P1000_READ_CUCL_DATA.

* 통관 Invoice Table Select..
*   PERFORM P1000_READ_CUCLIV_DATA.

* 통관요청/입고요청 Table Select..
  PERFORM P1000_READ_IVIT_DATA.

* 수입신고 Table Select..
  PERFORM P1000_READ_ZFIDR_DATA.

* 수입면허 Table Select..
  PERFORM P1000_READ_ZFIDS_DATA.

* T001W Table Select..
  PERFORM P1000_READ_T001W_DATA.

* T001L Table Select..
  PERFORM P1000_READ_T001L_DATA.

* NHJ 추가( 2001. 07. 03 )
* 입고 TABLE READ
   PERFORM P1000_READ_IN_DATA.

*-----------------------------------------------------------------------
* END-OF-SELECTION.
*-----------------------------------------------------------------------
END-OF-SELECTION.
  CHECK W_ERR_CHK NE 'Y'.
* Title Text Write.
  SET TITLEBAR 'TIT1'.
  SET PF-STATUS 'ZIM92'.

* Sort P/O, Request No. Internal Table.
  PERFORM P2000_SORT_IT_DATA.

* List Write...
  PERFORM P3000_WRITE_PO_DATA.

*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
AT LINE-SELECTION.

  DATA : L_TEXT_MA(20),
         L_TEXT_PO(20),
         L_TEXT_RN(20),
         L_TEXT_BL(24),
         L_TEXT_IVIT(20),
         L_TEXT_CIV(20),
         L_TEXT_IDR(20),
         L_TEXT_IDS(20),
         L_TEXT_IN(20).
  DATA : MM03_START_SICHT(15) TYPE C  VALUE 'BDEKLPQSVXZA'.

  GET CURSOR FIELD L_TEXT_MA.
  CASE L_TEXT_MA.   " 필드명..

    WHEN 'IT_PO-MATNR' OR 'IT_PO-TXZ01'.
      SET PARAMETER ID 'MAT' FIELD IT_PO-MATNR.
      SET PARAMETER ID 'BUK' FIELD IT_PO-BUKRS.
      SET PARAMETER ID 'WRK' FIELD IT_PO-WERKS.
      SET PARAMETER ID 'LAG' FIELD ''.
      SET PARAMETER ID 'MXX' FIELD MM03_START_SICHT.
      CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.

  ENDCASE.

  GET CURSOR FIELD L_TEXT_PO.
  CASE L_TEXT_PO.   " 필드명..

    WHEN 'IT_PO-EBELN' OR 'IT_PO-EBELP'.
      SET PARAMETER ID 'BES'  FIELD IT_PO-EBELN.
      SET PARAMETER ID 'BSP'  FIELD IT_PO-EBELP.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDCASE.
  CLEAR: IT_PO.

  GET CURSOR FIELD L_TEXT_RN.
  CASE L_TEXT_RN.   " 필드명..

    WHEN 'IT_RN-ZFOPNNO' OR 'IT_RN-ZFREQNO'.
      SET PARAMETER ID 'ZPREQNO' FIELD IT_RN-ZFREQNO.
      SET PARAMETER ID 'ZPOPNNO' FIELD ''.
      SET PARAMETER ID 'BES'     FIELD ''.
      CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.

  ENDCASE.
  CLEAR: IT_RN.

  GET CURSOR FIELD L_TEXT_BL.
  CASE L_TEXT_BL.   " 필드명..

    WHEN 'IT_BL-ZFHBLNO' OR 'IT_BL-ZFBLNO'.
      SET PARAMETER ID 'ZPBLNO'  FIELD IT_BL-ZFBLNO.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
    WHEN 'IT_CG-ZFCGNO'.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      SET PARAMETER ID 'ZPBLNO'  FIELD ''.
      SET PARAMETER ID 'ZPCGNO'  FIELD IT_CG-ZFCGNO.
      CALL TRANSACTION 'ZIM83' AND SKIP FIRST SCREEN.
  ENDCASE.
  CLEAR: IT_BL.

  GET CURSOR FIELD L_TEXT_IVIT.
  CASE L_TEXT_IVIT.   " 필드명..

    WHEN 'IT_IVIT-ZFIVNO'.
      SET PARAMETER ID 'ZPIVNO' FIELD IT_IVIT-ZFIVNO.
      SET PARAMETER ID 'ZPBLNO' FIELD ''.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      CALL TRANSACTION 'ZIM33' AND SKIP FIRST SCREEN.
  ENDCASE.
  CLEAR: IT_IVIT.

  GET CURSOR FIELD L_TEXT_CIV.
  CASE L_TEXT_CIV.   " 필드명..

    WHEN 'IT_CIV-ZFCIVRN' OR 'IT_CIV-ZFCIVNO'.
      SET PARAMETER ID 'ZPCIVRN' FIELD IT_CIV-ZFCIVRN.
      SET PARAMETER ID 'ZPCIVNO' FIELD ''.
      CALL TRANSACTION 'ZIM37' AND SKIP FIRST SCREEN.

  ENDCASE.
  CLEAR: IT_CIV.

  GET CURSOR FIELD L_TEXT_IDR.
  CASE L_TEXT_IDR.   " 필드명..

    WHEN 'IT_IDR-ZFIDRNO' OR 'IT_IDR-ZFCLSEQ'.
      SET PARAMETER ID 'ZPIDRNO' FIELD IT_IDR-ZFIDRNO.
      SET PARAMETER ID 'ZPCLSEQ' FIELD IT_IDR-ZFCLSEQ.
      SET PARAMETER ID 'ZPBLNO' FIELD IT_IDR-ZFBLNO.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      CALL TRANSACTION 'ZIM63' AND SKIP FIRST SCREEN.

  ENDCASE.
  CLEAR: IT_IDR.

  GET CURSOR FIELD L_TEXT_IDS.
  CASE L_TEXT_IDS.   " 필드명..

    WHEN 'IT_IDS-ZFIDRNO' OR 'IT_IDS-ZFCLSEQ'.
      SET PARAMETER ID 'ZPIDRNO' FIELD IT_IDS-ZFIDRNO.
      SET PARAMETER ID 'ZPCLSEQ' FIELD IT_IDS-ZFCLSEQ.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      SET PARAMETER ID 'ZPBLNO' FIELD  IT_IDS-ZFBLNO.
      CALL TRANSACTION 'ZIM76' AND SKIP FIRST SCREEN.

  ENDCASE.
  CLEAR: IT_IDS.
*>> NHJ 20001.07.03 입고 DATA DISPLAY
  GET CURSOR FIELD L_TEXT_IN.
  CASE  L_TEXT_IN.
     WHEN  'IT_IN-MBLNR'.
           SET  PARAMETER ID  'BUK'   FIELD   IT_IN-BUKRS.
           SET  PARAMETER ID  'MBN'   FIELD   IT_IN-MBLNR.
           SET  PARAMETER ID  'MJA'   FIELD   IT_IN-MJAHR.
*>> 입고문서 조회 FUNCTION CALL.
           CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              I_ACTION                  = 'A04'
              I_REFDOC                  = 'R02'
              I_NOTREE                  = 'X'
*             I_NO_AUTH_CHECK           =
              I_SKIP_FIRST_SCREEN       = 'X'
*             I_DEADEND                 = 'X'
              I_OKCODE                  = 'OK_GO'
*             I_LEAVE_AFTER_POST        =
*             i_new_rollarea            = 'X'
*             I_SYTCODE                 =
*             I_EBELN                   =
*             I_EBELP                   =
              I_MBLNR                   = IT_IN-MBLNR
              I_MJAHR                   = IT_IN-MJAHR
*             I_ZEILE                   =
           EXCEPTIONS
              ILLEGAL_COMBINATION       = 1
              OTHERS                    = 2.
      ENDCASE.
      CLEAR  L_TEXT_IN.
*&------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&------------------------------------------------------------------*
FORM P1000_READ_RN_DATA USING W_ERR_CHK.

  W_ERR_CHK = 'N'.
  CLEAR  W_TRIPLE.
  IF P_ALL = 'X'.
     W_TRIPLE = '%'.
  ELSEIF P_NO = 'X'.
     W_TRIPLE = ' %'.
  ELSEIF P_YES = 'X'.
     W_TRIPLE = 'X%'.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_RN
    FROM   ZTREQHD AS H INNER JOIN ZTREQIT AS I
    ON     H~ZFREQNO EQ I~ZFREQNO
    WHERE  H~ZFREQNO  IN  S_REQNO
    AND    H~ZFOPNNO  IN  S_OPNNO
    AND    H~EBELN    IN  S_EBELN
    AND    I~MATNR    IN  S_MATNR
    AND    H~LIFNR    IN  S_LIFNR
    AND    H~ZFREQTY  IN  S_REQTY
    AND    H~ZFTRANS  IN  S_TRANS
    AND    H~BUKRS    IN  S_BUKRS
    AND    H~ZFREQSD  IN  S_REQSD
    AND    H~ZFREQED  IN  S_REQED
    AND    H~ZFSHCU   IN  S_SHCU
    AND    H~ZFTRIPLE LIKE  W_TRIPLE.

    IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
    ENDIF.

    LOOP AT IT_RN.
      W_TABIX = SY-TABIX.
      ON CHANGE OF IT_RN-ZFREQNO.
        SELECT SINGLE * FROM ZTREQORJ
                       WHERE ZFREQNO = IT_RN-ZFREQNO
                         AND ZFLSG7O = '00010'.

        SELECT SINGLE * FROM T005T
                       WHERE LAND1   = ZTREQORJ-ZFORIG
                         AND SPRAS   = SY-LANGU.
      ENDON.

      MOVE: ZTREQORJ-ZFORIG TO IT_RN-ZFORIG,
            T005T-LANDX     TO IT_RN-LANDX.
      MODIFY IT_RN INDEX W_TABIX.
    ENDLOOP.

  ENDFORM.                    " P1000_READ_RN_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

  SELECT *
    INTO   CORRESPONDING FIELDS OF TABLE IT_PO
    FROM   EKKO AS H INNER JOIN EKPO AS I
    ON     H~EBELN EQ I~EBELN
    FOR ALL ENTRIES IN IT_RN
    WHERE  H~EBELN EQ IT_RN-EBELN
    AND    I~EBELP EQ IT_RN-EBELP.
*     AND    I~MATNR EQ IT_RN-MATNR
*     AND    H~LIFNR EQ IT_RN-LIFNR.

  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_LFA_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_LFA_DATA.

  SELECT *
    INTO   CORRESPONDING FIELDS OF TABLE IT_LFA
    FROM   LFA1
    FOR ALL ENTRIES IN IT_RN
    WHERE  LIFNR EQ IT_RN-LIFNR.

  SELECT *
    APPENDING  CORRESPONDING FIELDS OF TABLE IT_LFA
    FROM   LFA1
    FOR ALL ENTRIES IN IT_IMG03
    WHERE  LIFNR EQ IT_IMG03-LIFNR.

  SELECT *
    APPENDING  CORRESPONDING FIELDS OF TABLE IT_LFA
    FROM   LFA1
    FOR ALL ENTRIES IN IT_CIV
    WHERE  LIFNR EQ IT_CIV-ZFMAVN
       OR  LIFNR EQ IT_CIV-ZFOPBN.

ENDFORM.                    " P1000_READ_LFA_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_MS_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_MS
    FROM   ZTMSHD AS H INNER JOIN ZTMSIT AS I
    ON     H~ZFMSNO EQ I~ZFMSNO
    FOR ALL ENTRIES   IN  IT_RN
    WHERE  I~ZFREQNO  EQ  IT_RN-ZFREQNO.

  SELECT *
    APPENDING  CORRESPONDING FIELDS OF TABLE IT_MS
    FROM   ZTMSHD AS H INNER JOIN ZTMSIT AS I
    ON     H~ZFMSNO EQ I~ZFMSNO
    FOR ALL ENTRIES   IN IT_BL
    WHERE  H~ZFMSNO   EQ IT_BL-ZFMSNO.


ENDFORM.                    " P1000_READ_MS_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_BL
    FROM   ZTBL AS H INNER JOIN ZTBLIT AS I
    ON     H~ZFBLNO  EQ I~ZFBLNO
    FOR ALL ENTRIES IN IT_RN
    WHERE  I~EBELN   EQ IT_RN-EBELN
    AND    I~EBELP   EQ IT_RN-EBELP
    AND    I~ZFREQNO EQ IT_RN-ZFREQNO
    AND    I~ZFITMNO EQ IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_BL_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_CIV
    FROM   ZTCIVHD AS H INNER JOIN ZTCIVIT AS I
    ON     H~ZFCIVRN  EQ I~ZFCIVRN
    FOR ALL ENTRIES IN IT_RN
    WHERE  I~EBELN   EQ IT_RN-EBELN
    AND    I~EBELP   EQ IT_RN-EBELP
    AND    I~ZFREQNO EQ IT_RN-ZFREQNO
    AND    I~ZFITMNO EQ IT_RN-ZFITMNO.
*     AND    I~ZFBLNO  EQ IT_BL-ZFBLNO
*     AND    I~ZFBLIT  EQ IT_BL-ZFBLIT.

ENDFORM.                    " P1000_READ_CIV_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*       Read Cargo Data.
*----------------------------------------------------------------------*
FORM P1000_READ_CG_DATA.

  IF NOT IT_BL[] IS INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_CG
      FROM   ZTCGHD AS H INNER JOIN ZTCGIT AS I
      ON     H~ZFCGNO EQ I~ZFCGNO
      FOR ALL ENTRIES   IN  IT_BL
      WHERE  I~EBELN    EQ  IT_BL-EBELN
      AND    I~EBELP    EQ  IT_BL-EBELP
      AND    I~ZFREQNO  EQ  IT_BL-ZFREQNO
      AND    I~ZFITMNO  EQ  IT_BL-ZFITMNO
      AND    I~ZFBLNO   EQ  IT_BL-ZFBLNO
      AND    I~ZFBLIT   EQ  IT_BL-ZFBLIT.
  ENDIF.

ENDFORM.                    " P1000_READ_CG_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IMG03_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_IMG03_DATA.

  SELECT *
    INTO   CORRESPONDING FIELDS OF TABLE IT_IMG03
    FROM   ZTIMIMG03
    FOR ALL ENTRIES IN IT_CG
    WHERE  ZFBNARCD EQ IT_CG-ZFBNARCD.

ENDFORM.                    " P1000_READ_IMG03_DATA

*&---------------------------------------------------------------------
*&      Form  P1000_READ_CUCL_DATA
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------

FORM P1000_READ_CUCL_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_CUCL
    FROM ZTCUCLIV
    FOR ALL ENTRIES IN IT_IVIT
    WHERE ZFIVNO EQ IT_IVIT-ZFIVNO.

ENDFORM.                    " P1000_READ_CUCL_DATA

**&---------------------------------------------------------------------
*
**&      Form  P1000_READ_CUCLIV_DATA
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
*FORM P1000_READ_CUCLIV_DATA.
*
*   SELECT *
*     INTO CORRESPONDING FIELDS OF TABLE IT_CUCLIV
*     FROM ZTCUCLIV AS H INNER JOIN ZTCUCLIVIT AS I
*     ON     H~ZFIVNO EQ I~ZFIVNO
*     WHERE  ZFBLNO   EQ IT_CUCL-ZFBLNO
*       AND  ZFCLSEQ  EQ IT_CUCL-ZFCLSEQ.
*
*ENDFORM.                    " P1000_READ_CUCLIV_DATA
*

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IVIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_IVIT_DATA.
  DATA : L_TABIX LIKE SY-TABIX.

  IF NOT IT_BL[] IS INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_IVIT
      FROM ZTIVIT
      FOR ALL ENTRIES IN IT_BL
      WHERE   ZFBLNO EQ IT_BL-ZFBLNO
        AND   ZFBLIT EQ IT_BL-ZFBLIT
        AND ( ZFCGNO IS NULL
        OR    ZFCGNO EQ SPACE ).
  ENDIF.
*     EQ IT_BL-ZFBLNO.
  IF NOT IT_CG[] IS INITIAL.
    SELECT *
      APPENDING CORRESPONDING FIELDS OF TABLE IT_IVIT
      FROM ZTIVIT
      FOR ALL ENTRIES IN IT_CG
      WHERE ZFCGNO EQ IT_CG-ZFCGNO
        AND ZFCGIT EQ IT_CG-ZFCGIT.
  ENDIF.

*>> LOCAL 구매 DATA SELECT!
  SELECT  *
     APPENDING CORRESPONDING FIELDS OF TABLE IT_IVIT
     FROM   ZTIV AS H INNER JOIN ZTIVIT AS I
     ON       H~ZFIVNO   EQ    I~ZFIVNO
     FOR    ALL ENTRIES  IN    IT_RN
     WHERE  ( H~ZFREQTY  EQ    'LO' OR H~ZFREQTY EQ 'PU' )
     AND      I~ZFREQNO  EQ    IT_RN-ZFREQNO
     AND      I~ZFITMNO  EQ    IT_RN-ZFITMNO.

  LOOP AT IT_IVIT.
    L_TABIX = SY-TABIX.
    SELECT SINGLE ZFCUST ZFGRST
           INTO   (IT_IVIT-ZFCUST, IT_IVIT-ZFGRST)
           FROM   ZTIV
           WHERE  ZFIVNO EQ IT_IVIT-ZFIVNO.
    MODIFY IT_IVIT INDEX L_TABIX.
  ENDLOOP.
ENDFORM.                    " P1000_READ_IVIT_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZFIDR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_ZFIDR_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_IDR
    FROM ZTIDR  AS  H  INNER JOIN  ZTIDRHSD AS I
    ON   H~ZFBLNO  EQ  I~ZFBLNO
    AND  H~ZFCLSEQ EQ  I~ZFCLSEQ
    FOR ALL ENTRIES IN IT_IVIT
    WHERE I~ZFIVNO  EQ IT_IVIT-ZFIVNO
      AND I~ZFIVDNO EQ IT_IVIT-ZFIVDNO.

ENDFORM.                    " P1000_READ_ZFIDR_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZFIDS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_ZFIDS_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_IDS
    FROM ZTIDS  AS  H  INNER  JOIN  ZTIDSHSD  AS  I
    ON   H~ZFBLNO    EQ  I~ZFBLNO
    AND  H~ZFCLSEQ   EQ  I~ZFCLSEQ
    FOR ALL ENTRIES IN IT_IDR
    WHERE H~ZFBLNO  EQ  IT_IDR-ZFBLNO
      AND H~ZFCLSEQ EQ  IT_IDR-ZFCLSEQ.

ENDFORM.                    " P1000_READ_ZFIDS_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_T001W_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_T001W_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_T001W FROM T001W
    FOR ALL ENTRIES IN IT_IVIT
    WHERE WERKS   EQ IT_IVIT-WERKS.
*       AND ZFCLSEQ EQ IT_CUCL-ZFCLSEQ.

ENDFORM.                    " P1000_READ_T001W_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_T001L_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_T001L_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_T001L FROM T001L
    FOR ALL ENTRIES IN IT_IVIT
    WHERE WERKS   EQ IT_IVIT-WERKS
      AND LGORT   EQ IT_IVIT-LGORT.

ENDFORM.                    " P1000_READ_T001L_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE: /55 '[자재별 진행현황]'
             COLOR COL_HEADING INTENSIFIED OFF.

  WRITE: / 'Date: ' ,
            SY-DATUM .

  WRITE: / '   '  COLOR COL_HEADING INTENSIFIED OFF,
           ': P/O  ' ,
           '   '  COLOR COL_NORMAL  INTENSIFIED OFF,
           ': 수입의뢰  ',
           '   '  COLOR COL_NORMAL  INTENSIFIED ON,
           ': Commercial I/V  ',
           '   '  COLOR COL_TOTAL   INTENSIFIED OFF,
           ': B/L  ',
           '   '  COLOR COL_POSITIVE INTENSIFIED OFF,
           ': 하역  ',
           '   '  COLOR COL_NEGATIVE INTENSIFIED OFF,
           ': 통관요청  ',
           '   '  COLOR COL_GROUP    INTENSIFIED OFF,
           ': 수입신고  ',
           '   '  COLOR COL_GROUP    INTENSIFIED ON,
           ': 수입면허  ',
           '   '  COLOR COL_TOTAL    INTENSIFIED ON,
           ': 입고  '.
  SKIP 1.
ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_IT_DATA
*&---------------------------------------------------------------------*
*       SORTING INTERNAL TABLE..
*----------------------------------------------------------------------*
FORM P2000_SORT_IT_DATA.

  SORT IT_PO BY MATNR EBELN EBELP.
  SORT IT_RN BY MATNR EBELN ZFITMNO ZFREQNO.

ENDFORM.                    " P2000_SORT_IT_DATA

*----------------------------------------------------------------------*
* 결과화면 조회를 위한 PERFORM 문..
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P3000_WEITE_PO_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_PO_DATA.
  DATA : L_FIRST_LINE   VALUE  'Y',
         L_DATE(10).

*>>> 임시변수의 값들을 초기화..
  CLEAR: TEMP_MATNR, " 자재번?
         TEMP_EBELN, " P/O 번?
         TEMP_EBELP, " Item 번?
         TEMP_TXZ01. " 자재내?
  SKIP.
  LOOP AT IT_PO.
    IF TEMP_TXZ01 NE IT_PO-TXZ01.
      FORMAT COLOR COL_HEADING INTENSIFIED ON.
      ULINE.
      WRITE: / IT_PO-MATNR NO-GAP, IT_PO-TXZ01 NO-GAP,
               '                               ' NO-GAP,
               142 '' NO-GAP.
      HIDE: IT_PO.
      FORMAT RESET.
    ENDIF.

    IF IT_PO-EBELN NE TEMP_EBELN.
      READ TABLE IT_LFA WITH KEY LIFNR = IT_PO-LIFNR.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      CONCATENATE  IT_PO-AEDAT(4)  IT_PO-AEDAT+4(2)
                    IT_PO-AEDAT+6(2)
                    INTO L_DATE
                    SEPARATED BY '/'.

      READ TABLE IT_LFA WITH KEY LIFNR = IT_PO-LIFNR.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE: / IT_PO-EBELN NO-GAP.
      WRITE: 43 IT_PO-EBELP NO-GAP.
      WRITE: 68 IT_PO-MENGE UNIT IT_PO-MEINS NO-GAP,
             85 IT_PO-MEINS NO-GAP,
             91 IT_PO-PEINH NO-GAP, 97 IT_LFA-NAME1(20) NO-GAP.
*         WRITE: 132 L_DATE NO-GAP, '' NO-GAP.
      WRITE: 132 IT_PO-AEDAT NO-GAP, '' NO-GAP.
      HIDE: IT_PO.

    ELSEIF IT_PO-EBELP NE TEMP_EBELP.
      READ TABLE IT_LFA WITH KEY LIFNR = IT_PO-LIFNR.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE: / IT_PO-EBELN NO-GAP.
      WRITE: 43 IT_PO-EBELP NO-GAP.
      WRITE: 68 IT_PO-MENGE UNIT IT_PO-MEINS NO-GAP,
             85 IT_PO-MEINS NO-GAP,
             91 IT_PO-PEINH NO-GAP, 97 IT_LFA-NAME1(20) NO-GAP.
*         WRITE: 132 L_DATE NO-GAP, '' NO-GAP.
      WRITE: 132 IT_PO-AEDAT NO-GAP, '' NO-GAP.
      HIDE: IT_PO.
    ENDIF.
    PERFORM P3000_WRITE_RN_DATA.
    TEMP_MATNR = IT_PO-MATNR.
    TEMP_TXZ01 = IT_PO-TXZ01.
    TEMP_EBELN = IT_PO-EBELN.
    TEMP_EBELP = IT_PO-EBELP.
    CLEAR: IT_PO.
  ENDLOOP.

ENDFORM.                    " P3000_WEITE_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_RN_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_RN_DATA.

  DATA : L_FIRST_LINE   VALUE  'Y',
         L_DATE_F(10),
         L_DATE_T(10).

*>>> 임시변수의 값들을 초기화..
  CLEAR: TEMP_REQNO,
         TEMP_ITMNO,
         W_LOOP_CNT.

  LOOP AT IT_RN
     WHERE EBELN = IT_PO-EBELN
       AND EBELP = IT_PO-EBELP.

    W_LOOP_CNT = W_LOOP_CNT + 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    IF TEMP_REQNO NE IT_RN-ZFREQNO.
      IF L_FIRST_LINE = 'Y'.
        WRITE: /3 IT_RN-ZFREQNO NO-GAP, 15 IT_RN-ZFOPNNO(20) NO-GAP.
      ELSE.
        WRITE: /3 IT_RN-ZFREQNO NO-GAP, 15 IT_RN-ZFOPNNO(20) NO-GAP.
      ENDIF.
      L_FIRST_LINE = 'N'.

      WRITE: 45 IT_RN-ZFITMNO NO-GAP,
             52 IT_RN-LANDX NO-GAP,
             68 IT_RN-MENGE UNIT IT_RN-MEINS NO-GAP,
             85 IT_RN-MEINS NO-GAP, 142 '' NO-GAP.
      HIDE: IT_RN.
    ELSEIF TEMP_ITMNO NE IT_RN-ZFITMNO.
      WRITE: /45 IT_RN-ZFITMNO NO-GAP,
              52 IT_RN-LANDX NO-GAP,
              68 IT_RN-MENGE UNIT IT_RN-MEINS NO-GAP,
             85 IT_RN-MEINS NO-GAP, 142 '' NO-GAP.
      HIDE: IT_RN.
    ENDIF.
    LOOP AT IT_MS
       WHERE ZFREQNO = IT_RN-ZFREQNO.
      CONCATENATE IT_MS-ZFSHSDF(4)
                  IT_MS-ZFSHSDF+4(2)
                  IT_MS-ZFSHSDF+6(2)
                  INTO L_DATE_F
                  SEPARATED BY '/'.

      CONCATENATE IT_MS-ZFSHSDT(4)
                  IT_MS-ZFSHSDT+4(2)
                  IT_MS-ZFSHSDT+6(2)
                  INTO L_DATE_T
                  SEPARATED BY '/'.

*         WRITE: 100 IT_MS-ZFMSNM(18) NO-GAP,
*               120 L_DATE_F NO-GAP,
*               132 L_DATE_T NO-GAP,
*               142 '' NO-GAP.
      WRITE: 100 IT_MS-ZFMSNM(18) NO-GAP,
             120 IT_MS-ZFSHSDF NO-GAP,
             132 IT_MS-ZFSHSDT NO-GAP,
             142 '' NO-GAP.

    ENDLOOP.
    PERFORM P3000_WRITE_CIV_DATA.
    PERFORM P3000_WRITE_LO_IVIT_DATA.
    PERFORM P3000_WRITE_BL_DATA.

    IF W_LOOP_CNT LT 1.
      PERFORM P3000_WRITE_BL_DATA.
    ENDIF.

    TEMP_REQNO = IT_RN-ZFREQNO.
    TEMP_ITMNO = IT_RN-ZFITMNO.
    CLEAR: IT_RN.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_RN_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*       Subroutine for Writing Commercial Invoice Data.
*----------------------------------------------------------------------*
FORM P3000_WRITE_CIV_DATA.

  LOOP AT IT_CIV
     WHERE ZFREQNO = IT_RN-ZFREQNO
       AND ZFITMNO = IT_RN-ZFITMNO.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    WRITE: /3 IT_CIV-ZFCIVRN NO-GAP, '  ' NO-GAP,
              IT_CIV-ZFCIVNO(30) NO-GAP, IT_CIV-ZFCIVSQ NO-GAP,
           68 IT_CIV-CMENGE UNIT IT_CIV-MEINS NO-GAP,
           85 IT_CIV-MEINS NO-GAP.
    HIDE: IT_CIV.
    READ TABLE IT_LFA WITH KEY LIFNR = IT_CIV-ZFMAVN.
    IF SY-SUBRC EQ 0.
      WRITE IT_LFA-NAME1(20) NO-GAP.
    ELSE.
      WRITE: 142 '' NO-GAP.
    ENDIF.
    READ TABLE IT_LFA WITH KEY LIFNR = IT_CIV-ZFOPBN.
    IF SY-SUBRC EQ 0.
      WRITE: IT_LFA-NAME1(20) NO-GAP, 142 '' NO-GAP.
    ELSE.
      WRITE: 142 '' NO-GAP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_CIV_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_BL_DATA
*&---------------------------------------------------------------------*
*        Write Bill of Lading Data.
*----------------------------------------------------------------------*
FORM P3000_WRITE_BL_DATA.
  DATA : L_FIRST_LINE   VALUE  'Y',
         L_DATE_F(10),
         L_DATE_T(10).
*>>> 임시변수의 값들을 초기화..
  CLEAR : TEMP_BLNO,
          TEMP_BLIT.

  LOOP AT IT_BL
     WHERE ZFREQNO = IT_RN-ZFREQNO
       AND ZFITMNO = IT_RN-ZFITMNO.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

    IF TEMP_BLNO NE IT_BL-ZFBLNO.
      WRITE: /5 IT_BL-ZFBLNO NO-GAP, 17 IT_BL-ZFHBLNO NO-GAP,
             47 IT_BL-ZFBLIT NO-GAP, 54 IT_BL-ZFFORD NO-GAP,
             68 IT_BL-BLMENGE UNIT IT_BL-MEINS NO-GAP,
             85 IT_BL-MEINS NO-GAP, 90 IT_BL-ZFAPRTC NO-GAP,
            142 '' NO-GAP.
      PERFORM P3000_WRITE_BL_ADD_DATA.
      HIDE: IT_BL.
    ELSEIF TEMP_BLIT NE IT_BL-ZFBLIT.
      WRITE: /, /47 IT_BL-ZFBLIT NO-GAP, 54 IT_BL-ZFFORD NO-GAP,
                 68 IT_BL-BLMENGE UNIT IT_BL-MEINS NO-GAP,
                 85 IT_BL-MEINS NO-GAP, 90 IT_BL-ZFAPRTC NO-GAP,
                142 '' NO-GAP.
      PERFORM P3000_WRITE_BL_ADD_DATA.
      HIDE: IT_BL.
    ENDIF.

    LOOP AT IT_MS
       WHERE ZFMSNO = IT_BL-ZFMSNO.
      CONCATENATE IT_MS-ZFSHSDF(4)
                  IT_MS-ZFSHSDF+4(2)
                  IT_MS-ZFSHSDF+6(2)
                  INTO L_DATE_F
                  SEPARATED BY '/'.

      CONCATENATE IT_MS-ZFSHSDT(4)
                  IT_MS-ZFSHSDT+4(2)
                  IT_MS-ZFSHSDT+6(2)
                  INTO L_DATE_T
                  SEPARATED BY '/'.

*         WRITE: 100 IT_MS-ZFMSNM(18) NO-GAP,
*               120 L_DATE_F NO-GAP,
*               132 L_DATE_T NO-GAP,
*               142 '' NO-GAP.
      WRITE: 100 IT_MS-ZFMSNM(18) NO-GAP,
             120 IT_MS-ZFSHSDF NO-GAP,
             132 IT_MS-ZFSHSDT NO-GAP,
             142 '' NO-GAP.

    ENDLOOP.

**> 하역없는 통관요청이 존재할 경우.
    READ TABLE IT_IVIT WITH KEY ZFBLNO = IT_BL-ZFBLNO
                                ZFBLIT = IT_BL-ZFBLIT
                                ZFCGNO = ''.
    IF SY-SUBRC EQ 0.
      PERFORM P3000_WRITE_IVIT_DATA.
    ENDIF.
    PERFORM P3000_WRITE_CG_DATA.
    CLEAR: IT_BL.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_BL_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_BL_ADD_DATA
*&---------------------------------------------------------------------*
*       양도 B/L 여부와 유무환 여부를 출력..
*----------------------------------------------------------------------*
FORM P3000_WRITE_BL_ADD_DATA.

  IF IT_BL-ZFPOYN EQ 'Y'.
    WRITE: 95 '유환'.
  ELSE.
    WRITE: 95 '무환'.
  ENDIF.

  IF IT_BL-ZFRENT EQ 'X'.
    WRITE: 102 '양도 B/L'.
  ENDIF.

ENDFORM.                    " P3000_WRITE_BL_ADD_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CG_DATA
*&---------------------------------------------------------------------*
*       하역관련 Data를 출력..
*----------------------------------------------------------------------*
FORM P3000_WRITE_CG_DATA.

  CLEAR : TEMP_CGNO,
          TEMP_CGIT.

  LOOP AT IT_CG
     WHERE ZFBLNO = IT_BL-ZFBLNO
       AND ZFBLIT = IT_BL-ZFBLIT.
    FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
    READ TABLE IT_IMG03 WITH KEY ZFBNARCD = IT_CG-ZFBNARCD.
    IF SY-SUBRC EQ 0.
      READ TABLE IT_LFA   WITH KEY LIFNR    = IT_IMG03-LIFNR.
      IF SY-SUBRC NE 0.
        CLEAR : IT_LFA.
      ENDIF.
    ELSE.
      CLEAR : IT_LFA.
    ENDIF.

    IF TEMP_CGNO NE IT_CG-ZFCGNO.
      WRITE: /7 IT_CG-ZFCGNO NO-GAP, 19 IT_CG-ZFCGPT NO-GAP.
      WRITE: 23 IT_LFA-NAME1(20) NO-GAP, 49 IT_CG-ZFCGIT NO-GAP,
             68 IT_CG-CGMENGE UNIT IT_CG-MEINS NO-GAP,
             85 IT_CG-MEINS NO-GAP, 142 '' NO-GAP.
      HIDE: IT_CG.

    ELSEIF TEMP_CGIT NE IT_CG-ZFCGIT.
      WRITE: /, /23 IT_LFA-NAME1(20) NO-GAP, 49 IT_CG-ZFCGIT NO-GAP,
              68 IT_CG-CGMENGE UNIT IT_CG-MEINS NO-GAP,
              85 IT_CG-MEINS NO-GAP, 142 '' NO-GAP.
      HIDE: IT_CG.
      TEMP_CGNO = IT_CG-ZFCGNO.
      TEMP_CGIT = IT_CG-ZFCGIT.
    ENDIF.
    READ TABLE IT_IVIT WITH KEY ZFCGNO = IT_CG-ZFCGNO
                                ZFCGIT = IT_CG-ZFCGIT.
    IF SY-SUBRC EQ 0.
      PERFORM P3000_WRITE_IVIT_DATA.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_CG_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IVIT_DATA
*&---------------------------------------------------------------------*
*       통관/입고 요청 Data를 출력..
*----------------------------------------------------------------------*
FORM P3000_WRITE_IVIT_DATA.

  CLEAR : TEMP_IVNO,
          TEMP_IVDNO.

  IF IT_IVIT-ZFCGNO = ''.
    LOOP AT IT_IVIT WHERE ZFBLNO = IT_BL-ZFBLNO
                    AND   ZFBLIT = IT_BL-ZFBLIT
                    AND   ZFCGNO = SPACE.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
      IF TEMP_IVNO NE IT_IVIT-ZFIVNO.
        WRITE: /9 IT_IVIT-ZFIVNO NO-GAP, 21 IT_IVIT-TXZ01 NO-GAP,
               51 IT_IVIT-ZFIVDNO NO-GAP,
               68 IT_IVIT-CCMENGE UNIT IT_IVIT-MEINS NO-GAP,
               85 IT_IVIT-MEINS NO-GAP, 142 '' NO-GAP.
        PERFORM P3000_WRITE_OTHER_DATA.      " 통관/입고상태를 기술.

        HIDE: IT_IVIT.
      ELSEIF TEMP_IVDNO NE IT_IVIT-ZFIVDNO.
        WRITE: /, 21 IT_IVIT-TXZ01 NO-GAP,
               /51 IT_IVIT-ZFIVDNO NO-GAP,
               68 IT_IVIT-CCMENGE UNIT IT_IVIT-MEINS NO-GAP,
               85 IT_IVIT-MEINS NO-GAP, 142 '' NO-GAP.
        PERFORM P3000_WRITE_OTHER_DATA.      " 통관/입고상태를 기술.

        HIDE: IT_IVIT.
      ENDIF.
      TEMP_IVNO  = IT_IVIT-ZFIVNO.
      TEMP_IVDNO = IT_IVIT-ZFIVDNO.
*>> 통관상태에 따라서 통관자료 WRITE
      IF IT_IVIT-ZFCUST EQ '2'
      OR IT_IVIT-ZFCUST EQ '3'
      OR IT_IVIT-ZFCUST EQ 'Y'.
         PERFORM P3000_WRITE_IDR_DATA.
      ENDIF.

    ENDLOOP.
  ELSE.
    LOOP AT IT_IVIT WHERE ZFCGNO = IT_CG-ZFCGNO
                      AND ZFCGIT = IT_CG-ZFCGIT.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
      IF TEMP_IVNO NE IT_IVIT-ZFIVNO.
        WRITE: /9 IT_IVIT-ZFIVNO NO-GAP, 21 IT_IVIT-TXZ01 NO-GAP,
               51 IT_IVIT-ZFIVDNO NO-GAP,
               68 IT_IVIT-CCMENGE UNIT IT_IVIT-MEINS NO-GAP,
               85 IT_IVIT-MEINS NO-GAP, 142 '' NO-GAP.
        PERFORM P3000_WRITE_OTHER_DATA.      " 통관/입고상태를 기술.

        HIDE: IT_IVIT.
      ELSEIF TEMP_IVDNO NE IT_IVIT-ZFIVDNO.
        WRITE: /, 21 IT_IVIT-TXZ01 NO-GAP,
               /51 IT_IVIT-ZFIVDNO NO-GAP,
               68 IT_IVIT-CCMENGE UNIT IT_IVIT-MEINS NO-GAP,
               85 IT_IVIT-MEINS NO-GAP, 142 '' NO-GAP.
        PERFORM P3000_WRITE_OTHER_DATA.      " 통관/입고상태를 기술.
        HIDE: IT_IVIT.

      ENDIF.
*>> 통관상태에 따라서 통관자료 WRITE
      IF IT_IVIT-ZFCUST EQ '2'
      OR IT_IVIT-ZFCUST EQ '3'
      OR IT_IVIT-ZFCUST EQ 'Y'.
         PERFORM P3000_WRITE_IDR_DATA.
      ENDIF.

      TEMP_IVNO  = IT_IVIT-ZFIVNO.
      TEMP_IVDNO = IT_IVIT-ZFIVDNO.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " P3000_WRITE_IVIT_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_OTHER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_OTHER_DATA.

  IF IT_IVIT-ZFCUST EQ '1'.
    WRITE: 97 '수입신고 의뢰생성' NO-GAP.
  ELSEIF IT_IVIT-ZFCUST EQ '2'.
    WRITE: 97 '수입신고 의뢰대상' NO-GAP.
  ELSEIF IT_IVIT-ZFCUST EQ '3'.
    WRITE: 97 '수입신고 의뢰 중' NO-GAP.
  ELSEIF IT_IVIT-ZFCUST EQ 'Y'.
    WRITE: 97 '통관완료' NO-GAP.
  ELSEIF IT_IVIT-ZFCUST EQ 'N'.
    WRITE: 97 '미통관 대상(불가)' NO-GAP.
  ENDIF.
*>> NHJ 2001.07.03 ( 분할입고 가능으로 인한 입고완료 유무 CHECK DEL )
*  IF it_ivit-zfgrst EQ 'Y'.
*   WRITE: 120 '입고완료' NO-GAP.
* ELSEIF it_ivit-zfgrst EQ 'N'.
*   WRITE: 120 '입고대상' NO-GAP.
* ELSEIF it_ivit-zfgrst EQ 'X'.
*   WRITE: 120 '미입고대상' NO-GAP.
* ENDIF.

*  WRITE 142 '' NO-GAP.

ENDFORM.                    " P3000_WRITE_OTHER_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IDR_DATA
*&---------------------------------------------------------------------*
*       수입신고/면허 관련 데이터를 출력..
*----------------------------------------------------------------------*
FORM P3000_WRITE_IDR_DATA.

  IF IT_IVIT-ZFCUST EQ 'Y'.
*   IF sy-subrc EQ 0.
      READ TABLE IT_IDR WITH KEY ZFIVNO  = IT_IVIT-ZFIVNO
                                 ZFIVDNO = IT_IVIT-ZFIVDNO.

      FORMAT COLOR COL_GROUP INTENSIFIED OFF.
      WRITE: /11 IT_IDR-ZFIDRNO NO-GAP, 31 IT_IDR-ZFCUT NO-GAP,
              53 IT_IDR-ZFCLSEQ NO-GAP,
              68 IT_IDR-ZFQNT UNIT IT_IDR-ZFQNTM NO-GAP,
              85 IT_IDR-ZFQNTM NO-GAP, 132 IT_IDR-ZFIDWDT NO-GAP,
             142 '' NO-GAP.
*            READ TABLE WITH KEY
      HIDE: IT_IDR.
      READ TABLE IT_IDS WITH KEY ZFBLNO  = IT_IDR-ZFBLNO
                                 ZFCLSEQ = IT_IDR-ZFCLSEQ.
      IF SY-SUBRC EQ 0.
        FORMAT COLOR COL_GROUP INTENSIFIED ON.
        WRITE: /13 IT_IDS-ZFIDRNO NO-GAP, 55 IT_IDS-ZFCLSEQ NO-GAP,
                68 IT_IDS-ZFQNT UNIT IT_IDS-ZFQNTM NO-GAP,
                85 IT_IDS-ZFQNTM NO-GAP, 132 IT_IDS-ZFIDSDT NO-GAP,
                142 '' NO-GAP.
        HIDE: IT_IDS.
      ENDIF.
*   ENDIF.

*>> NHJ 2001.07.03 (입고 TABLE DATA READ).
    LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IVIT-ZFIVNO
                     AND    ZFIVDNO EQ  IT_IVIT-ZFIVDNO.
       PERFORM  P3000_WRITE_IN_DATA.
    ENDLOOP.

*    IF it_ivit-zfgrst EQ 'Y'.        " 입고상태가 'Y'이면..
*     FORMAT COLOR COL_TOTAL INTENSIFIED ON.
*     WRITE: /15 it_ivit-zfivno NO-GAP.
*     READ TABLE it_t001l WITH KEY lgort = it_ivit-lgort
*                                  werks = it_ivit-werks.
*     READ TABLE it_t001w WITH KEY werks = it_ivit-werks.
*     WRITE: 57 it_ivit-zfivdno NO-GAP,
*            68 it_ivit-grmenge UNIT it_ivit-meins NO-GAP,
*               it_ivit-meins NO-GAP,
*            97 it_t001w-name1 NO-GAP, it_t001l-lgobe NO-GAP,
*           142 '' NO-GAP.
*     HIDE: it_iVIT.
*    ENDIF.

  ELSEIF IT_IVIT-ZFCUST EQ '2'
      OR IT_IVIT-ZFCUST EQ '3'.
    IF SY-SUBRC EQ 0.
      READ TABLE IT_IDR WITH KEY ZFIVNO  = IT_IVIT-ZFIVNO
                                 ZFIVDNO = IT_IVIT-ZFIVDNO.
      WRITE: /11 IT_IDR-ZFIDRNO NO-GAP, 31 IT_IDR-ZFCUT NO-GAP,
              53 IT_IDR-ZFCLSEQ NO-GAP,
              68 IT_IDR-ZFQNT UNIT IT_IDR-ZFQNTM NO-GAP,
              85 IT_IDR-ZFQNTM NO-GAP, 132 IT_IDR-ZFIDWDT NO-GAP,
             142 '' NO-GAP.
      HIDE: IT_IDR.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_WRITE_IDR_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_LO_IVIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_LO_IVIT_DATA.

  CLEAR : TEMP_IVNO,
          TEMP_IVDNO.

  IF IT_RN-ZFREQTY  EQ 'LO' OR IT_RN-ZFREQTY EQ 'PU'.

    LOOP AT IT_IVIT WHERE ZFREQNO = IT_RN-ZFREQNO
                    AND   ZFITMNO = IT_RN-ZFITMNO.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
      IF TEMP_IVNO NE IT_IVIT-ZFIVNO.
        WRITE: /9 IT_IVIT-ZFIVNO NO-GAP, 21 IT_IVIT-TXZ01 NO-GAP,
               51 IT_IVIT-ZFIVDNO NO-GAP,
               68 IT_IVIT-CCMENGE UNIT IT_IVIT-MEINS NO-GAP,
               85 IT_IVIT-MEINS NO-GAP, 142 '' NO-GAP.
        PERFORM P3000_WRITE_OTHER_DATA.      " 통관/입고상태를 기술.

        HIDE: IT_IVIT.
      ELSEIF TEMP_IVDNO NE IT_IVIT-ZFIVDNO.
        WRITE: /, 21 IT_IVIT-TXZ01 NO-GAP,
               /51 IT_IVIT-ZFIVDNO NO-GAP,
               68 IT_IVIT-CCMENGE UNIT IT_IVIT-MEINS NO-GAP,
               85 IT_IVIT-MEINS NO-GAP, 142 '' NO-GAP.
        PERFORM P3000_WRITE_OTHER_DATA.      " 통관/입고상태를 기술.
        HIDE: IT_IVIT.
      ENDIF.
      TEMP_IVNO  = IT_IVIT-ZFIVNO.
      TEMP_IVDNO = IT_IVIT-ZFIVDNO.
*>> NHJ 2001.07.03 (입고 TABLE DATA READ).
    LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IVIT-ZFIVNO
                     AND    ZFIVDNO EQ  IT_IVIT-ZFIVDNO.
       PERFORM  P3000_WRITE_IN_DATA.
    ENDLOOP.

*>>> 입고상태가 'Y'이면..
*      IF it_ivit-zfgrst EQ 'Y'.        " 입고상태가 'Y'이면..
*       FORMAT COLOR COL_TOTAL INTENSIFIED ON.
*       WRITE: /15 it_ivit-zfivno NO-GAP.
*       READ TABLE it_t001l WITH KEY lgort = it_ivit-lgort
*                                    werks = it_ivit-werks.
*       READ TABLE it_t001w WITH KEY werks = it_ivit-werks.
*       WRITE: 57 it_ivit-zfivdno NO-GAP,
*              68 it_ivit-grmenge UNIT it_ivit-meins NO-GAP,
*                 it_ivit-meins NO-GAP,
*              97 it_t001w-name1 NO-GAP, it_t001l-lgobe NO-GAP,
*            142 '' NO-GAP.
*      HIDE: it_ivit.
*     ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P3000_WRITE_LO_IVIT_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_IN_DATA.

   SELECT  * INTO CORRESPONDING FIELDS OF TABLE IT_IN
   FROM    ZTIVHST  AS  A  INNER JOIN  ZTIVHSTIT  AS  B
   ON      A~ZFIVNO        EQ          B~ZFIVNO
   AND     A~ZFIVHST       EQ          B~ZFIVHST
   FOR     ALL  ENTRIES    IN          IT_IVIT
   WHERE   B~ZFIVNO        EQ          IT_IVIT-ZFIVNO
   AND     B~ZFIVDNO       EQ          IT_IVIT-ZFIVDNO
   AND     B~ZFGRST        EQ          'Y'.

ENDFORM.                    " P1000_READ_IN_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_IN_DATA.

   FORMAT COLOR COL_TOTAL INTENSIFIED ON.

   READ TABLE IT_T001L WITH KEY LGORT = IT_IN-LGORT
                                WERKS = IT_IN-WERKS.
   READ TABLE IT_T001W WITH KEY WERKS = IT_IN-WERKS.

   WRITE: /15  IT_IN-MBLNR     NO-GAP,
           68  IT_IN-GRMENGE   UNIT   IT_IN-MEINS NO-GAP,
               IT_IN-MEINS     NO-GAP,
           97  IT_T001W-NAME1  NO-GAP,
               IT_T001L-LGOBE  NO-GAP,
           142 ''              NO-GAP.
   HIDE: IT_IN.

ENDFORM.                    " P3000_WRITE_IN_DATA
