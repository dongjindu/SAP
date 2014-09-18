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
REPORT ZRIMPOSOHIS NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                     LINE-SIZE 180.

TABLES: EKKO,                " PO HEADER TABLE
        EKPO,                " PO ITEM TABLE
        ZTREQHD,             " 수입의뢰 Header Table..
        ZTREQIT,             " 수입의뢰 Item Table..
        ZTREQST,             " 수입의뢰 상태 TABLE
        ZTBL,                " B/L Table..
        ZTBLIT,              " B/L  Item Table..
        KNA1,                " CUSTOMER TABLE.
        LFA1,                " 거래처 Master Table..
        ZTMSHD,              " 모선관리 Header Table..
        ZTCGHD,              " 하역 Header Table..
        ZTCGIT,              " 하역 자재 Table..
        ZTREQORJ,            " 수입의뢰 원산지 내역 Table..
        ZTIMIMG03,           " 보세구역 코드 Table..
        ZTIMIMG10,           " 관세사 관리..
        T024,                " 구매그룹 TABLE.
        T005T,               " 국가이름 Table..
        T001I,               " 저장위치 Table..
        T001L,               " 저장위치 Table..
        T001W,               " 플랜트/분기 Table..
* 2002/08/05 Nashinho Insert.
*        ZV_COTM,             " 관세사 VIEW TABLE
        ZTCIVHD,             " Commercial Invoice Header..
        ZTCIVIT,             " Commercial Invoice Items..
        ZTIV,                " 통관요청/입고요청 Header..
        ZTIVIT,              " 통관요청/입고요청 Item Table..
        ZTIDS,               " 수입면허 Table..
        ZTIVHSTIT,           " 입고요청 ITEM.
        VBAK,                " SALES ODRER HEADER TABLE.
        VBAP,                " SALES ORDER BODY TABLE.
        LIKP,                " DELIVERY HEADER TABLE
        LIPS,                " DELIVERY ITEM TABLE
        MKPF.

*------------------------------------------*
* P/O 번호 조회를 위한 INTERNAL TABLE 선언 *
*------------------------------------------*
DATA: BEGIN OF IT_PO OCCURS 1000,
        EBELN    LIKE   EKPO-EBELN,           "구매문서번호.
        LIFNR    LIKE   EKKO-LIFNR,           "구매처.
        AEDAT    LIKE   EKPO-AEDAT,           "생성일.
        WAERS    LIKE   EKKO-WAERS,           "통화.
        EKGRP    LIKE   EKKO-EKGRP,           "구매그룹.
        EKNAM    LIKE   T024-EKNAM,           "구매그룹명.
        EBELP    LIKE   EKPO-EBELP,           "품목번호.
        MATNR    LIKE   EKPO-MATNR,           "자재.
        BUKRS    LIKE   EKPO-BUKRS,           "회사.
        WERKS    LIKE   EKPO-WERKS,           "플랜트.
        WERNM    LIKE   T001W-NAME1,          "플랜트명.
        TXZ01    LIKE   EKPO-TXZ01,           "자재내역.
        MENGE    LIKE   EKPO-MENGE,           "수량.
        MEINS    LIKE   EKPO-MEINS,           "단위.
        NETPR    LIKE   EKPO-NETPR,           "단가.
        PEINH    LIKE   EKPO-PEINH,           "PRICE UNIT.
        NAME1    LIKE   LFA1-NAME1,           "구매처명.
      END OF IT_PO.

*-----------------------------------------------*
* 수입의뢰 번호 조회를 위한 INTERNAL TABLE 선언 *
*-----------------------------------------------*
DATA: BEGIN OF IT_RN OCCURS 1000,
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,  " 수입의뢰번호.
        ZFREQTY   LIKE   ZTREQHD-ZFREQTY,  " 수입의뢰 TYPE
        EBELN     LIKE   ZTREQHD-EBELN,    " PURCHASING DOCUMENT NUMBER.
        EBELP     LIKE   ZTREQIT-EBELP,    " P/O ITEM NUMBER.
        LIFNR     LIKE   ZTREQHD-LIFNR,    " VENDOR'S ACCOUNT NUMBER.
        WAERS     LIKE   ZTREQHD-WAERS,    " CURRENCY KEY.
        ZFOPBN    LIKE   ZTREQHD-ZFOPBN,   " 개설은행.
        NAME1     LIKE   LFA1-NAME1,
        ZFOPNNO   LIKE   ZTREQHD-ZFOPNNO,  " 신용장-승인번호.
        ZFTRANS   LIKE   ZTREQHD-ZFTRANS,  " 운송방법.
        INCO1     LIKE   ZTREQHD-INCO1,    " 인도조건.
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,  " 수입문서 품목번호.
        MATNR     LIKE   ZTREQIT-MATNR,    " MATERIAL NUMBER.
        MENGE     LIKE   ZTREQIT-MENGE,    " 수입의뢰수량.
        MEINS     LIKE   ZTREQIT-MEINS,    " BASE UNIT OF MEASURE.
        NETPR     LIKE   ZTREQIT-NETPR,    " NET PRICE.
        PEINH     LIKE   ZTREQIT-PEINH,    " PRICE UNIT.
        BPRME     LIKE   ZTREQIT-BPRME,    " ORDER PRICE UNIT.
        ZFORIG    LIKE   ZTREQORJ-ZFORIG,  " 자재원산국..
        LANDX     LIKE   T005T-LANDX,      " 국가이름..
        ZFOPNDT   LIKE   ZTREQST-ZFOPNDT,  " 개설일.
        CDAT      LIKE   ZTREQST-CDAT,     " 생성일.
        END OF IT_RN.

*-----------------------------------------------------------------------
* B/L 번호 조회를 위한 INTERNAL TABLE DECLARATION.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_BL OCCURS 1000,
        ZFBLNO    LIKE   ZTBL-ZFBLNO,      " B/L 관리 번호..
        ZFMSNO    LIKE   ZTBL-ZFMSNO,      " 모선관리번호..
        ZFFORD    LIKE   ZTBL-ZFFORD,      " FORWARDER..
        ZFAPRTC   LIKE   ZTBL-ZFAPRTC,     " 도착항 코드..
        ZFAPRT    LIKE   ZTBL-ZFAPRT,      " 도착항..
        ZFHBLNO   LIKE   ZTBL-ZFHBLNO,     " HOUSE B/L NO..
        ZFREBELN  LIKE   ZTBL-ZFREBELN,    " 대표 P/O NO..
        LIFNR     LIKE   ZTBL-LIFNR,       " ACCOUNT NO..
        ZFOPNNO   LIKE   ZTBL-ZFOPNNO,     " 신용장-승인번호.
        ZFETA     LIKE   ZTBL-ZFETA,       " 도착일(ETD)..
        ZFPOYN    LIKE   ZTBL-ZFPOYN,      " 유환여부..
        ZFRENT    LIKE   ZTBL-ZFRENT,      " 양도 B/L 여부..
        ZFVIA     LIKE   ZTBL-ZFVIA,       " 운송방법.
        INCO1     LIKE   ZTBL-INCO1,       " 인도조건.
        ZFBLIT    LIKE   ZTBLIT-ZFBLIT,    " B/L 품목번호..
        EBELN     LIKE   ZTBLIT-EBELN,     " 구매문서번호..
        EBELP     LIKE   ZTBLIT-EBELP,     " 구매문서 품목번호..
        ZFREQNO   LIKE   ZTBLIT-ZFREQNO,   " 수입의뢰 관리번호..
        ZFITMNO   LIKE   ZTBLIT-ZFITMNO,   " 수입문서 품목번호..
        MATNR     LIKE   ZTBLIT-MATNR,     " 자재번호..
        ZFBLDT    LIKE   ZTBL-ZFBLDT,      " B/L 입수일.
        BLMENGE   LIKE   ZTBLIT-BLMENGE,   " B/L 수량..
        MEINS     LIKE   ZTBLIT-MEINS,     " 기본단위..
      END OF IT_BL.

*-----------------------------------------------------------------------
* 모선관리를 위한 Internal Table 선언..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_MS OCCURS 1000,
        ZFMSNO    LIKE   ZTMSHD-ZFMSNO,
        ZFMSNM    LIKE   ZTMSHD-ZFMSNM,
        ZFREQNO   LIKE   ZTMSIT-ZFREQNO,
        ZFSHSDF   LIKE   ZTMSHD-ZFSHSDF,
        ZFSHSDT   LIKE   ZTMSHD-ZFSHSDT,
      END OF IT_MS.

*-----------------------------------------------------------------------
*  DECLARATION OF INTERNAL TABLE FOR COMMERCIAL INVOICE DATA REFERENCE.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CIV OCCURS 1000,
        ZFCIVRN   LIKE ZTCIVHD-ZFCIVRN,  " COMMERCIAL INVOICE 관리번호..
        ZFCIVNO   LIKE ZTCIVHD-ZFCIVNO,  " COMMERCIAL INVOICE NUMBER..
        ZFMAVN    LIKE ZTCIVHD-ZFMAVN,   " 물대 거래처코드..
        ZFOPBN    LIKE ZTCIVHD-ZFOPBN,   " 개설은행 거래처코드..
        ZFCIDT    LIKE ZTCIVHD-ZFCIDT,   " 인보이스 일자.
        ZFCIVSQ   LIKE ZTCIVIT-ZFCIVSQ,  " COMMERCIAL INVOICE 품목번호..
        EBELN     LIKE ZTCIVIT-EBELN,    " 구매문서번호..
        EBELP     LIKE ZTCIVIT-EBELP,    " 구매문서 품목번호..
        ZFREQNO   LIKE ZTCIVIT-ZFREQNO,  " 수입의뢰 관리번호..
        ZFITMNO   LIKE ZTCIVIT-ZFITMNO,  " 수입문서 품목번호..
        ZFBLNO    LIKE ZTCIVIT-ZFBLNO,   " B/L 관리번호..
        ZFBLIT    LIKE ZTCIVIT-ZFBLIT,   " B/L 품목번호..
        ZFPRQN    LIKE ZTCIVIT-ZFPRQN,   " COMMERCIAL INVOICE 수량..
        MEINS     LIKE ZTCIVIT-MEINS,    " 기본단위..
      END OF IT_CIV.

*-----------------------------------------------------------------------
* 하역항 조회를 위한 INTERNAL TABLE 선언...
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CG OCCURS 1000,
        ZFCGNO    LIKE   ZTCGHD-ZFCGNO,    " 하역관리번호..
        ZFMSNO    LIKE   ZTCGHD-ZFMSNO,    " 모선관리번호..
        ZFARVLDT  LIKE   ZTCGHD-ZFARVLDT,  " 입항일자.
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
* Declaration of Internal Talbe for ZTIDR Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDR  OCCURS 1000.
        INCLUDE STRUCTURE ZTIDR.
*DATA  zfivno  LIKE  ztidrhsd-zfivno.
DATA : ZFIVDNO LIKE  ZTIDRHSD-ZFIVDNO,
       ZFQNT   LIKE  ZTIDRHSD-ZFQNT,
       ZFQNTM  LIKE  ZTIDRHSD-ZFQNTM,
       END OF IT_IDR.

*-----------------------------------------------------------------------
* DECLARATION OF INTERNAL TALBE FOR ZTIDS TABLE REFERENCE..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDS OCCURS 1000.
        INCLUDE STRUCTURE ZTIDS.
DATA :  ZFQNT   LIKE  ZTIDSHSD-ZFQNT,
        ZFQNTM  LIKE  ZTIDSHSD-ZFQNTM,
        ZFIVDNO LIKE  ZTIDSHSD-ZFIVDNO.
DATA: END OF IT_IDS.

*-------------------------------------------------*
* 입고 DATA를 READ 하기 위한 INTERNAL TABLE 선언. *
*-------------------------------------------------*
DATA : BEGIN OF IT_IN OCCURS 1000,
       ZFIVNO    LIKE   ZTIVIT-ZFIVNO,
       ZFIVHST   LIKE   ZTIVHST-ZFIVHST,
       ZFGRST    LIKE   ZTIV-ZFGRST,
       ZFCUNAM   LIKE   ZTIV-ZFCUNAM,
       ZFIVDNO   LIKE   ZTIVIT-ZFIVDNO,
       MATNR     LIKE   ZTIVIT-MATNR,
       GRMENGE   LIKE   ZTIVIT-GRMENGE,
       MEINS     LIKE   ZTIVIT-MEINS,
       WERKS     LIKE   ZTIVIT-WERKS,
       LGORT     LIKE   ZTIVIT-LGORT,
       BUDAT     LIKE   ZTIVHST-BUDAT,
       MBLNR     LIKE   ZTIVHST-MBLNR,
       MJAHR     LIKE   ZTIVHST-MJAHR,
       EBELN     LIKE   EKPO-EBELN,
       EBELP     LIKE   EKPO-EBELP,
       BUKRS     LIKE   ZTIV-BUKRS,
       BSTNK     LIKE   VBAK-BSTNK.
DATA : END OF IT_IN.

*-----------------------------------------------------------------------
* DECLARATION OF INTERNAL TALBE FOR ZTIVIT TABLE REFERENCE..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IVIT OCCURS 1000.
        INCLUDE STRUCTURE ZTIVIT.
DATA:   ZFCUST    LIKE   ZTIV-ZFCUST,
        ZFCCDT    LIKE   ZTIV-ZFCCDT,
        ZFGRST    LIKE   ZTIV-ZFGRST,
        ZFCUNAM   LIKE   ZTIV-ZFCUNAM,
        ZFCUT     LIKE   ZTIV-ZFCUT.
DATA: END OF IT_IVIT.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for VBAK Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SO OCCURS 1000,
        VBELN     LIKE   VBAK-VBELN,
        POSNR     LIKE   VBAP-POSNR,
        BSTNK     LIKE   VBAK-BSTNK,
        ERDAT     LIKE   VBAK-ERDAT,
        KUNNR     LIKE   VBAK-KUNNR,
        SMENG     LIKE   VBAP-SMENG,
        MEINS     LIKE   VBAP-MEINS,
        NETWR     LIKE   VBAP-NETWR,
        WAERK     LIKE   VBAP-WAERK,
      END OF IT_SO.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for LIKP Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_DE OCCURS 1000,
        VBELN     LIKE   LIKP-VBELN,
        POSNR     LIKE   LIPS-POSNR,
        LFDAT     LIKE   LIKP-LFDAT,
        KUNNR     LIKE   LIKP-KUNNR,
        LFIMG     LIKE   LIPS-LFIMG,
        MEINS     LIKE   LIPS-MEINS,
        VGBEL     LIKE   LIPS-VGBEL,
        VGPOS     LIKE   LIPS-VGPOS,
      END OF IT_DE.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for VBRK Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_BI OCCURS 1000,
        VBELN     LIKE   VBRK-VBELN,
        POSNR     LIKE   VBRP-POSNR,
        FKDAT     LIKE   VBRK-FKDAT,
        KUNRG     LIKE   VBRK-KUNRG,
        FKIMG     LIKE   VBRP-FKIMG,
        VRKME     LIKE   VBRP-VRKME,
        VGBEL     LIKE   LIPS-VGBEL,
        VGPOS     LIKE   LIPS-VGPOS,
      END OF IT_BI.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for VBRK Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_GI OCCURS 1000,
        MBLNR     LIKE   MKPF-MBLNR,
        MJAHR     LIKE   MKPF-MJAHR,
        BUDAT     LIKE   MKPF-BUDAT,
        XBLNR     LIKE   MKPF-XBLNR,
      END OF IT_GI.
*-----------------------------------------------------------------------
* DECLARATION OF VARIABLE FOR TABLE USAGE..
*-----------------------------------------------------------------------
DATA: W_TABIX     LIKE SY-TABIX,
      W_BEWTP     LIKE EKBE-BEWTP,
      W_AMOUNT    LIKE ZTREQHD-ZFLASTAM,
      W_ERR_CHK   TYPE C,
      W_TRIPLE(5) TYPE C,
      W_PAGE      TYPE I,
      W_TRANS(15) TYPE C.


*-----------------------------------------------------------------------
* Declaration of Variable for Instead of ON CHANGE OF Function..
*-----------------------------------------------------------------------
DATA: SV_TXZ01    LIKE ZTREQIT-TXZ01,
      SV_MATNR    LIKE ZTREQIT-MATNR,
      w_loop_cnt  TYPE i,
      temp_matnr  LIKE ekpo-matnr,         " 자재번호를 임시로 저장..
      temp_txz01  LIKE ekpo-txz01,         " 자재내역을 임시로 저장..
      temp_ebeln  LIKE ekpo-ebeln,         " P/O 번호를 임시로 저장..
      temp_ebelp  LIKE ekpo-ebelp,         " Item 번호를 임시로 저장..
      temp_reqno  LIKE ztreqhd-zfreqno,    " 수입의뢰관리번호 저장..
      temp_itmno  LIKE ztreqit-zfitmno,    " Item 번호를 임시로 저장..
      temp_blno   LIKE ztbl-zfblno,        " B/L 번호를 임시로 저장..
      temp_blit   LIKE ztblit-zfblit,      " B/L Item 번호 임시저장..
      temp_cgno   LIKE ztcghd-zfcgno,      " 하역관리번호를 임시저장..
      temp_cgit   LIKE ztcgit-zfcgit,      " 하역자재순번을 임시저장..
      temp_ivno   LIKE ztivit-zfivno,      " 통관/입고요청관리번호 저장.
      temp_ivdno  LIKE ztivit-zfivdno.    " Invoice Item 일련번호 저장..

*-----------------------------------------------------------------------
* HIDE VARIABLE.
*-----------------------------------------------------------------------
DATA: BEGIN OF docu,
        type(2)   TYPE c,
        code      LIKE ekko-ebeln,
        itmno     LIKE ekpo-ebelp,
        year      LIKE bkpf-gjahr,
      END OF docu.

*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
selection-screen skip 1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
               S_BUKRS   FOR ZTREQHD-BUKRS,
               s_ebeln   FOR ekko-ebeln,       " P/O No.
               s_matnr   FOR ekpo-matnr,       " 자재 No.
               s_reqno   FOR ztreqhd-zfreqno,  " 수입의뢰 No.
               s_opnno   FOR ztreqhd-zfopnno,  " 신용장-승인번호.
               s_lifnr   FOR ztreqhd-lifnr,    " Vendor.
               S_REQTY   FOR ZTREQHD-ZFREQTY,
               S_TRANS   FOR ZTREQHD-ZFTRANS,
               S_REQSD   FOR ZTREQHD-ZFREQSD,
               S_REQED   FOR ZTREQHD-ZFREQED,
               S_SHCU    FOR ZTREQHD-ZFSHCU.
* PARAMETERS :   P_TRIPLE  AS CHECKBOX.          " 삼국무역여?
SELECTION-SCREEN END OF BLOCK b2.

*.. 삼국무역 여?
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-R01.
  SELECTION-SCREEN : BEGIN OF LINE,
                     COMMENT 01(31) TEXT-R01.
    SELECTION-SCREEN : COMMENT 33(4) TEXT-R04.
    PARAMETERS : p_ALL RADIOBUTTON GROUP RDG.
    SELECTION-SCREEN : COMMENT 52(3) TEXT-R02.
    PARAMETERS : p_YES RADIOBUTTON GROUP RDG.
    SELECTION-SCREEN : COMMENT 70(3) TEXT-R03.
    PARAMETERS : p_NO  RADIOBUTTON GROUP RDG.
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
  PERFORM p3000_title_write.

*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 수입의뢰 No.
  PERFORM P1000_READ_RN_DATA USING W_ERR_CHK.
  CHECK W_ERR_CHK NE 'Y'.

* P/O Table Select..
  PERFORM P1000_READ_PO_DATA.

* B/L Table Select..
  PERFORM P1000_READ_BL_DATA.

* 모선 Table Select..
  PERFORM P1000_READ_MS_DATA.

* Commercial Invoice Table Select..
  PERFORM P1000_READ_CIV_DATA.

* 하역 Table Select..
  PERFORM P1000_READ_CG_DATA.

* 통관요청/입고요청 Table Select..
  PERFORM P1000_READ_IVIT_DATA.

* 수입신고 Table Select..
  PERFORM P1000_READ_ZTIDR_DATA.

* 수입면허 Table Select..
  PERFORM P1000_READ_ZTIDS_DATA.

* NHJ 추가( 2001. 07. 03 )
* 입고 TABLE READ
   PERFORM P1000_READ_IN_DATA.

* SALES ORDER TABLE SELECT.
   PERFORM P1000_READ_SO_DATA.

* DELIVERY TABLE SELECT.
   PERFORM P1000_READ_DE_DATA.

* BILLING TABLE SELECT.
   PERFORM P1000_READ_BI_DATA.

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

  DATA : L_TEXT(20).
  DATA : mm03_start_sicht(15) TYPE c  VALUE 'BDEKLPQSVXZA'.

  GET CURSOR FIELD L_TEXT.
  CASE L_TEXT.   " 필드명..
    WHEN 'IT_SO-VBELN'.
      SET PARAMETER ID 'AUN' FIELD IT_SO-VBELN.
      SET PARAMETER ID 'VAG' FIELD ''.
      SET PARAMETER ID 'VL'  FIELD ''.
      SET PARAMETER ID 'VF'  FIELD ''.
      SET PARAMETER ID 'PRO' FIELD ''.
      CALL TRANSACTION 'VA03' AND SKIP  FIRST SCREEN.
    WHEN 'IT_DE-VBELN'.
      SET PARAMETER ID 'VL'  FIELD IT_DE-VBELN.
      CALL TRANSACTION 'VL03N' AND SKIP  FIRST SCREEN.
    WHEN 'IT_BI-VBELN'.
      SET PARAMETER ID 'VF'  FIELD IT_BI-VBELN.
      SET PARAMETER ID 'BLN' FIELD ''.
      SET PARAMETER ID 'BUK' FIELD ''.
      SET PARAMETER ID 'GJR' FIELD ''.
      CALL TRANSACTION 'VF03' AND SKIP  FIRST SCREEN.
    WHEN 'IT_PO-MATNR' OR 'IT_PO-TXZ01'.
      SET PARAMETER ID 'MAT' FIELD it_po-matnr.
      SET PARAMETER ID 'BUK' FIELD it_po-bukrs.
      SET PARAMETER ID 'WRK' FIELD it_po-werks.
      SET PARAMETER ID 'LAG' FIELD ''.
      SET PARAMETER ID 'MXX' FIELD mm03_start_sicht.
      CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.
    WHEN 'IT_PO-EBELN' OR 'IT_PO-EBELP'.
      SET PARAMETER ID 'BES'  FIELD it_po-ebeln.
      SET PARAMETER ID 'BSP'  FIELD IT_PO-EBELP.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
    WHEN 'IT_RN-ZFOPNNO' OR 'IT_RN-ZFREQNO'.
      SET PARAMETER ID 'ZPREQNO' FIELD it_rn-zfreqno.
      SET PARAMETER ID 'ZPOPNNO' FIELD ''.
      SET PARAMETER ID 'BES'     FIELD ''.
      CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
    WHEN 'IT_BL-ZFHBLNO' OR 'IT_BL-ZFBLNO'.
      SET PARAMETER ID 'ZPBLNO'  FIELD it_bl-zfblno.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
    WHEN 'IT_CG-ZFCGNO'.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      SET PARAMETER ID 'ZPBLNO'  FIELD ''.
      SET PARAMETER ID 'ZPCGNO'  FIELD it_CG-zfCGno.
      CALL TRANSACTION 'ZIM83' AND SKIP FIRST SCREEN.
    WHEN 'IT_IVIT-ZFIVNO'.
      SET PARAMETER ID 'ZPIVNO' FIELD it_ivit-zfivno.
      SET PARAMETER ID 'ZPBLNO' FIELD ''.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      CALL TRANSACTION 'ZIM33' AND SKIP FIRST SCREEN.
    WHEN 'IT_CIV-ZFCIVRN' OR 'IT_CIV-ZFCIVNO'.
      SET PARAMETER ID 'ZPCIVRN' FIELD it_civ-zfcivrn.
      SET PARAMETER ID 'ZPCIVNO' FIELD ''.
      CALL TRANSACTION 'ZIM37' AND SKIP FIRST SCREEN.
    WHEN 'IT_IDR-ZFIDRNO' OR 'IT_IDR-ZFCLSEQ'.
      SET PARAMETER ID 'ZPIDRNO' FIELD it_idr-zfidrno.
      SET PARAMETER ID 'ZPCLSEQ' FIELD it_idr-zfclseq.
      SET PARAMETER ID 'ZPBLNO' FIELD it_idr-zfblno.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      CALL TRANSACTION 'ZIM63' AND SKIP FIRST SCREEN.
    WHEN 'IT_IDS-ZFIDRNO' OR 'IT_IDS-ZFCLSEQ'.
      SET PARAMETER ID 'ZPIDRNO' FIELD it_ids-zfidrno.
      SET PARAMETER ID 'ZPCLSEQ' FIELD it_ids-zfclseq.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      SET PARAMETER ID 'ZPBLNO' FIELD  IT_IDS-ZFBLNO.
      CALL TRANSACTION 'ZIM76' AND SKIP FIRST SCREEN.
*>> NHJ 20001.07.03 입고 DATA DISPLAY
     WHEN  'IT_IN-ZFIVNO' OR 'IT_IN-MBLNR'.
       SET  PARAMETER ID  'BUK'   FIELD   IT_IN-BUKRS.
       SET  PARAMETER ID  'MBN'   FIELD   IT_IN-MBLNR.
       SET  PARAMETER ID  'MJA'   FIELD   IT_IN-MJAHR.
*>> 입고문서 조회 FUNCTION CALL.
       CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              i_action                  = 'A04'
              i_refdoc                  = 'R02'
              i_notree                  = 'X'
*             I_NO_AUTH_CHECK           =
              i_skip_first_screen       = 'X'
*             I_DEADEND                 = 'X'
              i_okcode                  = 'OK_GO'
*             I_LEAVE_AFTER_POST        =
*             i_new_rollarea            = 'X'
*             I_SYTCODE                 =
*             I_EBELN                   =
*             I_EBELP                   =
              i_mblnr                   = IT_IN-MBLNR
              i_mjahr                   = IT_IN-MJAHR
*             I_ZEILE                   =
           EXCEPTIONS
              illegal_combination       = 1
              OTHERS                    = 2.
     WHEN  'MKPF-MBLNR' .
       SET  PARAMETER ID  'BUK'   FIELD   ''.
       SET  PARAMETER ID  'MBN'   FIELD   MKPF-MBLNR.
       SET  PARAMETER ID  'MJA'   FIELD   MKPF-MJAHR.
*>> 입고문서 조회 FUNCTION CALL.
       CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              i_action                  = 'A04'
              i_refdoc                  = 'R02'
              i_notree                  = 'X'
*             I_NO_AUTH_CHECK           =
              i_skip_first_screen       = 'X'
*             I_DEADEND                 = 'X'
              i_okcode                  = 'OK_GO'
*             I_LEAVE_AFTER_POST        =
*             i_new_rollarea            = 'X'
*             I_SYTCODE                 =
*             I_EBELN                   =
*             I_EBELP                   =
              i_mblnr                   = MKPF-MBLNR
              i_mjahr                   = MKPF-MJAHR
*             I_ZEILE                   =
           EXCEPTIONS
              illegal_combination       = 1
              OTHERS                    = 2.

  ENDCASE.
*&------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&------------------------------------------------------------------*
FORM p1000_read_rn_data USING w_err_chk.

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
    ON     H~ZFREQNO  EQ    I~ZFREQNO
    WHERE  H~ZFREQNO  IN    S_REQNO
    AND    H~ZFOPNNO  IN    S_OPNNO
    AND    H~EBELN    IN    S_EBELN
    AND    I~MATNR    IN    S_MATNR
    AND    H~LIFNR    IN    S_LIFNR
    AND    H~ZFREQTY  IN    S_REQTY
    AND    H~ZFTRANS  IN    S_TRANS
    AND    H~BUKRS    IN    S_BUKRS
    AND    H~ZFREQSD  IN    S_REQSD
    AND    H~ZFREQED  IN    S_REQED
    AND    H~ZFSHCU   IN    S_SHCU
    AND    H~ZFTRIPLE LIKE  W_TRIPLE.

    IF SY-SUBRC NE 0.
       W_ERR_CHK = 'Y'.  MESSAGE  S738.
       EXIT.
    ENDIF.

  ENDFORM.                    " P1000_READ_RN_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
FORM p1000_read_po_data.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PO
    FROM   EKKO  AS  H  INNER  JOIN  EKPO  AS I
    ON     H~EBELN      EQ     I~EBELN
    FOR    ALL  ENTRIES IN     IT_RN
    WHERE  H~EBELN      EQ     IT_RN-EBELN
    AND    I~EBELP      EQ     IT_RN-EBELP.

  IF SY-SUBRC NE 0. EXIT. ENDIF.

ENDFORM.                    " P1000_READ_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM p1000_read_ms_data.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MS
    FROM   ZTMSHD AS H INNER JOIN ZTMSIT AS I
    ON     H~ZFMSNO    EQ    I~ZFMSNO
    FOR    ALL ENTRIES IN    IT_RN
    WHERE   I~ZFREQNO  EQ    IT_RN-ZFREQNO.

  SELECT * APPENDING  CORRESPONDING FIELDS OF TABLE IT_MS
    FROM   ZTMSHD AS H INNER JOIN ZTMSIT AS I
    ON     H~ZFMSNO    EQ    I~ZFMSNO
    FOR    ALL ENTRIES IN    IT_BL
    WHERE  H~ZFMSNO    EQ    IT_BL-ZFMSNO.

ENDFORM.                    " P1000_READ_MS_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BL
    FROM   ZTBL AS H   INNER JOIN ZTBLIT AS I
    ON     H~ZFBLNO    EQ    I~ZFBLNO
    FOR    ALL ENTRIES IN    IT_RN
    WHERE  I~ZFREQNO   EQ    IT_RN-ZFREQNO
    AND    I~ZFITMNO   EQ    IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_BL_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_CIV
    FROM   ZTCIVHD  AS  H  INNER  JOIN  ZTCIVIT AS I
    ON     H~ZFCIVRN       EQ     I~ZFCIVRN
    FOR    ALL  ENTRIES    IN     IT_RN
    WHERE  I~ZFREQNO       EQ     IT_RN-ZFREQNO
    AND    I~ZFITMNO       EQ     IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_CIV_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*       Read Cargo Data.
*----------------------------------------------------------------------*
FORM P1000_READ_CG_DATA.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_CG
    FROM   ZTCGHD  AS  H  INNER  JOIN  ZTCGIT AS  I
    ON     H~ZFCGNO       EQ     I~ZFCGNO
    FOR    ALL  ENTRIES   IN     IT_BL
    WHERE  I~ZFBLNO       EQ     IT_BL-ZFBLNO
    AND    I~ZFBLIT       EQ     IT_BL-ZFBLIT.

ENDFORM.                    " P1000_READ_CG_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IVIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_IVIT_DATA.

  DATA : l_tabix LIKE sy-tabix.

  " BL 기준 통관요청 DATA.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IVIT
    FROM   ZTIVIT
    FOR    ALL  ENTRIES  IN  IT_BL
    WHERE  ZFBLNO        EQ  IT_BL-ZFBLNO
    AND    ZFBLIT        EQ  IT_BL-ZFBLIT
    AND  ( ZFCGNO        IS  NULL
    OR     ZFCGNO        EQ  SPACE ).

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE IT_IVIT
    FROM   ZTIVIT
    FOR    ALL  ENTRIES  IN  IT_CG
    WHERE  ZFCGNO        EQ  IT_CG-ZFCGNO
    AND    ZFCGIT        EQ  IT_CG-ZFCGIT.

*>> LOCAL 구매 DATA SELECT!
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE IT_IVIT
    FROM   ZTIV  AS  H   INNER  JOIN  ZTIVIT  AS  I
    ON     H~ZFIVNO      EQ     I~ZFIVNO
    FOR    ALL  ENTRIES  IN     IT_RN
    WHERE  ( H~ZFREQTY   EQ     'LO'
    OR       H~ZFREQTY   EQ     'PU' )
    AND      I~ZFREQNO   EQ     IT_RN-ZFREQNO
    AND      I~ZFITMNO   EQ     IT_RN-ZFITMNO.

  LOOP AT IT_IVIT.
     L_TABIX  =  SY-TABIX.
     SELECT SINGLE ZFCUST ZFGRST
     INTO   (IT_IVIT-ZFCUST, IT_IVIT-ZFGRST)
     FROM   ZTIV
     WHERE  ZFIVNO    EQ  IT_IVIT-ZFIVNO.
     MODIFY  IT_IVIT INDEX  L_TABIX.
  ENDLOOP.
ENDFORM.                    " P1000_READ_IVIT_DATA


*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE: /55 '[자재별 진행현황]'
             COLOR COL_HEADING INTENSIFIED OFF.

  WRITE: / 'DATE: ' ,
            SY-DATUM .

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_IT_DATA
*&---------------------------------------------------------------------*
*       SORTING INTERNAL TABLE..
*----------------------------------------------------------------------*
FORM P2000_SORT_IT_DATA.

  SORT IT_PO BY MATNR EBELN EBELP.

ENDFORM.                    " P2000_SORT_IT_DATA

*----------------------------------------------------------------------*
* 결과화면 조회를 위한 PERFORM 문..
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P3000_WEITE_PO_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_PO_DATA.

  DATA : L_FIRST_LINE   VALUE   'Y',
         L_DATE(10)     TYPE    C.

*>>> 임시변수의 값들을 초기화..
  CLEAR : SV_TXZ01, SV_MATNR.
  SKIP.
  LOOP AT IT_PO.

     " 자재 내역 WRITE.
     IF SV_TXZ01  NE  IT_PO-TXZ01  OR SV_MATNR NE IT_PO-MATNR.

        FORMAT COLOR COL_HEADING INTENSIFIED ON.
        ULINE.
        WRITE: / IT_PO-MATNR NO-GAP, IT_PO-TXZ01 NO-GAP,
                 '                               ' NO-GAP,
                 179 '' NO-GAP.
        HIDE: IT_PO.
        FORMAT RESET.
        MOVE : IT_PO-TXZ01  TO  SV_TXZ01,
               IT_PO-MATNR  TO  SV_MATNR.
      ENDIF.

      " 거래선명 GET.
      CLEAR : LFA1.
      SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ IT_PO-LIFNR.

      " 플랜트명 GET.
      CLEAR : T001W.
      SELECT SINGLE * FROM T001W WHERE WERKS EQ IT_PO-WERKS.

      " 구매그룹명 GET.
      CLEAR : T024.
      SELECT SINGLE * FROM T024 WHERE EKGRP EQ IT_PO-EKGRP.

      " ITEM 별 금액 COMPUTE.
      W_AMOUNT = IT_PO-MENGE * ( IT_PO-NETPR / IT_PO-PEINH ).

      " PO DATA WRITE.
       FORMAT COLOR COL_NORMAL INTENSIFIED ON.

       WRITE :  /(10) '오더'                NO-GAP,
                 (10) IT_PO-EBELN           NO-GAP,
                 (1)  '-'                   NO-GAP,
                 (5)  IT_PO-EBELP           ,
                 (3)  ''                    NO-GAP,
                 (5)  IT_PO-MEINS           NO-GAP,
                 (13) IT_PO-MENGE  UNIT     IT_PO-MEINS  NO-GAP,
                 (19) IT_PO-NETPR  CURRENCY IT_PO-WAERS  NO-GAP,
                 (5)  IT_PO-WAERS           NO-GAP,
                 (19) W_AMOUNT     CURRENCY IT_PO-WAERS  NO-GAP,
                 (10) IT_PO-AEDAT,
                 (10) IT_PO-LIFNR           NO-GAP,
                 (20) LFA1-NAME1            NO-GAP,
                 (4)  IT_PO-WERKS           ,
                 (15) T001W-NAME1           NO-GAP,
                 (3)  IT_PO-EKGRP           ,
                 (22) T024-EKNAM            NO-GAP.
      HIDE: IT_PO.

      " 수입의뢰 DATA WRITE.
      PERFORM P3000_WRITE_RN_DATA.
      CLEAR: IT_PO.

  ENDLOOP.

ENDFORM.                    " P3000_WEITE_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_RN_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_RN_DATA.

  DATA : L_FIRST_LINE   VALUE  'Y',
         L_DATE_F(10)   ,
         L_DATE_T(10).

  LOOP AT IT_RN WHERE  EBELN  =  IT_PO-EBELN
                AND    EBELP  =  IT_PO-EBELP.

     " 모선명 GET.
     CLEAR : IT_MS.
     READ TABLE IT_MS WITH KEY ZFREQNO = IT_RN-ZFREQNO.

     " 개설일 SET!
     CLEAR : ZTREQST.
     SELECT SINGLE * FROM ZTREQST
            WHERE  ZFREQNO  EQ  IT_RN-ZFREQNO
            AND    ZFAMDNO  EQ  ( SELECT MAX( ZFAMDNO )
                                  FROM   ZTREQST
                                  WHERE  ZFREQNO  EQ  IT_RN-ZFREQNO ).
     " 운송방법명 DISPLAY
     CASE IT_RN-ZFTRANS.
        WHEN 'A'.
           MOVE 'AIR  ' TO W_TRANS.
        WHEN 'O'.
           MOVE 'OCEAN' TO W_TRANS.
     ENDCASE.

     " 개설은행명 DISPLAY
     CLEAR LFA1.
     SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ IT_RN-ZFOPBN.

     FORMAT RESET.
     WRITE :  /(10) '개설'                   NO-GAP,
               (10) IT_RN-ZFREQNO            NO-GAP,
               (1)  '-'                      NO-GAP,
               (5)  IT_RN-ZFITMNO            NO-GAP,
               (5)  '  '                     ,
               (3)  '     '                  NO-GAP,
               (13) IT_RN-MENGE UNIT IT_RN-MEINS  NO-GAP,
               (18) W_TRANS RIGHT-JUSTIFIED  ,
               (9)  IT_RN-INCO1              NO-GAP,
               (14)  ''                      ,
               (10) ZTREQST-ZFOPNDT          ,
               (10) IT_RN-ZFOPBN             NO-GAP,
               (20) LFA1-NAME1               NO-GAP,
               (20) IT_RN-ZFOPNNO            NO-GAP,
               (19) IT_MS-ZFMSNM             NO-GAP.
     HIDE: IT_RN.
     PERFORM P3000_WRITE_CIV_DATA.
     PERFORM P3000_WRITE_LO_IVIT_DATA.
     PERFORM P3000_WRITE_BL_DATA.
     CLEAR: IT_RN.

  ENDLOOP.

ENDFORM.                    " P3000_WRITE_RN_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*       Subroutine for Writing Commercial Invoice Data.
*----------------------------------------------------------------------*
FORM P3000_WRITE_CIV_DATA.

  LOOP AT IT_CIV WHERE ZFREQNO = IT_RN-ZFREQNO
                 AND   ZFITMNO = IT_RN-ZFITMNO.

     FORMAT RESET.

     WRITE :  /(10) '송장'                NO-GAP,
               (10) IT_CIV-ZFCIVRN        NO-GAP,
               (1)  '-'                   NO-GAP,
               (5)  IT_CIV-ZFCIVSQ        NO-GAP,
               (9)  ' '                   NO-GAP,
               (13) IT_CIV-ZFPRQN  UNIT   IT_CIV-MEINS  NO-GAP,
               (43)  ' '                  NO-GAP,
               (10) IT_CIV-ZFCIDT         ,
               (35) IT_CIV-ZFCIVNO  UNDER IT_RN-ZFOPNNO .
      HIDE: IT_CIV.

  ENDLOOP.

ENDFORM.                    " P3000_WRITE_CIV_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_BL_DATA
*&---------------------------------------------------------------------*
*        Write Bill of Lading Data.
*----------------------------------------------------------------------*
FORM P3000_WRITE_BL_DATA.

  LOOP AT IT_BL WHERE ZFREQNO = IT_RN-ZFREQNO
                AND   ZFITMNO = IT_RN-ZFITMNO.

     " 선사명 SET.
     CLEAR : LFA1.
     SELECT SINGLE * FROM LFA1 WHERE LIFNR  EQ  IT_RN-LIFNR.

     FORMAT RESET.
     WRITE :  /(10) 'B/L'                      ,
             11(10) IT_BL-ZFBLNO           NO-GAP,
                (1)  '-'                   NO-GAP,
                (5)  IT_BL-ZFBLIT          NO-GAP,
              30(5)  '     '               NO-GAP,
              36(13) IT_BL-BLMENGE  UNIT   IT_BL-MEINS  NO-GAP,
              92(10) IT_BL-ZFBLDT          NO-GAP,
             103(10) IT_BL-ZFFORD          NO-GAP,
             113(20) LFA1-NAME1            NO-GAP,
             133(35) IT_BL-ZFHBLNO         NO-GAP.
     HIDE: IT_BL.
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
FORM p3000_write_bl_add_data.

  IF it_bl-zfpoyn EQ 'Y'.
    WRITE: 95 '유환'.
  ELSE.
    WRITE: 95 '무환'.
  ENDIF.

  IF it_bl-zfrent EQ 'X'.
    WRITE: 102 '양도 B/L'.
  ENDIF.

ENDFORM.                    " P3000_WRITE_BL_ADD_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CG_DATA
*&---------------------------------------------------------------------*
*       하역관련 Data를 출력..
*----------------------------------------------------------------------*
FORM P3000_WRITE_CG_DATA.

  LOOP AT IT_CG WHERE ZFBLNO = IT_BL-ZFBLNO
                AND   ZFBLIT = IT_BL-ZFBLIT.

    FORMAT RESET.
    WRITE :  /(10) '하역'                    ,
            11(10) IT_CG-ZFCGNO          NO-GAP,
              (1)  '-'                   NO-GAP,
              (5)  IT_CG-ZFCGIT          NO-GAP,
            36(13) IT_CG-CGMENGE    UNIT IT_CG-MEINS  NO-GAP,
            92(10) IT_CG-ZFARVLDT        NO-GAP.
    HIDE: IT_CG.
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

  IF IT_IVIT-ZFCGNO = ''.

     " 하역없는 통관일경우.
     LOOP AT IT_IVIT WHERE ZFBLNO = IT_BL-ZFBLNO
                     AND   ZFBLIT = IT_BL-ZFBLIT
                     AND   ZFCGNO = SPACE.

        " 관세사명 GET.
        CLEAR : ZTIMIMG10, LFA1.
        SELECT SINGLE * FROM ZTIMIMG10 WHERE ZFCUT EQ IT_IVIT-ZFCUT.
        SELECT SINGLE * FROM LFA1      WHERE LIFNR EQ ZTIMIMG10-ZFVEN.

        FORMAT RESET.
        WRITE :  /(10) '통관요청'                  ,
                11(10) IT_IVIT-ZFIVNO        NO-GAP,
                  (1)  '-'                   NO-GAP,
                  (5)  IT_IVIT-ZFIVDNO       NO-GAP,
                36(13) IT_IVIT-CCMENGE  UNIT IT_IVIT-MEINS  NO-GAP,
                92(10) IT_IVIT-ZFCCDT        NO-GAP,
               103(10) IT_IVIT-ZFCUT         NO-GAP,
               114(20) LFA1-NAME1            NO-GAP.
        HIDE: IT_IVIT.
*>> 통관상태에 따라서 통관자료 WRITE
       IF IT_IVIT-ZFCUST EQ '2'
       OR IT_IVIT-ZFCUST EQ '3'
       OR IT_IVIT-ZFCUST EQ 'Y'.
          PERFORM P3000_WRITE_IDR_DATA.
       ENDIF.

    ENDLOOP.
  ELSE.
    " 하역있는 통관자료 SET.
     LOOP AT IT_IVIT WHERE ZFCGNO = IT_CG-ZFCGNO
                       AND ZFCGIT = IT_CG-ZFCGIT.

        " 관세사명 GET.
        CLEAR : ZTIMIMG10, LFA1.
        SELECT SINGLE * FROM ZTIMIMG10 WHERE ZFCUT EQ IT_IVIT-ZFCUT.
        SELECT SINGLE * FROM LFA1      WHERE LIFNR EQ ZTIMIMG10-ZFVEN.

        FORMAT RESET.
        WRITE :  /(10) '통관요청'                  ,
                11(10) IT_IVIT-ZFIVNO        NO-GAP,
                  (1)  '-'                   NO-GAP,
                  (5)  IT_IVIT-ZFIVDNO       NO-GAP,
                36(13) IT_IVIT-CCMENGE  UNIT IT_IVIT-MEINS  NO-GAP,
                92(10) IT_IVIT-ZFCCDT        NO-GAP,
               103(10) IT_IVIT-ZFCUT         NO-GAP,
               114(20) LFA1-NAME1            NO-GAP.
        HIDE: IT_IVIT.
*>> 통관상태에 따라서 통관자료 WRITE
       IF IT_IVIT-ZFCUST EQ '2'
       OR IT_IVIT-ZFCUST EQ '3'
       OR IT_IVIT-ZFCUST EQ 'Y'.
          PERFORM P3000_WRITE_IDR_DATA.
       ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P3000_WRITE_IVIT_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IDR_DATA
*&---------------------------------------------------------------------*
*       수입신고/면허 관련 데이터를 출력..
*----------------------------------------------------------------------*
FORM P3000_WRITE_IDR_DATA.

  IF IT_IVIT-ZFCUST EQ 'Y'.

     READ TABLE IT_IDS WITH KEY ZFIVNO  = IT_IVIT-ZFIVNO
                                ZFIVDNO = IT_IVIT-ZFIVDNO.
* 2002/08/05 Nashinho Insert.
*     " 세관 SET!
*     CLEAR : ZV_COTM.
*     SELECT SINGLE * FROM ZV_COTM WHERE ZFCOTM EQ IT_IDS-ZFINRC.

     FORMAT RESET.
     WRITE :  /(10) '통관'                     ,
             11(10) IT_IDS-ZFIVNO         NO-GAP,
               (1)  '-'                   NO-GAP,
               (5)  IT_IDS-ZFIVDNO        NO-GAP,
             36(13) IT_IDS-ZFQNT     UNIT IT_IDS-ZFQNTM  NO-GAP,
             92(10) IT_IDS-ZFIDSDT        NO-GAP,
            103(10) IT_IDS-ZFINRC         NO-GAP,
* 2002/08/05 Nashinho Insert.
*            114(20) ZV_COTM-NAME1         NO-GAP,
            133(20) IT_IDS-ZFIDRNO        NO-GAP.

     HIDE: IT_IDS.
     " 입고 DATA WRITE.
     LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IVIT-ZFIVNO
                      AND    ZFIVDNO EQ  IT_IVIT-ZFIVDNO.
        PERFORM  P3000_WRITE_IN_DATA.
     ENDLOOP.
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

  IF IT_RN-ZFREQTY EQ 'LO' OR IT_RN-ZFREQTY EQ 'PU'.

     LOOP  AT  IT_IVIT WHERE  ZFREQNO  =  IT_RN-ZFREQNO
                       AND    ZFITMNO  =  IT_RN-ZFITMNO.

        LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IVIT-ZFIVNO
                         AND    ZFIVDNO EQ  IT_IVIT-ZFIVDNO.
           PERFORM  P3000_WRITE_IN_DATA.
        ENDLOOP.

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

   LOOP  AT  IT_IN.
      W_TABIX = SY-TABIX.
      MOVE : IT_IN-MBLNR  TO  IT_IN-BSTNK.
      MODIFY  IT_IN  INDEX  W_TABIX.
   ENDLOOP.

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

   " 스토리지 LOCATION GET.
   CLEAR : T001L.
   SELECT SINGLE * FROM T001L WHERE LGORT EQ IT_IN-LGORT
                              AND   WERKS EQ IT_IN-WERKS.

   " PLANT GET
   CLEAR : T001W.
   SELECT SINGLE * FROM T001W WHERE WERKS EQ IT_IN-WERKS.

   FORMAT RESET.
   WRITE :  /(10) '입고'                 NO-GAP,
             (10) IT_IN-ZFIVNO           NO-GAP,
             (01)  '-'                   NO-GAP,
             (05) IT_IN-ZFIVDNO          NO-GAP,
             (09) ''                     NO-GAP,
           36(13) IT_IN-GRMENGE  UNIT    IT_IN-MEINS,
           92(10) IT_IN-BUDAT            NO-GAP,
          103(10) IT_IN-ZFCUNAM          NO-GAP,
          133(20) IT_IN-MBLNR            NO-GAP.
   HIDE: IT_IN.

   " SALES ORDER DATA WRITE.
   LOOP  AT  IT_SO  WHERE  BSTNK   EQ  IT_IN-MBLNR.
      PERFORM P3000_WRITE_SO_DATA.
   ENDLOOP.

ENDFORM.                    " P3000_WRITE_IN_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIDR_DATA.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IDR
  FROM     ZTIDR  AS  H  INNER  JOIN  ZTIDRHSD  AS  I
  ON       H~ZFBLNO      EQ     I~ZFBLNO
  AND      H~ZFCLSEQ     EQ     I~ZFCLSEQ
  FOR      ALL ENTRIES   IN     IT_IVIT
  WHERE    I~ZFIVNO      EQ     IT_IVIT-ZFIVNO
  AND      I~ZFIVDNO     EQ     IT_IVIT-ZFIVDNO.

ENDFORM.                    " P1000_READ_ZTIDR_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIDS_DATA.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IDS
    FROM   ZTIDS  AS  H  INNER  JOIN  ZTIDSHSD  AS  I
    ON     H~ZFBLNO      EQ     I~ZFBLNO
    AND    H~ZFCLSEQ     EQ     I~ZFCLSEQ
    FOR    ALL ENTRIES   IN     IT_IDR
    WHERE  H~ZFBLNO      EQ     IT_IDR-ZFBLNO
    AND    H~ZFCLSEQ     EQ     IT_IDR-ZFCLSEQ.

ENDFORM.                    " P1000_READ_ZTIDS_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_SO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_SO_DATA.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_SO
    FROM   VBAK  AS  H  INNER JOIN  VBAP  AS  I
    ON     H~VBELN      EQ     I~VBELN
    FOR    ALL  ENTRIES IN     IT_IN
    WHERE  H~BSTNK      EQ     IT_IN-BSTNK.

ENDFORM.                    " P1000_READ_SO_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_DE_DATA.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DE
    FROM   LIKP  AS  H  INNER  JOIN  LIPS  AS  I
    ON     H~VBELN      EQ     I~VBELN
    FOR    ALL ENTRIES  IN     IT_SO
    WHERE  I~VGBEL      EQ     IT_SO-VBELN
    AND    I~VGPOS      EQ     IT_SO-POSNR.

ENDFORM.                    " P1000_READ_DE_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BI_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_BI_DATA.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BI
    FROM   VBRK  AS  H  INNER  JOIN  VBRP  AS  I
    ON     H~VBELN      EQ     I~VBELN
    FOR    ALL ENTRIES  IN     IT_DE
    WHERE  I~VGBEL      EQ     IT_DE-VBELN
    AND    I~VGPOS      EQ     IT_DE-POSNR
    AND    I~VGTYP      EQ     'J'.

ENDFORM.                    " P1000_READ_BI_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_SO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_SO_DATA.

   " 고객명 GET.
   CLEAR : KNA1.
   SELECT SINGLE * FROM KNA1 WHERE KUNNR EQ IT_SO-KUNNR.

   FORMAT RESET.
   WRITE :  /(10) 'S/O'                      ,
           11(10) IT_SO-VBELN            NO-GAP,
              (1)  '-'                   NO-GAP,
              (5) IT_SO-POSNR            NO-GAP,
            30(5)  '     '               NO-GAP,
            36(13) IT_SO-SMENG   UNIT    IT_SO-MEINS  NO-GAP,
            92(10) IT_SO-ERDAT           NO-GAP,
           103(10) IT_SO-KUNNR           NO-GAP,
           114(20) KNA1-NAME1            NO-GAP.
   HIDE: IT_SO.

   " DELIVERY DATA WRITE.
   LOOP AT IT_DE WHERE VGBEL EQ IT_SO-VBELN
                 AND   VGPOS EQ IT_SO-POSNR.
      PERFORM P3000_WRITE_DE_DATA.
   ENDLOOP.

ENDFORM.                    " P3000_WRITE_SO_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_DE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_DE_DATA.

   " 고객명 GET.
   CLEAR : KNA1.
   SELECT SINGLE * FROM KNA1 WHERE KUNNR EQ IT_DE-KUNNR.

   FORMAT RESET.
   WRITE :  /(10) 'Delivery'                   ,
           11(10) IT_DE-VBELN            NO-GAP,
              (1)  '-'                   NO-GAP,
              (5) IT_DE-POSNR            NO-GAP,
            30(5)  '     '               NO-GAP,
            36(13) IT_DE-LFIMG  UNIT    IT_DE-MEINS  NO-GAP,
            92(10) IT_DE-LFDAT           NO-GAP,
           103(10) IT_DE-KUNNR           NO-GAP,
           114(20) KNA1-NAME1            NO-GAP.
   HIDE: IT_DE.

   " BILLING DATA WRITE.
   LOOP AT IT_BI WHERE VGBEL EQ IT_DE-VBELN
                 AND   VGPOS EQ IT_DE-POSNR.
      PERFORM P3000_WRITE_BI_DATA.
   ENDLOOP.

   " GOOD ISSUE DATA WRITE.
   SELECT * FROM MKPF WHERE XBLNR EQ IT_DE-VBELN.
      WRITE :  /(10) 'G/I'                   ,
              11(10) MKPF-MBLNR            NO-GAP,
              92(10) MKPF-BUDAT            NO-GAP,
             103(20) MKPF-USNAM            NO-GAP.
      HIDE: MKPF.
  ENDSELECT.

ENDFORM.                    " P3000_WRITE_DE_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_BI_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_BI_DATA.

   " 고객명 GET.
   CLEAR : KNA1.
   SELECT SINGLE * FROM KNA1 WHERE KUNNR EQ IT_BI-KUNRG.

   FORMAT RESET.
   WRITE :  /(10) 'Billing'                   ,
           11(10) IT_BI-VBELN            NO-GAP,
              (1)  '-'                   NO-GAP,
              (5) IT_BI-POSNR            NO-GAP,
            30(5)  '     '               NO-GAP,
            36(13) IT_BI-FKIMG  UNIT    IT_BI-VRKME  NO-GAP,
            92(10) IT_BI-FKDAT           NO-GAP,
           103(10) IT_BI-KUNRG           NO-GAP,
           114(20) KNA1-NAME1            NO-GAP.
   HIDE: IT_BI.

ENDFORM.                    " P3000_WRITE_BI_DATA
