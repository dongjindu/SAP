*&---------------------------------------------------------------------*
*& Report ZRIMMAT01                                                    *
*&---------------------------------------------------------------------*
*&  프로그램명 : 미착재고 현황                                         *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2001.01.26                                            *
*&---------------------------------------------------------------------*
*&   DESC. : 1. 수입의뢰 건별로 미착재고 현황을 조회한다.              *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT ZRIMMAT01 NO STANDARD PAGE HEADING MESSAGE-ID ZIM.

*-----------------------------------------------------------------------
* 사용 TABLE DECLARE
*-----------------------------------------------------------------------
TABLES: EKKO,                " ABAP Standard Header Table..
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
* PO 정보 INTERNAL TABLE DECLARE
*-----------------------------------------------------------------------
DATA:   BEGIN OF IT_PO OCCURS 1000,       " Internal Table IT_PO..
          EBELN    LIKE   EKKO-EBELN,     " P/O Header No..
          BSART    LIKE   EKKO-BSART,     " Purchasing Document Type..
          LOEKZ    LIKE   EKKO-LOEKZ,     " Deletion indicator..
          ERNAM    LIKE   EKKO-ERNAM,     " Creator..
          LIFNR    LIKE   EKKO-LIFNR,     " Vendor's Account No..
          ZTERM    LIKE   EKKO-ZTERM,     " Terms of Payment Key..
          EKORG    LIKE   EKKO-EKORG,     " Purchasing Organization..
          EKGRP    LIKE   EKKO-EKGRP,     " Purchasing Group..
          WAERS    LIKE   EKKO-WAERS,     " Current Key..
          BEDAT    LIKE   EKKO-BEDAT,     " Purchasing Document Data..
          LLIEF    LIKE   EKKO-LLIEF,     " Supplying Vensor..
          INCO1    LIKE   EKKO-INCO1,     " Incoterms (Part1)..
          LIFRE    LIKE   EKKO-LIFRE,     " Difference Invoicing Party.
          EBELP    LIKE   EKPO-EBELP,     " P/O Item No..
          TXZ01    LIKE   EKPO-TXZ01,     " Short Text..
          MATNR    LIKE   EKPO-MATNR,     " Material No..
          WERKS    LIKE   EKPO-WERKS,     " Plant..
          LGORT    LIKE   EKPO-LGORT,     " Storage Location..
          MENGE    LIKE   EKPO-MENGE,     " Purchase Order Quantity..
          MEINS    LIKE   EKPO-MEINS,     " Order Unit..
          BPRME    LIKE   EKPO-BPRME,     " Order Price Unit..
          NETPR    LIKE   EKPO-NETPR,     " Net Price..
          PEINH    LIKE   EKPO-PEINH,     " Price Unit..
          ELIKZ    LIKE   EKPO-ELIKZ,     " Delivery Comience Indicator

        END OF IT_PO.
