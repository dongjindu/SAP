*----------------------------------------------------------------------*
*   INCLUDE ZRIMSTCVLMTOP                                              *
*----------------------------------------------------------------------*
tables: EKKO,                " ABAP Standard Header Table..
        EKPO,                " ABAP Standard Item Table..
        LIKP,                " Inbound Delivery Header
        ZTREQHD,             " Import Request Header Table..
        ZTREQST,             " Import Request Status Table..
        ZTREQIT,             " Import Request Item Table..
        ZTIV,                " Invoice Table..
        ZTIVIT,              " Invoice Item Table..
        ZTIVHST,             " INVOICE HISTORY TABLE
        ZTBL,                " B/L Table -ZTIVIT Table이 Item Table..
        ZTBLIT,              " B/L Item Table
        ZTIMIMG00,           " Import System Basic Config.
        LFA1,                " Vendor Master Table..
        ZTIDS,               " Customs Clearance Table..
        ZTIDSUS,             " Customs Clearance
        T001L,               " Storage Location Name
        T001W,               " Plant Name
        EKET.                " Delivery Date Table..

*------------------------------------------*
*Payment terms INTERNAL TABLE              *
*------------------------------------------*
DATA: BEGIN OF IT_IMG01 OCCURS 1000,
         BSART     LIKE   ZTIMIMG01-BSART,
         BSTYP     LIKE   ZTIMIMG01-BSTYP,
         ZTERM     LIKE   ZTIMIMG01-ZTERM,
         ZFREQTY   LIKE   ZTIMIMG01-ZFREQTY,
         ZFAPLDT   LIKE   ZTIMIMG01-ZFAPLDT,
      END OF IT_IMG01.

*------------------------------------------*
* P/O INTERNAL TABLE                       *
*------------------------------------------*
DATA: BEGIN OF IT_PO OCCURS 1000,
        WAERS      LIKE   EKKO-WAERS,
        WERKS      LIKE   EKPO-WERKS,
        EKGRP      LIKE   EKKO-EKGRP,
        LIFNR      LIKE   EKKO-LIFNR,
        MATKL      LIKE   EKPO-MATKL,
        MATNR      LIKE   EKPO-MATNR,
        TXZ01      LIKE   EKPO-TXZ01,
        EBELN      LIKE   EKPO-EBELN,
        EBELP      LIKE   EKPO-EBELP,
        MENGE      LIKE   EKPO-MENGE,
        MEINS      LIKE   EKPO-MEINS,
        NETPR      LIKE   EKPO-NETPR,
        BPUMZ      LIKE   EKPO-BPUMZ,
        BPUMN      LIKE   EKPO-BPUMN,
        PEINH      LIKE   EKPO-PEINH,
        AEDAT      LIKE   EKKO-AEDAT,
        ZTERM      LIKE   EKKO-ZTERM,
        ZFAMT      LIKE   ZTIDRHSD-ZFAMT,
      END OF IT_PO.
DATA : IT_LCTAB2   LIKE TABLE OF IT_PO WITH HEADER LINE.
*------------------------------------------*
* MAIN LIST Internal Table
*-------------------------------------------*
DATA: BEGIN OF IT_LCTAB1 OCCURS 0,
        WAERS      LIKE   EKKO-WAERS,
        WERKS      LIKE   EKPO-WERKS,        " Plant
        EKGRP      LIKE   EKKO-EKGRP,        " Purch. Grp.
        LIFNR      LIKE   EKKO-LIFNR,        " VENDER.
        MATKL      LIKE   EKPO-MATKL,        " Material Group.
        MATNR      LIKE   EKPO-MATNR,        " Material No..
        MEINS      LIKE   EKPO-MEINS,        " Order Unit..
        TXZ01      LIKE   EKPO-TXZ01,        " Text
        EBELN      LIKE   EKPO-EBELN,        " P/O Header No..
        EBELP      LIKE   EKPO-EBELP,        " P/O Item No..
        MENGE      LIKE   EKPO-MENGE,        " Purchase Order Quantity..
        ZFAMT      LIKE   ZTIDRHSD-ZFAMT,    " Amount
      END OF IT_LCTAB1.
DATA : IT_OPTAB1 LIKE TABLE OF IT_LCTAB1 WITH HEADER LINE.
DATA : IT_BLTAB1 LIKE TABLE OF IT_LCTAB1 WITH HEADER LINE.
DATA : IT_TRTAB1 LIKE TABLE OF IT_LCTAB1 WITH HEADER LINE.
DATA : IT_STTAB1 LIKE TABLE OF IT_LCTAB1 WITH HEADER LINE.
DATA : IT_GRTAB1 LIKE TABLE OF IT_LCTAB1 WITH HEADER LINE.

*-----------------------------------------------*
* Import Request Data INTERNAL TABLE            *
*-----------------------------------------------*
DATA: BEGIN OF IT_RN OCCURS 1000,
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,
        ZFREQTY   LIKE   ZTREQHD-ZFREQTY,
        EBELN     LIKE   ZTREQIT-EBELN,
        EBELP     LIKE   ZTREQIT-EBELP,
        MATNR     LIKE   ZTREQIT-MATNR,
        TXZ01     LIKE   ZTREQIT-TXZ01,
        MENGE     LIKE   ZTREQIT-MENGE,
        MEINS     LIKE   ZTREQIT-MEINS,
        WAERS     LIKE   ZTREQHD-WAERS,
        LIFNR     LIKE   ZTREQHD-LIFNR,
        ZFOPNNO   LIKE   ZTREQST-ZFOPNNO,
        ZFOPNDT   LIKE   ZTREQST-ZFOPNDT,
        ZFOPBN    LIKE   ZTREQHD-ZFOPBN,
        ZTERM     LIKE   ZTREQHD-ZTERM ,
        EKGRP     LIKE   ZTREQST-EKGRP,
        WERKS     LIKE   EKPO-WERKS,
        MATKL     LIKE   EKPO-MATKL,
        ZFAMT     LIKE   ZTIDRHSD-ZFAMT,
   END OF IT_RN.
DATA : IT_OPTAB2 LIKE TABLE OF IT_RN WITH HEADER LINE.

*-----------------------------------------------------------------------
* B/L Internal Table Declaration.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_BL OCCURS 1000,
        ZFBLNO    LIKE   ZTBL-ZFBLNO,
        ZFBLIT    LIKE   ZTBLIT-ZFBLIT,
        EBELN     LIKE   ZTBLIT-EBELN,
        EBELP     LIKE   ZTBLIT-EBELP,
        MATNR     LIKE   ZTBLIT-MATNR,
        TXZ01     LIKE   ZTBLIT-TXZ01,
        BLMENGE   LIKE   ZTBLIT-BLMENGE,
        MEINS     LIKE   ZTBLIT-MEINS,
        WERKS     LIKE   ZTBLIT-WERKS,
        MATKL     LIKE   ZTBLIT-MATKL,
        LIFNR     LIKE   ZTBL-LIFNR,
        ZFHBLNO   LIKE   ZTBL-ZFHBLNO,
        CDAT      LIKE   ZTBL-CDAT,
        ZFETD     LIKE   ZTBL-ZFETD,
        ZFETA     LIKE   ZTBL-ZFETA,
        ZFRETA    LIKE   ZTBL-ZFRETA,
        ZFSPRT    LIKE   ZTBL-ZFSPRT,
        ZFAPRT    LIKE   ZTBL-ZFAPRT,
        ZF20FT    LIKE   ZTBL-ZF20FT,
        ZF40FT    LIKE   ZTBL-ZF40FT,
        ZFREQNO   LIKE   ZTBLIT-ZFREQNO,
        ZFITMNO   LIKE   ZTBLIT-ZFITMNO,
        WAERS     LIKE   EKKO-WAERS,
        EKGRP     LIKE   EKKO-EKGRP,
        ZFAMT     LIKE   ZTIDRHSD-ZFAMT,
      END OF IT_BL.
DATA : IT_BLTAB2 LIKE TABLE OF IT_BL WITH HEADER LINE.
DATA : IT_IB     LIKE TABLE OF IT_BL WITH HEADER LINE.
DATA : IT_TRTAB2 LIKE TABLE OF IT_BL WITH HEADER LINE.
*-----------------------------------------------------------------------
* B/L Internal Table Declaration.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_TR OCCURS 1000,
        ZFBLNO    LIKE   ZTBLINR-ZFBLNO,
        ZFBLIT    LIKE   ZTBLIT-ZFBLIT,
        ZFBTSEQ   LIKE   ZTBLINR-ZFBTSEQ,
        EBELN     LIKE   ZTBLIT-EBELN,
        EBELP     LIKE   ZTBLIT-EBELP,
        MATNR     LIKE   ZTBLIT-MATNR,
        TXZ01     LIKE   ZTBLIT-TXZ01,
        BLMENGE   LIKE   ZTBLIT-BLMENGE,
        MEINS     LIKE   ZTBLIT-MEINS,
        WERKS     LIKE   ZTBLIT-WERKS,
        MATKL     LIKE   ZTBLIT-MATKL,
        ZFINDT    LIKE   ZTBLINR-ZFINDT,
        ZFBNARCD  LIKE   ZTBLINR-ZFBNARCD,
        ZFHBLNO   LIKE   ZTBL-ZFHBLNO,
        ZF20FT    LIKE   ZTBL-ZF20FT,
        ZF40FT    LIKE   ZTBL-ZF40FT,
        ZFBNARM   LIKE   ZTIMIMG03-ZFBNARM,
        WAERS     LIKE   EKKO-WAERS,
        EKGRP     LIKE   EKKO-EKGRP,
        LIFNR     LIKE   EKKO-LIFNR,
        ZFAMT     LIKE   ZTIDRHSD-ZFAMT,
      END OF IT_TR.
DATA : IT_STTAB2 LIKE TABLE OF IT_TR WITH HEADER LINE.
*-----------------------------------------------*
* INVOICE INTERNAL TABLE                        *
*-----------------------------------------------*
DATA: BEGIN OF IT_IDS OCCURS 1000,
        ZFIVNO    LIKE   ZTIV-ZFIVNO,
        ZFIVDNO   LIKE   ZTIVIT-ZFIVDNO,
        EBELN     LIKE   ZTIVIT-EBELN,
        EBELP     LIKE   ZTIVIT-EBELP,
        MATNR     LIKE   ZTIVIT-MATNR,
        TXZ01     LIKE   ZTIVIT-TXZ01,
        CCMENGE   LIKE   ZTIVIT-CCMENGE,
        MEINS     LIKE   ZTIVIT-MEINS,
        WERKS     LIKE   ZTIVIT-WERKS,
        LIFNR     LIKE   ZTIV-LIFNR,
        ZFCUST    LIKE   ZTIV-ZFCUST,
        ZFENTNO   LIKE   ZTIDSUS-ZFENTNO,
        ZFEDT     LIKE   ZTIDSUS-ZFEDT,
        ZFBLNO    LIKE   ZTIVIT-ZFBLNO,
        ZFBLIT    LIKE   ZTIVIT-ZFBLIT,
        ZFHBLNO   LIKE   ZTBL-ZFHBLNO,
        WAERS     LIKE   EKKO-WAERS,
        EKGRP     LIKE   EKKO-EKGRP,
        MATKL     LIKE   EKPO-MATKL,
        ZFAMT     LIKE   ZTIDRHSD-ZFAMT,
      END OF IT_IDS.
DATA : IT_GRTAB2 LIKE TABLE OF IT_IDS WITH HEADER LINE.
*-----------------------------------------------*
* G/R Data INTERNAL TABLE                       *
*-----------------------------------------------*
DATA: BEGIN OF IT_GR OCCURS 1000,
        ZFIVNO    LIKE   ZTIVHST-ZFIVNO,
        ZFIVHST   LIKE   ZTIVHST-ZFIVHST,
        ZFGRST    LIKE   ZTIVHST-ZFGRST,
        ZFCIVHST  LIKE   ZTIVHST-ZFCIVHST,
        BUDAT     LIKE   ZTIVHST-BUDAT,
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
      END OF IT_GR.

*------------------------------------------*
* WRITE  DATA INTERNAL TABLE               *
*------------------------------------------*
DATA: BEGIN OF IT_TAB OCCURS 0,
        WERKS      LIKE   EKPO-WERKS,        " Plant
        EKGRP      LIKE   EKKO-EKGRP,        " Purch. Grp.
        LIFNR      LIKE   EKKO-LIFNR,        " VENDER.
        MATKL      LIKE   EKPO-MATKL,        " Material Group
        MATNR      LIKE   EKPO-MATNR,        " Material No..
        TXZ01      LIKE   EKPO-TXZ01,        " 자재명.
        MEINS      LIKE   EKPO-MEINS,        " Order Unit..
        LC_MENGE   LIKE   EKPO-MENGE,        " LC-Qty
        BL_MENGE   LIKE   EKPO-MENGE,        " BL-Qty
        IB_MENGE   LIKE   EKPO-MENGE,        " Arrival-Qty
        TR_MENGE   LIKE   EKPO-MENGE,        " In-land Tras-Qty.
        ST_MENGE   LIKE   EKPO-MENGE,        " Clearance Qty
        GR_MENGE   LIKE   EKPO-MENGE,        " GR Qty
      END OF IT_TAB.
DATA: W_TABIX         LIKE SY-TABIX,
      W_PO_HD_ST      LIKE ZVEKKO_REQHD_ST,
      W_ERR_CHK       TYPE C,
      W_COUNT         TYPE I,
      W_BL_CNT        TYPE I,
      W_TR_CNT        TYPE I,
      W_PAGE          TYPE I,
      W_MOD           TYPE I,
      W_AMOUNT        LIKE ZTIV-ZFIVAMT,
      W_LOOP_CNT      TYPE I,
      W_NAME1         LIKE T001W-NAME1,
      W_LGOBE         LIKE T001L-LGOBE,
      COUNT           TYPE I,
      W_BIT           TYPE C,
      TOT_LINE        TYPE I,
      IN_CHK_YN(1)    TYPE C,
      INCLUDE(8)      TYPE C,
      W_SELECT(2)     TYPE C,
      W_EBELN         LIKE EKPO-EBELN,
      W_EBELP         LIKE EKPO-EBELP,
      V_VENDNM        LIKE LFA1-NAME1,
      V_BANKNM        LIKE LFA1-NAME1,
      SUM_IN_CHK(1)   TYPE C.

DATA : W_GROUP(5).
DATA: MAX_LINE        TYPE  I,
      CODE_LINE       TYPE  I,
      TEXT_LINE       TYPE  I,
      TITLE_LINE      TYPE  I,
      DETAIL_LINE     TYPE  I,
      W_GR_TITLE(20),
      W_FIELD(20),
      W_DT_TITLE(18).

FIELD-SYMBOLS : <FS>, <FS2>.

*-----------------------------------------------------------------------
* HIDE VARIABLE.
*-----------------------------------------------------------------------

DATA: BEGIN OF DOCU,
        TYPE(2)   TYPE C,
        CODE      LIKE EKKO-EBELN,
        ITMNO     LIKE EKPO-EBELP,
        YEAR      TYPE I,
      END OF DOCU.
