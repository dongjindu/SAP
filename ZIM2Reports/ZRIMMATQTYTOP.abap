*----------------------------------------------------------------------*
*   INCLUDE ZRIMMATQTYTOP                                              *
*----------------------------------------------------------------------*
tables: EKKO,                " ABAP Standard Header Table..
        EKPO,                " ABAP Standard Item Table..
        LIKP,                " Inbound Deliver Table
        ZTREQHD,             " Import Request Header Table..
        ZTREQST,             " Import Request Status Table..
        ZTREQIT,             " Import Request Item Table..
        ZTIV,                " Invoice Table..
        ZTIVIT,              " Invoice Item Table..
        ZTIVHST,             " INVOICE HISTORY TABLE
        ZTBL,                " B/L Table -ZTIVIT Table¿Ã Item Table..
        LFA1,                " Vendor Master Table..
        ZTCUCL,              " Customs Clearance Table..
        ZTCUCLIV,            " Customs Clearance Invoice Table..
        ZTCUCLIVIT,          " Customs Clearance Invoice Item..
        ZTIDR,               " Customs Declare Table..
        ZTIDS,               " Customs Clearance Table..
        ZTIMIMG00,           " Import System Basic Configuration.
        T001L,
        T001W,
        EKET.

*------------------------------------------*
* P/O Data INTERNAL TABLE Declare          *
*------------------------------------------*
DATA: BEGIN OF IT_PO OCCURS 1000,          " Internal Table IT_PO..
        WERKS    LIKE   EKPO-WERKS,        " Plant..
        LIFNR    LIKE   EKKO-LIFNR,        " Vendor..
        EBELN    LIKE   EKPO-EBELN,        " P/O Header No..
        EBELP    LIKE   EKPO-EBELP,        " P/O Item No..
        MATNR    LIKE   EKPO-MATNR,        " Material No..
        TXZ01    LIKE   EKPO-TXZ01,        " Material Text..
        MENGE    LIKE   EKPO-MENGE,        " Purchase Order Quantity..
        MEINS    LIKE   EKPO-MEINS,        " Order Unit..
      END OF IT_PO.

*-----------------------------------------------*
* Import Request Document Data INTERNAL TABLE   *
*-----------------------------------------------*
DATA: BEGIN OF IT_RN OCCURS 1000,
        WERKS     LIKE   EKPO-WERKS,       " Plant
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,  " Import Request No
        ZFREQTY   LIKE   ZTREQHD-ZFREQTY,  " Import Payment Type.
        EBELN     LIKE   ZTREQHD-EBELN,    " Purchasing Document Number.
        EBELP     LIKE   ZTREQIT-EBELP,    " Purchasing Document ITEM.
        LIFNR     LIKE   ZTREQHD-LIFNR,    " Vendor's Account Number.
        LLIEF     LIKE   ZTREQHD-LLIEF,    " Supplying Vendor.
        ZFBENI    LIKE   ZTREQHD-ZFBENI,   " Different Invoicing Party.
        ZFLASTAM  LIKE   ZTREQHD-ZFLASTAM, " Latest Open Amount
        WAERS     LIKE   ZTREQHD-WAERS,    " Currency Key.
        ZFUSDAM   LIKE   ZTREQHD-ZFUSDAM,  " USD Convert Amount.
        ZFUSD     LIKE   ZTREQHD-ZFUSD,    " Currency
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,  " Import Request Item No
        MATNR     LIKE   ZTREQIT-MATNR,    " Material Number.
        STAWN     LIKE   ZTREQIT-STAWN,    " Commodity Code.
        MENGE     LIKE   ZTREQIT-MENGE,    " Import Request Quantity
        MEINS     LIKE   ZTREQIT-MEINS,    " Base Unit of Measure.
        ZFOPNDT   LIKE   ZTREQST-ZFOPNDT,  " Opne Date
        ERNAM     LIKE   ZTREQST-ERNAM,    " Creater.
        EKORG     LIKE   ZTREQST-EKORG,    " Purch. Org.
        EKGRP     LIKE   ZTREQST-EKGRP,    " Purch. Grp.
        ZFOPNNO   LIKE   ZTREQST-ZFOPNNO,  " Approve No
        ZFOPAMT   LIKE   ZTREQST-ZFOPAMT,  " Open Amount
      END OF IT_RN.

*-----------------------------------------------*
* INVOICE Data INTERNAL TABLE Declare.          *
*-----------------------------------------------*
DATA: BEGIN OF IT_IV OCCURS 1000,
        LIFNR     LIKE   ZTIV-LIFNR,       " Vendor.
        BUKRS     LIKE   ZTIV-BUKRS,       " Company
        ZFIVNO    LIKE   ZTIV-ZFIVNO,      " Invoice Document No
        ZFGRST    LIKE   ZTIV-ZFGRST,      " G/R Status
        ZFPOYN    LIKE   ZTIV-ZFPOYN,      " Monetary, Non-Monetary
        ZFIVDNO   LIKE   ZTIVIT-ZFIVDNO,   " Invoice Sequence
        ZFREQNO   LIKE   ZTIVIT-ZFREQNO,   " Import Request Document No
        ZFITMNO   LIKE   ZTIVIT-ZFITMNO,   " Import Request Document Ite
        ZFBLNO    LIKE   ZTIVIT-ZFBLNO,    " B/L NO.
        ZFBLIT    LIKE   ZTIVIT-ZFBLIT,    " B/L ITEM.
        ZFCGNO    LIKE   ZTIVIT-ZFCGNO,    " Unloading Document No
        ZFCGIT    LIKE   ZTIVIT-ZFCGIT,    " Unloading Item No.
        ZFCUST    LIKE   ZTIV-ZFCUST,      " Customs Clearance Status
        MATNR     LIKE   ZTIVIT-MATNR,     " Material Number.
        CCMENGE   LIKE   ZTIVIT-CCMENGE,   " Clearance Quantity
        GRMENGE   LIKE   ZTIVIT-GRMENGE,   " G/R Quantity
        MEINS     LIKE   ZTIVIT-MEINS,     " Base Unit of Measure.
        WERKS     LIKE   ZTIVIT-WERKS,     " Plant
        LGORT     LIKE   ZTIVIT-LGORT,     " Storage Location
        MBLNR     LIKE   ZTIVHST-MBLNR,    " Material Document No
        MJAHR     LIKE   ZTIVHST-MJAHR,    " Material Document Year
        EBELN     LIKE   EKPO-EBELN,       " P/O
        EBELP     LIKE   EKPO-EBELP,       " P/O Item
      END OF IT_IV.

*-----------------------------------------------------------------------
* B/L Data Internal Table Declaration.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_BL OCCURS 1000,
        MATNR     LIKE   ZTBLIT-MATNR,     " Material Code
        TXZ01     LIKE   ZTBLIT-TXZ01,     " Material Text
        WERKS     LIKE   ZTBLIT-WERKS,     " Plant
        LGORT     LIKE   ZTBLIT-LGORT,     " Storage Location
        LIFNR     LIKE   ZTBL-LIFNR,       " Vendor
        ZFBLNO    LIKE   ZTBL-ZFBLNO,      " B/L Document No.
        ZFBLIT    LIKE   ZTBLIT-ZFBLIT,    " B/L Document Item
        EBELN     LIKE   ZTBLIT-EBELN,     " Representive P/O No.
        EBELP     LIKE   ZTBLIT-EBELP,     " P/O Item
        ZFHBLNO   LIKE   ZTBL-ZFHBLNO,     " House B/L No
        ZFINDT    LIKE   ZTBL-ZFINDT,      " Carry-in date
        ZFPOYN    LIKE   ZTBL-ZFPOYN,      " Monetary, Non-Monetary
        ZFELIKZ   LIKE   ZTBL-ZFELIKZ,     " B/L Delivery Complete
        BLMENGE   LIKE   ZTBLIT-BLMENGE,   " B/L Quantity
        MEINS     LIKE   ZTBLIT-MEINS,     " Unit
        ZFAPRTC   LIKE   ZTBL-ZFAPRTC,     " Arriving Port
        ZFFORD    LIKE   ZTBL-ZFFORD,      " Forwarder
        ZFREQNO   LIKE   ZTBLIT-ZFREQNO,   " Import Request Document No
        ZFITMNO   LIKE   ZTBLIT-ZFITMNO,   " Import Request Item No
      END OF IT_BL.

*-----------------------------------------------------------------------
* CIV Data Select!
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CIV OCCURS 1000,
        ZFWERKS   LIKE   ZTBL-ZFWERKS,
        LIFNR     LIKE   ZTBL-LIFNR,
        ZFCIVRN   LIKE   ZTCIVHD-ZFCIVRN,
        ZFCIVSQ   LIKE   ZTCIVIT-ZFCIVSQ,
        CMENGE    LIKE   ZTCIVIT-CMENGE,
        EBELN     LIKE   ZTCIVIT-EBELN,
        EBELP     LIKE   ZTCIVIT-EBELP,
        ZFREQNO   LIKE   ZTCIVIT-ZFREQNO,
        ZFITMNO   LIKE   ZTCIVIT-ZFITMNO,
      END OF IT_CIV.

*-----------------------------------------------------------------------
* Unloading Data Select!
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CG OCCURS 1000,
        WERKS     LIKE   ZTCGHD-WERKS,
        LGORT     LIKE   ZTIMIMG03-LGORT,
        LIFNR     LIKE   ZTBL-LIFNR,
        ZFCGNO    LIKE   ZTCGHD-ZFCGNO,
        ZFCGIT    LIKE   ZTCGIT-ZFCGIT,
        ZFMSNO    LIKE   ZTCGHD-ZFMSNO,
        ZFCGPT    LIKE   ZTCGHD-ZFCGPT,
        ZFBNARCD  LIKE   ZTCGIT-ZFBNARCD,
        CGMENGE   LIKE   ZTCGIT-CGMENGE,
        MEINS     LIKE   ZTCGIT-MEINS,
        EBELN     LIKE   ZTCGIT-EBELN,
        EBELP     LIKE   ZTCGIT-EBELP,
        ZFBLNO    LIKE   ZTCGIT-ZFBLNO,
        ZFBLIT    LIKE   ZTCGIT-ZFBLIT,
        ZFREQNO   LIKE   ZTCGIT-ZFREQNO,
        ZFITMNO   LIKE   ZTCGIT-ZFITMNO,
      END OF IT_CG.

*-----------------------------------------------------------------------
* Customs Declaration Internal Table
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDR OCCURS 1000,
      ZFBLNO       LIKE  ZTIDRUS-ZFBLNO,
      ZFCLSEQ      LIKE  ZTIDRUS-ZFCLSEQ,
      ZFENTNO      LIKE  ZTIDRUS-ZFENTNO,
      ZFCUT        LIKE  ZTIDRUS-ZFCTW,
      ZFCONO       LIKE  ZTIDRUSD-ZFCONO,
      ZFRONO       LIKE  ZTIDRUSD-ZFRONO,
      ZFQNT        LIKE  ZTIDRUSD-ZFQNT,
      ZFQNTM       LIKE  ZTIDRUSD-ZFQNTM,
      ZFIVNO       LIKE  ZTIDSUSD-ZFIVNO,
      ZFIVDNO      LIKE  ZTIDSUSD-ZFIVDNO,
      EBELN        LIKE  EKPO-EBELN,
      EBELP        LIKE  EKPO-EBELP,
      ZFREQNO      LIKE  ZTREQHD-ZFREQNO,
      ZFITMNO      LIKE  ZTREQIT-ZFITMNO,
      ZFBLIT       LIKE  ZTBLIT-ZFBLIT,
      ZFCGNO       LIKE  ZTCGHD-ZFCGNO,
      ZFCGIT       LIKE  ZTCGIT-ZFCGIT.
DATA  END OF IT_IDR.

*-----------------------------------------------------------------------
* Customs Clearance Internal Table
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDS OCCURS 1000,
      ZFBLNO       LIKE  ZTIDSUS-ZFBLNO,
      ZFCLSEQ      LIKE  ZTIDSUS-ZFCLSEQ,
      ZFENTNO      LIKE  ZTIDSUS-ZFENTNO,
      ZFCUT        LIKE  ZTIDSUS-ZFCTW,
      ZFCONO       LIKE  ZTIDSUSD-ZFCONO,
      ZFRONO       LIKE  ZTIDSUSD-ZFRONO,
      WERKS        LIKE  ZTIVIT-WERKS,
      LGORT        LIKE  ZTIVIT-LGORT,
      LIFNR        LIKE  ZTIV-LIFNR,
      ZFQNT        LIKE  ZTIDSUSD-ZFQNT,
      ZFQNTM       LIKE  ZTIDSUSD-ZFQNTM,
      EBELN        LIKE  EKPO-EBELN,
      EBELP        LIKE  EKPO-EBELP,
      ZFIVNO       LIKE  ZTIDSUSD-ZFIVNO,
      ZFIVDNO      LIKE  ZTIDSUSD-ZFIVDNO,
      ZFREQNO      LIKE  ZTREQHD-ZFREQNO,
      ZFITMNO      LIKE  ZTREQIT-ZFITMNO,
      ZFBLIT       LIKE  ZTBLIT-ZFBLIT,
      ZFCGNO       LIKE  ZTCGHD-ZFCGNO,
      ZFCGIT       LIKE  ZTCGIT-ZFCGIT.
DATA: END OF IT_IDS.

*------------------------------------------*
* WRITE  DATA INTERNAL TABLE               *
*------------------------------------------*
DATA: BEGIN OF IT_TAB OCCURS 0,
        WERKS      LIKE   EKPO-WERKS,
        LGORT      LIKE   ZTBLIT-LGORT,
        LIFNR      LIKE   ZTREQHD-LIFNR,
        MATNR      LIKE   EKPO-MATNR,
        MATNM      LIKE   EKPO-TXZ01,
        EBELN      LIKE   EKPO-EBELN,
        EBELP      LIKE   EKPO-EBELP,
        MENGE      LIKE   EKPO-MENGE,
        MEINS      LIKE   EKPO-MEINS,
        RE_MENGE   LIKE   EKPO-MENGE,
        BL_MENGE   LIKE   EKPO-MENGE,
        CG_MENGE   LIKE   EKPO-MENGE,
        IV_MENGE   LIKE   EKPO-MENGE,
        IDR_MENGE  LIKE   EKPO-MENGE,
        IDS_MENGE  LIKE   EKPO-MENGE,
        NO_MENGE   LIKE   EKPO-MENGE,
        IN_MENGE   LIKE   EKPO-MENGE,
        CIV_MENGE  LIKE   EKPO-MENGE,
        BLCG_MENGE LIKE   ZSIVIT-ZFNOCGMN,
        IN_CHK(12)  TYPE   C,
      END OF IT_TAB.

DATA: W_TABIX         LIKE SY-TABIX,
      W_PO_HD_ST      LIKE ZVEKKO_REQHD_ST,
      W_ERR_CHK       TYPE C,
      W_BL_CNT        TYPE I,
      W_TR_CNT        TYPE I,
      W_COUNT         TYPE I,
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
      WRITE_CHK       TYPE C,
      W_EBELN         LIKE EKPO-EBELN,
      W_EBELP         LIKE EKPO-EBELP,
      SV_LGORT        LIKE ZTBLIT-LGORT,
      SV_LGORT_NM     LIKE T001L-LGOBE,
      SV_WERKS        LIKE EKPO-WERKS,
      SV_WERKS_NM     LIKE T001W-NAME1,
      SV_LIFNR        LIKE ZTREQHD-LIFNR,
      SV_LIFNR_NM     LIKE LFA1-NAME1,
      SV_MATNR        LIKE EKPO-MATNR,
      SV_MEINS        LIKE EKPO-MEINS,
      SV_MATNM        LIKE EKPO-TXZ01,
      SUM_PO_MENGE    LIKE EKPO-MENGE,
      SUM_RE_MENGE    LIKE ZTREQIT-MENGE,
      SUM_BL_MENGE    LIKE ZTBLIT-BLMENGE,
      SUM_IV_MENGE    LIKE ZTIVIT-CCMENGE,
      SUM_CIV_MENGE   LIKE ZTCIVIT-CMENGE,
      SUM_CG_MENGE    LIKE ZTCGIT-CGMENGE,
      SUM_IDR_MENGE   LIKE ZTIDRHS-ZFQNT,
      SUM_IDS_MENGE   LIKE ZTIDSHS-ZFQNT,
      SUM_IN_MENGE    LIKE ZTBLIT-BLMENGE,
      SUM_NO_MENGE    LIKE ZTBLIT-BLMENGE,
      SUM_BLCG_MENGE  LIKE ZSIVIT-ZFNOCGMN,
      TOT_PO_MENGE    LIKE EKPO-MENGE,
      TOT_RE_MENGE    LIKE ZTREQIT-MENGE,
      TOT_BL_MENGE    LIKE ZTBLIT-BLMENGE,
      TOT_IV_MENGE    LIKE ZTIVIT-CCMENGE,
      TOT_CIV_MENGE   LIKE ZTCIVIT-CMENGE,
      TOT_CG_MENGE    LIKE ZTCGIT-CGMENGE,
      TOT_IDR_MENGE   LIKE ZTIDRHS-ZFQNT,
      TOT_IDS_MENGE   LIKE ZTIDSHS-ZFQNT,
      TOT_BLCG_MENGE  LIKE ZSIVIT-ZFNOCGMN,
      TOT_NO_MENGE    LIKE ZTIVIT-GRMENGE,
      TOT_IN_MENGE    LIKE ZTBLIT-BLMENGE,
      SUM_IN_CHK(1)   TYPE C.


*-----------------------------------------------------------------------
* HIDE VARIABLE.
*-----------------------------------------------------------------------

DATA: BEGIN OF DOCU,
        TYPE(2)   TYPE C,
        CODE      LIKE EKKO-EBELN,
        ITMNO     LIKE EKPO-EBELP,
        YEAR      TYPE I,
      END OF DOCU.
