*----------------------------------------------------------------------*
*   INCLUDE ZRIMDOCRPTTOP                                              *
*----------------------------------------------------------------------*
TABLES : EKKO,                    " PO HEADER
         EKPO,                    " PO Item
         LIKP,                    " Delivery Header
         LIPS,                    " Delivery Item
         ZTREQHD,                 " Import Request HEADER
         ZTMLCAMHD,               " LC AMEND HEADER
         ZTLLCAMHD,               " LOCAL LC AMEND HEADER
         ZTINS,                   " Insurance HEADER
         ZTCGIT,                  " Unloading ITEM
         ZTREQST,                 " Import Request Status.
         ZTBL,                    " BL HEADER
         ZTCGHD,                  " Unloading HEADER
         ZTLG,                    " LG HEADER
         ZTBLINR,                 " Bonded In HEADER
         ZTBLOUR,                 " Bonded out HEADER
         ZTCIVHD,                 " COMMERCIAL IV HEADER
         ZTIV,                    " Customs Clearance Request Header
         ZTIDR,                   " Customs Declaration HEADER
         ZTIDRUS,                 " CUSTOMS DECLARATION
         ZTIDSUS,                 " CUSTOMS CLEARANCE
         ZTIDS,                   " Customs Clearance HEADER
         T001W,                   " PLANT Name SELECT.
         T024E,                   " Purchasing Org. TEXT
         T024,                    " Purchasing Group TEXT
         LFA1,                    " VENDOR MASTER
         ZTIMIMG00,               " IMG
         ZTIMIMG10,               " Customs Management TABLE
         ZTIMIMG02,               " Customs/Vendor Match Code Table.
         ZTIMIMG03,               " Bonded Area Code Management TABLE
         ZTIVHST.                 " GR History TABLE

*-----------------------------------------------------------------------
*      INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : IT_EKKO    LIKE EKKO            OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTINS   LIKE ZTINS           OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTREQST LIKE ZTREQST         OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTLG    LIKE ZTLG            OCCURS 0   WITH HEADER LINE.
DATA : IT_INR     LIKE ZTBLINR         OCCURS 0   WITH HEADER LINE.
DATA : IT_OUR     LIKE ZTBLOUR         OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTIVHST LIKE ZTIVHST         OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTCUIV  LIKE ZTCUCLIV        OCCURS 0   WITH HEADER LINE.

*>> Import Request INTERNAL TABLE
DATA : BEGIN OF IT_ZTREQHD  OCCURS  0,
       ZFREQNO    LIKE  ZTREQHD-ZFREQNO,
       EBELN      LIKE  ZTREQIT-EBELN,
       ZFREQTY    LIKE  ZTREQHD-ZFREQTY,
       ZFOPNNO    LIKE  ZTREQST-ZFOPNNO,
       ZFOPNDT    LIKE  ZTREQST-ZFOPNDT,
       EKGRP      LIKE  ZTREQST-EKGRP,
       LIFNR      LIKE  ZTREQHD-LIFNR,
       ZFMATGB    LIKE  ZTREQHD-ZFMATGB,
       INCO1      LIKE  ZTREQHD-INCO1,
       ZTERM      LIKE  ZTREQHD-ZTERM,
       ZFOPBN     LIKE  ZTREQHD-ZFOPBN,
       ZFOPAMT    LIKE  ZTREQHD-ZFOPAMT,
       ZFLASTSD   LIKE  ZTREQHD-ZFLASTSD,
       ZFLASTED   LIKE  ZTREQHD-ZFLASTED,
       ZFLASTAM   LIKE  ZTREQHD-ZFLASTAM,
       WAERS      LIKE  ZTREQHD-WAERS.
DATA : END   OF   IT_ZTREQHD.

*>> Import Request Stataus INTERNAL TABLE
DATA : BEGIN OF  IT_ST OCCURS 100,
       ZFREQNO   LIKE   ZTREQHD-ZFREQNO,
       ZFOPNDT   LIKE   ZTREQST-ZFOPNDT,
       EKGRP     LIKE   ZTREQST-EKGRP.
DATA : END   OF  IT_ST.

*>> COMMERICAIL INVOICE DATA INTERNAL TABLE
DATA : BEGIN OF  IT_ZTCIV OCCURS 100,
       ZFCIVRN   LIKE   ZTCIVHD-ZFCIVRN,
       ZFREQNO   LIKE   ZTCIVIT-ZFREQNO.
DATA : END   OF  IT_ZTCIV.

DATA : BEGIN OF  IT_CIV OCCURS 100,
       ZFCIVRN   LIKE   ZTCIVHD-ZFCIVRN,
       ZFREQNO   LIKE   ZTCIVIT-ZFREQNO,
       ZFCIDT    LIKE   ZTCIVHD-ZFCIDT,
       ZFCIVNO   LIKE   ZTCIVHD-ZFCIVNO,
       ZFIVAMT   LIKE   ZTCIVHD-ZFIVAMT,
       ZFIVAMC   LIKE   ZTCIVHD-ZFIVAMC,
       ZFIVAMP   LIKE   ZTCIVHD-ZFIVAMP,
       ZFIVAMK   LIKE   ZTCIVHD-ZFIVAMK,
       ZFEXRT    LIKE   ZTCIVHD-ZFEXRT,
       ZFMAVN    LIKE   ZTCIVHD-ZFMAVN,
       ZFOPBN    LIKE   ZTCIVHD-ZFOPBN.
DATA : END   OF  IT_CIV.

*>> BL DATA INTERNAL TABLE
DATA : BEGIN OF  IT_ZTBL OCCURS 100,
       ZFBLNO    LIKE   ZTBL-ZFBLNO,
       ZFREQNO   LIKE   ZTBLIT-ZFREQNO.
DATA : END   OF  IT_ZTBL.

DATA : BEGIN OF  IT_BL OCCURS 100,
       ZFBLNO    LIKE   ZTBL-ZFBLNO,
       ZFREQNO   LIKE   ZTBLIT-ZFREQNO,
       ZFBLDT    LIKE   ZTBL-ZFBLDT,
       ZFETD     LIKE   ZTBL-ZFETD,
       ZFETA     LIKE   ZTBL-ZFETA,
       ZFHBLNO   LIKE   ZTBL-ZFHBLNO,
       ZFMBLNO   LIKE   ZTBL-ZFMBLNO,
       ZFCGHNO   LIKE   ZTBL-ZFCGHNO,
       ZFFORD    LIKE   ZTBL-ZFFORD,
       ZFCARNM   LIKE   ZTBL-ZFCARNM,
       ZFCARC    LIKE   ZTBL-ZFCARC,
       ZFBLAMT   LIKE   ZTBL-ZFBLAMT,
       ZFBLAMC   LIKE   ZTBL-ZFBLAMC.
DATA : END   OF  IT_BL.

*>> Unloading DATA INTERNAL TABLE.
DATA : BEGIN OF IT_ZTCG  OCCURS  100,
       ZFCGNO    LIKE   ZTCGHD-ZFCGNO,
       ZFBLNO    LIKE   ZTCGIT-ZFBLNO.
DATA : END   OF  IT_ZTCG.

DATA : BEGIN OF IT_CG  OCCURS  100,
       ZFCGNO    LIKE   ZTCGHD-ZFCGNO,
       ZFBLNO    LIKE   ZTCGIT-ZFBLNO,
       ZFETA     LIKE   ZTCGHD-ZFETA,
       ZFARVLDT  LIKE   ZTCGHD-ZFARVLDT,
       ZFCGPT    LIKE   ZTCGHD-ZFCGPT,
       ZFKEYM    LIKE   ZTCGHD-ZFKEYM.
DATA : END   OF IT_CG.

*>> Customs Clearance Request DATA INTERNAL TABLE.
DATA : BEGIN OF  IT_ZTIV  OCCURS  100,
       ZFIVNO    LIKE   ZTIV-ZFIVNO,
       ZFREQNO   LIKE   ZTIVIT-ZFREQNO,
       ZFBLNO    LIKE   ZTIVIT-ZFBLNO.
DATA : END  OF   IT_ZTIV.

DATA : BEGIN OF  IT_IV  OCCURS  100,
       ZFIVNO    LIKE   ZTIV-ZFIVNO,
       ZFREQNO   LIKE   ZTIVIT-ZFREQNO,
       ZFBLNO    LIKE   ZTIV-ZFBLNO,
       ZFCCDT    LIKE   ZTIV-ZFCCDT,
       ZFIVAMT   LIKE   ZTIV-ZFIVAMT,
       ZFIVAMC   LIKE   ZTIV-ZFIVAMC,
       ZFIVAMK   LIKE   ZTIV-ZFIVAMK,
       ZFEXRT    LIKE   ZTIV-ZFEXRT,
       ZFCUT     LIKE   ZTIV-ZFCUT,
       ZFLGRST   LIKE   ZTIV-ZFLGRST,
       ZFCLCD    LIKE   ZTIV-ZFCLCD,
       ZFCUST    LIKE   ZTIV-ZFCUST,
       ZFGRST    LIKE   ZTIV-ZFGRST,
       ERNAM     LIKE   ZTIV-ERNAM.
DATA : END  OF   IT_IV.

*>> Customs Declaration INTERNAL TABLE
DATA : BEGIN OF  IT_IDS  OCCURS  100,
       ZFBLNO    LIKE   ZTIDSUSD-ZFBLNO,
       ZFCLSEQ   LIKE   ZTIDSUSD-ZFCLSEQ,
       ZFIVNO    LIKE   ZTIDSUSD-ZFIVNO.
DATA : END   OF  IT_IDS.

DATA : BEGIN OF  IT_ZTIDS  OCCURS  100,
       ZFBLNO    LIKE      ZTIDSUS-ZFBLNO,
       ZFCLSEQ   LIKE      ZTIDSUS-ZFCLSEQ,
       ZFIDSDT   LIKE      ZTIDSUS-ZFEDT,
       ZFIDWDT   LIKE      ZTIDSUS-ZFEEDT,
       ZFIDRNO   LIKE      ZTIDSUS-ZFENTNO,
       ZFSTAMT   LIKE      ZTIDSUS-ZFIVAMT,
       ZFSTAMC   LIKE      ZTIDSUS-ZFIVAMC,
       ZFIVNO    LIKE      ZTIDSUSD-ZFIVNO,
       ZFCUT     LIKE      ZTIDSUS-ZFCTW,
       ZFINRC    LIKE      ZTIDSUS-ZFINRC.
DATA : END   OF  IT_ZTIDS.
DATA : BEGIN OF  IT_ZTIDR  OCCURS  100,
       ZFBLNO    LIKE      ZTIDRUS-ZFBLNO,
       ZFCLSEQ   LIKE      ZTIDRUS-ZFCLSEQ,
       ZFIDSDT   LIKE      ZTIDRUS-ZFEDT,
       ZFIDRNO   LIKE      ZTIDRUS-ZFENTNO,
       ZFSTAMT   LIKE      ZTIDRUS-ZFIVAMT,
       ZFSTAMC   LIKE      ZTIDRUS-ZFIVAMC,
       ZFIVNO    LIKE      ZTIDSUSD-ZFIVNO,
       ZFCUT     LIKE      ZTIDRUS-ZFCTW,
       ZFINRC    LIKE      ZTIDRUS-ZFINRC.
DATA : END   OF  IT_ZTIDR.
DATA : BEGIN OF  IT_IDR  OCCURS  100,
       ZFBLNO    LIKE      ZTIDRUS-ZFBLNO,
       ZFCLSEQ   LIKE      ZTIDRUS-ZFCLSEQ,
       ZFIDSDT   LIKE      ZTIDRUS-ZFEDT,
       ZFIDRNO   LIKE      ZTIDRUS-ZFENTNO,
       ZFSTAMT   LIKE      ZTIDRUS-ZFIVAMT,
       ZFSTAMC   LIKE      ZTIDRUS-ZFIVAMC,
       ZFIVNO    LIKE      ZTIDSUSD-ZFIVNO,
       ZFCUT     LIKE      ZTIDRUS-ZFCTW,
       ZFINRC    LIKE      ZTIDRUS-ZFINRC.
DATA : END   OF  IT_IDR.

*>> G/R INTERNAL TABLE Declare.
DATA : BEGIN OF IT_IN OCCURS 1000,
       ZFIVNO    LIKE   ZTIVIT-ZFIVNO,
       ZFIVHST   LIKE   ZTIVHST-ZFIVHST,
       ZFGRST    LIKE   ZTIV-ZFGRST,
       BLDAT     LIKE   ZTIVHST-BLDAT,
       BUDAT     LIKE   ZTIVHST-BUDAT,
       BWART     LIKE   ZTIVHST-BWART,
       ZFIVDNO   LIKE   ZTIVIT-ZFIVDNO,
       ZFIVAMT   LIKE   ZTIV-ZFIVAMT,
       ZFIVAMC   LIKE   ZTIV-ZFIVAMC,
       ERNAM     LIKE   ZTIV-ERNAM,
       MATNR     LIKE   ZTIVIT-MATNR,
       GRMENGE   LIKE   ZTIVIT-GRMENGE,
       MEINS     LIKE   ZTIVIT-MEINS,
       WERKS     LIKE   ZTIVIT-WERKS,
       LGORT     LIKE   ZTIVIT-LGORT,
       MBLNR     LIKE   ZTIVHST-MBLNR,
       MJAHR     LIKE   ZTIVHST-MJAHR,
       EBELN     LIKE   EKPO-EBELN,
       EBELP     LIKE   EKPO-EBELP,
       BUKRS     LIKE   ZTIV-BUKRS.
DATA : END OF IT_IN.
* Mothership Management
DATA : BEGIN OF IT_ZTMSHD OCCURS 1000,
       ZFREQNO   LIKE   ZTREQHD-ZFREQNO,
       ZFMSNO    LIKE   ZTMSHD-ZFMSNO,
       ZFMSNM    LIKE   ZTMSHD-ZFMSNM,
       ZFSHSDF   LIKE   ZTMSHD-ZFSHSDF.
DATA : END OF IT_ZTMSHD.
*-----------------------------------------------------------------------
* Variable DECLARE
*-----------------------------------------------------------------------
DATA : W_ERR_CHK     TYPE   C,
       W_BL_CNT      TYPE   I,
       W_TR_CNT      TYPE   I,
       W_LINE        TYPE   I,
       W_TABIX       LIKE   SY-TABIX,
       W_AMOUNT      LIKE   ZTIV-ZFIVAMT,
       SV_LIFNR      LIKE   EKKO-LIFNR,
       SV_NAME       LIKE   LFA1-NAME1,
       SV_TEXT(20)   TYPE   C,
       SV_TEXT1(20)  TYPE   C,
       SV_TEXT2(20)  TYPE   C,
       SV_BLNO(24)   TYPE   C,
       W_WERKS       LIKE   EKPO-WERKS,
       W_TEXT        LIKE   EKPO-TXZ01,
       W_MOD         TYPE   I,
       W_LINE_CNT    TYPE   I,
       W_IV_CNT      TYPE   I,
       SV_MBLNR      LIKE   ZTIVHST-MBLNR,
       W_CUT_NAME    LIKE   LFA1-NAME1,
       W_INRC_NAME   LIKE   LFA1-NAME1.
DATA : W_TXT(20)     TYPE   C,
       P_BUKRS       LIKE   ZTIMIMG00-ZFBUKRS.
