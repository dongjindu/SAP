*----------------------------------------------------------------------*
*   INCLUDE ZRIMDOCTREETOP                                             *
*----------------------------------------------------------------------*
TABLES : EKKO,                    " PO HEADER
         LIKP,                    " Inbound Delivery Header
         USR01,                   " User Profile
         ZTREQHD,                 " Import Request HEADER
         ZTMLCAMHD,               " LC AMEND HEADER
         ZTLLCAMHD,               " LOCAL LC AMEND HEADER
         ZTINS,                   " Insurance HEADER
         ZTREQST,                 " Import Request Status.
         ZTBL,                    " BL HEADER
         ZTCGHD,                  " Cargo HEADER
         ZTLG,                    " LG HEADER
         ZTBLINR,                 " Bonded-in HEADER
         ZTBLOUR,                 " Bonded-out HEADER
         ZTCIVHD,                 " COMMERCIAL IV HEADER
         ZTIV,                    " Customs Clearance Req INVOICE HEADER
         ZTIDR,                   " Customs Clearance HEADER
         ZTIDRUS,                 " Customs Declaration
         ZTIDS,                   " Customs Clearance HEADER
         ZTIDSUS,                 " Customs Clearance
         T024E,                   " Purchasing Group TEXT
         T024,                    " Purchasing Group TEXT
         LFA1,                    " VENDOR MASTER
         ZTIMIMG10,               " Customs Broker TABLE
         ZTIMIMG03,               " Bonded Aread TABLE
         ZTIVHST,                 " G/R History TABLE
         ZTIVHSTIT.               " Partial G/R TABLE

*-----------------------------------------------------------------------
*      INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : IT_EKKO    LIKE EKKO            OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTINS   LIKE ZTINS           OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTREQST LIKE ZTREQST         OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTLG    LIKE ZTLG            OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTIDS   LIKE ZTIDSUS         OCCURS 0   WITH HEADER LINE.
DATA : IT_INR     LIKE ZTBLINR         OCCURS 0   WITH HEADER LINE.
DATA : IT_OUR     LIKE ZTBLOUR         OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTIVHST LIKE ZTIVHST         OCCURS 0   WITH HEADER LINE.
DATA : IT_ZTCUIV  LIKE ZTCUCLIV        OCCURS 0   WITH HEADER LINE.

*>>  INTERNAL TABLE
DATA : BEGIN OF IT_ZTREQHD  OCCURS  0,
       ZFREQNO    LIKE  ZTREQHD-ZFREQNO,
       EBELN      LIKE  ZTREQHD-EBELN,
       ZFREQTY    LIKE  ZTREQHD-ZFREQTY,
       ZFOPNNO    LIKE  ZTREQST-ZFOPNNO,
       LIFNR      LIKE  ZTREQHD-LIFNR,
       ZFMATGB    LIKE  ZTREQHD-ZFMATGB,
       INCO1      LIKE  ZTREQHD-INCO1,
       ZFOPBN     LIKE  ZTREQHD-ZFOPBN,
       ZFOPAMT    LIKE  ZTREQHD-ZFOPAMT,
       ZFLASTSD   LIKE  ZTREQHD-ZFLASTSD,
       ZFLASTED   LIKE  ZTREQHD-ZFLASTED,
       ZFLASTAM   LIKE  ZTREQHD-ZFLASTAM,
       WAERS      LIKE  ZTREQHD-WAERS.
DATA : END   OF   IT_ZTREQHD.

*>> Import Request Status INTERNAL TABLE
DATA : BEGIN OF  IT_ST OCCURS 100,
       ZFREQNO   LIKE   ZTREQHD-ZFREQNO.
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
       ZFREQNO   LIKE   ZTBLIT-ZFREQNO,
       EBELN     LIKE   ZTBLIT-EBELN.
DATA : END   OF  IT_ZTBL.

DATA : BEGIN OF  IT_BL OCCURS 100,
       ZFBLNO    LIKE   ZTBL-ZFBLNO,
       EBELN     LIKE   ZTBLIT-EBELN,
       ZFREQNO   LIKE   ZTBLIT-ZFREQNO,
       ZFETD     LIKE   ZTBL-ZFETD,
       ZFETA     LIKE   ZTBL-ZFETA,
       ZFSHNO    LIKE   ZTBL-ZFSHNO,
       ZFHBLNO   LIKE   ZTBL-ZFHBLNO,
       ZFMBLNO   LIKE   ZTBL-ZFMBLNO,
       ZFCGHNO   LIKE   ZTBL-ZFCGHNO,
       ZFFORD    LIKE   ZTBL-ZFFORD,
       ZFCARNM   LIKE   ZTBL-ZFCARNM,
       ZFCARC    LIKE   ZTBL-ZFCARC,
       ZFBLAMT   LIKE   ZTBL-ZFBLAMT,
       ZFBLAMC   LIKE   ZTBL-ZFBLAMC.
DATA : END   OF  IT_BL.

*>> Cargo DATA INTERNAL TABLE.
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

*>> Customs Clerance Request DATA INTERNAL TABLE.
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
       ZFLGRST   LIKE   ZTIV-ZFLGRST,
       ZFCLCD    LIKE   ZTIV-ZFCLCD,
       ZFCUST    LIKE   ZTIV-ZFCUST,
       ZFGRST    LIKE   ZTIV-ZFGRST.
DATA : END  OF   IT_IV.

*>> Customs Declare INTERNAL TABLE
DATA : BEGIN OF  IT_IDR  OCCURS  100,
       ZFBLNO    LIKE   ZTIDRUSD-ZFBLNO,
       ZFCLSEQ   LIKE   ZTIDRUSD-ZFCLSEQ,
       ZFIVNO    LIKE   ZTIDRUSD-ZFIVNO.
DATA : END   OF  IT_IDR.

DATA : BEGIN OF  IT_ZTIDR  OCCURS  100,
       ZFBLNO    LIKE      ZTIDRUS-ZFBLNO,
       ZFCLSEQ   LIKE      ZTIDRUS-ZFCLSEQ,
       ZFENTNO   LIKE      ZTIDRUS-ZFENTNO,
       ZFIVAMT   LIKE      ZTIDRUS-ZFIVAMT,
       ZFIVAMC   LIKE      ZTIDRUS-ZFIVAMC,
       ZFIVNO    LIKE      ZTIDRUSD-ZFIVNO,
       ZFCUT     LIKE      ZTIDRUS-ZFCTW.
DATA : END   OF  IT_ZTIDR.

*>> G/R Data Internal Table Declare
DATA : BEGIN OF IT_IN OCCURS 1000,
       ZFIVNO    LIKE   ZTIVHSTIT-ZFIVNO,
       ZFIVHST   LIKE   ZTIVHSTIT-ZFIVHST,
       ZFGRST    LIKE   ZTIVHSTIT-ZFGRST,
       BLDAT     LIKE   ZTIVHST-BLDAT,
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
       BUKRS     LIKE   ZTIV-BUKRS.
DATA : END OF IT_IN.

*-----------------------------------------------------------------------
* Variable DECLARE
*-----------------------------------------------------------------------
DATA : W_ERR_CHK     TYPE   C,
       W_BL_CNT      TYPE   I,
       W_TR_CNT      TYPE   I,
       W_LINE        TYPE   I,
       SV_LIFNR      LIKE   EKKO-LIFNR,
       SV_NAME       LIKE   LFA1-NAME1,
       SV_TEXT(20)   TYPE   C,
       SV_TEXT1(20)  TYPE   C,
       SV_TEXT2(20)  TYPE   C,
       SV_BLNO(24)   TYPE   C,
       W_TABIX       LIKE   SY-TABIX,
       W_MOD         TYPE   I,
       W_LINE_CNT    TYPE   I,
       W_IV_CNT      TYPE   I,
       W_NODE_CNT    TYPE   I,
       W_NODE_CNT1   TYPE   I,
       W_SPACE       TYPE   I,
       W_SPACE1      TYPE   I,
       SV_MBLNR      LIKE   ZTIVHST-MBLNR,
       W_MBLNR       LIKE   ZTIVHST-MBLNR,
       W_MJAHR       LIKE   ZTIVHST-MJAHR.
