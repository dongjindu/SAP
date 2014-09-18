*&---------------------------------------------------------------------*
*& REPORT ZRIMMATHIS2                                                  *
*&---------------------------------------------------------------------*
*&  Program Name : Progress Status by Material                         *
*&  Created by   : Na shin ho INFOLINK LTD.                            *
*&  Created on   : 2001.04.06                                          *
*&---------------------------------------------------------------------*
*& [Change Log]
*&
*&---------------------------------------------------------------------*
REPORT ZRIMMATHIS2 NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                     LINE-SIZE 180.

TABLES: EKKO,                " ABAP STANDARD HEADER TABLE..
        EKPO,                " ABAP STANDARD ITEM TABLE..
        LIKP,                " Delivery Header Data
        LIPS,                " Delivery: Item data
        ZTREQHD,             " Import Request HEADER TABLE..
        ZTREQIT,             " Import Request ITEM TABLE..
        ZTBL,                " B/L TABLE..
        ZTBLIT,              " B/L  ITEM TABLE..
        LFA1,                " Vendor MASTER TABLE..
        ZTMSHD,              " Mothership Management HEADER TABLE..
        ZTCGHD,              " Cargo HEADER TABLE..
        ZTCGIT,              " Cargo Item TABLE..
        ZTREQORJ,            " Origin TABLE..
        ZTIMIMG03,           " Bonded Area TABLE..
        ZTIMIMG10,           " Customs Broker Management Table
        T024,                " Purchasing Group Name Table
        T005T,               " Country Name Table
        T001L,               " Storage Location TABLE..
        T001W,               " Plant TABLE..
        ZTCIVHD,             " COMMERCIAL INVOICE HEADER..
        ZTCIVIT,             " COMMERCIAL INVOICE ITEMS..
        ZTIV,                " Customs Clerarance Request HEADER..
        ZTIVIT,              " Customs Clerarance Request ITEM TABLE..
        ZTREQST,             " Import Request Status Management
        ZTIDSUS,             " Customs Clearance
        ZTIDSUSD,            " Customs Clearance Item
        ZTIDS,               " Import Licensce
        ZTIDSHSD,            " Import Licensce ITEM TABLE
        ZTIDR,               " Import Request
        ZTIDRUS,             " Customs Declaration
        ZTIVHST,             " G/R History.
        ZV_COTM.             " Customs Name VIEW.
*------------------------------------------*
* P/O INTERNAL TABLE Declare               *
*------------------------------------------*
DATA: BEGIN OF IT_PO OCCURS 1000,
        EBELN    LIKE   EKPO-EBELN,
        LIFNR    LIKE   EKKO-LIFNR,
        AEDAT    LIKE   EKPO-AEDAT,
        WAERS    LIKE   EKKO-WAERS,
        EKGRP    LIKE   EKKO-EKGRP,
        EKNAM    LIKE   T024-EKNAM,
        EBELP    LIKE   EKPO-EBELP,
        MATNR    LIKE   EKPO-MATNR,
        BUKRS    LIKE   EKPO-BUKRS,
        WERKS    LIKE   EKPO-WERKS,
        WERNM    LIKE   T001W-NAME1,
        TXZ01    LIKE   EKPO-TXZ01,
        MENGE    LIKE   EKPO-MENGE,
        MEINS    LIKE   EKPO-MEINS,
        NETPR    LIKE   EKPO-NETPR,
        PEINH    LIKE   EKPO-PEINH,
        NAME1    LIKE   LFA1-NAME1,
      END OF IT_PO.

*-----------------------------------------------*
* Import Request INTERNAL TABLE Declare         *
*-----------------------------------------------*
DATA: BEGIN OF IT_RN OCCURS 1000,
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,
        ZFREQTY   LIKE   ZTREQHD-ZFREQTY,
        EBELN     LIKE   ZTREQHD-EBELN,
        EBELP     LIKE   ZTREQIT-EBELP,
        LIFNR     LIKE   ZTREQHD-LIFNR,
        WAERS     LIKE   ZTREQHD-WAERS,
        ZFOPBN    LIKE   ZTREQHD-ZFOPBN,
        NAME1     LIKE   LFA1-NAME1,
        ZFOPNNO   LIKE   ZTREQHD-ZFOPNNO,
        ZFTRANS   LIKE   ZTREQHD-ZFTRANS,
        INCO1     LIKE   ZTREQHD-INCO1,
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,
        MATNR     LIKE   ZTREQIT-MATNR,
        MENGE     LIKE   ZTREQIT-MENGE,
        MEINS     LIKE   ZTREQIT-MEINS,
        NETPR     LIKE   ZTREQIT-NETPR,
        PEINH     LIKE   ZTREQIT-PEINH,
        BPRME     LIKE   ZTREQIT-BPRME,
        ZFORIG    LIKE   ZTREQORJ-ZFORIG,
        LANDX     LIKE   T005T-LANDX,
        ZFOPNDT   LIKE   ZTREQST-ZFOPNDT,
        CDAT      LIKE   ZTREQST-CDAT,
        END OF IT_RN.

*-----------------------------------------------------------------------
* B/L INTERNAL TABLE DECLARATION.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_BL OCCURS 1000,
        ZFBLNO    LIKE   ZTBL-ZFBLNO,
        ZFMSNO    LIKE   ZTBL-ZFMSNO,
        ZFFORD    LIKE   ZTBL-ZFFORD,
        ZFAPRTC   LIKE   ZTBL-ZFAPRTC,
        ZFAPRT    LIKE   ZTBL-ZFAPRT,
        ZFHBLNO   LIKE   ZTBL-ZFHBLNO,
        ZFREBELN  LIKE   ZTBL-ZFREBELN,
        LIFNR     LIKE   ZTBL-LIFNR,
        ZFOPNNO   LIKE   ZTBL-ZFOPNNO,
        ZFETA     LIKE   ZTBL-ZFETA,
        ZFPOYN    LIKE   ZTBL-ZFPOYN,
        ZFRENT    LIKE   ZTBL-ZFRENT,
        ZFVIA     LIKE   ZTBL-ZFVIA,
        INCO1     LIKE   ZTBL-INCO1,
        ZFBLIT    LIKE   ZTBLIT-ZFBLIT,
        EBELN     LIKE   ZTBLIT-EBELN,
        EBELP     LIKE   ZTBLIT-EBELP,
        ZFREQNO   LIKE   ZTBLIT-ZFREQNO,
        ZFITMNO   LIKE   ZTBLIT-ZFITMNO,
        MATNR     LIKE   ZTBLIT-MATNR,
        ZFBLDT    LIKE   ZTBL-ZFBLDT,
        BLMENGE   LIKE   ZTBLIT-BLMENGE,
        MEINS     LIKE   ZTBLIT-MEINS,
      END OF IT_BL.

*-----------------------------------------------------------------------
* Mothership INTERNAL TABLE Declare
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
        ZFCIVRN   LIKE ZTCIVHD-ZFCIVRN,
        ZFCIVNO   LIKE ZTCIVHD-ZFCIVNO,
        ZFMAVN    LIKE ZTCIVHD-ZFMAVN,
        ZFOPBN    LIKE ZTCIVHD-ZFOPBN,
        ZFCIDT    LIKE ZTCIVHD-ZFCIDT,
        ZFCIVSQ   LIKE ZTCIVIT-ZFCIVSQ,
        EBELN     LIKE ZTCIVIT-EBELN,
        EBELP     LIKE ZTCIVIT-EBELP,
        ZFREQNO   LIKE ZTCIVIT-ZFREQNO,
        ZFITMNO   LIKE ZTCIVIT-ZFITMNO,
        ZFBLNO    LIKE ZTCIVIT-ZFBLNO,
        ZFBLIT    LIKE ZTCIVIT-ZFBLIT,
        ZFPRQN    LIKE ZTCIVIT-ZFPRQN,
        MEINS     LIKE ZTCIVIT-MEINS,
      END OF IT_CIV.

*-----------------------------------------------------------------------
* Unloading Port INTERNAL TABLE Declare.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CG OCCURS 1000,
        ZFCGNO    LIKE   ZTCGHD-ZFCGNO,
        ZFMSNO    LIKE   ZTCGHD-ZFMSNO,
        ZFARVLDT  LIKE   ZTCGHD-ZFARVLDT,
        ZFETA     LIKE   ZTCGHD-ZFETA,
        ZFCGPT    LIKE   ZTCGHD-ZFCGPT,
        ZFCGIT    LIKE   ZTCGIT-ZFCGIT,
        EBELN     LIKE   ZTCGIT-EBELN,
        EBELP     LIKE   ZTCGIT-EBELP,
        ZFREQNO   LIKE   ZTCGIT-ZFREQNO,
        ZFITMNO   LIKE   ZTCGIT-ZFITMNO,
        ZFBLNO    LIKE   ZTCGIT-ZFBLNO,
        ZFBLIT    LIKE   ZTCGIT-ZFBLIT,
        MATNR     LIKE   ZTCGIT-MATNR,
        CGMENGE   LIKE   ZTCGIT-CGMENGE,
        MEINS     LIKE   ZTCGIT-MEINS,
        ZFBNARCD  LIKE   ZTCGIT-ZFBNARCD,
      END OF IT_CG.

*-----------------------------------------------------------------------
* Vendor Master INTERNAL TABLE Declare
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_LFA OCCURS 1000,
         LIFNR    LIKE   LFA1-LIFNR,
         NAME1    LIKE   LFA1-NAME1,
      END OF IT_LFA.

*-----------------------------------------------------------------------
* Bonded Area Code INTERNAL TABLE Declare
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IMG03 OCCURS 1000.
        INCLUDE STRUCTURE ZTIMIMG03.
DATA  END OF IT_IMG03.

*-----------------------------------------------------------------------
* DECLARATION OF INTERNAL TALBE FOR ZTIDR TABLE REFERENCE..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDR OCCURS 1000.
        INCLUDE STRUCTURE ZTIDRUS.
DATA  ZFIVDNO LIKE  ZTIDRUSD-ZFIVDNO.
DATA  ZFQNT   LIKE  ZTIDRUSD-ZFQNT.
DATA  ZFQNTM  LIKE  ZTIDRUSD-ZFQNTM.
DATA  END OF IT_IDR.

*-----------------------------------------------------------------------
* DECLARATION OF INTERNAL TALBE FOR ZTIDS TABLE REFERENCE..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDS OCCURS 1000.
        INCLUDE STRUCTURE ZTIDSUS.
DATA  ZFQNT   LIKE  ZTIDSUSD-ZFQNT.
DATA  ZFQNTM  LIKE  ZTIDSUSD-ZFQNTM.
DATA  ZFIVDNO LIKE  ZTIDSUSD-ZFIVDNO.
DATA: END OF IT_IDS.

*-----------------------------------------------------------------------
* DECLARATION OF INTERNAL TALBE FOR T001L TABLE REFERENCE..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_T001L OCCURS 1000.
        INCLUDE STRUCTURE T001L.
DATA: END OF IT_T001L.

*-----------------------------------------------------------------------
* DECLARATION OF INTERNAL TALBE FOR T001W TABLE REFERENCE..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_T001W OCCURS 1000.
        INCLUDE STRUCTURE T001W.
DATA: END OF IT_T001W.

*-------------------------------------------------*
* G/R DATA READ INTERNAL TABLE Declare            *
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
       BUKRS     LIKE   ZTIV-BUKRS.
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
* DECLARATION OF VARIABLE FOR TABLE USAGE..
*-----------------------------------------------------------------------
DATA: W_TABIX     LIKE SY-TABIX,
      W_BEWTP     LIKE EKBE-BEWTP,
      W_AMOUNT    LIKE ZTREQHD-ZFLASTAM,
      W_BL_CNT    TYPE I,
      W_TR_CNT    TYPE I,
      W_ERR_CHK   TYPE C,
      W_TRIPLE(5) TYPE C,
      W_PAGE      TYPE I,
      W_TRANS(15) TYPE C.

*-----------------------------------------------------------------------
* DECLARATION OF VARIABLE FOR INSTEAD OF ON CHANGE OF FUNCTION..
*-----------------------------------------------------------------------
DATA: W_LOOP_CNT  TYPE I,
      TEMP_MATNR  LIKE EKPO-MATNR,
      TEMP_TXZ01  LIKE EKPO-TXZ01,
      TEMP_EBELN  LIKE EKPO-EBELN,
      TEMP_EBELP  LIKE EKPO-EBELP,
      TEMP_REQNO  LIKE ZTREQHD-ZFREQNO,
      TEMP_ITMNO  LIKE ZTREQIT-ZFITMNO,
      TEMP_BLNO   LIKE ZTBL-ZFBLNO,
      TEMP_BLIT   LIKE ZTBLIT-ZFBLIT,
      TEMP_CGNO   LIKE ZTCGHD-ZFCGNO,
      TEMP_CGIT   LIKE ZTCGIT-ZFCGIT,
      TEMP_IVNO   LIKE ZTIVIT-ZFIVNO,
      TEMP_IVDNO  LIKE ZTIVIT-ZFIVDNO.

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
* Search Condition SELECTION WINDOW.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_EBELN   FOR  EKKO-EBELN,       " P/O NO.
               S_MATNR   FOR  EKPO-MATNR,       " Material NO.
               S_CDAT    FOR  ZTREQST-CDAT,     " Import Request Cr.dt
               S_REQNO   FOR  ZTREQHD-ZFREQNO,  " Import Request No
               S_OPNNO   FOR  ZTREQHD-ZFOPNNO,  " L/C Approve No
               S_LIFNR   FOR  ZTREQHD-LIFNR,    " VENDOR.
               S_REQTY   FOR  ZTREQHD-ZFREQTY,  " Payment Type
               S_TRANS   FOR  ZTREQHD-ZFTRANS,  " Transport Method
               S_INCO1   FOR  ZTREQHD-INCO1,    " Incoterms
               S_REQSD   FOR  ZTREQHD-ZFREQSD,  " Shipping Date
               S_REQED   FOR  ZTREQHD-ZFREQED,  " Expire Date
               S_SHCU    FOR  ZTREQHD-ZFSHCU,   " Shipping Port
               S_HBLNO   FOR  ZTBL-ZFHBLNO,     " House B/L No
               S_TRAID   FOR  LIKP-TRAID.       " Container No
SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.
  SET TITLEBAR 'ZIM92'.
*-----------------------------------------------------------------------
* TOP-OF-PAGE.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM P3000_TITLE_WRITE.

*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Import Request Data
  PERFORM P1000_READ_RN_DATA USING W_ERR_CHK.
  CHECK W_ERR_CHK NE 'Y'.

* P/O TABLE SELECT..
  PERFORM P1000_READ_PO_DATA.

* B/L TABLE SELECT..
  PERFORM P1000_READ_BL_DATA.

* Mothership TABLE SELECT..
  PERFORM P1000_READ_MS_DATA.

* COMMERCIAL INVOICE TABLE SELECT..
  PERFORM P1000_READ_CIV_DATA.

* Cargo TABLE SELECT..
  PERFORM P1000_READ_CG_DATA.

* Bonded Area TABLE SELECT..
  PERFORM P1000_READ_IMG03_DATA.

* Vendor Master TABLE SELECT..
  PERFORM P1000_READ_LFA_DATA.

* Customs Clerance/G.R. Request TABLE SELECT..
  PERFORM P1000_READ_IVIT_DATA.

* Customs Declaration TABLE SELECT..
  PERFORM P1000_READ_ZFIDR_DATA.

* Entry Summary TABLE SELECT..
  PERFORM P1000_READ_ZFIDS_DATA.

* T001W TABLE SELECT..
  PERFORM P1000_READ_T001W_DATA.

* T001L TABLE SELECT..
  PERFORM P1000_READ_T001L_DATA.

* G/R TABLE READ
  PERFORM P1000_READ_IN_DATA.

*-----------------------------------------------------------------------
* END-OF-SELECTION.
*-----------------------------------------------------------------------
END-OF-SELECTION.
  CHECK W_ERR_CHK NE 'Y'.
  SET TITLEBAR 'ZIM92'.
  SET PF-STATUS 'ZIM92'.

* SORT P/O, REQUEST NO. INTERNAL TABLE.
  PERFORM P2000_SORT_IT_DATA.

* LIST WRITE...
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
  CASE L_TEXT_MA.

    WHEN 'IT_PO-MATNR' OR 'IT_PO-TXZ01'.
      SET PARAMETER ID 'MAT' FIELD IT_PO-MATNR.
      SET PARAMETER ID 'BUK' FIELD IT_PO-BUKRS.
      SET PARAMETER ID 'WRK' FIELD IT_PO-WERKS.
      SET PARAMETER ID 'LAG' FIELD ''.
      SET PARAMETER ID 'MXX' FIELD MM03_START_SICHT.
      CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.

  ENDCASE.

  GET CURSOR FIELD L_TEXT_PO.
  CASE L_TEXT_PO.

    WHEN 'IT_PO-EBELN'.
      SET PARAMETER ID 'BES'  FIELD IT_PO-EBELN .
      SET PARAMETER ID 'BSP'  FIELD IT_PO-EBELP.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDCASE.
  CLEAR: IT_PO.

  GET CURSOR FIELD L_TEXT_RN.
  CASE L_TEXT_RN(5).

    WHEN 'IT_RN'.
      SET PARAMETER ID 'ZPREQNO' FIELD IT_RN-ZFREQNO.
      SET PARAMETER ID 'ZPOPNNO' FIELD ''.
      SET PARAMETER ID 'BES'     FIELD ''.
      CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.

  ENDCASE.
  CLEAR: IT_RN.

  GET CURSOR FIELD L_TEXT_BL.
  CASE L_TEXT_BL(5).

    WHEN 'IT_BL'.
      SET PARAMETER ID 'ZPBLNO'  FIELD IT_BL-ZFBLNO.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
    WHEN 'IT_CG'.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      SET PARAMETER ID 'ZPBLNO'  FIELD ''.
      SET PARAMETER ID 'ZPCGNO'  FIELD IT_CG-ZFCGNO.
      CALL TRANSACTION 'ZIM83' AND SKIP FIRST SCREEN.
  ENDCASE.
  CLEAR: IT_BL.

  GET CURSOR FIELD L_TEXT_IVIT.
  CASE L_TEXT_IVIT(7).

    WHEN 'IT_IVIT'.
      SET PARAMETER ID 'ZPIVNO' FIELD IT_IVIT-ZFIVNO.
      SET PARAMETER ID 'ZPBLNO' FIELD ''.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      CALL TRANSACTION 'ZIM33' AND SKIP FIRST SCREEN.
  ENDCASE.
  CLEAR: IT_IVIT.

  GET CURSOR FIELD L_TEXT_CIV.
  CASE L_TEXT_CIV(6).

    WHEN 'IT_CIV'.
      SET PARAMETER ID 'ZPCIVRN' FIELD IT_CIV-ZFCIVRN.
      SET PARAMETER ID 'ZPCIVNO' FIELD ''.
      CALL TRANSACTION 'ZIM37' AND SKIP FIRST SCREEN.

  ENDCASE.
  CLEAR: IT_CIV.

  GET CURSOR FIELD L_TEXT_IDR.
  CASE L_TEXT_IDR(6).

    WHEN 'IT_IDR' .
      SET PARAMETER ID 'ZPCLSEQ' FIELD IT_IDR-ZFCLSEQ.
      SET PARAMETER ID 'ZPBLNO' FIELD IT_IDR-ZFBLNO.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      CALL TRANSACTION 'ZIMCD3' AND SKIP FIRST SCREEN.

  ENDCASE.
  CLEAR: IT_IDR.

  GET CURSOR FIELD L_TEXT_IDS.
  CASE L_TEXT_IDS(6).

    WHEN 'IT_IDS'.
      SET PARAMETER ID 'ZPENTNO' FIELD IT_IDS-ZFENTNO.
      SET PARAMETER ID 'ZPCLSEQ' FIELD IT_IDS-ZFCLSEQ.
      SET PARAMETER ID 'ZPIVNO' FIELD  IT_IDS-ZFIVNO.
      CALL TRANSACTION 'ZIMCC3' AND SKIP FIRST SCREEN.

  ENDCASE.
  CLEAR: IT_IDS.
  GET CURSOR FIELD L_TEXT_IN.
  CASE  L_TEXT_IN.
    WHEN  'IT_IN-MBLNR' OR 'IT_IN-ZFIVNO'.
      SET  PARAMETER ID  'BUK'   FIELD   IT_IN-BUKRS.
      SET  PARAMETER ID  'MBN'   FIELD   IT_IN-MBLNR.
      SET  PARAMETER ID  'MJA'   FIELD   IT_IN-MJAHR.
      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          I_ACTION            = 'A04'
          I_REFDOC            = 'R02'
          I_NOTREE            = 'X'
          I_SKIP_FIRST_SCREEN = 'X'
          I_OKCODE            = 'OK_GO'
          I_MBLNR             = IT_IN-MBLNR
          I_MJAHR             = IT_IN-MJAHR
        EXCEPTIONS
          ILLEGAL_COMBINATION = 1
          OTHERS              = 2.
  ENDCASE.
  CLEAR  L_TEXT_IN.
*&------------------------------------------------------------------*
*&      FORM  P1000_READ_RN_DATA
*&------------------------------------------------------------------*
FORM P1000_READ_RN_DATA USING W_ERR_CHK.

  W_ERR_CHK = 'N'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_RN
    FROM   ( ZTREQHD AS H INNER JOIN ZTREQIT AS I
    ON     H~ZFREQNO     EQ  I~ZFREQNO             )
    INNER JOIN  ZTREQST AS  P
    ON     I~ZFREQNO     EQ  P~ZFREQNO
    WHERE  H~ZFREQNO     IN  S_REQNO
    AND    H~ZFOPNNO     IN  S_OPNNO
    AND    I~EBELN       IN  S_EBELN
    AND    I~MATNR       IN  S_MATNR
    AND    H~LIFNR       IN  S_LIFNR
    AND    H~ZFREQTY     IN  S_REQTY
    AND    H~ZFTRANS     IN  S_TRANS
    AND    H~INCO1       IN  S_INCO1
    AND    H~ZFREQSD     IN  S_REQSD
    AND    H~ZFREQED     IN  S_REQED
    AND    H~ZFSHCU      IN  S_SHCU
    AND    P~CDAT        IN  S_CDAT
    AND    H~ZFTRIPLE LIKE  W_TRIPLE.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S738.
    EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_RN_DATA

*&---------------------------------------------------------------------*
*&      FORM  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

  SELECT *
    INTO   CORRESPONDING FIELDS OF TABLE IT_PO
    FROM   EKKO AS H INNER JOIN EKPO AS I
    ON     H~EBELN EQ I~EBELN
    FOR ALL ENTRIES IN IT_RN
    WHERE  I~EBELN EQ IT_RN-EBELN
    AND    I~EBELP EQ IT_RN-EBELP.

  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  LOOP AT IT_RN.
    W_TABIX  = SY-TABIX.

    IF NOT S_HBLNO[] IS INITIAL.

      CLEAR : W_BL_CNT.
      SELECT COUNT( * )  INTO  W_BL_CNT
      FROM   ZTBL  AS  A INNER JOIN ZTBLIT AS B
      ON     A~ZFBLNO    EQ    B~ZFBLNO
      WHERE  A~ZFHBLNO   IN    S_HBLNO
      AND    B~EBELN     EQ    IT_RN-EBELN
      AND    B~EBELP     EQ    IT_RN-EBELP.

      IF W_BL_CNT GT 0.
        DELETE  IT_RN  INDEX  W_TABIX.
        CONTINUE.
      ENDIF.

    ENDIF.

    IF NOT S_TRAID[] IS INITIAL.

      CLEAR : W_TR_CNT.
      SELECT COUNT( * )   INTO   W_TR_CNT
      FROM   LIKP  AS  A  INNER  JOIN  LIPS AS B
      ON     A~VBELN      EQ     B~VBELN
      WHERE  A~TRAID      IN     S_TRAID
      AND    B~VGBEL      EQ     IT_RN-EBELN
      AND    B~VGPOS      EQ     IT_RN-EBELP.

      IF W_TR_CNT EQ 0.
        DELETE   IT_RN  INDEX  W_TABIX.
        CONTINUE.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
*&      FORM  P1000_READ_LFA_DATA
*&---------------------------------------------------------------------*
*       TEXT
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
*&      FORM  P1000_READ_MS_DATA
*&---------------------------------------------------------------------*
*       TEXT
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
*&      FORM  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_BL
    FROM   ZTBL AS H INNER JOIN ZTBLIT AS I
    ON     H~ZFBLNO  EQ I~ZFBLNO
    FOR ALL ENTRIES IN IT_RN
    WHERE  I~EBELN   EQ IT_RN-EBELN
    AND    I~EBELP   EQ IT_RN-EBELP.

ENDFORM.                    " P1000_READ_BL_DATA

*&---------------------------------------------------------------------*
*&      FORM  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_CIV
    FROM   ZTCIVHD AS H INNER JOIN ZTCIVIT AS I
    ON     H~ZFCIVRN  EQ I~ZFCIVRN
    FOR ALL ENTRIES  IN IT_RN
    WHERE  I~EBELN   EQ IT_RN-EBELN
    AND    I~EBELP   EQ IT_RN-EBELP
    AND    I~ZFREQNO EQ IT_RN-ZFREQNO
    AND    I~ZFITMNO EQ IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_CIV_DATA

*&---------------------------------------------------------------------*
*&      FORM  P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*       READ CARGO DATA.
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
*&      FORM  P1000_READ_IMG03_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM P1000_READ_IMG03_DATA.

  SELECT *
    INTO   CORRESPONDING FIELDS OF TABLE IT_IMG03
    FROM   ZTIMIMG03
    FOR ALL ENTRIES IN IT_CG
    WHERE  ZFBNARCD EQ IT_CG-ZFBNARCD.

ENDFORM.                    " P1000_READ_IMG03_DATA


*&---------------------------------------------------------------------*
*&      FORM  P1000_READ_IVIT_DATA
*&---------------------------------------------------------------------*
*       TEXT
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
  IF NOT IT_CG[] IS INITIAL.
    SELECT *
      APPENDING CORRESPONDING FIELDS OF TABLE IT_IVIT
      FROM ZTIVIT
      FOR ALL ENTRIES IN IT_CG
      WHERE ZFCGNO EQ IT_CG-ZFCGNO
        AND ZFCGIT EQ IT_CG-ZFCGIT.
  ENDIF.

*>> LOCAL Purchasing DATA SELECT!
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
    SELECT SINGLE ZFCUST ZFGRST ZFCCDT ZFCUT ZFCUNAM
           INTO   (IT_IVIT-ZFCUST, IT_IVIT-ZFGRST,
                   IT_IVIT-ZFCCDT, IT_IVIT-ZFCUT, IT_IVIT-ZFCUNAM)
           FROM   ZTIV
           WHERE  ZFIVNO EQ IT_IVIT-ZFIVNO.
    MODIFY IT_IVIT INDEX L_TABIX.
  ENDLOOP.
ENDFORM.                    " P1000_READ_IVIT_DATA

*&---------------------------------------------------------------------*
*&      FORM  P1000_READ_ZFIDR_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM P1000_READ_ZFIDR_DATA.

*( 09/16/11 Paul : Insert logic
  CHECK IT_IVIT IS NOT INITIAL.
*}
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_IDR
    FROM ZTIDRUS  AS  H  INNER JOIN  ZTIDRUSD AS I
    ON   H~ZFIVNO   EQ  I~ZFIVNO
    AND  H~ZFCLSEQ  EQ  I~ZFCLSEQ
    FOR ALL ENTRIES IN IT_IVIT
    WHERE I~ZFIVNO  EQ IT_IVIT-ZFIVNO
      AND I~ZFIVDNO EQ IT_IVIT-ZFIVDNO.

ENDFORM.                    " P1000_READ_ZFIDR_DATA

*&---------------------------------------------------------------------*
*&      FORM  P1000_READ_ZFIDS_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM P1000_READ_ZFIDS_DATA.
*( 09/16/11 Paul : Insert logic
  CHECK IT_IDR IS NOT INITIAL.
*}
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_IDS
    FROM ZTIDSUS  AS  H  INNER  JOIN  ZTIDSUSD  AS  I
    ON   H~ZFIVNO    EQ  I~ZFIVNO
    AND  H~ZFCLSEQ   EQ  I~ZFCLSEQ
    FOR ALL ENTRIES IN IT_IDR
    WHERE H~ZFIVNO  EQ  IT_IDR-ZFIVNO
      AND H~ZFCLSEQ EQ  IT_IDR-ZFCLSEQ.

ENDFORM.                    " P1000_READ_ZFIDS_DATA

*&---------------------------------------------------------------------*
*&      FORM  P1000_READ_T001W_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM P1000_READ_T001W_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_T001W FROM T001W
    FOR ALL ENTRIES IN IT_IVIT
    WHERE WERKS   EQ IT_IVIT-WERKS.
*       AND ZFCLSEQ EQ IT_CUCL-ZFCLSEQ.

ENDFORM.                    " P1000_READ_T001W_DATA

*&---------------------------------------------------------------------*
*&      FORM  P1000_READ_T001L_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM P1000_READ_T001L_DATA.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_T001L FROM T001L
    FOR ALL ENTRIES IN IT_IVIT
    WHERE WERKS   EQ IT_IVIT-WERKS
      AND LGORT   EQ IT_IVIT-LGORT.

ENDFORM.                    " P1000_READ_T001L_DATA

*&---------------------------------------------------------------------*
*&      FORM  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE: /55 '[Progress status by material]'
             COLOR COL_HEADING INTENSIFIED OFF.

  WRITE: / 'DATE: ' ,
            SY-DATUM .
ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      FORM  P2000_SORT_IT_DATA
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
*&      FORM  P3000_WEITE_PO_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_PO_DATA.

  DATA : L_FIRST_LINE   VALUE  'Y',
         L_DATE(10).

  CLEAR: TEMP_MATNR, TEMP_EBELN, TEMP_EBELP, TEMP_TXZ01.
  SKIP.
  LOOP AT IT_PO.
    IF TEMP_MATNR NE IT_PO-MATNR OR TEMP_TXZ01 NE IT_PO-TXZ01.
      FORMAT COLOR COL_HEADING INTENSIFIED ON.
      ULINE.
      WRITE: / IT_PO-MATNR NO-GAP, IT_PO-TXZ01 NO-GAP,
               '                               ' NO-GAP,
               179 '' NO-GAP.
      HIDE: IT_PO.
      FORMAT RESET.
    ENDIF.

    W_AMOUNT = IT_PO-MENGE * ( IT_PO-NETPR / IT_PO-PEINH ).

    IF IT_PO-EBELN NE TEMP_EBELN.
*>> Vendor Name DISPLAY.
      READ TABLE IT_LFA WITH KEY LIFNR = IT_PO-LIFNR.
      IF SY-SUBRC NE 0. CLEAR IT_LFA. ENDIF.
*>> Plant Name DISPLAY.
      READ TABLE IT_T001W  WITH KEY WERKS = IT_PO-WERKS.
      IF SY-SUBRC NE 0. CLEAR IT_T001W. ENDIF.
*>> Purchaing Group DISPLAY.
      SELECT SINGLE * FROM T024  WHERE EKGRP EQ IT_PO-EKGRP.
      IF SY-SUBRC NE 0. CLEAR T024.  ENDIF.

      FORMAT COLOR COL_NORMAL INTENSIFIED ON.

      WRITE :  /(10) 'P/O'                 NO-GAP,
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
                (20) IT_LFA-NAME1          NO-GAP,
                (4)  IT_PO-WERKS           ,
                (15) IT_T001W-NAME1        NO-GAP,
                (3)  IT_PO-EKGRP           ,
                (15) T024-EKNAM            NO-GAP.
      HIDE: IT_PO.
    ELSEIF IT_PO-EBELP NE TEMP_EBELP.
*>> Vendor Name DISPLAY.
      READ TABLE IT_LFA WITH KEY LIFNR = IT_PO-LIFNR.
      IF SY-SUBRC NE 0. CLEAR IT_LFA. ENDIF.
*>> Plant Name DISPLAY.
      READ TABLE IT_T001W  WITH KEY WERKS = IT_PO-WERKS.
      IF SY-SUBRC NE 0. CLEAR IT_T001W. ENDIF.
*>> Purchasing Group DISPLAY.
      SELECT SINGLE * FROM T024  WHERE EKGRP EQ IT_PO-EKGRP.
      IF SY-SUBRC NE 0. CLEAR T024.  ENDIF.

      FORMAT COLOR COL_NORMAL INTENSIFIED ON.

      WRITE :  /(10) 'P/O'                 NO-GAP,
                (10) IT_PO-EBELN           NO-GAP,
                (1)  '-'                   NO-GAP,
                (5)  IT_PO-EBELP           NO-GAP,
                (3)  ''                    NO-GAP,
                (5)  IT_PO-MEINS           NO-GAP,
                (13) IT_PO-MENGE  UNIT     IT_PO-MEINS  NO-GAP,
                (19) IT_PO-NETPR  CURRENCY IT_PO-WAERS  NO-GAP,
                (5)  IT_PO-WAERS           NO-GAP,
                (19) W_AMOUNT     CURRENCY IT_PO-WAERS  NO-GAP,
                (10) IT_PO-AEDAT           ,
                (10) IT_PO-LIFNR           NO-GAP,
                (20) IT_LFA-NAME1          NO-GAP,
                (4)  IT_PO-WERKS           ,
                (15) IT_T001W-NAME1        NO-GAP,
                (3)  IT_PO-EKGRP           ,
                (15) T024-EKNAM            NO-GAP.
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
*&      FORM  P3000_WRITE_RN_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_RN_DATA.

  DATA : L_FIRST_LINE   VALUE  'Y',
         L_DATE_F(10),
         L_DATE_T(10).

  CLEAR: TEMP_REQNO, TEMP_ITMNO, W_LOOP_CNT.

  LOOP AT IT_RN  WHERE EBELN = IT_PO-EBELN
                 AND   EBELP = IT_PO-EBELP.

    W_LOOP_CNT = W_LOOP_CNT + 1.
    FORMAT RESET.
    IF TEMP_REQNO NE IT_RN-ZFREQNO.

*>> Mothership Name SET!
      READ TABLE IT_MS WITH KEY ZFREQNO = IT_RN-ZFREQNO.
      IF SY-SUBRC NE 0. CLEAR IT_MS. ENDIF.
*>> Open Date SET!
      SELECT SINGLE * FROM ZTREQST
             WHERE  ZFREQNO  EQ  IT_RN-ZFREQNO
             AND    ZFAMDNO  EQ  ( SELECT MAX( ZFAMDNO )
                                FROM   ZTREQST
                                WHERE  ZFREQNO  EQ  IT_RN-ZFREQNO ).
      IF SY-SUBRC NE 0. CLEAR ZTREQST. ENDIF.
*>> Transportation Method DISPLAY
      CASE IT_RN-ZFTRANS.
        WHEN 'A'.
          MOVE 'AIR  ' TO W_TRANS.
        WHEN 'O'.
          MOVE 'OCEAN' TO W_TRANS.
      ENDCASE.
      IF SY-SUBRC NE 0. CLEAR ZTREQST. ENDIF.
*>> Open Bank Name DISPLAY
      SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ IT_RN-ZFOPBN.
      IF SY-SUBRC NE 0. CLEAR LFA1.    ENDIF.

      WRITE :  /(10) 'Open'                   NO-GAP,
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
    ELSEIF TEMP_ITMNO NE IT_RN-ZFITMNO.
*>> Mothership Name SET!
      READ TABLE IT_MS WITH KEY ZFREQNO = IT_RN-ZFREQNO.
      IF SY-SUBRC NE 0. CLEAR IT_MS. ENDIF.

*>> Open Date SET!
      SELECT SINGLE * FROM ZTREQST
             WHERE  ZFREQNO  EQ  IT_RN-ZFREQNO
             AND    ZFAMDNO  EQ  ( SELECT MAX( ZFAMDNO )
                                FROM   ZTREQST
                                WHERE  ZFREQNO  EQ  IT_RN-ZFREQNO ).
      IF SY-SUBRC NE 0. CLEAR ZTREQST. ENDIF.
*>> Transportation Method DISPLAY
      CASE IT_RN-ZFTRANS.
        WHEN 'A'.
          MOVE 'AIR  ' TO W_TRANS.
        WHEN 'O'.
          MOVE 'OCEAN' TO W_TRANS.
      ENDCASE.
*>> Open Bank Name DISPLAY
      SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ IT_RN-ZFOPBN.
      IF SY-SUBRC NE 0. CLEAR LFA1.    ENDIF.
      WRITE :  /(10) 'Open'                   NO-GAP,
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

    ENDIF.
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
*&      FORM  P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*       SUBROUTINE FOR WRITING COMMERCIAL INVOICE DATA.
*----------------------------------------------------------------------*
FORM P3000_WRITE_CIV_DATA.

  LOOP AT IT_CIV WHERE ZFREQNO = IT_RN-ZFREQNO
                 AND   ZFITMNO = IT_RN-ZFITMNO.

    FORMAT RESET.

    WRITE :  /(10) 'Invoice'             NO-GAP,
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
*&      FORM  P3000_WRITE_BL_DATA
*&---------------------------------------------------------------------*
*        WRITE BILL OF LADING DATA.
*----------------------------------------------------------------------*
FORM P3000_WRITE_BL_DATA.

  DATA : L_FIRST_LINE   VALUE  'Y',
         L_DATE_F(10),
         L_DATE_T(10).

  CLEAR : TEMP_BLNO, TEMP_BLIT.

  LOOP AT IT_BL
     WHERE ZFREQNO = IT_RN-ZFREQNO
       AND ZFITMNO = IT_RN-ZFITMNO.

*>> Forwarder Name
    SELECT SINGLE * FROM LFA1 WHERE LIFNR  EQ  IT_RN-LIFNR.
    IF SY-SUBRC NE 0. CLEAR LFA1. ENDIF.

    FORMAT RESET.

    FORMAT RESET.
    IF TEMP_BLNO NE IT_BL-ZFBLNO.
      WRITE :  /(10) 'B/L'                 NO-GAP,
                (10) IT_BL-ZFBLNO          NO-GAP,
                (1)  '-'                   NO-GAP,
                (5)  IT_BL-ZFBLIT          NO-GAP,
                (9)  '     '               NO-GAP,
                (13) IT_BL-BLMENGE  UNIT   IT_BL-MEINS  NO-GAP,
                (43)  '     '              NO-GAP,
                (10) IT_BL-ZFBLDT          ,
                (10) IT_BL-ZFFORD          NO-GAP,
                (20) LFA1-NAME1            NO-GAP,
                (35) IT_BL-ZFHBLNO         NO-GAP.
      HIDE: IT_BL.
    ELSEIF TEMP_BLIT NE IT_BL-ZFBLIT.
      WRITE :  /(10) ' '                   NO-GAP,
                (10) ' '                   NO-GAP,
                (1)  ' '                   NO-GAP,
                (5)  IT_BL-ZFBLIT          NO-GAP,
                (9)  '     '               NO-GAP,
                (13) IT_BL-BLMENGE  UNIT   IT_BL-MEINS  NO-GAP,
                (43)  '     '              NO-GAP,
                (10) IT_BL-ZFBLDT          ,
                (10) IT_BL-ZFFORD          NO-GAP,
                (20) LFA1-NAME1            NO-GAP,
                (35) IT_BL-ZFHBLNO         NO-GAP.
      HIDE: IT_BL.

    ENDIF.

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
*&      FORM  P3000_WRITE_BL_ADD_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_BL_ADD_DATA.

  IF IT_BL-ZFPOYN EQ 'Y'.
    WRITE: 95 'Mone'.
  ELSE.
    WRITE: 95 'None'.
  ENDIF.

  IF IT_BL-ZFRENT EQ 'X'.
    WRITE: 102 'Trns B/L'.
  ENDIF.

ENDFORM.                    " P3000_WRITE_BL_ADD_DATA

*&---------------------------------------------------------------------*
*&      FORM  P3000_WRITE_CG_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_CG_DATA.

  CLEAR : TEMP_CGNO,
          TEMP_CGIT.

  LOOP AT IT_CG
     WHERE ZFBLNO = IT_BL-ZFBLNO
       AND ZFBLIT = IT_BL-ZFBLIT.

    FORMAT RESET.
    IF TEMP_CGNO NE IT_CG-ZFCGNO.
      WRITE :  /(10) 'Unload'              NO-GAP,
                (10) IT_CG-ZFCGNO          NO-GAP,
                (1)  '-'                   NO-GAP,
                (5)  IT_CG-ZFCGIT          NO-GAP,
                (9)  ' '                   NO-GAP,
                (13) IT_CG-CGMENGE    UNIT IT_CG-MEINS  NO-GAP,
                (43) ''                    NO-GAP,
                (10) IT_CG-ZFARVLDT        NO-GAP.
      HIDE: IT_CG.
*
    ELSEIF TEMP_CGIT NE IT_CG-ZFCGIT.
      WRITE :  /(10) ''                    NO-GAP,
                (10) ''                    NO-GAP,
                (1)  ''                    NO-GAP,
                (5)  IT_CG-ZFCGIT          NO-GAP,
                (9)  ' '                   NO-GAP,
                (13) IT_CG-CGMENGE    UNIT IT_CG-MEINS  NO-GAP,
                (43) ''                    NO-GAP,
                (10) IT_CG-ZFARVLDT        NO-GAP.
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
*&      FORM  P3000_WRITE_IVIT_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_IVIT_DATA.

  CLEAR : TEMP_IVNO, TEMP_IVDNO.

  IF IT_IVIT-ZFCGNO = ''.
    LOOP AT IT_IVIT WHERE ZFBLNO = IT_BL-ZFBLNO
                    AND   ZFBLIT = IT_BL-ZFBLIT
                    AND   ZFCGNO = SPACE.

      FORMAT RESET.

      SELECT SINGLE * FROM ZTIMIMG10 WHERE ZFCUT EQ IT_IVIT-ZFCUT.
      IF SY-SUBRC NE 0.
        SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ ZTIMIMG10-ZFVEN.
        IF SY-SUBRC NE 0. CLEAR LFA1. ENDIF.
      ENDIF.

      IF TEMP_IVNO NE IT_IVIT-ZFIVNO.
        WRITE :  /(10) 'Clear req'           NO-GAP,
                  (10) IT_IVIT-ZFIVNO        NO-GAP,
                  (1)  '-'                   NO-GAP,
                  (5)  IT_IVIT-ZFIVDNO       NO-GAP,
                  (9)  ' '                   NO-GAP,
                  (13) IT_IVIT-CCMENGE  UNIT IT_IVIT-MEINS  NO-GAP,
                  (43)  ' '                  NO-GAP,
                  (10) IT_IVIT-ZFCCDT,
                  (10) IT_IVIT-ZFCUT         NO-GAP,
                  (20) LFA1-NAME1            NO-GAP.
        HIDE: IT_IVIT.
      ELSEIF TEMP_IVDNO NE IT_IVIT-ZFIVDNO.
        WRITE :  /(10) ''                    NO-GAP,
                  (10) ''                    NO-GAP,
                  (1)  ''                    NO-GAP,
                  (5)  IT_IVIT-ZFIVDNO       NO-GAP,
                  (9)  ' '                   NO-GAP,
                  (13) IT_IVIT-CCMENGE  UNIT IT_IVIT-MEINS  NO-GAP,
                  (43)  ' '                  NO-GAP,
                  (10) IT_IVIT-ZFCCDT,
                  (10) IT_IVIT-ZFCUT         NO-GAP,
                  (20) LFA1-NAME1            NO-GAP.

        HIDE: IT_IVIT.
      ENDIF.
      TEMP_IVNO  = IT_IVIT-ZFIVNO.
      TEMP_IVDNO = IT_IVIT-ZFIVDNO.

      IF IT_IVIT-ZFCUST EQ '2' OR IT_IVIT-ZFCUST EQ '3'
                               OR IT_IVIT-ZFCUST EQ 'Y'.
        PERFORM P3000_WRITE_IDR_DATA.
      ENDIF.

    ENDLOOP.
  ELSE.
    LOOP AT IT_IVIT WHERE ZFCGNO = IT_CG-ZFCGNO
                      AND ZFCGIT = IT_CG-ZFCGIT.

      SELECT SINGLE * FROM ZTIMIMG10 WHERE ZFCUT EQ IT_IVIT-ZFCUT.
      IF SY-SUBRC NE 0.
        SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ ZTIMIMG10-ZFVEN.
        IF SY-SUBRC NE 0. CLEAR LFA1. ENDIF.
      ENDIF.


      FORMAT RESET.
      IF TEMP_IVNO NE IT_IVIT-ZFIVNO.
        WRITE :  /(10) 'Clear req'           NO-GAP,
                  (10) IT_IVIT-ZFIVNO        NO-GAP,
                  (1)  '-'                   NO-GAP,
                  (5)  IT_IVIT-ZFIVDNO       NO-GAP,
                  (09)  ''                   NO-GAP,
                  (13) IT_IVIT-CCMENGE  UNIT IT_IVIT-MEINS  NO-GAP,
                  (43)  ''                   NO-GAP,
                  (10) IT_IVIT-ZFCCDT        ,
                  (10) IT_IVIT-ZFCUT         NO-GAP,
                  (20) LFA1-NAME1            NO-GAP.
        HIDE: IT_IVIT.
      ELSEIF TEMP_IVDNO NE IT_IVIT-ZFIVDNO.
        WRITE : 22(5)  IT_IVIT-ZFIVDNO       NO-GAP,
                36(13) IT_IVIT-CCMENGE  UNIT IT_IVIT-MEINS  NO-GAP,
                96(10) IT_IVIT-ZFCCDT        NO-GAP,
               107(10) IT_IVIT-ZFCUT         NO-GAP,
               118(20) LFA1-NAME1            NO-GAP.
        HIDE: IT_IVIT.

      ENDIF.
      IF IT_IVIT-ZFCUST EQ '2' OR IT_IVIT-ZFCUST EQ '3'
                               OR IT_IVIT-ZFCUST EQ 'Y'.
        PERFORM P3000_WRITE_IDR_DATA.
      ENDIF.

      TEMP_IVNO  = IT_IVIT-ZFIVNO.
      TEMP_IVDNO = IT_IVIT-ZFIVDNO.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " P3000_WRITE_IVIT_DATA
*&---------------------------------------------------------------------*
*&      FORM  P3000_WRITE_OTHER_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM P3000_WRITE_OTHER_DATA.

  IF IT_IVIT-ZFCUST EQ '1'.
    WRITE: 97 'To create declarance request' NO-GAP.
  ELSEIF IT_IVIT-ZFCUST EQ '2'.
    WRITE: 97 'To request declarance'        NO-GAP.
  ELSEIF IT_IVIT-ZFCUST EQ '3'.
    WRITE: 97 'In request declarance'        NO-GAP.
  ELSEIF IT_IVIT-ZFCUST EQ 'Y'.
    WRITE: 97 'Clearance completed'          NO-GAP.
  ELSEIF IT_IVIT-ZFCUST EQ 'N'.
    WRITE: 97 'Impossible to clear'          NO-GAP.
  ENDIF.
  IF IT_IVIT-ZFGRST EQ 'Y'.
    WRITE: 120 'G/R completed' NO-GAP.
  ELSEIF IT_IVIT-ZFGRST EQ 'N'.
    WRITE: 120 'To G/R'        NO-GAP.
  ELSEIF IT_IVIT-ZFGRST EQ 'X'.
    WRITE: 120 'Not object'    NO-GAP.
  ENDIF.

  WRITE 142 '' NO-GAP.

ENDFORM.                    " P3000_WRITE_OTHER_DATA
*&---------------------------------------------------------------------*
*&      FORM  P3000_WRITE_IDR_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_IDR_DATA.

  IF IT_IVIT-ZFCUST EQ 'Y'.
    READ TABLE IT_IDS WITH KEY ZFIVNO  = IT_IVIT-ZFIVNO
                               ZFIVDNO = IT_IVIT-ZFIVDNO.
*>> Customs SET!
    SELECT SINGLE * FROM ZV_COTM WHERE ZFCOTM EQ IT_IDS-ZFINRC.
    IF SY-SUBRC NE 0. CLEAR ZV_COTM. ENDIF.

    IF SY-SUBRC EQ 0.
      FORMAT RESET.
      WRITE :  /(10) 'Cust. Cle.'                NO-GAP,
                (10) IT_IDS-ZFIVNO         NO-GAP,
                (01)  '-'                  NO-GAP,
                (05)  IT_IDS-ZFIVDNO       NO-GAP,
                (09)  ' '                  NO-GAP,
                (13) IT_IDS-ZFQNT     UNIT IT_IDS-ZFQNTM  NO-GAP,
                (43)  ' '                  NO-GAP,
                (10) IT_IDS-ZFEDT        ,
                (10) IT_IDS-ZFINRC         NO-GAP,
                (20) ZV_COTM-NAME1         NO-GAP,
                (20) IT_IDS-ZFENTNO        NO-GAP.

      HIDE: IT_IDS.
    ENDIF.

    LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IVIT-ZFIVNO
                     AND    ZFIVDNO EQ  IT_IVIT-ZFIVDNO.
      PERFORM  P3000_WRITE_IN_DATA.
    ENDLOOP.
  ELSEIF IT_IVIT-ZFCUST EQ '2'
      OR IT_IVIT-ZFCUST EQ '3'.
    IF SY-SUBRC EQ 0.
      READ TABLE IT_IDR WITH KEY ZFIVNO  = IT_IVIT-ZFIVNO
                                 ZFIVDNO = IT_IVIT-ZFIVDNO.
      WRITE: /11 IT_IDR-ZFENTNO NO-GAP, 31 IT_IDR-ZFCTW NO-GAP,
              53 IT_IDR-ZFCLSEQ NO-GAP,
              68 IT_IDR-ZFQNT UNIT IT_IDR-ZFQNTM NO-GAP,
              85 IT_IDR-ZFQNTM NO-GAP, 132 IT_IDR-ZFEEDT NO-GAP,
             142 '' NO-GAP.
      HIDE: IT_IDR.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_WRITE_IDR_DATA
*&---------------------------------------------------------------------*
*&      FORM  P3000_WRITE_LO_IVIT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM P3000_WRITE_LO_IVIT_DATA.

  CLEAR : TEMP_IVNO,
          TEMP_IVDNO.

  IF IT_RN-ZFREQTY  EQ 'LO' OR IT_RN-ZFREQTY EQ 'PU'.
    LOOP AT IT_IVIT WHERE ZFREQNO = IT_RN-ZFREQNO
                      AND   ZFITMNO = IT_RN-ZFITMNO.

      LOOP  AT  IT_IN  WHERE  ZFIVNO  EQ  IT_IVIT-ZFIVNO
                       AND    ZFIVDNO EQ  IT_IVIT-ZFIVDNO.
        PERFORM  P3000_WRITE_IN_DATA.
      ENDLOOP.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " P3000_WRITE_LO_IVIT_DATA
*&---------------------------------------------------------------------*
*&      FORM  P1000_READ_IN_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM P1000_READ_IN_DATA.

  REFRESH IT_IN.

  LOOP AT IT_IVIT WHERE ZFGRST EQ 'Y'.

    MOVE-CORRESPONDING IT_IVIT  TO  IT_IN.

*>> Material Document SELECT.
    SELECT  SINGLE * FROM ZTIVHST
    WHERE   ZFIVNO   EQ   IT_IVIT-ZFIVNO
    AND     ZFIVHST  EQ   ( SELECT MAX( ZFIVHST )
                            FROM   ZTIVHST
                            WHERE  ZFIVNO  EQ  IT_IVIT-ZFIVNO ).

    MOVE : ZTIVHST-ZFIVHST    TO   IT_IN-ZFIVHST,
           ZTIVHST-BUDAT      TO   IT_IN-BUDAT,
           ZTIVHST-MBLNR      TO   IT_IN-MBLNR,
           ZTIVHST-MJAHR      TO   IT_IN-MJAHR.

    APPEND  IT_IN.

  ENDLOOP.

ENDFORM.                    " P1000_READ_IN_DATA

*&---------------------------------------------------------------------*
*&      FORM  P3000_WRITE_IN_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM P3000_WRITE_IN_DATA.

  FORMAT RESET.

  READ TABLE IT_T001L WITH KEY LGORT = IT_IN-LGORT
                               WERKS = IT_IN-WERKS.
  READ TABLE IT_T001W WITH KEY WERKS = IT_IN-WERKS.

  WRITE :  /(10) 'G/R'                 NO-GAP,
            (10) IT_IN-ZFIVNO          NO-GAP,
            (01)  '-'                  NO-GAP,
            (05) IT_IN-ZFIVDNO         NO-GAP,
            (09) ''                    NO-GAP,
            (13) IT_IN-GRMENGE  UNIT  IT_IN-MEINS,
            (42) ''                    NO-GAP,
            (10) IT_IN-BUDAT           ,
            (09) IT_IN-ZFCUNAM         ,
            (20) IT_IN-MBLNR           NO-GAP.
  HIDE: IT_IN.

ENDFORM.                    " P3000_WRITE_IN_DATA
