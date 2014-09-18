*&---------------------------------------------------------------------*
*&  INCLUDE ZRIMPRELTOP                                                *
*&---------------------------------------------------------------------*
*&  Program Name : Import Request Released Report Data Define          *
*&  Created by   : Kang Sukboung INFOLINK Ltd.                         *
*&  Created on   : 2000.01.28                                          *
*&---------------------------------------------------------------------*
*&   DESC.      :
*&
*&---------------------------------------------------------------------*

TABLES : ZTREQHD,         " Import Request Header
         ZTREQIT,         " Import Request Item
         ZTREQST,         " Import Request Status
         ZTPMTHD,         " Payment Notice Header
         ZTPMTIV,         " Payment Notice Invoice
         ZTBLINR,         " Bonded-in
         ZTIDS,           " Import license
         ZTCUCLIV,        " Clearance INVOICE....
         DD03D,           " Dynpro fields for table fields
         T024E,           " Purchasing Group
         T024,            " Purchasing Group
         LFA1,            " Vendor Master
         TINC,            " Incoterms.
         EKPO,            " Purchasing Document Item.
         ZVREQHD_ST,      " Import Request Header + Status View
         ZVEKKO_REQHD_ST, " EKKO + Import Request Header + Status View
         ZTOFF,           " OFFER SHEET
         ZTOFFFTX,        " OFFER SHEET FTX
         ZTIMIMGTX,       " EDI TEXT.
         ZTDHF1,          " EDI Flat Head
         ZTCDF1,          " Electronic Document No
         ZTIMIMG03,       " Bonded Area code
         ZTIMIMG00,       " Import System Basic Config
         SPOP,
         COBL,
         ZTIMIMG11,
         MBEW,
         T001W,
         T001K,
         T030,
         USR01,
         UF05A,
         BSIS,
         BKPF.

*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
        ZFREBELN     LIKE ZTBL-ZFREBELN,        " Repre P/O No.
        ZFBLNO       LIKE ZTBL-ZFBLNO,          " B/L No.
        ZFCIVNO      LIKE ZTCIVHD-ZFCIVNO,      " Commercial I/V
        W_PO(13)     TYPE C,
        ZFSHNO       LIKE ZTBL-ZFSHNO,          " Shipping No.
        ZFPOYN       LIKE ZTBL-ZFPOYN,          " Monetary Y/N.
        ZFETD        LIKE ZTBL-ZFETD,           " ETD
        ZFCARNM      LIKE ZTBL-ZFCARNM,         " Vessel Name.
        ZFSPRT       LIKE ZTBL-ZFSPRT,          " Loading Port.
        EKGRP        LIKE ZTBL-EKGRP,           " Purchasing Group.
        W_EKGRP(20)  TYPE C,                    " Purchasing Group name.
        LIFNR        LIKE ZTBL-LIFNR,           " Vendor.
        W_LIFNR(30)  TYPE C,                    " Vendor Name.
        ZFHBLNO      LIKE ZTBL-ZFHBLNO,         " House B/L No.
        ZFRGDSR      LIKE ZTBL-ZFRGDSR,         " Repre Name of goods.
        ZFETA        LIKE ZTBL-ZFETA,           " ETA.
        ZFRETA       LIKE ZTBL-ZFRETA,          " Real ETA.
        ZFVIA        LIKE ZTBL-ZFVIA,           " VIA
        ZFAPRT       LIKE ZTBL-ZFAPRT,          " Arrival Port.
        ZFFORD       LIKE ZTBL-ZFFORD,          " Shipping company.
        W_ZFFORD(30) TYPE C,                    " Name of Shipping Cop.
        GUBUN        TYPE C,
        ZFREQNO      LIKE ZTREQHD-ZFREQNO,      " Import Request No.
        ZFAMDNO      LIKE ZTREQST-ZFAMDNO,      " Amend Seq.
        ZFRLST1      LIKE ZTREQST-ZFRLST1,      " Release Status1
        ZFRLST2      LIKE ZTREQST-ZFRLST2,      " Release Status2
        ZFDOCST      LIKE ZTREQST-ZFDOCST,      " Document Status
        ZFEDIST      LIKE ZTREQST-ZFEDIST,      " EDI Status
        ZFEDICK      LIKE ZTREQST-ZFEDICK,      " EDI CHECK
        ZFCLOSE      LIKE ZTREQHD-ZFCLOSE,      " Close Status.
        ZFREQTY      LIKE ZTREQHD-ZFREQTY,      " Import Request type.
        ZFOPNNO      LIKE ZTREQHD-ZFOPNNO.      " L/C Approval No.
DATA: END OF IT_SELECTED.
*-----------------------------------------------------------------------
* Internal Table Define
*-----------------------------------------------------------------------
DATA : IT_ZVREQ      LIKE ZVREQHD_ST     OCCURS 0  WITH HEADER LINE.
DATA : IT_ZVOFF      LIKE ZVREQ_OFF      OCCURS 0  WITH HEADER LINE.
DATA : BAPIMEPOITEM  LIKE BAPIMEPOITEM   OCCURS 0 WITH HEADER LINE.
DATA : BAPIMEPOITEMX LIKE BAPIMEPOITEMX  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSBDIV     LIKE ZSBDIV         OCCURS 0 WITH HEADER LINE.
*-----------------------------------------------------------------------
* Internal Table Define: IT_TAB_DOWN.
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB_DOWN OCCURS 0,
         W_EDI_RECORD(65535)  TYPE C,
       END   OF IT_TAB_DOWN.

* Message Table.
DATA:   BEGIN OF XRETURN OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C.
DATA:   END   OF XRETURN.

*> RETURN MESSAGE
 DATA:   BEGIN OF RETURN OCCURS 0.
         INCLUDE STRUCTURE   BAPIRET2.
 DATA:   END   OF RETURN.

DATA : W_ERR_CHK(1)      TYPE C,
       W_REQ_CNT         TYPE I,
       W_SELECTED_LINES  TYPE P,
       W_PAGE            TYPE I,
       W_LINE            TYPE I,
       LINE(3)           TYPE N,
       W_COUNT           TYPE I,
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,
       W_TABIX          LIKE SY-TABIX,
       W_UPDATE_CNT     TYPE I,
       W_BUTTON_ANSWER  TYPE C,
       W_ITEM_CNT      LIKE SY-TABIX,
       W_AMOUNT        LIKE ZTIV-ZFIVAMT,
       W_TOT_AMOUNT    LIKE ZTIV-ZFIVAMT,
       W_LOCAL_AMT     LIKE ZTIV-ZFIVAMT,
       W_ZFPWDT        LIKE ZTPMTHD-ZFPWDT,
       W_EBELN         LIKE EKPO-EBELN,
       W_LFA1          LIKE LFA1,
       W_MENGE         LIKE ZTREQIT-MENGE,
       W_ZSREQIT       LIKE ZSREQIT,
       W_MAX_ZFAMDNO   LIKE ZTREQST-ZFAMDNO,
       P_BUKRS         LIKE ZTIMIMG00-ZFBUKRS,
       OK-CODE         LIKE SY-UCOMM,
       W_BLDAT         LIKE ZTBKPF-BLDAT,
       W_BUDAT         LIKE ZTBKPF-BUDAT,
       W_POSDT         LIKE ZTBKPF-BUDAT,
       W_DOCDT         LIKE ZTBKPF-BLDAT,
       ANTWORT         TYPE C,
       TEMP_WRBTR(16)  TYPE C,
       W_ACT_ACC       LIKE ZTBDIV-AKONT,
       W_PRI_ACC       LIKE ZTBDIV-AKONT,
       L_MENGE(17)     TYPE  C,
       L_MEINS(5)      TYPE  C,
       W_BELNR         LIKE  ZTBKPF-ZFACDO,
       W_GJAHR         LIKE  ZTBKPF-ZFFIYR,
       W_MODE          TYPE  C.


RANGES: R_ZFRLST1 FOR ZTREQST-ZFRLST1 OCCURS 0.

DATA  G_PARM_LINE.
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_LFA1_SELECT
*&---------------------------------------------------------------------*
FORM P1000_GET_LFA1_SELECT USING    P_LIFNR
                           CHANGING P_LFA1.
   CLEAR : P_LFA1.
   IF P_LIFNR IS INITIAL.
      EXIT.
   ENDIF.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
   CALL FUNCTION 'READ_LFA1'
        EXPORTING
              XLIFNR          = P_LIFNR
        IMPORTING
              XLFA1           = P_LFA1
        EXCEPTIONS
              KEY_INCOMPLETE  = 01
              NOT_AUTHORIZED  = 02
              NOT_FOUND       = 03.

   CASE SY-SUBRC.
      WHEN 01.     MESSAGE I025.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
   ENDCASE.

   TRANSLATE P_LFA1  TO UPPER CASE.

ENDFORM.                    " P1000_GET_LFA1_SELECT
