*&---------------------------------------------------------------------*
*& Include ZRIMTRCONFTOP                                               *
*&---------------------------------------------------------------------*
*&  Program Name : Delivery Completed list                             *
*&  Created By   : Na Hyun Joo                                         *
*&  Created On   : 2003.12.22                                          *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [Change Log]
*&
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------

TABLES: ZTTRHD,ZTTRIT,ZTTRITD, LIPS, LIKP, LFA1, T001W, EKPO, ZTBLIT,
        USR01, EKKO, MAKT, ZTIDSUS, ZTBL, ZTIMIMG00.

DATA : W_ERR_CHK(1)      TYPE C,
       W_FIELD_NM(6)     TYPE C,
       W_SELECTED_LINES  TYPE C,
       W_BUTTON_ANSWER   TYPE C,
       W_PAGE            TYPE C,
       W_COUNT           TYPE C,
       W_TABIX           TYPE SY-TABIX,
       W_LINE            TYPE C,
       W_LFA1            LIKE LFA1,
       W_ZFGIDT(10)      TYPE C,
       W_LIST_INDEX      TYPE C,
       WL_TRAID          LIKE LIKP-TRAID,
       P_BUKRS           LIKE ZTTRHD-BUKRS.

DATA :  BEGIN OF IT_TAB  OCCURS 0,
        MARK       TYPE  C,                " MARK
        ZFTRNO     LIKE  ZTTRHD-ZFTRNO,    " Delivery No
        ZFGIDT     LIKE  ZTTRHD-ZFGIDT,    " Delivery Date
        ZFDRDT     LIKE  ZTTRHD-ZFDRDT,    " Due Transportation date
        ZFREBELN   LIKE  ZTTRHD-ZFREBELN,  " Representive P/O
        WERKS      LIKE  ZTTRHD-WERKS,     " Representive Plant
        W_WERKS    LIKE  T001W-NAME1,      " Plant Name
        ZFTRCO     LIKE  ZTTRHD-ZFTRCO,    " Transportation agent
        W_TRCO     LIKE  LFA1-NAME1,       " Transportation agent Name
        ZFSENDER   LIKE  ZTTRHD-ZFSENDER,  " Sender
        ZFGIYN     LIKE  ZTTRHD-ZFGIYN,    " Delivery Completed yes/no
        W_GIYN(12) TYPE  C.
DATA:   END   OF IT_TAB.

DATA : IT_TMP      LIKE TABLE OF IT_TAB WITH HEADER LINE.
DATA : IT_DOIT     LIKE ZSDOIT OCCURS 50 WITH HEADER LINE.
DATA: BEGIN OF IT_TABIT OCCURS 0,
      ZFTRNO      LIKE  ZTTRIT-ZFTRNO,     " Delivery No
      TRAID       LIKE  LIKP-TRAID,        " Container No
      ZFHBLNO     LIKE  ZTBL-ZFHBLNO,      " House B/L
      ZFBLNO      LIKE  ZTBL-ZFBLNO,       " B/L Document No
      ZFETD       LIKE  ZTBL-ZFETD,        " ETD
      ZFETA       LIKE  ZTBL-ZFETA,        " ETA
      ZFRETA      LIKE  ZTBL-ZFRETA,       " Real Arriving Date
      ZFEDT       LIKE  ZTIDSUS-ZFEDT,     " Entry Date
      ZFREBELN    LIKE  ZTBL-ZFREBELN.     " Representive P/O
DATA: END OF IT_TABIT.

DATA : BEGIN OF IT_SELECTED OCCURS 0,
       ZFTRNO     LIKE ZTTRHD-ZFTRNO,      " TRANS. DOCUMENT NO.
       ZFGIDT     LIKE ZTTRHD-ZFGIDT,      " Delivery Date
       ZFDRDT     LIKE ZTTRHD-ZFDRDT,      " Due transportation date
       ZFGIYN     LIKE ZTTRHD-ZFGIYN.      " delivery yes/no
DATA : END OF IT_SELECTED.

DATA : IT_TRHD LIKE TABLE OF ZTTRHD WITH HEADER LINE.
DATA : IT_TRIT LIKE TABLE OF ZTTRIT WITH HEADER LINE.

DATA: BEGIN OF RG_SEL OCCURS 10,
         SIGN(1),
         OPTION(2),
         LOW  LIKE ZTTRHD-ZFTRNO,
         HIGH LIKE ZTTRHD-ZFTRNO,
      END   OF RG_SEL.
