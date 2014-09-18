*&---------------------------------------------------------------------*
*& Report  ZRIMMATTREE                                                 *
*&---------------------------------------------------------------------*
*&  Program Name : Hierachy by Row Material                            *
*&  Created by   : Na Shin Ho INFOLINK Ltd.                            *
*&  Created On   : 2001.01.07                                          *
*&---------------------------------------------------------------------*
*&   DESC. : 1.
*&
*&---------------------------------------------------------------------*
*& [Change Log]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMMATTREE NO STANDARD PAGE HEADING
                    MESSAGE-ID ZIM.

DATA: W_MBLNR   LIKE EKBE-BELNR,
      W_MJAHR   LIKE EKBE-GJAHR,
      W_SUBRC1  LIKE SY-SUBRC,
      W_SUBRC2  LIKE SY-SUBRC,
      W_COUNT   LIKE SY-TABIX,
      W_ZFHBLNO LIKE ZTBL-ZFHBLNO.

INCLUDE   ZRIMTREECOM.

TABLES: EKKO,
        EKPO,
        LIKP,
        LIPS,
        ZTREQHD,
        ZTREQST,
        ZTREQIT,
        ZTIV,
        ZTIVIT,
        ZTIVHST,
        ZTCIVHD,
        ZTCIVIT,
        ZTBL,
        ZTBLIT,
        LFA1,
        ZTIDR,
        ZTIDRUS,
        ZTIDRHSD,
        ZTIDRUSD,
        ZTIDS,
        ZTIDSUS,
        EKET.

*------------------------------------------*
* P/O INTERNAL TABLE Declare               *
*------------------------------------------*
DATA: BEGIN OF IT_PO OCCURS 1000,          " Internal Table IT_PO..
        EBELN    LIKE   EKKO-EBELN,        " P/O Header No..
        LIFNR    LIKE   EKKO-LIFNR,        " Vendor's Account No..
        ZTERM    LIKE   EKKO-ZTERM,        " Terms of Payment Key..
        EBELP    LIKE   EKPO-EBELP,        " P/O Item No..
        TXZ01    LIKE   EKPO-TXZ01,        " Short Text..
        MATNR    LIKE   EKPO-MATNR,        " Material No..
        MENGE    LIKE   EKPO-MENGE,        " Purchase Order Quantity..
        MEINS    LIKE   EKPO-MEINS,        " Order Unit..
        ELIKZ    LIKE   EKPO-ELIKZ,        " Delivery Comience Indicator
      END OF IT_PO.

*-----------------------------------------------*
* Import Request No INTERNAL TABLE Declare      *
*-----------------------------------------------*
DATA: BEGIN OF IT_RN OCCURS 1000,
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,
        ZFREQTY   LIKE   ZTREQHD-ZFREQTY,
        EBELN     LIKE   ZTREQHD-EBELN,
        EBELP     LIKE   ZTREQIT-EBELP,
        LIFNR     LIKE   ZTREQHD-LIFNR,
        LLIEF     LIKE   ZTREQHD-LLIEF,
        ZFBENI    LIKE   ZTREQHD-ZFBENI,
        ZTERM     LIKE   ZTREQHD-ZTERM,
        INCO1     LIKE   ZTREQHD-INCO1,
        ZFLASTAM  LIKE   ZTREQHD-ZFLASTAM,
        WAERS     LIKE   ZTREQHD-WAERS,
        ZFUSDAM   LIKE   ZTREQHD-ZFUSDAM,
        ZFMATGB   LIKE   ZTREQHD-ZFMATGB,
        ZFUSD     LIKE   ZTREQHD-ZFUSD,
        MATNR     LIKE   ZTREQIT-MATNR,
        STAWN     LIKE   ZTREQIT-STAWN,
        MENGE     LIKE   ZTREQIT-MENGE,
        MEINS     LIKE   ZTREQIT-MEINS,
        NETPR     LIKE   ZTREQIT-NETPR,
        PEINH     LIKE   ZTREQIT-PEINH,
        BPRME     LIKE   ZTREQIT-BPRME,
        TXZ01     LIKE   ZTREQIT-TXZ01,
        ZFAMDNO   LIKE   ZTREQST-ZFAMDNO,
        ZFDOCST   LIKE   ZTREQST-ZFDOCST,
        ZFRTNYN   LIKE   ZTREQST-ZFRTNYN,
        ZFRLST1   LIKE   ZTREQST-ZFRLST1,
        ZFRLST2   LIKE   ZTREQST-ZFRLST2,
        CDAT      LIKE   ZTREQST-CDAT,
        ZFREQDT   LIKE   ZTREQST-ZFREQDT,
        ZFOPNDT   LIKE   ZTREQST-ZFOPNDT,
        ERNAM     LIKE   ZTREQST-ERNAM,
        EKORG     LIKE   ZTREQST-EKORG,
        EKGRP     LIKE   ZTREQST-EKGRP,
        ZFOPNNO   LIKE   ZTREQST-ZFOPNNO,
        ZFOPAMT   LIKE   ZTREQST-ZFOPAMT,
      END OF IT_RN.
*-----------------------------------------------------------------------
* Commercial Invoice Internal Table Declaration.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CIV OCCURS 1000,
      ZFCIVRN  LIKE   ZTCIVHD-ZFCIVRN,
      ZFCIVNO  LIKE   ZTCIVHD-ZFCIVNO,
      ZFPOYN   LIKE   ZTCIVHD-ZFPOYN,
      ZFIVST   LIKE   ZTCIVHD-ZFIVST,
      ZFCIVSQ  LIKE   ZTCIVIT-ZFCIVSQ,
      EBELN    LIKE   ZTCIVIT-EBELN,
      EBELP    LIKE   ZTCIVIT-EBELP,
      ZFREQNO  LIKE   ZTCIVIT-ZFREQNO,
      ZFITMNO  LIKE   ZTCIVIT-ZFITMNO,
      ZFBLNO   LIKE   ZTCIVIT-ZFBLNO,
      ZFBLIT   LIKE   ZTCIVIT-ZFBLIT,
      MATNR    LIKE   ZTCIVIT-MATNR,
      STAWN    LIKE   ZTCIVIT-STAWN,
      CMENGE   LIKE   ZTCIVIT-CMENGE,
      ZFPRQN   LIKE   ZTCIVIT-ZFPRQN,
      MEINS    LIKE   ZTCIVIT-MEINS,
      ZFPRPYN  LIKE   ZTCIVIT-ZFPRPYN,
END OF IT_CIV.

*-----------------------------------------------------------------------
* B/L Internal Table Declaration.
*-----------------------------------------------------------------------
DATA: BEGIN OF  IT_BL OCCURS 1000,
      ZFBLNO    LIKE   ZTBL-ZFBLNO,
      ZFBLIT    LIKE   ZTBLIT-ZFBLIT,
      KOSTL     LIKE   ZTBL-KOSTL,
      ZFHBLNO   LIKE   ZTBL-ZFHBLNO,
      ZFMBLNO   LIKE   ZTBL-ZFMBLNO,
      ZFREBELN  LIKE   ZTBL-ZFREBELN,
      EKORG     LIKE   ZTBL-EKORG,
      EKGRP     LIKE   ZTBL-EKGRP,
      LIFNR     LIKE   ZTBL-LIFNR,
      ZFOPNNO   LIKE   ZTBL-ZFOPNNO,
      ZFETD     LIKE   ZTBL-ZFETD,
      ZFETA     LIKE   ZTBL-ZFETA,
      ZFRGDSR   LIKE   ZTBL-ZFRGDSR,
      ZFSPRTC   LIKE   ZTBL-ZFSPRTC,
      ZFSPRT    LIKE   ZTBL-ZFSPRT,
      ZFAPPC    LIKE   ZTBL-ZFAPPC,
      ZFAPRTC   LIKE   ZTBL-ZFAPRTC,
      ZFAPRT    LIKE   ZTBL-ZFAPRT,
      ZFNEWT    LIKE   ZTBL-ZFNEWT,
      ZFNEWTM   LIKE   ZTBL-ZFNEWTM,
      ZFTOWT    LIKE   ZTBL-ZFTOWT,
      ZFBLAMT   LIKE   ZTBL-ZFBLAMT,
      ZFBLAMC   LIKE   ZTBL-ZFBLAMC,
      ZFBLDT    LIKE   ZTBL-ZFBLDT,
      ZFBLADT   LIKE   ZTBL-ZFBLADT,
      ZFBLSDT   LIKE   ZTBL-ZFBLSDT,
      ZFPOYN    LIKE   ZTBL-ZFPOYN,
      ZFELIKZ   LIKE   ZTBL-ZFELIKZ,
      BLMENGE   LIKE   ZTBLIT-BLMENGE,
      MEINS     LIKE   ZTBLIT-MEINS,
      ZFREQNO   LIKE   ZTBLIT-ZFREQNO,
      ZFITMNO   LIKE   ZTBLIT-ZFITMNO,
      EBELN     LIKE   ZTBLIT-EBELN,
      EBELP     LIKE   ZTBLIT-EBELP,
    END OF IT_BL.

*-----------------------------------------------------------------------
* B/L Internal Table Declare..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_TEMP OCCURS 1000,
       ZFBLNO    LIKE   ZTIV-ZFBLNO,
      END OF IT_TEMP.
*-----------------------------------------------------------------------
* Unloading Table Internal Table Declare
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_CG OCCURS 0,
      ZFCGNO   LIKE  ZTCGHD-ZFCGNO,
      ZFMSNO   LIKE  ZTCGHD-ZFMSNO,
      ZFETA    LIKE  ZTCGHD-ZFETA,
      ZFARVLDT LIKE  ZTCGHD-ZFARVLDT,
      ZFCGPT   LIKE  ZTCGHD-ZFCGPT,
      BUKRS    LIKE  ZTCGHD-BUKRS,
      WERKS    LIKE  ZTCGHD-WERKS,
      ZFKEYM   LIKE  ZTCGHD-ZFKEYM,
      ZFCGIT   LIKE  ZTCGIT-ZFCGIT,
      EBELN    LIKE  ZTCGIT-EBELN,
      EBELP    LIKE  ZTCGIT-EBELP,
      ZFREQNO  LIKE  ZTCGIT-ZFREQNO,
      ZFITMNO  LIKE  ZTCGIT-ZFITMNO,
      ZFBLNO   LIKE  ZTCGIT-ZFBLNO,
      ZFBLIT   LIKE  ZTCGIT-ZFBLIT,
      CGMENGE  LIKE  ZTCGIT-CGMENGE,
      MEINS    LIKE  ZTCGIT-MEINS,
      ZFBNARCD LIKE  ZTCGIT-ZFBNARCD.
DATA  END OF IT_CG.

*-----------------------------------------------------------------------
* Bonded Area Code Internal Table Declare
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IMG03 OCCURS 1000.
        INCLUDE STRUCTURE ZTIMIMG03.
DATA  END OF IT_IMG03.

*---------------------------------------------------*
* Customs Clearance Request INTERNAL TABLE Declare. *
*---------------------------------------------------*
DATA: BEGIN OF IT_IV OCCURS 1000,
        ZFIVNO    LIKE   ZTIV-ZFIVNO,
        ZFCUST    LIKE   ZTIV-ZFCUST,
        ZFCDST    LIKE   ZTIV-ZFCDST,
        ZFCIVST   LIKE   ZTIV-ZFCIVST,
        ZFGRST    LIKE   ZTIV-ZFGRST,
        ZFPOYN    LIKE   ZTIV-ZFPOYN,
        ZFEXRT    LIKE   ZTIV-ZFEXRT,
        ZFIVDNO   LIKE   ZTIVIT-ZFIVDNO,
        ZFREQNO   LIKE   ZTIVIT-ZFREQNO,
        ZFITMNO   LIKE   ZTIVIT-ZFITMNO,
        ZFBLNO    LIKE   ZTIVIT-ZFBLNO,
        ZFBLIT    LIKE   ZTIVIT-ZFBLIT,
        ZFCGNO    LIKE   ZTIVIT-ZFCGNO,
        ZFCGIT    LIKE   ZTIVIT-ZFCGIT,
        MATNR     LIKE   ZTIVIT-MATNR,
        CCMENGE   LIKE   ZTIVIT-CCMENGE,
        MEINS     LIKE   ZTIVIT-MEINS,
        NETPR     LIKE   ZTIVIT-NETPR,
        PEINH     LIKE   ZTIVIT-PEINH,
        BPRME     LIKE   ZTIVIT-BPRME,
        TXZ01     LIKE   ZTIVIT-TXZ01,
        ZFIVAMT   LIKE   ZTIVIT-ZFIVAMT,
        ZFIVAMC   LIKE   ZTIVIT-ZFIVAMC,
        ZFIVAMK   LIKE   ZTIVIT-ZFIVAMK,
      END OF IT_IV.

*-----------------------------------------------------------------------
* Declaration of Internal Talbe for ZTIVIT Table Reference..
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IVIT OCCURS 1000.
        INCLUDE STRUCTURE ZTIVIT.
DATA:   ZFCUST    LIKE   ZTIV-ZFCUST,
        ZFHBLNO   LIKE   ZTBL-ZFHBLNO,
        ZFGRST    LIKE   ZTIV-ZFGRST.
DATA: END OF IT_IVIT.

*-----------------------------------------------------------------------
* Customs Declaration Internal Table Declare
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDR OCCURS 1000,
      ZFBLNO   LIKE   ZTIDRUS-ZFBLNO,
      ZFHBLNO  LIKE   ZTIDRUS-ZFHBLNO,
      ZFCLSEQ  LIKE   ZTIDRUS-ZFCLSEQ,
      ZFIDRNO  LIKE   ZTIDRUS-ZFENTNO,
      STAWN    LIKE   ZTIDRUSH-STAWN,
      ZFGDNM   LIKE   ZTIDRUSH-ZFGDNM,
      ZFCONO   LIKE   ZTIDRUSD-ZFCONO,
      ZFRONO   LIKE   ZTIDRUSD-ZFRONO,
      ZFQNT    LIKE   ZTIDRUSD-ZFQNT,
      ZFQNTM   LIKE   ZTIDRUSD-ZFQNTM,
      ZFIVNO   LIKE   ZTIDRUSD-ZFIVNO,
      ZFIVDNO  LIKE   ZTIDRUSD-ZFIVDNO,
  END OF IT_IDR.

*-----------------------------------------------------------------------
* Customs Clerance Internal Table Declare
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_IDS OCCURS 1000,
      ZFBLNO    LIKE    ZTIDSUS-ZFBLNO,
      ZFHBLNO   LIKE    ZTIDSUS-ZFHBLNO,
      ZFCLSEQ   LIKE    ZTIDSUS-ZFCLSEQ,
      ZFIDRNO   LIKE    ZTIDSUS-ZFENTNO,
      ZFCONO    LIKE    ZTIDSUSH-ZFCONO,
      STAWN     LIKE    ZTIDSUSH-STAWN,
      ZFGDNM    LIKE    ZTIDSUSH-ZFGDNM,
      ZFQNTM    LIKE    ZTIDSUSD-ZFQNTM,
      ZFRONO    LIKE    ZTIDSUSD-ZFRONO,
      ZFQNT     LIKE    ZTIDSUSD-ZFQNT,
      ZFIVNO    LIKE    ZTIDSUSD-ZFIVNO,
      ZFIVDNO   LIKE    ZTIDSUSD-ZFIVDNO.
DATA: END OF IT_IDS.

*-----------------------------------------------------------------------
* G/R Internal Table Declare.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_GR OCCURS 1000,
      ZFIVNO     LIKE ZTIVIT-ZFIVNO,
      ZFIVDNO    LIKE ZTIVIT-ZFIVDNO,
      ZFIVHST    LIKE ZTIVHST-ZFIVHST,
      ZFGRST     LIKE ZTIVHST-ZFGRST,
      ZFCIVHST   LIKE ZTIVHST-ZFCIVHST,
      MBLNR      LIKE ZTIVHST-MBLNR,
      MJAHR      LIKE ZTIVHST-MJAHR,
      GRMENGE    LIKE ZTIVHSTIT-GRMENGE,
      EBELN      LIKE ZTIVIT-EBELN,
      EBELP      LIKE ZTIVIT-EBELP,
      ZFREQNO    LIKE ZTIVIT-EBELP,
      ZFITMNO    LIKE ZTIVIT-ZFITMNO,
      MEINS      LIKE ZTIVHSTIT-MEINS.
DATA: END OF IT_GR.

DATA: W_TABIX    LIKE SY-TABIX,
      W_PO_HD_ST LIKE ZVEKKO_REQHD_ST,
      W_ERR_CHK  TYPE C,
      W_PAGE     TYPE I,
      W_BL_CNT   TYPE I,
      W_TR_CNT   TYPE I,
      W_CHECK    TYPE C VALUE 'N',
      W_AMOUNT   LIKE ZTIV-ZFIVAMT,
      COUNT      TYPE I.

*-----------------------------------------------------------------------
* HIDE VARIABLE.
*-----------------------------------------------------------------------
DATA: BEGIN OF DOCU,
        TYPE(2)   TYPE C,
        CODE      LIKE EKKO-EBELN,
        ITMNO     LIKE EKPO-EBELP,
        YEAR      TYPE I,
      END OF DOCU.

*-----------------------------------------------------------------------
* Search Condition Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_EBELN   FOR EKKO-EBELN,       " P/O No.
               S_MATNR   FOR EKPO-MATNR,       " Material No.
               S_REQNO   FOR ZTREQHD-ZFREQNO,  " Import Request No.
               S_LIFNR   FOR ZTREQHD-LIFNR,    " Vendor.
               S_MATGB   FOR ZTREQHD-ZFMATGB,  " Material Type.
               S_REQTY   FOR ZTREQHD-ZFREQTY,  " Import Request Type
               S_EKORG   FOR ZTREQST-EKORG,    " Purch. Org.
               S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
               S_WERKS   FOR EKPO-WERKS,       " Plant
               S_HBLNO   FOR ZTBL-ZFHBLNO,     " House B/L
               S_CONT    FOR LIKP-TRAID.       " Container No
PARAMETERS :   P_NAME    LIKE USR02-BNAME.     " Charge In

SELECT-OPTIONS:
               S_DOCST   FOR ZTREQST-ZFDOCST.  " Document Status

SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
INITIALIZATION.
   SET TITLEBAR 'TIT1'.

*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Import Request Data Get
   PERFORM P1000_READ_RN_DATA USING W_ERR_CHK.
   CHECK W_ERR_CHK NE 'Y'.

* P/O Table Select..
  PERFORM P1000_READ_PO_TABLE.
* Invoice No..
   PERFORM P1000_READ_CIV_DATA.
* B/L Table Select..
   PERFORM P1000_READ_BL_DATA.
* Unloading Table Select..
   PERFORM P1000_READ_CG_DATA.
* Bonded Area Code Table Select..
  PERFORM P1000_READ_IMG03_DATA.
* Customs Clearance/G.R. Request Data Select
   PERFORM P1000_READ_ZTIVIT_DATA.
* Customs Declaration Table Select..
   PERFORM P1000_READ_ZFIDR_DATA.
* Customs Clearance Table Select..
   PERFORM P1000_READ_ZFIDS_DATA.
* G/R Table Select.
   PERFORM P1000_READ_GR_DATA.
*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
END-OF-SELECTION.

   CHECK W_ERR_CHK NE 'Y'.

* Title Text Write.
   SET TITLEBAR 'TIT1'.
   SET PF-STATUS 'ZIM94'.

* HEAD WRITE
   PERFORM   P3000_FILL_ITAB_HEAD.
* Sort P/O, Request No. Internal Table.
   PERFORM P2000_SORT_IT_DATA.

   LOOP AT IT_PO.
      IF IT_PO-MATNR IS INITIAL.
            ON CHANGE OF IT_PO-TXZ01.
            PERFORM   P3000_LIFNR_NODE_WRITE.
         ENDON.
      ELSE.
         ON CHANGE OF IT_PO-MATNR.
            PERFORM   P3000_LIFNR_NODE_WRITE.
         ENDON.
      ENDIF.

* P/O, Item Change.
      ON CHANGE OF IT_PO-EBELN OR IT_PO-EBELP.
         PERFORM   P3000_EBELN_NODE_WRITE.
      ENDON.
* Import Request
      PERFORM P2000_ZTREQHD_PROCESS.
   ENDLOOP.
* Hierarchy output
   PERFORM HIERARCHY.                   " construct & draw the tree

*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'ERLE'.
      PERFORM    P2000_NODE_ACTION.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_PO
     FROM   EKKO AS H INNER JOIN EKPO AS I
     ON     H~EBELN EQ I~EBELN
     WHERE  H~EBELN IN S_EBELN
     AND    I~MATNR IN S_MATNR
     AND    H~LIFNR IN S_LIFNR
     AND    H~EKORG IN S_EKORG
     AND    H~EKGRP IN S_EKGRP
     AND    I~WERKS IN S_WERKS.

   IF SY-SUBRC NE 0.
   WRITE 'NOT A VALID INPUT.'.
   EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_PO_DATA

*&------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&------------------------------------------------------------------*
FORM P1000_READ_RN_DATA USING W_ERR_CHK.

   DATA: L_LINE_COUNT TYPE I.

   W_ERR_CHK = 'N'.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_RN
     FROM  ( ( ZTREQHD AS H INNER JOIN ZTREQIT AS I
               ON H~ZFREQNO EQ I~ZFREQNO )
                 INNER JOIN EKPO AS P
                       ON  I~EBELN EQ P~EBELN
                       AND I~EBELP EQ P~EBELP )
                       INNER JOIN EKKO AS K
                       ON  I~EBELN EQ K~EBELN
     WHERE  H~ZFREQNO  IN  S_REQNO
     AND    H~EBELN    IN  S_EBELN
     AND    I~MATNR    IN  S_MATNR
     AND    H~LIFNR    IN  S_LIFNR
     AND    H~ZFMATGB  IN  S_MATGB
     AND    H~ZFREQTY  IN  S_REQTY
     AND    K~EKORG    IN  S_EKORG
     AND    K~EKGRP    IN  S_EKGRP
     AND    K~BSTYP    EQ  'F'
     AND    P~WERKS    IN  S_WERKS.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

   CONCATENATE P_NAME '%' INTO P_NAME.

   LOOP AT IT_RN.
     W_TABIX = SY-TABIX.
     SELECT SINGLE ZFAMDNO ZFDOCST ZFRLST1 ZFRLST2 CDAT EKORG
                   EKGRP ZFOPNNO WAERS
              INTO (IT_RN-ZFAMDNO, IT_RN-ZFDOCST, IT_RN-ZFRLST1,
              IT_RN-ZFRLST2, IT_RN-CDAT, IT_RN-EKORG, IT_RN-EKGRP,
              IT_RN-ZFOPNNO, IT_RN-WAERS)
              FROM ZTREQST
              WHERE ZFREQNO EQ   IT_RN-ZFREQNO
              AND   EKORG   IN   S_EKORG
              AND   EKGRP   IN   S_EKGRP
              AND   ZFDOCST IN   S_DOCST
              AND   ERNAM   LIKE P_NAME
              AND   ZFAMDNO EQ ( SELECT MAX( ZFAMDNO ) FROM ZTREQST
                                 WHERE ZFREQNO EQ IT_RN-ZFREQNO ).

     IF SY-SUBRC = 0.
        MODIFY IT_RN INDEX W_TABIX.
     ELSE.
        DELETE IT_RN INDEX W_TABIX.
     ENDIF.
     SELECT SINGLE *
            FROM  ZTREQIT
            WHERE ZFREQNO = IT_RN-ZFREQNO
              AND ZFITMNO = IT_RN-ZFITMNO.
     IF SY-SUBRC EQ 0.
        MOVE ZTREQIT-MENGE TO IT_RN-MENGE.
        MODIFY IT_RN INDEX W_TABIX.
     ENDIF.

   ENDLOOP.

   DESCRIBE TABLE IT_RN LINES L_LINE_COUNT.
   IF L_LINE_COUNT = 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

   SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_CIV
        FROM   ZTCIVHD AS H INNER JOIN ZTCIVIT AS I
        ON     H~ZFCIVRN EQ I~ZFCIVRN
        FOR ALL ENTRIES IN IT_RN
        WHERE  I~EBELN   EQ IT_RN-EBELN
        AND    I~EBELP   EQ IT_RN-EBELP
        AND    I~ZFREQNO    EQ  IT_RN-ZFREQNO
        AND    I~ZFITMNO    EQ  IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_CIV_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_TABLE
*&---------------------------------------------------------------------*
FORM P1000_READ_PO_TABLE.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_PO
     FROM   EKKO AS H INNER JOIN EKPO AS I
     ON     H~EBELN EQ I~EBELN
     FOR ALL ENTRIES IN IT_RN
     WHERE  H~EBELN EQ IT_RN-EBELN
     AND    I~EBELP EQ IT_RN-EBELP.

     LOOP AT IT_RN.

        W_TABIX  =  SY-TABIX.

        IF NOT S_HBLNO[] IS INITIAL.

           CLEAR : W_BL_CNT.
           SELECT COUNT( * )   INTO  W_BL_CNT
           FROM   ZTBL  AS   A INNER JOIN ZTBLIT AS B
           ON     A~ZFBLNO     EQ    B~ZFBLNO
           WHERE  A~ZFHBLNO    IN    S_HBLNO
           AND    B~EBELN      EQ    IT_RN-EBELN
           AND    B~EBELP      EQ    IT_RN-EBELP.

           IF W_BL_CNT LE 0.
              DELETE  IT_RN  INDEX  W_TABIX.
              DELETE  IT_PO  WHERE  EBELN  =  IT_RN-EBELN
                             AND    EBELP  =  IT_RN-EBELP.
              CONTINUE.
           ENDIF.
        ENDIF.

        ">> Container No
        IF NOT S_CONT[] IS INITIAL.

           CLEAR : W_TR_CNT.
           SELECT COUNT( * )   INTO  W_TR_CNT
           FROM   LIKP  AS  A  INNER JOIN  LIPS AS B
           ON     A~VBELN      EQ    B~VBELN
           WHERE  A~TRAID      IN    S_CONT
           AND    B~VGBEL      EQ    IT_RN-EBELN
           AND    B~VGPOS      EQ    IT_RN-EBELP.

           IF W_TR_CNT LE 0.
              DELETE  IT_RN  INDEX  W_TABIX.
              DELETE  IT_PO  WHERE  EBELN = IT_RN-EBELN
                             AND    EBELP = IT_RN-EBELP.
              CONTINUE.
           ENDIF.
        ENDIF.

     ENDLOOP.

ENDFORM.                    " P1000_READ_PO_TABLE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

  SELECT *
       INTO CORRESPONDING FIELDS OF TABLE IT_BL
        FROM   ZTBL AS H INNER JOIN ZTBLIT AS I
        ON     H~ZFBLNO EQ I~ZFBLNO
        FOR ALL ENTRIES IN IT_RN
        WHERE  I~EBELN    EQ IT_RN-EBELN
        AND    I~EBELP    EQ IT_RN-EBELP
        AND    I~ZFREQNO  EQ  IT_RN-ZFREQNO
        AND    I~ZFITMNO  EQ  IT_RN-ZFITMNO.

ENDFORM.                    " P1000_READ_BL_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZFIDR_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_ZFIDR_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_IDR
     FROM ZTIDRUS AS  H  INNER JOIN ZTIDRUSD AS I
     ON   H~ZFIVNO   EQ I~ZFIVNO
     AND  H~ZFCLSEQ  EQ I~ZFCLSEQ
     FOR ALL ENTRIES IN IT_IVIT
     WHERE I~ZFIVNO  EQ IT_IVIT-ZFIVNO
     AND   I~ZFIVDNO EQ IT_IVIT-ZFIVDNO.

ENDFORM.                    " P1000_READ_ZFIDR_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZFIDS_DATA
*&---------------------------------------------------------------------*
*       수입면허 테이블 조회를 위한 Selection 문장..
*----------------------------------------------------------------------*
FORM P1000_READ_ZFIDS_DATA.

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_IDS
    FROM ZTIDSUS  AS  H  INNER  JOIN  ZTIDSUSD  AS  I
    ON   H~ZFIVNO    EQ  I~ZFIVNO
    AND  H~ZFCLSEQ   EQ  I~ZFCLSEQ
    FOR ALL ENTRIES  IN IT_IDR
    WHERE H~ZFIVNO   EQ  IT_IDR-ZFIVNO
      AND H~ZFCLSEQ  EQ  IT_IDR-ZFCLSEQ.

ENDFORM.                    " P1000_READ_ZFIDS_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_IT_DATA
*&---------------------------------------------------------------------*
*       SORTING INTERNAL TABLE..
*----------------------------------------------------------------------*
FORM P2000_SORT_IT_DATA.

  SORT IT_PO BY MATNR EBELN EBELP.
  SORT IT_RN BY EBELN ZFREQNO ZFITMNO ZFREQTY.

ENDFORM.                    " P2000_SORT_IT_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_FILL_ITAB_HEAD
*&---------------------------------------------------------------------*
FORM P3000_FILL_ITAB_HEAD.

  CLEAR : IT_TREELIST.
  IT_TREELIST-NAME = 'ROOT'.
  IT_TREELIST-COLOR = 3.
  IT_TREELIST-INTENSIV = '0'.
  IT_TREELIST-TEXT = 'Import progress Hierarchy'.
  IT_TREELIST-TLENGTH = STRLEN( IT_TREELIST-TEXT ).
  IT_TREELIST-TLEVEL = 1.
  IT_TREELIST-TCOLOR = 0.
  IT_TREELIST-TINTENSIV = '0'.
  IT_TREELIST-TEXT1 = '(By material,P/O,Import request)'.
  IT_TREELIST-TLENGTH1 = STRLEN( IT_TREELIST-TEXT1 ).
  IT_TREELIST-TCOLOR1 = 0.
  IT_TREELIST-TINTENSIV1 = '0'.

  APPEND      IT_TREELIST.

ENDFORM.                    " P3000_FILL_ITAB_HEAD

*&---------------------------------------------------------------------*
*&      Form  P3000_LIFNR_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LIFNR_NODE_WRITE.

  CLEAR : IT_TREELIST.

  IT_TREELIST-ID       = 2.                  " ID
  IT_TREELIST-TLEVEL   = 2.                  " LEVEL
  IT_TREELIST-NAME     = 'VD'.               " LEVEL
  IT_TREELIST-COLOR    = 9.                  " COLOR
  IT_TREELIST-INTENSIV = '1'.                " INTENSIVE

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
             INPUT   =   IT_PO-MATNR
       IMPORTING
             OUTPUT  =   IT_PO-MATNR.

* Material CODE
  IT_TREELIST-TEXT       = IT_PO-MATNR.
  IT_TREELIST-TLENGTH    = 18.
  IT_TREELIST-TCOLOR     = 9.                     " 색?
  IT_TREELIST-TINTENSIV  = '1'.                   " 밝?

* Material Description.
  IT_TREELIST-TEXT1      = IT_PO-TXZ01.
  IT_TREELIST-TLENGTH1   = 35.
  IT_TREELIST-TCOLOR1    = 9.
  IT_TREELIST-TINTENSIV1 = '1'.
* SPACE
  IT_TREELIST-TEXT2      = SPACE. " LFA1-NAME1.
  IT_TREELIST-TLENGTH2   = 46.
  IT_TREELIST-TCOLOR2    = 9.
  IT_TREELIST-TINTENSIV2 = '0'.

  APPEND     IT_TREELIST.

ENDFORM.                    " P3000_LIFNR_NODE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_EBELN_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_EBELN_NODE_WRITE.

   CLEAR : IT_TREELIST.

   IT_TREELIST-ID       = 3.                  " ID
   IT_TREELIST-TLEVEL   = 3.                  " LEVEL
   IT_TREELIST-NAME     = 'PO'.               " LEVEL
   IT_TREELIST-COLOR    = 2.                  " COLOR
   IT_TREELIST-INTENSIV = '1'.                " INTENSIVE
   IT_TREELIST-HIDE     = 'ME23N'.             " T-CODE
* P/O Number..
   IT_TREELIST-TEXT       = IT_PO-EBELN.
   IT_TREELIST-TLENGTH    = STRLEN( IT_TREELIST-TEXT ).
   IT_TREELIST-TCOLOR     = 2.
   IT_TREELIST-TINTENSIV  = '1'.
* PO Item No..
   IT_TREELIST-TEXT1      = IT_PO-EBELP.
   IT_TREELIST-TLENGTH1   = STRLEN( IT_TREELIST-TEXT1 ).
   IT_TREELIST-TCOLOR1    = 2.
   IT_TREELIST-TINTENSIV1 = '1'.

*  SPACE.
   IT_TREELIST-TEXT3      = SPACE.
   IT_TREELIST-TLENGTH3   = 52.
   IT_TREELIST-TCOLOR3    = 2.
   IT_TREELIST-TINTENSIV3 = '0'.
* Quantity.
   WRITE IT_PO-MENGE TO IT_TREELIST-TEXT4(23) UNIT IT_PO-MEINS.
   IT_TREELIST-TLENGTH4   = 23. "STRLEN( IT_TREELIST-TEXT4 ).
   IT_TREELIST-TCOLOR4   = 2.
   IT_TREELIST-TINTENSIV4 = '0'.

* Unit of measure.
   IT_TREELIST-TEXT5      = IT_PO-MEINS.
   IT_TREELIST-TLENGTH5   = 3. "STRLEN( IT_TREELIST-TEXT5 ).
   IT_TREELIST-TCOLOR5    = 2.
   IT_TREELIST-TINTENSIV5 = '0'.

   APPEND     IT_TREELIST.

ENDFORM.                    " P3000_EBELN_NODE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_ZFREQNO_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ZFREQNO_NODE_WRITE.

   CLEAR : IT_TREELIST.

   IT_TREELIST-ID       = 4.                  " ID
   IT_TREELIST-TLEVEL   = 4.                  " LEVEL
   IT_TREELIST-NAME     = 'RN'.               " LEVEL
   IT_TREELIST-COLOR    = 3.                  " COLOR
   IT_TREELIST-INTENSIV = '1'.                " INTENSIVE
   IT_TREELIST-HIDE     = 'ZIM03'.            " T-CODE
* Import Request Document No
   IT_TREELIST-TEXT       = IT_RN-ZFREQNO.
   IT_TREELIST-TLENGTH    = STRLEN( IT_TREELIST-TEXT ).
   IT_TREELIST-TCOLOR     = 3.
   IT_TREELIST-TINTENSIV  = '1'.
* Amend Sequence
   IT_TREELIST-TEXT1      = IT_RN-ZFAMDNO.
   IT_TREELIST-TLENGTH1   = STRLEN( IT_TREELIST-TEXT1 ).
   IT_TREELIST-TCOLOR1    = 3.
   IT_TREELIST-TINTENSIV1 = '1'.
* Approve No
   IT_TREELIST-TEXT2      = IT_RN-ZFOPNNO.
   IT_TREELIST-TLENGTH2   = 30.
   IT_TREELIST-TCOLOR2    = 3.
   IT_TREELIST-TINTENSIV2 = '0'.
* Payment Type
   IT_TREELIST-TEXT3 = IT_RN-ZFREQTY.
   IT_TREELIST-TLENGTH3   = 3.
   IT_TREELIST-TCOLOR3    = 3.
   IT_TREELIST-TINTENSIV3 = '0'.
* SPACE.
   IT_TREELIST-TEXT4 = SPACE.
   IT_TREELIST-TLENGTH4   = 13.
   IT_TREELIST-TCOLOR4    = 3.
   IT_TREELIST-TINTENSIV4 = '0'.

* Import Request Quantity
   WRITE IT_RN-MENGE TO IT_TREELIST-TEXT5(23) UNIT IT_RN-MEINS.
   IT_TREELIST-TLENGTH5   = 23.
   IT_TREELIST-TCOLOR5    = 3.
   IT_TREELIST-TINTENSIV5 = '0'.

* Import Request qty uom
   IT_TREELIST-TEXT6 = IT_RN-MEINS.
   IT_TREELIST-TLENGTH6   = 3.
   IT_TREELIST-TCOLOR6    = 3.
   IT_TREELIST-TINTENSIV6 = '0'.

   APPEND     IT_TREELIST.

ENDFORM.                    " P3000_ZFREQNO_NODE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_ZFBLNO_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ZFBLNO_NODE_WRITE.

   CLEAR : IT_TREELIST.

   IT_TREELIST-ID       = 6.                  " ID
   IT_TREELIST-TLEVEL   = 6.                  " LEVEL
   IT_TREELIST-NAME     = 'BL'.               " LEVEL
   IT_TREELIST-COLOR    = 5.                  " COLOR
   IT_TREELIST-INTENSIV = '1'.                " INTENSIVE
   IT_TREELIST-HIDE     = 'ZIM23'.            " T-CODE

*B/L No.
   IT_TREELIST-TEXT1      = IT_BL-ZFBLNO.
   IT_TREELIST-TLENGTH1   = 10.
   IT_TREELIST-TCOLOR1    = 5.                     " 색상.
   IT_TREELIST-TINTENSIV1 = '1'.                   " 밝기.
* ITEM NO.
   IT_TREELIST-TEXT2      = IT_BL-ZFBLIT.
   IT_TREELIST-TLENGTH2   = 5.
   IT_TREELIST-TCOLOR2    = 5.                     " 색상.
   IT_TREELIST-TINTENSIV2 = '0'.                   " 밝기.

   CLEAR W_COUNT.
   IF W_SUBRC1 NE 0.
      W_COUNT = 4.
   ENDIF.
* HOUSE BL NO.
   IT_TREELIST-TEXT3      = IT_BL-ZFHBLNO.
   IT_TREELIST-TLENGTH3   = 40 + W_COUNT.
   IT_TREELIST-TCOLOR3    = 5.
   IT_TREELIST-TINTENSIV3 = '0'.
* qty
   WRITE  IT_BL-BLMENGE  TO IT_TREELIST-TEXT4(23) UNIT IT_BL-MEINS.
   IT_TREELIST-TLENGTH4   = 23.
   IT_TREELIST-TCOLOR4    = 5.
   IT_TREELIST-TINTENSIV4 = '0'.

* uom
   IT_TREELIST-TEXT5      = IT_BL-MEINS .
   IT_TREELIST-TLENGTH5   = 3.
   IT_TREELIST-TCOLOR5    = 5.
   IT_TREELIST-TINTENSIV5 = '0'.

   APPEND IT_TREELIST.

ENDFORM.                    " P3000_ZFBLNO_NODE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTCIVHD_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ZTCIVHD_NODE_WRITE.

  CLEAR : IT_TREELIST.

  IT_TREELIST-ID       = 5.                   " ID
  IT_TREELIST-TLEVEL   = 5.                   " LEVEL
  IT_TREELIST-NAME     = 'CIV'.               " LEVEL
  IT_TREELIST-COLOR    =  4.                  " COLOR
  IT_TREELIST-INTENSIV = '1'.                " INTENSIVE
  IT_TREELIST-HIDE     = 'ZIM37'.            " T-CODE

*I/V No.
  MOVE  IT_CIV-ZFCIVNO TO IT_TREELIST-TEXT1.
  IF IT_CIV-ZFCIVNO IS INITIAL.
     MOVE IT_CIV-ZFCIVRN TO IT_TREELIST-TEXT1.
  ENDIF.

  IT_TREELIST-TLENGTH1   = 35.
  IT_TREELIST-TCOLOR1    = 4.
  IT_TREELIST-TINTENSIV1 = '1'.

  IT_TREELIST-TEXT2      = IT_CIV-ZFCIVSQ.
  IT_TREELIST-TLENGTH2   = 5.
  IT_TREELIST-TCOLOR2    = 4.
  IT_TREELIST-TINTENSIV2 = '0'.

  IF IT_CIV-ZFPOYN EQ 'Y'.
      IF IT_CIV-ZFPRPYN EQ 'Y'.
         IT_TREELIST-TEXT3      = 'Advance payment'.
         IT_TREELIST-TLENGTH3   = 5.
      ELSE.
         IT_TREELIST-TEXT3      = 'Monetary'.
         IT_TREELIST-TLENGTH3   = 5.
      ENDIF.
  ELSE.
      IT_TREELIST-TEXT3      = 'Non-monetary'.
      IT_TREELIST-TLENGTH3   = 5.
  ENDIF.
  IT_TREELIST-TCOLOR3    = 4.
  IT_TREELIST-TINTENSIV3 = '0'.
*>> SPACE.
  IT_TREELIST-TEXT4      = SPACE.
  IT_TREELIST-TLENGTH4   = 13.
  IT_TREELIST-TCOLOR4    = 4.
  IT_TREELIST-TINTENSIV4 = '0'.

*>> QTY
  WRITE IT_CIV-CMENGE TO IT_TREELIST-TEXT5(23) UNIT IT_CIV-MEINS.
  IT_TREELIST-TLENGTH5   = 23.
  IT_TREELIST-TCOLOR5    = 4.
  IT_TREELIST-TINTENSIV5 = '0'.
*>> UOM
  IT_TREELIST-TEXT6      = IT_CIV-MEINS.
  IT_TREELIST-TLENGTH6   = 3.
  IT_TREELIST-TCOLOR6    = 4.
  IT_TREELIST-TINTENSIV6 = '0'.

  APPEND     IT_TREELIST.

ENDFORM.                    " P3000_ZTCIVHD_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_NODE_SELECT
*&---------------------------------------------------------------------*
FORM P2000_NODE_SELECT TABLES   KNOTEN   STRUCTURE     SEUCOMM
                       USING    EXIT.
   CASE KNOTEN-NAME.
      WHEN 'PO'.     " P/O
         SET PARAMETER ID 'BES' FIELD KNOTEN-TEXT.
         SET PARAMETER ID 'BSP' FIELD KNOTEN-TEXT1.
      WHEN 'RN'.     " L/C
         SET PARAMETER ID 'ZPREQNO' FIELD KNOTEN-TEXT.
         SET PARAMETER ID 'ZPAMDNO' FIELD KNOTEN-TEXT1.
         SET PARAMETER ID 'ZPOPNNO' FIELD ''.
         SET PARAMETER ID 'BES'     FIELD ''.
         IF KNOTEN-TEXT1(5) EQ '00000'.
         CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
         ELSE.
         CALL TRANSACTION 'ZIM13' AND SKIP FIRST SCREEN.
         ENDIF.
         EXIT = ' '.      EXIT.
      WHEN 'CIV'.     " Invoice,
         READ TABLE IT_CIV WITH KEY ZFCIVNO = KNOTEN-TEXT1.
         IF SY-SUBRC EQ 0.
            SET PARAMETER ID 'ZPCIVRN' FIELD ' '.
            SET PARAMETER ID 'ZPCIVNO'  FIELD KNOTEN-TEXT1.
         ELSE.
            SET PARAMETER ID 'ZPCIVNO'  FIELD ' '.
            SET PARAMETER ID 'ZPCIVRN' FIELD  KNOTEN-TEXT1.
         ENDIF.
      WHEN 'BL'.     " B/L
         SET PARAMETER ID 'ZPBLNO'  FIELD KNOTEN-TEXT1.
         SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      WHEN 'CG'.     " Unloading
         SET PARAMETER ID 'ZPCGNO'  FIELD KNOTEN-TEXT1.
      WHEN 'IV'.     "  Customs Clearance Request
         SET PARAMETER ID 'ZPIVNO'  FIELD KNOTEN-TEXT2.
         SET PARAMETER ID 'ZPHBLNO' FIELD ''.
         SET PARAMETER ID 'ZPBLNO'  FIELD ''.

      WHEN 'IDR'.    " Customs Declaration
         SET PARAMETER ID 'ZPIVNO'  FIELD KNOTEN-TEXT1.
         SET PARAMETER ID 'ZPCLSEQ' FIELD ' '.

      WHEN 'IDS'.    " Customs Clearance
         SET PARAMETER ID 'ZPIVNO'  FIELD KNOTEN-TEXT1.
         SET PARAMETER ID 'ZPCLSEQ' FIELD ' '.
         SET PARAMETER ID 'ZPENTNO' FIELD ' '.
      WHEN 'GR'.     " Material Document No
         SET  PARAMETER ID  'BUK'   FIELD   ' '.
         SET  PARAMETER ID  'MBN'   FIELD   KNOTEN-TEXT1.
         SET  PARAMETER ID  'MJA'   FIELD   KNOTEN-TEXT2.

         WRITE: KNOTEN-TEXT1 TO W_MBLNR,
                KNOTEN-TEXT2 TO W_MJAHR.

         CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              I_ACTION                  = 'A04'
              I_REFDOC                  = 'R02'
              I_NOTREE                  = 'X'
              I_SKIP_FIRST_SCREEN       = 'X'
              I_OKCODE                  = 'OK_GO'
              I_MBLNR                   = W_MBLNR
              I_MJAHR                   = W_MJAHR
           EXCEPTIONS
              ILLEGAL_COMBINATION       = 1
              OTHERS                    = 2.

      WHEN OTHERS.
         MESSAGE E962.
   ENDCASE.
   IF KNOTEN-NAME NE 'GR' AND KNOTEN-NAME NE 'IDR'  AND
      KNOTEN-NAME NE 'IDS'.
     CALL TRANSACTION KNOTEN-HIDE(5) AND SKIP  FIRST SCREEN.
     EXIT = ' '.
   ELSEIF KNOTEN-NAME EQ 'IDR' OR KNOTEN-NAME EQ 'IDS'.
      CALL TRANSACTION KNOTEN-HIDE(6) AND SKIP  FIRST SCREEN.
      EXIT = ' '.
   ENDIF.

ENDFORM.                    "

*&---------------------------------------------------------------------*
*&      Form  P2000_HIDE_VAR_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_HIDE_VAR_MOVE USING   P_TYPE P_CODE P_ITMNO P_YEAR.

CLEAR :  DOCU.

MOVE: P_TYPE    TO   DOCU-TYPE,
      P_CODE    TO   DOCU-CODE,
      P_ITMNO   TO   DOCU-ITMNO,
      P_YEAR    TO   DOCU-YEAR.

HIDE  :  DOCU.
CLEAR :  DOCU.

ENDFORM.                    " P2000_HIDE_VAR_MOVE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CG_DATA.

   IF NOT IT_BL[] IS INITIAL.

      SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_CG
         FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
         ON     H~ZFCGNO   EQ  I~ZFCGNO
        FOR ALL ENTRIES    IN  IT_BL
        WHERE  I~EBELN   EQ  IT_BL-EBELN
          AND  I~EBELP   EQ IT_BL-EBELP
          AND  I~ZFREQNO EQ IT_BL-ZFREQNO
          AND  I~ZFITMNO EQ IT_BL-ZFITMNO
          AND  I~ZFBLNO  EQ IT_BL-ZFBLNO
          AND  I~ZFBLIT  EQ IT_BL-ZFBLIT.

   ENDIF.

ENDFORM.                    " P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_GR_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_GR_DATA.

   SELECT  * INTO CORRESPONDING FIELDS OF TABLE IT_GR
   FROM    ZTIVHST  AS  A  INNER JOIN  ZTIVHSTIT  AS  B
   ON      A~ZFIVNO        EQ          B~ZFIVNO
   AND     A~ZFIVHST       EQ          B~ZFIVHST
   FOR     ALL  ENTRIES    IN          IT_IVIT
   WHERE   B~ZFIVNO        EQ          IT_IVIT-ZFIVNO
   AND     B~ZFIVDNO       EQ          IT_IVIT-ZFIVDNO
   AND     B~ZFGRST        EQ          'Y'.

ENDFORM.                    " P1000_READ_GR_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_ZFCGNO_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_ZFCGNO_PROCESS.

  CLEAR IT_CG.
  LOOP AT IT_CG WHERE EBELN   = IT_BL-EBELN
                AND   EBELP   = IT_BL-EBELP
                AND   ZFREQNO = IT_BL-ZFREQNO
                AND   ZFITMNO = IT_BL-ZFITMNO
                AND   ZFBLNO  = IT_BL-ZFBLNO
                AND   ZFBLIT  = IT_BL-ZFBLIT.
      ON CHANGE OF IT_CG-ZFCGNO OR IT_CG-ZFCGIT.
         PERFORM  P3000_ZFCGNO_NODE_WRITE.
      ENDON.

  ENDLOOP.
  W_SUBRC2 = SY-SUBRC.

ENDFORM.                    " P2000_ZFCGNO_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P3000_ZFCGNO_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ZFCGNO_NODE_WRITE.

   CLEAR : IT_TREELIST.

   IT_TREELIST-ID       = 7.                  " ID
   IT_TREELIST-TLEVEL   = 7.                  " LEVEL
   IT_TREELIST-NAME     = 'CG'.               " LEVEL
   IT_TREELIST-COLOR    = 10.                 " COLOR
   IT_TREELIST-INTENSIV = '1'.                " INTENSIVE
   IT_TREELIST-HIDE     = 'ZIM83'.            " T-CODE

*.Unloading Document No
   IT_TREELIST-TEXT1      = IT_CG-ZFCGNO.
   IT_TREELIST-TLENGTH1   = 10.
   IT_TREELIST-TCOLOR1    = 10.
   IT_TREELIST-TINTENSIV1 = '1'.

   IT_TREELIST-TEXT2      = IT_CG-ZFCGIT.
   IT_TREELIST-TLENGTH2   = 5.
   IT_TREELIST-TCOLOR2    = 10.
   IT_TREELIST-TINTENSIV2 = '0'.
*>> space
   IT_TREELIST-TEXT3      = SPACE.
   IT_TREELIST-TLENGTH3   = 36.
   IT_TREELIST-TCOLOR3    = 10.
   IT_TREELIST-TINTENSIV3 = '0'.

*>> quantity
   WRITE IT_CG-CGMENGE TO IT_TREELIST-TEXT4(23) UNIT IT_CG-MEINS.
   IT_TREELIST-TLENGTH4   = 23.
   IT_TREELIST-TCOLOR4    = 10.
   IT_TREELIST-TINTENSIV4 = '0'.
*>> UOM
   IT_TREELIST-TEXT5      = IT_CG-MEINS.
   IT_TREELIST-TLENGTH5   = 3.
   IT_TREELIST-TCOLOR5    = 10.
   IT_TREELIST-TINTENSIV5 = '0'.

   APPEND IT_TREELIST.

ENDFORM.                    " P3000_ZFCGNO_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIDR_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_ZTIDR_PROCESS.

  CLEAR IT_IDR.
  LOOP AT IT_IDR WHERE ZFBLNO  = IT_IVIT-ZFBLNO
                   AND ZFIVNO  = IT_IVIT-ZFIVNO
                  AND  ZFIVDNO = IT_IVIT-ZFIVDNO.
     ON CHANGE OF IT_IDR-ZFIVNO OR  IT_IDR-ZFCLSEQ OR
                 IT_IDR-ZFCONO  OR IT_IDR-ZFRONO.
       PERFORM P3000_ZTIDR_NODE_WRITE.
     ENDON.

  ENDLOOP.

ENDFORM.                    " P2000_ZTIDR_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDR_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ZTIDR_NODE_WRITE.

   CLEAR : IT_TREELIST.

   IT_TREELIST-ID       = 9.                  " ID
   IT_TREELIST-TLEVEL   = 9.                  " LEVEL
   IT_TREELIST-NAME     = 'IDR'.              " LEVEL
   IT_TREELIST-COLOR    = 8.                  " COLOR
   IT_TREELIST-INTENSIV = '1'.                " INTENSIVE
   IT_TREELIST-HIDE     = 'ZIMCD3'.           " T-CODE

*Customs Clearance Request Document No
   IT_TREELIST-TEXT1      = IT_IDR-ZFIVNO.
   IT_TREELIST-TLENGTH1   = 24.
   IT_TREELIST-TCOLOR1    = 8.
   IT_TREELIST-TINTENSIV1 = '1'.
* Customs Clearance Sequnce
   IT_TREELIST-TEXT2      = IT_IDR-ZFCLSEQ.
   IT_TREELIST-TLENGTH2   = 5.
   IT_TREELIST-TCOLOR2    = 8.
   IT_TREELIST-TINTENSIV2 = '1'.
   CLEAR W_COUNT.
   IF W_SUBRC2 NE 0 AND W_SUBRC1 NE 0.
       W_COUNT = 4.
   ENDIF.
*>> Unloading Data exist
   IF W_SUBRC2 EQ 0.
       W_COUNT = W_COUNT - 4.
   ENDIF.

   IT_TREELIST-TEXT3      = IT_IDR-ZFIDRNO .
   IT_TREELIST-TLENGTH3   = 18 + W_COUNT.
   IT_TREELIST-TCOLOR3    = 8.
   IT_TREELIST-TINTENSIV3 = '1'.
* Qty
   WRITE IT_IDR-ZFQNT TO IT_TREELIST-TEXT4(23) UNIT IT_IDR-ZFQNTM.
   IT_TREELIST-TLENGTH4   = 23.
   IT_TREELIST-TCOLOR4    = 8.
   IT_TREELIST-TINTENSIV4 = '0'.
* UOM
   IT_TREELIST-TEXT5      = IT_IDR-ZFQNTM.
   IT_TREELIST-TLENGTH5   = 3.
   IT_TREELIST-TCOLOR5    = 8.
   IT_TREELIST-TINTENSIV5 = '0'.

   APPEND IT_TREELIST.

ENDFORM.                    " P3000_ZTIDR_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIDS_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_ZTIDS_PROCESS.

  CLEAR IT_IDS.
  LOOP AT IT_IDS WHERE ZFIVNO  EQ IT_IDR-ZFIVNO
                  AND  ZFCLSEQ EQ IT_IDR-ZFCLSEQ
                  AND  ZFCONO  EQ IT_IDR-ZFCONO
                  AND  ZFRONO  EQ IT_IDR-ZFRONO.
       ON CHANGE OF IT_IDS-ZFIVNO OR IT_IDS-ZFCLSEQ OR
                    IT_IDS-ZFCONO OR IT_IDS-ZFRONO.
          PERFORM P3000_ZTIDS_NODE_WRITE.
       ENDON.
  ENDLOOP.

ENDFORM.                    " P2000_ZTIDS_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDS_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ZTIDS_NODE_WRITE.

   CLEAR : IT_TREELIST.
   IT_TREELIST-ID       = 10.                  " ID
   IT_TREELIST-TLEVEL   = 10.                  " LEVEL
   IT_TREELIST-NAME     = 'IDS'.               " LEVEL
   IT_TREELIST-COLOR    = 9.                   " COLOR
   IT_TREELIST-INTENSIV = '1'.                 " INTENSIVE
   IT_TREELIST-HIDE     = 'ZIMCC3'.             " T-CODE
*>> Customs Clearance Request No
   IT_TREELIST-TEXT1      = IT_IDS-ZFIVNO.
   IT_TREELIST-TLENGTH1   = 24.
   IT_TREELIST-TCOLOR1    = 9.
   IT_TREELIST-TINTENSIV1 = '1'.
*>> Customs Clearance Sequence
   IT_TREELIST-TEXT2      = IT_IDS-ZFCLSEQ.
   IT_TREELIST-TLENGTH2   = 5.
   IT_TREELIST-TCOLOR2    = 9.
   IT_TREELIST-TINTENSIV2 = '0'.
   CLEAR W_COUNT.
   IF W_SUBRC2 NE 0 AND W_SUBRC1 NE 0.
      W_COUNT = 4.
   ENDIF.
*>> Unloading Data exist
   IF W_SUBRC2 EQ 0.
       W_COUNT = W_COUNT - 4.
   ENDIF.

*>> Entry No
   IT_TREELIST-TEXT3      = IT_IDS-ZFIDRNO.
   IT_TREELIST-TLENGTH3   = 14 + W_COUNT.
   IT_TREELIST-TCOLOR3    = 9.
   IT_TREELIST-TINTENSIV3 = '0'.
*>> Quantity
   WRITE IT_IDS-ZFQNT TO IT_TREELIST-TEXT4(23) UNIT IT_IDS-ZFQNTM.
   IT_TREELIST-TLENGTH4   = 23.
   IT_TREELIST-TCOLOR4    = 9.
   IT_TREELIST-TINTENSIV4 = '0'.
*>> UOM
   IT_TREELIST-TEXT5      = IT_IDS-ZFQNTM.
   IT_TREELIST-TLENGTH5   = 3.
   IT_TREELIST-TCOLOR5    = 9.
   IT_TREELIST-TINTENSIV5 = '0'.

   APPEND IT_TREELIST.

ENDFORM.                    " P3000_ZTIDS_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_GR_PROCESS
*&---------------------------------------------------------------------*
FORM P3000_GR_PROCESS.

   CLEAR : IT_TREELIST.

   IT_TREELIST-ID       = 11.                  " ID
   IT_TREELIST-TLEVEL   = 11.                  " LEVEL
   IT_TREELIST-NAME     = 'GR'.                " LEVEL
   IT_TREELIST-COLOR    = 6.                   " COLOR
   IT_TREELIST-INTENSIV = '1'.                 " INTENSIVE

*.GR Material Document No
   IT_TREELIST-TEXT1      = IT_GR-MBLNR .
   IT_TREELIST-TLENGTH1   = 10.
   IT_TREELIST-TCOLOR1    = 6.
   IT_TREELIST-TINTENSIV1 = '1'.
*.GR Material Document Year
   IT_TREELIST-TEXT2      = IT_GR-MJAHR .
   IT_TREELIST-TLENGTH2   = 4.
   IT_TREELIST-TCOLOR2    = 6.
   IT_TREELIST-TINTENSIV2 = '0'.

   CLEAR W_COUNT.
   IF W_SUBRC1 NE 0.
        ADD 4 TO W_COUNT.
   ENDIF.
*>> Unloading Data exist
   IF W_SUBRC2 EQ 0.
       W_COUNT = W_COUNT - 4.
   ENDIF.

*>> Local
   IF IT_BL-ZFBLNO IS INITIAL .
       ADD 12 TO W_COUNT.
   ENDIF.
* SPACE.
   IT_TREELIST-TEXT3      = SPACE.
   IT_TREELIST-TLENGTH3   = 25 + W_COUNT.
   IT_TREELIST-TCOLOR3    = 8.
   IT_TREELIST-TINTENSIV3 = '0'.

*.GR Quantity
   WRITE IT_GR-GRMENGE TO IT_TREELIST-TEXT4(23) UNIT IT_GR-MEINS.
   IT_TREELIST-TLENGTH4   = 23.
   IT_TREELIST-TCOLOR4    = 6.
   IT_TREELIST-TINTENSIV4 = '0'.
*.GR UOM
   IT_TREELIST-TEXT5      = IT_GR-MEINS.
   IT_TREELIST-TLENGTH5   = 3.
   IT_TREELIST-TCOLOR5    = 6.
   IT_TREELIST-TINTENSIV5 = '0'.

   APPEND IT_TREELIST.

ENDFORM.                    " P3000_GR_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTBL_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_ZTBL_PROCESS.

  CLEAR IT_BL.
  LOOP AT IT_BL WHERE EBELN   = IT_RN-EBELN
                AND   EBELP   = IT_RN-EBELP
                AND   ZFREQNO = IT_RN-ZFREQNO
                AND   ZFITMNO = IT_RN-ZFITMNO.
      ON CHANGE OF IT_BL-ZFBLNO OR IT_BL-ZFBLIT.
         PERFORM   P3000_ZFBLNO_NODE_WRITE.
      ENDON.
*>> Unloading
     PERFORM   P2000_ZFCGNO_PROCESS.
*>> Customs Clearance
     PERFORM   P2000_ZTIV_PROCESS.

  ENDLOOP.

ENDFORM.                    " P2000_ZTBL_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTCIVHD_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_ZTCIVHD_PROCESS.

  LOOP AT IT_CIV WHERE EBELN   = IT_RN-EBELN
                 AND   EBELP   = IT_RN-EBELP
                 AND   ZFREQNO = IT_RN-ZFREQNO
                 AND   ZFITMNO = IT_RN-ZFITMNO.

     ON CHANGE OF IT_CIV-ZFCIVRN OR IT_CIV-ZFCIVSQ.
        PERFORM P3000_ZTCIVHD_NODE_WRITE.
     ENDON.
  ENDLOOP.
  W_SUBRC1 = SY-SUBRC.

ENDFORM.                    " P2000_ZTCIVHD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTREQHD_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_ZTREQHD_PROCESS.

  CLEAR IT_RN.
  LOOP AT IT_RN WHERE EBELN   = IT_PO-EBELN
                AND   EBELP   = IT_PO-EBELP.

       W_TABIX = SY-TABIX.
       ON CHANGE OF IT_RN-ZFREQNO OR IT_RN-ZFITMNO.
          PERFORM   P3000_ZFREQNO_NODE_WRITE.
       ENDON.
* >> CIV.
       PERFORM P2000_ZTCIVHD_PROCESS.
* >> LO/PU.
       READ TABLE IT_BL WITH KEY ZFREQNO = IT_RN-ZFREQNO
                                 ZFITMNO = IT_RN-ZFITMNO.
       IF SY-SUBRC NE 0.
          CLEAR IT_BL.
          PERFORM  P2000_LOCAL_GR_PROCESS.
       ELSE.
* >>  MASTER L/C D/A D/P.
          PERFORM P2000_ZTBL_PROCESS.
       ENDIF.

  ENDLOOP.

ENDFORM.                    " P2000_ZTREQHD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_ZTIV_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_ZTIV_PROCESS.

  CLEAR IT_IVIT.
  LOOP AT IT_IVIT WHERE EBELN   = IT_BL-EBELN
                    AND EBELP   = IT_BL-EBELP
                    AND ZFREQNO = IT_BL-ZFREQNO
                    AND ZFITMNO = IT_BL-ZFITMNO
                    AND ZFBLNO  = IT_BL-ZFBLNO
                    AND ZFBLIT  = IT_BL-ZFBLIT.

      ON CHANGE OF IT_IVIT-ZFIVNO OR IT_IVIT-ZFIVDNO.
         PERFORM P3000_ZTIV_NODE_WRITE.
      ENDON.
*>> Customs Declaration
      PERFORM  P2000_ZTIDR_PROCESS.
**>> Customs Clearance
      PERFORM  P2000_ZTIDS_PROCESS.
*>> G/R
      PERFORM  P2000_GR_PROCESS.

  ENDLOOP.

ENDFORM.                    " P2000_ZTIV_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIV_NODE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ZTIV_NODE_WRITE.

  CLEAR : IT_TREELIST.

  IT_TREELIST-ID       = 8.                  " ID
  IT_TREELIST-TLEVEL   = 8.                  " LEVEL
  IT_TREELIST-NAME     = 'IV'.               " LEVEL
  IT_TREELIST-COLOR    =  7.                 " COLOR
  IT_TREELIST-INTENSIV = '1'.                " INTENSIVE
  IT_TREELIST-HIDE     = 'ZIM33'.            " T-CODE

  PERFORM P1000_READ_HBLNO USING    IT_IVIT-ZFBLNO
                           CHANGING IT_IVIT-ZFHBLNO.
*B/L No.
  IT_TREELIST-TEXT1      = IT_IVIT-ZFHBLNO.
  IT_TREELIST-TLENGTH1   = 24.
  IT_TREELIST-TCOLOR1    = 7.
  IT_TREELIST-TINTENSIV1 = '1'.

*I/V No.
  IT_TREELIST-TEXT2      = IT_IVIT-ZFIVNO.
  IT_TREELIST-TLENGTH2   = 10.
  IT_TREELIST-TCOLOR2    = 7.
  IT_TREELIST-TINTENSIV2 = '1'.

* I/V Item No
  IT_TREELIST-TEXT3      = IT_IVIT-ZFIVDNO.
  IT_TREELIST-TLENGTH3   = 5.
  IT_TREELIST-TCOLOR3    = 7.
  IT_TREELIST-TINTENSIV3 = '1'.

*>> Unloading Data Not exist
  READ TABLE IT_CG WITH KEY ZFBLNO = IT_IVIT-ZFBLNO
                            ZFBLIT = IT_IVIT-ZFBLIT.
  W_COUNT = -4.
  IF SY-SUBRC NE 0.
       ADD 4 TO W_COUNT.
  ENDIF.

*>> No B/L
  IF IT_IVIT-ZFBLNO IS INITIAL.
       ADD 4 TO W_COUNT.
  ENDIF.
*>> NO CIV
  IF W_SUBRC1 NE 0.
      ADD 4 TO W_COUNT.
  ENDIF.
* space
  IT_TREELIST-TEXT4      = ' '.
  IT_TREELIST-TLENGTH4   = 11 + W_COUNT.
  IT_TREELIST-TCOLOR4    = 7.
  IT_TREELIST-TINTENSIV4 = '0'.
* Quantity
  WRITE IT_IVIT-CCMENGE  TO IT_TREELIST-TEXT5(23) UNIT IT_IVIT-MEINS.
  IT_TREELIST-TLENGTH5   = 23.
  IT_TREELIST-TCOLOR5    = 7.
  IT_TREELIST-TINTENSIV5 = '0'.
* UOM
  IT_TREELIST-TEXT6      = IT_IVIT-MEINS.
  IT_TREELIST-TLENGTH6   = 3.
  IT_TREELIST-TCOLOR6    = 7.
  IT_TREELIST-TINTENSIV6 = '0'.

  APPEND     IT_TREELIST.

ENDFORM.                    " P3000_ZTIV_NODE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IMG03_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_IMG03_DATA.

  SELECT *
    INTO   CORRESPONDING FIELDS OF TABLE IT_IMG03
    FROM   ZTIMIMG03
    FOR ALL ENTRIES IN IT_CG
    WHERE  ZFBNARCD EQ IT_CG-ZFBNARCD.

ENDFORM.                    " P1000_READ_IMG03_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIVIT_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTIVIT_DATA.

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
    SELECT SINGLE ZFCUST ZFGRST
           INTO   (IT_IVIT-ZFCUST, IT_IVIT-ZFGRST)
           FROM   ZTIV
           WHERE  ZFIVNO EQ IT_IVIT-ZFIVNO.
    MODIFY IT_IVIT INDEX L_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_ZTIVIT_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_GR_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_GR_PROCESS.

    CLEAR IT_GR.
    LOOP AT IT_GR WHERE ZFIVNO  = IT_IVIT-ZFIVNO
                    AND ZFIVDNO = IT_IVIT-ZFIVDNO.

       ON CHANGE OF IT_GR-ZFIVNO OR IT_GR-MBLNR OR
                    IT_GR-ZFIVDNO OR IT_GR-ZFIVHST.
          PERFORM P3000_GR_PROCESS.
       ENDON.
    ENDLOOP.

ENDFORM.                    " P2000_GR_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_LOCAL_GR_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_LOCAL_GR_PROCESS.

  LOOP AT IT_IVIT WHERE EBELN   = IT_RN-EBELN
                    AND EBELP   = IT_RN-EBELP
                    AND ZFREQNO = IT_RN-ZFREQNO
                    AND ZFITMNO = IT_RN-ZFITMNO.

      ON CHANGE OF IT_IVIT-ZFIVNO OR IT_IVIT-ZFIVDNO.
         PERFORM P3000_ZTIV_NODE_WRITE.
      ENDON.

      LOOP AT IT_GR WHERE ZFIVNO  = IT_IVIT-ZFIVNO
                      AND ZFIVDNO = IT_IVIT-ZFIVDNO.

           ON CHANGE OF IT_GR-ZFIVNO OR IT_GR-MBLNR OR
                   IT_GR-ZFIVDNO OR IT_GR-ZFIVHST.
              PERFORM P3000_GR_PROCESS.
           ENDON.
      ENDLOOP.

  ENDLOOP.


ENDFORM.                    " P2000_LOCAL_GR_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_HBLNO
*&---------------------------------------------------------------------*
FORM P1000_READ_HBLNO USING    P_ZFBLNO
                      CHANGING P_ZFHBLNO.

   SELECT SINGLE *
          FROM  ZTBL
          WHERE ZFBLNO = P_ZFBLNO.
   IF SY-SUBRC EQ 0.
      MOVE ZTBL-ZFHBLNO TO  P_ZFHBLNO.
   ENDIF.

ENDFORM.                    " P1000_READ_HBLNO
