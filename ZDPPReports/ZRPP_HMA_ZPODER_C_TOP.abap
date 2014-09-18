*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_TOP                                        *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : EDIDC, ZTSD_UM.

**-------------------------------------------------------------------*
**  TYPE DEFINE
**-------------------------------------------------------------------*


TYPES : BEGIN OF T_ZSEG1 .
        INCLUDE STRUCTURE ZPOSEG1 .
TYPES : UPDDAT LIKE EDIDC-UPDDAT,
       UPDTIM LIKE EDIDC-UPDTIM,
       DOCNUM LIKE EDIDC-DOCNUM,
       END OF T_ZSEG1.



TYPES : BEGIN OF T_ZSEG2.
        INCLUDE STRUCTURE ZPOSEG2 .
TYPES : UPDDAT LIKE EDIDC-UPDDAT,
       UPDTIM LIKE EDIDC-UPDTIM,
       DOCNUM LIKE EDIDC-DOCNUM,
       END OF T_ZSEG2.


**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*

DATA : BEGIN OF GT_DATA OCCURS 0 ,
       DOCNUM LIKE EDIDC-DOCNUM,
       UPDDAT LIKE EDIDC-UPDDAT,
       UPDTIM LIKE EDIDC-UPDTIM,

       STATUS  LIKE ICON-ID,
       WO_SER   LIKE ZTPP_KSBOHMM-WO_SER,
       NATION   LIKE ZTPP_KSBOHMM-NATION,
       DEALER   LIKE ZTPP_KSBOHMM-DEALER,
       EXTC     LIKE ZTPP_KSBOHMM-EXTC,
       INTC     LIKE ZTPP_KSBOHMM-INTC,
       DEST     LIKE ZTPP_KSBOHMM-DEST,
       MOYE     LIKE ZTPP_KSBOHMM-MOYE,
       BMDL     LIKE ZTPP_KSBOHMM-BMDL,
       OCNN     LIKE ZTPP_KSBOHMM-OCNN,
       VERS     LIKE ZTPP_KSBOHMM-VERS 	,
       INITQTY  LIKE ZPOSEG1-IOQTY, "ZTPP_KSBOHMM-INITQTY	,
       MODQTY   LIKE ZPOSEG1-MOQTY,"ZTPP_KSBOHMM-MODQTY	,
       LCNO     LIKE ZTPP_KSBOHMM-LCNO	,
       LCNT     LIKE ZTPP_KSBOHMM-LCNT	,
       FLET     LIKE ZTPP_KSBOHMM-FLET	,
       REQ_DATE LIKE ZTPP_KSBOHMM-REQ_DATE	,
       CRT_DATE LIKE ZTPP_KSBOHMM-CRT_DATE	,
       CHG_DATE LIKE ZTPP_KSBOHMM-CHG_DATE	,

       POYEAR   LIKE	ZTSD_SODATA-POYEAR	,
       POMONTH  LIKE	ZTSD_SODATA-POMONTH	,
       ORDQTY   LIKE	ZTSD_SODATA-ORDQTY	,
       NEWQTY   LIKE	ZTSD_SODATA-NEWQTY	,
       ZSDAT	  LIKE	ZTSD_SODATA-ZSDAT	,
       ZSTIM	  LIKE	ZTSD_SODATA-ZSTIM	,
       ZUSER	  LIKE	ZTSD_SODATA-ZUSER	.


*       INCLUDE STRUCTURE ZT.
DATA : END OF GT_DATA.

DATA : GT_OLD LIKE TABLE OF GT_DATA WITH HEADER LINE.

DATA : BEGIN OF GT_ZVIN OCCURS 0 .
        INCLUDE STRUCTURE ZPOSEG2.
DATA : END OF GT_ZVIN .

*DATA : BEGIN OF GT_DATA OCCURS 0 .
*INCLUDE STRUCTURE ZRZSEG1.
*DATA : END OF GT_DATA.
DATA : GT_ZPOSEG1 LIKE TABLE OF ZPOSEG1 WITH HEADER LINE,
       GT_ZPOSEG2 LIKE TABLE OF ZPOSEG2 WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: OK_CODE LIKE SY-UCOMM.

DATA : GV_REPID LIKE SY-REPID ,
       GV_NEW(1).
DATA : WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER'.


CONSTANTS : C_ZPOSEG1 LIKE EDIDD-SEGNAM VALUE 'ZPOSEG1',
            C_MESTYP LIKE EDIDC-MESTYP VALUE 'ZPODER_MST',
            C_IDOCTP LIKE EDIDC-IDOCTP VALUE 'ZPODER01',
            "Message Type
            C_RCVPRT LIKE EDIDC-RCVPRT VALUE 'LS',
            "Partner type of receiver
            C_LOGSYS LIKE EDIDC-RCVPRN VALUE 'NDECLNT850',
            C_RCVPOR LIKE EDIDC-RCVPOR VALUE 'A000000014',
            C_SNDPRN LIKE EDIDC-SNDPRN VALUE 'UD1300',
            C_SNDPRT LIKE EDIDC-SNDPRT VALUE 'LS'.
