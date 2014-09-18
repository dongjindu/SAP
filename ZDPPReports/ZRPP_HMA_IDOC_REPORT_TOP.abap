*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_TOP                                        *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : EDIDC, EDIDD, ZTPP_WOSUM.

**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*
*
*DATA : BEGIN OF GT_DATA OCCURS 0,
*        STATUS LIKE ICON-ID.
*        INCLUDE STRUCTURE ZSPSEG.
*DATA : _IOQTY LIKE MSEG-MENGE,
*       _MOQTY LIKE MSEG-MENGE,
*       _CRT_DATE LIKE SY-DATUM,
*       _CHG_DATE LIKE SY-DATUM,
*       END OF GT_DATA.


  FIELD-SYMBOLS : <INTAB>    TYPE TABLE,
                  <INTAB_WA> TYPE ANY.
*DATA : GT_DATA TYPE REF TO DATA.

*DATA : GT_ZPOSEG1 LIKE TABLE OF ZPOSEG1 WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: OK_CODE LIKE SY-UCOMM.

DATA : GV_REPID LIKE SY-REPID ,
       GV_NEW(1),
       GV_DOCNUM LIKE EDIDC-DOCNUM.


CONSTANTS : C_ZSPSEG LIKE EDIDD-SEGNAM VALUE 'ZSPSEG',
            C_MESTYP LIKE EDIDC-MESTYP VALUE 'ZSPEC_ORD_MST',
            "Message Type
            C_RCVPRT LIKE EDIDC-RCVPRT VALUE 'LS',
            "Partner type of receiver
            C_LOGSYS LIKE EDIDC-RCVPRN VALUE 'NDECLNT850',
            C_RCVPOR LIKE EDIDC-RCVPOR VALUE 'A000000014',
            C_SNDPRN LIKE EDIDC-SNDPRN VALUE 'UD1300',
            C_SNDPRT LIKE EDIDC-SNDPRT VALUE 'LS',
            C_IDOCTP LIKE EDIDC-IDOCTP VALUE 'ZSPEC_ORD'.
