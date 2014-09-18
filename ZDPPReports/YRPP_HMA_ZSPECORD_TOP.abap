*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_TOP                                        *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : ztpp_wosum, ausp, cabn, edp13,  ztsd_ale_dest.

**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*
*
DATA : BEGIN OF gt_data OCCURS 0.
DATA :  status LIKE icon-id,
        select(1),               "Victor 06.24.2011
        desc(10),                "Victor
        check  LIKE ztsd_osr_log-gubn. "Victor  "Sepc = 'X' -> Like
        INCLUDE STRUCTURE zspseg.
DATA : _ioqty LIKE mseg-menge,
       _moqty LIKE mseg-menge,
       _crt_date LIKE sy-datum,
       _chg_date LIKE sy-datum,
       END OF gt_data.
DATA : wa_data TYPE zspseg.   "Victor 06.30.2011

DATA : gt_zposeg1 LIKE TABLE OF zposeg1 WITH HEADER LINE.
DATA : wa_ale_dest  TYPE ztsd_ale_dest.
DATA : l_nation LIKE ztsd_ale_dest-wo_nation.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: ok_code LIKE sy-ucomm.

DATA : gv_repid LIKE sy-repid ,
       gv_new(1),
       gv_docnum LIKE edidc-docnum.


CONSTANTS : c_zspseg LIKE edidd-segnam VALUE 'ZSPSEG',
            c_mestyp LIKE edidc-mestyp VALUE 'ZSPEC_ORD_MST',
            "Message Type
            c_rcvprt LIKE edidc-rcvprt VALUE 'LS',
            "Partner type of receiver
            c_logsys LIKE edidc-rcvprn VALUE 'NDECLNT850',
            c_rcvpor LIKE edidc-rcvpor VALUE 'A000000010',
            c_sndprn LIKE edidc-sndprn VALUE 'UD1300',
            c_sndprt LIKE edidc-sndprt VALUE 'LS',
            c_idoctp LIKE edidc-idoctp VALUE 'ZSPEC_ORD'.
