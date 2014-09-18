*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_TOP                                        *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : ztpp_po_idoc, mara, ztpp_wosum, edidc, ztsd_um.
**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*
*
DATA : BEGIN OF gt_item OCCURS 0 ,
       status LIKE icon-id,
       st_wohd LIKE icon-id,
       st_wocl LIKE icon-id,
       st_wosum LIKE icon-id.
        INCLUDE STRUCTURE zposeg2.
DATA    END OF gt_item.

DATA :  gt_idoc LIKE TABLE OF ztpp_po_idoc WITH HEADER LINE.


DATA : BEGIN OF gt_docno OCCURS 0,
        docnum LIKE edidc-docnum,
       END OF gt_docno,

       BEGIN OF gt_zposeg2 OCCURS 0  .
        INCLUDE STRUCTURE zposeg2 .
DATA :  docnum LIKE edidc-docnum,
       END OF gt_zposeg2.

DATA : BEGIN OF gt_zposeg1 OCCURS 0 .
        INCLUDE STRUCTURE zposeg1 .
DATA : docnum LIKE edidc-docnum ,
       END OF gt_zposeg1.

DATA : "GT_ZPOSEG1 LIKE TABLE OF ZPOSEG1 WITH HEADER LINE,
       gt_head LIKE TABLE OF gt_item WITH HEADER LINE.

DATA : gv_message LIKE bapireturn-message,
       gt_message LIKE TABLE OF bapireturn .


*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: ok_code LIKE sy-ucomm.

DATA : gv_repid LIKE sy-repid ,
       gv_new(1).

DATA:  w_flag(1).

CONSTANTS : c_zspseg LIKE edidd-segnam VALUE 'ZSPSEG',
            c_mestyp LIKE edidc-mestyp VALUE 'ZSPEC_ORD_MST',
            "Message Type
            c_rcvprt LIKE edidc-rcvprt VALUE 'LS',
            "Partner type of receiver
            c_logsys LIKE edidc-rcvprn VALUE 'NDECLNT850',
            c_rcvpor LIKE edidc-rcvpor VALUE 'A000000014',
            c_sndprn LIKE edidc-sndprn VALUE 'UD1300',
            c_sndprt LIKE edidc-sndprt VALUE 'LS',
            c_idoctp LIKE edidc-idoctp VALUE 'ZSPEC_ORD'.
