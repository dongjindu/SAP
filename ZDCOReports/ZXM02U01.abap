*----------------------------------------------------------------------*
*   INCLUDE ZXM02U01                                                   *
*----------------------------------------------------------------------*
DATA : RE_REQUISITION TYPE REF TO IF_PURCHASE_REQUISITION,
       RE_DATA     TYPE MEREQ_ITEM,
       EX_DATA     TYPE MEREQ_ITEM.

DATA : RE_REQ      TYPE REF TO IF_PURCHASE_REQUISITION,
       RE_HEADER   TYPE MEREQ_HEADER.

*WA_REQ_ITEM ?= IM_REQ_ITEM.
RE_DATA = IM_REQ_ITEM->GET_DATA( ).

EBAN-ZZVZ_PR   = RE_DATA-ZZVZ_PR.
EBAN-ZZVZ_ITEM = RE_DATA-ZZVZ_ITEM.
EBAN-ZZTYPE1   = RE_DATA-ZZTYPE1.


*RE_REQ  = IM_REQ_ITEM->GET_REQUISITION( ).
*
*RE_HEADER = RE_REQ->GET_DATA( ).
*
*SELECT SINGLE ZZVZ_PR ZZVZ_ITEM
*         FROM EBAN
*         INTO (EBAN-ZZVZ_PR, EBAN-ZZVZ_ITEM)
*        WHERE BANFN = RE_HEADER-BANFN
*          AND BNFPO = RE_DATA-BNFPO.
