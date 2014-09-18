* declaration for the long text
DATA: BEGIN OF ICDTXT_ZTRECST         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZTRECST        .

DATA: UPD_ICDTXT_ZTRECST         TYPE C.

TABLES: *ZTRECST
       , ZTRECST                       .
DATA: UPD_ZTRECST                        TYPE C.

