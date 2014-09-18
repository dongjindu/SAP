
DATA: UPD_ICDTXT_ZIBLCST         TYPE C.
DATA: BEGIN OF ICDTXT_ZIBLCST         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZIBLCST        .

TABLES: *ZTBLCST                       , ZTBLCST                       .
DATA: UPD_ZTBLCST                        TYPE C.

