
DATA: UPD_ICDTXT_ZICCHD          TYPE C.
DATA: BEGIN OF ICDTXT_ZICCHD          OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZICCHD         .

TABLES: *ZTIV                          , ZTIV                          .
DATA: UPD_ZTIV                           TYPE C.

