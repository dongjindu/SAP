
DATA: UPD_ICDTXT_ZICGHD          TYPE C.
DATA: BEGIN OF ICDTXT_ZICGHD          OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZICGHD         .

TABLES: *ZTCGHD                        , ZTCGHD                        .
DATA: UPD_ZTCGHD                         TYPE C.

