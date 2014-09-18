* declaration for the long text
DATA: BEGIN OF ICDTXT_ZIOTDOC         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZIOTDOC        .

DATA: UPD_ICDTXT_ZIOTDOC         TYPE C.

TABLES: *ZTREQHD
       , ZTREQHD                       .
DATA: UPD_ZTREQHD                        TYPE C.

TABLES: *ZTREQST
       , ZTREQST                       .
DATA: UPD_ZTREQST                        TYPE C.

