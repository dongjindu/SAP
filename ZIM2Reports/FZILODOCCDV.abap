* declaration for the long text
DATA: BEGIN OF ICDTXT_ZILODOC         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZILODOC        .

DATA: UPD_ICDTXT_ZILODOC         TYPE C.

TABLES: *ZTLLCHD
       , ZTLLCHD                       .
DATA: UPD_ZTLLCHD                        TYPE C.

TABLES: *ZTLLCSG23
       , ZTLLCSG23                     .
DATA: UPD_ZTLLCSG23                      TYPE C.

TABLES: *ZTREQHD
       , ZTREQHD                       .
DATA: UPD_ZTREQHD                        TYPE C.

TABLES: *ZTREQST
       , ZTREQST                       .
DATA: UPD_ZTREQST                        TYPE C.

