* declaration for the long text
DATA: BEGIN OF ICDTXT_ZILOADOC        OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZILOADOC       .

DATA: UPD_ICDTXT_ZILOADOC        TYPE C.

TABLES: *ZTLLCAMHD
       , ZTLLCAMHD                     .
DATA: UPD_ZTLLCAMHD                      TYPE C.

TABLES: *ZTREQHD
       , ZTREQHD                       .
DATA: UPD_ZTREQHD                        TYPE C.

TABLES: *ZTREQST
       , ZTREQST                       .
DATA: UPD_ZTREQST                        TYPE C.

