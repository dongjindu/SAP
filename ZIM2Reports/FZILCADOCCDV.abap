* declaration for the long text
DATA: BEGIN OF ICDTXT_ZILCADOC        OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZILCADOC       .

DATA: UPD_ICDTXT_ZILCADOC        TYPE C.

TABLES: *ZTMLCAMHD
       , ZTMLCAMHD                     .
DATA: UPD_ZTMLCAMHD                      TYPE C.

TABLES: *ZTREQHD
       , ZTREQHD                       .
DATA: UPD_ZTREQHD                        TYPE C.

TABLES: *ZTREQST
       , ZTREQST                       .
DATA: UPD_ZTREQST                        TYPE C.

