* declaration for the long text
DATA: BEGIN OF ICDTXT_ZILCDOC         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZILCDOC        .

DATA: UPD_ICDTXT_ZILCDOC         TYPE C.

TABLES: *ZTMLCHD
       , ZTMLCHD                       .
DATA: UPD_ZTMLCHD                        TYPE C.

TABLES: *ZTMLCSG2
       , ZTMLCSG2                      .
DATA: UPD_ZTMLCSG2                       TYPE C.

TABLES: *ZTMLCSG910
       , ZTMLCSG910                    .
DATA: UPD_ZTMLCSG910                     TYPE C.

TABLES: *ZTREQHD
       , ZTREQHD                       .
DATA: UPD_ZTREQHD                        TYPE C.

TABLES: *ZTREQST
       , ZTREQST                       .
DATA: UPD_ZTREQST                        TYPE C.

