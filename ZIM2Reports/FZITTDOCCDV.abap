* declaration for the long text
DATA: BEGIN OF ICDTXT_ZITTDOC         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZITTDOC        .

DATA: UPD_ICDTXT_ZITTDOC         TYPE C.

TABLES: *ZTREQHD
       , ZTREQHD                       .
DATA: UPD_ZTREQHD                        TYPE C.

TABLES: *ZTREQST
       , ZTREQST                       .
DATA: UPD_ZTREQST                        TYPE C.

TABLES: *ZTTTHD
       , ZTTTHD                        .
DATA: UPD_ZTTTHD                         TYPE C.

