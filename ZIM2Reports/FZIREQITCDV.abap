* declaration for the long text
DATA: BEGIN OF ICDTXT_ZIREQIT         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZIREQIT        .

DATA: UPD_ICDTXT_ZIREQIT         TYPE C.

TABLES: *ZTREQIT
       , ZTREQIT                       .
DATA: UPD_ZTREQIT                        TYPE C.

