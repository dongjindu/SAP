* declaration for the long text
DATA: BEGIN OF ICDTXT_ZIBLDOC         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZIBLDOC        .

DATA: UPD_ICDTXT_ZIBLDOC         TYPE C.

TABLES: *ZTBL
       , ZTBL                          .
DATA: UPD_ZTBL                           TYPE C.

