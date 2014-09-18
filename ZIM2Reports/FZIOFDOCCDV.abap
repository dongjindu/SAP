* declaration for the long text
DATA: BEGIN OF ICDTXT_ZIOFDOC         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZIOFDOC        .

DATA: UPD_ICDTXT_ZIOFDOC         TYPE C.

TABLES: *ZTOFF
       , ZTOFF                         .
DATA: UPD_ZTOFF                          TYPE C.

TABLES: *ZTOFFFTX
       , ZTOFFFTX                      .
DATA: UPD_ZTOFFFTX                       TYPE C.

