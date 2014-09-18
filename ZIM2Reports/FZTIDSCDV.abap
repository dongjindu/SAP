
DATA: UPD_ICDTXT_ZTIDS           TYPE C.
DATA: BEGIN OF ICDTXT_ZTIDS           OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZTIDS          .

TABLES: *ZTIDS                         , ZTIDS                         .
DATA: UPD_ZTIDS                          TYPE C.

