* declaration for the long text
DATA: BEGIN OF ICDTXT_ZIINDOC         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZIINDOC        .

DATA: UPD_ICDTXT_ZIINDOC         TYPE C.

TABLES: *ZTINS
       , ZTINS                         .
DATA: UPD_ZTINS                          TYPE C.

TABLES: *ZTINSRSP
       , ZTINSRSP                      .
DATA: UPD_ZTINSRSP                       TYPE C.

TABLES: *ZTINSSG3
       , ZTINSSG3                      .
DATA: UPD_ZTINSSG3                       TYPE C.

