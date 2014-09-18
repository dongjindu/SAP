
DATA: UPD_ICDTXT_ZICIVHD         TYPE C.
DATA: BEGIN OF ICDTXT_ZICIVHD         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZICIVHD        .

TABLES: *ZTCIVHD                       , ZTCIVHD                       .
DATA: UPD_ZTCIVHD                        TYPE C.

