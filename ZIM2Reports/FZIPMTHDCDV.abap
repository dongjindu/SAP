
DATA: UPD_ICDTXT_ZIPMTHD         TYPE C.
DATA: BEGIN OF ICDTXT_ZIPMTHD         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZIPMTHD        .

TABLES: *ZTPMTHD                       , ZTPMTHD                       .
DATA: UPD_ZTPMTHD                        TYPE C.

