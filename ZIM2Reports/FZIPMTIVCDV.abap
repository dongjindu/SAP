
DATA: UPD_ICDTXT_ZIPMTIV         TYPE C.
DATA: BEGIN OF ICDTXT_ZIPMTIV         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZIPMTIV        .

TABLES: *ZTPMTIV                       , ZTPMTIV                       .
DATA: UPD_ZTPMTIV                        TYPE C.

