* declaration for the long text
DATA: BEGIN OF ICDTXT_ZIINBDOC        OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZIINBDOC       .

DATA: UPD_ICDTXT_ZIINBDOC        TYPE C.

TABLES: *ZTINSB
       , ZTINSB                        .
DATA: UPD_ZTINSB                         TYPE C.

TABLES: *ZTINSBRSP
       , ZTINSBRSP                     .
DATA: UPD_ZTINSBRSP                      TYPE C.

TABLES: *ZTINSBSG3
       , ZTINSBSG3                     .
DATA: UPD_ZTINSBSG3                      TYPE C.

