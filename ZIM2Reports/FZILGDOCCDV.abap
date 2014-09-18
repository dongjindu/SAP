* declaration for the long text
DATA: BEGIN OF ICDTXT_ZILGDOC         OCCURS 20.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_ZILGDOC        .

DATA: UPD_ICDTXT_ZILGDOC         TYPE C.

TABLES: *ZTLG
       , ZTLG                          .
DATA: UPD_ZTLG                           TYPE C.

