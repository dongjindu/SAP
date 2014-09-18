***INCLUDE MPPSEL00 .
*---------------------------------------------------------------------*
* Common-Part fuer READ_INFOTYPE Option ALL                           *
*---------------------------------------------------------------------*
* 3.0B
* QNOK014286 210995 additional constants in MPPSEL00
*
DATA: BEGIN OF COMMON PART SELTAB.
DATA: BEGIN OF SELTAB OCCURS 5.
        INCLUDE STRUCTURE PRELP.
DATA:   OPERA(1).
DATA: END OF SELTAB.
DATA: END OF COMMON PART.

CONSTANTS:
      ON          VALUE '1',                                "QNOK014286
      OFF         VALUE '0',                                "QNOK014286
      YES         VALUE '1',                                "QNOK014286
      NO          VALUE '0',                                "QNOK014286
      LAST        VALUE '0',                                "QNOK014286
      FIRST       VALUE '1',                                "QNOK014286
      FINAL       VALUE '2',                                "QNOK014286
      INITIAL     VALUE '3',                                "QNOK014286
      ALL         VALUE '4',                                "QNOK014286
      NOP(3) TYPE C VALUE 'NOP'.                            "QNOK014286
