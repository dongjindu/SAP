*----------------------------------------------------------------------*
*   INCLUDE ZP000710                                                   *
*----------------------------------------------------------------------*

PROGRAM ZP000700          .
TABLES: P0007.

*tables: ZPLISxxxx.

FIELD-SYMBOLS:
<P0007> STRUCTURE P0007
          DEFAULT P0007.

*data: psave like pxxxx.
DATA: CALL_PROG LIKE SY-REPID.
