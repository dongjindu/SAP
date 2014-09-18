*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9883                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP988300 MESSAGE-ID RP.

TABLES: P9883.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9883
                       DEFAULT P9883.

DATA: PSAVE LIKE P9883.
