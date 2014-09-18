*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9100                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP910000 MESSAGE-ID RP.

TABLES: P9100.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9100
                       DEFAULT P9100.

DATA: PSAVE LIKE P9100.
