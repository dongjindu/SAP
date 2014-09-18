*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9882                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP988200 MESSAGE-ID RP.

TABLES: P9882.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9882
                       DEFAULT P9882.

DATA: PSAVE LIKE P9882.

DATA: gv_stext    TYPE hrp1000-stext.
