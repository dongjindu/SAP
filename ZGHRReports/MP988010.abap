*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9880                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP988000 MESSAGE-ID RP.

TABLES: P9880.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9880
                       DEFAULT P9880.

DATA: PSAVE LIKE P9880.

DATA: sptxt         TYPE sptxt.
DATA: gt_zghrlt0001  TYPE TABLE OF zghrlt0001 WITH HEADER LINE.

DATA: gr_docking_container  TYPE REF TO cl_gui_docking_container.
DATA: gr_html               TYPE REF TO cl_gui_html_viewer.
