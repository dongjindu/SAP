
*&---------------------------------------------------------------------*
*& Report  RFFMCHECKAKONTO                                             *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  rffmcheckakonto               .

TABLES: bsid, bsik.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-010.
SELECT-OPTIONS: so_bukrs FOR bsid-bukrs,
                so_gjahr FOR bsid-gjahr,
                so_bschl FOR bsid-bschl.
SELECTION-SCREEN END OF BLOCK sel.

DATA:
g_t_bsid    LIKE bsid  OCCURS 0   WITH HEADER LINE,
g_t_bsik    LIKE bsik  OCCURS 0   WITH HEADER LINE.

*----- offene BSID-Einträge prüfen
SELECT bukrs belnr gjahr buzei kunnr FROM bsid
       INTO CORRESPONDING FIELDS OF TABLE g_t_bsid
       WHERE bukrs IN so_bukrs
         AND gjahr IN so_gjahr
         AND bschl IN so_bschl
         AND umsks = space
         AND ( rebzg = space OR
               rebzj = space OR
               rebzz = space OR
               rebzt = space ).

LOOP AT g_t_bsid.
  WRITE: / text-010,
           g_t_bsid-kunnr,
           text-020,
           g_t_bsid-bukrs,
           g_t_bsid-belnr,
           g_t_bsid-gjahr,
           text-030,
           g_t_bsid-buzei.

ENDLOOP.
FREE g_t_bsid.

*----- offene BSIK-Einträge prüfen
SELECT bukrs belnr gjahr buzei lifnr FROM bsik
       INTO CORRESPONDING FIELDS OF TABLE  g_t_bsik
       WHERE bukrs IN so_bukrs
         AND gjahr IN so_gjahr
         AND bschl IN so_bschl
         AND umsks = space
         AND ( rebzg = space OR
               rebzj = space OR
               rebzz = space OR
               rebzt = space ).

LOOP AT g_t_bsik.
  WRITE: / text-040,
           g_t_bsik-lifnr,
           text-020,
           g_t_bsik-bukrs,
           g_t_bsik-belnr,
           g_t_bsik-gjahr,
           text-030,
           g_t_bsik-buzei.
ENDLOOP.
