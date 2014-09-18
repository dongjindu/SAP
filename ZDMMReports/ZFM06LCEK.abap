*eject
*----------------------------------------------------------------------*
*             COMMON DATA                                              *
*----------------------------------------------------------------------*
*             Datenfelder für die Listen der Einkaufsbelege            *
*----------------------------------------------------------------------*

DATA: BEGIN OF COMMON PART FM06LCEK.

*------------ Überschrift ---------------------------------------------*
DATA: BEGIN OF UEB,
          1(80),
          2(80),
          3(80),
          4(80),
          5(80),
      END OF UEB.
*{   INSERT         PA8K011747                                        2
* Note 505380                                                   C5031265
* Extended Header
data: begin of ueb1,
          1(200),
          2(200),
          3(200),
          4(200),
          5(200),
      end of ueb1.
*}   INSERT
*{   INSERT         KA5K001235                                        1
*----------------------------------------------------------------------
* zusätzliche Felder für Herstellerteil Nummer
*----------------------------------------------------------------------
DATA:  MAT_LEN TYPE I,
       OFFSET TYPE I,
       PAD TYPE I VALUE 3,
       I_MPNCNV LIKE V_MPNCNV OCCURS 1 WITH HEADER LINE,
       MANUFACTURER_LEN TYPE I,
       OUTPUT_LINE_WIDTH TYPE I,
       POSITION TYPE I,
       LEN TYPE I.

*}   INSERT

*------------ Hilfsfelder ---------------------------------------------*
DATA: REJECT.
DATA: LEERFLG.
DATA: NOT_FOUND.

DATA: END OF COMMON PART.

