*&---------------------------------------------------------------------*
*& Report  RFFMCHKPAYFLG                                               *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Der Report selektiert Rechnungen, die nicht storniert wurden, jedoch*
*& ohne Fortschreibung von Zahlungss‰tzen ein Payflag erhalten haben.  *
*& Selektion erfolgt auf S‰tze mit Werttyp 54, ohne Stornobelegnr, mit *
*& Payflag. Existieren zu diesen S‰tzen keine weiteren Werte ungleich  *
*& Btart 0350 und 0300 erfolgt die Ausgabe.                            *
*& Ist das Flag "Testlauf" nicht gesetzt, wird das Payflag aufgehoben  *
*& Stand: 12.02.01                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  zrffmcheckpayflag                 .

TABLES: fmifiit, bkpf.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-100.
PARAMETERS: p_bukrs LIKE fmifiit-bukrs MEMORY ID fik.
SELECT-OPTIONS: so_gjahr FOR fmifiit-kngjahr MEMORY ID gjr,
                so_belnr FOR fmifiit-knbelnr.
SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK con WITH FRAME TITLE text-110.
PARAMETERS: p_test LIKE lko74-testlauf DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK con.

DATA:
g_cursor        TYPE    cursor,
g_t_orig        LIKE    fmifiit  OCCURS 0  WITH HEADER LINE,
l_count_docs    TYPE    i,
l_count         TYPE    i.

OPEN CURSOR WITH HOLD g_cursor FOR
SELECT * FROM fmifiit WHERE knbelnr IN so_belnr
                        AND kngjahr IN so_gjahr
                        AND bukrs   =  p_bukrs
                        AND btart   = '0100'
                        AND wrttp   = '54'
                        AND payflg  = 'X'.

DO.
  REFRESH g_t_orig.

*----- Belegzeilen einlesen
  FETCH NEXT CURSOR g_cursor
    INTO TABLE g_t_orig
    PACKAGE SIZE 5000.

*----- Letzter Satz erreicht cursor schlieﬂen
  IF sy-subrc <> 0.
    CLOSE CURSOR g_cursor.
    EXIT.
  ENDIF.

  LOOP AT g_t_orig.
    CLEAR l_count.
    SELECT COUNT(*) INTO l_count FROM fmifiit
                    WHERE knbelnr = g_t_orig-knbelnr
                      AND kngjahr = g_t_orig-kngjahr
                      AND bukrs   = g_t_orig-bukrs
                      AND ( btart = '0200' OR btart = '0250' ).

    IF l_count = 0.
      SELECT SINGLE belnr FROM  bkpf INTO bkpf-belnr
             WHERE  belnr  = g_t_orig-knbelnr
             AND    gjahr  = g_t_orig-kngjahr
             AND    bukrs  = g_t_orig-bukrs
             AND    stblg  = space.
      IF sy-subrc = 0.
        l_count_docs = l_count_docs + 1.
        IF p_test IS INITIAL.
          UPDATE fmifiit SET payflg = space
                 WHERE fmbelnr = g_t_orig-fmbelnr
                   AND fikrs   = g_t_orig-fikrs
                   AND fmbuzei = g_t_orig-fmbuzei
                   AND btart   = g_t_orig-btart
                   AND rldnr   = g_t_orig-rldnr
                   AND gjahr   = g_t_orig-gjahr
                   AND stunr   = g_t_orig-stunr.

        ENDIF.
        WRITE: / text-010, g_t_orig-bukrs, g_t_orig-knbelnr,
               g_t_orig-kngjahr.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDDO.

FORMAT COLOR COL_TOTAL.
WRITE: / text-001, l_count_docs.
