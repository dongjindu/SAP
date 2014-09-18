*&---------------------------------------------------------------------*
*& Report  Z_DUPLICATED_INVOICES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_DUPLICATED_INVOICES.

TABLES: ekbe.
DATA:
  BEGIN OF xekbe,
*    gjahr type ekbe-gjahr,
*    belnr type ekbe-belnr,
*    buzei type ekbe-buzei,
    xlbnr TYPE ekbe-xblnr,
    lfgja TYPE ekbe-lfgja,
    lfbnr TYPE ekbe-lfbnr,
    lfpos TYPE ekbe-lfpos,
    ivcnt TYPE i,
  END OF xekbe.
DATA: xdupl TYPE TABLE OF ekbe WITH HEADER LINE.


SELECT-OPTIONS:
  so_ebeln FOR ekbe-ebeln,
  so_budat FOR ekbe-budat.

START-OF-SELECTION.
  SELECT i~xblnr i~lfgja i~lfbnr i~lfpos COUNT(*) AS ivcnt
    INTO xekbe
    FROM ekbe AS i INNER JOIN rbkp AS r
                      ON i~gjahr = r~gjahr
                     AND i~belnr = r~belnr
   WHERE i~ebeln IN so_ebeln
     AND i~vgabe EQ '2'
     AND i~knumv NE space
     AND i~budat IN so_budat
     AND r~stblg EQ space      "Exclude Reversed/Reversal IV
     AND EXISTS ( SELECT *
                    FROM ekbe AS g
                   WHERE g~ebeln IN so_ebeln
                     AND g~vgabe EQ '1'
                     AND g~budat IN so_budat
                     AND g~lfgja EQ i~lfgja
                     AND g~lfbnr EQ i~lfbnr
                     AND g~lfpos EQ i~lfpos )
  GROUP BY i~xblnr i~lfgja i~lfbnr i~lfpos.
    IF xekbe-ivcnt > 1.
      SELECT *
        APPENDING TABLE xdupl
        FROM ekbe AS d
       WHERE d~vgabe = '2'
         AND d~lfgja = xekbe-lfgja
         AND d~lfbnr = xekbe-lfbnr
         AND d~lfpos = xekbe-lfpos.
    ENDIF.
  ENDSELECT.

END-OF-SELECTION.
  SORT xdupl BY lfgja lfbnr lfpos gjahr belnr buzei.
  LOOP AT xdupl.
    WRITE:/ xdupl-gjahr, xdupl-belnr, xdupl-buzei, xdupl-lfgja, xdupl-lfbnr, xdupl-lfpos, xdupl-budat.
  ENDLOOP.
