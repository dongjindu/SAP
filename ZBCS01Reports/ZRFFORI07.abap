************************************************************************
*                                                                      *
* Includebaustein RFFORI07 zu den Formulardruckprogrammen RFFOxxxz     *
* mit Unterprogrammen für den Druck der Begleitliste                   *
*                                                                      *
************************************************************************


*----------------------------------------------------------------------*
* FORM BEGLEITLISTE                                                    *
*----------------------------------------------------------------------*
* Druck Begleitliste                                                   *
* gerufen von RFFOxxxz (END-OF-SELECTION)                              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM BEGLEITLISTE.


  SET LANGUAGE SY-LANGU.
  SET COUNTRY SPACE.
  FLG_BEGLEITL = 1.                    "Kopfzeilen für Begleitliste

* Beginne bei Seitenzahl '1'
  CLEAR sy-pagno.

  PERFORM PRINT_ON USING ' ' TEXT_002 PAR_PRIB PAR_SOFB 'LIST6S'.

  FLG_NEW_ZBNKL = FLG_END_ZBNKL = FLG_END_UBKNT = FLG_END_ZBUKR = 0.
  EXPORT FLG_BANKINFO TO MEMORY ID 'FLG_BANKINFO'.

* benutzte Flags:
* FLG_NEW_ZBNKL )
* FLG_END_ZBNKL ) für Simulation des Abschlusses (PAR_MAXP erreicht)
* FLG_END_UBKNT )
* FLG_END_ZBUKR   steuert die Überschriftszeilen 6-8 bei KOPF_ZEILEN

  IF FLG_SORT NE 1.
    SORT.
    FLG_SORT = 1.
  ENDIF.

  LOOP.

*-- Initialisierung Batch-Heading --------------------------------------
    AT FIRST.

      TXT_LINE2   = TEXT_801.          "Ausstellungsdatum
      WRITE REGUH-ZALDT DD/MM/YYYY TO TXT_LINE2+132.
      CONDENSE TXT_LINE2.
      IF TEXT_805 NE SPACE.
        TXT_LINE2+132 = '#'.
        TXT_LINE2+134 = TEXT_805.
        CONDENSE TXT_LINE2.
        WRITE REGUH-LAUFD DD/MM/YYYY TO TXT_LINE2+132.
        TXT_LINE2+143 = '/'.
        TXT_LINE2+145 = REGUH-LAUFI.
        CONDENSE TXT_LINE2.
        TRANSLATE TXT_LINE2 USING '# '.
      ENDIF.
      BHDGD-LINES = SY-LINSZ.
      BHDGD-UNAME = SY-UNAME.
      BHDGD-REPID = SY-REPID.

    ENDAT.


*-- Neuer Buchungskreis ------------------------------------------------
    AT NEW REGUH-ZBUKR.

      SUM_BUKRS    = 0.
      SUM_WAERS    = 0.
      SUM_WAERS_FW = 0.
      BHDGD-BUKRS  = REGUH-ZBUKR.
      PERFORM BUCHUNGSKREIS_DATEN_LESEN.
      WHILE REGUD-HWAER+4(1) EQ SPACE. "Rechts-Shiften der Hauswährung
        SHIFT REGUD-HWAER RIGHT.
        IF SY-INDEX > 5. EXIT. ENDIF.
      ENDWHILE.

    ENDAT.


*-- Neuer Zahlweg ------------------------------------------------------
    AT NEW REGUH-RZAWE.

      PERFORM ZAHLWEG_DATEN_LESEN.
      IMPORT FLG_BANKINFO FROM MEMORY ID 'FLG_BANKINFO'.
      IF FLG_BANKINFO EQ 2 AND T042Z-XBKKT EQ SPACE.
        FLG_BANKINFO = 1.
      ENDIF.
      TXT_LINE1 = TEXT_800.            "Zahlungsbegleitliste
      IF PAR_BEGL EQ 'D'.
        REPLACE '&RENUM' WITH HLP_SORT INTO TXT_LINE1.
      ENDIF.
      IF T042Z-TEXT1 NE SPACE.
        REPLACE '&FORM' WITH T042Z-TEXT1 INTO TXT_LINE1.
      ELSE.
        REPLACE:
          '-'     WITH SPACE INTO TXT_LINE1,
          '&FORM' WITH SPACE INTO TXT_LINE1.
      ENDIF.
      CONDENSE TXT_LINE1.
      BHDGD-INIFL = '0'.
      BHDGD-LINE1 = TXT_LINE1.
      BHDGD-LINE2 = TXT_LINE2.

    ENDAT.


*-- Neue Hausbank ------------------------------------------------------
    AT NEW REGUH-UBNKL.

      PERFORM HAUSBANK_DATEN_LESEN.

    ENDAT.


*-- Neue Kontonummer bei der Hausbank ----------------------------------
    AT NEW REGUH-UBKNT.

      NEW-PAGE.
      HLP_WAERS  = REGUH-WAERS.
      SUM_UBKNT  = 0.
      CNT_POSTEN = 0.

    ENDAT.


*-- Neue Empfängerbank -------------------------------------------------
    AT NEW REGUH-ZBNKL.

      PERFORM EMPFBANK_DATEN_LESEN.

      SUM_ZBNKL     = 0.
      SUM_ZBNKL_FW  = 0.
      FLG_NEW_ZBNKL = 0.
      IF REGUH-WAERS NE HLP_WAERS.     "Wechsel der Währung:
        PERFORM ABSCHLUSS_WAEHRUNG.    "Zwischensumme und neue Seite
        HLP_WAERS = REGUH-WAERS.
      ENDIF.

    ENDAT.


*-- Verarbeitung Zahlungsträger-Daten ----------------------------------
    AT NEW REGUH-VBLNR.

      LOOP AT TAB_SPLITTING
        WHERE ZBUKR EQ REGUH-ZBUKR
        AND   VBLNR EQ REGUH-VBLNR.
        REGUH-RWBTR = TAB_SPLITTING-RWBTR.
        REGUH-RBETR = TAB_SPLITTING-RBETR.
        PERFORM AUSGABE_ZAHLUNG.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        PERFORM AUSGABE_ZAHLUNG.
      ENDIF.

    ENDAT.


*-- Ende Empfänger-Bank ------------------------------------------------
    AT END OF REGUH-ZBNKL.

      IF FLG_END_ZBNKL EQ 0.
        PERFORM ABSCHLUSS_BANKLEITZAHL.
        FLG_END_ZBNKL = 1.
      ENDIF.

    ENDAT.


*-- Ende Kontonummer bei der Hausbank ----------------------------------
    AT END OF REGUH-UBKNT.

      IF FLG_END_UBKNT EQ 0.
        PERFORM ABSCHLUSS_WAEHRUNG.
        PERFORM ABSCHLUSS_HAUSBANK_KONTO.
        FLG_END_UBKNT = 1.
      ENDIF.

    ENDAT.

*-- Ende Buchungskreis -------------------------------------------------
    AT END OF REGUH-ZBUKR.

      FLG_END_ZBUKR = 1.
      TXT_LINE1 = TEXT_800.           "Zahlungsbegleitliste ohne Zahlweg
      REPLACE:
        '-'     WITH SPACE INTO TXT_LINE1,
        '&FORM' WITH SPACE INTO TXT_LINE1.
      IF PAR_BEGL EQ 'D'.
        REPLACE '&RENUM' WITH HLP_SORT INTO TXT_LINE1.
      ENDIF.
      BHDGD-INIFL = '0'.
      BHDGD-LINE1 = TXT_LINE1.
      BHDGD-LINE2 = TXT_LINE2.
      NEW-PAGE.

      TXT_BEGLEITL  = TEXT_815.        "Gesamtbetrag Buchungskreis in
      REPLACE '&WAERS' WITH REGUD-HWAER INTO TXT_BEGLEITL.
      CONDENSE TXT_BEGLEITL.
      WHILE TXT_BEGLEITL+73 EQ SPACE.  "rechtsbündig abstellen
        SHIFT TXT_BEGLEITL RIGHT.
        IF SY-INDEX > 73. EXIT. ENDIF.
      ENDWHILE.

      FORMAT COLOR 3 INTENSIFIED.
      WRITE:        SY-VLINE NO-GAP,
             32(74) TXT_BEGLEITL.
      WRITE SUM_BUKRS TO REGUD-SUMME CURRENCY T001-WAERS.
      TRANSLATE REGUD-SUMME USING ' *'.
      WRITE: 108    REGUD-SUMME,
             132    SY-VLINE.
      ULINE.
      FLG_END_ZBUKR = 0.

    ENDAT.

  ENDLOOP.

  PERFORM PRINT_OFF USING 'LIST6S' TEXT_002.

  FLG_BEGLEITL = 0.


ENDFORM.                               "Begleitliste



*----------------------------------------------------------------------*
* FORM AUSGABE_ZAHLUNG                                                 *
*----------------------------------------------------------------------*
* Daten zu einer Zahlung ausgeben                                      *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM AUSGABE_ZAHLUNG.
  DATA UP_ZBNKN(20) TYPE C.

  FLG_END_ZBNKL = FLG_END_UBKNT = 0.
  ADD 1 TO CNT_POSTEN.
  RESERVE 5 LINES.

  PERFORM ZAHLUNGS_DATEN_LESEN.
  SET COUNTRY SPACE.

* Rechts-Shiften der Belegwährung
  WHILE REGUD-WAERS+4(1) EQ SPACE.
    SHIFT REGUD-WAERS RIGHT.
    IF SY-INDEX > 5. EXIT. ENDIF.
  ENDWHILE.

* 1) Bank-Informationen des Empfängers bei neuer Bank(leitzahl)
  IF FLG_NEW_ZBNKL EQ 0 AND FLG_BANKINFO EQ 2.
    FORMAT COLOR 7 INTENSIFIED.
    WRITE / SY-VLINE NO-GAP.
    IF REGUH-ZBNKL NE SPACE.
      WRITE TEXT_802(15).
    ELSE.
      WRITE TEXT_807(15).
    ENDIF.
    WRITE SPACE NO-GAP.
    FORMAT COLOR 7 INTENSIFIED OFF.
    WRITE   REGUH-ZBNKS.
    IF REGUH-ZBNKL NE SPACE.
      WRITE REGUH-ZBNKL NO-GAP.        "BLZ Zahlungsempfg/-pflichtiger
    ELSE.
      WRITE REGUH-ZBNKY NO-GAP.        "Bankkey
    ENDIF.
    WRITE SPACE NO-GAP.
    FORMAT COLOR 7 INTENSIFIED.
    IF REGUD-XEINZ EQ SPACE.
      WRITE TEXT_803(30).
    ELSE.
      WRITE TEXT_806(30).
    ENDIF.
    WRITE SPACE NO-GAP.
    FORMAT COLOR 7 INTENSIFIED OFF.
    WRITE:  REGUD-ZBANK(60),           "Bank Empfänger (Name und Ort)
        132 SY-VLINE.
    FLG_NEW_ZBNKL = 1.
  ENDIF.

* 2) Summen-Informationen des Empfängers
  FORMAT COLOR 2 INTENSIFIED OFF.
  IF FLG_BANKINFO EQ 2.
    PERFORM BANK_ACCOUNT_CONVERSION USING UP_ZBNKN.
    WRITE:/       SY-VLINE NO-GAP,
           (20)   UP_ZBNKN,            "Bankkonto-Nummer
           (10)   REGUH-VBLNR.         "Zahlungsbelegnummer
  ELSE.
    WRITE:/       SY-VLINE NO-GAP,
           23(10) REGUH-VBLNR.         "nur Zahlungsbelegnummer
  ENDIF.
  IF REGUH-EMPFG(1) NE '>'.
    IF REGUH-LIFNR NE SPACE.
      WRITE: (10) REGUH-LIFNR.         "Lieferantennummer
    ELSE.
      WRITE: (10) REGUH-KUNNR.         "Kundennummer
    ENDIF.
  ELSE.
    REGUH-LIFNR = REGUH-EMPFG+1.       "für Alpha-Konvertierung
    WRITE: (10)   REGUH-LIFNR.         "abweichender Zahlungsempfänger
  ENDIF.
  WRITE:   (25)   REGUH-ZNME1,         "Name
           (20)   REGUD-ZPLOR.         "Pstlz/Ort
  READ TABLE ERR_EDI WITH KEY ZBUKR = REGUH-ZBUKR
                              VBLNR = REGUH-VBLNR
                              EDIBN = 'E'.
  IF SY-SUBRC NE 0.
    IF REGUH-WAERS NE T001-WAERS.
      WRITE: 91(20) REGUH-RWBTR CURRENCY REGUH-WAERS.
      ADD REGUH-RWBTR TO SUM_ZBNKL_FW.
      ADD REGUH-RWBTR TO SUM_WAERS_FW.
    ENDIF.
    WRITE:  112(20) REGUH-RBETR CURRENCY T001-WAERS,
            132     SY-VLINE.
    ADD REGUH-RBETR TO SUM_ZBNKL.
    ADD REGUH-RBETR TO SUM_WAERS.
  ELSE.
    WRITE:  112(20) TEXT_830 COLOR 6,  "EDI Versendefehler (RFFOEDI1)
            132     SY-VLINE.
  ENDIF.

  IF REGUH-PAYGR+18(2) EQ '$J'.        "Bankgebühr (nur Japan)
    TXT_BEGLEITL = TEXT_809.
    WHILE TXT_BEGLEITL+51 EQ SPACE.    "rechtsbündig abstellen
      SHIFT TXT_BEGLEITL RIGHT.
      IF SY-INDEX > 51. EXIT. ENDIF.
    ENDWHILE.
    WHILE REGUH-PAYGR(1) EQ 0.
      SHIFT REGUH-PAYGR(10) LEFT.
      IF SY-INDEX > 10. EXIT. ENDIF.
    ENDWHILE.
    REGUH-RSPE1 = - REGUH-RSPE1.
    PERFORM BANK_ACCOUNT_CONVERSION USING UP_ZBNKN.
    WRITE:/       SY-VLINE NO-GAP,
          (20)    UP_ZBNKN,            "Bankkonto-Nummer
          (10)    REGUH-PAYGR,         "Belegnummer Bankgebühr
          58(53)  TXT_BEGLEITL,
          112(20) REGUH-RSPE1 CURRENCY T001-WAERS,
          132     SY-VLINE.
    ADD REGUH-RSPE1 TO SUM_ZBNKL.
    ADD REGUH-RSPE1 TO SUM_WAERS.
  ENDIF.

* 3) Eventuell vorzeitiger Seiten-Abschluß (PAR_MAXP erreicht)
  IF CNT_POSTEN EQ PAR_MAXP AND PAR_MAXP NE 9999.
    PERFORM ABSCHLUSS_BANKLEITZAHL.
    PERFORM ABSCHLUSS_WAEHRUNG.
    PERFORM ABSCHLUSS_HAUSBANK_KONTO.
    NEW-PAGE.
    SUM_ZBNKL     = 0.                 "Summen wieder initialisieren
    SUM_ZBNKL_FW  = 0.
    SUM_UBKNT     = 0.
    CNT_POSTEN    = 0.
    FLG_NEW_ZBNKL = 0.                 "damit Empf-Daten gedruckt werden
    FLG_END_ZBNKL = 1.                 "Abschluß erledigt
    FLG_END_UBKNT = 1.                 "Abschluß erledigt
    HLP_WAERS     = REGUH-WAERS.
  ENDIF.


ENDFORM.                               "Ausgabe Zahlung



*----------------------------------------------------------------------*
* FORM ABSCHLUSS_BANKLEITZAHL                                          *
*----------------------------------------------------------------------*
* Ende Bankleitzahl Empfänger für Zahlungsbegleitliste                 *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM ABSCHLUSS_BANKLEITZAHL.


  IF FLG_BANKINFO EQ 2.
    NEW-LINE.
    FORMAT COLOR 3 INTENSIFIED OFF.
    WRITE /  SY-VLINE NO-GAP.
    IF REGUH-ZBNKL NE SPACE.
      WRITE: TEXT_802(15),             "BLZ Zahlungsempfg/-pflichtiger
             SPACE NO-GAP,
             REGUH-ZBNKS,
             REGUH-ZBNKL.
    ELSE.
      WRITE: TEXT_807(15),             "Bankkey
             SPACE NO-GAP,
             REGUH-ZBNKS,
             REGUH-ZBNKY.
    ENDIF.
    TXT_BEGLEITL = TEXT_810.
    IF T001-WAERS EQ REGUH-WAERS.      "Gesamtbetrag
      WHILE TXT_BEGLEITL+51 EQ SPACE.  "rechtsbündig abstellen
        SHIFT TXT_BEGLEITL RIGHT.
        IF SY-INDEX > 51. EXIT. ENDIF.
      ENDWHILE.
      WRITE: 58(53) TXT_BEGLEITL.
    ELSE.
      SY-FDPOS = STRLEN( TXT_BEGLEITL ).
      IF SY-FDPOS GE 20.
        WHILE TXT_BEGLEITL+29 EQ SPACE."rechtsbündig abstellen
          SHIFT TXT_BEGLEITL RIGHT.
          IF SY-INDEX > 29. EXIT. ENDIF.
        ENDWHILE.
        WRITE: 58(32) TXT_BEGLEITL.
      ELSE.
        WRITE: 71(19) TXT_BEGLEITL.
      ENDIF.
      WRITE SUM_ZBNKL_FW TO REGUD-SUMME CURRENCY REGUH-WAERS.
      TRANSLATE REGUD-SUMME USING ' *'.
      WRITE: 91 REGUD-SUMME+4.         "Betrag in Fremdwährung
    ENDIF.
    WRITE SUM_ZBNKL TO REGUD-SUMME CURRENCY T001-WAERS.
    TRANSLATE REGUD-SUMME USING ' *'.
    WRITE: 112 REGUD-SUMME+4,          "Betrag in Hauswährung
           132 SY-VLINE.
    ULINE.
  ENDIF.

  RESERVE 9 LINES.
  ADD SUM_ZBNKL TO SUM_UBKNT.


ENDFORM.                               "Abschluß Bankleitzahl



*----------------------------------------------------------------------*
* FORM ABSCHLUSS_WAEHRUNG                                              *
*----------------------------------------------------------------------*
* Ende Währung für Zahlungsbegleitliste                                *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM ABSCHLUSS_WAEHRUNG.

  IF FLG_BANKINFO NE 2.
    ULINE.
  ENDIF.
  FORMAT COLOR 3 INTENSIFIED.
  WRITE SY-VLINE NO-GAP.
  IF T001-WAERS EQ HLP_WAERS.
    WRITE: (78) TEXT_811.              "Zwischensumme (Hauswährung):
  ELSE.
    TXT_BEGLEITL = TEXT_812.           "Zwischensumme (Fremdw. &WAERS):
    IF PAR_ISOC EQ 'X'.
      CLEAR TCURC.
      SELECT SINGLE * FROM TCURC
        WHERE WAERS EQ HLP_WAERS.
      IF SY-SUBRC EQ 0.
        REPLACE '&WAERS' WITH TCURC-ISOCD INTO TXT_BEGLEITL.
      ELSE.
        REPLACE '&WAERS' WITH HLP_WAERS INTO TXT_BEGLEITL.
      ENDIF.
    ELSE.
      REPLACE '&WAERS' WITH HLP_WAERS INTO TXT_BEGLEITL.
    ENDIF.
    CONDENSE TXT_BEGLEITL.
    REPLACE ' )' WITH ')' INTO TXT_BEGLEITL.
    WRITE: (78) TXT_BEGLEITL.
    WRITE SUM_WAERS_FW TO REGUD-SUMME CURRENCY HLP_WAERS.
    TRANSLATE REGUD-SUMME USING ' *'.
    WRITE: 91 REGUD-SUMME+4.           "Zwischensumme in Fremdwährung
  ENDIF.
  WRITE SUM_WAERS TO REGUD-SUMME CURRENCY T001-WAERS.
  TRANSLATE REGUD-SUMME USING ' *'.
  WRITE: 112 REGUD-SUMME+4,            "Zwischensumme in Hauswährung
         132 SY-VLINE.
  ULINE.

  SUM_WAERS = SUM_WAERS_FW = 0.
  NEW-PAGE.


ENDFORM.                               "Abschluß Währung



*----------------------------------------------------------------------*
* FORM ABSCHLUSS_HAUSBANK_KONTO                                        *
*----------------------------------------------------------------------*
* Ende unsere Banknummer für Zahlungsbegleitliste                      *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM ABSCHLUSS_HAUSBANK_KONTO.


  READ TABLE TAB_SPLITTING INDEX 1.
  IF SY-SUBRC EQ 0 OR REGUD-XEINZ EQ 'X'.
    ADD SUM_UBKNT TO SUM_BUKRS.
  ELSE.
    SUBTRACT SUM_UBKNT FROM SUM_BUKRS.
  ENDIF.
  IF SUM_UBKNT NE 0 AND FLG_BANKINFO NE 0.
    FLG_END_UBKNT = 1.
    NEW-PAGE.
    FORMAT COLOR 3 INTENSIFIED OFF.

    WRITE:      SY-VLINE NO-GAP,
           (40) TEXT_813.              "Anzahl Zahlungs-Träger:
    WRITE CNT_POSTEN TO REGUD-ZAHLT DECIMALS '0'.
    TRANSLATE REGUD-ZAHLT USING ' *'.
    WRITE:      REGUD-ZAHLT.

    TXT_BEGLEITL = TEXT_814.           "Gesamtbetrag Bank-Konto in
    REPLACE '&WAERS' WITH REGUD-HWAER INTO TXT_BEGLEITL.
    CONDENSE TXT_BEGLEITL.
    WHILE TXT_BEGLEITL+50 EQ SPACE.    "rechtsbündig abstellen
      SHIFT TXT_BEGLEITL RIGHT.
      IF SY-INDEX > 50. EXIT. ENDIF.
    ENDWHILE.
    WRITE: (51) TXT_BEGLEITL.
    WRITE SUM_UBKNT TO REGUD-SUMME CURRENCY T001-WAERS.
    TRANSLATE REGUD-SUMME USING ' *'.
    WRITE: 108  REGUD-SUMME,               "Betrag in Hauswährung
           132  SY-VLINE.

*   Betrag in Worten
    CALL FUNCTION 'SPELL_AMOUNT'
         EXPORTING
              LANGUAGE  = SY-LANGU
              CURRENCY  = T001-WAERS
              AMOUNT    = SUM_UBKNT
              FILLER    = SPACE
         IMPORTING
              IN_WORDS  = SPELL
         EXCEPTIONS
              NOT_FOUND = 1
              TOO_LARGE = 2.

*   Ausgabe des Betrags in Worten, wenn die Sprache nicht Japanisch ist
    IF SY-LANGU NA 'J'.
      CASE SY-SUBRC.

        WHEN 0.

          TXT_BEGLEITL = TEXT_816.         "in Worten
          WHILE TXT_BEGLEITL+73 EQ SPACE.  "rechtsbündig abstellen
                SHIFT TXT_BEGLEITL RIGHT.
            IF SY-INDEX > 73.
              EXIT.
            ENDIF.
          ENDWHILE.
          WRITE:       SY-VLINE NO-GAP,
                58(74) TXT_BEGLEITL,
                132    SY-VLINE.

*         Der Text in SPELL-WORD ist linksbündig (z.B. DREIZEHN________)
*         Gewünschtes Aussehen: rechtsbündig gesternt (______*DREIZEHN*)
          SHIFT SPELL-WORD RIGHT.
          SPELL-WORD+0(1) = '*'.

          WHILE SPELL-WORD+128 EQ SPACE.   "rechtsbündig abstellen
                SHIFT SPELL-WORD RIGHT.
            IF SY-INDEX > 128.
              EXIT.
            ENDIF.
          ENDWHILE.
          SPELL-WORD+129(1) = '*'.

          WRITE: SY-VLINE NO-GAP,
                 SPELL-WORD(130),          "Betrag in Worten
             132 SY-VLINE.

        WHEN 1.

*         In Tabelle T015Z fehlt ein Eintrag, Übergabe über Systemtab.
          CLEAR ERR_T015Z.
          ERR_T015Z-SPRAS = SY-MSGV1.
          ERR_T015Z-EINH  = SY-MSGV2.
          ERR_T015Z-ZIFF  = SY-MSGV3.
          COLLECT ERR_T015Z.

        WHEN 2.

*         Betrag ist zum Umsetzen zu groß, keine Ausgabe, keine Meldung

      ENDCASE.
    ENDIF.

    ULINE.
  ENDIF.


ENDFORM.                               "Abschluß Hausbank Konto



*----------------------------------------------------------------------*
* FORM KOPF_ZEILEN                                                     *
*----------------------------------------------------------------------*
* Wird in der Begleitliste zum Zeitpunkt TOP-OF-PAGE aufgerufen        *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM KOPF_ZEILEN.

  DATA:
    UP_TEXT(20) TYPE C.

  PERFORM BATCH-HEADING(RSBTCHH0).
  FORMAT COLOR 1 INVERSE.
  ULINE.
  WRITE SY-VLINE NO-GAP.

  BNKA-BANKA = REGUD-UBNKA.
  IF REGUH-PAYGR+18(2) EQ '$J'.        "Zweigstelle (nur Japan)
    SY-FDPOS = STRLEN( BNKA-BANKA ).
    IF SY-FDPOS LT 30.
      ADD 1 TO SY-FDPOS.
      BNKA-BANKA+SY-FDPOS = REGUD-UBRCH.
    ELSE.
      BNKA-BANKA+29 = SPACE.
      BNKA-BANKA+30 = REGUD-UBRCH.
    ENDIF.
  ENDIF.

  IF FLG_END_ZBUKR EQ 0 AND FLG_BANKINFO NE 0.
    WRITE: REGUH-UBKNT,                "Unser Bankkonto
        23 BNKA-BANKA,                 "Bankname
    71(29) REGUD-UBORT,                "Bankort
       101 TEXT_820(3),                "HB:
           REGUH-HBKID,                "Hausbank-Kurzschlüssel
           SPACE.
  ENDIF.
  WRITE:
     114 TEXT_821(3),                  "BK:
         REGUH-ZBUKR,                  "Buchungskreis
     (2) SPACE.
  IF FLG_END_ZBUKR EQ 0.
    WRITE:
         TEXT_822(3),                  "ZW:
         REGUH-RZAWE.                  "Zahlweg
  ENDIF.
  WRITE 132 SY-VLINE.
  ULINE.

  FORMAT COLOR 1 INTENSIFIED INVERSE OFF.
  IF FLG_END_ZBUKR EQ 0 AND FLG_END_UBKNT EQ 0.
    IF HLP_LAUFK EQ 'P'.
      TXT_ZEILE = TEXT_834.            "Überschrift Empfängerkontonr HR
    ELSE.
      TXT_ZEILE = TEXT_804.            "Überschrift Empfängerkontonr FI
    ENDIF.
    IF FLG_BANKINFO NE 2.
      WRITE: SPACE TO TXT_ZEILE+1(20).
    ENDIF.
    IF TEXT_831 NE SPACE AND TEXT_832 NE SPACE.
      UP_TEXT = TEXT_831.
      REPLACE '&CURR' WITH REGUD-WAERS INTO UP_TEXT.
      CONDENSE UP_TEXT.
      WHILE UP_TEXT+19 EQ SPACE.
        SHIFT UP_TEXT BY 1 PLACES RIGHT.
      ENDWHILE.
      IF REGUH-WAERS EQ T001-WAERS.
        CLEAR TXT_ZEILE+89.
        WRITE UP_TEXT TO TXT_ZEILE+110.
      ELSE.
        WRITE UP_TEXT TO TXT_ZEILE+89.
        UP_TEXT = TEXT_832.
        REPLACE '&CURR' WITH REGUD-HWAER INTO UP_TEXT.
        CONDENSE UP_TEXT.
        WHILE UP_TEXT+19 EQ SPACE.
          SHIFT UP_TEXT BY 1 PLACES RIGHT.
        ENDWHILE.
        WRITE UP_TEXT TO TXT_ZEILE+110.
      ENDIF.
    ELSE.                              "alte Logik, kann gelöscht werden
      IF REGUH-WAERS NE T001-WAERS.    "(nur bei fehlender Übersetzung!)
        REPLACE '&FW' WITH REGUD-WAERS INTO TXT_ZEILE.
      ELSE.
        REPLACE '&FW' WITH '     ' INTO TXT_ZEILE.
      ENDIF.
      REPLACE '&HW' WITH REGUD-HWAER INTO TXT_ZEILE.
    ENDIF.
    TXT_ZEILE+131(1) = SY-VLINE.
    WRITE: / TXT_ZEILE.
    ULINE.
  ENDIF.


ENDFORM.

*----------------------------------------------------------------------*
* Form  BANK_ACCOUNT_CONVERSION                                        *
*----------------------------------------------------------------------*
* Prüfen ob eine lange (mehr als 18 Stellen) Kontonummer vorliegt      *
* und füllen der langen Kontonummer in ein 20 stelliges Zielfeld.      *
* Hat die lange Kontonummer mehr als 20 Stellen wird dies im           *
* 20 stelligen Zielfeld gekennzeichnet ( 'XXXXX>' ).                   *
* -> Änderung zu 4.0c                                                  *
*----------------------------------------------------------------------*
* USING-Parameter                                                      *
* <--- up_zbnkn                                                        *
*----------------------------------------------------------------------*
FORM BANK_ACCOUNT_CONVERSION USING UP_ZBNKN.
  DATA UP_SUBRC LIKE SY-SUBRC.

  CALL FUNCTION 'CONVERT_BANK_ACCOUNT_NUMBER'
       EXPORTING I_BANKS      = REGUH-ZBNKS
                 I_BANKK      = REGUH-ZBNKY
                 I_BANKN      = REGUH-ZBNKN
                 I_BKONT      = REGUH-ZBKON
                 I_BKREF      = REGUH-BKREF
                 I_BANKL      = REGUH-ZBNKL
       IMPORTING E_BANKN_LONG = UP_ZBNKN
                 E_SUBRC      = UP_SUBRC.

  IF UP_SUBRC <> 0.
    UP_ZBNKN+19(1) = '>'.
  ENDIF.
ENDFORM.
