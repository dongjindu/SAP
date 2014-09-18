*
*        Allgemeine FORM Unterprogramme zum RM-LVS
*
*INCLUDE ML03TTOP.               " Nur für lokalen Syntaxcheck
*---------------------------------------------------------------------*
*       Inhalt:                                                       *
*                                                                     *
*         FCODE                   Steuerung der Bildfolge             *
*         FCODE_STACK_POP         Fcode und T342 zurückschreiben      *
*         FCODE_STACK_PUSH        Fcode und T342 retten               *
*         T340_LESEN              Lesen eines T340 Eintrags           *
*---------------------------------------------------------------------*


*---------------------------------------------------------------------*
*       FORM FCODE                                                    *
*---------------------------------------------------------------------*
*       Die Prozedur FCODE bearbeitet die FCODE-Eingabe des Benutzers.*
*       In Abhaengigkeit dieser Eingabe und verschiedener Stati der   *
*       Transaktion wird ein Eintrag in T342 gelesen:                 *
*         CON_MODPL   Name des aktuellen Modulpools                   *
*         SY-DYNNR    Nummer des aktuellen Dynpros                    *
*         T340-AKTYP  Aktivitaetstyp der aktuellen Transaktion        *
*         FCODE       Vom Benutzer eingegebener FCODE                 *
*         VORGA       Derzeitiger Vorgangsschluessel der aktuellen    *
*                     Transaktion                                     *
*         VORDY       Dynamischer Vorgang in der aktuellen Trans-     *
*                     aktion                                          *
*       Der gefundene Eintrag steuert den weiteren Ablauf der         *
*       Transaktion:                                                  *
*         T342-FDYNP  Nummer des naechsten Dynpros                    *
*         T342-MODNR  Nummer des Unterprogramms, das den eingegebenen *
*                     FCODE verarbeitet.                              *
*         T342-MODN2  ggf. weiteres Unterprogramm für weitere Ver-    *
*                     arbeitung des FCODE's.                          *
*         T342-DUNKL  Vorschlagswert, ob das naechste Dynpro hell     *
*                     oder dunkel prozessiert werden soll.            *
*         T342-NFCHE  Vorschlagswert fuer den FCODE bei Hellablauf    *
*         T342-NFCDU  Vorschlagswert fuer den FCODE bei Dunkelablauf  *
*                     Ob das aktuelle Dynpro gerade hell oder dunkel  *
*                     abläuft, steht in der Variablen DUNKEL          *
*         T342-VORGR  Reset fuer den Vorgangsschluessel               *
*                                                                     *
*---------------------------------------------------------------------*
FORM FCODE.

*........passenden Eintrag in der Steuertabelle T342 lesen.............

  DYNNR_PACK = SY-DYNNR.
  UNPACK DYNNR_PACK TO DYNNR.
  SELECT SINGLE * FROM T342
  WHERE MODPL = CON_MODPL    AND
        LDYNP = DYNNR        AND
        TRTYP = T340-TRTYP   AND
        FCODE = FCODE        AND
        VORGA = VORGA        AND
        VORDY = VORDY.

*........Kein passender Eintrag vorhanden => TRTYP maskieren...........

  IF SY-SUBRC NE 0.
    SELECT SINGLE * FROM T342
    WHERE MODPL = CON_MODPL    AND
          LDYNP = DYNNR        AND
          TRTYP = MASK1        AND
          FCODE = FCODE        AND
          VORGA = VORGA        AND
          VORDY = VORDY.

*........Kein Eintrag vorhanden => Fehler..............................

    IF SY-SUBRC NE 0.
      CLEAR FCODE.
      MESSAGE ID 'L3' TYPE 'E' NUMBER '705'
              WITH CON_MODPL DYNNR T340-TRTYP FCODE.
    ENDIF.

  ENDIF.

*........Die dynamische Vorgangsart hat nur Weichenfunktion und muß
*........zurückgesetzt werden, um die Größe der T342 zu begrenzen.......

  CLEAR VORDY.

*........Lokale Routine anspringen, um dann entspr. MODNR aufzurufen...

  PERFORM FCODE_FORMS USING T342-MODNR.
  IF NOT ( T342-MODN2 IS INITIAL ) .
    PERFORM FCODE_FORMS USING T342-MODN2.
  ENDIF.

*........Vorschlag-Fcode setzen........................................

  IF DUNKL EQ CON_HELL.

*........Wenn Hellablauf => Fcode NFCHE...............................

    MOVE T342-NFCHE TO FCODE.

  ELSE.
    IF T342-NFCDU NE INIT_FCODE.

*........Wenn Dunkelablauf und Fcode NFCDU vorhanden => Fcode NFCDU...

      MOVE T342-NFCDU TO FCODE.

    ELSE.

*........Wenn Dunkelablauf und Fcode NFCDU initial => Fcode NFCHE.....

      MOVE T342-NFCHE TO FCODE.
    ENDIF.

  ENDIF.

*........Folgedynpro setzen...........................................

  SET SCREEN T342-FDYNP.

*........ggf. Vorgang zurücksetzen....................................

  IF T342-VORGR EQ CON_X.
    MOVE SPACE TO VORGA.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FCODE_STACK_POP                                          *
*---------------------------------------------------------------------*
*       Fcode und T342 zurückschreiben                                *
*---------------------------------------------------------------------*
FORM FCODE_STACK_POP.
  DATA: INDEX LIKE SY-INDEX.
  DESCRIBE TABLE FCODE_STACK LINES INDEX.
  READ TABLE FCODE_STACK INDEX INDEX.
  FCODE = FCODE_STACK-SAV_FCODE.
  VORGA = FCODE_STACK-SAV_VORGA.
* VORDY = FCODE_STACK-SAV_VORDY.
  SY-DYNNR = FCODE_STACK-SAV_DYNNR.
  MOVE-CORRESPONDING FCODE_STACK TO T342.
  DELETE FCODE_STACK INDEX INDEX.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FCODE_STACK_PUSH                                         *
*---------------------------------------------------------------------*
*       Fcode und T342 retten                                         *
*---------------------------------------------------------------------*
FORM FCODE_STACK_PUSH.
  CLEAR FCODE_STACK.
  FCODE_STACK-SAV_FCODE = FCODE.
  FCODE_STACK-SAV_VORGA = VORGA.
* FCODE_STACK-SAV_VORDY = VORDY.
  FCODE_STACK-SAV_DYNNR = SY-DYNNR.
  MOVE-CORRESPONDING T342 TO FCODE_STACK.
  APPEND FCODE_STACK.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM T340_LESEN                                               *
*---------------------------------------------------------------------*
*       Lesen der Feldauswahlleiste der aktuellen Transaktion         *
*---------------------------------------------------------------------*
*       INPUT:     P_TCODE             Transaktionscode               *
*---------------------------------------------------------------------*
FORM T340_LESEN USING VALUE(P_TCODE).

*........Tabelle lesen.................................................

  SELECT SINGLE * FROM T340
  WHERE TCODE = P_TCODE.

*.........Fehler, wenn Eintrag nicht vorhanden.........................

  IF SY-SUBRC NE 0.
    MESSAGE ID 'L3' TYPE 'E' NUMBER '703' WITH P_TCODE.
  ENDIF.

*........Setzen globaler Variablen anhand des Tabelleneintrags.........

  MOVE T340-FCODE TO FCODE.
  MOVE T340-DUNKL TO DUNKL.
  MOVE T340-VORGA TO VORGA.

ENDFORM.
