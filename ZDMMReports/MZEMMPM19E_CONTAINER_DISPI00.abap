*pruefen : Modul T303T_lESEN noch notwendig, wenn Fremdschlüssel funk.?


*eject
*---------------------------------------------------------------------*
*       Modulpool ML01SI00: Process-After-Input zu SAPML01S           *
*---------------------------------------------------------------------*
*       Inhalt:                                                       *
*                                                                     *
*         D0100_EXIT              Fcode E... auf Dynpro D0100         *
*         D0101_EXIT              Fcode E... auf Dynpro D0101         *
*         D0300_EXIT              Fcode E... auf Dynpro D0300         *
*         D0301_EXIT              Fcode E... auf Dynpro D0301         *
*         D0209_STERN1            Sterneingabe in BESTQ (Hilfskonstr.)*
*         D0209_STERN2            Sterneingabe in SOBKZ (Hilfskonstr.)*
*         D0209_SONUM_CONVERT     Konvertierung,Prüfung Sonderbest.Nr.*
*         EXISTENZ_LAGERPLATZ     Aus LS25. Prüfen  ob Platz existiert*
*         EXISTENZ_LENUM          Überprüft Existenz Lenum            *
*         EXISTENZ_LGNUM          Prüfen, ob Lagernummer gepflegt     *
*         EXISTENZ_LGTYP          Prüfen, ob Lagertyp gepflegt        *
*         EXISTENZ_MATNR          Prüfen, ob Materialnummer gepflegt  *
*         MAKTX_LESEN_MLVS        Lesen des Materialkurztexts         *
*         PLATZ_MGEWI_ERMITTELN   Ermitteln Platzgewicht              *
*         PLATZ_POSITION_TRENN    Trennen der Platzposition vom Platz *
*         SPERR_PRUEFEN_LAGP      "Kein Sperrgrund ohne Sperre"       *
*         SPERR_PRUEFEN_LLHM      "Kein Sperrgrund ohne Sperre"       *
*         SPERR_PRUEFEN_LQUA      "Kein Sperrgrund ohne Sperre"       *
*         LEIN_LESEN              Lesen eines LEIN Eintrags           *
*         T300T_LESEN_LAGP        Lesen eines T300T Eintrags          *
*         T300T_LESEN_LLHM        Lesen eines T300T Eintrags          *
*         T300T_lesen_LQUA        Lesen eines T300T Eintrags          *
*         T300T_LESEN_LEIN        Lesen eines T300T Eintrags          *
*         T301T_LESEN_LEIN        Lesen eines T301T Eintrags          *
*         T301T_LESEN_LAGP        Lesen eines T301T Eintrags          *
*         T301T_LESEN_LLHM        Lesen eines T301T Eintrags          *
*         T302T_LESEN             Lesen eines T302T Eintrags          *
*         T303T_LESEN             Lesen eines T303T Eintrags          *
*         T307T_LESEN_LEIN        Lesen eines T307T Eintrags          *
*         T309T_LESEN             Lesen eines T309T Eintrags          *
*         T330T_LESEN_LAGP        Lesen eines T330T Eintrags          *
*         T330T_LESEN_LEIN        Lesen eines T330T Eintrags          *
*         T330T_LESEN_LLHM        Lesen eines T330T Eintrags          *
*         T330T_LESEN_LQUA        Lesen eines T330T Eintrags          *
*         T331_STEIN_LESEN        Prüfen, ob Einlagerstrategie_P      *
*         T331_PRLET_LESEN        Prüfen, ob LPTYP erforderlich       *
*         T340D_LESEN             Lesen der Defaultwerte im Lager     *
*         T340D_LESEN_LQUA        Lesen der Defaultwerte im Lager     *
*         T343_LESEN              Lesen der Lagerplatzstruktur        *
*         SAV_TCODE_LESEN         Holt sav_tcode aus memory           *
*         STATUS_TEXT_LESEN       Statustext zur LE aus ADIC lesen    *
*         PLATZ_POSITION_TR_LE    Trennen der Platzposition vom P.    *
*         PLAUF_CHANGE_FCODE      Funktionscode ändern bei Platzpos.  *
*---------------------------------------------------------------------*


*---------------------------------------------------------------------*
*       MODULE D0100_EXIT                                             *
*---------------------------------------------------------------------*
*       Fcode E... Verarbeitung auf Dynpro D0100                      *
*---------------------------------------------------------------------*
MODULE D0100_EXIT.

  CASE FCODE.
    WHEN FCODE_BACK.                   "Zurück
      LEAVE.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN FCODE_ESC.                    "ABBRECHEN
      LEAVE.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN FCODE_RET.                    "Return/Beenden
      LEAVE.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

  WRITE FCODE TO SY-TCODE+1(3).
  WRITE 'L'   TO SY-TCODE(1).

  LEAVE TO TRANSACTION SY-TCODE.

ENDMODULE.
*eject
*---------------------------------------------------------------------*
*       MODULE D0101_EXIT                                             *
*---------------------------------------------------------------------*
*       Fcode E... Verarbeitung auf Dynpro D0101                      *
*---------------------------------------------------------------------*
MODULE D0101_EXIT.

  CHECK FCODE NE FCODE_BACK.

  IF T340-TRTYP = CON_ANZEIGEN.
*........Anzeigemodus - Abbrechen/beenden unkritisch....................
    CASE FCODE.
      WHEN FCODE_RET.
        LEAVE TO TRANSACTION SPACE.    "Rücksprung total
      WHEN OTHERS.
        CLEAR FCODE.
        LEAVE.                         "Eine Ebene zurück
        LEAVE TO TRANSACTION SY-TCODE.
    ENDCASE.
  ELSEIF FCODE = FCODE_ESC.
*........POPUP..Verarbeitung abbrechen/Vorher sichern?.................
    PERFORM FC930.
    PERFORM FC935.
  ELSEIF FCODE = FCODE_RET.
*........POPUP..Verarbeitung beenden / Datenverlust in Kauf nehmen?....
    PERFORM FC938.
    PERFORM FC940.
  ENDIF.
ENDMODULE.
*eject

*-----------------------------------------------------------------------
*        MODULE D0300_EXIT
*-----------------------------------------------------------------------
*        Fcode E... Verarbeitung auf Dynpro D0300
*-----------------------------------------------------------------------
MODULE D0300_EXIT.

  CASE FCODE.
    WHEN FCODE_BACK.
      LEAVE.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN FCODE_ESC.
      LEAVE.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN FCODE_RET.
      LEAVE.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

  WRITE FCODE TO SY-TCODE+1(3).
  WRITE CON_L TO SY-TCODE(1).

  LEAVE TO TRANSACTION SY-TCODE.

ENDMODULE.
*---------------------------------------------------------------------*
*        MODULE D0301_EXIT                                            *
*---------------------------------------------------------------------*
*        Fcode E... Verarbeitung auf Dynpro D0301                     *
*---------------------------------------------------------------------*
MODULE D0301_EXIT.

  CHECK FCODE NE FCODE_BACK.

  IF T340-TRTYP = CON_ANZEIGEN.
*........Anzeigemodus - Abbrechen/beenden unkritisch....................
    IF FCODE = FCODE_RET.
      LEAVE TO TRANSACTION SPACE.      "Rücksprung total
    ELSE.
      CLEAR FCODE.
      LEAVE.                           "Eine Ebene zurück
      LEAVE TO TRANSACTION SY-TCODE.
    ENDIF.
  ELSEIF FCODE = FCODE_ESC.
*........POPUP..Verarbeitung abbrechen/Vorher sichern?.................
    PERFORM FC930.
    PERFORM FC935.
  ELSEIF FCODE = FCODE_RET.
*........POPUP..Verarbeitung beenden / Datenverlust in Kauf nehmen?....
    PERFORM FC938.
    PERFORM FC940.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE D0209_STERN1                                           *
*---------------------------------------------------------------------*
*       Sterneingabe im Feld BESTQ, da * nicht mittransportiert wird  *
*---------------------------------------------------------------------*
MODULE D0209_STERN1.
  RL01S-BESTQ = CON_STERN.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE D0209_STERN2                                           *
*---------------------------------------------------------------------*
*       Sterneingabe im Feld SOBKZ, da * nicht mittransportiert wird  *
*---------------------------------------------------------------------*
MODULE D0209_STERN2.
  RL01S-SOBKZ = CON_STERN.
ENDMODULE.

*----------------------------------------------------------------------*
*       Module  D0209_SONUM_CONVERT  INPUT
*----------------------------------------------------------------------*
*       Überprüfung und Konvertierung der Sonderbestandsnummer(LS24)   *
*----------------------------------------------------------------------*
MODULE D0209_SONUM_CONVERT INPUT.
  IF NOT RL01S-LSONR IS INITIAL.
    PERFORM SONUM_CONV_EXT_INT(SAPFL000) USING RL01S-SOBKZ
                                               RL01S-LSONR
                                               RL01S-SONUM.
  ENDIF.
ENDMODULE.                             " D0209_SONUM_CONVERT  INPUT

*---------------------------------------------------------------------*
*       MODULE EXISTENZ_LAGERPLATZ                                    *
*---------------------------------------------------------------------*
*       Pruefen in der Lagp, ob Lagerplatz (aus LS25) existiert       *
*---------------------------------------------------------------------*
MODULE EXISTENZ_LAGERPLATZ.
  PERFORM LAGP_LESEN USING RL01S-LGNUM
                           RL01S-LGTYP
                           RL01S-LGPLA.
  IF SY-SUBRC NE 0.
    MESSAGE E007 WITH RL01S-LGPLA.     "Lagerplatz nicht vorhanden
  ENDIF.
  IF LAGP-ANZQU =  0.
    MESSAGE E120 WITH RL01S-LGPLA.     "Kein Lagerquant zu $ vorhanden
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE EXISTENZ_LENUM                                         *
*---------------------------------------------------------------------*
*       Pruefen in der Lagp, ob Lagereinheit (aus LS27) existiert     *
*---------------------------------------------------------------------*
MODULE EXISTENZ_LENUM.
  PERFORM LEIN_LESEN USING RL01S-LENUM.
  IF SY-SUBRC NE 0.
    MESSAGE E201.
*   Lagereinheit ist nicht vorhanden (Eingabe prüfen)
  ENDIF.
  IF LEIN-ANZQU =  0.
    MESSAGE E120 WITH RL01S-LENUM.     "Kein Lagerquant zu $ vorhanden
  ENDIF.
  WRITE: LEIN-LGNUM TO RL01S-LGNUM,
         LEIN-LGTYP TO RL01S-LGTYP,
         LEIN-LGPLA TO RL01S-LGPLA.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE EXISTENZ_LGNUM                                         *
*---------------------------------------------------------------------*
*       Pruefen in der Lqua, ob Lagerplatz (aus LS25) existiert       *
*---------------------------------------------------------------------*
MODULE EXISTENZ_LGNUM.
  PERFORM T300T_LESEN USING RL01S-LGNUM.
  IF SY-SUBRC NE 0.
    MESSAGE E001 WITH RL01S-LGPLA.     "Lagernummer nicht in T300
  ENDIF.

*........Berechtigungspruefung Lagernummer..............................

  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP RL01S-LGNUM.
*
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE EXISTENZ_LGTYP                                  *
*---------------------------------------------------------------------*
*       Pruefen in der Lqua, ob Lagerplatz (aus LS25) existiert       *
*---------------------------------------------------------------------*
MODULE EXISTENZ_LGTYP.
  PERFORM T301T_LESEN USING RL01S-LGNUM RL01S-LGTYP.
  IF SY-SUBRC NE 0.
    MESSAGE E003 WITH RL01S-LGTYP.     "Lagertyp nicht in T301
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE EXISTENZ_MATNR
*---------------------------------------------------------------------*
*       Prüfen, ob Materialnummer im Werk und Lagernummer existiert   *
*---------------------------------------------------------------------*
MODULE EXISTENZ_MATNR.
  PERFORM MLVS_LESEN USING RL01S-MATNR
                           RL01S-WERKS
                           RL01S-LGNUM.
  IF SY-SUBRC NE 0.
    MESSAGE E041 WITH SY-TCODE.   "Transaktion nicht in T341 beschrieben
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE KAPAZITAET_PRUEFEN
*---------------------------------------------------------------------*
*       Prüfen, ob Gesamtkapazität kleiner als belegte Kapazität      *
*---------------------------------------------------------------------*
MODULE KAPAZITAET_PRUEFEN.

  IF RL01S-BKAPV > LAGP-LKAPV.
    MESSAGE W029.                      "Gesamtkapazität kleiner als
                                       "belegte Kapazität
  ENDIF.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE GEWICHT_PRUEFEN                                        *
*---------------------------------------------------------------------*
*       Prüfen, ob maximales Gewicht kleiner als belegtes Gewicht     *
*---------------------------------------------------------------------*
MODULE GEWICHT_PRUEFEN.

  IF LAGP-MGEWI > LAGP-LGEWI.
    MESSAGE W030.                      "Maximales Gewicht kleiner als
                                       "belegtes Gewicht
  ENDIF.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE MAKTX_LESEN_MLVS                                       *
*---------------------------------------------------------------------*
*       Lesen des Materialkurztextes                                  *
*---------------------------------------------------------------------*
MODULE MAKTX_LESEN_MLVS.
  IF NOT LQUA-MATNR IS INITIAL.
    PERFORM MLVS_LESEN USING LQUA-MATNR
                             LQUA-WERKS
                             LQUA-LGNUM.
    IF SY-SUBRC NE 0.
      MESSAGE E041 WITH SY-TCODE.      "Interner Fehler ist aufgetreten
    ENDIF.
  ELSE.
    MOVE TEXT-006 TO MLVS-MAKTX.       "Platzhalter für Lagereinheit
  ENDIF.
ENDMODULE.
*eject
*---------------------------------------------------------------------*
*       MODULE PLATZ_MGEWI_ERMITTELN                                  *
*---------------------------------------------------------------------*
*       Platzgewicht neu ermitteln                                    *
*---------------------------------------------------------------------*
MODULE PLATZ_MGEWI_ERMITTELN.
  PERFORM PLATZGEWICHT_ERMITTELN.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE PLATZ_MGEWI_MESSAGE                                    *
*---------------------------------------------------------------------*
*       Message-Ausgabe wenn Platzgewicht neu ermittelt wurde.        *
*---------------------------------------------------------------------*
MODULE PLATZ_MGEWI_MESSAGE.
  PERFORM PLATZGEWICHT_MESSAGE.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE PLATZ_POSITION_TRENN                                   *
*       Das Modul trennt bei Einlagerstrategie_P wieder die PLatzposi-
*       tion vom Lagerplatz. Dies ist notwendig, damit beim Ändern    *
*       nicht mit ursprünglichen Lagerplatz upgedatet wird.           *
*---------------------------------------------------------------------*
MODULE PLATZ_POSITION_TRENN.
  IF LQUA-PLPOS NE SPACE.
    CLEAR: LQUA-PLPOS.
    CALL FUNCTION 'L_PLATZ_POSITION_TRENNEN'
         EXPORTING
              LGNUM    = LQUA-LGNUM
              LGTYP    = LQUA-LGTYP
              LGPLA    = LQUA-LGPLA
         IMPORTING
              O_LGPLA  = LQUA-LGPLA
              O_PLPOS  = LQUA-PLPOS
         EXCEPTIONS
              NOT_FOUND.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE SAVE_DATA                                              *
*---------------------------------------------------------------------*
MODULE SAVE_DATA.
  SAV_LGEWI = LAGP-LGEWI.
  SAV_MGEWI = LAGP-MGEWI.
ENDMODULE.

*eject
*---------------------------------------------------------------------*
*       MODULE SPERR_PRUEFEN_LAGP                                     *
*---------------------------------------------------------------------*
*       Das Modul SPERR_PRUEFEN_LAGP prueft, ob bei angegebenem       *
*       Sperrgrund auch der Lagerplatz gesperrt wurde. Es ist moeg-   *
*       lich grundlos zu sperren. Es ist nicht moeglich, bei Angabe   *
*       eines Grundes den Platz nicht zu sperren.                     *
*---------------------------------------------------------------------*
MODULE SPERR_PRUEFEN_LAGP.
  IF LAGP-SPGRU NE CON_NICHTGESPERRT.
    IF LAGP-SKZUE EQ CON_NICHTGESPERRT AND
       LAGP-SKZUA EQ CON_NICHTGESPERRT.
      MESSAGE E015.       "Angabe Sperrgrund nur beim Sperren  erlaubt
    ENDIF.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE SPERR_PRUEFEN_LQUA                                     *
*---------------------------------------------------------------------*
*  ??   Das Modul SPERR_PRUEFEN_Lqua prueft, ob bei angegebenem       *
*  ??   Sperrgrund auch das Lagerquant      gesperrt wurde. Es ist    *
*  ??   moeglich grundlos zu sperren, aber es ist nicht moeglich, bei *
*  ??   Angabe eines Grundes das Hilfsmittel nicht zu sperren.        *
*---------------------------------------------------------------------*
MODULE SPERR_PRUEFEN_LQUA.
  IF LQUA-SPGRU NE CON_NICHTGESPERRT.
    IF LQUA-SKZUE EQ CON_NICHTGESPERRT AND
       LQUA-SKZUA EQ CON_NICHTGESPERRT.
      MESSAGE E015.      "Angabe Sperrgrund nur beim Sperren erlaubt
    ENDIF.
  ENDIF.
ENDMODULE.
*eject

*---------------------------------------------------------------------*
*        MODULE LEIN_LESEN                                            *
*---------------------------------------------------------------------*
*        Lesen der Daten zur Lagereinheit aus Tabelle LEIN            *
*---------------------------------------------------------------------*
MODULE LEIN_LESEN.
  HLP_LGNUM = LEIN-LGNUM.
  IF LEIN-LENUM NE INIT_LENUM.
    PERFORM LEIN_LESEN USING LEIN-LENUM.
    IF SY-SUBRC NE 0.
      MESSAGE E201.                    "Lagereinheit nicht vorhanden
    ELSEIF LEIN-LGNUM NE HLP_LGNUM AND
           HLP_LGNUM NE INIT_LGNUM.
      LEIN-LGNUM = HLP_LGNUM.
      MESSAGE E200.                    "LE passt nicht zur Lagernummer
    ELSE.
      SET PARAMETER ID 'LGN' FIELD LEIN-LGNUM.
    ENDIF.
  ELSE.
    CLEAR LEIN.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T300T_LESEN_LAGP                                       *
*---------------------------------------------------------------------*
*       Lesen der Lagernummerbezeichnung aus Tabelle T300T            *
*---------------------------------------------------------------------*
MODULE T300T_LESEN_LAGP.

  perform t300t_lesen_lagp.
***  IF LAGP-LGNUM NE INIT_LGNUM.
***    PERFORM T300T_LESEN USING LAGP-LGNUM.
***    IF SY-SUBRC NE 0.
***      MESSAGE W002 WITH LAGP-LGNUM SY-LANGU. "Lgnum nicht in T300
***    ENDIF.
***  ELSE.
***    CLEAR T300T.
***  ENDIF.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T300T_LESEN_LQUA                                       *
*---------------------------------------------------------------------*
*       Lesen der Lagernummerbezeichnung aus Tabelle T300T            *
*---------------------------------------------------------------------*
MODULE T300T_LESEN_LQUA.
  IF LQUA-LGNUM NE INIT_LGNUM.
    PERFORM T300T_LESEN USING LQUA-LGNUM.
    IF SY-SUBRC NE 0.
      MESSAGE W002 WITH LQUA-LGNUM SY-LANGU.   "Lgnum nicht in T300
    ENDIF.
  ELSE.
    CLEAR T300T.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T300T_LESEN_LEIN                                       *
*---------------------------------------------------------------------*
*       Lesen der Lagernummerbezeichnung aus Tabelle T300T            *
*---------------------------------------------------------------------*
MODULE T300T_LESEN_LEIN.
  IF LEIN-LGNUM NE INIT_LGNUM.
    PERFORM T300T_LESEN USING LEIN-LGNUM.
    IF SY-SUBRC NE 0.
      MESSAGE W002 WITH LEIN-LGNUM SY-LANGU. "Lgnum nicht in T300
    ENDIF.
  ELSE.
    CLEAR T300T.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T301T_LESEN_LEIN                                       *
*---------------------------------------------------------------------*
*       Lesen der Lagertypbezeichnung aus Tabelle T301T               *
*---------------------------------------------------------------------*
MODULE T301T_LESEN_LEIN.
  IF LEIN-LGTYP NE INIT_LGTYP.
    PERFORM T301T_LESEN USING LEIN-LGNUM LEIN-LGTYP.
    IF SY-SUBRC NE 0.
      MESSAGE W004 WITH LEIN-LGTYP SY-LANGU.    "Lgtyp nicht in T301
    ENDIF.
  ELSE.
    CLEAR T301T.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T301T_LESEN_LAGP                                       *
*---------------------------------------------------------------------*
*       Lesen der Lagertypbezeichnung aus Tabelle T301T               *
*---------------------------------------------------------------------*
MODULE T301T_LESEN_LAGP.

  perform t301t_lesen_lagp.
***  IF LAGP-LGTYP NE INIT_LGTYP.
***    PERFORM T301T_LESEN USING LAGP-LGNUM LAGP-LGTYP.
***    IF SY-SUBRC NE 0.
***      MESSAGE W004 WITH LAGP-LGTYP SY-LANGU.    "Lgtyp nicht in T301
***    ENDIF.
***  ELSE.
***    CLEAR T301T.
***  ENDIF.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T302T_LESEN                                            *
*---------------------------------------------------------------------*
*       Lesen der Lagerbereichbezeichnung aus Tabelle T302T           *
*---------------------------------------------------------------------*
MODULE T302T_LESEN.

  perform t302t_lesen_mask.
***  IF LAGP-LGBER NE INIT_LGBER.
***    PERFORM T302T_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGBER.
***    IF SY-SUBRC NE 0.
***      MESSAGE W006 WITH LAGP-LGBER SY-LANGU.   "Bezeichnung zu Lgber
***    ENDIF.
***  ELSE.
***    CLEAR T302T.
***  ENDIF.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T303T_LESEN                                            *
*---------------------------------------------------------------------*
*       Lesen der Lagerplatztypbezeichnung aus Tabelle T303T          *
*---------------------------------------------------------------------*
MODULE T303T_LESEN.
*pruefen
*........Prüfung, ob eingegebener Platztyp in Tabelle T303T vorhanden...

  perform t303t_lesen_mask.
***  IF LAGP-LPTYP NE INIT_LPTYP.
***    PERFORM T303T_LESEN USING LAGP-LGNUM LAGP-LPTYP.
***    IF SY-SUBRC NE 0.
***      MESSAGE W020 WITH LAGP-LPTYP SY-LANGU.   "Bezeichnung zu Lgtyp
***    ENDIF.
***  ELSE.
***    CLEAR T303T.
***  ENDIF.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T307T_LESEN_LEIN                                       *
*---------------------------------------------------------------------*
*       Lesen der Bezeichnung des Lagereinheitentyps aus T307T        *
*---------------------------------------------------------------------*
MODULE T307T_LESEN_LEIN.
  IF LEIN-LETYP NE INIT_LETYP.
    PERFORM T307T_LESEN USING LEIN-LGNUM LEIN-LETYP.
    IF SY-SUBRC NE 0.
      MESSAGE W010 WITH LEIN-LETYP SY-LANGU.   "Lagereinheitentyp Bez.
    ENDIF.
  ELSE.
    CLEAR T307T.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T309T_LESEN                                            *
*---------------------------------------------------------------------*
*       Lesen der Brandschutzzonenbezeichnung                         *
*---------------------------------------------------------------------*
MODULE T309T_LESEN.
*........Prüfung, ob eingegebener Platztyp in Tabelle T303T vorhanden...

  perform t309t_lesen.
***  IF NOT LAGP-BRAND IS INITIAL.
***    SELECT SINGLE * FROM T309T
***       WHERE SPRAS = SY-LANGU
***         AND LGNUM = LAGP-LGNUM
***         AND BRAND = LAGP-BRAND.
***    IF SY-SUBRC NE 0.
***      MESSAGE W255 WITH SY-LANGU LAGP-LGNUM LAGP-BRAND.
*   Brandabschnittstext & & & ist nicht vorhanden
***    ENDIF.
***  ELSE.
***    CLEAR T309T.
***  ENDIF.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T330T_LESEN_LAGP                                       *
*---------------------------------------------------------------------*
*       Lesen der Sperrgrundbezeichnung aus Tabelle T330T             *
*---------------------------------------------------------------------*
MODULE T330T_LESEN_LAGP.

  perform t330t_lesen_lagp.
***  IF LAGP-SPGRU NE INIT_SPGRU.
***    PERFORM T330T_LESEN USING LAGP-LGNUM LAGP-SPGRU.
***    IF SY-SUBRC NE 0.
***      MESSAGE W010 WITH LAGP-SPGRU SY-LANGU."Bezeichnung zu Sperrgr..
***    ENDIF.
***  ELSE.
***    CLEAR T330T.
***  ENDIF.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T330T_LESEN_LEIN                                       *
*---------------------------------------------------------------------*
*       Lesen der Sperrgrundbezeichnung aus Tabelle T330T             *
*---------------------------------------------------------------------*
MODULE T330T_LESEN_LEIN.
  IF LEIN-SPGRU NE INIT_SPGRU.
    PERFORM T330T_LESEN USING LEIN-LGNUM LEIN-SPGRU.
    IF SY-SUBRC NE 0.
      MESSAGE W010 WITH LEIN-SPGRU SY-LANGU.   "Bezeichnung zu Sperrgr.
    ENDIF.
  ELSE.
    CLEAR T330T.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T330T_LESEN_LQUA                                       *
*---------------------------------------------------------------------*
*       Lesen der Sperrgrundbezeichnung aus Tabelle T330T             *
*---------------------------------------------------------------------*
MODULE T330T_LESEN_LQUA.
  IF LQUA-SPGRU NE INIT_SPGRU.
    PERFORM T330T_LESEN USING LQUA-LGNUM LQUA-SPGRU.
    IF SY-SUBRC NE 0.
      MESSAGE W010 WITH LQUA-SPGRU SY-LANGU.   "Bezeichnung zu Sperrgr..
    ENDIF.
  ELSE.
    CLEAR T330T.
  ENDIF.
ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE T331_LESEN                                             *
*---------------------------------------------------------------------*
*       Lagertypsteuerung lesen                                       *
*---------------------------------------------------------------------*
MODULE T331_LESEN.
  PERFORM T331_LESEN.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T331_STEIN_LESEN:                                      *
*---------------------------------------------------------------------*
*       Prüfen, ob Einlagerungsstrategie P  vorgesehen                *
*---------------------------------------------------------------------*
MODULE T331_STEIN_LESEN.
  MOVE: RL01S-LGNUM TO LAGP-LGNUM,
        RL01S-LGTYP TO LAGP-LGTYP.
  PERFORM T331_LESEN.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T331_PRLET_LESEN:                                      *
*---------------------------------------------------------------------*
*       Prüfen, ob Lagerplatztyp erforderlich (T331-PRLET = 'X')      *
*       (PRLET=Prüfen Lagereinheitstyperforderlich)                   *
*---------------------------------------------------------------------*
MODULE T331_PRLET_LESEN.

  PERFORM T331_LESEN.

*........Wurde PLatztyp gelöscht, auch Platztyp-Text löschen............

  IF LAGP-LPTYP = INIT_LPTYP AND T303T-PTYPT NE INIT_PTYPT.
    CLEAR T303T-PTYPT.
  ENDIF.

*.........Ist LET-Prüfung angeschaltet, ist Platztyp erforderlich.......

  IF T331-PRLET NE INIT_X AND
     LAGP-LPTYP = INIT_LPTYP AND
     T340-TRTYP NE CON_ANZEIGEN.
    MESSAGE E021.    "Eingabe Lagerplatztyp nach T331 erforderlich
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T340D_LESEN                                           *
*---------------------------------------------------------------------*
*       Lesen der Defaultwerte im Lager                               *
*---------------------------------------------------------------------*
MODULE T340D_LESEN.
  PERFORM T340D_GEWEI_LESEN USING LAGP-LGNUM.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T340D_LESEN_LQUA                                       *
*---------------------------------------------------------------------*
*       Lesen der Defaultwerte im Lager                               *
*---------------------------------------------------------------------*
MODULE T340D_LESEN_LQUA.
  PERFORM T340D_GEWEI_LESEN USING LQUA-LGNUM.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T343_LESEN                                            *
*---------------------------------------------------------------------*
*       Lesen der Lagerplatzstruktur aus Tabelle T343                 *
*---------------------------------------------------------------------*
MODULE T343_LESEN.
  PERFORM T343_LESEN USING RL01S-LGNUM RL01S-LGTYP RL01S-LFDNR.
  IF SY-SUBRC NE 0.
    MESSAGE W344.                      "Eintrag nicht in T343 enthalten
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE SAV_TCODE_LESEN                                        *
*---------------------------------------------------------------------*
*       SAV_TCODE aus memory lesen   (vor STATUS_TEXT_LESEN!)         *
*---------------------------------------------------------------------*
MODULE SAV_TCODE_LESEN.
  IMPORT SAV_TCODE FROM MEMORY ID ANZLE_ID.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE STATUS_TEXT_LESEN                                      *
*---------------------------------------------------------------------*
*       Statustext aus ADIC zur Lagereinheit erfassen                 *
*---------------------------------------------------------------------*
MODULE STATUS_TEXT_LESEN.
  CLEAR DDTEXT.

  PERFORM ADIC_TEXT_LESEN USING CON_A
                                SY-LANGU
                                CON_DNAME
                                LEIN-STATU
                                DDTEXT.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE PLATZ_POSITION_TR_LE                                   *
*       Das Modul trennt bei Einlagerstrategie_P wieder die Platzposi-
*       tion vom Lagerplatz. Dies ist notwendig, damit beim Ändern    *
*       nicht mit ursprünglichen Lagerplatz upgedatet wird.           *
*---------------------------------------------------------------------*
MODULE PLATZ_POSITION_TR_LE.
  IF LEIN-PLPOS NE SPACE.
    CLEAR: LEIN-PLPOS.
    CALL FUNCTION 'L_PLATZ_POSITION_TRENNEN'
         EXPORTING
              LGNUM    = LEIN-LGNUM
              LGTYP    = LEIN-LGTYP
              LGPLA    = LEIN-LGPLA
         IMPORTING
              O_LGPLA  = LEIN-LGPLA
              O_PLPOS  = LEIN-PLPOS
         EXCEPTIONS
              NOT_FOUND.
  ENDIF.
ENDMODULE.
*---------------------------------------------------------------------*
*      Module  T30AT_LESEN  INPUT                                     *
*---------------------------------------------------------------------*
*      Bezeichung zu Kommissionierbereich aus Tabelle T30AT lesen     *
*---------------------------------------------------------------------*
MODULE T30AT_LESEN INPUT.

  perform t30at_lesen_mask.
***  IF LAGP-KOBER NE INIT_KOBER.
***    PERFORM T30AT_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-KOBER.
***    IF SY-SUBRC NE 0.
***      MESSAGE W375 WITH LAGP-KOBER SY-LANGU.
*  Bezeichnung zu Kommissionierbereich & ist nicht in Sprache & gepflegt
***    ENDIF.
***  ELSE.
***    CLEAR T30AT.
***  ENDIF.

ENDMODULE.                             " T30AT_LESEN  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_FOR_VARIANT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_FOR_VARIANT INPUT.

  IF SY-TCODE = 'LS04'.
    I_VARIANT-REPORT = CON_RLLS0400.
  ENDIF.
  IF SY-TCODE = 'LS26'.
    I_VARIANT-REPORT = CON_RLLS2600.
  ENDIF.
  IF SY-TCODE = 'LS24'.
    I_VARIANT-REPORT = CON_RLLS2400.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = I_VARIANT
*           I_TABNAME_HEADER    =
*           I_TABNAME_ITEM      =
*           IT_DEFAULT_FIELDCAT =
            I_SAVE              = 'A'
       IMPORTING
*           E_EXIT              =
            ES_VARIANT          = E_VARIANT
       EXCEPTIONS
            NOT_FOUND           = 1
            PROGRAM_ERROR       = 2
            OTHERS              = 3.

  CASE SY-SUBRC.
    WHEN 0.
      MOVE E_VARIANT-VARIANT TO RL01S-LISTV.
    WHEN 1.
      MESSAGE S205(0K).
*   Keine Auswahl vorhanden!
    WHEN OTHERS.
      MESSAGE E705 WITH 'REUSE_ALV_VARIANT_F4' 'SY-SUBRC' '=' SY-SUBRC.
*   Interner Fehler ist aufgetreten (& & & &)
  ENDCASE.

ENDMODULE.                             " F4_FOR_VARIANT  INPUT

*---------------------------------------------------------------------*
*      Module  VARIANT_EXISTENCE  INPUT                               *
*---------------------------------------------------------------------*
*      Prüfen, ob selektierte Variante existiert                      *
*---------------------------------------------------------------------*
MODULE VARIANT_EXISTENCE INPUT.

  IF NOT RL01S-LISTV IS INITIAL.
    MOVE RL01S-LISTV  TO C_VARIANT-VARIANT.

    IF SY-TCODE = 'LS04'.
      MOVE CON_RLLS0400 TO C_VARIANT-REPORT.
    ENDIF.
    IF SY-TCODE = 'LS26'.
      MOVE CON_RLLS2600 TO C_VARIANT-REPORT.
    ENDIF.
    IF SY-TCODE = 'LS24'.
      MOVE CON_RLLS2400 TO C_VARIANT-REPORT.
    ENDIF.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
         EXPORTING
              I_SAVE        = 'A'
         CHANGING
              CS_VARIANT    = C_VARIANT
         EXCEPTIONS
              WRONG_INPUT   = 1
              NOT_FOUND     = 2
              PROGRAM_ERROR = 3
              OTHERS        = 4.

    CASE SY-SUBRC.
      WHEN 0.
*........alles in Ordnung..............................................
      WHEN 2.
        MESSAGE E204(0K).
*   Selektierte Variante nicht vorhanden!
        MESSAGE E204(0K).
      WHEN OTHERS.
        MESSAGE E705 WITH 'REUSE_ALV_VARIANT_EXISTENCE' 'SY-SUBRC'
                          '=' SY-SUBRC.
*    Interner Fehler ist aufgetreten (& & & &)
    ENDCASE.
  ENDIF.

ENDMODULE.                             " VARIANT_EXISTENCE  INPUT
*---------------------------------------------------------------------*
*      Module  EXISTENZ_LGORT  INPUT                                  *
*---------------------------------------------------------------------*
*      Prüft, ob Werk/Lagerort-Kombination definiert ist              *
*---------------------------------------------------------------------*
MODULE EXISTENZ_LGORT INPUT.
*........Bei Eingabe eines Lagerortes ist Werksangabe Pflicht..........
  IF ( RL01S-WERKS IS INITIAL ) AND ( NOT RL01S-LGORT IS INITIAL ).
    MESSAGE E224.
*    Bei Selektion mit Lagerort ist Werksangabe erforderlich

*........Prüfen, ob Werk/Lagerort-Kombination in T320..................
  ELSEIF ( NOT RL01S-WERKS IS INITIAL )
    AND ( NOT RL01S-LGORT IS INITIAL ).
    SELECT SINGLE * FROM T320 WHERE WERKS = RL01S-WERKS
                                AND LGORT = RL01S-LGORT.
    IF SY-SUBRC NE 0.
      MESSAGE E212.
*    Keine Daten zu Werk/Lagerort vorhanden (Eingabe prüfen)
    ENDIF.
  ENDIF.
ENDMODULE.                             " EXISTENZ_LGORT  INPUT
*---------------------------------------------------------------------*
*      Module  PLAUF_CHANGE_FCODE  INPUT                              *
*---------------------------------------------------------------------*
*      Bei Anzeige der Platzaufteilung den übergebenen Funktionscode  *
*      auf 'BEST' ändern und den originalen FCode sichern             *
*---------------------------------------------------------------------*
MODULE PLAUF_CHANGE_FCODE INPUT.

  IF FCODE CP 'BE++'.
    MOVE FCODE      TO SAV_FCODE.
    MOVE FCODE_BEST TO FCODE.
  ENDIF.

ENDMODULE.                             " PLAUF_CHANGE_FCODE  INPUT
*---------------------------------------------------------------------*
*      Module  EXISTENZ_MATNR_IN_LGNUM  INPUT                         *
*---------------------------------------------------------------------*
*      Ist Material in Lagernummer definiert?                         *
*---------------------------------------------------------------------*
MODULE EXISTENZ_MATNR_IN_LGNUM INPUT.

  SELECT SINGLE * FROM MLGN WHERE LGNUM = RL01S-LGNUM
                              AND MATNR = RL01S-MATNR.

  IF SY-SUBRC NE 0.
    MESSAGE E145 WITH RL01S-MATNR RL01S-LGNUM.
*   Material & ist in Lagernummer & nicht vorhanden
  ENDIF.

ENDMODULE.                             " EXISTENZ_MATNR_IN_LGNUM  INPUT
*&---------------------------------------------------------------------*
*&      Module  DATA_SAVE_D0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DATA_SAVE_D0400 INPUT.

  move lagp-lgnum to neu_lgnum.
  move lagp-lgtyp to neu_lgtyp.
  move lagp-lgpla to neu_lgpla.

  exit.

ENDMODULE.                 " DATA_SAVE_D0400  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALL_LT21_D4005  INPUT
*&---------------------------------------------------------------------*
* branch to transaction LT21 (Display TO) by double-click on LAGP-BTANR
*----------------------------------------------------------------------*
MODULE CALL_LT21_D4005 INPUT.

  check fcode eq 'AUSW'.
  clear fcode.
  check feld eq 'LAGP-BTANR'.
  check not lagp-btanr is initial.

  set parameter:  id 'LGN' field lagp-lgnum,
                  id 'TAN' field lagp-btanr.

  call transaction con_tcode_LT21 and skip first screen.

ENDMODULE.                 " CALL_LT21_D4005  INPUT
*&---------------------------------------------------------------------*
*&      Module  D4003_ABSCHLUSS  INPUT
*&---------------------------------------------------------------------*
*       Reset FCODE at the end of PAI in dynpro D4003
*----------------------------------------------------------------------*
MODULE D4003_ABSCHLUSS INPUT.

  if fcode = fcode_ausw.
    clear fcode.
  endif.

ENDMODULE.                 " D4003_ABSCHLUSS  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALL_OLML_D0400  INPUT
*&---------------------------------------------------------------------*
*       Branch to detail view of storage type data
*----------------------------------------------------------------------*
MODULE CALL_OLML_D0400 INPUT.

data: lgtyp_key(10) type c.

  check fcode eq 'AUSW'.
  check feld eq 'LAGP-LGTYP'.
  clear fcode.
  check not lagp-lgtyp is initial.

  concatenate lagp-mandt lagp-lgnum lagp-lgtyp into lgtyp_key.

CALL FUNCTION 'VIEW_MAINTENANCE_SINGLE_ENTRY'
     EXPORTING
          ACTION                       = 'SHOW'
*         CORR_NUMBER                  = '          '
          VIEW_NAME                    = 'V_T3010'
*         NO_WARNING_FOR_CLIENTINDEP   = ' '
*         RFC_DESTINATION_FOR_UPGRADE  = ' '
*         CLIENT_FOR_UPGRADE           = ' '
*         VARIANT_FOR_SELECTION        = ' '
*         NO_TRANSPORT                 = ' '
*         SUPPRESSDIALOG               = ' '
*         INSERT_KEY_NOT_FIXED         = ' '
*         COMPLEX_SELCONDS_USED        = ' '
*    IMPORTING
*         CORR_NUMBER                  =
*    TABLES
*         DBA_SELLIST                  =
*         EXCL_CUA_FUNCT               =
     CHANGING
          ENTRY                        = lgtyp_key
*    EXCEPTIONS
*         ENTRY_ALREADY_EXISTS         = 1
*         ENTRY_NOT_FOUND              = 2
*         CLIENT_REFERENCE             = 3
*         FOREIGN_LOCK                 = 4
*         INVALID_ACTION               = 5
*         NO_CLIENTINDEPENDENT_AUTH    = 6
*         NO_DATABASE_FUNCTION         = 7
*         NO_EDITOR_FUNCTION           = 8
*         NO_SHOW_AUTH                 = 9
*         NO_TVDIR_ENTRY               = 10
*         NO_UPD_AUTH                  = 11
*         SYSTEM_FAILURE               = 12
*         UNKNOWN_FIELD_IN_DBA_SELLIST = 13
*         VIEW_NOT_FOUND               = 14
*         OTHERS                       = 15
          .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

ENDMODULE.                 " CALL_OLML_D0400  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0400_EXIT  INPUT
*&---------------------------------------------------------------------*
*       Fcode E... Processing on dynpro D0400                          *
*----------------------------------------------------------------------*
MODULE D0400_EXIT INPUT.

  CHECK FCODE NE FCODE_BACK.

* S01 = create / S02 = change/delete / S03 = display
  if fcode eq 'S01' or
     fcode eq 'S02' or
     fcode eq 'S03'.
    if t340-trtyp eq con_anzeigen.
      WRITE FCODE TO SY-TCODE+1(3).
      WRITE 'L'   TO SY-TCODE(1).
      LEAVE TO TRANSACTION SY-TCODE.
    else.
      PERFORM FC930.
      WRITE FCODE TO SY-TCODE+1(3).
      WRITE 'L'   TO SY-TCODE(1).
      PERFORM FC935.
    endif.
  endif.

  IF T340-TRTYP = CON_ANZEIGEN.
*........Anzeigemodus - Abbrechen/beenden unkritisch...................
    CASE FCODE.
      WHEN FCODE_RET.
        LEAVE TO TRANSACTION SPACE.    "Rücksprung total
      WHEN OTHERS.
***        CLEAR FCODE.
***        LEAVE.                         "Eine Ebene zurück
***        LEAVE TO TRANSACTION SY-TCODE.
        clear antwort.
        perform fc930.
*.......generelles Verlassen der Transaktion...........................
        perform fc940.
    ENDCASE.
  ELSEIF FCODE = FCODE_ESC.
*........POPUP..Verarbeitung abbrechen/Vorher sichern?.................
    PERFORM FC930.
    if sy-datar is initial.
      PERFORM FC940.
    else.
      perform fc935.
    endif.
  ELSEIF FCODE = FCODE_RET.
*........POPUP..Verarbeitung beenden / Datenverlust in Kauf nehmen?....
    PERFORM FC938.
    PERFORM FC940.
  ENDIF.

ENDMODULE.                 " D0400_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0400_CHECK_LGBER  INPUT
*&---------------------------------------------------------------------*
*       Check field LAGP-LGBER if something is entered
*----------------------------------------------------------------------*
MODULE D0400_CHECK_LGBER INPUT.

  check fcode eq fcode_bu.

  if lagp-lgber is initial.
    message e037.
  endif.

ENDMODULE.                 " D0400_CHECK_LGBER  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0400_CHECK_LPTYP  INPUT
*&---------------------------------------------------------------------*
*       Check field LAGP-LPTYP if something is entered
*----------------------------------------------------------------------*
MODULE D0400_CHECK_LPTYP INPUT.

  check fcode eq fcode_bu.

  PERFORM T331_LESEN.

*.........Ist LET-Prüfung angeschaltet, ist Platztyp erforderlich.......
  IF T331-PRLET NE INIT_X AND
     LAGP-LPTYP = INIT_LPTYP AND
     T340-TRTYP NE CON_ANZEIGEN.
    MESSAGE E037.    "Eingabe Lagerplatztyp nach T331 erforderlich
  ENDIF.

ENDMODULE.                 " D0400_CHECK_LPTYP  INPUT
*&---------------------------------------------------------------------*
*&      Module  D4002_ABSCHLUSS  INPUT
*&---------------------------------------------------------------------*
*       Reset FCODE at the end of PAI in dynpro D4002
*----------------------------------------------------------------------*
MODULE D4002_ABSCHLUSS INPUT.

  if fcode = fcode_ausw.
    clear fcode.
  endif.

ENDMODULE.                 " D4002_ABSCHLUSS  INPUT

*&---------------------------------------------------------------------*
*&      Module  D4001_ABSCHLUSS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4001_ABSCHLUSS INPUT.

  if fcode = fcode_ausw.
    clear fcode.
  endif.

ENDMODULE.                 " D4001_ABSCHLUSS  INPUT
*&---------------------------------------------------------------------*
*&      Module  D4004_ABSCHLUSS  INPUT
*&---------------------------------------------------------------------*
*       Reset FCODE at the end of PAI in dynpro D4001
*----------------------------------------------------------------------*
MODULE D4004_ABSCHLUSS INPUT.

  if fcode = fcode_ausw.
    clear fcode.
  endif.

ENDMODULE.                 " D4004_ABSCHLUSS  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0400_CHECK_KEY  INPUT
*&---------------------------------------------------------------------*
*       Check if there was a change of the key values after
*         reading key data. If so, the user receives a message
*         and must confirm the new key.
*         Leave PAI and process PBO of dynpro D0400 without
*           processing the subscreens.
*----------------------------------------------------------------------*
MODULE D0400_CHECK_KEY INPUT.

  check not bin_is_checked is initial.

  if lagp-lgnum ne merk_lgnum or
     lagp-lgtyp ne merk_lgtyp or
     lagp-lgpla ne merk_lgpla.
    clear bin_is_checked.
*** Reset page-scrolling in tablecontrol -> Display starts with first
***   page and not with the page you actually scroll to.
    d4002-top_line = 1.
    d4003-top_line = 1.

    if t340-trtyp eq con_hinzufuegen or
       ( t340-trtyp eq con_veraendern and
         bin_is_changed eq con_x ) or
       ( t340-trtyp eq con_veraendern and
         fcode eq fcode_bu ) or
       ( t340-trtyp eq con_veraendern and
         fcode eq fcode_lo ).
      if ( ( not merk_lgnum is initial ) and
           ( not merk_lgtyp is initial ) and
           ( not merk_lgpla is initial ) ).
        clear fcode.
        message s038.
        leave screen.
      endif.
    endif.
  endif.

ENDMODULE.                 " D0400_CHECK_KEY  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CHANGES_D4001  INPUT
*&---------------------------------------------------------------------*
*       Check in change mode if a change was made in the entry fields.
*       If no change is made, the user can enter a new key.
*       If a change is made, the user receive a message and must confirm
*          the new key and enter the data again.
*----------------------------------------------------------------------*
MODULE CHECK_CHANGES_D4001 INPUT.

  clear bin_is_changed.

  check t340-trtyp eq con_veraendern.

  if lagp-lgber ne merk_lgber or
     lagp-kober ne merk_kober or
     lagp-brand ne merk_brand or
     lagp-lptyp ne merk_lptyp or
     lagp-lgewi ne merk_lgewi or
     lagp-lkapv ne merk_lkapv or
     lagp-verif ne merk_verif or
     lagp-sorlp ne merk_sorlp or
     lagp-reihf ne merk_reihf or
     lagp-skzue ne merk_skzue or
     lagp-skzua ne merk_skzua or
     lagp-spgru ne merk_spgru.
    bin_is_changed = con_x.
  endif.

ENDMODULE.                 " CHECK_CHANGES_D4001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALL_TCODE_D0203  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CALL_TCODE_D0203 INPUT.

data: help_jahr like mseg-mjahr.

  check fcode eq 'DOKL'.
  clear fcode.

  get cursor field feld line zeile.

  if feld eq 'LQUA-CHARG' and
     ( not lqua-charg is initial ).
    CALL FUNCTION 'L_BATCH_DISPLAY'
       EXPORTING
          I_MATNR   = lqua-matnr
          I_WERKS   = lqua-werks
          I_CHARG   = lqua-charg
*         I_LGORT   =
*    EXCEPTIONS
*         NOT_FOUND = 1
*         NO_DATA   = 2
*         OTHERS    = 3
          .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if feld eq 'LQUA-BTANR' and
     ( not lqua-btanr is initial ).
    CALL FUNCTION 'L_TO_DISPLAY'
       EXPORTING
          I_LGNUM   = lqua-lgnum
          I_TANUM   = lqua-btanr
*         I_TAPOS   =
*    EXCEPTIONS
*         NOT_FOUND = 1
*         NO_DATA   = 2
*         OTHERS    = 3
          .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if feld eq 'LQUA-LGPLA' and
     ( not lqua-lgpla is initial ).
    CALL FUNCTION 'L_BIN_DISPLAY'
       EXPORTING
          I_LGNUM   = lqua-lgnum
          I_LGTYP   = lqua-lgtyp
          I_LGPLA   = lqua-lgpla
*    EXCEPTIONS
*         NOT_FOUND = 1
*         NO_DATA   = 2
*         OTHERS    = 3
          .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if feld eq 'LQUA-WENUM' and
     ( not lqua-wenum is initial ).

     help_jahr = lqua-wdatu(4).

     CALL FUNCTION 'L_MATDOC_DISPLAY'
          EXPORTING
               I_MBLNR   = lqua-wenum
               I_WERKS   = lqua-werks
               I_MJAHR   = help_jahr
*         EXCEPTIONS
*              NOT_FOUND = 1
*              NO_DATA   = 2
*              OTHERS    = 3
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.
  endif.

  if feld eq 'LQUA-QPLOS' and
     ( not lqua-qplos is initial ).
    CALL FUNCTION 'L_INSPEC_LOT_DISPLAY'
       EXPORTING
          I_QPLOS   = lqua-qplos
*    EXCEPTIONS
*         NOT_FOUND = 1
*         NO_DATA   = 2
*         OTHERS    = 3
          .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if feld eq 'LQUA-LENUM' and
     ( not lqua-lenum is initial ).
    CALL FUNCTION 'L_SU_DISPLAY'
       EXPORTING
          I_LENUM   = lqua-lenum
*    EXCEPTIONS
*         NOT_FOUND = 1
*         NO_DATA   = 2
*         OTHERS    = 3
          .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if feld eq 'LQUA-VBELN' and
     ( not lqua-vbeln is initial ).
    CALL FUNCTION 'L_DELIVERY_DISPLAY'
      EXPORTING
        I_VBELN         = lqua-vbeln
*     EXCEPTIONS
*       NOT_FOUND       = 1
*       NO_DATA         = 2
*       OTHERS          = 3
          .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

ENDMODULE.                 " CALL_TCODE_D0203  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALL_TCODE_D0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CALL_TCODE_D0301 INPUT.

  check fcode eq 'DOKL'.
  clear fcode.

  get cursor field feld line zeile.

  if feld eq 'LEIN-LGPLA' and
     ( not lein-lgpla is initial ).
    CALL FUNCTION 'L_BIN_DISPLAY'
       EXPORTING
          I_LGNUM   = lein-lgnum
          I_LGTYP   = lein-lgtyp
          I_LGPLA   = lein-lgpla
*    EXCEPTIONS
*         NOT_FOUND = 1
*         NO_DATA   = 2
*         OTHERS    = 3
          .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if feld eq 'LEIN-BTANR' and
     ( not lein-btanr is initial ).
    CALL FUNCTION 'L_TO_DISPLAY'
       EXPORTING
          I_LGNUM   = lein-lgnum
          I_TANUM   = lein-btanr
*         I_TAPOS   =
*    EXCEPTIONS
*         NOT_FOUND = 1
*         NO_DATA   = 2
*         OTHERS    = 3
          .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

ENDMODULE.                 " CALL_TCODE_D0301  INPUT
*&---------------------------------------------------------------------*
*&      Module  PLAUF_CHANGE_FCODE_NEW  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PLAUF_CHANGE_FCODE_NEW INPUT.

  if fcode eq 'BEST'.
    exit.
  endif.

  IF FCODE CP 'BE++'.
    MOVE FCODE      TO SAV_FCODE.
    MOVE FCODE_BES2 TO FCODE.
  ENDIF.

ENDMODULE.                 " PLAUF_CHANGE_FCODE_NEW  INPUT
