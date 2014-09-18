
*---------------------------------------------------------------------*
*       Modulpool ML01SFFC: FCODE Bearbeitungen zu SAPML01S           *
*---------------------------------------------------------------------*
*       Inhalt:                                                       *
*                                                                     *
*         FC001  Dummy-Routine, die nichts tut                        *
*         FC002  Lesen LAGP und Vorbereitung fuers Hinzufuegen        *
*         FC003  Lesen LAGP und Vorbereitung fuers Veraendern         *
*         FC004  Lesen LAGP zum Anzeigen                              *
*         FC005  Hinzufuegen LAGP                                     *
*         FC006  Veraendern  LAGP                                     *
*         FC007  Loeschen    LAGP                                     *
*         FC008  Beenden Transaktion                                  *
*         FC009  Anzeigen Inventurdaten                               *
*         FC010  Anzeigen Leere Plaetze                               *
*         FC011  Zurueck ins Anforderdynpro                           *
*         FC018  Lesen LQUA und Vorbereitung fuers Hinzufuegen        *
*         FC019  Lesen LQUA und Vorbereitung fuers Veraendern         *
*         FC020  Lesen LQUA zum Anzeigen                              *
*         FC021  Hinzufuegen LQUA                                     *
*         FC022  Veraendern  LQUA                                     *
*         FC023  Loeschen    LQUA                                     *
*         FC024  Anzeigen Inventurdaten pro Quant                     *
*         FC025  Anzeigen Zusatzdaten zum Quant                       *
*         FC026  Anzeigen Quants pro Lagerplatz                       *
*         FC027  Anzeigen Quants zum Material                         *
*         FC028  Anz/Aend Quants pro Lagerplatz, Aufruf aus LS03/LS02 *
*         FC029  Lagerbestaende zum Material, Aufruf aus LS26         *
*         FC030  Maschinelles Anlegen von Lagerplätzen                *
*         FC031  Sprung ins Anfangsmenü                               *
*         FC032  Anzeigen Platzaufteilung zum Lagerplatz              *
*         FC033  Anzeigen Lagerplatzstatistik                         *
*         FC034  Reserve                                              *
*         FC035  Aufruf des Windows zu LS22/3 Inventurdaten           *
*         FC036  Aufruf des Windows zu LS22/3 Kundendaten             *
*         FC037  Rückruf aus Call Screen                              *
*         FC038  Transaktion beenden ohne POPUP                       *
*         FC039  Transaktion beenden mit POPUP                        *
*         FC040  F15 Beenden  Lagerplätze                             *
*         FC041  F15 Beenden  Lagerquants                             *
*         FC042  Lesen der LEIN                                       *
*         FC043  Anzeigen der Quants zur Lagereinheit (LS27)          *
*         FC044  Anzeigen der Quants zur Lagereinheit (LS32/LS33)     *
*         FC045  Anzeigen der Lagerplatzdaten zur Lagereinheit        *
*         FC046  Anzeigen der Inventurdaten zur Lagereinheit          *
*         FC047  Verändern der Sperrdaten Lagereinheit                *
*         FC048  Anzeigen Lagereinheiten zum Platz aus Transaktion LS28
*         FC049  Anzeigen Lagereinheiten zum Platz aus Transaktion LS03
*         FC050  Anzeigen Charge zum Quant                            *
*         FC051  Anzeigen Bestände zur Platzposition                  *
*         FC052  Anzeigen Bestände auf HU                             *
*         FC900  Aufruf des Call Screen Folgedynpro                   *
*         FC910  POPUP "Daten vorher sichern"                         *
*         FC920  Auswertung der Antwort auf Popup FC910               *
*         FC925  "Daten vorher sichern": JA/NEIN - Abfrage            *
*         FC930  Senden Popup zum Zeitpunkt AT-EXIT-COMMAND           *
*         FC940  Auswertung der Antwort auf Popup FC930               *
*         FC945  Auswertung der Antwort auf Popup FC930 beim Toggeln  *
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM FC001                                                    *
*---------------------------------------------------------------------*
*       Dummy-Routine fuers Nichtstun                                 *
*---------------------------------------------------------------------*
FORM FC001.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC002                                                    *
*---------------------------------------------------------------------*
*       FC002 trifft Vorbereitungen fuers Hinzufuegen eines neuen     *
*       LAGP-Satzes. Zunaechst wird gesperrt, dann gelesen. Der Satz  *
*       darf noch nicht vorhanden sein, sonst wird entsperrt, das     *
*       Satzbett neu initialisiert und der Fehler gemeldet.           *
*---------------------------------------------------------------------*
FORM FC002.
*........Berechtigungspruefung Lagernummer.............................

  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LAGP-LGNUM.

*........Plausibilitaetspruefung Lagerplatz............................

  PERFORM PRUEFUNG_LGPLA USING LAGP-LGPLA.

*........Lagerplatz sperren............................................
  PERFORM LAGP_SPERREN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
  PERFORM LAGP_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
  IF SY-SUBRC EQ 0.
    PERFORM LAGP_ENTSPERREN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
    PERFORM LAGP_INIT.
    MESSAGE E008 WITH LAGP-LGPLA.      "Lagerplatz bereits vorhanden
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM FC003                                                    *
*---------------------------------------------------------------------*
*       FC003 trifft Vorbereitungen fuers Veraendern eines vorhandenen*
*       LAGP-Satzes. Zunaechst wird gesperrt, dann gelesen. Der Satz  *
*       vorhanden sein, sonst wird entsperrt und der Fehler gemeldet. *
*---------------------------------------------------------------------*
FORM FC003.
*........Berechtigungspruefung Lagernummer.............................

  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LAGP-LGNUM.

*........Lagerplatz sperren............................................

* Unabhängig von der Sperrmimik wird beim Ändern Lagerplatz immer eine
* eine Lquax-Sperre abgesetzt um paralelle TA's und Inventur zu verhin-
* dern
  PERFORM LAGP_SPERREN_NEU USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
  PERFORM LAGP_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
  IF SY-SUBRC NE 0.
    PERFORM LAGP_ENTSPERREN_NEU USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
    MESSAGE E007 WITH LAGP-LGPLA.      "Lagerplatz nicht vorhanden
  ELSE.
    RL01S-BKAPV = LAGP-LKAPV - LAGP-RKAPV. "Belegte Kapazität ermittel
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC004                                                    *
*---------------------------------------------------------------------*
*       FC004 liest den LAGP-Satz, der angezeigt werden soll.         *
*       Ist er nicht vorhanden, wird eine entsprechende Fehlermeldung *
*       ausgegeben.                                                   *
*---------------------------------------------------------------------*
FORM FC004.
*........Berechtigungspruefung Lagernummer.............................

  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LAGP-LGNUM.

  PERFORM LAGP_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
  IF SY-SUBRC NE 0.
    MESSAGE E007 WITH LAGP-LGPLA.      "Lagerplatz nicht vorhanden
  ELSE.
    RL01S-BKAPV = LAGP-LKAPV - LAGP-RKAPV. "Belegte Kapazität ermitteln
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC005                                                    *
*---------------------------------------------------------------------*
*       Der aktuelle LAGP-Satz wird in die Datenbank aufgenommen.     *
*       Zuvor werden die vom System zu setzenden Daten in LAGP ein-   *
*       gestellt.                                                     *
*---------------------------------------------------------------------*
FORM FC005.
  PERFORM LAGP_SORLP_HINZU.
  PERFORM LAGP_REIHF_HINZU.
  PERFORM T340D_GEWEI_LESEN USING LAGP-LGNUM.
  MOVE: SY-UNAME TO LAGP-UNAME,
        SY-MANDT TO LAGP-MANDT,
        SY-DATLO TO LAGP-LAEDT.


  CALL FUNCTION 'L_LAGP_HINZUFUEGEN'   "IN UPDATE TASK
       EXPORTING
            XLAGP = LAGP.

  COMMIT WORK.
* EXPORT LAGP TO MEMORY.
* PERFORM LAGP_HINZUFUEGEN(SAPUL01S).
  MESSAGE S016 WITH LAGP-LGPLA.        "Lagerplatz hinzugefügt
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC006                                                    *
*---------------------------------------------------------------------*
*       Der aktuelle LAGP-Satz wird in der Datenbank veraendert.      *
*       Zuvor werden die vom System zu setzenden Daten in LAGPV ein-  *
*       gestellt.                                                     *
*---------------------------------------------------------------------*
FORM FC006.
* die veränderten Felder müssen in den verbucher gegeben werden. Im Ver-
* bucher muß nochmal nachgelesen werden, da sich in der Zwischenzeit im
* Platz oder in den dazugehörigen Quants durch eine paralelle
* TA-Quittierung etwas geändert haben könnte, da beim TA_Quittieren
* der Platz nicht mehr gesperrt wird.


  MOVE-CORRESPONDING LAGP TO LAGPV.
  MOVE: SY-UNAME TO LAGPV-UNAME,
        SY-DATLO TO LAGPV-LAEDT.

  CALL FUNCTION 'L_LAGP_VERAENDERN'
       EXPORTING
            XLAGPV = LAGPV.

  COMMIT WORK.
* EXPORT LAGP TO MEMORY.
* PERFORM LAGP_VERAENDERN(SAPUL01S).
  MESSAGE S017 WITH LAGPV-LGPLA.       "Lagerplatz geaendert
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC007                                                    *
*---------------------------------------------------------------------*
*       Der aktuelle LAGP-Satz wird aus der Datenbank geloescht.      *
*---------------------------------------------------------------------*
FORM FC007.
* Vor dem Löschen muß der Platz gesperrt werden, da sonst Verbuchungs-
* abbbrüche in der TA-Verarbeitung erfolgen können
  CALL FUNCTION 'L_BIN_LOCATION_ENQUEUE'
       EXPORTING
            I_ENQUE        = T340D-ENQUE
            I_LGNUM        = LAGP-LGNUM
            I_LGTYP        = LAGP-LGTYP
            I_LGPLA        = LAGP-LGPLA
            I_LENUM        = INIT_LENUM
       EXCEPTIONS
            FOREIGN_LOCK   = 01
            SYSTEM_FAILURE = 02.

  CASE SY-SUBRC.
    WHEN 1.    MESSAGE E170 WITH LAGP-LGPLA. "Platz bereits gesperrt
    WHEN 2.    MESSAGE E171.           "Fehler beim Sperren
  ENDCASE.

*Nach dem erfolgreichen Sperren muss der Lagerplatz nachgelesen werden
*da sich die Situation durch paralelle TA's verändert haben könnte
  PERFORM LAGP_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.

  PERFORM LAGP_LOESCHEN.

  IF SY-BINPT IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
         EXPORTING TITEL     = TEXT-205
                   TEXTLINE1 = TEXT-206
                   DEFAULTOPTION = ANTWORT_NEIN
*                  TEXTLINE2 = TEXT-207
         IMPORTING ANSWER    = ANTWORT.
  ELSE.
    ANTWORT = ANTWORT_JA.
  ENDIF.

  CLEAR FCODE.
  CASE ANTWORT.
    WHEN ANTWORT_NEIN.
    WHEN ANTWORT_JA.

      CALL FUNCTION 'L_LAGP_LOESCHEN'
           EXPORTING
                XLAGP = LAGP.

      COMMIT WORK.
      MESSAGE S018 WITH LAGP-LGPLA.    "Lagerplatz gelöscht
      PERFORM FC008.
  ENDCASE.
  PERFORM FCODE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC008                                                    *
*---------------------------------------------------------------------*
*       Die aktuelle Transaktion wird verlassen.                      *
*---------------------------------------------------------------------*
FORM FC008.

  IF SY-BINPT = CON_X.
    SET SCREEN 0.
    LEAVE SCREEN.
  ELSE.
    LEAVE.
    LEAVE TO TRANSACTION SY-TCODE.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC009                                                    *
*---------------------------------------------------------------------*
*       Anzeigen Inventurdaten in LS03                                *
*---------------------------------------------------------------------*
FORM FC009.
  DATA: CON_INVENTURHIST LIKE SY-TCODE     VALUE 'LI05'.
  SET PARAMETER:  ID 'LGN' FIELD LAGP-LGNUM,
                  ID 'LQN' FIELD LAGP-LGTYP,
                  ID 'LGP' FIELD LAGP-LGPLA.
  CALL TRANSACTION CON_INVENTURHIST AND SKIP FIRST SCREEN.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC010                                                    *
*---------------------------------------------------------------------*
*       Anzeigen leere Plaetze                                        *
*---------------------------------------------------------------------*
FORM FC010.
  IF NOT ( RL01S-PMAUF IS INITIAL ).
    IF T331-STEIN = CON_STEIN_P.
      EXPORT RL01S TO MEMORY ID LEPLA_ID.
*     submit rlls0401 and return.              " ersetzt durch RLLS0400
      SUBMIT RLLS0400 AND RETURN.
    ELSE.
      MESSAGE I345.
*   Für den Lagertyp ist keine Platzaufteilung vorgesehen
    ENDIF.
  ELSE.
    EXPORT RL01S TO MEMORY ID LEPLA_ID.
    SUBMIT RLLS0400 AND RETURN.
  ENDIF.
  LEAVE TO TRANSACTION SY-TCODE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC011                                                    *
*---------------------------------------------------------------------*
*       Ruecksprung aus einem CALL SCREEN                             *
*---------------------------------------------------------------------*
FORM FC011.
  CLEAR FCODE.
  LEAVE.
  LEAVE TO TRANSACTION SY-TCODE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC012                                                    *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM FC012.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC013                                                    *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM FC013.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC014                                                    *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM FC014.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC015                                                    *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM FC015.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC016                                                    *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM FC016.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC017                                                    *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM FC017.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC018                                                    *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM FC018.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC019                                                    *
*---------------------------------------------------------------------*
*       FC019 trifft Vorbereitungen fuers Veraendern eines vorhandenen*
*       LQUA-Satzes. Zunaechst wird gelesen , dann gesperrt, an-      *
*       schliessend erneut gelesen, um Inkonsistenzen zu vermeiden.   *
*---------------------------------------------------------------------*
FORM FC019.
*........Berechtigungspruefung Lagernummer.............................

  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LQUA-LGNUM.

  PERFORM LQUA_LESEN USING LQUA-LGNUM LQUA-LQNUM.
  IF SY-SUBRC NE 0.
    MESSAGE E119 WITH LQUA-LQNUM.      "Lagerquantnummer existiert nicht
  ENDIF.
  IMPORT SAV_TCODE FROM MEMORY ID ANZQU_ID.
*..Lagerplatz sperren..................................................
*..Hinweis: Sollte die TR Quant-Aendern ueber die TR Lagerplatz-Aendern
*.......... gestartet werden, wurde der Platz bereits gesperrt!.......
*           Gleiches gilt für die TR Lagereinheit_aendern
  IF SAV_TCODE <>  CON_TCODE_LS02 AND
     SAV_TCODE <>  CON_TCODE_LS32.
    PERFORM LAGP_SPERREN USING LQUA-LGNUM LQUA-LGTYP LQUA-LGPLA.
  ENDIF.
  PERFORM LQUA_LESEN USING LQUA-LGNUM LQUA-LQNUM.
  IF SY-SUBRC NE 0.
    PERFORM LAGP_ENTSPERREN USING LQUA-LGNUM LQUA-LGTYP LQUA-LGPLA.
*   PERFORM LQUA_INIT.
    MESSAGE E119 WITH LQUA-LQNUM.      "Lagerquantnummer existiert nicht
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC020                                                    *
*---------------------------------------------------------------------*
*       FC020 liest den LQUA-Satz, der angezeigt werden soll.         *
*       Ist er nicht vorhanden, wird eine entsprechende Fehlermeldung *
*       ausgegeben.                                                   *
*---------------------------------------------------------------------*
FORM FC020.
*........Berechtigungspruefung Lagernummer.............................

  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LQUA-LGNUM.

  PERFORM LQUA_LESEN USING LQUA-LGNUM LQUA-LQNUM.
  IF SY-SUBRC NE 0.
    MESSAGE E119 WITH LQUA-LQNUM.      "Lagerquantnummer existiert nicht
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC021                                                    *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM FC021.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC022                                                    *
*---------------------------------------------------------------------*
*       Der aktuelle LQUA-Satz wird in der Datenbank veraendert.      *
*       Zuvor werden die vom System zu setzenden Daten in LQUA ein-   *
*       gestellt.                                                     *
*---------------------------------------------------------------------*
FORM FC022.

*........Berechtigungspruefung Lagernummer.............................

  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LQUA-LGNUM.

  MOVE-CORRESPONDING LQUA TO LQUAV.

  CALL FUNCTION 'L_LQUA_VERAENDERN'
       EXPORTING
            XLQUAV = LQUAV.

  COMMIT WORK.
  MESSAGE S126 WITH LQUAV-LQNUM.       "Lagerquant geändert
  LEAVE.
  LEAVE TO TRANSACTION SY-TCODE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC023                                                    *
*---------------------------------------------------------------------*
*       Der aktuelle Lqua-Satz wird aus der Datenbank geloescht.      *
*       Funktion ist nur im Expertenmodus durch Eingabe von Fcode    *
*       'LO' bei LS22 erreichbar. Vorsicht: Es wird lediglich der     *
*       Quantzähler im Lagerplatz um 1 reduziert. Das Gewicht und     *
*       die Kapazitätsdaten im Lagerplatz werden nicht neu errechnet. *
*       Weitere mögliche negative Konsequenzenen können z.B im        *
*       LE-verwalteten Blocklager oder bei LE-Verwaltung(LEIN)        *
*       auftreten                                                     *
*---------------------------------------------------------------------*
FORM FC023.
*........Berechtigungspruefung Lagernummer.............................

  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LQUA-LGNUM.

  PERFORM LQUA_LOESCHEN.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            TITEL     = TEXT-208
            TEXTLINE1 = TEXT-209
       IMPORTING
            ANSWER    = ANTWORT.

  CLEAR FCODE.
  CASE ANTWORT.
    WHEN ANTWORT_NEIN.
    WHEN ANTWORT_JA.
      CALL FUNCTION 'L_LQUA_LOESCHEN'
           EXPORTING
                XLQUA = LQUA
                XLAGP = LAGP.
      COMMIT WORK.
      MESSAGE S127 WITH LQUA-LGNUM LQUA-LQNUM.    "Lagerquant gelöscht
      PERFORM FC008.
  ENDCASE.
  PERFORM FCODE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC024                                                    *
*---------------------------------------------------------------------*
*       Anzeigen Inventurdaten pro Quant                              *
*---------------------------------------------------------------------*
FORM FC024.
*........Berechtigungspruefung Lagernummer.............................

  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LQUA-LGNUM.

  PERFORM LINV_LESEN USING LQUA-LGNUM
                           LQUA-IVNUM
                           LQUA-IVPOS
                           LQUA-LQNUM
                           INIT_NANUM.
  IF SY-SUBRC =  0.
    PERFORM FC900.
  ELSE.
    MESSAGE I043  .  "Keine Daten zur Inventur vorhanden
    CLEAR FCODE. PERFORM FCODE.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC025                                                    *
*---------------------------------------------------------------------*
*       Anzeigen Zusatzdaten zum Quant                                *
*---------------------------------------------------------------------*
FORM FC025.
  PERFORM FC900.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC026                                                    *
*---------------------------------------------------------------------*
*       Anzeigen Quants pro Lagerplatz                                *
*---------------------------------------------------------------------*
FORM FC026.
  EXPORT RL01S TO MEMORY ID BZPLA_ID.
  SUBMIT RLLS2500 AND RETURN.
  LEAVE.
  LEAVE TO TRANSACTION SY-TCODE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC027                                                    *
*---------------------------------------------------------------------*
*       Anzeigen Quants zum Material                                  *
*---------------------------------------------------------------------*
FORM FC027.
  EXPORT RL01S TO MEMORY ID BZMAT_ID.
  SUBMIT RLLS2400 AND RETURN.
  LEAVE TO TRANSACTION SY-TCODE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC028                                                    *
*---------------------------------------------------------------------*
*       Anzeigen Quants pro Lagerplatz aus Transaktion LS03           *
*---------------------------------------------------------------------*
FORM FC028.

  IF LAGP-ANZQU = 0.
    CLEAR FCODE.
    MESSAGE I120 WITH LAGP-LGPLA.      "Kein Lagerquant zu ... vorhanden
  ELSE.

*........Verzweigen nach Aendern/Anzeigen Quants pro Lagerplatz LS25...

    MOVE: LAGP-LGNUM TO RL01S-LGNUM,
          LAGP-LGTYP TO RL01S-LGTYP,
          LAGP-LGPLA TO RL01S-LGPLA,
          SY-TCODE   TO SAV_TCODE,
          T340-TRTYP TO SAV_TRTYP.

    EXPORT RL01S T342 SAV_TCODE SAV_TRTYP TO MEMORY ID BZPLA_ID.
    SY-TCODE = CON_TRCODE1.
    SUBMIT RLLS2500 AND RETURN.



  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC029                                                    *
*---------------------------------------------------------------------*
*       Anzeigen Lagerbestaende zum Material                          *
*---------------------------------------------------------------------*
FORM FC029.
  EXPORT RL01S TO MEMORY ID BZMAT_ID.

*........Verzweigen nach Lagerbestände zum Material LS26................

  SUBMIT RLLS2600 AND RETURN.
  LEAVE.
  LEAVE TO TRANSACTION SY-TCODE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM FC030                                                    *
*---------------------------------------------------------------------*
*       Maschinelles Anlegen von Lagerplaetzen                        *
*---------------------------------------------------------------------*
FORM FC030.
  EXPORT RL01S TO MEMORY ID MAPLA_ID.

*........Verzweigen ins maschinelle Anlegen der Lagerplätze LS05.......

  SUBMIT RLLS0500 AND RETURN.
  LEAVE.
  LEAVE TO TRANSACTION SY-TCODE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC031                                                    *
*---------------------------------------------------------------------*
*       Sprung ins Anfangsmenü                                        *
*---------------------------------------------------------------------*
FORM FC031.

  MESSAGE W335.

*........Direktsprung aus der lfd.Transaktion ins LVS-Anfangsmenü......

  CALL TRANSACTION CON_MENU.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC032                                                    *
*---------------------------------------------------------------------*
*       Anzeigen Platzaufteilung zum Lagerplatz                       *
*---------------------------------------------------------------------*
FORM FC032.

  IF FCODE = FCODE_PL        AND
     LAGP-PLAUF IS INITIAL  .
    CLEAR FCODE.
    MESSAGE I023.

    PERFORM FCODE.
  ELSE.
    PERFORM STATISTIK_PLATZAUFTEILUNG.
    IF T337A-MAXQU < 9.
*     call screen t342-fdynp starting at 10 10 ending at 75 15.
      CALL SCREEN T342-FDYNP STARTING AT 10 10 ENDING AT 43 23.
    ELSE.
*     call screen t342-fdynp starting at 10 10 ending at 75 15.
      CALL SCREEN T342-FDYNP STARTING AT 10 10 ENDING AT 43 23.
    ENDIF.
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM FC033                                                    *
*---------------------------------------------------------------------*
*       Anzeigen Lagerplatzhistorie                                   *
*---------------------------------------------------------------------*
FORM FC033.

  IF FCODE = FCODE_HI.
    PERFORM FCODE_STACK_PUSH.
    CALL SCREEN 0106 STARTING AT 20 07 ENDING AT 60 17.
    PERFORM FCODE_STACK_POP.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC034                                                    *
*---------------------------------------------------------------------*
*       Reserve                                                       *
*---------------------------------------------------------------------*
FORM FC034.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC035                                                    *
*---------------------------------------------------------------------*
*       Verzweigung zu Window Inventurdaten aus LS22/23               *
*---------------------------------------------------------------------*
FORM FC035.
  PERFORM FCODE_STACK_PUSH.
  CALL SCREEN T342-FDYNP STARTING AT 07 05 ENDING AT 68 14.
  PERFORM FCODE_STACK_POP.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM FC036                                                    *
*---------------------------------------------------------------------*
*       Verzweigung  zu Window Kundendaten aus Ls22/23                *
*---------------------------------------------------------------------*
FORM FC036.
  PERFORM FCODE_STACK_PUSH.
  CALL SCREEN T342-FDYNP STARTING AT 35 04 ENDING AT 71 07.
  PERFORM FCODE_STACK_POP.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM FC037                                                    *
*---------------------------------------------------------------------*
*       Rückrufe aus CALL-SCREEN                                      *
*---------------------------------------------------------------------*
FORM FC037.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM FC038                                                    *
*---------------------------------------------------------------------*
*       Die Transaktion wird ohne POPUP verlassen. Es wird die        *
*       nächst höhere Menustufe angesteuert.                          *
*---------------------------------------------------------------------*
FORM FC038.
  LEAVE.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM FC039                                                    *
*---------------------------------------------------------------------*
*       Funktionstaste F3                                             *
*       Senden Popup "Daten vorher sichern"                           *
*---------------------------------------------------------------------*
FORM FC039.

  PERFORM FC910.
  PERFORM FC920.

ENDFORM.
*---------------------------------------------------------------------*
*.......Lagerplatzverarbeitung........................................*
*       Funktionstaste F15                                            *
*       Senden Popup "Daten vorher sichern"                           *
*---------------------------------------------------------------------*
FORM FC040.

  PERFORM FC910.

  CASE ANTWORT.
    WHEN ANTWORT_NEIN.
      PERFORM FC038.
    WHEN ANTWORT_JA.
      PERFORM FC005.
      PERFORM FC038.
  ENDCASE.
ENDFORM.
*---------------------------------------------------------------------*
*.......Quantverarbeitung.............................................*
*       Funktionstaste F15                                            *
*       Senden Popup "Daten vorher sichern"                           *
*---------------------------------------------------------------------*
FORM FC041.

  PERFORM FC910.

  CASE ANTWORT.
    WHEN ANTWORT_NEIN.
      PERFORM FC038.
    WHEN ANTWORT_JA.
      PERFORM FC011.
      PERFORM FC038.
  ENDCASE.
ENDFORM.

*-----------------------------------------------------------------------
*        FORM FC042
*-----------------------------------------------------------------------
* Überprüfen der Sperrdaten in LEIN und ggf. Sichern
*-----------------------------------------------------------------------
FORM FC042.
  IF LEIN-SKZUA IS INITIAL AND LEIN-SKZUE IS INITIAL.
    IF LEIN-SPGRU NE SPACE.
      MESSAGE E202.                    "Sperrgrund ohne Sperren
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING LEIN TO LEINV.


  CALL FUNCTION 'L_LEIN_VERAENDERN'
       EXPORTING
            XLEINV = LEINV.
  COMMIT WORK.
  MESSAGE S203 WITH LEINV-LENUM.       "Sicherung durchgeführt
ENDFORM.

*-----------------------------------------------------------------------
*        FORM FC043
*-----------------------------------------------------------------------
*       Anzeigen Quants zur Lagereinheit aus Transaktion LS27
*-----------------------------------------------------------------------
FORM FC043.
  EXPORT RL01S TO MEMORY ID BZLEN_ID.
  SUBMIT RLLS2700 AND RETURN.
  LEAVE.
  LEAVE TO TRANSACTION SY-TCODE.
ENDFORM.

*-----------------------------------------------------------------------
*        FORM FC044
*-----------------------------------------------------------------------
*       Anzeigen Quants zur Lagereinheit aus Transaktion LS33         *
*---------------------------------------------------------------------*-
FORM FC044.

  IF LEIN-ANZQU = 0.
    CLEAR FCODE.
    MESSAGE I120 WITH LEIN-LGPLA.      "Kein Lagerquant zu ... vorhanden
  ELSE.
*........Verzweigen nach Aendern/Anzeigen Quants pro Lagerplatz LS27...
    MOVE: LEIN-LGNUM TO RL01S-LGNUM,
          LEIN-LGTYP TO RL01S-LGTYP,
          LEIN-LGPLA TO RL01S-LGPLA,
          LEIN-LENUM TO RL01S-LENUM,
          SY-TCODE   TO SAV_TCODE,
          T340-TRTYP TO SAV_TRTYP.

    EXPORT RL01S T342 SAV_TCODE SAV_TRTYP TO MEMORY ID BZLEN_ID.
    SY-TCODE = CON_TCODE_LS27.
    SUBMIT RLLS2700 AND RETURN.
  ENDIF.

ENDFORM.

*-----------------------------------------------------------------------
*        FORM FC045
*-----------------------------------------------------------------------
*  Anzeigen der Lagerplatzdaten zur Lagereinheit
*-----------------------------------------------------------------------
FORM FC045.

  SET PARAMETER: ID 'LGN' FIELD LEIN-LGNUM,
                 ID 'LGT' FIELD LEIN-LGTYP,
                 ID 'LGP' FIELD LEIN-LGPLA.
***  CALL TRANSACTION CON_TCODE_LS03 AND SKIP FIRST SCREEN.
  CALL TRANSACTION CON_TCODE_LS03N AND SKIP FIRST SCREEN.

ENDFORM.

*-----------------------------------------------------------------------
*        FORM FC046
*-----------------------------------------------------------------------
*  Anzeigen der Inventurdaten zur Lagereinheit
*-----------------------------------------------------------------------
FORM FC046.

  SET PARAMETER: ID 'LGN' FIELD LEIN-LGNUM,
                 ID 'LGT' FIELD LEIN-LGTYP,
                 ID 'LGP' FIELD LEIN-LGPLA.
  CALL TRANSACTION CON_TCODE_LI05 AND SKIP FIRST SCREEN.

ENDFORM.

*-----------------------------------------------------------------------
*        FORM FC047
*-----------------------------------------------------------------------
*       FC047 trifft Vorbereitungen fuers Veraendern der Sperrdaten    *
*       zur Lagereinheit. Während dem Vorgang wird der zugehörige      *
*       Lagerplatz gesperrt.                                           *
*----------------------------------------------------------------------*
FORM FC047.

*........Berechtigungspruefung Lagernummer.............................

* PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
*         USING CON_BER_MP LEIN-LGNUM.

*........Lagerplatz sperren............................................
  HLP_LGNUM = LEIN-LGNUM.
  HLP_LGTYP = LEIN-LGTYP.
  HLP_LGPLA = LEIN-LGPLA.
  DO.
*   Hinweis: Lagerplatz nur sperren, wenn nicht schon gesperrt!
*   Sperrung erfolgte schon, wenn Sprung aus TR Lagerplatz_ändern

    IF SAV_TCODE <> CON_TCODE_LS02.
      PERFORM LAGP_SPERREN USING LEIN-LGNUM LEIN-LGTYP LEIN-LGPLA.
    ENDIF.
    PERFORM LEIN_LESEN USING LEIN-LENUM.
    IF SY-SUBRC NE 0.
      PERFORM LAGP_ENTSPERREN USING HLP_LGNUM HLP_LGTYP HLP_LGPLA.
      MESSAGE E007 WITH LEIN-LGPLA.    "Lagerplatz nicht vorhanden
    ENDIF.
    IF HLP_LGNUM = LEIN-LGNUM AND
       HLP_LGTYP = LEIN-LGTYP AND
       HLP_LGPLA = LEIN-LGPLA.
      EXIT.
    ELSE.
      PERFORM LAGP_ENTSPERREN USING HLP_LGNUM HLP_LGTYP HLP_LGPLA.
      HLP_LGNUM = LEIN-LGNUM.
      HLP_LGTYP = LEIN-LGTYP.
      HLP_LGPLA = LEIN-LGPLA.
    ENDIF.
  ENDDO.
ENDFORM.

*-----------------------------------------------------------------------
*        FORM FC048
*-----------------------------------------------------------------------
*       Anzeigen Lagereinheiten zum Platz aus Transaktion LS28
*-----------------------------------------------------------------------
FORM FC048.
  EXPORT RL01S TO MEMORY ID ANZLE_ID.
  SUBMIT RLLS2800 AND RETURN.
  LEAVE.
  LEAVE TO TRANSACTION SY-TCODE.
ENDFORM.

*-----------------------------------------------------------------------
*        FORM FC049
*-----------------------------------------------------------------------
*       Anzeigen Lagereinheiten zum Platz aus Transaktion LS03
*-----------------------------------------------------------------------
FORM FC049.

  IF LAGP-ANZLE = 0.
    CLEAR FCODE.
    MESSAGE I204 WITH LAGP-LGPLA.
  ELSE.
*-------Verzweigen nach LS28 Anzeigen/Ändern LE's-----------------------
    MOVE: LAGP-LGNUM TO RL01S-LGNUM,
          LAGP-LGTYP TO RL01S-LGTYP,
          LAGP-LGPLA TO RL01S-LGPLA,
          SY-TCODE   TO SAV_TCODE,
          T340-TRTYP TO SAV_TRTYP.

    EXPORT RL01S T342 SAV_TCODE SAV_TRTYP TO MEMORY ID ANZLE_ID.
    SY-TCODE = CON_TCODE_LS28.
    SUBMIT RLLS2800 AND RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FC050
*&---------------------------------------------------------------------*
*&      Anzeigen Charge zu Quant
*&---------------------------------------------------------------------*
FORM FC050.
  IF LQUA-CHARG IS INITIAL.
*.......Material ist nicht chargenpflichtig --> Fehlermeldung...........
    MESSAGE S025(L2).
*   Material ist nicht chargenpflichtig
  ELSE.
*.......Material ist chargenpflichtig --> Anzeigen der Charge...........
    SET PARAMETER: ID PARID_MATNR FIELD LQUA-MATNR,
                   ID PARID_WERKS FIELD LQUA-WERKS,
                   ID PARID_CHARG FIELD LQUA-CHARG.
    CALL TRANSACTION CON_TCODE_MSC3N aND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                                                    " FC050
*---------------------------------------------------------------------*
*      Form  FC051                                                    *
*---------------------------------------------------------------------*
*      Anzeigen Bestände zur Platzposition                            *
*---------------------------------------------------------------------*
FORM FC051.

*........Bestände zum Lagerplatz.......................................

  MOVE SAV_FCODE+2 TO HLP_PLPOS.
  CONCATENATE 'T337A-POS' HLP_PLPOS INTO HLP_FIELDNAME.

  ASSIGN (HLP_FIELDNAME) TO <FELDNAME>.

  SELECT * FROM LQUA WHERE LGNUM = LAGP-LGNUM
                       AND LGTYP = LAGP-LGTYP
                       AND LGPLA = LAGP-LGPLA
                       AND PLPOS = <FELDNAME>.
  ENDSELECT.
  IF SY-SUBRC EQ 0.
    IF LQUA-LENUM IS INITIAL.
*........Quantanzeige (Einzelquant)....................................
      SET PARAMETER:  ID 'LGN' FIELD LQUA-LGNUM,
                      ID 'LQN' FIELD LQUA-LQNUM.
      CALL TRANSACTION CON_TCODE_LS23 AND SKIP FIRST SCREEN.
    ELSE.
*........Bestand zur Lagereinheit......................................
      SET PARAMETER:  ID 'LEN' FIELD LQUA-LENUM.
      CALL TRANSACTION CON_TCODE_LS27 AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                                                    " FC051
*---------------------------------------------------------------------*
*       FORM FC052                                                    *
*---------------------------------------------------------------------*
*       Anzeigen Bestand auf HU                                       *
*---------------------------------------------------------------------*
FORM FC052.

  DATA: BEGIN OF I_HU OCCURS 0,
        LENUM LIKE VEKP-EXIDV,
        END OF I_HU.

  MOVE LEIN-LENUM TO I_HU-LENUM.
  APPEND I_HU.

  CALL FUNCTION 'HU_DISPLAY'
       EXPORTING
*         IF_DONT_FILL = ' '
            IT_HUS       = I_HU[]
*         IT_OBJECTS   =
*         IT_VENUM     =
*    IMPORTING
*         EF_FCODE     =
       EXCEPTIONS
            NO_HUS_FOUND = 1
            FATAL_ERROR  = 2
            OTHERS       = 3.

  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE E616 WITH LEIN-LENUM.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       Vorbereiten Aufruf des Call-Dynpros                           *
*---------------------------------------------------------------------*

FORM FC900.
  PERFORM FCODE_STACK_PUSH.
  MOVE T342-FCCAL TO FCODE.
  PERFORM FCODE.
  PERFORM FCODE_STACK_POP.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM FC910                                                    *
*---------------------------------------------------------------------*
*       Senden Popup "Daten vorher sichern"                           *
*---------------------------------------------------------------------*
FORM FC910.

  IF SAV_DATAR IS INITIAL AND
     SY-DATAR IS INITIAL.
    ANTWORT = ANTWORT_NEIN.
  ELSE.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
         EXPORTING
              TITEL     = TEXT-100
              TEXTLINE1 = TEXT-200
              TEXTLINE2 = TEXT-201
         IMPORTING
              ANSWER    = ANTWORT.

    IF NOT ANTWORT = ANTWORT_ABBRECHEN.
      CLEAR SAV_DATAR.
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM FC920                                                    *
*---------------------------------------------------------------------*
*       Verarbeitung nach Rückkehr aus POPUP                          *
*       Antwort nein: Wiederaufruf der Transaktion bzw. Rücksprung    *
*       Antwort ja  : Buchen (Feldprüfungen schon durchlaufen)        *
*---------------------------------------------------------------------*
FORM FC920.
  CLEAR FCODE.
  CASE ANTWORT.
    WHEN ANTWORT_NEIN.
      LEAVE.
      LEAVE TO TRANSACTION SY-TCODE.
    WHEN ANTWORT_JA.
      FCODE = FCODE_BU.
      PERFORM FCODE.
  ENDCASE.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM FC925                                                    *
*---------------------------------------------------------------------*
*       Verarbeitung nach Rückkehr aus POPUP                          *
*       Antwort nein: Sprung in höhere Menu-Stufe                     *
*       Antwort ja  : Buchen                                          *
*---------------------------------------------------------------------*
FORM FC925.

  CASE ANTWORT.
    WHEN ANTWORT_NEIN.
      PERFORM FC038.
    WHEN ANTWORT_JA.
      FCODE = FCODE_BU.
      PERFORM FCODE.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC930                                                    *
*---------------------------------------------------------------------*
*       Senden Popup "Daten werden verloren gehen"  (F12)             *
*---------------------------------------------------------------------*
FORM FC930.

  IF SAV_DATAR IS INITIAL AND
     SY-DATAR IS INITIAL.
    ANTWORT = ANTWORT_JA.
  ELSE.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
         EXPORTING
              DEFAULTOPTION = 'N'
              TITEL         = TEXT-101
              TEXTLINE1     = TEXT-202
              TEXTLINE2     = TEXT-203
         IMPORTING
              ANSWER        = ANTWORT.
    IF ANTWORT = ANTWORT_JA.
      CLEAR SAV_DATAR.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC935                                                    *
*---------------------------------------------------------------------*
*       Verarbeitung nach Rückkehr aus POPUP (AT-EXIT-COMMAND)        *
*       Antwort ja  : Rücksprung ohne sichern                      *
*---------------------------------------------------------------------*
FORM FC935.
  CLEAR FCODE.
  CASE ANTWORT.
    WHEN ANTWORT_JA.
      LEAVE.
      LEAVE TO TRANSACTION SY-TCODE.   "(Rück-)Sprung ohne sichern
    WHEN ANTWORT_NEIN.
    WHEN ANTWORT_ABBRECHEN.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC938                                                    *
*---------------------------------------------------------------------*
*       Senden Popup "Daten werden verloren gehen"  (F15)             *
*---------------------------------------------------------------------*
FORM FC938.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            DEFAULTOPTION = 'N'
            TITEL         = TEXT-100
            TEXTLINE1     = TEXT-212
            TEXTLINE2     = TEXT-213
       IMPORTING
            ANSWER        = ANTWORT.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FC940                                                    *
*---------------------------------------------------------------------*
*       Verarbeitung nach Rückkehr aus POPUP (AT-EXIT-COMMAND)        *
*       Antwort ja  : Sprung ohne sichern  (1 Ebene)                  *
*---------------------------------------------------------------------*
FORM FC940.
  CLEAR FCODE.
  CASE ANTWORT.
    WHEN ANTWORT_JA.
      LEAVE TO TRANSACTION SPACE.      "Rücksprung total
    WHEN ANTWORT_NEIN.
    WHEN ANTWORT_ABBRECHEN.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FC901
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FC901 using p_tabix p_tabix1 p_loopzeilen p_tabname value(p_input).

data: merk_tabix1 like sy-tabix.
data: hlp_tabix like sy-tabix.

  flg_keine_dynpros = con_false.

  case fcode.

*........Anzeigen erste Seite...........................................

    when fcode_anfang.
      p_tabix1 = 1.

*........Anzeige nächste Seite..........................................

    when fcode_vor.

      if p_input = con_false.
        p_tabix = p_tabix1 + p_loopzeilen.
        case p_tabname.
          when con_tab_ilqua400.
            read table ilqua400 index p_tabix.
          when con_tab_ilinv400.
            read table ilinv400 index p_tabix.
        endcase.
        if sy-subrc = 0.
          p_tabix1 = p_tabix.
        endif.

      else. "p_input = con_false

*........Merken des aktuellen TABIX1-Wertes.............................

        merk_tabix1 = p_tabix1.

*........Ganz normales Blättern.........................................

        perform fc901 using p_tabix
                            p_tabix1
                            p_loopzeilen
                            p_tabname
                            con_false.

*........Wenn sich der TABIX1 nicht geändert hat ==> es wurde nicht
*........geblättert ==> letzte Seite

        if merk_tabix1 = p_tabix1.
          hlp_tabix = p_tabix1 + p_loopzeilen - 1.

*........Prüfen, ob der letzten Zeile auf der letzten Seite ein
*........Tabellenwert zugeordnet ist (wenn nein ==> keine eingabe-
*........bereiten Zeilen)...............................................
          case p_tabname.
            when con_tab_ilqua400.
              read table ilqua400 index hlp_tabix.
            when con_tab_ilinv400.
              read table ilinv400 index hlp_tabix.
          endcase.

          if sy-subrc = 0.

*.........Prüfen, ob der ersten Zeile ein Tabellenwert zugeordnet ist
*.........wenn nein ==> man befindet sich schon auf der letzten Seite,
*.........auf der alle Zeilen eingabebereit sind........................
           case p_tabname.
             when con_tab_ilqua400.
               read table ilqua400 index p_tabix1.
             when con_tab_ilinv400.
               read table ilinv400 index p_tabix1.
           endcase.

           if sy-subrc = 0.
             p_tabix1 = p_tabix1 + p_loopzeilen.
           endif.
         endif.
       endif. "merk_tabix1 = p_tabix1

     endif. "p_input = con_false

*........Anzeigen vorige Seite..........................................
    when fcode_zurueck.
      p_tabix = p_tabix1 - p_loopzeilen.
      if p_tabix >= 1.
        p_tabix1 = p_tabix.
      else.
        p_tabix1 = 1.
      endif.

*........Anzeigen letzte Seite..........................................

    when fcode_ende.
      p_tabix = 1.
      sy-subrc = 0.
      while sy-subrc = 0.
        p_tabix1 = p_tabix.
        p_tabix  = p_tabix + p_loopzeilen.
        case p_tabname.
          when con_tab_ilqua400.
            read table ilqua400 index p_tabix.
          when con_tab_ilinv400.
            read table ilinv400 index p_tabix.
       endcase.
     endwhile.

*........Anzeigen gleiche Seite.........................................

   when others.
     p_tabix = p_tabix1.

 endcase.


ENDFORM.                    " FC901

*&---------------------------------------------------------------------*
*&      Form  FC053
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FC053.

* check if bin sectioning is valid (e-message)
  if fcode = fcode_auft.
    if not lagp-plauf is initial.
* Division of storage bins into sections
      perform T337A_LESEN.
    else.
      clear t337A.
    endif.
  endif.

  FUNC_TABSTRIP-ACTIVETAB = FCODE.

*.........An Anfang blättern.................

  FCODE = FCODE_ANFANG.
*** PERFORM FC052.

ENDFORM.                    " FC053
*&---------------------------------------------------------------------*
*&      Form  FC054
*&---------------------------------------------------------------------*
*       Branch to display quant data LS23
*----------------------------------------------------------------------*
FORM FC054.
data: anzahl_sel type i value 0.

  PERFORM BERECHTIGUNG_TCODE(SAPFL000) USING CON_LS23_ANZEIGEN.

  loop at ilqua400 where not kreuz is initial.
    add 1 to anzahl_sel.
    if not ilqua400-lqnum is initial.
      SET PARAMETER:  ID PARID_LGNUM FIELD LAGP-LGNUM,
                      ID PARID_LQNUM FIELD ILQUA400-LQNUM.

      CALL TRANSACTION CON_LS23_ANZEIGEN AND SKIP FIRST SCREEN.
    endif.
  endloop.

  if anzahl_sel eq 0.
    message s159.
  else.
    loop at ilqua400 where not kreuz is initial.
      clear ilqua400-kreuz.
      modify ilqua400.
    endloop.
  endif.

ENDFORM.                    " FC054
*&---------------------------------------------------------------------*
*&      Form  FC055
*&---------------------------------------------------------------------*
*       Branch to display storage unit data (single or list/report)
*----------------------------------------------------------------------*
FORM FC055.

DATA: BEGIN OF TAB OCCURS 10.
        INCLUDE STRUCTURE LQUA.
DATA: END OF TAB.

DATA: FLG_LE_REIN(1)      TYPE C,
      ZAEHLER             TYPE P,
      ANZAHL_SEL          TYPE P.

  SELECT * FROM LQUA
       WHERE LGNUM = LAGP-LGNUM
         AND LGTYP = LAGP-LGTYP
         AND LGPLA = LAGP-LGPLA.

    MOVE-CORRESPONDING LQUA TO TAB.
    APPEND TAB.
    ADD 1 TO ZAEHLER.

  ENDSELECT.

  IF ZAEHLER = 0.
    MESSAGE S120 WITH LAGP-LGPLA.      "No Quant for $ exist
    EXIT.
  ENDIF.

  SORT TAB BY LENUM.

  FLG_LE_REIN = CON_X.
  LOOP AT TAB.
    ON CHANGE OF TAB-LENUM.
      IF SY-TABIX > 1.
        CLEAR FLG_LE_REIN.
      ENDIF.
    ENDON.
  ENDLOOP.
  IF FLG_LE_REIN = CON_X AND
       TAB-LENUM IS INITIAL.
    CLEAR FLG_LE_REIN.
  ELSE.
    MOVE TAB-LENUM TO RL01S-LENUM.
  ENDIF.

  IF FLG_LE_REIN = CON_X.
    PERFORM BERECHTIGUNG_TCODE(SAPFL000) USING CON_LS33_ANZEIGEN.
    SET PARAMETER:  ID PARID_LENUM FIELD TAB-LENUM.
    CALL TRANSACTION CON_LS33_ANZEIGEN AND SKIP FIRST SCREEN.
  ELSE.
    loop at ilqua400 where not kreuz is initial.
      add 1 to anzahl_sel.
      IF ILQUA400-LENUM IS INITIAL.
        MESSAGE I205.              "Keine Lagereinheit zum Quant
      ELSE.
        PERFORM BERECHTIGUNG_TCODE(SAPFL000) USING CON_LS33_ANZEIGEN.
        SET PARAMETER:  ID PARID_LENUM FIELD ILQUA400-LENUM.
        CALL TRANSACTION CON_LS33_ANZEIGEN AND SKIP FIRST SCREEN.
      ENDIF.
    endloop.
    if anzahl_sel eq 0.
*-------Branch to LS28 Display mode (only if no line is selected)
      MOVE: LAGP-LGNUM TO RL01S-LGNUM,
            LAGP-LGTYP TO RL01S-LGTYP,
            LAGP-LGPLA TO RL01S-LGPLA,
            SY-TCODE   TO SAV_TCODE.
      EXPORT RL01S T342 SAV_TCODE TO MEMORY ID ANZLE_ID.
      SY-TCODE = CON_TCODE_LS28.
      SUBMIT RLLS2800 AND RETURN.
    ENDIF.
  ENDIF.

  loop at ilqua400 where not kreuz is initial.
    clear ilqua400-kreuz.
    modify ilqua400.
  endloop.

ENDFORM.                    " FC055
*&---------------------------------------------------------------------*
*&      Form  FC056
*&---------------------------------------------------------------------*
*       Data refresh on dynpro D0400
*----------------------------------------------------------------------*
FORM FC056.

  clear fcode.
  clear bin_is_checked.
  perform d0400_daten.

ENDFORM.                    " FC056
*&---------------------------------------------------------------------*
*&      Form  FC057
*&---------------------------------------------------------------------*
*       Sort inventory data ascending by selected column
*----------------------------------------------------------------------*
FORM FC057.
data: inttab type cxtab_column.
data: f5(50), f6(50).
data: init_tab(50).

  move 'LINV' to init_tab.

  loop at d4003-cols into inttab.
    IF INTTAB-SELECTED EQ 'X'.
      SPLIT INTTAB-SCREEN-NAME AT '-' INTO F5 F6.
      if f5 ne init_tab.
        message s046.
        exit.
      endif.
      SORT ilinv400 ascending BY (F6).

      clear inttab-selected.
      modify d4003-cols from inttab.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " FC057
*&---------------------------------------------------------------------*
*&      Form  FC058
*&---------------------------------------------------------------------*
*       Sort inventory data descending by selected column
*----------------------------------------------------------------------*
FORM FC058.

data: inttab type cxtab_column.
data: f5(50), f6(50).
data: init_tab(50).

  move 'LINV' to init_tab.

  loop at d4003-cols into inttab.
    IF INTTAB-SELECTED EQ 'X'.
      SPLIT INTTAB-SCREEN-NAME AT '-' INTO F5 F6.
      if f5 ne init_tab.
        message s046.
        exit.
      endif.
      SORT ilinv400 descending BY (F6).

      clear inttab-selected.
      modify d4003-cols from inttab.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " FC058
*&---------------------------------------------------------------------*
*&      Form  FC059
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FC059.

  IF FCODE = FCODE_OK4006 OR FCODE = FCODE_ESC4006.
    clear fcode.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDIF.

  clear fcode.

ENDFORM.                    " FC059
*&---------------------------------------------------------------------*
*&      Form  FC060
*&---------------------------------------------------------------------*
*       Paging of table control in Dynpro 4003 (Inventory data)
*----------------------------------------------------------------------*
FORM FC060.

  if fcode eq fcode_fpi.
    move fcode_first to fcode.
  endif.
  if fcode eq fcode_upi.
    move fcode_prev to fcode.
  endif.
  if fcode eq fcode_dpi.
    move fcode_next to fcode.
  endif.
  if fcode eq fcode_lpi.
    move fcode_last to fcode.
  endif.

  perform fc901 using reset2-lfdps
                      reset2-seite
                      d4003_zeilen
                      con_tab_ilinv400
                      con_true.

  d4003-top_line = reset2-seite.

ENDFORM.                    " FC060
*&---------------------------------------------------------------------*
*&      Form  FC061
*&---------------------------------------------------------------------*
*  Extract or include data of recount numbers in inventory data display
*----------------------------------------------------------------------*
FORM FC061.

  if not d4003_kz_zahl is initial.
    clear d4003_kz_zahl.
  else.
    move 'X' to d4003_kz_zahl.
  endif.

  refresh ilinv400.
  perform ilinv400_fuellen.

ENDFORM.                    " FC061
*&---------------------------------------------------------------------*
*&      Form  FC062
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FC062.

  leave to transaction con_tcode_ls01n.

ENDFORM.                    " FC062
*&---------------------------------------------------------------------*
*&      Form  FC063
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FC063.

  leave to transaction con_tcode_ls02n.

ENDFORM.                    " FC063
*&---------------------------------------------------------------------*
*&      Form  FC064
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FC064.

  leave to transaction con_tcode_ls03n.

ENDFORM.                    " FC064
*&---------------------------------------------------------------------*
*&      Form  FC065
*&---------------------------------------------------------------------*
*       Aufruf Verbucher / Datenwechsel
*----------------------------------------------------------------------*
FORM FC065.

  if fcode is initial.
    perform d0400_daten.
  endif.

  if fcode eq fcode_bu.
    clear bin_is_checked.
    if t340-trtyp eq con_hinzufuegen.
      perform fc005.
      clear neu_lgpla.
      perform fc008.
    endif.
    if t340-trtyp eq con_veraendern.
      perform fc006.
      perform fc008.
    endif.
  endif.

  if fcode eq fcode_lo.
    clear bin_is_checked.
    clear neu_lgpla.
    clear fcode.
    perform fc007.
  endif.

ENDFORM.                    " FC065
*&---------------------------------------------------------------------*
*&      Form  FC066
*&---------------------------------------------------------------------*
*       Scroll on Dynpro / Subscreen 4002
*----------------------------------------------------------------------*
FORM FC066.

  if fcode eq fcode_fp.
    move fcode_first to fcode.
  endif.
  if fcode eq fcode_up.
    move fcode_prev to fcode.
  endif.
  if fcode eq fcode_dp.
    move fcode_next to fcode.
  endif.
  if fcode eq fcode_lp.
    move fcode_last to fcode.
  endif.

  perform fc901 using reset-lfdps
                      reset-seite
                      d4002_zeilen
                      con_tab_ilqua400
                      con_true.

  d4002-top_line = reset-seite.

  clear fcode.

ENDFORM.                    " FC066
*&---------------------------------------------------------------------*
*&      Form  FC067
*&---------------------------------------------------------------------*
*       Same as FC002; except for message type W instead of E
*       FC002 trifft Vorbereitungen fuers Hinzufuegen eines neuen     *
*       LAGP-Satzes. Zunaechst wird gesperrt, dann gelesen. Der Satz  *
*       darf noch nicht vorhanden sein, sonst wird entsperrt, das     *
*       Satzbett neu initialisiert und der Fehler gemeldet.           *
*----------------------------------------------------------------------*
FORM FC067.

*........Berechtigungspruefung Lagernummer.............................
  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LAGP-LGNUM.

*........Plausibilitaetspruefung Lagerplatz............................
  PERFORM PRUEFUNG_LGPLA USING LAGP-LGPLA.

*........Lagerplatz sperren............................................
  PERFORM LAGP_SPERREN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
  PERFORM LAGP_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
  IF SY-SUBRC EQ 0.
    PERFORM LAGP_ENTSPERREN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
    PERFORM LAGP_INIT.
    d0400_record_exist = con_x.
    perform lock_save_button_d0400.
    MESSAGE W008 WITH LAGP-LGPLA.      "Lagerplatz bereits vorhanden
  else.
    clear d0400_record_exist.
  ENDIF.

ENDFORM.                    " FC067
*&---------------------------------------------------------------------*
*&      Form  FC068
*&---------------------------------------------------------------------*
*       Same as FC003; except for message type W instead of E
*       FC003 trifft Vorbereitungen fuers Veraendern eines vorhandenen*
*       LAGP-Satzes. Zunaechst wird gesperrt, dann gelesen. Der Satz  *
*       vorhanden sein, sonst wird entsperrt und der Fehler gemeldet. *
*----------------------------------------------------------------------*
FORM FC068.

*........Berechtigungspruefung Lagernummer.............................
  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LAGP-LGNUM.

*........Lagerplatz sperren............................................

* Unabhängig von der Sperrmimik wird beim Ändern Lagerplatz immer eine
* eine Lquax-Sperre abgesetzt um paralelle TA's und Inventur zu verhin-
* dern
  PERFORM LAGP_SPERREN_NEU USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
  PERFORM LAGP_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
  IF SY-SUBRC NE 0.
    bin_is_wrong = con_x.
    PERFORM LAGP_ENTSPERREN_NEU USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
    MESSAGE W007 WITH LAGP-LGPLA.      "Lagerplatz nicht vorhanden
  ELSE.
    clear bin_is_wrong.
    RL01S-BKAPV = LAGP-LKAPV - LAGP-RKAPV. "Belegte Kapazität ermittel
  ENDIF.

ENDFORM.                    " FC068
*&---------------------------------------------------------------------*
*&      Form  FC069
*&---------------------------------------------------------------------*
*       Same as FC004; except for message type W instead of E
*       FC004 liest den LAGP-Satz, der angezeigt werden soll.         *
*       Ist er nicht vorhanden, wird eine entsprechende Fehlermeldung *
*       ausgegeben.                                                   *
*----------------------------------------------------------------------*
FORM FC069.

*........Berechtigungspruefung Lagernummer.............................
  PERFORM BERECHTIGUNG_LGNUM(SAPFL000)
          USING CON_BER_MP LAGP-LGNUM.

  PERFORM LAGP_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGPLA.
  IF SY-SUBRC NE 0.
    MESSAGE W007 WITH LAGP-LGPLA.      "Lagerplatz nicht vorhanden
  ELSE.
    RL01S-BKAPV = LAGP-LKAPV - LAGP-RKAPV. "Belegte Kapazität ermitteln
  ENDIF.

ENDFORM.                    " FC069
