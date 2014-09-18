*---------------------------------------------------------------------*
*       FORM CHECK-COLLECTIVITY                                       *
*---------------------------------------------------------------------*
*  Verprobung, ob System als Zentrales System mit nur einem           *
*  Applikationsserver betrachtet werden kann                          *
*                                                                     *
*  Das ist der Fall, wenn                                             *
*  in der Statistik nur ein Server aufgetaucht ist und                *
*  - Die Beobachtung aber mindestens seit 14 Tagen aktiv ist          *
*  - Der Server mindestens vor 4 Tagen in der Stat. noch aktiv auft.  *
*                                                                     *
*  oder                                                               *
*  in der Statistik mehrere Server bekannt sind und                   *
*  - Seit SLEEPDAY Tagen hoechst. 1 Server in der Statistik auftaucht *
*                                                                     *
*  Ist obiges erfuellt, wird der TOTAL Modus der Statistik in         *
*  Schlafzustand versetzt. Dann und nur dann.                         *
*---------------------------------------------------------------------*
FORM CHECK-COLLECTIVITY.
  DATA: HLPDAY LIKE SY-DATUM,
        SLEEPDAY LIKE SY-INDEX,
        DAYCOUNT LIKE SY-INDEX.

  SLEEPDAY = 14.                       "Anzahl Tage
*
*--Wird Pflege eines TOTAL Servers ueberhaupt verlangt ?
  IF STAPAR-COLLECTIVEFLAG = 'N'.
    STAPAR-COLLECTIVEFLAG = ' '.
  ENDIF.

  CHECK STAPAR-COLLECTIVEFLAG <> ' '.  "TOTAL Server soll gepflegt wrd.
*
*--Logbuch nach in der Statistik aktiven Servern absuchen
  DESCRIBE TABLE TOTALLOG LINES LINES.
  CHECK LINES > 0.                     "Kein Servereintrag in Logbuch
                                       "nichts unternehmen !

  IF LINES = 1.                        "Nur ein Server eingetragen

    HLPDAY = TOTALLOG-FIRSTSYSTEMDAY.
    ADD 14 TO HLPDAY.                  "Schon mindestens 14 Tage
    IF HLPDAY > SY-DATUM.              "in Statistik aktiv ?
      IF STAPAR-COLLECTIVEFLAG <> 'X'. "Nein! Dann TOTAL Server pflegen.
        STAPAR-COLLECTIVEFLAG = 'X'.
        PERFORM ACTIVATE_STAPAR_2.     "STAPAR in MONI wegschreiben
        EXIT.
      ENDIF.
    ELSE.                              "Ja!
      HLPDAY = TOTALLOG-LASTSYSTEMDAY.
      ADD 4 TO HLPDAY.                 "War Statistik in letzt.4 Tagen
                                       "gepflegt ?
      IF HLPDAY <  SY-DATUM.           "Wenn nein, nichts machen!
        EXIT.
      ELSE.                            "Ja! Statistik ist gepflegt!
        IF STAPAR-COLLECTIVEFLAG <> 'S'.
          STAPAR-COLLECTIVEFLAG = 'S'. "Schlafzustand setzen.
          PERFORM ACTIVATE_STAPAR_2.   "STAPAR in MONI wegschreiben
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
*
*--Mehrere Server waren schon im System aktiv. Sind sie noch aktiv ?
  ELSE.                                "Mehrere Server im System

    CLEAR: LINES, DAYCOUNT.
    LOOP AT TOTALLOG.

      HLPDAY = TOTALLOG-LASTSYSTEMDAY.
      ADD SLEEPDAY TO HLPDAY.          "Server war vor xx Tagen im Sy.
      IF HLPDAY >= SY-DATUM.
        ADD 1 TO DAYCOUNT.             "Kurezlich noch aktiv !
      ENDIF.

      HLPDAY = TOTALLOG-LASTSYSTEMDAY.
      ADD 1 TO HLPDAY.                 "Statistik war gestern gepflegt?
      CHECK HLPDAY >= SY-DATUM.        "Wenn nein, unber. lassen !
      ADD 1 TO LINES.                  "Gestern noch aktiv !
    ENDLOOP.

    IF LINES > 1.                      ">1 Server seit gest. im System
      IF STAPAR-COLLECTIVEFLAG <> 'X'. "Schlafzustand aufheben !
        STAPAR-COLLECTIVEFLAG = 'X'.
        PERFORM ACTIVATE_STAPAR_2.     "STAPAR in MONI wegschreiben
      ENDIF.
    ENDIF.

    IF DAYCOUNT <= 1.                  "Nur Server seit 14 Tg im System
      IF STAPAR-COLLECTIVEFLAG <> 'S'. "Schlafzustand beginnen.
        STAPAR-COLLECTIVEFLAG = 'S'.
        PERFORM ACTIVATE_STAPAR_2.     "STAPAR in MONI wegschreiben
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.
