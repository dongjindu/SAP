* RKEREO31
* Reorganisation der Objektebene aus den Einzelposten
* Siehe OSS 93051
*
* Underungshistorie
* =================
*
***********************************************************************
*                                                                     *
*  Main program to rebuild CO-PA segment level from line items.       *
*                                                                     *
*  Sequence number 57 released Sep 23, 1999                           *
*  Suitable for R/3 systems of releases 3.0 - 4.6                     *
*                                                                     *
***********************************************************************
*
* 09.08.99 4.X         Aufw?tskompatible Behandlung von REC_WAERS
*                      -> f? 4.X die Vorlage rkevrk2e_4X.dnl
*                         verwenden.
* 22.12.98 Projekt     Hinzuf?en Selektion der Vorgangsart
*                      dh981222
* 26.03.97 Experiment. Benachrichtigung ?er Fehler im Kasten ganz
*                      unten im Protokoll (sonstige RC's noch nicht
*                      eliminiert).
*                      Laufzeit / 100' S?ze ausgewiesen.
* 17.03.97             Fehlerkorrektur delete_ce3:
*                      Returncode DELETE ignorieren
* 13.03.97 22* 31H 40A Alternativentwicklung
*                      Scan ?er CE1, schreiben Datafile CE3
*                      Einschr?kung: Temp-Platz f? sortieren CE1
*                                     Max. Gr?e Datafile CE3 <=2GB
*                      F? Release 2.2 nur via Hinweis erh?tlich.
*                      Release 2.1 wird nicht unterst?zt.
* ---------------------------------------------------------------------
* 12.01.96     22H 30C Anzahl Ausgaben im Standard-Fall verringert
* 04.01.96     22H 30C Version f? Auslieferung zusammengestellt
* ---------------------------------------------------------------------
* 14.07.95 21G 22F     Performance-Verbesserung f? ORACLE
* ---------------------------------------------------------------------
* 19.05.95 21F 22E     viele PARAMETERS auf NO-DISPLAY gesetzt,
*                      damit der User nicht komplett verwirrt wird.
*                      Reorganisation auch f? Plandaten eingebaut
* 18.05.95 21F         Portierung auf 2.1
* ---------------------------------------------------------------------
*    04.95     22C 30A Neuentwicklung aus Performancegr?den

REPORT RKEREO31 LINE-SIZE 120.

************************************************************************
*
*   Selektionsbild
*
*   VERBOSE    ausf?rliche Protokollierung           RKEVRK2E
*   LOCKCE3    Verbuchersperre EKC3xxxx setzen        RKEREO31
*   SINGLDEL   CE3 mit einem Statement l?chen        RKEVRK2E
*   GENONLY    nur Generieren                         RKEREO31
*   TESTMODE   Testmodus ohne L?chen/Schreiben CE3   RKEVRK2E
*   RAMAVAIL   nutzbarer Speicherplatz in MB          RKEVRK2E

PARAMETERS:
    OP_CO        LIKE TKEBL-ERKRS MEMORY ID ERB OBLIGATORY
  , F_YEAR       LIKE CEST1-GJAHR               OBLIGATORY
  , PLIKZ        LIKE CEST3-PLIKZ               OBLIGATORY
  , VRGAR        LIKE CEST3-VRGAR
  , TEMPFILE(80) TYPE C           LOWER CASE    OBLIGATORY
  , RAMAVAIL     TYPE I DEFAULT 50 NO-DISPLAY     " nicht unterst?zt
  .
SELECTION-SCREEN ULINE.

PARAMETERS: VERBOSE  DEFAULT 'X' NO-DISPLAY
          , LOCKCE3  DEFAULT 'X' NO-DISPLAY
          , SINGLDEL DEFAULT 'X' NO-DISPLAY       " nicht unterst?zt
          , GENONLY  DEFAULT ' ' NO-DISPLAY
          , TESTMODE AS CHECKBOX DEFAULT ' '
*         nur w?rend der Testzeit
          , XXSELCE1 DEFAULT 'X' NO-DISPLAY
          , XXDELCE3 DEFAULT 'X' NO-DISPLAY
          , XXINSCE3 DEFAULT 'X' NO-DISPLAY
          .

************************************************************************
*
*   Data
*

* Wenn local_version = 'X', wird die Vorlage ZKEVRK2E und der
* generierte Report ZK2Exxxx verwendet.
* Ansonsten die Produktivversion (RKEVRK2E und RK2Exxxx).
DATA: LOCAL_VERSION                      VALUE 'X'.

DATA: G_MODEL LIKE RS38M-PROGRAMM        VALUE '.KEVRK2E'
    , G_PROG  LIKE RS38M-PROGRAMM        VALUE '.K2Exxxx'
    , G_ENQUEUE_CE3 LIKE RS38L-NAME      VALUE 'ENQUEUE_EKC3xxxx'
    , G_DEQUEUE_CE3 LIKE RS38L-NAME      VALUE 'DEQUEUE_EKC3xxxx'
    .

* Die Einzelfunktionen in RKEVRK2E verwenden STAT (glob).
* Das Include wird auch in RKEVRK2E eingebunden.
INCLUDE RKEREOI1.                      " globales STAT (wie Typ)

DATA: G_START_DATE LIKE SY-DATUM
    , G_START_TIME LIKE SY-UZEIT
    , G_END_DATE LIKE SY-DATUM
    , G_END_TIME LIKE SY-UZEIT
    , G_RC LIKE SY-SUBRC
    .

************************************************************************
*
*   Initialization
*

INITIALIZATION.
* Testdatensatz
  IF SY-UNAME = 'HENRICH'.
    OP_CO = 'IDEA'.
    TEMPFILE = '/tmp/rkereo31.tmp'.
    F_YEAR = '1996'.
    PLIKZ = '0'.
    VRGAR = 'D'.
    TESTMODE = 'X'.
  ENDIF.


************************************************************************
*
*   Start of Selection
*

START-OF-SELECTION.

  GET TIME.
  G_START_DATE = SY-DATUM.
  G_START_TIME = SY-UZEIT.

  IF LOCAL_VERSION = 'X'.
    G_MODEL+0(1) = 'Z'.
    G_PROG+0(1) = 'Z'.
  ELSE.
    G_MODEL+0(1) = 'R'.
    G_PROG+0(1) = 'R'.
  ENDIF.
  G_PROG+4(4) = OP_CO.
  G_ENQUEUE_CE3+12(4) = OP_CO.
  G_DEQUEUE_CE3+12(4) = OP_CO.

  PERFORM SHOW_PARSCR.
  CLEAR STAT.

  PERFORM KSTOPWATCH USING 'Generate'(031).

  CALL FUNCTION 'RKE_GENERATE_ABAP'
       EXPORTING
            ERKRS = OP_CO
            FCODE = 'GEN '
            MODEL = G_MODEL
            PROGRAM = G_PROG
            PROG_TYPE = 'S'.

  CALL FUNCTION 'DB_COMMIT'.

  IF GENONLY = ' '.

    PERFORM KSTOPWATCH USING 'Enqueue'(030).

    IF LOCKCE3 = 'X'.
      CALL FUNCTION G_ENQUEUE_CE3.
    ENDIF.

    PERFORM KVSTOPWATCH USING 'Reorg'(032) STAT-START.

    PERFORM BUILD_CE3_FROM_LINE_ITEMS IN PROGRAM (G_PROG)
        USING F_YEAR
              PLIKZ
              VRGAR
              TEMPFILE
              RAMAVAIL
              VERBOSE
              SINGLDEL
              TESTMODE
              STAT
              XXSELCE1
              XXDELCE3
              XXINSCE3
              G_RC
              .

    PERFORM KVSTOPWATCH USING 'Dequeue'(033) STAT-ENDED.

    IF LOCKCE3 = 'X'.
      CALL FUNCTION G_DEQUEUE_CE3.
    ENDIF.

  ENDIF.

  GET TIME.
  G_END_DATE = SY-DATUM.
  G_END_TIME = SY-UZEIT.

  IF VERBOSE = 'X'.
    WRITE: / 'Time:', G_START_DATE, G_START_TIME,
             '-',     G_END_DATE,   G_END_TIME.
  ENDIF.

  STAT-START = G_START_TIME.
  STAT-ENDED = G_END_TIME.

  PERFORM SHOW_STAT.


************************************************************************
*
*   Zeitfunktionen
*

* Ausgabe aktuelle Zeit mit Kommentar
FORM KSTOPWATCH
      USING VALUE(BLA).
  GET TIME.
  WRITE: / SY-DATUM, SY-UZEIT, BLA.
ENDFORM.

* Besetzen einer Variable mit der aktuellen Zeit
FORM VSTOPWATCH
      CHANGING DIEZEIT.
  GET TIME FIELD DIEZEIT.
ENDFORM.

* Ausgabe aktuelle Zeit mit Kommentar und Besetzen einer Variable
FORM KVSTOPWATCH
      USING VALUE(BLA)
      CHANGING DIEZEIT.
  PERFORM KSTOPWATCH USING BLA.
  PERFORM VSTOPWATCH USING DIEZEIT.
ENDFORM.


************************************************************************
*
*   Ausgabefunktionen f? den Parameter-Schirm
*

FORM SHOW_PARSCR.

  SKIP.
  FORMAT INTENSIFIED.
  WRITE: / 'System Information'.
  FORMAT INTENSIFIED OFF.
  WRITE: / '  SY-DATUM', SY-DATUM
       , / '  SY-UZEIT', SY-UZEIT
       , / '  SY-TZONE', SY-TZONE
       , / '  SY-DAYST', SY-DAYST
       , / '  SY-SYSID', SY-SYSID
       , / '  SY-SAPRL', SY-SAPRL
       , / '  SY-MANDT', SY-MANDT
       , / '  SY-UNAME', SY-UNAME
       , / '  SY-LANGU', SY-LANGU
       , / '  SY-DBSYS', SY-DBSYS
       , / '  SY-DCSYS', SY-DCSYS
       , / '  SY-OPSYS', SY-OPSYS
       , / '  SY-HOST ', SY-HOST
       , / '  SY-LOCDB', SY-LOCDB
       , / '  SY-LOCOP', SY-LOCOP
       , / '  SY-REPID', SY-REPID
       .

  FORMAT INTENSIFIED.
  WRITE: / 'Parameter Screen'.
  FORMAT INTENSIFIED OFF.
  WRITE: / '  ERKRS   ', OP_CO
       , / '  GJAHR   ', F_YEAR
       , / '  PLIKZ   ', PLIKZ
       , / '  VRGAR   ', VRGAR
       , / '  VERBOSE ', VERBOSE
       , / '  LOCKCE3 ', LOCKCE3
       , / '  SINGLDEL', SINGLDEL
       , / '  GENONLY ', GENONLY
       , / '  TESTMODE', TESTMODE
       , / '  TEMPFILE', TEMPFILE
       .

  IF VERBOSE = 'X'.

    WRITE: / '  XXSELCE1', XXSELCE1
         , / '  XXDELCE3', XXDELCE3
         , / '  XXINSCE3', XXINSCE3
         .

    FORMAT INTENSIFIED.
    WRITE: / 'Locals'.
    FORMAT INTENSIFIED OFF.
    WRITE: / '  G_MODEL ', G_MODEL
         , / '  G_PROG  ', G_PROG
         .

  ENDIF.

  ULINE.

ENDFORM.



************************************************************************
*
*   Ausgabefunktionen f? die Statistik-R?mchen
*   Verwendet globale Struktur STAT.
*   Textnummern 100-117
*

FORM SHOW_STAT.

  DATA HILF TYPE I.                    " ohne Worte

  SKIP.

  WRITE: /(60) SY-ULINE.
  WRITE: / SY-VLINE,
      3 'Reorganisation der Objektebene'(100) INTENSIFIED,
      49 SY-DATUM,
      60 SY-VLINE.

* fester Teil: ERKRS, GJAHR, PAOBJNR, Zeit von...bis
  WRITE: /(60) SY-ULINE.
  WRITE: / SY-VLINE,
      3 'Ergebnisbereich'(103),
      49 OP_CO,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'Gesch?tsjahr'(104),
      49 F_YEAR,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'Plan-/Ist-Kennzeichen'(117),
      49 PLIKZ,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'Vorgangsart          ',
      49 VRGAR,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'Gestartet'(105),
      49 STAT-START,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'Beendet'(116),
      49 STAT-ENDED,
      60 SY-VLINE.

  IF TESTMODE = 'X'.
    WRITE: /(60) SY-ULINE.
        WRITE: / SY-VLINE,
        3 'TESTMODE - Database was not updated !!!',
        60 SY-VLINE.
  ENDIF.

  WRITE: /(60) SY-ULINE.
      IF G_RC > 0.
        WRITE: / SY-VLINE,
        3 'Operation was possibly not successful !',
        60 SY-VLINE.
      ELSE.
        WRITE: / SY-VLINE,
        3 'Operation was successful !',
        60 SY-VLINE.
      ENDIF.

  WRITE: /(60) SY-ULINE.
      WRITE: / SY-VLINE,
      3 'Anzahl gel?chte Objektebenens?ze'(106),
      49 STAT-DEL_CE3,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'Einzelposten gelesen'(107),
      49 STAT-CE1,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'OE-S?ze'(109),
      49 STAT-CE3,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'INSERT-Operationen'(112),
      49 STAT-CE3_INS,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'INSERT-Fehler'(113),
      49 STAT-CE3_INS_ERR,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'UPDATE-Operationen'(114),
      49 STAT-CE3_UPD,
      60 SY-VLINE.
  WRITE: / SY-VLINE,
      3 'UPDATE-Fehler'(115),
      49 STAT-CE3_UPD_ERR,
      60 SY-VLINE.

  HILF = STAT-ALERT_PALEDGER  +
         STAT-ALERT_PLIKZ     +
         STAT-ALERT_PASUBNR   +
         STAT-ALERT_PAPAOBJNR +
         STAT-ALERT_PAPASUBNR +
         STAT-ALERT_HRKFT       .
  IF HILF > 0.

    WRITE: /(60) SY-ULINE.

    WRITE: / SY-VLINE, 3 'Fehlerhafte Feldwerte'(201).
    FORMAT COLOR COL_NEGATIVE.
    WRITE: 49 HILF.
    FORMAT COLOR OFF.
    WRITE: 60 SY-VLINE.

    WRITE: / SY-VLINE, 5 'PALEDGER'.
    IF STAT-ALERT_PALEDGER > 0.
      FORMAT COLOR COL_NEGATIVE.
    ENDIF.
    WRITE: 49 STAT-ALERT_PALEDGER.
    IF STAT-ALERT_PALEDGER > 0.
      FORMAT COLOR OFF.
    ENDIF.
    WRITE: 60 SY-VLINE.

    WRITE: / SY-VLINE, 5 'PLIKZ'.
    IF STAT-ALERT_PLIKZ > 0.
      FORMAT COLOR COL_NEGATIVE.
    ENDIF.
    WRITE: 49 STAT-ALERT_PLIKZ.
    IF STAT-ALERT_PLIKZ >= 0.
      FORMAT COLOR OFF.
    ENDIF.
    WRITE: 60 SY-VLINE.

    WRITE: / SY-VLINE, 5 'PASUBNR'.
    IF STAT-ALERT_PASUBNR > 0.
      FORMAT COLOR COL_NEGATIVE.
    ENDIF.
    WRITE: 49 STAT-ALERT_PASUBNR.
    IF STAT-ALERT_PASUBNR > 0.
      FORMAT COLOR OFF.
    ENDIF.
    WRITE: 60 SY-VLINE.

    WRITE: / SY-VLINE, 5 'PAPAOBJNR'.
    IF STAT-ALERT_PAPAOBJNR > 0.
      FORMAT COLOR COL_NEGATIVE.
    ENDIF.
    WRITE: 49 STAT-ALERT_PAPAOBJNR.
    IF STAT-ALERT_PAPAOBJNR > 0.
      FORMAT COLOR OFF.
    ENDIF.
    WRITE: 60 SY-VLINE.

    WRITE: / SY-VLINE, 5 'PAPASUBNR'.
    IF STAT-ALERT_PAPASUBNR > 0.
      FORMAT COLOR COL_NEGATIVE.
    ENDIF.
    WRITE: 49 STAT-ALERT_PAPASUBNR.
    IF STAT-ALERT_PAPASUBNR > 0.
      FORMAT COLOR OFF.
    ENDIF.
    WRITE: 60 SY-VLINE.

    WRITE: / SY-VLINE, 5 'HRKFT'.
    IF STAT-ALERT_HRKFT > 0.
      FORMAT COLOR COL_NEGATIVE.
    ENDIF.
    WRITE: 49 STAT-ALERT_HRKFT.
    IF STAT-ALERT_HRKFT > 0.
      FORMAT COLOR OFF.
    ENDIF.
    WRITE: 60 SY-VLINE.

  ENDIF. " Summe Alert > 0

  WRITE: /(60) SY-ULINE.

ENDFORM.
