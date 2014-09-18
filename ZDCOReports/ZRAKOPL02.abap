REPORT RAKOPL02 MESSAGE-ID AB
                LINE-SIZE 123
                NO STANDARD PAGE HEADING.

************************************************************************
* CHANGE LOG
*-----------------------------------------------------------------------
* DATE      |  NAME          |Transport | Issue #  |      DESC
* 2011.09.15   yn.kim         UP1K920005            ECC6.0 Upgrade.
*                                                   + parameter add(2)
*-----------------------------------------------------------------------

* "perf   : Performanceverbesserung, kein nochmaliges Nachlesen
*           von ANEP/ANEA durch RA_AFABUCHEN. HW = 42793.
* "ukostl : Unterjährigen Kostenstellenwechsel/LSTAR-Wechsel zulassen.
* "msg    : Verbesserung des Abbruch- und Fehlerhandlings.
* "alv    : Umstellung Ausgabe auf ALV


* Tabelle TKOMP fuer RWIN-Aufruf.
*INCLUDE TIS01AD00.
INCLUDE IS01AD00.

* PG-Konstanten.
*INCLUDE TLAFARCON.
INCLUDE LAFARCON.

*** Global data declaration  Typepool für ALV ***
TYPE-POOLS: SLIS.

* TABLES- und DATA-Anweisungen                                   "alv
INCLUDE RAKOPL02_DATA.
*include t_rakopl02_data.

* Zusätzliche Selektionen                                        "alv

** << Start of modification on 10.17.2006 by Michelle
* INCLUDE RAKOPL02_SEL.
INCLUDE ZRAKOPL02_SEL.
*include t_rakopl02_sel.
** end of addition on 10.17.2006 by Michelle >>

* Data's und Forms für ALV-Anbindung                             "alv
INCLUDE RAKOPL02_ALV_DATA.                                       "alv
INCLUDE RAKOPL02_ALV_FORMS.                                      "alv

* Common Part mit der logischen Datenbank.                   "n.041231
INCLUDE DBADACOM.                                           "n.041231

AT SELECTION-SCREEN OUTPUT.
* Ausblenden Summenberichtsschalter der LDB ADA (stört!)
  LOOP AT SCREEN.
    IF SCREEN-NAME  = 'SRTVR'.
      SCREEN-INVISIBLE = ' '.
      SCREEN-ACTIVE = '0'.
    ELSEIF SCREEN-NAME = 'SRT_TXT'.
      SCREEN-ACTIVE    = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


INITIALIZATION.

* 'Werte zum Stand GJ-Beginn' als Default wegen
* Aufwärtskompatibilität
* (siehe internes Problem 0110010790 0003318191 1997)
  PA_XSETL = SPACE.
  PA_XGJBG = 'X'.
* 'Anlagen selektieren' nicht defaultmäßig
* (siehe internes Problem 0110010790 0003318191 1997
* von Alfred Schaller).
  PA_XANLG = SPACE.

* CO-Bereich ermitteln und vorschlagen.
  CLEAR HLP_BUKRS.
  GET PARAMETER ID 'BUK' FIELD HLP_BUKRS.
  CLEAR BEREICH1.
  SELECT * FROM T093C
    WHERE BUKRS EQ HLP_BUKRS.
    EXIT.
  ENDSELECT.
  IF SY-SUBRC EQ 0.
    SELECT * FROM T093A
      WHERE AFAPL  EQ T093C-AFAPL
      AND   BERTYP EQ CON_BERTYP_KAL.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC EQ 0.
      BEREICH1 = T093A-AFABE.
    ENDIF.
  ENDIF.
* Kein CO-Bereich gefunden ...
  IF BEREICH1 IS INITIAL.
*    ... dann nimm halt Bereich 01 (wegen den Amis ... ).
    BEREICH1 = '01'.
  ENDIF.

* Prüfungen von Selektionen (AT SELECTION-SCREEN ... )
  INCLUDE RAKOPL02_SELTEST.
*include t_rakopl02_seltest.

*---------------------------------------------------------------alv(beg)
* Process on value request
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F4_FOR_VARIANT.

AT SELECTION-SCREEN ON P_VARI.
  PERFORM CHECK_VARIANT.                                            "alv
*---------------------------------------------------------------alv(end)


START-OF-SELECTION.
*                                                " beg_ins " n. 812401
*// 2011.08.17 delete by YN.Kim ==  Duplicate.
***  RANGES: RG_GSBER FOR ANLZ-GSBER,
***          RG_KOSTL FOR ANLZ-KOSTL,
***          RG_WERKS FOR ANLZ-WERKS,
***          RG_STORT FOR ANLZ-STORT.
* Periodengenaue Planung ...
  IF PA_FCWKG IS INITIAL.
*   ... dann Selektionen auf zeitabhängige Daten
*   nicht in der logischen Datenbank prüfen (dort
*   findet Prüfung mit den zum GJ-Ende gültigen
*   ANLZ-Daten statt) sondern in KOTAB_PER_AUFBAUEN
*   für jede Periode separat.
    RG_GSBER[] = SO_GSBER[].
*   refresh so_gsber.                            " del     " n. 825129
    RG_KOSTL[] = SO_KOSTL[].
*   refresh so_kostl.                            " del     " n. 825129
    RG_WERKS[] = SO_WERKS[].
*   refresh so_werks.                            " del     " n. 825129
    RG_STORT[] = SO_STORT[].
*   refresh so_stort.                            " del     " n. 825129
  ENDIF.
*                                                " end_ins " n. 812401
* BuKrs-/KoKrs-Selektion abmischen.                        " n. 737285
  PERFORM KOKRS_BUKRS_SELECTION_MERGE.                      " n. 737285

* << Start of addition on 10.18.2006 by Michelle
  PERFORM DEPR USING 'X'.   " Depreciation Simulation for Costing Plan
* End of addition on 10.18.2006 by Michelle >>

* Default: Kein Satz selektiert
  RAREP-XNORCD = 'X'.

* Periodengenaue Planung ...                               " n. 312108
  DATA: FLAG_ANLP_GET_ALL LIKE SY-DATAR VALUE 'X'.          " n. 312108
  IF PA_FCWKG IS INITIAL.                                   " n. 312108
* ... dann muss SAPDBADA für echte Anlagen alle            " n. 312108
* ANLP-Sätze putten (nicht nur den letzten).               " n. 312108
    EXPORT FLAG_ANLP_GET_ALL TO MEMORY                      " n. 312108
      ID 'IM_FLAG_ANLP_GET_ALL'.                            " n. 312108
    FLG_PERIODS = 'X'.                                      " n. 412316
  ENDIF.                                                    " n. 312108

* Geschäftsjahr an logische Datenbank übergeben
* (Berichtsdatum wird hieraus abgeleitet).
  MOVE PA_GSJHR TO PA_GEJHR.

* Periodengenaue Planung ...
  IF PA_FCWKG IS INITIAL.
* ... dann Spaltebueberschrift 'Per' mit ausgeben.
    MOVE TEXT-201 TO UEBS_201.
  ENDIF.

* Planung leistungsartenabhaengig ...
  IF NOT PA_LSTAR IS INITIAL.
* ... dann auch Spaltenueberschrift fuer fixe Kosten.
    MOVE TEXT-203 TO UEBS_203.
  ENDIF.

* Zugangs-BWA bestimmen.
  SELECT SINGLE * FROM TABWV
    WHERE VORGA EQ 'RE'.
  IF SY-SUBRC EQ 0.
    MOVE TABWV-BWASL TO CON_ZUGANG_BWA.
  ELSE.
    MOVE '100'       TO CON_ZUGANG_BWA.
  ENDIF.

   *ANLA0-SIMVAR = PA_SIMVR.
   *ANLA0-XGPLA  = PA_XGPLA.

GET ANLA0.


GET ANLAV.
  ZANLAV = ANLAV.                                          "perf

* Eventuell Flag setzen: Kein Lesen auf der DB beim AfA-Rechnen.
  ON CHANGE OF ANLAV-WRTTP.
*   Werte ganz normal ....
    IF PA_XGJBG IS INITIAL.
*     ... dann bei Plansätzen nicht auf der DB lesen
*     und ANEPs selbst mitgeben.
      IF ANLAV-WRTTP EQ '1' OR         " InvProgPosition/PSP-El/Auft.
         ANLAV-WRTTP EQ '2' OR         " PSP-Element.
         ANLAV-WRTTP EQ '3' OR         " Auftrag.
         ANLAV-WRTTP EQ '4' .          " Anforderung.
        FLG_NO_DB = 'X'.
*       FLG_XANEP = 'X'.                                   "perf
      ELSE.
        CLEAR FLG_NO_DB.
*       CLEAR FLG_XANEP.                                   "perf
      ENDIF.
*   Werte zum GJ-Beginn nehmen ...
    ELSE.
*     ... dann auf keinen Fall auf der DB lesen
*     und leere XANEP selbst mitgeben
*     (ANEPs des GJ ignorieren)!
      FLG_NO_DB = 'X'.
*     FLG_XANEP = 'X'.                                     "perf
    ENDIF.
  ENDON.

  ON CHANGE OF ANLAV-BUKRS.

*-------------------------------------------------------------"alv(beg)

    L_PROCESS_BUKRS = 'X'.
* Prüfen, ob GJahr-variante höchstens 13 Perioden hat, wenn nein -> raus
* 1.Lesen der GJ

    L_T001-BUKRS = ANLAV-BUKRS.

    CALL FUNCTION 'T001_READ_FIAA'
         CHANGING
              F_T001 = L_T001.

    L_T009-PERIV = L_T001-PERIV.

* 2.Lesen Anzahl Perioden im Geschäftsjahr
    CALL FUNCTION 'T009_READ'
         EXPORTING
              F_T009 = L_T009
         IMPORTING
              F_T009 = L_T009.

* Ausgabe Message
    IF L_T009-ANZBP > 13.
      MESSAGE E065 WITH ANLAV-BUKRS L_T009-ANZBP.
*        l_process_bukrs = ' '.
    ELSE.
*-------------------------------------------------------------"alv(end)

*       Berechtigung zur Kostenplanung im Buchungskreis vorhanden?
      AUTHORITY-CHECK   OBJECT 'A_PERI_BUK'
                        ID     'AM_ACT_PER' FIELD CON_33
                        ID     'BUKRS'      FIELD ANLAV-BUKRS.
      IF SY-SUBRC NE 0.
*          Nein, dann Abbruch.
*          PERFORM MESSAGE_COLLECT USING 'A'                   "msg
*                                        'AB'                  "msg
*                                        '027'                 "msg
*                                        ANLCV-BUKRS           "msg
*                                        ' '                   "msg
*                                        ' '                   "msg
*                                        ' '.                  "msg
*          PERFORM ABEND.                                      "msg
        PERFORM ABEND USING 'AB'                  " F_MSGID "msg
                            '027'                 " F_MSGNO "msg
                            ANLAV-BUKRS           " F_MSGV1 "msg
                            ' '                   " F_MSGV2 "msg
                            ' '                   " F_MSGV3 "msg
                            ' '.                  " F_MSGV4 "msg
      ENDIF.

    ENDIF.                                                    "alv

  ENDON.

** Hat der Buchungskreis mehr als 13 Perioden, weiter mit       "alv
** nächstem Buchungskreis                                       "alv
*  check l_process_bukrs = 'X'.                                 "alv

* CHECK SELECT-OPTIONS.                                    "ukostl
* Nur Anlagen seleketieren, die aktiviert wurden ...
  CHECK NOT ANLAV-ZUGDT IS INITIAL.

* LstArt-abhängige Planung ohne LstArt-unabhängige Planung "ukostl
* und keine Leistungsart gesetzt ...                       "ukostl
* IF NOT PA_LSTAR    IS INITIAL AND                        "ukostl
*        PA_LSTAU    IS INITIAL AND                        "ukostl
*        ANLAV-LSTAR IS INITIAL .                          "ukostl
*   ... dann ignorieren.                                   "ukostl
*   REJECT.                                                "ukostl
* ENDIF.                                                   "ukostl

* Reine LstArt-unabhängige Planung ...                     "ukostl
* IF PA_LSTAR IS INITIAL.                                  "ukostl
*   ... dann LstArt ignorieren.                            "ukostl
*   CLEAR ANLAV-LSTAR.                                     "ukostl
* ENDIF.                                                   "ukostl

  ON CHANGE OF ANLAV-BUKRS.

*   T001 lesen.
    SELECT SINGLE * FROM T001
      WHERE BUKRS EQ ANLAV-BUKRS.
    IF SY-SUBRC NE 0.
*      PERFORM MESSAGE_COLLECT USING 'A'                   "msg
*                                    'AP'                  "msg
*                                    '100'                 "msg
*                                    SY-REPID              "msg
*                                    'T001_NOT_FOUND'      "msg
*                                    ' '                   "msg
*                                    ' '.                  "msg
*      PERFORM ABEND.                                      "msg
      PERFORM ABEND USING 'AP'                  " F_MSGID "msg
                          '100'                 " F_MSGNO "msg
                          SY-REPID              " F_MSGV1 "msg
                          'T001_NOT_FOUND'      " F_MSGV2 "msg
                          ' '                   " F_MSGV3 "msg
                          ' '.                  " F_MSGV4 "msg
    ENDIF.

*   T093C lesen.
    SELECT SINGLE * FROM T093C
      WHERE BUKRS EQ ANLAV-BUKRS.
    IF SY-SUBRC NE 0.
*      PERFORM MESSAGE_COLLECT USING 'A'                   "msg
*                                    'AP'                  "msg
*                                    '100'                 "msg
*                                    SY-REPID              "msg
*                                    'T093C_NOT_FOUND'     "msg
*                                    ' '                   "msg
*                                    ' '.                  "msg
*      PERFORM ABEND.                                      "msg
      PERFORM ABEND USING 'AP'                  " F_MSGID "msg
                          '100'                 " F_MSGNO "msg
                          SY-REPID              " F_MSGV1 "msg
                          'T093C_NOT_FOUND'     " F_MSGV2 "msg
                          ' '                   " F_MSGV3 "msg
                          ' '.                  " F_MSGV4 "msg
    ENDIF.
*   Falls eine Altdatenübernahme stattgefunden hat:        " n. 440555
*   Ermittle Periode der Altdatenübernahme.                " n. 440555
    IF NOT T093C-DATUM IS INITIAL.                          " n. 440555
      CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'                " n. 440555
           EXPORTING                                        " n. 440555
                I_DATE         = T093C-DATUM                " n. 440555
                I_PERIV        = T001-PERIV                 " n. 440555
           IMPORTING                                        " n. 440555
                E_BUPER        = HLP_ALTD_BUPER             " n. 440555
                E_GJAHR        = HLP_ALTD_GJAHR.            " n. 440555
    ELSE.                                                   " n. 440555
      CLEAR: HLP_ALTD_BUPER,                                " n. 440555
             HLP_ALTD_GJAHR.                                " n. 440555
    ENDIF.                                                  " n. 440555

*   T093D lesen.
    PERFORM T093D_GET_RELEVANT_AREA
            USING    T093C
                     BEREICH1
            CHANGING SAV_PLL_LEADING_AREA.
    SELECT SINGLE * FROM T093D
      WHERE BUKRS  EQ ANLAV-BUKRS
      AND   AFABER EQ SAV_PLL_LEADING_AREA.
    IF SY-SUBRC NE 0.
*      PERFORM MESSAGE_COLLECT USING 'A'                   "msg
*                                    'AP'                  "msg
*                                    '100'                 "msg
*                                    SY-REPID              "msg
*                                    'T093D_NOT_FOUND'     "msg
*                                    ' '                   "msg
*                                    ' '.                  "msg
*      PERFORM ABEND.                                      "msg
      PERFORM ABEND USING 'AB'                  " F_MSGID "msg
                          '028'                 " F_MSGNO "msg
                          ANLAV-BUKRS           " F_MSGV1 "msg
                          BEREICH1              " F_MSGV2 "msg
                          ' '                   " F_MSGV3 "msg
                          ' '.                  " F_MSGV4 "msg
    ENDIF.
*   Planung auf Kostenstelle/Innenauftrag ==>
*   In den Buchungsregeln muß auch Kontierung auf
*   Kostenstelle/Innenauftrag vorgesehen sein.
    IF ( NOT PA_PLKST     IS INITIAL AND
             T093D-AFBKST IS INITIAL )   OR
       ( NOT PA_PLAUF     IS INITIAL AND
             T093D-AFBAUF IS INITIAL )   .
      PERFORM ABEND USING 'AB'                  " F_MSGID
                          '033'                 " F_MSGNO
                          ANLAV-BUKRS                       " F_MSGV1
                          BEREICH1                          " F_MSGV2
                          ' '                               " F_MSGV3
                          ' '.                              " F_MSGV4
    ENDIF.
*   T093D in Arbeitstabelle einstellen.
    CLEAR X093D. REFRESH X093D.
    MOVE T093D TO X093D.
    APPEND X093D.

*   Währungsschlüssel ermitteln, Default = Hauswährung.
    SAV_WAERS = T001-WAERS.
*   Waehrungsschluessel aus T093B.
    SELECT SINGLE * FROM T093B
      WHERE BUKRS EQ ANLAV-BUKRS
      AND   AFABE EQ BEREICH1.
    IF SY-SUBRC EQ 0.
      SAV_WAERS = T093B-WAERS.
    ENDIF.

*   Kurztext zu AfA-Bereich lesen.
    CLEAR T093T-AFBKTX.
    SELECT SINGLE * FROM T093T
      WHERE SPRAS  EQ SY-LANGU
      AND   AFAPL  EQ T093C-AFAPL
      AND   AFABER EQ BEREICH1.
    IF SY-SUBRC NE 0.
      MOVE: SPACE TO T093T-AFBKTX,
            '?'   TO T093T-AFBKTX.
    ENDIF.

*   Ersten Tag des GJ in SAV_GJBEG merken.
    CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
         EXPORTING
              I_GJAHR        = PA_GSJHR
              I_PERIV        = T001-PERIV
              I_POPER        = CON_POPER
         IMPORTING
              E_DATE         = SAV_GJBEG
         EXCEPTIONS
              INPUT_FALSE    = 4
              T009_NOTFOUND  = 8
              T009B_NOTFOUND = 12.
    IF SY-SUBRC NE 0.
*      PERFORM MESSAGE_COLLECT USING 'A'                   "msg
*                                    'AP'                  "msg
*                                    '100'                 "msg
*                                    SY-REPID              "msg
*                               'FIRST_DAY_IN_PERIOD_GET'  "msg
*                                    ' '                   "msg
*                                    ' '.                  "msg
*      PERFORM ABEND.                                      "msg
      PERFORM ABEND USING 'AP'                  " F_MSGID "msg
                          '100'                 " F_MSGNO "msg
                          SY-REPID              " F_MSGV1 "msg
                  'FIRST_DAY_IN_PERIOD_GET'     " F_MSGV2 "msg
                          ' '                   " F_MSGV3 "msg
                          ' '.                  " F_MSGV4 "msg
    ENDIF.

*   Letzter Tag des GJ SAV_GJEND = Berichtsdatum BERDATUM.
    SAV_GJEND = BERDATUM.

*   Erste Periode des GJ in SAV_GJBEGPER merken.
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
         EXPORTING
              I_DATE         = SAV_GJBEG
              I_PERIV        = T001-PERIV
         IMPORTING
              E_BUPER        = SAV_GJBEGPER
         EXCEPTIONS
              INPUT_FALSE    = 4
              T009_NOTFOUND  = 8
              T009B_NOTFOUND = 12.
    IF SY-SUBRC NE 0.
*      PERFORM MESSAGE_COLLECT USING 'A'                   "msg
*                                    'AP'                  "msg
*                                    '100'                 "msg
*                                    SY-REPID              "msg
*                             'DATE_TO_PERIOD_CONVERT'     "msg
*                                    ' '                   "msg
*                                    ' '.                  "msg
*      PERFORM ABEND.                                      "msg
      PERFORM ABEND USING 'AP'                  " F_MSGID "msg
                          '100'                 " F_MSGNO "msg
                          SY-REPID              " F_MSGV1 "msg
                    'DATE_TO_PERIOD_CONVERT'    " F_MSGV2 "msg
                          ' '                   " F_MSGV3 "msg
                          ' '.                  " F_MSGV4 "msg
    ENDIF.

*   Planung-von-Periode ungueltig ==> Nimm erste Periode.
    IF PA_VONPE IS INITIAL.
      PA_VONPE = SAV_GJBEGPER.
    ENDIF.

*   Letzte Periode des GJ in SAV_GJENDPER merken.
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
         EXPORTING
              I_DATE         = SAV_GJEND
              I_PERIV        = T001-PERIV
         IMPORTING
              E_BUPER        = SAV_GJENDPER
         EXCEPTIONS
              INPUT_FALSE    = 4
              T009_NOTFOUND  = 8
              T009B_NOTFOUND = 12.
    IF SY-SUBRC NE 0.
*      PERFORM MESSAGE_COLLECT USING 'A'                   "msg
*                                    'AP'                  "msg
*                                    '100'                 "msg
*                                    SY-REPID              "msg
*                                 'DATE_TO_PERIOD_CONVERT' "msg
*                                    ' '                   "msg
*                                    ' '.                  "msg
*      PERFORM ABEND.                                      "msg
      PERFORM ABEND USING 'AP'                  " F_MSGID "msg
                          '100'                 " F_MSGNO "msg
                          SY-REPID              " F_MSGV1 "msg
                      'DATE_TO_PERIOD_CONVERT'  " F_MSGV2 "msg
                          ' '                   " F_MSGV3 "msg
                          ' '.                  " F_MSGV4 "msg
    ENDIF.

*   Planung-bis-Periode ungueltig ==> Nimm letzte Periode.
    IF PA_BISPE GT SAV_GJENDPER OR
       PA_BISPE IS INITIAL      .
      PA_BISPE = SAV_GJENDPER.
    ENDIF.

*   Letzten Tag der Bis-Periode in SAV_BISPEEND merken.
    CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
         EXPORTING
              I_GJAHR        = PA_GSJHR
              I_PERIV        = T001-PERIV
              I_POPER        = PA_BISPE
         IMPORTING
              E_DATE         = SAV_BISPEEND
         EXCEPTIONS
              INPUT_FALSE    = 4
              T009_NOTFOUND  = 8
              T009B_NOTFOUND = 12.
    IF SY-SUBRC NE 0.
*      PERFORM MESSAGE_COLLECT USING 'A'                   "msg
*                                    'AP'                  "msg
*                                    '100'                 "msg
*                                    SY-REPID              "msg
*                              'LAST_DAY_IN_PERIOD_GET'    "msg
*                                    ' '                   "msg
*                                    ' '.                  "msg
*      PERFORM ABEND.                                      "msg
      PERFORM ABEND USING 'AP'                  " F_MSGID "msg
                          '100'                 " F_MSGNO "msg
                          SY-REPID              " F_MSGV1 "msg
                      'LAST_DAY_IN_PERIOD_GET'  " F_MSGV2 "msg
                          ' '                   " F_MSGV3 "msg
                          ' '.                  " F_MSGV4 "msg
    ENDIF.

*   Report-Header, Teil A.
    WRITE:  SY-TITLE      TO HEAD_A-TITLE,
            '-'           TO HEAD_A-INFO1,
            BEREICH1      TO HEAD_A-INFO2,
            T093T-AFBKTX  TO HEAD_A-INFO3.
    CONDENSE HEAD_A.
*   Report-Header, Teil B.
    WRITE:  SPACE         TO HEAD_B-TITLE,
            TEXT-201      TO HEAD_B-INFO1(20),
            PA_VONPE      TO HEAD_B-INFO2+0(3),
            '-'           TO HEAD_B-INFO2+3(1),
            PA_BISPE      TO HEAD_B-INFO2+4(3).
    WRITE:   '-'          TO HEAD_B-INFO4(2),
             TEXT-402     TO HEAD_B-INFO4+2(10),
             PA_VERSI     TO HEAD_B-INFO4+13(3).
    IF PA_TESTL EQ SPACE.
      WRITE: '-'          TO HEAD_B-INFO5,
             TEXT-400     TO HEAD_B-INFO5+2(10).
    ELSE.
      WRITE: '-'          TO HEAD_B-INFO5,
             TEXT-401     TO HEAD_B-INFO5+2(10).
    ENDIF.
    CONDENSE HEAD_B.

*   Anzahl der Perioden des GJ.
    SAV_ANZPER = SAV_GJENDPER - SAV_GJBEGPER + 1.

*   Stückzahl-AfA-Schlüssel ermitteln.                "corni
    PERFORM GET_TAB_STCKAFA.                          "corni
    IF PA_XGJBG EQ CON_X.
      IF T093C-LGJAHR IS INITIAL.
        CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
             EXPORTING
                  I_DATE  = SY-DATLO
                  I_PERIV = T001-PERIV
             IMPORTING
                  E_GJAHR = T093C-LGJAHR.
      ENDIF.
*   Variable hlp_agjbeg abgeschafft. HW514310
    ENDIF.

*   Ermitteln des aktuell laufenden Geschäftsjahres.           "no514310
*   Wird benötigt für die Behandlung des geplanten             "no514310
*   Abgangsdatums anlav-gplab. Die log. Datenbank ADA kann     "no514310
*   für Vorjahre mit der Logik anlav-deakt = anlav-gplab       "no514310
*   geplantes Abgangsdatum nicht handhaben.                    "no514310
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'                  "no514310
         EXPORTING                                          "no514310
              I_DATE         = SY-DATUM                     "no514310
              I_PERIV        = T001-PERIV                   "no514310
         IMPORTING                                          "no514310
              E_GJAHR        = TEMP_GJAHR                   "no514310
         EXCEPTIONS                                         "no514310
              INPUT_FALSE    = 1                            "no514310
              T009_NOTFOUND  = 2                            "no514310
              T009B_NOTFOUND = 3                            "no514310
              OTHERS         = 4.                           "no514310
    IF SY-SUBRC <> 0.                                       "no514310
      MESSAGE ID SY-MSGID TYPE 'A' NUMBER SY-MSGNO          "no514310
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.           "no514310
    ENDIF.                                                  "no514310

  ENDON.                     " Ende: change of anlav-bukrs      no514310

* Wechsel von Buchungskreis oder Geschäftsbereich ==>
* Kokrs neu einlesen und in TKA01 bereithalten.
  IF ANLAV-BUKRS NE OLD_KOKRSKEY-BUKRS OR
     ANLAV-GSBER NE OLD_KOKRSKEY-GSBER .

    OLD_KOKRSKEY-BUKRS = ANLAV-BUKRS.
    OLD_KOKRSKEY-GSBER = ANLAV-GSBER.

*    Kostenrechnungskreis in TKA01 merken.
    CALL FUNCTION 'RK_KOKRS_FIND'
         EXPORTING
              BUKRS  = ANLAV-BUKRS
              GSBER  = ANLAV-GSBER
         IMPORTING
              T_KA01 = TKA01
         EXCEPTIONS
              OTHERS = 4.
    IF SY-SUBRC NE 0.
*       PERFORM MESSAGE_COLLECT USING 'A'                  "msg
*                                     'AP'                 "msg
*                                     '100'                "msg
*                                     SY-REPID             "msg
*                                     'RK_KOKRS_FIND'      "msg
*                                     ' '                  "msg
*                                     ' '.                 "msg
*       PERFORM ABEND.                                     "msg
      PERFORM ABEND USING 'AP'                 " F_MSGID "msg
                          '100'                " F_MSGNO "msg
                          SY-REPID             " F_MSGV1 "msg
                          'RK_KOKRS_FIND'      " F_MSGV2 "msg
                          ' '                  " F_MSGV3 "msg
                          ' '.                 " F_MSGV4 "msg
    ENDIF.
    CHECK SO_KOKRS.                                         " n. 724266
*    Prüfen, ob die benutzte Planversion im KoKrs existiert
*    und das Schreiben von Plandaten erlaubt.
    CALL FUNCTION 'K_VERSN_READ'
         EXPORTING
    "         I_GJAHR          = '0000'
              I_KOKRS          = TKA01-KOKRS
              I_VERSN          = PA_VERSI
    "         BYPASSING_BUFFER =
    "         I_ACTVT          =
         IMPORTING
              E_TKA09          = TKA09
    "         E_TKA07          =
    "         E_TKT09          =
         EXCEPTIONS
              NOT_FOUND        = 1
              NOT_FOUND_GJAHR  = 2
              NO_AUTHORITY     = 3
              OTHERS           = 4.
    IF SY-SUBRC NE 0.
*       PERFORM MESSAGE_COLLECT USING 'A'                  "msg
*                                     'AP'                 "msg
*                                     '100'                "msg
*                                     SY-REPID             "msg
*                                     'K_VERSN_READ'       "msg
*                                     ' '                  "msg
*                                     ' '.                 "msg
*       PERFORM ABEND.                                     "msg
      PERFORM ABEND USING 'AP'                 " F_MSGID "msg
                          '100'                " F_MSGNO "msg
                          SY-REPID             " F_MSGV1 "msg
                          'K_VERSN_READ'       " F_MSGV2 "msg
                          ' '                  " F_MSGV3 "msg
                          ' '.                 " F_MSGV4 "msg
    ENDIF.
    IF TKA09-PLANNING IS INITIAL.
*       PERFORM MESSAGE_COLLECT USING 'A'                  "msg
*                                     'GM'                 "msg
*                                     '057'                "msg
*                                     PA_VERSI             "msg
*                                     ' '                  "msg
*                                     ' '                  "msg
*                                     ' '.                 "msg
*       PERFORM ABEND.                                     "msg
      PERFORM ABEND USING 'GM'                 " F_MSGID "msg
                          '057'                " F_MSGNO "msg
                          PA_VERSI             " F_MSGV1 "msg
                          ' '                  " F_MSGV2 "msg
                          ' '                  " F_MSGV3 "msg
                          ' '.                 " F_MSGV4 "msg
    ENDIF.
*    Bei Echtlauf prüfen, ob dieser auch wirklich im
*    Heimatsystem des Kostenrechnungskreises durchgeführt wird.
    IF PA_TESTL EQ SPACE.
      CALL FUNCTION 'K_LOGSYSTEM_CHECK'
           EXPORTING
                I_KOKRS             = TKA01-KOKRS
*       "         I_TKA01             =
*       "         I_T000              =
           EXCEPTIONS
                LOGSYSTEM_DIFFERENT = 01.
      IF SY-SUBRC NE 0.
        PERFORM ABEND USING SY-MSGID          " F_MSGID
                            SY-MSGNO          " F_MSGNO
                            SY-MSGV1                        " F_MSGV1
                            SY-MSGV2                        " F_MSGV2
                            SY-MSGV3                        " F_MSGV3
                            SY-MSGV4.                       " F_MSGV4
      ENDIF.
    ENDIF.

  ENDIF.

* CHECK SO_KOKRS.                                " del     " n. 724266

* Behandlung der Objekte mit geplantem Abgang                  "no514310
* und/oder Deaktivierung. Angleichung an die RASIMU-Logik von  "no514310
* HW501101.                                                    "no514310
  IF NOT ANLAV-GPLAB IS INITIAL AND                         "no514310
         PA_XGPLA NE SPACE AND                              "no514310
         PA_GSJHR GE TEMP_GJAHR AND                         "no514310
         ANLAV-DEAKT IS INITIAL .                           "no514310
*   Objekte mit geplantem Abgang werden unter obigen           "no514310
*   Bedingungen wie deaktivierte Anlagen behandelt.            "no514310
    ANLAV-DEAKT = ANLAV-GPLAB .                             "no514310
  ELSE.                                                     "no514310
    CLEAR ANLAV-GPLAB .                                     "no514310
  ENDIF.                                                    "no514310

* Anlagen, die vor GJ-Beginn deaktiviert wurden,
* dürfen nicht mehr mitmachen!
  IF NOT ANLAV-DEAKT IS INITIAL.
    CHECK ANLAV-DEAKT GE SAV_GJBEG .                        "no514310
  ENDIF.

* Simulation Anlagen zum Stand GJ-Beginn.                  " n. 359427
  IF PA_XGJBG    <> SPACE AND                               " n. 359427
     ANLAV-WRTTP =  '0'   .    " Anlage.                   " n. 359427
*    Anlagen, die erst nach dem GJ-Beginn zugegangen sind, " n. 359427
*    dürfen nicht mehr mitmachen!                          " n. 359427
    IF ANLAV-ZUGDT GE SAV_GJBEG.                            " n. 359427
      REJECT 'ANLAV'.                                       " n. 359427
    ENDIF.                                                  " n. 359427
  ENDIF.                                                    " n. 359427

* Tabelle T095B lesen.
  MOVE: T001-KTOPL  TO KEY_T095B-KTOPL,
        ANLAV-KTOGR TO KEY_T095B-KTOGR,
        SAV_PLL_LEADING_AREA TO KEY_T095B-AFABE.
  ON CHANGE OF KEY_T095B.
    SELECT SINGLE * FROM T095B
      WHERE KTOPL = T001-KTOPL
      AND   KTOGR = ANLAV-KTOGR
      AND   AFABE = SAV_PLL_LEADING_AREA.
    IF SY-SUBRC NE 0.
      PERFORM ABEND USING 'AB'         " F_MSGID
                          '029'        " F_MSGNO
                          T001-KTOPL                        " F_MSGV1
                          ANLAV-KTOGR                       " F_MSGV2
                          BEREICH1                          " F_MSGV3
                          ' '.                              " F_MSGV4
    ENDIF.
  ENDON.

* Interne Tabelle fuer Kostensammlung pro Anlage zuruecksetzen.
  REFRESH KOTAB. CLEAR KOTAB.

* Interne Tabellen/Feldleisten fuer FB-Call zuruecksetzen/fuellen.
  MOVE-CORRESPONDING ANLAV TO XANLA.
  REFRESH XANLZ. CLEAR XANLZ.
  REFRESH XANLB. CLEAR XANLB.
  REFRESH XANLC. CLEAR XANLC.
  REFRESH XANEP. CLEAR XANEP.                              "perf
  REFRESH XANEA. CLEAR XANEA.                              "perf
  REFRESH XANLP. CLEAR XANLP.                               "n.412316


GET ANLZ.

* ANLZ-Tabelle fuer FB-Call fuettern.
  MOVE ANLZ TO XANLZ. APPEND XANLZ.


GET ANLB.

  ZANLB = ANLB.                                            "perf

* ANLB-Tabelle fuer FB-Call fuettern.
  MOVE ANLB TO XANLB. APPEND XANLB.


GET ANLCV.

* der GET ANLCV kommt für alle angeforderten und konstituierenden
* Bereiche vorbei. Man will aber nur den angefoderten weiterverarbeiten
  IF ANLCV-AFABE = BEREICH1.                                "> 539545
    ZANLCV = ANLCV.                                          "perf
  ENDIF.                                                    "> 539545

  MOVE-CORRESPONDING ANLCV TO XANLC.
* XANLC so aendern, als waere noch kein AfA-Buchungslauf erfolgt.
  IF       ANLAV-WRTTP = '0'      AND                       " n. 359427
     ( NOT PA_FCWKG IS INITIAL OR                           " n. 359427
           PA_XGJBG <> SPACE   )  .                         " n. 359427
    PERFORM AFABUCHUNGEN_STORNIEREN.
  ELSE.                                                     " n. 304202
* REFRESH XANLP.                                           " n. 412316
    DELETE XANLP WHERE ANLN1 <> ANLCV-ANLN1 OR              " n. 412316
                       ANLN2 <> ANLCV-ANLN2 OR              " n. 412316
                       GJAHR <> ANLCV-GJAHR.                " n. 412316
  ENDIF.                                                    " n. 304202
* ANLC-Tabelle fuer FB-Call fuettern.
  APPEND XANLC.

* Pseudo-XANEP aufbauen (bei Plansätzen).
  IF ANLAV-WRTTP EQ '1' OR
     ANLAV-WRTTP EQ '2' OR
     ANLAV-WRTTP EQ '3' OR
     ANLAV-WRTTP EQ '4' .
*    REFRESH XANEP. CLEAR XANEP.                           "perf
    MOVE: ANLAV-MANDT    TO XANEP-MANDT,
          ANLAV-BUKRS    TO XANEP-BUKRS,
          ANLCV-GJAHR    TO XANEP-GJAHR,
          ANLB-AFABE     TO XANEP-AFABE,
          '00001'        TO XANEP-LNRAN,
          CON_ZUGANG_BWA TO XANEP-BWASL,
          ANLCV-ANSWL    TO XANEP-ANBTR.
    IF ANLB-INBDA BETWEEN SAV_GJBEG AND SAV_GJEND.
      MOVE ANLB-INBDA   TO XANEP-BZDAT.
    ELSE.
      MOVE SAV_GJBEG    TO XANEP-BZDAT.
    ENDIF.
    APPEND XANEP.
  ENDIF.

* Periodengenaue Planung ...                               "perf
* IF PA_FCWKG IS INITIAL.                                  "perf
*   ... dann periodengenaue Kostentabelle zu Anlage        "perf
*   PERFORM KOTAB_PER_AUFBAUEN.                            "perf
* Keine periodengenaue Planung ...                         "perf
* ELSE.                                                    "perf
*   ... dann nicht-periodengenaue Kostentabelle zu A       "perf
*   PERFORM KOTAB_AUFBAUEN.                                "perf
* ENDIF.                                                   "perf

GET ANLP.                                                   " n. 304202
* Periodengenaue Planung.                                  " n. 304202
  IF PA_FCWKG IS INITIAL AND                                " n. 359427
     PA_XGJBG =  SPACE   .                                  " n. 359427
*    ANLPs aus den bisherigen AfA-Buchungen sammeln.       " n. 304202
    APPEND ANLP TO XANLP.                                   " n. 304202
  ENDIF.                                                    " n. 304202


GET ANEK.                                                  "perf


GET ANEPV.                                                 "perf

* Anlage ...
* IF ANLAV-WRTTP EQ '0'.                                   "perf
  IF ANLAV-WRTTP EQ '0'     AND
     PA_XGJBG    IS INITIAL .
*    ... dann auch XANEP/XANEA hier einlesen.
    MOVE-CORRESPONDING: ANEK  TO XANEP,                   "perf
                        ANEPV TO XANEP.                   "perf
    APPEND XANEP.                                         "perf
    IF NOT XANEP-XANTW IS INITIAL.                        "perf
      MOVE-CORRESPONDING: ANEK  TO XANEA,                "perf
                          ANEPV TO XANEA.                "perf
      APPEND XANEA.                                      "perf
    ENDIF.                                                "perf
  ENDIF.                                                   "perf


GET ANLAV LATE.

* Bewertungsbereich inaktiv?                               "KP552
  IF XANLB[] IS INITIAL.                                    "KP552
    REJECT 'ANLAV'.                                         "KP552
  ENDIF.                                                    "KP552

* Rückladen der gemerkten Segmente.                        "perf
  ANLAV = ZANLAV.                                          "perf
  ANLB  = ZANLB.                                           "perf
  ANLCV = ZANLCV.                                          "perf
* Periodengenaue Planung ...                               "perf
  IF PA_FCWKG IS INITIAL.                                  "perf
*   ... dann periodengenaue Kostentabelle zu Anlage.       "perf
    PERFORM KOTAB_PER_AUFBAUEN.                            "perf
* Keine periodengenaue Planung ...                         "perf
  ELSE.                                                    "perf
*   ... dann nicht-periodengenaue Kostentabelle zu Anlage. "perf
    PERFORM KOTAB_AUFBAUEN.                                "perf
  ENDIF.                                                   "perf

  LOOP AT KOTAB.
**  Extract-Satz fuellen.  (für CO-Fortschreibung)              "alv
    PERFORM EXTRACT_DATEN.
  ENDLOOP.

*   interne Ausgabetabelle füllen                               "alv
  IF NOT KOTAB[] IS INITIAL.                                  "alv
    IF PA_SUMMB = ' '.
      PERFORM FILL_OUTTAB.                                  "alv
    ELSE.
      PERFORM FILL_OUTTAB_SUM.                              "alv
    ENDIF.
  ENDIF.                                                      "alv



END-OF-SELECTION.

*-------------------------------------------------------------"alv(beg)

* Gar keine Datensaetze gefunden.
  IF  RAREP-XNORCD = 'X'.
    MESSAGE S020(AB).
*Else.                                           " del     " n. 751720
* include rakopl_call_alv.                       " del     " n. 751720
  ENDIF.

*-------------------------------------------------------------"alv(end)


*---------------------------------------------------------------------*
* Extract-Bestand fuer Aufbau der Summentabelle sortieren.            *
*---------------------------------------------------------------------*

  SORT BY X-KOKRS X-BUKRS X-GSBER X-KOSTL X-CAUFN X-LSTAR X-KOART
          X-PERAF X-WRTTP X-LFDHI X-LFDNR.
*       X-PRNAM X-PRGJR X-PRPOS X-POSNR X-EAUFN X-ANLN0 X-ANLN2.

*---------------------------------------------------------------------*
* Summentabelle aufbauen.  (für Übergabe ans CO!)                     *
*---------------------------------------------------------------------*

  LOOP.

* Konsistenzpruefung: Bereichswaehrung = Objektwaehrung?
    AT NEW X-KOSTL.
*    Echte Kostenstelle.
      IF X-KOSTL NE CON_INITKOSTL.
*       Kostenstelle zum GJ-Ende lesen.
        SELECT * FROM CSKS
          WHERE KOKRS EQ X-KOKRS
          AND   KOSTL EQ X-KOSTL
          AND   DATBI GE SAV_GJEND
          AND   DATAB LE SAV_GJEND
          ORDER BY DATAB.
        ENDSELECT.
      ENDIF.
    ENDAT.

* Aufbau Summentabelle auf Periodenebene.
    AT END OF X-PERAF.
      MOVE:  'KOART'      TO SUM-STUFE,
             X-KOKRS      TO SUM-KOKRS,
             X-BUKRS      TO SUM-BUKRS,
             X-GSBER      TO SUM-GSBER,
             X-KOSTL      TO SUM-KOSTL,
             X-CAUFN      TO SUM-CAUFN,
             X-LSTAR      TO SUM-LSTAR,
             X-KOART      TO SUM-KOART,
             X-PERAF      TO SUM-PERAF,
             SUM(X-GKOST) TO SUM-GKOST,
             SUM(X-FKOST) TO SUM-FKOST.
      COLLECT SUM.
    ENDAT.

  ENDLOOP.

  PERFORM KOSTL_CAUFN_GUELTIGKEIT_MERKEN.                   " n. 168475


** Gar keine Datensaetze gefunden.
*IF RAREP-XNORCD = 'X'.
*  MESSAGE S020(AB).
*ENDIF.

*SORT SUM.

** Leistungsartensummen aus Verdichtung der Kostenartensummen.
*REFRESH HLP_SUM. CLEAR HLP_SUM.
**
*LOOP AT SUM
*  WHERE STUFE EQ 'KOART'.
**
*  MOVE:  'LSTAR'      TO HLP_SUM-STUFE,
*         SUM-KOKRS    TO HLP_SUM-KOKRS,
*         SUM-BUKRS    TO HLP_SUM-BUKRS,
*         SUM-GSBER    TO HLP_SUM-GSBER,
*         SUM-KOSTL    TO HLP_SUM-KOSTL,
*         SUM-CAUFN    TO HLP_SUM-CAUFN,
*         SUM-LSTAR    TO HLP_SUM-LSTAR.
*  CLEAR:                 HLP_SUM-KOART.
*  MOVE:  SUM-PERAF    TO HLP_SUM-PERAF,
*         SUM-GKOST    TO HLP_SUM-GKOST,
*         SUM-FKOST    TO HLP_SUM-FKOST.
*  COLLECT HLP_SUM.
*ENDLOOP.
**
*LOOP AT HLP_SUM.
*  MOVE HLP_SUM TO SUM.
*  APPEND SUM.
*ENDLOOP.
**
*SORT SUM.

** Innenauftragssummen aus Verdichtung der Leistungsartensummen.
*REFRESH HLP_SUM. CLEAR HLP_SUM.
**
*LOOP AT SUM
*  WHERE STUFE EQ 'LSTAR'.
**
*  MOVE:  'CAUFN'      TO HLP_SUM-STUFE,
*         SUM-KOKRS    TO HLP_SUM-KOKRS,
*         SUM-BUKRS    TO HLP_SUM-BUKRS,
*         SUM-GSBER    TO HLP_SUM-GSBER,
*         SUM-KOSTL    TO HLP_SUM-KOSTL,
*         SUM-CAUFN    TO HLP_SUM-CAUFN.
*  CLEAR:                 HLP_SUM-LSTAR,
**                        HLP_SUM-LSTAR,                          "Frank
*                         HLP_SUM-KOART.
*  MOVE:  SUM-PERAF    TO HLP_SUM-PERAF,
*         SUM-GKOST    TO HLP_SUM-GKOST,
*         SUM-FKOST    TO HLP_SUM-FKOST.
*  COLLECT HLP_SUM.
*ENDLOOP.
**
*LOOP AT HLP_SUM.
*  MOVE HLP_SUM TO SUM.
*  APPEND SUM.
*ENDLOOP.
**
*SORT SUM.

** Kostenstellensummen aus Verdichtung der Innenauftragssummen.
*REFRESH HLP_SUM. CLEAR HLP_SUM.
**
*LOOP AT SUM
*  WHERE STUFE EQ 'CAUFN'.
**
*  MOVE:  'KOSTL'      TO HLP_SUM-STUFE,
*         SUM-KOKRS    TO HLP_SUM-KOKRS,
*         SUM-BUKRS    TO HLP_SUM-BUKRS,
*         SUM-GSBER    TO HLP_SUM-GSBER,
*         SUM-KOSTL    TO HLP_SUM-KOSTL.
*  CLEAR:                 HLP_SUM-CAUFN,
*                         HLP_SUM-LSTAR,
*                         HLP_SUM-KOART.
*  MOVE:  SUM-PERAF    TO HLP_SUM-PERAF,
*         SUM-GKOST    TO HLP_SUM-GKOST,
*         SUM-FKOST    TO HLP_SUM-FKOST.
*  COLLECT HLP_SUM.
*ENDLOOP.
**
*LOOP AT HLP_SUM.
*  MOVE HLP_SUM TO SUM.
*  APPEND SUM.
*ENDLOOP.
**
*SORT SUM.

** Geschaeftsbereichsummen aus Verdichtung der Kostenstellensummen.
*REFRESH HLP_SUM. CLEAR HLP_SUM.
**
*LOOP AT SUM
*  WHERE STUFE EQ 'KOSTL'.
**
*  MOVE:  'GSBER'      TO HLP_SUM-STUFE,
*         SUM-KOKRS    TO HLP_SUM-KOKRS,
*         SUM-BUKRS    TO HLP_SUM-BUKRS,
*         SUM-GSBER    TO HLP_SUM-GSBER.
*  CLEAR:                 HLP_SUM-KOSTL,
*                         HLP_SUM-CAUFN,
*                         HLP_SUM-LSTAR,
*                         HLP_SUM-KOART.
*  MOVE:  SUM-PERAF    TO HLP_SUM-PERAF,
*         SUM-GKOST    TO HLP_SUM-GKOST,
*         SUM-FKOST    TO HLP_SUM-FKOST.
*  COLLECT HLP_SUM.
*ENDLOOP.
**
*LOOP AT HLP_SUM.
*  MOVE HLP_SUM TO SUM.
*  APPEND SUM.
*ENDLOOP.
**
*SORT SUM.

** Buchungskreissummen aus Verdichtung der Geschaeftsbereichsummen.
*REFRESH HLP_SUM. CLEAR HLP_SUM.
**
*LOOP AT SUM
*  WHERE STUFE EQ 'GSBER'.
**
*  MOVE:  'BUKRS'      TO HLP_SUM-STUFE,
*         SUM-KOKRS    TO HLP_SUM-KOKRS,
*         SUM-BUKRS    TO HLP_SUM-BUKRS.
*  CLEAR:                 HLP_SUM-GSBER,
*                         HLP_SUM-KOSTL,
*                         HLP_SUM-CAUFN,
*                         HLP_SUM-LSTAR,
*                         HLP_SUM-KOART.
*  MOVE:  SUM-PERAF    TO HLP_SUM-PERAF,
*         SUM-GKOST    TO HLP_SUM-GKOST,
*         SUM-FKOST    TO HLP_SUM-FKOST.
*  COLLECT HLP_SUM.
*ENDLOOP.
**
*LOOP AT HLP_SUM.
*  MOVE HLP_SUM TO SUM.
*  APPEND SUM.
*ENDLOOP.
**
*SORT SUM.

** Kostenrechnungskreissummen aus Verdichtung der Buchungskreissummen.
*REFRESH HLP_SUM. CLEAR HLP_SUM.
**
*LOOP AT SUM
*  WHERE STUFE EQ 'BUKRS'.
**
*  MOVE:  'KOKRS'      TO HLP_SUM-STUFE,
*         SUM-KOKRS    TO HLP_SUM-KOKRS.
*  CLEAR:                 HLP_SUM-BUKRS,
*                         HLP_SUM-GSBER,
*                         HLP_SUM-KOSTL,
*                         HLP_SUM-CAUFN,
*                         HLP_SUM-LSTAR,
*                         HLP_SUM-KOART.
*  MOVE:  SUM-PERAF    TO HLP_SUM-PERAF,
*         SUM-GKOST    TO HLP_SUM-GKOST,
*         SUM-FKOST    TO HLP_SUM-FKOST.
*  COLLECT HLP_SUM.
*ENDLOOP.
**
*LOOP AT HLP_SUM.
*  MOVE HLP_SUM TO SUM.
*  APPEND SUM.
*ENDLOOP.

*SORT SUM.

*---------------------------------------------------------------------*
* Extract-Bestand nach Primaerschluessel sortieren.                   *
*---------------------------------------------------------------------*

  SORT.

*---------------------------------------------------------------------*
* Bestand abarbeiten.                                                 *
*---------------------------------------------------------------------*

  LOOP.

    AT NEW X-BUKRS.
*    Echtlauf, dann Sammeltabellen für Kostenübergabe an's CO
*    zurücksetzen, da die Übergabe AT END OF BUKRS stattfindet.
      IF PA_TESTL EQ SPACE.
        REFRESH: T_RKU01G, T_RKU01JA.
      ENDIF.
    ENDAT.

** Geschaeftsbereich wechselt ==> Neue Seite anfangen.
*  AT NEW X-GSBER.
*     IF PA_SUMMB NE SPACE.
*        NEW-PAGE.
*     ENDIF.
*  ENDAT.

** Kostenstelle + Summe ausgeben.
** wenn beide Schalter Planung auf Kostenstelle/Innenauftrag      "Frank
** gesetzt sind, kann es sein, daß der Innenauftrag (x-caufn)
    "Frank
** gepflegt ist, aber die Kostenstelle (x-kostl) nicht ==>        "Frank
** diese Konstellation muß abgefangen werden, sonst findet
    "Frank
** die Unterroutine ,,perform kostl_ausgeben keine Daten          "Frank
*  AT NEW X-KOSTL.
*     IF PA_PLKST NE SPACE           "Planung auf Kostenstelle    "Frank
*        AND NOT X-KOSTL IS INITIAL.                              "Frank
**    if pa_plauf eq space.      "Planung auf Kostenstelle.       "Frank
**       PERFORM KOSTL_AUSGEBEN.
*     ENDIF.
*  ENDAT.

** Innenauftrag + Summe ausgeben.
** wenn beide Schalter Planung auf Kostenstelle/Innenauftrag      "Frank
** gesetzt sind, kann es sein, daß die Kostenstelle (x-kostl)
    "Frank
** gepflegt ist, aber der Innenauftrag (x-caufn) nicht ==>        "Frank
** diese Konstellation muß abgefangen werden, sonst findet
    "Frank
** die Unterroutine ,,perform caufn_ausgeben keine Daten          "Frank
*  AT NEW X-CAUFN.
*     IF PA_PLAUF NE SPACE           "Planung auf Innenauftrag    "Frank
*        AND NOT X-CAUFN IS INITIAL.                              "Frank
**     if pa_plauf ne space.      "Planung auf Innenauftrag.      "Frank
**       PERFORM CAUFN_AUSGEBEN.
*     ENDIF.
*  ENDAT.

** Leistungsart + Summe ausgeben.
*  AT NEW X-LSTAR.
*    IF PA_LSTAR NE SPACE.
**     PERFORM LSTAR_AUSGEBEN.
*    ENDIF.
**   Stufe 'Leistungsart' kommt vor Stufe 'Kostenart' ==>
**   Sorge dafür, daß bei einem Gruppenwechsle der Stufe 'Leistungsart'
**   auf jeden Fall das Schlüsselwort für Kostenart gesetzt wird,
**   auch wenn die Kostenart gar nicht wechselt.
*    IF PA_SUMMB NE SPACE.
*       CLEAR OLD_KOART.
*    ENDIF.
*  ENDAT.

** Kostenart + Summe ausgeben.
*  AT NEW X-KOART.
**   Summenbericht: Schluesselwort zu KoArt fuellen,
**   wenn neue Kostenart anfaengt.
*    IF PA_SUMMB NE SPACE.
*      CLEAR UEBS_052.
*      IF X-KOART NE OLD_KOART.
*         MOVE TEXT-052 TO UEBS_052.
*         OLD_KOART = X-KOART.
*      ENDIF.
**   Einzelbericht.
*    ELSE.
**     Schluesselwort zu KoArt immer fuellen.
*      MOVE TEXT-052 TO UEBS_052.
**     Beim Einzelbericht kommt die Stufe Kostenart stets als letzte
**     vor den Einzelsätzen (Anlagen, InvProgPopsitionen, PSPs, ... ).
**     Das Schlüsselwort für die entsprechende Objektart der
**     dieser Einzelsätze wird gesetzt, fall diese Objektart wechselt:
**     Daher dafür sorgen, daß das Schlüsselwort beim ersten Einzelsatz
**     der Gruppenstufe der aktuellen Kostenart gesetzt wird.
*      CLEAR OLD_OBART.
*    ENDIF.
**   PERFORM KOART_AUSGEBEN.
*  ENDAT.

*  AT NEW X-ANLN2.
**   Einzelbericht: Schlüsselwort zum Objekt genau dann füllen,
**   wenn Objektart wechselt.
*    IF PA_SUMMB EQ SPACE.
*      CLEAR UEBS_053.
*      IF X-OBART NE OLD_OBART.
*         CASE X-OBART.
*         WHEN 'IP'.
*           MOVE TEXT-056 TO UEBS_053.
*         WHEN 'PR'.
*           MOVE TEXT-055 TO UEBS_053.
*         WHEN 'OR'.
*           MOVE TEXT-054 TO UEBS_053.
*         WHEN 'IO'.
*           MOVE TEXT-057 TO UEBS_053.
*         WHEN OTHERS.
*           MOVE TEXT-053 TO UEBS_053.
*         ENDCASE.
*         OLD_OBART = X-OBART.
*      ENDIF.
*    ENDIF.
*  ENDAT.

** Periodentabelle fuer Anlage aufbauen.
*  IF PA_SUMMB EQ SPACE.
*    MOVE: X-PERAF TO ANL_KO-PERAF,
*          X-GKOST TO ANL_KO-GKOST,
*          X-FKOST TO ANL_KO-FKOST.
*    COLLECT ANL_KO.
*  ENDIF.

** Anlage + Werte ausgeben.
*  AT END OF X-ANLN2.
*    IF PA_SUMMB EQ SPACE.
**     PERFORM DATEN_AUSGEBEN.
*    ENDIF.
**   Periodentabelle fuer Anlage zuruecksetzen.
*    REFRESH ANL_KO. CLEAR ANL_KO.
*  ENDAT.

    AT END OF X-KOART.
*   Kosten an's CO uebergeben.
*   IF PA_TESTL EQ SPACE.                                  "msg
*     Periodengenaue Planung.
      IF PA_FCWKG IS INITIAL.
        PERFORM KOSTEN_PER_MERKEN.
*     Keine periodengenaue Planung.
      ELSE.
        PERFORM KOSTEN_MERKEN.
      ENDIF.
*   ENDIF.                                                 "msg
    ENDAT.

** Kostenstellen optisch trennen.
*  AT END OF X-KOSTL.
*     IF PA_PLKST NE SPACE.     " Planung auf Kostenstellen.      "Frank
**    if pa_plauf eq space.     " Planung auf Kostenstellen.      "Frank
*        ULINE.
*     ENDIF.
*  ENDAT.

** Innenaufträge optisch trennen.
*  AT END OF X-CAUFN.
*     IF PA_PLAUF NE SPACE.     " Planung auf Innenaufträge.
*        ULINE.
*     ENDIF.
*  ENDAT.

** Summe Geschaeftsbereich.
*  AT END OF X-GSBER.
**    PERFORM GSBER_SUMME.
*  ENDAT.

* Summe Buchungskreis.
    AT END OF X-BUKRS.
*    PERFORM BUKRS_SUMME.
*    Summe je Buchungskreis an's CO übergeben, da beim
*    Wechsel des Buchungskreis auch Bereichswährung wechseln könnte.
*    Werte werden sowohl beim Echtlauf als auch beim       "msg
*    Testlauf übergeben! Beim Testlauf wird nur kein       "msg
*    COMMIT gemacht, die CO-Schnittstelle wird jedoch      "msg
*    durchlaufen um mögliche Fehler zu protokollieren.     "msg
*    IF PA_TESTL EQ SPACE.                                 "msg
*       Periodengenaue Planung.
      IF PA_FCWKG IS INITIAL.
        PERFORM KOSTEN_PER_UEBERGEBEN.
*       Keine periodengenaue Planung.
      ELSE.
        PERFORM KOSTEN_UEBERGEBEN.
      ENDIF.
*    ENDIF.                                                "msg
     IF PA_TESTL EQ SPACE.                                 "ins n.176150
        COMMIT WORK AND WAIT.                              "ins n.176150
     ENDIF.                                                "ins n.176150
    ENDAT.

** Summe Kostenrechnungskreis.
*  AT END OF X-KOKRS.
**    PERFORM KOKRS_SUMME.
*  ENDAT.

* Wenn im Echtlauf alles ohne Fehler durchgegangen ist,
* dann kommt man bis hierhin und kann tatsächlich die Verbuchung
* anstoßen.
* Wenn hingegen irgendein schwerer Fehler passiert ist, gibts vorher
* schon einen Abbruch, so daß der gesamte Lauf wiederholt werden muss.
* Im Testlauf werden alle Fehler protokolliert, die        "msg
* innerhalb der CO-Schnittstelle (FBs                      "msg
* K_COSTS_PLAN_INTERFACE_PERIOD/TOTAL)                     "msg
* via Message-Sammler gesammelt wurden.                    "msg
    AT LAST.
      IF PA_TESTL EQ SPACE.
*        COMMIT WORK.                                      "del n.176150
      ELSE.                                                 "msg
*       flg_errprot = 'X'.                                 "alv
*       PERFORM TOP_OF_PAGE.                               "alv
*                                                " beg_del " n. 751720
*       PERFORM ERROR_PROTOCOL.                            "msg
*                                                " end_del " n. 751720
      ENDIF.
    ENDAT.

  ENDLOOP.

* << Start of addition on 10.17.2006 by Michelle
  TABLES ZTCODEPR.

  IF PA_TAB = 'X' AND NOT GT_OUTTAB[] IS INITIAL.
    DATA: L_TCNT TYPE I,
          L_ANSWER,
          L_ICNT TYPE I,
          L_CNT(6),
          L_SUBRC TYPE SYSUBRC VALUE 0,
          LT_ZTCODEPR TYPE TABLE OF ZTCODEPR WITH HEADER LINE.

    CLEAR: L_ICNT, L_CNT, LT_ZTCODEPR.
    REFRESH LT_ZTCODEPR.

*    SELECT COUNT(*) INTO L_TCNT
*      FROM ZTCODEPR
*     WHERE KOKRS = SO_KOKRS-LOW
*       AND BDATJ = PA_GSJHR.
*
*    IF L_TCNT > 0.
*      CLEAR: L_ANSWER, L_SUBRC.
*      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*        EXPORTING
*          TEXTLINE1 = 'Table ZTCODEPR has data. Do you want to delete?'
*          TITEL     = 'Check!'
*         CANCEL_DISPLAY = 'X'
*       IMPORTING
*         ANSWER = L_ANSWER.
*
*      IF L_ANSWER = 'J'.
    DELETE FROM ZTCODEPR WHERE KOKRS = BUKRS-LOW
                           AND BDATJ = PA_GSJHR.
    COMMIT WORK.
*        L_SUBRC = SY-SUBRC.
*      ENDIF.
*    ENDIF.
*
*    IF L_SUBRC = 0.
    LOOP AT GT_OUTTAB.
      L_ICNT = L_ICNT + 1.

      MOVE-CORRESPONDING GT_OUTTAB TO LT_ZTCODEPR.
      LT_ZTCODEPR-BDATJ = PA_GSJHR.
      LT_ZTCODEPR-AEDAT = SY-DATUM.
      LT_ZTCODEPR-AENAM = SY-UNAME.

      APPEND LT_ZTCODEPR.
      CLEAR LT_ZTCODEPR.
    ENDLOOP.

    INSERT ZTCODEPR FROM TABLE LT_ZTCODEPR.

    IF SY-SUBRC = 0.
      L_CNT = L_ICNT.
      MESSAGE ID 'ZMCO' TYPE 'S' NUMBER '000'
          WITH 'Saved' SY-DBCNT ' out of ' L_CNT.
    ENDIF.
  ENDIF.

* ENDIF.
* End of addition on 10.17.2006 by Michelle >>

* << Start of addition on 10.18.2006 by Michelle
* Disable Depreciation Simulation for Costing Plan
  PERFORM DEPR USING SPACE.
* End of addition on 10.18.2006 by Michelle >>

  INCLUDE RAKOPL_CALL_ALV.                                  " n. 751720

*------------delete alv
*TOP-OF-PAGE.

** Report-Header.
*  WRITE:  /001(18) TEXT-U04
*                   COLOR COL_BACKGROUND INTENSIFIED ON,
*           020(04) PA_GSJHR
*                   COLOR COL_BACKGROUND INTENSIFIED OFF,
*           036     HEAD_A
*                   COLOR COL_BACKGROUND INTENSIFIED OFF.
*  CNT_COUNT = SY-LINSZ - 35.
*  POSITION CNT_COUNT.
*  WRITE:     (18) TEXT-U02
*                  COLOR COL_BACKGROUND INTENSIFIED ON,
*             (10) SY-DATLO DD/MM/YYYY
*                  COLOR COL_BACKGROUND INTENSIFIED OFF.
*  CNT_COUNT = SY-LINSZ - 3.
*  POSITION CNT_COUNT.
*  WRITE      (04) SY-PAGNO NO-SIGN.
*  WRITE: /036     HEAD_B
*                  COLOR COL_BACKGROUND INTENSIFIED OFF.

** Diesen Teil des Seitenheaders nur bringen, wenn nicht    "msg
** gerade das Fehlerprotokoll ausgegeben wird.              "msg
*  IF FLG_ERRPROT EQ SPACE.                                 "msg
*     IF PA_SUMMB EQ SPACE.
** Ausgabe Kostenstelle in top-of-page <==>                       "Frank
**          Planung auf Innenauftrag und Kostenstelle gesetzt,    "Frank
**          aber nur kostenstelle gepflegt ist, bzw. beides nicht "Frank
**          gepflegt ist.                                         "Frank
**                            oder                                "Frank
**          nur Planung auf Kostenstelle ist gesetzt              "Frank
*        IF ( ( PA_PLKST NE SPACE AND PA_PLAUF NE SPACE )         "Frank
*               AND X-CAUFN IS INITIAL )                          "Frank
*                               OR                                "Frank
*           ( PA_PLKST NE SPACE AND PA_PLAUF EQ SPACE ).          "Frank
*           WRITE: /001(18) TEXT-U03                              "Frank
*                           COLOR COL_BACKGROUND INTENSIFIED ON,  "Frank
*                   020(04) X-KOKRS NO-GAP                        "Frank
*                           COLOR COL_BACKGROUND INTENSIFIED OFF, "Frank
*                   025(10) X-KOSTL NO-GAP                        "Frank
*                           COLOR COL_BACKGROUND INTENSIFIED OFF, "Frank
*                   036(40) CSKT-LTEXT NO-GAP                     "Frank
*                           COLOR COL_BACKGROUND INTENSIFIED OFF. "Frank
** Ausgabe Innenauftrag in top-of-page <==>                       "Frank
**           Planung auf Innenauftrag und Kostenstelle gesetzt,   "Frank
**           aber nur Innenauftrag gepflegt ist                   "Frank
**                             oder                               "Frank
**           nur Planung auf Innenauftrag ist gesetzt.            "Frank
*        ELSEIF ( ( PA_PLKST NE SPACE AND PA_PLAUF NE SPACE )     "Frank
*                   AND NOT X-CAUFN IS INITIAL )                  "Frank
*                               OR                                "Frank
*               ( PA_PLAUF NE SPACE AND PA_PLKST EQ SPACE ).      "Frank
*           WRITE: /001(18) TEXT-U05                              "Frank
*                           COLOR COL_BACKGROUND INTENSIFIED ON,  "Frank
*                   020(12) X-CAUFN NO-GAP                        "Frank
*                           COLOR COL_BACKGROUND INTENSIFIED OFF, "Frank
*                   036(40) AUFKV-KTEXT NO-GAP                    "Frank
*                           COLOR COL_BACKGROUND INTENSIFIED OFF. "Frank
*        ENDIF.                                                   "Frank

**       if pa_plauf eq space.     " Planung auf Kostenstelle.    "Frank
**          write: /001(18) text-u03                              "Frank
**                          color col_background intensified on,  "Frank
**                  020(04) x-kokrs no-gap                        "Frank
**                          color col_background intensified off, "Frank
**                  025(10) x-kostl no-gap                        "Frank
**                          color col_background intensified off, "Frank
**                  036(40) cskt-ltext no-gap                     "Frank
**                          color col_background intensified off. "Frank
**       else.                    " Planung auf Innenaufträge.    "Frank
**          write: /001(18) text-u05                              "Frank
**                          color col_background intensified on,  "Frank
**                  020(12) x-caufn no-gap                        "Frank
**                          color col_background intensified off, "Frank
**                  036(40) aufkv-ktext no-gap                    "Frank
**                          color col_background intensified off. "Frank
**       endif.                                                   "Frank
*     ENDIF.
*     CNT_COUNT = SY-LINSZ - 35.
*     POSITION CNT_COUNT.
*     WRITE:     (18) TEXT-U06
*                     COLOR COL_BACKGROUND INTENSIFIED ON,
*                (05) SAV_WAERS
*                     COLOR COL_BACKGROUND INTENSIFIED OFF.
*     ULINE.

** Gruppenstufe.
*     FORMAT COLOR COL_BACKGROUND INTENSIFIED ON.
*     WRITE: /001(20) TEXT-100,
*             022(20) TEXT-101,
*             043(20) TEXT-102.
*     FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
*     WRITE: /001(20) X-KOKRS,
*             022(20) X-BUKRS,
*             043(20) X-GSBER.
*     ULINE.
** Spaltenueberschriften.
** Von-Periode < Bis-Periode  u n d  periodengenaue Planung ...
*     IF PA_VONPE LT PA_BISPE AND
*        PA_FCWKG IS INITIAL  .
**   ... dann zwei Wertfeldkolonnen.
*       WRITE: /001     SY-VLINE,
*               002(41) SPACE NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,

*               043     SY-VLINE,
*               044(03) UEBS_201 NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               047(01) SPACE NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               048(16) TEXT-202 NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               064(02) SPACE NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               066(16) UEBS_203 NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               082(01) SPACE NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               083     SY-VLINE,
*               084(03) UEBS_201 NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               087(01) SPACE NO-GAP

*                       COLOR COL_HEADING INTENSIFIED ON,
*               088(16) TEXT-202 NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               104(02) SPACE NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               106(16) UEBS_203 NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               122     SPACE NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               123     SY-VLINE.
** Ansonsten nur eine Wertfeldkolonne.
*     ELSE.
*       WRITE: /001     SY-VLINE,
*               002(41) SPACE NO-GAP
*                      COLOR COL_HEADING INTENSIFIED ON,
*               043     SY-VLINE,
*               044(39) SPACE NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               083     SY-VLINE,
*               084(03) UEBS_201 NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               087(01) SPACE NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               088(16) TEXT-202 NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               104(02) SPACE NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               106(16) UEBS_203 NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               122     SPACE NO-GAP
*                       COLOR COL_HEADING INTENSIFIED ON,
*               123     SY-VLINE.
*     ENDIF.

*  ENDIF.                                                   "msg

*  ULINE.
*---------end delete alv

*---------------------------------------------------------------alv(beg)
* Allgemeine Form-Routinen
*include rasort_alv_misc.

* Form-Routinen für den ABAP List Viewer
*include rasort_alv_prepare_table.
*include rasort_alv_tools.

* Forms
  INCLUDE RAKOPL02_FORM.
*include t_rakopl02_form.
*&---------------------------------------------------------------------*
*&      Form  DEPR
*&---------------------------------------------------------------------*
*       Depreciation Simulation for Costing Plan
*----------------------------------------------------------------------*
FORM DEPR USING P_SET.
  TABLES T093.

  DATA: L_AFABE  TYPE AFABE_D,
        L_AFAPL  TYPE AFAPL,
        L_AFABER TYPE AFABER.

  CLEAR: L_AFABE,L_AFAPL, L_AFABER.

  SELECT SINGLE AFABE INTO L_AFABE FROM T093A
      WHERE AFAPL = T093C-AFAPL
      AND BERTYP = '07'.    " Costing

  CHECK SY-SUBRC = 0.
  SELECT SINGLE AFAPL INTO L_AFAPL FROM T093
  WHERE AFAPL = T093C-AFAPL
    AND AFABER = L_AFABE.

* Ready for Depreciation Simulation for Costing Plan
  IF P_SET = 'X'.
*    T093-BUHBKT = '0'.                                     "UD1K951480
*    MODIFY T093.                                           "UD1K951480

* XRESTV  Smoothing (deprec. for past) when posting depreciation
* AFBRHY  Number of periods between two depreciation runs
* AFBKST  Indicator: Depreciation posting with cost center
    T093D-BUKRS  = BUKRS-LOW.
    T093D-AFABER = L_AFABE.
    T093D-XRESTV = 'X'.
    T093D-AFBRHY = '1'.
    T093D-AFBKST = 'X'.
    MODIFY T093D.
* by ig.moon 2008/11/21{
    COMMIT WORK.
* }

* Disable for Depreciation Simulation for Costing Plan
  ELSE.
    SELECT SINGLE AFABER INTO L_AFABER FROM T093D
    WHERE BUKRS  = BUKRS-LOW
      AND AFABER = L_AFABE.

    IF SY-SUBRC = 0.
      DELETE FROM T093D
        WHERE BUKRS  = BUKRS-LOW
          AND AFABER = L_AFABE.

* by ig.moon 2008/11/21{
    COMMIT WORK.
* }

    ENDIF.

  ENDIF.

ENDFORM.                    " DEPR
*&---------------------------------------------------------------------*
*&      Form  GET_TAB_STCKAFA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TAB_STCKAFA .

* Alternative: Ermittlung Stückzahlafaschlüssel über
* FB "AA_DEPR_KEY_IS_UOP?".

  RANGES: LRG_AFARSL FOR T090-AFARSL.

  DATA: L_AFARSL LIKE T090-AFARSL.

  REFRESH TAB_STCKAFA.

* AfA-Rechenschlüssel mit Stückzahl-Methode.
  SELECT AFARSL   FROM T090
         INTO     L_AFARSL
         WHERE  ( AFAMET EQ 'S' OR
                  AFAMET EQ 'T' ).
         LRG_AFARSL-LOW    = L_AFARSL.
         LRG_AFARSL-OPTION = 'EQ'.
         LRG_AFARSL-SIGN   = 'I'.
         APPEND LRG_AFARSL.
  ENDSELECT.

* Alle AfA-Schlüssel des Bewertungsplans selektieren,
* die einen Stückzahl-Rechenschlüssel benutzen.
  SELECT AFASL FROM T090C
         INTO  CORRESPONDING FIELDS OF TABLE TAB_STCKAFA
         WHERE AFAPL  EQ T093C-AFAPL
         AND   NAFASL IN LRG_AFARSL.

* AfA-Rechenschlüssel mit Stückzahl-Methode.          " < 3.0
* SELECT * FROM T090
*        WHERE  ( AFAMET EQ 'S' OR
*                 AFAMET EQ 'T' ).
*        LRG_AFARSL-LOW    = T090-AFARSL.
*        LRG_AFARSL-OPTION = 'EQ'.
*        LRG_AFARSL-SIGN   = 'I'.
*        APPEND LRG_AFARSL.
* ENDSELECT.

* Alle AfA-Schlüssel des Bewertungsplans selektieren, " < 3.0
* die einen Stückzahl-Rechenschlüssel benutzen.
* SELECT * FROM T090C
*        WHERE AFAPL  EQ T093C-AFAPL
*        AND   NAFASL IN LRG_AFARSL.
*        TAB_STCKAFA-AFASL = T090C-AFASL.
*        APPEND TAB_STCKAFA.
* ENDSELECT.

  SORT TAB_STCKAFA BY AFASL.

ENDFORM.                    " GET_TAB_STCKAFA
*&---------------------------------------------------------------------*
*&      Form  KOSTL_CAUFN_GUELTIGKEIT_MERKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form KOSTL_CAUFN_GUELTIGKEIT_MERKEN .

  DATA: L_SUBRC LIKE SY-SUBRC,                             " n. 168475
        L_TABIX LIKE SY-TABIX.                             " n. 168475
                                                           " n. 168475
* Maximale Von-/Bis-Gültigkeitsperioden                    " n. 168475
* von Kostenstellen und Innenaufträgen ermitteln.          " n. 168475
  LOOP AT SUM.                                             " n. 168475
*                                                          " n. 168475
    IF NOT SUM-KOSTL IS INITIAL       AND                  " n. 168475
           SUM-KOSTL <> CON_INITKOSTL .                    " n. 168475
       READ TABLE T_KOSTL_G                                " n. 168475
            WITH KEY KOKRS = SUM-KOKRS                     " n. 168475
                     KOSTL = SUM-KOSTL                     " n. 168475
            BINARY SEARCH.                                 " n. 168475
       L_SUBRC = SY-SUBRC.                                 " n. 168475
       L_TABIX = SY-TABIX.                                 " n. 168475
       IF L_SUBRC = 0.                                     " n. 168475
          IF T_KOSTL_G-PERAB > SUM-PERAF.                  " n. 168475
             T_KOSTL_G-PERAB = SUM-PERAF.                  " n. 168475
             MODIFY T_KOSTL_G INDEX L_TABIX.               " n. 168475
          ENDIF.                                           " n. 168475
          IF T_KOSTL_G-PERBI < SUM-PERAF.                  " n. 168475
             T_KOSTL_G-PERBI = SUM-PERAF.                  " n. 168475
             MODIFY T_KOSTL_G INDEX L_TABIX.               " n. 168475
          ENDIF.                                           " n. 168475
       ELSE.                                               " n. 168475
          CLEAR T_KOSTL_G.                                 " n. 168475
          T_KOSTL_G-KOKRS = SUM-KOKRS.                     " n. 168475
          T_KOSTL_G-KOSTL = SUM-KOSTL.                     " n. 168475
          T_KOSTL_G-PERAB = SUM-PERAF.                     " n. 168475
          T_KOSTL_G-PERBI = SUM-PERAF.                     " n. 168475
          INSERT T_KOSTL_G INDEX L_TABIX.                  " n. 168475
       ENDIF.                                              " n. 168475
    ENDIF.                                                 " n. 168475
*                                                          " n. 168475
    IF NOT SUM-CAUFN IS INITIAL       AND                  " n. 168475
           SUM-CAUFN <> CON_INITCAUFN .                    " n. 168475
       READ TABLE T_CAUFN_G                                " n. 168475
            WITH KEY KOKRS = SUM-KOKRS                     " n. 168475
                     CAUFN = SUM-CAUFN                     " n. 168475
            BINARY SEARCH.                                 " n. 168475
       L_SUBRC = SY-SUBRC.                                 " n. 168475
       L_TABIX = SY-TABIX.                                 " n. 168475
       IF L_SUBRC = 0.                                     " n. 168475
          IF T_CAUFN_G-PERAB > SUM-PERAF.                  " n. 168475
             T_CAUFN_G-PERAB = SUM-PERAF.                  " n. 168475
             MODIFY T_CAUFN_G INDEX L_TABIX.               " n. 168475
          ENDIF.                                           " n. 168475
          IF T_CAUFN_G-PERBI < SUM-PERAF.                  " n. 168475
             T_CAUFN_G-PERBI = SUM-PERAF.                  " n. 168475
             MODIFY T_CAUFN_G INDEX L_TABIX.               " n. 168475
          ENDIF.                                           " n. 168475
       ELSE.                                               " n. 168475
          CLEAR T_CAUFN_G.                                 " n. 168475
          T_CAUFN_G-KOKRS = SUM-KOKRS.                     " n. 168475
          T_CAUFN_G-CAUFN = SUM-CAUFN.                     " n. 168475
          T_CAUFN_G-PERAB = SUM-PERAF.                     " n. 168475
          T_CAUFN_G-PERBI = SUM-PERAF.                     " n. 168475
          INSERT T_CAUFN_G INDEX L_TABIX.                  " n. 168475
       ENDIF.                                              " n. 168475
    ENDIF.                                                 " n. 168475
*                                                          " n. 168475
  ENDLOOP.                                                 " n. 168475
                                                           " n. 168475

endform.                    " KOSTL_CAUFN_GUELTIGKEIT_MERKEN
