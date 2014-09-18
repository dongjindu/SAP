************************************************************************
* Program Name      : ZACO38R_DEPR
* Author            : Hyung Jin Youn
* Creation Date     : 15/09/2003
* Specifications By : Jin-Won Hong
* Pattern           : Report 1-1
* Development Request No: UD1K902180
* Add documentation :
* Description       : COPY from S_ALR_87099918
*                   - Primary Cost Planning: Depreciation/Interest
*                   - Program ID RAKOPL02 (except display logic)
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT RAKOPL02 MESSAGE-ID AB
                LINE-SIZE 123
                NO STANDARD PAGE HEADING.

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
INCLUDE RAKOPL02_SEL.
*include t_rakopl02_sel.

* Data's und Forms für ALV-Anbindung                             "alv
*Include ZACO38L_ALVD.
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

  CHECK SO_KOKRS.

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

  ZANLCV = ANLCV.                                          "perf

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
    MESSAGE E020(AB).
  ELSE.
    CLEAR  GT_OUTTAB.
*  include rakopl_call_alv.
  ENDIF.

**// Mod. insertion 2003.09.15 by Hyung Jin Youn
* For DATA transferring
  EXPORT GT_OUTTAB = GT_OUTTAB TO MEMORY ID 'ZMIDCO_001'.
**   The logic of displaying ALV list is removed .
**// End of Mod.

* Forms
  INCLUDE RAKOPL02_FORM.
