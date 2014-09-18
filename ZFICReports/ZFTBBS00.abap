REPORT RFTBBS00 MESSAGE-ID T0 NO STANDARD PAGE HEADING LINE-SIZE 155.
************************************************************************
* Datenbank-Tabellen
************************************************************************
TABLES:
* Steuertabellen:
  T100,                                " Nachrichten
  TZB0T,                               " Bezeichnung der Bewegungsart
* Finanzgesch?t:
  VTBFHA,                              " Gesch?t
  VTBFHAZU,                            " Vorgang
  VTBFHAPO,                            " Bewegung
  BHDGD.                               " Batch-Heading

************************************************************************
* Konstanten
************************************************************************
CONSTANTS:
* Boole'sche Variablen:
  XTRUE(1)  TYPE C VALUE 'X',
  XFALSE(1) TYPE C VALUE ' ',
* Transaktionscode:
  TRANSAKTION LIKE SY-TCODE VALUE 'TBB2',
* Berechtigungspr?ung:
  C_TRFCT LIKE AUTHA-TRFCT VALUE '03', " Funktion: Buchung
* Anwendungen:
  ANWENDUNG_WERTPAPIER  LIKE TZPA-RANTYP VALUE '2',
  ANWENDUNG_DEVISEN     LIKE TZPA-RANTYP VALUE '4',
  ANWENDUNG_GELDHANDEL  LIKE TZPA-RANTYP VALUE '5',
  ANWENDUNG_DERIVATIVE  LIKE TZPA-RANTYP VALUE '6',
* Buchungsstatus der Bewegung:
  BUCHUNG_DURCHGEFUEHRT        LIKE VTBFHAPO-SBEWEBE VALUE '2',
  BUCHUNG_STORNO_VORGEMERKT    LIKE VTBFHAPO-SBEWEBE VALUE '3',
  BUCHUNG_STORNO_DURCHGEFUEHRT LIKE VTBFHAPO-SBEWEBE VALUE '4',
* Status des automatischen Belegstorno:
  BELEGSTORNO_PROBLEMLOS LIKE VTBFHAPO-SSTORNOMAN VALUE ' ',
  BELEGSTORNO_FEHLSCHLAG LIKE VTBFHAPO-SSTORNOMAN VALUE '1'.

************************************************************************
* Variablen
************************************************************************
* Flags:
* Steuerung der Ausgabe des Seitenkopfes ('B' = Buchungen, 'F' = Fehler)
DATA FLG_SEITENKOPF(1) TYPE C VALUE 'B'.
* Ausgabe mindestens einer Bewegung erfolgt:
DATA FLG_BWG_AUSGEGEBEN LIKE XTRUE.

* Berechtigungspr?ung: Aktivit?
DATA H_ACTVT LIKE TACT-ACTVT.

* Hilfsvariablen:
DATA HLP_SUBRC LIKE SY-SUBRC.

* Identifikation des zuletzt ausgegebenen Gesch?ts:
DATA: BEGIN OF SAV_AUSGABE,
        BUKRS LIKE VTBFHA-BUKRS,
        RFHA  LIKE VTBFHA-RFHA,
      END OF SAV_AUSGABE.

* Text der Fehlernachricht:
* (Nicht in der Struktur, um interne Tabelle nicht zu belasten!)
DATA NACHRICHT_TEXT LIKE T100-TEXT.

* Hide-Bereich:
DATA: BEGIN OF HIDE_BEREICH.
        INCLUDE STRUCTURE VTBMSGPOST.
DATA:   TEXT  LIKE NACHRICHT_TEXT,
      END OF HIDE_BEREICH.

************************************************************************
* Interne Tabellen
************************************************************************
DATA:
* Arbeitstabelle der Gesch?te:
  I_FHA LIKE SAV_AUSGABE OCCURS 100 WITH HEADER LINE,
* Arbeitstabelle der zu buchenden Bewegungen zu einem Gesch?t:
  I_FHAPO LIKE VTBFHAPO OCCURS 50 WITH HEADER LINE,
* Fehlernachrichten:
  I_NACHRICHT LIKE VTBMSGPOST OCCURS 100 WITH HEADER LINE.

************************************************************************
* Selektionskriterien
************************************************************************
* Anwendung:
SELECTION-SCREEN BEGIN OF BLOCK APPLICATION WITH FRAME TITLE TEXT-S01.
PARAMETERS:
  P_TX     LIKE VTBLDB-SFOREX          " ??
           DEFAULT 'X',
  P_TM     LIKE VTBLDB-SMONEY          " ????
           DEFAULT 'X',
  P_TI     LIKE VTBLDB-SDERIVA         " ??????
           DEFAULT 'X',
  P_TS     LIKE VTBLDB-SSECURITY       " ????
           DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK APPLICATION.

* Allgemeine Abgrenzungen:
SELECTION-SCREEN BEGIN OF BLOCK COMMON WITH FRAME TITLE TEXT-C01.
SELECT-OPTIONS:
  S_BUKRS  FOR VTBFHA-BUKRS            " ????
           MEMORY ID BUK
           NO INTERVALS,
  S_RFHA   FOR VTBFHA-RFHA             " ????
           MATCHCODE OBJECT VTBA,
  S_SGSART FOR VTBFHA-SGSART           " ??????
           NO INTERVALS,
  S_SFHAAR FOR VTBFHA-SFHAART          " ????
           NO INTERVALS,
  S_RPORTB FOR VTBFHA-RPORTB           " Portfolio
           NO INTERVALS.
SELECTION-SCREEN END OF BLOCK COMMON.

* Buchungssteuerung:
SELECTION-SCREEN BEGIN OF BLOCK CONTROL WITH FRAME TITLE TEXT-CTR.
PARAMETERS:
  P_STGRD  LIKE BKPF-STGRD             " ?????
                OBLIGATORY,
  P_BUDAT  LIKE BKPF-BUDAT obligatory, " ???
  P_MONAT  LIKE BKPF-MONAT obligatory, " ????
  P_TEST   LIKE VTBPOSTING-STEST AS CHECKBOX  " ?????
           DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK CONTROL.

* Anwendung:
RANGES R_RANTYP FOR VTBFHA-RANTYP.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
* Berechtigungspr?ung:
  AUTHORITY-CHECK OBJECT 'F_T_TRANSB'
                  ID     'TCD'
                  FIELD  TRANSAKTION.
  IF SY-SUBRC <> 0.
    MESSAGE S172(00) WITH TRANSAKTION.
    EXIT.
  ENDIF.
* Berechtigungspr?ung: Setzen der Aktivit?
  IF P_TEST = XTRUE.
    H_ACTVT = '48'.                    " Aktivit?: Simulieren
  ELSE.
    H_ACTVT = '85'.                    " Aktivit?: Stornieren
  ENDIF.
* Aufbau des Range zur Selektion der Anwendungen:
  R_RANTYP-SIGN   = 'I'.
  R_RANTYP-OPTION = 'EQ'.
  IF P_TS = XTRUE.                     " ????(????:2)
    R_RANTYP-LOW = ANWENDUNG_WERTPAPIER.
    APPEND R_RANTYP.
  ENDIF.
  IF P_TX = XTRUE.                                          " ??(4)
    R_RANTYP-LOW = ANWENDUNG_DEVISEN.
    APPEND R_RANTYP.
  ENDIF.
  IF P_TM = XTRUE.                     " ????(5)
    R_RANTYP-LOW = ANWENDUNG_GELDHANDEL.
    APPEND R_RANTYP.
  ENDIF.
  IF P_TI = XTRUE.                     " ??????(6)
    R_RANTYP-LOW = ANWENDUNG_DERIVATIVE.
    APPEND R_RANTYP.
  ENDIF.
* Abbruch, falls keine Anwendung ausgew?lt worden ist:
  IF P_TX = XFALSE AND P_TM = XFALSE AND
     P_TI = XFALSE AND P_TS = XFALSE.
    PERFORM MELDUNG_ERFOLGLOSE_SELEKTION.
  ENDIF.
* Testlauf-Kennzeichen auf Boole'schen Wert setzen (zur Sicherheit):
  IF NOT P_TEST = XFALSE.
    P_TEST = XTRUE.
  ENDIF.
* Aufbau des Batch-Heading:
  BHDGD-INIFL = 0.          "?????
  BHDGD-LINE1 = SY-TITLE.   "??? ???
  BHDGD-LINES = SY-LINSZ.   "?????,?????
  BHDGD-REPID = SY-REPID.
  BHDGD-UNAME = SY-UNAME.
  BHDGD-MANDT = SY-MANDT.
  BHDGD-DATUM = SY-DATUM.
  BHDGD-ZEIT  = SY-UZEIT.

  CALL FUNCTION 'TB_PROGRESS_INDICATOR'  "??? ??????
       EXPORTING
            ID     = 'SELECTION'
       EXCEPTIONS
            OTHERS = 0.
  PERFORM BEWEGUNGEN_LESEN.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  SET PF-STATUS 'STANDARD'.
  PERFORM BEWEGUNGEN_BEARBEITEN.

************************************************************************
* TOP-OF-PAGE
************************************************************************
TOP-OF-PAGE.
  PERFORM SEITENKOPF_AUSGEBEN.

************************************************************************
* AT LINE-SELECTION
************************************************************************
*AT LINE-SELECTION.
*  PERFORM USER-COMMAND USING 'PICK'.

************************************************************************
* AT USER-COMMAND
************************************************************************
*AT USER-COMMAND.
*  PERFORM USER-COMMAND USING SY-UCOMM.

*----------------------------------------------------------------------*
*       Form  BEWEGUNGEN_LESEN
*----------------------------------------------------------------------*
FORM BEWEGUNGEN_LESEN.
  SELECT DISTINCT VTBFHA~BUKRS VTBFHA~RFHA
         INTO CORRESPONDING FIELDS OF TABLE I_FHA
         FROM ( VTBFHA INNER JOIN VTBFHAPO
                       ON VTBFHA~BUKRS = VTBFHAPO~BUKRS AND
                          VTBFHA~RFHA  = VTBFHAPO~RFHA )
*        Felder aus Gesch?t:
         WHERE  VTBFHA~BUKRS   IN S_BUKRS
         AND    VTBFHA~RFHA    IN S_RFHA
         AND    VTBFHA~RANTYP  IN R_RANTYP
         AND    VTBFHA~SGSART  IN S_SGSART
         AND    VTBFHA~SFHAART IN S_SFHAAR
         AND    VTBFHA~RPORTB  IN S_RPORTB
*        Felder aus Bwg. (beim Zugriff auf VTBFHAPO analog; s.u.):
         AND    VTBFHAPO~DBUCHUNG =  P_BUDAT        "???
         AND    VTBFHAPO~SBKKLAS  =  1              "structure
         AND    VTBFHAPO~SBEWEBE  =  2.             "????
  SORT I_FHA BY BUKRS RFHA.
* Beenden der Verarbeitung mit ensprechender Meldung, falls keine Bwg.
* zur Bearbeitung gefunden worden ist:
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM MELDUNG_ERFOLGLOSE_SELEKTION.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  MELDUNG_ERFOLGLOSE_SELEKTION
*----------------------------------------------------------------------*
FORM MELDUNG_ERFOLGLOSE_SELEKTION.
* Im Batch:
  IF NOT SY-BATCH IS INITIAL.
    SELECT SINGLE * FROM  T100
           WHERE  SPRSL       = SY-LANGU   "???
           AND    ARBGB       = 'T0'       "????????
           AND    MSGNR       = '917'.     "?????
    IF SY-SUBRC <> 0.
      CLEAR T100.
    ENDIF.
    SKIP 5.
    WRITE: / T100-TEXT.
    STOP.
* Im Online:
  ELSE.
    MESSAGE S917(T0).
    STOP.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  BEWEGUNGEN_BEARBEITEN
*----------------------------------------------------------------------*
FORM BEWEGUNGEN_BEARBEITEN.
  CALL FUNCTION 'TB_PROGRESS_INDICATOR'
       EXPORTING
            ID     = 'WORK'
       EXCEPTIONS
            OTHERS = 0.
  LOOP AT I_FHA.
    HLP_SUBRC = 0.
    SELECT SINGLE * FROM VTBFHA
                    WHERE BUKRS = I_FHA-BUKRS
                      AND RFHA  = I_FHA-RFHA.   "????
    IF NOT SY-SUBRC IS INITIAL.
      MESSAGE A999(T0)
              WITH SY-CPROG 'bewegungen_bearbeiten' 'VTBFHA'.
    ENDIF.
    CALL FUNCTION 'TB_DEAL_AUTHORITY_CHECK'
         EXPORTING
              BUKRS   = VTBFHA-BUKRS
              SGSART  = VTBFHA-SGSART
              SFHAART = VTBFHA-SFHAART
              RPORTB  = VTBFHA-RPORTB
              RLDEPO  = VTBFHA-RLDEPO
              TBEGRU  = VTBFHA-TBEGRU
              TRFCT   = C_TRFCT
              ACTVT   = H_ACTVT
         EXCEPTIONS
              OTHERS  = 1.
    IF SY-SUBRC <> 0.
      CALL FUNCTION 'TB_POSTING_MESSAGE_STORE'
           EXPORTING
                DEAL     = VTBFHA
                FLG_DEAL = XTRUE
                FLOW     = VTBFHAPO
                MSGID    = SY-MSGID
                MSGNO    = SY-MSGNO
                MSGTY    = SY-MSGTY
                MSGV1    = SY-MSGV1
                MSGV2    = SY-MSGV2
                MSGV3    = SY-MSGV3
                MSGV4    = SY-MSGV4
           TABLES
                MSGPOST  = I_NACHRICHT.
      CONTINUE.
    ENDIF.
    SELECT * FROM VTBFHAPO INTO TABLE I_FHAPO
     WHERE BUKRS       = I_FHA-BUKRS
       AND   RFHA        = I_FHA-RFHA
       AND   DBUCHUNG    = P_BUDAT
       AND   SBKKLAS     = 1    "structure
       AND   SBEWEBE     = 2. "BUCHUNG_STORNO_VORGEMERKT.
    IF NOT SY-SUBRC IS INITIAL.
      CONTINUE.
    ENDIF.
    PERFORM BEWEGUNGEN_STORNIEREN USING VTBFHA-SGSART VTBFHA-SFHAART.
    AT LAST.
      IF FLG_BWG_AUSGEGEBEN = XTRUE.
        ULINE.
      ENDIF.
    ENDAT.
  ENDLOOP.

  CALL FUNCTION 'TB_PROGRESS_INDICATOR'
       EXPORTING
            ID     = 'OUTPUT'
       EXCEPTIONS
            OTHERS = 0.
  PERFORM FEHLERNACHRICHTEN_AUSGEBEN.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  BEWEGUNGEN_STORNIEREN
*----------------------------------------------------------------------*
FORM BEWEGUNGEN_STORNIEREN USING PRODUKTART GESCHAEFTSART.
* Da die zu 4.6A in die VTBFHA* migrierten WP-Deals mehrere
* Bewegungen zu einer Order/Vertrag im selben Beleg unter einem
* AWKEY buchen konnten (z.B. Zahlbetrag und Stueckzinsen)
* mu?darauf geachtet werden, da?1 Beleg nur einmal storniert wird.
* Natuerlich muessen alle Bewegungen im TR auf '4' storniert gesetzt
* werden.
  DATA: BEGIN OF L_WRK_POSTKEY,
            AWKEY TYPE AWKEY,          "/k?ftig
            BELNR TYPE BELNR_D,        "/derzeit
        END   OF L_WRK_POSTKEY,
        L_TAB_POSTKEY LIKE L_WRK_POSTKEY OCCURS 0.

* Bei Echtlauf Bewegungen bis zum ersten Fehlers stornieren:
  IF P_TEST = XFALSE.
    CLEAR: L_WRK_POSTKEY, L_TAB_POSTKEY.
    LOOP AT I_FHAPO.
      UPDATE BKPF SET : AWTYP = SPACE
                        AWKEY = SPACE
           WHERE BUKRS = I_FHAPO-BUKRS
           AND   BELNR = I_FHAPO-BELNR
           AND   GJAHR = I_FHAPO-GJAHR.
      COMMIT WORK.

      CALL FUNCTION 'Z_TR_FBRA_POSTING'
           EXPORTING
                I_BUKRS           = I_FHAPO-BUKRS
                I_GJAHR           = I_FHAPO-GJAHR
                I_AUGBL           = I_FHAPO-BELNR
                I_STGRD           = P_STGRD
           EXCEPTIONS
                NOT_POSSIBLE_FBRA = 1
                NOT_POSSIBLE_FB08 = 2
                OTHERS            = 3.
  IF SY-SUBRC <> 0.
    RAISE NOT_POSSIBLE_FB08.
  ENDIF.
      IF SY-SUBRC <> 0.
        WRITE: '***reverse error'.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        I_FHAPO-SBEWEBE = '1'.  " posting ready
        CLEAR: I_FHAPO-BELNR,
               I_FHAPO-GJAHR,
               I_FHAPO-DBUCHUNG,
               I_FHAPO-AWKEY.
        VTBFHAPO = I_FHAPO.
        UPDATE VTBFHAPO.
        WRITE: ' reversed...'.
      ENDIF.
    ENDLOOP.
  ELSE.
*   Bei Testlauf alle zu stornierenden Bewegungen ausgeben:
    LOOP AT I_FHAPO.
      CLEAR HLP_SUBRC.
      "/Zahl.anordnung stornieren - simulativ! (XTRUE):
      IF  NOT I_FHAPO-PRKEY IS INITIAL
      AND     I_FHAPO-SSTORNOMAN = BELEGSTORNO_PROBLEMLOS.
        PERFORM ZAHLUNGSANFORDERUNG_STORNIEREN
                USING    I_FHAPO XTRUE
                CHANGING HLP_SUBRC.
      ENDIF.
*     /Beleg im FI stornieren -  simulativ! (XTRUE):
      IF HLP_SUBRC = 0 AND NOT I_FHAPO-BELNR IS INITIAL.
        PERFORM BELEG_STORNIEREN
                USING    I_FHAPO XTRUE
                CHANGING HLP_SUBRC.
      ENDIF.
      IF HLP_SUBRC = 0.
        PERFORM BUCHUNGSINFO_AUSGEBEN USING I_FHAPO
                                            PRODUKTART
                                            GESCHAEFTSART.
      ENDIF.
    ENDLOOP.
  ENDIF.                               "/Testlauf
ENDFORM.

*----------------------------------------------------------------------*
*       Form  BUCHUNGSINFO_SICHERN
*----------------------------------------------------------------------*
*       Buchungsinfo in die Bewegung eintragen
*----------------------------------------------------------------------*
FORM BUCHUNGSINFO_SICHERN USING BWG STRUCTURE VTBFHAPO.
  SELECT SINGLE * FROM VTBFHAPO
                  WHERE BUKRS  = BWG-BUKRS
                  AND   RFHA   = BWG-RFHA
                  AND   RFHAZU = BWG-RFHAZU
                  AND   RFHAZB = BWG-RFHAZB
                  AND   DCRDAT = BWG-DCRDAT
                  AND   TCRTIM = BWG-TCRTIM.
  VTBFHAPO = BWG.
  UPDATE VTBFHAPO.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  BUCHUNGSINFO_AUSGEBEN
*----------------------------------------------------------------------*
FORM BUCHUNGSINFO_AUSGEBEN USING VALUE(BWG) STRUCTURE VTBFHAPO
                                 VALUE(PRODUKTART)
                                 VALUE(GESCHAEFTSART).
  DATA RESTZEILEN LIKE SY-LINNO.       " Anzahl der Restzeilen der Seite
* Informationen zum Gesch?t bei Wechsel des Gesch?ts:
  IF SAV_AUSGABE-BUKRS <> BWG-BUKRS OR
     SAV_AUSGABE-RFHA  <> BWG-RFHA.
*   Seitenumbruch...
    IF SY-LINCT IS INITIAL.
*     ...?erfl?sig; nur Trennstrich zu vorigem Gesch?t
      IF NOT SAV_AUSGABE-BUKRS IS INITIAL.
        ULINE.
      ENDIF.
    ELSE.
*     ...falls der Platz auf der Seite nicht ausreicht:
      RESTZEILEN = SY-LINCT - SY-LINNO.
      IF RESTZEILEN < 4.               " f? ULINE, Gesch?t, Bwg, ULINE
        ULINE.
        NEW-PAGE.
      ELSE.
*       ...sonst reicht ein Trennstrich zum vorigen Gesch?t:
        IF NOT SAV_AUSGABE-BUKRS IS INITIAL.
          ULINE.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'TB_POSTING_DEAL_INFO_WRITE'
         EXPORTING
              COMPANYCODE = BWG-BUKRS
              DEAL        = BWG-RFHA
              DEALTYPE    = GESCHAEFTSART
              PRODUCTTYPE = PRODUKTART.
    SAV_AUSGABE-BUKRS = BWG-BUKRS.
    SAV_AUSGABE-RFHA  = BWG-RFHA.
  ENDIF.
* Seitenumbruch, falls der Platz f? Bewegung und ULINE nicht reicht:
  IF NOT SY-LINCT IS INITIAL.
    RESTZEILEN = SY-LINCT - SY-LINNO.
    IF  RESTZEILEN < 2
    OR
        RESTZEILEN < 3                 "/wg. Whrg.tausch
    AND BWG-WZBETR <> BWG-WBBETR
    AND BWG-WBBETR <> SPACE.

      ULINE.
      NEW-PAGE.
      CALL FUNCTION 'TB_POSTING_DEAL_INFO_WRITE'
           EXPORTING
                COMPANYCODE = BWG-BUKRS
                DEAL        = BWG-RFHA
                DEALTYPE    = GESCHAEFTSART
                PRODUCTTYPE = PRODUKTART.
    ENDIF.
  ENDIF.
* Informationen zur Bewegung (Buchung 1):
  CALL FUNCTION 'TB_POSTING_FLOW_INFO_WRITE'
       EXPORTING
            FLOW = BWG.
* Falls im Online gestartet, Beleginfo zum Zugriff bereithalten:
  IF SY-BATCH = XFALSE.
    HIDE_BEREICH-BUKRS = BWG-BUKRS.
    HIDE_BEREICH-BELNR = BWG-BELNR.
    HIDE_BEREICH-GJAHR = BWG-GJAHR.
    HIDE HIDE_BEREICH.
    CLEAR HIDE_BEREICH.
  ENDIF.

* Informationen zur Bewegung (Buchung 2, falls vorhanden):
  IF  BWG-BELNR2 <> SPACE.
    BWG-BELNR  = BWG-BELNR2.
    BWG-WZBETR = BWG-WBBETR.
    BWG-BZBETR = BWG-BBBETR.
    CALL FUNCTION 'TB_POSTING_FLOW_INFO_WRITE'
         EXPORTING
              FLOW = BWG.
*   Falls im Online gestartet, Beleginfo zum Zugriff bereithalten:
    IF SY-BATCH = XFALSE.
      HIDE_BEREICH-BUKRS = BWG-BUKRS.
      HIDE_BEREICH-BELNR = BWG-BELNR.
      HIDE_BEREICH-GJAHR = BWG-GJAHR.
      HIDE HIDE_BEREICH.
      CLEAR HIDE_BEREICH.
    ENDIF.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  BELEG_STORNIEREN
*----------------------------------------------------------------------*
FORM BELEG_STORNIEREN USING    BWG STRUCTURE VTBFHAPO
                               VALUE(SIMULATION) TYPE BOOLE_D
                      CHANGING SUBRC.

*             entweder ist AWKEY nicht initial
*             oder         FHAPO wird ?ergeben
*             oder         BUKRS/BELNR/GJAHR    !!!
  CALL FUNCTION 'TB_FI_DOCUMENT_REVERSE'
       EXPORTING
            COMPANYCODE         = BWG-BUKRS
            DOCUMENT            = BWG-BELNR
            DOCUMENT2           = BWG-BELNR2
            YEAR                = BWG-GJAHR
            OBJ_KEY             = BWG-AWKEY
            DATE_OF_REVERSAL    = P_BUDAT
            PERIOD_OF_REVERSAL  = P_MONAT
            REASON_FOR_REVERSAL = P_STGRD
            FLOW_IN             = BWG
            SIMULATION          = SIMULATION
       EXCEPTIONS
            ERROR               = 1.
  IF SY-SUBRC = 0.
    SUBRC = 0.
  ELSE.
    SUBRC = 4.
    CALL FUNCTION 'TB_POSTING_MESSAGE_STORE'
         EXPORTING
              DEAL     = VTBFHA
              FLG_DEAL = XFALSE
              FLOW     = I_FHAPO
              MSGID    = SY-MSGID
              MSGNO    = SY-MSGNO
              MSGTY    = SY-MSGTY
              MSGV1    = SY-MSGV1
              MSGV2    = SY-MSGV2
              MSGV3    = SY-MSGV3
              MSGV4    = SY-MSGV4
         TABLES
              MSGPOST  = I_NACHRICHT.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  zahlungsanforderung_stornieren
*----------------------------------------------------------------------*
FORM ZAHLUNGSANFORDERUNG_STORNIEREN
     USING    BWG               STRUCTURE VTBFHAPO
              VALUE(SIMULATION) TYPE BOOLE_D
     CHANGING SUBRC.

*** zu 4.6A/B existiert ein FB im FI-BL f? CHECK and POST PAYRQ
*** ==> entweder nur CHECK durchf?ren, bei Simulation = XTRUE
***     oder         CHECK&POST,        bei Echtlauf (simulation=XFALSE)
  IF SIMULATION = XFALSE.              "/Echtlauf

*** ab 4.6A: Umstellung auf asynchrone Stornierung von PAYRQ's
*** --> PAYRQ und Beleg in gleicher LUW!
    CALL FUNCTION 'TB_PAYMENT_REQUEST_REVERSE'
       EXPORTING
            I_KEYNO  = BWG-PRKEY
*            i_dialog = xtrue
            I_DIALOG = XFALSE

            I_STGRD  = P_STGRD
            I_STDAT  = P_BUDAT
       EXCEPTIONS
            OTHERS   = 1.

  ELSE.                                "/CHECK only, no posting!

    CALL FUNCTION 'TB_PAYMENT_REQUEST_REV_CHECK'
         EXPORTING
              I_KEYNO = BWG-PRKEY
              I_STGRD = P_STGRD
              I_STDAT = P_BUDAT
         EXCEPTIONS
              OTHERS  = 1.

  ENDIF.

  IF SY-SUBRC = 0.
    SUBRC = 0.
  ELSE.
    SUBRC = 4.
    CALL FUNCTION 'TB_POSTING_MESSAGE_STORE'
         EXPORTING
              DEAL     = VTBFHA
              FLG_DEAL = XFALSE
              FLOW     = I_FHAPO
              MSGID    = SY-MSGID
              MSGNO    = SY-MSGNO
              MSGTY    = SY-MSGTY
              MSGV1    = SY-MSGV1
              MSGV2    = SY-MSGV2
              MSGV3    = SY-MSGV3
              MSGV4    = SY-MSGV4
         TABLES
              MSGPOST  = I_NACHRICHT.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  FEHLERNACHRICHTEN_AUSGEBEN
*----------------------------------------------------------------------*
FORM FEHLERNACHRICHTEN_AUSGEBEN.
  DATA: LEERFELD(30) TYPE C,           " Zur Ausgabe bei fehlender Info
        RESTZEILEN   LIKE SY-LINNO.    " Anzahl der Restzeilen der Seite
  LOOP AT I_NACHRICHT.
*   Am Anfang der Ausgabe:
    AT FIRST.
      FLG_SEITENKOPF = 'F'.
      NEW-PAGE.
    ENDAT.
*   Wechsel des Gesch?ts:
    ON CHANGE OF I_NACHRICHT-BUKRS OR I_NACHRICHT-RFHA.
*     Seitenumbruch...
      IF SY-LINCT IS INITIAL.
*       ...?erfl?sig; nur Trennstrich zu vorigem Gesch?t
        IF SY-TABIX > 1.
          ULINE.
        ENDIF.
      ELSE.
*       ...falls der Platz auf der Seite nicht ausreicht:
        RESTZEILEN = SY-LINCT - SY-LINNO.
        IF RESTZEILEN < 4.             " f? ULINE, Gesch?t, Bwg, ULINE
          ULINE.
          NEW-PAGE.
        ELSE.
*         ...sonst reicht ein Trennstrich zum vorigen Gesch?t:
          IF SY-TABIX > 1.
            ULINE.
          ENDIF.
        ENDIF.
      ENDIF.
*     Informationen zum Gesch?t ausgeben:
      CALL FUNCTION 'TB_POSTING_DEAL_INFO_WRITE'
           EXPORTING
                COMPANYCODE = I_NACHRICHT-BUKRS
                DEAL        = I_NACHRICHT-RFHA
                DEALTYPE    = I_NACHRICHT-SFHAART
                PRODUCTTYPE = I_NACHRICHT-SGSART.
    ENDON.
*   Seitenumbruch, falls der Platz f? Bewegung und ULINE nicht reicht:
    IF NOT SY-LINCT IS INITIAL.
      RESTZEILEN = SY-LINCT - SY-LINNO.
      IF RESTZEILEN < 2.
        ULINE.
        NEW-PAGE.
*       Informationen zum Gesch?t ausgeben:
        CALL FUNCTION 'TB_POSTING_DEAL_INFO_WRITE'
             EXPORTING
                  COMPANYCODE = I_NACHRICHT-BUKRS
                  DEAL        = I_NACHRICHT-RFHA
                  DEALTYPE    = I_NACHRICHT-SFHAART
                  PRODUCTTYPE = I_NACHRICHT-SGSART.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
         EXPORTING
              LANGU = SY-LANGU
              MSGID = I_NACHRICHT-MSGID
              MSGNO = I_NACHRICHT-MSGNO
              MSGV1 = I_NACHRICHT-MSGV1
              MSGV2 = I_NACHRICHT-MSGV2
              MSGV3 = I_NACHRICHT-MSGV3
              MSGV4 = I_NACHRICHT-MSGV4
         IMPORTING
              TEXT  = NACHRICHT_TEXT.
*   Ausgabe der einzelnen Nachrichten:
    IF I_NACHRICHT-SFHAZBA IS INITIAL.
*     Nachricht zum Gesch?t insgesamt:
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: / SY-VLINE NO-GAP,
             (10) LEERFELD,            " DZTERM
             (4)  LEERFELD,            " SFHAZBA
             (1)  LEERFELD,            " SSIGN
             (1)  LEERFELD,            " SZART
             (30) LEERFELD,            " XBEWART
             (10) LEERFELD,            " BELNR
             (4)  LEERFELD,            " GJAHR
             NACHRICHT_TEXT NO-GAP.
      POSITION SY-LINSZ.
      WRITE  SY-VLINE.
    ELSE.
*     Nachricht zur einzelnen Bewegung:
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*     Bezeichnung der Bewegungsart:
      SELECT SINGLE * FROM  TZB0T
             WHERE  SSPRAS      = SY-LANGU
             AND    RANTYP      = I_NACHRICHT-RANTYP
             AND    SBEWART     = I_NACHRICHT-SFHAZBA.
      IF SY-SUBRC <> 0.
        CLEAR TZB0T-XBEWART.
      ENDIF.
      WRITE: / SY-VLINE NO-GAP,
             I_NACHRICHT-DZTERM,
             I_NACHRICHT-SFHAZBA,
             I_NACHRICHT-SSIGN,
             I_NACHRICHT-SZART,
             TZB0T-XBEWART,
             I_NACHRICHT-BELNR,
             I_NACHRICHT-GJAHR,
             NACHRICHT_TEXT NO-GAP.
      POSITION SY-LINSZ.
      WRITE  SY-VLINE.
    ENDIF.
*   Falls im Online gestartet, Fehlerinfo zum Zugriff bereithalten:
    IF SY-BATCH = XFALSE.
      MOVE-CORRESPONDING I_NACHRICHT TO HIDE_BEREICH.
      HIDE_BEREICH-TEXT = NACHRICHT_TEXT.
      HIDE HIDE_BEREICH.
      CLEAR HIDE_BEREICH.
    ENDIF.
*   Am Ende der Ausgabe:
    AT LAST.
      ULINE.
    ENDAT.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
*       Form SEITENKOPF_AUSGEBEN
*----------------------------------------------------------------------*
FORM SEITENKOPF_AUSGEBEN.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED.
  BHDGD-INIFL = 0.
  BHDGD-LINE1 = SY-TITLE.
  CLEAR BHDGD-LINE2.
  CASE FLG_SEITENKOPF.
*   Ausgabe der gebuchten Bewegungen:
    WHEN 'B'.
*     Seiten?erschrift:
      IF P_TEST = XFALSE.
        BHDGD-LINE2(30) = TEXT-HB1.
      ELSE.
        BHDGD-LINE2(30) = TEXT-HB3.
      ENDIF.
      PERFORM BATCH-HEADING(RSBTCHH0).
      ULINE.
*     Spalten?erschrift:
      CALL FUNCTION 'TB_POSTING_COLUMN_HEADER_WRITE'
           EXPORTING
                CLASS = '3'
           EXCEPTIONS
                CLASS = 01.
      ULINE.
*     Nach diesem Seitenkopf wird mindestens eine Bwg. ausgegeben:
      FLG_BWG_AUSGEGEBEN = XTRUE.
*   Ausgabe der Fehlernachrichten:
    WHEN 'F'.
*     Seiten?erschrift:
      BHDGD-LINE2(30) = TEXT-HF1.
      PERFORM BATCH-HEADING(RSBTCHH0).
      ULINE.
*     Spalten?erschrift:
      FORMAT COLOR COL_HEADING INTENSIFIED.
      WRITE TEXT-HF2.
      POSITION SY-LINSZ.
      WRITE SY-VLINE.
      ULINE.
  ENDCASE.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM USER-COMMAND
*----------------------------------------------------------------------*
FORM USER-COMMAND USING VALUE(UCOMM).
  DATA: TITEL(100) TYPE C.             " Titel zur Nachricht
  CASE UCOMM.
*   Ausw?len --> Beleganzeige:
    WHEN 'PICK'.
      IF HIDE_BEREICH IS INITIAL.
        MESSAGE S111(T0).              " Cursor auf Eintrag position.
      ELSEIF HIDE_BEREICH-BELNR IS INITIAL.
        MESSAGE S928(T0).
      ELSE.
*        set parameter id 'BUK' field hide_bereich-bukrs.
*        set parameter id 'BLN' field hide_bereich-belnr.
*        set parameter id 'GJR' field hide_bereich-gjahr.
*        call transaction 'FB03' and skip first screen.
        CALL FUNCTION 'TRCA_FI_DOCUMENT_DISPLAY'
               EXPORTING
                    COMPANYCODE = HIDE_BEREICH-BUKRS
                    DOCUMENT    = HIDE_BEREICH-BELNR
                    YEAR        = HIDE_BEREICH-GJAHR
*                   OBJ_TYPE    =
*                   OBJ_KEY     =
*                   OBJ_SYS     =
               EXCEPTIONS
                    NOT_FOUND   = 1
                    OTHERS      = 2
                    .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.
*   Langtext der Nachricht:
    WHEN 'MSG '.
      IF HIDE_BEREICH IS INITIAL.
        MESSAGE S111(T0).              " Cursor auf Eintrag position.
      ELSEIF HIDE_BEREICH-MSGNO IS INITIAL.
        MESSAGE S929(T0).
      ELSE.
*       Langtext der Fehlernachricht anzeigen:
*       Titel aufbauen:
        IF HIDE_BEREICH-RFHAZB IS INITIAL.
          TITEL+0(10) = TEXT-T01.      " Gesch?t
          TITEL+20    = HIDE_BEREICH-BUKRS.
          WRITE HIDE_BEREICH-RFHA TO TITEL+30.
          CONDENSE TITEL.
        ELSE.
          TITEL+0(10) = TEXT-T02.      " Bewegung
          WRITE HIDE_BEREICH-DZTERM TO TITEL+20 DD/MM/YYYY.
          TITEL+35 = HIDE_BEREICH-SFHAZBA.
          TITEL+45 = HIDE_BEREICH-SSIGN.
          TITEL+50 = HIDE_BEREICH-SZART.
          CONDENSE TITEL.
        ENDIF.

        CALL FUNCTION 'TB_MESSAGE_SHOW_HELPSCREEN'
             EXPORTING
                  LANGU = SY-LANGU
                  MSGID = HIDE_BEREICH-MSGID
                  MSGNO = HIDE_BEREICH-MSGNO
                  MSGV1 = HIDE_BEREICH-MSGV1
                  MSGV2 = HIDE_BEREICH-MSGV2
                  MSGV3 = HIDE_BEREICH-MSGV3
                  MSGV4 = HIDE_BEREICH-MSGV4
                  TEXT  = HIDE_BEREICH-TEXT
                  TITLE = TITEL.
      ENDIF.
  ENDCASE.
  CLEAR HIDE_BEREICH.
ENDFORM.
