
* Description       : COPIED FROM RFFORI06 AND CHANGED BY YONGPING LI
*                     FOR ZHR_RFFOUS_T_EE USE ONLY
* Modification Logs
* Date       Developer    RequestNo    Description
*11/02/2004  chris        UD1K


************************************************************************
*                                                                      *
* Includebaustein RFFORI06 zu den Formulardruckprogrammen RFFOxxxz     *
* mit Unterprogrammen für den Druck des Avises                         *
*                                                                      *
************************************************************************


*----------------------------------------------------------------------*
* FORM AVIS                                                            *
*----------------------------------------------------------------------*
* Druck Avis                                                           *
* Gerufen von END-OF-SELECTION (RFFOxxxz)                              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM AVIS.
  DATA: L_EMP TYPE I.

*----------------------------------------------------------------------*
* Abarbeiten der extrahierten Daten                                    *
*----------------------------------------------------------------------*
  IF FLG_SORT NE 2.
*    SORT BY avis.               "UD1K912809
    SORT BY                                                 "UD1K912809
         REGUH-ZBUKR                                        "UD1K912809
         HLP_SORTP1                                         "UD1K912809
         REGUH-RZAWE                                        "UD1K912809
         REGUH-UBNKS                                        "UD1K912809
         REGUH-UBNKY                                        "UD1K912809
         REGUH-UBNKL                                        "UD1K912809
         HLP_SORTH1                                         "UD1K912809
         HLP_SORTH2                                         "UD1K912809
         HLP_SORTH3                                         "UD1K912809
         REGUH-UBKNT                                        "UD1K912809
         REGUH-WAERS                                        "UD1K912809
         REGUH-ZBNKS                                        "UD1K912809
         REGUH-ZBNKY                                        "UD1K912809
         REGUH-ZBNKL                                        "UD1K912809
         REGUH-ZBNKN                                        "UD1K912809
         REGUH-VBLNR                                        "UD1K912809
         REGUP-BUKRS                                        "UD1K912809
         HLP_SORTP2                                         "UD1K912809
         HLP_SORTP3                                         "UD1K912809
         REGUP-BELNR.                                       "UD1K912809
    FLG_SORT = 2.
  ENDIF.

  LOOP.
    L_EMP = L_EMP + 1.
* Get Work schedule / Desc   - UD1K919600
*    at new reguh-pernr.  - UD1K919674

    SELECT SINGLE SCHKZ INTO L_SCHKZ
           FROM PA0007 WHERE PERNR = REGUH-PERNR
           AND ENDDA >= SY-DATUM AND
                 BEGDA <= SY-DATUM.

    SELECT SINGLE RTEXT INTO L_RTEXT FROM T508S
           WHERE SCHKZ = L_SCHKZ.

*    endat.  - UD1K919674
* end of changes  - UD1K919600

*-- Neuer zahlender Buchungskreis --------------------------------------
    AT NEW REGUH-ZBUKR.

      IF NOT REGUH-ZBUKR IS INITIAL.   "FPAYM
        PERFORM BUCHUNGSKREIS_DATEN_LESEN.
      ENDIF.                           "FPAYM
*     reading the description of kostl
      IF  R2  EQ 'X'.                                       "UD1K919725
        SELECT SINGLE KTEXT INTO KOSTL_DES
          FROM CSKT
          WHERE KOSTL = REGUP-KOSTL AND
                KOKRS = 'H201'      AND
                DATBI GE SY-DATUM   AND
                SPRAS = SY-LANGU.
      ELSE.                                                 "UD1K919725
        SELECT SINGLE SACHN INTO KOSTL_DES
       FROM T526
         WHERE WERKS = 'HMMA'
         AND   SACHX =  REGUH-ZTELX.
        REGUP-KOSTL = REGUH-ZTELX.
      ENDIF.                                                "UD1K919725

    ENDAT.
* begin of changes - UD1K919725
    IF R1 EQ 'X'.
      SELECT SINGLE SACHN INTO KOSTL_DES
           FROM T526
             WHERE WERKS = 'HMMA'
             AND   SACHX =  REGUH-ZTELX.
      REGUP-KOSTL = REGUH-ZTELX.
    ENDIF.
* End of changes - UD1K919725
    AT NEW HLP_SORTP1.
*     reading the description of kostl
      IF  R2  EQ 'X'.                                       "UD1K919725
        SELECT SINGLE KTEXT INTO KOSTL_DES
          FROM CSKT
          WHERE KOSTL = REGUP-KOSTL AND
                KOKRS = 'H201'      AND
                DATBI GT SY-DATUM   AND
                SPRAS = SY-LANGU.
      ELSE.                                                 "UD1K919725
        SELECT SINGLE SACHN INTO KOSTL_DES
         FROM T526
           WHERE WERKS = 'HMMA'
           AND   SACHX =  REGUH-ZTELX.
        REGUP-KOSTL = REGUH-ZTELX.
      ENDIF.                                                "UD1K919725

    ENDAT.

*-- Neuer Zahlweg ------------------------------------------------------
    AT NEW REGUH-RZAWE.

      FLG_PROBEDRUCK = 0.              "für diesen Zahlweg wurde noch
      FLG_SGTXT      = 0.              "kein Probedruck durchgeführt

      IF REGUH-RZAWE NE SPACE.
        PERFORM ZAHLWEG_DATEN_LESEN.

*       Zahlungsformular nur zum Lesen öffnen
        IF NOT T042E-ZFORN IS INITIAL.
          CALL FUNCTION 'OPEN_FORM'
               EXPORTING
                    FORM     = T042E-ZFORN
                    DIALOG   = SPACE
                    DEVICE   = 'SCREEN'
                    LANGUAGE = T001-SPRAS
               EXCEPTIONS
                    FORM     = 1.
          IF SY-SUBRC EQ 0.            "Formular existiert

*           Formular auf Segmenttext (Global &REGUP-SGTXT) untersuchen
            IF PAR_XDTA EQ SPACE.
              IF T042E-XAVIS NE SPACE AND T042E-ANZPO NE 99.
                CALL FUNCTION 'READ_FORM_LINES'
                     EXPORTING
                          ELEMENT = HLP_EP_ELEMENT
                     TABLES
                          LINES   = TAB_ELEMENT
                     EXCEPTIONS
                          ELEMENT = 1.
                IF SY-SUBRC EQ 0.
                  LOOP AT TAB_ELEMENT.
                    IF    TAB_ELEMENT-TDLINE   CS 'REGUP-SGTXT'
                      AND TAB_ELEMENT-TDFORMAT NE '/*'.
                      FLG_SGTXT = 1.   "Global für Segmenttext existiert
                      EXIT.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
            CALL FUNCTION 'CLOSE_FORM'.
          ENDIF.
        ENDIF.

      ENDIF.

*     Überschrift für den Formularabschluß modifizieren
      T042Z-TEXT1 = TEXT_001.

      DATA: L_NAME LIKE  ITCPO-TDDATASET.                   "UD1K912809
      IF   R1   EQ   'X'.                                   "UD1K919725
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'        "UD1K912809
               EXPORTING
            INPUT    =  REGUH-ZTELX                         "UD1K919725
               IMPORTING
            OUTPUT   =  L_NAME.                             "UD1K912809
      ELSE.                                                 "UD1K919725
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'        "UD1K912809
              EXPORTING
             INPUT    =  REGUP-KOSTL                        "UD1K919725
              IMPORTING
            OUTPUT   =  L_NAME.                             "UD1K912809
      ENDIF.                                                "UD1K919725

*     Vorschlag für die Druckparameter aufbauen

      PERFORM FILL_ITCPO USING PAR_PRIA
*                               'LIST5S'            "UD1K912809
                               L_NAME                       "UD1K912809
                               SPACE   "par_sofa via tab_ausgabe!
                               HLP_AUTH.

      EXPORT ITCPO TO MEMORY ID 'RFFORI06_ITCPO'.

    ENDAT.


*-- Neue Hausbank ------------------------------------------------------
    AT NEW REGUH-UBNKL.

      PERFORM HAUSBANK_DATEN_LESEN.

*     Felder für Formularabschluß initialisieren
      CNT_AVISE      = 0.
      CNT_AVEDI      = 0.
      CNT_AVFAX      = 0.
      CNT_AVMAIL     = 0.
      SUM_ABSCHLUSS  = 0.
      SUM_ABSCHL_EDI = 0.
      SUM_ABSCHL_FAX = 0.
      REFRESH TAB_EDI_AVIS.

      FLG_DRUCKMODUS = 0.
    ENDAT.


*-- Neue Empfängerbank -------------------------------------------------
    AT NEW REGUH-ZBNKL.

      PERFORM EMPFBANK_DATEN_LESEN.

    ENDAT.


*-- Neue Zahlungsbelegnummer -------------------------------------------
    AT NEW REGUH-VBLNR.

*     Prüfe, ob Avis auf Papier erzwungen wird
*     Check if advice on paper is forced
      IF FLG_PAPIERAVIS EQ 1.
        REGUH-EDIAV = SPACE.
      ENDIF.

*     Prüfe, ob HR-Formular zu verwenden ist
*     Check if HR-form is to be used
      HRXBLNR = REGUP-XBLNR.
      IF ( HLP_LAUFK EQ 'P' OR
           HRXBLNR-TXTSL EQ 'HR' AND HRXBLNR-TXERG EQ 'GRN' )
       AND HRXBLNR-XHRFO NE SPACE.
        HLP_XHRFO = 'X'.
      ELSE.
        HLP_XHRFO = SPACE.
      ENDIF.

*     HR-Formular besorgen
*     read HR form
      IF HLP_XHRFO EQ 'X'.
        PERFORM HR_FORMULAR_LESEN.
      ENDIF.

*     Prüfung, ob Avis erforderlich
      CNT_ZEILEN = 0.
      IF HLP_XHRFO EQ SPACE.
        IF FLG_SGTXT EQ 1.
          CNT_ZEILEN = REGUH-RPOST + REGUH-RTEXT.
        ELSE.
          CNT_ZEILEN = REGUH-RPOST.
        ENDIF.
      ELSE.
        DESCRIBE TABLE PFORM LINES CNT_ZEILEN.
      ENDIF.
      FLG_KEIN_DRUCK = 0.
      IF REGUH-EDIAV EQ 'V'.
*       Avis bereits versendet
        FLG_KEIN_DRUCK = 1.            "kein Druck erforderlich
      ELSEIF REGUH-RZAWE NE SPACE AND T042E-XSAVI IS INITIAL.
*       Avis zu Formular
        IF HLP_ZEILEN EQ 0 AND PAR_XDTA EQ SPACE.
          IF T042E-XAVIS EQ SPACE OR CNT_ZEILEN LE T042E-ANZPO.
            FLG_KEIN_DRUCK = 1.        "kein Druck erforderlich
          ENDIF.
*       Avis zum DTA
        ELSE.
          CLEAR TAB_KEIN_AVIS.
          MOVE-CORRESPONDING REGUH TO TAB_KEIN_AVIS.
          READ TABLE TAB_KEIN_AVIS.
          IF SY-SUBRC EQ 0.
            FLG_KEIN_DRUCK = 1.        "kein Druck erforderlich
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM FPAYM USING 1.           "FPAYM
      PERFORM ZAHLUNGS_DATEN_LESEN.
      IF REGUH-EDIAV NA ' V' AND HLP_XHRFO EQ SPACE.
        PERFORM SUMMENFELDER_INITIALISIEREN.
      ENDIF.

*     Schecknummer bei vornumerierten Schecks
      CLEAR REGUD-CHECT.
      READ TABLE TAB_SCHECKS WITH KEY
        ZBUKR = REGUH-ZBUKR
        VBLNR = REGUH-VBLNR.
      IF SY-SUBRC EQ 0.
        REGUD-CHECT = TAB_SCHECKS-CHECT.
      ELSEIF FLG_SCHECKNUM EQ 1.
        IF ZW_XVORL EQ SPACE.
          IF HLP_LAUFK NE 'P'.         "FI-Beleg vorhanden?
            SELECT * FROM PAYR
              WHERE ZBUKR EQ REGUH-ZBUKR
              AND   VBLNR EQ REGUH-VBLNR
              AND   GJAHR EQ REGUD-GJAHR
              AND   VOIDR EQ 0.
            ENDSELECT.
            SY-MSGV1 = REGUH-ZBUKR.
            SY-MSGV2 = REGUD-GJAHR.
            SY-MSGV3 = REGUH-VBLNR.
          ELSE.                        "HR-Abrechnung vorhanden?
            SELECT * FROM PAYR
              WHERE PERNR EQ REGUH-PERNR
              AND   SEQNR EQ REGUH-SEQNR
              AND   BTZNR EQ REGUH-BTZNR
              AND   VOIDR EQ 0.
            ENDSELECT.
            SY-MSGV1 = REGUH-PERNR.
            SY-MSGV2 = REGUH-SEQNR.
            SY-MSGV3 = REGUH-BTZNR.
          ENDIF.
          IF SY-SUBRC EQ 0.
            REGUD-CHECT = PAYR-CHECT.
          ELSE.
            READ TABLE ERR_FW_SCHECK WITH KEY
               ZBUKR = REGUH-ZBUKR
               VBLNR = REGUH-VBLNR.
            IF SY-SUBRC NE 0.
              IF SY-BATCH EQ SPACE.    "check does not exist
                MESSAGE A564(FS) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3.
              ELSE.
                MESSAGE S564(FS) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3.
                MESSAGE S549(FS).
                STOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          REGUD-CHECT = 'TEST'.
        ENDIF.
      ELSEIF FLG_AVIS EQ 1.
        IF HLP_LAUFK NE 'P'.         "FI-Beleg vorhanden?
          SELECT * FROM PAYR
            WHERE ZBUKR EQ REGUH-ZBUKR
            AND   VBLNR EQ REGUH-VBLNR
            AND   GJAHR EQ REGUD-GJAHR
            AND   VOIDR EQ 0.
          ENDSELECT.
        ELSE.                        "HR-Abrechnung vorhanden?
          SELECT * FROM PAYR
            WHERE PERNR EQ REGUH-PERNR
            AND   SEQNR EQ REGUH-SEQNR
            AND   BTZNR EQ REGUH-BTZNR
            AND   VOIDR EQ 0.
          ENDSELECT.
        ENDIF.
        IF SY-SUBRC EQ 0.
          REGUD-CHECT = PAYR-CHECT.
        ENDIF.
      ENDIF.

*     Berechnung Anzahl benötigter Wechsel
      IF REGUH-WEAMX EQ 0.
        REGUD-WECAN = 1.
      ELSE.
        REGUD-WECAN = REGUH-WEAMX.
        IF REGUH-WEHRS NE 0.
          ADD 1 TO REGUD-WECAN.
        ENDIF.
      ENDIF.

    ENDAT.


*-- Verarbeitung der Einzelposten-Informationen ------------------------
    AT DATEN.
      PERFORM FPAYM USING 2.           "FPAYM
      PERFORM EINZELPOSTENFELDER_FUELLEN.
      IF FLG_PAPIERAVIS EQ 1.
        REGUH-EDIAV = SPACE.
      ENDIF.
      IF REGUH-EDIAV NA ' V' AND HLP_XHRFO EQ SPACE.
        PERFORM SUMMENFELDER_FUELLEN.
      ENDIF.

    ENDAT.


*-- Ende der Zahlungsbelegnummer ---------------------------------------
    AT END OF REGUH-VBLNR.

      PERFORM FPAYM USING 2.           "FPAYM

*     Zahlungsbelegnummer bei Saldo-Null-Mitteilungen und
*     Zahlungsanforderungen nicht ausgeben
      IF ( REGUH-RZAWE EQ SPACE AND REGUH-XVORL EQ SPACE )
        OR T042Z-XZANF NE SPACE.
        REGUH-VBLNR = SPACE.
      ENDIF.

*     Stets Ausgabe via EDI, falls möglich
      IF FLG_PAPIERAVIS EQ 1.
        REGUH-EDIAV = SPACE.
      ENDIF.
      CLEAR REGUD-AVEDN.
      IF REGUH-EDIAV NA ' V' AND HLP_XHRFO EQ SPACE.
        CALL FUNCTION 'FI_EDI_REMADV_PEXR2001_OUT'
             EXPORTING
                  REGUH_IN   = REGUH
                  REGUD_IN   = REGUD
                  XEINZ_IN   = REGUD-XEINZ
             IMPORTING
                  DOCNUM_OUT = REGUD-AVEDN
             TABLES
                  TAB_REGUP  = TAB_REGUP
             EXCEPTIONS
                  OTHERS     = 4.
        IF SY-SUBRC EQ 0.
          ADD 1            TO CNT_AVEDI.
          ADD REGUH-RBETR  TO SUM_ABSCHL_EDI.
          WRITE:
            CNT_AVISE      TO REGUD-AVISE,
            CNT_AVEDI      TO REGUD-AVEDI,
            CNT_AVFAX      TO REGUD-AVFAX,
            CNT_AVMAIL     TO REGUD-AVMAIL,
            SUM_ABSCHLUSS  TO REGUD-SUMME CURRENCY T001-WAERS,
            SUM_ABSCHL_EDI TO REGUD-SUEDI CURRENCY T001-WAERS,
            SUM_ABSCHL_FAX TO REGUD-SUFAX CURRENCY T001-WAERS,
            SUM_ABSCHL_MAIL TO REGUD-SUMAIL CURRENCY T001-WAERS.
          TRANSLATE:
            REGUD-AVISE USING ' *',
            REGUD-AVEDI USING ' *',
            REGUD-AVFAX USING ' *',
            REGUD-AVMAIL USING ' *',
            REGUD-SUMME USING ' *',
            REGUD-SUEDI USING ' *',
            REGUD-SUFAX USING ' *',
            REGUD-SUMAIL USING ' *'.
          TAB_EDI_AVIS-REGUH = REGUH.
          TAB_EDI_AVIS-REGUD = REGUD.
          APPEND TAB_EDI_AVIS.
          FLG_KEIN_DRUCK = 1.
        ENDIF.
      ENDIF.

*     Ausgabe auf Fax oder Drucker (nur falls notwendig)
      IF FLG_KEIN_DRUCK EQ 0.

        PERFORM AVIS_NACHRICHTENART.
        PERFORM AVIS_OEFFNEN USING 'X'.
        PERFORM ZAHLUNGS_DATEN_LESEN_HLP.
        PERFORM SUMMENFELDER_INITIALISIEREN.
        PERFORM AVIS_SCHREIBEN.

      ENDIF.

    ENDAT.


*-- Ende der Hausbank --------------------------------------------------
    AT END OF REGUH-UBNKL.

      PERFORM FPAYM USING 3.           "FPAYM

      DATA:
        L_FORM  LIKE ITCTA-TDFORM,
        L_PAGES LIKE ITCTG OCCURS 0 WITH HEADER LINE.
      IF HLP_LAUFK NE '*'.
        IF L_FORM NE HLP_FORMULAR.
          L_FORM = HLP_FORMULAR.
          REFRESH L_PAGES.
          CALL FUNCTION 'READ_FORM'
               EXPORTING
                    FORM          = L_FORM
                    LANGUAGE      = T001-SPRAS
                    THROUGHCLIENT = 'X'
               TABLES
                    PAGES         = L_PAGES.
        ENDIF.
        READ TABLE L_PAGES WITH KEY TDPAGE = 'LAST'.
        IF SY-SUBRC NE 0.
          CLEAR L_PAGES-TDPAGE.
        ENDIF.
      ENDIF.

      IF L_PAGES-TDPAGE EQ 'LAST' AND  "Formularabschluß möglich
         ( CNT_AVISE NE 0              "Formularabschluß erforderlich
        OR CNT_AVEDI NE 0
        OR CNT_AVFAX NE 0
        OR CNT_AVMAIL NE 0 ) AND HLP_LAUFK NE '*'.

*       Formular für den Abschluß öffnen
        SET COUNTRY SPACE.
        CLEAR FINAA.
        FINAA-NACHA = '1'.
        PERFORM AVIS_OEFFNEN USING SPACE.

*       Liste aller Avis-Zwischenbelege ausgeben
        IF CNT_AVEDI NE 0.
          REFRESH TAB_ELEMENTS.
          CALL FUNCTION 'READ_FORM_ELEMENTS'
               EXPORTING
                    FORM     = HLP_FORMULAR
                    LANGUAGE = T001-SPRAS
               TABLES
                    ELEMENTS = TAB_ELEMENTS
               EXCEPTIONS
                    OTHERS   = 3.
          READ TABLE TAB_ELEMENTS WITH KEY
            WINDOW  = 'MAIN'
            ELEMENT = '676'.
          IF SY-SUBRC EQ 0.
            CALL FUNCTION 'START_FORM'
                 EXPORTING
                      FORM      = HLP_FORMULAR
                      LANGUAGE  = T001-SPRAS
                      STARTPAGE = 'EDI'.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = '675'
                      TYPE    = 'TOP'
                 EXCEPTIONS
                      OTHERS  = 4.
            SIC_REGUH = REGUH.
            SIC_REGUD = REGUD.
            LOOP AT TAB_EDI_AVIS.
              REGUH = TAB_EDI_AVIS-REGUH.
              REGUD = TAB_EDI_AVIS-REGUD.
              CALL FUNCTION 'WRITE_FORM'
                   EXPORTING
                        ELEMENT = '676'
                   EXCEPTIONS
                        OTHERS  = 4.
            ENDLOOP.
            CALL FUNCTION 'END_FORM'.
            REGUH = SIC_REGUH.
            REGUD = SIC_REGUD.
          ENDIF.
        ENDIF.

*       Formular für den Abschluß starten
        REGUP-VPOS2 = L_EMP.
        CALL FUNCTION 'START_FORM'
             EXPORTING
                  FORM      = HLP_FORMULAR
                  LANGUAGE  = T001-SPRAS
                  STARTPAGE = 'LAST'.

*       Ausgabe des Formularabschlusses
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  WINDOW = 'SUMMARY'
             EXCEPTIONS
                  WINDOW = 1.
        IF SY-SUBRC EQ 1.
          ERR_ELEMENT-FNAME = HLP_FORMULAR.
          ERR_ELEMENT-FENST = 'SUMMARY'.
          ERR_ELEMENT-ELEMT = SPACE.
          ERR_ELEMENT-TEXT  = SPACE.
          COLLECT ERR_ELEMENT.
        ENDIF.
        CLEAR L_EMP.
*       Formular beenden
        CALL FUNCTION 'END_FORM'.
      ENDIF.

      PERFORM AVIS_SCHLIESSEN.
      IF NOT ITCPP-TDSPOOLID IS INITIAL.
        UPDATE TSP01 SET   RQFINAL = 'C'
                     WHERE RQIDENT EQ ITCPP-TDSPOOLID.
      ENDIF.

      IF SY-BINPT EQ SPACE.
        COMMIT WORK.
      ENDIF.

    ENDAT.


*-- Ende des Zahlwegs --------------------------------------------------
    AT END OF REGUH-RZAWE.

      FREE MEMORY ID 'RFFORI06_ITCPO'.

    ENDAT.

  ENDLOOP.

ENDFORM.                               "Avis




*----------------------------------------------------------------------*
* FORM AVIS_NACHRICHTENART                                             *
*----------------------------------------------------------------------*
* Nachrichtenart ermitteln (Druck oder Fax)                            *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM AVIS_NACHRICHTENART.

  DATA UP_FIMSG LIKE FIMSG OCCURS 0 WITH HEADER LINE.
  STATICS UP_PROFILE LIKE SOPRD.

* Nachrichtenart ermitteln lassen
  CLEAR FINAA.
  FINAA-NACHA = '1'.
  CALL FUNCTION 'OPEN_FI_PERFORM_00002040_P'
       EXPORTING
            I_REGUH = REGUH
       TABLES
            T_FIMSG = UP_FIMSG
       CHANGING
            C_FINAA = FINAA.
  LOOP AT UP_FIMSG INTO FIMSG.
    PERFORM MESSAGE USING FIMSG-MSGNO.
  ENDLOOP.

* Nachrichtenart Fax (2) oder Mail (I) prüfen, sonst nur Druck (1)
  CASE FINAA-NACHA.
    WHEN '2'.
      CALL FUNCTION 'TELECOMMUNICATION_NUMBER_CHECK'
           EXPORTING
                SERVICE = 'TELEFAX'
                NUMBER  = FINAA-TDTELENUM
                COUNTRY = FINAA-TDTELELAND
           EXCEPTIONS
                OTHERS  = 4.
      IF SY-SUBRC NE 0.
        FINAA-NACHA = '1'.
        FINAA-FORNR = T042B-AFORN.
      ENDIF.
    WHEN 'I'.
      IF UP_PROFILE IS INITIAL.
        CALL FUNCTION 'SO_PROFILE_READ'
             IMPORTING
                  PROFILE = UP_PROFILE
             EXCEPTIONS
                  OTHERS  = 4.
        IF SY-SUBRC NE 0.
          UP_PROFILE-SMTP_EXIST = '-'.
        ENDIF.
      ENDIF.
      IF UP_PROFILE-SMTP_EXIST NE 'X' OR FINAA-INTAD IS INITIAL.
        FINAA-NACHA = '1'.
        FINAA-FORNR = T042B-AFORN.
      ENDIF.
    WHEN OTHERS.
      FINAA-NACHA = '1'.
  ENDCASE.

ENDFORM.



*----------------------------------------------------------------------*
* FORM AVIS_OEFFNEN                                                    *
*----------------------------------------------------------------------*
* Avis öffnen und bei Druck Probedruck erledigen                       *
*----------------------------------------------------------------------*
* GENUINE ist gesetzt bei echten Avisen, leer bei Formularabschluß     *
*----------------------------------------------------------------------*
FORM AVIS_OEFFNEN USING GENUINE.

  DATA: UP_DEVICE    LIKE ITCPP-TDDEVICE,
        UP_SENDER    LIKE SWOTOBJID,
        UP_RECIPIENT LIKE SWOTOBJID.

* Mail-Sender und -Empfänger ermitteln
  IF FINAA-NACHA EQ 'I'.
    PERFORM MAIL_VORBEREITEN USING    SY-UNAME  FINAA-INTAD
                             CHANGING UP_SENDER UP_RECIPIENT.
    IF UP_SENDER IS INITIAL.
      IF NOT REGUH-PERNR IS INITIAL.
        FIMSG-MSGV1 = REGUH-PERNR.
        FIMSG-MSGV2 = REGUH-SEQNR.
      ELSE.
        FIMSG-MSGV1 = REGUH-ZBUKR.
        FIMSG-MSGV2 = REGUH-VBLNR.
      ENDIF.
      PERFORM MESSAGE USING '387'.
      FINAA-NACHA = '1'.
      FINAA-FORNR = T042B-AFORN.
    ENDIF.
  ENDIF.

* Formular ermitteln
  IF NOT FINAA-FORNR IS INITIAL.
    HLP_FORMULAR = FINAA-FORNR.
  ELSE.
    HLP_FORMULAR = T042B-AFORN.
  ENDIF.

* Vorschlag für die Druckvorgaben holen und anpassen, Device setzen
  IMPORT ITCPO FROM MEMORY ID 'RFFORI06_ITCPO'.
  CASE FINAA-NACHA.
    WHEN '1'.
      UP_DEVICE = 'PRINTER'.
    WHEN '2'.
      ITCPO-TDSCHEDULE = FINAA-TDSCHEDULE.
      ITCPO-TDTELELAND = FINAA-TDTELELAND.
      ITCPO-TDTELENUM  = FINAA-TDTELENUM.
      ITCPO-TDFAXUSER  = FINAA-TDFAXUSER.
      ITCPO-TDSUFFIX1  = 'FAX'.
      UP_DEVICE = 'TELEFAX'.
    WHEN 'I'.
      ITCPO-TDTITLE    = TEXT_096.
      WRITE REGUH-ZALDT TO TXT_ZEILE DD/MM/YYYY.
      REPLACE '&' WITH TXT_ZEILE INTO ITCPO-TDTITLE.
      UP_DEVICE = 'MAIL'.
  ENDCASE.
  CLEAR:
    TOA_DARA,
    ARC_PARAMS.

* Druckvorgaben modifizieren lassen
  IF GENUINE EQ 'X'.
    CALL FUNCTION 'OPEN_FI_PERFORM_00002050_P'
         EXPORTING
              I_REGUH          = REGUH
              I_GJAHR          = REGUD-GJAHR
              I_NACHA          = FINAA-NACHA
              I_AFORN          = HLP_FORMULAR
         CHANGING
              C_ITCPO          = ITCPO
              C_ARCHIVE_INDEX  = TOA_DARA
              C_ARCHIVE_PARAMS = ARC_PARAMS.
    IF ITCPO-TDARMOD GT 1 AND PAR_ANZP NE 0.              "#EC PORTABLE
      PAR_ANZP = 0.
      PERFORM MESSAGE USING '384'.
    ENDIF.
  ENDIF.

* Name des Elements mit dem Anschreiben zusammensetzen
  IF REGUH-RZAWE NE SPACE.
    HLP_ELEMENT   = '610-'.
    HLP_ELEMENT+4 = REGUH-RZAWE.
    HLP_ELETEXT   = TEXT_610.
    REPLACE '&ZAHLWEG' WITH REGUH-RZAWE INTO HLP_ELETEXT.
  ELSE.
    HLP_ELEMENT   = '611-'.
    HLP_ELEMENT+4 = REGUH-AVISG.
    HLP_ELETEXT   = TEXT_611.
  ENDIF.

* Prüfen, ob ein Close/Open_form notwendig ist (Performance)
  IF FLG_DRUCKMODUS EQ 1.
    CHECK FINAA-NACHA NE '1' OR ITCPO-TDARMOD NE '1'.
  ENDIF.

* Dialog nur, wenn bei Druck der Drucker unbekannt
  IF PAR_PRIA EQ SPACE AND FINAA-NACHA EQ '1'.
    FLG_DIALOG = 'X'.
  ELSE.
    FLG_DIALOG = SPACE.
  ENDIF.

* Neue Spool-Id bei erstem Avis zum Druck oder bei Fax
  IF FLG_PROBEDRUCK EQ 0 OR FINAA-NACHA NE '1'.
    ITCPO-TDNEWID = 'X'.
  ELSE.
    ITCPO-TDNEWID = SPACE.
  ENDIF.

* Formular schließen, falls noch offen vom letzten Avis
  PERFORM AVIS_SCHLIESSEN.

* Formular öffnen
  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
            ARCHIVE_INDEX  = TOA_DARA
            ARCHIVE_PARAMS = ARC_PARAMS
            FORM           = HLP_FORMULAR
            DEVICE         = UP_DEVICE
            LANGUAGE       = T001-SPRAS
            OPTIONS        = ITCPO
            DIALOG         = FLG_DIALOG
            MAIL_SENDER    = UP_SENDER
            MAIL_RECIPIENT = UP_RECIPIENT
       IMPORTING
            RESULT         = ITCPP
       EXCEPTIONS
            FORM           = 1
            MAIL_OPTIONS   = 2.
  IF SY-SUBRC EQ 2.                    "E-Mailen nicht möglich,
    FIMSG-MSGID = SY-MSGID.            "also drucken
    FIMSG-MSGV1 = SY-MSGV1.
    FIMSG-MSGV2 = SY-MSGV2.
    FIMSG-MSGV3 = SY-MSGV3.
    FIMSG-MSGV4 = SY-MSGV4.
    PERFORM MESSAGE USING SY-MSGNO.
    IF NOT REGUH-PERNR IS INITIAL.
      FIMSG-MSGV1 = REGUH-PERNR.
      FIMSG-MSGV2 = REGUH-SEQNR.
    ELSE.
      FIMSG-MSGV1 = REGUH-ZBUKR.
      FIMSG-MSGV2 = REGUH-VBLNR.
    ENDIF.
    PERFORM MESSAGE USING '387'.
    CALL FUNCTION 'CLOSE_FORM'
         EXCEPTIONS
              OTHERS = 0.
    FINAA-NACHA   = '1'.
    UP_DEVICE     = 'PRINTER'.
    HLP_FORMULAR  = T042B-AFORN.
    CALL FUNCTION 'OPEN_FORM'
         EXPORTING
              ARCHIVE_INDEX  = TOA_DARA
              ARCHIVE_PARAMS = ARC_PARAMS
              FORM           = HLP_FORMULAR
              DEVICE         = UP_DEVICE
              LANGUAGE       = T001-SPRAS
              OPTIONS        = ITCPO
              DIALOG         = FLG_DIALOG
         IMPORTING
              RESULT         = ITCPP
         EXCEPTIONS
              FORM           = 1.
  ENDIF.
  IF SY-SUBRC EQ 1.                    "Abbruch:
    IF SY-BATCH EQ SPACE.              "Formular ist nicht aktiv!
      MESSAGE A069 WITH HLP_FORMULAR.
    ELSE.
      MESSAGE S069 WITH HLP_FORMULAR.
      MESSAGE S094.
      STOP.
    ENDIF.
  ENDIF.

* Druckmodus setzen
  IF FINAA-NACHA EQ '1' AND ITCPO-TDARMOD EQ '1'.
    FLG_DRUCKMODUS = 1.
  ELSE.
    FLG_DRUCKMODUS = 2.
  ENDIF.
  HLP_NACHA_LAST = FINAA-NACHA.

* letzte Druckparameter merken
  IF FINAA-NACHA EQ '1'.
    PAR_PRIA = ITCPP-TDDEST.
    PERFORM FILL_ITCPO_FROM_ITCPP.
    EXPORT ITCPO TO MEMORY ID 'RFFORI06_ITCPO'.
  ENDIF.

* Probedruck
  IF FLG_PROBEDRUCK EQ 0               "Probedruck noch nicht erledigt
    AND FINAA-NACHA EQ '1'.
    PERFORM DATEN_SICHERN.
    DO PAR_ANZP TIMES.
*     Probedruck-Formular starten
      CALL FUNCTION 'START_FORM'
           EXPORTING
                FORM     = HLP_FORMULAR
                LANGUAGE = T001-SPRAS.
*     Fenster mit Probedruck schreiben
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                WINDOW   = 'INFO'
                ELEMENT  = '605'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = HLP_ELEMENT
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      IF SY-SUBRC NE 0.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = '610'
             EXCEPTIONS
                  WINDOW  = 1
                  ELEMENT = 2.
      ENDIF.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '614'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '615'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      DO 5 TIMES.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT  = '625'
                  FUNCTION = 'APPEND'
             EXCEPTIONS
                  WINDOW   = 1
                  ELEMENT  = 2.
      ENDDO.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT  = '630'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                WINDOW  = 'TOTAL'
                ELEMENT = '630'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                WINDOW   = 'INFO'
                ELEMENT  = '605'
                FUNCTION = 'DELETE'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
*     Probedruck-Formular beenden
      CALL FUNCTION 'END_FORM'.
    ENDDO.
    PERFORM DATEN_ZURUECK.
    FLG_PROBEDRUCK = 1.                "Probedruck erledigt
  ENDIF.

ENDFORM.



*----------------------------------------------------------------------*
* FORM AVIS_SCHREIBEN                                                  *
*----------------------------------------------------------------------*
* Avis in Druckform ausgeben                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM AVIS_SCHREIBEN.

* Faxdeckblatt
  IF FINAA-NACHA EQ '2' AND FINAA-FORMC NE SPACE.
    PERFORM ADRESSE_LESEN USING T001-ADRNR.                 "SADR40A
    ITCFX-RTITLE     = REGUH-ZANRE.
    ITCFX-RNAME1     = REGUH-ZNME1.
    ITCFX-RNAME2     = REGUH-ZNME2.
    ITCFX-RNAME3     = REGUH-ZNME3.
    ITCFX-RNAME4     = REGUH-ZNME4.
    ITCFX-RPOCODE    = REGUH-ZPSTL.
    ITCFX-RCITY1     = REGUH-ZORT1.
    ITCFX-RCITY2     = REGUH-ZORT2.
    ITCFX-RPOCODE2   = REGUH-ZPST2.
    ITCFX-RPOBOX     = REGUH-ZPFAC.
    ITCFX-RPOPLACE   = REGUH-ZPFOR.
    ITCFX-RSTREET    = REGUH-ZSTRA.
    ITCFX-RCOUNTRY   = REGUH-ZLAND.
    ITCFX-RREGIO     = REGUH-ZREGI.
    ITCFX-RLANGU     = HLP_SPRACHE.
    ITCFX-RHOMECNTRY = T001-LAND1.
    ITCFX-RLINES     = '9'.
    ITCFX-RCTITLE    = SPACE.
    ITCFX-RCFNAME    = SPACE.
    ITCFX-RCLNAME    = SPACE.
    ITCFX-RCNAME1    = FINAA-NAMEP.
    ITCFX-RCNAME2    = SPACE.
    ITCFX-RCDEPTM    = FINAA-ABTEI.
    ITCFX-RCFAXNR    = FINAA-TDTELENUM.
    ITCFX-STITLE     = SADR-ANRED.
    ITCFX-SNAME1     = SADR-NAME1.
    ITCFX-SNAME2     = SADR-NAME2.
    ITCFX-SNAME3     = SADR-NAME3.
    ITCFX-SNAME4     = SADR-NAME4.
    ITCFX-SPOCODE    = SADR-PSTLZ.
    ITCFX-SCITY1     = SADR-ORT01.
    ITCFX-SCITY2     = SADR-ORT02.
    ITCFX-SPOCODE2   = SADR-PSTL2.
    ITCFX-SPOBOX     = SADR-PFACH.
    ITCFX-SPOPLACE   = SADR-PFORT.
    ITCFX-SSTREET    = SADR-STRAS.
    ITCFX-SCOUNTRY   = SADR-LAND1.
    ITCFX-SREGIO     = SADR-REGIO.
    ITCFX-SHOMECNTRY = REGUH-ZLAND.
    ITCFX-SLINES     = '9'.
    ITCFX-SCTITLE    = FSABE-SALUT.
    ITCFX-SCFNAME    = FSABE-FNAME.
    ITCFX-SCLNAME    = FSABE-LNAME.
    ITCFX-SCNAME1    = FSABE-NAMP1.
    ITCFX-SCNAME2    = FSABE-NAMP2.
    ITCFX-SCDEPTM    = FSABE-ABTEI.
    ITCFX-SCCOSTC    = FSABE-KOSTL.
    ITCFX-SCROOMN    = FSABE-ROOMN.
    ITCFX-SCBUILD    = FSABE-BUILD.
    CONCATENATE FSABE-TELF1 FSABE-TEL_EXTEN1
                INTO ITCFX-SCPHONENR1.
    CONCATENATE FSABE-TELF2 FSABE-TEL_EXTEN2
                INTO ITCFX-SCPHONENR2.
    CONCATENATE FSABE-TELFX FSABE-FAX_EXTENS
                INTO ITCFX-SCFAXNR.
    ITCFX-HEADER     = T042T-TXTKO.
    ITCFX-FOOTER     = T042T-TXTFU.
    ITCFX-SIGNATURE  = T042T-TXTUN.
    ITCFX-TDID       = T042T-TXTID.
    ITCFX-TDLANGU    = HLP_SPRACHE.
    ITCFX-SUBJECT    = SPACE.
    CALL FUNCTION 'START_FORM'
         EXPORTING
              ARCHIVE_INDEX = TOA_DARA
              FORM          = FINAA-FORMC
              LANGUAGE      = HLP_SPRACHE
              STARTPAGE     = 'FIRST'.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              WINDOW = 'RECEIVER'.
    CALL FUNCTION 'END_FORM'.
  ENDIF.

* Formular starten
  CALL FUNCTION 'START_FORM'
       EXPORTING
            ARCHIVE_INDEX = TOA_DARA
            FORM          = HLP_FORMULAR
            LANGUAGE      = HLP_SPRACHE.

  IF HLP_XHRFO EQ SPACE.

*   Fenster Info, Element Unsere Nummer (falls diese gefüllt ist)
    IF REGUH-EIKTO NE SPACE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                WINDOW   = 'INFO'
                ELEMENT  = '605'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'INFO'.
        ERR_ELEMENT-ELEMT = '605'.
        ERR_ELEMENT-TEXT  = TEXT_605.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDIF.

*   Fenster Carry Forward, Element Übertrag (außer letzte Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              WINDOW  = 'CARRYFWD'
              ELEMENT = '635'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.
    IF SY-SUBRC EQ 2.
      ERR_ELEMENT-FNAME = HLP_FORMULAR.
      ERR_ELEMENT-FENST = 'CARRYFWD'.
      ERR_ELEMENT-ELEMT = '635'.
      ERR_ELEMENT-TEXT  = TEXT_635.
      COLLECT ERR_ELEMENT.
    ENDIF.

*   Hauptfenster, Element Anschreiben-x (nur auf der ersten Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = HLP_ELEMENT
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.
    IF SY-SUBRC EQ 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '610'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      ERR_ELEMENT-FNAME = HLP_FORMULAR.
      ERR_ELEMENT-FENST = 'MAIN'.
      ERR_ELEMENT-ELEMT = HLP_ELEMENT.
      ERR_ELEMENT-TEXT  = HLP_ELETEXT.
      COLLECT ERR_ELEMENT.
    ENDIF.

*   Hauptfenster, Element Abweichender Zahlungsemfänger
    IF REGUD-XABWZ EQ 'X'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '612'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'MAIN'.
        ERR_ELEMENT-ELEMT = '612'.
        ERR_ELEMENT-TEXT  = TEXT_612.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDIF.

*   Hauptfenster, Element Zahlung erfolgt im Auftrag von
    IF REGUH-ABSBU NE REGUH-ZBUKR.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '613'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'MAIN'.
        ERR_ELEMENT-ELEMT = '613'.
        ERR_ELEMENT-TEXT  = TEXT_613.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDIF.

*   Hauptfenster, Element Unterschrift
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = '614'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.

*   Hauptfenster, Element Überschrift (nur auf der ersten Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = '615'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.
    IF SY-SUBRC EQ 2.
      ERR_ELEMENT-FNAME = HLP_FORMULAR.
      ERR_ELEMENT-FENST = 'MAIN'.
      ERR_ELEMENT-ELEMT = '615'.
      ERR_ELEMENT-TEXT  = TEXT_615.
      COLLECT ERR_ELEMENT.
    ENDIF.

*   Hauptfenster, Element Überschrift (ab der zweiten Seite oben)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = '615'
              TYPE    = 'TOP'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2. "Fehler bereits oben gemerkt

*   Hauptfenster, Element Übertrag (ab der zweiten Seite oben)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = '620'
              TYPE     = 'TOP'
              FUNCTION = 'APPEND'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2.
    IF SY-SUBRC EQ 2.
      ERR_ELEMENT-FNAME = HLP_FORMULAR.
      ERR_ELEMENT-FENST = 'MAIN'.
      ERR_ELEMENT-ELEMT = '620'.
      ERR_ELEMENT-TEXT  = TEXT_620.
      COLLECT ERR_ELEMENT.
    ENDIF.

  ELSE.

*   HR-Formular ausgeben
*   write HR form
    LOOP AT PFORM.
      CHECK SY-TABIX GT T042E-ANZPO.
      REGUD-TXTHR = PFORM-LINDA.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT  = '625-HR'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'MAIN'.
        ERR_ELEMENT-ELEMT = '625-HR'.
        ERR_ELEMENT-TEXT  = TEXT_625.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDLOOP.

  ENDIF.

* Ausgabe der Einzelposten
  FLG_DIFF_BUKRS = 0.
  LOOP AT TAB_REGUP.

    AT NEW BUKRS.
      REGUP-BUKRS = TAB_REGUP-BUKRS.
      IF  ( REGUP-BUKRS NE REGUH-ZBUKR OR FLG_DIFF_BUKRS EQ 1 )
      AND ( REGUH-ABSBU EQ SPACE OR REGUH-ABSBU EQ REGUH-ZBUKR ).
        FLG_DIFF_BUKRS = 1.
        SELECT SINGLE * FROM T001 INTO *T001
          WHERE BUKRS EQ REGUP-BUKRS.
        REGUD-ABSTX = *T001-BUTXT.
        REGUD-ABSOR = *T001-ORT01.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = '613'
             EXCEPTIONS
                  WINDOW  = 1
                  ELEMENT = 2.
        IF SY-SUBRC EQ 2.
          ERR_ELEMENT-FNAME = HLP_FORMULAR.
          ERR_ELEMENT-FENST = 'MAIN'.
          ERR_ELEMENT-ELEMT = '613'.
          ERR_ELEMENT-TEXT  = TEXT_613.
          COLLECT ERR_ELEMENT.
        ENDIF.
      ENDIF.
    ENDAT.

    REGUP = TAB_REGUP.
    PERFORM EINZELPOSTENFELDER_FUELLEN.

    IF HLP_XHRFO EQ SPACE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT  = '625'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'MAIN'.
        ERR_ELEMENT-ELEMT = '625'.
        ERR_ELEMENT-TEXT  = TEXT_625.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDIF.

    PERFORM SUMMENFELDER_FUELLEN.

    IF HLP_XHRFO EQ SPACE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT  = '625-TX'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
    ENDIF.

    AT END OF BUKRS.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT  = '629'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
    ENDAT.
  ENDLOOP.

  PERFORM ZIFFERN_IN_WORTEN.

* Summenfelder hochzählen und aufbereiten
  CASE FINAA-NACHA.
    WHEN '1'.
      ADD 1           TO CNT_AVISE.
      ADD REGUH-RBETR TO SUM_ABSCHLUSS.
    WHEN '2'.
      ADD 1           TO CNT_AVFAX.
      ADD REGUH-RBETR TO SUM_ABSCHL_FAX.
    WHEN 'I'.
      ADD 1           TO CNT_AVMAIL.
      ADD REGUH-RBETR TO SUM_ABSCHL_MAIL.
  ENDCASE.

  WRITE:
    CNT_AVISE       TO REGUD-AVISE,
    CNT_AVEDI       TO REGUD-AVEDI,
    CNT_AVFAX       TO REGUD-AVFAX,
    CNT_AVMAIL      TO REGUD-AVMAIL,
    SUM_ABSCHLUSS   TO REGUD-SUMME  CURRENCY T001-WAERS,
    SUM_ABSCHL_EDI  TO REGUD-SUEDI  CURRENCY T001-WAERS,
    SUM_ABSCHL_FAX  TO REGUD-SUFAX  CURRENCY T001-WAERS,
    SUM_ABSCHL_MAIL TO REGUD-SUMAIL CURRENCY T001-WAERS.
  TRANSLATE:
    REGUD-AVISE  USING ' *',
    REGUD-AVEDI  USING ' *',
    REGUD-AVFAX  USING ' *',
    REGUD-AVMAIL USING ' *',
    REGUD-SUMME  USING ' *',
    REGUD-SUEDI  USING ' *',
    REGUD-SUFAX  USING ' *',
    REGUD-SUMAIL USING ' *'.

  IF HLP_XHRFO EQ SPACE.

*   Hauptfenster, Element Gesamtsumme (nur auf der letzten Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = '630'
              FUNCTION = 'APPEND'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2.
    IF SY-SUBRC EQ 2.
      ERR_ELEMENT-FNAME = HLP_FORMULAR.
      ERR_ELEMENT-FENST = 'MAIN'.
      ERR_ELEMENT-ELEMT = '630'.
      ERR_ELEMENT-TEXT  = TEXT_630.
      COLLECT ERR_ELEMENT.
    ENDIF.

*   Hauptfenster, Element Bankgebühr (Japan)
    IF REGUH-PAYGR+18(2) EQ '$J'.
      WHILE REGUH-PAYGR(1) EQ 0.
        SHIFT REGUH-PAYGR(10) LEFT.
        IF SY-INDEX > 10. EXIT. ENDIF.
      ENDWHILE.
      SUBTRACT REGUH-RSPE1 FROM: REGUD-SWNET, SUM_ABSCHLUSS.
      WRITE:
         REGUD-SWNET TO REGUD-SWNES CURRENCY REGUH-WAERS,
         SUM_ABSCHLUSS  TO REGUD-SUMME CURRENCY T001-WAERS.
      TRANSLATE:
         REGUD-SWNES USING ' *',
         REGUD-SUMME USING ' *'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '634'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'MAIN'.
        ERR_ELEMENT-ELEMT = '634'.
        ERR_ELEMENT-TEXT  = TEXT_634.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDIF.

*   Fenster Carry Forward, Element Übertrag löschen
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              WINDOW   = 'CARRYFWD'
              ELEMENT  = '635'
              FUNCTION = 'DELETE'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2. "Fehler bereits oben gemerkt

*   Hauptfenster, Element Überschrift löschen
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = '615'
              TYPE     = 'TOP'
              FUNCTION = 'DELETE'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2. "Fehler bereits oben gemerkt

*   Hauptfenster, Element Übertrag löschen
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = '620'
              TYPE     = 'TOP'
              FUNCTION = 'DELETE'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2. "Fehler bereits oben gemerkt

*   Hauptfenster, Element Abschlußtext
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = '631'
              FUNCTION = 'APPEND'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2. "Ausgabe ist freigestellt

*   Fenster Total, Element Gesamtsumme
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              WINDOW  = 'TOTAL'
              ELEMENT = '630'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2. "Ausgabe ist freigestellt

  ENDIF.

* Formular beenden
  CALL FUNCTION 'END_FORM'.

ENDFORM.



*----------------------------------------------------------------------*
* FORM AVIS_SCHLIESSEN                                                 *
*----------------------------------------------------------------------*
* Avis schließen und Ausgabetabelle füllen                             *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM AVIS_SCHLIESSEN.

  CHECK FLG_DRUCKMODUS NE 0.

* Abschluß des Formulars
  CALL FUNCTION 'CLOSE_FORM'
       IMPORTING
            RESULT     = ITCPP
       EXCEPTIONS
            SEND_ERROR = 4.

  IF SY-SUBRC NE 0.                    "E-Mailen nicht möglich,
    FIMSG-MSGID = SY-MSGID.            "und zum Ausdruck ist es
    FIMSG-MSGV1 = SY-MSGV1.            "jetzt zu spät
    FIMSG-MSGV2 = SY-MSGV2.
    FIMSG-MSGV3 = SY-MSGV3.
    FIMSG-MSGV4 = SY-MSGV4.
    PERFORM MESSAGE USING SY-MSGNO.
    IF NOT REGUH-PERNR IS INITIAL.
      FIMSG-MSGV1 = REGUH-PERNR.
      FIMSG-MSGV2 = REGUH-SEQNR.
    ELSE.
      FIMSG-MSGV1 = REGUH-ZBUKR.
      FIMSG-MSGV2 = REGUH-VBLNR.
    ENDIF.
    PERFORM MESSAGE USING '388'.
  ELSE.
    CASE HLP_NACHA_LAST.
      WHEN '1'.
        IF ITCPP-TDSPOOLID NE 0.
          CLEAR TAB_AUSGABE.
          TAB_AUSGABE-NAME    = T042Z-TEXT1.
          TAB_AUSGABE-DATASET = ITCPP-TDDATASET.
          TAB_AUSGABE-SPOOLNR = ITCPP-TDSPOOLID.
          TAB_AUSGABE-IMMED   = PAR_SOFA.
          COLLECT TAB_AUSGABE.
        ENDIF.
      WHEN '2'.
        CLEAR TAB_AUSGABE.
        TAB_AUSGABE-NAME      = TEXT_094.
        TAB_AUSGABE-DATASET   = ITCPP-TDDATASET.
        COLLECT TAB_AUSGABE.
      WHEN 'I'.
        CLEAR TAB_AUSGABE.
        TAB_AUSGABE-NAME      = TEXT_095.
        TAB_AUSGABE-DATASET   = ITCPP-TDDATASET.
        COLLECT TAB_AUSGABE.
        COMMIT WORK.
    ENDCASE.
  ENDIF.

  CLEAR FLG_DRUCKMODUS.

ENDFORM.



*----------------------------------------------------------------------*
* FORM FPAYM                                                           *
*----------------------------------------------------------------------*
* Für die übergreifende Sortierung im Programm RFFOAVIS_FPAYM          *
* wurden Felder initialisiert, die nun wieder bereitgestellt           *
* werden müssen                                                        *
*----------------------------------------------------------------------*
* -> EVENT = 1 erster Aufruf pro Zahlung                               *
*            2 weitere Aufrufe pro Zahlung                             *
*            3 letzter Aufruf pro Lauf                                 *
*----------------------------------------------------------------------*
FORM FPAYM USING EVENT.                "FPAYM

  STATICS: UP_ZBUKR LIKE REGUH-ZBUKR,
           UP_HBKID LIKE REGUH-HBKID,
           UP_RZAWE LIKE REGUH-RZAWE,
           UP_XAVIS LIKE REGUH-XAVIS.

  CHECK REGUH-ZBUKR IS INITIAL OR NOT UP_XAVIS IS INITIAL.
  REGUH-RZAWE = SIC_REGUH-RZAWE.
  REGUH-ZBUKR = SIC_REGUH-ZBUKR.
  REGUH-UBNKS = SIC_REGUH-UBNKS.
  REGUH-UBNKY = SIC_REGUH-UBNKY.
  REGUH-UBNKL = SIC_REGUH-UBNKL.
  UP_XAVIS    = 'X'.

  IF EVENT EQ 1.
*   Customizing nachlesen (wurde nicht AT NEW erledigt, da Felder leer)
    ON CHANGE OF REGUH-ZBUKR.
      PERFORM BUCHUNGSKREIS_DATEN_LESEN.
    ENDON.
    ON CHANGE OF REGUH-UBNKS OR REGUH-UBNKY.
      PERFORM HAUSBANK_DATEN_LESEN.
    ENDON.
    ON CHANGE OF REGUH-ZBUKR OR REGUH-RZAWE.
      CLEAR: T042E, T042Z.
      IF REGUH-RZAWE NE SPACE.
        PERFORM ZAHLWEG_DATEN_LESEN.
      ELSE.
        REGUD-AUST1 = T001-BUTXT.
        REGUD-AUST2 = SPACE.
        REGUD-AUST3 = SPACE.
        REGUD-AUSTO = T001-ORT01.
      ENDIF.
      T042Z-TEXT1 = TEXT_001.
    ENDON.

*   Buchungskreis, Zahlweg, Hausbank merken für Event 3
    IF UP_ZBUKR IS INITIAL.
      UP_ZBUKR = REGUH-ZBUKR.
      UP_RZAWE = REGUH-RZAWE.
      UP_HBKID = REGUH-HBKID.
    ELSEIF UP_ZBUKR NE REGUH-ZBUKR.
      UP_ZBUKR = '*'.
      UP_RZAWE = '*'.
      UP_HBKID = '*'.
    ENDIF.
    IF UP_RZAWE NE REGUH-RZAWE.
      UP_RZAWE = '*'.
    ENDIF.
    IF UP_HBKID NE REGUH-HBKID.
      UP_HBKID = '*'.
    ENDIF.
  ENDIF.

  IF EVENT EQ 3.
    REGUH-ZBUKR = UP_ZBUKR.
    REGUH-RZAWE = UP_RZAWE.
    REGUH-HBKID = UP_HBKID.
    IF UP_HBKID EQ '*'.
      REGUH-UBNKS = '*'.
      REGUH-UBNKY = '*'.
      REGUH-UBNKL = '*'.
    ENDIF.
  ENDIF.

ENDFORM.



*----------------------------------------------------------------------*
* FORM MAIL_VORBEREITEN                                                *
*----------------------------------------------------------------------*
* Aus dem Benutzernamen wird das Sender-Objekt, aus der eMail-Adresse  *
* das Empfänger-Objekt erzeugt, welche an SAPscript zu übergeben sind  *
*----------------------------------------------------------------------*
* -> P_UNAME     Benutzer                                              *
* -> P_INTAD     Internet-Adresse                                      *
* <- P_SENDER    Sender-Objekt                                         *
* <- P_RECIPIENT Empfänger-Objekt                                      *
*----------------------------------------------------------------------*
FORM MAIL_VORBEREITEN USING    P_UNAME     LIKE SY-UNAME
                               P_INTAD     LIKE FINAA-INTAD
                      CHANGING P_SENDER    LIKE SWOTOBJID
                               P_RECIPIENT LIKE SWOTOBJID.

* Das include <cntn01> enthält die Definitionen der Makrobefehle zum
* Anlegen und Bearbeiten der Container, d.h. für den Zugriff aufs BOR
  INCLUDE <CNTN01>.

* Datendeklaration der BOR-Objekte
  DATA: SENDER         TYPE SWC_OBJECT,
        RECIPIENT      TYPE SWC_OBJECT.

* Deklaration einer Container-Datenstruktur zur Laufzeit
  SWC_CONTAINER CONTAINER.

*----------------------------------------------------------------------*
* Anlegen eines Senders (BOR-Objekt-ID)                                *
*----------------------------------------------------------------------*

* Erzeugen einer Objektreferenz auf den Objekttyp 'RECIPIENT'
* Die weitere Verarbeitung findet dann für die Objektreferenz
* 'sender' statt
  SWC_CREATE_OBJECT SENDER             " Objektreferenz
                    'RECIPIENT'        " Name eines Objekttyps
                    SPACE.             " objektspezifischer Schlüssel

* Initialisieren des zuvor deklarierten Containers
  SWC_CLEAR_CONTAINER CONTAINER.       " Container

* Unter dem Elementnamen 'AddressString' wird die Adresse des
* aufrufenden internen Benutzers in die Container-Instanz
* container eingetragen
  SWC_SET_ELEMENT CONTAINER            " Container
                  'AddressString'      " Elementname
                  P_UNAME.             " Wert des Elements

* Unter dem Elementnamen 'TypeId' wird der Adreßtyp 'interner
* Benutzer' in die Container-Instanz container eingetragen
  SWC_SET_ELEMENT CONTAINER            " Container
                  'TypeId'             " Elementname
                  'B'.                 " Wert des Elements

* Aufruf der Objektmethode 'FindAddress'
  SWC_CALL_METHOD SENDER               " Objektreferenz
                  'FindAddress'        " Name der Methode
                  CONTAINER.           " Container

* Fehler: Das Element ist nicht im Container enthalten
  IF SY-SUBRC NE 0.
    CLEAR: P_SENDER, P_RECIPIENT.
    FIMSG-MSGID = SY-MSGID.
    PERFORM MESSAGE USING SY-MSGNO.
    EXIT.
  ENDIF.

* Ermittlung der BOR-Objekt-ID
  SWC_OBJECT_TO_PERSISTENT SENDER
                           P_SENDER.

*----------------------------------------------------------------------*
* Anlegen eines Empfängers (BOR-Objekt-ID)                             *
*----------------------------------------------------------------------*

* Erzeugen einer Objektreferenz auf den Objekttyp 'RECIPIENT'.
* Die weitere Verarbeitung findet dann für die Objektreferenz
* 'recipient' statt
  SWC_CREATE_OBJECT RECIPIENT          " Objektreferenz
                    'RECIPIENT'        " Name eines Objekttyps
                    SPACE.             " objektspezifischer Schlüssel

* Initialisieren des zuvor deklarierten Containers
  SWC_CLEAR_CONTAINER CONTAINER.       " Container

* Unter dem Elementnamen 'AddressString' wird die Faxnummer bzw.
* die Internet-Adresse des Empfängers in die Container-Instanz
* container eingetragen
  SWC_SET_ELEMENT CONTAINER            " Container
                  'AddressString'      " Elementname
                  P_INTAD.

* Unter dem Elementnamen 'TypeId' wird der Adreßtyp 'Mail'
* in die Container-Instanz container eingetragen
  SWC_SET_ELEMENT CONTAINER            " Container
                  'TypeId'             " Elementname
                  'U'.                 " Wert des Elements

* Aufruf der Objektmethode 'CreateAddress'
  SWC_CALL_METHOD RECIPIENT            " Objektreferenz
                  'CreateAddress'      " Name der Methode
                  CONTAINER.           " Container

* Fehler: Anlegen des Adreßteils eines Recipient-Objekts nicht möglich
  IF SY-SUBRC NE 0.
    CLEAR: P_SENDER, P_RECIPIENT.
    FIMSG-MSGID = SY-MSGID.
    PERFORM MESSAGE USING SY-MSGNO.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*

* Initialisieren des zuvor deklarierten Containers
  SWC_CLEAR_CONTAINER CONTAINER.       " Container

* Mit dem Attribut 'Deliver' wird die Empfangsbestätigung abgewählt
  SWC_SET_ELEMENT CONTAINER            " Container
                  'Deliver'            " Elementname
                  SPACE.

* Aufruf der Objektmethode 'SetDeliver'
  SWC_CALL_METHOD RECIPIENT            " Objektreferenz
                  'SetDeliver'         " Name der Methode
                  CONTAINER.           " Container

*----------------------------------------------------------------------*

* Initialisieren des zuvor deklarierten Containers
  SWC_CLEAR_CONTAINER CONTAINER.       " Container

* Mit dem Attribut 'NotDeliver' wird die Bestätigung bei Nicht-Empfang
* angefordert
  SWC_SET_ELEMENT CONTAINER            " Container
                  'NotDeliver'         " Elementname
                  'X'.

* Aufruf der Objektmethode 'SetDeliver'
  SWC_CALL_METHOD RECIPIENT            " Objektreferenz
                  'SetDeliver'         " Name der Methode
                  CONTAINER.           " Container

*----------------------------------------------------------------------*

* Initialisieren des zuvor deklarierten Containers
  SWC_CLEAR_CONTAINER CONTAINER.       " Container

* Mit dem Attribut 'Read' wird die Gelesen-Bestätigung abgewählt
  SWC_SET_ELEMENT CONTAINER            " Container
                  'Read'               " Elementname
                  SPACE.

* Aufruf der Objektmethode 'SetDeliver'
  SWC_CALL_METHOD RECIPIENT            " Objektreferenz
                  'SetRead'            " Name der Methode
                  CONTAINER.           " Container

*----------------------------------------------------------------------*

* Ermittlung der BOR-Objekt-ID
  SWC_OBJECT_TO_PERSISTENT RECIPIENT
                           P_RECIPIENT.

* Initialisieren des zuvor deklarierten Containers
  SWC_CLEAR_CONTAINER CONTAINER.

ENDFORM.
