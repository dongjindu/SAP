*----------------------------------------------------------------------*
*   INCLUDE ZRASORT50                                                  *
*----------------------------------------------------------------------*

AT USER-COMMAND.

*     Dummy-Report.
DATA: DUMMY_REP LIKE TRSTI-SONAM.

  CASE SY-UCOMM.
*   Aufruf weiterer Reports (BBS-Schnittstelle).
    WHEN 'OREP'.
*     Summenliste auf Summenliste.
      IF NOT SUMMB IS INITIAL.
        PERFORM SUBMIT_ANLAGENREPORT USING DUMMY_REP 'X'.
*     Einzelliste auf Einzelliste.
      ELSE.
        PERFORM SUBMIT_ANLAGENREPORT USING DUMMY_REP ' '.
      ENDIF.
    WHEN 'FREP'.
*     Es darf nicht aufgepickt werden.
      IF FLG_PICK_UP EQ SPACE.
        MESSAGE E030.
      ENDIF.
      PERFORM SUBMIT_FREMDREPORT.
    WHEN 'HIST'.
      PERFORM BERICHTSHISTORIE_AUFRUFEN.
    WHEN 'BBSZ'.
      PERFORM BERICHT_ZUORDNEN.
    WHEN 'XXL'.
     PERFORM XXL_AUFRUFEN.
    WHEN 'KOMP'.
* Es darf nicht aufgepickt werden.
      IF FLG_PICK_UP EQ SPACE.
        MESSAGE E031.
      ENDIF.
      CLEAR FLG_PICK_UP.
      PERFORM KOMPLEX_AUFLOESEN.
    WHEN 'AICR'.
     PERFORM ARBVOR_AUFBAUEN.
    WHEN 'AIME'.
     PERFORM ARBVOR_AUFNEHMEN.
    WHEN 'AIDL'.
     PERFORM ANLAGE_LOESCHEN_AUS_AV.
    WHEN 'AIRL'.
     PERFORM ARBVOR_FREIGEBEN(RAWORK01).
    WHEN 'AIIN'.
     PERFORM ANLAGE_HINZUFUEGEN_IN_AV.
    WHEN 'AISR'.
     PERFORM ANLAGE_SUCHEN_IN_AV.
    WHEN 'AIER'.
     PERFORM AV_FEHLER_ANZEIGEN(RAWORK01).
    WHEN 'BARC'.
     PERFORM BARCODES_DRUCKEN(RABARC01).
    WHEN 'FRSH'.
     PERFORM DATEN_AUFFRISCHEN(RAWORK01).
    WHEN 'SAVE'.
     PERFORM DATEN_SICHERN(RAWORK01).
    WHEN 'ERLP'.
     PERFORM AV_ERLOES_PLANEN(RAWORK01).
    WHEN 'AIKP'.
     PERFORM AV_KOPFDATEN_AENDERN(RAWORK01).
      CLEAR SY-UCOMM.
    WHEN 'MBCK'.
*--2004/05/21
     perform change_original_t001b.
     leave to screen 0.
     PERFORM AV_BEARBEITUNG_BEENDEN(RAWORK01).

    WHEN 'MCAN'.
*--2004/05/21
     perform change_original_t001b.
     leave to screen 0.
     PERFORM AV_BEARBEITUNG_BEENDEN(RAWORK01).

  ENDCASE.
AT LINE-SELECTION.

*     Hilfsfeld: aufzurufender Report.
DATA: HLP_REPORT LIKE TRSTI-SONAM.

* Funktionalitaet nur auf Grundliste bieten.
  CHECK SY-LSIND LE 1.

* Simulationsvariante im Report-Header aufgepickt?
  IF     RANGE        EQ '8'     AND
     NOT T090U-SIMVAR IS INITIAL .
    LOOP AT X090I.
      WRITE: /001     X090I-AFABE,
                      X090I-ANLKL,
                      X090I-AFASL,
                      X090I-BDATU DD/MM/YYYY,
                      X090I-ADATU DD/MM/YYYY,
                      X090I-AFASLN,
                      X090I-NDPROZ.
      ENDLOOP.
    EXIT.
  ENDIF.

  CASE SY-UCOMM.
  WHEN 'PICK'.
* Es darf nicht aufgepickt werden.
      IF FLG_PICK_UP EQ SPACE.
        MESSAGE E031.
      ENDIF.
      CLEAR FLG_PICK_UP.
      IF SUMMB NE SPACE.
*  Summenbericht
        RANGE = '7'.
      ENDIF.
    CASE RANGE.
*   Fehlermessage aufgepickt?
    WHEN '0'.
      CALL FUNCTION 'HELPSCREEN_NA_CREATE'
        EXPORTING  LANGU   = SY-LANGU
                   MELDUNG = MSGH-MELDUNG
                   MELD_ID = MSGH-MELD_ID
                   MELD_NR = MSGH-MELD_NR
                   TITEL   = MSGH-TITEL
                   MSGV1   = MSGH-MSGV1
                   MSGV2   = MSGH-MSGV2
                   MSGV3   = MSGH-MSGV3
                   MSGV4   = MSGH-MSGV4.
*   Anlage/Auftrag/Projekt/Manf/IPG-Pos aufgepickt?
    WHEN '1'.
      CASE ANLAV-WRTTP.
*     Manf.
      WHEN '4'.
        SET PARAMETER ID 'IAF' FIELD ANLAV-IMAPO.
        CALL TRANSACTION 'IMA3N' AND SKIP FIRST SCREEN.
*     Auftrag.
      WHEN '3'.
        SET PARAMETER ID 'ANR' FIELD ANLAV-EAUFN.
        CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
*     Projekt.
      WHEN '2'.
*       SET PARAMETER ID 'PRO' FIELD HLP_POSID.
        SET PARAMETER ID 'PRO' FIELD ANLAV-TXA50(24).
        SET PARAMETER ID 'PSP' FIELD ' '.
        CALL TRANSACTION 'CJ13' AND SKIP FIRST SCREEN.
*     Investitionsprogrammposition.
      WHEN '1'.
        SET PARAMETER ID 'IMT' FIELD ANLAV-PRNAM.
        SET PARAMETER ID 'IMP' FIELD ANLAV-TXA50(24).
        SET PARAMETER ID 'GJR' FIELD ANLAV-PRGJR.
        CALL TRANSACTION 'IM13' AND SKIP FIRST SCREEN.
*     Anlage.
      WHEN OTHERS.
        IF BEREICH1 IS INITIAL OR
           BEREICH1 = '*'.
          BEREICH1 = CON_LEITBE.
        ENDIF.
        SET PARAMETER ID 'BUK' FIELD ANLAV-BUKRS.
        SET PARAMETER ID 'AN1' FIELD ANLAV-ANLN1.
        SET PARAMETER ID 'AN2' FIELD ANLAV-ANLN2.
        SET PARAMETER ID 'GJR' FIELD SAV_GJAHR.
        SET PARAMETER ID 'AFB' FIELD BEREICH1.
        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'                  "> 516077
          EXPORTING                                            "> 516077
            tcode         = 'AW01N'                            "> 516077
          EXCEPTIONS                                           "> 516077
            OK            = 0                                  "> 516077
            NOT_OK        = 2                                  "> 516077
            OTHERS        = 3.                                 "> 516077

        IF sy-subrc = 0.                                       "> 516077
          CALL TRANSACTION 'AW01N'.                            "> 557638
        ELSE.                                                  "> 516077
          MESSAGE e172(00) WITH 'AW01N'.                       "> 516077
        ENDIF.                                                 "> 516077
      ENDCASE.
*   Belegnummer aufgepickt?
    WHEN '3'.
     IF ANEPV-BELNR IS INITIAL  OR
        ANEK-XANTEI =  CON_NOFIBEL.
       MESSAGE ID 'AA' TYPE 'S' NUMBER '530'.
        ELSE.
         SET PARAMETER ID 'BLN'  FIELD ANEPV-BELNR.
         SET PARAMETER ID 'BUK'  FIELD ANEPV-BUKRS.
         SET PARAMETER ID 'GJR'  FIELD ANEPV-GJAHR.
         CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
     ENDIF.
*    Summenbericht auflösen in Einzelbericht d.h. Neuaufruf des Reports
    WHEN '7'.
*     Aufzurufenden Report bestimmen.
      CASE REPORT.
        WHEN 'RAUSMQ10'.
          HLP_REPORT = 'RAZUGA01'.
*       when 'RAUSAG01'.
*         hlp_report = 'RAABGA01'.
        WHEN OTHERS.
          HLP_REPORT = REPORT.
      ENDCASE.
*     Summenliste auf Einzelliste.
      PERFORM SUBMIT_ANLAGENREPORT USING HLP_REPORT ' '.
    ENDCASE.
  ENDCASE.
