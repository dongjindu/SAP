************************************************************************
*                                                                      *
* Include RFFORI99, used in the payment print reports RFFOxxxz         *
* international subroutines                                            *
*                                                                      *
* subroutine                       called by FORM / by REPORT, INCLUDE *
* -------------------------------------------------------------------- *
* INIT (initialization)                                       RFFOxxxy *
* F4_FORMULAR (value request for layout sets)                 RFFOxxxy *
* DATENTRAEGER_SELEKTIEREN (select data carrier)     RFFODTA0/RFFOEDI2 *
* VORBEREITUNG (preparation)                                  RFFOxxxy *
* PRUEFUNG (checks)                                           RFFOxxxy *
* EXTRACT_VORBEREITUNG (prepare extract at GET REGUH)         RFFOxxxy *
* EXTRACT (extracts data at GET REGUP)                        RFFOxxxy *
* SORTIERUNG (sort)                       EXTRACT/EXTRACT_VORBEREITUNG *
* SORTIERUNG_ASSIGN (help program for sort)                 SORTIERUNG *
* VORZEICHEN_SETZEN (set sign)                    EXTRACT_VORBEREITUNG *
*                                           EINZELPOSTENFELDER_FUELLEN *
* ISOCODE_UMSETZEN (read ISO code)                            RFFORI05 *
*                                            BUCHUNGSKREIS_DATEN_LESEN *
*                                                 ZAHLUNGS_DATEN_LESEN *
* BUCHUNGSKREIS_DATEN_LESEN (read company code data)          RFFORInn *
* ZAHLWEG_DATEN_LESEN (read payment method data)              RFFORInn *
* HAUSBANK_DATEN_LESEN (read house bank data)                 RFFORInn *
* HAUSBANK_KONTO_LESEN (read account data)                    RFFORInn *
* FILL_ITCPO (fill default values for ITCPO)                  RFFORInn *
* MODIFY_ITCPO (modify ITCPO for payment forms on paper)      RFFORInn *
* PRINT_ON  (call new-page print on with appropiate param.)   RFFORInn *
* PRINT_OFF (call new-page print off)                         RFFORInn *
* EMPFBANK_DATEN_LESEN (read payee's bank data)               RFFORInn *
* ZAHLUNGS_DATEN_LESEN (read payment data)                    RFFORInn *
* ZAHLUNGS_DATEN_LESEN_HLP (help program)         ZAHLUNGS_DATEN_LESEN *
* SACHBERARBEITER_KURZINFO (short info field clark)
* HR_REMITTANCE_ACKNOWLEDGEMENT                                EXTRACT *
* HR_REMITTANCE_ACKNOWLEDGE_RFC                          FEHLERMELDUNG *
* HR_FORMULAR_LESEN (read HR form)                         RFFORI01,06 *
* WEISUNGSSCHLUESSEL_LESEN (read instruction keys)            RFFORInn *
* WEISUNGSSCHLUESSEL_UMSETZEN (transpose instruction key)     RFFOxxxy *
* GET_CLEARING_CODE                                           RFFOxxxy *
* ADRESSE_LESEN (read customizing address)                    RFFORInn *
* BANKADRESSE_LESEN (read bank address)                       RFFORI99 *
* ADDR_GET                            ADRESSE_LESEN, BANKADRESSE_LESEN *
* LAENDER_LESEN (read country data)                           RFFORInn *
* ZAHLWEG_EINFUEGEN (insert payment method)                   RFFORInn *
* SUMMENFELDER_INITIALISIEREN (initialize total fields)       RFFORInn *
* EINZELPOSTENFELDER_FUELLEN (fill single item fields)        RFFORInn *
* SUMMENFELDER_FUELLEN (fill total fields)                    RFFORInn *
* DTA_GLOBALS_ERSETZEN (replace globals for DME)           RFFORI04,05 *
* DTA_TEXT_AUFBEREITEN (check text fields of DME)          RFFORI04,05 *
*                                                 DTA_GLOBALS_ERSETZEN *
* DATEN_SICHERN (save data for test print)                    RFFORInn *
* DATEN_ZURUECK (data back after test print)                  RFFORInn *
* ZIFFERN_IN_WORTEN (numbers in words)                        RFFORInn *
* DATUM_IN_DDMMYY (convert date to ddmmyy)                    RFFORInn *
* ABBRUCH_DURCH_UEBERLAUF (form overflow abend)            RFFORI04,05 *
* FEHLERMELDUNGEN (error messages)                            RFFOxxxy *
* MESSAGE (collect of messages)                        FEHLERMELDUNGEN *
*                                                             RFFOxxxy *
* INFORMATION (information)                                   RFFOxxxy *
*                                              ABBRUCH_DURCH_UEBERLAUF *
* AT LINE-SELECTION                                                    *
* BELEGDATEN_SCHREIBEN (append document number)               RFFOxxxy *
* TAB_BELEGE_SCHREIBEN (store table with doc.numbers) DATEI_SCHLIESSEN *
* ZUSATZFELD_FUELLEN (fill add'l field)                       RFFOxxxy *
* TEMSE_OEFFNEN (TemSe open)                                  RFFOxxxy *
* TEMSE_SCHREIBEN (TemSe write)                               RFFOxxxy *
* TEMSE_SCHLIESSEN (TemSe close)                              RFFOxxxy *
* NAECHSTER_INDEX (Next index)                                RFFOxxxy *
* TEMSE_NAME (TemSe name)                                TEMSE_OEFFNEN *
* DATEI_OEFFNEN (Open file)                                   RFFOxxxy *
* DATEI_SCHLIESSEN (Close file)                               RFFOxxxy *
* FUELLEN_REGUT (Fill initial REGUT-fields)                   RFFOxxxy *
* ABSCHLUSS_REGUT (Fill final REGUT-fields)                   RFFOxxxy *
* STORE_ON_FILE (store date either on TemSe or on file-syst)  RFFOxxxy *
* FORM READ_SCB_INDICATOR (reads t015l)                       RFFOxxxy *
*
************************************************************************



*----------------------------------------------------------------------*
* FORM INIT                                                            *
*----------------------------------------------------------------------*
* Belegung der Felder auf dem Selektionsbild aus dem Textpool SAPDBPYF *
*----------------------------------------------------------------------*
* Fill fields on the selection screen from textpool SAPDBPYF           *
*----------------------------------------------------------------------*
FORM init.


  LOOP AT tab_selfields.
    ASSIGN (tab_selfields-field) TO <selfield>.
    PERFORM text(sapdbpyf) USING tab_selfields-text <selfield>.
  ENDLOOP.


ENDFORM.                               "INIT



*----------------------------------------------------------------------*
* FORM F4_FORMULAR                                                     *
*----------------------------------------------------------------------*
* F4 für SAPscript-Formulare                                           *
* F4 for layout sets                                                   *
*----------------------------------------------------------------------*
FORM f4_formular USING layout_set.


* DATA hskeyreport LIKE skeyreport.

* CALL FUNCTION 'DISPLAY_REPORTING_TREE_F4'
*      EXPORTING
*           i_tree_id             = 'SAPF'
*           i_get_standard_client = 'X'
*           i_name_of_first_node  = 'FI-2'
*      IMPORTING
*           e_skeyreport          = hskeyreport
*      EXCEPTIONS
*           OTHERS                = 4.
* IF sy-subrc eq 0 and hskeyreport-report ne space.
*   layout_set = hskeyreport-report.
* ENDIF.
  DATA: hp_form_name LIKE thead-tdform.
  CALL FUNCTION 'DISPLAY_FORM_TREE_F4'
       EXPORTING
            p_tree_name     = 'FI-2'
*                   P_DISPLAY_MODE  = ' '
*                   I_FORM_NAME     =
       IMPORTING
            p_form_name     = hp_form_name
*                   P_FORM_LANGUAGE =
       EXCEPTIONS
*                   CANCELLED       = 1
*                   PARAMETER_ERROR = 2
*                   NOT_FOUND       = 3
            OTHERS          = 4
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF sy-subrc EQ 0 AND hp_form_name NE space.
    layout_set = hp_form_name.
  ENDIF.


ENDFORM.                               "F4 Formular



*----------------------------------------------------------------------*
* FORM DATENTRAEGER_SELEKTIEREN                                        *
*----------------------------------------------------------------------*
* Füllen der Parameter für die Selektion der Belege eines Datenträgers *
* fill parameters to select the documents of a data carrier            *
*----------------------------------------------------------------------*
* RENUM - reference number (FDTA)                                      *
* LAUFD - run date                                                     *
* LAUFI - run identification                                           *
* XVORL - will be SPACE if selection was successfull
* VBLNR - range of document numbers                                    *
*----------------------------------------------------------------------*
FORM datentraeger_selektieren TABLES vblnr
                              USING renum laufd laufi xvorl.


  RANGES up_vblnr FOR reguh-vblnr.
  DATA up_renum(10) TYPE n.
  up_renum = renum.
  renum    = up_renum.
  CALL FUNCTION 'GET_DOCUMENTS'
       EXPORTING
            i_belege     = 'X'
            i_refno      = renum
            i_regut      = 'X'
       IMPORTING
            e_regut      = regut
       TABLES
            tab_belege   = tab_belege30a
       EXCEPTIONS
            no_documents = 1
            no_regut     = 2
            wrong_number = 3.
  IF sy-subrc EQ 1.
    CALL FUNCTION 'GET_DOCUMENTS'
         EXPORTING
              i_belege   = space
              i_refno    = renum
              i_regut    = 'X'
         IMPORTING
              e_regut    = regut
         TABLES
              tab_belege = tab_belege30a.
    IF sy-batch EQ space.
      MESSAGE e288 WITH renum regut-laufd regut-laufi.
    ELSE.
      MESSAGE s288 WITH renum regut-laufd regut-laufi.
      STOP.
    ENDIF.
  ELSEIF sy-subrc NE 0.
    IF sy-batch EQ space.
      MESSAGE e287 WITH renum.
    ELSE.
      MESSAGE s287 WITH renum.
      STOP.
    ENDIF.
  ENDIF.
  laufi = regut-laufi.
  laufd = regut-laufd.
  xvorl = space.
  up_vblnr-option = 'EQ'.
  up_vblnr-sign   = 'I'.
  up_vblnr-high   = space.
  LOOP AT tab_belege30a.
    up_vblnr-low  = tab_belege30a-belnr.
    APPEND up_vblnr.
    tab_vblnr_renum       = tab_belege30a.
    tab_vblnr_renum-renum = renum.
    APPEND tab_vblnr_renum.
  ENDLOOP.
  FREE tab_belege30a.
  vblnr[] = up_vblnr[].


ENDFORM.                               "DATENTRAEGER_SELEKTIEREN



*----------------------------------------------------------------------*
* FORM VORBEREITUNG                                                    *
*----------------------------------------------------------------------*
* Vorbereitung der Verarbeitung                                        *
* preparation                                                          *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM vorbereitung.

  IF sy-binpt EQ space.
    COMMIT WORK.
  ENDIF.

* Laufkennung (Applikation) setzen
* Fill application of run
  hlp_laufk = zw_laufi+5(1).

* Probedruck vorbereiten
* prepare test print
  FIELD-SYMBOLS:
    <tabfeld>.

  DATA:
    up_tab        LIKE dfies-tabname,
    up_dfies      LIKE dfies OCCURS 0 WITH HEADER LINE,
    up_feld(20)   TYPE c,
    up_kreuz(132) TYPE c.

  IF par_anzp GT 0.

    CLEAR up_kreuz WITH 'X'.

    DO 5 TIMES.

      CASE sy-index.
        WHEN 1.
          up_tab  = 'FSABE'.
          up_feld = 'XXX_FSABE-'.
        WHEN 2.
          up_tab  = 'REGUD'.
          up_feld = 'XXX_REGUD-'.
        WHEN 3.
          up_tab  = 'REGUH'.
          up_feld = 'XXX_REGUH-'.
        WHEN 4.
          up_tab  = 'REGUP'.
          up_feld = 'XXX_REGUP-'.
        WHEN 5.
          up_tab  = 'SPELL'.
          up_feld = 'XXX_SPELL-'.
      ENDCASE.

      CALL FUNCTION 'DDIF_NAMETAB_GET'
           EXPORTING
                tabname   = up_tab
           TABLES
                dfies_tab = up_dfies
           EXCEPTIONS
                OTHERS    = 4.
      IF sy-subrc NE 0.
        REFRESH up_dfies.
      ENDIF.

      LOOP AT up_dfies.
        up_feld+10 = up_dfies-fieldname.
        ASSIGN (up_feld) TO <tabfeld>.
        CASE up_dfies-inttype.
          WHEN 'C'.
            <tabfeld> = up_kreuz.
          WHEN 'D'.
            <tabfeld> = '19000101'.
          WHEN OTHERS.
            CLEAR <tabfeld>.
        ENDCASE.
      ENDLOOP.

    ENDDO.

    CLEAR xxx_fsabe-salut.

  ENDIF.

* Vorbereitung für die Berechtigungsprüfung ----------------------------
* prepare authorization checks -----------------------------------------
  IF hlp_laufk NE '*'.
    IF hlp_laufk NE 'P'.               "Prüfung für FI-Bestände
      IF zw_xvorl EQ space.            "checks for FI-data
        hlp_fbtch = 25.
      ELSE.
        hlp_fbtch = 15.
      ENDIF.
      CLEAR flg_koart_auth.
      AUTHORITY-CHECK OBJECT 'F_REGU_KOA'
        ID 'KOART' FIELD 'K'
        ID 'FBTCH' FIELD hlp_fbtch.
      IF sy-subrc EQ 0.
        flg_koart_auth-k = 'X'.
      ENDIF.
      AUTHORITY-CHECK OBJECT 'F_REGU_KOA'
        ID 'KOART' FIELD 'D'
        ID 'FBTCH' FIELD hlp_fbtch.
      IF sy-subrc EQ 0.
        flg_koart_auth-d = 'X'.
      ENDIF.
      IF hlp_laufk EQ 'R'.
        AUTHORITY-CHECK OBJECT 'F_REGU_KOA'
          ID 'KOART' FIELD 'S'
          ID 'FBTCH' FIELD hlp_fbtch.
        IF sy-subrc EQ 0.
          flg_koart_auth-s = 'X'.
        ENDIF.
      ENDIF.
      IF flg_koart_auth EQ space.      "Keine Kontoarten-Berechtigung
        IF sy-batch EQ space.          "no account type authority
          MESSAGE a066 WITH hlp_fbtch.
        ELSE.
          MESSAGE s066 WITH hlp_fbtch.
          MESSAGE s094.
          STOP.
        ENDIF.
      ENDIF.
    ELSE.                              "Prüfung für HR-Bestände
      autha-repid = sy-repid.          "checks for HR-data
      CALL FUNCTION 'HR_PROGRAM_CHECK_AUTHORIZATION'
           EXPORTING
                repid = autha-repid
           IMPORTING
                subrc = hlp_subrc.
      IF hlp_subrc NE 0.
        IF sy-batch EQ space.
          MESSAGE a189.
        ELSE.
          MESSAGE s189.
          MESSAGE s094.
          STOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* Variantennamen merken ------------------------------------------------
* store name of report variant -----------------------------------------
  IF par_vari EQ space.
    par_vari = sy-slset.
  ENDIF.

* Sofortdruckparameter setzen ------------------------------------------
* activate parameters for immediate printing ---------------------------
  IF par_sofo NE space.
    par_sofw = 'X'.
    par_sofz = 'X'.
    par_sofa = 'X'.
    par_sofb = 'X'.
  ENDIF.

* Vorbelegen von Feldern -----------------------------------------------
* fields with fix values -----------------------------------------------
  hlp_maxbetrag = 10000000000000.
  hlp_zeilen    = 0.

  CLEAR txt_uline1 WITH '_'.
  CLEAR txt_uline2 WITH '='.

* Länder mit separatem Bankcode ----------------------------------------
* Countries with separate bank-code ------------------------------------
  REFRESH tab_bankcode.
  tab_bankcode-high   = space.
  tab_bankcode-sign   = 'I'.
  tab_bankcode-option = 'EQ'.
  tab_bankcode-low    = 'A'.   APPEND tab_bankcode.  "Österreich
  tab_bankcode-low    = 'CH'.  APPEND tab_bankcode.  "Schweiz
  tab_bankcode-low    = 'CDN'. APPEND tab_bankcode.  "Kanada
  tab_bankcode-low    = 'D'.   APPEND tab_bankcode.  "Deutschland
  tab_bankcode-low    = 'GB'.  APPEND tab_bankcode.  "Großbritannien
  tab_bankcode-low    = 'FL'.  APPEND tab_bankcode.  "Liechtenstein
  tab_bankcode-low    = 'I'.   APPEND tab_bankcode.  "Italien
  tab_bankcode-low    = 'IRL'. APPEND tab_bankcode.  "Irland
  tab_bankcode-low    = 'IS'.  APPEND tab_bankcode.  "Island
  tab_bankcode-low    = 'USA'. APPEND tab_bankcode.         "USA

* Lesen allgemeiner Texte aus dem Textpool SAPDBPYF --------------------
* Read texts from textpool SAPDBPYF ------------------------------------
  PERFORM text(sapdbpyf) USING:
   001 text_001, 505 text_505, 605 text_605, 800 text_800, 900 text_900,
   002 text_002, 510 text_510, 610 text_610, 801 text_801, 901 text_901,
   003 text_003, 512 text_512, 611 text_611, 802 text_802, 902 text_902,
   004 text_004, 513 text_513, 612 text_612, 803 text_803, 903 text_903,
   005 text_005, 514 text_514, 613 text_613, 804 text_804, 904 text_904,
   006 text_006, 515 text_515, 614 text_614, 805 text_805,
   090 text_090, 520 text_520, 615 text_615, 806 text_806,
   091 text_091, 525 text_525, 620 text_620, 807 text_807,
   092 text_092, 526 text_526, 625 text_625, 809 text_809,
   093 text_093, 530 text_530, 630 text_630, 810 text_810,
   094 text_094, 535 text_535, 634 text_634, 811 text_811,
   095 text_095, 540 text_540, 635 text_635, 812 text_812,
   096 text_096, 545 text_545,               813 text_813,
                 546 text_546,               814 text_814,
                 550 text_550,               815 text_815,
                 555 text_555,               816 text_816,
                                             820 text_820,
                                             821 text_821,
                                             822 text_822,
                                             830 text_830,
                                             831 text_831,
                                             832 text_832,
                                             834 text_834.

* Prüfung, ob Lauf erfolgreich abgeschlossen ---------------------------
* check that payment run was completed successfully --------------------
  IF hlp_laufk NE '*'.                 "keine Prüfung bei Online-Druck
    SELECT SINGLE * FROM reguv         "online print => no check
      WHERE laufd EQ zw_laufd
      AND   laufi EQ zw_laufi.
    IF zw_xvorl EQ space.
      IF reguv-xecht NE 'X'.
        IF sy-batch EQ space.
          MESSAGE a098 WITH zw_laufd zw_laufi.
        ELSE.
          MESSAGE s098 WITH zw_laufd zw_laufi.
          MESSAGE s094.
          STOP.
        ENDIF.
      ENDIF.
    ELSE.
      IF reguv-xvore NE 'X'.
        IF sy-batch EQ space.
          MESSAGE a099 WITH zw_laufd zw_laufi.
        ELSE.
          MESSAGE s099 WITH zw_laufd zw_laufi.
          MESSAGE s094.
          STOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* logisches System des Mandanten lesen
* read logical system of client
  SELECT SINGLE * FROM t000 WHERE mandt EQ sy-mandt.
  REFRESH tab_rfc.

* prüfen, ob IBAN-Funktionalität verfügbar
* check that IBAN function is available
*// 2011.08.11 ECC6 delete by kimyn ================ //*
***  CALL FUNCTION 'CHECK_IBAN_ACTIVE'
***       EXCEPTIONS
***            IBAN_NOT_ACTIVE = 4.
***  IF SY-SUBRC EQ 0.
***    FLG_IBAN = 1.
***  ENDIF.


ENDFORM.                               "VORBEREITUNG



*----------------------------------------------------------------------*
* FORM PRUEFUNG                                                        *
*----------------------------------------------------------------------*
* Prüfung der selektierten Daten                                       *
* Checks of selected data                                              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM pruefung.


* Keine Ausnahmen behandeln
* no exceptions
  IF reguh-vblnr EQ space.
    REJECT.
  ENDIF.

* Nachlesen der Buchungskreisdaten / des Geschäftsjahres
* read company code data / business year
  ON CHANGE OF reguh-zbukr.
    SELECT SINGLE * FROM t001
      WHERE bukrs EQ reguh-zbukr.
  ENDON.
  ON CHANGE OF reguh-zaldt OR t001-periv.
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
         EXPORTING
              i_date  = reguh-zaldt
              i_periv = t001-periv
         IMPORTING
              e_gjahr = regud-gjahr.
    xxx_reguh-zaldt = reguh-zaldt.
  ENDON.

* Berechtigungsprüfungen -----------------------------------------------
* authorization checks -------------------------------------------------
  IF hlp_laufk NA 'P*'.                "Prüfung für FI-Bestände
    AUTHORITY-CHECK OBJECT 'F_REGU_BUK'"checks for FI-data
      ID 'BUKRS' FIELD reguh-zbukr
      ID 'FBTCH' FIELD hlp_fbtch.
    IF sy-subrc NE 0.
      err_auth-autob = 'F_REGU_BUK'.
      err_auth-field = 'BUKRS'.
      err_auth-value = reguh-zbukr.
      err_auth-actvt = hlp_fbtch.
      COLLECT err_auth.
      REJECT.
    ENDIF.
    err_auth-autob = 'F_REGU_KOA'.
    err_auth-field = 'KOART'.
    err_auth-value = space.
    err_auth-actvt = hlp_fbtch.
    IF reguh-lifnr EQ space AND reguh-kunnr EQ space.
      IF flg_koart_auth-s EQ space.
        err_auth-value = 'S'.
      ENDIF.
    ELSEIF reguh-lifnr NE space AND reguh-kunnr NE space.
      IF flg_koart_auth-k EQ space AND flg_koart_auth-d EQ space.
        err_auth-value = 'D / K'.
      ENDIF.
    ELSEIF reguh-lifnr NE space.
      IF flg_koart_auth-k EQ space.
        err_auth-value = 'K'.
      ENDIF.
    ELSEIF flg_koart_auth-d EQ space.
      err_auth-value = 'D'.
    ENDIF.
    IF err_auth-value NE space.
      COLLECT err_auth.
      REJECT.
    ENDIF.
  ENDIF.

* Prüfung, ob Report für diesen Zahlweg zugelassen ist -----------------
* check that report is valid for this payment method -------------------
  IF flg_avis EQ 0.                    "all reports except RFFOAVIS
    IF reguh-rzawe EQ space.
      REJECT.
    ENDIF.
    CLEAR tab_t042z.
    READ TABLE tab_t042z WITH KEY land1 = t001-land1
                                  zlsch = reguh-rzawe.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM t042z
        WHERE land1 EQ t001-land1
        AND   zlsch EQ reguh-rzawe.
      IF sy-subrc NE 0.
        IF sy-batch EQ space.
          MESSAGE a350 WITH reguh-rzawe t001-land1.
        ELSE.
          MESSAGE s350 WITH reguh-rzawe t001-land1.
          MESSAGE s094.
          STOP.
        ENDIF.
      ENDIF.
      tab_t042z = t042z.
      IF tab_t042z-progn EQ sy-repid
        OR par_begl EQ 'D'.            "no check for RFFODTA0 / RFFOEDI0
        tab_t042z-xsele = 'X'.
      ELSE.
        tab_t042z-xsele = space.
        err_t042z-land1 = tab_t042z-land1.
        err_t042z-zlsch = tab_t042z-zlsch.
        APPEND err_t042z.
      ENDIF.
      APPEND tab_t042z.
      SORT tab_t042z.
    ENDIF.
    IF tab_t042z-xsele EQ space.
      REJECT.
    ENDIF.
    t042z       = tab_t042z.
    regud-xeinz = tab_t042z-xeinz.
    hlp_xeuro   = tab_t042z-xeuro.
  ELSE.                                "RFFOAVIS only
    IF reguh-rzawe NE space OR reguh-avisg EQ 'V'.
      REJECT.
    ENDIF.
  ENDIF.

* Prüfung, ob Beleg bereits gebucht ist --------------------------------
* check that payment document is updated -------------------------------
  IF par_belp EQ 'X'
    AND reguh-paygr+18(2) NE '$J'      "$J - restart of Japanese DME
    AND hlp_laufk NE 'P'.              "not HR

    IF reguh-pyord EQ space.           "payment document
      SELECT SINGLE * FROM bkpf
        WHERE bukrs EQ reguh-zbukr
        AND   belnr EQ reguh-vblnr
        AND   gjahr EQ regud-gjahr.
    ELSE.                              "payment order
      CLEAR BKPF.
      SELECT SINGLE * FROM pyordh
        WHERE pyord EQ reguh-pyord.
    ENDIF.

    IF sy-subrc NE 0.
      MOVE-CORRESPONDING reguh TO err_nicht_verbucht.
      COLLECT err_nicht_verbucht.
      REJECT.
    ENDIF.
    IF bkpf-stblg NE space.
      fimsg-msgv1 = reguh-zbukr.
      fimsg-msgv2 = reguh-vblnr.
      PERFORM message USING '381'.
      REJECT.
    ENDIF.
  ENDIF.

* Zahlungsbelegverprobung für Abrechnungsergebnisse
* payment document validation for payroll results
  IF PAR_BELP EQ 'X' AND REGUH-DORIGIN EQ 'HR-PY'.
    DATA UP_DOC1R TYPE DOC1R_FPM.
    DATA UP_DOC1T TYPE DOC1T_FPM.
    CALL FUNCTION 'FI_REF_DOCUMENT_FILL'
         EXPORTING
              IM_PERNR = REGUH-PERNR
              IM_SEQNR = REGUH-SEQNR
              IM_BTZNR = REGUH-BTZNR
         IMPORTING
              EX_DOC1R = UP_DOC1R
              EX_DOC1T = UP_DOC1T.
    CALL FUNCTION 'FI_REF_DOCUMENT_CHECK'
         EXPORTING
              IM_DOC1R  = UP_DOC1R
              IM_DOC1T  = UP_DOC1T
              IM_ORIGIN = REGUH-DORIGIN
         EXCEPTIONS
              NOT_FOUND = 4.
    IF SY-SUBRC NE 0.
      FIMSG-MSGID = SY-MSGID.
      FIMSG-MSGV1 = SY-MSGV1.
      FIMSG-MSGV2 = SY-MSGV2.
      FIMSG-MSGV3 = SY-MSGV3.
      FIMSG-MSGV4 = SY-MSGV4.
      PERFORM MESSAGE USING SY-MSGNO.
      REJECT.
    ENDIF.
  ENDIF.

* Prüfung, ob Beleg mit EDI versendet werden soll
* check whether document should be handled via EDI
  IF zw_edisl EQ space AND reguh-edibn EQ 'X'.
    MOVE-CORRESPONDING reguh TO err_edi.
    APPEND err_edi.
    REJECT.
  ENDIF.

* Abweichender Zahlungsempfänger ---------------------------------------
* alternative payee ----------------------------------------------------
  regud-xabwz   = space.
  IF reguh-empfg(1)    EQ '>' AND      "abweichender Zahlungsempfänger
     reguh-empfg+11(2) NE '>F'.        "im Stamm, aber nicht Filiale
    regud-xabwz = 'X'.
  ENDIF.
  IF reguh-empfg(1)    NE '>' AND      "abweichender Zahlungsempfänger
     reguh-empfg       NE space.       "im Beleg, nicht CPD-Konto
    IF reguh-lifnr NE space.
      SELECT SINGLE * FROM lfa1 WHERE lifnr EQ reguh-lifnr.
      IF sy-subrc EQ 0 AND lfa1-xcpdk EQ space.
        regud-xabwz = 'X'.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM kna1 WHERE kunnr EQ reguh-kunnr.
      IF sy-subrc EQ 0 AND kna1-xcpdk EQ space.
        regud-xabwz = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                               "PRUEFUNG



*----------------------------------------------------------------------*
* FORM EXTRACT_VORBEREITUNG                                            *
*----------------------------------------------------------------------*
* Sortierfelder und Vorzeichen setzen                                  *
* Online-Umsetzung alter Daten:                                        *
*    aus Releases < 2.0   Felder REGUH-UBNKY und REGUH-ZBNKY aus       *
*                         Bankleitzahl oder der Kontonummer füllen     *
*    aus Releases < 3.0   Feld REGUH-AUSFD mit Zahlungsdatum füllen,   *
*                         leeres REGUH-ABSBU (z.B. Online-Scheckdruck) *
*                         mit REGUH-ZBUKR füllen                       *
*    ab Releases 4.0      Feld REGUH-KOINH füllen, falls es leer ist   *
*                         Feld REGUH-VBLNR mit PYORD füllen für die    *
*                         richtige Referenz auf dem Zahlungsträger     *
*----------------------------------------------------------------------*
* fill sort fields and sign                                            *
* correct old data online:                                             *
*    release < 2.0        fill REGUH-UBNKY and REGUH-ZBNKY using       *
*                         bank number or account number                *
*    release < 3.0        fill REGUH-AUSFD with payment date           *
*                         fill REGUH-ABSBU with REGUH-ZBUKR if it is   *
*                         empty (e.g. post and print)                  *
*    as of release 4.0    fill REGUH-KOINH if it is empty              *
*                         fill REGUH-VBLNR with PYORD to have the      *
*                         correct reference on the payment medium      *
*----------------------------------------------------------------------*
FORM extract_vorbereitung.


  PERFORM vorzeichen_setzen USING 'H'.
  PERFORM sortierung USING 'H'.

* Reparatur 2.0 / repair 2.0
  IF reguh-ubnky EQ space.
    IF reguh-ubnkl NE space.
      reguh-ubnky = reguh-ubnkl.
    ELSE.
      reguh-ubnky = reguh-ubknt.
    ENDIF.
  ENDIF.
  IF reguh-zbnky EQ space.
    IF reguh-zbnkl NE space.
      reguh-zbnky = reguh-zbnkl.
    ELSE.
      reguh-zbnky = reguh-zbnkn.
    ENDIF.
  ENDIF.

* Reparatur 3.0 / repair 3.0
  IF reguh-ausfd LT reguh-zaldt.
    MOVE reguh-zaldt TO reguh-ausfd.
  ENDIF.
  IF reguh-absbu EQ space.
    reguh-absbu = reguh-zbukr.
  ENDIF.

* Reparatur 4.0 / repair 4.0
  IF reguh-koinh EQ space.
    reguh-koinh = reguh-znme1.
  ENDIF.
  IF reguh-pyord NE space.
    reguh-vblnr = reguh-pyord.
  ENDIF.


ENDFORM.                               "Extract Vorbereitung



*----------------------------------------------------------------------*
* FORM EXTRACT                                                         *
*----------------------------------------------------------------------*
* Sortierfelder füllen, Daten extrahieren                              *
* HR über erfolgreiche Zahlung informieren                             *
*----------------------------------------------------------------------*
* fill sort fields and extract data                                    *
* send information about payment to HR (3rd party remittance)          *
*----------------------------------------------------------------------*
FORM extract.


  PERFORM hr_remittance_acknowledgement.
  PERFORM sortierung USING 'P'.

* Begin of changes -  UD1K919725

** ENFORCE TO SORT BY KOSTL
  if  R2  eq 'X'.                                           "UD1K919725
    PERFORM sortierung_assign USING                         "UD1K912809
              'REGUP' 'KOSTL' SPACE 10 hlp_sortp1.          "UD1K912809
  else.                                                     "UD1K919725
* Enforce Sort on Time Admin
    PERFORM sortierung_assign USING                         "UD1K912809
              'REGUH' 'ZTELX' SPACE 10 hlp_sortp1.          "UD1K912809
  endif.                                                    "UD1K919725

* End of changes - * Begin - UD1K919725
  EXTRACT daten.
  flg_selektiert = 1.


ENDFORM.                               "Extract



*----------------------------------------------------------------------*
* FORM SORTIERUNG                                                      *
*----------------------------------------------------------------------*
* Vorbelegung der Sortierfelder gemäß den Vorgaben des Benutzers       *
* über T021M                                                           *
*----------------------------------------------------------------------*
* Filling the sort-fields with respect to the user's wishes (T021M)    *
*----------------------------------------------------------------------*
* Parameter SATZ bestimmt, welche Sortierfelder zu füllen sind         *
* parameter SATZ says which sort fields have to be filled              *
*----------------------------------------------------------------------*
FORM sortierung USING satz.

  STATICS:
    st_svarh LIKE t042e-svarh,
    st_svarp LIKE t042e-svarp.

* Lesen der Tabelle T021M mit den Sortiervarianten
  ON CHANGE OF reguh-zbukr OR reguh-rzawe.
    IF NOT reguh-rzawe IS INITIAL.
      SELECT SINGLE * FROM t042e
        WHERE zbukr EQ reguh-zbukr
          AND zlsch EQ reguh-rzawe.

      IF t042e-svarh IS INITIAL.
        st_svarh = hlp_svarh.
      ELSE.
        st_svarh = t042e-svarh.
      ENDIF.

      IF t042e-svarp IS INITIAL.
        st_svarp = hlp_svarp.
      ELSE.
        st_svarp = t042e-svarp.
      ENDIF.

    ELSE.
      st_svarh = hlp_svarh.
      st_svarp = hlp_svarp.
    ENDIF.
  ENDON.

  IF hlp_t021m_h-srvar NE st_svarh.
    SELECT SINGLE * FROM t021m INTO hlp_t021m_h
      WHERE progn = 'RFFO*   '
        AND anwnd = 'REGUH'
        AND srvar = st_svarh.
    IF sy-subrc NE 0.
      CLEAR hlp_t021m_h.
    ENDIF.
  ENDIF.
  IF hlp_t021m_p-srvar NE st_svarp.
    SELECT SINGLE * FROM t021m INTO hlp_t021m_p
      WHERE progn = 'RFFO*   '
        AND anwnd = 'REGUP'
        AND srvar = st_svarp.
    IF sy-subrc NE 0.
      CLEAR hlp_t021m_p.
    ENDIF.
  ENDIF.

* Füllen der Sortierfelder
  IF satz EQ 'H'.
    IF NOT reguh-srtf2 IS INITIAL.
      hlp_sorth1 = reguh-srtf2(16).
      hlp_sorth2 = reguh-srtf2+16(16).
      hlp_sorth3 = reguh-srtf2+32(16).
    ELSE.
      t021m = hlp_t021m_h.
      DO 3 TIMES.
        CASE sy-index.
          WHEN 1.
            PERFORM sortierung_assign USING
             t021m-tnam1 t021m-feld1 t021m-offs1 t021m-leng1 hlp_sorth1.
          WHEN 2.
            PERFORM sortierung_assign USING
             t021m-tnam2 t021m-feld2 t021m-offs2 t021m-leng2 hlp_sorth2.
          WHEN 3.
            PERFORM sortierung_assign USING
             t021m-tnam3 t021m-feld3 t021m-offs3 t021m-leng3 hlp_sorth3.
        ENDCASE.
      ENDDO.
    ENDIF.
  ELSE.
    t021m = hlp_t021m_p.
    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          PERFORM sortierung_assign USING
            t021m-tnam1 t021m-feld1 t021m-offs1 t021m-leng1 hlp_sortp1.
        WHEN 2.
          PERFORM sortierung_assign USING
            t021m-tnam2 t021m-feld2 t021m-offs2 t021m-leng2 hlp_sortp2.
        WHEN 3.
          PERFORM sortierung_assign USING
            t021m-tnam3 t021m-feld3 t021m-offs3 t021m-leng3 hlp_sortp3.
      ENDCASE.
    ENDDO.
  ENDIF.


ENDFORM.                               "SORTIERUNG



*----------------------------------------------------------------------*
* FORM SORTIERUNG_ASSIGN                                               *
*----------------------------------------------------------------------*
* Hilfsprogamm für die Sortierung                                      *
* help program for sort                                                *
*----------------------------------------------------------------------*
FORM sortierung_assign USING tnam feld offs leng sort.


  FIELD-SYMBOLS <feld>.                "Inhalt des Sortierfeldes
  DATA up_feld(21) TYPE c.             "Name des Sortierfeldes aus T021M

  CHECK NOT tnam IS INITIAL AND NOT feld IS INITIAL.
  up_feld    = tnam.                   "REGUH oder REGUP
  up_feld+10 = '-'.
  up_feld+11 = feld.                   "UZAWE,ZPST2,XBLNR etc.
  CONDENSE up_feld NO-GAPS.

  IF up_feld EQ 'REGUSRT-HZPST'.       "PLZ Zahlungsempfänger
    IF reguh-zpst2 NE space.
      up_feld = 'REGUH-ZPST2'.
    ELSE.
      up_feld = 'REGUH-ZPSTL'.
    ENDIF.
  ENDIF.

  IF up_feld EQ 'REGUSRT-HPSTL'.                            "PLZ
    IF reguh-pstl2 NE space.
      up_feld = 'REGUH-PSTL2'.
    ELSE.
      up_feld = 'REGUH-PSTLZ'.
    ENDIF.
  ENDIF.

  ASSIGN TABLE FIELD (up_feld) TO <feld>.
  CLEAR sort.                          "Sortierfeld nur füllen, wenn
  CHECK leng NE 0.                     "T021M-Eintrag nicht leer ist
  ASSIGN <feld>+offs(leng) TO <feld>.
  sort = <feld>.


ENDFORM.                               "SORTIERUNG_ASSIGN



*----------------------------------------------------------------------*
* FORM VORZEICHEN_SETZEN                                               *
*----------------------------------------------------------------------*
* H: Vorzeichen des regulierten Betrags umdrehen (bei Zahlungen)       *
* P: Vorzeichen der Betragsfelder abhängig vom Feld REGUP-SHKZG und    *
*    von T042Z-XEINZ setzen                                            *
*----------------------------------------------------------------------*
* H: change sign of payment amount (if outgoing payment);              *
* P: set sign of invoice amounts regarding REGUP-SHKZG and T042Z-XEINZ *
*----------------------------------------------------------------------*
* Parameter SATZ bestimmt, welche Felder zu behandeln sind (H oder P)  *
* parameter SATZ says which fields have to be maintained (H or P)      *
*----------------------------------------------------------------------*
FORM vorzeichen_setzen USING satz.


  IF satz EQ 'H'.                      "REGUH-Felder behandeln
    "maintain REGUH-data
    IF regud-xeinz EQ space.
      reguh-rbetr = - reguh-rbetr.
      reguh-rskon = - reguh-rskon.
      reguh-rwbtr = - reguh-rwbtr.
      reguh-rwskt = - reguh-rwskt.
    ENDIF.


  ELSE.                                "REGUP-Felder behandeln
    "maintain REGUP-data
    IF ( regud-xeinz EQ space AND regup-shkzg EQ 'H' ) OR
       ( regud-xeinz NE space AND regup-shkzg EQ 'S' ).
      regud-dmbtr = regup-dmbtr.
      regud-wrbtr = regup-wrbtr.
      regud-sknto = regup-sknto.
      regud-wskto = regup-wskto.
      regud-qsteu = regup-qbshh.
      regud-wqste = regup-qbshb.
      IF regup-xanet NE space.
        regud-dmbtr = regup-dmbtr + regup-mwsts.
        regud-wrbtr = regup-wrbtr + regup-wmwst.
      ENDIF.
    ELSE.
      regud-dmbtr = - regup-dmbtr.
      regud-wrbtr = - regup-wrbtr.
      regud-sknto = - regup-sknto.
      regud-wskto = - regup-wskto.
      regud-qsteu = - regup-qbshh.
      regud-wqste = - regup-qbshb.
      regup-qsshb = - regup-qsshb.
      IF regup-xanet NE space.
        regud-dmbtr = - regup-dmbtr - regup-mwsts.
        regud-wrbtr = - regup-wrbtr - regup-wmwst.
      ENDIF.
    ENDIF.

  ENDIF.


ENDFORM.                               "VORZEICHEN_SETZEN



*----------------------------------------------------------------------*
* FORM ISOCODE_UMSETZEN                                                *
*----------------------------------------------------------------------*
* Währungsschlüssel in ISO-Code umsetzen                               *
* read ISO code of currency                                            *
*----------------------------------------------------------------------*
* WAERS - umzusetzende Währungsschlüssel                               *
*         currency code to be maintained                               *
* ISOCD - umgesetzter Währungsschlüssel                                *
*         ISO code of currency                                         *
*----------------------------------------------------------------------*
FORM isocode_umsetzen USING waers isocd.

  sy-subrc = 0.
  IF tcurc-waers NE waers.
    CLEAR tcurc.
    SELECT SINGLE * FROM tcurc
      WHERE waers EQ waers.
  ENDIF.
  IF sy-subrc EQ 0 AND tcurc-isocd NE space.
    isocd = tcurc-isocd.
  ELSE.                                "ISO-Code nicht gefunden
    isocd = waers.                     "ISO code not found
    err_tcurc-waers = waers.
    COLLECT err_tcurc.
  ENDIF.


ENDFORM.                               "ISOCODE_UMSETZEN



*----------------------------------------------------------------------*
* FORM BUCHUNGSKREIS_DATEN_LESEN                                       *
*----------------------------------------------------------------------*
* Tabelle T001 lesen                                                   *
* Ausgabefeld für Hauswährung füllen                                   *
* Brieftexte bereitstellen                                             *
*----------------------------------------------------------------------*
* read table T001                                                      *
* fill print field for local currency                                  *
* read names of standard texts                                         *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM buchungskreis_daten_lesen.


* Tabelle T001 lesen ---------------------------------------------------
* read table T001 ------------------------------------------------------
  CLEAR t001.
  SELECT SINGLE * FROM t001
    WHERE bukrs EQ reguh-zbukr.

  hlp_sprache = t001-spras.
  IF hlp_sprache IS INITIAL.
    fimsg-msgv1 = reguh-zbukr.
    PERFORM message USING '242'.
  ENDIF.

* Avisformular lesen ---------------------------------------------------
* read name of remittance advice form ----------------------------------
  CLEAR t042b.
  SELECT SINGLE * FROM t042b
    WHERE zbukr EQ reguh-zbukr.
  IF sy-subrc NE 0.
    IF sy-batch EQ space.
      MESSAGE a345 WITH reguh-zbukr.
    ELSE.
      MESSAGE s345 WITH reguh-zbukr.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.
  IF hlp_aforn NE space.
    t042b-aforn = hlp_aforn.
  ENDIF.
  IF t042b-aforn EQ space AND par_avis NE space.
    IF sy-batch EQ space.
      MESSAGE a346 WITH reguh-zbukr.
    ELSE.
      MESSAGE s346 WITH reguh-zbukr.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.

* Ausgabefeld für die Hauswährung füllen -------------------------------
* fill print field for local currency ----------------------------------
  IF par_isoc EQ 'X'.                  "ISO code
    PERFORM isocode_umsetzen USING t001-waers regud-hwaer.
  ELSE.
    regud-hwaer = t001-waers.
  ENDIF.

* Brieftexte bereitstellen ---------------------------------------------
* read names of standard texts -----------------------------------------
  CLEAR t042t.
  SELECT SINGLE * FROM t042t
    WHERE bukrs EQ reguh-zbukr.
  IF sy-subrc NE 0.
    MOVE-CORRESPONDING reguh TO err_t042t.
    COLLECT err_t042t.
  ENDIF.
  MOVE-CORRESPONDING t042t TO regud.
  regud-txtko = t042t-txtko.
  regud-txtfu = t042t-txtfu.
  regud-txtun = t042t-txtun.
  regud-txtab = t042t-txtab.


ENDFORM.                               "BUCHUNGSKREIS_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM ZAHLWEG_DATEN_LESEN                                             *
*----------------------------------------------------------------------*
* Textschlüssel für OCRA-Zeile bereitstellen                           *
* Formulardaten (Name, maximale Postenanzahl, Austeller) lesen         *
*----------------------------------------------------------------------*
* read key in code line                                                *
* read form data (form name, line items per form, issuer)              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM zahlweg_daten_lesen.


* Textschlüssel für OCRA-Zeile bereitstellen ---------------------------
* read key in code line ------------------------------------------------
  CLEAR tab_t042z.
  tab_t042z-land1 = t001-land1.
  tab_t042z-zlsch = reguh-rzawe.
  READ TABLE tab_t042z.
  t042z = tab_t042z.
  regud-otxsl = t042z-txtsl.

* Formulardaten (Name, maximale Postenanzahl, Austeller) lesen ---------
* read form data (form name, line items per form, issuer) --------------
  CLEAR t042e.
  SELECT SINGLE * FROM t042e
    WHERE zbukr EQ reguh-zbukr
    AND   zlsch EQ reguh-rzawe.
  IF sy-subrc NE 0.
    IF sy-batch EQ space.
      MESSAGE a347 WITH reguh-rzawe reguh-zbukr.
    ELSE.
      MESSAGE s347 WITH reguh-rzawe reguh-zbukr.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.
  IF NOT t042e-xsavi IS INITIAL.       "see note 365942
    t042e-xavis = 'X'.
  ENDIF.
  IF hlp_zforn NE space.               "Formular überschreiben, wenn als
    t042e-zforn = hlp_zforn.           "Parameter vorgegeben
  ENDIF.                               "overwrite form name if wished
  IF t042e-zforn EQ space AND par_zdru NE space.
    IF sy-batch EQ space.
      MESSAGE a348 WITH reguh-rzawe reguh-zbukr.
    ELSE.
      MESSAGE s348 WITH reguh-rzawe reguh-zbukr.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.
  IF t042e-wforn EQ space AND flg_zettel EQ 1 AND
    ( par_xdta NE space OR t042z-xswec NE space ).
    IF sy-batch EQ space.
      MESSAGE a349 WITH reguh-rzawe reguh-zbukr.
    ELSE.
      MESSAGE s349 WITH reguh-rzawe reguh-zbukr.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.
  regud-aust1 = t042e-aust1.
  regud-aust2 = t042e-aust2.
  regud-aust3 = t042e-aust3.
  regud-austo = t042e-austo.

* Sortierdaten lesen
* read sort data
  SELECT SINGLE * FROM t021m INTO hlp_t021m_h
    WHERE progn = 'RFFO*   '
      AND anwnd = 'REGUH'
      AND srvar = t042e-svarh.
  IF sy-subrc NE 0.
    CLEAR hlp_t021m_h.
  ENDIF.
  SELECT SINGLE * FROM t021m INTO hlp_t021m_p
    WHERE progn = 'RFFO*   '
      AND anwnd = 'REGUP'
      AND srvar = t042e-svarp.
  IF sy-subrc NE 0.
    CLEAR hlp_t021m_p.
  ENDIF.


ENDFORM.                               "ZAHLWEG_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM HAUSBANK_DATEN_LESEN                                            *
*----------------------------------------------------------------------*
* Hausbank-Anschriftsdaten lesen                                       *
* Bankleitzahl ohne Aufbereitungszeichen für OCRA-Zeile speichern      *
*----------------------------------------------------------------------*
* read house bank address                                              *
* store numerical bank number                                          *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM hausbank_daten_lesen.


* Hausbank-Anschriftsdaten lesen ---------------------------------------
* read house bank address ----------------------------------------------
  CLEAR bnka.
  SELECT SINGLE * FROM bnka
    WHERE banks EQ reguh-ubnks
    AND   bankl EQ reguh-ubnky.
  regud-ubnka    = bnka-banka.
  regud-ubstr    = bnka-stras.
  regud-ubort    = bnka-ort01.
  regud-ubank    = bnka-banka.
  regud-ubank+61 = bnka-ort01.
  CONDENSE regud-ubank.
  regud-ubrch    = bnka-brnch.

* Bankleitzahl ohne Aufbereitungszeichen für OCRA-Zeile speichern ------
* store numerical bank number ------------------------------------------
  regud-obnkl = reguh-ubnkl.


ENDFORM.                               "HAUSBANK_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM HAUSBANK_KONTO_LESEN                                            *
*----------------------------------------------------------------------*
* Hausbank-Konto lesen                                                 *
*----------------------------------------------------------------------*
* read account at house bank                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM hausbank_konto_lesen.


* Hausbank-Konto lesen
* read account at house bank
  IF   t012k-bukrs NE reguh-zbukr
    OR t012k-hbkid NE reguh-hbkid
    OR t012k-hktid NE reguh-hktid.

    IF    tab_t012k-bukrs EQ reguh-zbukr
      AND tab_t012k-hbkid EQ reguh-hbkid
      AND tab_t012k-hktid EQ reguh-hktid.

      t012k    = tab_t012k.
      sy-subrc = 0.

    ELSE.

      READ TABLE tab_t012k WITH KEY bukrs = reguh-zbukr
                                    hbkid = reguh-hbkid
                                    hktid = reguh-hktid.
      IF sy-subrc = 0.
        t012k = tab_t012k.
      ELSE.
        SELECT SINGLE * FROM t012k
          WHERE bukrs EQ reguh-zbukr
            AND hbkid EQ reguh-hbkid
            AND hktid EQ reguh-hktid.
        IF sy-subrc = 0.
          tab_t012k = t012k.
          APPEND tab_t012k.
        ELSE.
          IF sy-batch EQ space.
            MESSAGE a095 WITH 'T012K' reguh-zbukr reguh-hbkid
                                      reguh-hktid.
          ELSE.
            MESSAGE s095 WITH 'T012K' reguh-zbukr reguh-hbkid
                                      reguh-hktid.
            MESSAGE s094.
            STOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                               "HAUSBANK_KONTO_LESEN



*----------------------------------------------------------------------*
* FORM FILL_ITCPO                                                      *
*----------------------------------------------------------------------*
* Füllen der Struktur itcpo                                            *
* fill structure itcpo                                                 *
*----------------------------------------------------------------------*
* p_tddest       - Druckername                 name of printer         *
* p_tddataset    - Datasetname                 dataset name            *
* p_tdimmed      - sofort drucken              print immediatly        *
*----------------------------------------------------------------------*
FORM fill_itcpo USING p_tddest     LIKE itcpo-tddest
                      p_tddataset  LIKE itcpo-tddataset
                      p_tdimmed    LIKE itcpo-tdimmed
                      p_tdautority LIKE itcpo-tdautority.

  CLEAR itcpo.
  itcpo-tdpageslct  = space.           "all pages
  itcpo-tdnewid     = 'X'.             "create new spool dataset
  itcpo-tdcopies    = 1.               "one copy
  itcpo-tddest      = p_tddest.        "name of printer
  itcpo-tdpreview   = space.           "no preview
  itcpo-tdcover     = space.           "no cover page
  itcpo-tddataset   = p_tddataset.     "dataset name
  IF zw_xvorl EQ space.
    itcpo-tdsuffix1 = p_tddest.        "name or printer
  ELSE.
    itcpo-tdsuffix1 = 'TEST'.          "test run
  ENDIF.
  itcpo-tdsuffix2   = par_vari.        "name of report variant
  itcpo-tdimmed     = p_tdimmed.       "print immediately?
  itcpo-tddelete    = space.           "do not delete after print
  itcpo-tdtitle     = t042z-text1.     "title of pop-up-window
  itcpo-tdcovtitle  = t042z-text1.     "title of print-cover
  itcpo-tdautority  = p_tdautority.    "print authority
  itcpo-tdarmod     = 1.               "print only

ENDFORM.                               " FILL_ITCPO



*----------------------------------------------------------------------*
* FORM FILL_ITCPO_FROM_ITCPP                                           *
*----------------------------------------------------------------------*
* Füllen der Struktur ITCPO aus der Resultatstruktur ITCPP             *
* fill structure ITCPO from the result structure ITCPP                 *
*----------------------------------------------------------------------*
FORM fill_itcpo_from_itcpp.

  itcpo-tdpageslct  = space.
  itcpo-tdcopies    = itcpp-tdcopies.
  itcpo-tddest      = itcpp-tddest.
  itcpo-tdpreview   = itcpp-tdpreview.
  itcpo-tdcover     = itcpp-tdcover.
  itcpo-tddataset   = itcpp-tddataset.
  itcpo-tdsuffix1   = itcpp-tdsuffix1.
  itcpo-tdsuffix2   = itcpp-tdsuffix2.
  itcpo-tdimmed     = itcpp-tdimmed.
  itcpo-tddelete    = itcpp-tddelete.
  itcpo-tdtitle     = itcpp-tdtitle.
  itcpo-tdcovtitle  = itcpp-tdcovtitle.
  itcpo-tdautority  = itcpp-tdautority.
  itcpo-tdreceiver  = itcpp-tdreceiver.
  itcpo-tddivision  = itcpp-tddivision.
  itcpo-tdlifetime  = itcpp-tdlifetime.
  itcpo-tdarmod     = 1.

ENDFORM.                               " FILL_ITCPO_FROM_ITCPP



*----------------------------------------------------------------------*
* FORM MODIFY_ITCPO                                                    *
*----------------------------------------------------------------------*
* Modify ITCPO and set archive parameters for optical archiving        *
* Routine can only be used for payment medium on paper !!!             *
* Test prints are not allowed in case of optical archiving !!!         *
*----------------------------------------------------------------------*
FORM modify_itcpo.


  DATA up_repid LIKE sy-repid.
  up_repid = sy-repid.
  CLEAR:
    toa_dara,
    arc_params.
  CALL FUNCTION 'OPEN_FI_PERFORM_00002060_P'
       EXPORTING
            i_reguh          = reguh
            i_gjahr          = regud-gjahr
            i_repid          = up_repid
            i_aforn          = t042e-zforn
       CHANGING
            c_itcpo          = itcpo
            c_archive_index  = toa_dara
            c_archive_params = arc_params.
  IF itcpo-tdarmod GT 1 AND par_anzp NE 0.                "#EC PORTABLE
    par_anzp = 0.
    PERFORM message USING '384'.
  ENDIF.


ENDFORM.                               " MODIFY_ITCPO



*----------------------------------------------------------------------*
* FORM PRINT_ON                                                        *
*----------------------------------------------------------------------*
* new-page print on mit passender Parametrisierung aufrufen            *
* call new-page print on with appropiate parametrization               *
*----------------------------------------------------------------------*
* P_BUKRS        - Buchungskreis               company code            *
* P_COVER_TEXT   - Titel des Spoolauftrags     title of spool request  *
* P_DESTINATION  - Ausgabegerät                output device           *
* P_IMMEDIATELY  - Druck sofort?               print immediately?      *
* P_LIST_DATASET - Name des Spool-Datasets     name of spool dataset   *
*----------------------------------------------------------------------*
FORM print_on USING p_bukrs        LIKE t001-bukrs
                    p_cover_text   TYPE any
                    p_destination  LIKE rfpdo-fordprib
                    p_immediately  LIKE tlsep-sofor
                    p_list_dataset LIKE tlsep-listn.


  DATA: BEGIN OF up_param,
          cpage LIKE tlsep-cpage,
          nllid LIKE tlsep-nllid,
          keeps LIKE tlsep-keeps,
          layot LIKE tlsep-layot,
        END OF up_param.

* TLSEP lesen ---------------------------------------------------------*
* read TLSEP  ---------------------------------------------------------*
  CLEAR sy-spono.
  SELECT SINGLE * FROM  tlsep
         WHERE  domai       = 'BUKRS'
         AND    werte       = p_bukrs.

  IF sy-subrc = 0.
    MOVE-CORRESPONDING tlsep TO up_param.
    IF up_param-layot IS INITIAL.
      up_param-layot = 'X_65_132'.
    ENDIF.
  ELSE.
    up_param-cpage = ' '.
    up_param-nllid = 'X'.
    up_param-keeps = 'X'.
    up_param-layot = 'X_65_132'.
  ENDIF.

  IF p_immediately EQ 'X'   AND        "immediate printing requested and
     sy-batch EQ space      AND        "NO batch-processing   BUT:
     p_destination EQ space OR         "printer not specified
     par_begl EQ 'D'        AND
     sy-tcode EQ 'FDTA'.
    NEW-PAGE
      PRINT ON
      LINE-SIZE                132
      LIST NAME                par_vari
      LIST AUTHORITY           hlp_auth
      DESTINATION              p_destination
      COVER TEXT               p_cover_text
      LIST DATASET             p_list_dataset
      IMMEDIATELY              p_immediately
      NEW LIST IDENTIFICATION  up_param-nllid
      KEEP IN SPOOL            up_param-keeps
      LAYOUT                   up_param-layot
      SAP COVER PAGE           up_param-cpage.
  ELSE.
    NEW-PAGE
      PRINT ON
      LINE-SIZE                132
      LIST NAME                par_vari
      LIST AUTHORITY           hlp_auth
      DESTINATION              p_destination
      COVER TEXT               p_cover_text
      LIST DATASET             p_list_dataset
      IMMEDIATELY              p_immediately
      NEW LIST IDENTIFICATION  up_param-nllid
      KEEP IN SPOOL            up_param-keeps
      LAYOUT                   up_param-layot
      SAP COVER PAGE           up_param-cpage
      NO DIALOG.
  ENDIF.


ENDFORM.                               "PRINT_ON



*----------------------------------------------------------------------*
* FORM PRINT_OFF                                                       *
*----------------------------------------------------------------------*
* new-page print off mit passender Parametrisierung aufrufen           *
* call new-page print off with appropiate parametrization              *
*----------------------------------------------------------------------*
* P_DATASET     - Name des Spool-Datasets      name of spool dataset   *
* P_NAME        - Name der Ausgabeliste        name of output list     *
*----------------------------------------------------------------------*
FORM print_off USING p_dataset  LIKE tab_ausgabe-dataset
                     p_name     TYPE any.


  NEW-PAGE PRINT OFF.
  CHECK sy-spono NE 0.
  CLEAR tab_ausgabe.
  tab_ausgabe-name    = p_name.
  tab_ausgabe-dataset = p_dataset.
  tab_ausgabe-spoolnr = sy-spono.
  COLLECT tab_ausgabe.


ENDFORM.                               "PRINT_OFF



*----------------------------------------------------------------------*
* FORM EMPFBANK_DATEN_LESEN                                            *
*----------------------------------------------------------------------*
* Empfängerbank-Anschriftsdaten lesen                                  *
* read address of payee                                                *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM empfbank_daten_lesen.


* Empfängerbank-Anschriftsdaten lesen ----------------------------------
* read address of payee ------------------------------------------------
  CLEAR bnka.
  SELECT SINGLE * FROM bnka
    WHERE banks EQ reguh-zbnks
    AND   bankl EQ reguh-zbnky.
  regud-zbnka    = bnka-banka.
  regud-zbstr    = bnka-stras.
  regud-zbort    = bnka-ort01.
  regud-zbank    = bnka-banka.
  regud-zbank+61 = bnka-ort01.
  CONDENSE regud-zbank.
  regud-zbrch    = bnka-brnch.

* Bankleitzahl ohne Aufbereitungszeichen für Begleitliste und DTA ------
* store numerical bank number ------------------------------------------
  hlp_zbnkl      = reguh-zbnkl.
  regud-ozbkl    = reguh-zbnkl.


ENDFORM.                               "EMPFBANK_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM ZAHLUNGS_DATEN_LESEN                                            *
*----------------------------------------------------------------------*
* Zahlungsbelegnummer mit führenden Nullen für OCRA-Zeile speichern    *
* Textschlüssel bei HR-Beständen modifizieren                          *
* Ausgabefeld für die Belegwährung füllen                              *
* Ländername des Zahlungsempfängers lesen                              *
* Ausgabefelder für Postleitzahl und Ort füllen                        *
* Buchhaltungssachbearbeiter lesen                                     *
* Absendenden Buchungskreis lesen                                      *
* Datum in Worten                                                      *
*----------------------------------------------------------------------*
* store payment document number with leading zeros for code line       *
* modify key in code line for HR data                                  *
* fill print field for currency                                        *
* read country name of payee                                           *
* read vehicle country key                                             *
* read accounting clerk name                                           *
* read sending company code                                            *
* dates in words                                                       *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM zahlungs_daten_lesen.


* Leeren der Einzelposten-Tabelle - Verwendung: Avis, User-Exit --------
* refresh table with single items - use: payment advice, user-exit -----
  REFRESH tab_regup.
  CLEAR tab_regup.
  PERFORM zahlungs_daten_lesen_hlp.


ENDFORM.                               "ZAHLUNGS_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM ZAHLUNGS_DATEN_LESEN_HLP                                        *
*----------------------------------------------------------------------*
* Hilfsprogamm für das Lesen der Zahlungsdaten                         *
* help program for read of payment data                                *
*----------------------------------------------------------------------*
FORM zahlungs_daten_lesen_hlp.


  STATICS up_sprache LIKE hlp_sprache.

  STATICS: BEGIN OF UP_UIBAN OCCURS 0,
             ZBUKR LIKE REGUH-ZBUKR,
             HBKID LIKE REGUH-HBKID,
             HKTID LIKE REGUH-HKTID,
             UIBAN LIKE REGUD-UIBAN,
           END OF UP_UIBAN.
  DATA:    UP_BKREF LIKE REGUH-BKREF.

  DATA BEGIN OF up_t001.
          INCLUDE STRUCTURE t001.
  DATA END OF up_t001.

  DATA: up_plort LIKE szad_field-addr_dc,
        up_pfstr LIKE szad_field-addr_dc.

* Sprache bestimmen, in der das Formular gelesen werden soll
  IF par_espr EQ 'X'.
    hlp_sprache = reguh-zspra.         "Empfängersprache
  ELSE.
    hlp_sprache = t001-spras.          "Buchungskreissprache
  ENDIF.

* Ausgabeformat des Zahlungsempfängers einstellen (Datum, Betrag)
  SET COUNTRY reguh-zland.
  IF sy-subrc NE 0.
    SET COUNTRY space.
  ENDIF.

* Zahlweg für den Formularabschluß
* payment method for summary
  regud-zwels = reguh-rzawe.

* Nummern mit führenden Nullen für OCRA-Zeile speichern ----------------
* store numbers with leading zeros for code line -----------------------
  regud-ovbln = reguh-vblnr.
  regud-ozbkt = reguh-zbnkn.

* IBAN -----------------------------------------------------------------
  IF FLG_IBAN EQ 1.

*   IBAN des Zahlungsempfängers ermitteln
*   determine IBAN of payee
    IF REGUH-ZIBAN IS INITIAL.
      CALL FUNCTION 'READ_IBAN_FROM_DB'
           EXPORTING
                I_BANKS           = REGUH-ZBNKS
                I_BANKL           = REGUH-ZBNKY
                I_BANKN           = REGUH-ZBNKN
                I_BKONT           = REGUH-ZBKON
                I_BKREF           = REGUH-BKREF
           IMPORTING
                E_IBAN            = REGUD-ZIBAN
                E_IBAN_VALID_FROM = HLP_DATE.
      IF HLP_DATE GT SY-DATLO.
        CLEAR REGUD-ZIBAN.
      ENDIF.
    ELSE.
      REGUD-ZIBAN = REGUH-ZIBAN.  "from external via PAYRQ, e.g. IHC
    ENDIF.

*   IBAN für unserer Hausbankkonto
*   IBAN for our house bank account
    READ TABLE UP_UIBAN WITH KEY ZBUKR = REGUH-ZBUKR
                                 HBKID = REGUH-HBKID
                                 HKTID = REGUH-HKTID.
    IF SY-SUBRC NE 0.
      CLEAR UP_UIBAN.
      UP_UIBAN-ZBUKR = REGUH-ZBUKR.
      UP_UIBAN-HBKID = REGUH-HBKID.
      UP_UIBAN-HKTID = REGUH-HKTID.
      SELECT SINGLE * FROM T012K WHERE BUKRS EQ REGUH-ZBUKR
                                 AND   HBKID EQ REGUH-HBKID
                                 AND   HKTID EQ REGUH-HKTID.
      IF SY-SUBRC EQ 0.
        UP_BKREF = T012K-REFZL.
      ENDIF.
      CALL FUNCTION 'READ_IBAN_FROM_DB'
           EXPORTING
                I_BANKS           = REGUH-UBNKS
                I_BANKL           = REGUH-UBNKY
                I_BANKN           = REGUH-UBKNT
                I_BKONT           = REGUH-UBKON
                I_BKREF           = UP_BKREF
           IMPORTING
                E_IBAN            = UP_UIBAN-UIBAN
                E_IBAN_VALID_FROM = HLP_DATE.
      IF HLP_DATE GT SY-DATLO.
        CLEAR UP_UIBAN-UIBAN.
      ENDIF.
      APPEND UP_UIBAN.
    ENDIF.
    REGUD-UIBAN = UP_UIBAN-UIBAN.

  ENDIF.

* Textschlüssel bei HR-Beständen modifizieren --------------------------
* modify key in code line for HR data ----------------------------------
  hrxblnr = regup-xblnr.
  IF hlp_laufk EQ 'P'                  "bei HR-Beständen ist spezieller
    AND T042Z-XSCHK EQ SPACE           "Textschlüssel zu verwenden
    AND HRXBLNR-TXTSL NE SPACE.        "(falls gefüllt, leer aus PU11)
    t042z-txtsl = hrxblnr-txtsl.       "use special text key for HR data
  ENDIF.
  regud-otxsl = t042z-txtsl.

* Ausgabefeld für die Belegwährung füllen ------------------------------
* fill print field for currency ----------------------------------------
  IF par_isoc EQ 'X'.                  "ISO Code
    PERFORM isocode_umsetzen USING reguh-waers regud-waers.
  ELSE.
    regud-waers = reguh-waers.
  ENDIF.

* Ländername des Zahlungsempfängers lesen ------------------------------
* read country name of payee -------------------------------------------
  CLEAR t005t.
  SELECT SINGLE * FROM t005t
    WHERE spras EQ hlp_sprache
    AND   land1 EQ reguh-land1.
  regud-landx = t005t-landx.

  IF reguh-land1 NE reguh-zland.
    CLEAR t005t.
    SELECT SINGLE * FROM t005t
      WHERE spras EQ hlp_sprache
      AND   land1 EQ reguh-zland.
  ENDIF.
  regud-zlndx = t005t-landx.

* Bezeichnung der Region lesen -----------------------------------------
* read name of region --------------------------------------------------
  CLEAR t005u.
  SELECT SINGLE * FROM t005u
    WHERE spras EQ hlp_sprache
    AND   land1 EQ reguh-zland
    AND   bland EQ reguh-zregi.
  regud-zregx = t005u-bezei.

* Ausgabefelder für Postleitzahl und Ort füllen ------------------------
* fill print field for postal code and city ----------------------------
  IF reguh-name1 NE space.
    IF reguh-adrnr IS INITIAL.
      CLEAR adrs.
      adrs-name1 = reguh-name1.
      adrs-stras = reguh-stras.
      adrs-pfach = reguh-pfach.
      adrs-pstl2 = reguh-pstl2.
      adrs-land1 = reguh-land1.
      adrs-pstlz = reguh-pstlz.
      adrs-ort01 = reguh-ort01.
      adrs-regio = reguh-regio.
      adrs-inlnd = t001-land1.
      adrs-anzzl = '4'.
      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
           EXPORTING
                adrswa_in  = adrs
           IMPORTING
                adrswa_out = adrs.
      regud-plort = adrs-lined.
      regud-pfstr = adrs-lined0.
    ELSE.
      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
           EXPORTING
                address_type           = '1'
                SENDER_COUNTRY         = T001-LAND1
                address_number         = reguh-adrnr
                number_of_lines        = '4'
           IMPORTING
                address_data_carrier   = up_plort
                address_data_carrier_0 = up_pfstr.
      regud-plort = up_plort.
      regud-pfstr = up_pfstr.
    ENDIF.
  ELSE.
    regud-plort = space.
    regud-pfstr = space.
  ENDIF.

  IF reguh-zadnr IS INITIAL.
    CLEAR adrs.
    adrs-name1 = reguh-znme1.
    adrs-stras = reguh-zstra.
    adrs-pfach = reguh-zpfac.
    adrs-pstl2 = reguh-zpst2.
    adrs-pfort = reguh-zpfor.
    adrs-land1 = reguh-zland.
    adrs-pstlz = reguh-zpstl.
    adrs-ort01 = reguh-zort1.
    adrs-ort02 = reguh-zort2.
    adrs-regio = reguh-zregi.
    adrs-inlnd = t001-land1.
    adrs-anzzl = '4'.
    CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
         EXPORTING
              adrswa_in  = adrs
         IMPORTING
              adrswa_out = adrs.
    regud-zplor = adrs-lined.
    regud-zpfst = adrs-lined0.
  ELSE.
    CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
         EXPORTING
              address_type           = '1'
              SENDER_COUNTRY         = T001-LAND1
              address_number         = reguh-zadnr
              number_of_lines        = '4'
         IMPORTING
              address_data_carrier   = up_plort
              address_data_carrier_0 = up_pfstr.
    regud-zplor = up_plort.
    regud-zpfst = up_pfstr.
  ENDIF.

* Name mit Schutzsternen -----------------------------------------------
* name with protective asterisks ---------------------------------------
  CLEAR regud-znm1s.
  TRANSLATE regud-znm1s USING ' *'.
  sy-fdpos = strlen( reguh-znme1 ).
  IF sy-fdpos GT 0.
    regud-znm1s(sy-fdpos) = reguh-znme1.
  ENDIF.
  CLEAR regud-znm2s.
  TRANSLATE regud-znm2s USING ' *'.
  sy-fdpos = strlen( reguh-znme2 ).
  IF sy-fdpos GT 0.
    regud-znm2s(sy-fdpos) = reguh-znme2.
  ENDIF.

* Buchhaltungssachbearbeiter lesen -------------------------------------
* read accounting clerk data -------------------------------------------
  IF t001s-bukrs NE reguh-absbu OR t001s-busab NE reguh-busab
                                OR hlp_sprache NE up_sprache.
    up_sprache = hlp_sprache.
    CLEAR: fsabe, t001s, regud-ubusa.
    CALL FUNCTION 'CORRESPONDENCE_DATA_BUSAB'
         EXPORTING
              i_bukrs = reguh-absbu
              i_busab = reguh-busab
              i_langu = hlp_sprache
         IMPORTING
              e_t001s = t001s
              e_fsabe = fsabe
         EXCEPTIONS
              OTHERS  = 01.
    IF sy-subrc EQ 0.
      PERFORM sachbearbeiter_kurzinfo USING fsabe regud-ubusa.
    ENDIF.
    IF regud-ubusa EQ space.
      regud-ubusa = t001s-sname.
    ENDIF.
  ENDIF.

* Absendenden Buchungskreis lesen --------------------------------------
* read sending company code --------------------------------------------
  regud-abstx = space.
  regud-absor = space.
  IF reguh-absbu NE reguh-zbukr.
    SELECT SINGLE * FROM t001 INTO up_t001
      WHERE bukrs EQ reguh-absbu.
    regud-abstx = up_t001-butxt.
    regud-absor = up_t001-ort01.
  ENDIF.

* Buchungsdatum des Zahlungsbelegs (in Worten) -------------------------
* posting date of payment document (in words) --------------------------
  SELECT SINGLE * FROM t015m
    WHERE spras EQ hlp_sprache
    AND   monum EQ reguh-zaldt+4(2).
  IF sy-subrc EQ 0.
    regud-zaliw(2)   = reguh-zaldt+6(2).
    regud-zaliw+2(2) = '. '.
    regud-zaliw+4    = t015m-monam.
  ELSE.
    CLEAR regud-zaliw.
  ENDIF.

* Wechselausstellungsdatum (in Worten) ---------------------------------
* bill of exchange issue date (in words) -------------------------------
  SELECT SINGLE * FROM t015m
    WHERE spras EQ hlp_sprache
    AND   monum EQ reguh-wdate+4(2).
  IF sy-subrc EQ 0.
    regud-wdaiw(2)   = reguh-wdate+6(2).
    regud-wdaiw+2(2) = '. '.
    regud-wdaiw+4    = t015m-monam.
  ELSE.
    CLEAR regud-wdaiw.
  ENDIF.

* Wechselfälligkeitsdatum (in Worten) ----------------------------------
* due date of the bill of exchange (in words) --------------------------
  SELECT SINGLE * FROM t015m
    WHERE spras EQ hlp_sprache
    AND   monum EQ reguh-wefae+4(2).
  IF sy-subrc EQ 0.
    regud-wefiw(2)   = reguh-wefae+6(2).
    regud-wefiw+2(2) = '. '.
    regud-wefiw+4    = t015m-monam.
  ELSE.
    CLEAR regud-wefiw.
  ENDIF.


ENDFORM.                               "ZAHLUNGS_DATEN_LESEN_HLP



*----------------------------------------------------------------------*
* FORM SACHBEARBEITER_KURZINFO                                         *
*----------------------------------------------------------------------*
* Aus den Daten des Sachbearbeiters (Struktur FSABE, gelesen mit       *
* Baustein CORRESPONDENCE_DATA_BUSAB) wird ein Textfeld mit einer      *
* Kurzinfo gefüllt (Anrede, Name, Telefonnummer). Sollte das Feld      *
* zu kurz sein, wird zuerst die Anrede, dann der hintere Teil des      *
* Namens unterdrückt.                                                  *
*----------------------------------------------------------------------*
* XFSABE   - Sachbearbeiterdaten                                       *
* TEXTFELD - Kurzinfo                                                  *
*----------------------------------------------------------------------*
FORM sachbearbeiter_kurzinfo USING xfsabe STRUCTURE fsabe textfeld.


  DATA:
    up_actln LIKE sy-fdpos,           "actually calculated string length
    up_maxln LIKE sy-fdpos,            "maximal string length (textfeld)
    up_lname LIKE fsabe-lname,
    up_salut LIKE fsabe-salut,
    up_telf1(40) TYPE c.

  DESCRIBE FIELD textfeld LENGTH up_maxln.
  up_lname     = xfsabe-lname.
  up_salut     = xfsabe-salut.
  CONCATENATE xfsabe-telf1 xfsabe-tel_exten1 INTO up_telf1.

  CONDENSE up_telf1 NO-GAPS.
  up_actln     = strlen( up_salut ) +
                 STRLEN( up_lname ) +
                 STRLEN( up_telf1 ) + 2.
  IF up_actln GT up_maxln.
    up_actln   = up_actln - STRLEN( up_salut ) - 1.
    CLEAR up_salut.
    IF up_actln GT up_maxln.
      up_actln = strlen( up_lname ) - up_actln + up_maxln.
      IF up_actln GT 0.
        up_lname+up_actln = space.
      ELSE.
        CLEAR up_lname.
      ENDIF.
    ENDIF.
  ENDIF.
  txt_zeile    = up_salut.
  txt_zeile+16 = up_lname.
  txt_zeile+52 = up_telf1.
  CONDENSE txt_zeile.
  textfeld     = txt_zeile.


ENDFORM.                               "SACHBEARBEITER_KURZINFO



*----------------------------------------------------------------------*
* FORM HR_REMITTANCE_ACKNOWLEDGEMENT                                   *
*----------------------------------------------------------------------*
* Zahlungen an Dritte (Third Party Remittance) an das HR zurückmelden  *
* report payments concerning 3rd parties back to HR                    *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
FORM hr_remittance_acknowledgement.


* Prüfen ob Rückmeldung notwendig
* check that payment is 3rd party remittance
  hrxblnr = regup-xblnr.
  CHECK:
    zw_xvorl      EQ space,
    zw_laufi+5(1) NE 'P',
    hrxblnr-txtsl EQ 'HR',
    hrxblnr-txerg EQ 'GRN' AND hrxblnr-xhrfo EQ 'X' OR
    hrxblnr-txerg EQ space AND hrxblnr-xhrfo EQ space,
    hrxblnr-remsn NE 0.

  SELECT SINGLE * FROM bkpf WHERE bukrs EQ regup-bukrs
                            AND   belnr EQ regup-belnr
                            AND   gjahr EQ regup-gjahr.
  IF bkpf-awsys EQ t000-logsys OR bkpf-awsys IS INITIAL.
    CALL FUNCTION 'RP_REMITTANCE_ACKNOWLEDGEMENT'
         EXPORTING
              laufd  = regup-laufd
              laufi  = regup-laufi
              bukrs  = regup-bukrs
              lifnr  = regup-lifnr
              xblnr  = regup-xblnr
         EXCEPTIONS
              OTHERS = 0.
  ELSE.
    CALL FUNCTION 'LOG_SYSTEM_GET_RFC_DESTINATION'
         EXPORTING
              logical_system  = bkpf-awsys
         IMPORTING
              rfc_destination = tab_rfc-dest
         EXCEPTIONS
              OTHERS          = 4.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      tab_rfc-bukrs = regup-bukrs.
      tab_rfc-belnr = regup-belnr.
      tab_rfc-gjahr = regup-gjahr.
      tab_rfc-lifnr = regup-lifnr.
      tab_rfc-xblnr = regup-xblnr.
      tab_rfc-zbukr = reguh-zbukr.
      tab_rfc-vblnr = reguh-vblnr.
      tab_rfc-awsys = bkpf-awsys.
      APPEND tab_rfc.
    ENDIF.
  ENDIF.


ENDFORM.                               "HR_REMITTANCE_ACKNOWLEDGEMENT



*----------------------------------------------------------------------*
* FORM HR_REMITTANCE_ACKNOWLEDGE_RFC                                   *
*----------------------------------------------------------------------*
* Zahlungen an Dritte (Third Party Remittance) via RFC zurückmelden    *
* report payments concerning 3rd parties back to HR (RFC)              *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
FORM hr_remittance_acknowledge_rfc.

  LOOP AT tab_rfc.
    CALL FUNCTION 'RP_REMITTANCE_ACKNOWLEDGEMENT'
         DESTINATION
              tab_rfc-dest
         EXPORTING
              laufd                 = zw_laufd
              laufi                 = zw_laufi
              bukrs                 = tab_rfc-bukrs
              lifnr                 = tab_rfc-lifnr
              xblnr                 = tab_rfc-xblnr
         EXCEPTIONS
              communication_failure = 4 MESSAGE txt_zeile
              system_failure        = 4 MESSAGE txt_zeile
              OTHERS                = 0.
    IF sy-subrc NE 0.
      fimsg-msgv1   = tab_rfc-awsys.
      fimsg-msgv2   = tab_rfc-dest.
      fimsg-msgv3   = tab_rfc-bukrs.
      fimsg-msgv4   = tab_rfc-belnr.
      PERFORM message USING 378.
      fimsg-msgv1   = txt_zeile.
      fimsg-msgv2   = txt_zeile+50.
      fimsg-msgv3   = txt_zeile+100.
      PERFORM message USING 379.
      fimsg-msgv1   = tab_rfc-zbukr.
      fimsg-msgv2   = tab_rfc-vblnr.
      PERFORM message USING 380.
      REJECT.
    ENDIF.
  ENDLOOP.

ENDFORM.                               "HR_REMITTANCE_ACKNOWLEDGE_RFC



*----------------------------------------------------------------------*
* FORM HR_FORMULAR_LESEN                                               *
*----------------------------------------------------------------------*
* HR-Formular besorgen und Steuerungszeilen entfernen                  *
* Bei Pfändungen (HR GRN) zusätzlich die Textfelder in REGUD füllen    *
* read HR form and delete command lines                                *
* in addition fill text fields in REGUD when payment is garnishment    *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
FORM hr_formular_lesen.


  FIELD-SYMBOLS:
    <feld>.
  DATA:
    up_nr(1)    TYPE n,
    up_feld(11) TYPE c,
    up_pform    LIKE pc408 OCCURS 9 WITH HEADER LINE.
  REFRESH: pform, up_pform.

  IF hrxblnr-txtsl EQ 'HR' AND hrxblnr-txerg EQ 'GRN'.
    IF regup-sgtxt CN '* '.
      WHILE regup-sgtxt(1) CA '* '.
        SHIFT regup-sgtxt.
      ENDWHILE.
    ENDIF.
    READ TABLE tab_rfc WITH KEY bukrs = regup-bukrs
                                belnr = regup-belnr
                                gjahr = regup-gjahr.
    IF sy-subrc NE 0.
      CALL FUNCTION 'RP_IMPORT_GARNISHMENT_LIST'
           EXPORTING
                sgtxt  = regup-sgtxt
           TABLES
                pform  = pform
                pform1 = up_pform
           EXCEPTIONS
                OTHERS = 8.
    ELSE.
      CALL FUNCTION 'RP_IMPORT_GARNISHMENT_LIST'
           DESTINATION
                tab_rfc-dest
           EXPORTING
                sgtxt                 = regup-sgtxt
           TABLES
                pform                 = pform
                pform1                = up_pform
           EXCEPTIONS
                communication_failure = 4 MESSAGE txt_zeile
                system_failure        = 4 MESSAGE txt_zeile
                OTHERS                = 8.
      IF sy-subrc EQ 4.
        sy-msgid = 'F0'.
        sy-msgno = 379.
        sy-msgv1 = txt_zeile.
        sy-msgv2 = txt_zeile+50.
        sy-msgv3 = txt_zeile+100.
        sy-msgv4 = space.
      ENDIF.
    ENDIF.
  ELSE.
    CALL FUNCTION 'RP_IMPORT_PAY_STATEMENT'
         EXPORTING
              laufd  = reguh-laufd
              laufi  = reguh-laufi
              pernr  = reguh-pernr
              seqnr  = reguh-seqnr
         TABLES
              pform  = pform
         EXCEPTIONS
              OTHERS = 8.
  ENDIF.
  IF sy-subrc NE 0.
    fimsg-msgid = sy-msgid.
    fimsg-msgv1 = sy-msgv1.
    fimsg-msgv2 = sy-msgv2.
    fimsg-msgv3 = sy-msgv3.
    fimsg-msgv4 = sy-msgv4.
    PERFORM message USING sy-msgno.
    fimsg-msgv1 = reguh-zbukr.
    fimsg-msgv2 = reguh-hbkid.
    fimsg-msgv3 = reguh-hktid.
    fimsg-msgv4 = regud-chect.
    PERFORM message USING '286'.
  ELSE.
    LOOP AT pform WHERE ltype NE f__ltype-txt.
      DELETE pform.
    ENDLOOP.
    IF hrxblnr-txtsl EQ 'HR' AND hrxblnr-txerg EQ 'GRN'.
      LOOP AT up_pform WHERE ltype NE f__ltype-txt.
        DELETE up_pform.
      ENDLOOP.
      DO 9 TIMES.
        up_nr         = sy-index.
        up_feld       = 'REGUD-TEXT '.
        up_feld+10(1) = up_nr.
        ASSIGN (up_feld) TO <feld>.
        CLEAR <feld>.
        READ TABLE up_pform INDEX sy-index.
        IF sy-subrc EQ 0.
          <feld> = up_pform-linda.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDIF.


ENDFORM.                               "HR_FORMULAR_LESEN


*----------------------------------------------------------------------*
* FORM WEISUNGSSCHLUESSEL_LESEN                                        *
*----------------------------------------------------------------------*
* Lesen des aktuellen Weisungsschlüssels zu den REGUH-Daten            *
* Read instruction key for current REGUH-contents                      *
* Release 3.0: key fields are country of bank and payment method       *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
FORM weisungsschluessel_lesen.

  DATA: up_dtaws LIKE reguh-dtaws.
  STATICS: up_t015w LIKE t015w OCCURS 0 WITH HEADER LINE.

  CLEAR t015w.                         "Clear old values

  IF NOT reguh-dtaws IS INITIAL.
    up_dtaws = reguh-dtaws.
  ELSE.
    IF reguh-zbukr NE t012d-bukrs OR reguh-hbkid NE t012d-hbkid.
      SELECT SINGLE * FROM t012d WHERE bukrs EQ reguh-zbukr
                                 AND   hbkid EQ reguh-hbkid.
      IF sy-subrc NE 0.
        CLEAR t012d.
      ENDIF.
    ENDIF.
    IF NOT t012d-dtaws IS INITIAL.
      up_dtaws = t012d-dtaws.
    ELSE.
      MOVE-CORRESPONDING reguh TO err_t012d.
      COLLECT err_t012d.
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE up_t015w WITH KEY banks = reguh-ubnks
                               zlsch = reguh-rzawe
                               dtaws = up_dtaws
                          INTO t015w.
  CHECK sy-subrc NE 0.
  SELECT SINGLE * FROM t015w
          WHERE banks EQ reguh-ubnks
            AND zlsch EQ reguh-rzawe
            AND dtaws EQ up_dtaws.
  IF sy-subrc NE 0.                    "specified entry not found
    SELECT SINGLE * FROM t015w
            WHERE banks EQ space
              AND zlsch EQ space
              AND dtaws EQ up_dtaws.
    IF sy-subrc NE 0.                  "general (=old) entry not found
      err_kein_dtaws-banks = reguh-ubnks.
      err_kein_dtaws-zlsch = reguh-rzawe.
      err_kein_dtaws-dtaws = up_dtaws.
      COLLECT err_kein_dtaws.          "Store error
    ELSE.
      t015w-banks = reguh-ubnks.
      t015w-zlsch = reguh-rzawe.
      APPEND t015w TO up_t015w.
    ENDIF.
  ELSE.
    APPEND t015w TO up_t015w.
  ENDIF.

ENDFORM.                               "WEISUNGSSCHLUESSEL_LESEN


*----------------------------------------------------------------------*
* Form  WEISUNGSSCHLUESSEL_UMSETZEN                                    *
*----------------------------------------------------------------------*
* Weisungsschlüssel in Schlüsselwort u. Zusatzinfo umsetzen            *
* transpose instruction key into keyword and additional information    *
*----------------------------------------------------------------------*
*  P_DTWS. - Verwender, Feld, Weisung (wird evtl. überschrieben)       *
*            country, field, instruction (may be overwritten)          *
*  P_TEXT1 - Schlüsselwort                                             *
*            code word                                                 *
*  P_TEXT2 - Zusatzinformation                                         *
*          - additional information                                    *
*----------------------------------------------------------------------*
FORM weisungsschluessel_umsetzen USING p_dtwsc LIKE t015w1-dtwsc
                                       p_dtwsf LIKE t015w1-dtwsf
                                       p_dtwsx LIKE t015w1-dtwsx
                                       p_text1 TYPE any
                                       p_text2 TYPE any.
  DATA up_i015w1_par LIKE i015w1_par.

  MOVE-CORRESPONDING reguh TO up_i015w1_par.

  CALL FUNCTION 'FI_PAYMENT_INSTRUCTION_CONVERT'
       EXPORTING
            i_dtwsc      = p_dtwsc
            i_dtwsf      = p_dtwsf
            i_i015w1_par = up_i015w1_par
            i_bnka       = bnka
            i_dtzus      = t015w-dtzus
       IMPORTING
            e_code       = p_text1
            e_addinfo    = p_text2
       CHANGING
            c_dtwsx      = p_dtwsx.

ENDFORM.                               " WEISUNGSSCHLUESSEL_UMSETZEN


*----------------------------------------------------------------------*
* Form  GET_CLEARING_CODE
*----------------------------------------------------------------------*
* Clearing Code ermitteln                                              *
* determine clearing code                                              *
*----------------------------------------------------------------------*
*  P_LAND  - Länderschlüssel der Bank des Zahlungsempfängers           *
*            country key of the bank of payee
*  P_SOCO  - Bankleitzahl
*            sort code
*  P_CLCO  - clearing code
*----------------------------------------------------------------------*
FORM get_clearing_code USING p_land   LIKE reguh-zbnks
                             p_soco   LIKE reguh-zbnkl
                             p_clco   TYPE c.

  PERFORM laender_lesen USING p_land.

  CALL FUNCTION 'GET_BANKCODE'
       EXPORTING
            i_banks  = p_land
            i_bankl  = p_soco
       IMPORTING
            e_clcode = p_clco
       EXCEPTIONS
            OTHERS   = 4.

ENDFORM.                               " GET_CLEARING_CODE


*----------------------------------------------------------------------*
* FORM ADRESSE_LESEN                                                   *
*----------------------------------------------------------------------*
* Lesen einer Customizingadresse (z.B. Buchungskreisadresse)           *
* Read customizing address (e.g. address of company code)              *
*----------------------------------------------------------------------*
* ADRNR - address number                                               *
*----------------------------------------------------------------------*
FORM adresse_lesen USING value(adrnr).

  PERFORM addr_get USING 'CA01' adrnr.

ENDFORM.                               " ADRESSE_LESEN


*----------------------------------------------------------------------*
* FORM BANKADRESSE_LESEN                                               *
*----------------------------------------------------------------------*
* Lesen einer Bankadresse                                              *
* Read bank address                                                    *
*----------------------------------------------------------------------*
* ADRNR - address number                                               *
*----------------------------------------------------------------------*
FORM bankadresse_lesen USING value(adrnr).

  PERFORM addr_get USING 'CA02' adrnr.

ENDFORM.                               " BANKADRESSE_LESEN


*----------------------------------------------------------------------*
* FORM GET_ADDR                                                        *
*----------------------------------------------------------------------*
* Lesen einer Adresse                                                  *
* Read address                                                         *
*----------------------------------------------------------------------*
* ADRGR - address group                                                *
* ADRNR - address number                                               *
*----------------------------------------------------------------------*
FORM addr_get USING adrgr adrnr.

  CHECK adrnr NE sadr-adrnr.
  CLEAR addr1_sel.
  addr1_sel-addrnumber = adrnr.
  CALL FUNCTION 'ADDR_GET'
       EXPORTING
            address_selection = addr1_sel
            address_group     = adrgr
       IMPORTING
            address_value     = addr1_val
            sadr              = sadr
       EXCEPTIONS
            OTHERS            = 4.                          "SADR40A
  IF sy-subrc NE 0.
    CLEAR sadr.
  ENDIF.

ENDFORM.                               " GET_ADDR


*----------------------------------------------------------------------*
* FORM LAENDER_LESEN                                                   *
*----------------------------------------------------------------------*
* Lesen der Länderdaten zum Land LAND1                                 *
* Read country data for LAND1                                          *
*----------------------------------------------------------------------*
* LAND1 - countrycode                                                  *
*----------------------------------------------------------------------*
FORM laender_lesen USING value(land1).

  CLEAR sy-subrc.

  IF tab_t005-land1 = land1.           "check last value
    t005 = tab_t005.
  ELSE.
    READ TABLE tab_t005 WITH KEY land1 = land1."check internal table
    IF sy-subrc EQ 0.                  "entry was found
      t005 = tab_t005.
    ELSE.
      SELECT SINGLE * FROM t005
             WHERE land1 = land1.
      IF sy-subrc NE 0.                "no entry found in T005
        CLEAR t005.
      ELSE.                            "entry found-> store temporarily
        tab_t005 = t005.               "fill header, too!
        APPEND tab_t005.
      ENDIF.
    ENDIF.
  ENDIF.

  IF t005-intca IS INITIAL.
    err_t005-land1 = land1.
    COLLECT err_t005.
    sy-subrc = 4.
  ENDIF.

ENDFORM.                               "LAENDER_LESEN


*----------------------------------------------------------------------*
* FORM ZAHLWEG_EINFUEGEN                                               *
*----------------------------------------------------------------------*
* Übergebenen Zahlweg in die übergebene Leiste übernehmen              *
* insert payment method into array of methods                          *
*----------------------------------------------------------------------*
* RZAWE  - payment method to insert                                    *
* LIST   - current list of payment methods                             *
*----------------------------------------------------------------------*
FORM zahlweg_einfuegen USING value(rzawe) list.

  DATA up_list LIKE regud-zwels.

  CHECK list NA rzawe.
  up_list = list.
  SHIFT up_list.
  up_list+9(1) = rzawe.
  list = up_list.

ENDFORM.                               "ZAHLWEG_EINFUEGEN


*----------------------------------------------------------------------*
* FORM SUMMENFELDER_INITIALISIEREN                                     *
*----------------------------------------------------------------------*
* Summenfelder initialisieren                                          *
* initialize total amount fields                                       *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM summenfelder_initialisieren.


* Summenfelder initialisieren ------------------------------------------
* initialize total amount fields ---------------------------------------
  regud-sdmbt = 0.
  regud-swrbt = 0.
  regud-ssknt = 0.
  regud-swskt = 0.
  regud-sqste = 0.
  regud-swqst = 0.
  regud-sskfb = 0.
  regud-sqssh = 0.

* Nettosummenfelder mit Schutzstern für das Anschreiben vorab belegen --
* fill net total fields with protective asterisks ----------------------
  WRITE:
    reguh-rbetr TO regud-snets CURRENCY t001-waers,
    reguh-rwbtr TO regud-swnes CURRENCY reguh-waers.
  TRANSLATE:
    regud-snets USING ' *',
    regud-swnes USING ' *'.
  PERFORM ziffern_in_worten.


ENDFORM.                               "SUMMENFELDER_INITIALISIEREN



*----------------------------------------------------------------------*
* FORM EINZELPOSTENFELDER_FUELLEN                                      *
*----------------------------------------------------------------------*
* Ausgabefelder füllen                                                 *
* fill single item fields                                              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM einzelpostenfelder_fuellen.

* Segmenttext ohne * aufbereiten ---------------------------------------
* segment text without leading * ---------------------------------------
  IF regup-sgtxt CN '* '.
    WHILE regup-sgtxt(1) CA '* '.
      SHIFT regup-sgtxt.
    ENDWHILE.
  ENDIF.

* Einzelposteninfo merken - Verwendung: Avis, User-Exit
* store item-information  - use: payment advice, user-exit
  IF tab_regup NE regup.               "nicht im Loop über TAB_REGUP
    tab_regup = regup.                 "not in LOOP AT TAB_REGUP
    APPEND tab_regup.
  ENDIF.

* Text zum Buchungsschlüssel lesen
* read text of posting key
  SELECT SINGLE * FROM tbslt
    WHERE spras EQ hlp_sprache
      AND bschl EQ regup-bschl
      AND umskz EQ regup-umskz.
  regud-bschx = tbslt-ltext.

* Betragsfelder (Abzüge und Netto) füllen ------------------------------
* fill single item amount fields (deductions and net) ------------------
  PERFORM vorzeichen_setzen USING 'P'.
  regud-abzug = regud-sknto + regud-qsteu.
  regud-wabzg = regud-wskto + regud-wqste.
  regud-netto = regud-dmbtr - regud-abzug.
  regud-wnett = regud-wrbtr - regud-wabzg.
  WRITE:
    regud-netto TO regud-netts CURRENCY t001-waers,
    regud-wnett TO regud-wnets CURRENCY reguh-waers.
  TRANSLATE:
    regud-netts USING ' *',
    regud-wnets USING ' *'.


ENDFORM.                               "EINZELPOSTENFELDER_FUELLEN



*----------------------------------------------------------------------*
* FORM SUMMENFELDER_FUELLEN                                            *
*----------------------------------------------------------------------*
* Summenfelder hochzählen                                              *
* Ausgabefelder füllen                                                 *
*----------------------------------------------------------------------*
* add up total amount fields                                           *
* fill print fields                                                    *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM summenfelder_fuellen.


  ADD regud-dmbtr TO regud-sdmbt.
  ADD regud-wrbtr TO regud-swrbt.
  ADD regud-sknto TO regud-ssknt.
  ADD regup-skfbt TO regud-sskfb.
  ADD regud-wskto TO regud-swskt.
  ADD regud-qsteu TO regud-sqste.
  ADD regud-wqste TO regud-swqst.
  ADD regup-qsshb TO regud-sqssh.
  regud-sabzg = regud-ssknt + regud-sqste.
  regud-swabz = regud-swskt + regud-swqst.
  regud-snett = regud-sdmbt - regud-sabzg.
  regud-swnet = regud-swrbt - regud-swabz.
  WRITE:
    regud-snett TO regud-snets CURRENCY t001-waers,
    regud-swnet TO regud-swnes CURRENCY reguh-waers.
  TRANSLATE:
    regud-snets USING ' *',
    regud-swnes USING ' *'.


ENDFORM.                               "SUMMENFELDER_FUELLEN



*----------------------------------------------------------------------*
* FORM DTA_GLOBALS_ERSETZEN                                            *
*----------------------------------------------------------------------*
* Ersetzt die Globals in Verwendungszweckzeilen im DTA                 *
* replace globals in the file extension fields (DME)                   *
*----------------------------------------------------------------------*
* TEXTFELD - Feld, in dem Globals ersetzt werden sollen                *
*            Field with globals to be replaced                         *
*----------------------------------------------------------------------*
FORM dta_globals_ersetzen USING textfeld.

  DATA: up_textfeld(512) TYPE c.
  up_textfeld = textfeld.

  WRITE regup-bldat TO hlp_datum DD/MM/YY.
  IF up_textfeld CS '&XBLNR'.
    sy-fdpos = strlen( regup-xblnr ).
    IF sy-fdpos GE 14.
      WRITE regup-bldat TO hlp_datum DDMMYY.
    ENDIF.
  ENDIF.
  REPLACE   '&BLDAT' WITH hlp_datum   INTO up_textfeld.
  REPLACE   '&EIKTO' WITH reguh-eikto INTO up_textfeld.
  REPLACE   '&GJAHR' WITH regud-gjahr INTO up_textfeld.
  IF reguh-lifnr NE space.
    REPLACE '&KTNRA' WITH reguh-lifnr INTO up_textfeld.
  ELSE.
    REPLACE '&KTNRA' WITH reguh-kunnr INTO up_textfeld.
  ENDIF.
  WRITE regud-netto TO hlp_betrag CURRENCY regud-hwaer.
  REPLACE   '&NETTO' WITH hlp_betrag  INTO up_textfeld.
  REPLACE   '&PERNR' WITH reguh-pernr INTO up_textfeld.
  WRITE regup-zbdxp TO hlp_betrag CURRENCY '3'.
  WRITE '%' TO hlp_betrag+15.
  REPLACE   '&PSATZ' WITH hlp_betrag  INTO up_textfeld.
  REPLACE   '&SEQNR' WITH reguh-seqnr INTO up_textfeld.
  REPLACE   '&SGTXT' WITH regup-sgtxt INTO up_textfeld.
  IF hlp_laufk EQ 'M'.
    DATA up_opbel(12) TYPE c.
    up_opbel(2) = reguh-seqnr(2).
    up_opbel+2  = reguh-vblnr.
    REPLACE '&VBLNR' WITH up_opbel    INTO up_textfeld.
  ELSE.
    REPLACE '&VBLNR' WITH reguh-vblnr INTO up_textfeld.
  ENDIF.
  REPLACE   '&VERTN' WITH regup-vertn INTO up_textfeld.
  WRITE regud-wrbtr  TO hlp_betrag CURRENCY regud-waers.
  REPLACE   '&WBRUT' WITH hlp_betrag  INTO up_textfeld.
  WRITE regud-wnett TO hlp_betrag CURRENCY regud-waers.
  REPLACE   '&WNETT' WITH hlp_betrag  INTO up_textfeld.
  REPLACE   '&WAERS' WITH reguh-waers INTO up_textfeld.
  REPLACE   '&BELNR' WITH regup-belnr INTO up_textfeld.
  REPLACE   '&XBLNR' WITH regup-xblnr INTO up_textfeld.
  WRITE reguh-zaldt TO hlp_datum DD/MM/YY.
  REPLACE   '&ZALDT' WITH hlp_datum   INTO up_textfeld.
  REPLACE   '&ZBUKR' WITH reguh-zbukr INTO up_textfeld.
  CONDENSE up_textfeld.
  PERFORM dta_text_aufbereiten USING up_textfeld.
  textfeld = up_textfeld.


ENDFORM.                               "DTA_GLOBALS_ERSETZEN



*----------------------------------------------------------------------*
* FORM DTA_TEXT_AUFBEREITEN                                            *
*----------------------------------------------------------------------*
* Textfelder im DTA müssen Upper Case und ohne Umlaute sein            *
* text fields in DME have to be upper case and without 'äöü' etc.      *
*----------------------------------------------------------------------*
* TEXTFELD - enthält den zu bearbeitenden Text                         *
*            text that is to be checked                                *
*----------------------------------------------------------------------*
FORM dta_text_aufbereiten USING textfeld.

  IF flg_no_replace EQ space.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
         EXPORTING
              intext  = textfeld
         IMPORTING
              outtext = textfeld
         EXCEPTIONS
              OTHERS  = 01.
  ENDIF.
  TRANSLATE textfeld TO UPPER CASE.

ENDFORM.                               "DTA_TEXT_AUFBEREITEN


*----------------------------------------------------------------------*
* FORM DATEN_SICHERN                                                   *
*----------------------------------------------------------------------*
* Sichern der REGUD-,REGUH-, REGUP-Informationen                       *
* Datenbankfelder für Probedruck belegen                               *
*----------------------------------------------------------------------*
* save REGUD-,REGUH-, REGUP-information during test print              *
* fill all fields with XXXXX                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM daten_sichern.


  sic_fsabe     = fsabe.
  sic_itcpo     = itcpo.
  sic_regud     = regud.
  sic_reguh     = reguh.
  sic_regup     = regup.
  fsabe         = xxx_fsabe.
  regud         = xxx_regud.
  reguh         = xxx_reguh.
  regup         = xxx_regup.
  spell         = xxx_spell.
  itcpo-tdarmod = 1.
  reguh-rzawe   = sic_reguh-rzawe.     "Zahlweg erhalten
  regud-txtko   = sic_regud-txtko.     "Textbausteine sollen auch beim
  regud-txtfu   = sic_regud-txtfu.     "Probedruck erscheinen
  regud-txtun   = sic_regud-txtun.     "payment method and text includes
  regud-txtab   = sic_regud-txtab.     "are valid during test print
  regud-chect   = sic_regud-chect.     "Scheckinformation
  regud-stapt   = sic_regud-stapt.     "check information


ENDFORM.                               "DATEN_SICHERN



*----------------------------------------------------------------------*
* FORM DATEN_ZURUECK                                                   *
*----------------------------------------------------------------------*
* Zurückladen der REGUD-,REGUH-, REGUP-Informationen                   *
* data back after test print                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM daten_zurueck.


  itcpo = sic_itcpo.
  MOVE-CORRESPONDING:
    sic_fsabe TO fsabe,
    sic_regud TO regud,
    sic_reguh TO reguh,
    sic_regup TO regup.
  CLEAR spell.


ENDFORM.                               "DATEN_ZURUECK



*----------------------------------------------------------------------*
* FORM ZIFFERN_IN_WORTEN                                               *
*----------------------------------------------------------------------*
* Umsetzten des Betrages und der Ziffern in Worte                      *
* transform numbers and digits in words                                *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM ziffern_in_worten.


  CLEAR spell.
  CALL FUNCTION 'SPELL_AMOUNT'
       EXPORTING
            language  = hlp_sprache
            currency  = reguh-waers
            amount    = regud-swnes
            filler    = hlp_filler
       IMPORTING
            in_words  = spell
       EXCEPTIONS
            not_found = 1
            too_large = 2.

  IF sy-subrc EQ 1.
*   in Tabelle T015Z fehlt ein Eintrag
*   entry in table T015Z not found
    CLEAR err_t015z.
    err_t015z-spras = sy-msgv1.
    err_t015z-einh  = sy-msgv2.
    err_t015z-ziff  = sy-msgv3.
    COLLECT err_t015z.

    IF hlp_sprache NE 'E'.
*     Letzter Versuch mit Sprache 'E' (besser als nichts)
*     Last trial with language 'E' (better than nothing)
      CALL FUNCTION 'SPELL_AMOUNT'
           EXPORTING
                language = 'E'
                currency = reguh-waers
                amount   = regud-swnes
                filler   = hlp_filler
           IMPORTING
                in_words = spell
           EXCEPTIONS
                OTHERS   = 1.
    ENDIF.
  ENDIF.

  IF sy-subrc EQ 2 OR spell-number GE hlp_maxbetrag.
*   Betrag ist zum Umsetzen zu groß
*   amount too large for transformation
    MOVE-CORRESPONDING reguh TO err_in_worten.
    COLLECT err_in_worten.
    CLEAR spell+403.                   "nur SPELL-DIGnn
  ENDIF.


ENDFORM.                               "ZIFFERN_IN_WORTEN



*----------------------------------------------------------------------*
* FORM DATUM_IN_DDMMYY                                                 *
*----------------------------------------------------------------------*
* Konvertierung des Datumfelds DATUM in das Format DDMMYY              *
* Diese Konvertierung geschieht unabhängig von den Benutzerfestwerten. *
*----------------------------------------------------------------------*
* Convert date-field (ccyymmdd) to the format ddmmyy.                  *
* This conversion does not take the user defaults into consideration.  *
*----------------------------------------------------------------------*
FORM datum_in_ddmmyy USING datum    TYPE d
                           ddmmyy TYPE any.


  DATA: up_str(6).

  up_str   = datum+6.                                       "Day
  up_str+2 = datum+4.                  "Month
  up_str+4 = datum+2.                  "Year
  ddmmyy = up_str.


ENDFORM.                               "DATUM_IN_DDMMYY



*----------------------------------------------------------------------*
* FORM ABBRUCH_DURCH_UEBERLAUF                                         *
*----------------------------------------------------------------------*
* Abbruch der Verarbeitung, da es zu unerlaubten Überlauf des Main-    *
* fensters kam. Grund: Die Positionen pro Formular sind in T042E       *
* zu groß definiert.                                                   *
*----------------------------------------------------------------------*
* abend of program because of an overflow of the main window,          *
* reason: number of line items per form are too large in T042E         *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM abbruch_durch_ueberlauf.


  IF sy-batch EQ space.
    MESSAGE a090 WITH reguh-rzawe reguh-zbukr.
  ELSE.
    MESSAGE s090 WITH reguh-rzawe reguh-zbukr.
    MESSAGE s091 WITH reguh-rzawe reguh-zbukr.
    MESSAGE s092 WITH reguh-rzawe reguh-zbukr.
    MESSAGE s093 WITH reguh-rzawe reguh-zbukr.
    PERFORM information.
    MESSAGE s094.
    STOP.
  ENDIF.


ENDFORM.                               "ABBRUCH_DURCH_UEBERLAUF



*----------------------------------------------------------------------*
* FORM FEHLERMELDUNGEN                                                 *
*----------------------------------------------------------------------*
* Ausgabe von Fehlermeldungen                                          *
* error messages                                                       *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM fehlermeldungen.


* Notlösung: Form Fehlermeldung ist die einzige gemeinsame Unterroutine
* aller RFFO-Programme zum Zeitpunkt End-Of-Selection. Daher wird hier
* der Baustein RP_REMITTANCE_ACKNOWLEDGEMENT gerufen, da zum Get-Zeit-
* punkt ein Laufzeitfehler wegen des RFC auftritt.
  PERFORM hr_remittance_acknowledge_rfc.

  SET LANGUAGE sy-langu.
  CLEAR fimsg.

* Fehlende Berechtigungen ----------------------------------------------
* Authority check errors -----------------------------------------------

  PERFORM berechtigung.

*  PERFORM GET_ERROR_LIST(SAPDBPYF) TABLES ERR_AUTH.
*  SORT ERR_AUTH.
*  LOOP AT ERR_AUTH.
*
*    AT FIRST.
*      ADD 1 TO CNT_ERROR.
*    ENDAT.
*
*    CASE ERR_AUTH-FIELD.
*      WHEN 'BRGRU'.
*        FIMSG-MSGNO   = '224'.
*        IF ERR_AUTH-AUTOB EQ 'F_KNA1_BED'.
*          FIMSG-MSGV1 = 'D'.
*        ELSE.
*          FIMSG-MSGV1 = 'K'.
*        ENDIF.
*      WHEN 'BUKRS'.
*        FIMSG-MSGNO   = '153'.
*        FIMSG-MSGV1   = ERR_AUTH-ACTVT.
*      WHEN 'KOART'.
*        FIMSG-MSGNO   = '154'.
*        FIMSG-MSGV1   = ERR_AUTH-ACTVT.
*    ENDCASE.
*    FIMSG-MSGV2       = ERR_AUTH-VALUE.
*    FIMSG-MSGV3       = ERR_AUTH-ACTVT.
*    FIMSG-MSGV4       = ERR_AUTH-AUTOB.
*    IF SY-BATCH EQ SPACE.
*      MESSAGE ID 'F0' TYPE 'I' NUMBER FIMSG-MSGNO
*        WITH FIMSG-MSGV1 FIMSG-MSGV2 FIMSG-MSGV3 FIMSG-MSGV4.
*    ENDIF.
*    PERFORM MESSAGE USING FIMSG-MSGNO.
*
*  ENDLOOP.


* nicht zulässige Zahlwege aus T042Z -----------------------------------
* not valid payment methods from T042Z ---------------------------------
  SORT err_t042z.
  LOOP AT err_t042z.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    AT NEW land1.
      CLEAR hlp_zwels.
    ENDAT.

    WRITE err_t042z-zlsch TO hlp_zwels+sy-tabix.

    AT END OF land1.
      CONDENSE hlp_zwels NO-GAPS.
      IF sy-batch EQ space.
        MESSAGE i282 WITH hlp_zwels err_t042z-land1 sy-repid.
      ENDIF.
      fimsg-msgv1 = hlp_zwels.
      fimsg-msgv2 = err_t042z-land1.
      fimsg-msgv3 = sy-repid.
      PERFORM message USING '282'.
    ENDAT.

  ENDLOOP.


* nichts selektiert ----------------------------------------------------
* no data selected -----------------------------------------------------
  IF flg_selektiert EQ 0.
    MESSAGE s073 WITH syst-repid.
  ENDIF.


* nicht gefundene Elemente und Fenster ---------------------------------
* elements and windows not found ---------------------------------------
  SORT err_element.
  LOOP AT err_element.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    IF err_element-elemt NE space.
      fimsg-msgv1 = err_element-fname.
      fimsg-msgv2 = err_element-fenst.
      fimsg-msgv3 = err_element-elemt.
      fimsg-msgv4 = err_element-text.
      PERFORM message USING '251'.
    ELSE.
      fimsg-msgv1 = err_element-fname.
      fimsg-msgv2 = err_element-fenst.
      PERFORM message USING '252'.
    ENDIF.

    AT LAST.
      PERFORM message USING '253'.
    ENDAT.

  ENDLOOP.


* nicht gedruckte Fremdwährungsschecks ---------------------------------
* checks in foreign currencies not printed -----------------------------
  SORT err_fw_scheck.
  LOOP AT err_fw_scheck.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    AT NEW fname.
      fimsg-msgv1 = err_fw_scheck-fname.
      PERFORM message USING '254'.
      fimsg-msgv1 = err_fw_scheck-fname.
      PERFORM message USING '255'.
      PERFORM message USING '256'.
    ENDAT.

    fimsg-msgv1 = err_fw_scheck-zbukr.
    fimsg-msgv2 = err_fw_scheck-vblnr.
    PERFORM message USING '257'.

  ENDLOOP.


* nicht in Worte umgesetzter Betrag ------------------------------------
* amount in words was not possible -------------------------------------
  SORT err_in_worten.
  LOOP AT err_in_worten.

    AT FIRST.
      ADD 1 TO cnt_error.
      PERFORM message USING '258'.
      PERFORM message USING '259'.
      PERFORM message USING '256'.
    ENDAT.

    fimsg-msgv1 = err_in_worten-zbukr.
    fimsg-msgv2 = err_in_worten-vblnr.
    PERFORM message USING '257'.

    AT LAST.
      PERFORM message USING '260'.
    ENDAT.

  ENDLOOP.


* nicht auf DTA ausgegebene Auslandsüberweisungen ----------------------
* foreign payment via DME not possible ---------------------------------
  SORT err_kein_dta.
  LOOP AT err_kein_dta.

    AT NEW error.
      ADD 1 TO cnt_error.
      fimsg-msgv1 = err_kein_dta-error.
      CASE err_kein_dta-error.
        WHEN 1.
          PERFORM message USING '261'.
        WHEN 2.
          PERFORM message USING '262'.
        WHEN 3.
          PERFORM message USING '263'.
        WHEN 4.
          PERFORM message USING '264'.
        WHEN 5.
          PERFORM message USING '265'.
        WHEN 6.
          PERFORM message USING '266'.
        WHEN 7.
          PERFORM message USING '267'.
        WHEN 8.
          PERFORM message USING '268'.
        WHEN 9.
          PERFORM message USING '269'.
        WHEN 10.
          PERFORM message USING '270'.
      ENDCASE.
      fimsg-msgv1 = err_kein_dta-error.
      PERFORM message USING '271'.
      PERFORM message USING '256'.
    ENDAT.

    fimsg-msgv1 = err_kein_dta-zbukr.
    fimsg-msgv2 = err_kein_dta-vblnr.
    PERFORM message USING '257'.

  ENDLOOP.


* nicht gefundene Weisungsschlüssel ------------------------------------
* missing instruction key ----------------------------------------------
  SORT err_kein_dtaws.
  LOOP AT err_kein_dtaws.

    AT FIRST.
      ADD 1 TO cnt_error.
      fimsg-msgv1 = err_kein_dtaws-banks.
      fimsg-msgv2 = err_kein_dtaws-zlsch.
      fimsg-msgv3 = err_kein_dtaws-dtaws.
      PERFORM message USING '291'.
    ENDAT.

    fimsg-msgv1 = err_kein_dtaws-banks.
    fimsg-msgv2 = err_kein_dtaws-zlsch.
    fimsg-msgv3 = err_kein_dtaws-dtaws.
    PERFORM message USING '292'.

    AT LAST.
      PERFORM message USING '293'.
    ENDAT.

  ENDLOOP.


* nicht verbuchte Belege (und daher nicht gedruckte Formulare) ---------
* payment documents not updated (therefore no form printed) ------------
  SORT err_nicht_verbucht.
  LOOP AT err_nicht_verbucht.

    AT FIRST.
      ADD 1 TO cnt_error.
      PERFORM message USING '272'.
      PERFORM message USING '273'.
      PERFORM message USING '256'.
    ENDAT.

    fimsg-msgv1   = err_nicht_verbucht-zbukr.
    IF err_nicht_verbucht-pyord EQ space.
      fimsg-msgv2 = err_nicht_verbucht-vblnr.
    ELSE.
      fimsg-msgv2 = err_nicht_verbucht-pyord.
    ENDIF.
    PERFORM message USING '257'.

    AT LAST.
      PERFORM message USING '274'.
    ENDAT.

  ENDLOOP.


* EDI Versendefehler ---------------------------------------------------
* error in EDI ---------------------------------------------------------
  SORT err_edi.
  READ TABLE err_edi WITH KEY edibn = 'E'.
  IF sy-subrc EQ 0.

    ADD 1 TO cnt_error.
    PERFORM message USING '357'.
    PERFORM message USING '358'.

    LOOP AT err_edi WHERE edibn EQ 'E'.
      fimsg-msgv1 = err_edi-zbukr.
      fimsg-msgv2 = err_edi-vblnr.
      fimsg-msgv3 = err_edi-rzawe.
      PERFORM message USING '257'.
    ENDLOOP.

    PERFORM message USING '359'.

  ENDIF.


* zuerst mit EDI versuchen ---------------------------------------------
* try EDI first --------------------------------------------------------
  READ TABLE err_edi WITH KEY edibn = 'X'.
  IF sy-subrc EQ 0.

    ADD 1 TO cnt_error.
    PERFORM message USING '360'.
    PERFORM message USING '256'.

    LOOP AT err_edi WHERE edibn EQ 'X'.
      fimsg-msgv1 = err_edi-zbukr.
      fimsg-msgv2 = err_edi-vblnr.
      PERFORM message USING '257'.
    ENDLOOP.

  ENDIF.


* Einträge in T005 falsch oder unvollständig ---------------------------
* T005-entries missing or not correct ----------------------------------
  SORT err_t005.
  LOOP AT err_t005.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_t005-land1.
    PERFORM message USING '241'.

    AT LAST.
      PERFORM message USING '276'.
    ENDAT.

  ENDLOOP.


* nicht gefundene Einträge in T012D ------------------------------------
* entries not found in T012D -------------------------------------------
  SORT err_t012d.
  LOOP AT err_t012d.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_t012d-zbukr.
    fimsg-msgv2 = err_t012d-hbkid.
    PERFORM message USING '275'.

    AT LAST.
      PERFORM message USING '276'.
    ENDAT.

  ENDLOOP.


* nicht gefundene Einträge in T015Z ------------------------------------
* entries not found in T015Z -------------------------------------------
  SORT err_t015z.
  LOOP AT err_t015z.

    AT FIRST.
      ADD 1 TO cnt_error.
      PERFORM message USING '277'.
    ENDAT.

    fimsg-msgv1 = err_t015z-spras.
    fimsg-msgv2 = err_t015z-einh.
    fimsg-msgv3 = err_t015z-ziff.
    PERFORM message USING '257'.

    AT LAST.
      PERFORM message USING '278'.
    ENDAT.

  ENDLOOP.


* fehlerhafte Einträge in T042E ----------------------------------------
* wrong entries in T042E -----------------------------------------------
  SORT err_t042e.
  LOOP AT err_t042e.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_t042e-rzawe.
    fimsg-msgv2 = err_t042e-zbukr.
    PERFORM message USING '283'.

  ENDLOOP.


* nicht gefundene Einträge in T042T ------------------------------------
* entries not found in T042T -------------------------------------------
  SORT err_t042t.
  LOOP AT err_t042t.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_t042t-zbukr.
    PERFORM message USING '279'.

    AT LAST.
      PERFORM message USING '280'.
    ENDAT.

  ENDLOOP.


* nicht in ISO-Code umgesetzte Währungsschlüssel -----------------------
* ISO code for currency not found --------------------------------------
  SORT err_tcurc.
  LOOP AT err_tcurc.

    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_tcurc-waers.
    PERFORM message USING '281'.

  ENDLOOP.


* Zahlungen mit zu großen Beträgen--------------------------------------
* payments with too high amounts----------------------------------------
  SORT err_betrag.
  LOOP AT err_betrag.
    AT FIRST.
      ADD 1 TO cnt_error.
    ENDAT.

    fimsg-msgv1 = err_betrag-waers.
    fimsg-msgv2 = err_betrag-rwbtr.
    fimsg-msgv3 = err_betrag-zbukr.
    fimsg-msgv4 = err_betrag-vblnr.
    PERFORM message USING '382'.
  ENDLOOP.


* Ausgabe des Fehlerprotokolls -----------------------------------------
* Output of error log --------------------------------------------------
  CALL FUNCTION 'FI_MESSAGE_CHECK'
       EXCEPTIONS
            no_message = 4.
  CHECK sy-subrc EQ 0.
  IF sy-batch NE space.
    CALL FUNCTION 'FI_MESSAGE_GET'
         TABLES
              t_fimsg = tab_fimsg.
    LOOP AT tab_fimsg.
      AT NEW msort.
        MESSAGE s257 WITH space space space space.
      ENDAT.
      fimsg = tab_fimsg.
      MESSAGE ID fimsg-msgid  TYPE 'S'     NUMBER fimsg-msgno
         WITH fimsg-msgv1     fimsg-msgv2  fimsg-msgv3  fimsg-msgv4.
    ENDLOOP.
  ELSEIF flg_selektiert EQ 0.
    ADD 1 TO cnt_error.
    fimsg-msgv1 = syst-repid.
    PERFORM message USING '073'.
  ENDIF.

  CLEAR hlp_auth.      " fehlermeldungen ohne Berechtigungsschutz
  PERFORM print_on USING ' ' text_003 par_prib par_sofb 'LISTFS'.

  FORMAT COLOR 6 INTENSIFIED.
  WRITE text_003.
  FORMAT RESET.
  SKIP 2.
  CALL FUNCTION 'FI_MESSAGE_PRINT'
       EXPORTING
            i_xskip = 'X'.

  PERFORM print_off USING 'LISTFS' text_003.
  IF SY-SPONO NE 0.
    TAB_AUSGABE-ERROR = 'X'.
    MODIFY TAB_AUSGABE INDEX SY-TABIX.
  ENDIF.

ENDFORM.                               "FEHLERMELDUNGEN



*----------------------------------------------------------------------*
* FORM MESSAGE                                                         *
*----------------------------------------------------------------------*
* Sammeln der Messages, die im Protokoll ausgegeben werden sollen.     *
* Vor Aufruf sind die FIMSG-MSGVn zu füllen, falls sie benutzt werden. *
* Es muß die ID übergeben werden, wenn sie von F0 abweicht.            *
* Wird kein Sortierkriterium übergeben, so wird CNT_ERROR benutzt.     *
*----------------------------------------------------------------------*
* MSGNO - Messagenummer                                                *
*----------------------------------------------------------------------*
FORM message USING msgno.


  IF fimsg-msort EQ space.
    fimsg-msort = cnt_error.
  ENDIF.
  IF fimsg-msgid EQ space.
    fimsg-msgid = 'F0'.
  ENDIF.
  fimsg-msgno   = msgno.
  fimsg-msgty   = 'S'.
  CALL FUNCTION 'FI_MESSAGE_COLLECT'
       EXPORTING
            i_fimsg = fimsg.
  CLEAR fimsg.


ENDFORM.                               "MESSAGE



*----------------------------------------------------------------------*
* FORM INFORMATION                                                     *
*----------------------------------------------------------------------*
* Ausgabe von Informationen über die erzeugten Spoolnummern            *
* information about generated spool datasets                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM information.

  "Bei bedingtem Loop über interne
  DATA:                                "Tabellen wird der Zeitpunkt AT
    up_titel(1) TYPE n VALUE 0,        "FIRST nicht durchlaufen!
    up_uline(1) TYPE n VALUE 0,        "LOOP durchlaufen ?
    up_rqident  LIKE tsp01-rqident.

  SORT tab_ausgabe.

* Erster Loop für Sofortdruck
  LOOP AT tab_ausgabe WHERE immed NE space.
    CHECK tab_ausgabe-spoolnr NE 0.
    up_rqident = tab_ausgabe-spoolnr.
    CALL FUNCTION 'RSPO_OUTPUT_SPOOL_REQUEST'
         EXPORTING
              spool_request_id = up_rqident
         EXCEPTIONS
              OTHERS           = 0.
  ENDLOOP.

  IMPORT flg_local FROM MEMORY ID 'MFCHKFN0'.
  CHECK:                               "keine Info bei Sofortdruck
    par_sofo EQ space,                 "und bei Neudruck eines Schecks
    sy-subrc NE 0.                     "no info when print immediately
  "and when re-print of a check
  IF sy-batch EQ space.
    NEW-PAGE NO-TITLE LINE-SIZE 81.
  ENDIF.

* Zweiter Loop zur Ausgabe der Files
  CLEAR tab_ausgabe.
  LOOP AT tab_ausgabe WHERE renum NE space.
    up_uline = 1.
    IF up_titel EQ 0.
      IF sy-batch EQ space.
        FORMAT COLOR 1 INTENSIFIED.
        flg_local = space.
        WRITE:
            / sy-uline.
        HIDE flg_local.
        WRITE:
            / sy-vline NO-GAP,
         (79) text_092,
           81 sy-vline.
        HIDE flg_local.
        WRITE:
            / sy-uline.
        HIDE flg_local.
        FORMAT COLOR 1 INTENSIFIED OFF.
        WRITE:
            / sy-vline NO-GAP,
         (79) text_093,
        40(1) sy-vline,
           81 sy-vline.
        HIDE flg_local.
        WRITE:
            / sy-uline.
        HIDE flg_local.
      ELSE.
        MESSAGE s065.
        WRITE:
          text_092 TO txt_zeile.
        MESSAGE s065 WITH txt_zeile.
        WRITE:
          text_093 TO txt_zeile,
          '/'      TO txt_zeile+37(1).
        CONDENSE txt_zeile.
        MESSAGE s065 WITH txt_zeile.
        MESSAGE s064.
      ENDIF.
      up_titel = 1.
    ENDIF.
    IF sy-batch EQ space.
      FORMAT COLOR 2 INTENSIFIED OFF.
      flg_local = 'F'.
      FORMAT HOTSPOT ON.
      WRITE:
          / sy-vline NO-GAP,
            tab_ausgabe-name,
         40 sy-vline NO-GAP,
            tab_ausgabe-filename(40),
         81 sy-vline.
      FORMAT HOTSPOT OFF.
      HIDE: flg_local, tab_ausgabe.
    ELSE.
      WRITE:
        tab_ausgabe-name TO txt_zeile,
        '/'              TO txt_zeile+37(1).
      CONDENSE txt_zeile.
      MESSAGE s065 WITH txt_zeile tab_ausgabe-filename.
    ENDIF.
    DELETE tab_ausgabe.
  ENDLOOP.

* Dritter Loop zur Ausgabe der Spool-Dateien
  CLEAR tab_ausgabe.
  LOOP AT tab_ausgabe.
    up_uline = 1.
    AT FIRST.
      IF sy-batch EQ space.
        FORMAT COLOR 1 INTENSIFIED.
        flg_local = space.
        WRITE:
            / sy-uline.
        HIDE flg_local.
        WRITE:
            / sy-vline NO-GAP,
         (79) text_090,
           81 sy-vline,
              sy-uline.
        HIDE flg_local.
        FORMAT COLOR 1 INTENSIFIED OFF.
        WRITE:
            / sy-vline NO-GAP,
         (79) text_091,
        40(1) sy-vline NO-GAP,
        55(1) sy-vline NO-GAP,
           81 sy-vline.
        HIDE flg_local.
        WRITE:
            / sy-uline.
        HIDE flg_local.
      ELSE.
        MESSAGE s065.
        WRITE:
          text_090 TO txt_zeile.
        MESSAGE s065 WITH txt_zeile.
        WRITE:
          text_091 TO txt_zeile,
          '/'      TO txt_zeile+37(1),
          '/'      TO txt_zeile+52(1).
        CONDENSE txt_zeile.
        MESSAGE s065 WITH txt_zeile.
        MESSAGE s064.
      ENDIF.
    ENDAT.
    IF sy-batch EQ space.
      IF tab_ausgabe-error EQ 'X'.
        FORMAT COLOR 6 INTENSIFIED.
      ELSE.
        FORMAT COLOR 2 INTENSIFIED OFF.
      ENDIF.
      IF tab_ausgabe-error EQ 'X'.
        flg_local = 'E'.
      ELSE.
        flg_local = 'S'.
      ENDIF.
      FORMAT HOTSPOT ON.
      WRITE:
          / sy-vline NO-GAP,
            tab_ausgabe-name,
         40 sy-vline NO-GAP,
            tab_ausgabe-dataset,
         55 sy-vline NO-GAP,
            tab_ausgabe-spoolnr,
         81 sy-vline.
      FORMAT HOTSPOT OFF.
      HIDE: flg_local, tab_ausgabe.
    ELSE.
      WRITE:
        tab_ausgabe-name     TO txt_zeile,
        '/'                  TO txt_zeile+37(1),
        tab_ausgabe-dataset  TO txt_zeile+40,
        '/'                  TO txt_zeile+52(1),
        tab_ausgabe-spoolnr  TO txt_zeile+55.
      CONDENSE txt_zeile.
      MESSAGE s065 WITH txt_zeile(50) txt_zeile+50.
    ENDIF.
  ENDLOOP.

  IF sy-batch EQ space AND up_uline NE 0.
    flg_local = space.
    WRITE:
      / sy-uline.
    HIDE flg_local.
  ENDIF.


ENDFORM.                               "INFORMATION



*----------------------------------------------------------------------*
* AT LINE-SELECTION                                                    *
*----------------------------------------------------------------------*
* Auswahl einer Zeile der Information                                  *
* Verzweigen in die DTA-Verwaltung oder Druckverwaltung                *
*----------------------------------------------------------------------*
AT LINE-SELECTION.

  TYPE-POOLS sp01r.
  DATA up_list TYPE sp01r_id_list WITH HEADER LINE.
  DATA BEGIN OF up_bdc OCCURS 9.
          INCLUDE STRUCTURE bdcdata.
  DATA END OF up_bdc.
  DATA up_fdta      LIKE tstc-tcode VALUE 'FDTA'.
  DATA up_laufd(10) TYPE c.
  DATA up_meldung   LIKE shkontext-meldung.
  DATA up_titel     LIKE shkontext-titel.

  IF sy-lsind EQ 1.

    CASE flg_local.
      WHEN space.                      "keine gültige Auswahl

      WHEN 'E'.                        "Fehlerliste
        FORMAT COLOR 6 INTENSIFIED.
        ULINE AT (102).
        WRITE:
          /     sy-vline NO-GAP,
          (100) text_003 NO-GAP,
                sy-vline.
        FORMAT COLOR 2.
        ULINE AT (102).
        REFRESH tab_fimsg.
        CALL FUNCTION 'FI_MESSAGE_GET'
             TABLES
                  t_fimsg = tab_fimsg.
        LOOP AT tab_fimsg.
          fimsg = tab_fimsg.
          CALL FUNCTION 'K_MESSAGE_TRANSFORM'
               EXPORTING
                    par_langu = sy-langu
                    par_msgid = fimsg-msgid
                    par_msgno = fimsg-msgno
                    par_msgty = fimsg-msgty
                    par_msgv1 = fimsg-msgv1
                    par_msgv2 = fimsg-msgv2
                    par_msgv3 = fimsg-msgv3
                    par_msgv4 = fimsg-msgv4
               IMPORTING
                    par_msgtx = txt_zeile
               EXCEPTIONS
                    OTHERS    = 8.
          WRITE:
            /    sy-vline    NO-GAP,
                 fimsg-msgid(2),
                 fimsg-msgno NO-GAP,
                 sy-vline    NO-GAP,
            (93) txt_zeile   NO-GAP,
                 sy-vline.
          HIDE: flg_local, fimsg, txt_zeile.
          AT END OF msort.
            ULINE AT (102).
          ENDAT.
        ENDLOOP.
        CLEAR: fimsg, txt_zeile.

      WHEN 'F'.                        "Sprung in die DTA-Verwaltung
        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
             EXPORTING
                  tcode  = up_fdta
             EXCEPTIONS
                  ok     = 0
                  OTHERS = 4.
        IF sy-subrc NE 0.
          MESSAGE s172(00) WITH up_fdta.
        ELSE.
          REFRESH up_bdc.
          CLEAR up_bdc.
          up_bdc-program  = 'SAPMFDTA'.
          up_bdc-dynpro   = '100'.
          up_bdc-dynbegin = 'X'.
          APPEND up_bdc.
          CLEAR up_bdc.
          up_bdc-fnam     = 'REGUT-RENUM'.
          up_bdc-fval     = tab_ausgabe-renum.
          APPEND up_bdc.
          CLEAR up_bdc.
          up_bdc-fval     = '/8'.
          up_bdc-fnam     = 'BDC_OKCODE'.
          APPEND up_bdc.
          CLEAR up_bdc.
          up_bdc-program  = 'SAPMFDTA'.
          up_bdc-dynpro   = '200'.
          up_bdc-dynbegin = 'X'.
          APPEND up_bdc.
          CLEAR up_bdc.
          up_bdc-fval     = '/BDA'.
          up_bdc-fnam     = 'BDC_OKCODE'.
          APPEND up_bdc.
          CALL TRANSACTION up_fdta USING up_bdc MODE 'E'.
        ENDIF.

      WHEN 'S'.                        "Sprung in die Druckverwaltung
        CHECK tab_ausgabe-spoolnr NE space.
        REFRESH up_list.
        up_list-id = tab_ausgabe-spoolnr.
        APPEND up_list.
        CALL FUNCTION 'RSPO_RID_SPOOLREQ_LIST'
             EXPORTING
                  id_list = up_list[]
             EXCEPTIONS
                  OTHERS  = 0.

    ENDCASE.

  ELSE.

    CHECK:
      flg_local EQ 'E',
      NOT fimsg-msgid IS INITIAL,
      NOT fimsg-msgno IS INITIAL.
    up_titel   = text_003.
    up_meldung = txt_zeile.
    CALL FUNCTION 'HELPSCREEN_NA_CREATE'
         EXPORTING
              langu   = sy-langu
              meldung = up_meldung
              meld_id = fimsg-msgid
              meld_nr = fimsg-msgno
              msgv1   = fimsg-msgv1
              msgv2   = fimsg-msgv2
              msgv3   = fimsg-msgv3
              msgv4   = fimsg-msgv4
              titel   = up_titel.
    CLEAR fimsg.

  ENDIF.


*----------------------------------------------------------------------*
* FORM BELEGDATEN_SCHREIBEN                                            *
*----------------------------------------------------------------------*
* Beleginformation zum Speichern in interne Tabelle schreiben          *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM belegdaten_schreiben.

  CLEAR tab_belege30a.
  tab_belege30a-mandt   = sy-mandt.
  IF reguh-pyord IS INITIAL.
    tab_belege30a-bukrs = reguh-zbukr.
    tab_belege30a-belnr = reguh-vblnr.
    tab_belege30a-gjahr = regud-gjahr.
    tab_belege30a-ubhkt = reguh-ubhkt.
  ELSE.
    tab_belege30a-pyord = reguh-pyord.
  ENDIF.
  APPEND tab_belege30a.


ENDFORM.                               "BELEGDATEN_SCHREIBEN



*----------------------------------------------------------------------*
* FORM TAB_BELEGE_SCHREIBEN                                            *
*----------------------------------------------------------------------*
* Alle Beleginformationen in die Datenbank sichern                     *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
FORM tab_belege_schreiben.

  DATA: relid LIKE sy-saprl.           "Current Release
  DATA: tab_belege40a LIKE dta_belege OCCURS 0 WITH HEADER LINE.

  rfdt-aedat = sy-datlo.
  rfdt-usera = sy-uname.
  rfdt-pgmid = sy-repid.

  relid = sy-saprl.
  EXPORT tab_belege40a FROM tab_belege30a
         relid                         "Tabelle und Release sichern
         TO DATABASE rfdt(fb)
         ID hlp_dta_id.
  IF sy-subrc NE 0.
    IF sy-batch EQ space.
      MESSAGE a226 WITH 'RFDT'.
    ELSE.
      MESSAGE s226 WITH 'RFDT'.
      STOP.
    ENDIF.
  ENDIF.

ENDFORM.                               "TAB_BELEGE_SCHREIBEN



*----------------------------------------------------------------------*
* FORM ZUSATZFELD_FUELLEN                                              *
*----------------------------------------------------------------------*
* Füllt die landes- oder formatspezifischen Zusatzdaten in REGUT-DTKEY *
* Fills the additional country or format specific information          *
*----------------------------------------------------------------------*
* EXPORT:                                                              *
* ZUSATZ  - Feld, in das die Zusatzdaten einzutragen sind              *
* KFZLAND - KFZ-Code des Landes des aufrufenden Druckprogramms         *
*----------------------------------------------------------------------*
FORM zusatzfeld_fuellen USING zusatz kfzland.

  DATA up_zusatz LIKE regut-dtkey.

* Formatspezifisches Zusatzfeld
  IF hlp_dtfor EQ 'MT100'.             "Swift international
    up_zusatz      = reguh-hbkid.

  ELSEIF hlp_dtfor EQ 'SAP IDOC'.      "IDoc für EDI
    up_zusatz      = reguh-hbkid.
    up_zusatz+5(1) = regud-xeinz.

  ELSEIF hlp_dtfor(5) EQ 'DTAUS'       "Deutschland Inland
      OR hlp_dtfor    EQ 'MTS'         "Neuseeland Inland
      OR hlp_dtfor    EQ 'BECS'.       "Australien Inland
    up_zusatz      = reguh-hbkid.
    up_zusatz+5(1) = regud-xeinz.
    up_zusatz+6    = reguh-hktid.

* Länderspezifisches Zusatzfeld
  ELSEIF kfzland EQ 'CH'.              "Schweiz
    up_zusatz+5(1) = regud-xeinz.

  ELSEIF kfzland EQ 'DK'               "Dänemark
      OR kfzland EQ 'F'.               "Frankreich
    up_zusatz      = reguh-hbkid.
    up_zusatz+5(1) = regud-xeinz.

  ELSEIF kfzland EQ 'ZA'.              "Südafrika
    SELECT * FROM regut
        WHERE banks EQ 'ZA'
          AND dtfor EQ hlp_dtfor       "Entweder nur ACB oder nur EFT
          AND laufd EQ reguh-laufd
          AND laufi EQ reguh-laufi.
      EXIT.
    ENDSELECT.
    IF sy-subrc EQ 0.                  "Eintrag gefunden
      up_zusatz = regut-dtkey.         "Da schon vorhanden -> kopieren
    ELSE.
      SELECT * FROM regut
        WHERE banks EQ 'ZA '
          AND dtfor EQ hlp_dtfor
        ORDER BY dtkey.
      ENDSELECT.
      IF sy-subrc NE 0.
        up_zusatz+0(4) = '0001'.
      ELSE.
        ADD 1 TO regut-dtkey+0(4).
        WRITE regut-dtkey TO up_zusatz+0(4) RIGHT-JUSTIFIED.
        WHILE up_zusatz+0(4) CA space.
          REPLACE space WITH '0' INTO up_zusatz+0(4).
        ENDWHILE.
      ENDIF.
    ENDIF.

  ELSE.                                "default
    up_zusatz = reguh-hbkid.
  ENDIF.

  zusatz = up_zusatz.

ENDFORM.                               "ZUSATZFELD_FUELLEN



*----------------------------------------------------------------------*
* FORM TEMSE_OEFFNEN                                                   *
*----------------------------------------------------------------------*
* Öffnen einer TemSe-Datei für schreibenden Zugriff. Ein Zusatzfeld    *
* im Schlüssel und eine 8-stellige Referenz-Nummer werden übergeben.   *
* Es wird ein Satz in die Datei geschrieben.                           *
* Open a temporary sequential file for write access. The file-name     *
* is generated in the form 'TEMSE_NAME'. One record is stored.         *
* 'DTKEY' contains key-field-information for this record. An 8-digit   *
* random number is returned.                                           *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
* *REGUT-DTKEY  - ein Zusatzkey zu den TemSe-Daten                     *
*               - an additional key                                    *
* EXPORT:                                                              *
* HLP_RENUM     - 8-stellige Nummer, noch nicht vergeben               *
*               - an 8-digit numeric to generate a reference-string    *
*----------------------------------------------------------------------*
FORM temse_oeffnen.
  DATA: _rc(5),
        _errmsg(100).

  PERFORM naechster_index USING hlp_renum.

  PERFORM fuellen_regut USING *regut-dtkey.

  PERFORM temse_name USING hlp_renum   "Dateinamen generieren lassen
                           hlp_temsename.

   *regut-tsnam = hlp_temsename.       "Name der TemSe-Datei

  CALL 'C_RSTS_OPEN_WRITE'
       ID 'HANDLE'  FIELD hlp_handle   "file-handle
       ID 'NAME'    FIELD hlp_temsename"gewünschter Dateiname
       ID 'BINARY'  FIELD 'X'          "binär öffnen !
       ID 'TYPE'    FIELD 'DATA'
       ID 'RECTYP'  FIELD 'U------'    "Zusatz zum binären Öffnen
       ID 'RC'      FIELD _rc
       ID 'ERRMSG'  FIELD _errmsg.

  IF sy-subrc NE 0.                    "Fehler beim Öffnen
    IF sy-batch EQ space.
      MESSAGE a182(fr) WITH hlp_temsename.
    ELSE.
      MESSAGE s182(fr) WITH hlp_temsename.
      STOP.
    ENDIF.
  ENDIF.


ENDFORM.                               "TEMSE_OEFFNEN



*----------------------------------------------------------------------*
* FORM TEMSE_SCHREIBEN                                                 *
*----------------------------------------------------------------------*
* Schreiben in die geöffnete TemSe-Datei.                              *
* writing data to the already opened file                              *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
* BUFFER - der zu schreibende Text                                     *
*        - data to be stored                                           *
*----------------------------------------------------------------------*
FORM temse_schreiben USING value(buffer).
  DATA: _rc(5),
        _errmsg(100).

  CALL 'C_RSTS_WRITE'
       ID 'HANDLE'  FIELD hlp_handle
       ID 'BUFF'    FIELD buffer
       ID 'RC'      FIELD _rc
       ID 'ERRMSG'  FIELD _errmsg.

  IF sy-subrc NE 0.                    "Fehler beim Schreiben
    IF sy-batch EQ space.
      MESSAGE a229.
    ELSE.
      MESSAGE s229.
      STOP.
    ENDIF.
  ENDIF.

ENDFORM.                               "TEMSE_SCHREIBEN



*----------------------------------------------------------------------*
* FORM TEMSE_SCHLIESSEN                                                *
*----------------------------------------------------------------------*
* Schließen der geöffneten TemSe-Datei.                                *
* close file                                                           *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
*----------------------------------------------------------------------*
FORM temse_schliessen.
  DATA: _rc(5),
        _errmsg(100).

  CALL 'C_RSTS_CLOSE'
       ID 'HANDLE'  FIELD hlp_handle
       ID 'RC'      FIELD _rc
       ID 'ERRMSG'  FIELD _errmsg.

  IF sy-subrc NE 0.                    " Fehler beim Schließen
    IF sy-batch EQ space.
      MESSAGE w230.
    ELSE.
      MESSAGE s230.
      STOP.
    ENDIF.
  ELSE.
    CLEAR hlp_handle.
  ENDIF.

ENDFORM.                               "TEMSE_SCHLIESSEN



*----------------------------------------------------------------------*
* FORM NAECHSTER_INDEX                                                 *
*----------------------------------------------------------------------*
* nächsten freien Index suchen                                         *
* get next free number                                                 *
*----------------------------------------------------------------------*
* NUMBER enthält diesen Index / contains a new number                  *
*----------------------------------------------------------------------*
FORM naechster_index USING number LIKE febkey-kukey.

  CALL FUNCTION 'GET_SHORTKEY_FOR_FEBKO'  "Nächsten freien Index holen
    EXPORTING   i_tname   = 'TEMSE'
    IMPORTING   e_kukey   =  number
    EXCEPTIONS  febkey_update_error = 1.

  IF sy-subrc = 1.
    IF sy-batch EQ space.
      MESSAGE a228 WITH 'FEBKEY'.
    ELSE.
      MESSAGE s228 WITH 'FEBKEY'.
      STOP.
    ENDIF.
  ELSEIF number EQ '00000000'.         "Field: FEBKEY-KUKEY: Numeric(8)
    PERFORM naechster_index USING number. "nächste Nummer holen
  ENDIF.

ENDFORM.                               "NAECHSTER_INDEX



*----------------------------------------------------------------------*
* FORM TEMSE_NAME                                                      *
*----------------------------------------------------------------------*
* TemSe-Namen generieren                                               *
* Generate a new TemSe-name                                            *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
*   NUMBER: eine 8-stellige Zufallszahl (noch unbenutzt)               *
*   NUMBER: is a 8-digit random number (currently unused)              *
* EXPORT:                                                              *
*   FILE enthält Namen der TemSe-Datei                                 *
*   FILE contains the new file-name                                    *
* BEISPIEL / EXAMPLE:                                                  *
*   NUMBER = 00193711, DATE = MAR 2, 1994, TIME = 08:12:37             *
*   --> FILE = DTA940302081237_3711                                    *
*----------------------------------------------------------------------*
FORM temse_name USING value(number)
                      file.

  DATA: up_name(64),
        up_num  LIKE febko-kukey,
        up_len  TYPE p.

  up_num = number.
  DESCRIBE FIELD file LENGTH up_len.

  IF up_len LT 20.                     "Mindestlänge des Namens
    IF sy-batch EQ space.
      MESSAGE a183.
    ELSE.
      MESSAGE s183.
      STOP.
    ENDIF.
  ENDIF.

  CLEAR up_name.
  up_name    = 'DTA'.
  up_name+3  = sy-datlo+2(6).
  up_name+9  = sy-timlo(6).
  up_name+15 = '_'.
  up_name+16 = up_num+4(4).

  file = up_name.

ENDFORM.                               "TEMSE_NAME



*----------------------------------------------------------------------*
* FORM DATEI_OEFFNEN                                                   *
*----------------------------------------------------------------------*
* die jeweilige Datei (TemSe/File) öffnen                              *
* open current file (either in TemSe or file-system)                   *
*----------------------------------------------------------------------*
FORM datei_oeffnen.

  IF hlp_temse CA par_dtyp.            "TemSe-Format
    PERFORM temse_oeffnen.
  ELSE.                                "disk-/tape-fmt on file-system
    PERFORM naechster_index USING hlp_renum.
    PERFORM fuellen_regut USING *regut-dtkey.
    ADD 1 TO cnt_filenr.
    hlp_filename    = par_unix.
    hlp_filename+45 = cnt_filenr.
    CONDENSE hlp_filename NO-GAPS.
    OPEN DATASET hlp_filename FOR OUTPUT.
    IF sy-subrc NE 0.
      IF sy-batch EQ space.
        MESSAGE a182(fr) WITH hlp_filename.
      ELSE.
        MESSAGE s182(fr) WITH hlp_filename.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

* Referenznr für RFDT sichern, Tabelle für Zahlungsbelege löschen
* store reference-number, refresh table for document-numbers
  CALL FUNCTION 'COMPUTE_CONTROL_NUMBER'
       EXPORTING
            i_refno  = hlp_renum
       IMPORTING
            e_result = hlp_resultat.
  regud-label = hlp_dta_id-refnr = hlp_resultat.
  CLEAR   tab_belege30a.
  REFRESH tab_belege30a.

ENDFORM.                               "DATEI_OEFFNEN



*----------------------------------------------------------------------*
* FORM DATEI_SCHLIESSEN                                                *
*----------------------------------------------------------------------*
* die jeweilige Datei (TemSe/File) schliessen                          *
* close current file (either in TemSe or file-system)                  *
*----------------------------------------------------------------------*
* Benutzt wird HLP_TEMSENAME und TEXT_006 bei Schreiben in TemSe bzw.  *
*              HLP_FILENAME  und TEXT_005 bei Schreiben ins Filesystem *
* Routine uses HLP_TEMSENAME and TEXT_006 when writing to TemSe resp.  *
*              HLP_FILENAME  and TEXT_005 when writing to file system  *
*----------------------------------------------------------------------*
FORM datei_schliessen.

  DATA:
    up_fname LIKE hlp_filename,
    up_text  LIKE rfpdo2-fordtext.

  PERFORM abschluss_regut.
  IF hlp_temse CA par_dtyp.            "TemSe
    PERFORM temse_schliessen.
    up_fname = hlp_temsename.
    up_text  = text_006.
  ELSE.                                "Filesystem
    CLOSE DATASET hlp_filename.
    up_fname = hlp_filename.
    up_text  = text_005.
  ENDIF.

  CLEAR tab_ausgabe.
  tab_ausgabe-name     = up_text.
  tab_ausgabe-filename = up_fname.
  tab_ausgabe-renum    = *regut-renum.
  REPLACE '&' WITH reguh-hbkid INTO tab_ausgabe-name.
  COLLECT tab_ausgabe.

* Gesammelte Zahlungsbelegdaten in Datenbank RFDT sichern
* Store payment documents (not for proposal run or HR !)
  IF reguh-xvorl IS INITIAL AND        "Kein Vorschlagslauf und
     hlp_laufk CA ' *R'.               "nur FI oder Payment Request
    PERFORM tab_belege_schreiben.
  ENDIF.

ENDFORM.                               "DATEI_SCHLIESSEN



*----------------------------------------------------------------------*
* FORM FUELLEN_REGUT                                                   *
*----------------------------------------------------------------------*
* REGUT-Felder mit den bereits bekannten Werten füllen                 *
* Fill REGUT-fields with current values                                *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
*----------------------------------------------------------------------*
FORM fuellen_regut USING value(dtkey).

  CLEAR *regut.
  MOVE-CORRESPONDING reguh TO *regut.  "ZBUKR,LAUFD,LAUFI,XVORL füllen
   *regut-banks = reguh-ubnks.

* Die bereits bekannten Funktionsfelder füllen
   *regut-dtkey  = dtkey.              "Zusatzkey
   *regut-tsusr  = sy-uname.
   *regut-report = sy-repid.

  CLEAR regut-lfdnr.                   "Wichtig bei Bankwechsel !
  SELECT * FROM regut                  "Bestimme die tatsächliche LFDNR,
         WHERE zbukr = reguh-zbukr     "  wenn nur ein zahlender
           AND banks = reguh-ubnks     "  Buchungskreis vorliegt
           AND laufd = reguh-laufd
           AND laufi = reguh-laufi
           AND xvorl = reguh-xvorl
           AND dtkey = dtkey
         ORDER BY lfdnr DESCENDING.    "Größte Nummer
    EXIT.
  ENDSELECT.
  IF NOT sy-subrc IS INITIAL.
    SELECT * FROM regut                "Bestimme die tatsächliche LFDNR,
           WHERE zbukr = space         "  wenn mehrere zahlende
             AND banks = reguh-ubnks   "  Buchungskreise im File vor-
             AND laufd = reguh-laufd   "  kommen (ZBUKR = SPACE)
             AND laufi = reguh-laufi
             AND xvorl = reguh-xvorl
             AND dtkey = dtkey
           ORDER BY lfdnr DESCENDING.  "Größte Nummer
      EXIT.
    ENDSELECT.
  ENDIF.

   *regut-lfdnr = regut-lfdnr + 1.     "nächstgrößeren Wert nehmen

ENDFORM.                               "FUELLEN_REGUT



*----------------------------------------------------------------------*
* FORM ABSCHLUSS_REGUT                                                 *
*----------------------------------------------------------------------*
* Noch unbesetzte REGUT-Felder füllen, Datensatz aktualisieren         *
* Fill missing REGUT-fields and update record on database              *
*----------------------------------------------------------------------*
* IMPORT: FILENAME - Nur bei TemSe: Vorschlag für Download-Dateinamen  *
*         FILENAME - only for TemSe: Proposal filename for download    *
*----------------------------------------------------------------------*
FORM abschluss_regut.

  DATA:   UP_LINES LIKE SY-LINNO.
  RANGES: UP_ZBUKR FOR  REGUTA-ZBUKR.

  IF hlp_tsdat IS INITIAL.             "Noch kein Datum erfasst
    hlp_tsdat = sy-datlo.
  ENDIF.

  IF hlp_tstim IS INITIAL.             "Noch keine Zeit erfasst
    hlp_tstim = sy-timlo.
  ENDIF.

* Noch unbelegte Funktionsfelder der Leiste für REGUT füllen
   *regut-waers = t001-waers.
   *regut-rbetr = sum_regut.
   *regut-renum = hlp_resultat.
   *regut-dtfor = hlp_dtfor.
  *regut-tsdat = hlp_tsdat.            "Werte der Zeiterfassung kopieren
   *regut-tstim = hlp_tstim.

  IF hlp_temse CA par_dtyp.            "TemSe
    IF NOT par_unix IS INITIAL.        "Wert wurde bereits angegeben
       *regut-dwnam = par_unix.        "als Vorschlagswert übernehmen
    ENDIF.
  ELSE.                                "keine Temse -> Download fertig !
     *regut-fsnam = hlp_filename.      "Dateinamen übernehmen,
     *regut-dwnam = hlp_filename.      "als Downloadnamen setzen und
     *regut-dwdat = sy-datlo.          "aktuelle Daten festhalten
     *regut-dwtim = sy-timlo.
     *regut-dwusr = sy-uname.
  ENDIF.

* Update DB table of paying company codes and origins of the file
* clear paying company code if it is not unique
  CLEAR: UP_LINES, UP_ZBUKR, UP_ZBUKR[].
  UP_ZBUKR-SIGN   = 'I'.
  UP_ZBUKR-OPTION = 'EQ'.
  LOOP AT TAB_REGUTA.
    UP_ZBUKR-LOW = TAB_REGUTA-ZBUKR.
    COLLECT UP_ZBUKR.
  ENDLOOP.
  DESCRIBE TABLE UP_ZBUKR LINES UP_LINES.
  IF UP_LINES > 1.
    CLEAR *REGUT-ZBUKR.
    DO.    "check REGUTA to avoid duprecs (see note 623539)
      SELECT COUNT(*) FROM REGUTA
                     WHERE BANKS EQ *REGUT-BANKS
                       AND LAUFD EQ *REGUT-LAUFD
                       AND LAUFI EQ *REGUT-LAUFI
                       AND XVORL EQ *REGUT-XVORL
                       AND DTKEY EQ *REGUT-DTKEY
                       AND LFDNR EQ *REGUT-LFDNR
                       AND ZBUKR IN UP_ZBUKR.
      IF SY-DBCNT NE 0.
        ADD 1 TO *REGUT-LFDNR.
      ELSE.
        EXIT.
      ENDIF.
      IF *REGUT-LFDNR EQ 999.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

  INSERT regut FROM *regut.
  IF sy-subrc NE 0.                    "Insert fehlerhaft
    DO 10 TIMES.                       "10 Versuche maximal !
       *regut-lfdnr = *regut-lfdnr + sy-index.
      INSERT regut FROM *regut.        "Suche springend nach freiem Wert
      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.
    ENDDO.

    IF sy-subrc NE 0.                  "Inserts fehlerhaft --> Abbruch
      IF sy-batch EQ space.
        MESSAGE a228 WITH 'REGUT'.
      ELSE.
        MESSAGE s228 WITH 'REGUT'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.                               "IF SY-SUBRC...

  READ TABLE tab_reguta INDEX 1.
  IF tab_reguta-lfdnr <> *regut-lfdnr.
    LOOP AT tab_reguta.
      tab_reguta-lfdnr = *regut-lfdnr.
      MODIFY TAB_REGUTA.
    ENDLOOP.
  ENDIF.
  INSERT reguta FROM TABLE tab_reguta.
  CLEAR: tab_reguta, tab_reguta[].

  CALL FUNCTION 'DB_COMMIT'.

  SELECT * FROM regut UP TO 1 ROWS     "Test, ob gerade ein Duplikat
         WHERE zbukr = *regut-zbukr    "  erstellt worden ist
           AND banks = *regut-banks    "  (gleiche Attribute, aber
           AND laufd = *regut-laufd    "  andere Referenznummer)
           AND laufi = *regut-laufi
           AND xvorl = *regut-xvorl
           AND dtkey = *regut-dtkey
           AND waers = *regut-waers
           AND rbetr = *regut-rbetr
           AND dtfor = *regut-dtfor
           AND renum NE *regut-renum.
  ENDSELECT.
  IF sy-subrc EQ 0.
    fimsg-msgv1 = regut-renum.
    fimsg-msgv2 = *regut-renum.
    PERFORM message USING 417.
  ENDIF.

ENDFORM.                               "ABSCHLUSS_REGUT



*----------------------------------------------------------------------*
* FORM STORE_ON_FILE                                                   *
*----------------------------------------------------------------------*
* Ausgabe in die TemSe oder in das File-System                         *
* Output into TemSe or into file-system                                *
*----------------------------------------------------------------------*
* IMPORT: DATEN : zu schreibende Daten                                 *
*                 data to be stored                                    *
*----------------------------------------------------------------------*
FORM store_on_file USING daten.

  IF hlp_temse CA par_dtyp.            "Temse
    PERFORM temse_schreiben USING daten.
  ELSE.
    TRANSFER daten TO hlp_filename.
  ENDIF.

* Fill internal table of paying company codes of actual DME file
* for FDTA authority check
  MOVE-CORRESPONDING
       *regut TO tab_reguta.
  tab_reguta-zbukr = reguh-zbukr.
  tab_reguta-dorigin = reguh-dorigin.
  COLLECT tab_reguta.

ENDFORM.                               "STORE_ON_FILE


*&---------------------------------------------------------------------*
*&      Form  PRUEFUNG_BETRAG
*&---------------------------------------------------------------------*
*       prüft, ob der maximal zulässige Betrag gemäß                   *
*       Formatbeschreibung nicht überschritten wird                    *
*       checks if maximum amount is not exceeded
*----------------------------------------------------------------------*
*       -> p_length     Länge des Betragsfeldes auf dem Datenträger    *
*                       length of dme field for the amount             *
*       -> p_amount     zu prüfender Betrag                            *
*                       amount to be checked                           *
*----------------------------------------------------------------------*
FORM pruefung_betrag USING p_length p_amount.

  DATA: _max_amount(18) TYPE n,
        _length TYPE i,
        _rwbtr LIKE reguh-rwbtr,
        _amount LIKE reguh-rwbtr.

  _amount              = abs( p_amount ).
  _length              = p_length.
  _max_amount(_length) = _amount.
  _rwbtr               = _max_amount(_length).

  IF _rwbtr NE _amount.
    err_betrag-waers = reguh-waers.
    err_betrag-rwbtr = reguh-rwbtr.
    err_betrag-zbukr = reguh-zbukr.
    err_betrag-vblnr = reguh-vblnr.
    COLLECT err_betrag.
    REJECT.
  ENDIF.

ENDFORM.                               " PRUEFUNG_BETRAG

*&---------------------------------------------------------------------*
* FORM READ_SCB_INDICATOR                                              *
*&---------------------------------------------------------------------*
*   reads table T015L. If no entry was found, sy-subrc is NOT 0 and    *
*   the workarea T015L is cleared. In this case no error message is    *
*   send, the calling program has to react by itself.                  *
*----------------------------------------------------------------------*

FORM read_scb_indicator USING p_lzbkz LIKE t015l-lzbkz.

  IF p_lzbkz IS INITIAL.
    sy-subrc = 4.
    CLEAR t015l.
    EXIT.
  ENDIF.

  sy-subrc = 0.
  CHECK p_lzbkz NE t015l-lzbkz.

  READ TABLE tab_t015l INTO t015l WITH TABLE KEY lzbkz = p_lzbkz.

  IF sy-subrc NE 0.
    SELECT SINGLE * FROM t015l WHERE lzbkz = p_lzbkz.
    IF sy-subrc EQ 0.
      INSERT t015l INTO TABLE tab_t015l.
    ELSE.
      CLEAR t015l.
    ENDIF.
  ENDIF.

ENDFORM.                               "READ_LZBKZ


*&---------------------------------------------------------------------*
*&      Form  GET_VALUE_DATE
*&---------------------------------------------------------------------*
*       compute the value date if it is initial
*----------------------------------------------------------------------*
FORM get_value_date.

  CHECK reguh-valut IS INITIAL.
  reguh-valut = reguh-zaldt.
  CALL FUNCTION 'DET_VALUE_DATE_FOR_PAYMENT'
       EXPORTING
            i_bldat            = reguh-zaldt
            i_budat            = reguh-zaldt
            i_bukrs            = reguh-zbukr
            i_faedt            = reguh-zaldt
            i_hbkid            = reguh-hbkid
            i_hktid            = reguh-hktid
            i_vorgn            = ' '
*           I_WBGRU            = ' '
            i_zlsch            = reguh-rzawe
       IMPORTING
            valuta             = reguh-valut
       EXCEPTIONS
            cal_id_error       = 1
            not_found_in_t012a = 2
            not_found_in_t012c = 3
            error_in_t012c     = 4
            OTHERS             = 5.

  IF sy-subrc NE 0.
    SELECT * FROM t042v WHERE bukrs EQ reguh-zbukr
                          AND zlsch EQ reguh-rzawe
                          AND hbkid EQ reguh-hbkid
                          AND hktid EQ reguh-hktid
                          AND betrg GE reguh-rbetr.
      EXIT.
    ENDSELECT.
    IF sy-subrc EQ 0.
      reguh-valut = reguh-valut + t042v-anztg.
    ENDIF.
  ENDIF.

ENDFORM.                               " GET_VALUE_DATE
*&---------------------------------------------------------------------*
*&      Form  BERECHTIGUNG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM berechtigung.

  DATA lt_fimsg LIKE TABLE OF fimsg WITH HEADER LINE.
* Lese Fehlertabelle aus LD und konvertiere in FIMSG Format
  CALL FUNCTION 'FI_PYF_AUTHORITY_OUTPUT'
       TABLES
            t_fimsg    = lt_fimsg
            t_err_auth = err_auth.
  LOOP AT lt_fimsg INTO fimsg.
    AT FIRST.         " to emphasize the Type (authority) of error
      ADD 1 TO cnt_error.
    ENDAT.
    IF sy-batch EQ space.
      MESSAGE ID lt_fimsg-msgid TYPE lt_fimsg-msgty
                                NUMBER lt_fimsg-msgno
                                WITH lt_fimsg-msgv1 lt_fimsg-msgv2
                                     lt_fimsg-msgv3 lt_fimsg-msgv4.
    ENDIF.
    PERFORM message USING lt_fimsg-msgno.
  ENDLOOP.

ENDFORM.                               " BERECHTIGUNG
