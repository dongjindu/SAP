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
form init.


  loop at tab_selfields.
    assign (tab_selfields-field) to <selfield>.
    perform text(sapdbpyf) using tab_selfields-text <selfield>.
  endloop.


endform.                               "INIT



*----------------------------------------------------------------------*
* FORM F4_FORMULAR                                                     *
*----------------------------------------------------------------------*
* F4 für SAPscript-Formulare                                           *
* F4 for layout sets                                                   *
*----------------------------------------------------------------------*
form f4_formular using layout_set.


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
  data: hp_form_name like thead-tdform.
  call function 'DISPLAY_FORM_TREE_F4'
       exporting
            p_tree_name     = 'FI-2'
*                   P_DISPLAY_MODE  = ' '
*                   I_FORM_NAME     =
       importing
            p_form_name     = hp_form_name
*                   P_FORM_LANGUAGE =
       exceptions
*                   CANCELLED       = 1
*                   PARAMETER_ERROR = 2
*                   NOT_FOUND       = 3
            others          = 4
            .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  if sy-subrc eq 0 and hp_form_name ne space.
    layout_set = hp_form_name.
  endif.


endform.                               "F4 Formular



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
form datentraeger_selektieren tables vblnr
                              using renum laufd laufi xvorl.


  ranges up_vblnr for reguh-vblnr.
  data up_renum(10) type n.
  up_renum = renum.
  renum    = up_renum.
  call function 'GET_DOCUMENTS'
       exporting
            i_belege     = 'X'
            i_refno      = renum
            i_regut      = 'X'
       importing
            e_regut      = regut
       tables
            tab_belege   = tab_belege30a
       exceptions
            no_documents = 1
            no_regut     = 2
            wrong_number = 3.
  if sy-subrc eq 1.
    call function 'GET_DOCUMENTS'
         exporting
              i_belege   = space
              i_refno    = renum
              i_regut    = 'X'
         importing
              e_regut    = regut
         tables
              tab_belege = tab_belege30a.
    if sy-batch eq space.
      message e288 with renum regut-laufd regut-laufi.
    else.
      message s288 with renum regut-laufd regut-laufi.
      stop.
    endif.
  elseif sy-subrc ne 0.
    if sy-batch eq space.
      message e287 with renum.
    else.
      message s287 with renum.
      stop.
    endif.
  endif.
  laufi = regut-laufi.
  laufd = regut-laufd.
  xvorl = space.
  up_vblnr-option = 'EQ'.
  up_vblnr-sign   = 'I'.
  up_vblnr-high   = space.
  loop at tab_belege30a.
    up_vblnr-low  = tab_belege30a-belnr.
    append up_vblnr.
    tab_vblnr_renum       = tab_belege30a.
    tab_vblnr_renum-renum = renum.
    append tab_vblnr_renum.
  endloop.
  free tab_belege30a.
  vblnr[] = up_vblnr[].


endform.                               "DATENTRAEGER_SELEKTIEREN



*----------------------------------------------------------------------*
* FORM VORBEREITUNG                                                    *
*----------------------------------------------------------------------*
* Vorbereitung der Verarbeitung                                        *
* preparation                                                          *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form vorbereitung.

  if sy-binpt eq space.
    commit work.
  endif.

* Laufkennung (Applikation) setzen
* Fill application of run
  hlp_laufk = zw_laufi+5(1).

* Probedruck vorbereiten
* prepare test print
  field-symbols:
    <tabfeld>.

  data:
    up_tab        like dfies-tabname,
    up_dfies      like dfies occurs 0 with header line,
    up_feld(20)   type c,
    up_kreuz(132) type c.

  if par_anzp gt 0.

    clear up_kreuz with 'X'.

    do 5 times.

      case sy-index.
        when 1.
          up_tab  = 'FSABE'.
          up_feld = 'XXX_FSABE-'.
        when 2.
          up_tab  = 'REGUD'.
          up_feld = 'XXX_REGUD-'.
        when 3.
          up_tab  = 'REGUH'.
          up_feld = 'XXX_REGUH-'.
        when 4.
          up_tab  = 'REGUP'.
          up_feld = 'XXX_REGUP-'.
        when 5.
          up_tab  = 'SPELL'.
          up_feld = 'XXX_SPELL-'.
      endcase.

      call function 'DDIF_NAMETAB_GET'
           exporting
                tabname   = up_tab
           tables
                dfies_tab = up_dfies
           exceptions
                others    = 4.
      if sy-subrc ne 0.
        refresh up_dfies.
      endif.

      loop at up_dfies.
        up_feld+10 = up_dfies-fieldname.
        assign (up_feld) to <tabfeld>.
        case up_dfies-inttype.
          when 'C'.
            <tabfeld> = up_kreuz.
          when 'D'.
            <tabfeld> = '19000101'.
          when others.
            clear <tabfeld>.
        endcase.
      endloop.

    enddo.

    clear xxx_fsabe-salut.

  endif.

* Vorbereitung für die Berechtigungsprüfung ----------------------------
* prepare authorization checks -----------------------------------------
  if hlp_laufk ne '*'.
    if hlp_laufk ne 'P'.               "Prüfung für FI-Bestände
      if zw_xvorl eq space.            "checks for FI-data
        hlp_fbtch = 25.
      else.
        hlp_fbtch = 15.
      endif.
      clear flg_koart_auth.
      authority-check object 'F_REGU_KOA'
        id 'KOART' field 'K'
        id 'FBTCH' field hlp_fbtch.
      if sy-subrc eq 0.
        flg_koart_auth-k = 'X'.
      endif.
      authority-check object 'F_REGU_KOA'
        id 'KOART' field 'D'
        id 'FBTCH' field hlp_fbtch.
      if sy-subrc eq 0.
        flg_koart_auth-d = 'X'.
      endif.
      if hlp_laufk eq 'R'.
        authority-check object 'F_REGU_KOA'
          id 'KOART' field 'S'
          id 'FBTCH' field hlp_fbtch.
        if sy-subrc eq 0.
          flg_koart_auth-s = 'X'.
        endif.
      endif.
      if flg_koart_auth eq space.      "Keine Kontoarten-Berechtigung
        if sy-batch eq space.          "no account type authority
          message a066 with hlp_fbtch.
        else.
          message s066 with hlp_fbtch.
          message s094.
          stop.
        endif.
      endif.
    else.                              "Prüfung für HR-Bestände
      autha-repid = sy-repid.          "checks for HR-data
      call function 'HR_PROGRAM_CHECK_AUTHORIZATION'
           exporting
                repid = autha-repid
           importing
                subrc = hlp_subrc.
      if hlp_subrc ne 0.
        if sy-batch eq space.
          message a189.
        else.
          message s189.
          message s094.
          stop.
        endif.
      endif.
    endif.
  endif.

* Variantennamen merken ------------------------------------------------
* store name of report variant -----------------------------------------
  if par_vari eq space.
    par_vari = sy-slset.
  endif.

* Sofortdruckparameter setzen ------------------------------------------
* activate parameters for immediate printing ---------------------------
  if par_sofo ne space.
    par_sofw = 'X'.
    par_sofz = 'X'.
    par_sofa = 'X'.
    par_sofb = 'X'.
  endif.

* Vorbelegen von Feldern -----------------------------------------------
* fields with fix values -----------------------------------------------
  hlp_maxbetrag = 10000000000000.
  hlp_zeilen    = 0.

  clear txt_uline1 with '_'.
  clear txt_uline2 with '='.

* Länder mit separatem Bankcode ----------------------------------------
* Countries with separate bank-code ------------------------------------
  refresh tab_bankcode.
  tab_bankcode-high   = space.
  tab_bankcode-sign   = 'I'.
  tab_bankcode-option = 'EQ'.
  tab_bankcode-low    = 'A'.   append tab_bankcode.  "Österreich
  tab_bankcode-low    = 'CH'.  append tab_bankcode.  "Schweiz
  tab_bankcode-low    = 'CDN'. append tab_bankcode.  "Kanada
  tab_bankcode-low    = 'D'.   append tab_bankcode.  "Deutschland
  tab_bankcode-low    = 'GB'.  append tab_bankcode.  "Großbritannien
  tab_bankcode-low    = 'FL'.  append tab_bankcode.  "Liechtenstein
  tab_bankcode-low    = 'I'.   append tab_bankcode.  "Italien
  tab_bankcode-low    = 'IRL'. append tab_bankcode.  "Irland
  tab_bankcode-low    = 'IS'.  append tab_bankcode.  "Island
  tab_bankcode-low    = 'USA'. append tab_bankcode.         "USA

* Lesen allgemeiner Texte aus dem Textpool SAPDBPYF --------------------
* Read texts from textpool SAPDBPYF ------------------------------------
  perform text(sapdbpyf) using:
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
  if hlp_laufk ne '*'.                 "keine Prüfung bei Online-Druck
    select single * from reguv         "online print => no check
      where laufd eq zw_laufd
      and   laufi eq zw_laufi.
    if zw_xvorl eq space.
      if reguv-xecht ne 'X'.
        if sy-batch eq space.
          message a098 with zw_laufd zw_laufi.
        else.
          message s098 with zw_laufd zw_laufi.
          message s094.
          stop.
        endif.
      endif.
    else.
      if reguv-xvore ne 'X'.
        if sy-batch eq space.
          message a099 with zw_laufd zw_laufi.
        else.
          message s099 with zw_laufd zw_laufi.
          message s094.
          stop.
        endif.
      endif.
    endif.
  endif.

* logisches System des Mandanten lesen
* read logical system of client
  select single * from t000 where mandt eq sy-mandt.
  refresh tab_rfc.

* prüfen, ob IBAN-Funktionalität verfügbar
* check that IBAN function is available
*// 2011.08.11 ECC6 delete by kimyn ================== //*
*** call function 'CHECK_IBAN_ACTIVE'
***      exceptions
***           iban_not_active = 4.
*** if sy-subrc eq 0.
***   flg_iban = 1.
*** endif.


endform.                               "VORBEREITUNG



*----------------------------------------------------------------------*
* FORM PRUEFUNG                                                        *
*----------------------------------------------------------------------*
* Prüfung der selektierten Daten                                       *
* Checks of selected data                                              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form pruefung.


* Keine Ausnahmen behandeln
* no exceptions
  if reguh-vblnr eq space.
    reject.
  endif.

* Nachlesen der Buchungskreisdaten / des Geschäftsjahres
* read company code data / business year
  on change of reguh-zbukr.
    select single * from t001
      where bukrs eq reguh-zbukr.
  endon.
  on change of reguh-zaldt or t001-periv.
    call function 'DATE_TO_PERIOD_CONVERT'
         exporting
              i_date  = reguh-zaldt
              i_periv = t001-periv
         importing
              e_gjahr = regud-gjahr.
    xxx_reguh-zaldt = reguh-zaldt.
  endon.

* Berechtigungsprüfungen -----------------------------------------------
* authorization checks -------------------------------------------------
  if hlp_laufk na 'P*'.                "Prüfung für FI-Bestände
    authority-check object 'F_REGU_BUK'"checks for FI-data
      id 'BUKRS' field reguh-zbukr
      id 'FBTCH' field hlp_fbtch.
    if sy-subrc ne 0.
      err_auth-autob = 'F_REGU_BUK'.
      err_auth-field = 'BUKRS'.
      err_auth-value = reguh-zbukr.
      err_auth-actvt = hlp_fbtch.
      collect err_auth.
      reject.
    endif.
    err_auth-autob = 'F_REGU_KOA'.
    err_auth-field = 'KOART'.
    err_auth-value = space.
    err_auth-actvt = hlp_fbtch.
    if reguh-lifnr eq space and reguh-kunnr eq space.
      if flg_koart_auth-s eq space.
        err_auth-value = 'S'.
      endif.
    elseif reguh-lifnr ne space and reguh-kunnr ne space.
      if flg_koart_auth-k eq space and flg_koart_auth-d eq space.
        err_auth-value = 'D / K'.
      endif.
    elseif reguh-lifnr ne space.
      if flg_koart_auth-k eq space.
        err_auth-value = 'K'.
      endif.
    elseif flg_koart_auth-d eq space.
      err_auth-value = 'D'.
    endif.
    if err_auth-value ne space.
      collect err_auth.
      reject.
    endif.
  endif.

* Prüfung, ob Report für diesen Zahlweg zugelassen ist -----------------
* check that report is valid for this payment method -------------------
  if flg_avis eq 0.                    "all reports except RFFOAVIS
    if reguh-rzawe eq space.
      reject.
    endif.
    clear tab_t042z.
    read table tab_t042z with key land1 = t001-land1
                                  zlsch = reguh-rzawe.
    if sy-subrc ne 0.
      select single * from t042z
        where land1 eq t001-land1
        and   zlsch eq reguh-rzawe.
      if sy-subrc ne 0.
        if sy-batch eq space.
          message a350 with reguh-rzawe t001-land1.
        else.
          message s350 with reguh-rzawe t001-land1.
          message s094.
          stop.
        endif.
      endif.
      tab_t042z = t042z.
      if tab_t042z-progn eq sy-repid
        or par_begl eq 'D'.            "no check for RFFODTA0 / RFFOEDI0
        tab_t042z-xsele = 'X'.
      else.
        tab_t042z-xsele = space.
        err_t042z-land1 = tab_t042z-land1.
        err_t042z-zlsch = tab_t042z-zlsch.
        append err_t042z.
      endif.
      append tab_t042z.
      sort tab_t042z.
    endif.
    if tab_t042z-xsele eq space.
      reject.
    endif.
    t042z       = tab_t042z.
    regud-xeinz = tab_t042z-xeinz.
    hlp_xeuro   = tab_t042z-xeuro.
  else.                                "RFFOAVIS only
    if reguh-rzawe ne space or reguh-avisg eq 'V'.
      reject.
    endif.
  endif.

* Prüfung, ob Beleg bereits gebucht ist --------------------------------
* check that payment document is updated -------------------------------
  if par_belp eq 'X'
    and reguh-paygr+18(2) ne '$J'      "$J - restart of Japanese DME
    and hlp_laufk ne 'P'.              "not HR

    if reguh-pyord eq space.           "payment document
      select single * from bkpf
        where bukrs eq reguh-zbukr
        and   belnr eq reguh-vblnr
        and   gjahr eq regud-gjahr.
    else.                              "payment order
      clear bkpf.
      select single * from pyordh
        where pyord eq reguh-pyord.
    endif.

    if sy-subrc ne 0.
      move-corresponding reguh to err_nicht_verbucht.
      collect err_nicht_verbucht.
      reject.
    endif.
    if bkpf-stblg ne space.
      fimsg-msgv1 = reguh-zbukr.
      fimsg-msgv2 = reguh-vblnr.
      perform message using '381'.
      reject.
    endif.
  endif.

* Zahlungsbelegverprobung für Abrechnungsergebnisse
* payment document validation for payroll results
  if par_belp eq 'X' and reguh-dorigin eq 'HR-PY'.
    data up_doc1r type doc1r_fpm.
    data up_doc1t type doc1t_fpm.
    call function 'FI_REF_DOCUMENT_FILL'
         exporting
              im_pernr  = reguh-pernr
              im_seqnr  = reguh-seqnr
              im_btznr  = reguh-btznr
         importing
              ex_doc1r  = up_doc1r
              ex_doc1t  = up_doc1t.
    call function 'FI_REF_DOCUMENT_CHECK'
         exporting
              im_doc1r  = up_doc1r
              im_doc1t  = up_doc1t
              im_origin = reguh-dorigin
         exceptions
              not_found = 4.
    if sy-subrc ne 0.
      fimsg-msgid = sy-msgid.
      fimsg-msgv1 = sy-msgv1.
      fimsg-msgv2 = sy-msgv2.
      fimsg-msgv3 = sy-msgv3.
      fimsg-msgv4 = sy-msgv4.
      perform message using sy-msgno.
      reject.
    endif.
  endif.

* Prüfung, ob Beleg mit EDI versendet werden soll
* check whether document should be handled via EDI
  if zw_edisl eq space and reguh-edibn eq 'X'.
    move-corresponding reguh to err_edi.
    append err_edi.
    reject.
  endif.

* Abweichender Zahlungsempfänger ---------------------------------------
* alternative payee ----------------------------------------------------
  regud-xabwz   = space.
  if reguh-empfg(1)    eq '>' and      "abweichender Zahlungsempfänger
     reguh-empfg+11(2) ne '>F'.        "im Stamm, aber nicht Filiale
    regud-xabwz = 'X'.
  endif.
  if reguh-empfg(1)    ne '>' and      "abweichender Zahlungsempfänger
     reguh-empfg       ne space.       "im Beleg, nicht CPD-Konto
    if reguh-lifnr ne space.
      select single * from lfa1 where lifnr eq reguh-lifnr.
      if sy-subrc eq 0 and lfa1-xcpdk eq space.
        regud-xabwz = 'X'.
      endif.
    else.
      select single * from kna1 where kunnr eq reguh-kunnr.
      if sy-subrc eq 0 and kna1-xcpdk eq space.
        regud-xabwz = 'X'.
      endif.
    endif.
  endif.


endform.                               "PRUEFUNG



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
form extract_vorbereitung.


  perform vorzeichen_setzen using 'H'.
  perform sortierung using 'H'.

* Reparatur 2.0 / repair 2.0
  if reguh-ubnky eq space.
    if reguh-ubnkl ne space.
      reguh-ubnky = reguh-ubnkl.
    else.
      reguh-ubnky = reguh-ubknt.
    endif.
  endif.
  if reguh-zbnky eq space.
    if reguh-zbnkl ne space.
      reguh-zbnky = reguh-zbnkl.
    else.
      reguh-zbnky = reguh-zbnkn.
    endif.
  endif.

* Reparatur 3.0 / repair 3.0
  if reguh-ausfd lt reguh-zaldt.
    move reguh-zaldt to reguh-ausfd.
  endif.
  if reguh-absbu eq space.
    reguh-absbu = reguh-zbukr.
  endif.

* Reparatur 4.0 / repair 4.0
  if reguh-koinh eq space.
    reguh-koinh = reguh-znme1.
  endif.
  if reguh-pyord ne space.
    reguh-vblnr = reguh-pyord.
  endif.


endform.                               "Extract Vorbereitung



*----------------------------------------------------------------------*
* FORM EXTRACT                                                         *
*----------------------------------------------------------------------*
* Sortierfelder füllen, Daten extrahieren                              *
* HR über erfolgreiche Zahlung informieren                             *
*----------------------------------------------------------------------*
* fill sort fields and extract data                                    *
* send information about payment to HR (3rd party remittance)          *
*----------------------------------------------------------------------*
form extract.


  perform hr_remittance_acknowledgement.
  perform sortierung using 'P'.
  extract daten.
  flg_selektiert = 1.


endform.                               "Extract



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
form sortierung using satz.

  statics:
    st_svarh like t042e-svarh,
    st_svarp like t042e-svarp.

* Lesen der Tabelle T021M mit den Sortiervarianten
  on change of reguh-zbukr or reguh-rzawe.
    if not reguh-rzawe is initial.
      select single * from t042e
        where zbukr eq reguh-zbukr
          and zlsch eq reguh-rzawe.

      if t042e-svarh is initial.
        st_svarh = hlp_svarh.
      else.
        st_svarh = t042e-svarh.
      endif.

      if t042e-svarp is initial.
        st_svarp = hlp_svarp.
      else.
        st_svarp = t042e-svarp.
      endif.

    else.
      st_svarh = hlp_svarh.
      st_svarp = hlp_svarp.
    endif.
  endon.

  if hlp_t021m_h-srvar ne st_svarh.
    select single * from t021m into hlp_t021m_h
      where progn = 'RFFO*   '
        and anwnd = 'REGUH'
        and srvar = st_svarh.
    if sy-subrc ne 0.
      clear hlp_t021m_h.
    endif.
  endif.
  if hlp_t021m_p-srvar ne st_svarp.
    select single * from t021m into hlp_t021m_p
      where progn = 'RFFO*   '
        and anwnd = 'REGUP'
        and srvar = st_svarp.
    if sy-subrc ne 0.
      clear hlp_t021m_p.
    endif.
  endif.

* Füllen der Sortierfelder
  if satz eq 'H'.
    if not reguh-srtf2 is initial.
      hlp_sorth1 = reguh-srtf2(16).
      hlp_sorth2 = reguh-srtf2+16(16).
      hlp_sorth3 = reguh-srtf2+32(16).
    else.
      t021m = hlp_t021m_h.
      do 3 times.
        case sy-index.
          when 1.
            perform sortierung_assign using
             t021m-tnam1 t021m-feld1 t021m-offs1 t021m-leng1 hlp_sorth1.
          when 2.
            perform sortierung_assign using
             t021m-tnam2 t021m-feld2 t021m-offs2 t021m-leng2 hlp_sorth2.
          when 3.
            perform sortierung_assign using
             t021m-tnam3 t021m-feld3 t021m-offs3 t021m-leng3 hlp_sorth3.
        endcase.
      enddo.
    endif.
  else.
    t021m = hlp_t021m_p.
    do 3 times.
      case sy-index.
        when 1.
          perform sortierung_assign using
            t021m-tnam1 t021m-feld1 t021m-offs1 t021m-leng1 hlp_sortp1.
        when 2.
          perform sortierung_assign using
            t021m-tnam2 t021m-feld2 t021m-offs2 t021m-leng2 hlp_sortp2.
        when 3.
          perform sortierung_assign using
            t021m-tnam3 t021m-feld3 t021m-offs3 t021m-leng3 hlp_sortp3.
      endcase.
    enddo.
  endif.


endform.                               "SORTIERUNG



*----------------------------------------------------------------------*
* FORM SORTIERUNG_ASSIGN                                               *
*----------------------------------------------------------------------*
* Hilfsprogamm für die Sortierung                                      *
* help program for sort                                                *
*----------------------------------------------------------------------*
form sortierung_assign using tnam feld offs leng sort.


  field-symbols <feld>.                "Inhalt des Sortierfeldes
  data up_feld(21) type c.             "Name des Sortierfeldes aus T021M

  check not tnam is initial and not feld is initial.
  up_feld    = tnam.                   "REGUH oder REGUP
  up_feld+10 = '-'.
  up_feld+11 = feld.                   "UZAWE,ZPST2,XBLNR etc.
  condense up_feld no-gaps.

  if up_feld eq 'REGUSRT-HZPST'.       "PLZ Zahlungsempfänger
    if reguh-zpst2 ne space.
      up_feld = 'REGUH-ZPST2'.
    else.
      up_feld = 'REGUH-ZPSTL'.
    endif.
  endif.

  if up_feld eq 'REGUSRT-HPSTL'.                            "PLZ
    if reguh-pstl2 ne space.
      up_feld = 'REGUH-PSTL2'.
    else.
      up_feld = 'REGUH-PSTLZ'.
    endif.
  endif.

  assign table field (up_feld) to <feld>.
  clear sort.                          "Sortierfeld nur füllen, wenn
  check leng ne 0.                     "T021M-Eintrag nicht leer ist
  assign <feld>+offs(leng) to <feld>.
  sort = <feld>.


endform.                               "SORTIERUNG_ASSIGN



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
form vorzeichen_setzen using satz.


  if satz eq 'H'.                      "REGUH-Felder behandeln
                                       "maintain REGUH-data
    if regud-xeinz eq space.
      reguh-rbetr = - reguh-rbetr.
      reguh-rskon = - reguh-rskon.
      reguh-rwbtr = - reguh-rwbtr.
      reguh-rwskt = - reguh-rwskt.
    endif.


  else.                                "REGUP-Felder behandeln
                                       "maintain REGUP-data
    if ( regud-xeinz eq space and regup-shkzg eq 'H' ) or
       ( regud-xeinz ne space and regup-shkzg eq 'S' ).
      regud-dmbtr = regup-dmbtr.
      regud-wrbtr = regup-wrbtr.
      regud-sknto = regup-sknto.
      regud-wskto = regup-wskto.
      regud-qsteu = regup-qbshh.
      regud-wqste = regup-qbshb.
      if regup-xanet ne space.
        regud-dmbtr = regup-dmbtr + regup-mwsts.
        regud-wrbtr = regup-wrbtr + regup-wmwst.
      endif.
    else.
      regud-dmbtr = - regup-dmbtr.
      regud-wrbtr = - regup-wrbtr.
      regud-sknto = - regup-sknto.
      regud-wskto = - regup-wskto.
      regud-qsteu = - regup-qbshh.
      regud-wqste = - regup-qbshb.
      regup-qsshb = - regup-qsshb.
      if regup-xanet ne space.
        regud-dmbtr = - regup-dmbtr - regup-mwsts.
        regud-wrbtr = - regup-wrbtr - regup-wmwst.
      endif.
    endif.

  endif.


endform.                               "VORZEICHEN_SETZEN



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
form isocode_umsetzen using waers isocd.

  sy-subrc = 0.
  if tcurc-waers ne waers.
    clear tcurc.
    select single * from tcurc
      where waers eq waers.
  endif.
  if sy-subrc eq 0 and tcurc-isocd ne space.
    isocd = tcurc-isocd.
  else.                                "ISO-Code nicht gefunden
    isocd = waers.                     "ISO code not found
    err_tcurc-waers = waers.
    collect err_tcurc.
  endif.


endform.                               "ISOCODE_UMSETZEN



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
form buchungskreis_daten_lesen.


* Tabelle T001 lesen ---------------------------------------------------
* read table T001 ------------------------------------------------------
  clear t001.
  select single * from t001
    where bukrs eq reguh-zbukr.

  hlp_sprache = t001-spras.
  if hlp_sprache is initial.
    fimsg-msgv1 = reguh-zbukr.
    perform message using '242'.
  endif.

* Avisformular lesen ---------------------------------------------------
* read name of remittance advice form ----------------------------------
  clear t042b.
  select single * from t042b
    where zbukr eq reguh-zbukr.
  if sy-subrc ne 0.
    if sy-batch eq space.
      message a345 with reguh-zbukr.
    else.
      message s345 with reguh-zbukr.
      message s094.
      stop.
    endif.
  endif.
  if hlp_aforn ne space.
    t042b-aforn = hlp_aforn.
  endif.
  if t042b-aforn eq space and par_avis ne space.
    if sy-batch eq space.
      message a346 with reguh-zbukr.
    else.
      message s346 with reguh-zbukr.
      message s094.
      stop.
    endif.
  endif.

* Ausgabefeld für die Hauswährung füllen -------------------------------
* fill print field for local currency ----------------------------------
  if par_isoc eq 'X'.                  "ISO code
    perform isocode_umsetzen using t001-waers regud-hwaer.
  else.
    regud-hwaer = t001-waers.
  endif.

* Brieftexte bereitstellen ---------------------------------------------
* read names of standard texts -----------------------------------------
  clear t042t.
  select single * from t042t
    where bukrs eq reguh-zbukr.
  if sy-subrc ne 0.
    move-corresponding reguh to err_t042t.
    collect err_t042t.
  endif.
  move-corresponding t042t to regud.
  regud-txtko = t042t-txtko.
  regud-txtfu = t042t-txtfu.
  regud-txtun = t042t-txtun.
  regud-txtab = t042t-txtab.


endform.                               "BUCHUNGSKREIS_DATEN_LESEN



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
form zahlweg_daten_lesen.


* Textschlüssel für OCRA-Zeile bereitstellen ---------------------------
* read key in code line ------------------------------------------------
  clear tab_t042z.
  tab_t042z-land1 = t001-land1.
  tab_t042z-zlsch = reguh-rzawe.
  read table tab_t042z.
  t042z = tab_t042z.
  regud-otxsl = t042z-txtsl.

* Formulardaten (Name, maximale Postenanzahl, Austeller) lesen ---------
* read form data (form name, line items per form, issuer) --------------
  clear t042e.
  select single * from t042e
    where zbukr eq reguh-zbukr
    and   zlsch eq reguh-rzawe.
  if sy-subrc ne 0.
    if sy-batch eq space.
      message a347 with reguh-rzawe reguh-zbukr.
    else.
      message s347 with reguh-rzawe reguh-zbukr.
      message s094.
      stop.
    endif.
  endif.
  if not t042e-xsavi is initial.       "see note 365942
    t042e-xavis = 'X'.
  endif.
  if hlp_zforn ne space.               "Formular überschreiben, wenn als
    t042e-zforn = hlp_zforn.           "Parameter vorgegeben
  endif.                               "overwrite form name if wished
  if t042e-zforn eq space and par_zdru ne space.
    if sy-batch eq space.
      message a348 with reguh-rzawe reguh-zbukr.
    else.
      message s348 with reguh-rzawe reguh-zbukr.
      message s094.
      stop.
    endif.
  endif.
  if t042e-wforn eq space and flg_zettel eq 1 and
    ( par_xdta ne space or t042z-xswec ne space ).
    if sy-batch eq space.
      message a349 with reguh-rzawe reguh-zbukr.
    else.
      message s349 with reguh-rzawe reguh-zbukr.
      message s094.
      stop.
    endif.
  endif.
  regud-aust1 = t042e-aust1.
  regud-aust2 = t042e-aust2.
  regud-aust3 = t042e-aust3.
  regud-austo = t042e-austo.

* Sortierdaten lesen
* read sort data
  select single * from t021m into hlp_t021m_h
    where progn = 'RFFO*   '
      and anwnd = 'REGUH'
      and srvar = t042e-svarh.
  if sy-subrc ne 0.
    clear hlp_t021m_h.
  endif.
  select single * from t021m into hlp_t021m_p
    where progn = 'RFFO*   '
      and anwnd = 'REGUP'
      and srvar = t042e-svarp.
  if sy-subrc ne 0.
    clear hlp_t021m_p.
  endif.


endform.                               "ZAHLWEG_DATEN_LESEN



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
form hausbank_daten_lesen.


* Hausbank-Anschriftsdaten lesen ---------------------------------------
* read house bank address ----------------------------------------------
  clear bnka.
  select single * from bnka
    where banks eq reguh-ubnks
    and   bankl eq reguh-ubnky.
  regud-ubnka    = bnka-banka.
  regud-ubstr    = bnka-stras.
  regud-ubort    = bnka-ort01.
  regud-ubank    = bnka-banka.
  regud-ubank+61 = bnka-ort01.
  condense regud-ubank.
  regud-ubrch    = bnka-brnch.

* Bankleitzahl ohne Aufbereitungszeichen für OCRA-Zeile speichern ------
* store numerical bank number ------------------------------------------
  regud-obnkl = reguh-ubnkl.


endform.                               "HAUSBANK_DATEN_LESEN



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
form hausbank_konto_lesen.


* Hausbank-Konto lesen
* read account at house bank
  if   t012k-bukrs ne reguh-zbukr
    or t012k-hbkid ne reguh-hbkid
    or t012k-hktid ne reguh-hktid.

    if    tab_t012k-bukrs eq reguh-zbukr
      and tab_t012k-hbkid eq reguh-hbkid
      and tab_t012k-hktid eq reguh-hktid.

      t012k    = tab_t012k.
      sy-subrc = 0.

    else.

      read table tab_t012k with key bukrs = reguh-zbukr
                                    hbkid = reguh-hbkid
                                    hktid = reguh-hktid.
      if sy-subrc = 0.
        t012k = tab_t012k.
      else.
        select single * from t012k
          where bukrs eq reguh-zbukr
            and hbkid eq reguh-hbkid
            and hktid eq reguh-hktid.
        if sy-subrc = 0.
          tab_t012k = t012k.
          append tab_t012k.
        else.
          if sy-batch eq space.
            message a095 with 'T012K' reguh-zbukr reguh-hbkid
                                      reguh-hktid.
          else.
            message s095 with 'T012K' reguh-zbukr reguh-hbkid
                                      reguh-hktid.
            message s094.
            stop.
          endif.
        endif.
      endif.
    endif.
  endif.


endform.                               "HAUSBANK_KONTO_LESEN



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
form fill_itcpo using p_tddest     like itcpo-tddest
                      p_tddataset  like itcpo-tddataset
                      p_tdimmed    like itcpo-tdimmed
                      p_tdautority like itcpo-tdautority.

  clear itcpo.
  itcpo-tdpageslct  = space.           "all pages
  itcpo-tdnewid     = 'X'.             "create new spool dataset
  itcpo-tdcopies    = 1.               "one copy
  itcpo-tddest      = p_tddest.        "name of printer
  itcpo-tdpreview   = space.           "no preview
  itcpo-tdcover     = space.           "no cover page
  itcpo-tddataset   = p_tddataset.     "dataset name
  if zw_xvorl eq space.
    itcpo-tdsuffix1 = p_tddest.        "name or printer
  else.
    itcpo-tdsuffix1 = 'TEST'.          "test run
  endif.
  itcpo-tdsuffix2   = par_vari.        "name of report variant
  itcpo-tdimmed     = p_tdimmed.       "print immediately?
  itcpo-tddelete    = space.           "do not delete after print
  itcpo-tdtitle     = t042z-text1.     "title of pop-up-window
  itcpo-tdcovtitle  = t042z-text1.     "title of print-cover
  itcpo-tdautority  = p_tdautority.    "print authority
  itcpo-tdarmod     = 1.               "print only

endform.                               " FILL_ITCPO



*----------------------------------------------------------------------*
* FORM FILL_ITCPO_FROM_ITCPP                                           *
*----------------------------------------------------------------------*
* Füllen der Struktur ITCPO aus der Resultatstruktur ITCPP             *
* fill structure ITCPO from the result structure ITCPP                 *
*----------------------------------------------------------------------*
form fill_itcpo_from_itcpp.

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

endform.                               " FILL_ITCPO_FROM_ITCPP



*----------------------------------------------------------------------*
* FORM MODIFY_ITCPO                                                    *
*----------------------------------------------------------------------*
* Modify ITCPO and set archive parameters for optical archiving        *
* Routine can only be used for payment medium on paper !!!             *
* Test prints are not allowed in case of optical archiving !!!         *
*----------------------------------------------------------------------*
form modify_itcpo.


  data up_repid like sy-repid.
  up_repid = sy-repid.
  clear:
    toa_dara,
    arc_params.
  call function 'OPEN_FI_PERFORM_00002060_P'
       exporting
            i_reguh          = reguh
            i_gjahr          = regud-gjahr
            i_repid          = up_repid
            i_aforn          = t042e-zforn
       changing
            c_itcpo          = itcpo
            c_archive_index  = toa_dara
            c_archive_params = arc_params.
  if itcpo-tdarmod gt 1 and par_anzp ne 0."#EC PORTABLE
    par_anzp = 0.
    perform message using '384'.
  endif.


endform.                               " MODIFY_ITCPO



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
form print_on using p_bukrs        like t001-bukrs
                    p_cover_text   type any
                    p_destination  like rfpdo-fordprib
                    p_immediately  like tlsep-sofor
                    p_list_dataset like tlsep-listn.


  data: begin of up_param,
          cpage like tlsep-cpage,
          nllid like tlsep-nllid,
          keeps like tlsep-keeps,
          layot like tlsep-layot,
        end of up_param.

* TLSEP lesen ---------------------------------------------------------*
* read TLSEP  ---------------------------------------------------------*
  clear sy-spono.
  select single * from  tlsep
         where  domai       = 'BUKRS'
         and    werte       = p_bukrs.

  if sy-subrc = 0.
    move-corresponding tlsep to up_param.
    if up_param-layot is initial.
      up_param-layot = 'X_65_132'.
    endif.
  else.
    up_param-cpage = ' '.
    up_param-nllid = 'X'.
    up_param-keeps = 'X'.
    up_param-layot = 'X_65_132'.
  endif.

  if p_immediately eq 'X'   and        "immediate printing requested and
     sy-batch eq space      and        "NO batch-processing   BUT:
     p_destination eq space or         "printer not specified
     par_begl eq 'D'        and
     sy-tcode eq 'FDTA'.
    new-page
      print on
      line-size                132
      list name                par_vari
      list authority           hlp_auth
      destination              p_destination
      cover text               p_cover_text
      list dataset             p_list_dataset
      immediately              p_immediately
      new list identification  up_param-nllid
      keep in spool            up_param-keeps
      layout                   up_param-layot
      sap cover page           up_param-cpage.
  else.
    new-page
      print on
      line-size                132
      list name                par_vari
      list authority           hlp_auth
      destination              p_destination
      cover text               p_cover_text
      list dataset             p_list_dataset
      immediately              p_immediately
      new list identification  up_param-nllid
      keep in spool            up_param-keeps
      layout                   up_param-layot
      sap cover page           up_param-cpage
      no dialog.
  endif.


endform.                               "PRINT_ON



*----------------------------------------------------------------------*
* FORM PRINT_OFF                                                       *
*----------------------------------------------------------------------*
* new-page print off mit passender Parametrisierung aufrufen           *
* call new-page print off with appropiate parametrization              *
*----------------------------------------------------------------------*
* P_DATASET     - Name des Spool-Datasets      name of spool dataset   *
* P_NAME        - Name der Ausgabeliste        name of output list     *
*----------------------------------------------------------------------*
form print_off using p_dataset  like tab_ausgabe-dataset
                     p_name     type any.


  new-page print off.
  check sy-spono ne 0.
  clear tab_ausgabe.
  tab_ausgabe-name    = p_name.
  tab_ausgabe-dataset = p_dataset.
  tab_ausgabe-spoolnr = sy-spono.
  collect tab_ausgabe.


endform.                               "PRINT_OFF



*----------------------------------------------------------------------*
* FORM EMPFBANK_DATEN_LESEN                                            *
*----------------------------------------------------------------------*
* Empfängerbank-Anschriftsdaten lesen                                  *
* read address of payee                                                *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form empfbank_daten_lesen.


* Empfängerbank-Anschriftsdaten lesen ----------------------------------
* read address of payee ------------------------------------------------
  clear bnka.
  select single * from bnka
    where banks eq reguh-zbnks
    and   bankl eq reguh-zbnky.
  regud-zbnka    = bnka-banka.
  regud-zbstr    = bnka-stras.
  regud-zbort    = bnka-ort01.
  regud-zbank    = bnka-banka.
  regud-zbank+61 = bnka-ort01.
  condense regud-zbank.
  regud-zbrch    = bnka-brnch.

* Bankleitzahl ohne Aufbereitungszeichen für Begleitliste und DTA ------
* store numerical bank number ------------------------------------------
  hlp_zbnkl      = reguh-zbnkl.
  regud-ozbkl    = reguh-zbnkl.


endform.                               "EMPFBANK_DATEN_LESEN



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
form zahlungs_daten_lesen.


* Leeren der Einzelposten-Tabelle - Verwendung: Avis, User-Exit --------
* refresh table with single items - use: payment advice, user-exit -----
  refresh tab_regup.
  clear tab_regup.
  perform zahlungs_daten_lesen_hlp.


endform.                               "ZAHLUNGS_DATEN_LESEN



*----------------------------------------------------------------------*
* FORM ZAHLUNGS_DATEN_LESEN_HLP                                        *
*----------------------------------------------------------------------*
* Hilfsprogamm für das Lesen der Zahlungsdaten                         *
* help program for read of payment data                                *
*----------------------------------------------------------------------*
form zahlungs_daten_lesen_hlp.


  statics up_sprache like hlp_sprache.

  statics: begin of up_uiban occurs 0,
             zbukr like reguh-zbukr,
             hbkid like reguh-hbkid,
             hktid like reguh-hktid,
             uiban like regud-uiban,
           end of up_uiban.
  data:    up_bkref like reguh-bkref.

  data begin of up_t001.
          include structure t001.
  data end of up_t001.

  data: up_plort like szad_field-addr_dc,
        up_pfstr like szad_field-addr_dc.

* Sprache bestimmen, in der das Formular gelesen werden soll
  if par_espr eq 'X'.
    hlp_sprache = reguh-zspra.         "Empfängersprache
  else.
    hlp_sprache = t001-spras.          "Buchungskreissprache
  endif.

* Ausgabeformat des Zahlungsempfängers einstellen (Datum, Betrag)
  set country reguh-zland.
  if sy-subrc ne 0.
    set country space.
  endif.

* Zahlweg für den Formularabschluß
* payment method for summary
  regud-zwels = reguh-rzawe.

* Nummern mit führenden Nullen für OCRA-Zeile speichern ----------------
* store numbers with leading zeros for code line -----------------------
  regud-ovbln = reguh-vblnr.
  regud-ozbkt = reguh-zbnkn.

* IBAN -----------------------------------------------------------------
  if flg_iban eq 1.

*   IBAN des Zahlungsempfängers ermitteln
*   determine IBAN of payee
    call function 'READ_IBAN_FROM_DB'
      exporting
        i_banks           = reguh-zbnks
        i_bankl           = reguh-zbnky
        i_bankn           = reguh-zbnkn
        i_bkont           = reguh-zbkon
        i_bkref           = reguh-bkref
      importing
        e_iban            = regud-ziban
        e_iban_valid_from = hlp_date.
    if hlp_date gt sy-datlo.
      clear regud-ziban.
    endif.

*   IBAN für unserer Hausbankkonto
*   IBAN for our house bank account
    read table up_uiban with key zbukr = reguh-zbukr
                                 hbkid = reguh-hbkid
                                 hktid = reguh-hktid.
    if sy-subrc ne 0.
      clear up_uiban.
      up_uiban-zbukr = reguh-zbukr.
      up_uiban-hbkid = reguh-hbkid.
      up_uiban-hktid = reguh-hktid.
      select single * from t012k where bukrs eq reguh-zbukr
                                 and   hbkid eq reguh-hbkid
                                 and   hktid eq reguh-hktid.
      if sy-subrc eq 0.
        up_bkref = t012k-refzl.
      endif.
      call function 'READ_IBAN_FROM_DB'
        exporting
          i_banks           = reguh-ubnks
          i_bankl           = reguh-ubnky
          i_bankn           = reguh-ubknt
          i_bkont           = reguh-ubkon
          i_bkref           = up_bkref
        importing
          e_iban            = up_uiban-uiban
          e_iban_valid_from = hlp_date.
      if hlp_date gt sy-datlo.
        clear up_uiban-uiban.
      endif.
      append up_uiban.
    endif.
    regud-uiban = up_uiban-uiban.

  endif.

* Textschlüssel bei HR-Beständen modifizieren --------------------------
* modify key in code line for HR data ----------------------------------
  hrxblnr = regup-xblnr.
  if hlp_laufk eq 'P'                  "bei HR-Beständen ist spezieller
    and t042z-xschk eq space           "Textschlüssel zu verwenden
    and hrxblnr-txtsl ne space.        "(falls gefüllt, leer aus PU11)
    t042z-txtsl = hrxblnr-txtsl.       "use special text key for HR data
  endif.
  regud-otxsl = t042z-txtsl.

* Ausgabefeld für die Belegwährung füllen ------------------------------
* fill print field for currency ----------------------------------------
  if par_isoc eq 'X'.                  "ISO Code
    perform isocode_umsetzen using reguh-waers regud-waers.
  else.
    regud-waers = reguh-waers.
  endif.

* Ländername des Zahlungsempfängers lesen ------------------------------
* read country name of payee -------------------------------------------
  clear t005t.
  select single * from t005t
    where spras eq hlp_sprache
    and   land1 eq reguh-land1.
  regud-landx = t005t-landx.

  if reguh-land1 ne reguh-zland.
    clear t005t.
    select single * from t005t
      where spras eq hlp_sprache
      and   land1 eq reguh-zland.
  endif.
  regud-zlndx = t005t-landx.

* Bezeichnung der Region lesen -----------------------------------------
* read name of region --------------------------------------------------
  clear t005u.
  select single * from t005u
    where spras eq hlp_sprache
    and   land1 eq reguh-zland
    and   bland eq reguh-zregi.
  regud-zregx = t005u-bezei.

* Ausgabefelder für Postleitzahl und Ort füllen ------------------------
* fill print field for postal code and city ----------------------------
  if reguh-name1 ne space.
    if reguh-adrnr is initial.
      clear adrs.
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
      call function 'ADDRESS_INTO_PRINTFORM'
           exporting
                adrswa_in  = adrs
           importing
                adrswa_out = adrs.
      regud-plort = adrs-lined.
      regud-pfstr = adrs-lined0.
    else.
      call function 'ADDRESS_INTO_PRINTFORM'
           exporting
                address_type           = '1'
                sender_country         = t001-land1
                address_number         = reguh-adrnr
                number_of_lines        = '4'
           importing
                address_data_carrier   = up_plort
                address_data_carrier_0 = up_pfstr.
      regud-plort = up_plort.
      regud-pfstr = up_pfstr.
    endif.
  else.
    regud-plort = space.
    regud-pfstr = space.
  endif.

  if reguh-zadnr is initial.
    clear adrs.
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
    call function 'ADDRESS_INTO_PRINTFORM'
         exporting
              adrswa_in  = adrs
         importing
              adrswa_out = adrs.
    regud-zplor = adrs-lined.
    regud-zpfst = adrs-lined0.
  else.
    call function 'ADDRESS_INTO_PRINTFORM'
         exporting
              address_type           = '1'
              sender_country         = t001-land1
              address_number         = reguh-zadnr
              number_of_lines        = '4'
         importing
              address_data_carrier   = up_plort
              address_data_carrier_0 = up_pfstr.
    regud-zplor = up_plort.
    regud-zpfst = up_pfstr.
  endif.

* Name mit Schutzsternen -----------------------------------------------
* name with protective asterisks ---------------------------------------
  clear regud-znm1s.
  translate regud-znm1s using ' *'.
  sy-fdpos = strlen( reguh-znme1 ).
  if sy-fdpos gt 0.
    regud-znm1s(sy-fdpos) = reguh-znme1.
  endif.
  clear regud-znm2s.
  translate regud-znm2s using ' *'.
  sy-fdpos = strlen( reguh-znme2 ).
  if sy-fdpos gt 0.
    regud-znm2s(sy-fdpos) = reguh-znme2.
  endif.

* Buchhaltungssachbearbeiter lesen -------------------------------------
* read accounting clerk data -------------------------------------------
  if t001s-bukrs ne reguh-absbu or t001s-busab ne reguh-busab
                                or hlp_sprache ne up_sprache.
    up_sprache = hlp_sprache.
    clear: fsabe, t001s, regud-ubusa.
    call function 'CORRESPONDENCE_DATA_BUSAB'
         exporting
              i_bukrs = reguh-absbu
              i_busab = reguh-busab
              i_langu = hlp_sprache
         importing
              e_t001s = t001s
              e_fsabe = fsabe
         exceptions
              others  = 01.
    if sy-subrc eq 0.
      perform sachbearbeiter_kurzinfo using fsabe regud-ubusa.
    endif.
    if regud-ubusa eq space.
      regud-ubusa = t001s-sname.
    endif.
  endif.

* Absendenden Buchungskreis lesen --------------------------------------
* read sending company code --------------------------------------------
  regud-abstx = space.
  regud-absor = space.
  if reguh-absbu ne reguh-zbukr.
    select single * from t001 into up_t001
      where bukrs eq reguh-absbu.
    regud-abstx = up_t001-butxt.
    regud-absor = up_t001-ort01.
  endif.

* Buchungsdatum des Zahlungsbelegs (in Worten) -------------------------
* posting date of payment document (in words) --------------------------
  select single * from t015m
    where spras eq hlp_sprache
    and   monum eq reguh-zaldt+4(2).
  if sy-subrc eq 0.
    regud-zaliw(2)   = reguh-zaldt+6(2).
    regud-zaliw+2(2) = '. '.
    regud-zaliw+4    = t015m-monam.
  else.
    clear regud-zaliw.
  endif.

* Wechselausstellungsdatum (in Worten) ---------------------------------
* bill of exchange issue date (in words) -------------------------------
  select single * from t015m
    where spras eq hlp_sprache
    and   monum eq reguh-wdate+4(2).
  if sy-subrc eq 0.
    regud-wdaiw(2)   = reguh-wdate+6(2).
    regud-wdaiw+2(2) = '. '.
    regud-wdaiw+4    = t015m-monam.
  else.
    clear regud-wdaiw.
  endif.

* Wechselfälligkeitsdatum (in Worten) ----------------------------------
* due date of the bill of exchange (in words) --------------------------
  select single * from t015m
    where spras eq hlp_sprache
    and   monum eq reguh-wefae+4(2).
  if sy-subrc eq 0.
    regud-wefiw(2)   = reguh-wefae+6(2).
    regud-wefiw+2(2) = '. '.
    regud-wefiw+4    = t015m-monam.
  else.
    clear regud-wefiw.
  endif.


endform.                               "ZAHLUNGS_DATEN_LESEN_HLP



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
form sachbearbeiter_kurzinfo using xfsabe structure fsabe textfeld.


  data:
    up_actln like sy-fdpos,           "actually calculated string length
    up_maxln like sy-fdpos,            "maximal string length (textfeld)
    up_lname like fsabe-lname,
    up_salut like fsabe-salut,
    up_telf1(40) type c.

  describe field textfeld length up_maxln.
  up_lname     = xfsabe-lname.
  up_salut     = xfsabe-salut.
  concatenate xfsabe-telf1 xfsabe-tel_exten1 into up_telf1.

  condense up_telf1 no-gaps.
  up_actln     = strlen( up_salut ) +
                 strlen( up_lname ) +
                 strlen( up_telf1 ) + 2.
  if up_actln gt up_maxln.
    up_actln   = up_actln - strlen( up_salut ) - 1.
    clear up_salut.
    if up_actln gt up_maxln.
      up_actln = strlen( up_lname ) - up_actln + up_maxln.
      if up_actln gt 0.
        up_lname+up_actln = space.
      else.
        clear up_lname.
      endif.
    endif.
  endif.
  txt_zeile    = up_salut.
  txt_zeile+16 = up_lname.
  txt_zeile+52 = up_telf1.
  condense txt_zeile.
  textfeld     = txt_zeile.


endform.                               "SACHBEARBEITER_KURZINFO



*----------------------------------------------------------------------*
* FORM HR_REMITTANCE_ACKNOWLEDGEMENT                                   *
*----------------------------------------------------------------------*
* Zahlungen an Dritte (Third Party Remittance) an das HR zurückmelden  *
* report payments concerning 3rd parties back to HR                    *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
form hr_remittance_acknowledgement.


* Prüfen ob Rückmeldung notwendig
* check that payment is 3rd party remittance
  hrxblnr = regup-xblnr.
  check:
    zw_xvorl      eq space,
    zw_laufi+5(1) ne 'P',
    hrxblnr-txtsl eq 'HR',
    hrxblnr-txerg eq 'GRN' and hrxblnr-xhrfo eq 'X' or
    hrxblnr-txerg eq space and hrxblnr-xhrfo eq space,
    hrxblnr-remsn ne 0.

  select single * from bkpf where bukrs eq regup-bukrs
                            and   belnr eq regup-belnr
                            and   gjahr eq regup-gjahr.
  if bkpf-awsys eq t000-logsys or bkpf-awsys is initial.
    call function 'RP_REMITTANCE_ACKNOWLEDGEMENT'
         exporting
              laufd  = regup-laufd
              laufi  = regup-laufi
              bukrs  = regup-bukrs
              lifnr  = regup-lifnr
              xblnr  = regup-xblnr
         exceptions
              others = 0.
  else.
    call function 'LOG_SYSTEM_GET_RFC_DESTINATION'
         exporting
              logical_system  = bkpf-awsys
         importing
              rfc_destination = tab_rfc-dest
         exceptions
              others          = 4.
    if sy-subrc ne 0.
      message id sy-msgid type 'A' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      tab_rfc-bukrs = regup-bukrs.
      tab_rfc-belnr = regup-belnr.
      tab_rfc-gjahr = regup-gjahr.
      tab_rfc-lifnr = regup-lifnr.
      tab_rfc-xblnr = regup-xblnr.
      tab_rfc-zbukr = reguh-zbukr.
      tab_rfc-vblnr = reguh-vblnr.
      tab_rfc-awsys = bkpf-awsys.
      append tab_rfc.
    endif.
  endif.


endform.                               "HR_REMITTANCE_ACKNOWLEDGEMENT



*----------------------------------------------------------------------*
* FORM HR_REMITTANCE_ACKNOWLEDGE_RFC                                   *
*----------------------------------------------------------------------*
* Zahlungen an Dritte (Third Party Remittance) via RFC zurückmelden    *
* report payments concerning 3rd parties back to HR (RFC)              *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
form hr_remittance_acknowledge_rfc.

  loop at tab_rfc.
    call function 'RP_REMITTANCE_ACKNOWLEDGEMENT'
         destination
              tab_rfc-dest
         exporting
              laufd                 = zw_laufd
              laufi                 = zw_laufi
              bukrs                 = tab_rfc-bukrs
              lifnr                 = tab_rfc-lifnr
              xblnr                 = tab_rfc-xblnr
         exceptions
              communication_failure = 4 message txt_zeile
              system_failure        = 4 message txt_zeile
              others                = 0.
    if sy-subrc ne 0.
      fimsg-msgv1   = tab_rfc-awsys.
      fimsg-msgv2   = tab_rfc-dest.
      fimsg-msgv3   = tab_rfc-bukrs.
      fimsg-msgv4   = tab_rfc-belnr.
      perform message using 378.
      fimsg-msgv1   = txt_zeile.
      fimsg-msgv2   = txt_zeile+50.
      fimsg-msgv3   = txt_zeile+100.
      perform message using 379.
      fimsg-msgv1   = tab_rfc-zbukr.
      fimsg-msgv2   = tab_rfc-vblnr.
      perform message using 380.
      reject.
    endif.
  endloop.

endform.                               "HR_REMITTANCE_ACKNOWLEDGE_RFC



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
form hr_formular_lesen.


  field-symbols:
    <feld>.
  data:
    up_nr(1)    type n,
    up_feld(11) type c,
    up_pform    like pc408 occurs 9 with header line.
  refresh: pform, up_pform.

  if hrxblnr-txtsl eq 'HR' and hrxblnr-txerg eq 'GRN'.
    if regup-sgtxt cn '* '.
      while regup-sgtxt(1) ca '* '.
        shift regup-sgtxt.
      endwhile.
    endif.
    read table tab_rfc with key bukrs = regup-bukrs
                                belnr = regup-belnr
                                gjahr = regup-gjahr.
    if sy-subrc ne 0.
      call function 'RP_IMPORT_GARNISHMENT_LIST'
           exporting
                sgtxt  = regup-sgtxt
           tables
                pform  = pform
                pform1 = up_pform
           exceptions
                others = 8.
    else.
      call function 'RP_IMPORT_GARNISHMENT_LIST'
           destination
                tab_rfc-dest
           exporting
                sgtxt                 = regup-sgtxt
           tables
                pform                 = pform
                pform1                = up_pform
           exceptions
                communication_failure = 4 message txt_zeile
                system_failure        = 4 message txt_zeile
                others                = 8.
      if sy-subrc eq 4.
        sy-msgid = 'F0'.
        sy-msgno = 379.
        sy-msgv1 = txt_zeile.
        sy-msgv2 = txt_zeile+50.
        sy-msgv3 = txt_zeile+100.
        sy-msgv4 = space.
      endif.
    endif.
  else.
    call function 'RP_IMPORT_PAY_STATEMENT'
         exporting
              laufd  = reguh-laufd
              laufi  = reguh-laufi
              pernr  = reguh-pernr
              seqnr  = reguh-seqnr
         tables
              pform  = pform
         exceptions
              others = 8.
  endif.
  if sy-subrc ne 0.
    fimsg-msgid = sy-msgid.
    fimsg-msgv1 = sy-msgv1.
    fimsg-msgv2 = sy-msgv2.
    fimsg-msgv3 = sy-msgv3.
    fimsg-msgv4 = sy-msgv4.
    perform message using sy-msgno.
    fimsg-msgv1 = reguh-zbukr.
    fimsg-msgv2 = reguh-hbkid.
    fimsg-msgv3 = reguh-hktid.
    fimsg-msgv4 = regud-chect.
    perform message using '286'.
  else.
    loop at pform where ltype ne f__ltype-txt.
      delete pform.
    endloop.
    if hrxblnr-txtsl eq 'HR' and hrxblnr-txerg eq 'GRN'.
      loop at up_pform where ltype ne f__ltype-txt.
        delete up_pform.
      endloop.
      do 9 times.
        up_nr         = sy-index.
        up_feld       = 'REGUD-TEXT '.
        up_feld+10(1) = up_nr.
        assign (up_feld) to <feld>.
        clear <feld>.
        read table up_pform index sy-index.
        if sy-subrc eq 0.
          <feld> = up_pform-linda.
        endif.
      enddo.
    endif.
  endif.


endform.                               "HR_FORMULAR_LESEN


*----------------------------------------------------------------------*
* FORM WEISUNGSSCHLUESSEL_LESEN                                        *
*----------------------------------------------------------------------*
* Lesen des aktuellen Weisungsschlüssels zu den REGUH-Daten            *
* Read instruction key for current REGUH-contents                      *
* Release 3.0: key fields are country of bank and payment method       *
*----------------------------------------------------------------------*
* No USING - parameters                                                *
*----------------------------------------------------------------------*
form weisungsschluessel_lesen.

  data: up_dtaws like reguh-dtaws.
  statics: up_t015w like t015w occurs 0 with header line.

  clear t015w.                         "Clear old values

  if not reguh-dtaws is initial.
    up_dtaws = reguh-dtaws.
  else.
    if reguh-zbukr ne t012d-bukrs or reguh-hbkid ne t012d-hbkid.
      select single * from t012d where bukrs eq reguh-zbukr
                                 and   hbkid eq reguh-hbkid.
      if sy-subrc ne 0.
        clear t012d.
      endif.
    endif.
    if not t012d-dtaws is initial.
      up_dtaws = t012d-dtaws.
    else.
      move-corresponding reguh to err_t012d.
      collect err_t012d.
      exit.
    endif.
  endif.

  read table up_t015w with key banks = reguh-ubnks
                               zlsch = reguh-rzawe
                               dtaws = up_dtaws
                          into t015w.
  check sy-subrc ne 0.
  select single * from t015w
          where banks eq reguh-ubnks
            and zlsch eq reguh-rzawe
            and dtaws eq up_dtaws.
  if sy-subrc ne 0.                    "specified entry not found
    select single * from t015w
            where banks eq space
              and zlsch eq space
              and dtaws eq up_dtaws.
    if sy-subrc ne 0.                  "general (=old) entry not found
      err_kein_dtaws-banks = reguh-ubnks.
      err_kein_dtaws-zlsch = reguh-rzawe.
      err_kein_dtaws-dtaws = up_dtaws.
      collect err_kein_dtaws.          "Store error
    else.
      t015w-banks = reguh-ubnks.
      t015w-zlsch = reguh-rzawe.
      append t015w to up_t015w.
    endif.
  else.
    append t015w to up_t015w.
  endif.

endform.                               "WEISUNGSSCHLUESSEL_LESEN


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
form weisungsschluessel_umsetzen using p_dtwsc like t015w1-dtwsc
                                       p_dtwsf like t015w1-dtwsf
                                       p_dtwsx like t015w1-dtwsx
                                       p_text1 type any
                                       p_text2 type any.
  data up_i015w1_par like i015w1_par.

  move-corresponding reguh to up_i015w1_par.

  call function 'FI_PAYMENT_INSTRUCTION_CONVERT'
       exporting
            i_dtwsc      = p_dtwsc
            i_dtwsf      = p_dtwsf
            i_i015w1_par = up_i015w1_par
            i_bnka       = bnka
            i_dtzus      = t015w-dtzus
       importing
            e_code       = p_text1
            e_addinfo    = p_text2
       changing
            c_dtwsx      = p_dtwsx.

endform.                               " WEISUNGSSCHLUESSEL_UMSETZEN


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
form get_clearing_code using p_land   like reguh-zbnks
                             p_soco   like reguh-zbnkl
                             p_clco   type c.

  perform laender_lesen using p_land.

  call function 'GET_BANKCODE'
       exporting
            i_banks  = p_land
            i_bankl  = p_soco
       importing
            e_clcode = p_clco
       exceptions
            others   = 4.

endform.                               " GET_CLEARING_CODE


*----------------------------------------------------------------------*
* FORM ADRESSE_LESEN                                                   *
*----------------------------------------------------------------------*
* Lesen einer Customizingadresse (z.B. Buchungskreisadresse)           *
* Read customizing address (e.g. address of company code)              *
*----------------------------------------------------------------------*
* ADRNR - address number                                               *
*----------------------------------------------------------------------*
form adresse_lesen using value(adrnr).

  perform addr_get using 'CA01' adrnr.

endform.                               " ADRESSE_LESEN


*----------------------------------------------------------------------*
* FORM BANKADRESSE_LESEN                                               *
*----------------------------------------------------------------------*
* Lesen einer Bankadresse                                              *
* Read bank address                                                    *
*----------------------------------------------------------------------*
* ADRNR - address number                                               *
*----------------------------------------------------------------------*
form bankadresse_lesen using value(adrnr).

  perform addr_get using 'CA02' adrnr.

endform.                               " BANKADRESSE_LESEN


*----------------------------------------------------------------------*
* FORM GET_ADDR                                                        *
*----------------------------------------------------------------------*
* Lesen einer Adresse                                                  *
* Read address                                                         *
*----------------------------------------------------------------------*
* ADRGR - address group                                                *
* ADRNR - address number                                               *
*----------------------------------------------------------------------*
form addr_get using adrgr adrnr.

  check adrnr ne sadr-adrnr.
  clear addr1_sel.
  addr1_sel-addrnumber = adrnr.
  call function 'ADDR_GET'
       exporting
            address_selection = addr1_sel
            address_group     = adrgr
       importing
            address_value     = addr1_val
            sadr              = sadr
       exceptions
            others            = 4.     "SADR40A
  if sy-subrc ne 0.
    clear sadr.
  endif.

endform.                               " GET_ADDR


*----------------------------------------------------------------------*
* FORM LAENDER_LESEN                                                   *
*----------------------------------------------------------------------*
* Lesen der Länderdaten zum Land LAND1                                 *
* Read country data for LAND1                                          *
*----------------------------------------------------------------------*
* LAND1 - countrycode                                                  *
*----------------------------------------------------------------------*
form laender_lesen using value(land1).

  clear sy-subrc.

  if tab_t005-land1 = land1.           "check last value
    t005 = tab_t005.
  else.
    read table tab_t005 with key land1 = land1."check internal table
    if sy-subrc eq 0.                  "entry was found
      t005 = tab_t005.
    else.
      select single * from t005
             where land1 = land1.
      if sy-subrc ne 0.                "no entry found in T005
        clear t005.
      else.                            "entry found-> store temporarily
        tab_t005 = t005.               "fill header, too!
        append tab_t005.
      endif.
    endif.
  endif.

  if t005-intca is initial.
    err_t005-land1 = land1.
    collect err_t005.
    sy-subrc = 4.
  endif.

endform.                               "LAENDER_LESEN


*----------------------------------------------------------------------*
* FORM ZAHLWEG_EINFUEGEN                                               *
*----------------------------------------------------------------------*
* Übergebenen Zahlweg in die übergebene Leiste übernehmen              *
* insert payment method into array of methods                          *
*----------------------------------------------------------------------*
* RZAWE  - payment method to insert                                    *
* LIST   - current list of payment methods                             *
*----------------------------------------------------------------------*
form zahlweg_einfuegen using value(rzawe) list.

  data up_list like regud-zwels.

  check list na rzawe.
  up_list = list.
  shift up_list.
  up_list+9(1) = rzawe.
  list = up_list.

endform.                               "ZAHLWEG_EINFUEGEN


*----------------------------------------------------------------------*
* FORM SUMMENFELDER_INITIALISIEREN                                     *
*----------------------------------------------------------------------*
* Summenfelder initialisieren                                          *
* initialize total amount fields                                       *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form summenfelder_initialisieren.


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
  write:
    reguh-rbetr to regud-snets currency t001-waers,
    reguh-rwbtr to regud-swnes currency reguh-waers.
  translate:
    regud-snets using ' *',
    regud-swnes using ' *'.
  perform ziffern_in_worten.


endform.                               "SUMMENFELDER_INITIALISIEREN



*----------------------------------------------------------------------*
* FORM EINZELPOSTENFELDER_FUELLEN                                      *
*----------------------------------------------------------------------*
* Ausgabefelder füllen                                                 *
* fill single item fields                                              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form einzelpostenfelder_fuellen.

* Segmenttext ohne * aufbereiten ---------------------------------------
* segment text without leading * ---------------------------------------
  if regup-sgtxt cn '* '.
    while regup-sgtxt(1) ca '* '.
      shift regup-sgtxt.
    endwhile.
  endif.

* Einzelposteninfo merken - Verwendung: Avis, User-Exit
* store item-information  - use: payment advice, user-exit
  if tab_regup ne regup.               "nicht im Loop über TAB_REGUP
    tab_regup = regup.                 "not in LOOP AT TAB_REGUP
    append tab_regup.
  endif.

* Text zum Buchungsschlüssel lesen
* read text of posting key
  select single * from tbslt
    where spras eq hlp_sprache
      and bschl eq regup-bschl
      and umskz eq regup-umskz.
  regud-bschx = tbslt-ltext.

* Betragsfelder (Abzüge und Netto) füllen ------------------------------
* fill single item amount fields (deductions and net) ------------------
  perform vorzeichen_setzen using 'P'.
  regud-abzug = regud-sknto + regud-qsteu.
  regud-wabzg = regud-wskto + regud-wqste.
  regud-netto = regud-dmbtr - regud-abzug.
  regud-wnett = regud-wrbtr - regud-wabzg.
  write:
    regud-netto to regud-netts currency t001-waers,
    regud-wnett to regud-wnets currency reguh-waers.
  translate:
    regud-netts using ' *',
    regud-wnets using ' *'.


endform.                               "EINZELPOSTENFELDER_FUELLEN



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
form summenfelder_fuellen.


  add regud-dmbtr to regud-sdmbt.
  add regud-wrbtr to regud-swrbt.
  add regud-sknto to regud-ssknt.
  add regup-skfbt to regud-sskfb.
  add regud-wskto to regud-swskt.
  add regud-qsteu to regud-sqste.
  add regud-wqste to regud-swqst.
  add regup-qsshb to regud-sqssh.
  regud-sabzg = regud-ssknt + regud-sqste.
  regud-swabz = regud-swskt + regud-swqst.
  regud-snett = regud-sdmbt - regud-sabzg.
  regud-swnet = regud-swrbt - regud-swabz.
  write:
    regud-snett to regud-snets currency t001-waers,
    regud-swnet to regud-swnes currency reguh-waers.
  translate:
    regud-snets using ' *',
    regud-swnes using ' *'.


endform.                               "SUMMENFELDER_FUELLEN



*----------------------------------------------------------------------*
* FORM DTA_GLOBALS_ERSETZEN                                            *
*----------------------------------------------------------------------*
* Ersetzt die Globals in Verwendungszweckzeilen im DTA                 *
* replace globals in the file extension fields (DME)                   *
*----------------------------------------------------------------------*
* TEXTFELD - Feld, in dem Globals ersetzt werden sollen                *
*            Field with globals to be replaced                         *
*----------------------------------------------------------------------*
form dta_globals_ersetzen using textfeld.

  data: up_textfeld(512) type c.
  up_textfeld = textfeld.

  write regup-bldat to hlp_datum dd/mm/yy.
  if up_textfeld cs '&XBLNR'.
    sy-fdpos = strlen( regup-xblnr ).
    if sy-fdpos ge 14.
      write regup-bldat to hlp_datum ddmmyy.
    endif.
  endif.
  replace   '&BLDAT' with hlp_datum   into up_textfeld.
  replace   '&EIKTO' with reguh-eikto into up_textfeld.
  replace   '&GJAHR' with regud-gjahr into up_textfeld.
  if reguh-lifnr ne space.
    replace '&KTNRA' with reguh-lifnr into up_textfeld.
  else.
    replace '&KTNRA' with reguh-kunnr into up_textfeld.
  endif.
  write regud-netto to hlp_betrag currency regud-hwaer.
  replace   '&NETTO' with hlp_betrag  into up_textfeld.
  replace   '&PERNR' with reguh-pernr into up_textfeld.
  write regup-zbdxp to hlp_betrag currency '3'.
  write '%' to hlp_betrag+15.
  replace   '&PSATZ' with hlp_betrag  into up_textfeld.
  replace   '&SEQNR' with reguh-seqnr into up_textfeld.
  replace   '&SGTXT' with regup-sgtxt into up_textfeld.
  if hlp_laufk eq 'M'.
    data up_opbel(12) type c.
    up_opbel(2) = reguh-seqnr(2).
    up_opbel+2  = reguh-vblnr.
    replace '&VBLNR' with up_opbel    into up_textfeld.
  else.
    replace '&VBLNR' with reguh-vblnr into up_textfeld.
  endif.
  replace   '&VERTN' with regup-vertn into up_textfeld.
  write regud-wrbtr  to hlp_betrag currency regud-waers.
  replace   '&WBRUT' with hlp_betrag  into up_textfeld.
  write regud-wnett to hlp_betrag currency regud-waers.
  replace   '&WNETT' with hlp_betrag  into up_textfeld.
  replace   '&WAERS' with reguh-waers into up_textfeld.
  replace   '&BELNR' with regup-belnr into up_textfeld.
  replace   '&XBLNR' with regup-xblnr into up_textfeld.
  write reguh-zaldt to hlp_datum dd/mm/yy.
  replace   '&ZALDT' with hlp_datum   into up_textfeld.
  replace   '&ZBUKR' with reguh-zbukr into up_textfeld.
  condense up_textfeld.
  perform dta_text_aufbereiten using up_textfeld.
  textfeld = up_textfeld.


endform.                               "DTA_GLOBALS_ERSETZEN



*----------------------------------------------------------------------*
* FORM DTA_TEXT_AUFBEREITEN                                            *
*----------------------------------------------------------------------*
* Textfelder im DTA müssen Upper Case und ohne Umlaute sein            *
* text fields in DME have to be upper case and without 'äöü' etc.      *
*----------------------------------------------------------------------*
* TEXTFELD - enthält den zu bearbeitenden Text                         *
*            text that is to be checked                                *
*----------------------------------------------------------------------*
form dta_text_aufbereiten using textfeld.

  if flg_no_replace eq space.
    call function 'SCP_REPLACE_STRANGE_CHARS'
         exporting
              intext  = textfeld
         importing
              outtext = textfeld
         exceptions
              others  = 01.
  endif.
  translate textfeld to upper case.

endform.                               "DTA_TEXT_AUFBEREITEN


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
form daten_sichern.


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


endform.                               "DATEN_SICHERN



*----------------------------------------------------------------------*
* FORM DATEN_ZURUECK                                                   *
*----------------------------------------------------------------------*
* Zurückladen der REGUD-,REGUH-, REGUP-Informationen                   *
* data back after test print                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form daten_zurueck.


  itcpo = sic_itcpo.
  move-corresponding:
    sic_fsabe to fsabe,
    sic_regud to regud,
    sic_reguh to reguh,
    sic_regup to regup.
  clear spell.


endform.                               "DATEN_ZURUECK



*----------------------------------------------------------------------*
* FORM ZIFFERN_IN_WORTEN                                               *
*----------------------------------------------------------------------*
* Umsetzten des Betrages und der Ziffern in Worte                      *
* transform numbers and digits in words                                *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form ziffern_in_worten.


  clear spell.
  call function 'SPELL_AMOUNT'
       exporting
            language  = hlp_sprache
            currency  = reguh-waers
            amount    = regud-swnes
            filler    = hlp_filler
       importing
            in_words  = spell
       exceptions
            not_found = 1
            too_large = 2.

*
  if sy-subrc = 0.
    z_00 = '00'.
    if not spell-decimal is initial.
    write spell-decimal(2) to z_00.
    endif.
  endif.

  if sy-subrc eq 1.
*   in Tabelle T015Z fehlt ein Eintrag
*   entry in table T015Z not found
    clear err_t015z.
    err_t015z-spras = sy-msgv1.
    err_t015z-einh  = sy-msgv2.
    err_t015z-ziff  = sy-msgv3.
    collect err_t015z.

    if hlp_sprache ne 'E'.
*     Letzter Versuch mit Sprache 'E' (besser als nichts)
*     Last trial with language 'E' (better than nothing)
      call function 'SPELL_AMOUNT'
           exporting
                language = 'E'
                currency = reguh-waers
                amount   = regud-swnes
                filler   = hlp_filler
           importing
                in_words = spell
           exceptions
                others   = 1.
    endif.
  endif.

  if sy-subrc eq 2 or spell-number ge hlp_maxbetrag.
*   Betrag ist zum Umsetzen zu groß
*   amount too large for transformation
    move-corresponding reguh to err_in_worten.
    collect err_in_worten.
    clear spell+403.                   "nur SPELL-DIGnn
  endif.


endform.                               "ZIFFERN_IN_WORTEN



*----------------------------------------------------------------------*
* FORM DATUM_IN_DDMMYY                                                 *
*----------------------------------------------------------------------*
* Konvertierung des Datumfelds DATUM in das Format DDMMYY              *
* Diese Konvertierung geschieht unabhängig von den Benutzerfestwerten. *
*----------------------------------------------------------------------*
* Convert date-field (ccyymmdd) to the format ddmmyy.                  *
* This conversion does not take the user defaults into consideration.  *
*----------------------------------------------------------------------*
form datum_in_ddmmyy using datum    type d
                           ddmmyy type any.


  data: up_str(6).

  up_str   = datum+6.                                       "Day
  up_str+2 = datum+4.                  "Month
  up_str+4 = datum+2.                  "Year
  ddmmyy = up_str.


endform.                               "DATUM_IN_DDMMYY



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
form abbruch_durch_ueberlauf.


  if sy-batch eq space.
    message a090 with reguh-rzawe reguh-zbukr.
  else.
    message s090 with reguh-rzawe reguh-zbukr.
    message s091 with reguh-rzawe reguh-zbukr.
    message s092 with reguh-rzawe reguh-zbukr.
    message s093 with reguh-rzawe reguh-zbukr.
    perform information.
    message s094.
    stop.
  endif.


endform.                               "ABBRUCH_DURCH_UEBERLAUF



*----------------------------------------------------------------------*
* FORM FEHLERMELDUNGEN                                                 *
*----------------------------------------------------------------------*
* Ausgabe von Fehlermeldungen                                          *
* error messages                                                       *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form fehlermeldungen.


* Notlösung: Form Fehlermeldung ist die einzige gemeinsame Unterroutine
* aller RFFO-Programme zum Zeitpunkt End-Of-Selection. Daher wird hier
* der Baustein RP_REMITTANCE_ACKNOWLEDGEMENT gerufen, da zum Get-Zeit-
* punkt ein Laufzeitfehler wegen des RFC auftritt.
  perform hr_remittance_acknowledge_rfc.

  set language sy-langu.
  clear fimsg.

* Fehlende Berechtigungen ----------------------------------------------
* Authority check errors -----------------------------------------------

  perform berechtigung.

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
  sort err_t042z.
  loop at err_t042z.

    at first.
      add 1 to cnt_error.
    endat.

    at new land1.
      clear hlp_zwels.
    endat.

    write err_t042z-zlsch to hlp_zwels+sy-tabix.

    at end of land1.
      condense hlp_zwels no-gaps.
      if sy-batch eq space.
        message i282 with hlp_zwels err_t042z-land1 sy-repid.
      endif.
      fimsg-msgv1 = hlp_zwels.
      fimsg-msgv2 = err_t042z-land1.
      fimsg-msgv3 = sy-repid.
      perform message using '282'.
    endat.

  endloop.


* nichts selektiert ----------------------------------------------------
* no data selected -----------------------------------------------------
  if flg_selektiert eq 0.
    message s073 with syst-repid.
  endif.


* nicht gefundene Elemente und Fenster ---------------------------------
* elements and windows not found ---------------------------------------
  sort err_element.
  loop at err_element.

    at first.
      add 1 to cnt_error.
    endat.

    if err_element-elemt ne space.
      fimsg-msgv1 = err_element-fname.
      fimsg-msgv2 = err_element-fenst.
      fimsg-msgv3 = err_element-elemt.
      fimsg-msgv4 = err_element-text.
      perform message using '251'.
    else.
      fimsg-msgv1 = err_element-fname.
      fimsg-msgv2 = err_element-fenst.
      perform message using '252'.
    endif.

    at last.
      perform message using '253'.
    endat.

  endloop.


* nicht gedruckte Fremdwährungsschecks ---------------------------------
* checks in foreign currencies not printed -----------------------------
  sort err_fw_scheck.
  loop at err_fw_scheck.

    at first.
      add 1 to cnt_error.
    endat.

    at new fname.
      fimsg-msgv1 = err_fw_scheck-fname.
      perform message using '254'.
      fimsg-msgv1 = err_fw_scheck-fname.
      perform message using '255'.
      perform message using '256'.
    endat.

    fimsg-msgv1 = err_fw_scheck-zbukr.
    fimsg-msgv2 = err_fw_scheck-vblnr.
    perform message using '257'.

  endloop.


* nicht in Worte umgesetzter Betrag ------------------------------------
* amount in words was not possible -------------------------------------
  sort err_in_worten.
  loop at err_in_worten.

    at first.
      add 1 to cnt_error.
      perform message using '258'.
      perform message using '259'.
      perform message using '256'.
    endat.

    fimsg-msgv1 = err_in_worten-zbukr.
    fimsg-msgv2 = err_in_worten-vblnr.
    perform message using '257'.

    at last.
      perform message using '260'.
    endat.

  endloop.


* nicht auf DTA ausgegebene Auslandsüberweisungen ----------------------
* foreign payment via DME not possible ---------------------------------
  sort err_kein_dta.
  loop at err_kein_dta.

    at new error.
      add 1 to cnt_error.
      fimsg-msgv1 = err_kein_dta-error.
      case err_kein_dta-error.
        when 1.
          perform message using '261'.
        when 2.
          perform message using '262'.
        when 3.
          perform message using '263'.
        when 4.
          perform message using '264'.
        when 5.
          perform message using '265'.
        when 6.
          perform message using '266'.
        when 7.
          perform message using '267'.
        when 8.
          perform message using '268'.
        when 9.
          perform message using '269'.
        when 10.
          perform message using '270'.
      endcase.
      fimsg-msgv1 = err_kein_dta-error.
      perform message using '271'.
      perform message using '256'.
    endat.

    fimsg-msgv1 = err_kein_dta-zbukr.
    fimsg-msgv2 = err_kein_dta-vblnr.
    perform message using '257'.

  endloop.


* nicht gefundene Weisungsschlüssel ------------------------------------
* missing instruction key ----------------------------------------------
  sort err_kein_dtaws.
  loop at err_kein_dtaws.

    at first.
      add 1 to cnt_error.
      fimsg-msgv1 = err_kein_dtaws-banks.
      fimsg-msgv2 = err_kein_dtaws-zlsch.
      fimsg-msgv3 = err_kein_dtaws-dtaws.
      perform message using '291'.
    endat.

    fimsg-msgv1 = err_kein_dtaws-banks.
    fimsg-msgv2 = err_kein_dtaws-zlsch.
    fimsg-msgv3 = err_kein_dtaws-dtaws.
    perform message using '292'.

    at last.
      perform message using '293'.
    endat.

  endloop.


* nicht verbuchte Belege (und daher nicht gedruckte Formulare) ---------
* payment documents not updated (therefore no form printed) ------------
  sort err_nicht_verbucht.
  loop at err_nicht_verbucht.

    at first.
      add 1 to cnt_error.
      perform message using '272'.
      perform message using '273'.
      perform message using '256'.
    endat.

    fimsg-msgv1   = err_nicht_verbucht-zbukr.
    if err_nicht_verbucht-pyord eq space.
      fimsg-msgv2 = err_nicht_verbucht-vblnr.
    else.
      fimsg-msgv2 = err_nicht_verbucht-pyord.
    endif.
    perform message using '257'.

    at last.
      perform message using '274'.
    endat.

  endloop.


* EDI Versendefehler ---------------------------------------------------
* error in EDI ---------------------------------------------------------
  sort err_edi.
  read table err_edi with key edibn = 'E'.
  if sy-subrc eq 0.

    add 1 to cnt_error.
    perform message using '357'.
    perform message using '358'.

    loop at err_edi where edibn eq 'E'.
      fimsg-msgv1 = err_edi-zbukr.
      fimsg-msgv2 = err_edi-vblnr.
      fimsg-msgv3 = err_edi-rzawe.
      perform message using '257'.
    endloop.

    perform message using '359'.

  endif.


* zuerst mit EDI versuchen ---------------------------------------------
* try EDI first --------------------------------------------------------
  read table err_edi with key edibn = 'X'.
  if sy-subrc eq 0.

    add 1 to cnt_error.
    perform message using '360'.
    perform message using '256'.

    loop at err_edi where edibn eq 'X'.
      fimsg-msgv1 = err_edi-zbukr.
      fimsg-msgv2 = err_edi-vblnr.
      perform message using '257'.
    endloop.

  endif.


* Einträge in T005 falsch oder unvollständig ---------------------------
* T005-entries missing or not correct ----------------------------------
  sort err_t005.
  loop at err_t005.

    at first.
      add 1 to cnt_error.
    endat.

    fimsg-msgv1 = err_t005-land1.
    perform message using '241'.

    at last.
      perform message using '276'.
    endat.

  endloop.


* nicht gefundene Einträge in T012D ------------------------------------
* entries not found in T012D -------------------------------------------
  sort err_t012d.
  loop at err_t012d.

    at first.
      add 1 to cnt_error.
    endat.

    fimsg-msgv1 = err_t012d-zbukr.
    fimsg-msgv2 = err_t012d-hbkid.
    perform message using '275'.

    at last.
      perform message using '276'.
    endat.

  endloop.


* nicht gefundene Einträge in T015Z ------------------------------------
* entries not found in T015Z -------------------------------------------
  sort err_t015z.
  loop at err_t015z.

    at first.
      add 1 to cnt_error.
      perform message using '277'.
    endat.

    fimsg-msgv1 = err_t015z-spras.
    fimsg-msgv2 = err_t015z-einh.
    fimsg-msgv3 = err_t015z-ziff.
    perform message using '257'.

    at last.
      perform message using '278'.
    endat.

  endloop.


* fehlerhafte Einträge in T042E ----------------------------------------
* wrong entries in T042E -----------------------------------------------
  sort err_t042e.
  loop at err_t042e.

    at first.
      add 1 to cnt_error.
    endat.

    fimsg-msgv1 = err_t042e-rzawe.
    fimsg-msgv2 = err_t042e-zbukr.
    perform message using '283'.

  endloop.


* nicht gefundene Einträge in T042T ------------------------------------
* entries not found in T042T -------------------------------------------
  sort err_t042t.
  loop at err_t042t.

    at first.
      add 1 to cnt_error.
    endat.

    fimsg-msgv1 = err_t042t-zbukr.
    perform message using '279'.

    at last.
      perform message using '280'.
    endat.

  endloop.


* nicht in ISO-Code umgesetzte Währungsschlüssel -----------------------
* ISO code for currency not found --------------------------------------
  sort err_tcurc.
  loop at err_tcurc.

    at first.
      add 1 to cnt_error.
    endat.

    fimsg-msgv1 = err_tcurc-waers.
    perform message using '281'.

  endloop.


* Zahlungen mit zu großen Beträgen--------------------------------------
* payments with too high amounts----------------------------------------
  sort err_betrag.
  loop at err_betrag.
    at first.
      add 1 to cnt_error.
    endat.

    fimsg-msgv1 = err_betrag-waers.
    fimsg-msgv2 = err_betrag-rwbtr.
    fimsg-msgv3 = err_betrag-zbukr.
    fimsg-msgv4 = err_betrag-vblnr.
    perform message using '382'.
  endloop.


* Ausgabe des Fehlerprotokolls -----------------------------------------
* Output of error log --------------------------------------------------
  call function 'FI_MESSAGE_CHECK'
       exceptions
            no_message = 4.
  check sy-subrc eq 0.
  if sy-batch ne space.
    call function 'FI_MESSAGE_GET'
         tables
              t_fimsg = tab_fimsg.
    loop at tab_fimsg.
      at new msort.
        message s257 with space space space space.
      endat.
      fimsg = tab_fimsg.
      message id fimsg-msgid  type 'S'     number fimsg-msgno
         with fimsg-msgv1     fimsg-msgv2  fimsg-msgv3  fimsg-msgv4.
    endloop.
  elseif flg_selektiert eq 0.
    add 1 to cnt_error.
    fimsg-msgv1 = syst-repid.
    perform message using '073'.
  endif.

  clear hlp_auth.      " fehlermeldungen ohne Berechtigungsschutz
  perform print_on using ' ' text_003 par_prib par_sofb 'LISTFS'.

  format color 6 intensified.
  write text_003.
  format reset.
  skip 2.
  call function 'FI_MESSAGE_PRINT'
       exporting
            i_xskip = 'X'.

  perform print_off using 'LISTFS' text_003.
  if sy-spono ne 0.
    tab_ausgabe-error = 'X'.
    modify tab_ausgabe index sy-tabix.
  endif.

endform.                               "FEHLERMELDUNGEN



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
form message using msgno.


  if fimsg-msort eq space.
    fimsg-msort = cnt_error.
  endif.
  if fimsg-msgid eq space.
    fimsg-msgid = 'F0'.
  endif.
  fimsg-msgno   = msgno.
  fimsg-msgty   = 'S'.
  call function 'FI_MESSAGE_COLLECT'
       exporting
            i_fimsg = fimsg.
  clear fimsg.


endform.                               "MESSAGE



*----------------------------------------------------------------------*
* FORM INFORMATION                                                     *
*----------------------------------------------------------------------*
* Ausgabe von Informationen über die erzeugten Spoolnummern            *
* information about generated spool datasets                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form information.

                                       "Bei bedingtem Loop über interne
  data:                                "Tabellen wird der Zeitpunkt AT
    up_titel(1) type n value 0,        "FIRST nicht durchlaufen!
    up_uline(1) type n value 0,        "LOOP durchlaufen ?
    up_rqident  like tsp01-rqident.

  sort tab_ausgabe.

* Erster Loop für Sofortdruck
  loop at tab_ausgabe where immed ne space.
    check tab_ausgabe-spoolnr ne 0.
    up_rqident = tab_ausgabe-spoolnr.
    call function 'RSPO_OUTPUT_SPOOL_REQUEST'
         exporting
              spool_request_id = up_rqident
         exceptions
              others           = 0.
  endloop.

  import flg_local from memory id 'MFCHKFN0'.
  check:                               "keine Info bei Sofortdruck
    par_sofo eq space,                 "und bei Neudruck eines Schecks
    sy-subrc ne 0.                     "no info when print immediately
                                       "and when re-print of a check
  if sy-batch eq space.
    new-page no-title line-size 81.
  endif.

* Zweiter Loop zur Ausgabe der Files
  clear tab_ausgabe.
  loop at tab_ausgabe where renum ne space.
    up_uline = 1.
    if up_titel eq 0.
      if sy-batch eq space.
        format color 1 intensified.
        flg_local = space.
        write:
            / sy-uline.
        hide flg_local.
        write:
            / sy-vline no-gap,
         (79) text_092,
           81 sy-vline.
        hide flg_local.
        write:
            / sy-uline.
        hide flg_local.
        format color 1 intensified off.
        write:
            / sy-vline no-gap,
         (79) text_093,
        40(1) sy-vline,
           81 sy-vline.
        hide flg_local.
        write:
            / sy-uline.
        hide flg_local.
      else.
        message s065.
        write:
          text_092 to txt_zeile.
        message s065 with txt_zeile.
        write:
          text_093 to txt_zeile,
          '/'      to txt_zeile+37(1).
        condense txt_zeile.
        message s065 with txt_zeile.
        message s064.
      endif.
      up_titel = 1.
    endif.
    if sy-batch eq space.
      format color 2 intensified off.
      flg_local = 'F'.
      format hotspot on.
      write:
          / sy-vline no-gap,
            tab_ausgabe-name,
         40 sy-vline no-gap,
            tab_ausgabe-filename(40),
         81 sy-vline.
      format hotspot off.
      hide: flg_local, tab_ausgabe.
    else.
      write:
        tab_ausgabe-name to txt_zeile,
        '/'              to txt_zeile+37(1).
      condense txt_zeile.
      message s065 with txt_zeile tab_ausgabe-filename.
    endif.
    delete tab_ausgabe.
  endloop.

* Dritter Loop zur Ausgabe der Spool-Dateien
  clear tab_ausgabe.
  loop at tab_ausgabe.
    up_uline = 1.
    at first.
      if sy-batch eq space.
        format color 1 intensified.
        flg_local = space.
        write:
            / sy-uline.
        hide flg_local.
        write:
            / sy-vline no-gap,
         (79) text_090,
           81 sy-vline,
              sy-uline.
        hide flg_local.
        format color 1 intensified off.
        write:
            / sy-vline no-gap,
         (79) text_091,
        40(1) sy-vline no-gap,
        55(1) sy-vline no-gap,
           81 sy-vline.
        hide flg_local.
        write:
            / sy-uline.
        hide flg_local.
      else.
        message s065.
        write:
          text_090 to txt_zeile.
        message s065 with txt_zeile.
        write:
          text_091 to txt_zeile,
          '/'      to txt_zeile+37(1),
          '/'      to txt_zeile+52(1).
        condense txt_zeile.
        message s065 with txt_zeile.
        message s064.
      endif.
    endat.
    if sy-batch eq space.
      if tab_ausgabe-error eq 'X'.
        format color 6 intensified.
      else.
        format color 2 intensified off.
      endif.
      if tab_ausgabe-error eq 'X'.
        flg_local = 'E'.
      else.
        flg_local = 'S'.
      endif.
      format hotspot on.
      write:
          / sy-vline no-gap,
            tab_ausgabe-name,
         40 sy-vline no-gap,
            tab_ausgabe-dataset,
         55 sy-vline no-gap,
            tab_ausgabe-spoolnr,
         81 sy-vline.
      format hotspot off.
      hide: flg_local, tab_ausgabe.
    else.
      write:
        tab_ausgabe-name     to txt_zeile,
        '/'                  to txt_zeile+37(1),
        tab_ausgabe-dataset  to txt_zeile+40,
        '/'                  to txt_zeile+52(1),
        tab_ausgabe-spoolnr  to txt_zeile+55.
      condense txt_zeile.
      message s065 with txt_zeile(50) txt_zeile+50.
    endif.
  endloop.

  if sy-batch eq space and up_uline ne 0.
    flg_local = space.
    write:
      / sy-uline.
    hide flg_local.
  endif.


endform.                               "INFORMATION



*----------------------------------------------------------------------*
* AT LINE-SELECTION                                                    *
*----------------------------------------------------------------------*
* Auswahl einer Zeile der Information                                  *
* Verzweigen in die DTA-Verwaltung oder Druckverwaltung                *
*----------------------------------------------------------------------*
at line-selection.

  type-pools sp01r.
  data up_list type sp01r_id_list with header line.
  data begin of up_bdc occurs 9.
          include structure bdcdata.
  data end of up_bdc.
  data up_fdta      like tstc-tcode value 'FDTA'.
  data up_laufd(10) type c.
  data up_meldung   like shkontext-meldung.
  data up_titel     like shkontext-titel.

  if sy-lsind eq 1.

    case flg_local.
      when space.                      "keine gültige Auswahl

      when 'E'.                        "Fehlerliste
        format color 6 intensified.
        uline at (102).
        write:
          /     sy-vline no-gap,
          (100) text_003 no-gap,
                sy-vline.
        format color 2.
        uline at (102).
        refresh tab_fimsg.
        call function 'FI_MESSAGE_GET'
             tables
                  t_fimsg = tab_fimsg.
        loop at tab_fimsg.
          fimsg = tab_fimsg.
          call function 'K_MESSAGE_TRANSFORM'
               exporting
                    par_langu = sy-langu
                    par_msgid = fimsg-msgid
                    par_msgno = fimsg-msgno
                    par_msgty = fimsg-msgty
                    par_msgv1 = fimsg-msgv1
                    par_msgv2 = fimsg-msgv2
                    par_msgv3 = fimsg-msgv3
                    par_msgv4 = fimsg-msgv4
               importing
                    par_msgtx = txt_zeile
               exceptions
                    others    = 8.
          write:
            /    sy-vline    no-gap,
                 fimsg-msgid(2),
                 fimsg-msgno no-gap,
                 sy-vline    no-gap,
            (93) txt_zeile   no-gap,
                 sy-vline.
          hide: flg_local, fimsg, txt_zeile.
          at end of msort.
            uline at (102).
          endat.
        endloop.
        clear: fimsg, txt_zeile.

      when 'F'.                        "Sprung in die DTA-Verwaltung
        call function 'AUTHORITY_CHECK_TCODE'
             exporting
                  tcode  = up_fdta
             exceptions
                  ok     = 0
                  others = 4.
        if sy-subrc ne 0.
          message s172(00) with up_fdta.
        else.
          refresh up_bdc.
          clear up_bdc.
          up_bdc-program  = 'SAPMFDTA'.
          up_bdc-dynpro   = '100'.
          up_bdc-dynbegin = 'X'.
          append up_bdc.
          clear up_bdc.
          up_bdc-fnam     = 'REGUT-RENUM'.
          up_bdc-fval     = tab_ausgabe-renum.
          append up_bdc.
          clear up_bdc.
          up_bdc-fval     = '/8'.
          up_bdc-fnam     = 'BDC_OKCODE'.
          append up_bdc.
          clear up_bdc.
          up_bdc-program  = 'SAPMFDTA'.
          up_bdc-dynpro   = '200'.
          up_bdc-dynbegin = 'X'.
          append up_bdc.
          clear up_bdc.
          up_bdc-fval     = '/BDA'.
          up_bdc-fnam     = 'BDC_OKCODE'.
          append up_bdc.
          call transaction up_fdta using up_bdc mode 'E'.
        endif.

      when 'S'.                        "Sprung in die Druckverwaltung
        check tab_ausgabe-spoolnr ne space.
        refresh up_list.
        up_list-id = tab_ausgabe-spoolnr.
        append up_list.
        call function 'RSPO_RID_SPOOLREQ_LIST'
             exporting
                  id_list = up_list[]
             exceptions
                  others  = 0.

    endcase.

  else.

    check:
      flg_local eq 'E',
      not fimsg-msgid is initial,
      not fimsg-msgno is initial.
    up_titel   = text_003.
    up_meldung = txt_zeile.
    call function 'HELPSCREEN_NA_CREATE'
         exporting
              langu   = sy-langu
              meldung = up_meldung
              meld_id = fimsg-msgid
              meld_nr = fimsg-msgno
              msgv1   = fimsg-msgv1
              msgv2   = fimsg-msgv2
              msgv3   = fimsg-msgv3
              msgv4   = fimsg-msgv4
              titel   = up_titel.
    clear fimsg.

  endif.


*----------------------------------------------------------------------*
* FORM BELEGDATEN_SCHREIBEN                                            *
*----------------------------------------------------------------------*
* Beleginformation zum Speichern in interne Tabelle schreiben          *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form belegdaten_schreiben.

  clear tab_belege30a.
  tab_belege30a-mandt   = sy-mandt.
  if reguh-pyord is initial.
    tab_belege30a-bukrs = reguh-zbukr.
    tab_belege30a-belnr = reguh-vblnr.
    tab_belege30a-gjahr = regud-gjahr.
    tab_belege30a-ubhkt = reguh-ubhkt.
  else.
    tab_belege30a-pyord = reguh-pyord.
  endif.
  append tab_belege30a.


endform.                               "BELEGDATEN_SCHREIBEN



*----------------------------------------------------------------------*
* FORM TAB_BELEGE_SCHREIBEN                                            *
*----------------------------------------------------------------------*
* Alle Beleginformationen in die Datenbank sichern                     *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
* no USING-parameters                                                  *
*----------------------------------------------------------------------*
form tab_belege_schreiben.

  data: relid like sy-saprl.           "Current Release
  data: tab_belege40a like dta_belege occurs 0 with header line.

  rfdt-aedat = sy-datlo.
  rfdt-usera = sy-uname.
  rfdt-pgmid = sy-repid.

  relid = sy-saprl.
  export tab_belege40a from tab_belege30a
         relid                         "Tabelle und Release sichern
         to database rfdt(fb)
         id hlp_dta_id.
  if sy-subrc ne 0.
    if sy-batch eq space.
      message a226 with 'RFDT'.
    else.
      message s226 with 'RFDT'.
      stop.
    endif.
  endif.

endform.                               "TAB_BELEGE_SCHREIBEN



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
form zusatzfeld_fuellen using zusatz kfzland.

  data up_zusatz like regut-dtkey.

* Formatspezifisches Zusatzfeld
  if hlp_dtfor eq 'MT100'.             "Swift international
    up_zusatz      = reguh-hbkid.

  elseif hlp_dtfor eq 'SAP IDOC'.      "IDoc für EDI
    up_zusatz      = reguh-hbkid.
    up_zusatz+5(1) = regud-xeinz.

  elseif hlp_dtfor(5) eq 'DTAUS'       "Deutschland Inland
      or hlp_dtfor    eq 'MTS'         "Neuseeland Inland
      or hlp_dtfor    eq 'BECS'.       "Australien Inland
    up_zusatz      = reguh-hbkid.
    up_zusatz+5(1) = regud-xeinz.
    up_zusatz+6    = reguh-hktid.

* Länderspezifisches Zusatzfeld
  elseif kfzland eq 'CH'.              "Schweiz
    up_zusatz+5(1) = regud-xeinz.

  elseif kfzland eq 'DK'               "Dänemark
      or kfzland eq 'F'.               "Frankreich
    up_zusatz      = reguh-hbkid.
    up_zusatz+5(1) = regud-xeinz.

  elseif kfzland eq 'ZA'.              "Südafrika
    select * from regut
        where banks eq 'ZA'
          and dtfor eq hlp_dtfor       "Entweder nur ACB oder nur EFT
          and laufd eq reguh-laufd
          and laufi eq reguh-laufi.
      exit.
    endselect.
    if sy-subrc eq 0.                  "Eintrag gefunden
      up_zusatz = regut-dtkey.         "Da schon vorhanden -> kopieren
    else.
      select * from regut
        where banks eq 'ZA '
          and dtfor eq hlp_dtfor
        order by dtkey.
      endselect.
      if sy-subrc ne 0.
        up_zusatz+0(4) = '0001'.
      else.
        add 1 to regut-dtkey+0(4).
        write regut-dtkey to up_zusatz+0(4) right-justified.
        while up_zusatz+0(4) ca space.
          replace space with '0' into up_zusatz+0(4).
        endwhile.
      endif.
    endif.

  else.                                "default
    up_zusatz = reguh-hbkid.
  endif.

  zusatz = up_zusatz.

endform.                               "ZUSATZFELD_FUELLEN



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
form temse_oeffnen.
  data: _rc(5),
        _errmsg(100).

  perform naechster_index using hlp_renum.

  perform fuellen_regut using *regut-dtkey.

  perform temse_name using hlp_renum   "Dateinamen generieren lassen
                           hlp_temsename.

   *regut-tsnam = hlp_temsename.       "Name der TemSe-Datei

  call 'C_RSTS_OPEN_WRITE'
       id 'HANDLE'  field hlp_handle   "file-handle
       id 'NAME'    field hlp_temsename"gewünschter Dateiname
       id 'BINARY'  field 'X'          "binär öffnen !
       id 'TYPE'    field 'DATA'
       id 'RECTYP'  field 'U------'    "Zusatz zum binären Öffnen
       id 'RC'      field _rc
       id 'ERRMSG'  field _errmsg.

  if sy-subrc ne 0.                    "Fehler beim Öffnen
    if sy-batch eq space.
      message a182(fr) with hlp_temsename.
    else.
      message s182(fr) with hlp_temsename.
      stop.
    endif.
  endif.


endform.                               "TEMSE_OEFFNEN



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
form temse_schreiben using value(buffer).
  data: _rc(5),
        _errmsg(100).

  call 'C_RSTS_WRITE'
       id 'HANDLE'  field hlp_handle
       id 'BUFF'    field buffer
       id 'RC'      field _rc
       id 'ERRMSG'  field _errmsg.

  if sy-subrc ne 0.                    "Fehler beim Schreiben
    if sy-batch eq space.
      message a229.
    else.
      message s229.
      stop.
    endif.
  endif.

endform.                               "TEMSE_SCHREIBEN



*----------------------------------------------------------------------*
* FORM TEMSE_SCHLIESSEN                                                *
*----------------------------------------------------------------------*
* Schließen der geöffneten TemSe-Datei.                                *
* close file                                                           *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
*----------------------------------------------------------------------*
form temse_schliessen.
  data: _rc(5),
        _errmsg(100).

  call 'C_RSTS_CLOSE'
       id 'HANDLE'  field hlp_handle
       id 'RC'      field _rc
       id 'ERRMSG'  field _errmsg.

  if sy-subrc ne 0.                    " Fehler beim Schließen
    if sy-batch eq space.
      message w230.
    else.
      message s230.
      stop.
    endif.
  else.
    clear hlp_handle.
  endif.

endform.                               "TEMSE_SCHLIESSEN



*----------------------------------------------------------------------*
* FORM NAECHSTER_INDEX                                                 *
*----------------------------------------------------------------------*
* nächsten freien Index suchen                                         *
* get next free number                                                 *
*----------------------------------------------------------------------*
* NUMBER enthält diesen Index / contains a new number                  *
*----------------------------------------------------------------------*
form naechster_index using number like febkey-kukey.

  call function 'GET_SHORTKEY_FOR_FEBKO'  "Nächsten freien Index holen
    exporting   i_tname   = 'TEMSE'
    importing   e_kukey   =  number
    exceptions  febkey_update_error = 1.

  if sy-subrc = 1.
    if sy-batch eq space.
      message a228 with 'FEBKEY'.
    else.
      message s228 with 'FEBKEY'.
      stop.
    endif.
  elseif number eq '00000000'.         "Field: FEBKEY-KUKEY: Numeric(8)
    perform naechster_index using number. "nächste Nummer holen
  endif.

endform.                               "NAECHSTER_INDEX



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
form temse_name using value(number)
                      file.

  data: up_name(64),
        up_num  like febko-kukey,
        up_len  type p.

  up_num = number.
  describe field file length up_len.

  if up_len lt 20.                     "Mindestlänge des Namens
    if sy-batch eq space.
      message a183.
    else.
      message s183.
      stop.
    endif.
  endif.

  clear up_name.
  up_name    = 'DTA'.
  up_name+3  = sy-datlo+2(6).
  up_name+9  = sy-timlo(6).
  up_name+15 = '_'.
  up_name+16 = up_num+4(4).

  file = up_name.

endform.                               "TEMSE_NAME



*----------------------------------------------------------------------*
* FORM DATEI_OEFFNEN                                                   *
*----------------------------------------------------------------------*
* die jeweilige Datei (TemSe/File) öffnen                              *
* open current file (either in TemSe or file-system)                   *
*----------------------------------------------------------------------*
form datei_oeffnen.

  if hlp_temse ca par_dtyp.            "TemSe-Format
    perform temse_oeffnen.
  else.                                "disk-/tape-fmt on file-system
    perform naechster_index using hlp_renum.
    perform fuellen_regut using *regut-dtkey.
    add 1 to cnt_filenr.
    hlp_filename    = par_unix.
    hlp_filename+45 = cnt_filenr.
    condense hlp_filename no-gaps.
    open dataset hlp_filename for output.
    if sy-subrc ne 0.
      if sy-batch eq space.
        message a182(fr) with hlp_filename.
      else.
        message s182(fr) with hlp_filename.
        stop.
      endif.
    endif.
  endif.

* Referenznr für RFDT sichern, Tabelle für Zahlungsbelege löschen
* store reference-number, refresh table for document-numbers
  call function 'COMPUTE_CONTROL_NUMBER'
       exporting
            i_refno  = hlp_renum
       importing
            e_result = hlp_resultat.
  regud-label = hlp_dta_id-refnr = hlp_resultat.
  clear   tab_belege30a.
  refresh tab_belege30a.

endform.                               "DATEI_OEFFNEN



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
form datei_schliessen.

  data:
    up_fname like hlp_filename,
    up_text  like rfpdo2-fordtext.

  perform abschluss_regut.
  if hlp_temse ca par_dtyp.            "TemSe
    perform temse_schliessen.
    up_fname = hlp_temsename.
    up_text  = text_006.
  else.                                "Filesystem
    close dataset hlp_filename.
    up_fname = hlp_filename.
    up_text  = text_005.
  endif.

  clear tab_ausgabe.
  tab_ausgabe-name     = up_text.
  tab_ausgabe-filename = up_fname.
  tab_ausgabe-renum    = *regut-renum.
  replace '&' with reguh-hbkid into tab_ausgabe-name.
  collect tab_ausgabe.

* Gesammelte Zahlungsbelegdaten in Datenbank RFDT sichern
* Store payment documents (not for proposal run or HR !)
  if reguh-xvorl is initial and        "Kein Vorschlagslauf und
     hlp_laufk ca ' *R'.               "nur FI oder Payment Request
    perform tab_belege_schreiben.
  endif.

endform.                               "DATEI_SCHLIESSEN



*----------------------------------------------------------------------*
* FORM FUELLEN_REGUT                                                   *
*----------------------------------------------------------------------*
* REGUT-Felder mit den bereits bekannten Werten füllen                 *
* Fill REGUT-fields with current values                                *
*----------------------------------------------------------------------*
* IMPORT:                                                              *
*----------------------------------------------------------------------*
form fuellen_regut using value(dtkey).

  clear *regut.
  move-corresponding reguh to *regut.  "ZBUKR,LAUFD,LAUFI,XVORL füllen
   *regut-banks = reguh-ubnks.

* Die bereits bekannten Funktionsfelder füllen
   *regut-dtkey  = dtkey.              "Zusatzkey
   *regut-tsusr  = sy-uname.
   *regut-report = sy-repid.

  clear regut-lfdnr.                   "Wichtig bei Bankwechsel !
  select * from regut                  "Bestimme die tatsächliche LFDNR,
         where zbukr = reguh-zbukr     "  wenn nur ein zahlender
           and banks = reguh-ubnks     "  Buchungskreis vorliegt
           and laufd = reguh-laufd
           and laufi = reguh-laufi
           and xvorl = reguh-xvorl
           and dtkey = dtkey
         order by lfdnr descending.    "Größte Nummer
    exit.
  endselect.
  if not sy-subrc is initial.
    select * from regut                "Bestimme die tatsächliche LFDNR,
           where zbukr = space         "  wenn mehrere zahlende
             and banks = reguh-ubnks   "  Buchungskreise im File vor-
             and laufd = reguh-laufd   "  kommen (ZBUKR = SPACE)
             and laufi = reguh-laufi
             and xvorl = reguh-xvorl
             and dtkey = dtkey
           order by lfdnr descending.  "Größte Nummer
      exit.
    endselect.
  endif.

   *regut-lfdnr = regut-lfdnr + 1.     "nächstgrößeren Wert nehmen

endform.                               "FUELLEN_REGUT



*----------------------------------------------------------------------*
* FORM ABSCHLUSS_REGUT                                                 *
*----------------------------------------------------------------------*
* Noch unbesetzte REGUT-Felder füllen, Datensatz aktualisieren         *
* Fill missing REGUT-fields and update record on database              *
*----------------------------------------------------------------------*
* IMPORT: FILENAME - Nur bei TemSe: Vorschlag für Download-Dateinamen  *
*         FILENAME - only for TemSe: Proposal filename for download    *
*----------------------------------------------------------------------*
form abschluss_regut.

  data: up_lines like sy-linno,
        up_zbukr like reguta-zbukr.

  if hlp_tsdat is initial.             "Noch kein Datum erfasst
    hlp_tsdat = sy-datlo.
  endif.

  if hlp_tstim is initial.             "Noch keine Zeit erfasst
    hlp_tstim = sy-timlo.
  endif.

* Noch unbelegte Funktionsfelder der Leiste für REGUT füllen
   *regut-waers = t001-waers.
   *regut-rbetr = sum_regut.
   *regut-renum = hlp_resultat.
   *regut-dtfor = hlp_dtfor.
  *regut-tsdat = hlp_tsdat.            "Werte der Zeiterfassung kopieren
   *regut-tstim = hlp_tstim.

  if hlp_temse ca par_dtyp.            "TemSe
    if not par_unix is initial.        "Wert wurde bereits angegeben
       *regut-dwnam = par_unix.        "als Vorschlagswert übernehmen
    endif.
  else.                                "keine Temse -> Download fertig !
     *regut-fsnam = hlp_filename.      "Dateinamen übernehmen,
     *regut-dwnam = hlp_filename.      "als Downloadnamen setzen und
     *regut-dwdat = sy-datlo.          "aktuelle Daten festhalten
     *regut-dwtim = sy-timlo.
     *regut-dwusr = sy-uname.
  endif.

* Update DB table of paying company codes and origins of the file
* clear paying company code if it is not unique
  clear: up_lines, up_zbukr.
  loop at tab_reguta.
    if tab_reguta-zbukr ne up_zbukr.
      add 1 to up_lines.
      up_zbukr = tab_reguta-zbukr.
    endif.
  endloop.
  if up_lines > 1.
    clear *regut-zbukr.
  endif.

  insert regut from *regut.
  if sy-subrc ne 0.                    "Insert fehlerhaft
    do 10 times.                       "10 Versuche maximal !
       *regut-lfdnr = *regut-lfdnr + sy-index.
      insert regut from *regut.        "Suche springend nach freiem Wert
      if sy-subrc eq 0.
        exit.
      endif.
    enddo.

    if sy-subrc ne 0.                  "Inserts fehlerhaft --> Abbruch
      if sy-batch eq space.
        message a228 with 'REGUT'.
      else.
        message s228 with 'REGUT'.
        stop.
      endif.
    endif.
  endif.                               "IF SY-SUBRC...

  read table tab_reguta index 1.
  if tab_reguta-lfdnr <> *regut-lfdnr.
    loop at tab_reguta.
      tab_reguta-lfdnr = *regut-lfdnr.
      modify tab_reguta.
    endloop.
  endif.
  insert reguta from table tab_reguta.
  clear: tab_reguta, tab_reguta[].

  call function 'DB_COMMIT'.

  select * from regut up to 1 rows     "Test, ob gerade ein Duplikat
         where zbukr = *regut-zbukr    "  erstellt worden ist
           and banks = *regut-banks    "  (gleiche Attribute, aber
           and laufd = *regut-laufd    "  andere Referenznummer)
           and laufi = *regut-laufi
           and xvorl = *regut-xvorl
           and dtkey = *regut-dtkey
           and waers = *regut-waers
           and rbetr = *regut-rbetr
           and dtfor = *regut-dtfor
           and renum ne *regut-renum.
  endselect.
  if sy-subrc eq 0.
    fimsg-msgv1 = regut-renum.
    fimsg-msgv2 = *regut-renum.
    perform message using 417.
  endif.

endform.                               "ABSCHLUSS_REGUT



*----------------------------------------------------------------------*
* FORM STORE_ON_FILE                                                   *
*----------------------------------------------------------------------*
* Ausgabe in die TemSe oder in das File-System                         *
* Output into TemSe or into file-system                                *
*----------------------------------------------------------------------*
* IMPORT: DATEN : zu schreibende Daten                                 *
*                 data to be stored                                    *
*----------------------------------------------------------------------*
form store_on_file using daten.

  if hlp_temse ca par_dtyp.            "Temse
    perform temse_schreiben using daten.
  else.
    transfer daten to hlp_filename.
  endif.

* Fill internal table of paying company codes of actual DME file
* for FDTA authority check
  move-corresponding
       *regut to tab_reguta.
  tab_reguta-zbukr = reguh-zbukr.
  tab_reguta-dorigin = reguh-dorigin.
  collect tab_reguta.

endform.                               "STORE_ON_FILE


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
form pruefung_betrag using p_length p_amount.

  data: _max_amount(18) type n,
        _length type i,
        _rwbtr like reguh-rwbtr,
        _amount like reguh-rwbtr.

  _amount              = abs( p_amount ).
  _length              = p_length.
  _max_amount(_length) = _amount.
  _rwbtr               = _max_amount(_length).

  if _rwbtr ne _amount.
    err_betrag-waers = reguh-waers.
    err_betrag-rwbtr = reguh-rwbtr.
    err_betrag-zbukr = reguh-zbukr.
    err_betrag-vblnr = reguh-vblnr.
    collect err_betrag.
    reject.
  endif.

endform.                               " PRUEFUNG_BETRAG

*&---------------------------------------------------------------------*
* FORM READ_SCB_INDICATOR                                              *
*&---------------------------------------------------------------------*
*   reads table T015L. If no entry was found, sy-subrc is NOT 0 and    *
*   the workarea T015L is cleared. In this case no error message is    *
*   send, the calling program has to react by itself.                  *
*----------------------------------------------------------------------*

form read_scb_indicator using p_lzbkz like t015l-lzbkz.

  if p_lzbkz is initial.
    sy-subrc = 4.
    clear t015l.
    exit.
  endif.

  sy-subrc = 0.
  check p_lzbkz ne t015l-lzbkz.

  read table tab_t015l into t015l with table key lzbkz = p_lzbkz.

  if sy-subrc ne 0.
    select single * from t015l where lzbkz = p_lzbkz.
    if sy-subrc eq 0.
      insert t015l into table tab_t015l.
    else.
      clear t015l.
    endif.
  endif.

endform.                               "READ_LZBKZ


*&---------------------------------------------------------------------*
*&      Form  GET_VALUE_DATE
*&---------------------------------------------------------------------*
*       compute the value date if it is initial
*----------------------------------------------------------------------*
form get_value_date.

  check reguh-valut is initial.
  reguh-valut = reguh-zaldt.
  call function 'DET_VALUE_DATE_FOR_PAYMENT'
       exporting
            i_bldat            = reguh-zaldt
            i_budat            = reguh-zaldt
            i_bukrs            = reguh-zbukr
            i_faedt            = reguh-zaldt
            i_hbkid            = reguh-hbkid
            i_hktid            = reguh-hktid
            i_vorgn            = ' '
*           I_WBGRU            = ' '
            i_zlsch            = reguh-rzawe
       importing
            valuta             = reguh-valut
       exceptions
            cal_id_error       = 1
            not_found_in_t012a = 2
            not_found_in_t012c = 3
            error_in_t012c     = 4
            others             = 5.

  if sy-subrc ne 0.
    select * from t042v where bukrs eq reguh-zbukr
                          and zlsch eq reguh-rzawe
                          and hbkid eq reguh-hbkid
                          and hktid eq reguh-hktid
                          and betrg ge reguh-rbetr.
      exit.
    endselect.
    if sy-subrc eq 0.
      reguh-valut = reguh-valut + t042v-anztg.
    endif.
  endif.

endform.                               " GET_VALUE_DATE
*&---------------------------------------------------------------------*
*&      Form  BERECHTIGUNG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form berechtigung.

  data lt_fimsg like table of fimsg with header line.
* Lese Fehlertabelle aus LD und konvertiere in FIMSG Format
  call function 'FI_PYF_AUTHORITY_OUTPUT'
       tables
            t_fimsg    = lt_fimsg
            t_err_auth = err_auth.
  loop at lt_fimsg into fimsg.
    at first.         " to emphasize the Type (authority) of error
      add 1 to cnt_error.
    endat.
    if sy-batch eq space.
      message id lt_fimsg-msgid type lt_fimsg-msgty
                                number lt_fimsg-msgno
                                with lt_fimsg-msgv1 lt_fimsg-msgv2
                                     lt_fimsg-msgv3 lt_fimsg-msgv4.
    endif.
    perform message using lt_fimsg-msgno.
  endloop.

endform.                               " BERECHTIGUNG
