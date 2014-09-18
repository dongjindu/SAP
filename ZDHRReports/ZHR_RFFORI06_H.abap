
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
FORM avis.


*----------------------------------------------------------------------*
* Abarbeiten der extrahierten Daten                                    *
*----------------------------------------------------------------------*
  IF flg_sort NE 2.
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
    flg_sort = 2.
  ENDIF.

  LOOP.

* Get Work schedule / Desc   - UD1K919600
*    at new reguh-pernr.  - UD1K919674

      select single SCHKZ into l_schkz
             from pa0007 where pernr = reguh-pernr.

      select single RTEXT into l_rtext from T508S
             where SCHKZ = l_schkz.

*    endat.  - UD1K919674
* end of changes  - UD1K919600

*-- Neuer zahlender Buchungskreis --------------------------------------
    AT NEW reguh-zbukr.

      IF NOT reguh-zbukr IS INITIAL.   "FPAYM
        PERFORM buchungskreis_daten_lesen.
      ENDIF.                           "FPAYM
*     reading the description of kostl
      select SINGLE ktExt into kostl_des
        from cskT
        where kostl = regup-kostl AND
              KOKRS = 'H201'      AND
              DATBI GE SY-DATUM   AND
              SPRAS = SY-LANGU.



    ENDAT.


   AT NEW HLP_SORTP1.
     select single SCHKZ into l_schkz
             from pa0007 where pernr = reguh-pernr.

      select single RTEXT into l_rtext from T508S
             where SCHKZ = l_schkz.


*    AT NEW HLP_SORTP1.
*     reading the description of kostl
*      select SINGLE ktExt into kostl_des
*        from cskT
*        where kostl = regup-kostl AND
*              KOKRS = 'H201'      AND
*              DATBI GT SY-DATUM   AND
*              SPRAS = SY-LANGU.

    ENDAT.

*-- Neuer Zahlweg ------------------------------------------------------
    AT NEW reguh-rzawe.

      flg_probedruck = 0.              "für diesen Zahlweg wurde noch
      flg_sgtxt      = 0.              "kein Probedruck durchgeführt

      IF reguh-rzawe NE space.
        PERFORM zahlweg_daten_lesen.

*       Zahlungsformular nur zum Lesen öffnen
        IF NOT t042e-zforn IS INITIAL.
          CALL FUNCTION 'OPEN_FORM'
               EXPORTING
                    form     = t042e-zforn
                    dialog   = space
                    device   = 'SCREEN'
                    language = t001-spras
               EXCEPTIONS
                    form     = 1.
          IF sy-subrc EQ 0.            "Formular existiert

*           Formular auf Segmenttext (Global &REGUP-SGTXT) untersuchen
            IF par_xdta EQ space.
              IF t042e-xavis NE space AND t042e-anzpo NE 99.
                CALL FUNCTION 'READ_FORM_LINES'
                     EXPORTING
                          element = hlp_ep_element
                     TABLES
                          lines   = tab_element
                     EXCEPTIONS
                          element = 1.
                IF sy-subrc EQ 0.
                  LOOP AT tab_element.
                    IF    tab_element-tdline   CS 'REGUP-SGTXT'
                      AND tab_element-tdformat NE '/*'.
                      flg_sgtxt = 1.   "Global für Segmenttext existiert
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
      t042z-text1 = text_001.

      DATA: L_NAME LIKE  itcpo-tddataset.                   "UD1K912809
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'          "UD1K912809
        EXPORTING
*          INPUT    =  REGUP-KOSTL
           INPUT    =  PA0007-SCHKZ
        IMPORTING
          OUTPUT   =  L_NAME.                               "UD1K912809

*     Vorschlag für die Druckparameter aufbauen

      PERFORM fill_itcpo USING par_pria
*                               'LIST5S'            "UD1K912809
                               L_NAME                       "UD1K912809
                               space   "par_sofa via tab_ausgabe!
                               hlp_auth.

      EXPORT itcpo TO MEMORY ID 'RFFORI06_ITCPO'.

    ENDAT.


*-- Neue Hausbank ------------------------------------------------------
    AT NEW reguh-ubnkl.

      PERFORM hausbank_daten_lesen.

*     Felder für Formularabschluß initialisieren
      cnt_avise      = 0.
      cnt_avedi      = 0.
      cnt_avfax      = 0.
      cnt_avmail     = 0.
      sum_abschluss  = 0.
      sum_abschl_edi = 0.
      sum_abschl_fax = 0.
      REFRESH tab_edi_avis.

      flg_druckmodus = 0.
    ENDAT.


*-- Neue Empfängerbank -------------------------------------------------
    AT NEW reguh-zbnkl.

      PERFORM empfbank_daten_lesen.

    ENDAT.


*-- Neue Zahlungsbelegnummer -------------------------------------------
    AT NEW reguh-vblnr.

*     Prüfe, ob Avis auf Papier erzwungen wird
*     Check if advice on paper is forced
      IF flg_papieravis EQ 1.
        reguh-ediav = space.
      ENDIF.

*     Prüfe, ob HR-Formular zu verwenden ist
*     Check if HR-form is to be used
      hrxblnr = regup-xblnr.
      IF ( hlp_laufk EQ 'P' OR
           hrxblnr-txtsl EQ 'HR' AND hrxblnr-txerg EQ 'GRN' )
       AND hrxblnr-xhrfo NE space.
        hlp_xhrfo = 'X'.
      ELSE.
        hlp_xhrfo = space.
      ENDIF.

*     HR-Formular besorgen
*     read HR form
      IF hlp_xhrfo EQ 'X'.
        PERFORM hr_formular_lesen.
      ENDIF.

*     Prüfung, ob Avis erforderlich
      cnt_zeilen = 0.
      IF hlp_xhrfo EQ space.
        IF flg_sgtxt EQ 1.
          cnt_zeilen = reguh-rpost + reguh-rtext.
        ELSE.
          cnt_zeilen = reguh-rpost.
        ENDIF.
      ELSE.
        DESCRIBE TABLE pform LINES cnt_zeilen.
      ENDIF.
      flg_kein_druck = 0.
      IF reguh-ediav EQ 'V'.
*       Avis bereits versendet
        flg_kein_druck = 1.            "kein Druck erforderlich
      ELSEIF reguh-rzawe NE space AND t042e-xsavi IS INITIAL.
*       Avis zu Formular
        IF hlp_zeilen EQ 0 AND par_xdta EQ space.
          IF t042e-xavis EQ space OR cnt_zeilen LE t042e-anzpo.
            flg_kein_druck = 1.        "kein Druck erforderlich
          ENDIF.
*       Avis zum DTA
        ELSE.
          CLEAR tab_kein_avis.
          MOVE-CORRESPONDING reguh TO tab_kein_avis.
          READ TABLE tab_kein_avis.
          IF sy-subrc EQ 0.
            flg_kein_druck = 1.        "kein Druck erforderlich
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM fpaym USING 1.           "FPAYM
      PERFORM zahlungs_daten_lesen.
      IF reguh-ediav NA ' V' AND hlp_xhrfo EQ space.
        PERFORM summenfelder_initialisieren.
      ENDIF.

*     Schecknummer bei vornumerierten Schecks
      CLEAR regud-chect.
      READ TABLE tab_schecks WITH KEY
        zbukr = reguh-zbukr
        vblnr = reguh-vblnr.
      IF sy-subrc EQ 0.
        regud-chect = tab_schecks-chect.
      ELSEIF flg_schecknum EQ 1.
        IF zw_xvorl EQ space.
          IF hlp_laufk NE 'P'.         "FI-Beleg vorhanden?
            SELECT * FROM payr
              WHERE zbukr EQ reguh-zbukr
              AND   vblnr EQ reguh-vblnr
              AND   gjahr EQ regud-gjahr
              AND   voidr EQ 0.
            ENDSELECT.
            sy-msgv1 = reguh-zbukr.
            sy-msgv2 = regud-gjahr.
            sy-msgv3 = reguh-vblnr.
          ELSE.                        "HR-Abrechnung vorhanden?
            SELECT * FROM payr
              WHERE pernr EQ reguh-pernr
              AND   seqnr EQ reguh-seqnr
              AND   btznr EQ reguh-btznr
              AND   voidr EQ 0.
            ENDSELECT.
            sy-msgv1 = reguh-pernr.
            sy-msgv2 = reguh-seqnr.
            sy-msgv3 = reguh-btznr.
          ENDIF.
          IF sy-subrc EQ 0.
            regud-chect = payr-chect.
          ELSE.
            READ TABLE err_fw_scheck WITH KEY
               zbukr = reguh-zbukr
               vblnr = reguh-vblnr.
            IF sy-subrc NE 0.
              IF sy-batch EQ space.    "check does not exist
                MESSAGE a564(fs) WITH sy-msgv1 sy-msgv2 sy-msgv3.
              ELSE.
                MESSAGE s564(fs) WITH sy-msgv1 sy-msgv2 sy-msgv3.
                MESSAGE s549(fs).
                STOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          regud-chect = 'TEST'.
        ENDIF.
      ELSEIF flg_avis EQ 1.
        IF hlp_laufk NE 'P'.         "FI-Beleg vorhanden?
          SELECT * FROM payr
            WHERE zbukr EQ reguh-zbukr
            AND   vblnr EQ reguh-vblnr
            AND   gjahr EQ regud-gjahr
            AND   voidr EQ 0.
          ENDSELECT.
        ELSE.                        "HR-Abrechnung vorhanden?
          SELECT * FROM payr
            WHERE pernr EQ reguh-pernr
            AND   seqnr EQ reguh-seqnr
            AND   btznr EQ reguh-btznr
            AND   voidr EQ 0.
          ENDSELECT.
        ENDIF.
        IF sy-subrc EQ 0.
          regud-chect = payr-chect.
        ENDIF.
      ENDIF.

*     Berechnung Anzahl benötigter Wechsel
      IF reguh-weamx EQ 0.
        regud-wecan = 1.
      ELSE.
        regud-wecan = reguh-weamx.
        IF reguh-wehrs NE 0.
          ADD 1 TO regud-wecan.
        ENDIF.
      ENDIF.

    ENDAT.


*-- Verarbeitung der Einzelposten-Informationen ------------------------
    AT daten.
      PERFORM fpaym USING 2.           "FPAYM
      PERFORM einzelpostenfelder_fuellen.
      IF flg_papieravis EQ 1.
        reguh-ediav = space.
      ENDIF.
      IF reguh-ediav NA ' V' AND hlp_xhrfo EQ space.
        PERFORM summenfelder_fuellen.
      ENDIF.

    ENDAT.


*-- Ende der Zahlungsbelegnummer ---------------------------------------
    AT END OF reguh-vblnr.

      PERFORM fpaym USING 2.           "FPAYM

*     Zahlungsbelegnummer bei Saldo-Null-Mitteilungen und
*     Zahlungsanforderungen nicht ausgeben
      IF ( reguh-rzawe EQ space AND reguh-xvorl EQ space )
        OR t042z-xzanf NE space.
        reguh-vblnr = space.
      ENDIF.

*     Stets Ausgabe via EDI, falls möglich
      IF flg_papieravis EQ 1.
        reguh-ediav = space.
      ENDIF.
      CLEAR regud-avedn.
      IF reguh-ediav NA ' V' AND hlp_xhrfo EQ space.
        CALL FUNCTION 'FI_EDI_REMADV_PEXR2001_OUT'
             EXPORTING
                  reguh_in   = reguh
                  regud_in   = regud
                  xeinz_in   = regud-xeinz
             IMPORTING
                  docnum_out = regud-avedn
             TABLES
                  tab_regup  = tab_regup
             EXCEPTIONS
                  OTHERS     = 4.
        IF sy-subrc EQ 0.
          ADD 1            TO cnt_avedi.
          ADD reguh-rbetr  TO sum_abschl_edi.
          WRITE:
            cnt_avise      TO regud-avise,
            cnt_avedi      TO regud-avedi,
            cnt_avfax      TO regud-avfax,
            cnt_avmail     TO regud-avmail,
            sum_abschluss  TO regud-summe CURRENCY t001-waers,
            sum_abschl_edi TO regud-suedi CURRENCY t001-waers,
            sum_abschl_fax TO regud-sufax CURRENCY t001-waers,
            sum_abschl_mail TO regud-sumail CURRENCY t001-waers.
          TRANSLATE:
            regud-avise USING ' *',
            regud-avedi USING ' *',
            regud-avfax USING ' *',
            regud-avmail USING ' *',
            regud-summe USING ' *',
            regud-suedi USING ' *',
            regud-sufax USING ' *',
            regud-sumail USING ' *'.
          tab_edi_avis-reguh = reguh.
          tab_edi_avis-regud = regud.
          APPEND tab_edi_avis.
          flg_kein_druck = 1.
        ENDIF.
      ENDIF.

*     Ausgabe auf Fax oder Drucker (nur falls notwendig)
      IF flg_kein_druck EQ 0.

        PERFORM avis_nachrichtenart.
        PERFORM avis_oeffnen USING 'X'.
        PERFORM zahlungs_daten_lesen_hlp.
        PERFORM summenfelder_initialisieren.
        PERFORM avis_schreiben.

      ENDIF.

    ENDAT.


*-- Ende der Hausbank --------------------------------------------------
    AT END OF reguh-ubnkl.

      PERFORM fpaym USING 3.           "FPAYM

      DATA:
        l_form  LIKE itcta-tdform,
        l_pages LIKE itctg OCCURS 0 WITH HEADER LINE.
      IF hlp_laufk NE '*'.
        IF l_form NE hlp_formular.
          l_form = hlp_formular.
          REFRESH l_pages.
          CALL FUNCTION 'READ_FORM'
               EXPORTING
                    form          = l_form
                    language      = t001-spras
                    throughclient = 'X'
               TABLES
                    pages         = l_pages.
        ENDIF.
        READ TABLE l_pages WITH KEY tdpage = 'LAST'.
        IF sy-subrc NE 0.
          CLEAR l_pages-tdpage.
        ENDIF.
      ENDIF.

      IF l_pages-tdpage EQ 'LAST' AND  "Formularabschluß möglich
         ( cnt_avise NE 0              "Formularabschluß erforderlich
        OR cnt_avedi NE 0
        OR cnt_avfax NE 0
        OR cnt_avmail NE 0 ) AND hlp_laufk NE '*'.

*       Formular für den Abschluß öffnen
        SET COUNTRY space.
        CLEAR finaa.
        finaa-nacha = '1'.
        PERFORM avis_oeffnen USING space.

*       Liste aller Avis-Zwischenbelege ausgeben
        IF cnt_avedi NE 0.
          REFRESH tab_elements.
          CALL FUNCTION 'READ_FORM_ELEMENTS'
               EXPORTING
                    form     = hlp_formular
                    language = t001-spras
               TABLES
                    elements = tab_elements
               EXCEPTIONS
                    OTHERS   = 3.
          READ TABLE tab_elements WITH KEY
            window  = 'MAIN'
            element = '676'.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'START_FORM'
                 EXPORTING
                      form      = hlp_formular
                      language  = t001-spras
                      startpage = 'EDI'.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      element = '675'
                      type    = 'TOP'
                 EXCEPTIONS
                      OTHERS  = 4.
            sic_reguh = reguh.
            sic_regud = regud.
            LOOP AT tab_edi_avis.
              reguh = tab_edi_avis-reguh.
              regud = tab_edi_avis-regud.
              CALL FUNCTION 'WRITE_FORM'
                   EXPORTING
                        element = '676'
                   EXCEPTIONS
                        OTHERS  = 4.
            ENDLOOP.
            CALL FUNCTION 'END_FORM'.
            reguh = sic_reguh.
            regud = sic_regud.
          ENDIF.
        ENDIF.

*       Formular für den Abschluß starten
        CALL FUNCTION 'START_FORM'
             EXPORTING
                  form      = hlp_formular
                  language  = t001-spras
                  startpage = 'LAST'.

*       Ausgabe des Formularabschlusses
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  window = 'SUMMARY'
             EXCEPTIONS
                  window = 1.
        IF sy-subrc EQ 1.
          err_element-fname = hlp_formular.
          err_element-fenst = 'SUMMARY'.
          err_element-elemt = space.
          err_element-text  = space.
          COLLECT err_element.
        ENDIF.

*       Formular beenden
        CALL FUNCTION 'END_FORM'.
      ENDIF.

      PERFORM avis_schliessen.
      IF NOT itcpp-tdspoolid IS INITIAL.
        UPDATE tsp01 SET   rqfinal = 'C'
                     WHERE rqident EQ itcpp-tdspoolid.
      ENDIF.

      IF sy-binpt EQ space.
        COMMIT WORK.
      ENDIF.

    ENDAT.


*-- Ende des Zahlwegs --------------------------------------------------
    AT END OF reguh-rzawe.

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
FORM avis_nachrichtenart.

  DATA up_fimsg LIKE fimsg OCCURS 0 WITH HEADER LINE.
  STATICS up_profile LIKE soprd.

* Nachrichtenart ermitteln lassen
  CLEAR finaa.
  finaa-nacha = '1'.
  CALL FUNCTION 'OPEN_FI_PERFORM_00002040_P'
       EXPORTING
            i_reguh = reguh
       TABLES
            t_fimsg = up_fimsg
       CHANGING
            c_finaa = finaa.
  LOOP AT up_fimsg INTO fimsg.
    PERFORM message USING fimsg-msgno.
  ENDLOOP.

* Nachrichtenart Fax (2) oder Mail (I) prüfen, sonst nur Druck (1)
  CASE finaa-nacha.
    WHEN '2'.
      CALL FUNCTION 'TELECOMMUNICATION_NUMBER_CHECK'
           EXPORTING
                service = 'TELEFAX'
                number  = finaa-tdtelenum
                country = finaa-tdteleland
           EXCEPTIONS
                OTHERS  = 4.
      IF sy-subrc NE 0.
        finaa-nacha = '1'.
        finaa-fornr = t042b-aforn.
      ENDIF.
    WHEN 'I'.
      IF up_profile IS INITIAL.
        CALL FUNCTION 'SO_PROFILE_READ'
             IMPORTING
                  profile = up_profile
             EXCEPTIONS
                  OTHERS  = 4.
        IF sy-subrc NE 0.
          up_profile-smtp_exist = '-'.
        ENDIF.
      ENDIF.
      IF up_profile-smtp_exist NE 'X' OR finaa-intad IS INITIAL.
        finaa-nacha = '1'.
        finaa-fornr = t042b-aforn.
      ENDIF.
    WHEN OTHERS.
      finaa-nacha = '1'.
  ENDCASE.

ENDFORM.



*----------------------------------------------------------------------*
* FORM AVIS_OEFFNEN                                                    *
*----------------------------------------------------------------------*
* Avis öffnen und bei Druck Probedruck erledigen                       *
*----------------------------------------------------------------------*
* GENUINE ist gesetzt bei echten Avisen, leer bei Formularabschluß     *
*----------------------------------------------------------------------*
FORM avis_oeffnen USING genuine.

  DATA: up_device    LIKE itcpp-tddevice,
        up_sender    LIKE swotobjid,
        up_recipient LIKE swotobjid.

* Mail-Sender und -Empfänger ermitteln
  IF finaa-nacha EQ 'I'.
    PERFORM mail_vorbereiten USING    sy-uname  finaa-intad
                             CHANGING up_sender up_recipient.
    IF up_sender IS INITIAL.
      IF NOT reguh-pernr IS INITIAL.
        fimsg-msgv1 = reguh-pernr.
        fimsg-msgv2 = reguh-seqnr.
      ELSE.
        fimsg-msgv1 = reguh-zbukr.
        fimsg-msgv2 = reguh-vblnr.
      ENDIF.
      PERFORM message USING '387'.
      finaa-nacha = '1'.
      finaa-fornr = t042b-aforn.
    ENDIF.
  ENDIF.

* Formular ermitteln
  IF NOT finaa-fornr IS INITIAL.
    hlp_formular = finaa-fornr.
  ELSE.
    hlp_formular = t042b-aforn.
  ENDIF.

* Vorschlag für die Druckvorgaben holen und anpassen, Device setzen
  IMPORT itcpo FROM MEMORY ID 'RFFORI06_ITCPO'.
  CASE finaa-nacha.
    WHEN '1'.
      up_device = 'PRINTER'.
    WHEN '2'.
      itcpo-tdschedule = finaa-tdschedule.
      itcpo-tdteleland = finaa-tdteleland.
      itcpo-tdtelenum  = finaa-tdtelenum.
      itcpo-tdfaxuser  = finaa-tdfaxuser.
      itcpo-tdsuffix1  = 'FAX'.
      up_device = 'TELEFAX'.
    WHEN 'I'.
      itcpo-tdtitle    = text_096.
      WRITE reguh-zaldt TO txt_zeile DD/MM/YYYY.
      REPLACE '&' WITH txt_zeile INTO itcpo-tdtitle.
      up_device = 'MAIL'.
  ENDCASE.
  CLEAR:
    toa_dara,
    arc_params.

* Druckvorgaben modifizieren lassen
  IF genuine EQ 'X'.
    CALL FUNCTION 'OPEN_FI_PERFORM_00002050_P'
         EXPORTING
              i_reguh          = reguh
              i_gjahr          = regud-gjahr
              i_nacha          = finaa-nacha
              i_aforn          = hlp_formular
         CHANGING
              c_itcpo          = itcpo
              c_archive_index  = toa_dara
              c_archive_params = arc_params.
    IF itcpo-tdarmod GT 1 AND par_anzp NE 0.              "#EC PORTABLE
      par_anzp = 0.
      PERFORM message USING '384'.
    ENDIF.
  ENDIF.

* Name des Elements mit dem Anschreiben zusammensetzen
  IF reguh-rzawe NE space.
    hlp_element   = '610-'.
    hlp_element+4 = reguh-rzawe.
    hlp_eletext   = text_610.
    REPLACE '&ZAHLWEG' WITH reguh-rzawe INTO hlp_eletext.
  ELSE.
    hlp_element   = '611-'.
    hlp_element+4 = reguh-avisg.
    hlp_eletext   = text_611.
  ENDIF.

* Prüfen, ob ein Close/Open_form notwendig ist (Performance)
  IF flg_druckmodus EQ 1.
    CHECK finaa-nacha NE '1' OR itcpo-tdarmod NE '1'.
  ENDIF.

* Dialog nur, wenn bei Druck der Drucker unbekannt
  IF par_pria EQ space AND finaa-nacha EQ '1'.
    flg_dialog = 'X'.
  ELSE.
    flg_dialog = space.
  ENDIF.

* Neue Spool-Id bei erstem Avis zum Druck oder bei Fax
  IF flg_probedruck EQ 0 OR finaa-nacha NE '1'.
    itcpo-tdnewid = 'X'.
  ELSE.
    itcpo-tdnewid = space.
  ENDIF.

* Formular schließen, falls noch offen vom letzten Avis
  PERFORM avis_schliessen.

* Formular öffnen
  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
            archive_index  = toa_dara
            archive_params = arc_params
            form           = hlp_formular
            device         = up_device
            language       = t001-spras
            options        = itcpo
            dialog         = flg_dialog
            mail_sender    = up_sender
            mail_recipient = up_recipient
       IMPORTING
            result         = itcpp
       EXCEPTIONS
            form           = 1
            mail_options   = 2.
  IF sy-subrc EQ 2.                    "E-Mailen nicht möglich,
    fimsg-msgid = sy-msgid.            "also drucken
    fimsg-msgv1 = sy-msgv1.
    fimsg-msgv2 = sy-msgv2.
    fimsg-msgv3 = sy-msgv3.
    fimsg-msgv4 = sy-msgv4.
    PERFORM message USING sy-msgno.
    IF NOT reguh-pernr IS INITIAL.
      fimsg-msgv1 = reguh-pernr.
      fimsg-msgv2 = reguh-seqnr.
    ELSE.
      fimsg-msgv1 = reguh-zbukr.
      fimsg-msgv2 = reguh-vblnr.
    ENDIF.
    PERFORM message USING '387'.
    CALL FUNCTION 'CLOSE_FORM'
         EXCEPTIONS
              OTHERS = 0.
    finaa-nacha   = '1'.
    up_device     = 'PRINTER'.
    hlp_formular  = t042b-aforn.
    CALL FUNCTION 'OPEN_FORM'
         EXPORTING
              archive_index  = toa_dara
              archive_params = arc_params
              form           = hlp_formular
              device         = up_device
              language       = t001-spras
              options        = itcpo
              dialog         = flg_dialog
         IMPORTING
              result         = itcpp
         EXCEPTIONS
              form           = 1.
  ENDIF.
  IF sy-subrc EQ 1.                    "Abbruch:
    IF sy-batch EQ space.              "Formular ist nicht aktiv!
      MESSAGE a069 WITH hlp_formular.
    ELSE.
      MESSAGE s069 WITH hlp_formular.
      MESSAGE s094.
      STOP.
    ENDIF.
  ENDIF.

* Druckmodus setzen
  IF finaa-nacha EQ '1' AND itcpo-tdarmod EQ '1'.
    flg_druckmodus = 1.
  ELSE.
    flg_druckmodus = 2.
  ENDIF.
  hlp_nacha_last = finaa-nacha.

* letzte Druckparameter merken
  IF finaa-nacha EQ '1'.
    par_pria = itcpp-tddest.
    PERFORM fill_itcpo_from_itcpp.
    EXPORT itcpo TO MEMORY ID 'RFFORI06_ITCPO'.
  ENDIF.

* Probedruck
  IF flg_probedruck EQ 0               "Probedruck noch nicht erledigt
    AND finaa-nacha EQ '1'.
    PERFORM daten_sichern.
    DO par_anzp TIMES.
*     Probedruck-Formular starten
      CALL FUNCTION 'START_FORM'
           EXPORTING
                form     = hlp_formular
                language = t001-spras.
*     Fenster mit Probedruck schreiben
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                window   = 'INFO'
                element  = '605'
                function = 'APPEND'
           EXCEPTIONS
                window   = 1
                element  = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element = hlp_element
           EXCEPTIONS
                window  = 1
                element = 2.
      IF sy-subrc NE 0.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = '610'
             EXCEPTIONS
                  window  = 1
                  element = 2.
      ENDIF.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element = '614'
           EXCEPTIONS
                window  = 1
                element = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element = '615'
           EXCEPTIONS
                window  = 1
                element = 2.
      DO 5 TIMES.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element  = '625'
                  function = 'APPEND'
             EXCEPTIONS
                  window   = 1
                  element  = 2.
      ENDDO.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element  = '630'
                function = 'APPEND'
           EXCEPTIONS
                window   = 1
                element  = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                window  = 'TOTAL'
                element = '630'
           EXCEPTIONS
                window  = 1
                element = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                window   = 'INFO'
                element  = '605'
                function = 'DELETE'
           EXCEPTIONS
                window   = 1
                element  = 2.
*     Probedruck-Formular beenden
      CALL FUNCTION 'END_FORM'.
    ENDDO.
    PERFORM daten_zurueck.
    flg_probedruck = 1.                "Probedruck erledigt
  ENDIF.

ENDFORM.



*----------------------------------------------------------------------*
* FORM AVIS_SCHREIBEN                                                  *
*----------------------------------------------------------------------*
* Avis in Druckform ausgeben                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM avis_schreiben.

* Faxdeckblatt
  IF finaa-nacha EQ '2' AND finaa-formc NE space.
    PERFORM adresse_lesen USING t001-adrnr.                 "SADR40A
    itcfx-rtitle     = reguh-zanre.
    itcfx-rname1     = reguh-znme1.
    itcfx-rname2     = reguh-znme2.
    itcfx-rname3     = reguh-znme3.
    itcfx-rname4     = reguh-znme4.
    itcfx-rpocode    = reguh-zpstl.
    itcfx-rcity1     = reguh-zort1.
    itcfx-rcity2     = reguh-zort2.
    itcfx-rpocode2   = reguh-zpst2.
    itcfx-rpobox     = reguh-zpfac.
    itcfx-rpoplace   = reguh-zpfor.
    itcfx-rstreet    = reguh-zstra.
    itcfx-rcountry   = reguh-zland.
    itcfx-rregio     = reguh-zregi.
    itcfx-rlangu     = hlp_sprache.
    itcfx-rhomecntry = t001-land1.
    itcfx-rlines     = '9'.
    itcfx-rctitle    = space.
    itcfx-rcfname    = space.
    itcfx-rclname    = space.
    itcfx-rcname1    = finaa-namep.
    itcfx-rcname2    = space.
    itcfx-rcdeptm    = finaa-abtei.
    itcfx-rcfaxnr    = finaa-tdtelenum.
    itcfx-stitle     = sadr-anred.
    itcfx-sname1     = sadr-name1.
    itcfx-sname2     = sadr-name2.
    itcfx-sname3     = sadr-name3.
    itcfx-sname4     = sadr-name4.
    itcfx-spocode    = sadr-pstlz.
    itcfx-scity1     = sadr-ort01.
    itcfx-scity2     = sadr-ort02.
    itcfx-spocode2   = sadr-pstl2.
    itcfx-spobox     = sadr-pfach.
    itcfx-spoplace   = sadr-pfort.
    itcfx-sstreet    = sadr-stras.
    itcfx-scountry   = sadr-land1.
    itcfx-sregio     = sadr-regio.
    itcfx-shomecntry = reguh-zland.
    itcfx-slines     = '9'.
    itcfx-sctitle    = fsabe-salut.
    itcfx-scfname    = fsabe-fname.
    itcfx-sclname    = fsabe-lname.
    itcfx-scname1    = fsabe-namp1.
    itcfx-scname2    = fsabe-namp2.
    itcfx-scdeptm    = fsabe-abtei.
    itcfx-sccostc    = fsabe-kostl.
    itcfx-scroomn    = fsabe-roomn.
    itcfx-scbuild    = fsabe-build.
    CONCATENATE fsabe-telf1 fsabe-tel_exten1
                INTO itcfx-scphonenr1.
    CONCATENATE fsabe-telf2 fsabe-tel_exten2
                INTO itcfx-scphonenr2.
    CONCATENATE fsabe-telfx fsabe-fax_extens
                INTO itcfx-scfaxnr.
    itcfx-header     = t042t-txtko.
    itcfx-footer     = t042t-txtfu.
    itcfx-signature  = t042t-txtun.
    itcfx-tdid       = t042t-txtid.
    itcfx-tdlangu    = hlp_sprache.
    itcfx-subject    = space.
    CALL FUNCTION 'START_FORM'
         EXPORTING
              archive_index = toa_dara
              form          = finaa-formc
              language      = hlp_sprache
              startpage     = 'FIRST'.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              window = 'RECEIVER'.
    CALL FUNCTION 'END_FORM'.
  ENDIF.

* Formular starten
  CALL FUNCTION 'START_FORM'
       EXPORTING
            archive_index = toa_dara
            form          = hlp_formular
            language      = hlp_sprache.

  IF hlp_xhrfo EQ space.

*   Fenster Info, Element Unsere Nummer (falls diese gefüllt ist)
    IF reguh-eikto NE space.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                window   = 'INFO'
                element  = '605'
                function = 'APPEND'
           EXCEPTIONS
                window   = 1
                element  = 2.
      IF sy-subrc EQ 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'INFO'.
        err_element-elemt = '605'.
        err_element-text  = text_605.
        COLLECT err_element.
      ENDIF.
    ENDIF.

*   Fenster Carry Forward, Element Übertrag (außer letzte Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              window  = 'CARRYFWD'
              element = '635'
         EXCEPTIONS
              window  = 1
              element = 2.
    IF sy-subrc EQ 2.
      err_element-fname = hlp_formular.
      err_element-fenst = 'CARRYFWD'.
      err_element-elemt = '635'.
      err_element-text  = text_635.
      COLLECT err_element.
    ENDIF.

*   Hauptfenster, Element Anschreiben-x (nur auf der ersten Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element = hlp_element
         EXCEPTIONS
              window  = 1
              element = 2.
    IF sy-subrc EQ 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element = '610'
           EXCEPTIONS
                window  = 1
                element = 2.
      err_element-fname = hlp_formular.
      err_element-fenst = 'MAIN'.
      err_element-elemt = hlp_element.
      err_element-text  = hlp_eletext.
      COLLECT err_element.
    ENDIF.

*   Hauptfenster, Element Abweichender Zahlungsemfänger
    IF regud-xabwz EQ 'X'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element = '612'
           EXCEPTIONS
                window  = 1
                element = 2.
      IF sy-subrc EQ 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'MAIN'.
        err_element-elemt = '612'.
        err_element-text  = text_612.
        COLLECT err_element.
      ENDIF.
    ENDIF.

*   Hauptfenster, Element Zahlung erfolgt im Auftrag von
    IF reguh-absbu NE reguh-zbukr.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element = '613'
           EXCEPTIONS
                window  = 1
                element = 2.
      IF sy-subrc EQ 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'MAIN'.
        err_element-elemt = '613'.
        err_element-text  = text_613.
        COLLECT err_element.
      ENDIF.
    ENDIF.

*   Hauptfenster, Element Unterschrift
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element = '614'
         EXCEPTIONS
              window  = 1
              element = 2.

*   Hauptfenster, Element Überschrift (nur auf der ersten Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element = '615'
         EXCEPTIONS
              window  = 1
              element = 2.
    IF sy-subrc EQ 2.
      err_element-fname = hlp_formular.
      err_element-fenst = 'MAIN'.
      err_element-elemt = '615'.
      err_element-text  = text_615.
      COLLECT err_element.
    ENDIF.

*   Hauptfenster, Element Überschrift (ab der zweiten Seite oben)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element = '615'
              type    = 'TOP'
         EXCEPTIONS
              window  = 1
              element = 2. "Fehler bereits oben gemerkt

*   Hauptfenster, Element Übertrag (ab der zweiten Seite oben)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element  = '620'
              type     = 'TOP'
              function = 'APPEND'
         EXCEPTIONS
              window   = 1
              element  = 2.
    IF sy-subrc EQ 2.
      err_element-fname = hlp_formular.
      err_element-fenst = 'MAIN'.
      err_element-elemt = '620'.
      err_element-text  = text_620.
      COLLECT err_element.
    ENDIF.

  ELSE.

*   HR-Formular ausgeben
*   write HR form
    LOOP AT pform.
      CHECK sy-tabix GT t042e-anzpo.
      regud-txthr = pform-linda.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element  = '625-HR'
                function = 'APPEND'
           EXCEPTIONS
                window   = 1
                element  = 2.
      IF sy-subrc EQ 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'MAIN'.
        err_element-elemt = '625-HR'.
        err_element-text  = text_625.
        COLLECT err_element.
      ENDIF.
    ENDLOOP.

  ENDIF.

* Ausgabe der Einzelposten
  flg_diff_bukrs = 0.
  LOOP AT tab_regup.

    AT NEW bukrs.
      regup-bukrs = tab_regup-bukrs.
      IF  ( regup-bukrs NE reguh-zbukr OR flg_diff_bukrs EQ 1 )
      AND ( reguh-absbu EQ space OR reguh-absbu EQ reguh-zbukr ).
        flg_diff_bukrs = 1.
        SELECT SINGLE * FROM t001 INTO *t001
          WHERE bukrs EQ regup-bukrs.
        regud-abstx = *t001-butxt.
        regud-absor = *t001-ort01.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = '613'
             EXCEPTIONS
                  window  = 1
                  element = 2.
        IF sy-subrc EQ 2.
          err_element-fname = hlp_formular.
          err_element-fenst = 'MAIN'.
          err_element-elemt = '613'.
          err_element-text  = text_613.
          COLLECT err_element.
        ENDIF.
      ENDIF.
    ENDAT.

    regup = tab_regup.
    PERFORM einzelpostenfelder_fuellen.

    IF hlp_xhrfo EQ space.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element  = '625'
                function = 'APPEND'
           EXCEPTIONS
                window   = 1
                element  = 2.
      IF sy-subrc EQ 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'MAIN'.
        err_element-elemt = '625'.
        err_element-text  = text_625.
        COLLECT err_element.
      ENDIF.
    ENDIF.

    PERFORM summenfelder_fuellen.

    IF hlp_xhrfo EQ space.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element  = '625-TX'
                function = 'APPEND'
           EXCEPTIONS
                window   = 1
                element  = 2.
    ENDIF.

    AT END OF bukrs.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element  = '629'
                function = 'APPEND'
           EXCEPTIONS
                window   = 1
                element  = 2.
    ENDAT.
  ENDLOOP.

  PERFORM ziffern_in_worten.

* Summenfelder hochzählen und aufbereiten
  CASE finaa-nacha.
    WHEN '1'.
      ADD 1           TO cnt_avise.
      ADD reguh-rbetr TO sum_abschluss.
    WHEN '2'.
      ADD 1           TO cnt_avfax.
      ADD reguh-rbetr TO sum_abschl_fax.
    WHEN 'I'.
      ADD 1           TO cnt_avmail.
      ADD reguh-rbetr TO sum_abschl_mail.
  ENDCASE.

  WRITE:
    cnt_avise       TO regud-avise,
    cnt_avedi       TO regud-avedi,
    cnt_avfax       TO regud-avfax,
    cnt_avmail      TO regud-avmail,
    sum_abschluss   TO regud-summe  CURRENCY t001-waers,
    sum_abschl_edi  TO regud-suedi  CURRENCY t001-waers,
    sum_abschl_fax  TO regud-sufax  CURRENCY t001-waers,
    sum_abschl_mail TO regud-sumail CURRENCY t001-waers.
  TRANSLATE:
    regud-avise  USING ' *',
    regud-avedi  USING ' *',
    regud-avfax  USING ' *',
    regud-avmail USING ' *',
    regud-summe  USING ' *',
    regud-suedi  USING ' *',
    regud-sufax  USING ' *',
    regud-sumail USING ' *'.

  IF hlp_xhrfo EQ space.

*   Hauptfenster, Element Gesamtsumme (nur auf der letzten Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element  = '630'
              function = 'APPEND'
         EXCEPTIONS
              window   = 1
              element  = 2.
    IF sy-subrc EQ 2.
      err_element-fname = hlp_formular.
      err_element-fenst = 'MAIN'.
      err_element-elemt = '630'.
      err_element-text  = text_630.
      COLLECT err_element.
    ENDIF.

*   Hauptfenster, Element Bankgebühr (Japan)
    IF reguh-paygr+18(2) EQ '$J'.
      WHILE reguh-paygr(1) EQ 0.
        SHIFT reguh-paygr(10) LEFT.
        IF sy-index > 10. EXIT. ENDIF.
      ENDWHILE.
      SUBTRACT reguh-rspe1 FROM: regud-swnet, sum_abschluss.
      WRITE:
         regud-swnet TO regud-swnes CURRENCY reguh-waers,
         sum_abschluss  TO regud-summe CURRENCY t001-waers.
      TRANSLATE:
         regud-swnes USING ' *',
         regud-summe USING ' *'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element = '634'
           EXCEPTIONS
                window  = 1
                element = 2.
      IF sy-subrc EQ 2.
        err_element-fname = hlp_formular.
        err_element-fenst = 'MAIN'.
        err_element-elemt = '634'.
        err_element-text  = text_634.
        COLLECT err_element.
      ENDIF.
    ENDIF.

*   Fenster Carry Forward, Element Übertrag löschen
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              window   = 'CARRYFWD'
              element  = '635'
              function = 'DELETE'
         EXCEPTIONS
              window   = 1
              element  = 2. "Fehler bereits oben gemerkt

*   Hauptfenster, Element Überschrift löschen
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element  = '615'
              type     = 'TOP'
              function = 'DELETE'
         EXCEPTIONS
              window   = 1
              element  = 2. "Fehler bereits oben gemerkt

*   Hauptfenster, Element Übertrag löschen
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element  = '620'
              type     = 'TOP'
              function = 'DELETE'
         EXCEPTIONS
              window   = 1
              element  = 2. "Fehler bereits oben gemerkt

*   Hauptfenster, Element Abschlußtext
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element  = '631'
              function = 'APPEND'
         EXCEPTIONS
              window   = 1
              element  = 2. "Ausgabe ist freigestellt

*   Fenster Total, Element Gesamtsumme
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              window  = 'TOTAL'
              element = '630'
         EXCEPTIONS
              window  = 1
              element = 2. "Ausgabe ist freigestellt

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
FORM avis_schliessen.

  CHECK flg_druckmodus NE 0.

* Abschluß des Formulars
  CALL FUNCTION 'CLOSE_FORM'
       IMPORTING
            result     = itcpp
       EXCEPTIONS
            send_error = 4.

  IF sy-subrc NE 0.                    "E-Mailen nicht möglich,
    fimsg-msgid = sy-msgid.            "und zum Ausdruck ist es
    fimsg-msgv1 = sy-msgv1.            "jetzt zu spät
    fimsg-msgv2 = sy-msgv2.
    fimsg-msgv3 = sy-msgv3.
    fimsg-msgv4 = sy-msgv4.
    PERFORM message USING sy-msgno.
    IF NOT reguh-pernr IS INITIAL.
      fimsg-msgv1 = reguh-pernr.
      fimsg-msgv2 = reguh-seqnr.
    ELSE.
      fimsg-msgv1 = reguh-zbukr.
      fimsg-msgv2 = reguh-vblnr.
    ENDIF.
    PERFORM message USING '388'.
  ELSE.
    CASE hlp_nacha_last.
      WHEN '1'.
        IF itcpp-tdspoolid NE 0.
          CLEAR tab_ausgabe.
          tab_ausgabe-name    = t042z-text1.
          tab_ausgabe-dataset = itcpp-tddataset.
          tab_ausgabe-spoolnr = itcpp-tdspoolid.
          tab_ausgabe-immed   = par_sofa.
          COLLECT tab_ausgabe.
        ENDIF.
      WHEN '2'.
        CLEAR tab_ausgabe.
        tab_ausgabe-name      = text_094.
        tab_ausgabe-dataset   = itcpp-tddataset.
        COLLECT tab_ausgabe.
      WHEN 'I'.
        CLEAR tab_ausgabe.
        tab_ausgabe-name      = text_095.
        tab_ausgabe-dataset   = itcpp-tddataset.
        COLLECT tab_ausgabe.
        COMMIT WORK.
    ENDCASE.
  ENDIF.

  CLEAR flg_druckmodus.

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
FORM fpaym USING event.                "FPAYM

  STATICS: up_zbukr LIKE reguh-zbukr,
           up_hbkid LIKE reguh-hbkid,
           up_rzawe LIKE reguh-rzawe,
           up_xavis LIKE reguh-xavis.

  CHECK reguh-zbukr IS INITIAL OR NOT up_xavis IS INITIAL.
  reguh-rzawe = sic_reguh-rzawe.
  reguh-zbukr = sic_reguh-zbukr.
  reguh-ubnks = sic_reguh-ubnks.
  reguh-ubnky = sic_reguh-ubnky.
  reguh-ubnkl = sic_reguh-ubnkl.
  up_xavis    = 'X'.

  IF event EQ 1.
*   Customizing nachlesen (wurde nicht AT NEW erledigt, da Felder leer)
    ON CHANGE OF reguh-zbukr.
      PERFORM buchungskreis_daten_lesen.
    ENDON.
    ON CHANGE OF reguh-ubnks OR reguh-ubnky.
      PERFORM hausbank_daten_lesen.
    ENDON.
    ON CHANGE OF reguh-zbukr OR reguh-rzawe.
      CLEAR: t042e, t042z.
      IF reguh-rzawe NE space.
        PERFORM zahlweg_daten_lesen.
      ELSE.
        regud-aust1 = t001-butxt.
        regud-aust2 = space.
        regud-aust3 = space.
        regud-austo = t001-ort01.
      ENDIF.
      t042z-text1 = text_001.
    ENDON.

*   Buchungskreis, Zahlweg, Hausbank merken für Event 3
    IF up_zbukr IS INITIAL.
      up_zbukr = reguh-zbukr.
      up_rzawe = reguh-rzawe.
      up_hbkid = reguh-hbkid.
    ELSEIF up_zbukr NE reguh-zbukr.
      up_zbukr = '*'.
      up_rzawe = '*'.
      up_hbkid = '*'.
    ENDIF.
    IF up_rzawe NE reguh-rzawe.
      up_rzawe = '*'.
    ENDIF.
    IF up_hbkid NE reguh-hbkid.
      up_hbkid = '*'.
    ENDIF.
  ENDIF.

  IF event EQ 3.
    reguh-zbukr = up_zbukr.
    reguh-rzawe = up_rzawe.
    reguh-hbkid = up_hbkid.
    IF up_hbkid EQ '*'.
      reguh-ubnks = '*'.
      reguh-ubnky = '*'.
      reguh-ubnkl = '*'.
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
FORM mail_vorbereiten USING    p_uname     LIKE sy-uname
                               p_intad     LIKE finaa-intad
                      CHANGING p_sender    LIKE swotobjid
                               p_recipient LIKE swotobjid.

* Das include <cntn01> enthält die Definitionen der Makrobefehle zum
* Anlegen und Bearbeiten der Container, d.h. für den Zugriff aufs BOR
  INCLUDE <cntn01>.

* Datendeklaration der BOR-Objekte
  DATA: sender         TYPE swc_object,
        recipient      TYPE swc_object.

* Deklaration einer Container-Datenstruktur zur Laufzeit
  swc_container container.

*----------------------------------------------------------------------*
* Anlegen eines Senders (BOR-Objekt-ID)                                *
*----------------------------------------------------------------------*

* Erzeugen einer Objektreferenz auf den Objekttyp 'RECIPIENT'
* Die weitere Verarbeitung findet dann für die Objektreferenz
* 'sender' statt
  swc_create_object sender             " Objektreferenz
                    'RECIPIENT'        " Name eines Objekttyps
                    space.             " objektspezifischer Schlüssel

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.       " Container

* Unter dem Elementnamen 'AddressString' wird die Adresse des
* aufrufenden internen Benutzers in die Container-Instanz
* container eingetragen
  swc_set_element container            " Container
                  'AddressString'      " Elementname
                  p_uname.             " Wert des Elements

* Unter dem Elementnamen 'TypeId' wird der Adreßtyp 'interner
* Benutzer' in die Container-Instanz container eingetragen
  swc_set_element container            " Container
                  'TypeId'             " Elementname
                  'B'.                 " Wert des Elements

* Aufruf der Objektmethode 'FindAddress'
  swc_call_method sender               " Objektreferenz
                  'FindAddress'        " Name der Methode
                  container.           " Container

* Fehler: Das Element ist nicht im Container enthalten
  IF sy-subrc NE 0.
    CLEAR: p_sender, p_recipient.
    fimsg-msgid = sy-msgid.
    PERFORM message USING sy-msgno.
    EXIT.
  ENDIF.

* Ermittlung der BOR-Objekt-ID
  swc_object_to_persistent sender
                           p_sender.

*----------------------------------------------------------------------*
* Anlegen eines Empfängers (BOR-Objekt-ID)                             *
*----------------------------------------------------------------------*

* Erzeugen einer Objektreferenz auf den Objekttyp 'RECIPIENT'.
* Die weitere Verarbeitung findet dann für die Objektreferenz
* 'recipient' statt
  swc_create_object recipient          " Objektreferenz
                    'RECIPIENT'        " Name eines Objekttyps
                    space.             " objektspezifischer Schlüssel

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.       " Container

* Unter dem Elementnamen 'AddressString' wird die Faxnummer bzw.
* die Internet-Adresse des Empfängers in die Container-Instanz
* container eingetragen
  swc_set_element container            " Container
                  'AddressString'      " Elementname
                  p_intad.

* Unter dem Elementnamen 'TypeId' wird der Adreßtyp 'Mail'
* in die Container-Instanz container eingetragen
  swc_set_element container            " Container
                  'TypeId'             " Elementname
                  'U'.                 " Wert des Elements

* Aufruf der Objektmethode 'CreateAddress'
  swc_call_method recipient            " Objektreferenz
                  'CreateAddress'      " Name der Methode
                  container.           " Container

* Fehler: Anlegen des Adreßteils eines Recipient-Objekts nicht möglich
  IF sy-subrc NE 0.
    CLEAR: p_sender, p_recipient.
    fimsg-msgid = sy-msgid.
    PERFORM message USING sy-msgno.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.       " Container

* Mit dem Attribut 'Deliver' wird die Empfangsbestätigung abgewählt
  swc_set_element container            " Container
                  'Deliver'            " Elementname
                  SPACE.

* Aufruf der Objektmethode 'SetDeliver'
  swc_call_method recipient            " Objektreferenz
                  'SetDeliver'         " Name der Methode
                  container.           " Container

*----------------------------------------------------------------------*

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.       " Container

* Mit dem Attribut 'NotDeliver' wird die Bestätigung bei Nicht-Empfang
* angefordert
  swc_set_element container            " Container
                  'NotDeliver'         " Elementname
                  'X'.

* Aufruf der Objektmethode 'SetDeliver'
  swc_call_method recipient            " Objektreferenz
                  'SetDeliver'         " Name der Methode
                  container.           " Container

*----------------------------------------------------------------------*

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.       " Container

* Mit dem Attribut 'Read' wird die Gelesen-Bestätigung abgewählt
  swc_set_element container            " Container
                  'Read'               " Elementname
                  SPACE.

* Aufruf der Objektmethode 'SetDeliver'
  swc_call_method recipient            " Objektreferenz
                  'SetRead'            " Name der Methode
                  container.           " Container

*----------------------------------------------------------------------*

* Ermittlung der BOR-Objekt-ID
  swc_object_to_persistent recipient
                           p_recipient.

* Initialisieren des zuvor deklarierten Containers
  swc_clear_container container.

ENDFORM.
