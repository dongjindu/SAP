***INCLUDE CORUAFWJ .

*----------------------------------------------------------------------*
*       FORM POST_GOODS_MOVEMENTS                                      *
*----------------------------------------------------------------------*
*       Warenbewegungen buchen                                         *
*----------------------------------------------------------------------*
FORM post_goods_movements.

* Felder der Struktur RESB_AFFW für RESB-Zugriff bereitstellen
  CALL FUNCTION 'DDIF_NAMETAB_GET'
       EXPORTING
            tabname   = con_tabnam_resb_affw
       TABLES
            dfies_tab = namtab.
  LOOP AT namtab.
    ftab_resb-fieldname = namtab-fieldname.
    APPEND ftab_resb.
  ENDLOOP.
  REFRESH namtab.

* Anzahl max. MSEG-Positionen im Beleg korrigieren
  IF paanzahl <= 0 OR
     paanzahl > 500.
    paanzahl = 100.
  ENDIF.

* Anzahl max. Sperren pro Beleg korrigieren
  IF pamxlock <= 0.
    pamxlock = 999.
  ENDIF.

  IF NOT sy-batch IS INITIAL AND
     modus_backgr <> '1'.
*   Verbuchungsdaten über Mermory übergeben, d.h. keine Protokollsätze
    SET UPDATE TASK LOCAL.
  ELSEIF modus_backgr = '1'.
*   Updatetask auch ausschalten, wenn Sofortbuchung aus Verbucher
    SET UPDATE TASK LOCAL.
  ENDIF.

  CLEAR: l_aufnr_old.
  REFRESH: msg_tab.
* Anzahl Durchläufe protokollieren
  n_runde = 0.
  DO.
    IF modus_backgr IS INITIAL.
      DESCRIBE TABLE paautyp LINES tabix.
      IF NOT tafwd_tab[] IS INITIAL.
      IF tabix = 0.
        IF pafehl IS INITIAL.
          IF paweonly IS INITIAL.
            SELECT * FROM affw INTO TABLE affw_tab
                   UP TO parows ROWS
                   FOR ALL ENTRIES IN tafwd_tab
                   WHERE weblnr <> space
                   AND   autyp  <> auftyp-corp
                   AND   inact = space
                   AND   werks IN pawerks
                   AND   lgort IN palgort
                   AND   wablnr = space
                   AND   ( msgno  = space OR
                         ( msgno = tafwd_tab-msgnr AND
                           msgid = tafwd_tab-arbgb ) ).
          ELSE.
            SELECT * FROM affw INTO TABLE affw_tab
                   UP TO parows ROWS
                   FOR ALL ENTRIES IN tafwd_tab
                   WHERE weblnr <> space
                   AND   autyp  <> auftyp-corp
                   AND   inact = space
                   AND   werks IN pawerks
                   AND   lgort IN palgort
                   AND   wablnr = space
                   AND   ( kzbew  = 'F' OR
                           refbln <> space )
                   AND   ( msgno  = space OR
                         ( msgno = tafwd_tab-msgnr AND
                           msgid = tafwd_tab-arbgb ) ).
          ENDIF.
        ELSE.
          IF paweonly IS INITIAL.
            SELECT * FROM affw INTO TABLE affw_tab
                   UP TO parows ROWS
                   WHERE weblnr <> space
                   AND   werks IN pawerks
                   AND   lgort IN palgort
                   AND   autyp  <> auftyp-corp
                   AND   inact = space
                   AND   wablnr = space.
          ELSE.
            SELECT * FROM affw INTO TABLE affw_tab
                   UP TO parows ROWS
                   WHERE weblnr <> space
                   AND   werks IN pawerks
                   AND   lgort IN palgort
                   AND   ( kzbew = 'F' OR
                           refbln <> space )
                   AND   autyp <> auftyp-corp
                   AND   inact = space
                   AND   wablnr = space.
          ENDIF.
        ENDIF.
      ELSE.
        IF pafehl IS INITIAL.
          IF paweonly IS INITIAL.
            SELECT * FROM affw INTO TABLE affw_tab
                   UP TO parows ROWS
                   FOR ALL ENTRIES IN tafwd_tab
                   WHERE weblnr <> space
                   AND   autyp  <> auftyp-corp
                   AND   werks IN pawerks
                   AND   lgort IN palgort
                   AND   autyp IN paautyp
                   AND   inact = space
                   AND   wablnr = space
                   AND   ( msgno  = space OR
                         ( msgno = tafwd_tab-msgnr AND
                           msgid = tafwd_tab-arbgb ) ).
          ELSE.
            SELECT * FROM affw INTO TABLE affw_tab
                   UP TO parows ROWS
                   FOR ALL ENTRIES IN tafwd_tab
                   WHERE weblnr <> space
                   AND   autyp  <> auftyp-corp
                   AND   inact = space
                   AND   autyp IN paautyp
                   AND   werks IN pawerks
                   AND   lgort IN palgort
                   AND   wablnr = space
                   AND   ( kzbew  = 'F' OR
                           refbln <> space )
                   AND   ( msgno  = space OR
                         ( msgno = tafwd_tab-msgnr AND
                           msgid = tafwd_tab-arbgb ) ).
          ENDIF.
        ELSE.
          IF paweonly IS INITIAL.
            SELECT * FROM affw INTO TABLE affw_tab
                   UP TO parows ROWS
                   WHERE weblnr <> space
                   AND   autyp  <> auftyp-corp
                   AND   inact = space
                   AND   autyp IN paautyp
                   AND   werks IN pawerks
                   AND   lgort IN palgort
                   AND   wablnr = space.
          ELSE.
            SELECT * FROM affw INTO TABLE affw_tab
                   UP TO parows ROWS
                   WHERE weblnr <> space
                   AND   ( kzbew  = 'F' OR
                           refbln <> space )
                   AND   autyp <> auftyp-corp
                   AND   inact = space
                   AND   autyp IN paautyp
                   AND   werks IN pawerks
                   AND   lgort IN palgort
                   AND   wablnr = space.
          ENDIF.
        ENDIF.
      ENDIF.
      ELSE.
        IF tabix = 0.
          IF pafehl IS INITIAL.
            IF paweonly IS INITIAL.
              SELECT * FROM affw INTO TABLE affw_tab
                     UP TO parows ROWS
                     WHERE weblnr <> space
                     AND   autyp  <> auftyp-corp
                     AND   inact = space
                     AND   werks IN pawerks
                     AND   lgort IN palgort
                     AND   wablnr = space
                     AND   msgno  = space.
            ELSE.
              SELECT * FROM affw INTO TABLE affw_tab
                     UP TO parows ROWS
                     WHERE weblnr <> space
                     AND   autyp  <> auftyp-corp
                     AND   inact = space
                     AND   werks IN pawerks
                     AND   lgort IN palgort
                     AND   wablnr = space
                     AND   ( kzbew  = 'F' OR
                             refbln <> space )
                     AND   msgno  = space .
            ENDIF.
          ELSE.
            IF paweonly IS INITIAL.
              SELECT * FROM affw INTO TABLE affw_tab
                     UP TO parows ROWS
                     WHERE weblnr <> space
                     AND   werks IN pawerks
                     AND   lgort IN palgort
                     AND   autyp  <> auftyp-corp
                     AND   inact = space
                     AND   wablnr = space.
            ELSE.
              SELECT * FROM affw INTO TABLE affw_tab
                     UP TO parows ROWS
                     WHERE weblnr <> space
                     AND   werks IN pawerks
                     AND   lgort IN palgort
                     AND   ( kzbew = 'F' OR
                             refbln <> space )
                     AND   autyp <> auftyp-corp
                     AND   inact = space
                     AND   wablnr = space.
            ENDIF.
          ENDIF.
        ELSE.
          IF pafehl IS INITIAL.
            IF paweonly IS INITIAL.
              SELECT * FROM affw INTO TABLE affw_tab
                     UP TO parows ROWS
                     WHERE weblnr <> space
                     AND   autyp  <> auftyp-corp
                     AND   inact = space
                     AND   werks IN pawerks
                     AND   lgort IN palgort
                     AND   autyp IN paautyp
                     AND   wablnr = space
                     AND   msgno  = space.
            ELSE.
              SELECT * FROM affw INTO TABLE affw_tab
                     UP TO parows ROWS
                     WHERE weblnr <> space
                     AND   autyp  <> auftyp-corp
                     AND   inact = space
                     AND   autyp IN paautyp
                     AND   werks IN pawerks
                     AND   lgort IN palgort
                     AND   wablnr = space
                     AND   ( kzbew  = 'F' OR
                             refbln <> space )
                     AND   msgno  = space.
            ENDIF.
          ELSE.
            IF paweonly IS INITIAL.
              SELECT * FROM affw
                     INTO CORRESPONDING FIELDS OF TABLE affw_tab
                     UP TO parows ROWS
                     WHERE weblnr <> space
                     AND   autyp  <> auftyp-corp
                     AND   inact = space
                     AND   autyp IN paautyp
                     AND   werks IN pawerks
                     AND   lgort IN palgort
                     AND   wablnr = space.
            ELSE.
              SELECT * FROM affw INTO TABLE affw_tab
                     UP TO parows ROWS
                     WHERE weblnr <> space
                     AND   ( kzbew  = 'F' OR
                             refbln <> space )
                     AND   autyp <> auftyp-corp
                     AND   inact = space
                     AND   autyp IN paautyp
                     AND   werks IN pawerks
                     AND   lgort IN palgort
                     AND   wablnr = space.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF pafehl IS INITIAL.
*     abhängige Warenbewegungen komplettieren, evtl. sind WA-Positionen
*     übergeordneten, nicht selektierten Aufträgen zugeordnet
*     nur notwendig, wenn nur fehlerfreie Positionen bearbeitet werden
      REFRESH refbln_tab.
      LOOP AT affw_tab WHERE refbln <> space.
        READ TABLE refbln_tab WITH KEY weblnr = affw_tab-weblnr
                                       BINARY SEARCH.
        IF sy-subrc <> 0.
          refbln_tab-weblnr = affw_tab-weblnr.
          INSERT refbln_tab INDEX sy-tabix.
        ENDIF.
        DELETE affw_tab.
      ENDLOOP.
      IF NOT refbln_tab[] IS INITIAL.
        SELECT * FROM affw APPENDING TABLE affw_tab
               FOR ALL ENTRIES IN refbln_tab
               WHERE weblnr = refbln_tab-weblnr
               AND   inact  = space
               AND   refbln NE space.
        REFRESH refbln_tab.
      ENDIF.
    ENDIF.

    DESCRIBE TABLE affw_tab LINES tabix.
    IF n_runde > 0 AND
      tabix = 0.
*     Bei Folgedurchlauf keine Fehlersätze gefunden
      EXIT.
    ENDIF.
*   Anzahl Durchläufe aktualisieren
    n_runde = n_runde + 1.
    n_total = tabix.

*   Auftragsstatus für Entnahmepositionen prüfen
    WHILE tabix > 0 AND
          pasimul IS INITIAL.
      READ TABLE affw_tab INDEX tabix.
      IF NOT affw_tab-dnotw IS INITIAL.
        CLEAR affw_tab-dnotw.
        MODIFY affw_tab INDEX tabix TRANSPORTING dnotw.
      ENDIF.
      IF l_aufnr_old <> affw_tab-aufnr.
*       Auftragsstatus auf Entnahme prüfen
        CLEAR flg_wrong_status.
        CLEAR subrc2.
        l_aufnr_old = affw_tab-aufnr.
        CALL FUNCTION 'CO_SF_HEADER_GOODS_ISSUE'
             EXPORTING
                aufnr        = affw_tab-aufnr
             EXCEPTIONS
                not_found    = 1
                not_activ    = 2
                wrong_status = 3
                wrong_type   = 4.
        IF NOT sy-subrc IS INITIAL.
          flg_wrong_status = yx.
          subrc2 = sy-subrc.
        ELSE.
*         Bei Auftragsnetzen im Falle des WE auch den
*         übergeordneten Auftrag auf Status prüfen.
          CLEAR l_maufnr.
          IF NOT affw_tab-kzbew  IS INITIAL AND
             NOT affw_tab-refbln IS INITIAL AND
             NOT affw_tab-aufnr  IS INITIAL.
            SELECT SINGLE maufnr FROM afko INTO l_maufnr
                  WHERE aufnr = affw_tab-aufnr.
            IF NOT l_maufnr IS INITIAL.
              CALL FUNCTION 'CO_SF_HEADER_GOODS_ISSUE'
                   EXPORTING aufnr = l_maufnr
                   EXCEPTIONS
                      not_found    = 1
                      not_activ    = 2
                      wrong_status = 3
                      wrong_type   = 4.
              IF NOT sy-subrc IS INITIAL.
*               Der Status des übergeordneten Auftrags erlaubt keinen
*               WA, daher muss auch dieser WE einen Fehler erhalten.
                IF 1 = 2. MESSAGE e523(ru). ENDIF.
                affw_dnotw_tab = affw_tab.
                affw_dnotw_tab-msgid = 'RU'.
                affw_dnotw_tab-msgty = 'E'.
                affw_dnotw_tab-msgno = '523'.
                affw_dnotw_tab-fwdat = sy-datum.
                affw_dnotw_tab-fwzet = sy-uzeit.
                CLEAR: affw_dnotw_tab-msgv1,
                       affw_dnotw_tab-msgv2,
                       affw_dnotw_tab-msgv3,
                       affw_dnotw_tab-msgv4.
                APPEND affw_dnotw_tab.
*               AFFW-Satz kann gelöscht werden.
                DELETE affw_tab INDEX tabix.
                tabix = tabix - 1.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF NOT flg_wrong_status IS INITIAL
         AND affw_tab-kzbew IS INITIAL.
*       AFFW-Satz kopieren.
        affw_dnotw_tab = affw_tab.
        CASE subrc2.
*         unzulässiger Status -> Fehlerinformation fortschreiben
          WHEN '1'.                             "Auftrag existiert nicht
            IF 1 = 2. MESSAGE e001(c6) WITH space. ENDIF.
            affw_dnotw_tab-msgid = 'C6'.
            affw_dnotw_tab-msgty = 'E'.
            affw_dnotw_tab-msgno = '001'.
            affw_dnotw_tab-msgv1 = affw_tab-aufnr.
            affw_dnotw_tab-fwdat = sy-datum.
            affw_dnotw_tab-fwzet = sy-uzeit.
          WHEN '2'.                             "Auftrag ist gelöscht
            IF 1 = 2. MESSAGE e002(c6) WITH space. ENDIF.
            affw_dnotw_tab-msgid = 'C6'.
            affw_dnotw_tab-msgty = 'E'.
            affw_dnotw_tab-msgno = '002'.
            affw_dnotw_tab-msgv1 = affw_tab-aufnr.
            affw_dnotw_tab-fwdat = sy-datum.
            affw_dnotw_tab-fwzet = sy-uzeit.
          WHEN '3'.                             "Keine Entnahmen erlaubt
            IF affw_tab-autyp = auftyp-netw. "Netzpläne
              IF 1 = 2. MESSAGE e014(c6) WITH space. ENDIF.
              affw_dnotw_tab-msgid = 'C6'.
              affw_dnotw_tab-msgty = 'E'.
              affw_dnotw_tab-msgno = '014'.
              affw_dnotw_tab-msgv1 = affw_tab-aufnr.
              affw_dnotw_tab-fwdat = sy-datum.
              affw_dnotw_tab-fwzet = sy-uzeit.
            ELSE.                            "Aufträge
              IF 1 = 2. MESSAGE e013(c6) WITH space. ENDIF.
              affw_dnotw_tab-msgid = 'C6'.
              affw_dnotw_tab-msgty = 'E'.
              affw_dnotw_tab-msgno = '013'.
              affw_dnotw_tab-msgv1 = affw_tab-aufnr.
              affw_dnotw_tab-fwdat = sy-datum.
              affw_dnotw_tab-fwzet = sy-uzeit.
            ENDIF.
          WHEN '4'.                             "Falscher Auftragstyp
            IF 1 = 2. MESSAGE e003(c6) WITH space. ENDIF.
            affw_dnotw_tab-msgid = 'C6'.
            affw_dnotw_tab-msgty = 'E'.
            affw_dnotw_tab-msgno = '003'.
            affw_dnotw_tab-msgv1 = affw_tab-aufnr.
            affw_dnotw_tab-fwdat = sy-datum.
            affw_dnotw_tab-fwzet = sy-uzeit.
        ENDCASE.
        CLEAR: affw_dnotw_tab-msgv2,
               affw_dnotw_tab-msgv3,
               affw_dnotw_tab-msgv4.
        APPEND affw_dnotw_tab.
*       AFFW-Satz kann gelöscht werden.
        DELETE affw_tab INDEX tabix.
*       WM-Chargenfindung braucht nun auch nicht mehr aufgerufen werden
        tabix = tabix - 1.
        CONTINUE.
      ENDIF.

*     Chargensplitt im WM für chargenpflichtige Materialien
      IF affw_tab-erfmg IS INITIAL AND
         affw_tab-kzear IS INITIAL AND
         affw_tab-elikz IS INITIAL.
*       keine Warenbewegung ist notwendig, da weder eine Menge
*       noch das Kennzeichen Endausgefasst vorhanden ist
        IF NOT affw_tab-msgno IS INITIAL.
*         Sätze mit Dummy-Fehlern sind nicht zu sperren
          tafwd_tab-arbgb = affw_tab-msgid.
          tafwd_tab-msgnr = affw_tab-msgno.
          READ TABLE tafwd_tab WITH KEY arbgb = tafwd_tab-arbgb
                                        msgnr = tafwd_tab-msgnr
                                        BINARY SEARCH.
          IF sy-subrc <> 0.
*           Fehlersatz sperren
            CALL FUNCTION 'ENQUEUE_ESAFFW'
                 EXPORTING
                      weblnr  = affw_tab-weblnr
                      weblpos = affw_tab-weblpos
                      _scope  = '3'  "Freigabe Sperre bei Ende TA & VB
                 EXCEPTIONS
                      OTHERS  = 01.
            IF sy-subrc = 0.
*             Fehlersatz nachlesen
              SELECT SINGLE * FROM affw INTO affw
                       WHERE weblnr  = affw_tab-weblnr
                       AND   weblpos = affw_tab-weblpos
                       AND   inact   = space.
              IF sy-subrc = 0 AND
                 affw_tab <> affw.
*               für Fehlersatz ist die AFFW-Sperre zurückzunehmen
                PERFORM dequeue_esaffw USING affw_tab-weblnr
                                             affw_tab-weblpos
                                             affw_tab-msgid
                                             affw_tab-msgno.
                sy-subrc = 4.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        IF sy-subrc = 0.
          APPEND affw_tab TO affw_del_tab.
        ENDIF.
        DELETE affw_tab INDEX tabix.
        tabix = tabix - 1.
        CONTINUE.
      ENDIF.

*     ursprüngliche Fehlermeldungen retten, wird benötigt für
*     Entsperren des AFFW-Satzes in Form RECEIVE_RESULT
      MOVE-CORRESPONDING affw_tab TO msg_tab.                 "#EC ENHOK
      APPEND msg_tab.
      l_batch_det = ' '.  "<--- batch determination switch
*     set switch as 'X' -> determination will be carried out
*     set switch as ' ' -> determination is no longer carried out
*     auf Chargenpflicht prüfen
      CALL FUNCTION 'MARC_SINGLE_READ'
          EXPORTING
              matnr             = affw_tab-matnr
              werks             = affw_tab-werks
          IMPORTING
              wmarc             = ls_marc
          EXCEPTIONS
              lock_on_marc      = 1
              lock_system_error = 2
              wrong_call        = 3
              not_found         = 4.
      IF NOT sy-subrc IS INITIAL.
        CLEAR ls_marc.
      ENDIF.
*     WM-Chargenfindung nur aufrufen, wenn das Material
*     chargenpflichtig mit initialer Charge oder nicht
*     chargenpflichtig mit initialer Bewertungsart ist
      IF NOT ls_marc-xchar IS INITIAL AND
        ( ( affw_tab-charg IS INITIAL AND
         NOT ls_marc-xchpf IS INITIAL ) OR
          ( affw_tab-bwtar IS INITIAL AND
             ls_marc-xchpf IS INITIAL ) ) AND
         NOT affw_tab-erfmg IS INITIAL AND
         NOT affw_tab-lgnum IS INITIAL AND
         NOT affw_tab-rsnum IS INITIAL AND
         NOT affw_tab-rspos IS INITIAL AND
         NOT l_batch_det    IS INITIAL.
        subrc = 0.
        IF NOT affw_tab-msgno IS INITIAL.
*         Sätze mit Dummy-Fehlern sind nicht zu sperren
          tafwd_tab-arbgb = affw_tab-msgid.
          tafwd_tab-msgnr = affw_tab-msgno.
          READ TABLE tafwd_tab WITH KEY arbgb = tafwd_tab-arbgb
                                        msgnr = tafwd_tab-msgnr
                                        BINARY SEARCH.
          IF sy-subrc <> 0.
*           Fehlersatz sperren
            CALL FUNCTION 'ENQUEUE_ESAFFW'
                 EXPORTING
                      weblnr  = affw_tab-weblnr
                      weblpos = affw_tab-weblpos
                      _scope  = '3'  "Freigabe Sperre bei Ende TA & VB
                 EXCEPTIONS
                      OTHERS  = 01.
            IF sy-subrc = 0.
*             Fehlersatz nachlesen
              SELECT SINGLE * FROM affw INTO affw
                       WHERE weblnr  = affw_tab-weblnr
                       AND   weblpos = affw_tab-weblpos
                       AND   inact   = space.
              IF sy-subrc = 0 AND
                 affw_tab <> affw.
*               für Fehlersatz ist die AFFW-Sperre zurückzunehmen
                PERFORM dequeue_esaffw USING affw_tab-weblnr
                                             affw_tab-weblpos
                                             affw_tab-msgid
                                             affw_tab-msgno.
                sy-subrc = 4.
              ENDIF.
            ENDIF.
            subrc = sy-subrc.
          ENDIF.
        ENDIF.
*       nur Reservierungen sperren, bei denen nicht storniert wird
*       wegen Buchung über MB_CANCEL_GOODS_MOVEMENT
        IF subrc = 0 AND affw_tab-smbln IS INITIAL.
          READ TABLE enq_rsnum WITH KEY rsnum = affw_tab-rsnum
                                        BINARY SEARCH.
          tabix_i = sy-tabix.
          IF sy-subrc <> 0.
*           Reservierungskopf sperren
            CALL FUNCTION 'ENQUEUE_EMRKPF'
                 EXPORTING
                      rsnum  = affw_tab-rsnum
                      _scope  = '3'  "Freigabe Sperre bei Ende TA & VB
                 EXCEPTIONS
                      OTHERS = 1.
            subrc = sy-subrc.
            IF sy-subrc = 0.
              enq_rsnum-rsnum = affw_tab-rsnum.
              enq_rsnum-count = 1.
              INSERT enq_rsnum INDEX tabix_i.
            ELSE.
*             bei Fehlersätzen ist die AFFW-Sperre zurückzunehmen
              PERFORM dequeue_esaffw USING affw_tab-weblnr
                                           affw_tab-weblpos
                                           affw_tab-msgid
                                           affw_tab-msgno.
            ENDIF.
          ELSE.
            enq_rsnum-count = enq_rsnum-count + 1.
            MODIFY enq_rsnum INDEX tabix_i.
          ENDIF.
        ENDIF.
        IF subrc <> 0.
*         Fehlersatz oder Reservierung bereits gesperrt.
*         --> Fehlersatz wird nicht berücksichtigt.
          n_locked = n_locked + 1.
          DELETE affw_tab INDEX tabix.
          tabix = tabix - 1 .
        ENDIF.
        CHECK subrc = 0.
*       Reservierung bereitstellen
        SELECT SINGLE * FROM resb WHERE rsnum = affw_tab-rsnum
                                  AND   rspos = affw_tab-rspos
                                  AND   rsart = affw_tab-rsart.
        subrc = sy-subrc.

        IF sy-subrc = 0 AND
           resb-splkz = '1'.
*         Entnahme zum Summensplittsatz nicht erlaubt
          subrc  = 4.
          n_fail = n_fail + 1.
          DELETE affw_tab INDEX tabix.
          affw_dnotw_tab = affw_tab.
          IF affw_dnotw_tab-fwdat IS INITIAL  OR
             affw_dnotw_tab-msgid <> 'RU' OR
             affw_dnotw_tab-msgno <> '496'.
            affw_dnotw_tab-fwdat = sy-datum.
            affw_dnotw_tab-fwzet = sy-uzeit.
          ENDIF.
          IF 1 = 2. MESSAGE e496(ru). ENDIF.                      "#EC *
          affw_dnotw_tab-msgid = 'RU'.
          affw_dnotw_tab-msgty = 'E'.
          affw_dnotw_tab-msgno = '496'.
          affw_dnotw_tab-msgv1 = affw_tab-rueck.
          affw_dnotw_tab-msgv2 = affw_tab-rmzhl.
          affw_dnotw_tab-msgv3 = affw_tab-aufnr.
          CLEAR affw_dnotw_tab-msgv4.
          affw_dnotw_tab-aenam = sy-uname.
          affw_dnotw_tab-laeda = sy-datum.
          APPEND affw_dnotw_tab.
        ELSEIF sy-subrc = 0 AND
           resb-berkz <> '0' AND
           resb-kzech <> '1' AND
           resb-kzech <> '2' AND
           resb-kzech <> '3'.
*         Chargenfindung erfolgt über WM
          MOVE-CORRESPONDING resb TO tmp_lresb.               "#EC ENHOK
          MOVE-CORRESPONDING affw_tab TO tmp_lresb.           "#EC ENHOK
          IF affw_tab-erfme = resb-meins.
            tmp_lresb-bdmng = affw_tab-erfmg.
          ELSE.
*           Entnahmemenge in Basiseinheit umrechnen
            CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
                 EXPORTING
                      input   = affw_tab-erfmg
                      kzmeinh = yx
                      matnr   = affw_tab-matnr
                      meinh   = affw_tab-erfme
                      meins   = resb-meins
                 IMPORTING
                      output  = tmp_lresb-bdmng
                 EXCEPTIONS
                      OTHERS  = 1.
            IF sy-subrc <> 0.
              tmp_lresb-bdmng = 0.
            ENDIF.
          ENDIF.
*         WM-Chargenfindung
          CALL FUNCTION 'L_PPIM_BATCH_DETERMINATION_INT'
               EXPORTING
                    i_lresb       = tmp_lresb
               TABLES
                    t_bdbatch     = bdbatch_tab
               EXCEPTIONS
                    error_message = 2
                    OTHERS        = 1.
          IF sy-subrc <> 0.
            n_fail = n_fail + 1.
*           die Sperre auf den Reservierungskopf ist zurückzunehmen
            PERFORM dequeue_emrkpf USING affw_tab-rsnum.
*           AFFW-Satz komplett für Update einlesen
            SELECT SINGLE * FROM affw INTO affw_dnotw_tab
                                 WHERE weblnr  = affw_tab-weblnr
                                 AND   weblpos = affw_tab-weblpos.
            CHECK sy-subrc = 0.
            affw_dnotw_tab = affw_tab.
            DELETE affw_tab INDEX tabix.
            IF affw_dnotw_tab-fwdat IS INITIAL  OR
               affw_dnotw_tab-msgid <> sy-msgid OR
               affw_dnotw_tab-msgno <> sy-msgno.
              affw_dnotw_tab-fwdat = sy-datum.
              affw_dnotw_tab-fwzet = sy-uzeit.
            ENDIF.
            affw_dnotw_tab-msgid = sy-msgid.
            affw_dnotw_tab-msgty = sy-msgty.
            affw_dnotw_tab-msgno = sy-msgno.
            affw_dnotw_tab-msgv1 = sy-msgv1.
            affw_dnotw_tab-msgv2 = sy-msgv2.
            affw_dnotw_tab-msgv3 = sy-msgv3.
            affw_dnotw_tab-msgv4 = sy-msgv4.
            affw_dnotw_tab-aenam = sy-uname.
            affw_dnotw_tab-laeda = sy-datum.
            APPEND affw_dnotw_tab.
          ELSE.
*           Setzen der lokalen Sperren
            CALL FUNCTION 'BF_SET_LOC_ENQ_TAB'
                 EXPORTING
                      vbeln     = affw_tab-kdauf
                      posnr     = affw_tab-kdpos
                      pspnr     = affw_tab-ps_psp_pnr
                 TABLES
                      l_bdbatch = bdbatch_tab.
*           Übernahme der Chargen
            menge = affw_tab-erfmg.
            affw_tab-dnotw = minus.
            DESCRIBE TABLE bdbatch_tab LINES tabix_i.
            IF tabix_i = 0.
              MODIFY affw_tab INDEX tabix.
            ELSE.
              DELETE affw_tab INDEX tabix.
              LOOP AT bdbatch_tab.
                IF sy-tabix > 1.
*                 für Insert-Sätze Original AFFW nachlesen, ansonsten
*                 bleiben bestimmte Felder in COGI leer
                  SELECT SINGLE * FROM affw
                         INTO CORRESPONDING FIELDS OF affw_tab
                         WHERE weblnr  = affw_tab-weblnr
                           AND weblpos = affw_tab-weblpos.
*                 neue Belegposition vergeben, falls die Warenbewegung
*                 schiefgeht
                  affw-weblnr = affw_tab-weblnr.
                  PERFORM find_new_weblpos USING affw-weblnr
                                                 affw-weblpos.
                  affw_tab-weblpos = affw-weblpos.
                  affw_tab-dnotw   = plus.
                ENDIF.
                IF NOT ls_marc-xchpf IS INITIAL.
                  affw_tab-charg = bdbatch_tab-charg.
                ELSE.
                  affw_tab-bwtar = bdbatch_tab-charg.
                ENDIF.
                affw_tab-erfmg = bdbatch_tab-erfmg.
                affw_tab-erfme = bdbatch_tab-erfme.
                INSERT affw_tab INDEX tabix.
                tabix = tabix + 1.
                menge = menge - bdbatch_tab-erfmg.
              ENDLOOP.
              IF menge > 0.
*               für Insert-Sätze Original AFFW nachlesen, ansonsten
*               bleiben bestimmte Felder in COGI leer
                SELECT SINGLE * FROM affw
                       INTO CORRESPONDING FIELDS OF affw_tab
                       WHERE weblnr  = affw_tab-weblnr
                         AND weblpos = affw_tab-weblpos.
*               neue Belegposition vergeben, falls die Warenbewegung
*               schiefgeht
                affw-weblnr = affw_tab-weblnr.
                PERFORM find_new_weblpos USING affw-weblnr
                                               affw-weblpos.
                affw_tab-weblpos = affw-weblpos.
                affw_tab-dnotw   = plus.
                CLEAR affw_tab-charg.
                affw_tab-erfmg = menge.
                affw_tab-erfme = bdbatch_tab-erfme.
                INSERT affw_tab INDEX tabix.
              ENDIF.
              tabix = tabix - tabix_i.
            ENDIF.
          ENDIF.                       "Splitt erfolgreich ?
        ELSEIF sy-subrc <> 0.
*         Reservierung nicht vorhanden
          n_fail = n_fail + 1.
          affw_dnotw_tab = affw_tab.
          DELETE affw_tab INDEX tabix.
          IF affw_dnotw_tab-fwdat IS INITIAL OR
             affw_dnotw_tab-msgid <> 'M7'    OR
             affw_dnotw_tab-msgno <> '520'.
            affw_dnotw_tab-fwdat = sy-datum.
            affw_dnotw_tab-fwzet = sy-uzeit.
          ENDIF.
          IF 1 = 2. MESSAGE e520(m7). ENDIF.                      "#EC *
          affw_dnotw_tab-msgid = 'M7'.
          affw_dnotw_tab-msgty = 'E'.
          affw_dnotw_tab-msgno = '520'.
          affw_dnotw_tab-msgv1 = affw_tab-rsnum.
          CLEAR: affw_dnotw_tab-msgv2,
                 affw_dnotw_tab-msgv3,
                 affw_dnotw_tab-msgv4.
          affw_dnotw_tab-aenam = sy-uname.
          affw_dnotw_tab-laeda = sy-datum.
          APPEND affw_dnotw_tab.
        ENDIF.                         "Reservierung gelesen
      ENDIF.                           "LGNUM ohne Charge
      tabix = tabix - 1.
    ENDWHILE.

    IF NOT affw_dnotw_tab[] IS INITIAL.
*     Update AFFW-Sätze
      LOOP AT affw_dnotw_tab.
        MOVE-CORRESPONDING affw_dnotw_tab
                        TO affwb_tab.
*       bei Sätzen, die über die AFFWV0 eingelesen wurden
*       nur eingeschränkter Update.
        IF affwb_tab-ernam IS INITIAL.
          affwb_tab-vbkz = vbkz_null.
        ELSE.
          affwb_tab-vbkz = vbkz_upda.
        ENDIF.
        APPEND affwb_tab.
      ENDLOOP.
    ENDIF.
    IF NOT affw_del_tab[] IS INITIAL.
*     Nachlesen Fehlersätze
      SELECT * FROM affw INTO TABLE affw_dnotw_tab
               FOR ALL ENTRIES IN affw_del_tab
               WHERE weblnr  = affw_del_tab-weblnr
               AND   weblpos = affw_del_tab-weblpos.
*     Löschen AFFW-Sätze
      LOOP AT affw_dnotw_tab.
        MOVE-CORRESPONDING affw_dnotw_tab
                        TO affwb_tab.
        affwb_tab-vbkz = vbkz_del.
        APPEND affwb_tab.
      ENDLOOP.
    ENDIF.

    IF NOT affwb_tab[] IS INITIAL.
      CALL FUNCTION 'CO_FW_AFFW_POST' IN UPDATE TASK
           EXPORTING
                i_called = 'C'
           TABLES
                affw_bt = affwb_tab.
      IF NOT sy-batch IS INITIAL OR
         modus_backgr = '1'.
*       Verbuchungsdaten über Memory übergeben, d.h. kein WAIT
        COMMIT WORK.
      ELSE.
*       Updatetask aktiv, d.h. WAIT ist notwendig
        COMMIT WORK AND WAIT.
      ENDIF.

      SORT affw_tab BY weblnr weblpos.
      LOOP AT affwb_tab.
*       Original-Fehlermeldung ist für Entsperren zu verwenden
        READ TABLE affw_tab WITH KEY mandt   = sy-mandt
                                     weblnr  = affwb_tab-weblnr
                                     weblpos = affwb_tab-weblpos
                                     BINARY SEARCH.
        CHECK sy-subrc = 0.

        DELETE affw_tab INDEX sy-tabix.

*       bei Fehlersätzen ist die AFFW-Sperre zurückzunehmen
        PERFORM dequeue_esaffw USING affw_tab-weblnr
                                     affw_tab-weblpos
                                     affw_tab-msgid
                                     affw_tab-msgno.
      ENDLOOP.
      REFRESH: affw_dnotw_tab,
               affw_del_tab.
      REFRESH: affwb_tab.
    ENDIF.

*   Tabelle der urpsrüngl. Fehlermeldungen für Binary Search sortieren
    SORT msg_tab BY weblnr weblpos.

*   wegen CO27 / CO11 - Unterscheidung WABLNR für Sortierung kurzfristig
    affw_tab-wablnr = yx.
    MODIFY affw_tab TRANSPORTING wablnr WHERE rueck IS INITIAL.

*   Sortieren nach Auftragstyp, Kennzeichen mit/ohne Rückmeldung,
*                  Storno-Materialbeleg und Jahr,
*                  Buchungs-, Belegdatum,
*                  Werk, Materialnummer,
*                  Reservierungsnum., Reservierungsposition,
*                  Auftragsnummer, (bei ungeplanter Entnahme, RSNUM=0)
*                  Rückmeldenummer und -zähler, Erfassungsdatum,
*                  Q-Kennzeichen, Soll/Haben-Kennzeichen.
*   Kein Sortierung mehr nach Lagerort und Charge, um Ergebnisse der
*   Bestands- und Chargenfindung nicht zu verwerfen
    SORT affw_tab STABLE BY autyp wablnr
                            smbln sjahr
                            budat bldat
                            werks matnr
                            rsnum rspos rsart
                            aufnr rueck rmzhl
                            ersda erzet
                            insmk shkzg.

*   wegen CO27 / CO11 - Unterscheidung WABLNR zurücksetzen
    CLEAR affw_tab-wablnr.
    MODIFY affw_tab TRANSPORTING wablnr
                    WHERE NOT wablnr IS INITIAL.

    REFRESH ind_tab.
    LOOP AT affw_tab.
      IF affw_tab-erfmg = 0.
        APPEND affw_tab TO affw_del_tab.
        CONTINUE.
      ENDIF.
      ind_tab-weblnr  = affw_tab-weblnr.
      ind_tab-weblpos = affw_tab-weblpos.
      ind_tab-tabix = sy-tabix.
      APPEND ind_tab.
*     Lagerort HUM-pflichtig?
      CLEAR flg_hupf.
      READ TABLE t001l_tab WITH KEY werks = affw_tab-werks
                                    lgort = affw_tab-lgort
                                    BINARY SEARCH
                                    TRANSPORTING xhupf.
      IF sy-subrc IS INITIAL.
        flg_hupf = t001l_tab-xhupf.
      ELSE.
        l_tabix = sy-tabix.
        SELECT SINGLE werks lgort xhupf FROM t001l
                INTO CORRESPONDING FIELDS OF t001l_tab
                    WHERE werks = affw_tab-werks
                      AND lgort = affw_tab-lgort.
        IF sy-subrc IS INITIAL.
          INSERT t001l_tab INDEX l_tabix.
          flg_hupf = t001l_tab-xhupf.
        ENDIF.
      ENDIF.
      IF affw_tab-refbln IS INITIAL.
*       HUM-pflichtige in eigenen Belegen buchen, zuerst Storno
        IF NOT flg_hupf IS INITIAL AND NOT affw_tab-sjahr IS INITIAL.
          hucanc_tab-tabix = ind_tab-tabix.
          APPEND hucanc_tab.
        ELSEIF NOT flg_hupf IS INITIAL AND affw_tab-sjahr IS INITIAL.
          hum_tab-tabix = ind_tab-tabix.
          APPEND hum_tab.
        ELSEIF flg_hupf IS INITIAL AND NOT affw_tab-sjahr IS INITIAL.
*         alle Stornos zu einem Ursprungsbeleg in einem Beleg buchen
          canc_tab-tabix = ind_tab-tabix.
          APPEND canc_tab.
        ELSEIF flg_hupf IS INITIAL AND affw_tab-sjahr IS INITIAL.
          IF affw_tab-kzbew IS INITIAL.
            wa_tab-tabix = ind_tab-tabix.
            APPEND wa_tab.
          ELSE.
            we_tab-tabix = ind_tab-tabix.
            APPEND we_tab.
          ENDIF.
        ENDIF.
      ELSE.
        APPEND affw_tab TO ref_tab.
      ENDIF.
      IF affw_tab-sjahr > 0.
        mseg_key-mblnr = affw_tab-smbln.
        mseg_key-mjahr = affw_tab-sjahr.
        mseg_key-zeile = affw_tab-smblp.
        APPEND mseg_key.
      ENDIF.
    ENDLOOP.
    SORT ind_tab BY weblnr weblpos.

    IF NOT affw_del_tab[] IS INITIAL.
      CALL FUNCTION 'CO_FW_DELETE_AND_GM_CLOSURE'
           EXPORTING
                i_modus = modus_backgr
           TABLES
                it_affw = affw_del_tab.
      CLEAR affw_del_tab.
      REFRESH affw_del_tab.
    ENDIF.

    tabix = 0.
    IF NOT mseg_key[] IS INITIAL.
      SELECT mblnr mjahr zeile sakto dmbtr exbwr bstmg
                               bwart menge meins FROM mseg
             APPENDING CORRESPONDING FIELDS
             OF TABLE bfwrt_tab
             FOR ALL ENTRIES IN mseg_key
             WHERE mblnr = mseg_key-mblnr
             AND   mjahr = mseg_key-mjahr
             AND   zeile = mseg_key-zeile.
    ENDIF.
    SORT bfwrt_tab BY mblnr mjahr zeile.
    DELETE ADJACENT DUPLICATES FROM bfwrt_tab.

*   freier Prozesse
    IF parallel <> 0.
      frei_pr = parallel.
    ENDIF.

************************************************************************
* Aufruf der Bestandsführung
************************************************************************

    paanzahl = paanzahl - 1.
    pamxlock = pamxlock - 1.
*   Tasknam initialisieren
    CLEAR tasknam.
    tasknam(1) = '2'.

    CLEAR loop_cntr.
*   zuerst die kombinierten Warenbewegungen (WE+WA im Auft.netz) buchen
    SORT ref_tab BY refbln.
    WHILE NOT ref_tab[] IS INITIAL.
      loop_cntr = loop_cntr + 1.
      tabix_wbtyp = 1.
      READ TABLE ref_tab INTO affw_tab INDEX tabix_wbtyp.
      bldat_old  = affw_tab-bldat.
      budat_old  = affw_tab-budat.
      werks_old  = affw_tab-werks.
      sjahr_old  = affw_tab-sjahr.
      autyp_old  = affw_tab-autyp.
      rueck_old  = affw_tab-rueck.
      refbln_old = affw_tab-refbln.
      DO.
        READ TABLE ref_tab INTO affw_tab INDEX tabix_wbtyp.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF affw_tab-refbln IS INITIAL.
*         Darf nicht sein, falls doch: Verarbeitung abbrechen
          REFRESH ref_tab.
          EXIT.
        ENDIF.
        tabix_act = sy-tabix.
        subrc = 0.
*       AFFWB_TAB füllen und ggf neue Tasks starten
        PERFORM fill_affwb_tab.
*       Wurde bereits vorab bei einer Warenbewegungsposition ein
*       Problem festgestellt, so darf keine Warenbewegung mit diesem
*       Referenzbeleg gebucht werden
        IF subrc <> 0.
          REFRESH affwb_tab.
          IF subrc = 8.
*           Wiederholversuch für die Warenbewegungen ist nicht möglich,
*           deshalb sind alle Positionen zu dem Referenzbeleg zu löschen
            LOOP AT ref_tab WHERE refbln = refbln_old.
              DELETE ref_tab.
              tabix_wbtyp = tabix_wbtyp - 1.
            ENDLOOP.
            READ TABLE ref_tab INDEX tabix_wbtyp.
            IF sy-subrc = 0.
              refbln_old = ref_tab-refbln.
            ENDIF.
          ELSE.
*           Wiederholversuch möglich, da nur Sperrproblem
*           auf folgendem Eintrag mit abweichender REFBLN positionieren
            DO.
              READ TABLE ref_tab INDEX tabix_wbtyp.
              IF sy-subrc <> 0.
                EXIT.
              ENDIF.
              IF ref_tab-refbln <> refbln_old.
                refbln_old = ref_tab-refbln.
                EXIT.
              ENDIF.
              tabix_wbtyp = tabix_wbtyp + 1.
            ENDDO.
          ENDIF.
        ENDIF.
      ENDDO.
      IF NOT affwb_tab[] IS INITIAL.
        PERFORM call_mb_create.
        FREE:  affwb_tab,
               imsegb_tab.
        CLEAR: imsegb_tab.
      ENDIF.
      tabix_para = 0.
      tabix_lock = 0.

      IF modus_backgr <> '1'.
*       Abschlußprotokoll erst ausgeben, wenn alle asynchronen Tasks
*       sich zurückgemeldet haben
        IF parallel <> 0.
          WAIT UNTIL parallel <= frei_pr.
        ELSE.
          WAIT UNTIL acti_pr <= 0.
        ENDIF.
      ELSE.
        IF loop_cntr > 4.
*         Wenn Aufruf aus Verbucher, dann maximal 5 Schleifendurchläufe
          EXIT.
        ENDIF.
        IF NOT ref_tab[] IS INITIAL.
          WAIT UP TO loop_cntr SECONDS.
        ENDIF.
      ENDIF.
    ENDWHILE.
    CLEAR refbln_old.

    CLEAR loop_cntr.
*   danach die Stornos buchen: immer ein Storno-Beleg für alle
*   Stornos zu einem Urpsrungsbeleg wegen Sperrproblem mit
*   MB_CANCEL_GOODS_MOVEMENT
    WHILE NOT canc_tab[] IS INITIAL.
      loop_cntr = loop_cntr + 1.
      tabix_wbtyp = 1.
      READ TABLE canc_tab INDEX tabix_wbtyp.
      READ TABLE affw_tab INDEX canc_tab-tabix.
*     Ursprungsbeleg merken
      sjahr_old = affw_tab-sjahr.
      smbln_old = affw_tab-smbln.
      DO.
        READ TABLE canc_tab INDEX tabix_wbtyp.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        READ TABLE affw_tab INDEX canc_tab-tabix.
        IF sy-subrc <> 0.
*         Darf nicht sein, falls doch: Verarbeitung abbrechen
          REFRESH canc_tab.
          EXIT.
        ENDIF.
        tabix_act = canc_tab-tabix.
        subrc = 0.
*       AFFWB_TAB füllen und ggf neue Tasks starten
        PERFORM fill_affwb_tab.
      ENDDO.
      IF NOT affwb_tab[] IS INITIAL.
        PERFORM call_mb_create.
        FREE:  affwb_tab,
               imsegb_tab.
        CLEAR: imsegb_tab.
      ENDIF.
      tabix_para = 0.
      tabix_lock = 0.

      IF modus_backgr <> '1'.
*       Abschlußprotokoll erst ausgeben, wenn alle asynchronen Tasks
*       sich zurückgemeldet haben
        IF parallel <> 0.
          WAIT UNTIL parallel <= frei_pr.
        ELSE.
          WAIT UNTIL acti_pr <= 0.
        ENDIF.
      ELSE.
        IF loop_cntr > 4.
*         Wenn Aufruf aus Verbucher, dann maximal 5 Schleifendurchläufe
          EXIT.
        ENDIF.
        IF NOT canc_tab[] IS INITIAL.
          WAIT UP TO loop_cntr SECONDS.
        ENDIF.
      ENDIF.
    ENDWHILE.

    CLEAR loop_cntr.
*   danach die HUM-Stornos buchen: immer ein Storno-Beleg für
*   alle Stornos zu einem Urpsrungsbeleg wegen Sperrproblem mit
*   MB_CANCEL_GOODS_MOVEMENT
    WHILE NOT hucanc_tab[] IS INITIAL.
      loop_cntr = loop_cntr + 1.
      tabix_wbtyp = 1.
      READ TABLE hucanc_tab INDEX tabix_wbtyp.
      READ TABLE affw_tab INDEX hucanc_tab-tabix.
*     Ursprungsbeleg merken
      sjahr_old = affw_tab-sjahr.
      smbln_old = affw_tab-smbln.
      DO.
        READ TABLE hucanc_tab INDEX tabix_wbtyp.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        READ TABLE affw_tab INDEX hucanc_tab-tabix.
        IF sy-subrc <> 0.
*         Darf nicht sein, falls doch: Verarbeitung abbrechen
          REFRESH hucanc_tab.
          EXIT.
        ENDIF.
        tabix_act = hucanc_tab-tabix.
        subrc = 0.
*       AFFWB_TAB füllen und ggf neue Tasks starten
        PERFORM fill_affwb_tab.
      ENDDO.
      IF NOT affwb_tab[] IS INITIAL.
        PERFORM call_mb_create.
        FREE:  affwb_tab,
               imsegb_tab.
        CLEAR: imsegb_tab.
      ENDIF.
      tabix_para = 0.
      tabix_lock = 0.
      IF modus_backgr <> '1'.
*       Abschlußprotokoll erst ausgeben, wenn alle asynchronen Tasks
*       sich zurückgemeldet haben
        IF parallel <> 0.
          WAIT UNTIL parallel <= frei_pr.
        ELSE.
          WAIT UNTIL acti_pr <= 0.
        ENDIF.
      ELSE.
        IF loop_cntr > 4.
*         Wenn Aufruf aus Verbucher, dann maximal 5 Schleifendurchläufe
          EXIT.
        ENDIF.
        IF NOT hucanc_tab[] IS INITIAL.
          WAIT UP TO loop_cntr SECONDS.
        ENDIF.
      ENDIF.
    ENDWHILE.

    CLEAR loop_cntr.
*   danach die HUM-Warenausgänge buchen
    WHILE NOT hum_tab[] IS INITIAL.
      loop_cntr = loop_cntr + 1.
      tabix_wbtyp = 1.
      READ TABLE hum_tab  INDEX tabix_wbtyp.
      READ TABLE affw_tab INDEX hum_tab-tabix.
      bldat_old = affw_tab-bldat.
      budat_old = affw_tab-budat.
      werks_old = affw_tab-werks.
      sjahr_old = affw_tab-sjahr.
      autyp_old = affw_tab-autyp.
      rueck_old = affw_tab-rueck.
      DO.
        READ TABLE hum_tab INDEX tabix_wbtyp.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        READ TABLE affw_tab INDEX hum_tab-tabix.
        IF sy-subrc <> 0.
*         Darf nicht sein, falls doch: Verarbeitung abbrechen
          REFRESH hum_tab.
          EXIT.
        ENDIF.
        tabix_act = hum_tab-tabix.
        subrc = 0.
*       AFFWB_TAB füllen und ggf neue Tasks starten
        PERFORM fill_affwb_tab.
      ENDDO.
      IF NOT affwb_tab[] IS INITIAL.
        PERFORM call_mb_create.
        FREE:  affwb_tab,
               imsegb_tab.
        CLEAR: imsegb_tab.
      ENDIF.
      tabix_para = 0.
      tabix_lock = 0.

      IF modus_backgr <> '1'.
*       Abschlußprotokoll erst ausgeben, wenn alle asynchronen Tasks
*       sich zurückgemeldet haben
        IF parallel <> 0.
          WAIT UNTIL parallel <= frei_pr.
        ELSE.
          WAIT UNTIL acti_pr <= 0.
        ENDIF.
      ELSE.
        IF loop_cntr > 4.
*         Wenn Aufruf aus Verbucher, dann maximal 5 Schleifendurchläufe
          EXIT.
        ENDIF.
        IF NOT hum_tab[] IS INITIAL.
          WAIT UP TO loop_cntr SECONDS.
        ENDIF.
      ENDIF.
    ENDWHILE.

    CLEAR loop_cntr.
*   danach die Wareneingänge buchen
    WHILE NOT we_tab[] IS INITIAL.
      loop_cntr = loop_cntr + 1.
      tabix_wbtyp = 1.
      READ TABLE we_tab INDEX tabix_wbtyp.
      READ TABLE affw_tab INDEX we_tab-tabix.
      bldat_old = affw_tab-bldat.
      budat_old = affw_tab-budat.
      werks_old = affw_tab-werks.
      sjahr_old = affw_tab-sjahr.
      autyp_old = affw_tab-autyp.
      rueck_old = affw_tab-rueck.
      DO.
        READ TABLE we_tab INDEX tabix_wbtyp.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        READ TABLE affw_tab INDEX we_tab-tabix.
        IF sy-subrc <> 0 OR
           affw_tab-kzbew IS INITIAL.
*         Darf nicht sein, falls doch: Verarbeitung abbrechen
          REFRESH we_tab.
          EXIT.
        ENDIF.
        tabix_act = we_tab-tabix.
        subrc = 0.
*       AFFWB_TAB füllen und ggf neue Tasks starten
        PERFORM fill_affwb_tab.
      ENDDO.
      IF NOT affwb_tab[] IS INITIAL.
        PERFORM call_mb_create.
        FREE:  affwb_tab,
               imsegb_tab.
        CLEAR: imsegb_tab.
      ENDIF.
      tabix_para = 0.
      tabix_lock = 0.

      IF modus_backgr <> '1'.
*       Abschlußprotokoll erst ausgeben, wenn alle asynchronen Tasks
*       sich zurückgemeldet haben
        IF parallel <> 0.
          WAIT UNTIL parallel <= frei_pr.
        ELSE.
          WAIT UNTIL acti_pr <= 0.
        ENDIF.
      ELSE.
        IF loop_cntr > 4.
*         Wenn Aufruf aus Verbucher, dann maximal 5 Schleifendurchläufe
          EXIT.
        ENDIF.
        IF NOT we_tab[] IS INITIAL.
          WAIT UP TO loop_cntr SECONDS.
        ENDIF.
      ENDIF.
    ENDWHILE.

    CLEAR loop_cntr.
*   schließlich die Warenausgänge buchen
    WHILE NOT wa_tab[] IS INITIAL.
      loop_cntr = loop_cntr + 1.
      tabix_wbtyp = 1.
      READ TABLE wa_tab INDEX tabix_wbtyp.
      READ TABLE affw_tab INDEX wa_tab-tabix.
      bldat_old = affw_tab-bldat.
      budat_old = affw_tab-budat.
      werks_old = affw_tab-werks.
      sjahr_old = affw_tab-sjahr.
      autyp_old = affw_tab-autyp.
      rueck_old = affw_tab-rueck.
      DO.
        READ TABLE wa_tab INDEX tabix_wbtyp.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        READ TABLE affw_tab INDEX wa_tab-tabix.
        IF sy-subrc <> 0 OR
           NOT affw_tab-kzbew IS INITIAL.
*         Darf nicht sein, falls doch: Verarbeitung abbrechen
          REFRESH wa_tab.
          EXIT.
        ENDIF.
        tabix_act = wa_tab-tabix.
        subrc = 0.
*       AFFWB_TAB füllen und ggf neue Tasks starten
        PERFORM fill_affwb_tab.
      ENDDO.
      IF NOT affwb_tab[] IS INITIAL.
        PERFORM call_mb_create.
        FREE:  affwb_tab,
               imsegb_tab.
        CLEAR: imsegb_tab.
      ENDIF.
      tabix_para = 0.
      tabix_lock = 0.

      IF modus_backgr <> '1'.
*       Abschlußprotokoll erst ausgeben, wenn alle asynchronen Tasks
*       sich zurückgemeldet haben
        IF parallel <> 0.
          WAIT UNTIL parallel <= frei_pr.
        ELSE.
          WAIT UNTIL acti_pr <= 0.
        ENDIF.
      ELSE.
        IF loop_cntr > 4.
*         Wenn Aufruf aus Verbucher, dann maximal 5 Schleifendurchläufe
          EXIT.
        ENDIF.
        IF NOT wa_tab[] IS INITIAL.
          WAIT UP TO loop_cntr SECONDS.
        ENDIF.
      ENDIF.
    ENDWHILE.

    IF NOT affw0_tab[] IS INITIAL.
      CALL FUNCTION 'CO_FW_AFFW_POST' IN UPDATE TASK
           EXPORTING
                i_called = 'D'
           TABLES
                affw_bt = affw0_tab.
    ENDIF.

    IF NOT sy-batch IS INITIAL OR
       modus_backgr = '1'.
*     Verbuchungsdaten über Memory übergeben, d.h. kein WAIT
      COMMIT WORK.
    ELSE.
*     Updatetask aktiv, d.h. WAIT ist notwendig
      COMMIT WORK AND WAIT.
    ENDIF.

************************************************************************
*   Abschlußprotokoll
************************************************************************
    IF panoprot IS INITIAL.
*     erzeugte Materialbelege und fehlerhafte Positionen protokolliere
      PERFORM protokoll_ergebnis.
    ENDIF.

    IF NOT pasimul IS INITIAL OR
       NOT pafehl IS INITIAL  OR
       pawdhl IS INITIAL      OR
       modus_backgr = '1'.
*     keine Mehrfachdurchläufe bei: Simulation                   oder
*                                   Selektion mit Fehlersätzen   oder
*                                   Wiederholung nicht gewünscht oder
*                                   Aufruf aus V2-Verbucher.
      EXIT.
    ENDIF.
*   Init der verwendeten Tabellen für Folgedurchlauf
    REFRESH: affw_tab,
             ind_tab,
             affw_dnotw_tab,
             mseg_tab,
             affw_prt.
    CLEAR: n_fail,
           n_ok,
           n_total.
  ENDDO.

ENDFORM.                               " POST_GOODS_MOVEMENTS
*eject
*----------------------------------------------------------------------*
*       FORM FILL_AFFWB_TAB                                            *
*----------------------------------------------------------------------*
*       AFFWB-Tabelle füllen und Aufrufe der Bestandsführung           *
*----------------------------------------------------------------------*
FORM fill_affwb_tab.

  DATA: lv_tasknam(7) TYPE n,
        lv_acttask(7) TYPE n.
  DATA: l_xhupf TYPE t001l-xhupf.

  IF refbln_old IS INITIAL.
*   keine Verarbeitung von abhängigen Warenbewegungen (WE+WA im A.Netz)
    IF tabix_para > paanzahl                OR
       tabix_lock > pamxlock                OR
       bldat_old <> affw_tab-bldat          OR
       budat_old <> affw_tab-budat          OR
       werks_old <> affw_tab-werks          OR
       ( smbln_old <> affw_tab-smbln AND         "neuer Beleg bei wechs-
        sjahr_old <> affw_tab-sjahr )       OR   "elndem Ursprungsbeleg
       ( autyp_old = auftyp-corp   AND
         affw_tab-autyp <> auftyp-corp )    OR
       ( autyp_old <> auftyp-corp  AND
         affw_tab-autyp = auftyp-corp )     OR
       ( rueck_old IS INITIAL           AND
         NOT affw_tab-rueck IS INITIAL )    OR
       ( NOT rueck_old IS INITIAL       AND
         affw_tab-rueck IS INITIAL ).
*
      IF NOT affwb_tab[] IS INITIAL.
        PERFORM call_mb_create.
        FREE:  affwb_tab,
               imsegb_tab.
        CLEAR: imsegb_tab.
      ENDIF.
      tabix_para = 0.
      tabix_lock = 0.
*     Beleg- und Buchungsdatum übernehmen
      bldat_old = affw_tab-bldat.
      budat_old = affw_tab-budat.
      autyp_old = affw_tab-autyp.
      werks_old = affw_tab-werks.
      sjahr_old = affw_tab-sjahr.
      smbln_old = affw_tab-smbln.
      rueck_old = affw_tab-rueck.
    ENDIF.
  ELSE.
    IF refbln_old <> affw_tab-refbln.
      IF NOT affwb_tab[] IS INITIAL.
        PERFORM call_mb_create.
        FREE:  affwb_tab,
               imsegb_tab.
        CLEAR: imsegb_tab.
      ENDIF.
      tabix_para = 0.
      tabix_lock = 0.
*     Referenzbelegnummer übernehmen
      refbln_old = affw_tab-refbln.
*     Beleg- und Buchungsdatum übernehmen
      bldat_old = affw_tab-bldat.
      budat_old = affw_tab-budat.
    ENDIF.
  ENDIF.
  tabix = tabix + 1.

  CLEAR flg_locked.
  subrc = 0.
  IF pasimul IS INITIAL            AND
     NOT affw_tab-msgno IS INITIAL AND
     affw_tab-dnotw IS INITIAL.
*   Keine Simulation                      UND
*   Warenbewegung ist Fehlersatz für COGI UND
*   WM-Chargenfindung war nicht aufgerufen (sonst bereits gesperrt)
*   --> Sperren des AFFW-Satzes und Nachlesen ist notwendig
*
*   Prüfen, ob Fehlersatz mit "Dummy"-Fehler definiert ist,
*   weil Sätze mit "Dummy"-Fehlern nicht zu sperren sind
    tafwd_tab-arbgb = affw_tab-msgid.
    tafwd_tab-msgnr = affw_tab-msgno.
    READ TABLE tafwd_tab WITH KEY arbgb = tafwd_tab-arbgb
                                  msgnr = tafwd_tab-msgnr
                                  BINARY SEARCH.
    IF sy-subrc <> 0.
*     Fehlersatz sperren
      CALL FUNCTION 'ENQUEUE_ESAFFW'
           EXPORTING
                weblnr  = affw_tab-weblnr
                weblpos = affw_tab-weblpos
                _scope  = '3'  "Freigabe Sperre bei Ende TA & VB
           EXCEPTIONS
                OTHERS  = 01.
      IF sy-subrc = 0.
*       Fehlersatz konnte gesperrt werden
*       Warenbewegungsdaten nachlesen
        SELECT SINGLE * FROM affw INTO affw
                        WHERE weblnr  = affw_tab-weblnr
                        AND   weblpos = affw_tab-weblpos
                        AND   wablnr  = space
                        AND   inact   = space.
        IF sy-subrc = 0 AND
           affw_tab <> affw.
*         Fehlersatz wurde zwischenzeitlich geändert
*         mögliche Sperren freigeben
*         bei Fehlersätzen ist die AFFW-Sperre zurückzunehmen
*         dazu muß die ursprüngliche Fehlermeldung hinzugelesen werden
          subrc = 8.
          CLEAR mseg_tab.
          READ TABLE msg_tab WITH KEY weblnr  = affw_tab-weblnr
                                      weblpos = affw_tab-weblpos
                                      BINARY SEARCH.
          PERFORM dequeue_esaffw USING affw_tab-weblnr
                                       affw_tab-weblpos
                                       msg_tab-msgid
                                       msg_tab-msgno.
        ELSEIF sy-subrc <> 0.
*         Fehlersatz wurde mittlerweile erfolgreich nachbearbeitet
*         und wurde deshalb gelöscht
          subrc = 4.
        ELSE.
*         Fehlersatz wurde erfolgreich gesperrt und nachgelesen
          subrc = 0.
          tabix_lock = tabix_lock + 1.
        ENDIF.
      ELSE.
*       Fehlersatz konnte nicht gesperrt werden
        subrc = 4.
        n_locked = n_locked + 1.
        flg_locked = yx.
      ENDIF.
    ENDIF.
  ENDIF.

  IF subrc = 0             AND
     pasimul IS INITIAL    AND
     refbln_old IS INITIAL AND
     affw_tab-kzbew IS INITIAL.
*   Prüfen, ob für WA-Material bereits Fehlbestand festgestellt wurde
    IF NOT affw_tab-budat IS INITIAL.
      datum = affw_tab-budat.
    ELSE.
      datum = sy-datum.
    ENDIF.
    READ TABLE m7021_tab WITH KEY monat = datum-monat
                                  matnr = affw_tab-matnr
                                  werks = affw_tab-werks
                                  lgort = affw_tab-lgort
                                  charg = affw_tab-charg
                                  sobkz = affw_tab-sobkz
                                  kdauf = affw_tab-kdauf
                                  kdpos = affw_tab-kdpos
                                  BINARY SEARCH.
    IF sy-subrc = 0.
*     Zeile nicht bearbeiten, da zum gleichen Material bereits
*     eine Unterschreitung des Bestands festgestellt wurde
      MOVE-CORRESPONDING affw_tab TO affw0_tab.
      IF affw0_tab-msgid IS INITIAL OR
         affw0_tab-msgno IS INITIAL.
        IF 1 = 2. MESSAGE e522(ru). ENDIF.                        "#EC *
        affw0_tab-fwdat = sy-datum.
        affw0_tab-fwzet = sy-uzeit.
        affw0_tab-msgty = 'E'.
        affw0_tab-msgid = 'RU'.
        affw0_tab-msgno = '522'.
        affw0_tab-msgv1 = affw_tab-matnr.
        affw0_tab-msgv2 = affw_tab-werks.
        affw0_tab-msgv3 = affw_tab-lgort.
        affw0_tab-msgv4 = affw_tab-charg.
      ENDIF.
      IF affw0_tab-fwdat IS INITIAL.
        affw0_tab-fwdat = sy-datum.
      ENDIF.
      affw0_tab-laeda = sy-datum.
      affw0_tab-aenam = sy-uname.
      affw0_tab-dispo = m7021_tab-dispo.
      affw0_tab-vbkz = vbkz_null.
      APPEND affw0_tab.
      READ TABLE affwb_tab INDEX 1.
      IF sy-subrc = 0.
*       Datum alt = Datum des ersten Eintrags aus der
*       aktuellen Portion
        bldat_old = affwb_tab-bldat.
        budat_old = affwb_tab-budat.
        autyp_old = affwb_tab-autyp.
        rueck_old = affwb_tab-rueck.
        werks_old = affwb_tab-werks.
        sjahr_old = affwb_tab-sjahr.
        smbln_old = affwb_tab-smbln.
      ELSE.
*       Portion konnte noch nicht gebildet werden, deshalb
*       Datum alt = Datum des folgenden Satzes
        tabix_wbtyp = tabix_wbtyp + 1.
        IF affw0_tab-kzbew IS INITIAL.
          READ TABLE wa_tab INDEX tabix_wbtyp.
          IF sy-subrc = 0.
            READ TABLE affw_tab INDEX wa_tab-tabix.
          ENDIF.
        ELSE.
          READ TABLE we_tab INDEX tabix_wbtyp.
          IF sy-subrc = 0.
            READ TABLE affw_tab INDEX we_tab-tabix.
          ENDIF.
        ENDIF.
        IF sy-subrc = 0.
          bldat_old = affw_tab-bldat.
          budat_old = affw_tab-budat.
          autyp_old = affw_tab-autyp.
          rueck_old = affw_tab-rueck.
          werks_old = affw_tab-werks.
          sjahr_old = affw_tab-sjahr.
          smbln_old = affw_tab-smbln.
        ENDIF.
*       aktuellen Eintrag wieder herstellen
        tabix_wbtyp = tabix_wbtyp - 1.
        IF affw0_tab-kzbew IS INITIAL.
          READ TABLE wa_tab INDEX tabix_wbtyp.
          IF sy-subrc = 0.
            READ TABLE affw_tab INDEX wa_tab-tabix.
          ENDIF.
        ELSE.
          READ TABLE we_tab INDEX tabix_wbtyp.
          IF sy-subrc = 0.
            READ TABLE affw_tab INDEX we_tab-tabix.
          ENDIF.
        ENDIF.
      ENDIF.
      subrc = 8.
      n_fail = n_fail + 1.
    ENDIF.
  ENDIF.

* nur Reservierungen sperren, bei denen nicht storniert wird
* wegen Buchung über MB_CANCEL_GOODS_MOVEMENT
  IF subrc = 0                     AND
     pasimul IS INITIAL            AND
         affw_tab-smbln IS INITIAL AND "nicht bei Stornos sperren
     NOT affw_tab-rsnum IS INITIAL AND
     NOT affw_tab-rspos IS INITIAL AND
     affw_tab-dnotw IS INITIAL.
*   Reservierungen, für die zuvor die WM-Chargenfindung aufge-
*   rufen wurde, wurden bereits zu diesem Zeitpunkt gesperrt.
    READ TABLE enq_rsnum WITH KEY rsnum = affw_tab-rsnum
                         BINARY SEARCH.
    tabix_i = sy-tabix.
    IF sy-subrc <> 0.
*     Reservierungskopf sperren
      CALL FUNCTION 'ENQUEUE_EMRKPF'
           EXPORTING
                rsnum  = affw_tab-rsnum
                _scope  = '3'  "Freigabe Sperre bei Ende TA & VB
           EXCEPTIONS
                OTHERS = 1.
      IF sy-subrc = 0.
        enq_rsnum-rsnum = affw_tab-rsnum.
        enq_rsnum-count = 1.
        INSERT enq_rsnum INDEX tabix_i.
        tabix_lock = tabix_lock + 1.
      ELSE.
        n_locked = n_locked + 1.
        flg_locked = yx.
        subrc = 4.
      ENDIF.
    ELSE.
      enq_rsnum-count = enq_rsnum-count + 1.
      MODIFY enq_rsnum INDEX tabix_i.
    ENDIF.
  ENDIF.
  IF subrc = 0.
    IF pasimul IS INITIAL.
*     bei Storno prüfen, ob der Ursprungsbeleg bereits in
*     einer aktiven Task verwendet wird
      IF NOT affw_tab-sjahr IS INITIAL.
        READ TABLE canc_task WITH KEY smbln = affw_tab-smbln
                                      sjahr = affw_tab-sjahr
                                      BINARY SEARCH.
        IF sy-subrc <> 0.
*         Ursprungsbeleg wird bisher noch nicht bearbeitet
          canc_task-smbln = affw_tab-smbln.
          canc_task-sjahr = affw_tab-sjahr.
          canc_task-task(1) = tasknam(1).
          canc_task-task+1 = tasknam+1 + 1.
          INSERT canc_task INDEX sy-tabix.
          tabix_lock = tabix_lock + 1.
        ELSEIF sy-subrc = 0.
          lv_tasknam = tasknam.
          lv_acttask = canc_task-task.
          IF lv_tasknam >= lv_acttask.
*         Ursprungsbeleg wird bereits in anderen Tasks behandelt
*         Ursprungsbeleg nicht berücksichtgen
            tabix_wbtyp = tabix_wbtyp + 1.
*           mögliche Sperren freigeben
*           bei Fehlersätzen ist die AFFW-Sperre zurückzunehmen
*           dazu muß die ursprüngliche Fehlermeldung hinzugelesen werden
            CLEAR msg_tab.
            READ TABLE msg_tab WITH KEY weblnr  = affw_tab-weblnr
                                        weblpos = affw_tab-weblpos
                                        BINARY SEARCH.
            PERFORM dequeue_esaffw USING affw_tab-weblnr
                                         affw_tab-weblpos
                                         msg_tab-msgid
                                         msg_tab-msgno.
            IF NOT msg_tab-msgno IS INITIAL.
              tafwd_tab-arbgb = msg_tab-msgid.
              tafwd_tab-msgnr = msg_tab-msgno.
              READ TABLE tafwd_tab WITH KEY arbgb = tafwd_tab-arbgb
                                            msgnr = tafwd_tab-msgnr
                                            BINARY SEARCH.
              IF sy-subrc = 0.
                tabix_lock = tabix_lock - 1.
              ENDIF.
            ENDIF.
            subrc = 4.
            EXIT.
          ENDIF.
        ENDIF.
*       Prüfen, ob im Stornofall die Reservierung vom MM-IM nicht
*       bereits in einer anderen Task gesperrt wurde (sonst M7 545)
        READ TABLE rkpf_task WITH KEY rsnum = affw_tab-rsnum
                                      BINARY SEARCH.
        IF sy-subrc <> 0.
*         Reservierung wird bisher noch nicht bearbeitet
          IF NOT affw_tab-rsnum IS INITIAL.
            rkpf_task-rsnum   = affw_tab-rsnum.
            rkpf_task-task(1) = tasknam(1).
            rkpf_task-task+1  = tasknam+1 + 1.
            INSERT rkpf_task INDEX sy-tabix.
            tabix_lock = tabix_lock + 1.
          ENDIF.
        ELSEIF sy-subrc = 0.
          lv_tasknam = tasknam.
          lv_acttask = rkpf_task-task.
          IF lv_tasknam >= lv_acttask.
*         Reservierung wird bereits in anderen Tasks behandelt
*         Reservierung nicht berücksichtigen
            tabix_wbtyp = tabix_wbtyp + 1.
*           Rückzählen des tabix wegen canc_task.
            tabix_lock = tabix_lock - 1.
*           mögliche Sperren freigeben
*           bei Fehlersätzen ist die AFFW-Sperre zurückzunehmen
*           dazu muß die ursprüngliche Fehlermeldung hinzugelesen werden
            CLEAR msg_tab.
            READ TABLE msg_tab WITH KEY weblnr  = affw_tab-weblnr
                                        weblpos = affw_tab-weblpos
                                        BINARY SEARCH.
            PERFORM dequeue_esaffw USING affw_tab-weblnr
                                         affw_tab-weblpos
                                         msg_tab-msgid
                                         msg_tab-msgno.
            IF NOT msg_tab-msgno IS INITIAL.
              tafwd_tab-arbgb = msg_tab-msgid.
              tafwd_tab-msgnr = msg_tab-msgno.
              READ TABLE tafwd_tab WITH KEY arbgb = tafwd_tab-arbgb
                                            msgnr = tafwd_tab-msgnr
                                            BINARY SEARCH.
              IF sy-subrc = 0.
                tabix_lock = tabix_lock - 1.
              ENDIF.
            ENDIF.
            subrc = 4.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
*     Prüfen ob Material bereits in einer aktiven Task verwendet
      READ TABLE mat_task WITH KEY matnr = affw_tab-matnr
                                   werks = affw_tab-werks
                                   BINARY SEARCH.
      IF sy-subrc <> 0.
*       Material wird bisher noch nicht bearbeitet
        mat_task-matnr = affw_tab-matnr.
        mat_task-werks = affw_tab-werks.
        mat_task-task(1)  = tasknam(1).
        mat_task-task+1 = tasknam+1 + 1.
        INSERT mat_task INDEX sy-tabix.
        tabix_lock = tabix_lock + 2.             "MARC und MBEW
      ELSEIF sy-subrc = 0.
        lv_tasknam = tasknam+1.
        lv_acttask = mat_task-task+1.
        IF lv_tasknam >= lv_acttask.
*         Material wird bereits in anderen Tasks behandelt
*         Material nicht berücksichtgen
          tabix_wbtyp = tabix_wbtyp + 1.
*         mögliche Sperren freigeben
*         bei Fehlersätzen ist die AFFW-Sperre zurückzunehmen
*         dazu muß die ursprüngliche Fehlermeldung hinzugelesen werden
          CLEAR msg_tab.
          READ TABLE msg_tab WITH KEY weblnr  = affw_tab-weblnr
                                      weblpos = affw_tab-weblpos
                                      BINARY SEARCH.
          PERFORM dequeue_esaffw USING affw_tab-weblnr
                                       affw_tab-weblpos
                                       msg_tab-msgid
                                       msg_tab-msgno.
          IF NOT msg_tab-msgno IS INITIAL.
            tafwd_tab-arbgb = msg_tab-msgid.
            tafwd_tab-msgnr = msg_tab-msgno.
            READ TABLE tafwd_tab WITH KEY arbgb = tafwd_tab-arbgb
                                          msgnr = tafwd_tab-msgnr
                                          BINARY SEARCH.
            IF sy-subrc = 0.
              tabix_lock = tabix_lock - 1.
            ENDIF.
          ENDIF.
          IF affw_tab-kzbew IS INITIAL AND
             NOT affw_tab-rsnum IS INITIAL.
*           die Sperre auf den Reservierungskopf ist zurückzunehmen
            PERFORM dequeue_emrkpf USING affw_tab-rsnum.
*           RKPF-Sperre noch aktiv oder zurückgenommen
            READ TABLE enq_rsnum WITH KEY rsnum = affw_tab-rsnum
                                          BINARY SEARCH.
            IF sy-subrc <> 0.
              tabix_lock = tabix_lock - 1.
            ENDIF.
          ENDIF.
          subrc = 4.
          EXIT.
        ENDIF.
      ENDIF.
      IF NOT affw_tab-kdauf IS INITIAL AND
         ( NOT affw_tab-kzbew IS INITIAL OR
           NOT affw_tab-sobkz IS INITIAL ).
*       Prüfen ob Kundenauftrag bereits in einer aktiven Task verwendet
        READ TABLE kda_task WITH KEY kdauf = affw_tab-kdauf
                                     kdpos = affw_tab-kdpos
                                     BINARY SEARCH.
        IF sy-subrc <> 0.
*         Material wird bisher noch nicht bearbeitet
          kda_task-kdauf = affw_tab-kdauf.
          kda_task-kdpos = affw_tab-kdpos.
          kda_task-task(1) = tasknam(1).
          kda_task-task+1  = tasknam+1 + 1.
          INSERT kda_task INDEX sy-tabix.
          tabix_lock = tabix_lock + 1.
        ELSEIF sy-subrc = 0.
          lv_tasknam = tasknam+1.
          lv_acttask = kda_task-task+1.
          IF lv_tasknam >= lv_acttask.
*           Kundenauftrag wird bereits in anderen Tasks behandelt
*           Kundenauftrag nicht berücksichtgen
            tabix_wbtyp = tabix_wbtyp + 1.
*           mögliche Sperren freigeben
*           bei Fehlersätzen ist die AFFW-Sperre zurückzunehmen
*           dazu muß die ursprüngliche Fehlermeldung hinzugelesen werden
            CLEAR msg_tab.
            PERFORM dequeue_esaffw USING affw_tab-weblnr
                                         affw_tab-weblpos
                                         msg_tab-msgid
                                         msg_tab-msgno.
            IF NOT msg_tab-msgno IS INITIAL.
              tafwd_tab-arbgb = msg_tab-msgid.
              tafwd_tab-msgnr = msg_tab-msgno.
              READ TABLE tafwd_tab WITH KEY arbgb = tafwd_tab-arbgb
                                            msgnr = tafwd_tab-msgnr
                                            BINARY SEARCH.
              IF sy-subrc = 0.
                tabix_lock = tabix_lock - 1.
              ENDIF.
            ENDIF.
            IF affw_tab-kzbew IS INITIAL AND
               NOT affw_tab-rsnum IS INITIAL.
*             die Sperre auf den Reservierungskopf ist zurückzunehmen
              PERFORM dequeue_emrkpf USING affw_tab-rsnum.
*             RKPF-Sperre noch aktiv oder zurückgenommen
              READ TABLE enq_rsnum WITH KEY rsnum = affw_tab-rsnum
                                            BINARY SEARCH.
              IF sy-subrc <> 0.
                tabix_lock = tabix_lock - 1.
              ENDIF.
            ENDIF.
            subrc = 4.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
      IF NOT affw_tab-kzbew IS INITIAL.
*       Prüfen ob Auftrag bei WE bereits in einer aktiven Task verwendet
        READ TABLE auf_task WITH KEY aufnr = affw_tab-aufnr
                                     BINARY SEARCH.
        IF sy-subrc <> 0.
*         Material wird bisher noch nicht bearbeitet
          auf_task-aufnr = affw_tab-aufnr.
          auf_task-task(1) = tasknam(1).
          auf_task-task+1  = tasknam+1 + 1.
          INSERT auf_task INDEX sy-tabix.
          tabix_lock = tabix_lock + 1.
        ELSEIF sy-subrc = 0.
          lv_tasknam = tasknam+1.
          lv_acttask = auf_task-task+1.
          IF lv_tasknam >= lv_acttask.
*           Material wird bereits in anderen Tasks behandelt
*           Material nicht berücksichtgen
            tabix_wbtyp = tabix_wbtyp + 1.
*           mögliche Sperren freigeben
*           bei Fehlersätzen ist die AFFW-Sperre zurückzunehmen
*           dazu muß die ursprüngliche Fehlermeldung hinzugelesen werden
            CLEAR msg_tab.
            PERFORM dequeue_esaffw USING affw_tab-weblnr
                                         affw_tab-weblpos
                                         msg_tab-msgid
                                         msg_tab-msgno.
            IF NOT msg_tab-msgno IS INITIAL.
              tafwd_tab-arbgb = msg_tab-msgid.
              tafwd_tab-msgnr = msg_tab-msgno.
              READ TABLE tafwd_tab WITH KEY arbgb = tafwd_tab-arbgb
                                            msgnr = tafwd_tab-msgnr
                                            BINARY SEARCH.
              IF sy-subrc = 0.
                tabix_lock = tabix_lock - 1.
              ENDIF.
            ENDIF.
            subrc = 4.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*   Anzahl der Einträge in AFFWB_TAB
    tabix_para = tabix_para + 1.
*   Daten des Fehlerbelegs übernehmen
    MOVE-CORRESPONDING affw_tab TO affwb_tab.
    IF affw_tab-dnotw <> plus.
      affwb_tab-vbkz  = vbkz_null.
    ELSE.
      affwb_tab-ersda = sy-datum.
      affwb_tab-erzet = sy-uzeit.
      affwb_tab-ernam = sy-uname.
      affwb_tab-vbkz  = vbkz_ins.
    ENDIF.
    APPEND affwb_tab.

*   Sicherheitsprüfung gegen mehrfache Warenbewegungen
    READ TABLE x WITH KEY weblnr  = affw_tab-weblnr
                          weblpos = affw_tab-weblpos
                          BINARY SEARCH.
    IF sy-subrc = 0.
      MESSAGE a888 WITH sy-repid.
    ELSE.
      x-weblnr  = affw_tab-weblnr.
      x-weblpos = affw_tab-weblpos.
      x-task(1) = tasknam(1).
      x-task    = tasknam+1 + 1.
      INSERT x INDEX sy-tabix.
    ENDIF.
  ENDIF.

  IF modus_backgr <> '1' OR
     flg_locked IS INITIAL.
*   Indextabelle bereinigen
    IF refbln_old IS INITIAL.
      CLEAR l_xhupf.
      READ TABLE t001l_tab WITH KEY werks = affw_tab-werks
                                    lgort = affw_tab-lgort
                                    BINARY SEARCH
                                    TRANSPORTING xhupf.
      IF sy-subrc IS INITIAL.
        l_xhupf = t001l_tab-xhupf.
      ENDIF.
      IF l_xhupf IS INITIAL.
        IF affw_tab-sjahr IS INITIAL.
          IF affw_tab-kzbew IS INITIAL.
            DELETE wa_tab INDEX tabix_wbtyp.
          ELSE.
            DELETE we_tab INDEX tabix_wbtyp.
          ENDIF.
        ELSE.
          DELETE canc_tab INDEX tabix_wbtyp.
        ENDIF.
      ELSE.
        IF affw_tab-sjahr IS INITIAL.
          DELETE hum_tab INDEX tabix_wbtyp.
        ELSE.
          DELETE hucanc_tab INDEX tabix_wbtyp.
        ENDIF.
      ENDIF.
    ELSE.
      DELETE ref_tab INDEX tabix_wbtyp.
    ENDIF.
  ELSE.
*   Konnten während der Verarbeitung im Verbucher die Position wegen
*   einer Sperrsituation nicht gebucht werden, so ist der Index zu
*   erhöhen, damit er auf den folgenden Eintrag in der Tabelle verweist
    tabix_wbtyp = tabix_wbtyp + 1.
  ENDIF.

ENDFORM.                               " FILL_AFFWB_TAB
*eject
*----------------------------------------------------------------------*
*       FORM FIND_NEW_WEBLPOS                                          *
*----------------------------------------------------------------------*
*       neue Positionsnummer für Fehlerbeleg suchen                    *
*----------------------------------------------------------------------*
FORM find_new_weblpos USING fw_blnr  TYPE affw-weblnr
                            fw_blpos TYPE affw-weblpos.

  DATA: fw_subrc LIKE sy-subrc,
        fw_tabix LIKE sy-tabix.

* größte WEBLPOS je vergebener WEBLNR suchen
  READ TABLE weblpos_max WITH KEY weblnr = fw_blnr
                                           BINARY SEARCH.
  fw_subrc = sy-subrc.
  fw_tabix = sy-tabix.
  IF sy-subrc <> 0.
    SELECT MAX( weblpos ) FROM affw INTO weblpos_max-weblpos
                          WHERE weblnr = fw_blnr.
    weblpos_max-weblnr = fw_blnr.
  ENDIF.
  DO.
    weblpos_max-weblpos = weblpos_max-weblpos + 1.
*   Fehlersatz sperren
    CALL FUNCTION 'ENQUEUE_ESAFFW'
         EXPORTING
              weblnr  = fw_blnr
              weblpos = weblpos_max-weblpos
              _scope  = '3'  "Freigabe Sperre bei Ende TA & VB
         EXCEPTIONS
              OTHERS  = 01.
    IF sy-subrc = 0.
      EXIT.
    ENDIF.
  ENDDO.
  IF fw_subrc = 0.
    MODIFY weblpos_max INDEX fw_tabix.
  ELSE.
    INSERT weblpos_max INDEX fw_tabix.
  ENDIF.
  fw_blpos = weblpos_max-weblpos.

ENDFORM.                               " FIND_NEW_WEBLPOS
*eject
*----------------------------------------------------------------------*
*       FORM CALL_MB_CREATE                                            *
*----------------------------------------------------------------------*
*       markierte Einträge sichern                                     *
*----------------------------------------------------------------------*
FORM call_mb_create.

* Tabellen der Schnittstelle zu CORUAFW3
  DATA: BEGIN OF fw_tab OCCURS 0.
          INCLUDE TYPE affwb.
  DATA: END   OF fw_tab.
  DATA: BEGIN OF im_tab OCCURS 0.
          INCLUDE TYPE imseg.
  DATA: END   OF im_tab.

  DATA: xallp TYPE xfeld.
  DATA: l_subrc LIKE sy-subrc.
  DATA: l_flg_hupf TYPE xfeld.

  CONSTANTS:  mhd_necessary LIKE sy-subrc      VALUE '1',
              hsd_necessary LIKE sy-subrc      VALUE '2'.

  IF NOT pasimul IS INITIAL.
    IF panoprot IS INITIAL.
*     AFFW-Daten für Protokollierung bereitstellen
      LOOP AT affwb_tab.
        MOVE-CORRESPONDING affwb_tab TO affw_prt.
        APPEND affw_prt.
      ENDLOOP.
    ENDIF.
    EXIT.
  ENDIF.

* Kopfdaten des Materialbelegs bereitstellen
  CLEAR imkpf.
  imkpf-bldat = bldat_old.
  imkpf-budat = budat_old.
* Sicherheitspruefungen
  READ TABLE affwb_tab INDEX 1.
  IF imkpf-budat < affwb_tab-budat.
    imkpf-budat = affwb_tab-budat.
  ENDIF.
  IF imkpf-bldat IS INITIAL.
    imkpf-bldat = sy-datum.
  ENDIF.
  IF imkpf-budat IS INITIAL.
    imkpf-budat = sy-datum.
  ENDIF.

* Zusatzdaten der RESB bereitstellen
  REFRESH: resb_key,
           resb_tab.
  LOOP AT affwb_tab.
    IF NOT affwb_tab-rsnum IS INITIAL AND
       NOT affwb_tab-rspos IS INITIAL.
*     RESB-Keys der Reservierungen für Einlesen RESB-Daten sammeln
      resb_key-rsnum = affwb_tab-rsnum.
      resb_key-rspos = affwb_tab-rspos.
      resb_key-rsart = affwb_tab-rsart.
      APPEND resb_key.
    ENDIF.
  ENDLOOP.
  IF modus_backgr = '1'.
*   bei asynchroner Verbuchung Usernamen zum Druck übergeben
    imkpf-pr_uname = affwb_tab-ernam.
  ENDIF.
  IF NOT affwb_tab-refbln IS INITIAL.
*   abhängige Warenbewegungen werden gebucht
    IF affwb_tab-smbln IS INITIAL.
*     Beim Erfassen der Warenbewegungen sind Wareneingänge (KZBEW = F)
*     vor den Warenausgängen zu buchen
      SORT affwb_tab BY kzbew DESCENDING.
    ELSE.
      SORT affwb_tab BY kzbew ASCENDING.
    ENDIF.
  ENDIF.
* Reservierungsdaten bereitstellen
  IF NOT resb_key[] IS INITIAL.
    SELECT (ftab_resb) FROM resb INTO TABLE resb_tab
                                 FOR ALL ENTRIES IN resb_key
                                 WHERE rsnum = resb_key-rsnum
                                 AND   rspos = resb_key-rspos
                                 AND   rsart = resb_key-rsart.
    SORT resb_tab BY rsnum rspos rsart.
  ENDIF.

  LOOP AT affwb_tab.
    l_subrc = 0.
*   Schnittstellentabelle der Bestandsführung füllen
    CLEAR imsegb_tab.
    MOVE-CORRESPONDING affwb_tab TO imsegb_tab.               "#EC ENHOK
    CLEAR imsegb_tab-line_id.
    imsegb_tab-mat_kdauf = affwb_tab-kdauf.
    imsegb_tab-mat_kdpos = affwb_tab-kdpos.
    imsegb_tab-mat_pspnr = affwb_tab-ps_psp_pnr.
*   bei der Verbuchung der AFFW-Sätze wird MHDAT
*   nach IMSEG-VFDAT  oder IMSEG-HSDAT geschrieben
    IF affwb_tab-shkzg EQ con_shkzg-soll.
      CALL FUNCTION 'CO_FW_CHECK_MHD_NECESSARY'
           EXPORTING
               matnr_imp     = affwb_tab-matnr
               werks_imp     = affwb_tab-werks
               bwart_imp     = affwb_tab-bwart
           EXCEPTIONS
               mhd_necessary = 1
               hsd_necessary = 2.
       IF sy-subrc = mhd_necessary.
         imsegb_tab-vfdat = affwb_tab-mhdat.
         CLEAR imsegb_tab-mhdat.
       ELSEIF sy-subrc = hsd_necessary.
         imsegb_tab-hsdat = affwb_tab-mhdat.
         CLEAR imsegb_tab-mhdat.
       ENDIF.
     ENDIF.
*   temporäres Kennz. KZECH steht in AFFW_TAB-DNOTW bereit
    IF NOT affwb_tab-lgnum IS INITIAL.
      IF imsegb_tab-kzech <> plus.
        imsegb_tab-kzech = affwb_tab-dnotw.
      ELSE.
        imsegb_tab-kzech = minus.
      ENDIF.
      IF NOT affwb_tab-dnotw IS INITIAL.
        CLEAR affwb_tab-dnotw.
        MODIFY affwb_tab TRANSPORTING dnotw.
      ENDIF.
    ENDIF.
*   Fehlerbeleg merken
    imsegb_tab-lfbnr = affwb_tab-weblnr.
    imsegb_tab-lfpos = affwb_tab-weblpos.

*   Sonderlogik für Netzpläne
    IF affwb_tab-autyp = auftyp-netw.
      imsegb_tab-nplnr = imsegb_tab-aufnr.
      CLEAR imsegb_tab-aufnr.
    ENDIF.
    IF NOT imsegb_tab-aufnr IS INITIAL.
*     Status-Objektnummer des Vorgangs bestimmen
*     Darüber kann in der Bestandsführung bestimmt werden, ob
*     ein Netzplan vorgangs- bzw. kopfkontiert geführt wird
      or_objnr-aufnr = imsegb_tab-aufnr.
      imsegb_tab-objnr = or_objnr.
    ENDIF.

    IF NOT imsegb_tab-kzbew IS INITIAL.
*     Kundennummer darf nicht übergeben werden beim Wareneingang
      CLEAR imsegb_tab-kunnr.
    ENDIF.

    IF NOT imsegb_tab-rsnum IS INITIAL AND
       NOT imsegb_tab-rspos IS INITIAL.
      READ TABLE resb_tab WITH KEY rsnum = imsegb_tab-rsnum
                                   rspos = imsegb_tab-rspos
                                   rsart = imsegb_tab-rsart
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        IF resb-splkz = '1'.
*         Entnahme zum Summensplittsatz nicht erlaubt
          n_fail = n_fail + 1.
          affw_dnotw_tab = affw_tab.
          IF affw_dnotw_tab-fwdat IS INITIAL  OR
             affw_dnotw_tab-msgid <> 'RU' OR
             affw_dnotw_tab-msgno <> '496'.
            affw_dnotw_tab-fwdat = sy-datum.
            affw_dnotw_tab-fwzet = sy-uzeit.
          ENDIF.
          IF 1 = 2. MESSAGE e496(ru). ENDIF.                      "#EC *
          affw_dnotw_tab-msgid = 'RU'.
          affw_dnotw_tab-msgty = 'E'.
          affw_dnotw_tab-msgno = '496'.
          affw_dnotw_tab-msgv1 = affw_tab-rueck.
          affw_dnotw_tab-msgv2 = affw_tab-rmzhl.
          affw_dnotw_tab-msgv3 = affw_tab-aufnr.
          CLEAR affw_dnotw_tab-msgv4.
          affw_dnotw_tab-aenam = sy-uname.
          affw_dnotw_tab-laeda = sy-datum.
          APPEND affw_dnotw_tab.
*         Anzahl Einträge der IMSEG_TAB korrigieren
          tabix = tabix - 1.
          CONTINUE.
        ENDIF.
*       Zusatzdaten der Reservierung übernehmen
        imsegb_tab-rshkz = resb_tab-shkzg.  "S.-H.-Kennzeichen Reserv.
        imsegb_tab-bdter = resb_tab-bdter.  "Bedarfstermin
        imsegb_tab-bdmng = resb_tab-bdmng.  "Bedarfsmenge
        imsegb_tab-enmng = resb_tab-enmng.  "entnommene Menge
        imsegb_tab-kzbws = resb_tab-kzbws.  "Kz. Bewertung Sonderbestand
        IF imsegb_tab-prvbe = resb_tab-prvbe. "PVB nicht geändert
          imsegb_tab-lgnum = resb_tab-lgnum.  "Lagernummer
          imsegb_tab-lgtyp = resb_tab-lgtyp.  "Lagertyp
          imsegb_tab-lgpla = resb_tab-lgpla.  "Lagerplatz
          imsegb_tab-berkz = resb_tab-berkz.  "Bereitstellungs-Kennz
        ENDIF.
      ENDIF.
      CLEAR: imsegb_tab-bwlvs.              "Bewegungsart LVS
    ENDIF.
*   Do not clear finished indicators if storage location is HU managed
*   --> delivereries are created
    CLEAR l_flg_hupf.
    CALL FUNCTION 'V51S_HU_LGORT'
      EXPORTING
        if_lgort      = imsegb_tab-lgort
        if_werks      = imsegb_tab-werks
      IMPORTING
        ef_hu_managed = l_flg_hupf
      EXCEPTIONS
        OTHERS        = 0.
    IF l_flg_hupf IS INITIAL.
*     Die Kennzeichen werden im Verbucher gesetzt, sofern alle AFFW-
*     Sätze zum Objekt verarbeitet werden konnten
      CLEAR: imsegb_tab-kzear,
             imsegb_tab-elikz.
    ENDIF.
*   Beim Storno ist der ursprüngl. Preis zu verwenden
    IF NOT imsegb_tab-sjahr IS INITIAL.
      READ TABLE bfwrt_tab
                 WITH KEY mblnr = imsegb_tab-smbln
                          mjahr = imsegb_tab-sjahr
                          zeile = imsegb_tab-smblp
                          BINARY SEARCH.
      IF sy-subrc = 0.
        IF bfwrt_tab-bwart NE t156n-bwart.
          SELECT SINGLE * FROM t156n
                          WHERE fcode EQ 'ST  '
                          AND bwart EQ bfwrt_tab-bwart.
          IF sy-subrc <> 0.
            CLEAR t156n.
          ENDIF.
        ENDIF.
        imsegb_tab-xstor = t156n-xstor.     "Kz Belegstorno
        imsegb_tab-mblnr = bfwrt_tab-mblnr. "Ursprungsbeleg
        imsegb_tab-mjahr = bfwrt_tab-mjahr.
        imsegb_tab-mblpo = bfwrt_tab-zeile.
        imsegb_tab-menge = bfwrt_tab-menge.
        imsegb_tab-meins = bfwrt_tab-meins.
        imsegb_tab-konto = bfwrt_tab-sakto.
        imsegb_tab-dmbtr = bfwrt_tab-dmbtr.
        CLEAR imsegb_tab-exbwr.
        imsegb_tab-bstmg = bfwrt_tab-bstmg.
      ENDIF.
    ENDIF.

*   Wenn automatischer WE bzw. ungeplante Entnahme, dann
*   Schlüssel der Reservierung vollständig löschen
    IF NOT imsegb_tab-kzbew IS INITIAL OR
       imsegb_tab-rsnum IS INITIAL.
      CLEAR: imsegb_tab-rsnum,
             imsegb_tab-rspos,
             imsegb_tab-rsart.
    ENDIF.

    APPEND imsegb_tab.
  ENDLOOP.

  CLEAR msg_text.
* Tasknam zur Identifizierung nutzen
  tasknam(1) = '2'.
  tasknam+1  = tasknam+1 + 1.
  IF refbln_old IS INITIAL.
    CLEAR xallp.
  ELSE.
    xallp = yx.
  ENDIF.
  IF paseriel IS INITIAL.
*   Aufruf nicht aus V2-Verbucher
    IF debug IS INITIAL.
*     Warenbewegung in asynchroner Task (parallelisiert) starten
      CALL FUNCTION 'CO_FW_GOODS_MOVEMENTS_BY_AFFW'
           STARTING NEW TASK tasknam
           DESTINATION IN GROUP pagroup
           PERFORMING receive_result ON END OF TASK
           EXPORTING
                imkpf_str = imkpf                             "#EC ENHOK
                xallp_imp = xallp
           TABLES
                affwb_tab = affwb_tab                         "#EC ENHOK
                imseg_tab = imsegb_tab                        "#EC ENHOK
                resb_tab  = resb_tab                          "#EC ENHOK
           EXCEPTIONS
                communication_failure = 1 MESSAGE msg_text
                system_failure        = 2 MESSAGE msg_text
                resource_failure      = 3
                OTHERS                = 4.
    ELSE.
*     Vorsicht: der folgende Funktionsbaustein darf im Debugging-Modus
*               nicht wiederholt aufgerufen werden
*     Warenbewegung für Tests in gleicher Task starten
      CALL FUNCTION 'CO_FW_GOODS_MOVEMENTS_BY_AFFW'
           EXPORTING
                imkpf_str = imkpf                             "#EC ENHOK
                xallp_imp = xallp
           TABLES
                affwb_tab = affwb_tab                         "#EC ENHOK
                imseg_tab = imsegb_tab                        "#EC ENHOK
                resb_tab  = resb_tab
           EXCEPTIONS
                OTHERS    = 1.
    ENDIF.
    l_subrc = sy-subrc.
  ELSE.
*   Serieller Aufruf der Bestandsführung
    fw_tab[] = affwb_tab[].                                   "#EC ENHOK
    im_tab[] = imsegb_tab[].                                  "#EC ENHOK

*   Aufruf aus Hintergrundjob CORUAFWP
*   Rollbereichswechsel werden erzwungen
    EXPORT pakumul
           imkpf
           xallp
           fw_tab
           im_tab
           resb_tab
           TO MEMORY ID 'GOODSMOVE_COGI'.
    SUBMIT coruafw3 AND RETURN.

*   Ergebnisse verarbeiten
    PERFORM receive_result USING tasknam.
    l_subrc = 0.
  ENDIF.

  CASE l_subrc.
    WHEN 0.
      IF parallel <> 0.
        frei_pr = frei_pr - 1.
        WAIT UNTIL frei_pr > 0.
      ELSE.
        acti_pr = acti_pr + 1.
      ENDIF.
    WHEN 1 OR 2 OR 4.
      CLEAR fault_tab.
      fault_tab-task  = tasknam.
      fault_tab-msg   = msg_text.
      fault_tab-subrc = sy-subrc.
      LOOP AT affwb_tab.
        fault_tab-weblnr  = affwb_tab-weblnr.
        fault_tab-weblpos = affwb_tab-weblpos.
        APPEND fault_tab.
      ENDLOOP.
*     Einträge in Task-Tabellen löschen
      DELETE canc_task WHERE task = tasknam.
      DELETE rkpf_task WHERE task = tasknam.
      DELETE mat_task WHERE task = tasknam.
      DELETE auf_task WHERE task = tasknam.
      DELETE kda_task WHERE task = tasknam.

*     Nachricht für Jobprotokoll ausgeben
      DESCRIBE TABLE affwb_tab LINES sy-tfill.
      IF modus_backgr <> '1'.
        IF sy-batch IS INITIAL.
          MOVE l_subrc TO g_subrc1.
          MOVE sy-tfill TO g_counter.
          CONCATENATE
            text-014 g_subrc1 g_slash g_counter g_slash msg_text(70)
            INTO g_txt SEPARATED BY space.
        ELSE.
          MESSAGE i889(co) WITH text-014   "Failed Task
                                l_subrc
                                sy-tfill
                                msg_text.
        ENDIF.
      ENDIF.
    WHEN 3.
      fault_tab-task = tasknam.
      fault_tab-msg  = msg_text.
      fault_tab-subrc = sy-subrc.
      LOOP AT affwb_tab.
        fault_tab-weblnr  = affwb_tab-weblnr.
        fault_tab-weblpos = affwb_tab-weblpos.
        APPEND fault_tab.
      ENDLOOP.
*     Einträge in Task-Tabellen löschen
      DELETE canc_task WHERE task = tasknam.
      DELETE rkpf_task WHERE task = tasknam.
      DELETE mat_task WHERE task = tasknam.
      DELETE auf_task WHERE task = tasknam.
      DELETE kda_task WHERE task = tasknam.

*     Nachricht für Jobprotokoll ausgeben
      DESCRIBE TABLE affwb_tab LINES sy-tfill.
      IF modus_backgr <> '1'.
        IF sy-batch IS INITIAL.
          MOVE l_subrc TO g_subrc1.
          MOVE sy-tfill TO g_counter.
          CONCATENATE
            text-014 g_subrc1 g_slash g_counter g_slash msg_text(70)
            INTO g_txt SEPARATED BY space.
        ELSE.
          MESSAGE i889(co) WITH text-014   "Failed Task
                                l_subrc
                                sy-tfill
                                msg_text.
        ENDIF.
      ENDIF.
*     Wartezeit erhöhen
      IF wait_time = 0.
        wait_time = wait_time + '0.5'.
      ENDIF.
      IF parallel <> 0.
        old_pr = frei_pr.
        WAIT UNTIL old_pr < frei_pr UP TO wait_time SECONDS.
      ELSE.
        old_pr = acti_pr.
        WAIT UNTIL old_pr > acti_pr UP TO wait_time SECONDS.
      ENDIF.
      IF sy-subrc = 8.
*       angegebene Zeitschranke WAIT_TIME wurde überschritten
        wait_time = wait_time + '0.5'.
      ENDIF.
  ENDCASE.

ENDFORM.                               " CALL_MB_CREATE
*eject
*----------------------------------------------------------------------*
*       FORM RECEIVE_RESULT                                            *
*----------------------------------------------------------------------*
*       Ergebnisse des asynchronen Funktionsbausteins entgegennehmen   *
*----------------------------------------------------------------------*
FORM receive_result USING taskname TYPE any.                 "#EC CALLED

  DATA: BEGIN OF fw_tab OCCURS 0.
          INCLUDE TYPE affwb.
  DATA: END   OF fw_tab.
  DATA: BEGIN OF im_tab OCCURS 0.
          INCLUDE TYPE imseg.
  DATA: END   OF im_tab.

* Einträge in Task-Tabellen löschen
  DELETE canc_task WHERE task = taskname.
  DELETE rkpf_task WHERE task = taskname.
  DELETE mat_task WHERE task = taskname.
  DELETE auf_task WHERE task = taskname.
  DELETE kda_task WHERE task = taskname.

  IF paseriel IS INITIAL.
*   Ergebnisse je Task entgegennehmen
    RECEIVE RESULTS FROM FUNCTION 'CO_FW_GOODS_MOVEMENTS_BY_AFFW'
            TABLES
                 affwb_tab = fw_tab                          "#EC ENHOK
                 imseg_tab = im_tab                          "#EC ENHOK
            EXCEPTIONS
                 communication_failure = 1 message msg_text
                 system_failure        = 2 message msg_text.
  ELSE.
*   Ergebnisse aus Memory übernehmen
    IMPORT fw_tab
           im_tab
           FROM MEMORY ID 'GOODSMOVE_COGI'.
  ENDIF.

  IF sy-subrc = 0.
    LOOP AT fw_tab.
      IF NOT fw_tab-wablnr IS INITIAL.
*       Beleg wurde erzeugt
        n_ok = n_ok + 1.
        READ TABLE mseg_tab WITH KEY wablnr = fw_tab-wablnr
                                     mjahr  = fw_tab-mjahr
                                     BINARY SEARCH.
        IF sy-subrc <> 0.
          mseg_tab-wablnr = fw_tab-wablnr.
          mseg_tab-mjahr  = fw_tab-mjahr.
          mseg_tab-budat  = fw_tab-budat.
          mseg_tab-bldat  = fw_tab-bldat.
          INSERT mseg_tab INDEX sy-tabix.
        ENDIF.
      ELSE.
*       Warenbewegung war fehlerhaft
        n_fail = n_fail + 1.

        IF fw_tab-msgid = 'M7' AND
           fw_tab-msgno = '021'.
          MOVE-CORRESPONDING fw_tab TO m7021_tab.             "#EC ENHOK
          IF fw_tab-budat IS INITIAL.
            fw_tab-budat = sy-datum.
          ENDIF.
          datum = fw_tab-budat.
          m7021_tab-monat = datum-monat.
          READ TABLE m7021_tab WITH KEY monat = m7021_tab-monat
                                        matnr = m7021_tab-matnr
                                        werks = m7021_tab-werks
                                        lgort = m7021_tab-lgort
                                        charg = m7021_tab-charg
                                        sobkz = m7021_tab-sobkz
                                        kdauf = m7021_tab-kdauf
                                        kdpos = m7021_tab-kdpos
                                        dispo = m7021_tab-dispo
                                        BINARY SEARCH.
          IF sy-subrc <> 0.
            MOVE-CORRESPONDING fw_tab TO m7021_tab.           "#EC ENHOK
            m7021_tab-monat = datum-monat.
            INSERT m7021_tab INDEX sy-tabix.
          ENDIF.
        ENDIF.

        IF panoprot IS INITIAL.
          MOVE-CORRESPONDING fw_tab TO affw_prt.              "#EC ENHOK
          APPEND affw_prt.
        ENDIF.
      ENDIF.
*     mögliche Sperren freigeben
*     bei Fehlersätzen ist die AFFW-Sperre zurückzunehmen
*     dazu muß die ursprüngliche Fehlermeldung hinzugelesen werden
      CLEAR mseg_tab.
      READ TABLE msg_tab WITH KEY weblnr  = fw_tab-weblnr
                                  weblpos = fw_tab-weblpos
                                  BINARY SEARCH.
      PERFORM dequeue_esaffw USING fw_tab-weblnr
                                   fw_tab-weblpos
                                   msg_tab-msgid
                                   msg_tab-msgno.
      IF NOT fw_tab-rsnum IS INITIAL AND
         NOT fw_tab-rspos IS INITIAL.
*       Reservierungskopf entsperren
        PERFORM dequeue_emrkpf USING fw_tab-rsnum.
      ENDIF.
    ENDLOOP.
  ELSE.
    fault_tab-task = tasknam.                                     "#EC *
    fault_tab-msg  = msg_text.
    fault_tab-subrc = sy-subrc.
    LOOP AT fw_tab.
      fault_tab-weblnr  = fw_tab-weblnr.
      fault_tab-weblpos = fw_tab-weblpos.
      APPEND fault_tab.
    ENDLOOP.
  ENDIF.
* Returncode zurücksetzen
  sy-subrc = 0.

  IF parallel <> 0.
    frei_pr = frei_pr + 1.
  ELSE.
    acti_pr = acti_pr - 1.
  ENDIF.

ENDFORM.                               " RECEIVE_RESULT
*eject
*----------------------------------------------------------------------*
*       FORM PROTOKOLL_ERGEBNIS                                        *
*----------------------------------------------------------------------*
*       Beleg und fehlerhafte Warenbewegungen protokollieren           *
*----------------------------------------------------------------------*
FORM protokoll_ergebnis.

*  flg_prot = 1.
* Task-Abbrüche protokollieren
  READ TABLE fault_tab INDEX 1.
  IF sy-subrc = 0.
*    flg_prot = 2.
    tasknam = fault_tab-task.
    msg_text = fault_tab-msg.
*    LOOP AT fault_tab.
*      MOVE-CORRESPONDING fault_tab TO gs_fault_tab.
*      APPEND gs_fault_tab TO gt_fault_tab.
*    ENDLOOP.
  ENDIF.
* erzeugte Materialbelege protokollieren
  IF NOT mseg_tab[] IS INITIAL.
*    flg_prot = 3.
    LOOP AT mseg_tab.
      MOVE-CORRESPONDING mseg_tab TO gs_mseg_tab.
      APPEND gs_mseg_tab TO gt_mseg_tab.
    ENDLOOP.
  ENDIF.
* Protokollieren der Warenbewegungen
* Echtlauf: nur die fehlerhaften Warenbewegungen protokollieren
* Simulation: alle potentiellen Warenbewegungen protokollieren
  IF NOT affw_prt[] IS INITIAL.
*    flg_prot = 4.
    SORT affw_prt BY matnr werks lgort charg bwart budat bldat.
    LOOP AT affw_prt.
      IF NOT affw_prt-kzbew IS INITIAL.
        affw_prt-kzear = affw_prt-elikz.
      ENDIF.
      MOVE-CORRESPONDING affw_prt TO gs_affw_prt.             "#EC ENHOK
      IF pasimul IS INITIAL.
        CHECK NOT affw_prt-msgid IS INITIAL.
*       Fehlermeldung aufbereiten
        msg_number = affw_prt-msgno.
        CALL FUNCTION 'K_MESSAGE_TRANSFORM'
             EXPORTING
                  par_msgid         = affw_prt-msgid
                  par_msgno         = msg_number
                  par_msgty         = affw_prt-msgty
                  par_msgv1         = affw_prt-msgv1
                  par_msgv2         = affw_prt-msgv2
                  par_msgv3         = affw_prt-msgv3
                  par_msgv4         = affw_prt-msgv4
             IMPORTING
                  par_msgtx         = msg_text
             EXCEPTIONS
                  no_message_found  = 1
                  par_msgid_missing = 2
                  par_msgno_missing = 3
                  par_msgty_missing = 4
                  OTHERS            = 5.
        IF sy-subrc <> 0.
          CLEAR msg_text.
        ENDIF.
        MOVE: msg_text TO gs_affw_prt-msg_text.
      ENDIF.
      APPEND gs_affw_prt TO gt_affw_prt.
    ENDLOOP.
  ENDIF.

  CLEAR g_flag.

* display protocol
  IF NOT gt_mseg_tab[] IS INITIAL.
    flg_prot = 3.
    PERFORM output_alv1.
  ELSEIF gt_mseg_tab[] IS INITIAL AND
     NOT gt_affw_prt[] IS INITIAL.
    flg_prot = 4.
    PERFORM output_alv.
  ENDIF.

ENDFORM.                               " PROTOKOLL_ERGEBNIS
*eject
*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_EMRKPF
*&---------------------------------------------------------------------*
*     Sperre auf Reservierungskopf zurücknehmen
*----------------------------------------------------------------------*
FORM dequeue_emrkpf USING rsnum_in LIKE resb-rsnum.

  CHECK NOT rsnum_in IS INITIAL.
* RKPF-Sperre
  READ TABLE enq_rsnum WITH KEY rsnum = rsnum_in
                                BINARY SEARCH.
  CHECK sy-subrc = 0.
  enq_rsnum-count = enq_rsnum-count - 1.
  IF enq_rsnum-count = 0.
    DELETE enq_rsnum INDEX sy-tabix.
*   einzelne Reservierung entsperren
    CALL FUNCTION 'DEQUEUE_EMRKPF'
         EXPORTING
              rsnum  = rsnum_in
         EXCEPTIONS
              OTHERS = 0.
  ELSE.
    MODIFY enq_rsnum INDEX sy-tabix.
  ENDIF.

ENDFORM.                               " DEQUEUE_EMRKPF
*eject
*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_ESAFFW
*&---------------------------------------------------------------------*
*     Sperre auf AFFW-Fehlersatz zurücknehmen
*----------------------------------------------------------------------*
FORM dequeue_esaffw USING weblnr_in  TYPE affw-weblnr
                          weblpos_in TYPE affw-weblpos
                          msgid_in   TYPE affw-msgid
                          msgno_in   TYPE affw-msgno.

  CHECK NOT msgno_in IS INITIAL.
  tafwd_tab-arbgb = msgid_in.
  tafwd_tab-msgnr = msgno_in.
  READ TABLE tafwd_tab WITH KEY arbgb = tafwd_tab-arbgb
                                msgnr = tafwd_tab-msgnr
                                BINARY SEARCH.
  CHECK sy-subrc <> 0.
* AFFW-Sperre freigeben
  CALL FUNCTION 'DEQUEUE_ESAFFW'
       EXPORTING
            weblnr  = weblnr_in
            weblpos = weblpos_in
       EXCEPTIONS
            OTHERS  = 0.

ENDFORM.                               " DEQUEUE_ESAFFW
*eject
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_alv.

  DATA : lt_fieldcat TYPE slis_t_fieldcat_alv,
         lt_eventtab TYPE slis_t_event,
         ls_fieldcat TYPE slis_fieldcat_alv,
         ls_layout   TYPE slis_layout_alv,
         ls_variant  TYPE disvariant.
  DATA:  lv_repid LIKE sy-repid.

  FIELD-SYMBOLS: <fs1> TYPE slis_fieldcat_alv.

  ADD 1 TO xg_counter.

  IF gt_affw_prt[] IS INITIAL AND
     gt_mseg_tab[] IS NOT INITIAL.
    EXIT.
  ENDIF.
* Set variant
  IF pasimul IS INITIAL.
    CONCATENATE sy-repid 'F' INTO lv_repid.
    ls_variant-report = lv_repid.
  ELSE.
    CONCATENATE sy-repid 'S' INTO lv_repid.
    ls_variant-report = lv_repid.
  ENDIF.
* Build the layout for the ALV
  PERFORM layout_get CHANGING ls_layout.
* Build the Event List for the Simple ALV.
  PERFORM eventtab_alv_build USING 0 CHANGING lt_eventtab.
* Build the field catalog for the ALV.
  PERFORM fieldcat_alv_merge USING gc_tab
                                   gc_stru
                             CHANGING  lt_fieldcat.

* Subroutine to build Field catalog in ALV
  PERFORM fieldcat_build  CHANGING lt_fieldcat.

  IF gt_affw_prt[] IS INITIAL.
    LOOP AT lt_fieldcat ASSIGNING <fs1>.
      <fs1>-tech = 'X'.
    ENDLOOP.
  ENDIF.
* Display Result
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program             = gc_repid
      i_callback_pf_status_set       = gc_pf_status
      i_callback_user_command        = gc_user_command
      is_layout                      = ls_layout
      it_fieldcat                    = lt_fieldcat
      it_events                      = lt_eventtab
      i_save                         = yx
      is_variant                     = ls_variant
    TABLES
      t_outtab                       = gt_affw_prt
    EXCEPTIONS
      program_error                  = 1
      OTHERS                         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                               " OUTPUT_ALV
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_ALV1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_alv1 .

  DATA : lt_fieldcat TYPE slis_t_fieldcat_alv,
         lt_eventtab TYPE slis_t_event,
         ls_fieldcat TYPE slis_fieldcat_alv,
         ls_layout   TYPE slis_layout_alv,
         ls_variant  TYPE disvariant.
  DATA:  lv_repid LIKE sy-repid.

  ADD 1 TO xg_counter.

* Set variant
  IF gt_affw_prt[] IS INITIAL.
    CONCATENATE sy-repid 'M' INTO lv_repid.
    ls_variant-report = lv_repid.
  ELSE.
    CLEAR ls_variant.
  ENDIF.
* Build the layout for the ALV
  PERFORM layout_get CHANGING ls_layout.
* Build the Event List for the Simple ALV.
  IF gt_affw_prt[] IS INITIAL.
    PERFORM eventtab_alv_build USING 0 CHANGING lt_eventtab.
  ELSE.
    PERFORM eventtab_alv_build USING 1 CHANGING lt_eventtab.
  ENDIF.
* Build the field catalog for the ALV.
  PERFORM fieldcat_alv_merge USING gc_tab1
                                   gc_stru1
                                   CHANGING lt_fieldcat.
* Display list
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program         = gc_repid
      i_callback_pf_status_set   = gc_pf_status
      i_callback_user_command    = gc_user_command
      is_layout                  = ls_layout
      it_fieldcat                = lt_fieldcat
      it_events                  = lt_eventtab
      i_save                     = yx
      is_variant                 = ls_variant
    TABLES
      t_outtab                   = gt_mseg_tab
    EXCEPTIONS
      program_error              = 1
      OTHERS                     = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                               " OUTPUT_ALV1
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*      <--P_LT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM fieldcat_build CHANGING xt_fieldcat TYPE slis_t_fieldcat_alv.

  FIELD-SYMBOLS : <fs> TYPE slis_fieldcat_alv.

* Hiding all the fields of Blank Internal table passed to the ALV
* Function Module
  LOOP AT xt_fieldcat ASSIGNING <fs>.
    CASE <fs>-fieldname.
      WHEN 'KZEAR' .
        IF sy-batch IS INITIAL.
          <fs>-checkbox = gc_x.
        ENDIF.
      WHEN 'MSGID' OR 'MSGTY' OR 'MSGNO' OR 'MSGV1' OR 'MSGV2' OR
            'MSGV3' OR 'MSGV4'.
        <fs>-tech = gc_x .
      WHEN 'MSG_TEXT'.
        IF pasimul = gc_x.
          <fs>-tech = gc_x.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.                               " FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_GET
*&---------------------------------------------------------------------*
*      <--XS_LAYOUT  text
*----------------------------------------------------------------------*
FORM layout_get CHANGING xs_layout TYPE slis_layout_alv.

  CONSTANTS lc_x  TYPE c VALUE 'X'.

  CLEAR xs_layout.

  CASE xg_counter.
    WHEN 1.
      xs_layout-list_append       = gc_y.
      xs_layout-min_linesize      = gc_width.
    WHEN OTHERS.
      xs_layout-list_append       = gc_x.
  ENDCASE.

ENDFORM.                               " LAYOUT_GET
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_ALV_BUILD
*&---------------------------------------------------------------------*
*      <--XT_EVENTTAB  text
*----------------------------------------------------------------------*
FORM eventtab_alv_build USING u_counter TYPE n
                        CHANGING xt_eventtab TYPE slis_t_event.

  DATA: ls_events TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = xt_eventtab.

  READ TABLE xt_eventtab INTO ls_events WITH KEY name =
  slis_ev_top_of_page.
  IF sy-subrc = 0.
    ls_events-form = gc_top_of_page.
    MODIFY xt_eventtab FROM ls_events INDEX sy-tabix TRANSPORTING form.
  ENDIF.

  READ TABLE xt_eventtab INTO ls_events
                         WITH KEY name = slis_ev_end_of_list.
  IF sy-subrc = 0.
    IF u_counter = 1.
      ls_events-form = gc_end_of_list1.
      MODIFY xt_eventtab FROM ls_events INDEX sy-tabix TRANSPORTING
      form.
    ELSE.
      ls_events-form = gc_end_of_list.
     MODIFY xt_eventtab FROM ls_events INDEX sy-tabix TRANSPORTING
            form.
    ENDIF.
  ENDIF.

ENDFORM.                               " EVENTTAB_ALV_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE1                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page1.                                          "#EC CALLED

  REFRESH: gt_commentary.

* Subroutine to build heading
  PERFORM header_build USING gt_commentary.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
         it_list_commentary = gt_commentary.

ENDFORM.                    "TOP_OF_PAGE1
*&---------------------------------------------------------------------*
*&      Form  HEADER_BUILD
*&---------------------------------------------------------------------*
*      -->XT_TOP_OF_PAGE  text
*----------------------------------------------------------------------*
FORM header_build USING xt_top_of_page TYPE slis_t_listheader.

  DATA: dat1(10) TYPE c,
        dat2(8)  TYPE c.

  CLEAR: gs_commentary, g_key, g_tdline.               "New code

  WRITE: sy-datlo TO dat1,
          sy-timlo TO dat2.

  CONCATENATE dat1 dat2 sy-title INTO g_tdline SEPARATED BY space.
* build main header line
  PERFORM append_commentary USING gc_com_h g_key g_tdline.

  CLEAR gs_commentary.

  IF g_txt IS NOT INITIAL.
    gs_commentary-typ  = gc_com_h.
    gs_commentary-info = g_txt.
    APPEND gs_commentary TO xt_top_of_page.
    CLEAR gs_commentary.
  ENDIF.

  IF pasimul = yx.
    CALL FUNCTION 'PAK_GET_SHORTTEXT_DTEL'
      EXPORTING
        i_elem_key   = gc_p_simul
        i_language   = sy-langu
      IMPORTING
        e_short_text = g_sh_txt.

    gs_commentary-typ  = gc_com_s.
    gs_commentary-info = g_sh_txt.
    APPEND gs_commentary TO xt_top_of_page.
    CLEAR: gs_commentary.
  ENDIF.

* additional header line
  gs_commentary-typ  = gc_com_s.
  IF flg_prot = 3.
    gs_commentary-info = text-001.
    flg_prot = flg_prot + 1.
  ELSEIF flg_prot = 4.
    IF pasimul IS INITIAL.
      gs_commentary-info = text-002.
    ELSE.
      gs_commentary-info = text-009.
    ENDIF.
  ENDIF.
  APPEND gs_commentary TO xt_top_of_page.
  CLEAR: gs_commentary.

ENDFORM.                               " HEADER_BUILD
*&---------------------------------------------------------------------*
*&      Form  APPEND_COMMENTARY
*&---------------------------------------------------------------------*
*      -->I_TYP     Commentary type
*      -->I_KEY     Commentary key
*      -->I_TDLINE  Commenatry line
*----------------------------------------------------------------------*
FORM append_commentary USING i_typ TYPE slis_listheader-typ
                             i_key TYPE slis_listheader-key
                             i_tdline TYPE tline-tdline.

* Check string length and wrap if necessary
  IF STRLEN( i_tdline ) > 60.
    CLEAR: gs_tline, gt_tline.
    gs_tline-tdline = i_tdline.
    APPEND gs_tline TO gt_tline.
*   Wrap the 'info' line into separate lines
    CALL FUNCTION 'FORMAT_TEXTLINES'
      EXPORTING
        formatwidth = 60
        linewidth   = 132
        startline   = 1
      TABLES
        lines       = gt_tline
      EXCEPTIONS
        OTHERS      = 0.
*   Get back string parts and append to commentary table
    LOOP AT gt_tline INTO gs_tline.
      CLEAR gs_commentary.
      IF sy-tabix = 1.
        gs_commentary-key = i_key.
      ENDIF.
      gs_commentary-typ = i_typ.
      gs_commentary-info = gs_tline-tdline.
      APPEND gs_commentary TO gt_commentary.
    ENDLOOP.
  ELSE.   "String length less than 60
    CLEAR gs_commentary.
    gs_commentary-typ = i_typ.
    gs_commentary-key = i_key.
    gs_commentary-info = i_tdline.
    APPEND gs_commentary TO gt_commentary.
  ENDIF.

ENDFORM.                               " APPEND_COMMENTARY
*---------------------------------------------------------------------*
*       FORM END_OF_LIST                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM end_of_list.                                           "#EC CALLED

  REFRESH: gt_commentary.

* Subroutine to build heading
  PERFORM footer_build USING gt_commentary.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
         it_list_commentary = gt_commentary.

ENDFORM.                               " END_OF_LIST
*---------------------------------------------------------------------*
*       FORM END_OF_LIST                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM end_of_list1.                                          "#EC CALLED

  IF affw_prt IS NOT INITIAL.
    PERFORM output_alv.
  ELSE.
    PERFORM footer_build USING gt_commentary.
  ENDIF.

ENDFORM.                               " END_OF_LIST
*&---------------------------------------------------------------------*
*&      Form  FOOTER_BUILD
*&---------------------------------------------------------------------*
*      -->XT_END_OF_LIST  text
*----------------------------------------------------------------------*
FORM footer_build USING xt_end_of_list TYPE slis_t_listheader.

  DATA: lv_dat1(10) TYPE c,
        lv_dat2(8)  TYPE c,
        lv_total(5) TYPE c,
        lv_txt1(60) TYPE c.

  CLEAR: gs_commentary.
* number of records selected
  lv_total = n_total.
  CONCATENATE text-003 lv_total INTO lv_txt1 SEPARATED BY space.
  gs_commentary-typ  = gc_com_a.
  gs_commentary-info = lv_txt1.
  APPEND gs_commentary TO xt_end_of_list.
  CLEAR: gs_commentary,lv_txt1,lv_total.

  IF pasimul IS INITIAL.
*   number of locked GM
    lv_total = n_locked.
    CONCATENATE text-004 lv_total INTO lv_txt1 SEPARATED BY space.
    gs_commentary-typ  = gc_com_a.
    gs_commentary-info = lv_txt1.
    APPEND gs_commentary TO xt_end_of_list.
    CLEAR: gs_commentary,lv_txt1,lv_total.
*   number of failed GM
    lv_total = n_fail.
    CONCATENATE text-005 lv_total INTO lv_txt1 SEPARATED BY space.
    gs_commentary-typ  = gc_com_a.
    gs_commentary-info = lv_txt1.
    APPEND gs_commentary TO xt_end_of_list.
    CLEAR: gs_commentary, lv_txt1,lv_total.
*   number of successful GM
    lv_total = n_ok.
    CONCATENATE text-006 lv_total INTO lv_txt1 SEPARATED BY space.
    gs_commentary-typ  = gc_com_a.
    gs_commentary-info = lv_txt1.
    APPEND gs_commentary TO xt_end_of_list.
    CLEAR: gs_commentary,lv_txt1,lv_total.
*   number of processes
    CONCATENATE text-013 tasknam INTO lv_txt1 SEPARATED BY space.
    gs_commentary-typ  = gc_com_a.
    gs_commentary-info = lv_txt1.
    APPEND gs_commentary TO xt_end_of_list.
    CLEAR: gs_commentary,lv_txt1.
  ENDIF.

* report started
  WRITE: start_dat TO lv_dat1,
         start_tim TO lv_dat2.
  CONCATENATE text-011 lv_dat1 lv_dat2  INTO lv_txt1 SEPARATED BY space.
  gs_commentary-typ  = gc_com_a.
  gs_commentary-info = lv_txt1.
  APPEND gs_commentary TO xt_end_of_list.
  CLEAR: gs_commentary,lv_dat1,lv_dat2,lv_txt1.

* report finished
  WRITE: sy-datlo TO lv_dat1,
         sy-timlo TO lv_dat2.
  CONCATENATE text-012 lv_dat1 lv_dat2  INTO lv_txt1 SEPARATED BY space.
  gs_commentary-typ  = gc_com_a.
  gs_commentary-info = lv_txt1.
  APPEND gs_commentary TO xt_end_of_list.
  CLEAR: gs_commentary,lv_dat1,lv_dat2,lv_txt1.

ENDFORM.                               " FOOTER_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_ALV_MERGE
*&---------------------------------------------------------------------*
*      -->IV_TAB  text
*      -->IV_STRU  text
*      <--XT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM fieldcat_alv_merge USING    iv_tab
                                 iv_stru
                        CHANGING xt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name             = gc_repid
      i_internal_tabname         = iv_tab
      i_structure_name           = iv_stru
      i_inclname                 = gc_repid
    CHANGING
      ct_fieldcat                = xt_fieldcat
    EXCEPTIONS
      inconsistent_interface     = 1
      program_error              = 2
      OTHERS                     = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                               " FIELDCAT_ALV_MERGE
*&--------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&--------------------------------------------------------------------*
*      -->RT_EXTAB   text
*---------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.        "#EC CALLED

  DATA: wa_extab TYPE slis_extab.

  IF gt_mseg_tab[] IS NOT INITIAL AND
     gt_affw_prt[] IS NOT INITIAL.
*   if both lists are displayed some functions are to be suppressed
    wa_extab-fcode = '&ODN'.           "Sortieren ab
    APPEND wa_extab TO rt_extab.
    wa_extab-fcode = '&OUP'.           "Sortieren auf
    APPEND wa_extab TO rt_extab.
    wa_extab-fcode = '&ILT'.           "Filtern
    APPEND wa_extab TO rt_extab.
    wa_extab-fcode = '&OAD'.           "Layout holen
    APPEND wa_extab TO rt_extab.
    wa_extab-fcode = '&OLX'.           "Layout ändern
    APPEND wa_extab TO rt_extab.
    wa_extab-fcode = '&OL0'.           "Layout ändern
    APPEND wa_extab TO rt_extab.
    wa_extab-fcode = '&AVE'.           "Layout sichern
    APPEND wa_extab TO rt_extab.
    wa_extab-fcode = '&ERW'.           "Layout verwalten
    APPEND wa_extab TO rt_extab.
    wa_extab-fcode = '&AQW'.           "Textverarbeitung
    APPEND wa_extab TO rt_extab.
    wa_extab-fcode = '&XXL'.           "Tabellenkalkulation
    APPEND wa_extab TO rt_extab.
    wa_extab-fcode = '&XML'.           "XML-Export
    APPEND wa_extab TO rt_extab.
  ELSEIF gt_mseg_tab[] IS NOT INITIAL AND
         gt_affw_prt[] IS     INITIAL.
*   only mat.docs are displayed, allow additional functions
    LOOP AT rt_extab INTO wa_extab WHERE fcode = '&ILD'
                                   OR    fcode = '&URL'.
      DELETE rt_extab.
    ENDLOOP.
  ELSEIF gt_mseg_tab[] IS     INITIAL AND
         gt_affw_prt[] IS NOT INITIAL.
*   only failed GMs are displayed, allow additional functions
    LOOP AT rt_extab INTO wa_extab WHERE fcode = '&ILD'
                                   OR    fcode = '&AVE'
                                   OR    fcode = '&ERW'
                                   OR    fcode = '&SUM'
                                   OR    fcode = '&UMC'
                                   OR    fcode = '&URL'.
      DELETE rt_extab.
    ENDLOOP.
  ENDIF.

* set status
  SET PF-STATUS 'GUI_STATUS_ALV' EXCLUDING rt_extab.             "#EC *

ENDFORM.                               " SET_PF_STATUS
*&--------------------------------------------------------------------*
*&      Form  USER_COMMANDS
*&--------------------------------------------------------------------*
*      -->R_UCOMM    text
*      -->RS_SELFIELDtext
*---------------------------------------------------------------------*
FORM user_commands USING r_ucomm LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.    "#EC CALLED

  DATA l_dokname(20) TYPE c.
  DATA lt_link TYPE STANDARD TABLE OF tline WITH HEADER LINE.

  CASE r_ucomm.
    WHEN 'DETA' OR '&IC1'.                "detail button or doubleclick
      GET CURSOR LINE tabix.
      READ LINE tabix.
      IF rs_selfield-sel_tab_field(4) = 'GT_M'.
*       Show material document
        READ TABLE gt_mseg_tab INTO gt_mseg_tab
          INDEX rs_selfield-tabindex.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              i_mblnr             = gt_mseg_tab-wablnr
              i_mjahr             = gt_mseg_tab-mjahr
            EXCEPTIONS
              illegal_combination = 1
              OTHERS              = 2.
          CHECK sy-subrc = 0.
        ENDIF.
      ENDIF.
      IF rs_selfield-sel_tab_field(4) = 'GT_A'.
*       Show error information
        READ TABLE gt_affw_prt INTO gt_affw_prt
          INDEX rs_selfield-tabindex.
        IF sy-subrc IS INITIAL.
          CHECK NOT gt_affw_prt-msgno IS INITIAL.
          CONCATENATE gt_affw_prt-msgid gt_affw_prt-msgno
            INTO l_dokname.
          CALL FUNCTION 'HELP_OBJECT_SHOW'
            EXPORTING
              dokclass                      = 'NA'
              doklangu                      = sy-langu
              dokname                       = l_dokname
              msg_var_1                     = gt_affw_prt-msgv1
              msg_var_2                     = gt_affw_prt-msgv2
              msg_var_3                     = gt_affw_prt-msgv3
              msg_var_4                     = gt_affw_prt-msgv4
            TABLES
              links                         = lt_link.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                               " USER_COMMANDS
