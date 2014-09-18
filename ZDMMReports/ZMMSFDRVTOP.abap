*&---------------------------------------------------------------------*
*&  Include           ZMMSFDRVTOP
*&---------------------------------------------------------------------*
program ZMMSFDRV message-id me.
type-pools:   addi, meein,
              mmpur.
tables: nast,                          "Messages
        *nast,                         "Messages
        tnapr,                         "Programs & Forms
        itcpo,                         "Communicationarea for Spool
        arc_params,                    "Archive parameters
        toa_dara,                      "Archive parameters
        addr_key.                      "Adressnumber for ADDRESS

type-pools szadr.
tables : varposr,
         rwerter,
         mtxh.

data  umbruch type i value 4.
data  headerflag.
data  begin of vartab occurs 15.
        include structure varposr.
data  end   of vartab.
data  tabn type i.
data  taba type i.
data  ebelph like ekpo-ebelp.
data  bis type i.
data  xmax type i.
data  tab like varposr-yzeile.
data  diff type i.
data  ldat_sam like eket-eindt.

data: s, v type i.
data: sampr like pekpov-netpr, varpr like pekpov-netpr.

* Struktur zur Variantenbildung

data: begin of wertetab occurs 30.
        include structure rwerter.
data: atzhl like econf_out-atzhl,
      end of wertetab.

* Interne Tabelle fuer Konditionen
data: begin of kond occurs 30.
        include structure komvd.
data: end of kond.
* Hilfsfelder
data:
      merknamex(15) type c,            "Merkmalname x-Achse
      merknamey(15) type c,            "Merkmalname y-Achse
      merknrx like rwerter-nr,         "Int. Merkmal x-Achse
      merknry like rwerter-nr,         "Int. Merkmal y-Achse
      i type i value 1,
      nr like cawn-atinn.
data: inserterror(1),sum type i,menge type i,gsumh type i, xmaxh type i.
data: gsumv type i.
* Matrixflag
data: m_flag value 'x'.

*- Tabellen -----------------------------------------------------------*
tables: cpkme,
        ekvkp,
        ekko,
        pekko,
        rm06p,
        ekpo,
        pekpo,
        pekpov,
        pekpos,
        eket,
        ekek,
        ekes,
        ekeh,
        ekkn,
        ekpa,
        ekbe,
        eine, *eine,
        lfa1,
        likp,
       *lfa1,
        kna1,
        komk,
        komp,
        komvd,
        ekomd,
        econf_out,
        thead, *thead,
        sadr,
        mdpa,
        mdpm,
        mkpf,
        tinct,
        ttxit,
        tmsi2,
        tq05,
        tq05t,
        t001,
        t001w,
        t006, *t006,
        t006a, *t006a,
        t024,
        t024e,
        t027a,
        t027b,
        t052,
        t161n,
        t163d,
        t166a,
        t165p,
        t166c,
        t166k,
        t166p,
        t166t,
        t166u,
        t165m,
        t165a,
        tmamt,
       *mara,                          "HTN 4.0C
        mara,
        marc,
        mt06e,
        makt,
        vbak,
        vbkd,
       *vbkd,
        vbap.
tables: drad,
        drat.
tables: addr1_sel,
        addr1_val.
tables: v_htnm, rampl,tmppf.           "HTN-Abwicklung

tables: stxh.              "schnellerer Zugriff auf Texte Dienstleistung

tables: t161.              "Abgebotskennzeichen für Dienstleistung

*- INTERNE TABELLEN ---------------------------------------------------*
*- Tabelle der Positionen ---------------------------------------------*
data: begin of xekpo occurs 10.
        include structure ekpo.
data:     bsmng like ekes-menge,
      end of xekpo.

*- Key für xekpo ------------------------------------------------------*
data: begin of xekpokey,
         mandt like ekpo-mandt,
         ebeln like ekpo-ebeln,
         ebelp like ekpo-ebelp,
      end of xekpokey.

*- Tabelle der Einteilungen -------------------------------------------*
data: begin of xeket occurs 10.
        include structure eket.
data:     fzete like pekpo-wemng,
      end of xeket.

*- Tabelle der Einteilungen temporär ----------------------------------*
data: begin of teket occurs 10.
        include structure beket.
data: end of teket.

data: begin of zeket.
        include structure eket.
data:  end of zeket.

*- Tabelle der Positionszusatzdaten -----------------------------------*
data: begin of xpekpo occurs 10.
        include structure pekpo.
data: end of xpekpo.

*- Tabelle der Positionszusatzdaten -----------------------------------*
data: begin of xpekpov occurs 10.
        include structure pekpov.
data: end of xpekpov.

*- Tabelle der Zahlungbedingungen--------------------------------------*
data: begin of zbtxt occurs 5,
         line(50),
      end of zbtxt.

*- Tabelle der Merkmalsausprägungen -----------------------------------*
data: begin of tconf_out occurs 50.
        include structure econf_out.
data: end of tconf_out.

*- Tabelle der Konditionen --------------------------------------------*
data: begin of tkomv occurs 50.
        include structure komv.
data: end of tkomv.

data: begin of tkomk occurs 1.
        include structure komk.
data: end of tkomk.

data: begin of tkomvd occurs 50.       "Belegkonditionen
        include structure komvd.
data: end of tkomvd.

data: begin of tekomd occurs 50.       "Stammkonditionen
        include structure ekomd.
data: end of tekomd.

*- Tabelle der Bestellentwicklung -------------------------------------*
data: begin of xekbe occurs 10.
        include structure ekbe.
data: end of xekbe.

*- Tabelle der Bezugsnebenkosten --------------------------------------*
data: begin of xekbz occurs 10.
        include structure ekbz.
data: end of xekbz.

*- Tabelle der WE/RE-Zuordnung ----------------------------------------*
data: begin of xekbez occurs 10.
        include structure ekbez.
data: end of xekbez.

*- Tabelle der Positionssummen der Bestellentwicklung -----------------*
data: begin of tekbes occurs 10.
        include structure ekbes.
data: end of tekbes.

*- Tabelle der Bezugsnebenkosten der Bestandsführung ------------------*
data: begin of xekbnk occurs 10.
        include structure ekbnk.
data: end of xekbnk.

*- Tabelle für Kopieren Positionstexte (hier wegen Infobestelltext) ---*
data: begin of xt165p occurs 10.
        include structure t165p.
data: end of xt165p.

*- Tabelle der Kopftexte ----------------------------------------------*
data: begin of xt166k occurs 10.
        include structure t166k.
data: end of xt166k.

*- Tabelle der Positionstexte -----------------------------------------*
data: begin of xt166p occurs 10.
        include structure t166p.
data: end of xt166p.

*- Tabelle der Anahngstexte -------------------------------------------*
data: begin of xt166a occurs 10.
        include structure t166a.
data: end of xt166a.

*- Tabelle der Textheader ---------------------------------------------*
data: begin of xthead occurs 10.
        include structure thead.
data: end of xthead.

data: begin of xtheadkey,
         tdobject like thead-tdobject,
         tdname like thead-tdname,
         tdid like thead-tdid,
      end of xtheadkey.

data: begin of qm_text_key occurs 5,
         tdobject like thead-tdobject,
         tdname like thead-tdname,
         tdid like thead-tdid,
         tdtext like ttxit-tdtext,
      end of qm_text_key.

*- Tabelle der Nachrichten alt/neu ------------------------------------*
data: begin of xnast occurs 10.
        include structure nast.
data: end of xnast.

data: begin of ynast occurs 10.
        include structure nast.
data: end of ynast.

*------ Struktur zur Übergabe der Adressdaten --------------------------
data:    begin of addr_fields.
        include structure sadrfields.
data:    end of addr_fields.

*------ Struktur zur Übergabe der Adressreferenz -----------------------
data:    begin of addr_reference.
        include structure addr_ref.
data:    end of addr_reference.

*------ Tabelle zur Übergabe der Fehler -------------------------------
data:    begin of error_table occurs 10.
        include structure addr_error.
data:    end of error_table.

*------ Tabelle zur Übergabe der Adressgruppen ------------------------
data:    begin of addr_groups occurs 3.
        include structure adagroups.
data:    end of addr_groups.

*- Tabelle der Aenderungsbescheibungen --------------------------------*
data: begin of xaend occurs 10,
         ebelp like ekpo-ebelp,
         zekkn like ekkn-zekkn,
         etenr like eket-etenr,
         ctxnr like t166c-ctxnr,
         rounr like t166c-rounr,
         insert,
         flag_adrnr,
      end of xaend.

data: begin of xaendkey,
         ebelp like ekpo-ebelp,
         zekkn like ekkn-zekkn,
         etenr like eket-etenr,
         ctxnr like t166c-ctxnr,
         rounr like t166c-rounr,
         insert,
         flag_adrnr,
      end of xaendkey.

*- Tabelle der Textänderungen -----------------------------------------*
data: begin of xaetx occurs 10,
         ebelp like ekpo-ebelp,
         textart like cdshw-textart,
         chngind like cdshw-chngind,
      end of xaetx.

*- Tabelle der geänderten Adressen ------------------------------------*
data: begin of xadrnr occurs 5,
         adrnr like sadr-adrnr,
         tname like cdshw-tabname,
         fname like cdshw-fname,
      end of xadrnr.

*- Tabelle der gerade bearbeiteten aktive Komponenten -----------------*
data begin of mdpmx occurs 10.
        include structure mdpm.
data end of mdpmx.

*- Tabelle der gerade bearbeiteten Sekundärbedarfe --------------------*
data begin of mdsbx occurs 10.
        include structure mdsb.
data end of mdsbx.

*- Struktur des Archivobjekts -----------------------------------------*
data: begin of xobjid,
        objky  like nast-objky,
        arcnr  like nast-optarcnr,
      end of xobjid.

* Struktur für zugehörigen Sammelartikel
data: begin of sekpo.
        include structure ekpo.
data:   first_varpos,
      end of sekpo.

*- Struktur für Ausgabeergebnis zB Spoolauftragsnummer ----------------*
data: begin of result.
        include structure itcpp.
data: end of result.

*- Struktur für Internet NAST -----------------------------------------*
data: begin of intnast.
        include structure snast.
data: end of intnast.

*- HTN-Abwicklung
data: begin of htnmat occurs 0.
        include structure v_htnm.
data:  revlv like rampl-revlv,
      end of htnmat.

data  htnamp like rampl  occurs 0 with header line.

*- Hilfsfelder --------------------------------------------------------*
data: hadrnr(8),                       "Key TSADR
      elementn(30),                    "Name des Elements
      save_el(30),                     "Rettfeld für Element
      retco like sy-subrc,             "Returncode Druck
      insert,                          "Kz. neue Position
      h-ind like sy-tabix,             "Hilfsfeld Index
      h-ind1 like sy-tabix,            "Hilfsfeld Index
      f1 type f,                       "Rechenfeld
      h-menge like ekpo-menge,         "Hilfsfeld Mengenumrechnung
      h-meng1 like ekpo-menge,         "Hilfsfeld Mengenumrechnung
      h-meng2 like ekpo-menge,         "Hilfsfeld Mengenumrechnung
      ab-menge like ekes-menge,        "Hilfsfeld bestätigte Menge
      kzbzg like konp-kzbzg,           "Staffeln vorhanden?
      hdatum like eket-eindt,          "Hilfsfeld Datum
      hmahnz like ekpo-mahnz,          "Hilfsfeld Mahnung
      addressnum like ekpo-adrn2,      "Hilfsfeld Adressnummer
      tablines like sy-tabix,          "Zähler Tabelleneinträge
      entries  like sy-tfill,          "Zähler Tabelleneinträge
      hstap,                           "statistische Position
      hsamm,                           "Positionen mit Sammelartikel
      hloep,                           "Gelöschte Positionen im Spiel
      hkpos,                           "Kondition zu löschen
      kopfkond,                        "Kopfkonditionen vorhanden
      no_zero_line,                    "keine Nullzeilen
      xdrflg like t166p-drflg,         "Hilfsfeld Textdruck
      xprotect,                        "Kz. protect erfolgt
      archiv_object like toa_dara-ar_object, "für opt. Archivierung
      textflag,                        "Kz. druckrel. Positionstexte
      flag,                            "allgemeines Kennzeichen
      spoolid(10),                     "Spoolidnummer
      xprogram like sy-repid,          "Programm
      lvs_recipient like swotobjid,    "Internet
      lvs_sender like swotobjid,       "Internet
      timeflag,                        "Kz. Uhrzeit bei mind. 1 Eint.
      h_vbeln like vbak-vbeln,
      h_vbelp like vbap-posnr.

*- Drucksteuerung -----------------------------------------------------*
data: aendernsrv.
data: xdruvo.                          "Druckvorgang
data: neu  value '1',                  "Neudruck
      aend value '2',                  "Änderungsdruck
      mahn value '3',                  "Mahnung
      absa value '4',                  "Absage
      lpet value '5',                  "Lieferplaneinteilung
      lpma value '6',                  "Mahnung Lieferplaneinteilung
      aufb value '7',                  "Auftragsbestätigung
      lpae value '8',                  "Änderung Lieferplaneinteilung
      lphe value '9',                  "Historisierte Einteilungen
      preisdruck,                      "Kz. Gesamtpreis drucken
      kontrakt_preis,                  "Kz. Kontraktpreise drucken
      we   value 'E'.                  "Wareneingangswert

*- Hilfsfelder Lieferplaneinteilung -----------------------------------*
data:
      xlpet,                           "Lieferplaneinteilung
      xfz,                             "Fortschrittszahlendarstellung
      xoffen,                          "offene WE-Menge
      xlmahn,                          "Lieferplaneinteilungsmahnung
      fzflag,                          "KZ. Abstimmdatum erreicht
      xnoaend,                         "keine Änderungsbelege da  LPET
      xetdrk,                        "Druckrelevante Positionen da LPET
      xetefz like eket-menge,          "Einteilungsfortschrittszahl
      xwemfz like eket-menge,          "Lieferfortschrittszahl
      xabruf like ekek-abruf,          "Alter Abruf
      p_abart like ekek-abart.         "Abrufart

*data: sum-euro-price like komk-fkwrt.                       "302203
data: sum-euro-price like komk-fkwrt_euro.                  "302203
data: euro-price like ekpo-effwr.

*- Hilfsfelder für Ausgabemedium --------------------------------------*
data: xdialog,                         "Kz. POP-UP
      xscreen,                         "Kz. Probeausgabe
      xformular like tnapr-fonam,      "Formular
      xdevice(10).                     "Ausgabemedium

*- Hilfsfelder für QM -------------------------------------------------*
data: qv_text_i like tq09t-kurztext,   "Bezeichnung Qualitätsvereinb.
      tl_text_i like tq09t-kurztext,   "Bezeichnung Technische Lieferb.
      zg_kz.                           "Zeugnis erforderlich

*- Hilfsfelder für Änderungsbeleg -------------------------------------*
data: objectid              like cdhdr-objectid,
      tcode                 like cdhdr-tcode,
      planned_change_number like cdhdr-planchngnr,
      utime                 like cdhdr-utime,
      udate                 like cdhdr-udate,
      username              like cdhdr-username,
      cdoc_planned_or_real  like cdhdr-change_ind,
      cdoc_upd_object       like cdhdr-change_ind value 'U',
      cdoc_no_change_pointers like cdhdr-change_ind.


*- Common-Part für Änderungsbeleg -------------------------------------*
*include zzfm06lccd.
data:    begin of common part fm06lccd.

*------- Tabelle der Änderunsbelegzeilen (temporär) -------------------*
data: begin of edit occurs 50.             "Änderungsbelegzeilen temp.
        include structure cdshw.
data: end of edit.

data: begin of editd occurs 50.             "Änderungsbelegzeilen temp.
        include structure cdshw.            "für Dienstleistungen
data: end of editd.


*------- Tabelle der Änderunsbelegzeilen (Ausgabeform) ----------------*
data: begin of ausg occurs 50.             "Änderungsbelegzeilen
        include structure cdshw.
data:   changenr like cdhdr-changenr,
        udate    like cdhdr-udate,
        utime    like cdhdr-utime,
      end of ausg.

*------- Tabelle der Änderunsbelegköpfe -------------------------------*
data: begin of icdhdr occurs 50.           "Änderungbelegköpfe
        include structure cdhdr.
data: end of icdhdr.

*------- Key Tabelle der Änderunsbelegköpfe --------------------------*
data: begin of hkey,                       "Key für ICDHDR
        mandt like cdhdr-mandant,
        objcl like cdhdr-objectclas,
        objid like cdhdr-objectid,
        chang like cdhdr-changenr,
      end of hkey.

*------- Key der geänderten Tabelle für Ausgabe ----------------------*
data: begin of ekkey,                    "Tabellenkeyausgabe
        ebeln like ekko-ebeln,
        ebelp like ekpo-ebelp,
        zekkn like ekkn-zekkn,
        etenr like eket-etenr,
        abruf like ekek-abruf,
        ekorg like ekpa-ekorg,           "Änderungsbelege Partner
        ltsnr like ekpa-ltsnr,           "Änderungsbelege Partner
        werks like ekpa-werks,           "Änderungsbelege Partner
        parvw like ekpa-parvw,           "Änderungsbelege Partner
        parza like ekpa-parza,           "Änderungsbelege Partner
        consnumber like adr2-consnumber, "Änderungsbelege Adressen
        comm_type  like adrt-comm_type,  "Änderungsbelege Adressen
      end of ekkey.

data:    end of common part.
*- Direktwerte --------------------------------------------------------*
************************************************************************
*          Direktwerte                                                 *
************************************************************************
*------- Werte zu Trtyp und Aktyp:
constants:  hin value 'H',             "Hinzufuegen
            ver value 'V',             "Veraendern
            anz value 'A',             "Anzeigen
            erw value 'E'.             "Bestellerweiterung

constants:
* BSTYP
  bstyp-info value 'I',
  bstyp-ordr value 'W',
  bstyp-banf value 'B',
  bstyp-best value 'F',
  bstyp-anfr value 'A',
  bstyp-kont value 'K',
  bstyp-lfpl value 'L',
  bstyp-lerf value 'Q',

* BSAKZ
  bsakz-norm value ' ',
  bsakz-tran value 'T',
  bsakz-rahm value 'R',
* BSAKZ-BEIS VALUE 'B',  "not used
* BSAKZ-KONS VALUE 'K',  "not used
* BSAKZ-LOHN VALUE 'L', "not used
* BSAKZ-STRE VALUE 'S', "not used
* BSAKZ-MENG VALUE 'M', "not used
* BSAKZ-WERT VALUE 'W', "not used
* PSTYP
  pstyp-lagm value '0',
  pstyp-blnk value '1',
  pstyp-kons value '2',
  pstyp-lohn value '3',
  pstyp-munb value '4',
  pstyp-stre value '5',
  pstyp-text value '6',
  pstyp-umlg value '7',
  pstyp-wagr value '8',
  pstyp-dien value '9',

* Kzvbr
  kzvbr-anla value 'A',
  kzvbr-unbe value 'U',
  kzvbr-verb value 'V',
  kzvbr-einz value 'E',
  kzvbr-proj value 'P',

* ESOKZ
  esokz-pipe value 'P',
  esokz-lohn value '3',
  esokz-konsi value '2',               "konsi
  esokz-charg value '1',               "sc-jp
  esokz-norm value '0'.

constants:
* Handling von Unterpositionsdaten
       sihan-nix  value ' ',           "keine eigenen Daten
       sihan-anz  value '1', "Daten aus Hauptposition kopiert, nicht änd
       sihan-kop  value '2', "Daten aus Hauptposition kopiert, aber ände
       sihan-eig  value '3'. "eigene Daten (nicht aus Hauptposition kopi

* Unterpositionstypen
constants:
  uptyp-hpo value ' ',                 "Hauptposition
  uptyp-var value '1',                 "Variante
  uptyp-nri value '2',           "Naturalrabatt Inklusive (=Dreingabe)
  uptyp-ler value '3',                 "Leergut
  uptyp-nre value '4',           "Naturalrabatt Exklusive (=Draufgabe)
  uptyp-lot value '5',                 "Lot Position
  uptyp-dis value '6',                 "Display Position
  uptyp-vks value '7',                 "VK-Set Position
  uptyp-mpn value '8',                 "Austauschposition (A&D)
  uptyp-sls value '9',           "Vorkommisionierungsposition (retail)
  uptyp-div value 'X'.           "HP hat UP's mit verschiedenen Typen

* Artikeltypen
constants:
  attyp-sam(2) value '01',             "Sammelartikel
  attyp-var(2) value '02',             "Variante
  attyp-we1(2) value '20',             "Wertartikel
  attyp-we2(2) value '21',             "Wertartikel
  attyp-we3(2) value '22',             "Wertartikel
  attyp-vks(2) value '10',             "VK-Set
  attyp-lot(2) value '11',             "Lot-Artikel
  attyp-dis(2) value '12'.             "Display

* Konfigurationsherkunft
constants:
  kzkfg-fre value ' ',                 "Konfiguration sonst woher
  kzkfg-kan value '1',                 "noch nicht konfiguriert
  kzkfg-eig value '2'.                 "Eigene Konfiguration

constants:
  c_ja   type c value 'X',
  c_nein type c value ' '.

* Vorgangsart, welche Anwendung den Fkt-Baustein aufruft
constants:
  cva_ab(1) value 'B',     "Automatische bestellung (aus banfen)
  cva_we(1) value 'C',                 "Wareneingang
  cva_bu(1) value 'D',     "Übernahme bestellungen aus fremdsystem
  cva_au(1) value 'E',                 "Aufteiler
  cva_kb(1) value 'F',                 "Kanban
  cva_fa(1) value 'G',                 "Filialauftrag
  cva_dr(1) value 'H',                                      "DRP
  cva_en(1) value '9',                 "Enjoy
  cva_ap(1) value '1',                                      "APO
  cva_ed(1) value 'T'.     "EDI-Eingang Auftragsbestätigung Update Preis

* Status des Einkaufsbeleges (EKKO-STATU)
constants:
  cks_ag(1) value 'A',                 "Angebot vorhanden für Anfrage
  cks_ab(1) value 'B',     "Automatische Bestellung (aus Banfen) ME59
  cks_we(1) value 'C',                 "Bestellung aus Wareneingang
  cks_bu(1) value 'D',                 "Bestellung aus Datenübernahme
  cks_au(1) value 'E',     "Bestellung aus Aufteiler (IS-Retail)
  cks_kb(1) value 'F',                 "Bestellung aus Kanban
  cks_fa(1) value 'G',     "Bestellung aus Filialauftrag (IS-Retail)
  cks_dr(1) value 'H',                 "Bestellung aus DRP
  cks_ba(1) value 'I',                 "Bestellung aus BAPI
  cks_al(1) value 'J',                 "Bestellung aus ALE-Szenario
  cks_sb(1) value 'S',                 "Sammelbestellung (IS-Retail)
  cks_ap(1) value '1',                                      "APO
  cks_en(1) value '9',                 "Enjoy Bestellung
  cks_fb(1) value 'X'.                 "Bestellung aus Funktionsbaustein

* Vorgang aus T160
constants:
  vorga-angb(2) value 'AG',   "Angebot zur Anfrage    ME47, ME48
  vorga-lpet(2) value 'LE',   "Lieferplaneinteilung   ME38, ME39
  vorga-frge(2) value 'EF',   "Einkaufsbelegfreigabe  ME28, ME35, ME45
  vorga-frgb(2) value 'BF',   "Banffreigabe           ME54, ME55
  vorga-bgen(2) value 'BB',            "Best. Lief.unbekannt   ME25
  vorga-anha(2) value 'FT',   "Textanhang             ME24, ME26,...
  vorga-banf(2) value 'B ',   "Banf                   ME51, ME52, ME53
  vorga-anfr(2) value 'A ',   "Anfrage                ME41, ME42, ME43
  vorga-best(2) value 'F ',   "Bestellung             ME21, ME22, ME23
  vorga-kont(2) value 'K ',   "Kontrakt               ME31, ME32, ME33
  vorga-lfpl(2) value 'L ',   "Lieferplan             ME31, ME32, ME33
  vorga-mahn(2) value 'MA',            "Liefermahnung          ME91
  vorga-aufb(2) value 'AB'.            "Bestätigungsmahnung    ME92

* Felder für Feldauswahl (früher FMMEXCOM)
data:       endmaske(210) type c,
            kmaske(140) type c,
            auswahl0 type brefn,
            auswahl1 type brefn,
            auswahl2 type brefn,
            auswahl3 type brefn,
            auswahl4 type brefn,
            auswahl5 type brefn,
            auswahl6 type brefn.

* Sonderbestandskennzeichen
constants:
  sobkz-kdein value 'E',               "Kundeneinzel
  sobkz-prein value 'Q',               "Projekteinzel
  sobkz-lohnb value 'O'.               "Lohnbearbeiterbeistell

* Min-/Maxwerte für Datenelemente
constants:
* offener Rechnungseingangswert / Feldlänge: 13 / Dezimalstellen: 2
  c_max_orewr       like rm06a-orewr   value '99999999999.99',
  c_max_orewr_f     type f             value '99999999999.99',
  c_max_orewr_x(15) type c             value '**************',

  c_max_proz_p(3)   type p decimals 2  value '999.99',          "@80545
  c_max_proz_x(6)   type c             value '******',          "@80545

  c_max_menge       like ekpo-menge  value '9999999999.999',    "@83886
  c_max_menge_f     type f           value '9999999999.999',    "@83886

  c_max_netwr       like ekpo-netwr  value '99999999999.99',    "@83886
  c_max_netwr_f     type f           value '99999999999.99'.    "@83886


* Distribution Indicator Account assignment
constants:
  c_dist_ind-single   value ' ',       "no multiple = single
  c_dist_ind-quantity value '1',       "quantity distribution
  c_dist_ind-percent  value '2'.       "percentag

* Datendefinitionen für Dienstleistungen
tables: eslh,
        esll,
        ml_esll,
        rm11p.

data  begin of gliederung occurs 50.
        include structure ml_esll.
data  end   of gliederung.

data  begin of leistung occurs 50.
        include structure ml_esll.
data  end   of leistung.

data  return.

*- interne Tabelle für Abrufköpfe -------------------------------------*
data: begin of xekek          occurs 20.
        include structure iekek.
data: end of xekek.

*- interne Tabelle für Abrufköpfe alt----------------------------------*
data: begin of pekek          occurs 20.
        include structure iekek.
data: end of pekek.

*- interne Tabelle für Abrufeinteilungen ------------------------------*
data: begin of xekeh          occurs 20.
        include structure iekeh.
data: end of xekeh.

*- interne Tabelle für Abrufeinteilungen ------------------------------*
data: begin of tekeh          occurs 20.
        include structure iekeh.
data: end of tekeh.

*- Zusatztabelle Abruf nicht vorhanden XEKPO---------------------------*
data: begin of xekpoabr occurs 20,
         mandt like ekpo-mandt,
         ebeln like ekpo-ebeln,
         ebelp like ekpo-ebelp,
      end of xekpoabr.

*-- Daten Hinweis 39234 -----------------------------------------------*
*- Hilfstabelle Einteilungen ------------------------------------------*
data: begin of heket occurs 10.
        include structure eket.
data:       tflag like sy-calld,
      end of heket.

*- Key für HEKET ------------------------------------------------------*
data: begin of heketkey,
         mandt like eket-mandt,
         ebeln like eket-ebeln,
         ebelp like eket-ebelp,
         etenr like eket-etenr,
      end of heketkey.

data: h_subrc like sy-subrc,
      h_tabix like sy-tabix,
      h_field like cdshw-f_old,
      h_eindt like rvdat-extdatum.
data  z type i.

* Defintionen für Formeln

type-pools msfo.

data: variablen type msfo_tab_variablen with header line.

data: formel type msfo_formel.

* Definition für Rechnungsplan

data: tfpltdr like fpltdr occurs 0 with header line.

data: fpltdr like fpltdr.

* Definiton Defaultschema für Dienstleistung

constants: default_kalsm like t683-kalsm value 'MS0000',
           default_kalsm_stamm like t683-kalsm value 'MS0001'.

data: bstyp like ekko-bstyp,
      bsart like ekko-bsart.


data dkomk like komk.

* Defintion für Wartungsplan
tables: rmipm.

data: mpos_tab like mpos occurs 0 with header line,
      zykl_tab like mmpt occurs 0 with header line.

data: print_schedule.

data: begin of d_tkomvd occurs 50.
        include structure komvd.
data: end of d_tkomvd.
data: begin of d_tkomv occurs 50.
        include structure komv.
data: end of d_tkomv.


* Definition Drucktabellen blockweises Lesen

data: leistung_thead like stxh occurs 1 with header line.
data: gliederung_thead like stxh occurs 1 with header line. "HS

data: begin of thead_key,
        mandt    like sy-mandt,
        tdobject like stxh-tdobject,
        tdname   like stxh-tdname,
        tdid     like stxh-tdid,
        tdspras  like stxh-tdspras.
data: end of thead_key.

ranges: r1_tdname for stxh-tdname,
        r2_tdname for stxh-tdname.

data: begin of doktab occurs 0.
        include structure drad.
data  dktxt like drat-dktxt.
data: end of doktab.

*  Additionals Tabelle (CvB/4.0c)
data: l_addis_in_orders type line of addi_buying_print_itab
                                        occurs 0 with header line.
*  Die Additionals-Strukturen müssen bekannt sein
tables: wtad_buying_print_addi, wtad_buying_print_extra_text.

data: ls_print_data_to_read type lbbil_print_data_to_read.
data: ls_bil_invoice type lbbil_invoice.
data: lf_fm_name            type rs38l_fnam.
data: ls_control_param      type ssfctrlop.
data: ls_composer_param     type ssfcompop.
data: ls_recipient          type swotobjid.
data: ls_sender             type swotobjid.
data: lf_formname           type tdsfname.
data: ls_addr_key           like addr_key,
      dunwitheket           type xfeld.

data: l_zekko                  like ekko,
      l_xpekko                 like pekko,
      l_xekpo                like table of ekpo,
      l_wa_xekpo             like ekpo.

data: l_xekpa like ekpa occurs 0,
      l_wa_xekpa like ekpa.
data: l_xpekpo  like pekpo occurs 0,
      l_wa_xpekpo like pekpo,
      l_xeket   like table of eket with header line,
      l_xekkn  like table of ekkn with header line,
      l_xekek  like table of ekek with header line,
      l_xekeh   like table of ekeh with header line,
      l_xkomk like table of komk with header line,
      l_xtkomv  type komv occurs 0,
      l_wa_xtkomv type komv.
data   ls_ssfcompop  type     ssfcompop.
