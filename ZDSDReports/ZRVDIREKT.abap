*---------------------------------------------------------------------*
*     Direktwerte fuer RV                                           *
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*     A-B-C-...                                                       *
*---------------------------------------------------------------------*
DATA:   BEGIN OF COMMON PART rvdirekt.
CONSTANTS:
      char_ TYPE c VALUE ' ',
      chara TYPE c VALUE 'A',
      charb TYPE c VALUE 'B',
      charc TYPE c VALUE 'C',
      chard TYPE c VALUE 'D',
      chare TYPE c VALUE 'E',
      charf TYPE c VALUE 'F',
      charg TYPE c VALUE 'G',
      charh TYPE c VALUE 'H',
      chari TYPE c VALUE 'I',
      charj TYPE c VALUE 'J',
      chark TYPE c VALUE 'K',
      charl TYPE c VALUE 'L',
      charm TYPE c VALUE 'M',
      charn TYPE c VALUE 'N',
      charo TYPE c VALUE 'O',
      charp TYPE c VALUE 'P',
      charq TYPE c VALUE 'Q',
      charr TYPE c VALUE 'R',
      chars TYPE c VALUE 'S',
      chart TYPE c VALUE 'T',
      charu TYPE c VALUE 'U',
      charv TYPE c VALUE 'V',
      charw TYPE c VALUE 'W',
      charx TYPE c VALUE 'X',
      chary TYPE c VALUE 'Y',
      charz TYPE c VALUE 'Z',
      char0 TYPE c VALUE '0',
      char1 TYPE c VALUE '1',
      char2 TYPE c VALUE '2',
      char3 TYPE c VALUE '3',
      char4 TYPE c VALUE '4',
      char5 TYPE c VALUE '5',
      char6 TYPE c VALUE '6',
      char7 TYPE c VALUE '7',
      char8 TYPE c VALUE '8',
      char9 TYPE c VALUE '9',
      char20(2) TYPE c VALUE '20',
      char* TYPE c VALUE '*',
      char$ TYPE c VALUE '$',
      char% TYPE c VALUE '%',
*     CHAR+ TYPE C VALUE '+',  <-- Umstellung der Variable auf char_plus
      char- TYPE c VALUE '-',
      char_plus TYPE c VALUE '+',                          "note 304108
      char00(2) TYPE c VALUE '00'.                         "note 304108

*eject

*---------------------------------------------------------------------*
*     logische Direktwerte                                            *
*---------------------------------------------------------------------*
CONSTANTS:
      ja           TYPE c VALUE 'J',
      nein         TYPE c VALUE 'N',
      screen_on    TYPE c VALUE '1',
      screen_off   TYPE c VALUE '0',
*eject

*---------------------------------------------------------------------*
*     Direktwerte zum Update-Kennzeichen                              *
*---------------------------------------------------------------------*
      updkz_old(1)      VALUE ' ',     "Keine Veraenderung
      updkz_new(1)      VALUE 'I',     "Neue Position
      updkz_update(1)   VALUE 'U',     "Geaenderte Position
      updkz_delete(1)   VALUE 'D',     "Löschen

*eject

*---------------------------------------------------------------------*
*     Direktwerte zum Selektionskennzeichen                           *
*---------------------------------------------------------------------*
      selektiert(1)     VALUE 'X',     "Selektiert
      abgearbeitet(1)   VALUE ' ',     "abgearbeitet, früher '*'

*eject

*----------------------------------------------------------------------*
*     Direktwerte für Zeitpunkte                                       *
*----------------------------------------------------------------------*
      time_hhmmss_low(6)                VALUE '000000',

*eject

*----------------------------------------------------------------------*
*     Direktwerte für Positionsnummern                                 *
*----------------------------------------------------------------------*
      posnr_low         LIKE vbap-posnr  VALUE '000000',
      posnr_first       LIKE vbap-posnr  VALUE '000001',
      posnr_high        LIKE vbap-posnr  VALUE '999999',
      pvsrt_high        LIKE vbapd-pvsrt VALUE '999999',

      etenr_low LIKE vbep-etenr VALUE '0000',

* Nummer des aktuellen Abrufs in Lieferplänen mit Abrufen
      abrli_low LIKE vblb-abrli VALUE '0000',
* ABLLI, wenn es nur einen Abruf gibt
      ablli_high LIKE vblb-ablli VALUE '9999',

*eject

*----------------------------------------------------------------------*
*     Direktwerte für Materialtypen                                    *
*----------------------------------------------------------------------*
  sam_typ LIKE mara-attyp VALUE '01',
  var_typ LIKE mara-attyp VALUE '02',
  set_typ LIKE mara-attyp VALUE '10',
  lot_typ LIKE mara-attyp VALUE '11',
  dis_typ LIKE mara-attyp VALUE '12',

*eject

*---------------------------------------------------------------------*
*     Direktwert für logistische Mengeneinheiten                      *
*---------------------------------------------------------------------*
  appview_sd(2) VALUE '02',

*eject

*---------------------------------------------------------------------*
*     Hilfswerte                                                      *
*---------------------------------------------------------------------*
        help-mandt(3) VALUE '000',
*eject

*---------------------------------------------------------------------*
*     Funktionscodes                                                  *
*---------------------------------------------------------------------*
  fcode_abbrechen                LIKE t185f-fcode VALUE 'ABBR',
  fcode_abbrechen1               LIKE t185f-fcode VALUE 'ABBA',
  fcode_abbrechen2               LIKE t185f-fcode VALUE 'ABBC',
  fcode_abbrechen3               LIKE t185f-fcode VALUE 'ABBB',
  fcode_abrufkopf                LIKE t185f-fcode VALUE 'DSHD',
  fcode_abruf_generieren         LIKE t185f-fcode VALUE 'PABG',
  fcode_abruf_mit_vor            LIKE t185f-fcode VALUE 'PABV',
  fcode_abruf_ohne_vor           LIKE t185f-fcode VALUE 'PABN',
  fcode_abruf_ohne_vor_at_exit   LIKE t185f-fcode VALUE '/EPABN',
  fcode_aendern                  LIKE t185f-fcode VALUE 'AEND',
  fcode_aenderung                LIKE t185f-fcode VALUE 'PAEN',
  fcode_aenderung_dat            LIKE t185f-fcode VALUE 'PAED',
  fcode_aenderung_fz             LIKE t185f-fcode VALUE 'PAEF',
  fcode_anlegen                  LIKE t185f-fcode VALUE 'ANLE',
  fcode_anforderung              LIKE t185f-fcode VALUE 'ENT2',
  fcode_ausgeben                 LIKE t185f-fcode VALUE 'LIST',
  fcode_auftraggeber_anlegen     LIKE t185f-fcode VALUE 'IAAN',
  fcode_auftraggeber_anzeigen    LIKE t185f-fcode VALUE 'IAUF',
  fcode_beleg_absagen            LIKE t185f-fcode VALUE 'BABS',
  fcode_beleg_verfuegbarkeit     LIKE t185f-fcode VALUE 'BVFP',
  fcode_beleg_loeschen           LIKE t185f-fcode VALUE 'LOES',
  fcode_beleg_suchen             LIKE t185f-fcode VALUE 'SUCH',
  fcode_beleg_suchen_lp          LIKE t185f-fcode VALUE 'SULP',
  fcode_bestellanforderung       LIKE t185f-fcode VALUE 'EIBA',
  fcode_buchhaltung_kopf         LIKE t185f-fcode VALUE 'KBUC',
  fcode_crlt                     LIKE t185f-fcode VALUE 'CRLT',
  fcode_cross_selling            LIKE t185f-fcode VALUE 'CRSL',
  fcode_cursor_selection         LIKE t185f-fcode VALUE '/CS',
  fcode_default_uebersicht       LIKE t185f-fcode VALUE 'DEF0',
  fcode_default_uebersicht_1     LIKE t185f-fcode VALUE 'DEF1',
  fcode_detail1_einteilung       LIKE t185f-fcode VALUE 'EID1',
  fcode_drucken                  LIKE t185f-fcode VALUE 'DRCK',
  fcode_eingang_lab              LIKE t185f-fcode VALUE 'PLEG',
  fcode_eingang_fab              LIKE t185f-fcode VALUE 'PFEG',
  fcode_einteilung               LIKE t185f-fcode VALUE 'PEIN',
  fcode_einteilung_detail        LIKE t185f-fcode VALUE 'SHLI',
  fcode_fplan_pos                LIKE t185f-fcode VALUE 'PFPL',
  fcode_fplan_kopf               LIKE t185f-fcode VALUE 'KFPL',
  fcode_freigabe_lab             LIKE t185f-fcode VALUE 'PLFG',
  fcode_freigabe_fab             LIKE t185f-fcode VALUE 'PfFG',
  fcode_rplan                    LIKE t185f-fcode VALUE 'KRPL',
  fcode_wkv                      LIKE t185f-fcode VALUE 'ZUKV',
  fcode_ende                     LIKE t185f-fcode VALUE 'BABA',
  fcode_ende2                    LIKE t185f-fcode VALUE 'BAB2',
  fcode_ende_neu                 LIKE t185f-fcode VALUE 'BEEN',
  fcode_ende_ohne_pruefung       LIKE t185f-fcode VALUE 'BAC1',
  fcode_erste_position           LIKE t185f-fcode VALUE 'POS1',
  fcode_erste_seite              LIKE t185f-fcode VALUE 'P-- ',
  fcode_fehlerprotokoll          LIKE t185f-fcode VALUE 'FEAZ',
  fcode_feinabruf                LIKE t185f-fcode VALUE 'PFEI',
  fcode_feinabruf_einteilung     LIKE t185f-fcode VALUE 'PFEI_SCHED',
  fcode_folgef_auf1              LIKE t185f-fcode VALUE 'FAU1',
  fcode_folgef_fak1              LIKE t185f-fcode VALUE 'FFA1',
  fcode_folgef_lief1             LIKE t185f-fcode VALUE 'FLI1',
  fcode_gleiche_seite            LIKE t185f-fcode VALUE 'ENT1',
  fcode_idoc_abruf               LIKE t185f-fcode VALUE 'IDOC',
  fcode_kaufmann_kopf            LIKE t185f-fcode VALUE 'KKAU',
  fcode_kaufmann_pos             LIKE t185f-fcode VALUE 'PKAU',
  fcode_komm_lief_menge_sich     LIKE t185f-fcode VALUE 'MOD2',
  fcode_komm_menge_sich          LIKE t185f-fcode VALUE 'MOD1',
  fcode_komm_menge_uebernehmen   LIKE t185f-fcode VALUE 'KOMU',
  fcode_kondition_anlegen        LIKE t185f-fcode VALUE 'KOAN',
  fcode_kondition_position       LIKE t185f-fcode VALUE 'PKON',
  fcode_kondition_position_subsc LIKE t185f-fcode VALUE 'PKO1',
  fcode_kondition_kopf           LIKE t185f-fcode VALUE 'KKON',
  fcode_kondition_kopf_subsc     LIKE t185f-fcode VALUE 'KKO1',
  fcode_konfiguration            LIKE t185f-fcode VALUE 'POCO',
  fcode_konfiguration1           LIKE t185f-fcode VALUE 'POKO',
  fcode_kontierung_detail        LIKE t185f-fcode VALUE 'COBL_PICK',
  fcode_kopieren                 LIKE t185f-fcode VALUE 'COPY',
  fcode_kopieren_anfrage         LIKE t185f-fcode VALUE 'RANF',
  fcode_kopieren_angebot         LIKE t185f-fcode VALUE 'RANG',
  fcode_kopieren_auftrag         LIKE t185f-fcode VALUE 'RAUF',
  fcode_kopieren_faktura         LIKE t185f-fcode VALUE 'RFAK',
  fcode_kopieren_kontrakt        LIKE t185f-fcode VALUE 'RKON',
  fcode_abruf_kontraktliste      LIKE t185f-fcode VALUE 'LIKO',
  fcode_abruf_partnerliste       LIKE t185f-fcode VALUE 'LIPA',
  fcode_kopieren_lieferplan      LIKE t185f-fcode VALUE 'RLIP',
  fcode_kpar                     LIKE t185f-fcode VALUE 'KPAR',
  fcode_kpar_sub                 LIKE t185f-fcode VALUE 'KPAR_SUB',
  fcode_kundenstammblatt         LIKE t185f-fcode VALUE 'KUST',
  fcode_kundenstammblatt_trans   LIKE t185f-fcode VALUE 'VC/2',
  fcode_kurzanzeige              LIKE t185f-fcode VALUE 'SUMM',
  fcode_langtext                 LIKE t185f-fcode VALUE 'TEDE',
  fcode_letzte_position          LIKE t185f-fcode VALUE 'POSL',
  fcode_letzte_seite             LIKE t185f-fcode VALUE 'P++ ',
  fcode_lieferabruf              LIKE t185f-fcode VALUE 'PLEI',
  fcode_lieferabruf_einteilung   LIKE t185f-fcode VALUE 'PLEI_SCHED',
  fcode_lieferart_vorgeben       LIKE t185f-fcode VALUE 'LIAR',
  fcode_liefergruppe_bilden      LIKE t185f-fcode VALUE 'LGKO',
  fcode_liefersit_pos            LIKE t185f-fcode VALUE 'ILIF',
  fcode_liste_anfrage            LIKE t185f-fcode VALUE 'VA15',
  fcode_liste_angebot            LIKE t185f-fcode VALUE 'VA25',
  fcode_liste_auftrag            LIKE t185f-fcode VALUE 'VA05',
  fcode_liste_kontrakt           LIKE t185f-fcode VALUE 'VA45',
  fcode_liste_lieferplan         LIKE t185f-fcode VALUE 'VA35',
  fcode_liste_posvorschlag       LIKE t185f-fcode VALUE 'VA55',
  fcode_markieren                LIKE t185f-fcode VALUE 'MARK',
  fcode_markieren_alle           LIKE t185f-fcode VALUE 'MKAL',
  fcode_mat_eingeben             LIKE t185f-fcode VALUE 'EING',
  fcode_naechstes_bild           LIKE t185f-fcode VALUE 'POAL',
  fcode_naechste_position        LIKE t185f-fcode VALUE 'POS+',
  fcode_naechste_seite           LIKE t185f-fcode VALUE 'P+  ',
  fcode_objektstatus_position    LIKE t185f-fcode VALUE 'PSTC',
  fcode_objektstatus_kopf        LIKE t185f-fcode VALUE 'KSTC',
  fcode_positionieren            LIKE t185f-fcode VALUE 'POSI',
  fcode_positionieren_popo       LIKE t185f-fcode VALUE 'POPO',
  fcode_positionsauswahl         LIKE t185f-fcode VALUE 'RUE1',
  fcode_ppar                     LIKE t185f-fcode VALUE 'PPAR',
  fcode_ppar_sub                 LIKE t185f-fcode VALUE 'PPAR_SUB',
  fcode_planabruf                LIKE t185f-fcode VALUE 'PPEI',
  fcode_planabruf_einteilung     LIKE t185f-fcode VALUE 'PPEI_SCHED',
  fcode_prkl                     LIKE t185f-fcode VALUE 'PRKL',
  fcode_protokoll                LIKE t185f-fcode VALUE 'PROT',
  fcode_pruefen                  LIKE t185f-fcode VALUE 'PRUF',
  fcode_schnellaend_fak_sperre   LIKE t185f-fcode VALUE 'SFSP',
  fcode_sichern                  LIKE t185f-fcode VALUE 'SICH',
  fcode_referenz                 LIKE t185f-fcode VALUE 'PREF',
  fcode_sort_aufloesen           LIKE t185f-fcode VALUE 'AUFL',
  fcode_splitt_menge             LIKE t185f-fcode VALUE 'SPLM',
  fcode_text_control_detail      LIKE t185f-fcode VALUE 'TCDE',
  fcode_text_control_loeschen    LIKE t185f-fcode VALUE 'TCLO',
  fcode_text_control_entsperren  LIKE t185f-fcode VALUE 'TCEN',
  fcode_tracking                 LIKE t185f-fcode VALUE 'TRAC',
  fcode_transportinfo_netz       LIKE t185f-fcode VALUE 'LOKN',
  fcode_transportinfo_weg        LIKE t185f-fcode VALUE 'LOKW',
  fcode_transportinfo_netz_liste LIKE t185f-fcode VALUE 'TILN',
  fcode_transportinfo_weg_liste  LIKE t185f-fcode VALUE 'TILW',
  fcode_uebernehmen              LIKE t185f-fcode VALUE 'UEBR',
  fcode_uebernehmen_menge_pv     LIKE t185f-fcode VALUE 'PVMV',
  fcode_uebernehmen_referenz     LIKE t185f-fcode VALUE 'RUEB',
  fcode_uebersicht_einzeilig     LIKE t185f-fcode VALUE 'UER1',
  fcode_uebersicht_mit_tec       LIKE t185f-fcode VALUE 'UERT',
  fcode_uebersicht_zweizeilig    LIKE t185f-fcode VALUE 'UER2',
  fcode_uebersicht_besteller     LIKE t185f-fcode VALUE 'UBST',
  fcode_uebersicht_konfiguration LIKE t185f-fcode VALUE 'UECO',
  fcode_umfang_bestellungen      LIKE t185f-fcode VALUE 'KBES',
  fcode_umfang_einteilung        LIKE t185f-fcode VALUE 'UMFA',
  fcode_umfang_hauptpositionen   LIKE t185f-fcode VALUE 'UHAU',
  fcode_umfang_markierte         LIKE t185f-fcode VALUE 'UMAR',
  fcode_umfang_positionen        LIKE t185f-fcode VALUE 'UALL',
  fcode_umfang_produktvorschlag  LIKE t185f-fcode VALUE 'PVSW',
  fcode_umfang_struktur          LIKE t185f-fcode VALUE 'POST',
  fcode_umfang_vbkd_position     LIKE t185f-fcode VALUE 'UKDP',
  fcode_unvollst_ausl_auft       LIKE t185f-fcode VALUE 'WUMA',
  fcode_verkaufshilfsmittel      LIKE t185f-fcode VALUE 'PADD',
  fcode_verpacken                LIKE t185f-fcode VALUE 'VERP',
  fcode_vorige_position          LIKE t185f-fcode VALUE 'POS-',
  fcode_vorige_seite             LIKE t185f-fcode VALUE 'P-  ',
  fcode_vorschlag_efz            LIKE t185f-fcode VALUE 'PEFZ',
  fcode_weiter                   LIKE t185f-fcode VALUE 'ENT1',
  fcode_xvbap_anlegen            LIKE t185f-fcode VALUE 'POAN',
  fcode_xvbap_loeschen           LIKE t185f-fcode VALUE 'POLO',
  fcode_xvbap_loeschen_einzeln   LIKE t185f-fcode VALUE 'POLE',
  fcode_xvbep_anlegen            LIKE t185f-fcode VALUE 'EIAN',
  fcode_xvbep_loeschen           LIKE t185f-fcode VALUE 'EILO',
  fcode_xvbep_loeschen_einzeln   LIKE t185f-fcode VALUE 'EILE',
  fcode_xvbsn_anlegen            LIKE t185f-fcode VALUE 'AEAN',
  fcode_xvbsn_loeschen           LIKE t185f-fcode VALUE 'AELO',
  fcode_xvbsn_loeschen_einzeln   LIKE t185f-fcode VALUE 'AELE',
  fcode_xlips_anlegen            LIKE t185f-fcode VALUE 'POAN',
  fcode_xlips_loeschen           LIKE t185f-fcode VALUE 'POLO',
  fcode_xlips_loeschen_einzeln   LIKE t185f-fcode VALUE 'POLE',
  fcode_zuordnen_kontrakt_pos    LIKE t185f-fcode VALUE 'APPL',
  fcode_zuordnen_kontrakt_kopf   LIKE t185f-fcode VALUE 'APHC',
  fcode_zurueck                  LIKE t185f-fcode VALUE 'BACK',
  fcode_zurueck1                 LIKE t185f-fcode VALUE 'BAC1',
  fcode_zurueck2                 LIKE t185f-fcode VALUE 'BAC2',
  fcode_zurueck3                 LIKE t185f-fcode VALUE 'BAC3',
  fcode_zurueck9                 LIKE t185f-fcode VALUE 'BAC9',
  fcode_zusaetze_lab             LIKE t185f-fcode VALUE 'PLZU',
  fcode_zusaetze_fab             LIKE t185f-fcode VALUE 'PFZU',
  fcode_reparatur                LIKE t185f-fcode VALUE 'PREP',
  fcode_gefahrgut                LIKE t185f-fcode VALUE 'KGGP',
  fcode_gefahrgut_zusatzdaten    LIKE t185f-fcode VALUE 'EID4',
  fcode_kopf_texte               LIKE t185f-fcode VALUE 'KTEX',
  fcode_kopf_texte_neu           LIKE t185f-fcode VALUE 'KTEX_SUB',
  fcode_position_texte_neu       LIKE t185f-fcode VALUE 'PTEX_SUB',


*eject

* Partnerverwendungen
  parvw_ag(2)   TYPE c VALUE 'AG',     "Auftraggeber
  parvw_re(2)   TYPE c VALUE 'RE',     "Rechnungsempfänger
  parvw_rg(2)   TYPE c VALUE 'RG',     "Regulierer
  parvw_we(2)   TYPE c VALUE 'WE',     "Warenempfänger
  parvw_sp(2)   TYPE c VALUE 'SP',     "Spediteur
  parvw_lf(2)   TYPE c VALUE 'LF',     "Lieferant
  parvw_rs(2)   TYPE c VALUE 'RS',     "Rechnungssteller
  parvw_tu(2)   TYPE c VALUE 'TU',     "Umschlagsdienstleiter
  parvw_tz(2)   TYPE c VALUE 'TZ',     "Zollagent
  parvw_pe(2)   TYPE c VALUE 'PE',     "Personal
  parvw_a(2)    TYPE c VALUE 'A',      "Arbeitsplatz
  parvw_o(2)    TYPE c VALUE 'O',      "Organisationseinheit
  parvw_s(2)    TYPE c VALUE 'S',      "Stelle
  parvw_us(2)   TYPE c VALUE 'US',     "User
  parvw_ed(2)   TYPE c VALUE 'ED',     "EDI-Workitemempfänger

* Partner-Call-Back
  part_call_back_prog_sales TYPE progname VALUE 'SAPMV45A',
  part_call_back_form_sales TYPE sdperform VALUE 'PARTNER_CALL_BACK',
  part_call_back_event_new TYPE sd_partner_call_back_event
  VALUE 'NEW',
  part_call_back_event_change TYPE sd_partner_call_back_event
  VALUE 'CHANGE',
  part_call_back_event_delete TYPE sd_partner_call_back_event
  VALUE 'DELETE',
  part_call_back_roletype_ag TYPE sd_partner_roletype VALUE '0001',
  part_call_back_roletype_we TYPE sd_partner_roletype VALUE '0002',
  part_call_back_roletype_rg TYPE sd_partner_roletype VALUE '0003',
  part_call_back_roletype_re TYPE sd_partner_roletype VALUE '0004',
  part_call_back_roletype_sb TYPE sd_partner_roletype VALUE '0013',

* GUI-Status für die Partner-Unvollständigkeitspflege auf dem Subscreen
  status_fs_kpar_sub LIKE t185v-status VALUE 'FS_KP_S',
  status_fs_ppar_sub LIKE t185v-status VALUE 'FS_PP_S',

* Dynprogruppe
  dyngr_anforderung(4) TYPE c VALUE 'ANFO', "Einstiegsbilder
  dyngr_uebersicht(4)  TYPE c VALUE 'LIST', "Erfassungsbilder
  dyngr_kopf(4)        TYPE c VALUE 'KOPF', "Kopfdetail
  dyngr_position(4)    TYPE c VALUE 'POSI', "Positionsdetail

* Dynpros
  dynp_partner_kopf(4) TYPE c VALUE '4701', "Partnersubscreen Kopf

*---------------------------------------------------------------------*
*     Aktivitäten für Berechtigungsprüfungen                          *
*---------------------------------------------------------------------*
  actvt_01(2)   TYPE c VALUE '01',     "Anlegen
  actvt_02(2)   TYPE c VALUE '02',     "Ändern
  actvt_03(2)   TYPE c VALUE '03',     "Anzeigen
  actvt_04(2)   TYPE c VALUE '04',     "Drucken
  actvt_06(2)   TYPE c VALUE '06',     "Löschen
  actvt_31(2)   TYPE c VALUE '31',     "Rückmelden
  actvt_49(2)   TYPE c VALUE '49',     "Anzeigen aus Archiv
  actvt_c1(2)   TYPE c VALUE 'C1',     "Pflegen Zahlungskarten
  actvt_c2(2)   TYPE c VALUE 'C2'.     "Anzeigen Zahlungskarten

* VBTYPen für Lieferung, Faktura usw.
INCLUDE rvvbtyp.

* Verbrauchskennzeichen
CONSTANTS:
      kzvbr_e TYPE c VALUE 'E',        " Einzelfertigung
      kzvbr_p TYPE c VALUE 'P',        " Einzelfertigung / Projekt

* Konfiguration
      config_tvap(2)           TYPE c VALUE 'CD',
      config_no_bom(1)         TYPE c VALUE 'C',
      config_bom(1)            TYPE c VALUE 'D',
      config_t459k(2)          TYPE c VALUE '+*',
      config_muss(1)           TYPE c VALUE '+',
      config_kann(1)           TYPE c VALUE '*',
*     config_vbps(10)          type c value 'VBPS      ',         " P30
      config_match_hinweis(2)  TYPE c VALUE '13',   " Hinweis auf MV
      config_match_ersetzen(2) TYPE c VALUE '24',   " KMAT ersetzen
      config_match_aendern(2)  TYPE c VALUE '34',   " auch im Ändern
      config_vbps              LIKE tclt-obtab VALUE 'VBPS',
      config_hinzufuegen       LIKE tvvfz-zeitp VALUE '101',
      config_aendern           LIKE tvvfz-zeitp VALUE '102',
      config_fix(2)            TYPE c VALUE 'BC',
      appl_sd(2)               TYPE c VALUE 'SD',
      appl_vertrieb(2)         TYPE c VALUE 'V ',
      aut_manuell              LIKE vcsd_update-ataut01 VALUE 'M',
      aut_soft(2)              VALUE 'U8',
      config_nodis             TYPE c VALUE '1',

* Montageabwicklung
      atodyn(2)  TYPE c VALUE '34',    " dynamische Montage (1:n)
      atosta(2)  TYPE c VALUE '12',    " statische Montage  (1:1)
      eigdyn     TYPE c VALUE 'D',     " Eigenfertigung FAUF/PAUF dyn.
      eigen      TYPE c VALUE 'E',     " Eigenfertigung FAUF/PAUF
      fremd      TYPE c VALUE 'F',     " Fremdfertigung BANF
      mopauf     TYPE c VALUE '1',     " Montage mit Planauftrag
      mofauf     TYPE c VALUE '2',     " Montage mit FAUF oder Netzplan
      mofaud     TYPE c VALUE '3',     " Montage mit FAUF dynamisch
      mopaud     TYPE c VALUE '4',     " Montage mit Planauftrag dynam.
      fauf(2)    TYPE c VALUE '10',    " Auftragstyp Fertigung
      netz(2)    TYPE c VALUE '20',    " Auftragstyp Netzplan
      service(2) TYPE c VALUE '30',    " Auftragstyp Service
      prozess(2) TYPE c VALUE '40',    " Auftragstyp Prozess

*Verkaufsbeleg - Kennzeichnung.
 vbklt_fakt_anf          LIKE tvak-vbklt VALUE 'A', "Fakturaanforderung
 vbklt_ausl_auft         LIKE tvak-vbklt VALUE 'B', "Auslieferungsauftr.
 vbklt_lp_ausl_auft      LIKE tvak-vbklt VALUE 'C', "Liefpl.m.Ausl.Auf.
 vbklt_rech_korr         LIKE tvak-vbklt VALUE 'D', "Rechnungskorrektur
 vbklt_ausl_auft_korr    LIKE tvak-vbklt VALUE 'E', "Auslief.auftr.Korr.
 vbklt_repa_auft         LIKE tvak-vbklt VALUE 'F', "Reparaturauftrag.
 vbklt_wart_auft         LIKE tvak-vbklt VALUE 'G', "Wartungsauftrag.
 vbklt_lp_edl            LIKE tvak-vbklt VALUE 'H', "Lieferpl. mit EDL
 vbklt_edl_entn          LIKE tvak-vbklt VALUE 'I', "EDL-Entnahme
 vbklt_edl_entn_korr     LIKE tvak-vbklt VALUE 'K', "EDL-Korrektur
 vbklt_reparatur(2)                      VALUE 'FG', "Reparatur

* Objekttyp für Status
      objekttyp_vbp(3) VALUE 'VBP',
      objekttyp_vbk(3) VALUE 'VBK',

* Vorgänge
      vrgng_sdef LIKE tj01-vrgng VALUE 'SDEF',
      vrgng_babs LIKE tj01-vrgng VALUE 'BABS',
      vrgng_anfr LIKE tj01-vrgng VALUE 'SDRQ',
      vrgng_ange LIKE tj01-vrgng VALUE 'SDQU',
      vrgng_auft LIKE tj01-vrgng VALUE 'SDOR',
      vrgng_conf LIKE tj01-vrgng VALUE 'CONF',   " Ändern Konfiguration
      vrgng_pglt LIKE tj01-vrgng VALUE 'PGLT',   " Ändern Parametergült
      vrgng_koao LIKE tj01-vrgng VALUE 'KOAO',
      vrgng_koaq LIKE tj01-vrgng VALUE 'KOAQ',
      vrgng_rku1 LIKE tj01-vrgng VALUE 'RKU1',
      vrgng_rku2 LIKE tj01-vrgng VALUE 'RKU2',
      vrgng_rku3 LIKE tj01-vrgng VALUE 'RKU3',
      vrgng_rkl  LIKE tj01-vrgng VALUE 'RKL ',
      vrgng_rks  LIKE tj01-vrgng VALUE 'RKS ',
      vrgng_lief LIKE tj01-vrgng VALUE 'SDDN',
      vrgng_sdgi LIKE tj01-vrgng VALUE 'SDGI',
      vrgng_fakt LIKE tj01-vrgng VALUE 'SDIN',
      vrgng_rfbu LIKE tj01-vrgng VALUE 'RFBU',
      vrgng_rfst LIKE tj01-vrgng VALUE 'RFST',     " Anzahlung
      vrgng_rmwa LIKE tj01-vrgng VALUE 'RMWA',
      vrgng_rmwe LIKE tj01-vrgng VALUE 'RMWE',
      vrgng_rmwl LIKE tj01-vrgng VALUE 'RMWL',
      vrgng_rmba LIKE tj01-vrgng VALUE 'RMBA',
      vrgng_rmbe LIKE tj01-vrgng VALUE 'RMBE',
      vrgng_psco LIKE tj01-vrgng VALUE 'PSCO',
      vrgng_ppma LIKE tj01-vrgng VALUE 'PPMA',     " Montageauftrag
      vrgng_kabk LIKE tj01-vrgng VALUE 'KABK',
      vrgng_sddn LIKE tj01-vrgng VALUE 'SDDN',
      vrgng_pms2 LIKE tj01-vrgng VALUE 'PMS2',
      vrgng_pms4 LIKE tj01-vrgng VALUE 'PMS4',
      vrgng_pmsb LIKE tj01-vrgng VALUE 'PMSB',
      vrgng_bdcu LIKE tj01-vrgng VALUE 'BDCU',     " Bedarfe unvollst.K
      vrgng_sdrr LIKE tj01-vrgng VALUE 'SDRR',     " RepRückmeldung
      vrgng_sd00 LIKE tj01-vrgng VALUE 'SD00',     " Faktura
      vrgng_rmrp LIKE tj01-vrgng VALUE 'RMRP',     " Rechnungsprüfung
      vrgng_kamv LIKE tj01-vrgng VALUE 'KAMV',     " Aufteilungsregel
      vrgng_coin LIKE tj01-vrgng VALUE 'COIN',

* Vorgänge im Bereich Serialnummernpflege
      vrgng_sdc1 LIKE tj01-vrgng VALUE 'SDC1',   " Auftrag -> Lieferung
      vrgng_sdc2 LIKE tj01-vrgng VALUE 'SDC2',   " Kontrakt -> Kontr.
      vrgng_sdc4 LIKE tj01-vrgng VALUE 'SDC4',   " Auftrag -> Auftrag
      vrgng_sdc5 LIKE tj01-vrgng VALUE 'SDC5',   " Auftrag -> Kontrakt
      vrgng_sdau LIKE tj01-vrgng VALUE 'SDAU',   " Pflege im Auftrag
      vrgng_sdw1 LIKE tj01-vrgng VALUE 'SDW1',   " Pflege im Kontrakt
      vrgng_sdd2 LIKE tj01-vrgng VALUE 'SDD2',   " Löschen kontr.-artig
      vrgng_sdd3 LIKE tj01-vrgng VALUE 'SDD3',   " Löschen auftr.-artig
      vrgng_sdr2 LIKE tj01-vrgng VALUE 'SDR2',   " Nummer kontr.-artig
      vrgng_sdr3 LIKE tj01-vrgng VALUE 'SDR3',   " Nummer auftr.-artig
      vrgng_sdwp LIKE tj01-vrgng VALUE 'SDWP',   " Verbucher kontr.artig
      vrgng_sdap LIKE tj01-vrgng VALUE 'SDAP',   " Verbucher auftr.artig

* processes for service contracts
      vrgng_anak LIKE tj01-vrgng VALUE 'ANAK',   " accept quotation

* STATUS
      status_enfa LIKE jest-stat VALUE 'I0097',
      status_abgs LIKE jest-stat VALUE 'I0046',       "Abgeschlossen
      status_zkal LIKE jest-stat VALUE 'I0164',       "zu kalkulieren
      status_kalk LIKE jest-stat VALUE 'I0165',       "kalkuliert
      status_neft LIKE jest-stat VALUE 'I0235',
      status_eftg LIKE jest-stat VALUE 'I0234',
      status_ao01 LIKE jest-stat VALUE 'I0337',       "Fehler ATP Mont
      status_ao02 LIKE jest-stat VALUE 'I0338',       "Prüfe ATP
      status_ao03 LIKE jest-stat VALUE 'I0339',       "Liefergruppen
      status_konu LIKE jest-stat VALUE 'I0038',       "unvollst. Konf.
      status_konx LIKE jest-stat VALUE 'I0444',       "Konf. fixiert
      status_ve   LIKE jest-stat VALUE 'I0218',       "VerwEntscheid
      status_mabg LIKE jest-stat VALUE 'I0072',       "Meldung abgeschl
      status_pakt LIKE jest-stat VALUE 'I0281',       "Prüflos aktiv
      status_irep LIKE jest-stat VALUE 'I0384',       "in Reparatur
      status_trep LIKE jest-stat VALUE 'I0385',       "Teil repariert
      status_repa LIKE jest-stat VALUE 'I0386',       "Repariert
      status_kzen LIKE jest-stat VALUE 'I0387',       "Kaufm. zu ents.
      status_kent LIKE jest-stat VALUE 'I0388',       "Kaufm. entschie.
      status_anan LIKE jest-stat VALUE 'I0395',       "Angebot angelegt
      status_anak LIKE jest-stat VALUE 'I0396',       "Angebot angenomm.
      status_tfak LIKE jest-stat VALUE 'I0398',       "Teilfakturiert
      status_lovm LIKE jest-stat VALUE 'I0076',       "Löschvormerkung

* Abrufarten
      abart_lab      LIKE vbep-abart VALUE '1',
      abart_fab      LIKE vbep-abart VALUE '2',
      abart_entnahme LIKE vbep-abart VALUE '3',
      abart_pab      LIKE vbep-abart VALUE '5',
      abart_mais(2)                  VALUE '67',
      abart_abruf(3)                 VALUE '125',

* Sonderregeln Automobilzulieferer
      automo_audi       LIKE t665a-automo VALUE '01',
      automo_bmw        LIKE t665a-automo VALUE '02',
      automo_ford       LIKE t665a-automo VALUE '03',
      automo_mb         LIKE t665a-automo VALUE '04',
      automo_opel       LIKE t665a-automo VALUE '05',
      automo_vw         LIKE t665a-automo VALUE '06',
      automo_saab       LIKE t665a-automo VALUE '07',
      automo_volvo      LIKE t665a-automo VALUE '08',
      automo_psa        LIKE t665a-automo VALUE '09',
      automo_renault    LIKE t665a-automo VALUE '10',
      automo_defined_z1 LIKE t665a-automo VALUE 'Z1',
      automo_defined_z2 LIKE t665a-automo VALUE 'Z2',
      automo_defined_z3 LIKE t665a-automo VALUE 'Z3',
      automo_defined_z4 LIKE t665a-automo VALUE 'Z4',
      automo_defined_z5 LIKE t665a-automo VALUE 'Z5',
      automo_defined_z6 LIKE t665a-automo VALUE 'Z6',
      automo_defined_z7 LIKE t665a-automo VALUE 'Z7',
      automo_defined_z8 LIKE t665a-automo VALUE 'Z8',
      automo_defined_z9 LIKE t665a-automo VALUE 'Z9',
      automo_defined_z0 LIKE t665a-automo VALUE 'Z0',

* Folgefunktionen
      fofun_beda_verfuegbar  LIKE tvfo-fofun VALUE '01',
      fofun_banf             LIKE tvfo-fofun VALUE '02',
      fofun_beda_beschaffung LIKE tvfo-fofun VALUE '03',
      fofun_vepvg            LIKE tvfo-fofun VALUE '04',
      fofun_fauf_pauf        LIKE tvfo-fofun VALUE '05',
      fofun_montagesperre    LIKE tvfo-fofun VALUE '06',
      fofun_kommi            LIKE tvfo-fofun VALUE '11',
      fofun_packen           LIKE tvfo-fofun VALUE '12',
      fofun_wa               LIKE tvfo-fofun VALUE '13',
      fofun_distrb           LIKE tvfo-fofun VALUE '14',

* Fakturarelevanz
*     ACHTUNG: FKREL im RMCSSU08 mitpflegen !!!!
      con_fkrel_liefer(9)             VALUE 'AHJKMQRTU',
      con_fkrel_liefermenge(10)       VALUE 'AGHJKMQRTU',
      con_fkrel_strecke(2)            VALUE 'BF',
      con_fkrel_re_menge(1)           VALUE 'F',
      con_fkrel_auftrag(5)            VALUE 'BCFGI',
      con_fkrel_fplan LIKE vbap-fkrel VALUE 'I',
      con_fkrel_proforma(3)           VALUE 'DLN',
      con_fkrel_max_proforma(5)       VALUE ' DJLN',
      con_fkrel_no(1)                 VALUE ' ',

* Fakturierformen
      faktf_pauschal LIKE vbkd-faktf VALUE '01',
      faktf_aufwand  LIKE vbkd-faktf VALUE '02',

* Fakturierungsregel aus Fakturierungsplantermin
      con_fareg_teilfak(5)      VALUE '12345',
      con_fareg_anzahlungen(2)  VALUE '45',
      con_fareg_teilfaktura(5)  VALUE '12345',
      con_fareg_schluss(1)      VALUE '3',
      con_fareg_periodisch(1)   VALUE '6',
      con_fareg_proz(2)         VALUE '14',
      con_fareg_wert(2)         VALUE '25',
      con_fareg_summieren(3)    VALUE '123',
* Verteilung von wertmäßigen Kopffakturierungsplanterminen
      con_differenz_ermitteln(1) VALUE 'A',
      con_differenz_verteilen(1) VALUE 'B',

* Workflow-Ereignisse
      c_event_alecreated LIKE swetypecou-event
                              VALUE 'ALECREATED',
      c_event_alechanged LIKE swetypecou-event
                              VALUE 'ALECHANGED',
      c_wf_param_item_changes LIKE swcont-element
                              VALUE 'LINEITEMCHANGES',
      c_wf_param_po_requisition LIKE swcont-element
                              VALUE 'PURCHASEREQUISITION',
      c_wf_param_po_req_item LIKE swcont-element
                              VALUE 'PURCHREQUISITIONITEM',
      c_obj_type_salesorder LIKE swotobjid-objtype
                              VALUE 'BUS2032',

* Aktivitäten bei CALL_FUNCTION
      activity_atp_simulate(4)    VALUE 'SIMU',
      activity_atp_area(4)        VALUE 'V03R',
      activity_purchase(4)        VALUE 'ME21',
      activity_invoice_receipt(4) VALUE 'MR01',
      activity_invoice_request(4) VALUE 'VA90',
      activity_service_request(4) VALUE 'IW51',
      activity_good_movement(4)   VALUE 'MB11',
*     Montage, Fertigungsaufträge, Planaufträge, Netzpläne:
      activity_assembly(4)        VALUE 'CO01',
      activity_assembly_co40(4)   VALUE 'CO40',        "n_667847
      activity_assembly_co08(4)   VALUE 'CO08',        "n_667847
      activity_assembly_co02(4)   VALUE 'CO02',        "n_810497
      activity_assembly_co07(4)   VALUE 'CO07',        "n_810497
      activity_assembly_co10(4)   VALUE 'CO10',        "n_810497
      activity_service_order(4)   VALUE 'IW31',
*     Kreditaktivitäten beginnen alle mit CR
      activity_credit(2)          VALUE 'CR',
*     Kreditaktivitäten im einzelnen
      activity_credit_recheck(4)  VALUE 'CRCH',
      activity_credit_authori(4)  VALUE 'CRAU', "komplette Prüfung
      activity_credit_authpre(4)  VALUE 'CRAP', "interne Prüfung
      activity_credit_release(4)  VALUE 'CRRL',
      activity_credit_reject(4)   VALUE 'CRRJ',
      activity_credit_forward(4)  VALUE 'CRFW',
*     Kreditneuaufbau RFDKLI20 ohne Änderungsbelege
      activity_credit_recreate(4) VALUE 'CRRE',
*     Unterkontrakte im PUSH-Modus aus Gruppenkontrakt aktualisieren
      activity_gruko_pushdata(4) VALUE 'GKPU',
*     Reparaturabwicklung
      act_rueck_inspection(4)          VALUE 'RINS',
      act_rueck_notification(4)        VALUE 'RNOT',
      act_rueck_service_order(4)       VALUE 'RSOR',
      act_rueck_start_of_repair(4)     VALUE 'RSST',
      act_dialog_pull_inspection(4)    VALUE 'DPIL',
      act_dialog_pull_service_order(4) VALUE 'DPSO',
      act_dialog(4)                    VALUE 'DIAL',
      act_nodialog(4)                  VALUE 'NDIA',
*     Abrechnung der Bonusabsprachen
      activity_rebate_set(4)           VALUE 'VBO1',


* Zu sichernde Kalkulationsart
      costing_method_product    VALUE '1',
      costing_method_unit       VALUE '2',
      costing_method_both       VALUE 'A',

* Kalkulation
      kalku_erlaubt(4)       VALUE ' AX',
      kalku_erforderlich     VALUE 'X',
      kalku_erlaubt_mit_sts  VALUE ' ',
      kalku_erlaubt_ohne_sts VALUE 'A',
      kalku_nicht_erlaubt    VALUE 'B',

* automatische Kalkulation
      aucost_kalkulieren(2)         VALUE 'AB',
      aucost_kalkulieren_ohne_vorm  VALUE 'A',
      aucost_kalkulieren_mit_vorm   VALUE 'B',

* Vertragsaktivitäten
      veda_vaktsch_vertrag(4)   VALUE '0001',
      veda_vaktsch_mail(4)      VALUE '0002',
      veda_vaktsch_angebot(4)   VALUE '0003',
      veda_vaktsch_kontakt(4)   VALUE '0004',


* Produktselektion (UEPVW)
      produktselektion          VALUE 'A',

* Naturalrabatt (UEPVW)
      natrab_drein              VALUE 'B',
      natrab_drauf              VALUE 'C',
      natrab_drein_no_upos      VALUE '3',

* Naturalrabatt (VBAPD-NATRAB)
      natrab_min_qty            VALUE 'D',
      naturalrabatt(2)          VALUE 'BC',
      natrab_possible(3)        VALUE 'BCD',

* Cross Selling (UEPVW)
      cross_selling             VALUE 'F',

* Anteilsmengeneinheiten / variable Mengeneinheiten
      kzfme_ws(2)               VALUE 'AB',        "Wirkstoff/Stahl

* Positionsarten
      posar_wert                VALUE 'A',
      posar_text                VALUE 'B',

* Erledigungsregel
      con_erlre_wertkontrakt(1) VALUE 'E',

* Abrufsteuerung zum Wertkontrakt
      rktio_sperren(3)          VALUE 'ABC',
      rktio_nicht_sperren(2)    VALUE ' D',

* Positionstypenverwendung
      con_vwpos_wertkontrakt(4) VALUE 'VCTR',
      con_vwpos_fakt_rel(4)     VALUE 'SEIN',
      con_vwpos_not_fakt_rel(4) VALUE 'SENI',

* Reparaturabwicklung
*     Zeitpunkte
      reparaturannahme        LIKE tvrmavt-bezei VALUE '101',
      reparaturstart          LIKE tvrmavt-bezei VALUE '102',
      rueckmeldung            LIKE tvrmavt-bezei VALUE '103',
*     Positionstypenklassifizierung
      vkgru_rma               LIKE vbap-vkgru VALUE '1',
      vkgru_rep_reparaturanfo LIKE vbap-vkgru VALUE 'I01',
      vkgru_dyn_posten        LIKE vbap-vkgru VALUE 'I02',
      vkgru_dyn_posten_ag     LIKE vbap-vkgru VALUE 'I03',
      vkgru_rep_retoure       LIKE vbap-vkgru VALUE '101',
      vkgru_rep_reparatur     LIKE vbap-vkgru VALUE '102',
      vkgru_rep_auslieferung  LIKE vbap-vkgru VALUE '103',
      vkgru_rep_leihgeraetbes LIKE vbap-vkgru VALUE '104',
      vkgru_rep_leihgeraetabh LIKE vbap-vkgru VALUE '105',
      vkgru_rep_austauschteil LIKE vbap-vkgru VALUE '106',
      vkgru_rep_verschrottung LIKE vbap-vkgru VALUE '107',
      vkgru_rep_gutschrift    LIKE vbap-vkgru VALUE '108',
      vkgru_rep_lastschrift   LIKE vbap-vkgru VALUE '109',
* Reparaturabwicklung Ende

* Sperrobjekt für Batch/Dialogverarbeitung
      con_enqueue_batch LIKE vbuk-vbeln VALUE '$%&sdbatch',

* Businessobjekttypen und Ereignisse
      bus_2015(7)               VALUE 'BUS2015',
      bus_2030(7)               VALUE 'BUS2030',
      bus_2031(7)               VALUE 'BUS2031',
      bus_2032(7)               VALUE 'BUS2032',
      bus_2033(7)               VALUE 'BUS2033',
      bus_2034(7)               VALUE 'BUS2034',
      bus_2035(7)               VALUE 'BUS2035',
      bus_2051(7)               VALUE 'BUS2051',
      bus_2090(7)               VALUE 'BUS2090',
      bus_2094(7)               VALUE 'BUS2094',
      bus_2095(7)               VALUE 'BUS2095',
      bus_2096(7)               VALUE 'BUS2096',
      bus_2102(7)               VALUE 'BUS2102',
      bus_2103(7)               VALUE 'BUS2103',
      bus_likp(7)               VALUE 'LIKP',
      bus_lips(7)               VALUE 'LIPS',
      bus_vbap(7)               VALUE 'VBAP',
      bus_vbrk(7)               VALUE 'VBRK',
      bus_vbrp(7)               VALUE 'VBRP',
      bus_vbrl(10)               VALUE 'BUS2037001',
      event_created(7)          VALUE 'CREATED',
      event_changed(7)          VALUE 'CHANGED',
      event_deleted(7)          VALUE 'DELETED',
* Anwendungsgetriggerte Statistik
* Prozesse
      process_salesorder(10)    VALUE 'A020022353',
* Funktionen
      function_configuration(10) VALUE 'A020020262',
      function_assembly(10)      VALUE 'A020022749',
* Container-ID im Memory für SD_SALES_DOCU_MAINTAIN
      container_sales_doc(12)    VALUE '2032_dialog',

*---------------------------------------------------------------------*
* Typ der Unterposition (UEPVW)
      uepvw_prodsel             VALUE 'A',
      uepvw_natrab_drein        VALUE 'B',
      uepvw_natrab_drauf        VALUE 'C',
      uepvw_repair              VALUE 'D',

* Technischer Vorgang (APO-ATP)
      tproc_dialog(2)           TYPE c VALUE 'AA',
      tproc_batch_input(2)      TYPE c VALUE 'BB',
      tproc_bapi(2)             TYPE c VALUE 'CC',
      tproc_edi(2)              TYPE c VALUE 'DD',
      tproc_neuterminierung(2)  TYPE c VALUE 'EE',

* Aktivität (APO-ATP)
      actyp_new                 VALUE 'A',
      actyp_change              VALUE 'B',
      actyp_copy                VALUE 'C',

* SourcingPosition (Ergebnis der regelbasierten ATP im APO)
      srcps_mainitem             VALUE 'A',
      srcps_subitem              VALUE 'B',

* Internal reason for rejection '00'
      abgru_00  LIKE vbap-abgru  VALUE '00',

*---------------------------------------------------------------------*
* Obergrenzen für Feldüberlauf, Floatingpointkonstanten               *
*---------------------------------------------------------------------*
      max_quan15    TYPE f VALUE '9.999999999990000E+14',
      max_quan10    TYPE p VALUE '9999999999999',
      quan_1        LIKE vbep-wmeng VALUE '1.000',    "Menge     1,000
      quan_0        LIKE vbap-kmpmg VALUE '0.000',    "Menge     0,000
      e-3           TYPE f VALUE '1E-03',
      e-4           TYPE f VALUE '1E-04',
      -e-4           TYPE f VALUE '-1E-04',
      e-6           TYPE f VALUE '1E-06',
      dec1_100      TYPE p DECIMALS 1 VALUE '100.0'.  "Prozent 100,0

*-----------------------------------------------------------------------
*            T185-Kopfgruppen
*-----------------------------------------------------------------------
CONSTANTS:  kopgr_mais(4) TYPE c VALUE 'AM  ',
            kopgr_ausl_auft_korr(4) TYPE c VALUE 'AMK ',
            kopgr_retoure(4) TYPE c VALUE 'RE  ',
            kopgr_lieferplan(4) TYPE c VALUE 'LL  '.

*-----------------------------------------------------------------------
*            EDI-Nachrichtentypen
*-----------------------------------------------------------------------
CONSTANTS: mestyp_delivery_schedule LIKE edidc-mestyp VALUE 'DELINS',
           mestyp_edl_delivery LIKE edidc-mestyp VALUE 'EDLNOT',
           mestyp_self_billing LIKE edidc-mestyp VALUE 'GSVERF',
           mestyp_order        LIKE edidc-mestyp VALUE 'ORDERS',
           mestyp_ordchg       LIKE edidc-mestyp VALUE 'ORDCHG',
           mestyp_delord       LIKE edidc-mestyp VALUE 'DELORD'.

DATA:      idoc_mestyp         LIKE edidc-mestyp.
*{   INSERT         PA8K039149                                        1
* IBU A&D/E&C, project bill of service IS-3.0a (4.5a), sd_srv
  CONSTANTS:
* function codes (full screen)
  FCODE_PBOS_POSITION      LIKE T185F-FCODE VALUE 'PBOS', " full screen
* Type of bill of service
  LEISTUNGSVERZEICHNIS(02) VALUE '01',     " Outline layout
  AUFMASS(2)               VALUE '02'.     " Service lists
*
* IBU A&D/E&C, project bill of service IS-4.6, payment
  CONSTANTS: CON_FKREL_DP     VALUE  'S',    " Clearing without bill.pl.
             CON_FAR_DPR      VALUE '1',     " Anzahlung
             CON_FAR_DMR      VALUE '2',     " Fakturierung
             CON_FAR_CLEARING VALUE '3'.    " Anzahlungsverrechnung
*
* IBU A&D/E&C, RE-SCM, Real Estate Sales, IS-4.6b
  CONSTANTS: FCODE_REUNITSALE LIKE T185F-FCODE VALUE 'KIMM'.
*
*
*}   INSERT

DATA: END OF COMMON PART.

* SD-protocoltypes
INCLUDE lv01apro.

* Text Objects
INCLUDE tdobjects.

* Message Handler
INCLUDE rvmessage.

*-----------------------------------------------------------------------
* Konstanten für Verbuchung der Bedarfe im R/3 und Verbuchung im APO
*-----------------------------------------------------------------------

INCLUDE bddirekt.


*-----------------------------------------------------------------------
*       Objekttypen
*-----------------------------------------------------------------------

CONSTANTS :   objtype_vbrk LIKE nast-objtype VALUE 'VBRK'.
CONSTANTS :   objtype_vbrl LIKE nast-objtype VALUE 'BUS2037001'.
*-----------------------------------------------------------------------
*       Branchenschalter
*-----------------------------------------------------------------------

CONSTANTS :   sysdef_standard LIKE tsysdef-sysdef VALUE '00'.
CONSTANTS :   sysdef_retail   LIKE tsysdef-sysdef VALUE '01'.
CONSTANTS :   sysdef_ignorieren LIKE tsysdef-sysdef VALUE '99'.
