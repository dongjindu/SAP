*
*        Allgemeine Konstanten zum RM-LVS
*

*........allgemeine Felder..............................................

constants:
         con_off               type c   value '0',
         con_on                type c   value '1',
         con_true              type c   value '1',
         con_false             type c   value '0',
         found                 type c   value '1',
         not_found             type c   value '0',
         con_split             type c   value '1',
         con_no_split          type c   value '0',
         con_hu_split          type c   value '2',
         con_SAVED             type c   value '1',
         con_GOUT(4)           type c   value 'GOUT',
         con_x                 type c   value 'X',
         con_uno               type p   value 1,
         mask1(1)              type c   value '*',
         mask2(2)              type c   value '**',
         mask3(3)              type c   value '***',
         con_exit_sfifo(3)     type c   value '+++',
*{   INSERT         PA8K016508                                        1
*        DI46C2 - Auto 3.0 - VHU
*        Auslagern konkreter HUs aus WM
         con_vhu_wm(3)         type c   value '###',
*}   INSERT
         dollar(1)             type c   value '$',
         tapos_high_value(4)   type c   value '9999'.

*........Mengenkonstanten...............................................

constants:
         con_menge_1           like ltap-vsolm  value '1'.

*........Transaktionstypen/Aktivitaetstypen/PF-Status...................

constants:
         con_anzeigen          type c   value 'A',
         con_hinzufuegen       type c   value 'H',
         con_korrigieren       type c   value 'K',
         con_loeschen          type c   value 'L',
         con_quittieren        type c   value 'D',
         con_veraendern        type c   value 'V',
         con_melden_r2         type c   value 'M'.

*........Transaktionstypen/Aktivitaetstypen/PF-Status...................

constants:
         con_posty_normal      like ltap-posty value ' ',
         con_posty_tb          like ltap-posty value '1',
         con_posty_quit        like ltap-posty value '2'.

*........Feldauswahl....................................................

constants:
         con_minus             type c   value '-',    " Inaktiv
         con_plus              type c   value '+',    " Muss
         con_punkt             type c   value '.',    " Kann
         con_stern             type c   value '*'.    " Nur Anzeige

*........Hell/Dunkelsteuerung...........................................

constants:
         con_dunkel            type c   value 'D',
         con_hell              type c   value 'H'.

*........Viewnamen......................................................

constants:
         con_mlvs(5)           type c   value 'MLVS ', " Material LGNUM
         con_mlvst(5)          type c   value 'MLVST', " Material LGTYP
         con_mbew(5)           type c   value 'MBEW ', " Bewertungsart
         con_mchar(5)          type c   value 'MCHAR', " Charge
* >>>>>>>>>> BEGIN OF INSERTION HP_217089 >>>>>>>>>>
         con_mmsta(5)          type c   value 'MMSTA'. " Materialstatus
* <<<<<<<<<< END OF INSERTION HP_217089 <<<<<<<<<<

*........Konstanten für Nummernkreisverwaltung..........................

constants:
         con_inventur           like inri-object  value 'LVS_IVNUM',
         con_quantzaehler       like inri-object  value 'LVS_LQNUM',
         con_transportauftrag   like inri-object  value 'LVS_TANUM',
         con_transportbedarf    like inri-object  value 'LVS_TBNUM',
         con_umbuchungsnummer   like inri-object  value 'LVS_UBNUM',
         con_referenznummer     like inri-object  value 'LVS_RFNUM',
         con_kommunikationssatz like inri-object  value 'RV_SNKOM',
         con_lagereinheit       like inri-object  value 'LVS_LENUM',
         con_lagerbeleg         like inri-object  value 'LVS_LBELN'.

*........Fehlermeldungen und Nachrichten................................

constants:
         con_fehler             type c  value 'F',
         con_warnung            type c  value 'W',
         con_msgty_a            like sy-msgty value 'A',
         con_msgty_e            like sy-msgty value 'E',
         con_msgty_w            like sy-msgty value 'W'.

*........Sperrmimik.....................................................

constants:
         con_enque_mat_lgnum    like t340d-enque value ' ',
         con_enque_mat_lgpla    like t340d-enque value 'A',
         con_enque_mat_shared   like t340d-enque value 'B',
         con_enqueue_shared     type c           value 'S',
         con_enqueue_strong(1)  type c           value '1',
         con_enqueue_weak(1)    type c           value '2',
         con_wildcard(1)        type c           value '?',
         con_trans_wildcard(2)  type c           value ' ?'.

*........Berechtigungen.................................................

constants:
         con_ber_db             type c  value 'D',    "Datenbankzugriff
         con_ber_mp             type c  value 'M',    "Modulpool
         con_ber_rp             type c  value 'R'.    "Report

constants:
         con_ber_sfunc_1        type c  value '1',    "Auto.TAs direkt..
         con_ber_sfunc_2        type c  value '2',    "Rep.ABAPs ausf.
         con_ber_sfunc_3        type c  value '3'.    "Quit. für Sub-Pos

*........Kommissionierkennzeichen für Shipping..........................

constants:
      con_komkz_a(1)        type c value 'A',   "ohne WM
      con_komkz_b(1)        type c value 'B',   "mit WM Festplatz
      con_komkz_c(1)        type c value 'C',   "mit WM Chaotisch
      con_komkz_d(1)        type c value 'D',   "Lean WM
      con_komkz_1(1)        type c value '1',   "Einschritt 2-St.Kommi
      con_komkz_2(1)        type c value '2'.   "Zweischritt 2-St.Kommi

*........Stammsatzarten bei Sonderbestandskennzeichen...................

constants:
         sobkz_lieferant        type c  value 'K',
         sobkz_einzelbestand    type c  value 'E',
*{   INSERT         PA8K016508                                        2
* DI A&D PCS Special stock indicator
         sobkz_customerstock    type c  value 'B',
* DI A&D SUB Special stock indicators
         sobkz_lb_kunde         type c  value 'C',
         sobkz_lb_einzelbestand type c  value 'F',
         sobkz_lb_mehrweg       type c  value 'I',
         sobkz_lb_lieferant     type c  value 'J',
         sobkz_lb_projekt       type c  value 'R',
*}   INSERT
         sobkz_kunde            type c  value 'W',
         sobkz_kunde_leih       type c  value 'V',
         sobkz_lohnbearb        type c  value 'O',
         sobkz_auftrag          type c  value 'A',
         sobkz_projekt          type c  value 'Q',
         sobkz_mehrweg          type c  value 'M',
         sobkz_verselemente     type c  value 'Y'.

*........Mengeneinheitenbezeichnungen und Umrechnungsrichtungen.........

constants:
         con_ausme                 type c value 'A',
         con_bstme                 type c value 'B',
         con_lvsme                 type c value 'L',
         con_meins                 type c value 'K',
         con_lvsme_streng          type c value 'M',
         con_meins_streng          type c value 'N',
         con_meins_altme           like sy-marky value ' ',
         con_altme_meins           like sy-marky value 'X'.

*........Leistungsdaten und Splitt......................................

constants:
         con_kzlei_space    like ltak-kzlei   value ' ',   "keine Leist
         con_kzlei_1        like ltak-kzlei   value '1',   "Solldaten
         con_kzlei_2        like ltak-kzlei   value '2',   "Istdaten
         con_kzlei_3        like ltak-kzlei   value '3',   "Soll u. Ist
         con_kzlei_4        like ltak-kzlei   value '4'.   "Leist.lohn

constants:
         con_kistz_space    like ltak-kistz   value ' ',   "keine Ist-D.
         con_kistz_1        like ltak-kistz   value '1',   "Nettodauer
         con_kistz_2        like ltak-kistz   value '2',   "Uhrzeit man.
         con_kistz_3        like ltak-kistz   value '3'.   "Uhrzeit auto

constants:
         con_hrsts_space    like ltak-hrsts   value ' ',   "nicht HR-rel
         con_hrsts_1        like ltak-hrsts   value '1',   "offen
         con_hrsts_2        like ltak-hrsts   value '2',   "übertragen
         con_hrsts_3        like ltak-hrsts   value '3'.   "fehlerhaft

constants:
         con_splitkommb     like t312b-splkb  value 'X'.   "Splitt KB
*........Funktionscodes für Sortierung..................................

constants:
         sort_menge(4)             type c value 'SORM',
         sort_lagertyp(4)          type c value 'SORT',
         sort_charge(4)            type c value 'SORC',
         sort_mhd(4)               type c value 'SORH',
         sort_datum(4)             type c value 'SORD',
         sort_asc(4)               type c value 'SRTA',
         sort_dsc(4)               type c value 'SRTD'.

*........Parameter......................................................

constants:
         parid_ivnum(3)            type c value 'IVN',
         parid_lenum(3)            type c value 'LEN',
         parid_lgnum(3)            type c value 'LGN',
         parid_lgtyp(3)            type c value 'LGT',
         parid_lgpla(3)            type c value 'LGP',
         parid_lqnum(3)            type c value 'LQN',
         parid_matnr(3)            type c value 'MAT',
         parid_msgvs(3)            type c value 'MSV',
         parid_nanum(3)            type c value 'NAN',
         parid_tanum(3)            type c value 'TAN',
         parid_tapos(3)            type c value 'TAP',
         parid_tbnum(3)            type c value 'TBN',
         parid_tbpos(3)            type c value 'TBP',
         parid_ubnum(3)            type c value 'UBN',
         parid_vbeln(3)            type c value 'VL ',
         parid_anlif(3)            type c value 'VLM',
         parid_l_call_lt0f(20)     type c value 'L_CALL_LT0F',
         parid_werks(3)            type c value 'WRK',
         parid_lgort(3)            type c value 'LAG',
         parid_betyp(3)            type c value 'LBT',
         parid_benum(3)            type c value 'LBE',
         parid_dirta(3)            type c value 'LDT',
         parid_dirt2(3)            type c value 'LDI',
         parid_autta(3)            type c value 'LAU',
         parid_charg(3)            type c value 'CHA',
         parid_meinh(3)            type c value 'LMB',
         parid_einlm(3)            type c value 'LGE',
         parid_posnr(3)            type c value 'LLP'.

*........Behälterstati..................................................

constants:
         con_wm_in_einl            type c value 'A',
         con_wm_eingelagert        type c value 'B',
         con_wm_in_ausl            type c value 'C',
         con_wm_in_uml             type c value 'D'.

*........Inventurstati..................................................

constants:
         con_angezaehlt            like linv-istat value 'A',
         con_ausgebucht_lvs        like linv-istat value 'L',
         con_ausgebucht_rm         like linv-istat value 'M',
         con_storniert             like linv-istat value 'S',
         con_gezaehlt              like linv-istat value 'Z',
         con_nichtgezaehlt         like linv-istat value 'N'.

*........Inventurarten..................................................

constants:
         kzinv_pe                  like t331-kzinv value 'PE',
         kzinv_pn                  like t331-kzinv value 'PN',
         kzinv_pz                  like t331-kzinv value 'PZ',
         kzinv_st                  like t331-kzinv value 'ST'.

*........Werte für T331-SPEIN und T331-SPAUS............................

constants:
         con_spein_platz           like t331-spein value '1',
         con_spein_quant           like t331-spein value '2',
         con_spaus_platz           like t331-spein value '1',
         con_spaus_quant           like t331-spein value '2'.

*........Werte für T331B-SUMME..........................................

constants:
         con_summe_nein            like t331b-summe value ' ',
         con_summe_o_charg         like t331b-summe value '1',
         con_summe_m_charg         like t331b-summe value '2'.

*........Werte für T331-KAPAP...........................................

constants:
         con_kapaz_keine     type c   value ' ', "Keine Prüfung
         con_kapaz_gewicht   type c   value '1', "Kapazitätspr. Gewicht
         con_kapaz_mengel    type c   value '2', "Kapazitätspr. Menge 1
         con_kapaz_menget    type c   value '3', "Kapazitätspr. Menge 2
         con_kapaz_mat       type c   value '4', "Kapazitätspr. Material
         con_kapaz_let       type c   value '5', "Kapazitätspr. LET
         con_kapaz_mat_let   type c   value '6'. "Kapazitätspr. Mat+LET

*........Werte für T331-TBZUG...........................................

constants:
         con_tbzug_nltyp     type c   value ' ', "Zugriff über Typindex
         con_tbzug_matnr     type c   value '1', "Zugriff über Mat.-Idx
         con_tbzug_join      type c   value '2'. "Zugriff über DB-Join

*........Druckstati ....................................................

constants:
         con_gedruckt              like linp-dstat value 'X',
         con_nichtgedruckt         like linp-dstat value ' '.

*........Druckzeitpunkte für Kommilisten-Druck..........................

constants:
         con_drsta_sofort          like t329s-drsta value '1'.

*........Memory-IDs.....................................................

constants:
         druck_id(8)         type c   value 'LVSDRUCK',
         dzmhd_id(8)         type c   value 'LVSDZMHD',
         tbmat_id(8)         type c   value 'LVSTBMAT',
         tbtyp_id(8)         type c   value 'LVSTBTYP',
         tbbel_id(8)         type c   value 'LVSTBBEL',
         lfbel_id(8)         type c   value 'LVSLFBEL',
         lubul_id(8)         type c   value 'LVSLUBUL',
         tabeh_id(8)         type c   value 'LVSTABEH',
         inven_id(8)         type c   value 'LVSINVEN',
         bzmat_id(8)         type c   value 'LVSBZMAT', "Best. zum Mat.
         bzlen_id(8)         type c   value 'LVSBZLEN', "Best. zur LE
         anzle_id(8)         type c   value 'LVSANZLE', "Anzeigen LE
         anzqu_id(8)         type c   value 'LVSANZQU', "Anzeigen Quant
         tbzpp_id(8)         type c   value 'LVSTBZPP',
         tapar_id(8)         type c   value 'LVSTAPAR',
         lepla_id(8)         type c   value 'LVSLEPLA', "Leere Plätze
         mapla_id(8)         type c   value 'LVSMAPLA', "Masch. Anl. Pl.
         bzpla_id(8)         type c   value 'LVSBZPLA', "Best. z. Platz
         eidat_id(8)         type c   value 'LVSEIDAT', "Erfass.Ist-Dat.
         eidar_id(8)         type c   value 'LVSEIDAR', "Erfass.Ist:Rück
         kldat_id(8)         type c   value 'LVSKLDAT', "Korr.Leist-Dat.
         monit_id(8)         type c   value 'LVSMONIT'. "Lagerleistand

*........Transportarten.................................................

constants:
         con_einlagerung           type c value 'E',
         con_auslagerung           type c value 'A',
         con_umlagerung            type c value 'U'.

*........Spezielle TA-Bezüge...........................................

constants:
         con_spezi_a  like ltak-spezi value 'A',
         con_spezi_b  like ltak-spezi value 'B'.

*........Transportbedarfskopfstatus.....................................

constants:
         con_endgeliefert          type c value 'E',
         con_teilgeliefert         type c value 'T',
         con_offen                 type c value ' ',
         con_start_statu           type c value 'S'.

*........Umbuchungsstatus...............................................

constants:
         con_umgebucht             type c value 'U',
         con_teilumgebucht         type c value 'T'.

*........Materialstatus.................................................

constants:
         con_material_warnung      type c value 'A',
         con_material_fehler       type c value 'B'.

*........Belegformat zur Materialbereitstellung für PP..................

constants:
          gc_beltp_tr               like lresb-beltp value 'TR',
          gc_beltp_dl               like lresb-beltp value 'DL'.

*........Bereitstellungskennzeichen WM <-> PP...........................

constants:
         con_berkz_nicht_relevant  type c value '0',
         con_berkz_kommi           type c value '1',
         con_berkz_kanban          type c value '2',
         con_berkz_abruf           type c value '3',
         con_berkz_manuell         type c value '4'.

*........Bedarfsart der Reservierungen..................................

constants:
         con_abh_reservierung(2)   type c value 'AR',
         con_sek_bedarf(2)         type c value 'SB'.

*........Funktionscodes, die überall benötigt werden....................

constants:
         fcode_anfang(4)        type c   value 'P-- ',      " PF21
         fcode_vor(4)           type c   value 'P+  ',      " PF22
         fcode_zurueck(4)       type c   value 'P-  ',      " PF23
         fcode_ende(4)          type c   value 'P++ ',      " PF24
         fcode_markieren(4)     type c   value 'MARK',      " PF09
         fcode_mark_loeschen(4) type c   value 'MRKL',      "
         fcode_mark_setzen(4)   type c   value 'MRKA'.      "

*........Gefahrstoffverwaltung..........................................

constants:
         con_gefal_inaktiv     like t331-gefal value ' ',
         con_gefal_nur_typ     like t331-gefal value '1',
         con_gefal_typ_bereich like t331-gefal value '2'.

*........Defaultwerte...................................................

constants:
         default_lgber         like lagp-lgber value '001'.

*........Werte für interne Tabelle Strenges FIFO........................

constants:
         con_inclusive       type c   value 'I',
         con_exclusive       type c   value 'E',
         con_equal(2)        type c   value 'EQ',
         con_smaller(2)      type c   value 'LT',
         con_smaller_eq(2)   type c   value 'LE',
         con_greater(2)      type c   value 'GT',
         con_greater_eq(2)   type c   value 'GE',
         con_between(2)      type c   value 'BT',
         con_pattern(2)      type c   value 'CP'.

*........Strategien.....................................................

constants:
         con_stein_b         type c   value 'B',    "Blocklager
         con_stein_c         type c   value 'C',    "Freilager
         con_stein_f         type c   value 'F',    "Fixplatz
         con_stein_i         type c   value 'I',    "Identisches Quant
         con_stein_k         type c   value 'K',    "Strategie K
         con_stein_l         type c   value 'L',    "Leerplatz
         con_stein_p         type c   value 'P',    "Palettentyp
         con_stein_p_maxqu   like t337a-maxqu value 99,
         con_stein_q         type c   value 'Q',    "Quantnummer
         con_stein_r         type c   value 'R',    "Referenznummer
         con_stein_blank     type c   value ' ',    "Keine Strategie
         con_staus_a         type c   value 'A',    "Anbruch
         con_staus_f         type c   value 'F',    "Fifo
         con_staus_h         type c   value 'H',            "MHD
         con_staus_l         type c   value 'L',    "Lifo
         con_staus_m         type c   value 'M',    "Manipulationsmenge
         con_staus_p         type c   value 'P',    "Festplatz
         con_staus_r         type c   value 'R',    "Referenznummer
         con_staus_blank     type c   value ' '.    "Keine Strategie

*........Abrunden im Blocklager.......................................

constants:
         con_abrun_x         type c   value 'X',
         con_abrun_y         type c   value 'Y'.

*........Nullkontrolle..................................................

constants:
         con_nulko_nein      type c   value '0',
         con_nulko_ja_s      type c   value '1',
         con_nulko_ja_u      type c   value '2',
         con_nulko_leer_s    type c   value '3',
         con_nulko_leer_u    type c   value '4',
         con_nulko_belegt_s  type c   value '5',
         con_nulko_belegt_u  type c   value '6'.

*........Eingabemöglichkeiten beim Quittieren ........................

constants:
         con_nplei_1         type c   value '1'.

*........Status von Lagereinheiten......................................

constants:
         con_lein_frei       like lein-statu value ' ',
         con_lein_umlagern   like lein-statu value '1',
         con_lein_umlagern_a like lein-statu value 'A',   " nur intern
         con_lein_zulagern   like lein-statu value '2',
         con_lein_auslagern  like lein-statu value '3',
         con_lein_bilden     like lein-statu value '4',
         con_lein_bilden_a   like lein-statu value 'B',   " nur intern
         con_lein_umbuchen   like lein-statu value '5',
         con_lein_umbuchen_a like lein-statu value 'C',   " nur intern
         con_lein_loeschen   like lein-statu value '6'.

*........Antworten auf CHECK-Dynpros....................................

constants:
         antwort_abbrechen    type c  value 'A',
         antwort_nein         type c  value 'N',
         antwort_ja           type c  value 'J'.

*........Sammelgang.....................................................

constants:
         con_refnrbelegtyp_b  like t311a-rbtyp   value 'B',
         con_refnrbelegtyp_l  like t311a-rbtyp   value 'L'.

*........Batch Input/Call Transaction...................................

constants:
         anz_alles(1)         type c   value 'A',
         anz_fehler(1)        type c   value 'E',
         anz_nichts(1)        type c   value 'N',
         upd_asynchron(1)     type c   value 'A',
         upd_synchron(1)      type c   value 'S'.

*........Melden an R2...................................................

constants:
         con_melden_1         like t321d-kzvmd   value '1',
         con_melden_2         like t321d-kzvmd   value '2',
         con_melden_von       like ltak-kr2so    value 'V',
         con_melden_nach      like ltak-kr2so    value 'N',
         con_melden_von_diff  like ltak-kr2so    value '1',

         vorgangsschl_rmmat   like t321d-vorgk   value '001',
         vorgangsschl_lief    like t321d-vorgk   value '002',

         con_first            like apqi-qstate   value 'F',
         con_last             like apqi-qstate   value 'L',
         con_single           like apqi-qstate   value 'S'.

*........Werte für T340D-KAPPC..........................................

constants:
         con_kappc_neu        like t340d-kappc   value '1',
         con_kappc_alt        like t340d-kappc   value 'X'.

*........Werte für T340D-KOSPL..........................................

constants:
         con_kospl_kober      like t340d-kospl   value 'A'.

*........Werte für T321-WENUM...........................................

constants:
         con_wenum_neu        like t321-wenum    value '1',
         con_wenum_alt        like t321-wenum    value 'X'.

*........Werte für T321-TBFKZ...........................................

constants:
         con_tbfkz_create     like t321-tbfkz    value 'X',
         con_tbfkz_cancel     like t321-tbfkz    value '1',
         con_kein_tb_storno   like t321-tbfkz    value 'Q'.

*........Aufrufstellen in der IM-Verarbeitung...........................

constants:
         con_imart_leit       like rl03a-imart   value '1',
         con_imart_gegen      like rl03a-imart   value '2',
         con_imart_umbu1      like rl03a-imart   value '3',
         con_imart_umbu2      like rl03a-imart   value '4'.

*........Nachrichtentypen ( Message-Typen )............................

constants:
         mestyp_to           like edidc-mestyp value 'WMTORD',
         mestyp_ph           like edidc-mestyp value 'WMPIHU',
         mestyp_tc           like edidc-mestyp value 'WMTOCO',
         mestyp_rr           like edidc-mestyp value 'WMRREF',
         mestyp_ca           like edidc-mestyp value 'WMCATO',
         mestyp_bi           like edidc-mestyp value 'WMBBIN',
         mestyp_iv           like edidc-mestyp value 'WMINVE',
         mestyp_tr           like edidc-mestyp value 'WMTREQ',
         mestyp_su           like edidc-mestyp value 'WMSUMO'.

*........Kennzeichen für KZSUB: Übergabe an SUB-System.................

constants:
         con_kzsub_uebergeben like ltap-kzsub  value 'X',
         con_kzsub_stornoanf  like ltap-kzsub  value '1'.

*........LVS-Identifikation für Kommunikationssätze.....................

constants:
         lvs_komid(2)         type c             value 'WM'.

*........Satzarten der Kommunikationssätze..............................

constants:
         con_lk01             like ldk07-sakom   value 'WM01',
         con_lk02             like ldk07-sakom   value 'WM02',
         con_lk03             like ldk07-sakom   value 'WM03',
         con_lk04             like ldk07-sakom   value 'WM04',
         con_lk05             like ldk07-sakom   value 'WM05',
         con_lk06             like ldk07-sakom   value 'WM06',
         con_lk07             like ldk07-sakom   value 'WM07',
         con_lk08             like ldk07-sakom   value 'WM08',
         con_lk09             like ldk07-sakom   value 'WM09'.

*........Name der logischen POOL-Tabellen der Komm.Sätze................

constants:
         con_ldk00(5)         type c             value 'LDK00',
         con_ldk01(5)         type c             value 'LDK01',
         con_ldk02(5)         type c             value 'LDK02',
         con_ldk03(5)         type c             value 'LDK03',
         con_ldk04(5)         type c             value 'LDK04',
         con_ldk05(5)         type c             value 'LDK05',
         con_ldk06(5)         type c             value 'LDK06',
         con_ldk07(5)         type c             value 'LDK07',
         con_ldk08(5)         type c             value 'LDK08',
         con_ldk09(5)         type c             value 'LDK09'.

*.........Konstanten für den Aufbau von Ranges-Tabellen.................

constants:
         con_ran_sig_i(1)                        value 'I',
         con_ran_opt_eq(2)                       value 'EQ',
         con_ran_opt_bt(2)                       value 'BT',
         con_ran_opt_le(2)                       value 'LE'.

*........Konstanten für Datum und Uhrzeit...............................

constants:
         con_default_uzeit    like sy-uzeit       value '235959'.

*........Konstanten für Chargenprüfung..................................

constants:
         con_chpru_0         type c   value '0', "Minimalprüfung
         con_chpru_1         type c   value '1', "Prüfung Chargeneingabe
         con_chpru_2         type c   value '2'. "Prüfung Chargensatz

constants:
         con_kzdzv_0 like tcuch-kzdzv value '0', "Zustandsverw. n. aktiv
         con_kzdzv_1 like tcuch-kzdzv value '1'. "Zustandsverw. aktiv

constants:
         con_kzdch_0 like tcuch-kzdch value '0', "Werksebene (MCHA)
         con_kzdch_1 like tcuch-kzdch value '1', "Materialebene (MCH1)
         con_kzdch_2 like tcuch-kzdch value '2'. "Mandantenebene (MCH1)

constants:
         con_kzfme_wirk  like ltbp-kzfme value 'A',
         con_kzfme_prod  like ltbp-kzfme value 'B'.

constants:
         con_kzwsm_wirk  like mlvs-kzwsm value 'A',
         con_kzwsm_prod  like mlvs-kzwsm value 'B'.

*........Konstanten für Messages........................................

constants:
         con_msgid_sh        like sy-msgid value 'SH',
         con_msgid_l2        like sy-msgid value 'L2',
         con_msgid_l3        like sy-msgid value 'L3',
         con_msgid_l9        like sy-msgid value 'L9',
         con_msgno_no_help   like sy-msgno value '007'.

*........Konstanten für Prüflosbearbeitung..............................

constants:
         con_stikz_we_zone   like lqpl1-lvs_stikz value '2',
         con_stikz_platz     like lqpl1-lvs_stikz value '1',
         con_stikz_im_lager  like lqpl1-lvs_stikz value '3'.

*........Werte für T325-KZSTI..Stichprobenbehandlung...................

constants:
         con_kzsti_prpla         like t325-kzsti      value '1',
         con_kzsti_we            like t325-kzsti      value '2',
         con_kzsti_einlag        like t325-kzsti      value '3',
         con_kzsti_igno          like t325-kzsti      value '4'.

*........Bezugsobjekte für Behälter....................................

constants:
         con_vpobj_lieferung     like vekp-vpobj      value '01',
         con_vpobj_verkaufsbeleg like vekp-vpobj      value '02',
         con_vpobj_lieferavis    like vekp-vpobj      value '03',
         con_vpobj_transport     like vekp-vpobj      value '04',
         con_vpobj_nichts        like vekp-vpobj      value '05',
         con_vpobj_frei          like vekp-vpobj      value '06',
         con_vpobj_pp_auftrag    like vekp-vpobj      value '08',
         con_vpobj_hulgort_frei  like vekp-vpobj      value '12'.




*........Werte für T320-OBEST..........................................

constants:
         con_obest_miniwm        like t320-obest      value '1',
         con_obest_frm           like t320-obest      value '2',
         con_obest_mini_out      like t320-obest      value '3',
         con_obest_mini_in       like t320-obest      value '4'.


*........Kommimenge und Einlagermenge übernehmen.......................

constants:
         con_komim_kommi         like rl03t-komim     value '1',
         con_komim_wa            like rl03t-komim     value '2',
         con_komim_nix           like rl03t-komim     value '3',
         con_komim_wa_only       like rl03t-komim     value '4',
         con_einlm_einla         like rl03t-komim     value '1',
         con_einlm_we            like rl03t-komim     value '2',
         con_einlm_nix           like rl03t-komim     value '3',
         con_einlm_we_only       like rl03t-komim     value '4'.

*........Konstanten für Transportaarten innerhalb der 2-st. Kommi.......
constants: con_taart_direkt     like ltak-l2ska value ' ', "Direkte Komm
           con_taart_entnahme   like ltak-l2ska value '1', "Entnahme
           con_taart_aufteilung like ltak-l2ska value '2'. "Aufteilung
*........Konstanten für getrennte TA-Quittierung der 2-st. Kommi.......
constants: con_2st_all          type c value ' ',         "ohne Einschr
           con_2st_direkt       type c value '1',         "Direkte Komm
           con_2st_entnahme     type c value '2',         "Entnahme
           con_2st_aufteilung   type c value '3'.         "Aufteilung

*........Konstanten für Relevanz der 2-Stufigkeit...............i.......
constants: con_refnr_no_rel    like t311-l2skr value ' ', "nicht relev.
           con_refnr_rel_1s   like t311-l2skr value '1', "relevant 1-Sch
           con_refnr_rel_2s   like t311-l2skr value '2', "relevant 2-Sch
           con_mat_no_rel     like t311-l2skr value ' ', "nicht relevant
           con_mat_rel_1s     like t311-l2skr value '1', "relevant 1-Sch
           con_mat_rel_2s     like t311-l2skr value '2', "relevant 2-Sch
           con_pos_no_check   like ltbp-l2skr value ' ', "kein Prüfung
           con_pos_no_rel     like ltbp-l2skr value ' ', "nicht relevant
           con_pos_rel_2s     like t311-l2skr value '2', "relevant 2-Sch
           con_pos_rel_1s     like t311-l2skr value '1'. "relevant 1-Sch

*........Kennzeichen für Bestimmung der 2-Stufigkeit beim Bilden REFNR..
constants: con_2st_pruefung_keine  like t326-l2skp value ' ',
           con_2st_pruefung_hell   like t326-l2skp value '1',
           con_2st_pruefung_dunkel like t326-l2skp value '2'.

*........Konstanten für Bestands/Chargenfindung.........................

constants:
         con_best             type c          value '1', "Bestandsfdg.
         con_char             type c          value '2', "Chargenfdg.
         con_best_char        type c          value '3'. "beides

*........Verpacken......................................................

constants: con_vpobj_05     like vekp-vpobj value '05', "Objekt TA
           con_verp_1       like sy-tabix   value '1',  "Verp.vorschlag
           con_verp_2       like sy-tabix   value '2',  "Verp. gemeldet
           con_schleifen_1  like sy-tabix   value '1'.  "eine Schleife

*........Verarbeitung bei Einlager-TA zu Anlieferung....................

data:    con_einta_system  like rl03t-einta value ' ', " System
         con_einta_lfposi  like rl03t-einta value '1', " TA für LF-Posi
         con_einta_hus     like rl03t-einta value '2'. " TA für HUs

*........Abwicklung bei Transportauftrag zur Lieferung..................

data: con_talif_lfposi  like rl03t-talif value 'L', "über Lieferposi.
      con_talif_hus     like rl03t-talif value 'H'. "über Hand.Units

*........Vertriebsbelegtypen............................................

constants: con_vbtyp_anlief   like likp-vbtyp value '7',  "Anlieferung
           con_vbtyp_retoure  like likp-vbtyp value 'T',  "Anlief/Retour
           con_vbtyp_auslief  like likp-vbtyp value 'J',  "Auslieferung
           con_vbtyp_cancel   like likp-vbtyp value 'h',  "Cancel GM
           con_vbtyp_pp       like likp-vbtyp value 'j',  "PP-Aufruf
           con_vbtyp_dummy    like likp-vbtyp value ' '.  "Dummy

*........Art der Lieferung..............................................

data: con_aufer_umbuchung like vblkk-aufer value 'H'. "Umbuch.lieferung

*........Antworten auf Popup TA-Abwicklung..............................

data:    antwort_lfposi       type c  value '1',
         antwort_hus          type c  value '2'.

*........Vorgängerbelegtypen in Lieferung...............................

constants:
      con_rfvgtyp_bestellung like lips-rfvgtyp value 'V'. "Bestellung

*........Vorgängerbelegtypen in LSEG...................................

constants: con_lvbtp_anlieferung   like lseg-lvbtp value 'A',
           con_lvbtp_auslieferung  like lseg-lvbtp value 'B',
           con_lvbtp_umbuchung     like lseg-lvbtp value 'C',
           con_lvbtp_umbuchung_2   like lseg-lvbtp value 'D',
           con_lvbtp_inventur      like lseg-lvbtp value 'E',
           con_lvbtp_ta            like lseg-lvbtp value 'F',
           con_lvbtp_ta_2          like lseg-lvbtp value 'G',
           con_lvbtp_aufnahme      like lseg-lvbtp value 'H',
           con_lvbtp_huinv_plus    like lseg-lvbtp value 'I',
           con_lvbtp_huinv_minus   like lseg-lvbtp value 'J'.

*##ngr400-anf
*........Konstanten für Positionstypen..................................

constants:
           con_leergut        like dm07m-uptyp     value '3'.
*##ngr400-end

*........Zentrale Sperrobjekte..........................................

constants:

           con_umbu_lgort          like llvsx-lvsob value 'UMBU-LGORT',
           con_lvsob_1             like llvsx-lvsob value 'NACH_FIXPL',
           con_rfmon               like llvsx-lvsob value 'RF_MONITOR'.
*........IM-Buchung aus WM getriggert...................................

constants:
      con_aufruf_lo(2)      type c   value 'LO', "Umlagerung Lagerort
      con_aufruf_vu(2)      type c   value 'VU', "Umbuchung von unten
      con_aufruf_ad(2)      type c   value 'DF', "Ausbuchen Differenzen
      con_aufruf_pa(2)      type c   value 'PA', "Ein-,Auspacken HU dez.
      con_aufruf_ba(2)      type c   value 'BA', "Best.aufnahme HU dez.
      con_aufruf_hm(2)      type c   value 'HM', "Aufruf aus HUMO dez.
      con_r3_melden_sofort  like ltap-imrel value '1',
      con_r3_melden_batch   like ltap-imrel value '2',
      con_im_tcode_umbu     like sy-tcode value 'MB11',
      con_im_tcode_diff     like sy-tcode value 'MB11',
      con_anzahl_4999       like mseg-zeile  value '4999'.

*........Kennzeichen fürs Zurücksetzen der Referenz im TB...............

constants:
      con_refrs_kanban      like ltbc-refrs value '1'.

*........Benötigte Objekte für WMS-Komponente (BAPIs)...................

constants:
      objtyp_customerdel   like bdi_blink-bobjtype value 'LIKP',
      objtyp_shippingnot   like bdi_blink-bobjtype value 'BUS2015',
      objtyp_goodsmvt      like bdi_blink-bobjtype value 'BUS2017'.

*........Benötigte Methoden (BAPIs).....................................

constants:
      method_customerdel_replica   like bdi_blink-method value
                                   'SAVEREPLICA',
      method_shippingnot_replica   like bdi_blink-method value
                                   'SAVEREPLICA',
      method_goodsmvt_create       like bdi_blink-method value
                                   'CREATEFROMDATA',
      method_customerdel_confirm   like bdi_blink-method value
                                   'CONFIRMDECENTRAL',
      method_shippingnot_confirm   like bdi_blink-method value
                                   'CONFIRMDECENTRAL'.

*........Benötigte Filterobjekte (BAPIs)................................

constants:
      filter_whse_no         like bdi_fobj-objtype value  "Lagernummer
                             'WHSE_NO',
      filter_plant           like bdi_fobj-objtype value  "Werk
                             'PLANT',
      filter_stge_loc        like bdi_fobj-objtype value  "Lagerort
                             'STGE_LOC'.
*........Konstanten für RFC-Typen.......................................

constants:
      con_rfctype_r3         like rfcdes-rfctype value '3'.
*........constants for RFC-Queues.......................................
DATA: CON_RFC_CALL_L03X      LIKE TRFCQOUT-QNAME VALUE 'WM_L03X',
      CON_RFC_CALL_R2R3      LIKE TRFCQOUT-QNAME VALUE 'R2R3_QUEUE',
      CON_RFC_CALL_WMPP      LIKE TRFCQOUT-QNAME VALUE 'MW_KANBAN'.
*........Konstanten für HU-Abwicklung...................................

data: con_hupos_anlieferung   like lips-hupos value  'A'.

data: con_fhuta_normal        like ltap-fhuta  value ' ',
      con_fhuta_pick_ta       like ltap-fhuta  value '1'.

data: con_pikta_huta_verboten like t331-pikta  value ' ',
      con_pikta_huta_erlaubt  like t331-pikta  value '1',
      con_pikta_bwlvs_def     like t331-pikta  value '2',
      con_pikta_inhalt        like t331-pikta  value '3'.

data: con_hupik_hu_in_pickhu  like ltap-hupik  value ' ',
      con_hupik_hu_pick       like ltap-hupik  value 'X',
      con_hupik_hu_inhalt     like ltap-hupik  value 'A'.


data: con_pikhu_auto_create   like t319-authu  value '1',
      con_pikhu_pack_determ   like t319-authu  value '2'.

data: con_velin_mat           like vepo-velin  value '1', "Material
      con_velin_pack          like vepo-velin  value '2', "Verpackung
      con_velin_sub           like vepo-velin  value '3'. "Unter-HU

*........Hu-Status......................................................

*DATA: CON_HUSTA_AUF_LGORT     LIKE VEKP-STATUS VALUE '0030',
data: con_auf_hu_lgort(3)                      value 'CDX',
      con_husta_auf_nicht_hu_lgort
                              like vekp-status value '0020',
      con_husta_inv_hu        like vekp-status value '0031',
      con_lein_westa_wefeh    like lein-westa  value '1'.  "We-Buchung

*........Warenbewegungsstatus...........................................

data: con_wbstk_nicht_relevant like vbuk-wbstk  value ' ',
      con_wbstk_relevant       like vbuk-wbstk  value 'A',
      con_wbstk_teil           like vbuk-wbstk  value 'B',
      con_wbstk_erledigt       like vbuk-wbstk  value 'C'.

*........HU Rückmeldestatus.............................................

data: con_hucon_lieferung   like ltak-hucon  value ' ',  "an Lieferung
      con_hucon_fertauftrag like ltak-hucon  value '1'.  "an PP-Auftrag

*........Hu: WA-Buchung über LF bzw. PP.................................

data: con_hucon_delivery    like ltak-hucon  value ' ',
      con_hucon_pp          like ltak-hucon  value '1'.

*........Konstanten für Getrennt Quittierung...........................

constants:
      con_quknz_normal        like rl03t-quknz value ' ',
      con_quknz_entnahme      like rl03t-quknz value '1',
      con_quknz_transport     like rl03t-quknz value '2'.

*........Konstanten für Getrennt (Zweischritt) Quittierung f. Sub-System

constants:
      con_qusub_both_one      like rl03t-quknz value ' ',
      con_qusub_removal       like rl03t-quknz value '1',
      con_qusub_transport     like rl03t-quknz value '2',
      con_qusub_both_more     like rl03t-quknz value ' '.

*........Konstanten für Berechtigungsprüfung............................

constants:
      con_ls22_anzeigen(4)   type c value 'LS22',
      con_ls23_anzeigen(4)   type c value 'LS23',
      con_ls33_anzeigen(4)   type c value 'LS33'.

*........Memory-IDs.....................................................
constants: memory_mb51_flag(60)     type c   value 'MB51_FLAG',
           memory_dd07(60)          type c   value 'DD07'.

*........Konstanten für Arten der IM-Buchungen........................

constants:
      con_kipos_receipt       type c value 'E',
      con_kipos_issue         type c value 'A',
      con_kipos_trapo         type c value 'U'.

*........Konstanten für TA-Positionssplitt beim Quittieren..............
constants:
      con_spitm_no_split      type c value space,
      con_spitm_split_normal  type c value 'A',
      con_spitm_split_su_bulk type c value 'B'.

*........Konstanten Pick&Pack beim Quittieren...........................
constants:
      con_pckpf_no_pack       type c value 'B'.

*........Abwicklung Verfallsdatum bei TA zur Anlieferung...............

constants:
      con_vfdat_relevant(1)    type c value 'X', "Verfallsdatum relev.
      con_vfdat_in_delivery(1) type c value 'Y'. "Verfallsdatum in Lief.

*-----------------------------------------------------------------------
