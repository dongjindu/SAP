************************************************************************
*                                                                      *
* Include RFFORI00, used in the payment print reports RFFOxxxz         *
*                                                                      *
* Tables / fields / field-groups / at selection-screen                 *
*                                                                      *
************************************************************************


*----------------------------------------------------------------------*
* Tables                                                               *
*----------------------------------------------------------------------*
tables:
  addr1_sel,                           "interface to ADDR_GET
  addr1_val,                           "interface to ADDR_GET
  adrs,                                "address into printform
  arc_params,                          "archive parameters
  autha,                               "HR authority check
  bhdgd,                               "for batch-heading
  bkpf,                                "for payment document valdation
  bnka,                                "bank master record
  fimsg,                               "FI messages
  finaa,                               "message type (print or fax)
  fsabe,                               "Accounting clerk
 *fsabe,                               "Accounting clerk (local)
  itcfx,                               "addresses for fax
  itcpo,                               "spool print parameters SAPscript
  itcpp,                               "result of print SAPscript
  hrxblnr,                             "structure for special HR data
  kna1,                                "customers
  lfa1,                                "vendors
  payr,                                "check register
 *payr,                                "info about reprinted checks
  pcec,                                "check numbers
 *pcec,                                "local memory for PCEC-data
  pyordh,                              "payment order (no payment doc.)
  regud,                               "form print transfer data
  regut,                               "internal data for TemSe-access
 *regut,                               "local memory for regut-data
  reguta,                              "paying company codes of a REGUT
                                       "  entry for authority check
  reguv,                               "control records of paymnt progr.
  rfdt,                                "indx-table for posting data
  sadr,                                "interface to ADDR_GET   "SADR40A
  spell,                               "digits and numbers in words
  sscrfields,                          "for modifying UCOMM
  t000,                                "logical system of client
  t001,                                "company code information
 *t001,                                "company code information
  t001s,                               "accounting clerks
 *t001s,                               "accounting clerks
  t005,                                "countries
  t005u,                               "region names
  t005t,                               "country names
  t012,                                "house banks
  t012k,                               "house bank accounts
  t012d,                               "parameters DME and Z1
  t015l,                               "SCB-(LZB-)ratios Germany
  t015m,                               "names of months
  t015w,                               "instruction keys DME Germany
  t015z,                               "numbers in words (function unit)
  t021m,                               "sort
  t042b,                               "name of remittance advice form
  t042e,                               "payment method / company code
  t042l,                               "names of bank transaction codes
  t042m,                               "user numbers with the bank
  t042n,                               "bank transaction codes
  t042t,                               "text modules for payment forms
  t042v,
  t042z,                               "payment method / country
  t045t,                               "DME- user-identification
  tbslt,                               "posting key names
  tcurc,                               "currency codes (ISO)
  tcurx,                               "decimal places in currencies
  toa_dara,                            "archive index
  tlsep,                               "control table for list sep.
  tsp01,                               "status of spool dataset
  tsp03,                               "printers
  tvoid,                               "void reason codes
  tvoit.                               "void reason codes, text

*----------------------------------------------------------------------*
* Fields                                                               *
*----------------------------------------------------------------------*

data: z_00(2) type c.

* internal tables (TAB) ------------------------------------------------
data begin of tab_vblnr_renum occurs 100.
        include structure dta_belege.  "Belege zu einer DTA-Referenznr
data    renum like regut-renum.        "documents of REGUT-RENUM
data end of tab_vblnr_renum.

data: begin of tab_ausgabe occurs 3,   "Spoolnummern der Formulare und
  filename(47)    type c,              "Listen und Filename bei DTA
  renum           like regut-renum,    "spool numbers of forms and lists
  dataset         like sy-prdsn,       "and file names (DME)
  name(35)        type c,
  spoolnr         like sy-sponr,
  immed(1)        type c,
  error(1)        type c,
end of tab_ausgabe.

ranges tab_bankcode for t005-landk.    "Länder mit separatem Bankcode
                                       "countries with separate bank cd.
data begin of tab_belege30a occurs 100."Belegeinträge in RFDT-Rel.3.0A
        include structure dta_belege.  "doc-# to be stored in RFDT-3.0A
data end of tab_belege30a.

ranges: tab_check for payr-checf.      "Schecks / range for checks

data: begin of tab_dta_zettel occurs 3,"sammelt die Währungsinformation
  waers           like reguh-waers,    "für den Begleitzettel bei DTA
  summe(7)        type p,
  ubknt           like reguh-ubknt,
  uwaer           like t012k-waers,
end of tab_dta_zettel.

data: begin of tab_edi_avis occurs 10, "Avise, die per EDI versendet
        reguh like reguh,              "wurden (Info für die Listaus-
        regud like regud,              "gabe im Avisformular)
      end of tab_edi_avis.             "advices sent by EDI

data  begin of tab_element occurs 23.  "nimmt gelesenes Element auf
        include structure tline.       "element of a SAPscript form
data  end of tab_element.

data  begin of tab_element2 occurs 23. "nimmt zusätzliches Element auf
        include structure tline.       "additional element of a
data  end of tab_element2.             "SAPscript form

data  begin of tab_elements occurs 30. "Liste aller Elemente eines
        include structure itcwe.       "Formulars
data  end of tab_elements.             "list or all elements

data  begin of tab_fimsg occurs 5.     "nimmt gesammelte Messages auf
        include structure fimsg.       "error messages
data: end of tab_fimsg,

begin of tab_kein_avis occurs 10,      "Zahlungsformulare, zu denen kein
  zbukr           like reguh-zbukr,    "Avis gedruckt werden soll (DTA)
  vblnr           like reguh-vblnr,    "payments which do not need a
end of tab_kein_avis,                  "remittance advice (DME)

begin of tab_regup occurs 200.         "Liste Einzelposten für User-Exit
        include structure regup.       "list of items (for user-exits)
data: end of tab_regup,

begin of tab_rfc occurs 0,             "destination for 3rd party
  bukrs           like regup-bukrs,    "remittance
  belnr           like regup-belnr,
  gjahr           like regup-gjahr,
  dest            like tbdestination-rfcdest,
  awsys           like bkpf-awsys,
  lifnr           like regup-lifnr,
  xblnr           like regup-xblnr,
  zbukr           like reguh-zbukr,
  vblnr           like reguh-vblnr,
end of tab_rfc,

begin of tab_schecks occurs 10,        "vornumerierte Schecks: zum
  zbukr           like reguh-zbukr,    "Zahlungsbeleg gehörende Schecknr
  vblnr           like reguh-vblnr,    "prenumbered checks: check number
  chect           like regud-chect,    "of a payment documents
end of tab_schecks,

begin of tab_splitting occurs 10,      "Belgien: Bei DTA werden keine
  zbukr           like reguh-zbukr,    "Avise erstellt, sondern die
  vblnr           like reguh-vblnr,    "Belege gesplittet. Je Zahlung
  rwbtr           like reguh-rwbtr,    "(nicht je Beleg) wird eine Info
  rbetr           like reguh-rbetr,    "in der Begleitliste ausgegeben
end of tab_splitting,

begin of tab_t005 occurs 5.            "Länderdaten
        include structure t005.        "country-data
data: end of tab_t005,

begin of tab_t012k occurs 5.           "Währungsdaten
        include structure t012k.       "currency-data
data: end of tab_t012k,

begin of tab_t042z occurs 5.           "selektierte Zahlwege
        include structure t042z.       "payment methods selected
data:   xsele(1)  type c,
end of tab_t042z,

begin of tab_uebergreifend occurs 10,  "Buchungskreisübergreifende Zah-
  zbukr           like reguh-zbukr,    "lungen für vornumerierte Schecks
  vblnr           like reguh-vblnr,    "cross-company payments when pre-
end of tab_uebergreifend.              "numbered checks are used

data:
  tab_t015l type sorted table of t015l "LZB-Kennzeichen
            with unique key lzbkz      "SCB Indicators
            with header line,

*Table for paying company codes of a DME file for authority
*check in transaction FDTA
tab_reguta type table of reguta with header line.

* error tables (ERR) ---------------------------------------------------
data begin of err_auth occurs 1.       "nicht selektierte Daten wegen
        include structure error_list.  "fehlender Berechtigung
data: end of err_auth,                 "missing authorization

begin of err_edi occurs 10,            "Fehler bei EDI:
  laufd like reguh-laufd,              "EDIBN=E - Fehler beim Versenden
  laufi like reguh-laufi,              "EDIBN=X - noch nicht versendet,
  xvorl like reguh-xvorl,              "          daher keine Verarbeitg
  zbukr like reguh-zbukr,
  lifnr like reguh-lifnr,
  kunnr like reguh-kunnr,
  empfg like reguh-empfg,
  vblnr like reguh-vblnr,
  rzawe like reguh-rzawe,
  edibn like reguh-edibn,
end of err_edi,

begin of err_element occurs 1,         "Element im Fenster des Formulars
  fname           like t042b-aforn,    "nicht gefunden (Erläuterung aus
  fenst(8)        type c,              "den Textelementen des Reports)
  elemt(6)        type c,              "elements not found in windows of
  text(30)        type c,              "a form (explanation coming from
end of err_element,                    "the report text elements)

begin of err_fw_scheck occurs 1,       "nicht gedruckte Fremdwährungs-
  fname           like t042b-aforn,    "schecks, da &REGUD-WAERS& im
  zbukr           like reguh-zbukr,    "Formular fehlt
  vblnr           like reguh-vblnr,    "not printed foreign currency
end of err_fw_scheck,                  "checks (&REGUD-WAERS& missing)

begin of err_in_worten occurs 1,       "Schecks ohne Betrag in Worten
  zbukr           like reguh-zbukr,    "(Betrag zu groß)
  vblnr           like reguh-vblnr,    "checks without amount in words
end of err_in_worten,                  "(amount too large)

begin of err_kein_dta occurs 1,        "nicht auf DTA ausgegebene Aus-
  error(2)        type n,              "landsüberweisungen, Begründung
  zbukr           like reguh-zbukr,    "durch den Fehlertyp ERROR
  vblnr           like reguh-vblnr,    "foreign payment not via DME,
end of err_kein_dta,                   "reason through error type ERROR

begin of err_kein_dtaws occurs 1,      "Kein Weisungsschlüssel vorhanden
  banks           like reguh-ubnks,    "Bankland          /bank country
  zlsch           like reguh-rzawe,    "Zahlweg           /paymt method
  dtaws           like reguh-dtaws,    "Weisungsschlüssel /indicator
end of err_kein_dtaws,                 "missing indicator

begin of err_nicht_verbucht occurs 1,  "nicht verbuchte Belege, zu denen
  zbukr           like reguh-zbukr,    "keine Formulare augegeben wurden
  vblnr           like reguh-vblnr,    "(Ergebnis der Beleg-Verprobung)
  pyord           like reguh-pyord,    "no forms (document not updated)
end of err_nicht_verbucht,

begin of err_t005 occurs 1,            "fehlende Einträge in T005
  land1           like t005-land1,     "missing entries in T005
end of err_t005,

begin of err_t012d occurs 1,           "fehlende Einträge in T012D
  zbukr           like reguh-zbukr,    "missing entries in T012D
  hbkid           like reguh-hbkid,
end of err_t012d,

begin of err_t015z occurs 1,           "fehlende Einträge in T015Z
  spras(2)        type c,              "missing entries in T015Z
  einh            like t015z-einh,
  ziff            like t015z-ziff,
end of err_t015z,

begin of err_t042b occurs 1,           "Avisformularname fehlt in T042B
  zbukr           like reguh-zbukr,    "missing form name for remittance
end of err_t042b,                      "advice form in T042B

begin of err_t042e occurs 1,           "fehlende Einträge in T042E
  zbukr           like reguh-zbukr,    "missing entries in T042E
  rzawe           like reguh-rzawe,
end of err_t042e,

begin of err_t042t occurs 1,           "fehlende Einträge in T042T
  zbukr           like reguh-zbukr,    "(Brieftextnamen nicht gefunden)
end of err_t042t,                      "missing text names in T042T

begin of err_t042z occurs 1,           "nicht zugelassene Zahlwege
  land1           like t042z-land1,    "für dieses Programm
  zlsch           like t042z-zlsch,    "not valid payment methods for
end of err_t042z,                      "this program

begin of err_tcurc occurs 1,           "Währungsschlüssel wurde nicht
  waers           like t001-waers,     "in ISO-Code umgesetzt
end of err_tcurc,                      "ISO code not found

begin of err_betrag occurs 0,          "maximal zulässiger Betrag gem.
  waers like reguh-waers,
  rwbtr(18),                           "Formatbeschreibung wurde
  zbukr like reguh-zbukr,              "überschritten
  vblnr like reguh-vblnr,              "maximum amount exceeded
end of err_betrag.


* text fields, titles, headings (TXT) ----------------------------------
data:
  txt_begleitl(79)  type c,            "text line in the summary list
  txt_line1         like bhdgd-line1,  "text for BHDGD-LINE1
  txt_line2         like bhdgd-line2,  "text for BHDGD-LINE2
  txt_uline1(132)   type c,            "single uline
  txt_uline2(132)   type c,            "double uline
  txt_zeile(132)    type c.            "work field for text


* text fields from text pool SAPDBPYF (filled in RFFORI00 and RFFORI0T)
data:
  text_001          like rfpdo2-fordtext,
  text_002          like rfpdo2-fordtext,
  text_003          like rfpdo2-fordtext,
  text_004          like rfpdo2-fordtext,
  text_005          like rfpdo2-fordtext,
  text_006          like rfpdo2-fordtext,
  text_090          like rfpdo2-fordtext,
  text_091          like rfpdo2-fordtext,
  text_092          like rfpdo2-fordtext,
  text_093          like rfpdo2-fordtext,
  text_094          like rfpdo2-fordtext,
  text_095          like rfpdo2-fordtext,
  text_096          like rfpdo2-fordtext,
* TEXT_101                             "print payment forms
* TEXT_102                             "print checks
* TEXT_103                             "print bills of exchange
* TEXT_104                             "data medium exchange
* TEXT_105                             "create payment advice
* TEXT_106                             "print list
* TEXT_107                             "on printer
* TEXT_108                             "print immediately
  text_505          like rfpdo2-fordtext,
  text_510          like rfpdo2-fordtext,
  text_512          like rfpdo2-fordtext,
  text_513          like rfpdo2-fordtext,
  text_514          like rfpdo2-fordtext,
  text_515          like rfpdo2-fordtext,
  text_520          like rfpdo2-fordtext,
  text_525          like rfpdo2-fordtext,
  text_526          like rfpdo2-fordtext,
  text_530          like rfpdo2-fordtext,
  text_535          like rfpdo2-fordtext,
  text_540          like rfpdo2-fordtext,
  text_545          like rfpdo2-fordtext,
  text_546          like rfpdo2-fordtext,
  text_550          like rfpdo2-fordtext,
  text_555          like rfpdo2-fordtext,
  text_605          like rfpdo2-fordtext,
  text_610          like rfpdo2-fordtext,
  text_611          like rfpdo2-fordtext,
  text_612          like rfpdo2-fordtext,
  text_613          like rfpdo2-fordtext,
  text_614          like rfpdo2-fordtext,
  text_615          like rfpdo2-fordtext,
  text_620          like rfpdo2-fordtext,
  text_625          like rfpdo2-fordtext,
  text_630          like rfpdo2-fordtext,
  text_634          like rfpdo2-fordtext,
  text_635          like rfpdo2-fordtext,
  text_800          like rfpdo2-fordtext,
  text_801          like rfpdo2-fordtext,
  text_802          like rfpdo2-fordtext,
  text_803          like rfpdo2-fordtext,
  text_804          like rfpdo2-fordtext,
  text_805          like rfpdo2-fordtext,
  text_806          like rfpdo2-fordtext,
  text_807          like rfpdo2-fordtext,
  text_809          like rfpdo2-fordtext,
  text_810          like rfpdo2-fordtext,
  text_811          like rfpdo2-fordtext,
  text_812          like rfpdo2-fordtext,
  text_813          like rfpdo2-fordtext,
  text_814          like rfpdo2-fordtext,
  text_815          like rfpdo2-fordtext,
  text_816          like rfpdo2-fordtext,
  text_820          like rfpdo2-fordtext,
  text_821          like rfpdo2-fordtext,
  text_822          like rfpdo2-fordtext,
  text_830          like rfpdo2-fordtext,
  text_831          like rfpdo2-fordtext,
  text_832          like rfpdo2-fordtext,
  text_834          like rfpdo2-fordtext,
  text_900          like rfpdo2-fordtext,
  text_901          like rfpdo2-fordtext,
  text_902          like rfpdo2-fordtext,
  text_903          like rfpdo2-fordtext,
  text_904          like rfpdo2-fordtext.


* Flags, pointer (FLG) -------------------------------------------------
data:
  flg_avis(1)       type n value 0,    "1 - if program RFFOAVIS runs
  flg_bankinfo(1)   type n value 0,    "0 - sum. list without bank info
                                       "1 - only house bank info (check)
                                       "2 - complete bank info
  flg_begleitl(1)   type n value 0,    "1 - top of page for summary list
  flg_dialog(1)     type c,            "X - printer dialogue screen
  flg_diff_bukrs(1) type n value 0,    "1 - cocd of inv <> cocd of paymt
  flg_druckmodus(1) type n value 0,    "0 - keine Spool offen
                                       "1 - nur drucken ohne archivieren
                                       "2 - faxen oder archivieren ....
  flg_end_ubknt(1)  type n value 0,    "simulation in summary list
  flg_end_zbnkl(1)  type n value 0,    "simulation in summary list
  flg_end_zbukr(1)  type n value 0,    "header line in summary list
  flg_file_open(1)  type n value 0,    "1 - file has been opened
  flg_fw_scheck(1)  type n value 0,    "1 - symbol &REGUD-WAERS& found
  flg_iban(1)       type n value 0,    "1 - IBAN active
  flg_kein_druck(1) type n value 0,    "1 - print not possible/necessary
  begin of flg_koart_auth,             "authority for account type
    k(1)            type c,            "X - creditors
    d(1)            type c,            "X - debitors
    s(1)            type c,            "X - GL accounts
  end of flg_koart_auth,
  flg_liste(1)      type n value 0,    "1 - list is started (print on)
  flg_local(1)      type c,            "local flag
  flg_neud(1)       type n value 0,    "1 - reprint checks
                                       "2 - reprint all (without range)
  flg_new_page(1)   type n value 0,    "new page in summary list
  flg_new_zbnkl(1)  type n value 0,    "simulation in summary list
  flg_no_replace(1) type c,            "X - no replacing of special char
  flg_papieravis(1) type n value 0,    "1 - advice on paper, not EDI etc
  flg_probedruck(1) type n value 0,    "0 - test print not jet finished
  flg_pruefung(1)   type n value 0,    "0 - no check necessary
  flg_restart(1)    type n value 0,    "0 - no restart
                                       "1 - restart, but only partially
                                       "2 - complete restart
  flg_schecknum(1)  type n value 0,    "1 - prenumbered checks active
  flg_selektiert(1) type n value 0,    "0 - nothing selected
  flg_sgtxt(1)      type n value 0,    "0 - do not print segment text
  flg_sort(1)       type n value 0,    "0 - not sorted
                                       "1 - sort
                                       "2 - sort by avis
  flg_zettel(1)     type n value 1.    "0 - no form for acc.sheet needed

* Counter (CNT) --------------------------------------------------------
data:
  cnt_anlagen(5)    type p,            "number of annexed papers
  cnt_avise(5)      type p,            "number of remittance advices
  cnt_avedi(5)      type p,            "number of rem. advices via EDI
  cnt_avfax(5)      type p,            "number of rem. advices via fax
  cnt_avmail(5)     type p,            "number of rem. advices via mail
  cnt_error(10)     type n,            "sort field for error messages
  cnt_erwteil(2)    type n,            "text field number (DME)
  cnt_formulare(5)  type p,            "number of payment forms
  cnt_filenr(2)     type n,            "file number (DME)
  cnt_hinweise(5)   type p,            "number of forms with advice note
  cnt_posten(5)     type p,            "number of items (summary list)
  cnt_records(5)    type p,            "number of records (DME)
  cnt_seiten(5)     type p,            "number of pages printed
  cnt_zeilen(5)     type p.            "number of text lines needed

* total fields, sums (SUM) ---------------------------------------------
data:
  sum_abschluss(8)  type p,            "paymt amount summary per form
  sum_abschl_edi(8) type p,            "paymt amount summary via EDI
  sum_abschl_fax(8) type p,            "paymt amount summary via fax
  sum_abschl_mail(8) type p,           "paymt amount summary via mail
  sum_bukrs(8)      type p,            "total per company code
  sum_regut(8)      type p,            "paymt amount summary per file
  sum_ubknt(8)      type p,            "total per house bank account
  sum_waers(8)      type p,            "total per currency
  sum_waers_fw(8)   type p,            "total per currency (foreign c.)
  sum_zbnkl(8)      type p,            "total per bank of payee
  sum_zbnkl_fw(8)   type p.            "total per bank of payee (for.c.)

* other help fields (HLP) ----------------------------------------------
data:
  hlp_aforn         like t042b-aforn,  "alternative advice name (param.)
  hlp_auth          like itcpo-tdautority, " print authority
  hlp_betrag(18)    type c,            "amount CURRENCY ...
  hlp_checf_restart like payr-checf,   "first check (restart)
  hlp_date          type d,            "date-field
  hlp_datfeld       type d,            "date-field (last execution date)
  hlp_datum(8)      type c,            "date DD/MM/YY
  begin of hlp_dta_id,                 "Struktur der key-ID für RFDT
    progr(4)        type c value 'RFFO',
    refnr           like regut-renum,
    filler(2)       type c value '  ',
  end of hlp_dta_id,
  hlp_dta_zwels     like regud-zwels,  "list of payment methods/file
  hlp_dtfin(10)     type n,            "T012D-DTFIN numeric
  hlp_dtfor         like regut-dtfor,  "dme format name of TemSe
  hlp_dtfor_long    like t042ofi-formt,"dme format name of selection
  hlp_element(6)    type c,            "name of SAPscript elements
  hlp_eletext(30)   type c,            "explanation text to an element
  hlp_ep_element(6) type c,            "element name of single item info
  hlp_fbtch(2)      type n,            "action type (authority check)
  hlp_filename(47)  type c,            "file name (DME)
  hlp_filler        type c value '*',  "filler in spell_amount
  hlp_form_zwels    like regud-zwels,  "list of payment methods/form
  hlp_formular      like t042b-aforn,  "form (customizing, ofi, param.)
  hlp_handle        like rststype-handle,"Handle to write into TemSe
  hlp_ktwae         like tcurc-waers,  "currency
  hlp_laufk         like ilaufk-laufk, "equals REGUH-LAUFI+5(1)
  hlp_maxbetrag(14) type n,            "max. amount for digits in words
  hlp_maxstellen(2) type n,            "max. dec. places for digits i.w.
  hlp_offset(2)     type p,            "offset (writing to a text field)
  hlp_page(5)       type c,            "current page from SAPscript
  hlp_plort(50)     type c,            "<Country>-<postal code> <city>
  hlp_renum(8)      type n,            "internal reference number(TemSe)
  hlp_resultat(10)  type n,            "buffer for reference-number
  hlp_rzawe         like reguh-rzawe,  "for re-reading data (DME)
  hlp_seite(5)      type c,            "page counter
  hlp_sort(32)      type c,            "sort field
  hlp_sorth1(16)    type c,            "sort field for letters
  hlp_sorth2(16)    type c,            "sort field for letters
  hlp_sorth3(16)    type c,            "sort field for letters
  hlp_sortp1(16)    type c,            "sort field for single items
  hlp_sortp2(16)    type c,            "sort field for single items
  hlp_sortp3(16)    type c,            "sort field for single items
  hlp_svarh         like t042e-svarh,  "sort variant for correspondence
  hlp_svarp         like t042e-svarp,  "sort variant for single item
  hlp_sprache       like reguh-zspra,  "language for reading texts
  hlp_subrc         like sy-subrc,     "return code
  hlp_t021m_h       like t021m,        "content of T021M for REGUH
  hlp_t021m_p       like t021m,        "content of T021M for REGUP
  hlp_temse(10)     type c,            "report-depending: TemSe-DTYPES
  hlp_temsename     like rststype-name,"filename
  hlp_tsdat         like regut-tsdat,  "date of file generation
  hlp_tstim         like regut-tstim,  "time of file generation
  hlp_ubknt         like reguh-ubknt,  "bank account (bill of exchange)
  hlp_ubnkl         like reguh-ubnkl,  "bank number (bill of exchange)
  hlp_ubnks         like reguh-ubnks,  "bank country (bill of exchange)
  hlp_waers         like reguh-waers,  "currency code
  hlp_xeuro         like t042z-xeuro,  "X - EU internal transfer
  hlp_xhrfo         like hrxblnr-xhrfo,"X - use HR form
  hlp_zbnkl(15)     type n,            "numerical bank number of receiv.
  hlp_zbnkn(18)     type n,            "numerical bank account of recei.
  hlp_zeilen(2)     type n,            "number of text lines (DME)
  hlp_zforn         like t042e-zforn,  "alternative form name (param.)
  hlp_zwels(20)     type c.            "list of payment methods

* saved data during text print (SIC) -----------------------------------
data:
  sic_fsabe like fsabe,
  sic_itcpo like itcpo,
  sic_regud like regud,
  sic_reguh like reguh,
  sic_regup like regup.


* fields for test print (XXX) ------------------------------------------
data:
  xxx_fsabe like fsabe,
  xxx_regud like regud,
  xxx_reguh like reguh,
  xxx_regup like regup,
  xxx_spell like spell.


* Schnittstelle Scheckmanagement / HR
include rpc4f_0c.

data: pinfo like pc407,
      pform like pc408 occurs 70 with header line.



*----------------------------------------------------------------------*
* Field-groups, standard sort and special sort                         *
*----------------------------------------------------------------------*
field-groups:
  header,
  daten,
  avis.



insert
  reguh-zbukr                          "paying company code
  reguh-rzawe                          "payment method
  reguh-ubnks                          "country of house bank
  reguh-ubnky                          "bank key (for sort)
  reguh-ubnkl                          "bank number of house bank
  reguh-ubknt                          "account number at house bank
  reguh-waers                          "currency code
  reguh-zbnks                          "country of payee's bank
  reguh-zbnky                          "bank key (for sort)
  reguh-zbnkl                          "bank number of payee's bank
  reguh-zbnkn                          "account number of payee
  reguh-lifnr                          "creditor number
  reguh-kunnr                          "debitor number
  reguh-empfg                          "payee is CPD / alternative payee
  reguh-vblnr                          "payment document number
  regud-xeinz                          "X - incoming payment
  hlp_xeuro                            "X - EU internal transfer
  hlp_sort                             "sort field (e.g. for BACS)
  hlp_sorth1                           "sort field for letters
  hlp_sorth2                           "sort field for letters
  hlp_sorth3                           "sort field for letters
  hlp_sortp1                           "sort field for single items
  hlp_sortp2                           "sort field for single items
  hlp_sortp3                           "sort field for single items
  regup-bukrs                          "company code
  regup-belnr                          "invoice document number
  regup-shkzg                          "debit/credit indicator
into header.

insert
  reguh
  regup
  regud-gjahr
  regud-xabwz
into daten.

insert
  reguh-zbukr                          "paying company code
  reguh-rzawe                          "payment method
  reguh-ubnks                          "country of house bank
  reguh-ubnky                          "bank key (for sort)
  reguh-ubnkl                          "bank number of house bank
  hlp_sorth1                           "sort field for letters
  hlp_sorth2                           "sort field for letters
  hlp_sorth3                           "sort field for letters
  reguh-ubknt                          "account number at house bank
  reguh-waers                          "currency code
  reguh-zbnks                          "country of payee's bank
  reguh-zbnky                          "bank key (for sort)
  reguh-zbnkl                          "bank number of payee's bank
  reguh-zbnkn                          "account number of payee
  reguh-vblnr                          "payment document number
  regup-bukrs                          "company code
  hlp_sortp1                           "sort field for single items
  hlp_sortp2                           "sort field for single items
  hlp_sortp3                           "sort field for single items
  regup-belnr                          "invoice document number
into avis.



*----------------------------------------------------------------------*
*  checks of the entries on the selection screen                       *
*----------------------------------------------------------------------*
at selection-screen on par_pria.
  if par_pria ne space.
    select single * from tsp03
      where padest eq par_pria.
    if sy-subrc ne 0.
      message e078.
    endif.
  endif.

at selection-screen on par_prib.
  if par_prib ne space.
    select single * from tsp03
      where padest eq par_prib.
    if sy-subrc ne 0.
      message e078.
    endif.
  endif.

at selection-screen on par_priw.
  if par_priw ne space.
    select single * from tsp03
      where padest eq par_priw.
    if sy-subrc ne 0.
      message e078.
    endif.
  endif.

at selection-screen on par_priz.
  if par_priz ne space.
    select single * from tsp03
      where padest eq par_priz.
    if sy-subrc ne 0.
      message e078.
    endif.
  endif.

at selection-screen.
  if sscrfields-ucomm eq 'PRIN'.       "no difference between starting
    sscrfields-ucomm = 'ONLI'.         "with F8 or F13
  endif.
  if par_xdta ne space and par_zdru ne space.
    message e067.
  endif.
  if par_maxp eq 0 and par_begl eq 'X'.
    par_begl = space.
  endif.
  if  par_zdru eq space
  and par_xdta eq space
  and par_avis eq space
  and ( par_begl eq space or par_maxp eq 0 ).
    message e071.
  endif.
