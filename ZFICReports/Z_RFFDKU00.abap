report rffdku00 message-id rq
* no standard page heading
  line-size 132.

tables: lfa1,
        lfb1,
        bsik,
        kna1,
        knb1,
        bsid,
        ska1,
        skb1,
        bsis,
        bkpf,
        bseg,
*       Tabellen für vorerfasste Belege
        vbkpf,
        vbsegk,
        vbsegd,
        vbsegs,
        rseg,                          "Belegposition Eingangsrechnung
        atpra.   "Treasury: Verrechnungskonto für Zahlungsanordnungen

tables: t001,
        tgsb,
        t035,
        t074,
        t042,
        rfpdo1,                        "Doku Parameter
        rfsdo.                         "Doku Select-Options

tables: payrq,                         "Payment Request
        cpayrq,                        "Payment Request mit FD-Feldern
        pyordh.                        "Zahlungsaufträge

tables: dd01l.                         "Domaenen

* Konstanten
* Maximale Anzahl von Eintraegen in einem Range, die einem Select
* uebergeben werden darf
data: maxrange  type i value 50.       "Abbruch wenn 'aufgeloester'
                                       "Select groesser 8K ist
* Flags
data: formok-flag(1),        "1 - Form-Routine vollstaendig durchlaufen
      nextkto-flag(1),       "1 - weiter mit naechstem Konto/BK
      nextbkpf-flag(1),      "1 - weiter mit naechstem Belegkopf
      deb-flag(1),           "1 - bereits ein Debitorposten gelesen
      kred-flag(1),          "1 - bereits ein Kreditorposten gelesen
      sak-flag(1),           "1 - bereits ein Sachkontoposten gelesen
      zbukrs-flag(1) type c, "1 - es gibt zahlende Buchungskreise
      veb-flag(1),                     "1 - vorerfasster Beleg
      pr-flag(1),                      "1 - Payment Request
* 1 - Beleg nicht FD-relevant, aber ggf. muessen FD-Felder im BSEG
* zurueckgesetzt werden, da frueher Gruppe oder Ebene im Stammsatz
* gepflegt war oder der Sonderumsatz frueher FD-relevant war oder
* in vorerfaßten Belegen noch FD-Daten stehen, obwohl im Beleg Bestell-
* bezug vorhanden ist bzw. ein Payment Request vorhanden ist
      reset-flag(1).         "1 - BSEG-FD-Felder initialisieren

* Arbeitsfelder
data: wagrupp like bseg-fdgrp,
      waebene like bseg-fdlev,
      wakoart like bseg-koart,         "fuer Protokoll
      watext(132)  type c,             "fuer Protokoll
      wazv    like knb1-xzver,   "Zahlungsverhalten wird aufgezeichnet
      wabukrs like bseg-bukrs,
      warelbk like bseg-bukrs,         "relevanter Buchungskreis
      waamount like pyordh-rwbtr,

*     X=FD-Fortschreibung gemäß Parameter p_restr gewünscht
*     (siehe Doku bei end-of-selection)
      waxfdok(1) type c,

      bklines type i,
      gblines type i,
      cntr    type i,
      cntr2   type i,
*     abweichende Zahlungswährung
      abw_zahlwaehrung like bseg-pycur.

* Feldleisten
data: begin of olddata,                "alten FD-Daten im BSEG
        fdgrp like bseg-fdgrp,
        fdlev like bseg-fdlev,
        fdtag like bseg-fdtag,
        fdwbt like bseg-fdwbt,
      end   of olddata.

data: begin of ibsegkey,               "Key-Felder von BSEG
        ibukrs like payrq-bukrs,       "entspricht BSEG-Key
        ibelnr like payrq-belnr,
        igjahr like payrq-gjahr,
        ibuzei like payrq-buzei,
      end   of ibsegkey.

data: icpayrq like payrq.

* Interne Tabellen
data: i035  like t035  occurs 30  with header line. "Dispogruppen
data: i074  like t074  occurs 30  with header line. "Sonderumsatzarten
data: vbtab like rf40s occurs 250 with header line. "Summensatzverbuch.
data: itab_cpayrq like cpayrq occurs 0   with header line.
data: tab_cpayrq  like cpayrq occurs 250 with header line.
data: t_pyordp    like pyordp occurs 10  with header line.

*------- interne Tabelle für Update von Zahlungsaufträgen, bei denen ---
*------- die Ebene von der Ebene im Stammsatz abweicht -----------------
data: t_pyordh_update like pyordh occurs 100 with header line.

data: begin of itab_skb1 occurs 250,
        saknr like skb1-saknr,
        bukrs like skb1-bukrs,
        fdlev like skb1-fdlev,
        waers like skb1-waers,
        xopvw like skb1-xopvw,         "X - offene Postenverwaltung
        notfound(1)     type c,   "1 - Konto konnte nicht gelesen werden
      end   of itab_skb1.

data: begin of bbtab occurs 0,
*------- Bestellbezugtabelle zum Erkennen, welche vorerfaßten Belege
*------- nicht fortgeschrieben werden duerfen, da Betraege bereits
*------- ueber die MM-Schnittstelle in die Finanzdispo fortgeschrieben
*------- wurden (vgl. Hinweise 312672 und 447017 - MR41/MIR7)
         bukrs like vbsegs-bukrs,
         belnr like vbsegs-belnr,
         gjahr like vbsegs-gjahr,
       end of bbtab.

* Enthält alle Felder aus PAYRQ. Wegen binary search sind am Anfang
* die BSEG-Key-Felder nochmals gespeichert.
* Für Debitoren/Kreditoren gilt: Nach dem Lesen der Belegzeile
* wird PAYRQ nachgelesen. Stehen in BSEG FD-Daten und gibt es einen
* Payment Request, werden die FD-Daten initialisiert.
* Für Sachkonten gilt: Da die FD-Daten in der Sachkontozeile und nicht
* im Payment Request stehen, sind nur die Payment Requests relevant,
* bei denen die Sachkontobuchung noch nicht erfolgt ist.
* Bei Änderung der Tabelle muß Select auf PAYRQ angepaßt werden, d.h.
* neue Felder sind dem Select mitzugeben
data: begin of itab_pr occurs 0.
*       erster Include wegen Zugriff auf Tabelle mit binary search
        include structure ibsegkey.    "entspricht BSEG-Key
        include structure payrq.
data: end   of itab_pr.

field-groups: header,
              daten.

insert
  bseg-koart
  bseg-fdgrp
  bseg-hkont
  bseg-bukrs
into header.

insert
  olddata-fdgrp
  olddata-fdlev
  olddata-fdtag
  olddata-fdwbt
  bseg-gjahr
  bseg-belnr
  bseg-buzei
  bseg-fdlev
  bseg-fdtag
  bseg-fdwbt
  bseg-gsber
  bkpf-waers                           "Belegkopfwährung
  abw_zahlwaehrung
  bseg-kunnr
  bseg-lifnr
  bseg-saknr
  bseg-shkzg
  bseg-umskz
  bseg-xpypr                           "X-Zahlungsauftrag vorhanden
  veb-flag                             "1-vorerfasster Beleg
  pr-flag                              "1-Payment Request
  icpayrq
into daten.

* Ranges fuer verbesserten Select; falls R_xxxxx2 gefuellt: R_xxxxx leer
ranges:
 r_bukrs  for bseg-bukrs occurs 50,    "BK die relevant und berechtigt
 r_bukrs2 for bseg-bukrs occurs 50,    "BK die relevant und berechtigt
 r_gsber  for bseg-gsber occurs 50,    "GB fuer die berechtigt
 r_gsber2 for bseg-gsber occurs 50.    "GB fuer die berechtigt

* Parameter und Select-Options
selection-screen begin of block one with frame title text-100.
select-options: s_koart for  bseg-koart,
                s_saknr for  ska1-saknr matchcode object sako,
                s_kunnr for  kna1-kunnr matchcode object debi,
                s_lifnr for  lfa1-lifnr matchcode object kred,
                s_bukrs for  bseg-bukrs,
                s_gjahr for  bkpf-gjahr,
                s_belnr for  bkpf-belnr,
                s_prkey for  payrq-keyno,
                s_pyord for  pyordh-pyord.
parameters:     p_restr like rfffpdo1-fdbuildup no-display,
                p_test  like rfpdo1-allgtest default 'X'.
selection-screen end   of block one.

at selection-screen on p_restr.
  if p_restr = '1'   "nur Sachkonten werden fortgeschrieben
  or p_restr = '3'.  "nur Sachkonten ohne OP-Verwaltung werden fortgesch
*   Bei Einschränkung auf Sachkonten ist nur Wert '2' zulässig
    message e166.
  endif.

  if p_restr = '5'   "nur Debitoren werden fortgeschrieben
  or p_restr = '6'.  "nur Kreditoren werden fortgeschrieben
*   Prüfen, ob Vertragskontokorrent (FI-CA) im System aktiv ist
*   (falls es die Domäne APPLK_KK gibt, dann sollte das der Fall sein)
    select domname from dd01l into dd01l-domname up to 1 rows
           where domname  = 'APPLK_KK'.
    endselect.
    if sy-subrc = 0.
*     Personenkonten-Abgrenzungen wegen Vertragskontokorrent eingeschrän
      message e283.   "-> hier nur p_restr = '4' möglich
    endif.
  endif.

at selection-screen.
  if p_restr = '2'.   "nur Sachkonten mit OP-Verwaltung werden fortgesch
* or p_restr = '1'.   "kann hier nicht sein - siehe oben
*   siehe Doku im Langtext von Meldung RQ-167
    select * from payrq up to 1 rows
           where augdt = '00000000'    "keine ausgeglichenen Posten
           and   xreve <> 'X'          "keine stornierten Posten
           and ( koart = 'D' or koart = 'K' )
*          Fortschreibung erfolgt trotz Personenkontoart auf Bankkonto
*          bzw. Bankverrechnungskonto (vgl. Coding im FB
*          cash_forecast_pr_check)
           and ( bkhbk <> space or hbkid <> space ).
*          hier nicht auf belnr = space oder <> space abfragen
    endselect.
    if sy-subrc = 0.
      if  s_koart is initial
      and s_saknr is initial
      and s_kunnr is initial
      and s_lifnr is initial
      and s_bukrs is initial
      and s_gjahr is initial
      and s_belnr is initial.
*       okay
      else.
        if sy-binpt is initial and sy-batch is initial.
*         Bei Einschränkung auf Sachkonten sollten andere Abgrenzungen l
          message w167.
        endif.
      endif.
    endif.
  endif.                               "if p_restr = '2'

* Falls Vertragskontokorrent aktiv ist, kann keine Abgrenzung auf
* Kontoart 'D' oder 'K' vorgenommen werden und es darf auch nicht auf
* Debitoren oder Kreditoren abgegrenzt werden. Im Vertragskontokorrent
* können Dispogruppen verwendet werden, die in keinem Debitor- oder
* Kreditorstammsatz auftauchen und somit ist eine Zuordnung von
* Dispogruppen zu Debitoren oder Kreditoren nicht mehr möglich und
* somit auch kein Löschen von Sätzen aus FDSR mittels Dispogruppen.
* Prüfen, ob Vertragskontokorrent (FI-CA) im System aktiv ist
* (falls es die Domäne APPLK_KK gibt, dann sollte das der Fall sein)
  select domname from dd01l into dd01l-domname up to 1 rows
         where domname  = 'APPLK_KK'.
  endselect.
  if sy-subrc = 0.
    if ( 'D' in s_koart and not 'K' in s_koart )
    or ( 'K' in s_koart and not 'D' in s_koart )
    or not s_kunnr[] is initial
    or not s_lifnr[] is initial.
*     Personenkonten-Abgrenzungen wegen Vertragskontokorrent eingeschrän
      message e283.
    endif.
  endif.

* Start-of-selection
start-of-selection.

* Tabelle T035 zwischenspeichern
  select * from t035 into table i035.

* Tabelle T074 zwischenspeichern
  select * from t074 into table i074
         where ebene <> space.         "fuer Finanzdispo relevant

* Abspeichern der BK, in denen die Finanzdispo aktiv ist und fuer die
* berechtigt
  r_bukrs-sign   = 'I'.
  r_bukrs-option = 'EQ'.
  select * from t001
         where bukrs in s_bukrs
         and   xfdis <> space.

    authority-check object 'F_FDSB_BUK'
      id 'BUKRS' field  t001-bukrs
      id 'ACTVT' field  '01'.          "hinzufuegen (in FDSB)
    if sy-subrc <> 0.
      perform protokoll using '236'.
    else.
      authority-check object 'F_FDSR_BUK'
        id 'BUKRS' field  t001-bukrs
        id 'ACTVT' field  '01'.        "hinzufuegen (in FDSR)
      if sy-subrc <> 0.
        perform protokoll using '236'.
      else.
*       Berechtigung für FDSB und FDSR vorhanden
        r_bukrs-low = t001-bukrs.
        append r_bukrs.
      endif.
    endif.
  endselect.

  describe table r_bukrs lines bklines.
  if bklines = 0.
*   FD in BK nicht aktiv oder keine Berechtigung
    perform protokoll using '240'.
    stop.
  endif.

* entsprechende Pruefung fuer GB unsinnig, da fuer GB Space immer
* berechtigt

* Abspeichern der GB, für die Berechtigung vorhanden
  r_gsber-sign   = 'I'.
  r_gsber-option = 'EQ'.

  append r_gsber.                      "für GB Space immer berechtigt
  select * from tgsb.

    authority-check object 'F_FDSB_GSB'
      id 'GSBER' field  tgsb-gsber
      id 'ACTVT' field  '01'.          "hinzufuegen (in FDSB)
    if sy-subrc <> 0.
      perform protokoll using '238'.
    else.
      authority-check object 'F_FDSR_GSB'
        id 'GSBER' field  tgsb-gsber
        id 'ACTVT' field  '01'.        "hinzufuegen (in FDSR)
      if sy-subrc <> 0.
        perform protokoll using '238'.
      else.
*       Berechtigung für FDSB und FDSR vorhanden
        r_gsber-low = tgsb-gsber.
        append r_gsber.
      endif.
    endif.
  endselect.

*----------------------------prueft ob ranges zu gross fuer select---*
  perform ranges_check.

* ermitteln, ob zahlende Buchungskreise im Spiel sein können
  select * from t042.
    if  t042-zbukr <> space
    and t042-zbukr <> t042-bukrs.
      zbukrs-flag = '1'.
      exit.
    endif.
  endselect.

* r_bukrs und r_gsber können im obigen Perform initialisiert worden
* sein -> im select ggf. gegen kopierte Ranges verproben
  select * from payrq
         where keyno in s_prkey
         and   bukrs in r_bukrs
         and   bukrs in s_bukrs
         and   gsber in r_gsber
         and ( koart in s_koart or koart = space )
*        vgl. Doku ganz oben (bei Sachkonten sind nur die Payment
*        Requests ohne Buchung interessant)
         and ( koart = 'D' or koart = 'K'
          or   koart = 'S' or koart = ' ' )

         and ( koart = 'D' or koart = 'K' or belnr = space )

         and ( ( koart = 'D' and parno in s_kunnr )
          or  ( koart = 'K' and parno in s_lifnr )
          or  ( koart = 'S' and ubhkt in s_saknr )
          or  ( koart = ' ' and ubhkt in s_saknr ) )

         and ( belnr in s_belnr or belnr = space )
         and ( gjahr in s_gjahr or gjahr = '0000' )
*        leeres Ausgleichsdatum auch notwendig, um Payment Requests mit
*        leerer Belegnummer aber gefülltem Ausgleichsdatum auszuschlies-
*        sen (vgl. Doku in Cash_Forecast_Pr_Check), da FI-Beleg gebucht
         and   augdt = '00000000'
*        order by darf nicht geändert werden wegen binary search
         order by bukrs belnr gjahr buzei.  "entspricht BSEG-Key

*-----Ranges waren zu gross fuer Select: Ranges jetzt verproben------*
*-----(Beim Select mit FOR ALL ENTRIES zu arbeiten geht hier nicht,--*
*-----da dann ORDER BY nicht genutzt werden könnte)------------------*
    if bklines > maxrange.
      check: payrq-bukrs in r_bukrs2.
    endif.
    if gblines > maxrange.
      check: payrq-gsber in r_gsber2.
    endif.

    if 'S' in s_koart.
*     auch Kontoart ' ' akzeptieren (entspricht 'S')
    else.
      check: payrq-koart <> space.
    endif.

*   Payment Requests, die zu Bankkontoüberträgen gehören, werden hier
*   nicht berücksichtigt, da ansonsten 2 Fortschreibungen auf 2 ver-
*   schiedenen Konten (die sich in unterschiedlichen Buchungskreisen
*   befinden können und die einmal OP-geführt und einmal
*   nicht-OP-geführt sein können und eines der beiden Konten
*   FD-relevant sein kann, das andere nicht) ausgeführt werden müßte
*   und somit z.B. auch eine Fortschreibung auf ein BK und ein Konto
*   geschehen müßte, das gemäß Abgrenzungen nicht fortzuschreiben wäre.
    if  payrq-zbukr <> space
    and payrq-ggrup <> space.
*     -> Bankkontoübertrag
      check: 1 = 2.
    endif.

*   für späteren Zugriff mit binary search, da beim binary search davon
*   ausgegangen wird, daß die relevanten Felder ganz oben stehen
    move: payrq-bukrs to itab_pr-ibukrs,
          payrq-belnr to itab_pr-ibelnr,
          payrq-gjahr to itab_pr-igjahr,
          payrq-buzei to itab_pr-ibuzei.
*   alle Felder aus Payrq
    move-corresponding payrq to itab_pr.
    append itab_pr.
  endselect.

* Es werden alle offenen Debitoren-, Kreditoren- und Sachkontenposten
* bearbeitet in allen BK, in denen die Finanzdispo aktiv ist
* Zusätzlich werden die vorerfassten Belege bearbeitet. (Vgl. LF040F00
* Formroutinen Finanzdispo_...). Die vorerfassten Belege aus MM werden
* aber nur beruecksichtigt, wenn entweder in T001 der Schalter fuer
* die MM->FD Fortschreibung aus ist oder aber der Beleg keinen Bestell-
* bezug hat. Ab Release 4.6 werden vollständig gesicherte vorerfasste
* Belege mit Bestellbezug wie gebuchte Belege behandelt, d.h. die
* Bestellung wird abgebaut und der vorerfasste Beleg steht in CM

*---------------------------------------------------------------------*
*   Tabelle mit Belegnummern aufbauen, die nicht in die Finanzdispo
*   fortgeschrieben werden duerfen wegen Bestellbezug. Der Bestellbezug
*   kann (bis einschl. 4.6B) nur in einer Sachkontenzeile stehen,
*   d.h. in VBSEGS. Ab 4.6C ist andere Logik notwendig, da dann die
*   Vorerfassung mit MIR7 statt mit MR41 erfolgt und dann kann die
*   VBSEGS-Zeile fehlen. Daher muß dann über VBKPF und RSEG ermittelt
*   werden, ob ein Bestellbezug besteht.
*---------------------------------------------------------------------*

* Beim Select keine Verprobung gegen r_gsber, r_saknr,
* da sonst ggf. der Bestellbezug nicht erkannt wird
  select * from vbsegs                 "Belegsegment Vorerfassung Sachk.
         where bukrs in r_bukrs
         and   bukrs in s_bukrs
         and   gjahr in s_gjahr
         and   belnr in s_belnr
         and   koart = 'B'             "Bestellbezug
         order by bukrs belnr gjahr.
    if t001-bukrs <> vbsegs-bukrs.
      select single * from t001 where bukrs = vbsegs-bukrs.
    endif.

    if t001-xfdmm is initial.
*     keine Fortschreibung MM in Finanzdispo, daher sind alle
*     vorerfaßten Belege zu berücksichtigen
    else.
*     Beleg muss übergangen werden, da bereits über MM-Schnittstelle
*     in Finanzdispo fortgeschrieben. Ggf. werden FD-Felder initiali-
*     siert
      if  bbtab-bukrs = vbsegs-bukrs
      and bbtab-belnr = vbsegs-belnr
      and bbtab-gjahr = vbsegs-gjahr.
*       bereits in Tabelle
      else.
        move-corresponding vbsegs to bbtab.
*       falls der vorerfasste Beleg vollständig gesichert ist, wird
*       der Eintrag unten wieder rausgeworfen, da dann der vorerfasste
*       Beleg in CM benötigt wird
        append bbtab.
      endif.
    endif.
  endselect.

* Hinweis zum Debugging und Belegabgrenzung: es gibt 3 verschiedene
* Belegnummern: Bestellung, Eingangsrechnung MM und FI-Beleg -> es muß
* aus die FI-Belegnr. abgegrenzt werden
  select * from vbkpf                  "Belegkopf vorerfaßter Beleg
         where bukrs in r_bukrs
         and   bukrs in s_bukrs
         and   gjahr in s_gjahr
         and   belnr in s_belnr
         and   awtyp = 'RMRP'    "Rechnungseingang (MM-Rechnungsprüfung)
         and   awkey <> space          "Bestellbezug
         order by bukrs.
    if t001-bukrs <> vbkpf-bukrs.
      select single * from t001 where bukrs = vbkpf-bukrs.
    endif.

    if t001-xfdmm is initial.
*     keine Fortschreibung MM in Finanzdispo, daher sind alle
*     vorerfaßten Belege zu berücksichtigen
    else.
      if  vbkpf-bstat = 'V'
      and vbkpf-xprfg = 'X'.
*       -> vollständig gesicherter vorerfasster Beleg. Da dann das
*       Bestellobligo bereits abgebaut ist, muß der vorerfasste Beleg
*       im CM stehen -> ggf. Beleg aus bbtab rauswerfen
        loop at bbtab
             where bukrs = vbkpf-bukrs
             and   belnr = vbkpf-belnr
             and   gjahr = vbkpf-gjahr.
          delete bbtab.
          exit.   "kann nur 1 Eintrag sein nach obiger append-Logik
        endloop.
      else.
        select belnr from rseg into rseg-belnr up to 1 rows
               where belnr = vbkpf-awkey(10)        "Belegnr.
               and   gjahr = vbkpf-awkey+10(4).     "Geschäftsjahr
        endselect.
        if  sy-subrc = 0.   "vorerfaßter Beleg hat Bestellbezug
*       and rseg-belnr <> space.   "trifft dann immer zu
*         Beleg muss übergangen werden, da bereits über MM-Schnittstelle
*         in Finanzdispo fortgeschrieben. Ggf. werden FD-Felder
*         initialisiert
          move-corresponding vbkpf to bbtab.
          append bbtab.
        endif.
      endif.   "if  vbkpf-bstat = 'V' ...
    endif.   "if t001-xfdmm is initial
  endselect.

  sort bbtab.
  delete adjacent duplicates from bbtab.

*---------------------------------------------------------------------*
* offene Debitorenposten                                              *
*---------------------------------------------------------------------*
  if 'D' in s_koart.
    clear wabukrs.
    wakoart = 'D'.
    veb-flag = '0'.                    "1-vorerfasster Beleg

    select * from bsid                 "Bib der offenen Debitoren-Posten
           where bukrs in r_bukrs
           and   bukrs in s_bukrs
           and   gsber in r_gsber
           and   kunnr in s_kunnr
           and   gjahr in s_gjahr
           and   belnr in s_belnr
           and   xarch =  space
*          Abfrage auf XPYPR = Space wäre falsch, da ggf. trotz
*          Zahlungsauftrag die FD-Felder in BSEG gefüllt werden, um
*          diese beim Löschen vom Zahlungsauftrag wieder verfügbar zu
*          haben
*     order by bukrs kunnr gjahr belnr buzei.
      order by primary key.            "verbessert Performance

*-----Ranges waren zu gross fuer Select: Ranges jetzt verproben------*
*-----(Beim Select mit FOR ALL ENTRIES zu arbeiten geht hier nicht,--*
*-----da dann ORDER BY nicht genutzt werden könnte)------------------*
      if bklines > maxrange.
        check: bsid-bukrs in r_bukrs2.
      endif.
      if gblines > maxrange.
        check: bsid-gsber in r_gsber2.
      endif.
      perform debitorzeile.
    endselect.

*---vorerfasste Belege bearbeiten
    veb-flag = '1'.                    "1-vorerfasster Beleg
    clear: bseg, bsid, bkpf.
    select * from vbsegd               "Belegsegment Vorerfassung Debit.
           where bukrs in r_bukrs
           and   bukrs in s_bukrs
           and   gsber in r_gsber
           and   kunnr in s_kunnr
           and   gjahr in s_gjahr
           and   belnr in s_belnr
      order by bukrs kunnr gjahr belnr buzei.

*-----Ranges waren zu gross fuer Select: Ranges jetzt verproben------*
      if bklines > maxrange.
        check: vbsegd-bukrs in r_bukrs2.
      endif.
      if gblines > maxrange.
        check: vbsegd-gsber in r_gsber2.
      endif.

      check: vbsegd-kunnr <> space.
      clear: bseg, bsid.
      move-corresponding vbsegd to bseg.
      move-corresponding vbsegd to bsid.
      bseg-xopvw = 'X'.                "fehlt in vbsegd
      bseg-koart = 'D'.
      if bseg-skfbt is initial.   "kann bei vorerfassten Belegen fehlen
        bseg-skfbt = bseg-wrbtr.
        bsid-skfbt = bseg-wrbtr.
      endif.
      if bseg-dmbtr is initial.
        bseg-dmbtr = bseg-wrbtr.
        bsid-dmbtr = bseg-wrbtr.
      endif.

      perform debitorzeile.
    endselect.
  endif.

*---------------------------------------------------------------------*
* offene Kreditorenposten                                             *
*---------------------------------------------------------------------*
  if 'K' in s_koart.
    clear wabukrs.
    wakoart = 'K'.
    veb-flag = '0'.                    "1-vorerfasster Beleg
    select * from bsik   "Bib der offenen Kreditoren-Posten
           where bukrs in r_bukrs
           and   bukrs in s_bukrs
           and   gsber in r_gsber
           and   lifnr in s_lifnr
           and   gjahr in s_gjahr
           and   belnr in s_belnr
           and   xarch =  space
*          Abfrage auf XPYPR = Space wäre falsch, da ggf. trotz
*          Zahlungsauftrag die FD-Felder in BSEG gefüllt werden, um
*          diese beim Löschen vom Zahlungsauftrag wieder verfügbar zu
*          haben
*     order by bukrs lifnr gjahr belnr buzei.
      order by primary key.            "verbessert Performance

*-----Ranges waren zu gross fuer Select: Ranges jetzt verproben------*
      if bklines > maxrange.
        check: bsik-bukrs in r_bukrs2.
      endif.
      if gblines > maxrange.
        check: bsik-gsber in r_gsber2.
      endif.
      perform kreditorzeile.
    endselect.

*---vorerfasste Belege bearbeiten
    veb-flag = '1'.                    "1-vorerfasster Beleg
    clear: bseg, bsik, bkpf.
    select * from vbsegk             "Belegsegment Vorerfassung Kredit.
           where bukrs in r_bukrs
           and   bukrs in s_bukrs
           and   gsber in r_gsber
           and   lifnr in s_lifnr
           and   gjahr in s_gjahr
           and   belnr in s_belnr
      order by bukrs lifnr gjahr belnr buzei.

*-----Ranges waren zu gross fuer Select: Ranges jetzt verproben------*
      if bklines > maxrange.
        check: vbsegk-bukrs in r_bukrs2.
      endif.
      if gblines > maxrange.
        check: vbsegk-gsber in r_gsber2.
      endif.

      check: vbsegk-lifnr <> space.
      clear: bseg, bsik.
      move-corresponding vbsegk to bseg.
      move-corresponding vbsegk to bsik.
      bseg-xopvw = 'X'.                "fehlt in vbsegk
      bseg-koart = 'K'.
      if bseg-skfbt is initial.   "kann bei vorerfassten Belegen fehlen
        bseg-skfbt = bseg-wrbtr.
        bsik-skfbt = bseg-wrbtr.
      endif.
      if bseg-dmbtr is initial.
        bseg-dmbtr = bseg-wrbtr.
        bsik-dmbtr = bseg-wrbtr.
      endif.

      perform kreditorzeile.
    endselect.
  endif.

*---------------------------------------------------------------------*
* offene Posten der kontokorrent gefuehrten Sachkonten                *
*---------------------------------------------------------------------*
  if 'S' in s_koart.
    clear wabukrs.
    wakoart = 'S'.
    veb-flag = '0'.                    "1-vorerfasster Beleg

*   BSIS = Bib der offenen Sachkonten-Posten; dennoch wird anschließend
*   zusätzlich geprüft, ob im Moment im Sachkontostamm die OP-Verwaltung
*   gesetzt ist: falls nein, wird das Konto nicht verarbeitet, da die
*   Beträge ggf. durch den RFFUEB00 bereits saldenmäßig aufgebaut wurden
    select * from bsis   "Bib der offenen Sachkonten-Posten
           where bukrs in r_bukrs
           and   bukrs in s_bukrs
           and   gsber in r_gsber
           and   hkont in s_saknr
           and   gjahr in s_gjahr
           and   belnr in s_belnr
           and   xarch =  space
*     order by bukrs hkont gjahr belnr buzei.
      order by primary key.            "verbessert Performance

*-----Ranges waren zu gross fuer Select: Ranges jetzt verproben------*
      if bklines > maxrange.
        check: bsis-bukrs in r_bukrs2.
      endif.
      if gblines > maxrange.
        check: bsis-gsber in r_gsber2.
      endif.

      perform sachkontozeile.
    endselect.

*---vorerfasste Belege bearbeiten
    veb-flag = '1'.                    "1-vorerfasster Beleg
    clear: bseg, bsis, bkpf.
    select * from vbsegs          "Belegsegment Vorerfassung Sachkonten
           where bukrs in r_bukrs
           and   bukrs in s_bukrs
           and   gsber in r_gsber
*          SAKNR statt HKONT, da HKONT in Struktur VBSEGS fehlt
           and   saknr in s_saknr
           and   gjahr in s_gjahr
           and   belnr in s_belnr
      order by bukrs saknr gjahr belnr buzei.

*-----Ranges waren zu gross fuer Select: Ranges jetzt verproben------*
      if bklines > maxrange.
        check: vbsegs-bukrs in r_bukrs2.
      endif.
      if gblines > maxrange.
        check: vbsegs-gsber in r_gsber2.
      endif.

      check: vbsegs-saknr <> space.
      clear: bseg, bsis.
      move-corresponding vbsegs to bseg.
      move-corresponding vbsegs to bsis.
      bseg-hkont = vbsegs-saknr.
      bsis-hkont = vbsegs-saknr.
*     kann hier immer auf 'X' gesetzt werden, da nicht-OP-geführte
*     Konten vom RFFDKU00 nicht bearbeitet werden
      bseg-xopvw = 'X'.                "fehlt in vbsegs
      bseg-koart = 'S'.
      if bseg-dmbtr is initial.
        bseg-dmbtr = bseg-wrbtr.
        bsis-dmbtr = bseg-wrbtr.
      endif.

      perform sachkontozeile.
    endselect.
  endif.


end-of-selection.
  sort.

* Die Fortschreibung erfolgt in 5 Schritten. Im ersten Loop werden
* die Payment Requests, im zweiten Loop die BSEG bearbeitet. Findet
* der erste Schritt statt (weil Payment Request zu der Belegzeile
* vorhanden ist), werden im zweiten Schritt die BSEG-FD-Felder
* initialisiert und ansonsten nichts in FD fortgeschrieben.
* Bei der Bearbeitung der Payment Requests wird geprüft, ob Gründe
* vorliegen, die die FD-Fortschreibung verhindern. Dies sind:
* - der vorerfaßte Beleg steht in der Bestellbezugstabelle bbtab
*   (siehe Doku zu dieser Tabelle)
* - ermitteltes Sachkonto ist nicht kontokorrent -> Fortschreibung hat
*   durch RFFUEB00 zu erfolgen
* Ist der Payment Request für FD relevant, so wird über den FB
* Cash_Forecast_Pr_Post sowohl FDZA wie auch FDSB bzw. FDSR fortge-
* schrieben. Somit darf dann im zweiten Loop nichts mehr in die FD
* fortgeschrieben werden.

* Im dritten Schritt werden dann die Payment Requests bearbeitet, die
* bei der Buchung auf das Treasury-Verrechnungskonto erzeugt wurden.
* Wird in einem separaten Schritt durchgeführt, da sonst Probleme
* bei der Abgrenzung von BSIS gegen S_SAKNR und außerdem würde erkannt
* werden, daß das TR.Verr.kto nicht FD-relevant ist und die Auswertung
* der dazugehörigen Payment Requests würde nicht erfolgen.

* Im vierten Schritt werden Zahlungsaufträge bearbeitet.

* Im fünften Schritt wird das Vertragskontokorrent (FI-CA) bearbeitet

* Hinweis zum Parameter p_restr bzw. Feld waxfdok:
* Wird ein Satz gemäß diesem Parameter nicht gewünscht, dann gilt:
* - es werden keine Summensätze in FDSB bzw. FDSR fortgeschrieben
* - dem FB Cash_Forecast_Pr_Post werden diese Sätze nicht übergeben,
*   so daß dieser auch keine Sätze in FDZA schreibt
* - es findet kein Update auf bseg, vbsegd, vbsegk, vbsegs statt, außer
*   der Update besteht im Initialisieren der FD-Felder (mit einer
*   Ausnahme, die gesondert dokumentiert ist)
* - es findet kein Update von pyordh statt (wäre aber unkritisch, wenn
*   es durchgeführt würde, aber vom Coding her ist es leichter, es
*   nicht zu tun)

*--- erster Schritt (Payment Requests bearbeiten)
  refresh: itab_cpayrq.
  loop.   "erster Loop über den Zwischendatenbestand
    check: pr-flag = '1'.              "1 - Payment Request
*   Im Loop stehen nur die Payment Requests drin, die zu einer Beleg-
*   zeile eines Beleges oder vorerfaßten Beleges gehören

*   Da außerhalb vom Loop über den Zwischendatenbestand veb-flag nicht
*   mehr bekannt ist, muß die Verprobung gegen die Tabelle bbtab hier
*   erfolgen.
    if veb-flag = '1'.                 "1 - vorerfaßter Beleg
      loop at bbtab
           where bukrs = itab_cpayrq-bukrs
           and   belnr = itab_cpayrq-belnr
           and   gjahr = itab_cpayrq-gjahr.
        exit.
      endloop.
      if sy-subrc = 0.
*       hier nützt das Initialisieren der FD-Felder nichts, da diese
*       erst anschließend aufgebaut werden -> Satz übergehen
      else.
        itab_cpayrq = icpayrq.
        append itab_cpayrq.
      endif.
    else.
      itab_cpayrq = icpayrq.
      append itab_cpayrq.
    endif.
  endloop.   "erster Loop über den Zwischendatenbestand

* In itab_cpayrq noch die Payment Requests ohne Belegnummer aufnehmen,
* da diese nicht im Zwischendatenbestand sind.
* Payment Requests für Sachkonten, bei denen die Belegnummer leer, aber
* das Ausgleichsdatum gefüllt ist (tritt bei Banküberträgen auf - vgl.
* Doku in Cash_Forecast_Pr_Check), sind nicht relevant, da die
* FI-Buchung bereits erfolgt ist. Diese Payment Request sind aber auch
* nicht in itab_pr, weil bei Select auf PAYRQ nur die Sätze mit
* leerem Ausgleichsdatum selektiert wurden und Bankkontoüberträge
* sowieso nicht selektiert wurden.

  loop at itab_pr
       where ibelnr = space.   "immer identisch wie itab_pr-belnr
    move-corresponding itab_pr to itab_cpayrq.
    append itab_cpayrq.
  endloop.

* FB reichert itab_cpayrq mit FD-Daten an
* (Payment Requests von Bankkontoüberträgen können nicht in
* itab_cpayrq sein, wurden bereits bei der Selektion auf PAYRQ entfernt)
  call function 'CASH_FORECAST_PR_CHECK'
       tables
            tab_cpayrq_new = itab_cpayrq.
*      exceptions
*           others                 = 1.

  loop at itab_cpayrq.
    if itab_cpayrq-fdlev = space.
*     Payment Request ist für FD nicht relevant. Im zweiten Schritt
*     findet aber dennoch das Initialisieren der BSEG-FD-Daten statt.
    else.
*     notwendige Verprobungen durchführen (s.o.) und ggf. die Sätze
*     aus itab_cpayrq wieder löschen, z.B. wenn das Konto nicht konto-
*     korrent geführt wird.
      if itab_cpayrq-fdkto <> space.
*       FD-Fortschreibung auf Sachkonto -> FD-Fortschreibung darf nur
*       bei kontokorrente Sachkonten erfolgen, da die nicht
*       kontokorrenten vom RFFUEB00 aufgebaut werden.

*       bestimme relevanten Buchungskreis (falls zahlender Buchungs-
*       kreis im Spiel ist)
        perform bestimme_relevanten_bk
                using    zbukrs-flag
                         itab_cpayrq-zbukr
                         itab_cpayrq-ggrup
                         itab_cpayrq-fdkto
                         itab_cpayrq-bukrs
                changing warelbk.      "relevanter BK

        loop at itab_skb1
             where saknr = itab_cpayrq-fdkto
             and   bukrs = warelbk.
          exit.
        endloop.
        if sy-subrc <> 0.
          select single * from skb1
                 where saknr = itab_cpayrq-fdkto
                 and   bukrs = warelbk.
          if sy-subrc <> 0.
*           kann nicht sein, da FB Cash_Forecast_Pr_Check SKB1 lesen
*           konnte
            raise e_invalid_skb1.      "raise ohne exception -> dump
          endif.
          move-corresponding skb1 to itab_skb1.
          append itab_skb1.
        endif.
        if itab_skb1-xopvw <> 'X'.     "X - kontokorrentes Konto
*         FD-Felder löschen ginge auch
          delete itab_cpayrq.
        else.
*         Einschränkung gemäß Parameter p_restr auswerten
          call function 'CASH_FORECAST_BUILD_UP_RESTRIC'
               exporting
                    i_fdbuildup     = p_restr
                    i_koart         = 'S'
                    i_saknr_xopvw   = itab_skb1-xopvw
               importing
                    e_x_build_up_cm = waxfdok.

          if waxfdok <> 'X'.           "X - FD-Fortschreibung gewünscht
            delete itab_cpayrq.
          endif.
        endif.
      elseif itab_cpayrq-fdgrp <> space.
*       Einschränkung gemäß Parameter p_restr auswerten
        call function 'CASH_FORECAST_BUILD_UP_RESTRIC'
             exporting
                  i_fdbuildup         = p_restr
                  i_koart             = itab_cpayrq-koart   "D/K
*                 i_saknr_xopvw       =
             importing
                  e_x_build_up_cm     = waxfdok.

        if waxfdok <> 'X'.             "X - FD-Fortschreibung gewünscht
          delete itab_cpayrq.
        endif.

      endif.                           "if itab_cpayrq-fdkto <> space
    endif.
  endloop.                             "loop at itab_cpayrq

  if p_test = space.
    cntr = 0.
    refresh: tab_cpayrq.
    loop at itab_cpayrq
         where fdlev <> space.
      cntr = cntr + 1.
      tab_cpayrq = itab_cpayrq.
      tab_cpayrq-modus = 'I'.          "'I' - Insert
      append tab_cpayrq.

      if cntr = 250.
        cntr = 0.
*       Falls es zu Abbrüchen kommt, weil mehrere Verbucher parallel
*       arbeiten -> IN UPDATE TASK entfernen

*       FB schreibt FD-Daten fort, d.h. FDZA, FDSB und FDSR
        call function 'CASH_FORECAST_PR_POST' in update task
             tables
                  tab_cpayrq_new = tab_cpayrq.
*                 TAB_CPAYRQ_OLD =
*            exceptions
*                 others         = 1.

        commit work.
        refresh: tab_cpayrq.
      endif.

    endloop.

*   nicht bei at last im Loop, da loop at .. where ..
    if cntr > 0.
*     FB schreibt FD-Daten fort, d.h. FDZA, FDSB und FDSR
      call function 'CASH_FORECAST_PR_POST' in update task
           tables
                tab_cpayrq_new = tab_cpayrq.
*               TAB_CPAYRQ_OLD =
*          exceptions
*               others         = 1.

      commit work.
      refresh: tab_cpayrq.
    endif.
  endif.


*--- zweiter Schritt
*--- (BSEG-FD-Felder initialisieren, sofern Payment Requests vorhanden
*---  waren. Ansonsten ggf. BSEG und FDSB und FDSR fortschreiben)
  loop.   "zweiter Loop über den Zwischendatenbestand
*   kann im Testsystem aktiviert werden, damit sehr grosse Betraege
*   nicht die FF70 Anzeige ruinieren
*   CHECK: BSEG-FDWBT < 1000000 AND BSEG-FDWBT > -1000000.  "QQ
*   CHECK: BSEG-DMBTR < 1000000.                            "QQ

    cntr = 0.
    if p_test = space.
      if  bseg-fdgrp = olddata-fdgrp
      and bseg-fdlev = olddata-fdlev
      and bseg-fdtag = olddata-fdtag
      and bseg-fdwbt = olddata-fdwbt.
*       Finanzdispodaten haben sich nicht geaendert -> kein Belegzeilen-
*       Update, nur Summensatzfortschreibung, aber auch nur dann, wenn
*       Fortschreibung gemäß Parameter p_restr relevant ist
        call function 'CASH_FORECAST_BUILD_UP_RESTRIC'
             exporting
                  i_fdbuildup         = p_restr
                  i_koart             = bseg-koart
*                 Nur für Koart 'S' relevant und dann ist es hier
*                 immer 'X', da nur solche Belegzeilen
*                 vom RFFDKU00 bearbeitet werden
                  i_saknr_xopvw       = 'X'
             importing
                  e_x_build_up_cm     = waxfdok.

      else.
*       Belegzeile updaten
        if veb-flag = '1'.             "1-vorerfasster Beleg
          case bseg-koart.
            when 'D'.
              if vbsegd-fdlev = space.
*               FD-Daten werden initialisiert, z.B. weil die Fort-
*               schreibung der FD-Daten über Payment Request erfolgt.
*               Dies wird hier immer durchgeführt, selbst wenn gemäß
*               Parameter p_restr die Belegzeile nicht relevant ist.
*               Sonst würde es zu Problemen führen, wenn danach die
*               Belegzeile geändert oder ausgeziffert wird -> in der
*               Finanzdispo würde der Betrag mit umgedrehtem Vorzeichen
*               fortgeschrieben werden.
                waxfdok = 'X'.
              else.
*               Fortschreibung nur, wenn Belegzeile gemäß
*               Parameter p_restr relevant ist
                call function 'CASH_FORECAST_BUILD_UP_RESTRIC'
                     exporting
                          i_fdbuildup     = p_restr
                          i_koart         = 'D'
                     importing
                          e_x_build_up_cm = waxfdok.
              endif.

              if waxfdok = 'X'.
                update vbsegd
                  set   fdgrp = bseg-fdgrp
                        fdlev = bseg-fdlev
                        fdtag = bseg-fdtag
                        fdwbt = bseg-fdwbt
                  where bukrs = bseg-bukrs
                  and   belnr = bseg-belnr
                  and   gjahr = bseg-gjahr
                  and   buzei = bseg-buzei.
              endif.

            when 'K'.
              if vbsegk-fdlev = space.
*               siehe oben
                waxfdok = 'X'.
              else.
                call function 'CASH_FORECAST_BUILD_UP_RESTRIC'
                     exporting
                          i_fdbuildup     = p_restr
                          i_koart         = 'K'
                     importing
                          e_x_build_up_cm = waxfdok.
              endif.

              if waxfdok = 'X'.
                update vbsegk
                  set   fdgrp = bseg-fdgrp
                        fdlev = bseg-fdlev
                        fdtag = bseg-fdtag
                        fdwbt = bseg-fdwbt
                  where bukrs = bseg-bukrs
                  and   belnr = bseg-belnr
                  and   gjahr = bseg-gjahr
                  and   buzei = bseg-buzei.
              endif.

            when 'S'.
              if vbsegs-fdlev = space.
*               siehe oben
                waxfdok = 'X'.
              else.
                call function 'CASH_FORECAST_BUILD_UP_RESTRIC'
                     exporting
                          i_fdbuildup         = p_restr
                          i_koart             = 'S'
*                         ist hier immer 'X', da nur solche Belegzeilen
*                         vom RFFDKU00 bearbeitet werden
                          i_saknr_xopvw       = 'X'
                     importing
                          e_x_build_up_cm     = waxfdok.
              endif.

              if waxfdok = 'X'.
                update vbsegs
                  set   fdgrp = bseg-fdgrp
                        fdlev = bseg-fdlev
                        fdtag = bseg-fdtag
                        fdwbt = bseg-fdwbt
                  where bukrs = bseg-bukrs
                  and   belnr = bseg-belnr
                  and   gjahr = bseg-gjahr
                  and   buzei = bseg-buzei.
              endif.
          endcase.

        else.
          if bseg-fdlev = space.
*           siehe oben
            waxfdok = 'X'.
          else.
            call function 'CASH_FORECAST_BUILD_UP_RESTRIC'
                 exporting
                      i_fdbuildup         = p_restr
                      i_koart             = bseg-koart
*                     nur für bseg-koart 'S' relevant und dann
*                     ist es hier immer 'X', da nur solche Belegzeilen
*                     vom RFFDKU00 bearbeitet werden
                      i_saknr_xopvw       = 'X'
                 importing
                      e_x_build_up_cm     = waxfdok.
          endif.

          if waxfdok = 'X'.
            update bseg
              set   fdgrp = bseg-fdgrp
                    fdlev = bseg-fdlev
                    fdtag = bseg-fdtag
                    fdwbt = bseg-fdwbt
              where bukrs = bseg-bukrs
              and   belnr = bseg-belnr
              and   gjahr = bseg-gjahr
              and   buzei = bseg-buzei.
          endif.
        endif.

        if sy-subrc <> 0.
          perform protokoll using '300'.
        endif.

        if waxfdok = 'X'.              "-> Update wurde durchgeführt
          cntr2 = cntr2 + 1.
        endif.

        if cntr2 = 250.
          cntr2 = 0.
          commit work.
        endif.
      endif.   "if bseg-fdgrp = olddata-fdgrp and ...

      if  bseg-fdgrp is initial
      and bseg-fdlev is initial
      and bseg-fdtag is initial
      and bseg-fdwbt is initial.
*       Sonderfall: nur BSEG-Felder mussten zurueckgesetzt werden,
*       da frueher einmal im Stammsatz die Gruppe bzw. Ebene gepflegt
*       war.
      elseif waxfdok <> 'X'.
*       Belegzeile gemäß Parameter p_restr nicht relevant -> oben wurde
*       kein Update auf bseg bzw. vbsegd/vbsegk/vbsegs durchgeführt
*       und jetzt darf auch keine Summensatzfortschreibung erfolgen
      elseif bseg-xpypr = 'X'.
*       für die Belegzeile gibt es einen Zahlungsauftrag -> diese werden
*       später separat behandelt und schreiben FDSB fort -> hier keine
*       Summensatzfortschreibung. Die FD-Felder in BSEG wurden oben
*       trotz Zahlungsauftrag gesetzt, damit diese beim Löschen des
*       Zahlungsauftrags wieder zur Verfügung stehen
      else.
*       Verbuchung in Summensatzdateien FDSB und FDSR vorbereiten
*       ueber VBTAB
        clear vbtab.
        move-corresponding bseg to vbtab.

        if bseg-koart = 'S'.
*         nicht skb1-waers da Konto OP-verwaltet wird und diese Konten
*         werden ab 4.5A in Transaktionswährung fortgeschrieben
          vbtab-curra = bkpf-waers.    "Belegkopfwährung
        else.
*         vbtab-currd = bseg-pswsl.    "Transaktionswaehrung
          if abw_zahlwaehrung <> space.
            vbtab-currd = abw_zahlwaehrung.   "abweichende Zahlungswähr.
          else.
            vbtab-currd = bkpf-waers.  "Belegkopfwährung
          endif.
        endif.
        collect vbtab.
        cntr = cntr + 1.

        if cntr = 250.
          cntr = 0.
*         Falls es zu Abbrüchen kommt, weil mehrere Verbucher parallel
*         arbeiten -> IN UPDATE TASK entfernen, wegen folgendem Problem:
*         Geht der Insert schief, wird hart abgebrochen und der
*         zweite Update-Versuch wird nicht mehr durchgeführt. Statt
*         Update und Insert Modify zu nehmen, geht nicht, da beim
*         Modify die SET-Variante nicht möglich ist
*         CALL FUNCTION 'CASH_FORECAST_SUMMARY_REC_UPD'
          call function 'CASH_FORECAST_SUMMARY_REC_UPD' in update task
               tables
                    tab_rf40s = vbtab.
          commit work.
          refresh vbtab.
        endif.
      endif.                           "if bseg-fdgrp is initial and ...
    endif.                             "if p_test = space

    at last.
      if p_test = space.
*       siehe Doku oben
*       CALL FUNCTION 'CASH_FORECAST_SUMMARY_REC_UPD'
        call function 'CASH_FORECAST_SUMMARY_REC_UPD' in update task
             tables
                  tab_rf40s = vbtab.
        commit work.
        refresh vbtab.
      endif.
    endat.
  endloop.   "zweiter Loop über den Zwischendatenbestand

*--- dritter Schritt: Payment Requests berücksichtigen, die bei der
*--- Buchung auf das Treasury-Verr.konto erzeugt wurden. Diese werden
*--- über den Payment Request und nicht über die Buchung in FD fort-
*--- geschrieben.
  if  p_test = space
  and 'S' in s_koart.

    clear   itab_cpayrq.
    refresh itab_cpayrq.

*   hier darf nicht gegen s_saknr oder gegen payrq-ubhkt verprobt werden
*   da nur das Konto relevant ist, das FD ermittelt -> dieses muß
*   dann gegen s_saknr geprüft werden

    select * from atpra    "Treasury: Verr.konto für Zahlungsanordnungen
           where rprac <> space.       "Tr.Verr.kto gefüllt
      check: atpra-bukrs in r_bukrs.
      check: atpra-bukrs in r_bukrs2.
*     check: atpra-bukrs in s_bukrs.   "unnötig, in oben enthalten

      select * from payrq
             where keyno in s_prkey
*            Belnr Space wurde bereits oben berücksichtigt
             and   belnr <> space
             and   belnr in s_belnr
*            Payment Request wurde noch nicht bezahlt
             and   augdt = '00000000'
*            Tr.Verr.kto
             and   hkont = atpra-rprac
             and   bukrs = atpra-bukrs
             and ( koart = 'S' or koart = space )
             and   gjahr in s_gjahr.

        check: payrq-gsber in r_gsber.
        check: payrq-gsber in r_gsber2.

*       vgl. Doku beim vorigen Select auf payrq
        if  payrq-zbukr <> space
        and payrq-ggrup <> space.
*         -> Bankkontoübertrag
          check: 1 = 2.
        endif.

        move-corresponding payrq to itab_cpayrq.
        append itab_cpayrq.
      endselect.
    endselect.

*   FB reichert itab_cpayrq mit FD-Daten an
    call function 'CASH_FORECAST_PR_CHECK'
         tables
              tab_cpayrq_new = itab_cpayrq.
*        exceptions
*             others                 = 1.

    loop at itab_cpayrq.
*     notwendige Verprobungen durchführen (s.o.) und ggf. die Sätze
*     aus itab_cpayrq wieder löschen, z.B. wenn das Konto nicht konto-
*     korrent geführt wird.
      if  itab_cpayrq-fdlev <> space
      and itab_cpayrq-fdkto <> space
      and itab_cpayrq-fdkto in s_saknr.
*       FD-Fortschreibung auf Sachkonto -> FD-Fortschreibung darf nur
*       bei kontokorrente Sachkonten erfolgen, da die nicht
*       kontokorrenten vom RFFUEB00 aufgebaut werden.

*       bestimme relevanten Buchungskreis (falls zahlender Buchungs-
*       kreis im Spiel ist)
        perform bestimme_relevanten_bk
                using    zbukrs-flag
                         itab_cpayrq-zbukr
                         itab_cpayrq-ggrup
                         itab_cpayrq-fdkto
                         itab_cpayrq-bukrs
                changing warelbk.      "relevanter BK

        loop at itab_skb1
             where saknr = itab_cpayrq-fdkto
             and   bukrs = warelbk.
          exit.
        endloop.
        if sy-subrc <> 0.
          select single * from skb1
                 where saknr = itab_cpayrq-fdkto
                 and   bukrs = warelbk.
          if sy-subrc <> 0.
*           kann nicht sein, da FB Cash_Forecast_Pr_Check SKB1 lesen
*           konnte
            raise e_invalid_skb1.      "raise ohne exception -> dump
          endif.
          move-corresponding skb1 to itab_skb1.
          append itab_skb1.
        endif.
        if itab_skb1-xopvw <> 'X'.     "X - kontokorrentes Konto
          delete itab_cpayrq.
        else.
*         Einschränkung gemäß Parameter p_restr auswerten
          call function 'CASH_FORECAST_BUILD_UP_RESTRIC'
               exporting
                    i_fdbuildup     = p_restr
                    i_koart         = 'S'
                    i_saknr_xopvw   = itab_skb1-xopvw
               importing
                    e_x_build_up_cm = waxfdok.

          if waxfdok <> 'X'.           "X - FD-Fortschreibung gewünscht
            delete itab_cpayrq.
          endif.
        endif.
      else.
        delete itab_cpayrq.
      endif.                           "if itab_cpayrq-fdlev <> space...
    endloop.                           "loop at itab_cpayrq

    refresh: tab_cpayrq.
    loop at itab_cpayrq.
*     in itab_cpayrq stehen nur noch die relevanten Einträge
      tab_cpayrq = itab_cpayrq.
      tab_cpayrq-modus = 'I'.          "'I' - Insert
      append tab_cpayrq.
    endloop.

*   Falls es zu Abbrüchen kommt, weil mehrere Verbucher parallel
*   arbeiten -> IN UPDATE TASK entfernen

*   FB schreibt FD-Daten fort, d.h. FDZA, FDSB und FDSR
*   Da es sich nur um wenige Einträge handeln kann, braucht der
*   Update nicht portioniert werden
    call function 'CASH_FORECAST_PR_POST' in update task
         tables
              tab_cpayrq_new = tab_cpayrq.
*             TAB_CPAYRQ_OLD =
*        exceptions
*             others         = 1.

    commit work.
    refresh: tab_cpayrq.

*   sollte in der BSEG-Zeile vom Tr.verr.konto die FD-Daten gesetzt
*   sein, so werden diese zurückgesetzt.
    loop at itab_cpayrq.
*     in itab_cpayrq stehen nur noch die relevanten Einträge
*     (falls hier itab_cpayrq wegen Parameter p_restr oben gelöscht
*     wurde, so wird hier kein Update durchgeführt im Gegensatz zu
*     den sonstigen Updates auf bseg bzw. bsegd/bsegk/bsegs in diesem
*     Report. Aber das sollte hier (hoffentlich) unkritisch sein)
      update bseg
             set fdlev  = space
                 fdgrp  = space
                 fdwbt  = 0
                 fdtag  = '00000000'
             where bukrs = itab_cpayrq-bukrs
             and   belnr = itab_cpayrq-belnr
             and   gjahr = itab_cpayrq-gjahr
             and   buzei = itab_cpayrq-buzei.
    endloop.
*   Können nicht viele Einträge sein, da das Zahlungsprogramm die
*   Payment Requests regelmäßig bezahlen wird -> portionieren nicht
*   nötig.
    commit work.
  endif.                               "if  p_test <> space ...


*--- vierter Schritt: Zahlungsaufträge berücksichtigen, die
*--- OP-verwaltete Sachkonten betreffen.

* Zahlungsaufträge bearbeiten ohne Berücksichtigung von Geschäftsjahr,
* da dieses in PYORDH nicht bekannt ist.

* Ist zahlender Buchungskreis im Spiel, wird dieser gegen die Abgrenzun-
* gen geprüft, d.h. der ursprüngliche Buchungskreis spielt keine Rolle

* Gibt es für eine Personenkontenzeile einen Zahlungsauftrag, so ist das
* Feld bseg-xpypr gefüllt, die FD-Felder aus BSEG werden aber nicht
* gelöscht, sondern wurden oben bereits gesetzt, um bei Rücknahme
* eines Zahlungsauftrags die FD-Informationen wieder aus dem BSEG
* verfügbar zu haben, um FD wieder auf dem Personenkonto
* fortschreiben zu können.
* Im Zahlungsauftrag sind die Felder BNKKO, CURRA und FDLEV nur dann
* gefüllt, wenn der Kunde im Customizing zum zahlenden Buchungskreis
* für die Hausbank, den Zahlweg und die Währung ein Bank(verr.)konto
* hinterlegt hat (das ist nicht zwingend). Gibt es aber einen
* Zahlungsauftrag, dann wird generell die Personenkontenzeile nicht
* mehr in die Finanzdispo fortgeschrieben, egal ob der Zahlungsauftrag
* in die FD fortgeschrieben wird oder nicht.

  if  p_test = space
  and 'S' in s_koart.

    select * from pyordh
           where pyord in s_pyord
           and   zbukr in s_bukrs
           and   zbukr in r_bukrs
           and   bnkko in s_saknr
           and   bnkko <> space
           order by zbukr bnkko.

*-----Ranges waren zu gross fuer Select: Ranges jetzt verproben------*
*-----(Beim Select mit FOR ALL ENTRIES zu arbeiten geht hier nicht,--*
*-----da dann ORDER BY nicht genutzt werden könnte)------------------*
      if bklines > maxrange.
        check: pyordh-zbukr in r_bukrs2.
      endif.

      if  itab_skb1-bukrs = pyordh-zbukr
      and itab_skb1-saknr = pyordh-bnkko.
*       Tabelleneintrag schon gelesen
      else.
        loop at itab_skb1
             where bukrs = pyordh-zbukr
             and   saknr = pyordh-bnkko.
          exit.
        endloop.

        if sy-subrc <> 0.
          select single * from skb1
                 where saknr = pyordh-bnkko
                 and   bukrs = pyordh-zbukr.
          check: sy-subrc = 0.
          move-corresponding skb1 to itab_skb1.
          append itab_skb1.
        endif.
      endif.

      check: itab_skb1-xopvw = 'X'.    "nur OP-verwaltete Sachkonten
      check: itab_skb1-fdlev <> space. "Ebene im Stammsatz gesetzt

*     Einschränkung gemäß Parameter p_restr auswerten
      call function 'CASH_FORECAST_BUILD_UP_RESTRIC'
           exporting
                i_fdbuildup     = p_restr
                i_koart         = 'S'
                i_saknr_xopvw   = itab_skb1-xopvw
           importing
                e_x_build_up_cm = waxfdok.

      check: waxfdok = 'X'.            "X - FD-Fortschreibung gewünscht

*     Update auf pyordh vorbereiten, falls Ebene nicht identisch mit
*     Ebene im Stammsatz, z.B. weil im Stammsatz die Ebene gerade erst
*     gesetzt wurde, da die Finanzdispo gerade erst aktiviert wird
      if pyordh-fdlev <> itab_skb1-fdlev.
        pyordh-fdlev    = itab_skb1-fdlev.
        t_pyordh_update = pyordh.
        append t_pyordh_update.
      endif.

*     vgl. Füllen von xf40s im FB 'PYORD_INSERT'
      clear: vbtab.
      vbtab-bukrs = pyordh-zbukr.
      vbtab-koart = 'S'.
      vbtab-hkont = pyordh-bnkko.
      vbtab-fdlev = itab_skb1-fdlev.   "identisch wie pyordh-fdlev
      vbtab-curra = pyordh-waers.      "Zahlungswährung
      vbtab-fdtag = pyordh-valut.

*     (vgl. Coding in FB pyord_insert)
*     aus pyordh-rwbtr Zahlungsanforderungen (F-59) rausrechnen, da
*     diese nicht FD-relevant sind
      select * from pyordp into table t_pyordp
             where pyord = pyordh-pyord.
      clear: waamount.
      loop at t_pyordp.
        select single bukrs from bseg into bseg-bukrs
               where bukrs =  t_pyordp-bukrs   "vollständiger Key
               and   belnr =  t_pyordp-belnr
               and   gjahr =  t_pyordp-gjahr
               and   buzei =  t_pyordp-buzei
               and   umskz <> space              "-> Zahlungsanforderung
               and   rebzt =  'P'.
        if sy-subrc = 0.
*         Zahlungsanforderung rausrechnen
*         In pyordp sind die Beträge immer positiv
          if pyordh-rwbtr < 0.
            waamount = waamount + t_pyordp-wrbtr - t_pyordp-wskto.
          else.
            waamount = waamount - t_pyordp-wrbtr + t_pyordp-wskto.
          endif.
        endif.
      endloop.   "at t_pyordp

      if waamount <> 0.
        pyordh-rwbtr = pyordh-rwbtr + waamount.
      endif.

      check: pyordh-rwbtr <> 0.
*     Fortschreibung in Zahlungswährung da Zahlungsauftrag
      vbtab-fdwbt = pyordh-rwbtr.      "Betrag des ZA in Zahlwährung
      append vbtab.
    endselect.

    describe table vbtab lines cntr.
    if cntr <> 0.
*     CALL FUNCTION 'CASH_FORECAST_SUMMARY_REC_UPD'
      call function 'CASH_FORECAST_SUMMARY_REC_UPD' in update task
           tables
                tab_rf40s = vbtab.
      commit work.
      refresh vbtab.
    endif.

*   ggf. Ebene in den Zahlungsaufträgen setzen gemäß Sachkontoebene
    loop at t_pyordh_update.
      update pyordh
             set   fdlev = t_pyordh_update-fdlev
             where pyord = t_pyordh_update-pyord.
    endloop.

    if sy-subrc = 0.
      commit work.
    endif.

  endif.                               "if  p_test = space...
*--- vierter Schritt Ende

*--- fünfter Schritt: Vertragskontokorrent (FI-CA) berücksichtigen
  if p_test = space.
    perform vertragskontokorrent.
  endif.                               "if p_test = space
*--- fünfter Schritt Ende


* Ausgabe außerhalb vom Loop über Zwischendatenbestand, damit die
* Ausgabe auch erfolgt, falls nur Payment Requests bearbeitet
* wurden
  if p_test = space.
    skip 2.
    uline.
    write: / text-001.                 "Ende Verarbeitung
  else.
    skip 2.
    uline.
    write: / text-002.                 "Ende Testlauf
  endif.


*---------------------------------------------------------------------*
* Unterroutinen in alphabetischer Reihenfolge                         *
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM BESTIMME_RELEVANTEN_BK                                   *
*---------------------------------------------------------------------*
*       bestimmt relevanten Buchungskreis (falls zahlender Buchungs-
*       kreis im Spiel ist)
*---------------------------------------------------------------------*
form bestimme_relevanten_bk
                using    u_zbukrs-flag    type c
                         u_payrq-zbukr    like t001-bukrs
                         u_payrq-ggrup    like payrq-ggrup
                         u_payrq-fdkto    like skb1-saknr
                         u_payrq-bukrs    like t001-bukrs
                changing c_warelbk        like t001-bukrs.

  data: l_s_t042 like t042.

  c_warelbk = u_payrq-bukrs.           "Defaultwert

  if u_zbukrs-flag = '1'.
*   zahlende Buchungskreise sind im Spiel

    if  u_payrq-zbukr <> space
    and u_payrq-ggrup <> space.
*     es handelt sich um einen Kontoübertrag, dort spielt der zahlende
*     BK keine Rolle -> kann hier aber nicht sein, da Bankkontoüberträge
*     bereits bei der Selektion auf PAYRQ entfernt wurden
      raise e_wrong_source_code.
    else.
*     es handelt sich NICHT um einen Kontoübertrag

      if u_payrq-fdkto <> space.
*       die Fortschreibung in der Finanzdispo erfolgt auf einem Sachkto.

*       zahlenden BK über T042 ermitteln
*       (gleiche Logik wie im FB CASH_FORECAST_PR_CHECK)
        call function 'FI_FC_GET_PARAMETERS_CC'
             exporting
                  i_bukrs         = u_payrq-bukrs
             importing
                  e_s_t042        = l_s_t042
             exceptions
                  entry_not_found = 1
                  others          = 2.

        if  sy-subrc = 0
        and l_s_t042-zbukr <> space
        and l_s_t042-zbukr <> u_payrq-bukrs.

          c_warelbk = l_s_t042-zbukr.  "zahlender BK

        endif.
      endif.
    endif.
  endif.
endform.


*---------------------------------------------------------------------*
*       FORM DATENERMITTLUNG                                          *
*---------------------------------------------------------------------*
form datenermittlung.
  data: l_fdwaers like fdsb-dispw.

  bseg-fdgrp = wagrupp.                "bei Sachkonten immer Space
  bseg-fdlev = waebene.   "bei Sachkonten gemaess Stammsatz, bei Deb/Kr.
                                       "gemaess T035 oder T074

* Ermittlung Finanzdispoebene und -datum
  call function 'CASH_FORECAST_LEVEL_AND_DATE'
       exporting
            i_bkpf          = bkpf
            i_bseg          = bseg
            i_t001          = t001
            payment_history = wazv
       importing
            e_bseg          = bseg
       exceptions
            not_found_t035  = 1
            not_found_t036  = 2
            not_found_t039  = 3
            not_found_t042  = 4
            wrong_level     = 5.

  if sy-subrc <> 0.
    nextkto-flag = '1'.
    case sy-subrc.
      when '1'.
        perform protokoll using '220'.
      when '2'.
        perform protokoll using '222'.
      when '3'.
        perform protokoll using '224'.
      when '4'.
        perform protokoll using '226'.
      when '5'.
        perform protokoll using '228'.
        perform protokoll using '229'.
    endcase.
    perform protokoll using '208'.     "weiter mit naechstem BK/Konto
    exit.
  endif.

* obiger FB hat bei SHB-Vorgaengen die Standardebene bzw. die gesperrte
* Ebene gesetzt -> wieder Ebene gemaess T074 setzen
* Ausgesternt: ab 4.6C berücksichtigt der obige FB die SHB-Vorgänge
* if bseg-umskz <> space.
*   bseg-fdlev = waebene.
* endif.

* Ermittlung Finanzdispobetrag
  if veb-flag = '1'.                   "1-vorerfasster Beleg
    call function 'CASH_FORECAST_AMOUNT'
         exporting
              i_bkpf     = bkpf
              i_bseg     = bseg
              i_t001     = t001
              i_xposting = space       "da vorerfasster Beleg
         importing
              e_bseg     = bseg
              e_fdwaers  = l_fdwaers.
  else.
    call function 'CASH_FORECAST_AMOUNT'
         exporting
              i_bkpf     = bkpf
              i_bseg     = bseg
              i_t001     = t001
              i_xposting = 'X'         "gebuchter Beleg
         importing
              e_bseg     = bseg
              e_fdwaers  = l_fdwaers.
  endif.

* ggf. abweichende Zahlungswährung berücksichtigen (dann ist bseg-fdwbt
* bereits korrekt gesetzt); wird mit extrahiert
  if  bseg-koart ca 'DK'
  and l_fdwaers <> space
  and l_fdwaers <> bkpf-waers.
    abw_zahlwaehrung = l_fdwaers.
  else.
    clear: abw_zahlwaehrung.
  endif.

  if  bseg-fdlev = space
  or  bseg-fdtag = '00000000'
  or  bseg-fdwbt = 0.
*   Finanzdispodaten nicht ermittelbar -> kein Update durchfuehren
    clear: abw_zahlwaehrung.
    exit.
  endif.
  formok-flag = '1'.
endform.                               "datenermittlung

*---------------------------------------------------------------------*
*       FORM D-BELEGLESEN                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form d-beleglesen.
* ggf. Belegkopf lesen
* (bei vorerfassten Belegen braucht kein neuer VBKPF gelesen werden, nur
*  weil sich das Key-Feld AUSBK geaendert hat. Es reicht, den ersten
*  passenden Belegkopf zu lesen, da Inhalt in allen identisch bis auf
*  dieses Feld)
  if  bkpf-bukrs = bsid-bukrs
  and bkpf-belnr = bsid-belnr
  and bkpf-gjahr = bsid-gjahr.
* ggf. alle Bibs bis zum naechsten Belegkopf ueberlesen
    if nextbkpf-flag = '1'.
      exit.
    endif.
  else.                                "neuen Belegkopf lesen
    nextbkpf-flag = '0'.

    if veb-flag = '1'.                 "1-vorerfasster Beleg
      select single * from vbkpf
             where ausbk = vbsegd-ausbk
             and   bukrs = bsid-bukrs
             and   belnr = bsid-belnr
             and   gjahr = bsid-gjahr.
      if sy-subrc   <> 0.
        select * from vbkpf
               where bukrs = bsid-bukrs
               and   belnr = bsid-belnr
               and   gjahr = bsid-gjahr.
          exit.
        endselect.
      endif.
    else.
      select single * from bkpf
             where bukrs = bsid-bukrs
             and   belnr = bsid-belnr
             and   gjahr = bsid-gjahr.
    endif.

    if sy-subrc   <> 0.
      nextbkpf-flag = '1'.
      perform protokoll using '212'.   "Belegkopf fehlt
      perform protokoll using '214'.   "weiter mit naechstem Beleg
      exit.
    endif.

    if veb-flag = '1'.
      clear: bkpf.
      move-corresponding vbkpf to bkpf.
    endif.

    if reset-flag <> '1'.
      if bkpf-bstat = space            "normaler Beleg
*     statistischer Beleg, z.B. Anz.anford.
      or ( bkpf-bstat = 'S' and bsid-umskz <> space )
      or bkpf-bstat = 'V'              "vorerfasster Beleg
      or bkpf-bstat = 'W'.             "vorerfasster Beleg
*       o.k.
      else.
        nextbkpf-flag = '1'.
        exit.
      endif.
    endif.
  endif.   "if bkpf-bukrs = bsid-bukrs and ...

* Belegzeile lesen
  if veb-flag = '1'.
*   bei vorerfassten Belegen ist Select auf VDBSEGD bereits erfolgt
  else.
    select single * from bseg
           where bukrs = bsid-bukrs
           and   belnr = bsid-belnr
           and   gjahr = bsid-gjahr
           and   buzei = bsid-buzei.
    if sy-subrc   <> 0.
      perform protokoll using '216'.   "Belegzeile fehlt
      perform protokoll using '218'.   "Belegzeile uebersprungen
      exit.
    endif.
  endif.

  if reset-flag <> '1'.
* diskontierte Wechsel sind fuer die Finanzdispo nicht relevant
*   if  bsid-umsks = 'W'
*   and bseg-wverw <> space.           "Wechselverwendung
*     exit.
*   endif.
  endif.
  formok-flag = '1'.
endform.                               "d-beleglesen


*---------------------------------------------------------------------*
*       FORM DEBITORZEILE                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form debitorzeile.
  clear: reset-flag.
  if deb-flag <> '1'.   "nur beim ersten Debitorposten moeglich
    deb-flag = '1'.
    skip 2.
    perform protokoll using '200'.     "Deb. werden bearbeitet
  endif.

  if wabukrs <> bsid-bukrs.            "BK hat gewechselt
    wabukrs = bsid-bukrs.
    perform protokoll using '234'.
  endif.

  clear olddata.
  if  knb1-bukrs = bsid-bukrs
  and knb1-kunnr = bsid-kunnr.
* ggf. alle Bibs bis zum naechsten BK- oder Kontowechsel ueberlesen
    if nextkto-flag = '1'.
      exit.
    endif.
  else.                                "Bk oder Konto hat gewechselt
    nextkto-flag = '0'.

    if t001-bukrs <> bsid-bukrs.
      select single * from t001
             where bukrs = bsid-bukrs.
      if sy-subrc <> 0.
        nextkto-flag = '1'.
        perform protokoll using '232'. "Eintrag fehlt in T001
        exit.
      endif.
    endif.

    select single * from knb1
           where kunnr = bsid-kunnr
           and   bukrs = bsid-bukrs.
    if sy-subrc <> 0.
      nextkto-flag = '1'.
      perform protokoll using '206'.   "Debitorkonto fehlt
      perform protokoll using '208'.   "weiter mit naechstem BK/Konto
      exit.
    endif.
  endif.   "if knb1-bukrs = bsid-bukrs and ...

* Finanzdispogruppe im B-Segment gepflegt?
  if knb1-fdgrv = space.
* Sonderfall: keine Summensatzfortschreibung, aber ggf. muessen die
* Belege zurueckgesetzt werden, falls frueher einmal Gruppe gesetzt war
    reset-flag = '1'.
  else.
    if veb-flag = '1'.                 "1-vorerfaßter Beleg
      if  bbtab-bukrs = bsid-bukrs
      and bbtab-belnr = bsid-belnr
      and bbtab-gjahr = bsid-gjahr.
        reset-flag = '1'.
      else.
*       enthält alle vorerfaßten Belege mit Bestellbezug, sofern diese
*       nicht vollständig gesichert sind -> falls Eintrag vorhanden,
*       darf nur ggf. initialisiert werden
        loop at bbtab
             where bukrs = bsid-bukrs
             and   belnr = bsid-belnr
             and   gjahr = bsid-gjahr.
          exit.
        endloop.
        if sy-subrc = 0.
          reset-flag = '1'.
        endif.
      endif.
    endif.
  endif.                               "if knb1-fdgrv = space

  if reset-flag <> '1'.
* normale Verarbeitung
    wagrupp = knb1-fdgrv.
    wazv    = knb1-xzver.              "Zahlungsverhalten aufgezeichnet

* pruefen, ob Finanzdispogruppe in Tabelle I035 gepflegt
    if i035-grupp <> knb1-fdgrv.
      loop at i035
           where grupp =  knb1-fdgrv
           and   ebene <> space.
        exit.
      endloop.
      if sy-subrc <> 0.
        nextkto-flag = '1'.
        perform protokoll using '210'. "Dispogruppe fehlt in T035
        perform protokoll using '208'. "weiter mit naechstem BK/Kto
        exit.
      endif.
    endif.

    if bsid-umskz = space.
      waebene = i035-ebene.
    else.
* pruefen, ob Sonderumsatz fuer Finanzdispo relevant
      loop at i074
           where ktopl = t001-ktopl
           and   koart = 'D'
           and   umskz = bsid-umskz
           and   hkont = knb1-akont.
        exit.
      endloop.
* Eintrag fehlt in T074 oder T074-Ebene war Space
      if sy-subrc <> 0.
        reset-flag = '1'.
      else.
        waebene = i074-ebene.
      endif.
    endif.
  endif.                               "if reset-flag <> '1'

* Debitoren-Belegzeile (und ggf. Belegkopf) wird gelesen. Bei vorer-
* fassten Belegen nur den Belegkopf, da VBSEGD bereits gelesen
  formok-flag = '0'.
  perform d-beleglesen.
  check formok-flag = '1'.   "Formroutine vollstaendig durchlaufen

* if veb-flag = '1'.                   "1-vorerfasster Beleg
*   Transaktionswaehrungsschluessel wird bei Summensatzupdate benoetigt
*   move bkpf-waers to: bseg-pswsl, bsid-pswsl.
* endif.

* alte Finanzdispodaten retten
  move-corresponding bseg to olddata.

* Payment Request nachlesen
  read table itab_pr with key
                    ibukrs = bseg-bukrs"entspricht BSEG-Key
                    ibelnr = bseg-belnr
                    igjahr = bseg-gjahr
                    ibuzei = bseg-buzei
                 binary search.        "wegen Performance

  if sy-subrc = 0.
    pr-flag    = '1'.
    reset-flag = '1'.   "BSEG-FD-Felder müssen zurückgesetzt werden
    clear icpayrq.
    move-corresponding itab_pr to icpayrq.   "für Extract
  else.
    clear: pr-flag.
  endif.

  if reset-flag = '1'.
*   Belege muessen ggf. zurueckgesetzt werden, falls frueher einmal
*   die Gruppe gesetzt war oder Sonderumsatz FD-relevant war oder
*   die FD-Fortschreibung über Payment Request erfolgt (gilt auch dann
*   wenn der Payment Request dann gar nicht in die Finanzdispo geschrie-
*   ben wird, z.B. weil das fortzuschreibende Sachkonto nicht
*   OP-verwaltet wird und somit der RFFUEB00 für die Fortschreibung
*   zuständig ist).
    if  bseg-fdlev is initial
    and bseg-fdgrp is initial
    and bseg-fdwbt is initial
    and bseg-fdtag is initial.
*     Initialisieren nicht nötig -> Extract unterdrücken
      exit.                            "Formroutine verlassen
    else.
      clear: bseg-fdlev,
             bseg-fdgrp,
             bseg-fdwbt,
             bseg-fdtag.
    endif.
  else.   "trifft nie zu, falls Payment Request gefunden wurde
*   Finanzdispodatum und -betrag ermitteln
    formok-flag = '0'.
    perform datenermittlung.
    if formok-flag = '1'.
*     Formroutine vollstaendig durchlaufen -> Extract durchführen
    else.
*     Extract nur notwendig, wenn die FD-Felder initialisiert werden
*     müssen. (Hier kann nicht auf die bseg-FD-Felder abgefragt werden,
*     da diese ggf. in der Formroutine bereits initialisiert wurden)
      if  olddata-fdlev is initial
      and olddata-fdgrp is initial
      and olddata-fdwbt is initial
      and olddata-fdtag is initial.
*       Initialisieren nicht nötig -> Extract unterdrücken
        exit.                          "Formroutine verlassen
      else.
        reset-flag = '1'.   "BSEG-FD-Felder müssen zurückgesetzt werden
*       notwendig, falls die Formroutine nicht initialisiert hat
        clear: bseg-fdlev,
               bseg-fdgrp,
               bseg-fdwbt,
               bseg-fdtag.
      endif.
    endif.
  endif.

  extract daten.
endform.                               "debitorzeile


*---------------------------------------------------------------------*
*       FORM K-BELEGLESEN                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form k-beleglesen.
* ggf. Belegkopf lesen
* (bei vorerfassten Belegen braucht kein neuer VBKPF gelesen werden, nur
*  weil sich das Key-Feld AUSBK geaendert hat. Es reicht, den ersten
*  passenden Belegkopf zu lesen, da Inhalt in allen identisch bis auf
*  dieses Feld)
  if  bkpf-bukrs = bsik-bukrs
  and bkpf-belnr = bsik-belnr
  and bkpf-gjahr = bsik-gjahr.
* ggf. alle Bibs bis zum naechsten Belegkopf ueberlesen
    if nextbkpf-flag = '1'.
      exit.
    endif.
  else.                                "neuen Belegkopf lesen
    nextbkpf-flag = '0'.

    if veb-flag = '1'.                 "1-vorerfasster Beleg
      select single * from vbkpf
                    where ausbk = vbsegk-ausbk
                    and   bukrs = bsik-bukrs
                    and   belnr = bsik-belnr
                    and   gjahr = bsik-gjahr.
      if sy-subrc <> 0.
        select * from vbkpf
               where bukrs = bsik-bukrs
               and   belnr = bsik-belnr
               and   gjahr = bsik-gjahr.
          exit.
        endselect.
      endif.
    else.
      select single * from bkpf
             where bukrs = bsik-bukrs
             and   belnr = bsik-belnr
             and   gjahr = bsik-gjahr.
    endif.

    if sy-subrc   <> 0.
      nextbkpf-flag = '1'.
      perform protokoll using '212'.   "Belegkopf fehlt
      perform protokoll using '214'.   "weiter mit naechstem Beleg
      exit.
    endif.

    if veb-flag = '1'.                 "1-vorerfasster Beleg
      clear: bkpf.
      move-corresponding vbkpf to bkpf.
    endif.

    if reset-flag <> '1'.
      if bkpf-bstat = space            "normaler Beleg
*     statistischer Beleg, z.B. Anz.anford.
      or ( bkpf-bstat = 'S' and bsik-umskz <> space )
      or bkpf-bstat = 'V'              "vorerfasster Beleg
      or bkpf-bstat = 'W'.             "vorerfasster Beleg
*       o.k.
      else.
        nextbkpf-flag = '1'.
        exit.
      endif.
    endif.
  endif.   "if bkpf-bukrs = bsik-bukrs and ...

* Belegzeile lesen
  if veb-flag = '1'.
*   bei vorerfassten Belegen ist Select auf VDBSEGK bereits erfolgt
  else.
    select single * from bseg
           where bukrs = bsik-bukrs
           and   belnr = bsik-belnr
           and   gjahr = bsik-gjahr
           and   buzei = bsik-buzei.
    if sy-subrc   <> 0.
      perform protokoll using '216'.   "Belegzeile fehlt
      perform protokoll using '218'.   "Belegzeile uebersprungen
      exit.
    endif.
  endif.

  if reset-flag <> '1'.
* diskontierte Wechsel sind fuer die Finanzdispo nicht relevant
*   if  bsik-umsks = 'W'
*   and bseg-wverw <> space.           "Wechselverwendung
*     exit.
*   endif.
  endif.
  formok-flag = '1'.
endform.                               "k-beleglesen

*---------------------------------------------------------------------*
*       FORM KREDITORZEILE                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form kreditorzeile.
  clear: reset-flag.
  if kred-flag <> '1'.   "nur beim ersten Kreditorposten moeglich
    kred-flag = '1'.
    if deb-flag = '1'.
      skip 2.
      uline.
      skip 2.
    endif.
    perform protokoll using '202'.     "Kred. werden bearbeitet
  endif.

  if wabukrs <> bsik-bukrs.            "BK hat gewechselt
    wabukrs = bsik-bukrs.
    perform protokoll using '234'.
  endif.

  clear olddata.
  if  lfb1-bukrs = bsik-bukrs
  and lfb1-lifnr = bsik-lifnr.
* ggf. alle Bibs bis zum naechsten BK- oder Kontowechsel ueberlesen
    if nextkto-flag = '1'.
      exit.
    endif.
  else.                                "Bk oder Konto hat gewechselt
    nextkto-flag = '0'.

    if t001-bukrs <> bsik-bukrs.
      select single * from t001
             where bukrs = bsik-bukrs.
      if sy-subrc <> 0.
        nextkto-flag = '1'.
        perform protokoll using '232'. "Eintrag fehlt in T001
        exit.
      endif.
    endif.

    select single * from lfb1
           where lifnr = bsik-lifnr
           and   bukrs = bsik-bukrs.
    if sy-subrc <> 0.
      nextkto-flag = '1'.
      perform protokoll using '206'.   "Konto fehlt
      perform protokoll using '208'.   "weiter mit naechstem BK/Konto
      exit.
    endif.
  endif.   "if lfb1-bukrs = bsik-bukrs and ...

* Finanzdispogruppe im B-Segment gepflegt?
  if lfb1-fdgrv = space.
* Sonderfall: keine Summensatzfortschreibung, aber ggf. muessen die
* Belege zurueckgesetzt werden, falls frueher einmal Gruppe gesetzt war
    reset-flag = '1'.
  else.
    if veb-flag = '1'.                 "1-vorerfaßter Beleg
      if  bbtab-bukrs = bsik-bukrs
      and bbtab-belnr = bsik-belnr
      and bbtab-gjahr = bsik-gjahr.
        reset-flag = '1'.
      else.
*       enthält alle vorerfaßten Belege mit Bestellbezug, sofern diese
*       nicht vollständig gesichert sind -> falls Eintrag vorhanden,
*       darf nur ggf. initialisiert werden
        loop at bbtab
             where bukrs = bsik-bukrs
             and   belnr = bsik-belnr
             and   gjahr = bsik-gjahr.
          exit.
        endloop.
        if sy-subrc = 0.
          reset-flag = '1'.
        endif.
      endif.
    endif.
  endif.                               "if lfb1-fdgrv = space

  if reset-flag <> '1'.
* normale Verarbeitung
    wagrupp = lfb1-fdgrv.
    wazv    = space.                   "nur fuer Debitoren relevant

* pruefen, ob Finanzdispogruppe in Tabelle I035 gepflegt
    if i035-grupp <> lfb1-fdgrv.
      loop at i035
        where grupp =  lfb1-fdgrv
        and   ebene <> space.
        exit.
      endloop.
      if sy-subrc <> 0.
        nextkto-flag = '1'.
        perform protokoll using '210'. "Dispogruppe fehlt in T035
        perform protokoll using '208'. "weiter mit naechstem BK/Kto
        exit.
      endif.
    endif.

    if bsik-umskz = space.
      waebene = i035-ebene.
    else.
* pruefen, ob Sonderumsatz fuer Finanzdispo relevant
      loop at i074
        where ktopl = t001-ktopl
        and   koart = 'K'
        and   umskz = bsik-umskz
        and   hkont = lfb1-akont.
        exit.
      endloop.
* Eintrag fehlt in T074 oder T074-Ebene war Space
      if sy-subrc <> 0.
        reset-flag = '1'.
      else.
        waebene = i074-ebene.
      endif.
    endif.
  endif.                               "if reset-flag <> '1'

* Kreditoren-Belegzeile (und ggf. Belegkopf) wird gelesen. Bei vorer-
* fassten Belegen nur den Belegkopf, da VBSEGK bereits gelesen.
  formok-flag = '0'.
  perform k-beleglesen.
  check formok-flag = '1'.   "Formroutine vollstaendig durchlaufen

* if veb-flag = '1'.                   "1-vorerfasster Beleg
*   Transaktionswaehrungsschluessel wird bei Summensatzupdate benoetigt
*   move bkpf-waers to: bseg-pswsl, bsik-pswsl.
* endif.

* alte Finanzdispodaten retten
  move-corresponding bseg to olddata.

* Payment Request nachlesen
  read table itab_pr with key
                    ibukrs = bseg-bukrs"entspricht BSEG-Key
                    ibelnr = bseg-belnr
                    igjahr = bseg-gjahr
                    ibuzei = bseg-buzei
                 binary search.        "wegen Performance

  if sy-subrc = 0.
    pr-flag    = '1'.
    reset-flag = '1'.   "BSEG-FD-Felder müssen zurückgesetzt werden
    clear icpayrq.
    move-corresponding itab_pr to icpayrq.   "für Extract
  else.
    clear: pr-flag.
  endif.

  if reset-flag = '1'.
*   Belege muessen ggf. zurueckgesetzt werden, falls frueher einmal
*   die Gruppe gesetzt war oder Sonderumsatz FD-relevant war oder
*   die FD-Fortschreibung über Payment Request erfolgt
    if  bseg-fdlev is initial
    and bseg-fdgrp is initial
    and bseg-fdwbt is initial
    and bseg-fdtag is initial.
*     Initialisieren nicht nötig -> Extract unterdrücken
      exit.                            "Formroutine verlassen
    else.
      clear: bseg-fdlev,
             bseg-fdgrp,
             bseg-fdwbt,
             bseg-fdtag.
    endif.
  else.   "trifft nie zu, falls Payment Request gefunden wurde
* Finanzdispodatum und -betrag ermitteln
    formok-flag = '0'.
    perform datenermittlung.
    if formok-flag = '1'.
*     Formroutine vollstaendig durchlaufen -> Extract durchführen
    else.
*     Extract nur notwendig, wenn die FD-Felder initialisiert werden
*     müssen. (Hier kann nicht auf die bseg-FD-Felder abgefragt werden,
*     da diese ggf. in der Formroutine bereits initialisiert wurden)
      if  olddata-fdlev is initial
      and olddata-fdgrp is initial
      and olddata-fdwbt is initial
      and olddata-fdtag is initial.
*       Initialisieren nicht nötig -> Extract unterdrücken
        exit.                          "Formroutine verlassen
      else.
        reset-flag = '1'.   "BSEG-FD-Felder müssen zurückgesetzt werden
*       notwendig, falls die Formroutine nicht initialisiert hat
        clear: bseg-fdlev,
               bseg-fdgrp,
               bseg-fdwbt,
               bseg-fdtag.
      endif.
    endif.
  endif.

  extract daten.
endform.                               "kreditorzeile

*---------------------------------------------------------------------*
*       FORM PROTOKOLL                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TEXTNR                                                        *
*---------------------------------------------------------------------*
form protokoll using textnr type c.
  case textnr.
* Meldungen innerhalb der Selektionsphase (-> wakoart gesetzt)
    when '200'.
      write: /1 text-200.
    when '202'.
      write: /1 text-202.
    when '204'.
      write: /1 text-204.
    when '206'.
      write  text-206 to watext.
      if wakoart = 'D'.
        replace '&KTNRB' with bsid-kunnr into watext.
        replace '&BUKRS' with bsid-bukrs into watext.
      elseif wakoart = 'K'.
        replace '&KTNRB' with bsik-lifnr into watext.
        replace '&BUKRS' with bsik-bukrs into watext.
      else.
        replace '&KTNRB' with bsis-hkont into watext.
        replace '&BUKRS' with bsis-bukrs into watext.
      endif.
      write: /1 watext.
    when '208'.
      write  text-208 to watext.
      if wakoart = 'D'.
        replace '&KTNRB' with bsid-kunnr into watext.
        replace '&BUKRS' with bsid-bukrs into watext.
      elseif wakoart = 'K'.
        replace '&KTNRB' with bsik-lifnr into watext.
        replace '&BUKRS' with bsik-bukrs into watext.
      else.
        replace '&KTNRB' with bsis-hkont into watext.
        replace '&BUKRS' with bsis-bukrs into watext.
      endif.
      write: /1 watext.
    when '210'.
      write  text-210 to watext.
      if wakoart = 'D'.
        replace '&KTNRB' with knb1-kunnr into watext.
        replace '&BUKRS' with knb1-bukrs into watext.
        replace '&GRUPP' with knb1-fdgrv into watext.
      elseif wakoart = 'K'.
        replace '&KTNRB' with lfb1-lifnr into watext.
        replace '&BUKRS' with lfb1-bukrs into watext.
        replace '&GRUPP' with lfb1-fdgrv into watext.
      endif.
      write: /1 watext.
    when '212'.
      if veb-flag = '1'.               "1-vorerfasster Beleg
        write  text-213 to watext.
      else.
        write  text-212 to watext.
      endif.
      if wakoart = 'D'.
        replace '&BELNR' with bsid-belnr into watext.
        replace '&BUKRS' with bsid-bukrs into watext.
        replace '&GJAHR' with bsid-gjahr into watext.
      elseif wakoart = 'K'.
        replace '&BELNR' with bsik-belnr into watext.
        replace '&BUKRS' with bsik-bukrs into watext.
        replace '&GJAHR' with bsik-gjahr into watext.
      else.
        replace '&BELNR' with bsis-belnr into watext.
        replace '&BUKRS' with bsis-bukrs into watext.
        replace '&GJAHR' with bsis-gjahr into watext.
      endif.
      write: /1 watext.
    when '214'.
      write: /1 text-214.
    when '216'.
      write  text-216 to watext.
      if wakoart = 'D'.
        replace '&BUZEI' with bsid-buzei into watext.
        replace '&BELNR' with bsid-belnr into watext.
        replace '&BUKRS' with bsid-bukrs into watext.
        replace '&GJAHR' with bsid-gjahr into watext.
      elseif wakoart = 'K'.
        replace '&BUZEI' with bsik-buzei into watext.
        replace '&BELNR' with bsik-belnr into watext.
        replace '&BUKRS' with bsik-bukrs into watext.
        replace '&GJAHR' with bsik-gjahr into watext.
      else.
        replace '&BUZEI' with bsis-buzei into watext.
        replace '&BELNR' with bsis-belnr into watext.
        replace '&BUKRS' with bsis-bukrs into watext.
        replace '&GJAHR' with bsis-gjahr into watext.
      endif.
      write: /1 watext.
    when '218'.
      write: /1 text-218.
    when '220'.
      write  text-220 to watext.
      replace '&GRUPP' with bseg-fdgrp into watext.
      replace '&BELNR' with bkpf-belnr into watext.
      replace '&BUKRS' with bkpf-bukrs into watext.
      replace '&GJAHR' with bkpf-gjahr into watext.
      write: /1 watext.
    when '222'.
      write  text-222 to watext.
      replace '&EBENE' with bseg-fdlev into watext.
      replace '&BELNR' with bkpf-belnr into watext.
      replace '&BUKRS' with bkpf-bukrs into watext.
      replace '&GJAHR' with bkpf-gjahr into watext.
      write: /1 watext.
    when '224'.
      write  text-224 to watext.
      replace '&EBENE' with bseg-fdlev into watext.
      write: /1 watext.
    when '226'.
      write  text-226 to watext.
      replace '&BUKRS' with bkpf-bukrs into watext.
      write: /1 watext.
    when '228'.
      write  text-228 to watext.
      replace '&EBENE' with bseg-fdlev into watext.
      write: /1 watext.
    when '229'.                        "Fortsetzung von Text-228
      write  text-229 to watext.
      replace '&KOART' with bseg-koart into watext.
      replace '&BELNR' with bkpf-belnr into watext.
      replace '&BUZEI' with bseg-buzei into watext.
      replace '&BUKRS' with bkpf-bukrs into watext.
      replace '&GJAHR' with bkpf-gjahr into watext.
      write: /1 watext.
    when '232'.
      write  text-232 to watext.
      if wakoart = 'D'.
        replace '&BUKRS' with bsid-bukrs into watext.
      elseif wakoart = 'K'.
        replace '&BUKRS' with bsik-bukrs into watext.
      else.
        replace '&BUKRS' with bsis-bukrs into watext.
      endif.
      write: /1 watext.
    when '234'.
      write  text-234 to watext.
      replace '&BUKRS' with wabukrs into watext.
      write: /1 watext.
    when '236'.
      write  text-236 to watext.
      replace '&BUKRS' with t001-bukrs into watext.
      write: /1 watext.
    when '238'.
      write  text-238 to watext.
      replace '&GSBER' with tgsb-gsber into watext.
      write: /1 watext.
    when '240'.
      write: /1 text-241.              "FD in BK nicht aktiv
      write: /1 text-242.              "oder
      write: /1 text-240.              "keine Berechtigung

* Meldungen innerhalb von Loop Endloop ueber Zwischendatenbestand
    when '300'.
      write  text-300 to watext.
      replace '&BELNR' with bseg-belnr into watext.
      replace '&BUZEI' with bseg-buzei into watext.
      replace '&BUKRS' with bseg-bukrs into watext.
      replace '&GJAHR' with bseg-gjahr into watext.
      write: /1 watext.
  endcase.
endform.                               "protokoll

*---------------------------------------------------------------------*
*       FORM RANGES_CHECK                                             *
*---------------------------------------------------------------------*
*       prueft ob ranges zu gross fuer select                         *
*---------------------------------------------------------------------*
form ranges_check.
  describe table r_bukrs lines bklines.
  if bklines > maxrange.
    refresh r_bukrs2.
    loop at r_bukrs.
      r_bukrs2 = r_bukrs.
      append r_bukrs2.
    endloop.
    clear   r_bukrs.
    refresh r_bukrs.                   "sonst Abbruch im Select
  endif.

  describe table r_gsber lines gblines.
  if gblines > maxrange.
    refresh r_gsber2.
    loop at r_gsber.
      r_gsber2 = r_gsber.
      append r_gsber2.
    endloop.
    clear   r_gsber.
    refresh r_gsber.                   "sonst Abbruch im Select
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM SACHKONTOZEILE                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form sachkontozeile.
* Im Gegensatz zu Kontoart 'D' und 'K' spielen Payment Requests bei
* Sachkontenzeilen keine Rolle, da Sachkontobuchung bereits erfolgt
* und die FD-Daten in der Belegzeile stehen.
  clear: reset-flag.
  clear: pr-flag.
  if sak-flag <> '1'.
    sak-flag = '1'.
    if deb-flag = '1' or kred-flag = '1'.
      skip 2.
      uline.
      skip 2.
    endif.
    perform protokoll using '204'.     "Sachk. werden bearbeitet
  endif.

  if wabukrs <> bsis-bukrs.            "BK hat gewechselt
    wabukrs = bsis-bukrs.
    perform protokoll using '234'.
  endif.

  clear olddata.
  if  skb1-bukrs = bsis-bukrs
  and skb1-saknr = bsis-hkont.
* ggf. alle Bibs bis zum naechsten BK- oder Kontowechsel ueberlesen
    if nextkto-flag = '1'.
      exit.
    endif.
  else.                                "Bk oder Konto hat gewechselt
    nextkto-flag = '0'.

    if t001-bukrs <> bsis-bukrs.
      select single * from t001
             where bukrs = bsis-bukrs.
      if sy-subrc <> 0.
        nextkto-flag = '1'.
        perform protokoll using '232'. "Eintrag fehlt in T001
        exit.
      endif.
    endif.

*   vor select in itab_skb1 zu suchen sinnlos, da die Daten sortiert
*   angeliefert werden, d.h. das Sachkonto kann noch nicht gelesen sein
    select single * from skb1
           where saknr = bsis-hkont
           and   bukrs = bsis-bukrs.
    if sy-subrc <> 0.
      nextkto-flag = '1'.
      perform protokoll using '206'.   "Konto fehlt
      perform protokoll using '208'.   "weiter mit naechstem BK/Konto
*     itab_skb1 wird für Payment Requests später benötigt
      clear: itab_skb1.
      move bsis-hkont to itab_skb1-saknr.
      move bsis-bukrs to itab_skb1-bukrs.
      move '1'        to itab_skb1-notfound.
      append itab_skb1.
      exit.
    else.
*     itab_skb1 wird für Payment Requests später benötigt
      clear: itab_skb1.
      move-corresponding skb1 to itab_skb1.
      append itab_skb1.
    endif.
  endif.   "if skb1-bukrs = bsis-bukrs and ...

* Finanzdispoebene im B-Segment gepflegt?
  if skb1-fdlev = space.
*   Sonderfall: keine Summensatzfortschreibung, aber ggf. muessen die
*   Belege zurueckgesetzt werden, falls frueher einmal Ebene gesetzt war
    reset-flag = '1'.   "gilt auch für Bankkontoposten
  elseif skb1-xopvw <> 'X'.            "X - offene Postenverwaltung
*   Ebene gesetzt, aber Konto nicht OP-geführt
    if  veb-flag <> '1' "bei vorerfaßten Belegen gibt es kein bsis-xopvw
    and bsis-xopvw = 'X'. "früher war mal skb1-xopvw gesetzt
      reset-flag = '1'.   "gilt auch für Bankkontoposten
    else.
      check: 1 = 2.       "nicht relevant, da Bankkonto
    endif.
  else.
*   Ebene gesetzt und Konto ist OP-geführt
    if veb-flag = '1'.                 "1-vorerfaßter Beleg
      if  bbtab-bukrs = bsis-bukrs
      and bbtab-belnr = bsis-belnr
      and bbtab-gjahr = bsis-gjahr.
        reset-flag = '1'.
      else.
*       enthält alle vorerfaßten Belege mit Bestellbezug, sofern diese
*       nicht vollständig gesichert sind -> falls Eintrag vorhanden,
*       darf nur initialisiert werden
        loop at bbtab
             where bukrs = bsis-bukrs
             and   belnr = bsis-belnr
             and   gjahr = bsis-gjahr.
          exit.
        endloop.
        if sy-subrc = 0.
          reset-flag = '1'.
        endif.
      endif.
    else.   "-> normaler Beleg
*     Falls das Konto früher nicht-OP-geführt war und nachträglich auf
*     OP-geführt gesetzt wurde, dann musste das Konto zuvor auf Saldo
*     Null gebracht werden. Die alten Belege und die dazugehörigen
*     Gegenbelege haben BSIS-/BSEG-XOPVW = Space und dürfen hier nicht
*     berücksichtigt werden, da sonst hier BSEG-DMBTR mit BKPF-WAERS
*     fortgeschrieben würde, es also zu einem Schiefstand käme.
*     Hier einfach so tun, als wäre BSEG-XOPVW = 'X' würde zwar hier zu
*     einer korrekten Fortschreibung führen (BSEG-WRBTR mit BKPF-WAERS),
*     aber würde so ein Beleg storniert werden, dann würde es wieder zu
*     einem Schiefstand in der Finanzdispo kommen, da dann aus der
*     Finanzdispo BSEG-FDWBT mit SKB1-WAERS rausgenommen würde.
      if bsis-xopvw = space.
        reset-flag = '1'.
      endif.
    endif.                             "if veb-flag = '1'
  endif.                               "if skb1-fdlev = space

  if reset-flag <> '1'.
*   normale Verarbeitung
    wagrupp = space.                   "nur fuer Deb/Kred relevant
    waebene = skb1-fdlev.
    wazv    = space.                   "nur fuer Debitoren relevant
  endif.

* Sachkonten-Belegzeile (und ggf. Belegkopf) wird gelesen. Bei vorer-
* fassten Belegen nur den Belegkopf, da VBSEGS bereits gelesen.
  formok-flag = '0'.
  perform s-beleglesen.
  check formok-flag = '1'.   "Formroutine vollstaendig durchlaufen

* if veb-flag = '1'.                   "1-vorerfasster Beleg
*   Transaktionswaehrungsschluessel wird bei Summensatzupdate benoetigt
*   move bkpf-waers to: bseg-pswsl, bsis-pswsl.
* endif.

* alte Finanzdispodaten retten
  move-corresponding bseg to olddata.

* Im Gegensatz zu Kontoart 'D' und 'K' spielen Payment Requests bei
* Sachkontenzeilen keine Rolle, da Sachkontobuchung bereits erfolgt.
* Daher erfolgt an dieser Stelle kein Zugriff auf itab_pr.
  clear: pr-flag.

  if reset-flag = '1'.
*   Belege muessen ggf. zurueckgesetzt werden, falls frueher einmal
*   die Ebene gesetzt war oder das OP-Verwaltungskennzeichen im
*   Stammsatz geändert wurde
    if  bseg-fdlev is initial
    and bseg-fdgrp is initial
    and bseg-fdwbt is initial
    and bseg-fdtag is initial.
*     Initialisieren nicht nötig -> Extract unterdrücken
      exit.                            "Formroutine verlassen
    else.
      clear: bseg-fdlev,
             bseg-fdgrp,
             bseg-fdwbt,
             bseg-fdtag.
    endif.
  else.
*   Finanzdispodatum und -betrag ermitteln
    formok-flag = '0'.
    perform datenermittlung.
    if formok-flag = '1'.
*     Formroutine vollstaendig durchlaufen -> Extract durchführen
    else.
*     Extract nur notwendig, wenn die FD-Felder initialisiert werden
*     müssen. (Hier kann nicht auf die bseg-FD-Felder abgefragt werden,
*     da diese ggf. in der Formroutine bereits initialisiert wurden)
      if  olddata-fdlev is initial
      and olddata-fdgrp is initial
      and olddata-fdwbt is initial
      and olddata-fdtag is initial.
*       Initialisieren nicht nötig -> Extract unterdrücken
        exit.                          "Formroutine verlassen
      else.
        reset-flag = '1'.   "BSEG-FD-Felder müssen zurückgesetzt werden
*       notwendig, falls die Formroutine nicht initialisiert hat
        clear: bseg-fdlev,
               bseg-fdgrp,
               bseg-fdwbt,
               bseg-fdtag.
      endif.
    endif.
  endif.

  extract daten.
endform.                               "sachkontozeile

*---------------------------------------------------------------------*
*       FORM S-BELEGLESEN                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form s-beleglesen.
* ggf. Belegkopf lesen
* (bei vorerfassten Belegen braucht kein neuer VBKPF gelesen werden, nur
*  weil sich das Key-Feld AUSBK geaendert hat. Es reicht, den ersten
*  passenden Belegkopf zu lesen, da Inhalt in allen identisch bis auf
*  dieses Feld)
  if  bkpf-bukrs = bsis-bukrs
  and bkpf-belnr = bsis-belnr
  and bkpf-gjahr = bsis-gjahr.
* ggf. alle Bibs bis zum naechsten Belegkopf ueberlesen
    if nextbkpf-flag = '1'.
      exit.
    endif.
  else.                                "neuen Belegkopf lesen
    nextbkpf-flag = '0'.

    if veb-flag = '1'.                 "1-vorerfasster Beleg
      select single * from vbkpf
             where ausbk = vbsegs-ausbk
             and   bukrs = bsis-bukrs
             and   belnr = bsis-belnr
             and   gjahr = bsis-gjahr.
      if sy-subrc <> 0.
        select * from vbkpf
               where bukrs = bsis-bukrs
               and   belnr = bsis-belnr
               and   gjahr = bsis-gjahr.
          exit.
        endselect.
      endif.
    else.
      select single * from bkpf
             where bukrs = bsis-bukrs
             and   belnr = bsis-belnr
             and   gjahr = bsis-gjahr.
    endif.

    if sy-subrc   <> 0.
      nextbkpf-flag = '1'.
      perform protokoll using '212'.   "Belegkopf fehlt
      perform protokoll using '214'.   "weiter mit naechstem Beleg
      exit.
    endif.

    if veb-flag = '1'.                 "1-vorerfasster Beleg
      clear: bkpf.
      move-corresponding vbkpf to bkpf.
    endif.

    if reset-flag <> '1'.
      if bkpf-bstat = space            "normaler Beleg
      or bkpf-bstat = 'V'              "vorerfasster Beleg
      or bkpf-bstat = 'W'.             "vorerfasster Beleg
*       o.k.
      else.
        nextbkpf-flag = '1'.
        exit.
      endif.
    endif.
  endif.   "if bkpf-bukrs = bsis-bukrs and ...

* Belegzeile lesen
  if veb-flag = '1'.
*   bei vorerfassten Belegen ist Select auf VDBSEGS bereits erfolgt
  else.
    select single * from bseg
           where bukrs = bsis-bukrs
           and   belnr = bsis-belnr
           and   gjahr = bsis-gjahr
           and   buzei = bsis-buzei.
    if sy-subrc   <> 0.
      perform protokoll using '216'.   "Belegzeile fehlt
      perform protokoll using '218'.   "Belegzeile uebersprungen
      exit.
    endif.

*   Falls bei Debitoren und Kreditoren beim Abstimmkonto bei Kontover-
*   waltung der Schalter 'Einzelpostenanzeige' gesetzt ist, werden
*   in BSIS auch die Zeilen der Abstimmkonten mit BSEG-KOART gleich
*   'D' oder 'K' angeliefert -> diese ueberlesen
    check: bseg-koart = 'S'.
  endif.

  formok-flag = '1'.
endform.                               "s-beleglesen

*---------------------------------------------------------------------*
*       FORM Vertragskontokorrent (FI-CA)                             *
*---------------------------------------------------------------------*
form vertragskontokorrent.
  data: l_t_fica_sk like rf40s occurs 0 with header line,
        l_t_fica_pk like rf40s occurs 0 with header line.

  check: p_test = space.
* Daten bearbeiten ohne Berücksichtigung von Geschäftsjahr.
* Prüfen, ob Vertragskontokorrent (FI-CA) im System aktiv ist
* (falls es die Domäne APPLK_KK gibt, dann sollte das der Fall sein)
  select domname from dd01l into dd01l-domname up to 1 rows
         where domname  = 'APPLK_KK'.
  endselect.
  check: sy-subrc = 0.

  clear: cntr, vbtab.
  refresh: vbtab.

* a) OP-geführte Sachkonten bearbeiten
* (p_restr kann dann hier nur Space, 2 oder 4 sein.)
  if 'S' in s_koart
  and ( p_restr = space                "alle Konten bearbeiten
  or    p_restr = '2' ).  "OP-geführte Sachkonten bearbeiten
    perform fi-ca_get_data_for_cm(rffdfica)
            tables l_t_fica_sk l_t_fica_pk
            using  'X' space
            if found.   "falls vorhanden (d.h. FI-CA aktiv)
    if not l_t_fica_sk[] is initial.
      sort l_t_fica_sk by bukrs hkont. "für Zugriff auf itab_skb1
      loop at l_t_fica_sk
           where hkont in s_saknr
           and   bukrs in s_bukrs.

        if  itab_skb1-bukrs = l_t_fica_sk-bukrs
        and itab_skb1-saknr = l_t_fica_sk-hkont.
*         Tabelleneintrag schon gelesen
        else.
          loop at itab_skb1
               where bukrs = l_t_fica_sk-bukrs
               and   saknr = l_t_fica_sk-hkont.
            exit.
          endloop.

          if sy-subrc <> 0.
            select single * from skb1
                   where saknr = l_t_fica_sk-hkont
                   and   bukrs = l_t_fica_sk-bukrs.
            check: sy-subrc = 0.
            move-corresponding skb1 to itab_skb1.
            append itab_skb1.
          endif.
        endif.

        check: itab_skb1-xopvw = 'X'.  "nur OP-verwaltete Sachkonten

*       Ebene verproben
        call function 'CASH_FORECAST_CHECK_LEVEL_GRP'
             exporting
                  fdlev = l_t_fica_sk-fdlev
                  koart = 'S'.

        collect l_t_fica_sk into vbtab.
        cntr = cntr + 1.

        if cntr = 250.
          cntr = 0.
*         Falls es zu Abbrüchen kommt, weil mehrere Verbucher parallel
*         arbeiten -> IN UPDATE TASK entfernen, wegen folgendem Problem:
*         Geht der Insert schief, wird hart abgebrochen und der
*         zweite Update-Versuch wird nicht mehr durchgeführt. Statt
*         Update und Insert Modify zu nehmen, geht nicht, da beim
*         Modify die SET-Variante nicht möglich ist
*         CALL FUNCTION 'CASH_FORECAST_SUMMARY_REC_UPD'
          call function 'CASH_FORECAST_SUMMARY_REC_UPD' in update task
               tables
                    tab_rf40s = vbtab.
          commit work.
          refresh vbtab.
        endif.
      endloop.                         "at l_t_fica_sk

*     nicht bei at last im Loop, da loop at .. where ..
      if cntr > 0.
*       siehe Doku oben
*       CALL FUNCTION 'CASH_FORECAST_SUMMARY_REC_UPD'
        call function 'CASH_FORECAST_SUMMARY_REC_UPD' in update task
             tables
                  tab_rf40s = vbtab.
        commit work.
        refresh vbtab.
      endif.
    endif.                             "if not l_t_fica_sk[] is initial
  endif.                               "if 'S' in s_koart ...

  clear: cntr, vbtab.
  refresh: vbtab.

* b) Personenkonten bearbeiten
* (p_restr kann dann hier nur Space, 2 oder 4 sein.)
  if 'D' in s_koart                    "dann ist auch 'K' in s_koart
  and ( p_restr = space                "alle Konten bearbeiten
  or    p_restr = '4' ).               "Personenkonten bearbeiten
    refresh: l_t_fica_sk, l_t_fica_pk.
    perform fi-ca_get_data_for_cm(rffdfica)
            tables l_t_fica_sk l_t_fica_pk
            using  space 'X'
            if found.   "falls vorhanden (d.h. FI-CA aktiv)
    if not l_t_fica_pk[] is initial.
      loop at l_t_fica_pk
           where bukrs in s_bukrs.
*       Gruppe und Ebene verproben
        call function 'CASH_FORECAST_CHECK_LEVEL_GRP'
             exporting
                  fdgrp = l_t_fica_pk-fdgrp
                  fdlev = l_t_fica_pk-fdlev
                  koart = 'D'.         "'D' oder 'K', ist egal

        collect l_t_fica_pk into vbtab.
        cntr = cntr + 1.

        if cntr = 250.
          cntr = 0.
*         siehe Doku oben
*         CALL FUNCTION 'CASH_FORECAST_SUMMARY_REC_UPD'
          call function 'CASH_FORECAST_SUMMARY_REC_UPD' in update task
               tables
                    tab_rf40s = vbtab.
          commit work.
          refresh vbtab.
        endif.
      endloop.                         "at l_t_fica_pk

*     nicht bei at last im Loop, da loop at .. where ..
      if cntr > 0.
*       siehe Doku oben
*       CALL FUNCTION 'CASH_FORECAST_SUMMARY_REC_UPD'
        call function 'CASH_FORECAST_SUMMARY_REC_UPD' in update task
             tables
                  tab_rf40s = vbtab.
        commit work.
        refresh vbtab.
      endif.
    endif.                             "if not l_t_fica_pk[] is initial
  endif.                               "if 'D' in s_koart ...
endform.                               "Vertragskontokorrent
