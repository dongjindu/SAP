*-----------------------------------------------------------------------
*
* Daten fuer Schnittstelle zu MONI
* RSSTAT* - Reports, SAPMSS03 Modulpool
*
*-----------------------------------------------------------------------
*
*--- Hostnamenerweiterung ----------------------------------------------
*
types: short_host like sapwlserv-hostshort,
       short_inst like sapwlserv-instshort,
       long_host  like sapwlserv-host,
       long_inst  like sapwlserv-name.

data   myname   like sapwlserv-name.   "local instance

*
*---Data fuer Inhaltsverzeichnis MONI (TOC)-----------------------------
*
data: begin of toc occurs 0.
        include structure moni_v01.    "Streichview
data: end of toc.
data: begin of toc_line.
        include structure moni_v01.    "Streichview
data: end of toc_line,
      srtf2 like moni-srtf2,           "Hex Null Wert
*
*---Data fuer UNIX-File Pfadname----------------------------------------
      begin of pfad,
        pfadname(100) type c,
        statname(5) type c value '/stat',
      end of pfad,
*
*---Data fuer Dateischnittstelle zu MONI--------------------------------
*
*-----Index
      begin of moniindex occurs 100,   "Index fuer Statistikfile
               zeit type p,            "Zeit in Form YYYYMMDDhhmm
               rcrdcnt like sy-index,  "Zaehler
      end of moniindex,

*---B11K008240: RCRDCNTDD neu aufgenommen-------------------------------
      begin of ix,                     "Index Information
        maxtim like moniindex-zeit,    "Maximalzeit in Moniindex
        mintim like moniindex-zeit,    "Minimalzeit in Moniindex
        offset like sy-index,          "Max. Index in MONIINDEX
        rcrdcntdd type p,              "Offsetzaehler Statistik
        signature(40),                 "Erstn 40 Bytes des Files
      end of ix,
*
*-----Monikey
      begin of monikeydd,              "Schluessel bei MONI Ex/Import
        systemid(2),                   "Systemnummer
        cpuid(8),                      "Instance abbreviation
        type(4),                       "Art der Daten
        datum like sy-datum,           "Datum
      end of monikeydd,
*-----Monikey-Variante
      begin of monikey,
        system(2),                     "Systemnummer
        cpuid(8),                      "Instance abbreviation
        key(12),                       "Monikey Schluessel
      end of monikey,
*---Data fuer Aufruf UNIX Kommando--------------------------------------
      begin of tabl occurs 0,
        line(132),
      end of tabl,
      begin of parcom,                 "Struktur fuer UNIX-Aufrufe
        command(16) type c,
        path(50) type c,
      end of parcom,
*
*-----Enqueue Hilfsfelder-----------------------------------------------
*
      lockkey  like moni-srtfd,        "Lockkey
      lockdate like sy-datum,          "Lockdatum
      locktime like sy-uzeit,          "Lockzeit
      lockname like sy-uname,          "Lockname
      lockflag value 'Y',              "Flag lokal gesperrt
*
*------Aus Tablerecord STATB aufzubauende Tabellen-tabelle--------------
       begin of tabs occurs 20,
         tname(10),
         modcnt like sy-index,
         dircnt like sy-index,
         seqcnt like sy-index,
         tabtime like sy-index,
         avgtime type p decimals 1,
         totcnt like sy-index,
         cross(1) type c,
       end of tabs,
*
*
*------Kernstrukturen fuer MONI-Tabellen
*
*------Kernstruktur Verteilung der Antwortzeit-------------------------
*
begin of stat_moni_3,
         cnt001 like sy-index,         " < 0,1 s
         cnt002 like sy-index,         " < 0,2 s
         cnt003 like sy-index,         " < 0,3 s
         cnt004 like sy-index,         " < 0,4 s
         cnt005 like sy-index,         " < 0,5 s
         cnt006 like sy-index,         " < 1,0 s
         cnt007 like sy-index,         " < 2,0 s
         cnt008 like sy-index,         " < 3,0 s
         cnt009 like sy-index,         " <10,0 s
end of stat_moni_3,
*------Kernstruktur Perioden Summary-----------------------------------
*
begin of stat_moni_5,
         elapsedti type p,
         respti type p,                "Response time (ms)
         cputi type p,                 "CPU time (ms)
         queueti type p,               "Wait time (ms)
         loadgenti type p,             "Load/Generating time
         committi type p,              "Commit time
         ddicti type p,                "DDIC time
         queti type p,                 "B.I.queue time
         cpicti type p,                "CPIC time
         mcodeti type p,               "Matchcode time

         rollincnt type p,             "Anzahl Roll in
         rollinstep  type p,           "Anzahl Roll in Dialogschritte
         rollinti type p,              "Zeit Roll in
         rolloutcnt type p,            "Anzahl Roll out
         rolloutti type p,             "Zeit Roll out

         readdircnt type p,            "Dir. read requests
         readdirti type p,             "Time for read dir. req.
         readdirbuf type p,            "Dir. buffer reads
         readdirrec type p,            "Dir. read records


         readseqcnt type p,            "Seq. read requests
         readseqti type p,             "Time for read seq. req.
         readseqbuf type p,            "Seq. buffer reads
         readseqrec type p,            "Seq. read records

         chngcnt type p,               "Change requestst
         chngti type p,                "Time for change req.
         chngrec type p,               "Changed records

         phyreadcnt type p,            "Phys. DB reads
         phychngrec type p,            "Phys. DB changes

         mcreadcnt type p,             "Matchcode reads
         mcchngcnt type p,             "Matchcode changes
         phycalls type p,              "Phys. DB calls
end of stat_moni_5.
*
*---Strukturen/Teabellen für die Anwendungsstatistik
*
data: application_stat like sapwlvdas occurs 0,
      appl_hitl_dbcalls like sapwlashit occurs 0,
      appl_hitl_respti like sapwlashit occurs 0,
      begin of appl_workload occurs 0.
include      structure asoutput.
data: end of appl_workload.
data: okey_text_type(12) value 'Techn. text'.
                                       "Auszugebender Texttyp in der AS
data: okey_texte         type astat_t_otx2.
data: as_hitl_typ(1).
data: save_as_tabix like sy-tabix,
      first_as_rec  like sy-tabix,
      last_as_rec   like sy-tabix.
constants: tech_text(12) value 'Techn. text',
           normal_text(12) value 'Descr. text'.
*
*-----Standardfelder Statistik-----------------------------------------
*
data: rdate(8) type c value '00000000',"Standarddatum
      rsystem  type short_inst value '00000000',  "Instanzenkürzel
      rinstance like sapwlinst-name value space,  "Instanz
      rsystemid(2) value '00',         "Standardsystem
      rperiod(8) value '        ',     "Standardperiodentyp
      rcommand(8) value '    ',        "Standardkommando
      rcomment(24),
      rtstart like sy-uzeit value '000000',  "Unteres Zeitlimit
      rtstop  like sy-uzeit value '235959'.  "Oberes Zeitlimit
*-----Flags
data:
batchtype(1),                          "Flag bei Batch-Records,Type=3
accountingflag(1),                     "Flag bei Accounting-Exit
nomoreheaderflag(1) value ' ',         "Flag bei Ueberschrift nur 1x
shortreportflag(1) value ' ',          "Flag bei Kurzbericht
runtime type p,                        "Laufzeit in Sek.
runtime2 like runtime,                 "Laufzeit in Sek.
storeclock like sy-uzeit,              "Zwischenspeicher Uhrzeit
noauthflag(1) value ' ',               "Flag bei nicht vorh. Autorisierg
quickrefrflag(1) value ' ',            "Flag fuer Quick-Refresh
snapshotflag(1) value 'N',             "Flag fuer Zeitslot Analyse
noupdateflag(1) value ' ',             "Flag fuer: No Update of data
collectiveflag(1) value 'Y',  "Flag fuer Statistiksze. Systemtotal
callstatactiveflag(1) value 'Y',       "Call.Stat. = aktiv
externalflag(1) value 'N',             "Flag fuer Fremddatenuebernahme
repairedflag(1) value 'N',      "Flag bei zu reparierenden Records
logicaluseflag(1) value 'N',    "Flag bei log.selektierten Records
tabsusedflag(1) value 'X',             "Flag, ob TABS benutzt wird
deletefileflag(1) value 'N',           "Flag bei zu loesch.Statfile
initflag(1)  type c value 'N',
exitflag(1)  type c value 'Y',
usersflag(1) type c value 'N',
tablsflag(1) type c value 'N',
accntflag(1) type c value 'N',
tcodeflg1(1) type c value 'N',
tcodeflg2(1) type c value 'N',
tasksflag(1) type c value 'N',
clientflg(1) type c value 'N',
scopeflag(1) type c value ' ',
newinstanceflag(1) value ' ',          "Flag bei Nachbarinstanz
noindexflag(1) value ' ',              "Flag, falls keine Indexpflege
selwpflag(1) value ' ',
accountbyclientflag(1) value ' ',      "Accounting profile by client ?
antiflag(1) value ' ',                 "Flag, if unused Tcodes are shwn.

readsummflag(1)  value ' ',            "B11K008020 QQI
readhitlflag(1)  value ' ',            "B11K008020 QQI
readsummlflag(1)  value ' ',           "B11K008020 QQI
namelength_flag type i value 0,        "Flag für Namensraumerweiterung
save_namelength_flag type i value 0,   "save Flag für Namensraumerw.
*
*---Diverse Hilfsfelder/Arbeitsfelder
*
     begin of text_tab occurs 1.
        include structure textpool.
data : end of text_tab.
data: tcodetext like tstct-ttext,
reporttext like textpool-entry,
screentext like d020t-dtxt,
devctext like tdevct-ctext,
trdirtext(60),
trcltext like trclt-text,
funtext1(20),
funtext2(20),
tcode_level like sy-tabix value 0,     "Anzeigelevel TCODE
tcode_oldlevel like sy-tabix value 0,  "Hilfsfeld Anzeigelevel TCODE
tcode_picklevel like sy-tabix value -1,"Anzeigelevel Pickup TCOD
tcode_oldpicklevel like sy-tabix value -1,"Hilfs.Anz.level Pickup TCOD
linno like sy-linno,                   "Aktuelle Listzeile
savelsind like sy-lsind,               "Aktuelle Verzweigungsliste
deletesize like sy-tabix value 0,      "Max. erlaubter Dateigroesse
saversystem type short_host,
saverdate like rdate,
saverperiod like rperiod,
savedate(8),                           "Eingangsdatum sichern
savetcode(8),                          "Hilfsfeld Zwischenspeicher TCode
savetabix like sy-tabix,
tabixsave like sy-tabix,
act_date like rdate,        "aktuelles Datum bei Lesen STATREC.
sortcucol like sy-cucol,               "Sortierspalte
pname(8),
week type p,
tabfollowflag(1) value 'N',            "Folgt Tabellensatz auf Statstz
countlimit type p,
status(4) type c,
modus(4) type c,
index like sy-tabix,
seite like sy-index,
eof(1) type c value 'N',
steuer(4) type c,
stufe type p,
tcodeentries like sy-tabix,            "Zähler für Anz. Profileinträge
tableentries like sy-tabix,            "Zähler für Anz. Profileinträge
accountingentries like sy-tabix,       "Zähler für Anz. Profileinträge
cliententries like sy-tabix,           "Zähler für Anz. Profileinträge
userentries like sy-tabix,             "  - id -
step type p,
output,
modline like sy-index value 5,         "Modifizierungszeile in RSSTAT20
* !!! MODLINE wird auch noch einmal in RSSTATHL gesetzt !!!
deltaday type p,
*
hit1minval type p,                     "Niedrigster Wert HITL1
hit2minval type p,                     "Niedrigster Wert HITL1
xhit1minval type p,                    "Niedrigster Wert XHITL1
xhit2minval type p,                    "Niedrigster Wert XHITL1



tabrec like sy-tabix,                  "Aktueller Wert von stat/tabrec
headlinesize like sy-tabix,            "Anzahl Zeilen fuer Uebrschrift
time4(4) type c,                       "Hilfsvariable Indexaufbau
indexkey like moniindex-zeit,          "Hilfsfeld Indexaufbau
startrba like sy-index,                "Startbyte seq. Datei
offsetcnt like sy-index,               "Akuteller Zaehler aus STARTRBA
lstatrcrd like sy-index,               "Laenge des stat. Records
total_delta  type p value 0,           "Anzahl Deltasätze beim Reorg
parvalue(40) type c,                   "Parameterwert im Profil
checkuzeit like sy-index,              "Hilfsfeld Stat.sammlung
indexappend(1) type c value 'N',       "Flag bei Indexfortschreibung
maxsize like sy-tabix,                 "Groesse Statistikfile in Mb
counter like sy-index,                 "Zaehler stat. Records
misscounter like sy-index,             "Zaehler in RSSTATMO
misscounter1 like sy-index,            "Zaehler in RSSTATMO
misscounter2 like sy-index,            "Zaehler in RSSTATMO
misscounter3 like sy-index,            "Zaehler in RSSTATMO
misscounter4 like sy-index,            "Zaehler in RSSTATMO
successcounter like sy-index,          "Zaehler in RSSTATMO
tabreccounter like sy-index,           "Zaehler in RSSTATMO
statreccounter like sy-index,          "Zaehler in RSSTATMO
tabentriescnt  like sy-index,          "Zaehler in RSSTATMO
tabentries     like sy-index,          "Zaehler in RSSTATMO
rcrdcnt        like sy-index,          "Counter in RSSTATMO
lstcnt         like sy-index,          "Counter in RSSTATMO
seltime type t,                        "Selektionszeit
rtasktype type x,                      "Tasktyp(01=D,02=U,03=S0,4=B,...)
cpuid(8) type c,                       "Instance abbreviation
shift like sy-fdpos,                   "Shift für Recordreparatur
shiftadd like sy-index,                "Shift Hilfsvariable
systemid(2) type c,
oldendtime like sy-uzeit,              "Hilfswert fuer PERIOD-ENDTIME
d1 like sy-datum,                      "Zeitwerte bei BUILD_SYSTEMTOTAL
d2 like sy-datum,
t1 like sy-uzeit,
t2 like sy-uzeit,
pathok value 'Y',
lines like sy-index,                   "Feld f. Anzahl Tabelleneintraege
dbcalti type p,
countx type p,
help like sy-index,
help1 like sy-index,
*
fcode(4) type c,
acode like fcode,
code like fcode,
*
save_sta1_tcode like sta1-tcode,       "fuer Angabe der Auswahl fuer die
                                       "Verzweigungsliste
*
rc like sy-subrc.                      " 'Returncode' aus RSSTAT80

*-----------------------------------------------------------------------
* Hilfsdatenfelder für die Namensraumerweiterung ---
* Ausgabe von Tabellen-, Transactionscode-, Programm- und Entwicklungs-
* klassennamen in alter (kurzer) oder neuer (langer) Form
*-----------------------------------------------------------------------
data: namehelp_tcode           like sapwlobjct-tcode,
      namehelp_table           like sapwlobjct-table,
      namehelp_report          like sapwlobjct-report,
      namehelp_devc            like sapwlobjct-devc,
      namehelp_fcode           like sapwlobjct-fcode,
      namehelp_status          like sapwlobjct-status,
      namehelp_tcode_or_report like sapwlobjct-tcode_repo,
      namehelp_entry_id        like sapwlobjct-entry_id,
      namehelp_btcjob          like sapwlobjct-btcjob,
      namelength_tcode         type i value 20 , "4, "UD1K930846
      namelength_table         type i value 10,
      namelength_report        type i value 40, "8,
      namelength_devc          type i value 4,
      namelength_fcode         type i value 4,
      namelength_status        type i value 8,
      namelength_entry_id      type i value 32,
      namelength_uline         type i value 160, "100
      namelength_save_status   like status.
* sowie Hostnamenerweiterung
data: short_instance           like sapwlserv-instshort,
      long_instance            like sapwlinst-name,
      long_rsystem             type long_inst.
*-----------------------------------------------------------------------

*
*-----------------------------------------------------------------------
* Zuordnung der echten Stunden zu Zeitintervallen fuer Zeitprofil
*-----------------------------------------------------------------------
*
*-echte Stunden :     000102030405060708091011121314151617181920212223
*
*-zugeordnete Anfangsstunden:------------------------------------------
*
data: tim1(48) value '000000000000060708091011121314151617181920212121',
*
*-zugeordnete Endstunden:----------------------------------------------
*
      tim2(48) value '060606060606070809101112131415161718192021242424'.
*
*----------------------------------------------------------------------
*---Datum, ab dem die Wochenzaehlung beginnt. (30.12.1990)--------------
*   Auf keinen Fall aendern !!!!!!!!!!!!!!!!!!!!!!!!!!!
*----------------------------------------------------------------------
*
data: basisday like sy-datum value '19901230'.
*--Schwelle fuer Waitlimitzaehler
data: queuethreshold type p  value  50.
*
*
*------SUMMARY: Summensaetze pro Jahr-----------------------------------
*
data   summary like sapwlsumry occurs 0 with header line.
*
*------XUMMARY:Summensaetze pro Periode (SUMMARY verdichtet)------------
*
data:  begin of xummary occurs 10.
        include structure summary.
data:  end of xummary,
*
*-----Hilfstab. fuer FORM FILL-TABLES----------------------------------
*
      begin of savtim occurs 5,
        tasktype like summary-tasktype,
        lastrecti like summary-lastrecti,
        firstrecti like summary-lastrecti,
      end of savtim,
      begin of wp_tab occurs 15,
        tasktype like summary-tasktype,"Tasktyp
        wpid(2),                       "Workprozessnummer
      end of wp_tab.
*
*------PERIOD: Logbuchdatei fuer Initialzeit, Endezeit-----------------
*
data:  begin of period,
         rcrdcntdd like sy-index,      "Zaehler Dataset
         initdate(8) type c,           "Beginn Recording Tag
         inittime like sy-uzeit,       "Beginn Recording Uhrzeit
         enddate(8) type c,            "Ende Recording Tag
         endtime like sy-uzeit,        "Ende Recording Uhrzeit
         lastdelday(8) type c,         "Letzter geloeschter Tag
         lastcomday(8) type c,         "Letzer kompletter Tag
         lastdelweek type p,           "Letzte geloeschte Woche
         lastcomweek type p,           "Letzte komplette Woche
         lastdelmonth(6) type c,       "Letzter geloeschter Monat
         lastcommonth(6) type c,       "Letzter kompletter Monat
         lastdelyear(4) type c,        "Letztes geloeschtes Jahr
         lastcomyear(4) type c,        "Letztes komplettes Jahr
         lastdelsumday(8)  type c,     "Letzter gel.Tagessummensatz
         lastcomsumday(8)  type c,     "Letztes verdichtetes Datum
         lastdelsumweek(8) type c,     "Letzter gel.Wochensummensatz
         lastcomsumweek(8) type c,     "Letzte  verdichtete Woche
         lastdelsummonth(8) type c,    "Letzter gel.Monatssummensatz
         lastcomsummonth(8) type c,    "Letzter verdichteter Monat
         lastdelsumyear(8)  type c,    "Letzter gel.Jahressummensatz
         lastcomsumyear(8)  type c,    "Letztes verdichtetes Jahr
*
         totalcnt type p,              "Anzahl Saetze total
         rsystemid(2),                 "letzte Systemid
         dbsys like sy-dbsys,          "DB-System
         dcsys like sy-dcsys,          "DC-System
         opsys like sy-opsys,          "Operating System
         sysid like sy-sysid,          "System ID
         saprl like sy-saprl,          "SAP Release
         unamefield(100) type c,       "UNIX 'uname -a' Information
*
         lastupdateday(8) type c,      "Tag des letzten UPDATE-MONI
         lastupdatetime like sy-uzeit, "Zeit des letzten UPDATE-MONI
         lastupdateuser(12),           "User des letzten UPDATE-MONI
         lastupdateobjects like sy-index,"Anzahl Saetze in UPDATE-MONI
*
         lastreorgday(8) type c,       "Tag des letzten UPDATE-MONI
         lastreorgtime like sy-uzeit,  "Zeit des letzten UPDATE-MONI
         lastreorguser(12),            "User des letzten UPDATE-MONI
         lastreorgobjects like sy-index,"Anzahl Saetze in UPDATE-MONI
*
         signature(40),                "Signatur fuer MONI-INDEX
*
         path(124),                    "PFAD-Name (rein informativ)
         comment(60),                  "Kommentar
         owner(12),                    "First user of Statistics
         rsystem(8),      "like sy-host,  "Erster Server der Statistik
*
         tableactiveflag(1) value ' ', "Flag: Tabellenstatistik an/aus
         vbactiveflag(1) value ' ',    "Flag: Verbucher auf Server aktiv
         clientprofiledate(8),         "Datum: Beginn Client Profil
       end of period,
*
* Struktur NRLOG zu Korrektur B20K001188 angelegt QQI
* NRLOG wird ab 1.3A mit PERIOD in den Logbuchsaetzen der MONI
* abgelegt und enthaelt Detailinformation ueber die Instanzen
       begin of nrlog occurs 5,
         rsystemid(2),                 "Instanzennummer
         path(60),                     "Letzter Pfadname
         lastupdateday(8) type c,      "Tag des letzten UPDATE-MONI
         lastupdatetime like sy-uzeit, "Zeit des letzten UPDATE-MONI
         lastupdateuser(12),           "User des letzten UPDATE-MONI
         lastupdateobjects like sy-index,"Anzahl Saetze in UPDATE-MONI
         flag01,                       "Flag zur freien Verf.
         flag02,                       "Flag zur freien Verf.
         flag03,                       "Flag zur freien Verf.
         flag04,                       "Flag zur freien Verf.
         flag05,                       "Flag zur freien Verf.
         flag06,                       "Flag zur freien Verf.
         flag07,                       "Flag zur freien Verf.
         flag08,                       "Flag zur freien Verf.
         flag09,                       "Flag zur freien Verf.
         flag10,                       "Flag zur freien Verf.
         count01 type p,               "Zaehler zur freien Verf.
         count02 type p,               "Zaehler zur freien Verf.
         count03 type p,               "Zaehler zur freien Verf.
         count04 type p,               "Zaehler zur freien Verf.
         count05 type p,               "Zaehler zur freien Verf.
         count06 type p,               "Zaehler zur freien Verf.
         count07 type p,               "Zaehler zur freien Verf.
         count08 type p,               "Zaehler zur freien Verf.
         count09 type p,               "Zaehler zur freien Verf.
         count10 type p,               "Zaehler zur freien Verf.
       end of nrlog,
       begin of xnrlog occurs 5.
        include structure nrlog.
data:  end of xnrlog.
*
data:  begin of totallog occurs 10,
         rsystem(8),                   "Name des Applikationsservers
         firstsystemday(8),            "Erster TOTAL-Eintrag fuer Server
         lastsystemday(8),             "LetzterTOTAL-Eintrag fuer Server
         count1 like sy-index,         "Anzahl Eintraege
         count2 like sy-index,
         count3 like sy-index,
         count4 like sy-index,
         count5 like sy-index,
         count6 like sy-index,
         varinfo1(10),                 "User beim ersten Eintrag
         varinfo2(10),                 "Uhrzeit des ersten Eintrages
         varinfo3(10),                 "User beim letzten Eintrag
         varinfo4(10),                 "Uhrzeit des letzten Eintrages
         varinfo5(10),
         varinfo6(10),
       end of totallog.

* --- extension for tasktypes ---------------------------------------- *

* --- BINK129577: Korrektur angebotene Daten bei 'Other period' --------
data:  dynp0013_cursor     like sy-stepl,
       dynp0013_size       like sy-stepl.
* --- BINK129577 -------------------------------------------------------

include rsstattt.
