*   Data Anweisung uer Statistik-Parameter in bezug auf MONI datenbank
*
tables: parstat.
data: cuaprofileflag(1).
data: begin of stapar occurs 0,
*
*-------Versionsnummer--------------------------------------------------
        version(10),                   "Versionsname
        vertype(3),                    "Versionnummer (OLD/NEW)
        active(1),                  "kennzeichnet einzig aktive Version
*
*-------Administrative Information--------------------------------------
        lastchangedate like sy-datum,  "Letzter Versionsaenderungstag
        lastchangetime like sy-uzeit,
        lastchangeuser(12),
        sincedate      like sy-datum,  "Ab wann sind Aenderungen aktiv
*
*-------Flags-Values (X/ )---------------------------------------------
*
        collectiveflag(1),             "TOTAL-Server wird aufgebaut
        callstatactiveflag(1),      "Table access Stat. wird mitgeschrie
        deletefileflag(1),          "Loeschen Stat.file nach RSSTTAT80
        deleteexternalflag(1),         "Loeschvermerk Externe Daten
        onlytotalflag(1),              "RSSTAT60: Nur TOTAL server reorg
        tocflag(1),                    "RSSTAT60: Generiere TOC
*-------Numerische Werte------------------------------------------------
        res_days     like sy-index,    "Residenzzeiten intensive Daten
        res_weeks    like sy-index,
        res_months   like sy-index,
        res_years    like sy-index,
        quiet_days   like sy-index,    "Toleranzzeiten bei Statistikruhe
        res_dayssum   like sy-index,   "Residenzzeiten lose Daten
        res_weekssum  like sy-index,
        res_monthssum like sy-index,
        res_yearsum   like sy-index,
        maxobjects like sy-index,      "Max.Objektanzahl bei Reorg.
        countlimit like sy-index,     "Max.Zaehler bei RSSTAT80, 0 =inf.
        aclimit like sy-index,        "Schwellwert bei Tabellenstatistik
        statfilesize like sy-index,    "Max.Stat.file size (MB)
                                       "fuer delete nach RSSTAT80.
*
*-------Muster fuer Automatisierte Reorglauefe--------------------------
        reorg80_hour(24),              "Postione= Stunde, X bedeutet ja
        reorg80_day(7),                "Pos.=Tag (1=So) , X bedeutet ja
        reorg60_total(6),             "Server, der TOTAL mitreorganisiet
        reorg60_hour(24),              "Postione= Stunde, X bedeutet ja
        reorg60_day(7),                "Pos.=Tag (1=So) , X bedeutet ja
                                       "(* = zentral, blank = keiner)
        accountlevel(1),               "Accounting Exit level
        userclient(1),                 "Add Client to user name
        cuaprofile(1),                 "Activate CUA Profile if 'X'

*-------Erweiterung paralleler Kollektor RSSTAT83
        max_no_rfc like parstat-max_no_rfc,   "Max. no. RFCs
        cum_week like parstat-cum_week,       "Kummuliere Wochen
        cum_month like parstat-cum_month,     "Kummuliere Monate
        cum_hitl  like parstat-cum_hitl ,     "Kummuliere Hitlisten
        cum_mem   like parstat-cum_mem  ,     "Kummuliere Memory-Profil
        cum_acct  like parstat-cum_acct ,     "Kummuliere Accounting-Pr.
        cum_rfc   like parstat-cum_rfc  ,     "Kummuliere RFC-Profile
        cum_termio like parstat-cum_termio,   "Kummuliere Term-IO-Profil
        cum_fcode like parstat-cum_fcode,     "Kummuliere CUA-Profil

      end of stapar.

*
*-------Kollektorparameter für Application Statistik
data: begin of as_coll_par occurs 0,
*        collectiveflag(1),             "TOTAL-Server wird aufgebaut
        countlimit like sy-index,     "Max.Zaehler bei RSSTAT89, 0 =inf.
        astatfilesze like sy-index,    "Max.Appl.Stat.file size (MB)
                                       "fuer delete nach RSSTAT89.
      end of as_coll_par.
