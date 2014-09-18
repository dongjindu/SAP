*------Kernstrukturen fuer MONI-Tabellen
*
*      2.2B SUMMA für Accounting eingeführt QQI 7/94 K11094978
*
*------Kernstrukturen fuer Zeitwerte und Zaehlwerte--------------------
*
DATA:  BEGIN OF stat_moni_1,
       respti   TYPE p,
       cputi   TYPE p,
       queueti   TYPE p,

       readdircnt TYPE p,
       readdirti   TYPE p,
       readdirbuf TYPE p,

       readseqcnt TYPE p,
       readseqti   TYPE p,
       phyreadcnt TYPE p,

       chngcnt TYPE p,
       chngti TYPE p,
       phychngrec TYPE p,

       guitime TYPE p,                 "Frontend: GUI time
       guinettime TYPE p,              "          Net time
       guicnt TYPE p,                  "          Roundtrips

END OF stat_moni_1,
*
*------Kernstruktur transferierte Bytes--------------------------------
*
BEGIN OF stat_moni_2,
       dsqlcnt TYPE p,
       abapscnt TYPE p,
       abaplcnt TYPE p,
       dynpscnt TYPE p,
       dynplcnt TYPE p,
       cualcnt TYPE p,
       ntabcnt TYPE p,
       quecnt TYPE p,
       ddiccnt TYPE p,
       cpiccnt TYPE p,
END OF stat_moni_2,
*
*------Kernstruktur Tabellenstatistik----------------------------------
*
BEGIN OF stat_moni_4,
       modcnt  LIKE sta1-modcnt,       "Number of changed rows
       dircnt  LIKE sta1-dircnt,       "Number of rows read directly
       seqcnt  LIKE sta1-seqcnt,       "Number of rows read sequential
       tabtime LIKE sta1-tabtime,      "Time for accesses
END OF stat_moni_4.
*-----------------------------------------------------------------------
*------Table structures for MONI----------------------------------------
*-----------------------------------------------------------------------
*
*------HITL1:  Hitliste pro CPU-Id und Tag, Zeitverbrauch---------------
*              Struktur wie STATS
*
*
DATA:  BEGIN OF hitl1 OCCURS 40.
*        include structure stats_cuml.
        INCLUDE STRUCTURE sapwlhitl.
DATA:  END OF hitl1.
*
* --- max. # of hitlist records (for each tasktype) ------------------ *
*
DATA   tt_hitl_max_records TYPE i VALUE 40.
*
* --- hitlist structure similar to HITL1 for tasktypes --------------- *
*
DATA  tt_hitl_respti LIKE sapwlhitl OCCURS 160 WITH HEADER LINE.
                                       "top scorer of response time

DATA: BEGIN OF tt_hitl_respti_min OCCURS 0,   "min. value of resp. times
        tasktype LIKE stats_cuml-tasktype,    "top scorers
        respti   LIKE stats_cuml-respti,
      END   OF tt_hitl_respti_min.

DATA  BEGIN OF tt_hitl1_backup OCCURS 0.      "backup of total hitlist
        INCLUDE STRUCTURE hitl1.
DATA  END   OF tt_hitl1_backup.
*
*------HITL2:  Hitliste pro CPU-Id und Tag, DB-Requests-----------------
*              Struktur wie STATS
*
DATA:  BEGIN OF hitl2 OCCURS 40.
*        include structure stats_cuml.
        INCLUDE STRUCTURE sapwlhitl.
DATA:  END OF hitl2.

DATA  BEGIN OF tt_hitl2_backup OCCURS 0.      "backup of total hitlist
        INCLUDE STRUCTURE hitl2.
DATA  END   OF tt_hitl2_backup.
*
* --- hitlist structure similar to HITL2 for tasktypes --------------- *
*
DATA  tt_hitl_dbcalls LIKE sapwlhitl OCCURS 160 WITH HEADER LINE.
                                       "top scorer of DB calls

DATA: BEGIN OF tt_hitl_dbcalls_min OCCURS 0, "min. values of DB calls
        tasktype LIKE stats_cuml-tasktype,   "top scorers
        dbcalls  LIKE stats_cuml-dbcalls,
      END   OF tt_hitl_dbcalls_min.
*
*------SUMMA:  Summensaetze für Accounting Exit------------------------
*
DATA:  BEGIN OF summa OCCURS 10,
        mandt LIKE sy-mandt,
        username(12) TYPE c,
        account(12) TYPE c,
        count LIKE sy-index,
        dcount LIKE sy-index,
        ucount LIKE sy-index,
        bcount LIKE sy-index,
        scount LIKE sy-index,
        respti   TYPE p,
        cputi   TYPE p,
        queueti   TYPE p,
* --- extensions for accounting exit --------------------------------- *
        value_0 TYPE p,  "new meaning: usr DB access time (milliseconds)
        value_1 TYPE p,  "new meaning: sys load & gen time(milliseconds)
        value_2 TYPE p,  "new meaning: accessed user data (bytes)
        value_3 TYPE p,  "new meaning: accessed sys  data (bytes)
* -------------------------------------------------------------------- *
        value_4 TYPE p,
        value_5 TYPE p,
       END OF summa.
*
* --- SUMMA similar structure for tasktype split --------------------- *
*
DATA  tt_summa LIKE sapwluswlx OCCURS 0 WITH HEADER LINE.

DATA  BEGIN OF tt_summa_backup OCCURS 0. "contains old SUMMA structure
        INCLUDE STRUCTURE summa.       "if selected task type is <>
DATA  END   OF tt_summa_backup.        "'*' (total)

DATA BEGIN OF tt_more_summa OCCURS 0.  "same as TT_SUMMA, used only as
        INCLUDE STRUCTURE tt_summa.    "return table for GET-SUM...
DATA END   OF tt_more_summa.           "...OF-LONG-TERMS form

*
*------SUMMU:  Summensaetze pro CPU-Id, Tag bzgl. User-, Tcodprofil----
*
DATA: BEGIN OF summu OCCURS 100.
*       ACCOUNT(12) TYPE C,
*       TCODE(8) TYPE C,
*       COUNT LIKE SY-INDEX,
*       DCOUNT LIKE SY-INDEX,
*       UCOUNT LIKE SY-INDEX,
*       BCOUNT LIKE SY-INDEX,
*       ECOUNT LIKE SY-INDEX,
*       SCOUNT LIKE SY-INDEX.
*        INCLUDE STRUCTURE STAT_MONI_1.
*        INCLUDE STRUCTURE STAT_MONI_2.
        INCLUDE STRUCTURE sapwlustc.
DATA    entry_id_type.
DATA   END OF summu.
*
* --- SUMMU similar structure for tasktype split --------------------- *
*
DATA  tt_summu LIKE sapwlustcx OCCURS 250 WITH HEADER LINE.

DATA  BEGIN OF tt_summu_backup OCCURS 0. "contains old SUMMU structure
        INCLUDE STRUCTURE summu.       "if selected task type is <>
DATA  END   OF tt_summu_backup.        "'*' (total)

DATA BEGIN OF tt_more_summu OCCURS 0.  "same as TT_SUMMU, used only as
        INCLUDE STRUCTURE tt_summu.    "return table for GET-SUM...
DATA END   OF tt_more_summu.           "...OF-LONG-TERMS form

DATA  tt_users_of_entry_id LIKE sapwluenti OCCURS 0 WITH HEADER LINE.

DATA BEGIN OF tt_more_users_of_entry_id OCCURS 0.
        INCLUDE STRUCTURE tt_users_of_entry_id.
DATA END   OF tt_more_users_of_entry_id.

DATA BEGIN OF tt_users_of_entry_id_agg OCCURS 0. "list of users as desc.
      INCLUDE STRUCTURE tt_users_of_entry_id."above, but for aggregation
DATA END   OF tt_users_of_entry_id_agg.

DATA: BEGIN OF tt_user_list OCCURS 0,  "list of users. Used for display
        account LIKE tt_summu-account, "of users of one tcode/entry_id
      END   OF tt_user_list.
*
*------SUMMT:  Summensaetze pro CPU-Id, Tag bzgl. Zeitprofil-----------
*
DATA:  BEGIN OF summt OCCURS 200,
       time     LIKE sapwltimes-time,
       tcode    LIKE sta1-tcode,
       entry_id LIKE sapwltimes-entry_id,
       count LIKE sy-index.
        INCLUDE STRUCTURE stat_moni_1.
DATA   END OF summt.

*
* --- SUMMT similar structure for tasktype split --------------------- *
*
DATA  tt_summt LIKE sapwltimes OCCURS 0 WITH HEADER LINE.

DATA BEGIN OF tt_more_summt OCCURS 0.  "same as TT_SUMMT, used only as
        INCLUDE STRUCTURE tt_summt.    "return table for GET-SUM...
DATA END   OF tt_more_summt.           "...OF-LONG-TERMS form
* -------------------------------------------------------------------- *

*
*
*------SUMMR:  Summensaetze pro Tag/Tasktyp/Zeitintervall---------------
*
DATA  summr LIKE sapwltskti OCCURS 0 WITH HEADER LINE.
*
*------SUMML:  Tabellensummensaetze pro Transaktion und Tabelle---------
*
DATA:  BEGIN OF summl OCCURS 0,
       tcode       LIKE sapwlustc-tcode, "Transaktion/Report
       tasktype(1) TYPE c,             "Tasktyp
       tname       LIKE pftab-tabname. "Tabellenname
        INCLUDE STRUCTURE stat_moni_4.
DATA   END OF summl.

*
* --- SUMML similar structure for tasktype extension ----------------- *
*

DATA  tt_summl LIKE sapwltabtr OCCURS 0 WITH HEADER LINE.

DATA  BEGIN OF tt_more_summl OCCURS 0.
        INCLUDE STRUCTURE tt_summl.
DATA  END   OF tt_more_summl.

DATA  BEGIN OF tt_summl_backup OCCURS 0. "contains old SUMML structure
        INCLUDE STRUCTURE summl.       "if selected task type is <>
DATA  END   OF tt_summl_backup.        "'*' (total)
*
*------SUMMY:  Byteverkehr zwischen Praesentation und Applikation-------
*              Korr. B11k008175 QQI: Struktur angelegt
DATA  summy LIKE sapwltmbyt OCCURS 0 WITH HEADER LINE.
*
*
*-----XHITL1:  Hitliste pro CPU-Id und Tag, Zeitverbrauch---------------
*              Struktur wie STATS
*
DATA:  BEGIN OF xhitl1 OCCURS 40.
        INCLUDE STRUCTURE hitl1.
DATA:  END OF xhitl1.

*
* --- similar structure to XHITL1 for tasktypes ---------------------- *
*
DATA  BEGIN OF tt_more_hitl_respti OCCURS 0.
        INCLUDE STRUCTURE tt_hitl_respti.
DATA  END   OF tt_more_hitl_respti.

DATA  BEGIN OF tt_more_hitl_respti_min OCCURS 0.
        INCLUDE STRUCTURE tt_hitl_respti_min.
DATA  END   OF tt_more_hitl_respti_min.
*
*-----XHITL2:  Hitliste pro CPU-Id und Tag, DB-Requests-----------------
*              Struktur wie STATS
*
DATA:  BEGIN OF xhitl2 OCCURS 40.
        INCLUDE STRUCTURE hitl2.
DATA:  END OF xhitl2.
*
* --- similar structure to XHITL2 for tasktypes ---------------------- *
*
DATA  BEGIN OF tt_more_hitl_dbcalls OCCURS 0.
        INCLUDE STRUCTURE tt_hitl_dbcalls.
DATA  END   OF tt_more_hitl_dbcalls.

DATA  BEGIN OF tt_more_hitl_dbcalls_min OCCURS 0.
        INCLUDE STRUCTURE tt_hitl_dbcalls_min.
DATA  END   OF tt_more_hitl_dbcalls_min.
*
*------XUMMA:  Summensaetze für Accounting Exit-------------------------
*
DATA:  BEGIN OF xumma OCCURS 500.
        INCLUDE STRUCTURE summa.
DATA:  END OF xumma.
*
*------XUMMU:  Summensaetze pro CPU-Id, Tag bzgl. User-, Tcodprofil----
*
DATA:  BEGIN OF xummu OCCURS 500.
        INCLUDE STRUCTURE summu.
DATA:  END OF xummu.
*
*------XUMMT:  Summensaetze pro CPU-Id, Tag bzgl. Zeitprofil-----------
*
DATA:  BEGIN OF xummt OCCURS 100.
        INCLUDE STRUCTURE summt.
DATA:  END OF xummt.
*
*------XUMMR:  Summensaetze pro Tag/Tasktyp/Zeitintervall---------------
*
DATA:  BEGIN OF xummr OCCURS 70.
        INCLUDE STRUCTURE summr.
DATA:  END OF xummr.
*
*------XUMML:  Tabellensummensaetze pro Transaktion und Tabelle---------
*
DATA:  BEGIN OF xumml OCCURS 0.
        INCLUDE STRUCTURE summl.
DATA:  END OF xumml.
*
*------XUMMY:  Byteverkehr zwischen Praesentation und Applikation-------
*              Korr. B11k008175 QQI: Struktur angelegt
*
DATA:  BEGIN OF xummy OCCURS 20.
        INCLUDE STRUCTURE summy.
DATA:  END OF xummy.

* --- Extensions for statistic version 2 ----------------------------- *

* RFC statistic

DATA  rfc_client_statistic        LIKE sapwlrfcc  OCCURS 0
                                                  WITH HEADER LINE.
DATA  rfc_server_statistic        LIKE sapwlrfcs  OCCURS 0
                                                  WITH HEADER LINE.
DATA  rfc_client_dest_statistic   LIKE sapwlrfccd OCCURS 0
                                                  WITH HEADER LINE.
DATA  rfc_server_dest_statistic   LIKE sapwlrfcsd OCCURS 0
                                                  WITH HEADER LINE.

* Spool statistic

DATA: spool_activity_statistic LIKE sapwlspoac OCCURS 0
                                                  WITH HEADER LINE.
DATA: BEGIN OF spool_print_statistic.
        INCLUDE STRUCTURE sapwlspopr.
DATA: END OF spool_print_statistic.

* Memory usage statistic

DATA  memory_statistic            LIKE sapwlmem   OCCURS 0
                                                  WITH HEADER LINE.

* Collected instances

DATA  instance_table              LIKE sapwlinst  OCCURS 0
                                                  WITH HEADER LINE.
