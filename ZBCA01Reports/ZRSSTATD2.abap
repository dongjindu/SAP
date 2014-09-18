*
*-----------------------------------------------------------------------
*  Daten fuer RSSTAT20 (Anzeige Statistik-Einzelrecords)
*-----------------------------------------------------------------------
DATA   BEGIN OF stats OCCURS   0.
        INCLUDE STRUCTURE sapwlstatc.
DATA:   origin     TYPE i,             "original record number
        jobname    LIKE sapwlpfbtc-jobname,
        jobstep    LIKE sapwlpfbtc-jobstep,
        cua_prog   LIKE sapwlpfnrm-cua_prog,
        cua_func   LIKE sapwlpfnrm-cua_func,
        memsum     LIKE sapwlpfnrm-memsum,
        usedbytes  LIKE sapwlpfnrm-usedbytes,
        privsum    LIKE sapwlpfnrm-privsum,
        maxbytes   LIKE sapwlpfnrm-maxbytes,
        maxbytesdi LIKE sapwlpfnrm-maxbytesdi,
        privmode   LIKE sapwlpfnrm-privmode,
        wprestart  LIKE sapwlpfnrm-wprestart,
        guinettime LIKE sapwlpfnrm-guinettime,
        guitime    LIKE sapwlpfnrm-guitime,
        guicnt     LIKE sapwlpfnrm-guicnt,
       END OF stats,
       BEGIN OF sel.
        INCLUDE STRUCTURE statl.
DATA:  END OF sel,
       BEGIN OF sel2.
        INCLUDE STRUCTURE statl.
DATA:  END OF sel2,
       BEGIN OF statl_hlp.
        INCLUDE STRUCTURE statl.
DATA:  END OF statl_hlp,
        BEGIN OF ex OCCURS 3,
          ex(4),
        END OF ex,
        details(8) VALUE 'X      ',
        BEGIN OF stad,
          disp(1),
          dbcl(1),
          stor(1),
          db5f(1),
          neth(1),
          matc(1),
          rfc,
          spool,
        END OF stad,
      xflag,                           "Hilfsflag für Listfarben
answ1,
rmaxcnt2 TYPE i,
count LIKE sy-index,                   "Hilfsfeld Zaehler Records
byt LIKE sy-index,                     "Hilfsfeld bei Check Bytes
chg LIKE sy-index,                     "HIlfsfeld bei Check Phys.Chgs.
savetime LIKE sy-uzeit,                "Hilfsfeld bei Check Uhrzeit
loadgenti LIKE sy-index,               "Lade+Generierungszeit
tabslistcnt LIKE sy-index,             "Anzahl Entries in int. Tab.
selectedflag(1),                       "Flag bei Daten ausselektiert
wpid LIKE sy-index,                    "Hilfsfeld Workprozess
scrno TYPE i,                          "Hilfsfeld Screen Nummer
bi_flag(1) VALUE ' ',                  "Flag bei BI-Tasktype Selektion
string(40),                            "Hilfsfeld bei Selektion
savecucol LIKE sy-cucol.               "Hilfsfeld bei Sortieren TABS

DATA: BEGIN OF tabslist OCCURS   0,    "Sammelt Einträge der i.T. TABS
         tname(10).                    "Tabellennanme
        INCLUDE STRUCTURE stat_moni_4.
DATA: END OF tabslist.

DATA  dynp0050_recs_displayed TYPE i.
DATA  dynp0050_okcode(4).

DATA  read_continue_offset             LIKE sapwlsfidx-offset VALUE -1.
DATA  last_read_continue_offset        LIKE sapwlsfidx-offset VALUE -1.
DATA  index_available                                         VALUE 'X'.

DATA: BEGIN OF rec_statistic,          "record statistic
        normal          TYPE i,
        btc             TYPE i,
        table           TYPE i,
        rfc_cli         TYPE i,
        rfc_cli_dest    TYPE i,
        rfc_serv        TYPE i,
        rfc_serv_dest   TYPE i,
        spool_print     TYPE i,
        spool_activity  TYPE i,
        file            LIKE sapwlpstrc-filename,
        read_pos_start  LIKE sapwlsfihd-recordno,
        read_pos_end    LIKE sapwlsfihd-recordno,
      END   OF rec_statistic.
*--- Data for new version of SAPWL_STATREC_READ(_FILE)
DATA  start_read_recordno          LIKE  sapwlsfidx-recordno.
DATA:  continue_recordno_forward   LIKE  sapwlsfihd-recordno,
       continue_recordno_backward  LIKE  sapwlsfihd-recordno.
DATA  read_direction               LIKE  sapwlpstrc-flag VALUE 'X'.
DATA  eof_reached                  LIKE  sapwlpstrc-flag.
DATA: save_continue_recordno_backw LIKE  sapwlsfihd-recordno,
      save_continue_recordno_forw  LIKE  sapwlsfihd-recordno.
