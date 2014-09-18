*--Unterprogramme fuer Hitlistenauswertung (auch Einzelsatzauswertung)-
*
*
*------------------------------------------------------------------
* WRITE-HIT1: Hitliste 1 ausgeben
*------------------------------------------------------------------
*
FORM write-hit1.
  DATA: colorflag, cutflag.

  IF shortreportflag IS INITIAL.
    REFRESH ex.
*-- Transactioncode Profile und Appl.Workl.Overview für CPI-C, RFC
*   und ALE ausblenden
    IF tasktype = 'C' OR tasktype = 'R' OR tasktype = 'L'.
      ex-ex = 'TCOD'.
      APPEND ex.
      ex-ex = 'RAPP'.
      APPEND ex.
    ENDIF.

    SET PF-STATUS '0010' EXCLUDING ex.
    SET TITLEBAR '010'.
  ELSE.
    NEW-PAGE.
    namelength_uline = 63 + namelength_tcode + namelength_report.
  ENDIF.

  LOOP AT hitl1.                       "Ausgabeschleife HITL1
    index = sy-tabix.                  "Counter fuer Einzelanzeige.
*   move-corresponding hitl1 to stats.
    PERFORM move_hitl_to_stats USING hitl1.
    PERFORM flip_flop USING colorflag.
    PERFORM write-hitl1-record.        "FORM Routine Einzelanzeige.
    CLEAR stats.
*---Bei Kurzbericht nur die ersten 20 Zeilen---------------------------
    IF shortreportflag = 'X' AND sy-tabix >= 20.
      ULINE AT (namelength_uline).
*      skip 1.
      WRITE /00 ' Only top 20 entries are displayed.'.
      SKIP 2.
      cutflag = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF shortreportflag IS INITIAL OR cutflag IS INITIAL.
    ULINE AT /(namelength_uline).
  ENDIF.

ENDFORM.
*
*------------------------------------------------------------------
* WRITE-HIT2: Hitliste 2 ausgeben
*------------------------------------------------------------------
*
FORM write-hit2.
  DATA: colorflag, cutflag.

  IF shortreportflag IS INITIAL.
    REFRESH ex.
*-- Transactioncode Profile und Appl.Workl.Overview für CPI-C, RFC
*   und ALE ausblenden
    IF tasktype = 'C' OR tasktype = 'R' OR tasktype = 'L'.
      ex-ex = 'TCOD'.
      APPEND ex.
      ex-ex = 'RAPP'.
      APPEND ex.
    ENDIF.

    SET PF-STATUS '0090' EXCLUDING ex.
    SET TITLEBAR '011'.
  ELSE.
    NEW-PAGE.
    namelength_uline = 73 + namelength_report + namelength_tcode.
  ENDIF.

  LOOP AT hitl2.
    index = sy-tabix.                  "Counter fuer Einzelanzeige
    PERFORM move_hitl_to_stats USING hitl2.
    PERFORM flip_flop USING colorflag.
    PERFORM write-hitl2-record.
*---Bei Kurzbericht nur die ersten 20 Zeilen---------------------------
    IF shortreportflag = 'X' AND sy-tabix >= 20.
      ULINE AT (namelength_uline).
      WRITE /02 'Only top 20 entries are displayed.'.
      cutflag = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF shortreportflag IS INITIAL OR cutflag IS INITIAL.
    ULINE AT /(namelength_uline).
  ENDIF.
ENDFORM.
*
*------------------------------------------------------------------
* DISPLAY-HITLIST: Verzweigungs-Hitliste ausgeben
*------------------------------------------------------------------
*
FORM display-hitlist.
  DATA: help TYPE p,
        help1 TYPE p,
        pf_status(8),                  " B20K007183
        sortname LIKE sta2-sortname.

  sy-lsind = 1.
  sortname = sta2-sortname.
  PERFORM head-detail.                 "Kopfleiste
  sta2-sortname = sortname.

  IF status = 'SING'.
    PERFORM set_hitl_status USING 'DETAIL' steuer.
    CASE steuer.
      WHEN 'RFC'.
        PERFORM rfc_detail.
      WHEN 'RFCI'.
        PERFORM rfc_int_detail.
      WHEN 'SPOO'.
        PERFORM spool_detail.
      WHEN 'NETH'.
*       pf_status = '0020'.            " B20K007183
*       set pf-status pf_status.       " B20K007183
        PERFORM netz-detail.
      WHEN 'DBCL'.
*       pf_status = '0021'.            " B20K007183
*       set pf-status pf_status.       " B20K007183
        PERFORM dbcl-detail.
      WHEN 'MATC'.
*       pf_status = '0022'.            " B20K007183
*       set pf-status pf_status.       " B20K007183
        PERFORM matc-detail.
      WHEN 'DISP'.
*       pf_status = '0023'.            " B20K007183
*       set pf-status pf_status.       " B20K007183
        PERFORM disp-detail.
      WHEN 'DB5F'.
*       pf_status = '0024'.            " B20K007183
*       set pf-status pf_status.       " B20K007183
        PERFORM db5f-detail.
      WHEN 'STOR'.
*       pf_status = '0025'.            " B20K007183
*       set pf-status pf_status.       " B20K007183
        PERFORM stor-detail.
      WHEN 'ALRC'.
*       pf_status = '0027'.
*       set pf-status pf_status.
        PERFORM disp-detail.
        PERFORM dbcl-detail.
        PERFORM stor-detail.
        PERFORM db5f-detail.
        PERFORM rfc_detail.
        PERFORM rfc_int_detail.
        PERFORM netz-detail.
        PERFORM matc-detail.
        PERFORM spool_detail.
    ENDCASE.
  ELSE.
    PERFORM set_hitl_status USING 'MAIN' steuer.
*   set pf-status '0020' excluding steuer.
    CASE steuer.
      WHEN 'RFC'.
        PERFORM rfc_hitl_detail.
      WHEN 'DBCL'.
        PERFORM dbcl-detail.
      WHEN 'MATC'.
        PERFORM matc-detail.
      WHEN 'DISP'.
        PERFORM disp-detail.
      WHEN 'DB5F'.
        PERFORM db5f-detail.
      WHEN 'STOR'.
        PERFORM stor-detail.
      WHEN 'NETH'.
        PERFORM netz-detail.
      WHEN 'ALRC'.
        PERFORM disp-detail.
        PERFORM dbcl-detail.
        PERFORM stor-detail.
        PERFORM netz-detail.
        PERFORM rfc_hitl_detail.
        PERFORM db5f-detail.
        PERFORM matc-detail.
    ENDCASE.
  ENDIF.
*
  help = sy-tabix + headlinesize.
  help1 = help MOD sy-srows.
  seite = ( help - help1 ) / sy-srows.
  ADD 1 TO seite.
  sy-pagno = seite.                    "Blaettern in der Grundliste
ENDFORM.
*
*
*------------------------------------------------------------------
* HEAD-HIT1: Listueberschrift Hitliste 1
*------------------------------------------------------------------
*
FORM head-hit1.
  DATA  string(20).
  DATA  position TYPE i.
  DATA  length   TYPE i.
  DATA  save_sytabix LIKE sy-tabix.
check  p_p1 eq '' and p_p2 eq ''.
  IF shortreportflag <> 'X'.
    namelength_uline = 109 + namelength_tcode + namelength_report.
    NEW-PAGE LINE-SIZE namelength_uline.
  ELSE.
    namelength_uline = 63 + namelength_tcode + namelength_report.
    NEW-PAGE LINE-SIZE namelength_uline.
  ENDIF.

  PERFORM set_int_header.

  IF status = 'SING'.                  "Anzeige Einzelrecords RSSTAT20
    modline      = 7.
    save_sytabix = sy-tabix.           "FBST. ändert sy-tabix!
    CALL FUNCTION 'SAPWL_INST_LONG'
         EXPORTING
              inst_short                    = rsystem
         IMPORTING
              inst_long                     = long_instance
              host_long                     = sta2-rsystem
         EXCEPTIONS
              could_not_determine_long_name = 1
              OTHERS                        = 2.
    IF sy-subrc <> 0.
      IF rsystem = cpuid.
        long_instance = myname. sta2-rsystem = myname(32).
      ELSE.
        long_instance = sta2-rsystem = rsystem.
      ENDIF.
    ENDIF.
    sy-tabix = save_sytabix.
    sta1-path    = statl-spath.

    WRITE: / 'Instance      :', long_instance,
           AT namelength_uline space,
           / 'Statistic file:', statl-spath,
           AT namelength_uline space,
           / 'Analyzed time :', statl-sdat NO-GAP,
             '/' NO-GAP, statl-stime, '-',
              statl-senddat NO-GAP, '/' NO-GAP, statl-sendtime.
    IF NOT ( statl-sresptime IS INITIAL AND
             statl-sdbtime   IS INITIAL AND
             statl-scputime  IS INITIAL AND
             statl-sbytes    IS INITIAL AND
             statl-schg      IS INITIAL     ).
      WRITE '  (with further selection criteria)'.
    ENDIF.
    WRITE AT namelength_uline space.
    SKIP.
  ENDIF.

  PERFORM set_nonint_header.           " B20K008345

  IF shortreportflag = 'X'.
    length = 2 + namelength_tcode.
    ULINE AT (namelength_uline).
    WRITE: /                                            sy-vline NO-GAP,
              (8)                 space,
           AT (length)            space,
           AT (namelength_report) space,
              (1)                 space,
              (4)                 space,
              (2)                 space,
              (12)                space      NO-GAP,    sy-vline NO-GAP,
              (8)                 'Response' NO-GAP,    sy-vline NO-GAP,
              (8)                 'Memory',
              (7)                 'CPU'      NO-GAP,    sy-vline NO-GAP,
            /                                           sy-vline NO-GAP,
              (8)                 'End time',
           AT (length)            'Tcode',
           AT (namelength_report) 'Program',
              (1)                 'T',
*              (4)                 'Scr.',
*              (2)                 'WP',
              (4)                 '',
              (2)                 '',
              (4)                 '',
              (2)                 '',
              (12)                'User'     NO-GAP,    sy-vline NO-GAP,
              (8)                 'time/ms'  NO-GAP,    sy-vline NO-GAP,
              (8)                 'used/kB',
              (7)                 'time/ms'  NO-GAP,    sy-vline NO-GAP.
  ELSE.
    position = 38 + namelength_tcode + namelength_report.
    NEW-LINE.
    WRITE:   AT position(8) 'Response',
                        (8) 'Memory',
                        (8) 'Wait    ',
                        (8) 'CPU     ',
                        (8) 'DB req. ',
                        (8) 'Load/Gen.',
                        (8) 'kBytes  ',
                        (8) 'Phys. db'.
  if P_DIST  eq 'X'.
    WRITE:  /2(8)                 '',
           AT (namelength_tcode)  'Tcode',
              space,
            At (namelength_report) 'Program',
              (1)                 'T',
*              (4)                 'Scr.',
*              (2)                 'Wp',
              (4)                 '',
              (2)                 '',
              (12)                'User',
              (8)                 'time(ms)',
              (8)                 'used(kB)',
              (8)                 'time(ms)',
              (8)                 'time(ms)',
              (8)                 'time(ms)',
              (8)                 'time(ms)',
              (8)                 'transfer',
              (8)                 'changes '.
   else.
       WRITE:  /2(8)                 'End time',
           AT (namelength_tcode)  'Tcode',
              space,
            At (namelength_report) 'Program',
              (1)                 'T',
*              (4)                 'Scr.',
*              (2)                 'Wp',
              (4)                 '',
              (2)                 '',
              (12)                'User',
              (8)                 'time(ms)',
              (8)                 'used(kB)',
              (8)                 'time(ms)',
              (8)                 'time(ms)',
              (8)                 'time(ms)',
              (8)                 'time(ms)',
              (8)                 'transfer',
              (8)                 'changes '.
  endif.
  ENDIF.

  IF sy-lsind < 1.
    IF status =  'SING'.

      sel = sel2 = statl.
     if p_dist eq 'X'.

     else.
      WRITE: /2(8)  sel-stime        INPUT INTENSIFIED COLOR 1
                                     USING EDIT MASK '__:__:__',
             AT (namelength_tcode)   sel-stcod
                                     INPUT INTENSIFIED COLOR 1,
                                     space,
             AT (namelength_report)  sel-sprogram
                                     INPUT INTENSIFIED COLOR 1,
             (1)                     sel-stask
                                     INPUT INTENSIFIED COLOR 1,
             (4)                     sel-sscreen
                                     INPUT INTENSIFIED COLOR 1,
             (2)                     sel-swpid
                                     INPUT INTENSIFIED COLOR 1,
             (12)                    sel-sbenu
                                     INPUT INTENSIFIED COLOR 1.
    endif.
    ENDIF.
    FORMAT INTENSIFIED OFF.
    ULINE AT /(namelength_uline).
  ELSE.
    PERFORM set_nonint_header.
    ULINE AT (namelength_uline).
  ENDIF.
ENDFORM.
*
*------------------------------------------------------------------
* HEAD-HIT2: Listueberschrift Hitliste 2
*------------------------------------------------------------------
*
FORM head-hit2.
  DATA  position TYPE i.
  DATA  length   TYPE i.

  IF shortreportflag <> 'X'.
    namelength_uline = 109 + namelength_tcode + namelength_report.
    NEW-PAGE LINE-SIZE namelength_uline.
    WRITE: /1 ' '.                     " DH 23.02.97
*  else.                                              " DH 23.02.97
  ENDIF.

*  write: /1 ' '.                                     " DH 23.02.97
* SUMMARY.
  FORMAT INTENSIFIED OFF.              " B20K008345
  IF shortreportflag = 'X'.
    namelength_uline = 73 + namelength_report + namelength_tcode.
    ULINE AT (namelength_uline).

    length = 2 + namelength_tcode.
    WRITE:  /                                           sy-vline NO-GAP,
              (8)                 space,
           AT (length)            space,
           AT (namelength_report) space,
              (1)                 space,
              (4)                 space,
              (2)                 space,
              (12)                space      NO-GAP,    sy-vline NO-GAP,
              (8)                 'DB'       NO-GAP,    sy-vline NO-GAP,
              (8)                 'DB'       NO-GAP,    sy-vline NO-GAP,
              (8)                 'Read'     NO-GAP,    sy-vline NO-GAP,
              (8)                 'Change  ' NO-GAP,    sy-vline NO-GAP,
            /                                           sy-vline NO-GAP,
              (8)                 'End time',
           AT (length)            'Tcode',
           AT (namelength_report) 'Program',
              (1)                 'T',
              (4)                 'Scr.',
              (2)                 'WP',
              (12)                'User'     NO-GAP,    sy-vline NO-GAP,
              (8)                 'requests' NO-GAP,    sy-vline NO-GAP,
              (8)                 'time/ms'  NO-GAP,    sy-vline NO-GAP,
              (8)                 'time/ms'  NO-GAP,    sy-vline NO-GAP,
              (8)                 'time/ms'  NO-GAP,    sy-vline NO-GAP.
  ELSE.
    position = 38 + namelength_tcode + namelength_report.
    WRITE:   /(9)                   ' End time',
           AT (namelength_tcode)     'Tcode',
                                     space,
           AT (namelength_report)    'Program ',
              (1)                    'T',
              (4)                    'Scr.',
              (2)                    'Wp',
              (11)                   'User     ',
              (8)                    ' DB time',
              (8)                    '   Reads',
              (8)                    'Readtime',
              (8)                    ' Changes',
              (8)                    'Chg.time',
              (8)                    'Waittime',
              (8)                    '  kBytes',
              (8)                    'Phys.chg'.
  ENDIF.
  IF sy-lsind < 1.
    IF shortreportflag = 'X'.
      FORMAT INTENSIFIED OFF. ULINE AT (namelength_uline).  " 75
    ELSE.
      FORMAT INTENSIFIED OFF. ULINE AT /(namelength_uline).
                                       " B20K008345
    ENDIF.
  ELSE.
    ULINE AT /(namelength_uline).
  ENDIF.

* detail.
ENDFORM.
*
*------------------------------------------------------------------
* WRITE-HITL1-RECORD: Zeilenausgabe Hitliste 1
*------------------------------------------------------------------
*
FORM write-hitl1-record.

  PERFORM move_stats_to_sta2.

  DIVIDE sta2-maxmem BY 1024.

  IF rperiod(1) = 'D' OR sta2-date = sy-datum OR status =  'SING'.
    IF sy-tabix = 1.
      sel2 = sel = statl.
      sel2-stime = sel-stime = sta2-endti.
    ENDIF.
   if P_DIST  eq 'X'.
    WRITE: / sy-vline, 2(8) ''.
   else.
    WRITE: / sy-vline, 2(8) sta2-endti USING EDIT MASK '__:__:__'.
   endif.
  ELSE.
    WRITE: / sy-vline, 2(2) sta2-date+6(2), 4(1) '.',
                       5(2) sta2-date+4(2).
  ENDIF.
*
  PERFORM check_record.
*

* Namensraumerweiterung

  namehelp_report = sta2-report.
  namehelp_tcode  = sta2-tcode.
*  CALL FUNCTION 'SAPWL_WORKLOAD_TRUNCATE_NAME'
*       EXPORTING
*            namelength       = namelength_flag
*            report           = namehelp_report
*            tcode            = namehelp_tcode
*       IMPORTING
*            truncated_report = namehelp_report
*            truncated_tcode  = namehelp_tcode
*       EXCEPTIONS
*            OTHERS           = 1.
  sta2-report = namehelp_report.
  sta2-tcode  = namehelp_tcode.

  IF shortreportflag = 'X'.
    WRITE: AT 11(namelength_tcode)   sta2-tcode,
                                     space,
            AT 17(namelength_report)  sta2-report,
*            AT 31(namelength_report)  sta2-report,

                (1)                  sta2-task,
*                (4)                  sta2-dynpronr,
*                (2)                  sta2-wp NO-SIGN,
                (4)                  space,
                (2)                  space,
                (12)                 sta2-account NO-GAP,
                                     sy-vline NO-GAP,
                (8)                  sta2-respti NO-GAP,
                                     sy-vline NO-GAP,
                (8)                  sta2-maxmem NO-GAP,
                (8)                  sta2-cputi NO-GAP,
                                     sy-vline.
  ELSE.
    WRITE: AT 11(namelength_tcode)   sta2-tcode,
                                     space,
           AT 31(namelength_report)  sta2-report,
                (1)                  sta2-task,
*               (4)                  sta2-dynpronr,
*               (2)                  sta2-wp NO-SIGN,
                (6)                  '',
*               (2)                  space,
                (12)                 sta2-account NO-GAP,
                                     sy-vline NO-GAP,
                (8)                  sta2-respti NO-GAP,
                                     sy-vline NO-GAP,
                (8)                  sta2-maxmem NO-GAP,
                                     sy-vline NO-GAP,
                (8)                  sta2-queueti NO-GAP,
                                     sy-vline NO-GAP,
                (8)                  sta2-cputi NO-GAP,
                                     sy-vline NO-GAP,
                (8)                  sta2-dbcallti NO-GAP,
                                     sy-vline NO-GAP,
                (8)                  loadgenti NO-GAP,
                                     sy-vline NO-GAP,
                (8)                  sta2-avbytes NO-GAP,
                                     sy-vline NO-GAP,
                (8)                  sta2-phychngcnt NO-GAP,
                                     sy-vline.
  ENDIF.
   l_cnt = l_cnt + 1.
   loop at  it_tab where name eq  sta2-report.
     FORMAT   reset.
     FORMAT COLOR  COL_POSITIVE.
     WRITE: / sy-vline, 2(8) ''.
     write : 50(25) it_tab-include.
     FORMAT  reset.
     l_cnt1 = l_cnt1 + 1.
   endloop.


  HIDE sy-tabix.

  MULTIPLY sta2-maxmem BY 1024.

ENDFORM.
*
*------------------------------------------------------------------
* WRITE-HITL2-RECORD: Zeilenausgabe Hitliste 2
*------------------------------------------------------------------
*
FORM write-hitl2-record.

  PERFORM move_stats_to_sta2.

  IF rperiod(1) = 'D' OR sta2-date = sy-datum OR status =  'SING'.
    WRITE: / sy-vline, 2(8) sta2-endti USING EDIT MASK '__:__:__'.
  ELSE.
    WRITE: / sy-vline, 2(2) sta2-date+6(2), 4(1) '.',
                       5(2) sta2-date+4(2).
  ENDIF.
*
  PERFORM check_record.
*

* Namensraumerweiterung

  namehelp_tcode  = sta2-tcode.
  namehelp_report = sta2-report.
  CALL FUNCTION 'SAPWL_WORKLOAD_TRUNCATE_NAME'
       EXPORTING
            namelength       = namelength_flag
            report           = namehelp_report
            tcode            = namehelp_tcode
       IMPORTING
            truncated_report = namehelp_report
            truncated_tcode  = namehelp_tcode
       EXCEPTIONS
            OTHERS           = 1.
  sta2-report = namehelp_report.
  sta2-tcode  = namehelp_tcode.

  IF shortreportflag = 'X'.
    WRITE: AT 11(namelength_tcode)  sta2-tcode,
                                    space,
           AT   (namelength_report) sta2-report,
                (1)                 sta2-task,
                (4)                 sta2-dynpronr,
                (2)                 sta2-wp NO-SIGN,
                (12)                sta2-account NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-dbcalls NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-dbcallti NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-readti NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-chngti NO-GAP,
                                    sy-vline.
  ELSE.
    WRITE: AT 11(namelength_tcode)  sta2-tcode,
                                    space,
           AT   (namelength_report) sta2-report,
                (1)                 sta2-task,
                (4)                 sta2-dynpronr,
                (2)                 sta2-wp NO-SIGN,
                (12)                sta2-account NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-dbcallti NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-readcnt NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-readti NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-chngcnt NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-chngti NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-queueti NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-avbytes NO-GAP,
                                    sy-vline NO-GAP,
                (8)                 sta2-phychngcnt NO-GAP,
                                    sy-vline.
  ENDIF.
  HIDE sy-tabix.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM CHECK_RECORD                                             *
*---------------------------------------------------------------------*
FORM check_record.
  DATA: BEGIN OF helpchr,
          ch(4),
        END OF helpchr,
        hexer(2) TYPE x.
*
  helpchr-ch = sta2-wpid(2).           "Work Process Nummer
  MOVE helpchr TO sta2-wpid.
  sta2-wp = sta2-wpid.
*
  PERFORM tt_convert_number_to_letter USING sta2-tasktype sta2-task.

  sta2-chngcnt    = sta2-inscnt + sta2-delcnt + sta2-updcnt.

  sta2-readcnt    = sta2-readseqcnt + sta2-readdircnt.

  sta2-mccalls    = sta2-mcreadcnt + sta2-mcupdcnt +
                    sta2-mcinscnt  + sta2-mcdelcnt.

  sta2-phychngcnt = sta2-phyinscnt + sta2-phyupdcnt + sta2-phydelcnt.

*--Korr. B20K002350 QQI 160393---Neue Berechnung DB Calls dir. --------*
*DEL STA2-PHYREADDIR = STA2-READDIRCNT - STA2-READDIRBUF.

*  STA2-PHYREADDIR = 2 * ( STA2-READDIRCNT - STA2-READDIRBUF ).
*  IF STA2-PHYREADDIR > STA2-PHYREADCNT.
*    STA2-PHYREADDIR = STA2-PHYREADCNT.
*  ENDIF.
*  SUBTRACT STA2-PHYREADDIR FROM STA2-PHYREADCNT.
  CLEAR sta2-phyreaddir.
  sta2-phycalls = sta2-phyreadcnt + sta2-phyreaddir + sta2-phychngcnt.

  sta2-bufcalls =  sta2-readdirbuf + sta2-readseqbuf.

  sta2-reccnt =
           sta2-readdirrec + sta2-readseqrec + sta2-delrec +
           sta2-insrec + sta2-updrec.

*
  sta2-clock    = sta2-endti.

  sta2-dispti   = sta2-respti - sta2-queueti.

* BINK086610 Anfang (G. Derwand)
* sta2-readti   = sta2-readseqti + sta2-readdirti + sta2-txxxti.
  sta2-readti   = sta2-readseqti + sta2-readdirti.
* BINK086610 Ende
  sta2-chngti   = sta2-delti + sta2-insti + sta2-updti.

  sta2-dbcallti = sta2-readti + sta2-mcti +
                  sta2-chngti +  sta2-committi.

* sta2-rollti =  sta2-rollinti  + sta2-rolloutti.
  sta2-rollti =  sta2-rollinti.

  sta2-loadti =  sta2-cualoadti + sta2-reploadti + sta2-dynploadti.

  loadgenti   =  sta2-loadti + sta2-generateti.

  sta2-procti =  sta2-respti   -  sta2-generateti - sta2-lockti -
                 sta2-queueti  -  sta2-loadti     - sta2-dbcallti -
*                sta2-rollti   -        "BINK077407
                 sta2-rollinti.                             "BINK077407

  IF sta2-tasktype NE tt_rfc AND sta2-tasktype NE tt_cpic AND
     sta2-tasktype NE tt_ale.
    SUBTRACT sta2-rolled_out FROM sta2-procti.
    ADD      sta2-rolled_out TO   sta2-rollti.
  ENDIF.

  IF sta2-tcode = 'SE41'.
    sta2-cuascnt = sta2-dsqlcnt.
    sta2-dsqlcnt = 0.
  ELSE.
    sta2-cuascnt = 0.
  ENDIF.

  sta2-bytes =
             sta2-ntabcnt  + sta2-quecnt   + sta2-cpiccnt  +
             sta2-ddiccnt  + sta2-dynpscnt + sta2-dynplcnt +
             sta2-abapscnt + sta2-abaplcnt + sta2-cualcnt  +
             sta2-dsqlcnt.

  sta2-avbytes =  ( sta2-bytes * 10 ) / 1024.
*

  IF sta2-dbcalls > 0.
    sta2-avtdb = ( 10 * sta2-dbcallti ) / sta2-dbcalls.
  ELSE.
    sta2-avtdb = 0.
  ENDIF.

  IF sta2-readdircnt <> 0.
    sta2-avtreaddir = ( sta2-readdirti * 10 ) / sta2-readdircnt.
  ELSE.
    sta2-avtreaddir = 0.
  ENDIF.

  IF sta2-readseqcnt <> 0.
    sta2-avtreadseq = ( sta2-readseqti * 10 ) / sta2-readseqcnt.
  ELSE.
    sta2-avtreadseq = 0.
  ENDIF.

  IF sta2-updcnt <> 0.
    sta2-avtupd  = ( sta2-updti * 10 ) / sta2-updcnt.
  ELSE.
    sta2-avtupd = 0.
  ENDIF.

  IF sta2-delcnt <> 0.
    sta2-avtdel = ( sta2-delti * 10 ) / sta2-delcnt.
  ELSE.
    sta2-avtdel = 0.
  ENDIF.

  IF sta2-inscnt <> 0.
    sta2-avtins = ( sta2-insti * 10 ) / sta2-inscnt.
  ELSE.
    sta2-avtins = 0.
  ENDIF.
*-Rollbereichsgrößen
  helpchr-ch = sta2-rollkey(4).
  MOVE helpchr TO hexer.
  sta2-mainmode = hexer.
  helpchr-ch = sta2-rollkey+4(4).
  MOVE helpchr TO hexer.
  sta2-submode = hexer.
*-Authorisierung anzeigen von Usernamen
  IF noauthflag = 'X' AND              "Authorisierung nicht vorhanden
    sta2-account <> sy-uname.
    sta2-terminalid = '   -?-      '.
    sta2-account    = '   -?-      '.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM TEXT_DISPLAY                                             *
*---------------------------------------------------------------------*
*       Display text for Report, Tcode, Screen                        *
*---------------------------------------------------------------------*
FORM text_display.
  DATA  flag.

  CLEAR: tcodetext, reporttext, screentext, devctext,
         funtext1, funtext2, trdirtext, trcltext.

  PERFORM move_stats_to_sta2.
*
  PERFORM check_record.
  PERFORM screen_text_information USING sta2-tcode sta2-report
          sta2-dynpronr sta2-syucomm sta2-sypfkey
          tcodetext reporttext screentext devctext funtext1 funtext2
          trcltext  trdirtext.
  SET PF-STATUS '0015'.
  SET TITLEBAR '030'.
  WINDOW STARTING AT 1 6 ENDING AT 76 12.
  FORMAT RESET.

* Namensraumerweiterung
  namehelp_tcode = sta2-tcode.
  namehelp_report = sta2-report.
  IF namelength_flag < 1.
    CALL FUNCTION 'SAPWL_WORKLOAD_TRUNCATE_NAME'
         EXPORTING
              report           = namehelp_report
              tcode            = namehelp_tcode
*             TABLE            = ' '
*             DEVC             = ' '
         IMPORTING
              truncated_report = namehelp_report
              truncated_tcode  = namehelp_tcode
*             TRUNCATED_TABLE  =
*             TRUNCATED_DEVC   =
         EXCEPTIONS
              OTHERS           = 1.
  ENDIF.

  CALL FUNCTION 'SAPWL_INST_LONG'
       EXPORTING
            inst_short                    = rsystem
       IMPORTING
            inst_long                     = long_instance
*           HOST_LONG                     =
       EXCEPTIONS
            could_not_determine_long_name = 1
            OTHERS                        = 2.
  IF sy-subrc <> 0.
    long_instance = rsystem.
  ENDIF.
  WRITE: /1(20) long_instance,         " rsystem,
                sta2-endti,
         AT (namelength_tcode) namehelp_tcode,
         AT (namelength_report) namehelp_report,
            sta2-dynpronr,
            sta2-mandt,
            sta2-account.
  ULINE AT /(namelength_uline).
*
  IF tcodetext NE space OR reporttext NE space OR screentext NE
  space OR devctext NE space.
    IF devctext NE space.
      PERFORM flip_flop USING flag.

* Namensraumerweiterung
      namehelp_devc = tadir-devclass.
      IF namelength_flag < 1.
        CALL FUNCTION 'SAPWL_WORKLOAD_TRUNCATE_NAME'
             EXPORTING
*                 report           = ' '
*                 TCODE            = ' '
*                 TABLE            = ' '
                  devc             = namehelp_devc
             IMPORTING
*                 truncated_report =
*                 TRUNCATED_TCODE  =
*                 TRUNCATED_TABLE  =
                  truncated_devc   = namehelp_devc
             EXCEPTIONS
                  OTHERS           = 1.
      ENDIF.
      WRITE: /01 'Dev. class ', 14 ':', 16 devctext,
             AT 77(namelength_devc) namehelp_devc.
    ENDIF.
    IF tcodetext NE space.
      PERFORM flip_flop USING flag.

* Namensraumerweiterung
      namehelp_tcode = sta2-tcode.
      IF namelength_flag < 1.
        CALL FUNCTION 'SAPWL_WORKLOAD_TRUNCATE_NAME'
             EXPORTING
*                  report           = namehelp_report
                  tcode            = namehelp_tcode
*                  table            = namehelp_table
*                  devc             = namehelp_devc
             IMPORTING
*                  truncated_report = namehelp_report
                  truncated_tcode  = namehelp_tcode
*                  truncated_table  = namehelp_table
*                  truncated_devc   = namehelp_devc
             EXCEPTIONS
                  OTHERS           = 1.
      ENDIF.

      WRITE: /01 'Transaction', 14 ':', 16(69) space, 16 tcodetext,
              AT 77(namelength_tcode) namehelp_tcode.
    ENDIF.
    IF trcltext NE space.
      PERFORM flip_flop USING flag.
      WRITE: /01 'Program class', 14 ':', 16(60) trcltext,
      77(8) trclt-code.
    ENDIF.
*
    IF reporttext NE space.
      PERFORM flip_flop USING flag.

* Namensraumerweiterung
      namehelp_report = sta2-report.
      IF namelength_flag < 1.
        CALL FUNCTION 'SAPWL_WORKLOAD_TRUNCATE_NAME'
             EXPORTING
                  report           = namehelp_report
*                  tcode            = namehelp_tcode
*                  table            = namehelp_table
*                  devc             = namehelp_devc
             IMPORTING
                  truncated_report = namehelp_report
*                  truncated_tcode  = namehelp_tcode
*                  truncated_table  = namehelp_table
*                  truncated_devc   = namehelp_devc
             EXCEPTIONS
                  OTHERS           = 1.
      ENDIF.

      WRITE: /01 'Program    ', 14 ':', 16 reporttext(60),
             AT 77(namelength_report) namehelp_report.
    ENDIF.
*
    IF trdirtext NE space.
      WRITE: /1 'Created/Chgd', 14 ':', 16(69) trdirtext.
    ENDIF.
*
    IF screentext NE space.
      PERFORM flip_flop USING flag.
      WRITE: /01 'Screen     ', 14 ':', 16 screentext,
              77(8) sta2-dynpronr.
    ENDIF.
*
    IF funtext1 NE space OR funtext2 NE space.
      PERFORM flip_flop USING flag.
      WRITE: /01 'Function   ', 14 ':', 16(60) space, 16 funtext2,
              77(8) sta2-syucomm.
    ENDIF.
  ELSE.
    WRITE: /01 'No information available for selected record'.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM SCREEN_TEXT_INFORMATION                                  *
*---------------------------------------------------------------------*
*       Einlesen von Texten zum Einzelsatzobjekt                      *
*---------------------------------------------------------------------*
*  -->  TCODE                                                         *
*  -->  REPORT                                                        *
*  -->  DYNPRONR                                                      *
*  -->  TCODETEXT                                                     *
*  -->  REPORTTEXT                                                    *
*  -->  SCREENTEXT                                                    *
*  -->  DEVCTEXT                                                      *
*  -->  FUNTEXT1                                                      *
*  -->  FUNTEXT2                                                      *
*  -->  TRCLTEXT                                                      *
*  -->  TRDIRTEXT                                                     *
*---------------------------------------------------------------------*
FORM screen_text_information USING tcode report dynpronr function
            funcpool
            tcodetext reporttext screentext devctext funtext1 funtext2
            trcltext trdirtext.
  DATA: hlptbx LIKE sy-tabix,
        rdirtext(60),
        funkey(4).
*-Import Structure for Function codes (SE41)
  DATA: BEGIN OF fun OCCURS 0,
          code(4),
          no(2),
          type,
          shorttext(20),
          longtext(20),
          textnr(4),
          path,
          modif,
        END OF fun,
        eudbkey(31).
*
  hlptbx = sy-tabix.
  PERFORM special_reports_retranslate USING report tcode.
*
*--Tcode text read
  IF tcode NE space.
    SELECT SINGLE * FROM tstct WHERE sprsl = sy-langu
                               AND   tcode = tcode.
    IF sy-subrc = 0.
      tcodetext = tstct-ttext.
    ELSE.
      tcodetext = space.
    ENDIF.
  ENDIF.
*
*--Tcode application class read text
  IF tcode NE space.
    SELECT SINGLE * FROM tadir WHERE pgmid = 'R3TR'
                               AND object  = 'TRAN'
                               AND obj_name    = tcode.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM tdevct WHERE spras = sy-langu
                                  AND   devclass = tadir-devclass.
      IF sy-subrc = 0.
        devctext = tdevct-ctext.
      ELSE.
        devctext = space.
      ENDIF.
    ELSE.
      devctext = space.
    ENDIF.
  ENDIF.
*
*--Report text read
  IF report NE space.
    READ TEXTPOOL report INTO text_tab LANGUAGE sy-langu.
    IF sy-subrc EQ 0.
      READ TABLE text_tab WITH KEY 'R' BINARY SEARCH.
      IF text_tab-entry NE space.
        reporttext = text_tab-entry.
      ELSE.
        reporttext = space.
      ENDIF.
    ELSE.
      reporttext = space.
    ENDIF.
*---Trdir informations
    SELECT SINGLE * FROM trdir WHERE name = report.
    IF sy-subrc = 0.
      rdirtext(12) = trdir-cnam.
      rdirtext+12(1) = '/'.
      rdirtext+13(12) = trdir-unam.
      WRITE trdir-udat TO rdirtext+25.
      trdirtext = rdirtext.
*---Reporting class information
      IF trdir-clas NE space.
        SELECT SINGLE * FROM trclt WHERE lang = sy-langu AND
                                         code = trdir-clas.
        IF sy-subrc = 0.
          trcltext = trclt-text.
        ELSE.
          trcltext = space.
        ENDIF.
      ELSE.
        trcltext = space.
      ENDIF.

    ELSE.
      trdirtext = space.
    ENDIF.
  ENDIF.
*
*--Function text read
  IF funcpool NE space AND function NE space.
    CLEAR: eudbkey.
    eudbkey(8) = funcpool.
    eudbkey+30(1) = sy-langu.
    IMPORT fun FROM DATABASE eudb(cu) ID eudbkey.
    IF sy-subrc EQ 0.
      SORT fun.
      funkey = function.
      READ TABLE fun WITH KEY funkey(4) BINARY SEARCH.
      IF sy-subrc = 0.
        funtext1 = fun-shorttext.
        funtext2 = fun-longtext.
      ELSE.
        funtext1 = space.
        funtext2 = space.
      ENDIF.
    ELSE.
      funtext1 = fun-shorttext.
      funtext2 = fun-longtext.
    ENDIF.
  ENDIF.
*
*--Screen text read
  IF dynpronr NE space AND report NE space.
    SELECT SINGLE * FROM d020t WHERE prog = report
                               AND   dynr  = dynpronr
                               AND   lang  = sy-langu.
    IF sy-subrc = 0.
      screentext = d020t-dtxt.
    ELSE.
      screentext = space.
    ENDIF.
  ENDIF.
*--Screen text read
  IF dynpronr NE space AND report NE space.
    SELECT SINGLE * FROM d020t WHERE prog = report
                               AND   dynr  = dynpronr
                               AND   lang  = sy-langu.
    IF sy-subrc = 0.
      screentext = d020t-dtxt.
    ELSE.
      screentext = space.
    ENDIF.
  ENDIF.
*
  sy-tabix = hlptbx.
ENDFORM.
*
*------------------------------------------------------------------
* DISP-DETAIL: Verzweigungshitliste Disp.Time analyse
*------------------------------------------------------------------
*
FORM disp-detail.
  IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
    FORMAT RESET.
    SKIP 1.
    FORMAT INTENSIFIED ON.
    WRITE: /06 'Analysis of time in work process'.
    FORMAT INTENSIFIED OFF.
    ULINE /5(81).
  ENDIF.
* Zeile 01
  WRITE: /5 sy-vline, 7(23) 'CPU time                 ', 40 'ms'.
  WRITE: 32(8) sta2-cputi.
  WRITE: 47(25) 'Number      Roll ins     '.
  WRITE: 73(8) sta2-rollincnt, 85 sy-vline.
* Zeile 02
  WRITE: /5 sy-vline, 7(23) 'RFC+CPIC time'            , 40 'ms'.
  WRITE  32(8) sta2-cpicti.
  WRITE:  5 sy-vline, 47(25) '            Roll outs    '.
  WRITE:  73(8) sta2-rolloutcnt, 85 sy-vline.
* Zeile 03
  WRITE: /5 sy-vline.
  WRITE: 47(25) '            Enqueues     '.
  WRITE: 73(8) sta2-lockcnt, 85 sy-vline.
** Zeile 04
*  write: /5 sy-vline, 6(38) '--------------------------------------',
*          44 sy-vline, 85 sy-vline.
*  write:  9 'Response time',40 'ms'.
*  write: 32(8) sta2-respti.
** Zeile 05
*  write: /5 sy-vline, 7(22) 'Wait for work process ', 40 'ms',
*         44 sy-vline, 85 sy-vline.
*  write: 32(8) sta2-queueti.
*  write:  47(25) 'Load time   Program      ', 82 'ms', 85 sy-vline.
*  write: 73(8) sta2-reploadti, 85 sy-vline.
** Zeile 06
*  write: /5 sy-vline, 7(25) 'Processing time        ', 40 'ms',
*         44 sy-vline.
*  write: 32(8)  sta2-procti.
*  write: 47(25) '            Screen       ', 82 'ms'.
*  write: 73(8) sta2-dynploadti, 85 sy-vline.
** Zeile 07
*  write: /5 sy-vline, 7(25) 'Load time                ', 40 'ms',
*         44 sy-vline.
*  write: 32(8)  sta2-loadti.
*  write: 47(25) '            CUA interf.  ', 82 'ms'.
*  write: 73(8) sta2-cualoadti, 85 sy-vline.
** Zeile 08
*  write: /5 sy-vline, 7(25) 'Generating time          ', 40 'ms',
*         44 sy-vline.
*  write: 32(8) sta2-generateti, 85 sy-vline.
** Zeile 09
*  if sta2-tasktype = tt_rfc or sta2-tasktype = tt_cpic or
*     sta2-tasktype = tt_ale.
*    write: /5 sy-vline, 7(25) 'Roll (in) time           ', 40 'ms',
*           44 sy-vline, 85 sy-vline.
*  else.
*    write: /5 sy-vline, 7(25) 'Roll (in+wait) time      ', 40 'ms',
*           44 sy-vline, 85 sy-vline.
*  endif.
*  write: 32(8) sta2-rollti,  85 sy-vline.
*  write: 47(25) 'Roll time   Out          ', 82 'ms'.
*  write: 73(8) sta2-rolloutti, 85 sy-vline.
** Zeile 10
*  write: /5 sy-vline, 7(25)  'Database request time    ', 40 'ms',
*         44 sy-vline.
*  write: 32(8)  sta2-dbcallti.
*  write: 47(25) '            In           ', 82 'ms'.
*  write: 73(8) sta2-rollinti, 85 sy-vline.
** Zeile 11
*  write: /5 sy-vline, 7(25) 'Enqueue time             ', 40 'ms',
*         44 sy-vline.
*  write: 32(8) sta2-lockti.
*  write: 47(25) '            Wait         ', 82 'ms',
*         73(8) sta2-rolled_out, 85 sy-vline.
*
                                       "Frontend
* Zeile 04
  WRITE: /5 sy-vline, 85 sy-vline.
* Zeile 05
  WRITE: /5 sy-vline, 6(38) '--------------------------------------',
          44 sy-vline.
  WRITE:  9 'Response time',40 'ms'.
  WRITE: 32(8) sta2-respti.
  WRITE: 47(25) 'Load time   Program      ', 82 'ms'.
  WRITE: 73(8) sta2-reploadti, 85 sy-vline.
* Zeile 6
  WRITE: /5 sy-vline, 44 sy-vline.
  WRITE: 47(25) '            Screen       ', 82 'ms'.
  WRITE: 73(8) sta2-dynploadti, 85 sy-vline.
* Zeile 07
  WRITE: /5 sy-vline, 7(22) 'Wait for work process ', 40 'ms',
         44 sy-vline.
  WRITE: 32(8) sta2-queueti.
  WRITE: 47(25) '            CUA interf.  ', 82 'ms'.
  WRITE: 73(8) sta2-cualoadti, 85 sy-vline.
* Zeile 08
  WRITE: /5 sy-vline, 7(25) 'Processing time        ', 40 'ms',
         44 sy-vline, 85 sy-vline.
  WRITE: 32(8)  sta2-procti.
* Zeile 09
  WRITE: /5 sy-vline, 7(25) 'Load time                ', 40 'ms',
         44 sy-vline.
  WRITE: 32(8)  sta2-loadti.
  WRITE: 47(25) 'Roll time   Out          ', 82 'ms'.
  WRITE: 73(8) sta2-rolloutti, 85 sy-vline.
* Zeile 10
  WRITE: /5 sy-vline, 7(25) 'Generating time          ', 40 'ms',
         44 sy-vline, 85 sy-vline.
  WRITE: 32(8) sta2-generateti.
  WRITE: 47(25) '            In           ', 82 'ms'.
  WRITE: 73(8) sta2-rollinti.
* Zeile 11
  IF sta2-tasktype = tt_rfc OR sta2-tasktype = tt_cpic OR
     sta2-tasktype = tt_ale.
    WRITE: /5 sy-vline, 7(25) 'Roll (in) time           ', 40 'ms',
           44 sy-vline.
  ELSE.
    WRITE: /5 sy-vline, 7(25) 'Roll (in+wait) time      ', 40 'ms',
           44 sy-vline.
  ENDIF.
  WRITE: 32(8) sta2-rollti.
  WRITE: 47(25) '            Wait         ', 82 'ms',
         73(8) sta2-rolled_out, 85 sy-vline.
* Zeile 12
  WRITE: /5 sy-vline, 7(25)  'Database request time    ', 40 'ms',
         44 sy-vline, 85 sy-vline.
  WRITE: 32(8)  sta2-dbcallti.
* Zeile 13
  WRITE: /5 sy-vline, 7(25) 'Enqueue time             ', 40 'ms',
         44 sy-vline.
  WRITE: 32(8) sta2-lockti.
  WRITE: 47(25) 'Frontend    No.roundtrips'.
  WRITE: 73(8) sta2-guicnt, 85 sy-vline.
* Zeile 14
  WRITE: /5 sy-vline, 44 sy-vline.
  WRITE: 47(25) '            GUI time   ', 82 'ms',
         71(10) sta2-guitime, 85 sy-vline.
* Zeile15
  IF status = 'SING'.
    "GUI net time nur in Einzelsätzen bekannt
    WRITE: /5 sy-vline, 44 sy-vline.
    WRITE: 47(25) '            Net time   ', 82 'ms',
           71(10) sta2-guinettime, 85 sy-vline.
  ENDIF.
  ULINE /5(81).
ENDFORM.
*
*------------------------------------------------------------------
* DBCL-DETAIL: Verzweigungshitliste DB call Analyse
*------------------------------------------------------------------
*
FORM dbcl-detail.

  IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
    FORMAT RESET.
    SKIP 1.
  ENDIF.
  IF sta2-dbcalls <= 0.
    IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
      ULINE /5(81).
    ENDIF.
    WRITE:/5 sy-vline, 85 sy-vline,
           7 'No database requests for this record'.
    ULINE /5(81).
    EXIT.
  ENDIF.
  IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
    FORMAT INTENSIFIED ON.
    WRITE: /06 'Analysis of ABAP/4 database requests',
           '(only explicitly by application)'.
    FORMAT INTENSIFIED OFF.
    ULINE /5(81).
  ENDIF.
  WRITE:/5 sy-vline,
         7(29) 'Database requests total    ',
         33(12) sta2-dbcalls,
         53(15) 'Request time   ',
         68(12) sta2-dbcallti, 80 'ms',
         85 sy-vline.
  WRITE:/5 sy-vline,
         53(15) 'Matchcode time.',
         68(12) sta2-mcti, 80 'ms',
         85 sy-vline.
  WRITE:/5 sy-vline,
         53(15) 'Commit time    ',
         68(12) sta2-committi, 80 'ms',
         85 sy-vline.
  WRITE:/5 sy-vline, 85 sy-vline.
  WRITE:/5 sy-vline,
         7(29) 'Requests on T??? tables     ',
         33(12) sta2-txxxcnt,
         53(15) 'Request time   ',
         68(12) sta2-txxxti, 80 'ms',
         85 sy-vline.
  ULINE /5(81).
* PERFORM SET_NONINT_HEADER.           " k11K049873
  WRITE:/5 sy-vline,
          7(17) 'Type of          ',   23 sy-vline,
         24(9)  '         ',           33 sy-vline,
         34(9)  'Database ',           43 sy-vline,
         44(9)  ' Requests',           54 sy-vline,         "K11K059928
         55(9)  'Database ',           64 sy-vline,
         65(9)  ' Request ',           74 sy-vline,         "K11K059928
         75(9)  'Avg.time ',           85 sy-vline.

  WRITE:/5 sy-vline,
          7(17) 'ABAP/4 request   ',   23 sy-vline,
         24(9)  'Requests',            33 sy-vline,
         34(9)  '    rows ',           43 sy-vline,
         44(9)  'to buffer',           54 sy-vline,
         55(9)  '   calls ',           64 sy-vline,
         65(9)  'time(ms) ',           74 sy-vline,
         75(9)  'per req. ',           85 sy-vline.

  ULINE /5(81).                        " K11K049873
  WRITE:/5 sy-vline,
          7(17) 'Total            ',    23 sy-vline,
         24(10)  sta2-dbcalls,          33 sy-vline,
         34(10)  sta2-reccnt,           43 sy-vline,
         44(10)  sta2-bufcalls,         54 sy-vline,
         55(10)  sta2-phycalls,         64 sy-vline,
         65(10)  sta2-dbcallti,         74 sy-vline,
         75(10)  sta2-avtdb,            85 sy-vline.
  DETAIL.
  WRITE:/5 sy-vline,
                                       23 sy-vline,
                                       33 sy-vline,
                                       43 sy-vline,
                                       54 sy-vline,         "K11K059928
                                       64 sy-vline,
                                       74 sy-vline,         "K11K059928
                                       85 sy-vline.
  WRITE:/5 sy-vline,
          7(17) 'Direct read      ',    23 sy-vline,
         24(10)  sta2-readdircnt,       33 sy-vline,
         34(10)  sta2-readdirrec,       43 sy-vline,
         44(10)  sta2-readdirbuf,       54 sy-vline,
*         54(9)  STA2-PHYREADDIR,       63 SY-VLINE,
         55(10)  space,                 64 sy-vline,
         65(10)  sta2-readdirti,        74 sy-vline,
         75(10)  sta2-avtreaddir,       85 sy-vline.

  WRITE:/5 sy-vline,
          7(17) 'Sequential read  ',    23 sy-vline,
         24(10)  sta2-readseqcnt,       33 sy-vline,
         34(10)  sta2-readseqrec,       43 sy-vline,
         44(10)  sta2-readseqbuf,       54 sy-vline,
         55(10)  sta2-phyreadcnt,       64 sy-vline,
         65(10)  sta2-readseqti,        74 sy-vline,
         75(10)  sta2-avtreadseq,       85 sy-vline.

  WRITE:/5 sy-vline,
          7(17) 'Update           ',    23 sy-vline,
         24(10)  sta2-updcnt,           33 sy-vline,
         34(10)  sta2-updrec,           43 sy-vline,
         44(10)  '         ',           54 sy-vline,
         55(10)  sta2-phyupdcnt,        64 sy-vline,
         65(10)  sta2-updti,            74 sy-vline,
         75(10)  sta2-avtupd,           85 sy-vline.

  WRITE:/5 sy-vline,
          7(17) 'Delete           ',    23 sy-vline,
         24(10)  sta2-delcnt,           33 sy-vline,
         34(10)  sta2-delrec,           43 sy-vline,
         44(10)  '         ',           54 sy-vline,
         55(10)  sta2-phydelcnt,        64 sy-vline,
         65(10)  sta2-delti,            74 sy-vline,
         75(10)  sta2-avtdel,           85 sy-vline.

  WRITE:/5 sy-vline,
          7(17) 'Insert           ',    23 sy-vline,
         24(10)  sta2-inscnt,           33 sy-vline,
         34(10)  sta2-insrec,           43 sy-vline,
         44(10)  '         ',           54 sy-vline,
         55(10)  sta2-phyinscnt,        64 sy-vline,
         65(10)  sta2-insti,            74 sy-vline,
         75(10)  sta2-avtins,           85 sy-vline.
  ULINE /5(81).
  IF sta2-tablepoint > 0.
    WRITE: /5 sy-vline,
            7 'Note: Tables were saved in the tablebuffer.    ',
            85 sy-vline.
    ULINE /5(81).
  ENDIF.
ENDFORM.
*
*------------------------------------------------------------------
* MATC-DETAIL: Verzweigungshitliste Matchcode call analysis
*------------------------------------------------------------------
*
FORM matc-detail.
  IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
    FORMAT RESET.
    SKIP 1.
  ENDIF.
  IF sta2-mccalls <= 0.
    IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
      ULINE /5(81).
    ENDIF.
    WRITE: /5 sy-vline, 7 'No matchcode requests for this record',
           85 sy-vline.
    ULINE /5(81).
    EXIT.
  ENDIF.
  IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
    FORMAT INTENSIFIED ON.
    WRITE: /07 'Analysis of matchcode requests'.
    FORMAT INTENSIFIED OFF.
    ULINE /5(81).
  ENDIF.
  PERFORM set_nonint_header.
  WRITE:/9(16) 'Total requests ', 27(8) sta2-mccalls.
  WRITE: 05 sy-vline, 85 sy-vline.
  WRITE:/9(16) '      time     ', 27(8) sta2-mcti, 'ms'.
  WRITE: 05 sy-vline, 85 sy-vline.
  WRITE:/9(26) '-------------------------'.
  WRITE: 05 sy-vline, 85 sy-vline.
  WRITE:/9(16) 'Read           ', 27(8) sta2-mcreadcnt.
  WRITE: 05 sy-vline, 85 sy-vline.
  WRITE:/9(16) 'Update         ', 27(8) sta2-mcupdcnt.
  WRITE: 05 sy-vline, 85 sy-vline.
  WRITE:/9(16) 'Insert         ', 27(8) sta2-mcinscnt.
  WRITE: 05 sy-vline, 85 sy-vline.
  WRITE:/9(16) 'Delete         ', 27(8) sta2-mcdelcnt.
  WRITE: 05 sy-vline, 85 sy-vline.
  ULINE /5(81).
ENDFORM.
*
*------------------------------------------------------------------
* STOR-DETAIL: Verzweigungshitliste Storage Analyse
*------------------------------------------------------------------
*
FORM stor-detail.
  IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
    FORMAT RESET.
    SKIP 1.
    FORMAT INTENSIFIED ON.
    WRITE: /6 'Task and memory information'.
    FORMAT INTENSIFIED OFF.
    ULINE /05(81).
  ENDIF.
  IF status <> 'SING' AND rperiod(1) <> 'D'.
    WRITE: /05 sy-vline, 07(21) 'Time of day', 30 sta2-clock USING
    EDIT MASK '__:__:__', 85 sy-vline.
  ENDIF.
*
*---Batchjobs: Jobname und Stepnummer zuordnen   " 4.1994 QQI
  IF sta2-task <> 'U'.                 "Kein VB-Task
    IF sta2-task = 'B' AND sta2-btcstepnr > 0.
      sta1-tfirst = sta2-endti - ( sta2-respti / 1000 ).
      WRITE: /05 sy-vline, 07(22) 'Job step/name/start',
             (03)    sta2-btcstepnr,
             (32)    sta2-btcjobname,
             (10)    sta1-tfirst USING EDIT MASK '__:__:__',
             85      sy-vline.
      ULINE /05(81).
    ELSEIF sta2-task = 'Y' OR sta2-task = 'A'.              "BINK073862
      WRITE: /05 sy-vline, 85 sy-vline.
    ELSE.
      WRITE: /05 sy-vline, 07(21) 'Terminal Id',
             31(12)  sta2-terminalid, 85 sy-vline.
    ENDIF.
  ELSE.
  ENDIF.
*
  WRITE:/05 sy-vline, 07 'Terminal In-message  ',
         (11) sta2-inputlen, 'Bytes', 85 sy-vline.
*
  WRITE:/05 sy-vline, 07 'Terminal Out-message ',
         (11) sta2-outputlen, 'Bytes', 85 sy-vline.
  ULINE /05(81).
  WRITE: /05 sy-vline, 07(21) 'Client               ',
         36(3) sta2-mandt, 85 sy-vline..
  WRITE: /05 sy-vline, 07(21) 'Work process no      ',
         35(5) sta2-wp, 85 sy-vline.
  WRITE: /05 sy-vline, 07(21) 'Modus                ', 85 sy-vline.
  WRITE: 23(5) sta2-mainmode, 29 '/', 30(5) sta2-submode.
  WRITE: /05 sy-vline, 07(21) 'Task type            ', 85 sy-vline.
*
  CASE sta2-task.                      "Tasktypen bestimmen
    WHEN 'D'.
      WRITE: 33(06) 'Dialog'.
    WHEN 'U'.
      WRITE: 28(11) 'Update (V1)'.
    WHEN '2'.
      WRITE: 28(11) 'Update (V2)'.
    WHEN 'S'.
      WRITE: 33(06) ' Spool'.
    WHEN 'B'.
      WRITE: 33(06) ' Batch'.
    WHEN 'E'.
      WRITE: 33(07) ' Enqueue'.
    WHEN 'Y'.
      WRITE: 33(08) ' BufSync'.
    WHEN 'A'.
      WRITE: 33(09) ' AutoABAP'.                            "BINK073862
    WHEN 'I'.
      WRITE: 33(07) ' B.Input'.
    WHEN 'L'.
      WRITE  33(07) '   ALE'.
    WHEN 'R'.
      WRITE  33(07) '   RFC'.
    WHEN 'C'.
      WRITE  33(07) ' CPI-C'.
    WHEN OTHERS.
      WRITE: 33(06) 'Others'.
  ENDCASE.
*   "VB Schluessel            geaendert      B20K001672 QQI
  WRITE: /05 sy-vline, 07(21) 'Update time stamp    ', 85 sy-vline.
  IF sta2-v1sati > 0.
    sta2-updkey = sta2-v1sati.
    WRITE : 31 sta2-updkey    USING EDIT MASK '__:__:__', 39 '.',
            40 sta2-updkey+6(2).
    IF sta2-tasktype = '02'.
      WRITE : 46 sta2-terminalid.
    ENDIF.
  ELSE.
    WRITE : 35 'None'.
  ENDIF.
  ULINE /05(81).
*
  WRITE: /05 sy-vline, 07(32) 'Batch-input queue time  ',
         (11) sta2-queti, 'ms', 85 sy-vline.
  WRITE: /05 sy-vline, 07(32) 'RFC interface time ',
         (11) sta2-cpicti, 'ms', 85 sy-vline.

  WRITE: /05 sy-vline, 07(32) 'DDIC interface time  ',
         (11) sta2-ddicti, 'ms', 85 sy-vline.
  ULINE /05(81).
  WRITE: /05 sy-vline, 7(32) 'Total memory used',
         (11) sta2-maxmem, 'Bytes', 85 sy-vline.
  WRITE:/05 sy-vline, 07(32) 'Max. memory used in roll area',
         (11) sta2-maxroll, 'Bytes', 85 sy-vline.
  WRITE:/05 sy-vline, 07(32) 'Max. memory used in paging area',
         (11) sta2-maxpage, 'Bytes', 85 sy-vline.
  ULINE /5(53).
  WRITE:  5   sy-vline, 7(27) ' Max. extended memory used',
          58  sy-vline, 85 sy-vline,
         /5   sy-vline, 7(32) ' In session',
         (11) sta2-maxbytes, 'Bytes', 58 sy-vline, 85 sy-vline,
         /5   sy-vline, 7(32) ' In transaction',
         (11) sta2-maxbytesdi, 'Bytes', 58 sy-vline, 85 sy-vline.
  ULINE /5(81).
  WRITE: /5   sy-vline, 7(32) 'Extended memory in use',
         (11) sta2-usedbytes, 'Bytes', 85 sy-vline,
         /5   sy-vline, 7(32) 'Private memory in use',
         (11) sta2-privsum, 'Bytes', 85 sy-vline,
         /5   sy-vline, 7(32) 'Workprocess in PRIV mode'.
  IF sta2-privmode NE 0.
    WRITE: (11) 'Yes' RIGHT-JUSTIFIED, 85 sy-vline.
  ELSE.
    WRITE: (11) 'No' RIGHT-JUSTIFIED, 85 sy-vline.
  ENDIF.
  WRITE: /05 sy-vline, 7(32) 'Workprocess restarted'.
  IF sta2-restart NE 0.
    WRITE: (11) 'Yes' RIGHT-JUSTIFIED, 85 sy-vline.
  ELSE.
    WRITE: (11) 'No' RIGHT-JUSTIFIED, 85 sy-vline.
  ENDIF.
  ULINE /05(81).
  IF sta2-task = 'D' AND sta2-sypfkey <> space.
    WRITE: /05 sy-vline, 07 'TCode',
            32(20) sta2-tcode, 85 sy-vline.

    WRITE: /05 sy-vline, 07 'Dynpro No.',
            32(8) sta2-dynpronr, 85 sy-vline.

    WRITE: /05 sy-vline, 07 'CUA reference program',
           32(40) sta2-sypfkey, 85 sy-vline.

    WRITE: /05 sy-vline, 07 'CUA internal command ',
           32(20) sta2-syucomm, 85 sy-vline.
    ULINE /05(81).
  ENDIF.
ENDFORM.
*
*------------------------------------------------------------------
* DB5F-DETAIL: Verzweigungshitliste 5 Tables Analyse
*------------------------------------------------------------------
*
FORM db5f-detail.
  DATA  maxrcs(4).
  DATA  tabrecno TYPE i.
  LOCAL sta1.

  IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
    FORMAT RESET.
    SKIP.
  ENDIF.
*
*----Anzeige der 5 Zeit-intensivsten Tabellen bei Hitlisten-------------
  IF status <> 'SING'.
    IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
      FORMAT INTENSIFIED ON.
      WRITE: /06 'Top 5 tables by requested log. records'.
      FORMAT INTENSIFIED OFF.
      ULINE /05(91).
    ENDIF.
    IF sta2-tabname1 = '          '.
      WRITE: /05 sy-vline, 7 'No table requests for this record ',
              95 sy-vline.
*      IF STA2-TABNAME2+1(1) <> SPACE.
*        WRITE: /05 SY-VLINE,
*                  'Profile parameter stat/tabrec was set to',
*                  STA2-TABNAME2, 95 SY-VLINE.
*      ENDIF.
      ULINE /05(91).
    ELSE.
      WRITE: /07(20) 'Table       ', 29(10) 'Dir. reads',
              42(10) 'Seq. reads  ', 58(10) 'Changes    '.
      WRITE: 05 sy-vline, 95 sy-vline.
      ULINE /05(91).
      WRITE:
           /07(20) sta2-tabname1,   30(10) sta2-tab1dircnt,
            43(10) sta2-tab1seqcnt, 56(10) sta2-tab1updcnt.
      WRITE: 05 sy-vline, 95 sy-vline.
      IF sta2-tabname2+1(1) <> space.
        WRITE:
             /07(20) sta2-tabname2,   30(10) sta2-tab2dircnt,
              43(10) sta2-tab2seqcnt, 56(10) sta2-tab2updcnt.
      ENDIF.
      WRITE: 05 sy-vline, 95 sy-vline.
      IF sta2-tabname3 <> space.
        WRITE:
             /07(20) sta2-tabname3,   30(10) sta2-tab3dircnt,
              43(10) sta2-tab3seqcnt, 56(10) sta2-tab3updcnt.
      ENDIF.
      WRITE: 05 sy-vline, 95 sy-vline.
      IF sta2-tabname4 <> space.
        WRITE:
             /07(20) sta2-tabname4,   30(10) sta2-tab4dircnt,
              43(10) sta2-tab4seqcnt, 56(10) sta2-tab4updcnt.
      ENDIF.
      WRITE: 05 sy-vline, 95 sy-vline.
      IF sta2-tabname5 <> space.
        WRITE:
             /07(20) sta2-tabname5,   30(10) sta2-tab5dircnt,
              43(10) sta2-tab5seqcnt, 56(10) sta2-tab5updcnt.
      ENDIF.
      WRITE: 05 sy-vline, 95 sy-vline.
      ULINE /05(91).
    ENDIF.
*
*----Anzeige alle Tabellen die in TABLERECORDS protokolliert sind ------
  ELSE.
    IF rsystem <> cpuid.               "Externe Daten
      IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
        FORMAT INTENSIFIED ON.
        WRITE: /07 'Table accesses sorted by', sta2-sortname,
                   '(list might be truncated !)'.
        FORMAT INTENSIFIED OFF.
      ENDIF.
    ELSE.
      DESCRIBE TABLE tabs LINES lines.
      IF lines > 0 AND ( scopeflag IS INITIAL OR sy-pfkey = 'DETAIL' ).
        FORMAT INTENSIFIED ON.
        WRITE: 7 'Table accesses sorted by', sta2-sortname,
                 '(list might be incomplete)'.
        FORMAT INTENSIFIED OFF.
      ELSE.

        CALL FUNCTION 'PF_READ_PARAMETER'
             IMPORTING
                  tabrecno = tabrecno
             EXCEPTIONS
                  OTHERS   = 0.
        IF tabrecno <= 0.
          IF scopeflag IS INITIAL OR sy-pfkey = 'DETAIL'.
            ULINE /5(81).
          ENDIF.
          IF tabrec = '0'.
            WRITE:/5 sy-vline, 85 sy-vline,
                 7 'Profile parameter stat/tabrec is set to',
                   (3) tabrec.
          ENDIF.
          WRITE: /5 sy-vline, 85 sy-vline,
                  7 'Runtime value of stat/tabrec is set to ',
                    (3) tabrecno.
          WRITE:/5 sy-vline, 85 sy-vline,
                 7 'Maybe there was no recording of table accesses!'.
          ULINE /5(81).
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    DESCRIBE TABLE tabs LINES lines.
    IF lines < 1.
      IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
        ULINE /05(81).
      ENDIF.
      WRITE: /7 'No table accesses for this record '.
      WRITE:  05 sy-vline, 85 sy-vline.
    ELSE.
      IF scopeflag IS INITIAL OR sy-pfkey = 'DETAIL'.
        ULINE /5(81).
      ENDIF.
      WRITE: /05 sy-vline, 85 sy-vline,
              28 '-------- Number of rows accessed ----------'.
      WRITE: /05 sy-vline, 85 sy-vline,
              07(20) 'Table name', 28(10) '     Total',
              39(10) 'Dir. reads', 50(10) 'Seq. reads',
              61(10) '   Changes', 72(10) ' Time (ms)'.
      WRITE: /05 sy-vline, 85 sy-vline,
              07(20) '--------------------',
              28(10) '----------',
              39(10) '----------',    50(10) '----------',
              61(10) '----------',    72(10) '----------'.
      LOOP AT tabs.
        IF tabs-cross IS INITIAL.
          sta1-tname    = tabs-tname.
          sta1-totalrec = tabs-totcnt.
          sta1-dircnt   = tabs-dircnt.
          sta1-seqcnt   = tabs-seqcnt.
          sta1-modcnt   = tabs-modcnt.
          sta1-tabtime  = tabs-tabtime.
          WRITE: /05 sy-vline, 85 sy-vline,
                  07(10) sta1-tname,
                  28(10) sta1-totalrec,
                  39(10) sta1-dircnt,
                  50(10) sta1-seqcnt,
                  61(10) sta1-modcnt,
                  72(10) sta1-tabtime.
        ELSE.
          sta1-tname     = tabs-tname.
          sta1-ttotalrec = tabs-totcnt.
          sta1-tdircnt   = tabs-dircnt.
          sta1-tseqcnt   = tabs-seqcnt.
          sta1-tmodcnt   = tabs-modcnt.
          sta1-ttabtime  = tabs-tabtime.
          WRITE: /05 sy-vline, 85 sy-vline,
                  07(10) sta1-tname,
                  28(10) sta1-ttotalrec,
                  39(10) sta1-tdircnt,
                  50(10) sta1-tseqcnt,
                  61(10) sta1-tmodcnt,
                  72(10) sta1-ttabtime,
                 /5 sy-vline, 85 sy-vline.
        ENDIF.
      ENDLOOP.
    ENDIF.
    ULINE /05(81).
  ENDIF.
ENDFORM.
*
*------------------------------------------------------------------
* NETZ-DETAIL: Verzweigungshitliste DB-Services Analyse
*------------------------------------------------------------------
*
FORM netz-detail.
  IF sta2-bytes > 0.
    IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
      SKIP 1.
      FORMAT RESET.
      FORMAT INTENSIFIED ON.
      WRITE: /07(30) 'Bytes transfered             '.
      FORMAT INTENSIFIED OFF.
      ULINE /5(81).
    ENDIF.
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'Total                        ',
            36(15) sta2-bytes,   52 'Bytes'.
    ULINE /5(81).
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'Nametab interface            ',
            36(15) sta2-ntabcnt, 52 'Bytes'.
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'CPIC interface               ',
            36(15) sta2-cpiccnt, 52 'Bytes'.
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'DDIC interface               ',
            36(15) sta2-ddiccnt, 52 'Bytes'.
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'Batch-Input queue            ',
            36(15) sta2-quecnt, 52 'Bytes'.
    ULINE /5(81).
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'Screen source                ',
            36(15) sta2-dynpscnt, 52 'Bytes'.
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'ABAP/4 source                ',
            36(15) sta2-abapscnt, 52 'Bytes'.
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'CUA source                   ',
            36(15) sta2-cuascnt, 52 'Bytes'.
    ULINE /5(81).
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'Screen load                  ',
            36(15) sta2-dynplcnt, 52 'Bytes'.
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'ABAP/4 load                  ',
            36(15) sta2-abaplcnt, 52 'Bytes'.
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'CUA load                     ',
            36(15) sta2-cualcnt, 52 'Bytes'.
    ULINE /5(81).
    WRITE: /05 sy-vline, 85 sy-vline.
    WRITE:  07(30) 'Requested by application     ',
            36(15) sta2-dsqlcnt, 52 'Bytes'.
  ELSE.
    IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
      SKIP 1.
      FORMAT RESET.
      ULINE /5(81).
    ENDIF.
    WRITE: /05 sy-vline,
            07 'No transfered bytes for this record', 85 sy-vline.
  ENDIF.
  ULINE /5(81).
ENDFORM.
*---------------------------------------------------------------------*
*       FORM SPECIAL_REPORTS_RETRANSLATE                              *
*---------------------------------------------------------------------*
*       Rückübersetzung der SPECIAL-TCODES/REPPRTS                    *
*---------------------------------------------------------------------*
*  <->  REPORT                                                        *
*  -->  TCODE                                                         *
*---------------------------------------------------------------------*
FORM special_reports_retranslate USING report tcode.
  DATA rep(3).
*  IF TCODE EQ SPACE.
  TRANSLATE report TO UPPER CASE.
  CASE report.
    WHEN 'AUTOABAP'.
      report = 'SAPMSSY6'.
    WHEN 'LOGOFF'.
      report = 'SAPMSYST'.
    WHEN 'LOGIN_PW'.
      report = 'SAPMSYST'.
    WHEN 'MAINMENU'.
      report = 'SAPMSYST'.
    WHEN 'NEW PSWD'.
      report = 'SAPMSYST'.
    WHEN 'LOGIN_OK'.
      report = 'SAPMSSY0'.
    WHEN 'CPI-C' OR 'RFC' OR 'ALE'.
      report = 'SAPMSSY1'.
    WHEN 'REP_EDIT'.
      report = 'SAPMSEDT'.
    WHEN 'DEBUGGER'.
      report = 'SAPMSSY3'.
    WHEN 'REP_LIST'.
      report = 'SAPMSSY0'.
    WHEN 'R2 LGOUT'.
      report = 'SAPMSYST'.
    WHEN 'R2 LOGIN'.
      report = 'SAPMSYST'.
    WHEN 'UPD CTRL'.
      report = 'SAPMSSY4'.
    WHEN 'SCHEDULR'.
      report = 'SAPMSSY7'.
    WHEN OTHERS.
      rep = report.
      IF rep = '(B)'.
        report = 'SAPMSSY2'.
      ENDIF.
  ENDCASE.

*    REP = REPORT.
*    IF REPORT EQ '(B)'.
*      REPORT = 'SAPMSSY2'.
*    ENDIF.
*  ELSEIF
*    TCODE = 'SE38' OR                  "Tcode SE38 und Genossen
*    TCODE = 'SE31' OR                  "Tcode SE38 und Genossen
*    TCODE = 'SA38'.                    "Tcode SE38 und Genossen
*    TRANSLATE REPORT TO UPPER CASE.
*    CASE REPORT.
*      WHEN 'REP_EDIT'.
*        REPORT = 'SAPMSEDT'.
*      WHEN 'DEBUGGER'.
*        REPORT = 'SAPMSSY3'.
*      WHEN 'REP_LIST'.
*        REPORT = 'SAPMSSY0'.
*    ENDCASE.
*  ENDIF.
ENDFORM.
*
*------------------------------------------------------------------
* HEAD-DETAIL: Ausgabe Kopfzeilen
*------------------------------------------------------------------
*
FORM head-detail.
  CASE status.                         "write header
    WHEN 'HIT1'.
      PERFORM head-hit1.               "Top40 disp.TI
    WHEN 'HIT2'.
      PERFORM head-hit2.               "Top40 DB-requests
    WHEN 'SING'.
      PERFORM head-hit1.               "Single records
  ENDCASE.

  PERFORM set_int_header.
  sy-tabix = sy-tabix - 1.
  IF sy-tabix NE 0.                    "write line before

    CASE status.
      WHEN 'HIT1'.
        READ TABLE hitl1 INDEX sy-tabix.
*       move-corresponding hitl1 to stats.
        PERFORM move_hitl_to_stats USING hitl1.
        PERFORM write-hitl1-record.
      WHEN 'HIT2'.
        READ TABLE hitl2 INDEX sy-tabix.
*       move-corresponding hitl2 to stats.
        PERFORM move_hitl_to_stats USING hitl2.
        PERFORM write-hitl2-record.
      WHEN 'SING'.
        READ TABLE stats INDEX sy-tabix.
        PERFORM write-hitl1-record.
    ENDCASE.

  ELSE.
    SKIP 1.
  ENDIF.
*
  PERFORM set_nonint_header.
  sy-tabix = sy-tabix + 1.

  CASE status.
    WHEN 'HIT1'.
      READ TABLE hitl1 INDEX sy-tabix.
*     move-corresponding hitl1 to stats.
      PERFORM move_hitl_to_stats USING hitl1.
      PERFORM write-hitl1-record.
    WHEN 'HIT2'.
      READ TABLE hitl2 INDEX sy-tabix.
*     move-corresponding hitl2 to stats.
      PERFORM move_hitl_to_stats USING hitl2.
      PERFORM write-hitl2-record.
    WHEN 'SING'.
      READ TABLE stats INDEX sy-tabix.
      PERFORM write-hitl1-record.
  ENDCASE.

* SUMMARY.                             " K11K049873
  PERFORM set_int_header.
  IF sy-tabix NE index.
    sy-tabix = sy-tabix + 1.

    CASE status.                       "write selected line
      WHEN 'HIT1'.
        READ TABLE hitl1 INDEX sy-tabix.
*       move-corresponding hitl1 to stats.
        PERFORM move_hitl_to_stats USING hitl1.
        PERFORM write-hitl1-record.
      WHEN 'HIT2'.
        READ TABLE hitl2 INDEX sy-tabix.
*       move-corresponding hitl2 to stats.
        PERFORM move_hitl_to_stats USING hitl2.
        PERFORM write-hitl2-record.
      WHEN 'SING'.
        READ TABLE stats INDEX sy-tabix.
        PERFORM write-hitl1-record.
    ENDCASE.

    sy-tabix = sy-tabix - 1.

    CASE status.                       "Reposition on selected line
      WHEN 'HIT1'.
        READ TABLE hitl1 INDEX sy-tabix.
*       move-corresponding hitl1 to stats.
        PERFORM move_hitl_to_stats USING hitl1.
      WHEN 'HIT2'.
        READ TABLE hitl2 INDEX sy-tabix.
*       move-corresponding hitl2 to stats.
        PERFORM move_hitl_to_stats USING hitl2.
      WHEN 'SING'.
        READ TABLE stats INDEX sy-tabix.
      WHEN OTHERS.
    ENDCASE.

  ELSE.
    SKIP 1.
  ENDIF.

  PERFORM move_stats_to_sta2.

  PERFORM check_record.
* perform resort-tabs(rsstat20).
  PERFORM set_nonint_header.
  ULINE AT /(namelength_uline).
  FORMAT INTENSIFIED OFF COLOR 4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MOVE_STATS_TO_STA2
*&---------------------------------------------------------------------*
*       Move contents of global STATS into global STA2                 *
*----------------------------------------------------------------------*
*       no parameters
*----------------------------------------------------------------------*
FORM move_stats_to_sta2.
  DATA  save_tabix LIKE sy-tabix.      "never change sy-tabix  :-(
  DATA: BEGIN OF null_and_space,
          null TYPE x,
          spc  TYPE c,
        END   OF null_and_space.
  DATA  save_stats LIKE stats.                              "B30K012402

  save_tabix = sy-tabix.
  save_stats = stats.                                       "B30K012402
  CLEAR sta2.
  PERFORM: check_dec USING stats-respti,                    "B30K012402
           check_dec USING stats-dbcalls,
           check_dec USING stats-cputi,
           check_dec USING stats-queueti,
           check_dec USING stats-rollinti,
           check_dec USING stats-rolloutti,
           check_dec USING stats-lockti,
           check_dec USING stats-txxxti,
           check_dec USING stats-insti,
           check_dec USING stats-updti,
           check_dec USING stats-delti,
           check_dec USING stats-committi,
           check_dec USING stats-reploadti,
           check_dec USING stats-cualoadti,
           check_dec USING stats-dynploadti,
           check_dec USING stats-queti,
           check_dec USING stats-ddicti,
           check_dec USING stats-cpicti,
           check_dec USING stats-mcti.
  MOVE-CORRESPONDING stats TO sta2.
  stats = save_stats.                                       "B30K012402
  sta2-maxmem     = stats-usedbytes +
                    stats-privsum +
                    stats-maxroll +
                    stats-maxpage.
  sta2-restart    = stats-wprestart.
  CASE stats-tasktype.
    WHEN tt_dia OR tt_ale OR tt_rfc OR tt_cpic.
      sta2-sypfkey = stats-cua_prog.
      sta2-syucomm = stats-cua_func.
      TRANSLATE: sta2-sypfkey  USING null_and_space,
                 sta2-syucomm  USING null_and_space.
    WHEN tt_btc.
      sta2-btcjobname = stats-jobname.
      sta2-btcstepnr  = stats-jobstep.
  ENDCASE.
  TRANSLATE: sta2-tcode    USING null_and_space,
             sta2-report   USING null_and_space,
             sta2-dynpronr USING null_and_space,
             sta2-sypfkey  USING null_and_space,
             sta2-syucomm  USING null_and_space.
  sy-tabix = save_tabix.
ENDFORM.                               " MOVE_STATS_TO_STA2
*&---------------------------------------------------------------------*
*&      Form  RFC_DETAIL
*&---------------------------------------------------------------------*
*       Write detailed information about RFC calls                     *
*----------------------------------------------------------------------*
*       no parameters (uses current record stored in STATS)
*----------------------------------------------------------------------*
FORM rfc_detail.
  DATA  save_tabix       LIKE sy-tabix."never change sy-tabix :-(
  DATA  long(16)         TYPE p.
  DATA  time(16)         TYPE p.                            "B30K017115
  DATA  runtime_rfcrecno TYPE i.
  DATA  profile_rfcrecno LIKE profparam-value.

  save_tabix = sy-tabix.

  READ TABLE norm_subrecord_index WITH KEY recordno = stats-origin
       BINARY SEARCH.
  IF sy-subrc NE 0                               OR
     ( norm_subrecord_index-rfc_cli_b  = 0 AND
       norm_subrecord_index-rfc_serv_b = 0 AND
       norm_subrecord_index-rfc_clid_b = 0 AND
       norm_subrecord_index-rfc_servdb = 0     ).
    IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
      FORMAT RESET.
      SKIP.
      ULINE /5(81).
    ENDIF.
    CALL FUNCTION 'PF_READ_PARAMETER'
         IMPORTING
              rfcrecno = runtime_rfcrecno
         EXCEPTIONS
              OTHERS   = 0.
    CALL FUNCTION 'SAPTUNE_PROFILE_PARAMETER'
         EXPORTING
              name              = 'stat/rfcrec'
         IMPORTING
              value             = profile_rfcrecno
         EXCEPTIONS
              unknown_parameter = 1
              no_authorization  = 2
              OTHERS            = 3.
    IF runtime_rfcrecno <= 0.
      CONDENSE profile_rfcrecno.
      WRITE: /5 sy-vline, 'Profile parameter stat/rfcrec is set to',
                (4) profile_rfcrecno, 85 sy-vline,
             /5 sy-vline, 'Runtime value of stat/rfcrec is set to',
                (4) runtime_rfcrecno LEFT-JUSTIFIED, 85 sy-vline,
             /5 sy-vline, 'Maybe there was no recording of RFCs!',
                85 sy-vline.
    ELSE.
      WRITE: /5 sy-vline, 'No RFC subrecords for this record',
             85 sy-vline.
    ENDIF.
    ULINE /5(81).
  ELSE.
    IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
      FORMAT RESET.
      SKIP.
      FORMAT INTENSIFIED ON.
      WRITE: /6 'Remote function calls'.
      FORMAT INTENSIFIED OFF.
      ULINE /5(81).
    ENDIF.
    IF norm_subrecord_index-rfc_cli_b > 0.
      WRITE: /5 sy-vline, 'Client subrecords', 85 sy-vline,
             /5 sy-vline, '-----------------', 85 sy-vline.
      LOOP AT v2_rfc_client_records
                           FROM norm_subrecord_index-rfc_cli_b
                           TO   norm_subrecord_index-rfc_cli_e.
        WRITE: /5 sy-vline,                                 85 sy-vline,
               /5 sy-vline, 'Target        ',
                  v2_rfc_client_records-target,             85 sy-vline,
               /5 sy-vline, 'User ID       ',
                  v2_rfc_client_records-userid,             85 sy-vline,
               /5 sy-vline, 'Local  destin.',
                  v2_rfc_client_records-dest,
                  'IP address',
                  v2_rfc_client_records-ip,                 85 sy-vline,
               /5 sy-vline, 'Remote destin.',
                  v2_rfc_client_records-remotedest,
                  'IP address',
                  v2_rfc_client_records-remoteip,           85 sy-vline,
               /5 sy-vline, 'Program       ',
                  v2_rfc_client_records-progname,           85 sy-vline,
               /5 sy-vline, 'Function      ',
                  v2_rfc_client_records-funcname,           85 sy-vline,
               /5 sy-vline, 'Transaction ID',
                  v2_rfc_client_records-tid,                85 sy-vline.
        PERFORM set_get_tid USING v2_rfc_client_records-tid 'S'.
        long = v2_rfc_client_records-receive.
        WRITE: /5 sy-vline, 'Received data ',
                  (32) long, 'Bytes',                       85 sy-vline.
        long = v2_rfc_client_records-send.
        WRITE: /5 sy-vline, 'Sent data     ',
                  (32) long, 'Bytes',                       85 sy-vline.
        time = v2_rfc_client_records-calltime.
        WRITE: /5 sy-vline, 'Calling time  ',
                  (32) time, 'ms',                          85 sy-vline.
        time = v2_rfc_client_records-rexetime.
        WRITE: /5 sy-vline, 'Rem. exe. time',
                  (32) time, 'ms',                          85 sy-vline.
      ENDLOOP.
      ULINE /5(81).
    ENDIF.
    IF norm_subrecord_index-rfc_serv_b > 0.
      WRITE: /5 sy-vline, 'Server subrecords', 85 sy-vline,
             /5 sy-vline, '-----------------', 85 sy-vline.
      LOOP AT v2_rfc_server_records
                           FROM norm_subrecord_index-rfc_serv_b
                           TO   norm_subrecord_index-rfc_serv_e.
        WRITE: /5 sy-vline,                                 85 sy-vline,
               /5 sy-vline, 'Target        ',
                  v2_rfc_server_records-target,             85 sy-vline,
               /5 sy-vline, 'User ID       ',
                  v2_rfc_server_records-userid,             85 sy-vline,
               /5 sy-vline, 'RFC Caller    ',
                  v2_rfc_server_records-caller,             85 sy-vline,
               /5 sy-vline, 'Local  destin.',
                  v2_rfc_server_records-dest,
                  'IP address',
                  v2_rfc_server_records-ip,                 85 sy-vline,
               /5 sy-vline, 'Remote destin.',
                  v2_rfc_server_records-remotedest,
                  'IP address',
                  v2_rfc_server_records-remoteip,           85 sy-vline,
               /5 sy-vline, 'Program       ',
                  v2_rfc_server_records-progname,           85 sy-vline,
               /5 sy-vline, 'Function      ',
                  v2_rfc_server_records-funcname,           85 sy-vline,
               /5 sy-vline, 'Transaction ID',
                  v2_rfc_server_records-tid,                85 sy-vline.
        PERFORM set_get_tid USING v2_rfc_server_records-tid 'S'.
        long = v2_rfc_server_records-receive.
        WRITE: /5 sy-vline, 'Received data ',
                  (32) long, 'Bytes',                       85 sy-vline.
        long = v2_rfc_server_records-send.
        WRITE: /5 sy-vline, 'Sent data     ',
                  (32) long, 'Bytes',                       85 sy-vline.
        time = v2_rfc_server_records-calltime.
        WRITE: /5 sy-vline, 'Calling time  ',
                  (32) time, 'ms',                          85 sy-vline.
        time = v2_rfc_server_records-exetime.
        WRITE: /5 sy-vline, 'Rem. exe. time',
                  (32) time, 'ms',                          85 sy-vline.
      ENDLOOP.
      ULINE /5(81).
    ENDIF.
    IF norm_subrecord_index-rfc_clid_b > 0.
      WRITE: /5 sy-vline, 'Client destination subrecords', 85 sy-vline,
             /5 sy-vline, '-----------------------------', 85 sy-vline.
      LOOP AT v2_rfc_client_dest_records
                           FROM norm_subrecord_index-rfc_clid_b
                           TO   norm_subrecord_index-rfc_clid_e.
        WRITE: /5 sy-vline,                                 85 sy-vline,
               /5 sy-vline, 'Target        ',
                  v2_rfc_client_dest_records-target,        85 sy-vline,
               /5 sy-vline, 'User ID       ',
                  v2_rfc_client_dest_records-userid,        85 sy-vline,
               /5 sy-vline, 'Local  destin.',
                  v2_rfc_client_dest_records-dest,
                  'IP address',
                  v2_rfc_client_dest_records-ip,            85 sy-vline,
               /5 sy-vline, 'Remote destin.',
                  v2_rfc_client_dest_records-remotedest,
                  'IP address',
                  v2_rfc_client_dest_records-remoteip,      85 sy-vline,
               /5 sy-vline, 'Program       ',
                  v2_rfc_client_dest_records-progname,      85 sy-vline,
               /5 sy-vline, 'Function      ',
                  v2_rfc_client_dest_records-funcname,      85 sy-vline,
               /5 sy-vline, 'Transaction ID',
                  v2_rfc_client_dest_records-tid,           85 sy-vline.
        PERFORM set_get_tid USING v2_rfc_client_dest_records-tid 'S'.
        long = v2_rfc_client_dest_records-calls.
        WRITE: /5 sy-vline, 'Calls         ',
                  (32) long,                                85 sy-vline,
               /5 sy-vline, 'Start timestmp',
                  v2_rfc_client_dest_records-rfcstart,      85 sy-vline.
        long = v2_rfc_client_dest_records-receive.
        WRITE: /5 sy-vline, 'Received data ',
                  (32) long, 'Bytes',                       85 sy-vline.
        long = v2_rfc_client_dest_records-send.
        WRITE: /5 sy-vline, 'Sent data     ',
                  (32) long, 'Bytes',                       85 sy-vline.
        time = v2_rfc_client_dest_records-calltime.
        WRITE: /5 sy-vline, 'Calling time  ',
                  (32) time, 'ms',                          85 sy-vline.
        time = v2_rfc_client_dest_records-rexetime.
        WRITE: /5 sy-vline, 'Rem. exe. time',
                  (32) time, 'ms',                          85 sy-vline.
      ENDLOOP.
      ULINE /5(81).
    ENDIF.
    IF norm_subrecord_index-rfc_servdb > 0.
      WRITE: /5 sy-vline, 'Server destination subrecords', 85 sy-vline,
             /5 sy-vline, '-----------------------------', 85 sy-vline.
      LOOP AT v2_rfc_server_dest_records
                           FROM norm_subrecord_index-rfc_servdb
                           TO   norm_subrecord_index-rfc_servde.
        WRITE: /5 sy-vline,                                 85 sy-vline,
               /5 sy-vline, 'Target        ',
                  v2_rfc_server_dest_records-target,        85 sy-vline,
               /5 sy-vline, 'User ID       ',
                  v2_rfc_server_dest_records-userid,        85 sy-vline,
               /5 sy-vline, 'RFC Caller    ',
                  v2_rfc_server_dest_records-caller,        85 sy-vline,
               /5 sy-vline, 'Local  destin.',
                  v2_rfc_server_dest_records-dest,
                  'IP address',
                  v2_rfc_server_dest_records-ip,            85 sy-vline,
               /5 sy-vline, 'Remote destin.',
                  v2_rfc_server_dest_records-remotedest,
                  'IP address',
                  v2_rfc_server_dest_records-remoteip,      85 sy-vline,
               /5 sy-vline, 'Program       ',
                  v2_rfc_server_dest_records-progname,      85 sy-vline,
               /5 sy-vline, 'Function      ',
                  v2_rfc_server_dest_records-funcname,      85 sy-vline,
               /5 sy-vline, 'Transaction ID',
                  v2_rfc_server_dest_records-tid,           85 sy-vline.
        PERFORM set_get_tid USING v2_rfc_server_dest_records-tid 'S'.
        long = v2_rfc_server_dest_records-calls.
        WRITE: /5 sy-vline, 'Calls         ',
                  (32) long,                                85 sy-vline,
               /5 sy-vline, 'Start timestmp',
                  v2_rfc_server_dest_records-rfcstart,      85 sy-vline.
        long = v2_rfc_server_dest_records-receive.
        WRITE: /5 sy-vline, 'Received data ',
                  (32) long, 'Bytes',                       85 sy-vline.
        long = v2_rfc_server_dest_records-send.
        WRITE: /5 sy-vline, 'Sent data     ',
                  (32) long, 'Bytes',                       85 sy-vline.
        time = v2_rfc_server_dest_records-calltime.
        WRITE: /5 sy-vline, 'Calling time  ',
                  (32) time, 'ms',                          85 sy-vline.
        time = v2_rfc_server_dest_records-exetime.
        WRITE: /5 sy-vline, 'Rem. exe. time',
                  (32) time, 'ms',                          85 sy-vline.
      ENDLOOP.
      ULINE /5(81).
    ENDIF.
  ENDIF.
  sy-tabix = save_tabix.
ENDFORM.                               " RFC_DETAIL
*&---------------------------------------------------------------------*
*&      Form  SET_HITL_STATUS
*&---------------------------------------------------------------------*
*       Set status for hitlist display                                 *
*----------------------------------------------------------------------*
*  -->  STATUS    name of status
*  -->  UCOMM     user command executed
*----------------------------------------------------------------------*
FORM set_hitl_status USING status ucomm.
  DATA  save_tabix LIKE sy-tabix.      "never change sy-tabix :-(
  DATA: BEGIN OF excl_list OCCURS 0,
          command(4),
        END   OF excl_list.

  save_tabix = sy-tabix.
  CASE status.
    WHEN 'DETAIL'.
      IF NOT ucomm = 'RFCI'.
        "RFC Intervals Button muß stehenbleiben, da dafür eigene Liste
        APPEND ucomm TO excl_list.
      ENDIF.
      READ TABLE norm_subrecord_index WITH KEY recordno = stats-origin
           BINARY SEARCH.
      IF sy-subrc NE 0.
        APPEND: 'DB5F' TO excl_list,
                'RFC'  TO excl_list,
                'RFCI' TO excl_list,
                'SORT' TO excl_list,
                'SPOO' TO excl_list.
      ELSE.
        IF norm_subrecord_index-tab_b = 0.
          APPEND: 'DB5F' TO excl_list,
                  'SORT' TO excl_list.
        ENDIF.
        IF norm_subrecord_index-rfc_cli_b  = 0  AND
           norm_subrecord_index-rfc_serv_b = 0  AND
           norm_subrecord_index-rfc_clid_b = 0  AND
           norm_subrecord_index-rfc_servdb = 0.
          APPEND 'RFC' TO excl_list.
        ENDIF.
        IF norm_subrecord_index-ti_int_b  = 0.
          APPEND 'RFCI' TO excl_list.
        ENDIF.
        IF norm_subrecord_index-spo_pri_b  = 0  AND
           norm_subrecord_index-spo_act_b  = 0.
          APPEND 'SPOO' TO excl_list.
        ENDIF.
      ENDIF.
      IF stats-mcreadcnt = 0 AND
         stats-mcupdcnt  = 0 AND
         stats-mcinscnt  = 0 AND
         stats-mcdelcnt  = 0.
        APPEND 'MATC' TO excl_list.
      ENDIF.
      IF stats-ntabcnt  = 0 AND stats-quecnt   = 0 AND
         stats-cpiccnt  = 0 AND stats-ddiccnt  = 0 AND
         stats-dynpscnt = 0 AND stats-dynplcnt = 0 AND
         stats-abapscnt = 0 AND stats-abaplcnt = 0 AND
         stats-cualcnt  = 0 AND sta2-dsqlcnt   = 0.
        APPEND 'NETH' TO excl_list.
      ENDIF.
      SET PF-STATUS status EXCLUDING excl_list.
    WHEN 'MAIN'.
      IF tasktype = 'C' OR tasktype = 'R' OR tasktype = 'L'.
        APPEND: 'TCOD' TO excl_list,
                'RAPP' TO excl_list.
      ENDIF.
      SET PF-STATUS '0020' EXCLUDING excl_list.
    WHEN OTHERS.
  ENDCASE.
  sy-tabix = save_tabix.
ENDFORM.                               " SET_HITL_STATUS
*&---------------------------------------------------------------------*
*&      Form  MOVE_HITL_TO_STATS
*&---------------------------------------------------------------------*
*       Move the hitlist record to the STATS structure                 *
*----------------------------------------------------------------------*
*  -->  REC       hitlist record
*----------------------------------------------------------------------*
FORM move_hitl_to_stats USING rec STRUCTURE sapwlhitl. "stats_cuml.
  LOCAL sapwlpfnrm.

  MOVE-CORRESPONDING: rec TO stats,
                      rec TO sapwlpfnrm.
  CALL FUNCTION 'SAPWL_STATREC_GET_ENTRY_ID'
       EXPORTING
            v2_record             = sapwlpfnrm
       IMPORTING
            converted_report_name = stats-report.
ENDFORM.                               " MOVE_HITL_TO_STATS
*&---------------------------------------------------------------------*
*&      Form  SET_GET_TID
*&---------------------------------------------------------------------*
*       Set/Get the transaction ID stored in a RFC record
*----------------------------------------------------------------------*
*      <-> TID   transaction ID                                        *
*      --> MODE  s)et or g)et the transaction ID                       *
*----------------------------------------------------------------------*
FORM set_get_tid USING    tid  LIKE pfrfccli-tid
                          mode TYPE c.
  STATICS local_tid LIKE pfrfccli-tid.

  CASE mode.
    WHEN 'S'. "set (store) the transaction ID in the "Hide" part
      local_tid = tid.
      HIDE local_tid.
    WHEN 'G'.
      tid = local_tid.
  ENDCASE.
  CLEAR local_tid.
ENDFORM.                               " SET_GET_TID
*&---------------------------------------------------------------------*
*&      Form  CHECK_DEC
*&---------------------------------------------------------------------*
*       Check a variable for the maximum integer value and reduce      *
*       the number to the maximum if necessary                         *
*----------------------------------------------------------------------*
*  <->  NUM       number
*----------------------------------------------------------------------*
FORM check_dec USING num.
  CONSTANTS max_int TYPE i VALUE 2147483647.

  IF num > max_int.
    num = max_int.
  ENDIF.
ENDFORM.                               " CHECK_DEC
*&---------------------------------------------------------------------*
*&      Form  SPOOL_DETAIL
*&---------------------------------------------------------------------*
*       Write detailed information about spool steps.                  *
*----------------------------------------------------------------------*
*       no parameters (uses current record stored in STATS)
*----------------------------------------------------------------------*
FORM spool_detail.
  DATA  save_tabix LIKE sy-tabix.      "never change sy-tabix :-(
  DATA: mode(15),
*   Typkonvertierung 'DECIMALS 0' nach 'DECIMALS 3',
*   daher automatisch Division durch 1000 (us -> ms).
        milliseconds TYPE p DECIMALS 3.
  DATA  runtime_sporecno TYPE i.
  DATA  profile_sporecno LIKE profparam-value.

  save_tabix = sy-tabix.

  READ TABLE norm_subrecord_index WITH KEY recordno = stats-origin
       BINARY SEARCH.

  IF sy-subrc          NE 0                      OR
     ( norm_subrecord_index-spo_pri_b  = 0 AND
       norm_subrecord_index-spo_act_e = 0 ).
    IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
      FORMAT RESET.
      SKIP.
      ULINE /5(81).
    ENDIF.
    CALL FUNCTION 'PF_READ_PARAMETER'
         IMPORTING
              sporecno = runtime_sporecno
         EXCEPTIONS
              OTHERS   = 0.
    CALL FUNCTION 'SAPTUNE_PROFILE_PARAMETER'
         EXPORTING
              name              = 'stat/sporec'
         IMPORTING
              value             = profile_sporecno
         EXCEPTIONS
              unknown_parameter = 1
              no_authorization  = 2
              OTHERS            = 3.
    IF runtime_sporecno <= 0.
      CONDENSE profile_sporecno.
      WRITE: /5 sy-vline, 'Profile parameter stat/sporec is set to',
                (4) profile_sporecno, 85 sy-vline,
             /5 sy-vline, 'Runtime value of stat/sporec is set to',
                (4) runtime_sporecno LEFT-JUSTIFIED, 85 sy-vline,
             /5 sy-vline,
                'Maybe there was no recording of spool activities!',
                85 sy-vline.
    ELSE.
      WRITE: /5 sy-vline, 'No spool subrecords for this record',
             85 sy-vline.
    ENDIF.
    ULINE /5(81).
  ELSE.
    IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
      FORMAT RESET.
      SKIP.
      FORMAT INTENSIFIED ON.
      WRITE: /6 'Spool information'.
      FORMAT INTENSIFIED OFF.
      ULINE /5(81).
    ENDIF.
    IF norm_subrecord_index-spo_pri_b > 0.
      WRITE: /5 sy-vline, 'Spool statistic', 85 sy-vline.
      IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
        WRITE: /5 sy-vline, '---------------', 85 sy-vline.
      ENDIF.
      LOOP AT v2_spool_print_records
                           FROM norm_subrecord_index-spo_pri_b
                           TO   norm_subrecord_index-spo_pri_e.
        WRITE: /5 sy-vline,                                 85 sy-vline.
        milliseconds = v2_spool_print_records-respti.
        WRITE: /5 sy-vline, 8 'Time           ',
                (21) milliseconds DECIMALS 1, 'ms',         85 sy-vline,
               /5 sy-vline, 8 'No of spooljobs',
                  v2_spool_print_records-processed,         85 sy-vline,
               /5 sy-vline, 8 'Processed pages',
                  v2_spool_print_records-pjpages,           85 sy-vline,
               /5 sy-vline, 8 'Bytes of job   ',
                  v2_spool_print_records-pjbytes, ' Bytes', 85 sy-vline,
               /5 sy-vline, 8 'Processed bytes',
                v2_spool_print_records-procbytes, ' Bytes', 85 sy-vline,
               /5 sy-vline, 8 'Transfer. bytes',
                  v2_spool_print_records-trbytes, ' Bytes', 85 sy-vline.
      ENDLOOP.
      ULINE /5(81).
    ENDIF.
    IF norm_subrecord_index-spo_act_b > 0.
      IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
        WRITE: /5 sy-vline, 'Spool activities', 85 sy-vline,
               /5 sy-vline, '----------------', 85 sy-vline.
        WRITE: /5 sy-vline,                               85 sy-vline,
               /5 sy-vline, ' Activity', 32 '   Time in ms      ',
                                                          85 sy-vline,
               /5 sy-vline,                               85 sy-vline.
      ELSE.
        WRITE: /5 sy-vline, 'Spool activities',
                32 'Time in ms         ',                85 sy-vline,
               /5 sy-vline,                              85 sy-vline.
      ENDIF.
      LOOP AT v2_spool_activity_records
                           FROM norm_subrecord_index-spo_act_b
                           TO   norm_subrecord_index-spo_act_e.
        milliseconds = v2_spool_activity_records-respti.
        PERFORM get_spool_activity_mode
                       USING v2_spool_activity_records-currmode
                       CHANGING mode.
        IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
          WRITE: /5 sy-vline, 9 mode,                      85 sy-vline.
        ELSE.
          WRITE: /5 sy-vline, 8 mode,                      85 sy-vline.
        ENDIF.
        WRITE:    28(18) milliseconds DECIMALS 1,          85 sy-vline.
      ENDLOOP.
      ULINE /5(81).
    ENDIF.
  ENDIF.
  sy-tabix = save_tabix.
ENDFORM.                               " SPOOL_DETAIL
*&---------------------------------------------------------------------*
*&      Form  GET_SPOOL_ACTIVITY_MODE
*&---------------------------------------------------------------------*
*       Gets signification of mode abbreviation.
*----------------------------------------------------------------------*
*      -->faelle  Abbreviation                                         *
*      <--P_MODE  Signification                                        *
*----------------------------------------------------------------------*
FORM get_spool_activity_mode USING    faelle
                             CHANGING p_mode.

  CASE faelle.
    WHEN '-'.
      p_mode = 'Idle'.
    WHEN 'C'.
      p_mode = 'Control'.
    WHEN 'D'.
      p_mode = 'Debug Message'.
    WHEN 'I'.
      p_mode = 'Initializing'.
    WHEN 'M'.
      p_mode = 'Message'.
    WHEN 'Q'.
      p_mode = 'Query'.
    WHEN 'U'.
      p_mode = 'Undefined'.
    WHEN OTHERS.
      p_mode = 'unknown'.
  ENDCASE.
ENDFORM.                               " GET_SPOOL_ACTIVITY_MODE

*&---------------------------------------------------------------------*
*&      Form  RFC_INT_DETAIL
*&---------------------------------------------------------------------*
*       Write detailed information about RFC time intervals            *
*----------------------------------------------------------------------*
*       no parameters (uses current record stored in STATS)
*----------------------------------------------------------------------*
FORM rfc_int_detail.
  DATA  save_tabix       LIKE sy-tabix.
  DATA: BEGIN OF intervals OCCURS 0.
          INCLUDE STRUCTURE pftiintrec.
  DATA: END OF intervals,
        header LIKE snodetext-text1,
        local(39),
        remote(39).

  save_tabix = sy-tabix.

  READ TABLE norm_subrecord_index WITH KEY recordno = stats-origin
       BINARY SEARCH.
  IF sy-subrc NE 0  OR
       norm_subrecord_index-ti_int_b  = 0.
    FORMAT RESET.
    SKIP.
    ULINE /5(81).
    WRITE: /5 sy-vline,
              'No RFC time intervals for this record.',
           85 sy-vline.
    ULINE /5(81).
  ELSE.
    IF scopeflag EQ space OR sy-pfkey = 'DETAIL'.
      FORMAT RESET.
      SKIP.
      FORMAT INTENSIFIED ON.
      WRITE: /6 'Remote function call time intervals'.
      FORMAT INTENSIFIED OFF.
      ULINE /5(81).
    ENDIF.
    CLEAR: local, remote.              "Destinationen bestimmen
    IF norm_subrecord_index-ti_int_b > 0.
      WRITE: /5 sy-vline, 'Use button ''RFC Intervals''.', 85 sy-vline.
      ULINE /5(81).
      IF steuer = 'RFCI'.
        CLEAR intervals. REFRESH intervals.
        LOOP AT v2_rfc_time_int_records
                             FROM norm_subrecord_index-ti_int_b
                             TO   norm_subrecord_index-ti_int_e.
          MOVE-CORRESPONDING v2_rfc_time_int_records TO intervals.
          APPEND intervals.
        ENDLOOP.
        IF norm_subrecord_index-rfc_clid_b > 0.
          LOOP AT v2_rfc_client_dest_records
                             FROM norm_subrecord_index-rfc_clid_b
                             TO   norm_subrecord_index-rfc_clid_e.
            IF sy-tabix = norm_subrecord_index-rfc_clid_b.
              local = v2_rfc_client_dest_records-dest.
              remote = v2_rfc_client_dest_records-remotedest.
            ELSE.
              IF NOT local = v2_rfc_client_dest_records-dest.
                local = 'various'.
              ENDIF.
              IF NOT remote = v2_rfc_client_dest_records-remotedest.
                remote = 'various'.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSEIF norm_subrecord_index-rfc_servdb > 0.
          LOOP AT v2_rfc_server_dest_records
                             FROM norm_subrecord_index-rfc_servdb
                             TO   norm_subrecord_index-rfc_servde.
            IF sy-tabix = norm_subrecord_index-rfc_servdb.
              remote = v2_rfc_server_dest_records-dest.
              local = v2_rfc_server_dest_records-remotedest.
            ELSE.
              IF NOT remote = v2_rfc_server_dest_records-dest.
                local = 'various'.
              ENDIF.
              IF NOT local = v2_rfc_server_dest_records-remotedest.
                remote = 'various'.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          local = remote = '?'.
        ENDIF.
        header = 'RFC ($ -> $) '.
        REPLACE '$' WITH local INTO header.
        REPLACE '$' WITH remote INTO header.
        CONDENSE header.
                                       "Baum erstellen und ausgeben
        CALL FUNCTION 'TH_MK_INTERVAL_TREE'
             EXPORTING
                  insert_root_node = 1
                  root_node_text   = header
                  root_id          = 9999
                  parent_id        = 10000
                  mode             = 0
             TABLES
                  intervals        = intervals.
        CALL FUNCTION 'TH_MK_INTERVAL_TREE'
             EXPORTING
                  mode = 2.
      ENDIF.
    ENDIF.
  ENDIF.
  sy-tabix = save_tabix.
ENDFORM.                               " RFC_INT_DETAIL
*&---------------------------------------------------------------------*
*&      Form  RFC_HITL_DETAIL
*&---------------------------------------------------------------------*
*       Write detailed RFC information in hitlists.
*----------------------------------------------------------------------*
*       no parameters (uses current record in hitl1 / hitl2)
*----------------------------------------------------------------------*
FORM rfc_hitl_detail.

  DATA: BEGIN OF rfc_hit_details,
          rfcdatfrom TYPE sapwlhitl-rfcdatfrom,
          rfcreceive TYPE sapwlhitl-rfcreceive,
          rfcsend    TYPE sapwlhitl-rfcsend,
          rfcexetime TYPE sapwlhitl-rfcexetime,
          rfccalltim TYPE sapwlhitl-rfccalltim,
          rfccalls   TYPE sapwlhitl-rfccalls,
        END OF rfc_hit_details.

  CASE status.
    WHEN 'HIT1'.
      MOVE-CORRESPONDING hitl1 TO rfc_hit_details.
    WHEN 'HIT2'.
      MOVE-CORRESPONDING hitl2 TO rfc_hit_details.
  ENDCASE.

  SKIP.
  FORMAT RESET.
  FORMAT INTENSIFIED ON.
  WRITE: /6 'Remote function calls'.
  ULINE /5(81).

  IF rfc_hit_details-rfcdatfrom = space.
    FORMAT INTENSIFIED OFF.
    WRITE: /5 sy-vline, 'No RFC subrecords for this record',
           85 sy-vline.
  ELSEIF rfc_hit_details-rfcdatfrom = 'S'.
    WRITE: /5 sy-vline, 'As server', 85 sy-vline.
  ELSEIF rfc_hit_details-rfcdatfrom = 'C'.
    WRITE: /5 sy-vline, 'As client', 85 sy-vline.
  ELSE.
    WRITE: /5 sy-vline, 'As server and client', 85 sy-vline.
  ENDIF.

  IF NOT rfc_hit_details-rfcdatfrom = space.
    FORMAT INTENSIFIED OFF.
    WRITE: /5 sy-vline, 85 sy-vline.
    WRITE: /5 sy-vline, 'Calls         ',
           (20) rfc_hit_details-rfccalls, 85 sy-vline.
    WRITE: /5 sy-vline, 'Received data ',
           (20) rfc_hit_details-rfcreceive, 'Bytes', 85 sy-vline.
    WRITE: /5 sy-vline, 'Sent data     ',
           (20) rfc_hit_details-rfcsend, 'Bytes', 85 sy-vline.
    WRITE: /5 sy-vline, 'Calling time  ',
           (20) rfc_hit_details-rfccalltim, 'ms', 85 sy-vline.
    WRITE: /5 sy-vline, 'Rem. exe. time',
           (20) rfc_hit_details-rfcexetime, 'ms', 85 sy-vline.
  ENDIF.

  ULINE /5(81).

ENDFORM.                               " RFC_HITL_DETAIL
