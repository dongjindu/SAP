*   Formroutinen fuer Statistik-Parameter in bezug auf MONI Datenbank  *
*                                                                      *
*                                                                      *
*  Korr.: 13A  B20K001672  FORM CHECK-COLLECTIVITY    eingebaut   QQI  *
*              B20K001672  TOCFLAG Default auf ' ' gesetzt        QQI  *
*         20A  B20K002660  CHECK-COLLECTIVITY auf RSSTATCK tranf. QQI  *
*----------------------------------------------------------------------*
*

*---------------------------------------------------------------------*
*       FORM GET_STANDARD_STAPAR                                      *
*---------------------------------------------------------------------*
*       Einlesen der Reorg-Parameter                                  *
*---------------------------------------------------------------------*
form get_standard_stapar.
*
*-------Versionsbezeichnung---------------------------------------------
  stapar-version         = 'STANDARD'. "Version
  stapar-vertype         = 'NEW'.      "Versionnummer
  stapar-active          = 'X'.        "aktive Version
*
*-------Administrative Information ueber Aenderung der Parameter--------
  stapar-lastchangedate  = sy-datum.   "Versionsaenderungstag
  stapar-lastchangetime  = sy-uzeit.
  stapar-lastchangeuser  = 'SAPSYS'.
  stapar-sincedate       = sy-datum.   "Ab wann sind Aenderungen akt
*
*-------Flags-Values (X/ )---------------------------------------------
*
  stapar-collectiveflag     = 'X'.     "TOTAL-Server wird gepflegt
  stapar-callstatactiveflag = 'X'.     "Tab.ac.stat. wird mitgeschrie
  stapar-deletefileflag     = ' '.     "Loeschen Stat.file RSSTTAT80
  stapar-deleteexternalflag = ' '.     "Loeschvermerk Externe Daten
  stapar-cuaprofile         = 'X'.     "Loeschvermerk Externe Daten
                                       "in RSSTAT60
  "B20K001672 QQI Defaultwert von TOCFLAG geaendert
  stapar-tocflag            = ' '.     "Generate Table of contensts
  stapar-onlytotalflag      = ' '.     "Reorg nur TOTAL server
*-------Numerische Werte------------------------------------------------
  stapar-res_days        = 2.          "Residenzzeit intensive Daten
  stapar-res_weeks       = 2.
  stapar-res_months      = 2.
  stapar-res_years       = 0.
  stapar-quiet_days      = stapar-res_days.   "Tol.zeit Statistikruhe
  stapar-res_dayssum     = stapar-res_days + 18."Res.zeit lose Daten
  stapar-res_weekssum    = stapar-res_weeks + 18.
  stapar-res_monthssum   = stapar-res_months + 12.
  stapar-res_yearsum     = stapar-res_years + 0.
  stapar-countlimit      = 30000.     "Max.Zaehler bei RSSTAT80, 0 =inf.
  stapar-maxobjects      = 100.        "Anzahl max. Objekte in RSSTAT60
  stapar-statfilesize    = 100.        "Max.Stat.file size (MB)
  stapar-aclimit         = 50.         "Table access threshold
  "fuer delete nach RSSTAT80, 0=inf.
*
*-------Muster fuer Automatisierte Reorglauefe--------------------------
  stapar-reorg80_hour    = ' X    XX X X X X X X X  '.
  stapar-reorg80_day     = 'XXXXXXX'.
  stapar-reorg60_total   = '      '.
  stapar-reorg60_hour    = ' X      X    X       X  '.
  stapar-reorg60_day     = 'XXXXXXX'.
*
*------Erweiterung paralleler Kollektor RSSTAT83------------------------
  stapar-max_no_rfc      = 999.
  stapar-cum_week        = 'L'.
  stapar-cum_month       = 'L'.
  stapar-cum_hitl        = 'X'.
  stapar-cum_mem         = 'X'.
  stapar-cum_acct        = 'X'.
  stapar-cum_rfc         = 'X'.
  stapar-cum_termio      = 'X'.
  stapar-cum_fcode       = 'X'.

endform.
*---------------------------------------------------------------------*
*       FORM CHECK_STAPAR_CONSISTENCY                                 *
*---------------------------------------------------------------------*
form check_stapar_consistency.
  data: string(24).
*
*-------Versionsnummer--------------------------------------------------
  if stapar-version = space.
    stapar-version = 'STANDARD'.
  endif.
  if stapar-vertype <> 'NEW' or stapar-vertype <> 'OLD'.
    stapar-vertype = 'NEW'.
  endif.
  if stapar-active = space.
    stapar-active = 'X'.
  endif.
*-------Flags-Values (X/ )---------------------------------------------
*
  if stapar-collectiveflag  <> 'X' and stapar-collectiveflag <> ' '
     and stapar-collectiveflag <> 'S'.
    stapar-collectiveflag  = 'X'.
  endif.
  if stapar-callstatactiveflag  <> 'X' and stapar-callstatactiveflag
    <> ' '.
    stapar-callstatactiveflag = 'X'.
  endif.
  if stapar-deletefileflag  <> 'X' and stapar-deletefileflag <> ' '.
    stapar-deletefileflag  = ' '.
  endif.
  if stapar-deleteexternalflag  <> 'X' and stapar-deleteexternalflag
    <> ' '.
    stapar-deleteexternalflag = ' '.
  endif.

  if stapar-tocflag  <> 'X' and stapar-tocflag <> ' '.
    stapar-tocflag  = 'X'.
  endif.

  if stapar-cuaprofile  <> 'X' and stapar-cuaprofile <> ' '.
    stapar-cuaprofile  = 'X'.
  endif.

  if stapar-onlytotalflag  <> 'X' and stapar-onlytotalflag <> ' '.
    stapar-onlytotalflag  = ' '.
  endif.
*-------Numerische Werte------------------------------------------------
  if stapar-res_days    <  2. stapar-res_days        =  2. endif.
  if stapar-res_days    > 31. stapar-res_days        = 31. endif.
  if stapar-res_weeks   <  2. stapar-res_weeks       =  2. endif.
  if stapar-res_weeks   > 20. stapar-res_weeks       = 20. endif.
  if stapar-res_months  <  2. stapar-res_months      =  2. endif.
  if stapar-res_months  > 20. stapar-res_months      = 20. endif.
  if stapar-res_years   <  0. stapar-res_years       =  0. endif.
  if stapar-res_years   >  5. stapar-res_years       =  5. endif.

  if stapar-res_dayssum < stapar-res_days.
    stapar-res_dayssum   = stapar-res_days.
  endif.
  if stapar-res_dayssum < 20.
    stapar-res_dayssum     =  20.
  endif.
  if stapar-res_dayssum > 99.
    stapar-res_dayssum     = 99.
  endif.

  if stapar-res_weekssum < stapar-res_weeks.
    stapar-res_weekssum  = stapar-res_days.
  endif.
  if stapar-res_weekssum <  5.
    stapar-res_weekssum   =  5.
  endif.
  if stapar-res_weekssum > 31.
    stapar-res_weekssum   = 31.
  endif.

  if stapar-res_monthssum < stapar-res_months.
    stapar-res_monthssum = stapar-res_months.
  endif.
  if stapar-res_monthssum <    5.
    stapar-res_monthssum  =  5.
  endif.
  if stapar-res_monthssum >   24.
    stapar-res_monthssum  = 24.
  endif.

  if stapar-res_yearsum  <   0.
    stapar-res_yearsum  =  0.
  endif.
  if stapar-res_yearsum  >   5.
    stapar-res_yearsum  = 5.
  endif.
  if stapar-res_yearsum < stapar-res_years.
    stapar-res_yearsum = stapar-res_years.
  endif.

  if stapar-countlimit < 0. stapar-countlimit = 0. endif.

  if stapar-statfilesize < 0.  stapar-statfilesize = 0.  endif.
  "fuer delete nach RSSTAT80, 0=inf.

  if stapar-maxobjects < 0.
    stapar-maxobjects = 0.
  endif.

  if stapar-aclimit < 0.
    stapar-aclimit = 0.
  endif.
  if stapar-aclimit > 1000.
    stapar-aclimit = 1000.
  endif.


*
*-------Muster fuer Automatisierte Reorglauefe--------------------------
  string = stapar-reorg80_hour.
  condense string no-gaps.
  if string ca ' '.
    if sy-fdpos < 2.
      stapar-reorg80_hour  = ' X    XX X X X X X X X  '.
    endif.
  endif.

  string = stapar-reorg80_day.
  condense string no-gaps.
  if string ca ' '.
    if sy-fdpos < 2.
      stapar-reorg80_day  = 'XXXXXXX'.
    endif.
  endif.

  string = stapar-reorg60_day.
  condense string no-gaps.
  if string ca ' '.
    if sy-fdpos < 2.
      stapar-reorg60_day  = 'XXXXXXX'.
    endif.
  endif.

  string = stapar-reorg60_hour.
  condense string no-gaps.
  if string ca ' '.
    if sy-fdpos < 2.
      stapar-reorg60_hour  = ' X      X    X       X  '.
    endif.
  endif.

*------Erweiterung paralleler Kollektor RSSTAT83
  if stapar-max_no_rfc = 0.
    stapar-max_no_rfc = 999.
  endif.
  if stapar-cum_week <> 'X' and stapar-cum_week <> ' '
  and stapar-cum_week <> 'L'.     "late cumul. as of rel. 4.6C
    stapar-cum_week = 'L'.
  endif.
  if stapar-cum_month <> 'X' and stapar-cum_month <> ' '
  and stapar-cum_month <> 'L'.     "late cumul. as of rel. 4.6C
    stapar-cum_month = 'L'.
  endif.
  if stapar-cum_hitl <> 'X' and stapar-cum_hitl <> ' '.
    stapar-cum_hitl = 'X'.
  endif.
  if stapar-cum_mem <> 'X' and stapar-cum_mem <> ' '.
    stapar-cum_mem = 'X'.
  endif.
  if stapar-cum_acct <> 'X' and stapar-cum_acct <> ' '.
    stapar-cum_acct = 'X'.
  endif.
  if stapar-cum_rfc <> 'X' and stapar-cum_rfc <> ' '.
    stapar-cum_rfc = 'X'.
  endif.
  if stapar-cum_termio <> 'X' and stapar-cum_termio <> ' '.
    stapar-cum_termio = 'X'.
  endif.
  if stapar-cum_fcode <> 'X' and stapar-cum_fcode <> ' '.
    stapar-cum_fcode = 'X'.
  endif.

endform.
*---------------------------------------------------------------------*
*       FORM GET_STAPAR                                               *
*---------------------------------------------------------------------*
* RETCODE = 0.   "Spezifizierte Parameter gefunden                    *
* RETCODE = 2.   "Standarparameter nicht in MONI transportierbar      *
* RETCODE = 4.   "Standardparmeter genommen und in MONI transportiert *
* RETCODE = 6.   "Spezifizierte Parameter gefunden,nicht aktive jedoch*
*---------------------------------------------------------------------*
form get_stapar using retcode.
  data: key like moni-srtfd,
        as_key like moni-srtfd.

  data: wl_param_wa like sapwlparam.

* --- 'normale' Statistik --- *
*-Versuche Parameter aus Hauptspeicher zu erhalten----------------------
  import stapar from memory id 'REORG-PARAMETERS'.
  if sy-subrc <> 0.
*---Holen Parameter aus Datenbank---------------------------------------
    key+00(02) = '--'.
    key+02(20) = 'TOTAL   REORG PARAM.'.
    import stapar from database moni(pa) id key.
  endif.
*-Parameter nicht vorhanden/ nicht lesbar-------------------------------
  if sy-subrc <> 0.
    retcode = sy-subrc.
    refresh: stapar.
    clear:   stapar.
    perform get_standard_stapar.
    perform check_stapar_consistency.
    append stapar.
    export stapar to database moni(pa) id key.
    export stapar to memory id key.
    if sy-subrc <> 0.
      retcode = 2.
    endif.
    delete stapar index 1.
  else.
    retcode = 6.

*-----js 11/99
    read table stapar with key active = 'X'.

    loop at stapar into wl_param_wa where active = 'X'.
        retcode = 0.
        delete stapar index sy-tabix.
    endloop.

  endif.

* --- Anwendungs-Statistik --- *
*-Versuche Parameter aus Hauptspeicher zu erhalten----------------------
  import as_coll_par from memory id 'AS-REORG-PARAMETERS'.
  if sy-subrc <> 0.
*---Holen Parameter aus Datenbank---------------------------------------
    as_key+00(02) = '--'.
    as_key+02(20) = 'TOTAL   AS REORG PAR'.
    import as_coll_par from database moni(pa) id as_key.
  endif.
*-Parameter nicht vorhanden/ nicht lesbar-------------------------------
  if sy-subrc <> 0.
    retcode = sy-subrc.
    refresh: as_coll_par.
    clear:   as_coll_par.
*-------Standardparameter für den Application Statistic Collector
    as_coll_par-countlimit = 30000.   "Max.Zaehler bei RSSTAT89, 0 =inf.
    as_coll_par-astatfilesze = 50.     "Max.Appl.Stat.file size (MB)
                                       "fuer delete nach RSSTAT89.
*   as_coll_par-collectiveflag = ' '. "TOTAL-Server wird nicht aufgebaut
*-------Parameterkonsistenz prüfen
    if as_coll_par-countlimit < 0. as_coll_par-countlimit = 0. endif.
    if as_coll_par-astatfilesze < 0.
      as_coll_par-astatfilesze = 0.
    endif.
    append as_coll_par.
    export as_coll_par to database moni(pa) id as_key.
    export as_coll_par to memory id as_key.
    refresh as_coll_par.
*    DELETE as_coll_par INDEX 1.
    exit.
  else.
    read table as_coll_par index 1.
    refresh as_coll_par.
*    LOOP AT as_coll_par.
*      DELETE as_coll_par INDEX sy-tabix.
*      EXIT.
*    ENDLOOP.
  endif.

endform.
*---------------------------------------------------------------------*
*       FORM ACTIVATE_STAPAR                                          *
*---------------------------------------------------------------------*
form activate_stapar.
  data: key like moni-srtfd,
        as_key like moni-srtfd,
        lines like sy-tabix.
*
*-------Administrative Information--------------------------------------
  stapar-active = 'X'.
  stapar-lastchangedate  = sy-datum.   "Versionsaenderungstag
  stapar-lastchangetime  = sy-uzeit.
  stapar-lastchangeuser  = sy-uname.
  stapar-sincedate       = sy-datum.   "Ab wann sind Aenderungen akt
  append stapar.
  append as_coll_par.
*-Holen Parameter-------------------------------------------------------
  key+00(02) = '--'.
  key+02(20) = 'TOTAL   REORG PARAM.'.
  as_key+00(02) = '--'.
  as_key+02(20) = 'TOTAL   AS REORG PAR'.
  export stapar to database moni(pa) id key.
  export stapar to memory id key.
  export as_coll_par to database moni(pa) id as_key.
  export as_coll_par to memory id as_key.
  if sy-subrc = 0.
    message s081.
  else.
    message s082.
  endif.
  describe table stapar lines lines.
  if lines > 0.
    delete stapar index lines.
  endif.
  refresh as_coll_par.
*  DESCRIBE TABLE as_coll_par LINES lines.
*  IF lines > 0.
*    DELETE as_coll_par INDEX lines.
*  ENDIF.
endform.
*---------------------------------------------------------------------*
*       FORM ACTIVATE_STAPAR_2                                        *
* Wie ACTIVATE_STAPAR, jedoch ohne Aenderung der Lastchange-Daten     *
*---------------------------------------------------------------------*
form activate_stapar_2.
  data: key like moni-srtfd,
        lines like sy-tabix.
*
*-------Administrative Information--------------------------------------
  stapar-active = 'X'.
  append stapar.
*-Holen Parameter-------------------------------------------------------
  key+00(02) = '--'.
  key+02(20) = 'TOTAL   REORG PARAM.'.
  export stapar to database moni(pa) id key.
  export stapar to memory id key.
  if sy-subrc = 0.
    message s081.
  else.
    message s082.
  endif.
  describe table stapar lines lines.
  if lines > 0.
    delete stapar index lines.
  endif.
endform.
