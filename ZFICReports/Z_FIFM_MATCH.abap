report rffmrc07 message-id fi
                line-size 90
                no standard page heading.
*----------------------------------------------------------------------*
*                                                                      *
* Dieser Report gleich die EINZELPOSTEN der FI Bank/Verrechnungs und   *
* gegebenenfalls Steuerkonten gegen die HHM EINZELPOSTEN ab.           *
* Die Voraussetzungen für einen erfolgreichen Lauf dieses Report sind  *
* sehr umfangreich. Folgende Gegebenheiten sind einzuhalten            *
* CUSTOMIZING                                                          *
*  Ausweis auf Zahlungsdatum                                           *
*  RE-Fortschreibung (WE/RE)                                           *
*  Lagerkonzept (Manuell kontiert oder Lagerfinanzstelle)              *
*                                                                      *
* AKTIVITÄTEN                                                          *
*  Ausgleich ist durchgeführt                                          *
*  RFFMS200 ist gelaufen                                               *
*  Akonto-Zahlungen wurden ins HHM gebucht                             *
*  Skonto wurde ins HHM gebucht                                        *
*  MWST-Anpassungen wurden ins HHM gebucht                             *
*                                                                      *
* BUCHNUGSRESTRIKTIONEN                                                *
*  Keine 90-60-30 Buchungen                                            *
*  Keine Kontokorrent-Umbuchungen                                      *
*  Keine finanzkreisübergreifenden Buchungen                           *
*                                                                      *
* CUSTOMIZING                                                          *
*  Alle beteiligten Buchungskreise und der Finanzkreis haben die       *
*  gleiche Geschäftsjahrevariante                                      *
*                                                                      *
*----------------------------------------------------------------------*

*-- Includedateien
include:
  ififmcon_value_types,
  ififmcon_bool.

*-Typepools------------------------------------------------------------*
type-pools:
  fmfi.

*-- Konstanten
constants:
  con_no_error       type c value 'A',
  con_fm_doc_missing type c value 'B',
  con_false_amount   type c value 'C',
  con_false_date     type c value 'D'.

*-- Tabellendeklaration
tables:
  fmifiit,
  bkpf,
  bseg,
  skb1,
  t001,
  fm01,
  bvor,
  fmrc07,
  fmsd07.

*-- Datendeklarationen
data:
  begin of g_t_bkpf occurs 0,
    bvorg like bkpf-bvorg,
    bukrs like bkpf-bukrs,
    belnr like bkpf-belnr,
    gjahr like bkpf-gjahr,
    budat like bkpf-budat,
    cpudt like bkpf-cpudt,
    stblg like bkpf-stblg,
  end   of g_t_bkpf,

  begin of g_t_fmifiit occurs 0,
    belnr   like fmifiit-vobelnr,
    bukrs   like fmifiit-bukrs,
    gjahr   like fmifiit-vogjahr,
    fkbtr   like fmifiit-fkbtr,
    shkzg   like bseg-shkzg,
    fmbelnr like fmifiit-fmbelnr,
    fikrs   like fmifiit-fikrs,
    btart   like fmifiit-btart,
    stunr   like fmifiit-stunr,
    fipos   like bseg-fipos,
    budat   like bkpf-budat,
    cpudt   like bkpf-budat,
    storn   type c,
    rerror  type c,
  end of g_t_fmifiit,

  g_t_res_outp        like fmrc07      occurs 0 with header line,
  g_t_dis_docs        like fmrc07      occurs 0 with header line,
  g_t_res_docs        like fmrc07      occurs 0 with header line,
  g_t_docs_visible    like fmrc07      occurs 0 with header line,
  g_t_docs_invisible  like fmrc07      occurs 0 with header line,
  g_t_days_rc         like fmsd07      occurs 0 with header line,
  l_t_days_rc         like fmsd07      occurs 0 with header line,
  g_t_days_rca        like fmsd07      occurs 0 with header line,
  g_t_bkpf_all        like g_t_bkpf    occurs 0 with header line,
  g_t_bseg            like g_t_fmifiit occurs 0 with header line,
  g_t_result          like g_t_fmifiit occurs 0 with header line,
  g_t_t001            like t001        occurs 0 with header line,
  g_t_fm01            like fm01        occurs 0 with header line,
  g_t_bvor            like bvor        occurs 0 with header line,

* by ig.moon 6/24/2013 {
  g_t_outlist    type standard table of fmrc07_output
                          with non-unique key bukrs belnr gjahr,
* }


  g_end_of_sel type c,
  g_field(20)  type c,
  l_idx        type i.

data gt_fmifiit_a type table of fmifiit with header line.
data gt_bkpf_a type table of bkpf with header line.

ranges:
  g_t_ledger   for  fmifiit-rldnr.

************************************************************************
* SELECTION
************************************************************************

selection-screen skip.

selection-screen begin of block date with frame title text-020.
select-options:
  s_fikrs for fmifiit-fikrs memory id fik
                            matchcode object fikr obligatory.

*-- Selektion über Datum
select-options:
  s_belnr for fmifiit-vobelnr,              " NO-DISPLAY,
  s_cpudt for bkpf-cpudt,
  s_budat for bkpf-budat.

parameters:
  p_today as checkbox.
selection-screen end of block date.

selection-screen skip.

*-- Steuerung
selection-screen begin of block control with frame title text-010.
parameters:
  p_test as checkbox default 'X',
  p_prot as checkbox default 'X',
  p_eron as checkbox default 'X'.
selection-screen end of block control.

*- U1 Start
include ziarch_comm01.
*- U1 End

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
at selection-screen.
  if not p_test is initial and p_prot is initial.
    message e178.
*    Parameterauswahl ist nicht sinnvoll
  endif.

************************************************************************
* AT START-OF-SELECTION.
************************************************************************
start-of-selection.

* Andy Modification
*  PERFORM test_active TABLES
*                          g_t_ledger.

*-- Alle Finanzkreise holen
  select * from fm01
   into  table g_t_fm01
  where  fikrs in s_fikrs.

*-- Selektionsbereich setzen
  perform set_selection_date tables
                               s_budat
                               s_cpudt
                             using
                               p_today.

*-- Über Finanzkreise loopen
  loop at g_t_fm01.

*-- Buchungskreise zum Fikrs holen
    call function 'BUKRS_GET_FROM_FIKRS'
      exporting
        ip_fikrs = g_t_fm01-fikrs
      tables
        t_t001   = g_t_t001.

*-- Köpfe der FI Belege holen
    perform fi_get_header tables
                            g_t_bkpf_all
                            g_t_days_rc
                            g_t_bvor.

    describe table g_t_bkpf_all lines sy-tfill.
    check sy-tfill > 0.

    l_t_days_rc[] = g_t_days_rc[].

*-- Die einzelnen Tage abarbeiten
    loop at g_t_days_rc.

*-- Päckchen bilden
      perform get_bkpf_pack tables
                              g_t_bkpf_all
                              g_t_bkpf
                              l_t_days_rc.

*-- Summen über alle Konten selektieren(G_T_GLT0 füllen mit RACCT = ' ')
      perform fi_get_line_items tables
                                    g_t_bseg
                                    g_t_bkpf.

*-- Einzelposten einlesen FM
      perform fm_get_line_items tables
                                  g_t_bkpf
                                  g_t_fmifiit
                                  g_t_ledger.

*-- Vergleichen
      perform fi_comp_fm_doc tables
                                 g_t_bseg
                                 g_t_fmifiit
                                 g_t_res_docs.

*-- Fehler durch Bukrs-übergreifende Belege aussortieren
      perform check_cc tables
                          g_t_bkpf_all
                          g_t_bvor
                          g_t_res_docs.

*-- Abgeglichener Tag wegschreiben
      if p_test is initial.
        perform write_res_pack tables
                                   g_t_res_docs
                                 using
                                   g_t_days_rc
                                   con_off.
      endif.

*-- Belege fürs Reporting merken
      if not p_prot is initial.
        append lines of g_t_res_docs to g_t_res_outp.
      endif.
      refresh g_t_res_docs.

*-- Housekeeping pro Tag
      refresh: g_t_res_docs,
               g_t_fmifiit,
               g_t_bseg,
               g_t_bkpf.

    endloop.

*-- Housekeeping pro Finanzkreis
    refresh: g_t_bkpf_all,
             g_t_days_rc,
             g_t_t001,
             g_t_bvor.

    sort g_t_res_docs.
    delete adjacent duplicates from g_t_res_docs.

*   Rest wegschreiben
    if p_test is initial.
      perform write_res_pack tables
                               g_t_res_docs
                             using
                               g_t_days_rc
                               con_on.
    endif.

  endloop.

************************************************************************
* END-OF-SELECTION.
************************************************************************
end-of-selection.

*Andy modification
*  SET PF-STATUS 'LIST'.

*----- Ausgabetabelle retten
  append lines of g_t_res_outp to g_t_dis_docs.

*-- Ergebnis ausgeben
  if p_prot is initial.
    format color col_normal.
    write / text-240.
  else.
    perform display_days_reconciled.

* by ig.moon 6/24/2013 {
*    PERFORM display_line_items(rffmsd07)
*                                      TABLES
*                                        g_t_dis_docs.

    perform transfer_line_items(rffmsd07) tables g_t_dis_docs
                                                 g_t_outlist.

    perform display_list(rffmsd07) using g_t_outlist
                                         text-400.
* }

  endif.

************************************************************************
* AT USER-COMMAND
************************************************************************
at user-command.
  case sy-ucomm.

*----- Alle Belege sichtbar machen
    when 'ALSI'.
      refresh g_t_dis_docs.
      append lines of g_t_res_outp to g_t_dis_docs.
      perform display_days_reconciled.
      loop at g_t_dis_docs.
        g_t_dis_docs-invisible = con_off.
        modify g_t_dis_docs transporting invisible.
      endloop.
      perform display_line_items(rffmsd07) tables
                                              g_t_dis_docs.

*----- Ausgewählte Belege unsichtbar machen
    when 'UNSI'.
      l_idx = 0.
      do.

*----- Buchungskreis ermitteln
        l_idx = l_idx + 1.
        clear g_t_dis_docs-bukrs.
        read line l_idx field value g_t_dis_docs-bukrs.
        if sy-subrc <> 0. exit. endif.
        check not g_t_dis_docs-bukrs is initial.

*----- Geschäftsjahr ermitteln
        l_idx = l_idx + 1.
        clear g_t_dis_docs-gjahr.
        read line l_idx field value g_t_dis_docs-gjahr.
        check not g_t_dis_docs-gjahr is initial.

*----- Ausgewählte Belege ermitteln
        do.
          l_idx = l_idx + 1.
          clear g_t_dis_docs-invisible.
          clear g_t_dis_docs-belnr.
          read line l_idx field value g_t_dis_docs-invisible
                                      g_t_dis_docs-belnr.

          search sy-lisel for text-120.
          if sy-subrc = 0. exit. endif.

          if g_t_dis_docs-invisible = con_off.
            loop at g_t_res_outp where bukrs = g_t_dis_docs-bukrs
                                   and belnr = g_t_dis_docs-belnr
                                   and gjahr = g_t_dis_docs-gjahr.
              move-corresponding g_t_res_outp to g_t_docs_visible.
              g_t_docs_visible-invisible = g_t_dis_docs-invisible.
              append g_t_docs_visible.
            endloop.
          elseif g_t_dis_docs-invisible = con_on.
            loop at g_t_res_outp where bukrs = g_t_dis_docs-bukrs
                                   and belnr = g_t_dis_docs-belnr
                                   and gjahr = g_t_dis_docs-gjahr.
              move-corresponding g_t_res_outp to g_t_docs_invisible.
              g_t_docs_invisible-invisible = g_t_dis_docs-invisible.
              append g_t_docs_invisible.
            endloop.
          endif.
        enddo.
      enddo.

*----- Unsichtbare Belege modifizieren
      perform update_invisible tables
                                 g_t_docs_invisible.

*----- Nur sichbare Belege ausgeben
      refresh g_t_dis_docs.
      append lines of g_t_docs_visible to g_t_dis_docs.
      perform display_days_reconciled.
      perform display_line_items(rffmsd07) tables
                                              g_t_dis_docs.
      refresh g_t_docs_visible.
      refresh g_t_docs_invisible.
      clear   g_t_docs_visible.
      clear   g_t_docs_invisible.

*----- Alle Belege markieren
    when 'ACTIV'.
      l_idx = 0.
      do.
        l_idx = sy-index + 1.
        read line l_idx field value g_t_dis_docs-invisible.
        if sy-subrc = 0.
          modify current line field value g_t_dis_docs-invisible
                                                            from con_on.
        else.
          exit.
        endif.
      enddo.

*----- Alle Markierungen löschen
    when 'INACTIV'.
      l_idx = 0.
      do.
        l_idx = sy-index + 1.
        read line l_idx field value g_t_dis_docs-invisible.
        if sy-subrc = 0.
          modify current line field value g_t_dis_docs-invisible
                                                           from con_off.
        else.
          exit.
        endif.
      enddo.
  endcase.
  sy-lsind = 0.

************************************************************************
* AT LINE-SELECTION
************************************************************************
at line-selection.

*-- Ausgewählter Belege anzeigen
  get cursor field g_field.

  if g_field =  'G_T_DIS_DOCS-BTRFM'.
    read table g_t_dis_docs
      with key bukrs = g_t_dis_docs-bukrs
               belnr = g_t_dis_docs-belnr
               gjahr = g_t_dis_docs-gjahr.
    check sy-subrc = 0.

    if not ( g_t_dis_docs is initial ).
      perform show_fm_document(rffmsd07)
                                      using
                                        g_t_dis_docs.
    else.
      message i605.
    endif.
  else.
    set parameter id 'BUK' field g_t_dis_docs-bukrs.
    set parameter id 'BLN' field g_t_dis_docs-belnr.
    set parameter id 'GJR' field g_t_dis_docs-gjahr.

    if not ( g_t_dis_docs is initial ).
      call transaction 'FB03' and skip first screen.
    else.
      message i605.
    endif.
  endif.
  clear g_t_dis_docs.

************************************************************************
* Unterprorgamme
************************************************************************

*---------------------------------------------------------------------*
*       FORM FI_GET_LINE_ITEMS                                        *
*---------------------------------------------------------------------*
*       FI EP selektieren und ans FM angleichen                       *
*---------------------------------------------------------------------*
form fi_get_line_items tables
                         c_t_bseg     structure g_t_bseg
                         u_t_bkpf     structure g_t_bkpf.

  data:
    l_t_bkpf like g_t_bkpf occurs 0 with header line,
    l_t_bseg like g_t_bseg occurs 0 with header line.

  append lines of u_t_bkpf    to l_t_bkpf.

  describe table l_t_bkpf lines sy-tfill.
  check sy-tfill > 0.

  sort l_t_bkpf by belnr bukrs gjahr.

*-- Alle relevanten Einzelposten einlesen
  select dmbtr as fkbtr
         belnr gjahr
         bukrs fipos
         shkzg buzei    from bseg
  appending corresponding fields of table l_t_bseg
  for all entries in l_t_bkpf
  where belnr = l_t_bkpf-belnr
    and bukrs = l_t_bkpf-bukrs
    and gjahr = l_t_bkpf-gjahr
    and ktosl <> 'BUV'
    and xref1 <> fmfi_con_euro_fi.

  describe table l_t_bseg lines sy-tfill.
  check sy-tfill > 0.

  sort l_t_bseg by belnr bukrs gjahr.

*-- FI Belege noch aufbereiten und evtl. löschen
  perform clean_fi_documents tables
                               l_t_bseg
                               c_t_bseg
                               l_t_bkpf.

  sort c_t_bseg    by belnr bukrs gjahr.
endform.                    "fi_get_line_items

*---------------------------------------------------------------------*
*       FORM FM_GET_LINE_ITEMS                                        *
*---------------------------------------------------------------------*
*       FM Einzelposten selektieren                                   *
*---------------------------------------------------------------------*
form fm_get_line_items tables
                           u_t_bkpf     structure g_t_bkpf
                           c_t_fmifiit  structure g_t_fmifiit
                           u_t_ledger   structure range_c2.

  data:
    l_t_bkpf like g_t_bkpf occurs 0 with header line.

  append lines of u_t_bkpf    to l_t_bkpf.

  describe table l_t_bkpf lines sy-tfill.
  check sy-tfill > 0.

  select btart   fikrs
         fkbtr   stunr
         fipex   as fipos
         vobukrs as bukrs
         vobelnr as belnr
         vogjahr as gjahr
         vobuzei as buzei
         fmbelnr as fmbelnr
         zhldt   as budat from fmifiit
   appending corresponding fields of table g_t_fmifiit
    for all entries in l_t_bkpf
  where vobelnr =  l_t_bkpf-belnr
    and vobukrs =  l_t_bkpf-bukrs
    and vogjahr =  l_t_bkpf-gjahr
and ( wrttp  =  wrttp9  or
      wrttp  =  wrttp9a or
      wrttp  =  wrttp9b )
    and btart  <>  fmfi_con_btart_acold
    and btart  <>  fmfi_con_btart_acnew
    and rldnr  in  u_t_ledger.

*- U1 Start
  if p_arch eq 'X'.
    perform archive_read_fmifiit tables l_t_bkpf u_t_ledger.
  endif.
*- U1 End

endform.                    "fm_get_line_items

*---------------------------------------------------------------------*
*       FORM SET_SELECTION_DATE                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form set_selection_date tables
                          c_budat structure s_cpudt
                          c_cpudt structure s_cpudt
                        using
                          u_today.

*-- Buchungsdatum überschreibt CPU-Datum
  describe table c_budat lines sy-tfill.

  if sy-tfill > 0.
    refresh c_cpudt.
  else.

*-- Wurde kein Datum zur Selektion angegeben, den heutigen
*-- Tag als Selektionsdatum nehmen.
    check not u_today is initial.

    refresh c_cpudt.
    s_cpudt-sign   = 'I'.
    s_cpudt-option = 'EQ'.
    s_cpudt-low    = sy-datum.
    append s_cpudt.

    read table s_cpudt  index 1.

*-- Geschäftsjahr für READ_QUICK Bausteine
*   call function 'FM_DATE_TO_PERIODE_CONVERT'
*        exporting
*             i_date  = s_cpudt-low
*             i_fikrs = g_t_fm01-fikrs
*        importing
*             e_gjahr = g_gjahr.
  endif.
endform.                    "set_selection_date

*---------------------------------------------------------------------*
*       FORM GET_BKPF_PACK                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  U_T_BKPF_IN                                                   *
*  -->  U_T_BKPF_OUT                                                  *
*---------------------------------------------------------------------*
form get_bkpf_pack tables
                     c_t_bkpf_all structure g_t_bkpf
                     c_t_bkpf     structure g_t_bkpf
                     u_t_days_rc  structure g_t_days_rc.

  statics:
    s_cpudt          like sy-datum,
    s_index_next_day like sy-tabix value 2,
    s_index_from     like sy-tabix value 1.

  data:
    l_index_to       like sy-tabix,
    l_flg_end        type c.

  if s_cpudt is initial.
    read table u_t_days_rc index 1.
    s_cpudt = u_t_days_rc-cpudt.
  endif.

*-- Index auf nächstes Erfassungsdatum bestimmen
  loop at u_t_days_rc from s_index_next_day where cpudt <> s_cpudt.
    s_cpudt          = u_t_days_rc-cpudt.
    s_index_next_day = sy-tabix.
    exit.
  endloop.
  if sy-subrc <> 0.
    describe table c_t_bkpf_all lines l_index_to.
    l_flg_end = con_on.
  else.
    read table c_t_bkpf_all with key cpudt = s_cpudt
      binary search.
    l_index_to = sy-tabix - 1.
  endif.

*-- Zeilen übertragen
  append lines of c_t_bkpf_all from s_index_from to l_index_to
               to c_t_bkpf.

  if l_flg_end = con_on.
    s_index_from     = 1.
    s_index_next_day = 2.
  else.
    s_index_from     = l_index_to       + 1.
    s_index_next_day = s_index_next_day + 1.
  endif.

  sort c_t_bkpf     by       belnr bukrs gjahr.
endform.                    "get_bkpf_pack
*---------------------------------------------------------------------*
*       FORM FI_GET_HEADER                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form fi_get_header tables
                     c_t_bkpf     structure g_t_bkpf
                     c_t_days_rc  structure g_t_days_rc
                     c_t_bvor     structure g_t_bvor.

  data:
    l_t_docs_rc like fmrc07   occurs 0 with header line,
    l_t_bkpf_cc like g_t_bkpf occurs 0 with header line.

  describe table g_t_t001 lines sy-tfill.
  check sy-tfill > 0.

*-- Einzelpostenköpfe selektieren
  select bvorg bukrs belnr gjahr budat cpudt from bkpf
    into corresponding fields of table c_t_bkpf
     for all entries in g_t_t001
   where bukrs = g_t_t001-bukrs
     and belnr in s_belnr
     and cpudt in s_cpudt
     and budat in s_budat
     and bstat =  ' '.

*- U1 Start
  if p_arch eq 'X'.
    perform archive_read_bkpf tables g_t_t001.
  endif.
*- U1 End

**-- Buchungskreisübergeifende Belege holen
*  LOOP AT c_t_bkpf WHERE NOT bvorg IS initial.
*    APPEND c_t_bkpf TO l_t_bkpf_cc.
*  ENDLOOP.
*
*  DESCRIBE TABLE l_t_bkpf_cc LINES sy-tfill.
*  IF sy-tfill > 0.
*    SELECT * FROM bvor
*       INTO TABLE c_t_bvor
*        FOR ALL ENTRIES IN l_t_bkpf_cc
*      WHERE bvorg =  l_t_bkpf_cc-bvorg.
*
*    DESCRIBE TABLE c_t_bvor LINES sy-tfill.
*    IF sy-tfill > 0.
*      SELECT bvorg bukrs belnr
*             gjahr budat
*             cpudt       FROM bkpf
*        INTO CORRESPONDING FIELDS OF TABLE l_t_bkpf_cc
*         FOR ALL ENTRIES IN c_t_bvor
*       WHERE bukrs = c_t_bvor-bukrs
*         AND gjahr = c_t_bvor-gjahr
*         AND belnr = c_t_bvor-belnr.
*
*      SORT c_t_bvor.
*    ENDIF.
*
*    DESCRIBE TABLE l_t_bkpf_cc LINES sy-tfill.
*    IF sy-tfill > 0.
*
*      SORT c_t_bkpf BY belnr bukrs gjahr.
*
*      LOOP AT l_t_bkpf_cc.
*        READ TABLE c_t_bkpf
*          WITH KEY belnr = l_t_bkpf_cc-belnr
*                   bukrs = l_t_bkpf_cc-bukrs
*                   gjahr = l_t_bkpf_cc-gjahr
*          BINARY SEARCH.
*        IF sy-subrc <> 0.
*          APPEND l_t_bkpf_cc TO c_t_bkpf.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.

  sort c_t_bkpf by cpudt belnr bukrs gjahr.

*-- Abzugleichene Buchungsdaten ermitteln
  loop at c_t_bkpf.
    read table g_t_t001
      with key bukrs = c_t_bkpf-bukrs.
    if not sy-subrc is initial.

*-- Aus neuem Buchungskreis kann neuer FIKRS entstehen
      clear g_t_t001.
      g_t_t001-bukrs = c_t_bkpf-bukrs.
      call function 'FMFK_GET_FIKRS_FROM_BUKRS'
        exporting
          i_bukrs = g_t_t001-bukrs
        importing
          e_fikrs = g_t_t001-fikrs.
      append g_t_t001.
    endif.
    c_t_days_rc-cpudt = c_t_bkpf-cpudt.
    c_t_days_rc-fikrs = g_t_t001-fikrs.
    collect c_t_days_rc.
  endloop.

  sort c_t_days_rc  by fikrs cpudt.
endform.                    "fi_get_header

*---------------------------------------------------------------------*
*       FORM CLEAN_FI_DOCUMENTS                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  U_T_BSEG                                                      *
*---------------------------------------------------------------------*
form clean_fi_documents tables
                          u_t_bseg     structure g_t_bseg
                          c_t_bseg     structure g_t_bseg
                          u_t_bkpf     structure g_t_bkpf.

  data:
    l_f_fmci like fmci.

  clear: u_t_bkpf-budat.

  loop at u_t_bseg where not fipos is initial.

*----- Aus neuem Buchungskreis kann neuer FIKRS entstehen
    read table g_t_t001
      with key bukrs = u_t_bseg-bukrs.

    if sy-subrc <> 0.
      call function 'FMFK_GET_FIKRS_FROM_BUKRS'
        exporting
          i_bukrs = u_t_bseg-bukrs
        importing
          e_fikrs = g_t_t001-fikrs.
      g_t_t001-bukrs = u_t_bseg-bukrs.
      append g_t_t001.
    endif.

    call function 'fmci_READ_QUICK'
      exporting
        ip_fikrs          = g_t_t001-fikrs
        ip_fipos          = u_t_bseg-fipos
        ip_gjahr          = u_t_bseg-gjahr
        ip_flg_buffer_all = con_on
      importing
        f_fmci           = l_f_fmci.

    if l_f_fmci-fivor = '90' or l_f_fmci-fivor = '80'.
      read table u_t_bkpf
        with key belnr = u_t_bseg-belnr
                 bukrs = u_t_bseg-bukrs
                 gjahr = u_t_bseg-gjahr
        binary search.

      c_t_bseg       = u_t_bseg.
      if c_t_bseg-shkzg = 'H'.
        multiply c_t_bseg-fkbtr by -1.
      endif.
      if not u_t_bkpf-stblg is initial.
        c_t_bseg-storn = con_on.
      endif.
      c_t_bseg-budat = u_t_bkpf-budat.
      c_t_bseg-cpudt = u_t_bkpf-cpudt.
      append c_t_bseg.
    endif.
  endloop.
endform.                    "clean_fi_documents

*---------------------------------------------------------------------*
*       FORM FI_COMP_FM_DOC                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form fi_comp_fm_doc tables
                      u_t_bseg     structure g_t_bseg
                      u_t_fmifiit  structure g_t_fmifiit
                      c_t_res_docs structure g_t_res_docs.

  data:
    begin of l_t_fm_amount occurs 0,
      fm_amount like fmifiit-fkbtr,
      fm_budat  like fmifiit-zhldt,
    end   of l_t_fm_amount,

    l_len_fm_amount like sy-tfill,
    l_fi_amount     like fmifiit-fkbtr,
    l_budatfi       like g_t_res_docs-budatfi,
    l_f_bseg        like g_t_fmifiit.

  sort u_t_fmifiit by belnr bukrs gjahr.

  loop at u_t_bseg.

    add u_t_bseg-fkbtr to l_fi_amount.
    l_f_bseg = u_t_bseg.

    at end of gjahr.
      check l_fi_amount <> 0.

*-- Fehlerdatei aufbauen
      clear c_t_res_docs.
      c_t_res_docs-bukrs   = l_f_bseg-bukrs.
      c_t_res_docs-gjahr   = l_f_bseg-gjahr.
      c_t_res_docs-belnr   = l_f_bseg-belnr.
      c_t_res_docs-cpudt   = l_f_bseg-cpudt.
      c_t_res_docs-budatfi = l_f_bseg-budat.
      c_t_res_docs-btrfi   = l_fi_amount.

      read table g_t_t001
        with key bukrs = l_f_bseg-bukrs.
      c_t_res_docs-fikrs = g_t_t001-fikrs.

      read table u_t_fmifiit
        with key belnr = l_f_bseg-belnr
                 bukrs = l_f_bseg-bukrs
                 gjahr = l_f_bseg-gjahr
        binary search.

*-- Fehlerauswertung
      if sy-subrc <> 0.
        if l_f_bseg-storn is initial.
          c_t_res_docs-error = con_fm_doc_missing.
          append c_t_res_docs.
        elseif p_eron is initial.

*-- Falls storniert wurde, sind FI-Beträge sowieso null
*--  und es ist ok, wenn kein FM-Beleg existiert
          c_t_res_docs-error = con_no_error.
          append c_t_res_docs.
        endif.
      else.

        loop at u_t_fmifiit from sy-tabix.

*-- Summe der FM Belegzeilen berechnen
          l_t_fm_amount-fm_amount = u_t_fmifiit-fkbtr.
          l_t_fm_amount-fm_budat  = u_t_fmifiit-budat.
          collect l_t_fm_amount.

*-- Belegende
          at end of gjahr.
            exit.
          endat.
        endloop.

*-- Nullzeilen interessieren nicht
        delete l_t_fm_amount where fm_amount =  0.
        describe table l_t_fm_amount lines l_len_fm_amount.

        if l_len_fm_amount > 1.

*-- Fortschreibedatum ( Jeder FI Beleg hat nur ein Buchungsdatum )
          loop at l_t_fm_amount.
            c_t_res_docs-budatfm = l_t_fm_amount-fm_budat.
            c_t_res_docs-btrfm   = l_t_fm_amount-fm_amount.
            c_t_res_docs-error   = con_false_date.
            append c_t_res_docs.
          endloop.
        else.
          read table l_t_fm_amount index 1.
          if sy-subrc <> 0.
            l_t_fm_amount-fm_amount = 0.
          endif.
          c_t_res_docs-btrfm   = l_t_fm_amount-fm_amount.
          c_t_res_docs-budatfm = l_t_fm_amount-fm_budat.

          if l_t_fm_amount-fm_amount <> l_fi_amount.
            if l_f_bseg-storn is initial.
              c_t_res_docs-error = con_false_amount.
              append c_t_res_docs.
            else.

*-- Falls FM Betrag ungleich FI Betrag
*--  darf die FM Seite allenfalls null sein
              if l_t_fm_amount-fm_amount = 0.
                if p_eron is initial.
                  c_t_res_docs-error = con_no_error.
                  append c_t_res_docs.
                endif.
              else.
                c_t_res_docs-error = con_false_amount.
                append c_t_res_docs.
              endif.
            endif.
          else.
            if p_eron is initial.
              c_t_res_docs-error = con_no_error.
              append c_t_res_docs.
            endif.
          endif.
        endif.
      endif.
      refresh: l_t_fm_amount.
      clear  : l_fi_amount.
    endat.
  endloop.
endform.                    "fi_comp_fm_doc
*---------------------------------------------------------------------*
*       FORM WRITE_RES_PACK                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form write_res_pack tables
                      u_t_res_docs structure g_t_res_docs
                    using
                      u_f_days_rc  structure g_t_days_rc
                      u_flg_end    type c.

  statics:
    s_flg_old_read,
    s_t_days_rca  like g_t_days_rc  occurs 0 with header line,
    s_t_days_ins  like g_t_days_rc  occurs 0 with header line,
    s_t_res_ins   like g_t_res_docs occurs 0 with header line.

  if s_flg_old_read is initial.

*-- Bereits abgeglichene Buchungsdaten ermitteln
    select * from fmsd07
      into   table s_t_days_rca
      where  cpudt in s_cpudt.

    sort s_t_days_rca by fikrs cpudt.
    s_flg_old_read = con_on.
  endif.

  if u_flg_end = con_off.

*-- Daten in die Updatetabellen übertragen
    read table s_t_days_rca
      with key fikrs = u_f_days_rc-fikrs
               cpudt = u_f_days_rc-cpudt
      binary search.

    if sy-subrc <> 0.
      append u_f_days_rc  to s_t_days_ins.
    endif.

    append lines of u_t_res_docs to s_t_res_ins.

*-- Anzahl muss sich lohnen
    describe table s_t_res_ins lines sy-tfill.
    check sy-tfill > 5000.
  endif.

*-- Neue abgeglichene Datumsangaben wegschreiben
  modify fmsd07 from table s_t_days_ins.

*-- Neue Belege wegschreiben
  modify fmrc07 from table s_t_res_ins.

  commit work.

  refresh: s_t_days_ins,
           s_t_res_ins.
endform.                    "write_res_pack

*---------------------------------------------------------------------*
*       FORM DAYS_RECONCILED                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form display_days_reconciled.
  data:
    l_toggle type i value 1,
    l_flag   type c value 1.

*-- Datum von bis ausgeben
  format color col_group intensified on.
  uline /1(21).
  write: / sy-vline, text-250,
           21 sy-vline.
  uline /1(21).

  loop at l_t_days_rc.

*-- Farbintensität triggern
    if l_toggle < 0.
      format color col_normal intensified on.
    else.
      format color col_normal intensified off.
    endif.
    l_toggle = l_toggle * ( -1 ).

*-- Ausgeben
    write: / sy-vline , l_t_days_rc-cpudt,
             21 sy-vline.
    l_flag = con_on.
  endloop.

  if l_flag = con_on.
    uline /1(21).
  endif.
endform.                    "display_days_reconciled
*&---------------------------------------------------------------------*
*&      Form  TEST_ACTIV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form test_active tables
                     c_t_ledger structure range_c2.
  data:
    l_t_t001        like t001  occurs 0 with header line,
    l_f_global_data type fmfi_control_data,
    l_flg_active    type c.

  select single * from fm01
   where fikrs in s_fikrs.
  if sy-subrc <> 0.
    message w003 with s_fikrs-low.
*   Finanzkreis & nicht vorhanden
  endif.

*-- Buchungskreis holen
  call function 'BUKRS_GET_FROM_FIKRS'
    exporting
      ip_fikrs = fm01-fikrs
    tables
      t_t001   = l_t_t001.

  read table l_t_t001 index 1.

*----- Buchungskreis prüfen
  call function 'FI_COMPANY_CODE_DATA'
    exporting
      i_bukrs = l_t_t001-bukrs.

*----- Globale Daten besorgen
  call function 'FMCA_GET_INIT_INFO'
    exporting
      i_bukrs          = l_t_t001-bukrs
    changing
      c_f_control_data = l_f_global_data.

*----- Ist Zahlungsabgleich aktiv?
  if l_f_global_data-flg_paym_rc = con_off.
    message e177.
*    Zahlungsabgleich ist inaktiv
  endif.

*----- In Profil 500 nur das Verpflichtungsbudget
  call function 'FM_PAYMENT_PROFILE_CHECK'
    exporting
      i_bukrs      = l_t_t001-bukrs
    importing
      e_flg_active = l_flg_active.
  check l_flg_active = con_on.

  message i181.
*   Der Zahlungsabgleich wird nur im Verpflichtungsbudget durchgeführt

  c_t_ledger-sign   = 'I'.
  c_t_ledger-option = 'EQ'.
  c_t_ledger-low    = fmfi_con_ldnr_commitment.
  append c_t_ledger.
endform.                               " TEST_ACTIV
*&---------------------------------------------------------------------*
*&      Form  UPDATE_INVISIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_DOCS_INVISIBLE  text
*----------------------------------------------------------------------*
form update_invisible tables
                         u_t_docs_invisible structure fmrc07.

  loop at u_t_docs_invisible.
    update fmrc07   set invisible = u_t_docs_invisible-invisible
                  where bukrs     = u_t_docs_invisible-bukrs
                    and belnr     = u_t_docs_invisible-belnr
                    and gjahr     = u_t_docs_invisible-gjahr.
  endloop.

endform.                               " UPDATE_INVISIBLE
*&---------------------------------------------------------------------*
*&      Form  check_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_BKPF_ALL  text
*      -->P_G_T_RES_DOCS  text
*----------------------------------------------------------------------*
form check_cc tables   u_t_bkpf_all structure g_t_bkpf_all
                       u_t_bvor     structure g_t_bvor
                       c_t_res_docs structure g_t_res_docs.

  data:
    l_btrfi like fmrc07-btrfi,
    l_btrfm like fmrc07-btrfm,
    begin of l_t_index_check_cc occurs 0,
      min like sy-tabix,
      max like sy-tabix,
    end of l_t_index_check_cc,
    l_flg_false_date type c.

  clear c_t_res_docs.
  sort c_t_res_docs.

  loop at u_t_bvor.

*-- Sammeln aller Einträge zu einem buchungskreisübergr. Vorgang
    read table c_t_res_docs with key
                               bukrs = u_t_bvor-bukrs
                               belnr = u_t_bvor-belnr
                               gjahr = u_t_bvor-gjahr
                            binary search.
    if sy-subrc is initial.
      l_t_index_check_cc-min = sy-tabix.
      if c_t_res_docs-error = con_false_date.
        l_flg_false_date = 'X'.
      endif.
      loop at c_t_res_docs from sy-tabix.
        add:
          c_t_res_docs-btrfi to l_btrfi,
          c_t_res_docs-btrfm to l_btrfm.
        at end of gjahr.
          l_t_index_check_cc-max = sy-tabix.

*-- Merken der Indizes der betroffenen Einträge
          append l_t_index_check_cc.
          exit.
        endat.
      endloop.
    endif.

*-- Auswertung: Stimmen FI und FM Beträge jetzt überein,
*--  werden die Einträge aus der Residualliste gelöscht
*--  bzw als fehelrfrei gekennzeichnet
    at end of bvorg.
      if l_btrfi = l_btrfm and l_flg_false_date is initial.
        describe table l_t_index_check_cc lines sy-tfill.
        if not sy-tfill is initial.
          sort l_t_index_check_cc by min descending.

*-- Schleife über die Tabelle der gemerkten Indizes
          loop at l_t_index_check_cc.
            if p_eron is initial.
              loop at c_t_res_docs from l_t_index_check_cc-min
                                     to l_t_index_check_cc-max.
                c_t_res_docs-error = con_no_error.
                modify c_t_res_docs transporting error.
              endloop.
            else.
              delete c_t_res_docs from l_t_index_check_cc-min
                                    to l_t_index_check_cc-max.
            endif.
          endloop.
        endif.
      endif.
      clear: l_btrfi, l_btrfm, l_flg_false_date.
      refresh l_t_index_check_cc.
    endat.
  endloop.

endform.                    " check_cc
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_FMIFIIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_T_BKPF  text
*----------------------------------------------------------------------*
form archive_read_fmifiit  tables  p_t structure g_t_bkpf
                                   u_t_ledger   structure range_c2.
  types: begin of ty_fmifiit,
            fmbelnr       type      fm_belnr,
            fikrs   type  fikrs,
            fmbuzei   type  fm_buzei,
            btart   type  fm_btart,
            rldnr   type  rldnr,
            gjahr   type  gjahr,
            stunr   type  fm_stunr,
            vobelnr   type  fm_vobelnr,
            vobukrs   type  fm_vobukrs,
            vogjahr   type  fm_vogjahr,
            wrttp   type  fm_wrttp,
                archivekey type arkey,
                archiveofs type admi_offst.
  types: end of ty_fmifiit.

  data: l_handle    type sytabix,
        lt_fmifiit     type table of fmifiit with header line,
        l_archindex like aind_str2-archindex,
        l_gentab    like aind_str2-gentab.

  data: lt_inx_fmifiit type table of ty_fmifiit,
        ls_inx_fmifiit type ty_fmifiit.

  constants: c_ztfmifiit_001(13) value 'ZTFMIFIIT_001'.

* 1. Input the archive infostructure name
  clear l_archindex.
  l_archindex = c_ztfmifiit_001.

* 2. Get the structure table using infostructure
  clear l_gentab.
  select single gentab into l_gentab from aind_str2
   where archindex = l_archindex.

  check sy-subrc = 0 and not l_gentab is initial.

* 3. Get the archived data from structure table
  clear lt_inx_fmifiit[].
  select * into corresponding fields of table lt_inx_fmifiit
    from (l_gentab)
    for all entries in p_t
  where vobelnr =  p_t-belnr
    and vobukrs =  p_t-bukrs
    and vogjahr =  p_t-gjahr
and ( wrttp  =  wrttp9  or
      wrttp  =  wrttp9a or
      wrttp  =  wrttp9b )
    and btart  <>  fmfi_con_btart_acold
    and btart  <>  fmfi_con_btart_acnew
    and rldnr  in  u_t_ledger.


  check not lt_inx_fmifiit[] is initial.

* 4. Get more archived data looping structure table
  clear: gt_fmifiit_a, gt_fmifiit_a[].
  loop at lt_inx_fmifiit into ls_inx_fmifiit.
*  4.1 Read information from archivekey & offset
    clear l_handle.
    call function 'ARCHIVE_READ_OBJECT'
      exporting
        object                    = 'FM_DOC_FI'
        archivkey                 = ls_inx_fmifiit-archivekey
        offset                    = ls_inx_fmifiit-archiveofs
      importing
        archive_handle            = l_handle
      exceptions
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        others                    = 13.

    check sy-subrc = 0.

*  4.2 Read table from information
    clear: lt_fmifiit, lt_fmifiit[].
    call function 'ARCHIVE_GET_TABLE'
      exporting
        archive_handle          = l_handle
        record_structure        = 'FMIFIIT'
        all_records_of_object   = 'X'
      tables
        table                   = lt_fmifiit
      exceptions
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        others                  = 4.

    check sy-subrc = 0 and not lt_fmifiit[] is initial.

* 5. Append archived data table to finally interal table
    insert lines of lt_fmifiit into table gt_fmifiit_a.
  endloop.

  check not gt_fmifiit_a[] is initial.

  sort gt_fmifiit_a.
  delete adjacent duplicates from gt_fmifiit_a comparing all fields.

  loop at gt_fmifiit_a.
    clear g_t_fmifiit.
    g_t_fmifiit-btart   = gt_fmifiit_a-btart.
    g_t_fmifiit-fikrs   = gt_fmifiit_a-fikrs.
    g_t_fmifiit-fkbtr   = gt_fmifiit_a-fkbtr.
    g_t_fmifiit-stunr   = gt_fmifiit_a-stunr.
    g_t_fmifiit-fipos   = gt_fmifiit_a-fipex.
    g_t_fmifiit-bukrs   = gt_fmifiit_a-vobukrs.
    g_t_fmifiit-belnr   = gt_fmifiit_a-vobelnr.
    g_t_fmifiit-gjahr   = gt_fmifiit_a-vogjahr.
*    g_t_fmifiit-buzei   = gt_fmifiit_a-vobuzei.
    g_t_fmifiit-fmbelnr = gt_fmifiit_a-fmbelnr.
*    g_t_fmifiit-budatr  = gt_fmifiit_a-zhldt.
    append g_t_fmifiit.  clear g_t_fmifiit.
  endloop.

endform.                    " ARCHIVE_READ_FMIFIIT
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_T001  text
*----------------------------------------------------------------------*
form archive_read_bkpf  tables   p_g_t_t001 structure t001.

  types: begin of ty_bkpf,
            fmbelnr       type      fm_belnr,
            fikrs   type  fikrs,
            fmbuzei   type  fm_buzei,
            btart   type  fm_btart,
            rldnr   type  rldnr,
            gjahr   type  gjahr,
            stunr   type  fm_stunr,
            vobelnr   type  fm_vobelnr,
            vobukrs   type  fm_vobukrs,
            vogjahr   type  fm_vogjahr,
            wrttp   type  fm_wrttp,
                archivekey type arkey,
                archiveofs type admi_offst.
  types: end of ty_bkpf.

  data: l_handle    type sytabix,
        lt_bkpf     type table of bkpf with header line,
        l_archindex like aind_str2-archindex,
        l_gentab    like aind_str2-gentab.

  data: lt_inx_bkpf type table of ty_bkpf,
        ls_inx_bkpf type ty_bkpf.

  constants: c_ztbkpf_001(9) value 'ZBKPF_001'.

* 1. Input the archive infostructure name
  clear l_archindex.
  l_archindex = c_ztbkpf_001.

* 2. Get the structure table using infostructure
  clear l_gentab.
  select single gentab into l_gentab from aind_str2
   where archindex = l_archindex.

  check sy-subrc = 0 and not l_gentab is initial.

* 3. Get the archived data from structure table
  clear lt_inx_bkpf[].
  select * into corresponding fields of table lt_inx_bkpf
    from (l_gentab)
    for all entries in p_g_t_t001
   where bukrs = p_g_t_t001-bukrs
     and belnr in s_belnr
     and cpudt in s_cpudt
     and budat in s_budat
     and bstat =  ' '.

  check not lt_inx_bkpf[] is initial.

* 4. Get more archived data looping structure table
  clear: gt_bkpf_a, gt_bkpf_a[].
  loop at lt_inx_bkpf into ls_inx_bkpf.
*  4.1 Read information from archivekey & offset
    clear l_handle.
    call function 'ARCHIVE_READ_OBJECT'
      exporting
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bkpf-archivekey
        offset                    = ls_inx_bkpf-archiveofs
      importing
        archive_handle            = l_handle
      exceptions
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        others                    = 13.

    check sy-subrc = 0.

*  4.2 Read table from information
    clear: lt_bkpf, lt_bkpf[].
    call function 'ARCHIVE_GET_TABLE'
      exporting
        archive_handle          = l_handle
        record_structure        = 'BKPF'
        all_records_of_object   = 'X'
      tables
        table                   = lt_bkpf
      exceptions
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        others                  = 4.

    delete lt_bkpf where not ( cpudt in s_cpudt and bstat =  ' ' ).

    check sy-subrc = 0 and not lt_bkpf[] is initial.

* 5. Append archived data table to finally interal table
    insert lines of lt_bkpf into table gt_bkpf_a.
  endloop.

  check not gt_bkpf_a[] is initial.

  sort gt_bkpf_a.
  delete adjacent duplicates from gt_bkpf_a comparing all fields.

  loop at gt_bkpf_a.
    clear g_t_bkpf_all.
    move-corresponding gt_bkpf_a to g_t_bkpf_all.
    append g_t_bkpf_all.
  endloop.

endform.                    " ARCHIVE_READ_BKPF
