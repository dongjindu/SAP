REPORT rffmrc07 MESSAGE-ID fi
                LINE-SIZE 90
                NO STANDARD PAGE HEADING.

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
INCLUDE:
  ififmcon_value_types,
  ififmcon_bool.

*-Typepools------------------------------------------------------------*
TYPE-POOLS:
  fmfi.

*-- Konstanten
CONSTANTS:
  con_no_error       TYPE c VALUE 'A',
  con_fm_doc_missing TYPE c VALUE 'B',
  con_false_amount   TYPE c VALUE 'C',
  con_false_date     TYPE c VALUE 'D'.

*-- Tabellendeklaration
TABLES:
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
DATA:
  BEGIN OF g_t_bkpf OCCURS 0,
    bvorg LIKE bkpf-bvorg,
    bukrs LIKE bkpf-bukrs,
    belnr LIKE bkpf-belnr,
    gjahr LIKE bkpf-gjahr,
    budat LIKE bkpf-budat,
    cpudt LIKE bkpf-cpudt,
    stblg LIKE bkpf-stblg,
  END   OF g_t_bkpf,

  BEGIN OF g_t_fmifiit OCCURS 0,
    belnr   LIKE fmifiit-vobelnr,
    bukrs   LIKE fmifiit-bukrs,
    gjahr   LIKE fmifiit-vogjahr,
    fkbtr   LIKE fmifiit-fkbtr,
    shkzg   LIKE bseg-shkzg,
    fmbelnr LIKE fmifiit-fmbelnr,
    fikrs   LIKE fmifiit-fikrs,
    btart   LIKE fmifiit-btart,
    stunr   LIKE fmifiit-stunr,
    fipos   LIKE bseg-fipos,
    budat   LIKE bkpf-budat,
    cpudt   LIKE bkpf-budat,
    storn   TYPE c,
    rerror  TYPE c,
  END OF g_t_fmifiit,

  g_t_res_outp        LIKE fmrc07      OCCURS 0 WITH HEADER LINE,
  g_t_dis_docs        LIKE fmrc07      OCCURS 0 WITH HEADER LINE,
  g_t_res_docs        LIKE fmrc07      OCCURS 0 WITH HEADER LINE,
  g_t_docs_visible    LIKE fmrc07      OCCURS 0 WITH HEADER LINE,
  g_t_docs_invisible  LIKE fmrc07      OCCURS 0 WITH HEADER LINE,
  g_t_days_rc         LIKE fmsd07      OCCURS 0 WITH HEADER LINE,
  l_t_days_rc         LIKE fmsd07      OCCURS 0 WITH HEADER LINE,
  g_t_days_rca        LIKE fmsd07      OCCURS 0 WITH HEADER LINE,
  g_t_bkpf_all        LIKE g_t_bkpf    OCCURS 0 WITH HEADER LINE,
  g_t_bseg            LIKE g_t_fmifiit OCCURS 0 WITH HEADER LINE,
  g_t_result          LIKE g_t_fmifiit OCCURS 0 WITH HEADER LINE,
  g_t_t001            LIKE t001        OCCURS 0 WITH HEADER LINE,
  g_t_fm01            LIKE fm01        OCCURS 0 WITH HEADER LINE,
  g_t_bvor            LIKE bvor        OCCURS 0 WITH HEADER LINE,

* by ig.moon 6/24/2013 {
  g_t_outlist    TYPE STANDARD TABLE OF fmrc07_output
                          WITH NON-UNIQUE KEY bukrs belnr gjahr,
* }


  g_end_of_sel TYPE c,
  g_field(20)  TYPE c,
  l_idx        TYPE i.

DATA gt_fmifiit_a TYPE TABLE OF fmifiit WITH HEADER LINE.
DATA gt_bkpf_a TYPE TABLE OF bkpf WITH HEADER LINE.

RANGES:
  g_t_ledger   FOR  fmifiit-rldnr.

************************************************************************
* SELECTION
************************************************************************

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK date WITH FRAME TITLE text-020.
SELECT-OPTIONS:
  s_fikrs FOR fmifiit-fikrs MEMORY ID fik
                            MATCHCODE OBJECT fikr OBLIGATORY.

*-- Selektion über Datum
SELECT-OPTIONS:
  s_belnr FOR fmifiit-vobelnr,              " NO-DISPLAY,
  s_cpudt FOR bkpf-cpudt,
  s_budat FOR bkpf-budat.

PARAMETERS:
  p_today AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK date.

SELECTION-SCREEN SKIP.

*-- Steuerung
SELECTION-SCREEN BEGIN OF BLOCK control WITH FRAME TITLE text-010.
PARAMETERS:
  p_test AS CHECKBOX DEFAULT 'X',
  p_prot AS CHECKBOX DEFAULT 'X',
  p_eron AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK control.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  IF NOT p_test IS INITIAL AND p_prot IS INITIAL.
    MESSAGE e178.
*    Parameterauswahl ist nicht sinnvoll
  ENDIF.

************************************************************************
* AT START-OF-SELECTION.
************************************************************************
START-OF-SELECTION.

* Andy Modification
*  PERFORM test_active TABLES
*                          g_t_ledger.

*-- Alle Finanzkreise holen
  SELECT * FROM fm01
   INTO  TABLE g_t_fm01
  WHERE  fikrs IN s_fikrs.

*-- Selektionsbereich setzen
  PERFORM set_selection_date TABLES
                               s_budat
                               s_cpudt
                             USING
                               p_today.

*-- Über Finanzkreise loopen
  LOOP AT g_t_fm01.

*-- Buchungskreise zum Fikrs holen
    CALL FUNCTION 'BUKRS_GET_FROM_FIKRS'
      EXPORTING
        ip_fikrs = g_t_fm01-fikrs
      TABLES
        t_t001   = g_t_t001.

*-- Köpfe der FI Belege holen
    PERFORM fi_get_header TABLES
                            g_t_bkpf_all
                            g_t_days_rc
                            g_t_bvor.

    DESCRIBE TABLE g_t_bkpf_all LINES sy-tfill.
    CHECK sy-tfill > 0.

    l_t_days_rc[] = g_t_days_rc[].

*-- Die einzelnen Tage abarbeiten
    LOOP AT g_t_days_rc.

*-- Päckchen bilden
      PERFORM get_bkpf_pack TABLES
                              g_t_bkpf_all
                              g_t_bkpf
                              l_t_days_rc.

*-- Summen über alle Konten selektieren(G_T_GLT0 füllen mit RACCT = ' ')
      PERFORM fi_get_line_items TABLES
                                    g_t_bseg
                                    g_t_bkpf.

*-- Einzelposten einlesen FM
      PERFORM fm_get_line_items TABLES
                                  g_t_bkpf
                                  g_t_fmifiit
                                  g_t_ledger.

*-- Vergleichen
      PERFORM fi_comp_fm_doc TABLES
                                 g_t_bseg
                                 g_t_fmifiit
                                 g_t_res_docs.

*-- Fehler durch Bukrs-übergreifende Belege aussortieren
      PERFORM check_cc TABLES
                          g_t_bkpf_all
                          g_t_bvor
                          g_t_res_docs.

*-- Abgeglichener Tag wegschreiben
      IF p_test IS INITIAL.
        PERFORM write_res_pack TABLES
                                   g_t_res_docs
                                 USING
                                   g_t_days_rc
                                   con_off.
      ENDIF.

*-- Belege fürs Reporting merken
      IF NOT p_prot IS INITIAL.
        APPEND LINES OF g_t_res_docs TO g_t_res_outp.
      ENDIF.
      REFRESH g_t_res_docs.

*-- Housekeeping pro Tag
      REFRESH: g_t_res_docs,
               g_t_fmifiit,
               g_t_bseg,
               g_t_bkpf.

    ENDLOOP.

*-- Housekeeping pro Finanzkreis
    REFRESH: g_t_bkpf_all,
             g_t_days_rc,
             g_t_t001,
             g_t_bvor.

    SORT g_t_res_docs.
    DELETE ADJACENT DUPLICATES FROM g_t_res_docs.

*   Rest wegschreiben
    IF p_test IS INITIAL.
      PERFORM write_res_pack TABLES
                               g_t_res_docs
                             USING
                               g_t_days_rc
                               con_on.
    ENDIF.

  ENDLOOP.

************************************************************************
* END-OF-SELECTION.
************************************************************************
END-OF-SELECTION.

*Andy modification
*  SET PF-STATUS 'LIST'.

*----- Ausgabetabelle retten
  APPEND LINES OF g_t_res_outp TO g_t_dis_docs.

*-- Ergebnis ausgeben
  IF p_prot IS INITIAL.
    FORMAT COLOR COL_NORMAL.
    WRITE / text-240.
  ELSE.
    PERFORM display_days_reconciled.

* by ig.moon 6/24/2013 {
*    PERFORM display_line_items(rffmsd07)
*                                      TABLES
*                                        g_t_dis_docs.

    PERFORM transfer_line_items(rffmsd07) TABLES g_t_dis_docs
                                                 g_t_outlist.

    PERFORM display_list(rffmsd07) USING g_t_outlist
                                         text-400.
* }

  ENDIF.

************************************************************************
* AT USER-COMMAND
************************************************************************
AT USER-COMMAND.
  CASE sy-ucomm.

*----- Alle Belege sichtbar machen
    WHEN 'ALSI'.
      REFRESH g_t_dis_docs.
      APPEND LINES OF g_t_res_outp TO g_t_dis_docs.
      PERFORM display_days_reconciled.
      LOOP AT g_t_dis_docs.
        g_t_dis_docs-invisible = con_off.
        MODIFY g_t_dis_docs TRANSPORTING invisible.
      ENDLOOP.
      PERFORM display_line_items(rffmsd07) TABLES
                                              g_t_dis_docs.

*----- Ausgewählte Belege unsichtbar machen
    WHEN 'UNSI'.
      l_idx = 0.
      DO.

*----- Buchungskreis ermitteln
        l_idx = l_idx + 1.
        CLEAR g_t_dis_docs-bukrs.
        READ LINE l_idx FIELD VALUE g_t_dis_docs-bukrs.
        IF sy-subrc <> 0. EXIT. ENDIF.
        CHECK NOT g_t_dis_docs-bukrs IS INITIAL.

*----- Geschäftsjahr ermitteln
        l_idx = l_idx + 1.
        CLEAR g_t_dis_docs-gjahr.
        READ LINE l_idx FIELD VALUE g_t_dis_docs-gjahr.
        CHECK NOT g_t_dis_docs-gjahr IS INITIAL.

*----- Ausgewählte Belege ermitteln
        DO.
          l_idx = l_idx + 1.
          CLEAR g_t_dis_docs-invisible.
          CLEAR g_t_dis_docs-belnr.
          READ LINE l_idx FIELD VALUE g_t_dis_docs-invisible
                                      g_t_dis_docs-belnr.

          SEARCH sy-lisel FOR text-120.
          IF sy-subrc = 0. EXIT. ENDIF.

          IF g_t_dis_docs-invisible = con_off.
            LOOP AT g_t_res_outp WHERE bukrs = g_t_dis_docs-bukrs
                                   AND belnr = g_t_dis_docs-belnr
                                   AND gjahr = g_t_dis_docs-gjahr.
              MOVE-CORRESPONDING g_t_res_outp TO g_t_docs_visible.
              g_t_docs_visible-invisible = g_t_dis_docs-invisible.
              APPEND g_t_docs_visible.
            ENDLOOP.
          ELSEIF g_t_dis_docs-invisible = con_on.
            LOOP AT g_t_res_outp WHERE bukrs = g_t_dis_docs-bukrs
                                   AND belnr = g_t_dis_docs-belnr
                                   AND gjahr = g_t_dis_docs-gjahr.
              MOVE-CORRESPONDING g_t_res_outp TO g_t_docs_invisible.
              g_t_docs_invisible-invisible = g_t_dis_docs-invisible.
              APPEND g_t_docs_invisible.
            ENDLOOP.
          ENDIF.
        ENDDO.
      ENDDO.

*----- Unsichtbare Belege modifizieren
      PERFORM update_invisible TABLES
                                 g_t_docs_invisible.

*----- Nur sichbare Belege ausgeben
      REFRESH g_t_dis_docs.
      APPEND LINES OF g_t_docs_visible TO g_t_dis_docs.
      PERFORM display_days_reconciled.
      PERFORM display_line_items(rffmsd07) TABLES
                                              g_t_dis_docs.
      REFRESH g_t_docs_visible.
      REFRESH g_t_docs_invisible.
      CLEAR   g_t_docs_visible.
      CLEAR   g_t_docs_invisible.

*----- Alle Belege markieren
    WHEN 'ACTIV'.
      l_idx = 0.
      DO.
        l_idx = sy-index + 1.
        READ LINE l_idx FIELD VALUE g_t_dis_docs-invisible.
        IF sy-subrc = 0.
          MODIFY CURRENT LINE FIELD VALUE g_t_dis_docs-invisible
                                                            FROM con_on.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

*----- Alle Markierungen löschen
    WHEN 'INACTIV'.
      l_idx = 0.
      DO.
        l_idx = sy-index + 1.
        READ LINE l_idx FIELD VALUE g_t_dis_docs-invisible.
        IF sy-subrc = 0.
          MODIFY CURRENT LINE FIELD VALUE g_t_dis_docs-invisible
                                                           FROM con_off.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
  ENDCASE.
  sy-lsind = 0.

************************************************************************
* AT LINE-SELECTION
************************************************************************
AT LINE-SELECTION.

*-- Ausgewählter Belege anzeigen
  GET CURSOR FIELD g_field.

  IF g_field =  'G_T_DIS_DOCS-BTRFM'.
    READ TABLE g_t_dis_docs
      WITH KEY bukrs = g_t_dis_docs-bukrs
               belnr = g_t_dis_docs-belnr
               gjahr = g_t_dis_docs-gjahr.
    CHECK sy-subrc = 0.

    IF NOT ( g_t_dis_docs IS INITIAL ).
      PERFORM show_fm_document(rffmsd07)
                                      USING
                                        g_t_dis_docs.
    ELSE.
      MESSAGE i605.
    ENDIF.
  ELSE.
    SET PARAMETER ID 'BUK' FIELD g_t_dis_docs-bukrs.
    SET PARAMETER ID 'BLN' FIELD g_t_dis_docs-belnr.
    SET PARAMETER ID 'GJR' FIELD g_t_dis_docs-gjahr.

    IF NOT ( g_t_dis_docs IS INITIAL ).
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ELSE.
      MESSAGE i605.
    ENDIF.
  ENDIF.
  CLEAR g_t_dis_docs.

************************************************************************
* Unterprorgamme
************************************************************************

*---------------------------------------------------------------------*
*       FORM FI_GET_LINE_ITEMS                                        *
*---------------------------------------------------------------------*
*       FI EP selektieren und ans FM angleichen                       *
*---------------------------------------------------------------------*
FORM fi_get_line_items TABLES
                         c_t_bseg     STRUCTURE g_t_bseg
                         u_t_bkpf     STRUCTURE g_t_bkpf.

  DATA:
    l_t_bkpf LIKE g_t_bkpf OCCURS 0 WITH HEADER LINE,
    l_t_bseg LIKE g_t_bseg OCCURS 0 WITH HEADER LINE.

  APPEND LINES OF u_t_bkpf    TO l_t_bkpf.

  DESCRIBE TABLE l_t_bkpf LINES sy-tfill.
  CHECK sy-tfill > 0.

  SORT l_t_bkpf BY belnr bukrs gjahr.

*-- Alle relevanten Einzelposten einlesen
  SELECT dmbtr AS fkbtr
         belnr gjahr
         bukrs fipos
         shkzg buzei    FROM bseg
  APPENDING CORRESPONDING FIELDS OF TABLE l_t_bseg
  FOR ALL ENTRIES IN l_t_bkpf
  WHERE belnr = l_t_bkpf-belnr
    AND bukrs = l_t_bkpf-bukrs
    AND gjahr = l_t_bkpf-gjahr
    AND ktosl <> 'BUV'
    AND xref1 <> fmfi_con_euro_fi.

  DESCRIBE TABLE l_t_bseg LINES sy-tfill.
  CHECK sy-tfill > 0.

  SORT l_t_bseg BY belnr bukrs gjahr.

*-- FI Belege noch aufbereiten und evtl. löschen
  PERFORM clean_fi_documents TABLES
                               l_t_bseg
                               c_t_bseg
                               l_t_bkpf.

  SORT c_t_bseg    BY belnr bukrs gjahr.
ENDFORM.                    "fi_get_line_items

*---------------------------------------------------------------------*
*       FORM FM_GET_LINE_ITEMS                                        *
*---------------------------------------------------------------------*
*       FM Einzelposten selektieren                                   *
*---------------------------------------------------------------------*
FORM fm_get_line_items TABLES
                           u_t_bkpf     STRUCTURE g_t_bkpf
                           c_t_fmifiit  STRUCTURE g_t_fmifiit
                           u_t_ledger   STRUCTURE range_c2.

  DATA:
    l_t_bkpf LIKE g_t_bkpf OCCURS 0 WITH HEADER LINE.

  APPEND LINES OF u_t_bkpf    TO l_t_bkpf.

  DESCRIBE TABLE l_t_bkpf LINES sy-tfill.
  CHECK sy-tfill > 0.

  SELECT btart   fikrs
         fkbtr   stunr
         fipex   AS fipos
         vobukrs AS bukrs
         vobelnr AS belnr
         vogjahr AS gjahr
         vobuzei AS buzei
         fmbelnr AS fmbelnr
         zhldt   AS budat FROM fmifiit
   APPENDING CORRESPONDING FIELDS OF TABLE g_t_fmifiit
    FOR ALL ENTRIES IN l_t_bkpf
  WHERE vobelnr =  l_t_bkpf-belnr
    AND vobukrs =  l_t_bkpf-bukrs
    AND vogjahr =  l_t_bkpf-gjahr
AND ( wrttp  =  wrttp9  OR
      wrttp  =  wrttp9a OR
      wrttp  =  wrttp9b )
    AND btart  <>  fmfi_con_btart_acold
    AND btart  <>  fmfi_con_btart_acnew
    AND rldnr  IN  u_t_ledger.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_fmifiit TABLES l_t_bkpf u_t_ledger.
  ENDIF.
*- U1 End

ENDFORM.                    "fm_get_line_items

*---------------------------------------------------------------------*
*       FORM SET_SELECTION_DATE                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_selection_date TABLES
                          c_budat STRUCTURE s_cpudt
                          c_cpudt STRUCTURE s_cpudt
                        USING
                          u_today.

*-- Buchungsdatum überschreibt CPU-Datum
  DESCRIBE TABLE c_budat LINES sy-tfill.

  IF sy-tfill > 0.
    REFRESH c_cpudt.
  ELSE.

*-- Wurde kein Datum zur Selektion angegeben, den heutigen
*-- Tag als Selektionsdatum nehmen.
    CHECK NOT u_today IS INITIAL.

    REFRESH c_cpudt.
    s_cpudt-sign   = 'I'.
    s_cpudt-option = 'EQ'.
    s_cpudt-low    = sy-datum.
    APPEND s_cpudt.

    READ TABLE s_cpudt  INDEX 1.

*-- Geschäftsjahr für READ_QUICK Bausteine
*   call function 'FM_DATE_TO_PERIODE_CONVERT'
*        exporting
*             i_date  = s_cpudt-low
*             i_fikrs = g_t_fm01-fikrs
*        importing
*             e_gjahr = g_gjahr.
  ENDIF.
ENDFORM.                    "set_selection_date

*---------------------------------------------------------------------*
*       FORM GET_BKPF_PACK                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  U_T_BKPF_IN                                                   *
*  -->  U_T_BKPF_OUT                                                  *
*---------------------------------------------------------------------*
FORM get_bkpf_pack TABLES
                     c_t_bkpf_all STRUCTURE g_t_bkpf
                     c_t_bkpf     STRUCTURE g_t_bkpf
                     u_t_days_rc  STRUCTURE g_t_days_rc.

  STATICS:
    s_cpudt          LIKE sy-datum,
    s_index_next_day LIKE sy-tabix VALUE 2,
    s_index_from     LIKE sy-tabix VALUE 1.

  DATA:
    l_index_to       LIKE sy-tabix,
    l_flg_end        TYPE c.

  IF s_cpudt IS INITIAL.
    READ TABLE u_t_days_rc INDEX 1.
    s_cpudt = u_t_days_rc-cpudt.
  ENDIF.

*-- Index auf nächstes Erfassungsdatum bestimmen
  LOOP AT u_t_days_rc FROM s_index_next_day WHERE cpudt <> s_cpudt.
    s_cpudt          = u_t_days_rc-cpudt.
    s_index_next_day = sy-tabix.
    EXIT.
  ENDLOOP.
  IF sy-subrc <> 0.
    DESCRIBE TABLE c_t_bkpf_all LINES l_index_to.
    l_flg_end = con_on.
  ELSE.
    READ TABLE c_t_bkpf_all WITH KEY cpudt = s_cpudt
      BINARY SEARCH.
    l_index_to = sy-tabix - 1.
  ENDIF.

*-- Zeilen übertragen
  APPEND LINES OF c_t_bkpf_all FROM s_index_from TO l_index_to
               TO c_t_bkpf.

  IF l_flg_end = con_on.
    s_index_from     = 1.
    s_index_next_day = 2.
  ELSE.
    s_index_from     = l_index_to       + 1.
    s_index_next_day = s_index_next_day + 1.
  ENDIF.

  SORT c_t_bkpf     BY       belnr bukrs gjahr.
ENDFORM.                    "get_bkpf_pack
*---------------------------------------------------------------------*
*       FORM FI_GET_HEADER                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fi_get_header TABLES
                     c_t_bkpf     STRUCTURE g_t_bkpf
                     c_t_days_rc  STRUCTURE g_t_days_rc
                     c_t_bvor     STRUCTURE g_t_bvor.

  DATA:
    l_t_docs_rc LIKE fmrc07   OCCURS 0 WITH HEADER LINE,
    l_t_bkpf_cc LIKE g_t_bkpf OCCURS 0 WITH HEADER LINE.

  DESCRIBE TABLE g_t_t001 LINES sy-tfill.
  CHECK sy-tfill > 0.

*-- Einzelpostenköpfe selektieren
  SELECT bvorg bukrs belnr gjahr budat cpudt FROM bkpf
    INTO CORRESPONDING FIELDS OF TABLE c_t_bkpf
     FOR ALL ENTRIES IN g_t_t001
   WHERE bukrs = g_t_t001-bukrs
     AND belnr IN s_belnr
     AND cpudt IN s_cpudt
     AND budat IN s_budat
     AND bstat =  ' '.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bkpf TABLES g_t_t001.
  ENDIF.
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

  SORT c_t_bkpf BY cpudt belnr bukrs gjahr.

*-- Abzugleichene Buchungsdaten ermitteln
  LOOP AT c_t_bkpf.
    READ TABLE g_t_t001
      WITH KEY bukrs = c_t_bkpf-bukrs.
    IF NOT sy-subrc IS INITIAL.

*-- Aus neuem Buchungskreis kann neuer FIKRS entstehen
      CLEAR g_t_t001.
      g_t_t001-bukrs = c_t_bkpf-bukrs.
      CALL FUNCTION 'FMFK_GET_FIKRS_FROM_BUKRS'
        EXPORTING
          i_bukrs = g_t_t001-bukrs
        IMPORTING
          e_fikrs = g_t_t001-fikrs.
      APPEND g_t_t001.
    ENDIF.
    c_t_days_rc-cpudt = c_t_bkpf-cpudt.
    c_t_days_rc-fikrs = g_t_t001-fikrs.
    COLLECT c_t_days_rc.
  ENDLOOP.

  SORT c_t_days_rc  BY fikrs cpudt.
ENDFORM.                    "fi_get_header

*---------------------------------------------------------------------*
*       FORM CLEAN_FI_DOCUMENTS                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  U_T_BSEG                                                      *
*---------------------------------------------------------------------*
FORM clean_fi_documents TABLES
                          u_t_bseg     STRUCTURE g_t_bseg
                          c_t_bseg     STRUCTURE g_t_bseg
                          u_t_bkpf     STRUCTURE g_t_bkpf.

  DATA:
    l_f_fmfpo LIKE fmfpo.

  CLEAR: u_t_bkpf-budat.

  LOOP AT u_t_bseg WHERE NOT fipos IS INITIAL.

*----- Aus neuem Buchungskreis kann neuer FIKRS entstehen
    READ TABLE g_t_t001
      WITH KEY bukrs = u_t_bseg-bukrs.

    IF sy-subrc <> 0.
      CALL FUNCTION 'FMFK_GET_FIKRS_FROM_BUKRS'
        EXPORTING
          i_bukrs = u_t_bseg-bukrs
        IMPORTING
          e_fikrs = g_t_t001-fikrs.
      g_t_t001-bukrs = u_t_bseg-bukrs.
      APPEND g_t_t001.
    ENDIF.

    CALL FUNCTION 'FMFPO_READ_QUICK'
      EXPORTING
        ip_fikrs          = g_t_t001-fikrs
        ip_fipos          = u_t_bseg-fipos
        ip_gjahr          = u_t_bseg-gjahr
        ip_flg_buffer_all = con_on
      IMPORTING
        f_fmfpo           = l_f_fmfpo.

    IF l_f_fmfpo-fivor = '90' OR l_f_fmfpo-fivor = '80'.
      READ TABLE u_t_bkpf
        WITH KEY belnr = u_t_bseg-belnr
                 bukrs = u_t_bseg-bukrs
                 gjahr = u_t_bseg-gjahr
        BINARY SEARCH.

      c_t_bseg       = u_t_bseg.
      IF c_t_bseg-shkzg = 'H'.
        MULTIPLY c_t_bseg-fkbtr BY -1.
      ENDIF.
      IF NOT u_t_bkpf-stblg IS INITIAL.
        c_t_bseg-storn = con_on.
      ENDIF.
      c_t_bseg-budat = u_t_bkpf-budat.
      c_t_bseg-cpudt = u_t_bkpf-cpudt.
      APPEND c_t_bseg.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "clean_fi_documents

*---------------------------------------------------------------------*
*       FORM FI_COMP_FM_DOC                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fi_comp_fm_doc TABLES
                      u_t_bseg     STRUCTURE g_t_bseg
                      u_t_fmifiit  STRUCTURE g_t_fmifiit
                      c_t_res_docs STRUCTURE g_t_res_docs.

  DATA:
    BEGIN OF l_t_fm_amount OCCURS 0,
      fm_amount LIKE fmifiit-fkbtr,
      fm_budat  LIKE fmifiit-zhldt,
    END   OF l_t_fm_amount,

    l_len_fm_amount LIKE sy-tfill,
    l_fi_amount     LIKE fmifiit-fkbtr,
    l_budatfi       LIKE g_t_res_docs-budatfi,
    l_f_bseg        LIKE g_t_fmifiit.

  SORT u_t_fmifiit BY belnr bukrs gjahr.

  LOOP AT u_t_bseg.

    ADD u_t_bseg-fkbtr TO l_fi_amount.
    l_f_bseg = u_t_bseg.

    AT END OF gjahr.
      CHECK l_fi_amount <> 0.

*-- Fehlerdatei aufbauen
      CLEAR c_t_res_docs.
      c_t_res_docs-bukrs   = l_f_bseg-bukrs.
      c_t_res_docs-gjahr   = l_f_bseg-gjahr.
      c_t_res_docs-belnr   = l_f_bseg-belnr.
      c_t_res_docs-cpudt   = l_f_bseg-cpudt.
      c_t_res_docs-budatfi = l_f_bseg-budat.
      c_t_res_docs-btrfi   = l_fi_amount.

      READ TABLE g_t_t001
        WITH KEY bukrs = l_f_bseg-bukrs.
      c_t_res_docs-fikrs = g_t_t001-fikrs.

      READ TABLE u_t_fmifiit
        WITH KEY belnr = l_f_bseg-belnr
                 bukrs = l_f_bseg-bukrs
                 gjahr = l_f_bseg-gjahr
        BINARY SEARCH.

*-- Fehlerauswertung
      IF sy-subrc <> 0.
        IF l_f_bseg-storn IS INITIAL.
          c_t_res_docs-error = con_fm_doc_missing.
          APPEND c_t_res_docs.
        ELSEIF p_eron IS INITIAL.

*-- Falls storniert wurde, sind FI-Beträge sowieso null
*--  und es ist ok, wenn kein FM-Beleg existiert
          c_t_res_docs-error = con_no_error.
          APPEND c_t_res_docs.
        ENDIF.
      ELSE.

        LOOP AT u_t_fmifiit FROM sy-tabix.

*-- Summe der FM Belegzeilen berechnen
          l_t_fm_amount-fm_amount = u_t_fmifiit-fkbtr.
          l_t_fm_amount-fm_budat  = u_t_fmifiit-budat.
          COLLECT l_t_fm_amount.

*-- Belegende
          AT END OF gjahr.
            EXIT.
          ENDAT.
        ENDLOOP.

*-- Nullzeilen interessieren nicht
        DELETE l_t_fm_amount WHERE fm_amount =  0.
        DESCRIBE TABLE l_t_fm_amount LINES l_len_fm_amount.

        IF l_len_fm_amount > 1.

*-- Fortschreibedatum ( Jeder FI Beleg hat nur ein Buchungsdatum )
          LOOP AT l_t_fm_amount.
            c_t_res_docs-budatfm = l_t_fm_amount-fm_budat.
            c_t_res_docs-btrfm   = l_t_fm_amount-fm_amount.
            c_t_res_docs-error   = con_false_date.
            APPEND c_t_res_docs.
          ENDLOOP.
        ELSE.
          READ TABLE l_t_fm_amount INDEX 1.
          IF sy-subrc <> 0.
            l_t_fm_amount-fm_amount = 0.
          ENDIF.
          c_t_res_docs-btrfm   = l_t_fm_amount-fm_amount.
          c_t_res_docs-budatfm = l_t_fm_amount-fm_budat.

          IF l_t_fm_amount-fm_amount <> l_fi_amount.
            IF l_f_bseg-storn IS INITIAL.
              c_t_res_docs-error = con_false_amount.
              APPEND c_t_res_docs.
            ELSE.

*-- Falls FM Betrag ungleich FI Betrag
*--  darf die FM Seite allenfalls null sein
              IF l_t_fm_amount-fm_amount = 0.
                IF p_eron IS INITIAL.
                  c_t_res_docs-error = con_no_error.
                  APPEND c_t_res_docs.
                ENDIF.
              ELSE.
                c_t_res_docs-error = con_false_amount.
                APPEND c_t_res_docs.
              ENDIF.
            ENDIF.
          ELSE.
            IF p_eron IS INITIAL.
              c_t_res_docs-error = con_no_error.
              APPEND c_t_res_docs.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      REFRESH: l_t_fm_amount.
      CLEAR  : l_fi_amount.
    ENDAT.
  ENDLOOP.
ENDFORM.                    "fi_comp_fm_doc
*---------------------------------------------------------------------*
*       FORM WRITE_RES_PACK                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_res_pack TABLES
                      u_t_res_docs STRUCTURE g_t_res_docs
                    USING
                      u_f_days_rc  STRUCTURE g_t_days_rc
                      u_flg_end    TYPE c.

  STATICS:
    s_flg_old_read,
    s_t_days_rca  LIKE g_t_days_rc  OCCURS 0 WITH HEADER LINE,
    s_t_days_ins  LIKE g_t_days_rc  OCCURS 0 WITH HEADER LINE,
    s_t_res_ins   LIKE g_t_res_docs OCCURS 0 WITH HEADER LINE.

  IF s_flg_old_read IS INITIAL.

*-- Bereits abgeglichene Buchungsdaten ermitteln
    SELECT * FROM fmsd07
      INTO   TABLE s_t_days_rca
      WHERE  cpudt IN s_cpudt.

    SORT s_t_days_rca BY fikrs cpudt.
    s_flg_old_read = con_on.
  ENDIF.

  IF u_flg_end = con_off.

*-- Daten in die Updatetabellen übertragen
    READ TABLE s_t_days_rca
      WITH KEY fikrs = u_f_days_rc-fikrs
               cpudt = u_f_days_rc-cpudt
      BINARY SEARCH.

    IF sy-subrc <> 0.
      APPEND u_f_days_rc  TO s_t_days_ins.
    ENDIF.

    APPEND LINES OF u_t_res_docs TO s_t_res_ins.

*-- Anzahl muss sich lohnen
    DESCRIBE TABLE s_t_res_ins LINES sy-tfill.
    CHECK sy-tfill > 5000.
  ENDIF.

*-- Neue abgeglichene Datumsangaben wegschreiben
  MODIFY fmsd07 FROM TABLE s_t_days_ins.

*-- Neue Belege wegschreiben
  MODIFY fmrc07 FROM TABLE s_t_res_ins.

  COMMIT WORK.

  REFRESH: s_t_days_ins,
           s_t_res_ins.
ENDFORM.                    "write_res_pack

*---------------------------------------------------------------------*
*       FORM DAYS_RECONCILED                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM display_days_reconciled.
  DATA:
    l_toggle TYPE i VALUE 1,
    l_flag   TYPE c VALUE 1.

*-- Datum von bis ausgeben
  FORMAT COLOR COL_GROUP INTENSIFIED ON.
  ULINE /1(21).
  WRITE: / sy-vline, text-250,
           21 sy-vline.
  ULINE /1(21).

  LOOP AT l_t_days_rc.

*-- Farbintensität triggern
    IF l_toggle < 0.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDIF.
    l_toggle = l_toggle * ( -1 ).

*-- Ausgeben
    WRITE: / sy-vline , l_t_days_rc-cpudt,
             21 sy-vline.
    l_flag = con_on.
  ENDLOOP.

  IF l_flag = con_on.
    ULINE /1(21).
  ENDIF.
ENDFORM.                    "display_days_reconciled
*&---------------------------------------------------------------------*
*&      Form  TEST_ACTIV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM test_active TABLES
                     c_t_ledger STRUCTURE range_c2.
  DATA:
    l_t_t001        LIKE t001  OCCURS 0 WITH HEADER LINE,
    l_f_global_data TYPE fmfi_control_data,
    l_flg_active    TYPE c.

  SELECT SINGLE * FROM fm01
   WHERE fikrs IN s_fikrs.
  IF sy-subrc <> 0.
    MESSAGE w003 WITH s_fikrs-low.
*   Finanzkreis & nicht vorhanden
  ENDIF.

*-- Buchungskreis holen
  CALL FUNCTION 'BUKRS_GET_FROM_FIKRS'
    EXPORTING
      ip_fikrs = fm01-fikrs
    TABLES
      t_t001   = l_t_t001.

  READ TABLE l_t_t001 INDEX 1.

*----- Buchungskreis prüfen
  CALL FUNCTION 'FI_COMPANY_CODE_DATA'
    EXPORTING
      i_bukrs = l_t_t001-bukrs.

*----- Globale Daten besorgen
  CALL FUNCTION 'FMCA_GET_INIT_INFO'
    EXPORTING
      i_bukrs          = l_t_t001-bukrs
    CHANGING
      c_f_control_data = l_f_global_data.

*----- Ist Zahlungsabgleich aktiv?
  IF l_f_global_data-flg_paym_rc = con_off.
    MESSAGE e177.
*    Zahlungsabgleich ist inaktiv
  ENDIF.

*----- In Profil 500 nur das Verpflichtungsbudget
  CALL FUNCTION 'FM_PAYMENT_PROFILE_CHECK'
    EXPORTING
      i_bukrs      = l_t_t001-bukrs
    IMPORTING
      e_flg_active = l_flg_active.
  CHECK l_flg_active = con_on.

  MESSAGE i181.
*   Der Zahlungsabgleich wird nur im Verpflichtungsbudget durchgeführt

  c_t_ledger-sign   = 'I'.
  c_t_ledger-option = 'EQ'.
  c_t_ledger-low    = fmfi_con_ldnr_commitment.
  APPEND c_t_ledger.
ENDFORM.                               " TEST_ACTIV
*&---------------------------------------------------------------------*
*&      Form  UPDATE_INVISIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_DOCS_INVISIBLE  text
*----------------------------------------------------------------------*
FORM update_invisible TABLES
                         u_t_docs_invisible STRUCTURE fmrc07.

  LOOP AT u_t_docs_invisible.
    UPDATE fmrc07   SET invisible = u_t_docs_invisible-invisible
                  WHERE bukrs     = u_t_docs_invisible-bukrs
                    AND belnr     = u_t_docs_invisible-belnr
                    AND gjahr     = u_t_docs_invisible-gjahr.
  ENDLOOP.

ENDFORM.                               " UPDATE_INVISIBLE
*&---------------------------------------------------------------------*
*&      Form  check_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_BKPF_ALL  text
*      -->P_G_T_RES_DOCS  text
*----------------------------------------------------------------------*
FORM check_cc TABLES   u_t_bkpf_all STRUCTURE g_t_bkpf_all
                       u_t_bvor     STRUCTURE g_t_bvor
                       c_t_res_docs STRUCTURE g_t_res_docs.

  DATA:
    l_btrfi LIKE fmrc07-btrfi,
    l_btrfm LIKE fmrc07-btrfm,
    BEGIN OF l_t_index_check_cc OCCURS 0,
      min LIKE sy-tabix,
      max LIKE sy-tabix,
    END OF l_t_index_check_cc,
    l_flg_false_date TYPE c.

  CLEAR c_t_res_docs.
  SORT c_t_res_docs.

  LOOP AT u_t_bvor.

*-- Sammeln aller Einträge zu einem buchungskreisübergr. Vorgang
    READ TABLE c_t_res_docs WITH KEY
                               bukrs = u_t_bvor-bukrs
                               belnr = u_t_bvor-belnr
                               gjahr = u_t_bvor-gjahr
                            BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      l_t_index_check_cc-min = sy-tabix.
      IF c_t_res_docs-error = con_false_date.
        l_flg_false_date = 'X'.
      ENDIF.
      LOOP AT c_t_res_docs FROM sy-tabix.
        ADD:
          c_t_res_docs-btrfi TO l_btrfi,
          c_t_res_docs-btrfm TO l_btrfm.
        AT END OF gjahr.
          l_t_index_check_cc-max = sy-tabix.

*-- Merken der Indizes der betroffenen Einträge
          APPEND l_t_index_check_cc.
          EXIT.
        ENDAT.
      ENDLOOP.
    ENDIF.

*-- Auswertung: Stimmen FI und FM Beträge jetzt überein,
*--  werden die Einträge aus der Residualliste gelöscht
*--  bzw als fehelrfrei gekennzeichnet
    AT END OF bvorg.
      IF l_btrfi = l_btrfm AND l_flg_false_date IS INITIAL.
        DESCRIBE TABLE l_t_index_check_cc LINES sy-tfill.
        IF NOT sy-tfill IS INITIAL.
          SORT l_t_index_check_cc BY min DESCENDING.

*-- Schleife über die Tabelle der gemerkten Indizes
          LOOP AT l_t_index_check_cc.
            IF p_eron IS INITIAL.
              LOOP AT c_t_res_docs FROM l_t_index_check_cc-min
                                     TO l_t_index_check_cc-max.
                c_t_res_docs-error = con_no_error.
                MODIFY c_t_res_docs TRANSPORTING error.
              ENDLOOP.
            ELSE.
              DELETE c_t_res_docs FROM l_t_index_check_cc-min
                                    TO l_t_index_check_cc-max.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      CLEAR: l_btrfi, l_btrfm, l_flg_false_date.
      REFRESH l_t_index_check_cc.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " check_cc
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_FMIFIIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_T_BKPF  text
*----------------------------------------------------------------------*
FORM archive_read_fmifiit  TABLES  p_t STRUCTURE g_t_bkpf
                                   u_t_ledger   STRUCTURE range_c2.
  TYPES: BEGIN OF ty_fmifiit,
            fmbelnr       TYPE      fm_belnr,
            fikrs   TYPE  fikrs,
            fmbuzei   TYPE  fm_buzei,
            btart   TYPE  fm_btart,
            rldnr   TYPE  rldnr,
            gjahr   TYPE  gjahr,
            stunr   TYPE  fm_stunr,
            vobelnr   TYPE  fm_vobelnr,
            vobukrs   TYPE  fm_vobukrs,
            vogjahr   TYPE  fm_vogjahr,
            wrttp   TYPE  fm_wrttp,
                archivekey TYPE arkey,
                archiveofs TYPE admi_offst.
  TYPES: END OF ty_fmifiit.

  DATA: l_handle    TYPE sytabix,
        lt_fmifiit     TYPE TABLE OF fmifiit WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_fmifiit TYPE TABLE OF ty_fmifiit,
        ls_inx_fmifiit TYPE ty_fmifiit.

  CONSTANTS: c_ztfmifiit_001(13) VALUE 'ZTFMIFIIT_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_ztfmifiit_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_fmifiit[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_fmifiit
    FROM (l_gentab)
    FOR ALL ENTRIES IN p_t
  WHERE vobelnr =  p_t-belnr
    AND vobukrs =  p_t-bukrs
    AND vogjahr =  p_t-gjahr
AND ( wrttp  =  wrttp9  OR
      wrttp  =  wrttp9a OR
      wrttp  =  wrttp9b )
    AND btart  <>  fmfi_con_btart_acold
    AND btart  <>  fmfi_con_btart_acnew
    AND rldnr  IN  u_t_ledger.


  CHECK NOT lt_inx_fmifiit[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_fmifiit_a, gt_fmifiit_a[].
  LOOP AT lt_inx_fmifiit INTO ls_inx_fmifiit.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FM_DOC_FI'
        archivkey                 = ls_inx_fmifiit-archivekey
        offset                    = ls_inx_fmifiit-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
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
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_fmifiit, lt_fmifiit[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'FMIFIIT'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_fmifiit
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_fmifiit[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_fmifiit INTO TABLE gt_fmifiit_a.
  ENDLOOP.

  CHECK NOT gt_fmifiit_a[] IS INITIAL.

  SORT gt_fmifiit_a.
  DELETE ADJACENT DUPLICATES FROM gt_fmifiit_a COMPARING ALL FIELDS.

  LOOP AT gt_fmifiit_a.
    CLEAR g_t_fmifiit.
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
    APPEND g_t_fmifiit.  CLEAR g_t_fmifiit.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_FMIFIIT
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_T001  text
*----------------------------------------------------------------------*
FORM archive_read_bkpf  TABLES   p_g_t_t001 STRUCTURE t001.

  TYPES: BEGIN OF ty_bkpf,
            fmbelnr       TYPE      fm_belnr,
            fikrs   TYPE  fikrs,
            fmbuzei   TYPE  fm_buzei,
            btart   TYPE  fm_btart,
            rldnr   TYPE  rldnr,
            gjahr   TYPE  gjahr,
            stunr   TYPE  fm_stunr,
            vobelnr   TYPE  fm_vobelnr,
            vobukrs   TYPE  fm_vobukrs,
            vogjahr   TYPE  fm_vogjahr,
            wrttp   TYPE  fm_wrttp,
                archivekey TYPE arkey,
                archiveofs TYPE admi_offst.
  TYPES: END OF ty_bkpf.

  DATA: l_handle    TYPE sytabix,
        lt_bkpf     TYPE TABLE OF bkpf WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bkpf TYPE TABLE OF ty_bkpf,
        ls_inx_bkpf TYPE ty_bkpf.

  CONSTANTS: c_ztbkpf_001(9) VALUE 'ZBKPF_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_ztbkpf_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bkpf[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bkpf
    FROM (l_gentab)
    FOR ALL ENTRIES IN p_g_t_t001
   WHERE bukrs = p_g_t_t001-bukrs
     AND belnr IN s_belnr
     AND cpudt IN s_cpudt
     AND budat IN s_budat
     AND bstat =  ' '.

  CHECK NOT lt_inx_bkpf[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bkpf_a, gt_bkpf_a[].
  LOOP AT lt_inx_bkpf INTO ls_inx_bkpf.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bkpf-archivekey
        offset                    = ls_inx_bkpf-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
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
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bkpf, lt_bkpf[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BKPF'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bkpf
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    DELETE lt_bkpf WHERE NOT ( cpudt IN s_cpudt AND bstat =  ' ' ).

    CHECK sy-subrc = 0 AND NOT lt_bkpf[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bkpf INTO TABLE gt_bkpf_a.
  ENDLOOP.

  CHECK NOT gt_bkpf_a[] IS INITIAL.

  SORT gt_bkpf_a.
  DELETE ADJACENT DUPLICATES FROM gt_bkpf_a COMPARING ALL FIELDS.

  LOOP AT gt_bkpf_a.
    CLEAR g_t_bkpf_all.
    MOVE-CORRESPONDING gt_bkpf_a TO g_t_bkpf_all.
    APPEND g_t_bkpf_all.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_BKPF
