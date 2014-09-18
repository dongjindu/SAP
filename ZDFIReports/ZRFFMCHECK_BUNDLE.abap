*&---------------------------------------------------------------------*
*& Report  ZRFFMCHECK_BUNDLE                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
*& bereitgestellt zur Aufklärung der großen Belegbündel an Tübingen    *
*& durch Berater (Markus Herhold, Lutz Rohloff)                        *
*&---------------------------------------------------------------------*
* Programm analysiert für einen oder mehrere Startbelege das zugehörige
* Belegbündel nach Anzahl der Belege pro Kontonummer und Geschäfts-
* jahr. Dadurch können die Konten ermittelt werden, die die meisten
* aktuellen Belege im Bündel besitzen - diese Konten können dann
* manuell geschlossen werden

report  zrffmcheck_bundle             .

*-Typepools------------------------------------------------------------*
type-pools:
fmfi.

*-Includes-------------------------------------------------------------*
include:
ifmficon,
ififmcon_bool,
ififmcon_appl,
fm_rffms200_data.

*-Tabellen-------------------------------------------------------------*
tables:
  bkpf.

*- Datendeklaration ---------------------------------------------------*
data:
  g_t_fi_keys    type fmpa_t_fi_header_type       with header line,
  g_t_header     like g_t_fi_header  occurs 0     with header line,
  g_t_items      like g_t_fi_items   occurs 0     with header line,
  g_fikrs        like t001-fikrs,

*- Zeilen pro Konto und Periode
  begin of g_t_lines  occurs 0,
    kunnr   like bseg-kunnr,
    lifnr   like bseg-lifnr,
    opcnt   type i,
    clcnt   type i,
    count   type i,
  end of g_t_lines,

*-Dokumente
  begin of g_t_docs_cleared  occurs 0,
    kunnr   like bseg-kunnr,
    lifnr   like bseg-lifnr,
    bukrs   like bseg-bukrs,
    belnr   like bseg-belnr,
    gjahr   like bseg-gjahr,
    augbl   like bseg-augbl,
    augdt   like bseg-augdt,
  end of g_t_docs_cleared,

  begin of g_t_docs_open occurs 0,
    kunnr   like bseg-kunnr,
    lifnr   like bseg-lifnr,
    bukrs   like bseg-bukrs,
    belnr   like bseg-belnr,
    gjahr   like bseg-gjahr,
  end of g_t_docs_open.

*- Parameter und Select-Options (S200) --------------------------------*

selection-screen begin of block block01 with frame title text-010.
parameters:
*- Buchungskreis
  p_bukrs like bkpf-bukrs   obligatory memory id buk,

*- Geschäftsjahr
  p_gjahr like bkpf-gjahr   obligatory memory id gjr.
select-options:
so_belnr for bkpf-belnr   obligatory.

selection-screen end of block block01.

************************************************************************
start-of-selection.
************************************************************************
*------Message-Handler initialisieren
  call function 'MESSAGES_INITIALIZE'.

*----- Buchungskreis lesen
  call function 'FMFK_GET_FIKRS_FROM_BUKRS'
       exporting
            i_bukrs = p_bukrs
       importing
            e_fikrs = g_fikrs.

*----- KEYs der umzusetzenden FM Belege einlesen
  call function 'FM_PAYTR_READ_DOCUMENT_KEYS_FM'
       exporting
            i_bukrs   = p_bukrs
            i_fikrs   = g_fikrs
            i_gjahr   = p_gjahr
            i_hhm     = con_off
       tables
            t_fi_keys = g_t_fi_keys
            t_belnr   = so_belnr.

  loop at g_t_fi_keys where dlflg is initial.

*----- Belegweise Initialisieren
    refresh: g_t_header,
             g_t_items,
             g_t_lines,
             g_t_docs_open,
             g_t_docs_cleared.

*----- Fi Belege lesen
    call function 'FM_PAYTR_READ_DOCUMENTS_FI'
         tables
              t_fi_keys         = g_t_fi_keys
              t_header          = g_t_header
              t_items           = g_t_items
         exceptions
              fi_header_missing = 1.

    delete g_t_items where belnr cs '99$RD$'.

*----- Belege pro Konto und Jahr verdichten.
    loop at g_t_items where not kunnr is initial
                        or not lifnr is initial.

*------------------------------
      g_t_lines-kunnr = g_t_items-kunnr.
      g_t_lines-lifnr = g_t_items-lifnr.
      if g_t_items-augbl is initial.
        g_t_lines-opcnt = 1.
        g_t_lines-clcnt = 0.
        g_t_docs_open-kunnr = g_t_items-kunnr.
        g_t_docs_open-lifnr = g_t_items-lifnr.
        g_t_docs_open-bukrs = g_t_items-bukrs.
        g_t_docs_open-belnr = g_t_items-belnr.
        g_t_docs_open-gjahr = g_t_items-gjahr.
        collect g_t_docs_open.
      else.
        g_t_lines-clcnt = 1.
        g_t_lines-opcnt = 0.
        g_t_docs_cleared-kunnr = g_t_items-kunnr.
        g_t_docs_cleared-lifnr = g_t_items-lifnr.
        g_t_docs_cleared-bukrs = g_t_items-bukrs.
        g_t_docs_cleared-belnr = g_t_items-belnr.
        g_t_docs_cleared-gjahr = g_t_items-gjahr.
        g_t_docs_cleared-augbl = g_t_items-augbl.
        g_t_docs_cleared-augdt = g_t_items-augdt.
        collect g_t_docs_cleared.
      endif.
      g_t_lines-count = 1.
      collect g_t_lines.

*------------------------------
    endloop.

    describe table g_t_header lines sy-tfill.
    format color col_total intensified on.
    write:/ sy-tfill, text-001,
                       g_t_fi_keys-bukrs,
                       g_t_fi_keys-belnr,
                       g_t_fi_keys-gjahr, text-002.
    loop at g_t_lines.

*----- Konten ausgeben
      skip.
      format color col_group intensified on.
      if not g_t_lines-kunnr is initial.
        write: / text-003, g_t_lines-kunnr,
               / text-005, g_t_lines-count, text-006.
      else.
        write: / text-004, g_t_lines-lifnr,
               / text-005, g_t_lines-count, text-006.
      endif.

*----- Offene Belege ausgeben
      skip.
      format color col_total intensified off.
      write: / text-007, g_t_lines-opcnt.

      if g_t_lines-opcnt > 0.
        format color col_heading intensified on.
        uline /(24).
        write at: /001 sy-vline, 002(5)  text-008,
                   007 sy-vline, 008(10) text-009,
                   018 sy-vline, 019(5)  text-010,
                   024 sy-vline.
        uline /(24).
        loop at g_t_docs_open where kunnr = g_t_lines-kunnr
                                and lifnr = g_t_lines-lifnr.
          write at: /001 sy-vline, 002(5)  g_t_docs_open-bukrs
                                            color col_key,
                     007 sy-vline, 008(10) g_t_docs_open-belnr
                                            color col_key,
                     018 sy-vline, 019(5)  g_t_docs_open-gjahr
                                            color col_key,
                     024 sy-vline.
        endloop.
        uline /(24).
      endif.

*----- Ausgegichene Belege ausgeben
      skip.
      format color col_total intensified off.
      write: / text-011, g_t_lines-clcnt.

      if g_t_lines-clcnt > 0.
        format color col_heading intensified on.
        uline /(46).
        write at: /001 sy-vline, 002(5)  text-008,
                   007 sy-vline, 008(10) text-009,
                   018 sy-vline, 019(5)  text-010,
                   024 sy-vline, 025(10) text-012,
                   035 sy-vline, 036(10) text-013,
                   046 sy-vline.
        uline /(46).
        loop at g_t_docs_cleared where kunnr = g_t_lines-kunnr
                                   and lifnr = g_t_lines-lifnr.
          write at: /001 sy-vline, 002(5)  g_t_docs_cleared-bukrs
                                            color col_key,
                     007 sy-vline, 008(10) g_t_docs_cleared-belnr
                                            color col_key,
                     018 sy-vline, 019(5)  g_t_docs_cleared-gjahr
                                            color col_key,
                     024 sy-vline, 025(10) g_t_docs_cleared-augbl
                                            color col_key,
                     035 sy-vline, 036(10) g_t_docs_cleared-augdt
                                            color col_key,
                     046 sy-vline.
        endloop.
        uline /(46).
      endif.
    endloop.
    uline.
  endloop.
