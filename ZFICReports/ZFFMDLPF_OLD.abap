* Notes: 498580
* if c/f, then reset to delete payment data
*
report rffmdlpf_old.
include: ififmcon_value_types,
         ififmcon_bool,
         ififmcon_appl,
         lfmauequ,
         rfeposc1.

type-pools:
  fmfi,
  slis.

tables:
  fmifiit.

data:
  begin of g_t_augbl occurs 0,
    kunnr   like bseg-kunnr,
    lifnr   like bseg-lifnr,
    augdt   like bseg-augdt,
    augbl   like bseg-augbl,
    gjahr   like bseg-gjahr,
    bukrs   like bseg-bukrs,
    belnr   like bseg-belnr,
  end   of g_t_augbl,

  begin of g_t_ledger occurs 5,
    rldnr like fmifiit-rldnr,
    payment_fdate,
  end   of g_t_ledger,

  g_t_fmifiit_del     like fmifiit         occurs 0 with header line,
  g_t_dummy           like fmifiit         occurs 0 with header line,
  g_t_muster          like fmifiit         occurs 0 with header line,
  g_t_90_30           like bkpf            occurs 0 with header line,
  g_t_all             like bkpf            occurs 0 with header line,
  g_t_bkpf            like bkpf            occurs 0 with header line,
  g_t_bkpf_cc         like bkpf            occurs 0 with header line,
  g_t_postab          like postab          occurs 0 with header line,
  g_f_fm01d           like fm01d,
  g_fikrs             like fmifiit-fikrs,

  g_cnt_lines         type i.

selection-screen begin of block selektion with frame title text-020.
parameter:
  p_bukrs like fmifiit-bukrs obligatory,
  p_gjahr like fmifiit-kngjahr obligatory.

select-options
  s_belnr for  fmifiit-knbelnr.
selection-screen end   of block selektion.

selection-screen skip 1.

selection-screen begin of block steuerung with frame title text-030.
parameter:
  p_test like lko74-testlauf  default 'X'.
selection-screen end   of block steuerung.

start-of-selection.

  call function 'FMFK_GET_FIKRS_FROM_BUKRS'
    exporting
      i_bukrs = p_bukrs
    importing
      e_fikrs = g_fikrs.

  if sy-subrc <> 0.
  endif.

*  select single fikrs into g_fikrs
*    from  t001
*    where bukrs = p_bukrs.

  call function 'FMAU_AUTHORITY_FIFM'
       exporting
            i_actvt       = fmau_ac_ini
            i_auth_object = 'F_FICB_FKR'
            i_fikrs       = g_fikrs
            i_msgty       = 'E'.

  if sy-subrc <> 0.
  endif.

*----- Die Daten zum Finanzkreis lesen
  call function 'FMFK_FIKRS_READ'
    exporting
      ip_fikrs            = g_fikrs
      ip_application_data = con_on
      ip_applc            = applc_ca
    importing
      f_fm01d             = g_f_fm01d.

  check g_f_fm01d-fm_paym_s200_rc  = con_off and
        g_f_fm01d-fm_paym_s200_nrc = con_on.

*--- Selektion
  select * from fmifiit
    into table g_t_fmifiit_del
   where knbelnr in s_belnr
     and kngjahr  = p_gjahr
     and   bukrs  = p_bukrs
     and   vrgng <> fmfi_con_orgvg_dpclearing
     and   vrgng <> fmfi_con_orgvg_dpc_transfer.

*---- Musterbeleg holen
  loop at g_t_fmifiit_del.
    g_t_ledger-rldnr = g_t_fmifiit_del-rldnr.
    collect g_t_ledger.
  endloop.

  perform payment_muster(rffms200_old) tables g_t_ledger
                                              g_t_fmifiit_del
                                              g_t_muster
                                        using 'I'.

*---- Nur Satze der neuen Fortschreibung uberarbeiten
  delete g_t_fmifiit_del where stunr+15(1) = 'T'
                            or ( btart <> fmfi_con_btart_reduction and
                                 btart <> fmfi_con_btart_payed ).

*--- Nur die Satze des aktuellen Geschaftsjahres bearbeiten
  loop at g_t_muster.
    loop at g_t_fmifiit_del where knbelnr = g_t_muster-knbelnr
                              and kngjahr = g_t_muster-kngjahr
                              and   bukrs = g_t_muster-bukrs
                              and fmbuzei = g_t_muster-fmbuzei.
      if g_t_muster-gjahr <> g_t_fmifiit_del-gjahr.
        delete g_t_fmifiit_del.
      endif.
    endloop.
  endloop.

*---- Testbeleg holen ( gesplttet in CC und nicht CC )
  perform split_cc_not_cc tables g_t_fmifiit_del
                                 g_t_all
                                 g_t_bkpf
                                 g_t_bkpf_cc.

*---- 90-30 Buchungen rauswerfen
  perform delete_90_30_not_cc tables g_t_fmifiit_del
                                     g_t_90_30
                                     g_t_bkpf.

  perform delete_90_30_cc tables g_t_fmifiit_del
                                 g_t_90_30
                                 g_t_bkpf_cc.

*--- alle restlichen Belege von DB loschen
  describe table g_t_fmifiit_del lines sy-tfill.
  g_cnt_lines = sy-tfill.

  delete fmifiit from table g_t_fmifiit_del.

*----- Ausgleiche holen
  loop at g_t_all.
    call function 'GET_CLEARED_ITEMS'
      exporting
        i_belnr = g_t_all-belnr
        i_bukrs = g_t_all-bukrs
        i_gjahr = g_t_all-gjahr
        i_bvorg = g_t_all-bvorg
      tables
        t_items = g_t_postab
      exceptions
        others  = 1.
    loop at g_t_postab.
      g_t_augbl-belnr = g_t_postab-belnr.
      g_t_augbl-gjahr = g_t_postab-gjahr.
      g_t_augbl-bukrs = g_t_postab-bukrs.
      collect g_t_augbl.
    endloop.
    refresh g_t_postab.
  endloop.

*----- Payflgs im alten Geschaftsjahr offnen
  sort g_t_90_30 by bukrs belnr gjahr.
  loop at g_t_augbl.
    read table g_t_90_30
      with key bukrs = g_t_augbl-bukrs
               belnr = g_t_augbl-belnr
               gjahr = g_t_augbl-gjahr.
    if sy-subrc <> 0.
      update fmifiit
         set payflg = 'Y'
       where vobelnr = g_t_augbl-belnr
         and vogjahr = g_t_augbl-gjahr
         and vobukrs = g_t_augbl-bukrs
         and wrttp   = wrttp6
         and stunr like '%P'.
      if sy-subrc <> 0.
        update fmifiit
           set payflg = 'Y'
         where vobelnr = g_t_augbl-belnr
           and vogjahr = g_t_augbl-gjahr
           and vobukrs = g_t_augbl-bukrs
           and wrttp   = wrttp6.
      endif.
    else.
      update fmifiit
         set payflg = 'Y'
       where vobelnr = g_t_augbl-belnr
         and vogjahr = g_t_augbl-gjahr
         and vobukrs = g_t_augbl-bukrs
         and ( knbelnr <> g_t_augbl-belnr
            or kngjahr <> g_t_augbl-gjahr
            or   bukrs <> g_t_augbl-bukrs )
         and wrttp   = wrttp6
         and stunr like '%P'.
      if sy-subrc <> 0.
        update fmifiit
           set payflg = 'Y'
         where vobelnr = g_t_augbl-belnr
           and vogjahr = g_t_augbl-gjahr
           and vobukrs = g_t_augbl-bukrs
         and ( knbelnr <> g_t_augbl-belnr
            or kngjahr <> g_t_augbl-gjahr
            or   bukrs <> g_t_augbl-bukrs )
           and wrttp   = wrttp6.
      endif.
    endif.
  endloop.

*--- Summensatze anpassen
*--- Vorzeichen wechseln
  call function 'FM_CONVERT_SIGN_FI'
    tables
      t_fmifiit = g_t_fmifiit_del.

*---Summensatztabelle anpassen
  call function 'FM_TOTALS_UPDATE_FI'
    tables
      t_fmifiit     = g_t_dummy
      t_fmifiit_del = g_t_fmifiit_del.

  if p_test is initial.
    commit work.
  else.
    rollback work.
  endif.

*----- Ausgabe
  perform display_list tables g_t_fmifiit_del
                       using  text-050.

*&---------------------------------------------------------------------*
*&      Form  split_cc_not_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_FMIFIIT_DEL  text
*      -->P_G_T_BKPF  text
*      -->P_G_T_BKPF_CC  text
*----------------------------------------------------------------------*
form split_cc_not_cc tables u_t_fmifiit_del structure fmifiit
                            c_t_all         structure bkpf
                            c_t_bkpf        structure bkpf
                            c_t_bkpf_cc     structure bkpf.
  data:
    l_t_bvor like bvor occurs 0 with header line.

*---- Belege auf CC testen
  loop at u_t_fmifiit_del.
    c_t_bkpf-bukrs = g_t_fmifiit_del-vobukrs.
    c_t_bkpf-belnr = g_t_fmifiit_del-vobelnr.
    c_t_bkpf-gjahr = g_t_fmifiit_del-vogjahr.
    collect c_t_bkpf.
  endloop.

  check not c_t_bkpf[] is initial.

  append lines of c_t_bkpf to c_t_all.

  select bukrs belnr gjahr bvorg from bkpf
    into corresponding fields of table c_t_bkpf_cc
     for all entries in c_t_bkpf
   where bukrs = c_t_bkpf-bukrs
     and belnr = c_t_bkpf-belnr
     and gjahr = c_t_bkpf-gjahr
     and bvorg <> space.
  if not c_t_bkpf_cc[] is initial.

*----- Buchungskreisubergreifender Teil einlesen
    select * from bvor
      into   table l_t_bvor
       for   all entries in c_t_bkpf_cc
     where   bvorg = c_t_bkpf_cc-bvorg
       and   xarch = space.

    select * from bkpf
      into   table c_t_bkpf_cc
       for   all entries in l_t_bvor
     where   bukrs = l_t_bvor-bukrs
       and   belnr = l_t_bvor-belnr
       and   gjahr = l_t_bvor-gjahr.

    sort c_t_bkpf_cc by bukrs belnr gjahr.
  endif.

*----- Fur nicht buchungskreisubergreifende Buchungen mussen nur
*----- Belege mit identischem Zahlungs und Rechnungsbezug untersucht
*----- werden
  refresh: c_t_bkpf.

  loop at u_t_fmifiit_del.
    if u_t_fmifiit_del-bukrs   = u_t_fmifiit_del-vobukrs and
       u_t_fmifiit_del-knbelnr = u_t_fmifiit_del-vobelnr and
       u_t_fmifiit_del-kngjahr = u_t_fmifiit_del-vogjahr.

      read table c_t_bkpf_cc
        with key bukrs = u_t_fmifiit_del-vobukrs
                 belnr = u_t_fmifiit_del-vobelnr
                 gjahr = u_t_fmifiit_del-vogjahr.
      check sy-subrc <> 0.

      c_t_bkpf-bukrs = u_t_fmifiit_del-vobukrs.
      c_t_bkpf-belnr = u_t_fmifiit_del-vobelnr.
      c_t_bkpf-gjahr = u_t_fmifiit_del-vogjahr.
      collect c_t_bkpf.
    endif.
  endloop.
endform.                    " split_cc_not_cc
*&---------------------------------------------------------------------*
*&      Form  delete_90_30_not_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_BKPF  text
*----------------------------------------------------------------------*
form delete_90_30_not_cc tables c_t_fmifiit_del structure fmifiit
                                c_t_90_30       structure bkpf
                                c_t_bkpf        structure bkpf.
  data:
    l_t_bseg  like bseg   occurs 0 with header line,

    l_f_fmci like fmci,

    l_fikrs   like fmifiit-fikrs,
    l_bukrs   like fmifiit-bukrs.

  check not c_t_bkpf[] is initial.

  select bukrs belnr gjahr fipos lifnr kunnr xopvw
    from bseg
    into corresponding fields of table l_t_bseg
     for all entries in c_t_bkpf
   where bukrs = c_t_bkpf-bukrs
     and belnr = c_t_bkpf-belnr
     and gjahr = c_t_bkpf-gjahr
     and ktosl <> 'BUV'
     and xref1 <> fmfi_con_euro_fi.

  loop at l_t_bseg.

*----- Finnazkreis nachlesen
    if l_bukrs <> l_t_bseg-bukrs.
      call function 'FMFK_GET_FIKRS_FROM_BUKRS'
        exporting
          i_bukrs = l_t_bseg-bukrs
        importing
          e_fikrs = l_fikrs.
      l_bukrs = l_t_bseg-bukrs.
    endif.

*---- Finanzposition nachlesen
    call function 'fmci_READ_QUICK'
      exporting
        ip_flg_buffer_all = con_on
        ip_fikrs          = l_fikrs
        ip_gjahr          = l_t_bseg-gjahr
        ip_fipos          = l_t_bseg-fipos
      importing
        f_fmci           = l_f_fmci.

*---- Belege trennen
    if l_f_fmci-fivor > '60'.
      delete c_t_fmifiit_del where vobukrs = l_t_bseg-bukrs
                               and vobelnr = l_t_bseg-belnr
                               and vogjahr = l_t_bseg-gjahr
                               and bukrs   = l_t_bseg-bukrs
                               and knbelnr = l_t_bseg-belnr
                               and kngjahr = l_t_bseg-gjahr.
      c_t_90_30-bukrs = l_t_bseg-bukrs.
      c_t_90_30-gjahr = l_t_bseg-gjahr.
      c_t_90_30-belnr = l_t_bseg-belnr.
      collect c_t_90_30.
    endif.
  endloop.
endform.                    " delete_90_30_not_cc
*&---------------------------------------------------------------------*
*&      Form  delete_90_30_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_FMIFIIT_DEL  text
*      -->P_G_T_BKPF_CC  text
*----------------------------------------------------------------------*
form delete_90_30_cc tables c_t_fmifiit_del structure fmifiit
                            c_t_90_30       structure bkpf
                            c_t_bkpf        structure bkpf.
  data:
    begin of l_t_bseg occurs 0,
      bvorg like bkpf-bvorg,
      bukrs like bseg-bukrs,
      belnr like bseg-belnr,
      gjahr like bseg-gjahr,
      fipos like bseg-fipos,
      kunnr like bseg-kunnr,
      lifnr like bseg-lifnr,
      xopvw like bseg-xopvw,
    end   of l_t_bseg,

    l_t_payments like l_t_bseg occurs 0 with header line,

    l_f_fmci  like fmci,
    l_lo_fivor like fmci-fivor value '90',
    l_hi_fivor like fmci-fivor value '00',

    l_fikrs   like fmifiit-fikrs,
    l_bukrs   like fmifiit-bukrs.

  check not c_t_bkpf[] is initial.

  select bukrs belnr gjahr fipos kunnr lifnr xopvw
    from bseg
    into corresponding fields of table l_t_bseg
     for all entries in c_t_bkpf
   where bukrs = c_t_bkpf-bukrs
     and belnr = c_t_bkpf-belnr
     and gjahr = c_t_bkpf-gjahr
     and ktosl <> 'BUV'
     and xref1 <> fmfi_con_euro_fi.

*----- Gemeinsamer key einschreiben
  sort l_t_bseg by bukrs belnr gjahr.
  loop at c_t_bkpf.
    read table l_t_bseg with key bukrs = c_t_bkpf-bukrs
                                 belnr = c_t_bkpf-belnr
                                 gjahr = c_t_bkpf-gjahr
    binary search.

    check sy-subrc = 0.

    loop at l_t_bseg from sy-tabix.
      if l_t_bseg-bukrs <> c_t_bkpf-bukrs or
         l_t_bseg-belnr <> c_t_bkpf-belnr or
         l_t_bseg-gjahr <> c_t_bkpf-gjahr.
        exit.
      endif.

      l_t_bseg-bvorg = c_t_bkpf-bvorg.
      modify l_t_bseg transporting bvorg.
    endloop.
  endloop.

  sort l_t_bseg by bvorg.

  loop at l_t_bseg.

*----- Finnazkreis nachlesen
    if l_bukrs <> l_t_bseg-bukrs.
      call function 'FMFK_GET_FIKRS_FROM_BUKRS'
        exporting
          i_bukrs = l_t_bseg-bukrs
        importing
          e_fikrs = l_fikrs.
      l_bukrs = l_t_bseg-bukrs.
    endif.

*---- Finanzposition nachlesen
    call function 'fmci_READ_QUICK'
      exporting
        ip_flg_buffer_all = con_on
        ip_fikrs          = l_fikrs
        ip_gjahr          = l_t_bseg-gjahr
        ip_fipos          = l_t_bseg-fipos
      importing
        f_fmci           = l_f_fmci.

*---- Zahlungen merken
    if l_f_fmci-fivor = '90'.
      l_t_payments-belnr = l_t_bseg-belnr.
      l_t_payments-gjahr = l_t_bseg-gjahr.
      l_t_payments-bukrs = l_t_bseg-bukrs.
      collect l_t_payments.
    endif.

    if l_f_fmci-fivor < l_lo_fivor.
      l_lo_fivor = l_f_fmci-fivor.
    endif.

    if l_f_fmci-fivor > l_hi_fivor.
      l_hi_fivor = l_f_fmci-fivor.
    endif.

    at end of bvorg.
      if l_lo_fivor = '30' and l_hi_fivor = '90'.
        loop at l_t_payments.
          delete c_t_fmifiit_del where vobukrs = l_t_payments-bukrs
                                   and vobelnr = l_t_payments-belnr
                                   and vogjahr = l_t_payments-gjahr.
          c_t_90_30-bukrs = l_t_payments-bukrs.
          c_t_90_30-gjahr = l_t_payments-gjahr.
          c_t_90_30-belnr = l_t_payments-belnr.
          collect c_t_90_30.
        endloop.
      endif.
      refresh: l_t_payments.

      l_lo_fivor = '90'.
      l_hi_fivor = '00'.
    endat.
  endloop.
endform.                    " delete_90_30_cc
*---------------------------------------------------------------------*
*  FORM display_list
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  C_T_OUTLIST
*  -->  U_TEXT
*---------------------------------------------------------------------*
form display_list tables c_t_outlist structure fmifiit
                  using  u_text      type c.
  data:
    l_t_fieldcat type slis_t_fieldcat_alv,
    l_f_layout   type slis_layout_alv,
    l_text       type lvc_title.

*----- Uberschrift zusammenbauen
  move u_text to l_text.

*----- Pfeil ausgeben
  l_f_layout-box_fieldname = 'INVISIBLE'.
  l_f_layout-info_fieldname = 'LCOL'.

*----- pass it to re-use function
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program          = 'RFFMDLPF_OLD'
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      i_structure_name            = 'FMIFIIT'
      i_grid_title                = l_text
      is_layout                   = l_f_layout
      it_fieldcat                 = l_t_fieldcat
    tables
      t_outtab                    = c_t_outlist.
endform.                    " display_list
*&---------------------------------------------------------------------*
*&      Form  html_top_of_page
*&---------------------------------------------------------------------*
*----- this routine is called back from function                       *
*----- REUSE_ALV_GRID_DISPLAY and creates a commentary block           *
*----- on top of the ALV list                                          *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form html_top_of_page using r_top  type ref to cl_dd_document.
  data:
    l_text   type sdydo_text_element,
    s_table  type ref to cl_dd_table_element,
    col_key  type ref to cl_dd_area,
    col_info type ref to cl_dd_area,
    col_keyx type ref to cl_dd_area,
    col_infox type ref to cl_dd_area,
    l_date(10) type c,
    l_time(10) type c.

  write sy-datum to l_date.
  write sy-uzeit to l_time.

  call method r_top->add_table
    exporting
      no_of_columns = 3
      with_heading  = space
      border        = '0'
    importing
      table         = s_table.

  call method s_table->add_column
    importing
      column = col_key.
  call method s_table->add_column
    importing
      column = col_info.
  call method s_table->add_column
    importing
      column = col_keyx.
  call method s_table->add_column
    importing
      column = col_infox.

  l_text = 'Datum'(060).
  call method col_keyx->add_text
    exporting
      text         = l_text
      sap_emphasis = 'STRONG'.
  call method col_infox->add_gap
    exporting
      width = 6.
  l_text = l_date.
  call method col_infox->add_text
    exporting
      text      = l_text
      sap_style = 'KEY'.

  call method s_table->new_row.

  l_text = 'Zeit'(070).
  call method col_keyx->add_text
    exporting
      text         = l_text
      sap_emphasis = 'STRONG'.
  call method col_infox->add_gap
    exporting
      width = 6.
  l_text = l_time.
  call method col_infox->add_text
    exporting
      text      = l_text
      sap_style = 'KEY'.
endform.                    " html_top_of_page
