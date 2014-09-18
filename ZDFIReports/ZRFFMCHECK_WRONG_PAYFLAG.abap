*&---------------------------------------------------------------------*
*& Report  ZRFFMCHECK_WRONG_PAYFLAG                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*& für Kunden bereitgestellt durch Hinweis 435328                      *
*&                                                                     *
*&---------------------------------------------------------------------*
*Es werden die Rechnungen selektiert, bei denen trotz Über- oder
*Unterzahlung bereits das Erledigtkennzeichen (Payflag) gesetzt
*ist.

report  zrffmcheck_wrong_payflag  line-size 126.

include: ififmcon_value_types,
         ififmcon_bool.

type-pools:
  fmfi.

tables:
  fmifiit,
  bkpf.

data:
  g_t_fmifiit_sel     like fmifiit         occurs 0 with header line,
  g_t_fmifiit_key     like fmifiit         occurs 0 with header line,
  g_t_fmifiit_tmp     like fmifiit         occurs 0 with header line,
  g_c_fmifiit         type cursor,

  begin of g_t_fmifiit occurs 0,
    bukrs   like fmifiit-bukrs,
    kngjahr like fmifiit-kngjahr,
    knbelnr like fmifiit-knbelnr,
    fmbuzei like fmifiit-fmbuzei,
    btart   like fmifiit-btart,
    fipex   like fmifiit-fipex,
    fkbtr   like fmifiit-fkbtr,
  end of g_t_fmifiit,
  g_f_fmifiit       like g_t_fmifiit,
  g_t_fmifiit_opay like g_t_fmifiit occurs 0 with header line,
  g_t_fmifiit_upay like g_t_fmifiit occurs 0 with header line,
  g_t_fmifiit_all   like fmifiit    occurs 0 with header line,
  l_fkbtr_original  like fmifiit-fkbtr,
  l_fkbtr_payed     like fmifiit-fkbtr,
  l_fkbtr_orig_pos  like fmifiit-fkbtr,
  l_fkbtr_pay_pos   like fmifiit-fkbtr,
  l_abs_diff        like fmifiit-fkbtr,
  l_payflg          like fmifiit-payflg,
  l_knbelnr         like fmifiit-knbelnr,
  l_kngjahr         like fmifiit-kngjahr,
  l_bukrs           like fmifiit-bukrs,
  l_tabix           like sy-tabix.

parameters:
  p_bukrs like fmifiit-bukrs   obligatory,
  p_knghr like fmifiit-kngjahr obligatory.
select-options:
  s_knbln for fmifiit-knbelnr.
selection-screen skip 1.

parameters:
  p_pf_upd as checkbox,
  p_rv_dsp as checkbox.



open cursor with hold g_c_fmifiit for
  select *
    from fmifiit
   where knbelnr  in s_knbln
     and kngjahr  = p_knghr
     and bukrs    = p_bukrs
     and btart    = fmfi_con_btart_original
     and wrttp    = wrttp6
     and payflg   = 'X'.

do.

*--- Selektion
  fetch next cursor g_c_fmifiit
    into table g_t_fmifiit_sel
    package size 5000.
  if sy-subrc <> 0.
    exit.
  endif.

*----- Belegkey extrahieren
  loop at g_t_fmifiit_sel.
    g_t_fmifiit_key-bukrs   = g_t_fmifiit_sel-bukrs.
    g_t_fmifiit_key-knbelnr = g_t_fmifiit_sel-knbelnr.
    g_t_fmifiit_key-kngjahr = g_t_fmifiit_sel-kngjahr.
    collect g_t_fmifiit_key.
  endloop.

  describe table g_t_fmifiit_key lines sy-tfill.
  if sy-tfill > 0.
    select *
     from fmifiit
     into table g_t_fmifiit_tmp
      for all entries in g_t_fmifiit_key
    where knbelnr = g_t_fmifiit_key-knbelnr
      and kngjahr = g_t_fmifiit_key-kngjahr
      and bukrs   = g_t_fmifiit_key-bukrs
      and ( wrttp = wrttp6
       or   wrttp = wrttp9 ).
  endif.

  append lines of g_t_fmifiit_tmp to g_t_fmifiit_all.
  loop at g_t_fmifiit_tmp.
    move-corresponding g_t_fmifiit_tmp to g_t_fmifiit.
    append g_t_fmifiit.
  endloop.

  sort g_t_fmifiit by bukrs kngjahr knbelnr fmbuzei.
  loop at g_t_fmifiit.
    g_f_fmifiit = g_t_fmifiit.
    if g_t_fmifiit-btart = fmfi_con_btart_original.
      l_fkbtr_original = l_fkbtr_original + g_t_fmifiit-fkbtr.
    endif.

    if g_t_fmifiit-btart = fmfi_con_btart_payed.
      l_fkbtr_payed = l_fkbtr_payed + g_t_fmifiit-fkbtr.
    endif.

    at end of fmbuzei.
      l_fkbtr_orig_pos = abs( l_fkbtr_original ).
      l_fkbtr_pay_pos  = abs( l_fkbtr_payed ).
      l_abs_diff = abs( l_fkbtr_orig_pos - l_fkbtr_pay_pos ).

      if l_abs_diff > '0.05'.
        if l_fkbtr_orig_pos < l_fkbtr_pay_pos.
          g_t_fmifiit_opay = g_f_fmifiit.
          g_t_fmifiit_opay-fkbtr = l_fkbtr_payed - l_fkbtr_original.
          append g_t_fmifiit_opay.
        elseif l_fkbtr_orig_pos > l_fkbtr_pay_pos.
          g_t_fmifiit_upay = g_f_fmifiit.
          g_t_fmifiit_upay-fkbtr = l_fkbtr_payed - l_fkbtr_original.
          append g_t_fmifiit_upay.
        endif.
      endif.
      clear l_fkbtr_original.
      clear l_fkbtr_payed.
    endat.
  endloop.

  refresh: g_t_fmifiit_tmp,
           g_t_fmifiit_sel,
           g_t_fmifiit_key,
           g_t_fmifiit.
enddo.

*----- Stornos ausschließen
sort g_t_fmifiit_all by knbelnr kngjahr bukrs.
loop at g_t_fmifiit_upay.
  clear l_fkbtr_payed.
  select single belnr from  bkpf into bkpf-belnr
         where  belnr  =  g_t_fmifiit_upay-knbelnr
         and    gjahr  =  g_t_fmifiit_upay-kngjahr
         and    bukrs  =  g_t_fmifiit_upay-bukrs
         and    stblg  <> space.
  if sy-subrc = 0.

*----- Gab es dazu Zahlungssätze?
    read table g_t_fmifiit_all with key
                                  knbelnr = g_t_fmifiit_upay-knbelnr
                                  kngjahr = g_t_fmifiit_upay-kngjahr
                                  bukrs   = g_t_fmifiit_upay-bukrs
                                  binary search.
    if sy-subrc = 0.
      l_tabix = sy-tabix.
      l_knbelnr = g_t_fmifiit_all-knbelnr.
      l_kngjahr = g_t_fmifiit_all-kngjahr.
      l_bukrs   = g_t_fmifiit_all-bukrs.
      loop at g_t_fmifiit_all from l_tabix.
        if l_knbelnr <> g_t_fmifiit_all-knbelnr
        or l_kngjahr <> g_t_fmifiit_all-kngjahr
        or l_bukrs   <> g_t_fmifiit_all-bukrs.
          exit.
        endif.
        if g_t_fmifiit_all-btart   = fmfi_con_btart_payed.
          l_fkbtr_payed = l_fkbtr_payed + g_t_fmifiit_all-fkbtr.
        endif.
      endloop.
    endif.
    if l_fkbtr_payed = 0.
      delete g_t_fmifiit_upay.
    else.
      if p_rv_dsp is initial.
        delete g_t_fmifiit_upay.
      endif.
    endif.
  endif.
endloop.


perform write_wrong_payments tables
                               g_t_fmifiit_opay
                               g_t_fmifiit_upay.

if p_pf_upd = 'X'.
  loop at g_t_fmifiit_opay.
    update fmifiit set payflg = l_payflg
                 where bukrs   = g_t_fmifiit_opay-bukrs
                   and kngjahr = g_t_fmifiit_opay-kngjahr
                   and knbelnr = g_t_fmifiit_opay-knbelnr.
  endloop.
  loop at g_t_fmifiit_upay.
    update fmifiit set payflg = l_payflg
                 where bukrs   = g_t_fmifiit_upay-bukrs
                   and kngjahr = g_t_fmifiit_upay-kngjahr
                   and knbelnr = g_t_fmifiit_upay-knbelnr.
  endloop.
endif.

*&---------------------------------------------------------------------*
*&      Form  write_wrong_payments
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_g_t_fmifiit_opay  text
*----------------------------------------------------------------------*
form write_wrong_payments
                      tables
                         u_t_fmifiit_opay  structure g_t_fmifiit_opay
                         u_t_fmifiit_upay  structure g_t_fmifiit_upay.

*----- Überzahlungen
  describe table g_t_fmifiit_opay lines sy-tfill.
  if sy-tfill = 0.
    write /  text-001 color col_total.
  else.
    format color col_total intensified on.
    write: / text-002, sy-tfill.

    skip 1.
*----Spaltenüberschriften
    format color col_heading intensified on.
    uline (63).
    write at: /001 sy-vline, 002(5)  text-020 color col_key,
               007 sy-vline, 008(10) text-021 color col_key,
               018 sy-vline, 019(5)  text-022 color col_key,
               024 sy-vline, 025(13) text-023,
               038 sy-vline, 039(24) text-024,
               063 sy-vline.
    uline (63).
    format color col_normal intensified on.
    loop at g_t_fmifiit_opay.
      write at: /001 sy-vline, 002(5)  g_t_fmifiit_opay-bukrs
                                       color col_key,           "Bukrs
                 007 sy-vline, 008(10) g_t_fmifiit_opay-knbelnr
                                       color col_key,           "Belnr
                 018 sy-vline, 019(5)  g_t_fmifiit_opay-kngjahr
                                       color col_key,           "Gjahr
                 024 sy-vline, 025(13) g_t_fmifiit_opay-fkbtr, "fkbtr
                 038 sy-vline, 039(24) g_t_fmifiit_opay-fipex, "fipex
                 063 sy-vline.
    endloop.
    uline (63).
  endif.

*----- Unterzahlungen
  describe table g_t_fmifiit_upay lines sy-tfill.
  if sy-tfill = 0.
    write /  text-003 color col_total.
  else.
    format color col_total intensified on.
    write: / text-004, sy-tfill.

    skip 1.
*----Spaltenüberschriften
    format color col_heading intensified on.
    uline (63).
    write at: /001 sy-vline, 002(5)  text-020 color col_key,
               007 sy-vline, 008(10) text-021 color col_key,
               018 sy-vline, 019(5)  text-022 color col_key,
               024 sy-vline, 025(13) text-023,
               038 sy-vline, 039(24) text-024,
               063 sy-vline.
    uline (63).
    format color col_normal intensified on.
    loop at g_t_fmifiit_upay.
      write at: /001 sy-vline, 002(5)  g_t_fmifiit_upay-bukrs
                                       color col_key,           "Bukrs
                 007 sy-vline, 008(10) g_t_fmifiit_upay-knbelnr
                                       color col_key,           "Belnr
                 018 sy-vline, 019(5)  g_t_fmifiit_upay-kngjahr
                                       color col_key,           "Gjahr
                 024 sy-vline, 025(13) g_t_fmifiit_upay-fkbtr,  "fkbtr
                 038 sy-vline, 039(24) g_t_fmifiit_upay-fipex,  "fipex
                 063 sy-vline.
    endloop.
    uline (63).
  endif.
endform.                    " write_wrong_payments
