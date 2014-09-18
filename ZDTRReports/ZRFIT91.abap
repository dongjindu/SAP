REPORT ZRFIT91 .


tables: ztfi_cmal, ztfi_fmal.

data: begin of it_cm occurs 0,
        datum like ztfi_cmal-datum,
        belnr like ztfi_cmal-belnr,
        dmshb like ztfi_cmal-dmshb,
      end of it_cm.
data: it_fm like it_cm occurs 0 with header line.

parameters: p_bukrs like ztfi_cmal-bukrs memory id BUK.
parameters: p_gjahr like ztfi_cmal-gjahr memory id GJR.
select-options: s_datum for ztfi_cmal-datum.

select datum belnr sum( dmshb ) into table it_cm
  from ztfi_cmal
  where bukrs = p_bukrs
    and gjahr = p_gjahr
    and datum in s_datum
    and not grupp like 'W%'
  group by datum belnr.

select datum belnr sum( dmshb ) into table it_fm
  from ztfi_fmal
  where bukrs = p_bukrs
    and gjahr = p_gjahr
    and datum in s_datum
  group by datum belnr.

sort it_fm by datum belnr.
data: l_idx like sy-tabix.

loop at it_cm.
  l_idx = sy-tabix.
  read table it_fm with key datum = it_cm-datum
                            belnr = it_cm-belnr
                   binary search.
  if sy-subrc = 0 and it_cm-dmshb = it_fm-dmshb.
    delete it_cm index l_idx.
  endif.
endloop.

ranges: r_datum for ztfi_cmal-datum.
ranges: r_belnr for ztfi_cmal-belnr.

loop at it_cm.
  refresh: r_datum, r_belnr.

  r_datum-sign = 'I'.
  r_datum-option = 'EQ'.
  r_datum-low    = it_cm-datum.
  append r_datum.

  r_belnr-sign = 'I'.
  r_belnr-option = 'EQ'.
  r_belnr-low = it_cm-belnr.
  append r_belnr.

  SUBMIT ZRFIT08
          WITH P_BUKRS = p_bukrs
          WITH P_GJAHR = p_gjahr
          WITH S_DATUM in r_datum
          WITH S_BELNR in r_belnr
       and return.

  write:/ 'Reconciled: ', it_cm-datum, it_cm-belnr.
endloop.
