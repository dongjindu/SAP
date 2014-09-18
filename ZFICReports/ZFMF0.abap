REPORT ZFMF0 .

Tables: FMIFIIT, BKPF, BSAK, BSAS, BSAD, T001.

data: i_fm   like fmifiit occurs 0 with header line,
      i_bsak like bsak    occurs 0 with header line,
      i_bsas like bsas    occurs 0 with header line,
      i_bsad like bsad    occurs 0 with header line.

data: g_augdt like bsak-augdt.
parameters: p_bukrs     like bkpf-bukrs  OBLIGATORY memory id BUK.
parameters: p_gjahr     like bkpf-gjahr  OBLIGATORY .
select-options: s_belnr for  bkpf-belnr.
parameters: p_log       as checkbox.
parameters: p_test      as checkbox,
            p_via       as checkbox.
* Only vendor...customer...
*
* after FM fiscal year change, payment date is determined
* from report run parameter
*
* GL is fine, it determine payment date from document.
*
* year: payment year
* belnr

data: g_fikrs    like t001-fikrs,
      g_err(1)   type c,
      g_list(1)  type c.

if p_log = 'X'.
  g_err  = ' '.
  g_list = 'X'.
else.
  g_err  = 'X'.
  g_list = ' '.
endif.

select single fikrs into g_fikrs from t001
  where bukrs = p_bukrs.
check sy-subrc = 0.

select * from fmifiit
   into table i_fm
   where fikrs  = g_fikrs
     and WRTTP  = '54'
     and payflg = space
     and gjahr  = p_gjahr
     and bukrs  = p_bukrs
     and knbelnr in s_belnr.


loop at i_fm.

  clear g_augdt.

* no multi-vendor...
  select single augdt into g_augdt
    from bsak
    where bukrs = i_fm-bukrs
      and belnr = i_fm-knBELNR
      and gjahr = i_fm-kngjahr.

  if g_augdt is initial.
    select single augdt into g_augdt
      from bsad
      where bukrs = i_fm-bukrs
        and belnr = i_fm-knBELNR
        and gjahr = i_fm-kngjahr.
  endif.

  if not g_augdt is initial.
    perform run_fmf0.

  endif.
endloop.
*&---------------------------------------------------------------------*
*&      Form  run_fmf0
*&---------------------------------------------------------------------*
FORM run_fmf0.
  ranges: r_belnr  for bkpf-belnr.

  refresh r_belnr.
  r_belnr-option = 'EQ'.
  r_belnr-sign   = 'I'.
  r_belnr-low    = i_fm-knbelnr.
  append r_belnr.

  if p_via = 'X'.
    SUBMIT RFFMS200
            WITH P_BUKRS   = i_fm-bukrs
            WITH P_GJAHR   = i_fm-kngjahr
            WITH P_ERR    = g_err
            WITH P_LISTE  = g_list
            WITH P_TEST   = p_test
            WITH P_PSTDAT = g_augdt
            WITH S_BELNR  in r_belnr
       VIA SELECTION-SCREEN
       and return.
  else.
    SUBMIT RFFMS200
          WITH P_BUKRS   = i_fm-bukrs
          WITH P_GJAHR   = i_fm-kngjahr
          WITH P_ERR    = g_err
          WITH P_LISTE  = g_list
          WITH P_TEST   = p_test
          WITH P_PSTDAT = g_augdt
          WITH S_BELNR  in r_belnr
     and return.
  endif.
ENDFORM.                                                    " run_fmf0
