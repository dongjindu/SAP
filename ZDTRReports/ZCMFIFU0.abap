report zcmfifu0.
tables: t036q, t036v, t037, t036, fdsb, fdza, t001, skb1, bkpf.
select-options: s_bukrs for fdsb-bukrs.
parameters:     p_fdtag like fdsb-datum default sy-datum obligatory.

ranges: r_fdlev for fdsb-ebene,
        r_gjahr for bkpf-gjahr,
        r_bmon  for bkpf-monat,
        r_bukrs for fdsb-bukrs,
        r_saknr for skb1-saknr.
data: begin of t_report occurs 0,
        line(72),
      end   of t_report.
data: begin of t_bukrs occurs 0,
        bukrs like fdsb-bukrs,
      end   of t_bukrs.
data: list_id, found-flag.
* remove P_FDTAG from sceen if update can not been done on value date
at selection-screen output.
  read report 'RFFUEB00' into t_report.
  if sy-subrc = 0.
    loop at t_report.
      if t_report-line cs 'Valutagenaue Fortschreibung'.
        found-flag = '1'.
        exit.
      endif.
    endloop.
    if found-flag ne '1'.
      loop at screen.
        if screen-name cs 'P_FDTAG'.   "will not been shown on screen
          screen-input     = '0'.
          screen-invisible = '1'.
          modify screen.
        endif.
      endloop.
    endif.
    free t_report.
  endif.

start-of-selection.
* fill r_fdlev with all levels
r_fdlev-sign   = 'I'.
r_fdlev-option = 'EQ'.
select ebene from t036 into r_fdlev-low.
  append r_fdlev.
endselect.
* remove those levels which do not belong to FI
select ebene from t036q into t036q-ebene.
  read table r_fdlev with key low = t036q-ebene.
  if sy-subrc = 0.
    delete r_fdlev index sy-tabix.
  endif.
endselect.
select * from t036v.
  if t036v-fdlevsk <> space.
    read table r_fdlev with key low = t036v-fdlevsk.
    if sy-subrc = 0.
      delete r_fdlev index sy-tabix.
    endif.
  endif.
* level must be deleted because not relevant for FI
  if t036v-fdlevpk <> space.
    read table r_fdlev with key low = t036v-fdlevpk.
    if sy-subrc = 0.
      delete r_fdlev index sy-tabix.
    endif.
  endif.
  if t036v-fdlevsk2 <> space.
    read table r_fdlev with key low = t036v-fdlevsk2.
    if sy-subrc = 0.
      delete r_fdlev index sy-tabix.
    endif.
  endif.
  if t036v-fdlevpk2 <> space.
    read table r_fdlev with key low = t036v-fdlevpk2.
    if sy-subrc = 0.
      delete r_fdlev index sy-tabix.
    endif.
  endif.
endselect.
select ebene from t037 into t037-ebene.
  read table r_fdlev with key low = t037-ebene.
  if sy-subrc = 0.
    delete r_fdlev index sy-tabix.
  endif.
endselect.

read table r_fdlev index 1.
check: sy-subrc = 0.
select bukrs from t001 into t_bukrs-bukrs where xfdis <> space
                                          and   bukrs in s_bukrs.
  append t_bukrs.
endselect.
* delete relevant entries from FDSB, FDZA.
loop at t_bukrs.
* determine bank accounts of this company code
  perform fill_r_saknr.
  read table r_saknr index 1.
  check sy-subrc = 0.

  delete from fdsb where ebene in r_fdlev
                   and   bnkko in r_saknr
                   and   bukrs = t_bukrs.
  delete from fdza where bukrs = t_bukrs
                   and   bnkko in r_saknr.
  delete from fdza where gbukr = t_bukrs
                   and   ggrup in r_saknr.
  commit work.
endloop.
* business year
* (will be ignored with 45B Sup.Pack.35, 46B-22, 46C-11 -> note 150047)
call function 'BUILD_DEFAULT_YEAR'
  tables
    xgjahr = r_gjahr.
* relevant months: 1 bis 16
* (will be ignored with 45B Sup.Pack.35, 46B-22, 46C-11 -> note 150047;
* SD_STIDA will also be ignored)
call function 'BUILD_DEFAULT_PERIOD'
  tables
    xmonat = r_bmon.
list_id = 'X'.
* decide whether RFFUEB00 or RFFUEB20 is relevant
* (since 4.5A it is RFFUEB00 again)
read report 'RFFUEB20' into t_report.
if sy-subrc = 0.
  free t_report.
  loop at t_bukrs.
    perform fill_r_saknr.
    refresh: r_bukrs.
    r_bukrs-sign   = 'I'.
    r_bukrs-option = 'EQ'.
    r_bukrs-low    = t_bukrs.
    append r_bukrs.
    submit rffueb20
           to sap-spool
              immediately space        "-> print not immediately
              keep in spool 'X'
              destination '????'
              new list identification list_id
              line-size 132
              cover page 'X'
              without spool dynpro
         with sd_saknr in r_saknr
         with sd_bukrs in r_bukrs
         with sd_gjahr in r_gjahr
         with sd_stida = sy-datum
         with s_bmon   in r_bmon
         with p_fdtag  = sy-datum
         with p_test   = space
    and return.
    clear list_id.
  endloop.
  if sy-subrc = 0.
    write: / 'Please check your SAP-SPOOL  !!!!!!!!!!!'.
  endif.
else.
  loop at t_bukrs.
    perform fill_r_saknr.
    refresh: r_bukrs.
    r_bukrs-sign   = 'I'.
    r_bukrs-option = 'EQ'.
    r_bukrs-low    = t_bukrs.
    append r_bukrs.
    submit rffueb00
           to sap-spool
              immediately space        "-> print not immediately
              keep in spool 'X'
              destination '????'
              new list identification list_id
              line-size 132
              cover page 'X'
              without spool dynpro
         with sd_saknr in r_saknr
         with sd_bukrs in r_bukrs
         with sd_gjahr in r_gjahr  "see docu above
         with sd_stida = sy-datum  "see docu above
         with s_bmon   in r_bmon   "see docu above
         with p_fdtag  = p_fdtag
         with p_test   = space
    and return.
    clear list_id.
  endloop.
  if sy-subrc = 0.
    write: / 'Please check your SAP-SPOOL  !!!!!!!!!!!'.
  endif.
endif.

*---------------------------------------------------------------------*
*       FORM FILL_R_SAKNR                                             *
*---------------------------------------------------------------------*
form fill_r_saknr.
  refresh: r_saknr.
  r_saknr-sign   = 'I'.
  r_saknr-option = 'EQ'.
  select * from skb1 where bukrs = t_bukrs
                     and   fdlev <> space
                     and   xopvw =  space.
    r_saknr-low = skb1-saknr.
    append r_saknr.
  endselect.
endform.                    "FILL_R_SAKNR
