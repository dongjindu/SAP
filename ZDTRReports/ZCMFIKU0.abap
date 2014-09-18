report zcmfiku0.
tables: t036q, t036v, t037, t036, fdsb, fdsr, fdza, t001, skb1.
select-options: s_bukrs for fdsb-bukrs.

ranges: r_fdlev for fdsb-ebene,
        r_bukrs for fdsb-bukrs,
        r_saknr for skb1-saknr.
data: begin of t_report occurs 0,
        line(72),
      end   of t_report.
data: begin of t_bukrs occurs 0,
        bukrs like fdsb-bukrs,
      end   of t_bukrs.
data: list_id.
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
* delete relevant entries from FDSB, FDSR, FDZA.
loop at t_bukrs.
* determine clearing accounts of this company code
  perform fill_r_saknr.
  read table r_saknr index 1.
  if sy-subrc = 0.
    delete from fdsb where ebene in r_fdlev
                     and   bnkko in r_saknr
                     and   bukrs = t_bukrs.
    delete from fdsr where ebene in r_fdlev
                     and   bukrs = t_bukrs.
    delete from fdza where bukrs = t_bukrs
                     and   ( grupp <> space or bnkko in r_saknr ).
    delete from fdza where gbukr = t_bukrs
                     and   ggrup in r_saknr.
  else.
    delete from fdsr where ebene in r_fdlev
                     and   bukrs = t_bukrs.
    delete from fdza where bukrs = t_bukrs
                     and   grupp <> space.
  endif.
  commit work.
endloop.
list_id = 'X'.
* decide whether RFFDKU00 or RFFDKU20 is relevant
read report 'RFFDKU20' into t_report.
if sy-subrc = 0.
  free t_report.
  loop at t_bukrs.
    perform fill_r_saknr.
    refresh: r_bukrs.
    r_bukrs-sign   = 'I'.
    r_bukrs-option = 'EQ'.
    r_bukrs-low    = t_bukrs.
    append r_bukrs.
    submit rffdku20
           to sap-spool
              immediately space        "-> print not immediately
              keep in spool 'X'
              destination '????'
              new list identification list_id
              line-size 132
              cover page 'X'
              without spool dynpro
              with s_saknr in r_saknr
* only 1 company code to avoid problems with size of extract
              with s_bukrs in r_bukrs
              with p_test  = space
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
    submit rffdku00
           to sap-spool
              immediately space        "-> print not immediately
              keep in spool 'X'
              destination '????'
              new list identification list_id
              line-size 132
              cover page 'X'
              without spool dynpro
              with s_saknr in r_saknr
* only 1 company code to avoid problems with size of extract
              with s_bukrs in r_bukrs
              with p_test  = space
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
*       ........                                                      *
*---------------------------------------------------------------------*
form fill_r_saknr.
  refresh: r_saknr.
  r_saknr-sign   = 'I'.
  r_saknr-option = 'EQ'.
  select * from skb1 where bukrs = t_bukrs
                     and   fdlev <> space
                     and   xopvw <> space.
    r_saknr-low = skb1-saknr.
    append r_saknr.
  endselect.
endform.
