report zcmmm001.
tables: fdm1, fdm2, fdsr, t036q.
data: banf-lev like t036-ebene.        "Banf
data: best-lev like t036-ebene.        "Bestellung
data: lief-lev like t036-ebene.        "Lieferplan(einteilungen)

select-options: s_bukrs for fdm1-bukrs.

start-of-selection.
  select single * from t036q where inteb = '001'.   "Banf
  if sy-subrc = 0. banf-lev = t036q-ebene. endif.

  select single * from t036q where inteb = '002'.   "Bestellung
  if sy-subrc = 0. best-lev = t036q-ebene. endif.

  select single * from t036q where inteb = '003'.   "Lieferplan
  if sy-subrc = 0. lief-lev = t036q-ebene. endif.

* Prüfen: Banfen haben andere Ebene als Bestellungen/Lieferpläne
  if banf-lev <> space.
    if banf-lev = best-lev
    or banf-lev = lief-lev.
      write: / 'Level for purchase requisitions must be unique !!!'.
      write: / 'Please change level in view V_T036o'.
      stop.
    endif.
  endif.

  check: best-lev <> space or lief-lev <> space.

  if best-lev <> space.
    delete from fdsr where bukrs in s_bukrs
                     and   ebene = best-lev.
  endif.

  if  lief-lev <> space
  and lief-lev <> best-lev.
    delete from fdsr where bukrs in s_bukrs
                     and   ebene = lief-lev.
  endif.

* in FDM1 stehen die Bestellungen und die Lieferplaneinteilungen
  delete from fdm1 where bukrs in s_bukrs.
  commit work.
* baut Bestellungen und Lieferplaneinteilungen neu auf
  submit rffdmm10 with s_bukrs in s_bukrs.
