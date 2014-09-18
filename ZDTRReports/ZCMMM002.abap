report ZCMMM002.
tables: fdm1, fdm2, fdsr, t036q, t001.
data: banf-lev like t036-ebene.        "Banf
data: best-lev like t036-ebene.        "Bestellung
data: lief-lev like t036-ebene.        "Lieferplan(einteilungen)
ranges: r_bukrs for t001-bukrs.

* RFFDMM20 braucht den BK -> Loop über r_bukrs liefert diesen
select-options: s_bukrs for t001-bukrs.

start-of-selection.
  select single * from t036q where inteb = '001'.   "Banf
  if sy-subrc = 0. banf-lev = t036q-ebene. endif.
  if banf-lev = space.
    write: / 'In view V_T036o no level is maintained for purchase req.'.
    stop.
  endif.

  select single * from t036q where inteb = '002'.   "Bestellung
  if sy-subrc = 0. best-lev = t036q-ebene. endif.

  select single * from t036q where inteb = '003'.   "Lieferplan
  if sy-subrc = 0. lief-lev = t036q-ebene. endif.

* Prüfen: Banfen haben andere Ebene als Bestellungen/Lieferpläne
  if banf-lev = best-lev
  or banf-lev = lief-lev.
    write: / 'Level for purchase requisitions must be unique !!!'.
    write: / 'Please change level in view V_T036o'.
    stop.
  endif.

* Range r_bukrs füllen
  r_bukrs-sign = 'I'. r_bukrs-option = 'EQ'.
  select * from t001 where bukrs in s_bukrs
                     and xfdis ne space and xfdmm ne space.
    r_bukrs-low = t001-bukrs. append r_bukrs.
  endselect.
  if sy-subrc ne 0.
    write: / 'No relevant company code found'.
    stop.
  endif.

  loop at r_bukrs.
    delete from fdsr where bukrs = r_bukrs-low and ebene = banf-lev.
*   in FDM2 stehen die Banfen
    delete from fdm2 where bukrs = r_bukrs-low.
    commit work.
*   Ausgabe über Spool, da sonst im Job-Log die Infos vom ersten Report-
*   aufruf durch den zweiten überschrieben werden.
*   Auch bei Online-Ausführung sinnvoll, da sonst der zweite Aufruf erst
*   losläuft, wenn das Protokoll vom ersten mit F3 verlassen wird.
    submit rffdmm20
           with p_bukrs = r_bukrs-low
           to sap-spool
           immediately space
           new list identification 'X'
           without spool dynpro
           and return.
  endloop.

  write: / 'Please check your Spool Request(s)'.
  if sy-batch <> space.
    write: / 'Please check Job-Log, too'.
  endif.
