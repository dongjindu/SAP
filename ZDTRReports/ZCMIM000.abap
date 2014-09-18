report ZCMIM000.
tables: fdi1, fdsb, fdsr, t036v, tzpa.
ranges: r_gsart for tzpa-gsart,
        r_ebene for t036-ebene.

* T036V ist zwar BK-abhängig, aber da die Ebenen selbst (z.B. der Kurz-
* text) nicht BK-abhängig sind, ist eine BK-Abgrenzung hier nicht
* zwingend notwendig.
*parameters:
*  p_bukrs like bkpf-bukrs memory id buk obligatory.

* Ermittlung der Produktarten, die zu Immobilien gehören
r_gsart-sign   = 'I'.
r_gsart-option = 'EQ'.
select * from tzpa
       where rantyp = '3'            "3-Mietvertrag - Immobilien
       or    rantyp = '8'            "8-Verwaltungsvertrag - Immobilien
       or    rantyp = '9'.           "9-Allgemeiner Vertrag - Immobilien
  r_gsart-low = tzpa-gsart.
  append r_gsart.
endselect.
check: sy-subrc = 0.

* Ermittlung der Ebenen, die zu diesen Produktarten gehören
r_ebene-sign   = 'I'.
r_ebene-option = 'EQ'.
select * from t036v
       where gsart in r_gsart.
  r_ebene-low = t036v-fdlevsk.
  if r_ebene-low <> space. collect r_ebene. endif.
  r_ebene-low = t036v-fdlevpk.
  if r_ebene-low <> space. collect r_ebene. endif.
  r_ebene-low = t036v-fdlevsk2.
  if r_ebene-low <> space. collect r_ebene. endif.
  r_ebene-low = t036v-fdlevpk2.
  if r_ebene-low <> space. collect r_ebene. endif.
endselect.

read table r_ebene index 1.
check: sy-subrc = 0.

delete from fdsb where ebene in r_ebene.
delete from fdsr where ebene in r_ebene.
delete from fdi1 where bukrs = space or bukrs <> space.
commit work.
submit rffdim00
       with p_test = space.
