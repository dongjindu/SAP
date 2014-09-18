report ZCMTR000.
* Treasury und Wertpapiere werden hier gemeinsam behandelt, da dort
* die gleichen Ebenen verwendet werden können (z.B. beim Bond als
* Underlying)
tables: fdt1, fdw1, fdsb, fdsr, t036v, tzpa, trdir.
data:   tabname(4) type c.
ranges: r_gsart for tzpa-gsart,
        r_ebene for t036-ebene.

* T036V ist zwar BK-abhängig, aber da die Ebenen selbst (z.B. der Kurz-
* text) nicht BK-abhängig sind, ist eine BK-Abgrenzung hier nicht
* zwingend notwendig.
*parameters:
*  p_bukrs like bkpf-bukrs memory id buk obligatory.

* Ermittlung der Produktarten, die zu Treasury und zu Wertpapieren
* gehören
r_gsart-sign   = 'I'.
r_gsart-option = 'EQ'.
select * from tzpa
       where rantyp = '2'              "2-Wertpapiere
       or    rantyp = '4'              "4-Devisen
       or    rantyp = '5'              "5-Geldhandel
       or    rantyp = '6'.             "6-Derivate
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
delete from fdt1 where bukrs = space or bukrs <> space.
delete from fdw1 where bukrs = space or bukrs <> space.
* ab 470x200 gibt es die FDC1, die die FDW1 ablöst
select single * from trdir
       where name = 'RTPM_CM_UPDATE'.   "gibt es ab 470x200
if sy-subrc = 0.   "-> dann gibt es auch die FDC1
  tabname = 'FDC1'.
  delete from (tabname) where company_code =  space
                        or    company_code <> space.
endif.
commit work.

* Ausgabe über Spool, da sonst im Job-Log die Infos vom ersten Reports
* durch den zweiten überschrieben werden.
* Auch bei Online-Ausführung sinnvoll, da sonst der zweite Report erst
* losläuft, wenn das Protokoll vom ersten mit F3 verlassen wird.
if tabname <> space.                                        "ab 470x200
  submit rtpm_cm_update
         with p_test = space
         to sap-spool
         immediately space
         new list identification 'X'
         without spool dynpro
         and return.
else.   "bis 470x110
  submit rffdwp00
         with p_test = space
         to sap-spool
         immediately space
         new list identification 'X'
         without spool dynpro
         and return.
endif.

submit rffdtr00
       to sap-spool
       immediately space
       new list identification 'X'
       without spool dynpro
       and return.

write: / 'You got two Spool Requests - please check them !!!!'.
if sy-batch <> space.
  write: / 'Please check Job-Log, too'.
endif.
