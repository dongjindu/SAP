*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU00_READ                                              *
*----------------------------------------------------------------------*

*if single item costing...
describe table t_kis1 lines sy-index.
if sy-index = 0. append t_kis1. endif.

*get 1st level BOM
select idnrk maktx into corresponding fields of table lt_upg
  from stpo as s
  inner join makt as t
     on t~matnr = s~idnrk
    and t~spras = sy-langu
  where stlty = 'M'
    and stlnr =  f_ckiuser-stnum
    and datuv <= f_ckiuser-aldat.

check sy-subrc = 0.

include zxckau0z_ldc.
