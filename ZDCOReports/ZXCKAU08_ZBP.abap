*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU08_ZBP                                               *
*----------------------------------------------------------------------*
* Note 1011897 - CK40N reference costing: materials missing in selection

*l_poper = f_matbw-bwdat+4(2).
*
**data: l_sobsl type sobsl.
**select single sobsl into l_sobsl from marc
**   where matnr = f_matbw-matnr and werks = f_matbw-werks.
**check l_sobsl = space.
*
*select single wertn peinh into (exp_preis, exp_peinh)
*  from ztcou102 as a
*  inner join tka02 as b
*     on a~kokrs = b~kokrs
*  inner join t001k as c
*     on b~bukrs = c~bukrs
*  where c~bwkey = f_matbw-werks
*    and a~bdatj = f_matbw-bwdat(4)
*    and a~poper = l_poper
*    and a~kalka = 'BP'
*    and a~ver   = '0'
*    and a~matnr = f_matbw-MATNR.
*
** EXP_PEINH = same with input
*if sy-subrc <> 0.
  select single GRSPR peinh into (exp_preis, exp_peinh)
    from ztcou102 as a
    inner join tka02 as b
       on a~kokrs = b~kokrs
    inner join t001k as c
       on b~bukrs = c~bukrs
    where c~bwkey = f_matbw-werks
      and a~bdatj = f_matbw-bwdat(4)
      and a~poper = '001'
      and a~kalka = 'BP'
      and a~ver   = '0'
      and a~matnr = f_matbw-MATNR.
*endif.

if sy-subrc = 0.
  EXP_WAERS = f_matbw-waers.
endif.
