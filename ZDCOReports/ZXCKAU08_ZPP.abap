*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU08_ZPP                                               *
*----------------------------------------------------------------------*
DATA: lv_frp like ztcou102-poper,
      lv_top like ztcou102-poper,
      lv_fr  like sy-datum,
      lv_to  like sy-datum.

*Effective quarterly ...
CALL FUNCTION 'Z_CO_GET_QUATER_START_END'
     EXPORTING
          TODAY      = f_matbw-bwdat
     IMPORTING
          START_DATE = lv_fr
          END_DATE   = lv_to.
lv_frp = lv_fr+4(2).
lv_top = lv_to+4(2).
data: lw_poper like ztcou102-poper.
select a~poper a~GRSPR a~peinh into (lw_poper, exp_preis, exp_peinh)
  from ztcou102 as a
  inner join tka02 as b
     on a~kokrs = b~kokrs
  inner join t001k as c
     on b~bukrs = c~bukrs
  where c~bwkey = f_matbw-werks
    and a~bdatj = f_matbw-bwdat(4)
    and a~poper between lv_frp and lv_top
    and a~matnr = f_matbw-MATNR
    and a~kalka = 'R1'
    and a~ver   = '0'
    and a~matnr = f_matbw-MATNR
  order by a~poper descending.

   exit.
endselect.

if sy-subrc = 0.
  EXP_WAERS = f_matbw-waers.
endif.
