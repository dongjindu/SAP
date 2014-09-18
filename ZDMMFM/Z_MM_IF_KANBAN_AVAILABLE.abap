FUNCTION Z_MM_IF_KANBAN_AVAILABLE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"     VALUE(I_MATNR) TYPE  MATNR
*"     VALUE(I_PRVBE) TYPE  PRVBE OPTIONAL
*"  EXPORTING
*"     VALUE(E_COUNT) TYPE  INT4
*"----------------------------------------------------------------------

  clear e_count.
  data: l_cnt like sy-index.

*-check S2L Kanban
  select count( * ) into l_cnt
     from pkhd
     where matnr = i_matnr
       and werks = i_werks
       and rksta = 'I'.
  if sy-subrc = 0.
    e_count = 20.    "unlimit

  else.
*-- Manual Kanban
    SELECT count( * ) into e_count
      FROM PKHD
      inner join pkps
         on pkhd~pknum = pkps~pknum
     WHERE MATNR = i_MATNR
       AND WERKS = i_WERKS
       AND PKBST IN ('1','5','6').
  endif.

ENDFUNCTION.
