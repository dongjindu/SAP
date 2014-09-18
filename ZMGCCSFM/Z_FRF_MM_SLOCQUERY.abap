function z_frf_mm_slocquery.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(MATNR) TYPE  MATNR
*"     VALUE(WERKS) TYPE  WERKS_D
*"     VALUE(LGORT) TYPE  LGORT_D
*"  EXPORTING
*"     VALUE(MAT_OUT) TYPE  MATNR
*"     VALUE(QTY) TYPE  LABST
*"     VALUE(MEINS) TYPE  MEINS
*"  EXCEPTIONS
*"      INVALID_MATNR
*"----------------------------------------------------------------------

  select single meins into meins
      from mara where matnr eq matnr.
  if sy-subrc eq 0..
    mat_out = matnr.
    select single labst into qty
           from mard
           where matnr eq matnr
             and werks eq werks
             and lgort eq lgort ..
    if sy-subrc eq 0.
    else.
      qty = 0.
    endif.

  else.
    raise invalid_matnr.
  endif.



endfunction.
