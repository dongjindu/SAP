FUNCTION z_bi_get_eng_stock_by_part.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"  TABLES
*"      I_ENG STRUCTURE  ZSBW_ENGLIST1 OPTIONAL
*"      I_STOCK STRUCTURE  ZSBW_ENGSTCK1 OPTIONAL
*"----------------------------------------------------------------------

  REFRESH: i_stock.

  LOOP AT i_eng.

    SELECT * INTO CORRESPONDING FIELDS OF i_stock
    FROM mbew
    WHERE matnr = i_eng-matnr
      AND lbkum <> 0.

      i_stock-waers = 'USD'.
      i_stock-meins = 'EA'.

      APPEND i_stock.

    ENDSELECT.

  ENDLOOP.


ENDFUNCTION.
