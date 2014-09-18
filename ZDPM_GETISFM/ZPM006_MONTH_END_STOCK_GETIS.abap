FUNCTION zpm006_month_end_stock_getis .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0006
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0006
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND lgort EQ t_data-lgort
        AND matnr EQ t_data-matnr
        AND lfgja EQ t_data-lfgja
        AND lfmon EQ t_data-lfmon .

  ENDLOOP .


ENDFUNCTION.
