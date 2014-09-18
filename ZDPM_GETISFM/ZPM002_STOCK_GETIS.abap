FUNCTION ZPM002_STOCK_GETIS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0002
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0002
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND lgort EQ t_data-lgort
        AND matnr EQ t_data-matnr
        AND zcdate EQ t_data-zcdate .

  ENDLOOP .


ENDFUNCTION.
