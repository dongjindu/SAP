FUNCTION zpm004_other_gi_getis .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0004
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0004
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND lgort EQ t_data-lgort
        AND matnr EQ t_data-matnr
        AND mblnr EQ t_data-mblnr
        AND mjahr EQ t_data-mjahr
        AND zeile EQ t_data-zeile .

  ENDLOOP .


ENDFUNCTION.
