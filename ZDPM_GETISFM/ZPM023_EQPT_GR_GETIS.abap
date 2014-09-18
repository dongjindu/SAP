FUNCTION zpm023_eqpt_gr_getis .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0023
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0023
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND equnr EQ t_data-equnr
        AND mblnr EQ t_data-mblnr
        AND mjahr EQ t_data-mjahr
        AND zeile EQ t_data-zeile .

  ENDLOOP .


ENDFUNCTION.
