FUNCTION zpm014_eqpt_getis .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0014
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0014
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND swerk EQ t_data-swerk
        AND equnr EQ t_data-equnr .

  ENDLOOP .


ENDFUNCTION.
