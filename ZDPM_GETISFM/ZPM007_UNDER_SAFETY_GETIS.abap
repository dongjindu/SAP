FUNCTION ZPM007_UNDER_SAFETY_GETIS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0007
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0007
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND lgort EQ t_data-lgort
        AND matnr EQ t_data-matnr .

  ENDLOOP .


ENDFUNCTION.
