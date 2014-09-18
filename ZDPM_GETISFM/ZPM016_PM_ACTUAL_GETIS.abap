FUNCTION ZPM016_PM_ACTUAL_GETIS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0016
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0016
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND warpl EQ t_data-warpl
        AND abnum EQ t_data-abnum
        AND WAPOS EQ t_data-WAPOS .

  ENDLOOP .


ENDFUNCTION.
