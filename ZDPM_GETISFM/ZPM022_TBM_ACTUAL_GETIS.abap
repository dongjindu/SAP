FUNCTION zpm022_tbm_actual_getis .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0022
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0022
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND warpl EQ t_data-warpl
        AND abnum EQ t_data-abnum
        AND wapos EQ t_data-wapos
        AND aufnr EQ t_data-aufnr
        AND rsnum EQ t_data-rsnum
        AND rspos EQ t_data-rspos .

  ENDLOOP .


ENDFUNCTION.
