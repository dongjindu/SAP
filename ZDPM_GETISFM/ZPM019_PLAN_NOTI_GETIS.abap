FUNCTION zpm019_plan_noti_getis .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0019
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0019
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND iwerk EQ t_data-iwerk
        AND warpl EQ t_data-warpl
        AND abnum EQ t_data-abnum
        AND wapos EQ t_data-wapos
        AND qmnum EQ t_data-qmnum .

  ENDLOOP .


ENDFUNCTION.
