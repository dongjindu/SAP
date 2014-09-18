FUNCTION ZPM018_PLAN_ORDER_GETIS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0018
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0018
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND warpl EQ t_data-warpl
        AND abnum EQ t_data-abnum
        AND WAPOS EQ t_data-WAPOS
        AND AUFNR EQ t_data-AUFNR
        AND APLZL EQ t_data-APLZL .

  ENDLOOP .


ENDFUNCTION.
