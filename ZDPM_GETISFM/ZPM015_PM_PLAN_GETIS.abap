FUNCTION zpm015_pm_plan_getis .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0015
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0015
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND warpl EQ t_data-warpl
        AND abnum EQ t_data-abnum
        AND zaehl EQ t_data-zaehl .

  ENDLOOP .


ENDFUNCTION.
