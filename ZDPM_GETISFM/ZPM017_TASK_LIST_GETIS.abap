FUNCTION ZPM017_TASK_LIST_GETIS .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0017
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0017
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND ZFLEQ EQ t_data-ZFLEQ
        AND PLNTY EQ t_data-PLNTY
        AND PLNNR EQ t_data-PLNNR
        AND PLNAL EQ t_data-PLNAL
        AND PLNKN EQ t_data-PLNKN .

  ENDLOOP .


ENDFUNCTION.
