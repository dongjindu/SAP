FUNCTION zpm005_po_gr_getis .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZHKPMT0005
*"----------------------------------------------------------------------


*. Transfer result
  LOOP AT t_data .

    UPDATE zhkpmt0005
       SET zifflag   = t_data-zifflag
           zifresult = t_data-zifresult
      WHERE bukrs EQ t_data-bukrs
        AND werks EQ t_data-werks
        AND lgort EQ t_data-lgort
        AND matnr EQ t_data-matnr
        AND ebeln EQ t_data-ebeln
        AND ebelp EQ t_data-ebelp
        AND mblnr EQ t_data-mblnr .

  ENDLOOP .


ENDFUNCTION.
