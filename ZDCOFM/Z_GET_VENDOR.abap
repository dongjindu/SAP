FUNCTION z_get_vendor.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(MATNR) LIKE  EKBE-MATNR
*"     VALUE(LAST_DATE) LIKE  EKBE-BUDAT
*"  EXPORTING
*"     REFERENCE(LIFNR) LIKE  EKKO-LIFNR
*"----------------------------------------------------------------------
  DATA : l_budat TYPE budat.

data : begin of it_ekbe occurs 0,
       lifnr like ekko-lifnr,
       budat like ekbe-budat,
       end of it_ekbe.



  SELECT b~lifnr  MAX( a~budat ) INTO (lifnr, l_budat)
    FROM ekbe AS a
   INNER JOIN ekko AS b
      ON a~ebeln    =  b~ebeln
   WHERE a~matnr    =  matnr
     AND a~bewtp    =  'E'
     AND a~budat    <= last_date
     GROUP by b~lifnr.
  ENDSELECT.


ENDFUNCTION.
