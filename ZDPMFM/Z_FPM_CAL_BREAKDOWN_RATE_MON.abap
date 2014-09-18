FUNCTION Z_FPM_CAL_BREAKDOWN_RATE_MON.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_MONTH) LIKE  ZSPM_PARAM-ZMONTH
*"     REFERENCE(I_PLANT) LIKE  ZSPM_PARAM-SWERK OPTIONAL
*"     REFERENCE(I_SHOP) LIKE  ZSPM_PARAM-SHOP OPTIONAL
*"     REFERENCE(I_MAUEH) LIKE  ZSPM_OPTIME-MAUEH
*"  TABLES
*"      T_RATE STRUCTURE  ZSPM_BDMON
*"----------------------------------------------------------------------

  CLEAR: T_RATE, T_RATE[], IT_SHOP, IT_SHOP[].

  IF I_SHOP EQ SPACE.
*    SELECT BEBER INTO  CORRESPONDING FIELDS OF TABLE IT_SHOP
*                 FROM  T357.
    SELECT  DISTINCT INGRP AS SHOP
            INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
            FROM  T024I.
  ELSE.
    MOVE I_SHOP TO  IT_SHOP-SHOP.
    APPEND IT_SHOP.
  ENDIF.

  LOOP AT IT_SHOP.
    PERFORM CAL_MONTH_RATE USING  T_RATE
                                  I_PLANT
                                  IT_SHOP-SHOP
                                  I_MONTH
                                  I_MAUEH.
    IF NOT T_RATE IS INITIAL.
      APPEND T_RATE.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
