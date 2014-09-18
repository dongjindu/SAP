FUNCTION Z_FPM_CAL_BREAKDOWN_TIME_MON.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_MONTH) LIKE  ZSPM_PARAM-ZMONTH
*"     REFERENCE(I_SHOP) LIKE  ZSPM_PARAM-SHOP OPTIONAL
*"     REFERENCE(I_MAUEH) LIKE  ZSPM_OPTIME-MAUEH
*"  TABLES
*"      T_TIME STRUCTURE  ZSPM_BDTIME
*"----------------------------------------------------------------------

  CLEAR: T_TIME, T_TIME[], IT_SHOP, IT_SHOP[].

  IF I_SHOP EQ SPACE.
    SELECT  DISTINCT INGRP AS SHOP
            INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
            FROM  T024I.
  ELSE.
    MOVE I_SHOP TO  IT_SHOP-SHOP.
    APPEND IT_SHOP.
  ENDIF.

  LOOP AT IT_SHOP.
    PERFORM GE_breakDOWN_TIME USING  T_TIME
                                    IT_SHOP-SHOP
                                    I_MONTH
                                    I_MAUEH.
    IF NOT T_time IS INITIAL.
      APPEND T_time.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
