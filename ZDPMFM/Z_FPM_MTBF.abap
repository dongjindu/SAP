FUNCTION Z_FPM_MTBF.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(E_AJAHR) LIKE  ZTPM_MTBT-AJAHR
*"     REFERENCE(E_SHOP) LIKE  ZTPM_MTBT-SHOP OPTIONAL
*"  TABLES
*"      T_RATE STRUCTURE  ZTPM_MTBT OPTIONAL
*"----------------------------------------------------------------------
   CLEAR: WA_OPTIME, WA_SUM_BREAK, T_RATE.

  CONCATENATE E_AJAHR '0101' INTO WA_STR_DAY.
  CONCATENATE E_AJAHR '1231' INTO WA_END_DAY.

*  SELECT COUNT(*) INTO  WA_LINES
*           FROM  ZVPM_ANBD
*           WHERE SHOP   EQ  E_SHOP
*           AND   AUSVN  BETWEEN WA_STR_DAY AND WA_END_DAY
*           AND   QMART  IN  ('M1', 'M2', 'M3')
*           AND   LVORM  EQ  ' '.
*
*    SELECT  SUM( OPTIME ) INTO WA_OPTIME
*            FROM  ZTPM_OPTIME
*            WHERE AJAHR = E_AJAHR
*            AND   SHOP  = E_SHOP.

    T_RATE-ZMTBT   = 'MTBF'.                          "// MTBF
    IF WA_LINES NE 0.
      T_RATE-AVRATE  = WA_OPTIME / WA_LINES.
    ENDIF.
    T_RATE-SHOP    = IT_SHOP-SHOP.
    T_RATE-AJAHR   = E_AJAHR.
    APPEND T_RATE.

  ENDFUNCTION.
