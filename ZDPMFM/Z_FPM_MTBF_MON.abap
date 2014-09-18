FUNCTION Z_FPM_MTBF_MON.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(E_ZMONTH) LIKE  ZTPM_MTBT-ZMONTH
*"     REFERENCE(E_SHOP) LIKE  ZTPM_MTBT-SHOP
*"  TABLES
*"      T_RATE STRUCTURE  ZTPM_MTBT OPTIONAL
*"----------------------------------------------------------------------
  CLEAR: WA_OPTIME, WA_SUM_BREAK, T_RATE.

*// 2011.08.05 changed by kim.yn
*  CONCATENATE E_ZMONTH'01' INTO WA_STR_DAY.
  CONCATENATE sy-datum(4) E_ZMONTH '01' INTO WA_STR_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_STR_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_END_DAY.

*  SELECT COUNT(*) INTO  WA_LINES
*           FROM  ZVPM_ANBD
*           WHERE SHOP   EQ  E_SHOP
*           AND   AUSVN  BETWEEN WA_STR_DAY AND WA_END_DAY
*           AND   QMART  IN  ('M1', 'M2', 'M3')
*           AND   LVORM  EQ  ' '.
*
*  SELECT  SUM( OPTIME ) INTO WA_OPTIME
*          FROM  ZTPM_OPTIME
*          WHERE AJAHR  = E_ZMONTH(4)
*          AND   ZMONTH = E_ZMONTH+4(2)
*          AND   SHOP   = E_SHOP.
*
  T_RATE-ZMTBT   = 'MTBF'.                          "// MTBF
  IF WA_LINES NE 0.
    T_RATE-AVRATE  = WA_OPTIME / WA_LINES.
  ENDIF.
  T_RATE-SHOP    = IT_SHOP-SHOP.
*// 2011.08.05 changed by kim.yn
  T_RATE-AJAHR   = WA_END_DAY(4).
*  T_RATE-AJAHR   = E_ZMONTH(4).
  T_RATE-ZMONTH  = E_ZMONTH.
  APPEND T_RATE.



ENDFUNCTION.
