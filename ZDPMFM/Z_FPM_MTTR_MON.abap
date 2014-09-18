FUNCTION Z_FPM_MTTR_MON.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(E_ZMONTH) LIKE  ZTPM_MTBT-ZMONTH
*"     REFERENCE(E_SHOP) LIKE  ZTPM_MTBT-SHOP OPTIONAL
*"  TABLES
*"      T_RATE STRUCTURE  ZTPM_MTBT OPTIONAL
*"----------------------------------------------------------------------
  CLEAR: WA_OPTIME, WA_SUM_BREAK.

*// 2011.08.05 changed by kim.yn
*  CONCATENATE E_ZMONTH '01' INTO WA_STR_DAY.
  CONCATENATE sy-datum(4) E_ZMONTH '01' INTO WA_STR_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_STR_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_END_DAY.

*  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_MTBT
*           FROM  ZVPM_ANBD
*           WHERE SHOP   EQ  E_SHOP
*           AND   AUSVN  BETWEEN WA_STR_DAY AND WA_END_DAY
*           AND ( QMART  EQ 'M1' OR
*                 QMART  EQ 'M2' OR
*                 QMART  EQ 'M3').

  DESCRIBE TABLE IT_MTBT LINES WA_LINES.

  LOOP AT IT_MTBT.
    CLEAR: WA_INTERVAL.
    IF NOT IT_MTBT-AUSVN IS INITIAL AND
       NOT IT_MTBT-AUSBS IS INITIAL.
      CALL FUNCTION 'Z_FCA_GET_TIME_INTERVAL'
           EXPORTING
                S_DATE   = IT_MTBT-AUSVN
                S_TIME   = IT_MTBT-AUZTV
                E_DATE   = IT_MTBT-AUSBS
                E_TIME   = IT_MTBT-AUZTB
           IMPORTING
                INTERVAL = WA_INTERVAL.
    ENDIF.
    WA_SUM_BREAK = WA_SUM_BREAK + WA_INTERVAL.
  ENDLOOP.

  CLEAR: T_RATE.
  T_RATE-ZMTBT   = 'MTTR'.                          "// MTTR
  IF WA_LINES NE 0.
    T_RATE-AVRATE  = WA_SUM_BREAK / WA_LINES.
  ENDIF.
  T_RATE-SHOP     = E_SHOP.
*// 2011.08.05 changed by kim.yn
  T_RATE-AJAHR   = WA_END_DAY(4).
*  T_RATE-AJAHR   = E_ZMONTH(4).
  T_RATE-ZMONTH   = E_ZMONTH.
  APPEND T_RATE.

  CLEAR: WA_OPTIME, WA_SUM_BREAK, T_RATE.

ENDFUNCTION.
