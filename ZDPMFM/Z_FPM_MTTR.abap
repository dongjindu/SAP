FUNCTION Z_FPM_MTTR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(E_AJAHR) LIKE  ZTPM_MTBT-AJAHR
*"     REFERENCE(E_SHOP) LIKE  ZTPM_MTBT-SHOP OPTIONAL
*"  TABLES
*"      T_RATE STRUCTURE  ZTPM_ANMT OPTIONAL
*"----------------------------------------------------------------------
  CLEAR: WA_OPTIME, WA_SUM_BREAK.

  CONCATENATE E_AJAHR '0101' INTO WA_STR_DAY.
  CONCATENATE E_AJAHR '1231' INTO WA_END_DAY.

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
  T_RATE-SHOP    = E_SHOP.
  T_RATE-AJAHR   = E_AJAHR.
  APPEND T_RATE.

ENDFUNCTION.
