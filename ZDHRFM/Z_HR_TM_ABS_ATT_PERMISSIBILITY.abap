FUNCTION Z_HR_TM_ABS_ATT_PERMISSIBILITY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(PERNR) LIKE  P0001-PERNR
*"     REFERENCE(BEGDA) LIKE  P0001-BEGDA
*"     REFERENCE(ENDDA) LIKE  P0001-ENDDA
*"     REFERENCE(AWART) LIKE  P2001-AWART
*"  TABLES
*"      I0001 STRUCTURE  P0001
*"  EXCEPTIONS
*"      ATT_ABS_INV_FOR_PA_PSA
*"      ATT_ABS_INV_EE_SUB_GRP_STAT
*"      ATT_ABS_INV_PER_SCHED
*"      ATT_ABS_INV_DAY_SCHED
*"----------------------------------------------------------------------
  DATA: wa_zptimepm TYPE zptimepm,
        wa_1011  TYPE hrp1011,
        w_exception TYPE c.

  DATA: X0000 TYPE TABLE OF P0000 WITH HEADER LINE,
        X0001 TYPE TABLE OF P0001 WITH HEADER LINE,
        X0007 TYPE TABLE OF P0007 WITH HEADER LINE,
        W_PERIOD_WORK_SCHEDULE LIKE T551A-ZMODN,
        W_DAY_SCHEDULE         LIKE T551A-TPRG1.

  X0001[] = I0001[].

  CLEAR W_EXCEPTION.

  PROVIDE MASSN MASSG STAT1 STAT2 STAT3 FROM X0000
          PERSG PERSK BUKRS WERKS BTRTL
          KOSTL PLANS GSBER ABKRS VDSK1
          ANSVH ORGEH STELL FISTL GEBER FROM X0001
          SCHKZ                         FROM X0007
  BETWEEN BEGDA AND ENDDA.

    CASE X0001-PERSK.
      WHEN 'U2'.
*      CHECK AWART = '1020' OR
*            AWART = '1021' OR
*            AWART = '1023'.
*            CHECK BEGDA >= '20060717'.
        RAISE ATT_ABS_INV_FOR_PA_PSA.
    ENDCASE.

*THIS MAY NOT WORK UNLESS BEGDA ALWAYS = ENDDA

    SELECT SINGLE * FROM ZPTIMEPM INTO WA_ZPTIMEPM
*      WHERE PERSG  = X0001-PERSG
       where PERSK  = X0001-PERSK
       AND  SUBTY  = AWART
       AND  BEGDA <= BEGDA
       AND  ENDDA >= ENDDA.

    IF SY-SUBRC IS INITIAL.
      IF X0000-STAT2 = '3'.
        IF X0000-STAT1 = '1' OR
           X0000-STAT3 = '3'.

          SELECT SINGLE * FROM HRP1011 INTO WA_1011
            WHERE PLVAR = '01'
            AND OTYPE = 'S'
            AND OBJID = X0001-PLANS
            AND BEGDA <= BEGDA
            AND ENDDA >= ENDDA.

          IF SY-SUBRC IS INITIAL.

*            IF
*            WA_1101-ZZ_WKSIN = 'B' AND
*               X0007-SCHKZ(3) = 'OFF'.
*              IF WA_ZPTIMEPM-PMIND_FLEX_OFF = 'X'.
*              ELSE.
*                RAISE ATT_ABS-INV_EE_SUB_GRP_STAT.
*              ENDIF.
*            ELSE.

*              IF WA_ZPTIMEPM-PMIND = 'X'.
*              ELSE.
*                RAISE ATT_ABS-INV_EE_SUB_GRP_STAT.
*              ENDIF.
*            ELSE.
*
*              IF WA_ZPTIMEPM-PMIND = 'X'.
*              ELSE.
*                RAISE ATT_ABS-INV_EE_SUB_GRP_STAT.
          ENDIF.
        ENDIF.
      ENDIF.
*        ELSE.
*          RAISE ATT_ABS_INV_EE_SUB_GRP_STAT.
*          ENDIF.
*        ELSE.
*          RAISE ATT_ABS_INV_EE_SUB_GRP_STAT.
    ENDIF.
*
    CALL FUNCTION 'Z_HR-GET_PER_DAY_SCHEDULES'
         EXPORTING
              PERNR        = PERNR
              DATE         = ENDDA
         IMPORTING
              ZMODN        = W_PERIOD_WORK_SCHEDULE
              TPRGX        = W_DAY_SCHEDULE
         EXCEPTIONS
              NOT_FOUND    = 1
              IT0001_ERROR = 2
              IT0007_ERROR = 3
              T001P_ERROR  = 4
              T503_ERROR   = 5
              T508A_ERROR  = 6
              T508Z_ERROR  = 7
              T551A_ERROR  = 8
              OTHERS       = 9.
    IF
    SY-SUBRC
    <>
    0.
      RAISE ATT_ABS_INV_DAY_SCHED.
    ELSE.
      CASE AWART.
        WHEN '1020' OR '1021' OR '1022'.

          IF NOT W_DAY_SCHEDULE(3) = 'OFF'.

            RAISE ATT_ABS_INV_DAY_SCHED.
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDPROVIDE.

ENDFUNCTION.
