FUNCTION Z_FPM_MAINTENANCE_RATIO.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_SHOP) LIKE  ZSPM_PARAM-SHOP
*"     REFERENCE(I_AJAHR) LIKE  ZSPM_PARAM-AJAHR
*"     REFERENCE(I_ZMONTH) LIKE  ZSPM_PARAM-MONTH
*"  TABLES
*"      T_PMRO STRUCTURE  ZTPM_PMRO
*"      T_ALL_AFVGD STRUCTURE  ZSPM_PMRO_B OPTIONAL
*"      T_AUART STRUCTURE  ZSPM_AUART OPTIONAL
* Date        Developer   Request       Description
* 03/21/2007  Manju       UD1K940146    Increased length of ZPTIME from
*                                       7 to 15 to avoid ABAP Short dump
*"----------------------------------------------------------------------
  DATA: DAUER_CX     TYPE F,
        WA_SUM_DAUER TYPE F.

  DATA: WA_FIRST_DAY LIKE SY-DATUM,
        WA_LAST_DAY LIKE SY-DATUM.

  CLEAR : T_PMRO, T_PMRO[].
  CLEAR : IT_TEMP, IT_TEMP[].
  CLEAR : T_ALL_AFVGD, T_ALL_AFVGD[].

*** Set Select condition
  CONCATENATE I_AJAHR I_ZMONTH '01' INTO WA_FIRST_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_FIRST_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_LAST_DAY.

**** Set order type...
  IF T_AUART IS INITIAL.
    MOVE 'PM01' TO T_AUART-AUART.
    APPEND T_AUART.
    MOVE 'PM02' TO T_AUART-AUART.
    APPEND T_AUART.
    MOVE 'PM03' TO T_AUART-AUART.
    APPEND T_AUART.
    MOVE 'PM04' TO T_AUART-AUART.
    APPEND T_AUART.
    MOVE 'PM05' TO T_AUART-AUART.
    APPEND T_AUART.
  ENDIF.


  LOOP AT T_AUART.
**** Plan Number
    SELECT SINGLE COUNT( * )
           INTO IT_TEMP-ZPLAND              "//Plan Number
           FROM CAUFV AS A
                INNER JOIN VIQMEL AS B
                ON A~AUFNR = B~AUFNR
           WHERE A~AUART = T_AUART-AUART
           AND   A~GSTRS BETWEEN WA_FIRST_DAY AND WA_LAST_DAY
           AND   B~INGRP = I_SHOP.

**** Actual Number
    SELECT SINGLE COUNT( * )
           INTO IT_TEMP-ZACTAL               "//Actual Number
           FROM CAUFV AS A
                INNER JOIN AFRU AS B
                ON A~AUFNR = B~AUFNR
                   INNER JOIN VIQMEL AS C
                     ON B~AUFNR = C~AUFNR
           WHERE A~AUART = T_AUART-AUART
           AND   A~LOEKZ EQ ' '
           AND   B~VORNR = '0010'
           AND   B~IEDD BETWEEN WA_FIRST_DAY AND WA_LAST_DAY
           AND   B~AUERU EQ 'X'
           AND   B~STOKZ EQ ' '
           AND   B~STZHL EQ SPACE
           AND   C~INGRP = I_SHOP.


**** Actual working time
    SELECT A~AUFNR B~VORNR A~KAPVERSA B~ARBID B~IEDD B~ISDD
           B~IEDZ B~ISDZ C~BEAZE C~KALID
           INTO CORRESPONDING FIELDS OF TABLE IT_AFVGD
           FROM CAUFV AS A
                INNER JOIN AFRU AS B
                ON A~AUFNR = B~AUFNR
                   INNER JOIN VIAFVC AS C
                   ON  B~AUFPL = C~AUFPL
                   AND B~APLZL = C~APLZL
                     INNER JOIN VIQMEL AS D
                     ON B~AUFNR = D~AUFNR
           WHERE A~AUART = T_AUART-AUART
           AND   A~LOEKZ EQ ' '
           AND   B~IEDD BETWEEN WA_FIRST_DAY AND WA_LAST_DAY
           AND   B~AUERU EQ 'X'
           AND   B~STOKZ EQ ' '
           AND   B~STZHL EQ SPACE
           AND   D~INGRP = I_SHOP.
    IF SY-SUBRC EQ 0.

      LOOP AT IT_AFVGD.
**** Scheduling using operating time
        CALL FUNCTION 'CX_SCHED_VIA_OPERATING_TIME'
             EXPORTING
                  I_ARBID      = IT_AFVGD-ARBID
                  I_DATE_END   = IT_AFVGD-IEDD
                  I_DATE_START = IT_AFVGD-ISDD
                  I_FCALID     = IT_AFVGD-KALID
                  I_TIME_END   = IT_AFVGD-IEDZ
                  I_TIME_START = IT_AFVGD-ISDZ
                  I_UNIT       = IT_AFVGD-BEAZE
                  I_VERSA      = IT_AFVGD-KAPVERSA
             IMPORTING
                  E_DURATION   = DAUER_CX.
        IF DAUER_CX < 0.
          DAUER_CX = 0.
        ELSE.
          MOVE-CORRESPONDING IT_AFVGD TO T_ALL_AFVGD.
          MOVE 'MIN' TO T_ALL_AFVGD-BEAZE.
          IF IT_AFVGD-BEAZE NE SPACE AND DAUER_CX NE SPACE.
            CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
                 EXPORTING
                      CHAR_UNIT       = T_ALL_AFVGD-BEAZE
                      DECIMALS        = 0
                      EXPONENT        = 0
                      FLTP_VALUE_SI   = DAUER_CX
                      INDICATOR_VALUE = 'X'
                      MASC_SYMBOL     = ' '
                 IMPORTING
                      CHAR_VALUE      = T_ALL_AFVGD-ZPTIME.
          ENDIF.

*          WRITE  DAUER_CX UNIT IT_AFVGD-BEAZE
*                 TO T_ALL_AFVGD-ZPTIME_B EXPONENT 0.
          MOVE :  I_SHOP         TO T_ALL_AFVGD-SHOP,
                  I_AJAHR        TO T_ALL_AFVGD-AJAHR,
                  I_ZMONTH       TO T_ALL_AFVGD-ZMONTH,
                  T_AUART-AUART  TO T_ALL_AFVGD-AUART.
          APPEND T_ALL_AFVGD.

          WA_SUM_DAUER = WA_SUM_DAUER  + DAUER_CX.
          CLEAR DAUER_CX.
        ENDIF.

      ENDLOOP.
*      WRITE WA_SUM_DAUER  UNIT IT_AFVGD-BEAZE
*            TO IT_TEMP-ZPTIME_B EXPONENT 0.
      IT_TEMP-ZPTIME = WA_SUM_DAUER. "//Actual Working Time
      CLEAR: WA_SUM_DAUER.
    ENDIF.

    MOVE :  'MIN'          TO IT_TEMP-MEINS,
            I_SHOP         TO IT_TEMP-SHOP,
            I_AJAHR        TO IT_TEMP-AJAHR,
            I_ZMONTH       TO IT_TEMP-ZMONTH,
            T_AUART-AUART  TO IT_TEMP-AUART.
    APPEND IT_TEMP.
    CLEAR : IT_TEMP.
  ENDLOOP.

*** CONVERT 'PM01', 'PM04', 'PM05' TO 'PM01'
  LOOP AT IT_TEMP.
    IF IT_TEMP-AUART = 'PM01' OR
       IT_TEMP-AUART = 'PM04' OR
       IT_TEMP-AUART = 'PM05'.
      MOVE 'PM01' TO IT_TEMP-AUART.
    ENDIF.
**** Accumulate Values by Order Type and period...
    MOVE-CORRESPONDING  IT_TEMP TO T_PMRO.
*    WRITE IT_TEMP-ZPTIME  UNIT IT_TEMP-MEINS
*                          TO   T_PMRO-ZPTIME_B EXPONENT 0.
*    IF IT_TEMP-MEINS NE SPACE AND IT_TEMP-ZPTIME NE SPACE.
*      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
*           EXPORTING
*                CHAR_UNIT       = IT_TEMP-MEINS
*                DECIMALS        = 0
*                EXPONENT        = 0
*                FLTP_VALUE_SI   = IT_TEMP-ZPTIME
*                INDICATOR_VALUE = 'X'
*                MASC_SYMBOL     = ' '
*           IMPORTING
*                CHAR_VALUE      = T_PMRO-ZPTIME_B.
*    ENDIF.
    COLLECT T_PMRO.
  ENDLOOP.

ENDFUNCTION.
