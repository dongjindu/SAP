*----------------------------------------------------------------------*
*   INCLUDE MZAHR0011F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
FORM SELECT_MAIN_DATA.
  DATA: L_ZCODE LIKE ZTHR_PCP02-ZCODE.
*
  CLEAR IT_MAINT. REFRESH IT_MAINT.
*
  CLEAR ZTHR_PCP00.
  SELECT ZOBJC ZSENR ZHEDC ANCUR
    INTO (ZTHR_PCP00-ZOBJC, ZTHR_PCP00-ZSENR,
          ZTHR_PCP00-ZHEDC, ZTHR_PCP00-ANCUR)
    FROM ZTHR_PCP00 WHERE ZYEAR = W_ZYEAR
                      AND ZMONS = W_ZMONS
                      AND ZVERS = W_ZVERS
                      AND ZCOST = W_KOSTL
                      AND ZOBJC IN R_ZJOBC.
    IT_MAINT-ZOBJC = ZTHR_PCP00-ZOBJC.
    IT_MAINT-ZSENR = ZTHR_PCP00-ZSENR.
    IT_MAINT-ZHEDC = ZTHR_PCP00-ZHEDC.
    COLLECT IT_MAINT. CLEAR IT_MAINT.
  ENDSELECT.
*... pay increase ratio
  IF SY-DATUM+4(2) >= 1 AND SY-DATUM+4(2) <= 6.
    L_ZCODE = '10000'.
  ELSE.
    L_ZCODE = '10010'.
  ENDIF.
*
  CLEAR ZTHR_PCP02.
  SELECT SINGLE ZVAL1 INTO ZTHR_PCP02-ZVAL1
    FROM ZTHR_PCP02 WHERE ZMODL = '02'
                      AND ZGRUP = '1060'
                      AND ZCODE = L_ZCODE.
*
  LOOP AT IT_MAINT.
    IT_MAINT-ZVAL1 = ZTHR_PCP02-ZVAL1.

    CLEAR HRP1000.
    SELECT SINGLE SHORT INTO HRP1000-SHORT
      FROM HRP1000 WHERE PLVAR = '01'
                     AND OTYPE = 'C'
                     AND OBJID = IT_MAINT-ZOBJC
                     AND ISTAT = '1'
                     AND ENDDA = '99991231'
                     AND LANGU = SY-LANGU.
    IT_MAINT-ZOBJT = HRP1000-SHORT.
    MODIFY IT_MAINT. CLEAR IT_MAINT.
  ENDLOOP.
ENDFORM.                    " SELECT_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  COPY_DATA_BY_JOB
*&---------------------------------------------------------------------*
FORM COPY_DATA_BY_JOB.
  DATA: L_COUNT     LIKE ZTHR_PCP05-ZSEQN,
        L_FORMS(10),
        L_ZHEDC     LIKE ZTHR_PCP00-ZHEDC.
*
  CASE W_ZGRUP.
    WHEN '1040'. L_COUNT = 1.
    WHEN '1050'. L_COUNT = 0.
  ENDCASE.
*
  CLEAR IT_COPYT. REFRESH IT_COPYT.
*
  DO 5 TIMES.
    L_COUNT = L_COUNT + 1.
    IF L_COUNT > 5. EXIT. ENDIF.
    CLEAR ZTHR_PCP05.
*    SELECT SINGLE ZWKTM INTO ZTHR_PCP05-ZWKTM
     SELECT SINGLE ZHEDC ZTOTM
            INTO (ZTHR_PCP05-ZHEDC, ZTHR_PCP05-ZTOTM)
      FROM ZTHR_PCP05 WHERE ZSCST = W_KOSTL
                        AND ZYEAR = W_ZYEAR
                        AND ZMONS = W_ZMONS
                        AND ZVERS = W_ZVERS
                        AND ZGRUP = W_ZGRUP
                        AND ZSEQN = L_COUNT.
    CASE L_COUNT.
      WHEN 1. L_FORMS = 'Weekday'.
      WHEN 2. L_FORMS = 'Overtime'.
      WHEN 3. L_FORMS = 'Saturday'.
      WHEN 4. L_FORMS = 'Sunday'.
      WHEN 5. L_FORMS = 'Holiday'.
    ENDCASE.

    CLEAR L_ZHEDC.
    SORT IT_MAINT BY ZOBJC.
    LOOP AT IT_MAINT.
      IT_COPYT-ZSEQN = L_COUNT.
      IT_COPYT-FORMS = L_FORMS.
      IT_COPYT-ZOBJC = IT_MAINT-ZOBJC.
      IT_COPYT-ZOBJT = IT_MAINT-ZOBJT.
      IT_COPYT-ZSENR = IT_MAINT-ZSENR.
      IT_COPYT-ZHEDC = IT_MAINT-ZHEDC.
*      IT_COPYT-ZWKTM = ZTHR_PCP05-ZWKTM.
*  WORK HOURS =  Monthly working hrs / Head Count *
*                        H.C by Seniority
      IT_COPYT-ZWKTM = ( ZTHR_PCP05-ZTOTM / ZTHR_PCP05-ZHEDC ) *
                        IT_COPYT-ZHEDC .
      IT_COPYT-ZVAL1 = IT_MAINT-ZVAL1.
      IT_COPYT-HOURS = IT_MAINT-HOURS.
      L_ZHEDC = L_ZHEDC + IT_MAINT-ZHEDC.
*     Working hours  * HOURS PAY * RATE( 0.4 OR 0.5 )
      IT_COPYT-AMUNT = IT_COPYT-HOURS * IT_COPYT-ZVAL1 * IT_COPYT-ZWKTM.
      AT END OF ZOBJC.
        IT_COPYT-ZHEDT = L_ZHEDC.
        CLEAR L_ZHEDC.
      ENDAT.
      APPEND IT_COPYT. CLEAR IT_COPYT.
    ENDLOOP.
  ENDDO.
*
  SORT IT_COPYT BY ZSEQN ZOBJC.
  REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
  DESCRIBE TABLE IT_COPYT LINES TC9000-LINES.
ENDFORM.                    " COPY_DATA_BY_JOB
*&---------------------------------------------------------------------*
*&      Form  GET_HOURLY_SALARY
*&---------------------------------------------------------------------*
FORM GET_HOURLY_SALARY.
  DATA : W_ZSALY LIKE ZTHR_PCPXX-ZSALY.

  CLEAR IT_PCPXX. REFRESH IT_PCPXX.
  SORT IT_MAINT BY ZOBJC.
*
  LOOP AT IT_MAINT.
    CLEAR W_ZSALY.
    CLEAR ZTHR_PCPXX.
    SELECT PERNR  ZSALY
             INTO (ZTHR_PCPXX-PERNR, ZTHR_PCPXX-ZSALY)
      FROM ZTHR_PCPXX WHERE ZYEAR = W_ZYEAR
                        AND ZVERS = W_ZVERS
                        AND ZCOST = W_KOSTL
                        AND ZOBJC = IT_MAINT-ZOBJC
                        AND ZSENR = IT_MAINT-ZSENR.
      IT_PCPXX-PERNR = ZTHR_PCPXX-PERNR.
      IT_PCPXX-ZOBJC = IT_MAINT-ZOBJC.
      IT_PCPXX-ZSENR = IT_MAINT-ZSENR.
      IT_PCPXX-ZSALY = ZTHR_PCPXX-ZSALY.
      W_ZSALY        = W_ZSALY + IT_PCPXX-ZSALY.
      APPEND IT_PCPXX. CLEAR IT_PCPXX.
    ENDSELECT.
*     Salary/hourly = Monthly salary / Monthly working hours (173.33 h)
      IT_MAINT-HOURS = W_ZSALY / ( 17333 / 100 ) .
      MODIFY IT_MAINT.
   ENDLOOP.
*
*  LOOP AT IT_PCPXX.
*
*
*  ENDLOOP.
ENDFORM.                    " GET_HOURLY_SALARY
*&---------------------------------------------------------------------*
*&      Form  DATA_SAVE_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DATA_SAVE_PERIOD.

endform.                    " DATA_SAVE_PERIOD
*&---------------------------------------------------------------------*
*&      Form  DATA_SAVE_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DATA_SAVE_YEAR.

endform.                    " DATA_SAVE_YEAR
