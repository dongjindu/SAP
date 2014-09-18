*----------------------------------------------------------------------*
*   INCLUDE MZAHR0009F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
FORM make_basic_data.
  DATA: l_begda     LIKE sy-datum,
        l_years     TYPE i,
        l_count(2)  TYPE n,
        l_zval1     TYPE p DECIMALS 2,
        l_entry     LIKE hida OCCURS 1 WITH HEADER LINE.
  DATA : wa_answer TYPE c,
         wa_title(50) TYPE c.
  TABLES : pa0000.
  IF w_xyear IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Input Source year'.
  ENDIF.
*
  IF w_zyear = space OR w_zvers = space.
    MESSAGE w001 WITH 'Please make a selection'.
    EXIT.
  ENDIF.
*
  CLEAR it_pcpxx. REFRESH it_pcpxx.
  CLEAR it_pcp00. REFRESH it_pcp00.
  CLEAR it_cpy00. REFRESH it_cpy00.
  CLEAR it_calsl. REFRESH it_calsl.
*
  SELECT SINGLE * FROM zthr_pcp00
                  WHERE zyear = w_zyear
                    AND zvers = w_zvers.
*Caution: Basic data will be deleted for selected version !

  IF sy-subrc EQ 0.
    wa_title = 'Caution: Basic and input data will be deleted  !'.
    PERFORM pop_up_message USING wa_title
                           CHANGING wa_answer  .
    IF wa_answer <> 'J'.
      EXIT.
    ELSE.
      PERFORM data_delete_history .
    ENDIF.
  ENDIF.

*
  w_ptext = '10% processing...........'.
*  DO 5 TIMES.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 10
            text       = w_ptext.
*  ENDDO.
*
  PERFORM get_401k_wage_type.
  PERFORM get_closing_date.
  PERFORM get_ee_group.
  PERFORM job_code_selection_check.
  PERFORM job_paid-leave-up-to-manager.
  PERFORM pay_increse_rate.
*
  CLEAR pa0001.
  SELECT pernr werks persg persk kostl stell abkrs
    INTO (pa0001-pernr, pa0001-werks, pa0001-persg,
          pa0001-persk, pa0001-kostl, pa0001-stell,
          pa0001-abkrs)
    FROM pa0001 WHERE endda = '99991231'
                  AND persg IN r_persg .

    SELECT SINGLE * FROM pa0000
                    WHERE pernr = pa0001-pernr
                      AND massn IN ('Z5','Z7','Z8','ZE','ZH','ZI',
                                    'ZW','ZX','ZY').
    CHECK sy-subrc NE 0.

    IF pa0001-persg <> '2'.
      it_cpy00-zyear = w_zyear.
      it_cpy00-zvers = w_zvers.
      it_cpy00-zcost = pa0001-kostl.
      it_cpy00-zpera = pa0001-werks.
      it_cpy00-zperg = pa0001-persg.
      it_cpy00-zsubg = pa0001-persk.
      it_cpy00-zobjc = pa0001-stell.

      CLEAR: l_entry, l_entry[], l_begda, l_years.
* be hired Date
      CALL FUNCTION 'HR_ENTRY_DATE'
           EXPORTING
                persnr      = pa0001-pernr
           IMPORTING
                entrydate   = l_begda
           TABLES
                entry_dates = l_entry.

      PERFORM original_hire_date USING l_begda .
      it_cpy00-zhedc = 1.

      it_calsl-persg = pa0001-persg.
      it_calsl-persk = pa0001-persk.
      it_calsl-stell = pa0001-stell.
      it_calsl-zhedc = 1.

** changed by jslee
*      PERFORM CALCURATE_MONTHLY_SALARY_XX USING L_BEGDA.
      PERFORM calcurate_monthly_salary_zz USING l_begda.
* 401K
*      PERFORM CALCURATE_MONTHLY_401K USING L_BEGDA.

      IF pa0001-persg = '9' AND pa0001-persk = 'U2'.
        it_calsl-zsaly = it_calsl-act01 = w_betrg.
        it_calsl-houry  = w_zhouy.
        it_calsl-ansal  = w_ansal.

      ELSEIF ( ( pa0001-persg = '1' AND pa0001-persk = 'U2' ) OR
               ( pa0001-persg = '1' AND pa0001-persk = 'U3' ) ).
        it_calsl-zsaly = it_calsl-act02 = w_betrg.
        it_calsl-houry  = w_zhouy.
        it_calsl-ansal  = w_ansal.
      ELSE.
        it_calsl-zsaly = it_calsl-act03 = w_betrg.
        it_calsl-houry  = w_zhouy.
        it_calsl-ansal  = w_ansal.
      ENDIF.

      PERFORM append_pernr_info_table USING l_begda.

      COLLECT it_cpy00. CLEAR it_cpy00.
      COLLECT it_calsl. CLEAR it_calsl.
    ENDIF.
  ENDSELECT.
*
  w_ptext = '80% processing...........'.
*  DO 5 TIMES.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 80
            text       = w_ptext.
*  ENDDO.
*... pay increase ratio
  CLEAR: zthr_pcp02, l_zval1.

  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '2'
                      AND zgrup = '1060'
                      AND zcode = '10020'.
  MOVE zthr_pcp02-zval1 TO l_zval1.
  IF l_zval1 < 1.
    l_zval1 = l_zval1 + 1.
  ENDIF.
*
  DO 12 TIMES.
    l_count = l_count + 1.
    LOOP AT it_cpy00.
      MOVE-CORRESPONDING it_cpy00 TO it_pcp00.
      it_pcp00-zmons = l_count.
      IF it_cpy00-zperg = '9' AND it_cpy00-zsubg = 'U2'.
        CLEAR it_calsl.
        READ TABLE it_calsl WITH KEY persg = it_cpy00-zperg
                                     persk = it_cpy00-zsubg
                                     kostl = it_cpy00-zcost
                                     stell = it_cpy00-zobjc
                                     zsenr = it_cpy00-zsenr.
        it_pcp00-act01 = ( it_calsl-act01 / it_calsl-zhedc ) * l_zval1.
        it_pcp00-houry = it_calsl-houry / it_calsl-zhedc.
        it_pcp00-ansal = it_calsl-ansal / it_calsl-zhedc.

*      ELSEIF PA0001-PERSG = '1' AND PA0001-PERSK = 'U2'.
      ELSEIF
      ( it_cpy00-zperg = '1' AND it_cpy00-zsubg = 'U2' ) OR
      ( it_cpy00-zperg = '1' AND it_cpy00-zsubg = 'U3' ) .
        CLEAR it_calsl.
        READ TABLE it_calsl WITH KEY persg = it_cpy00-zperg
                                     persk = it_cpy00-zsubg
                                     kostl = it_cpy00-zcost
                                     stell = it_cpy00-zobjc
                                     zsenr = it_cpy00-zsenr.
        it_pcp00-act02 = ( it_calsl-act02 / it_calsl-zhedc ) * l_zval1.
        it_pcp00-houry = it_calsl-houry / it_calsl-zhedc.
        it_pcp00-ansal = it_calsl-ansal / it_calsl-zhedc.

      ELSE.
        CLEAR it_calsl.
        READ TABLE it_calsl WITH KEY persg = it_cpy00-zperg
                                     persk = it_cpy00-zsubg
                                     kostl = it_cpy00-zcost
                                     stell = it_cpy00-zobjc
                                     zsenr = it_cpy00-zsenr.
        it_pcp00-act03 = ( it_calsl-act03 / it_calsl-zhedc ) * l_zval1.
        it_pcp00-houry = it_calsl-houry / it_calsl-zhedc.
        it_pcp00-ansal = it_calsl-ansal / it_calsl-zhedc.

      ENDIF.
*      it_pcp00-zsaly  = ( it_calsl-zsaly / it_calsl-zhedc ) * l_zval1.
*      it_pcp00-mthly  = it_pcp00-zsaly.
*      it_pcp00-omthly = it_pcp00-zsaly.
      it_pcp00-mthly  = ( it_calsl-zsaly / it_calsl-zhedc ) * l_zval1.
      it_pcp00-omthly = ( it_calsl-zsaly / it_calsl-zhedc ) * l_zval1.

      it_pcp00-ancur  = 'USD'.
      it_pcp00-erdat  = sy-datum.
      it_pcp00-erzet  = sy-uzeit.
      it_pcp00-ernam  = sy-uname.
      APPEND it_pcp00. CLEAR it_pcp00.
    ENDLOOP.
  ENDDO.
*
  w_ptext = '95% processing...........'.
*  DO 5 TIMES.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 95
            text       = w_ptext.
*  ENDDO.
*
  CLEAR zthr_pcp00.
*add : New costs are added by new year business plan
  PERFORM new_costcenter TABLES it_pcp00.

  MODIFY zthr_pcp00 FROM TABLE it_pcp00.
  IF sy-subrc = 0.
    CLEAR zthr_pcpxx.
    MODIFY zthr_pcpxx FROM TABLE it_pcpxx.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s001 WITH 'Transaction was processed successfully'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE s001 WITH 'Error during processing of Basic DB Creation'.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s001 WITH 'Error during processing of Basic DB Creation'.
  ENDIF.
ENDFORM.                    " MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  CALCURATE_MONTHLY_SALARY
*&---------------------------------------------------------------------*
FORM calcurate_monthly_salary USING p_begda.
  CLEAR it_payrt.
*... get pay result
  CALL FUNCTION 'HRCM_PAYROLL_RESULTS_GET'
       EXPORTING
            pernr              = pa0001-pernr
            begda              = w_begda
            endda              = w_endda
       IMPORTING
            payroll_result_tab = it_payrt.
*... G/L account - 601100
  IF pa0001-persg = '9' AND pa0001-persk = 'U2'.
    CLEAR: w_betrg, w_wrkmo.
    LOOP AT it_payrt INTO wa_payrt.
      IF wa_payrt-inter-versc-fpbeg < p_begda AND  " middle entering
         wa_payrt-inter-versc-fpend > p_begda.
        CONTINUE.
      ELSEIF wa_payrt-inter-versc-fpbeg < w_endda AND
             wa_payrt-inter-versc-fpbeg > w_endda.
        CONTINUE.
      ELSE.
        w_wrkmo = w_wrkmo + 1.
        LOOP AT wa_payrt-inter-rt INTO wa_reslt WHERE lgart IN r_month.
*                                                 AND CNTR1 EQ '01'.
          w_betrg = w_betrg + wa_reslt-betrg.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    w_betrg = w_betrg / w_wrkmo.
*... G/L account - 601110
  ELSEIF pa0001-persg = '1' AND pa0001-persk = 'U2'.
    it_calsl-kostl = pa0001-kostl.
    CLEAR: w_betrg, w_wrkmo.
    LOOP AT it_payrt INTO wa_payrt.
      IF wa_payrt-inter-versc-fpbeg < p_begda AND  " middle entering
         wa_payrt-inter-versc-fpend > p_begda.
        CONTINUE.
      ELSEIF wa_payrt-inter-versc-fpbeg < w_endda AND
             wa_payrt-inter-versc-fpbeg > w_endda.
        CONTINUE.
      ELSE.
        w_wrkmo = w_wrkmo +
        ( wa_payrt-inter-versc-fpend - wa_payrt-inter-versc-fpbeg + 1 ).
        LOOP AT wa_payrt-inter-rt INTO wa_reslt WHERE lgart IN r_wekly.
*                                                 AND CNTR1 EQ '01'.
          w_betrg = w_betrg + wa_reslt-betrg.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    w_betrg = ( w_betrg / w_wrkmo ) * ( 305 / 10 ).
*... G/L account - 601120
  ELSE.
    CLEAR: w_betrg, w_wrkmo.
  ENDIF.
ENDFORM.                    " CALCURATE_MONTHLY_SALARY
*&---------------------------------------------------------------------*
*&      Form  GET_CLOSING_DATE
*&---------------------------------------------------------------------*
FORM get_closing_date.
  IF w_xyear = w_zyear .
    w_begda = w_endda = sy-datum.
    w_begda+4(4) = '0101'.
  ELSE.
    w_begda = w_endda = w_xyear.
    w_begda+4(4) = '0101'.
    w_endda+4(4) = '1231'.
  ENDIF.
* get wage type by G/L account
  CLEAR: r_month, r_wekly, r_houry, r_bonus.
  REFRESH: r_month, r_wekly, r_houry, r_bonus.
  CLEAR: it_wage[], it_wage.

  CLEAR zthr_pcp02.
  SELECT zval1 zval2 zval3
    INTO (zthr_pcp02-zval1, zthr_pcp02-zval2, zthr_pcp02-zval3)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1220'
                      AND zval2 IN ('601100', '601110', '601120',
                                    '601200', '601310').
    r_month-sign = r_wekly-sign = r_houry-sign = r_bonus-sign =
    r_w401-sign = 'I'.
    r_month-option = r_wekly-option = r_houry-option =
    r_bonus-option = r_w401-option = 'EQ'.

    CASE zthr_pcp02-zval2.
      WHEN '601100'.
        r_month-low = zthr_pcp02-zval1.
        APPEND r_month. CLEAR r_month.
      WHEN '601110'.
        r_wekly-low = zthr_pcp02-zval1.
        APPEND r_wekly. CLEAR r_wekly.
      WHEN '601120'.
        r_houry-low = zthr_pcp02-zval1.
        APPEND r_houry. CLEAR r_houry.
      WHEN '601200'.
        r_bonus-low = zthr_pcp02-zval1.
        APPEND r_bonus. CLEAR r_bonus.
      WHEN '601360'.
        r_w401-low = zthr_pcp02-zval1.
        APPEND r_w401. CLEAR r_w401.
    ENDCASE.

    MOVE : zthr_pcp02-zval1 TO it_wage-val1,
           zthr_pcp02-zval2 TO it_wage-val2,
           zthr_pcp02-zval3 TO it_wage-val3.
    APPEND it_wage . CLEAR it_wage.

  ENDSELECT.
*... test data
*  W_BEGDA = '20030101'.
*  W_ENDDA = '20031231'.
ENDFORM.                    " GET_CLOSING_DATE
*&---------------------------------------------------------------------*
*&      Form  CALCURATE_MONTHLY_SALARY_XX
*&---------------------------------------------------------------------*
FORM calcurate_monthly_salary_xx USING t_begda.
  DATA : wa_diffday TYPE i.
  DATA: l_zval1     TYPE p DECIMALS 2.
  DATA : l_bdate TYPE d,
         l_edate TYPE d.
  CLEAR it_payrt.

  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '2'
                      AND zgrup = '1060'
                      AND zcode = '10020'.
  MOVE zthr_pcp02-zval1 TO l_zval1.
  IF l_zval1 < 1.
    l_zval1 = l_zval1 + 1.
  ENDIF.

*... get pay result
  CALL FUNCTION 'HRCM_PAYROLL_RESULTS_GET'
       EXPORTING
            pernr              = pa0001-pernr
            begda              = w_begda
            endda              = w_endda
       IMPORTING
            payroll_result_tab = it_payrt.
*
  CLEAR: w_betrg, w_wrkmo, w_zhouy, w_bonus,w_nhouy .
  CLEAR: l_bdate , l_edate.

  LOOP AT it_payrt INTO wa_payrt.
    IF wa_payrt-inter-versc-fpbeg <  t_begda AND  " middle entering
       wa_payrt-inter-versc-fpend >  t_begda.
      CONTINUE.
    ELSEIF wa_payrt-inter-versc-fpbeg < w_endda AND
           wa_payrt-inter-versc-fpbeg > w_endda.
      CONTINUE.
    ELSE.
* JSLEE INSER 05/10/04
      CHECK wa_payrt-inter-versc-fpbeg >= w_begda .
*   DATE CACULATION
      IF l_bdate IS INITIAL.
        l_bdate = wa_payrt-inter-versc-fpbeg.
      ENDIF.
      IF l_edate IS INITIAL.
        l_edate = wa_payrt-inter-versc-fpend.
      ENDIF.

      IF l_bdate >  wa_payrt-inter-versc-fpbeg.
        l_bdate = wa_payrt-inter-versc-fpbeg.
      ENDIF.

      IF l_edate < wa_payrt-inter-versc-fpend.
        l_edate = wa_payrt-inter-versc-fpend.
      ENDIF.



*... G/L account - 601100
      IF pa0001-persg = '9' AND pa0001-persk = 'U2'.
        w_wrkmo = w_wrkmo + 1.
        LOOP AT wa_payrt-inter-rt INTO wa_reslt WHERE lgart IN r_month.
          w_betrg = w_betrg + wa_reslt-betrg.
        ENDLOOP.
*... G/L account - 601110
* change 1,u2 or 1, u3 .
*      ELSEIF PA0001-PERSG = '1' AND PA0001-PERSK = 'U2'.
      ELSEIF ( ( pa0001-persg = '1' AND pa0001-persk = 'U2' ) OR
               ( pa0001-persg = '1' AND pa0001-persk = 'U3' ) ).

*    total wage amount
        LOOP AT wa_payrt-inter-rt INTO wa_reslt WHERE lgart IN r_wekly.
          w_betrg = w_betrg + wa_reslt-betrg.
        ENDLOOP.
*... G/L account - 601120
      ELSE.
* JSLEE DELETE 2004 05 10
*    total  working day
*        W_WRKMO = W_WRKMO +
*    ( WA_PAYRT-INTER-VERSC-FPEND - WA_PAYRT-INTER-VERSC-FPBEG + 1 ).
*    total wage amount
        LOOP AT wa_payrt-inter-rt INTO wa_reslt WHERE lgart IN r_houry.
          w_betrg = w_betrg + wa_reslt-betrg.
        ENDLOOP.
      ENDIF.
*... G/L account - 601200
      LOOP AT wa_payrt-inter-rt INTO wa_reslt WHERE lgart IN r_bonus.
        w_bonus = w_bonus + wa_reslt-betrg.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
*
  IF pa0001-persg = '9' AND pa0001-persk = 'U2'.
    it_calsl-kostl = pa0001-kostl.
    w_betrg = w_betrg / w_wrkmo.
*      ELSEIF PA0001-PERSG = '1' AND PA0001-PERSK = 'U2'.
  ELSEIF ( ( pa0001-persg = '1' AND pa0001-persk = 'U2' ) OR
               ( pa0001-persg = '1' AND pa0001-persk = 'U3' ) ).

    it_calsl-kostl = pa0001-kostl.
* JSLEE INSERT 04/05/10
    CLEAR : w_wrkmo.
    CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
         EXPORTING
              beg_da     = l_bdate
              end_da     = l_edate
         IMPORTING
              no_cal_day = w_wrkmo.

* jslee modify 04/05/06
* Monthly wage
    w_betrg = ( (  w_betrg / w_wrkmo  ) * 305 ) / 10 .
* hourly wage
    w_zhouy =  w_betrg / ( 2080  / 12 ) .
    w_nhouy = ( w_betrg * l_zval1 ) / ( 2080  / 12 ) .
*   ( ( ( ( W_BETRG * L_ZVAL1 ) /  W_WRKMO  ) * 305 )
*   / 10 )  / ( 2080  / 12 ).

  ELSE.
    it_calsl-kostl = pa0001-kostl.
    it_calsl-zsenr = it_cpy00-zsenr.
* JSLEE INSERT 04/05/10
    CLEAR : w_wrkmo.
    CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
         EXPORTING
              beg_da     = l_bdate
              end_da     = l_edate
         IMPORTING
              no_cal_day = w_wrkmo.

* Monthly wage
    w_betrg = ( (  w_betrg /  w_wrkmo  ) * 305 ) / 10 .
* hourly wage
    w_zhouy = ( w_betrg * l_zval1 ) / ( 2080  / 12 ) .
  ENDIF.
ENDFORM.                    " CALCURATE_MONTHLY_SALARY_XX
*&---------------------------------------------------------------------*
*&      Form  APPEND_PERNR_INFO_TABLE
*&---------------------------------------------------------------------*
FORM append_pernr_info_table USING p_begda.
  DATA : l_end   LIKE sy-datum,
         l_ahday TYPE i,
         f_ahday(8) TYPE p DECIMALS 2,
         l_begin LIKE sy-datum.

  TABLES : pa0041 .
  DATA : l_code LIKE p0041-dar01.
  DATA : l_date LIKE p0041-dat01.

  CONCATENATE w_zyear '1231' INTO l_end.
  CLEAR l_begin .
*Original Hire Date
  SELECT SINGLE *
                FROM pa0041
                WHERE pernr = pa0001-pernr
                  AND endda = '99991231' .

  DO 12 TIMES VARYING l_code FROM pa0041-dar01 NEXT pa0041-dar02
              VARYING l_date FROM pa0041-dat01 NEXT pa0041-dat02 .
    IF l_code = 'Z1'.
      l_begin = l_date.
      EXIT.
    ENDIF.

  ENDDO.

  IF l_begin IS INITIAL.
    l_begin = p_begda.
  ENDIF.
* Defferience day mont year
  CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
       EXPORTING
            beg_da  = l_begin
            end_da  = l_end
       IMPORTING
            no_year = l_ahday.

  PERFORM caculation_holiy_day
                               USING   pa0001-stell
                               CHANGING l_ahday f_ahday.
  it_pcpxx-zyear = w_zyear.
  it_pcpxx-zvers = w_zvers.
  it_pcpxx-pernr = pa0001-pernr.
  it_pcpxx-zcost = pa0001-kostl.
  it_pcpxx-zperg = pa0001-persg.
  it_pcpxx-zsubg = pa0001-persk.
  it_pcpxx-zobjc = pa0001-stell.
  it_pcpxx-zsenr = it_cpy00-zsenr.
  it_pcpxx-zsaly = w_betrg.
  it_pcpxx-ansal = w_total.
  it_pcpxx-zhouy = w_zhouy.
  it_pcpxx-nhouy = w_nhouy.
  it_pcpxx-bonus = w_bonus.
  it_pcpxx-z401k = w_401k.
  w_nhce = ( w_nhce / w_wrkmo ) * 26.
  it_pcpxx-znhce = w_nhce.
  it_pcpxx-zanhce = w_anhce.
  it_pcpxx-ahday = f_ahday.
  it_pcpxx-begda = l_begin .
  it_pcpxx-ancur = 'USD'.
  it_pcpxx-erdat = sy-datum.
  it_pcpxx-erzet = sy-uzeit.
  it_pcpxx-ernam = sy-uname.
  it_pcpxx-zpera = pa0001-werks.
*
  APPEND it_pcpxx. CLEAR it_pcpxx.
ENDFORM.                    " APPEND_PERNR_INFO_TABLE
*&---------------------------------------------------------------------*
*&      Form  MAKE_OVERTIME_WORK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_overtime_work.
  DATA : wa_answer TYPE c,
         wa_title(50)  TYPE c.

* DATA CLEAR .
  CLEAR it_pcpxx. REFRESH it_pcpxx.
  CLEAR it_pcp00. REFRESH it_pcp00.
  CLEAR it_cpy00. REFRESH it_cpy00.
  CLEAR it_calsl. REFRESH it_calsl.
*
  wa_title = 'realy Calculate Cost plan '.
  PERFORM pop_up_message  USING  wa_title
                          CHANGING wa_answer.

  IF wa_answer <> 'J'.
    EXIT.
  ENDIF.

  SELECT * INTO TABLE it_pcp00
           FROM zthr_pcp00
           WHERE zyear = w_zyear
             AND zvers = w_zvers .
*             and ZCOST = '0000044001'
*             AND NOT ( ZPERG = '9' AND ZSUBG = 'U2'  ) .

* SELECT MASTER DATA.

  PERFORM job_code_selection .
*  PERFORM overtime_code_book_data_read.
  PERFORM attendvance_inc.                                  " 601180
  PERFORM 601330_cc_code.
  PERFORM read_other_code.
*  PERFORM WORK_EXTRA_PREMIUM .

  LOOP AT it_pcp00.
* Calculate Pay Weekday and Overtime work
*    PERFORM calculation_normal.
    PERFORM calculation_overtime.
    PERFORM calculation_attend_inc.
    PERFORM calculation_601320.
    PERFORM calculation_601330.      "WORKER'S COM
    PERFORM calculation_601340_ad.   "AD&D
    PERFORM calculation_691350.      "Sort Term
    PERFORM calculation_691360.
*    PERFORM calculation_691380.
*    PERFORM calculation_691390.
    PERFORM calculation_601430.      "Life insurance
    PERFORM calculation_601360.      "Long term
    PERFORM calculation_601300.      "Pension
******
    PERFORM calculation_601400.
    PERFORM calculation_601410.

    PERFORM calculation_601380.      "Social
    PERFORM calculation_601390.      "Medi

    PERFORM total_cal.
*check header count
    PERFORM header_count.

    MODIFY it_pcp00.

  ENDLOOP.

  CLEAR zthr_pcp00.
  MODIFY zthr_pcp00 FROM TABLE it_pcp00.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s001 WITH 'Transaction was processed successfully'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s001 WITH 'Error during processing of Basic DB Creation'.
  ENDIF.


ENDFORM.                    " MAKE_OVERTIME_WORK
*&---------------------------------------------------------------------*
*&      Form  JOB_CODE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_code_selection_check.

  CLEAR :  r_jobcode[],  r_jobcode.
  CLEAR zthr_pcp02.

  SELECT zval1 zval4
      INTO (zthr_pcp02-zval1, zthr_pcp02-zval4)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1040' .

    r_jobcode-sign   = 'I'.
    r_jobcode-option = 'EQ'.
    MOVE zthr_pcp02-zval1 TO r_jobcode-low.

    APPEND r_jobcode.  CLEAR r_jobcode.

  ENDSELECT.
  DELETE r_jobcode WHERE sign = ''.
  IF r_jobcode[] IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Not Define 02-1040 Job Code'.
  ENDIF.
* OT JOB CODE


ENDFORM.                    " JOB_CODE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  CALULATION_OVERTIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_overtime.
  IF NOT ( it_pcp00-zperg = '9' AND it_pcp00-zsubg  = 'U2' ).

* CHANGE CODE
    IF it_pcp00-zobjc IN r_otcode. " Office
      IF ( it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U2' ) OR
          (  it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U3' ).
        PERFORM office_overtime_cal.                        "1040
      ELSE.
        PERFORM by_week_overtime_cal.                       "1050
      ENDIF.
    ELSE .                           " Production Job
*      IF ( it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U2' ) OR
*         ( it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U3' ) OR
*         ( it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U0' ).

*        PERFORM office_overtime_cal.
*      ELSE.
      PERFORM by_week_overtime_cal_p.
    ENDIF.
  ENDIF.


ENDFORM.                    " CALULATION_OVERTIME
*&---------------------------------------------------------------------*
*&      Form  OVERTIME_CODE_BOOK_DATA_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM overtime_code_book_data_read.
*  CLEAR : w_increse_rate1, w_increse_rate2.
*  CLEAR : zthr_pcp02.
*  SELECT zval1 zcode
*         INTO (zthr_pcp02-zval1,zthr_pcp02-zcode)
*         FROM zthr_pcp02
*         WHERE zmodl = '02'
*          AND zgrup = '1060'
*          AND zcode  IN ('10000','10010','10020').
*
*    CASE zthr_pcp02-zcode.
*      WHEN '10000'.                                         " 01~06
*        MOVE : zthr_pcp02-zval1 TO w_increse_rate1 .
*        IF w_increse_rate1 < 1.
*          w_increse_rate1 = w_increse_rate1 + 1 .
*        ENDIF.
*      WHEN '10010' .
*        MOVE : zthr_pcp02-zval1 TO w_increse_rate2.
*        IF w_increse_rate2 < 1.
*          w_increse_rate2 = w_increse_rate2 + 1 .
*        ENDIF.
*
*      WHEN '10020'.
*        MOVE : zthr_pcp02-zval1 TO w_increse_rate3.
*        IF w_increse_rate3 < 1.
*          w_increse_rate3 = w_increse_rate3 + 1 .
*        ENDIF.
*
*    ENDCASE.
*
*  ENDSELECT.

ENDFORM.                    " OVERTIME_CODE_BOOK_DATA_READ
*&---------------------------------------------------------------------*
*&      Form  OFFICE_OVERTIME_CAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM office_overtime_cal.

  DATA: w_overtime LIKE zthr_pcp05-ztotm,
        w_rate     LIKE w_increse_rate1,
        w_zhedc    LIKE zthr_pcp05-zhedc,
        w_wtime    LIKE zthr_pcp05-ztotm .

  DATA : BEGIN OF it_pcp05 OCCURS 0.
          INCLUDE STRUCTURE zthr_pcp05.
  DATA : END OF it_pcp05.

  CHECK it_pcp00-zobjc IN r_otcode.
* change by jslee 05/18/2004  * change logic

  SELECT  * INTO TABLE it_pcp05
      FROM zthr_pcp05
      WHERE zscst = it_pcp00-zcost
        AND zyear = it_pcp00-zyear
        AND zmons = it_pcp00-zmons
        AND zvers = it_pcp00-zvers
        AND zgrup = '1040'
        AND zseqn > 1.

*  ( IT_PCP05-ZTOTM /

  CLEAR : it_pcp00-act06.
  LOOP AT it_pcp05 .
    CLEAR : w_wtime .
*    IF ZTHR_PCP05-ZHEDC <> 0.
*      W_WTIME = ZTHR_PCP05-ZTOTM / ZTHR_PCP05-ZHEDC .
*    ENDIF.
*Get increae rate
    PERFORM overtime_code_book_read_v1 USING it_pcp00-zmons
                                             it_pcp05-zgrup  .

    IF it_pcp05-zhedc <> 0.
      w_wtime = it_pcp05-ztotm / it_pcp05-zhedc .
    ENDIF.

    it_pcp00-act06 =  it_pcp00-act06 + ( it_pcp00-houry * w_wtime
                   *  it_pcp00-zhedc  * it_pcp05-zextr
                    * w_increse_rate1 ).
*    it_pcp00-act06 =  it_pcp00-act06 + ( it_pcp00-houry * w_wtime
*                      * it_pcp05-zextr * w_increse_rate1 ).
*
  ENDLOOP.

ENDFORM.                    " OFFICE_OVERTIME_CAL
*&---------------------------------------------------------------------*
*&      Form  PRODUCT_OVERTIME_CAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM by_week_overtime_cal.

  DATA: w_overtime LIKE zthr_pcp05-ztotm,
        w_zhedc    LIKE zthr_pcp05-zhedc.
  DATA : BEGIN OF it_pcp05 OCCURS 0.
          INCLUDE STRUCTURE zthr_pcp05.
  DATA : END OF it_pcp05.

  DATA : w_wtime LIKE zthr_pcp05-ztotm.


* change by jslee 05/18/2004  * change logic

  SELECT  * INTO TABLE it_pcp05
      FROM zthr_pcp05
      WHERE zscst = it_pcp00-zcost
        AND zyear = it_pcp00-zyear
        AND zmons = it_pcp00-zmons
        AND zvers = it_pcp00-zvers
        AND zseqn > 1
        AND zgrup = '1050' .

  CLEAR : it_pcp00-act07.

  LOOP AT it_pcp05 .
    CLEAR : w_wtime .
*    IF ZTHR_PCP05-ZHEDC <> 0.
*      W_WTIME = ZTHR_PCP05-ZTOTM / ZTHR_PCP05-ZHEDC .
*    ENDIF.
*Get increae rate
    PERFORM overtime_code_book_read_v1 USING it_pcp00-zmons
                                             it_pcp05-zgrup  .

    IF it_pcp05-zhedc <> 0.
      w_wtime = it_pcp05-ztotm / it_pcp05-zhedc .
    ENDIF.

    IF it_pcp05-zseqn = '1'.
      it_pcp00-act03 =   it_pcp00-houry *  it_pcp00-zhedc * w_wtime
                                   * it_pcp05-zextr * w_increse_rate1 .
    ELSE.
      it_pcp00-act07 =  it_pcp00-act07 + ( it_pcp00-houry
                     *  it_pcp00-zhedc *   w_wtime   * it_pcp05-zextr
                     * w_increse_rate1 ).
*       it_pcp00-act07 =  it_pcp00-act07 + ( it_pcp00-houry
*                      *  w_wtime   * it_pcp05-zextr
*                      *  w_increse_rate1 ).
*
    ENDIF.

  ENDLOOP.


ENDFORM.                    " PRODUCT_OVERTIME_CAL
*&---------------------------------------------------------------------*
*&      Form  BY_WEEK_OVERTIME_CAL_P
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM by_week_overtime_cal_p.
  DATA: w_rate     LIKE w_increse_rate1.
  DATA: w_overtime LIKE zthr_pcp05-ztotm,
        w_zhedc    LIKE zthr_pcp05-zhedc.

  DATA : BEGIN OF it_pcp05 OCCURS 0.
          INCLUDE STRUCTURE zthr_pcp05.
  DATA : END OF it_pcp05.
  DATA : w_wtime LIKE zthr_pcp05-ztotm.
  REFRESH it_pcp05.

  SELECT *  FROM zthr_pcp02 WHERE zmodl = '02'
                              AND zgrup = '1050'
                              AND zval1 = it_pcp00-zobjc.
    IF sy-subrc = 0.
* change by jslee 05/18/2004  * change logic

      SELECT  * INTO TABLE it_pcp05
          FROM zthr_pcp05
          WHERE zscst = it_pcp00-zcost
            AND zyear = it_pcp00-zyear
            AND zmons = it_pcp00-zmons
            AND zvers = it_pcp00-zvers
            AND zseqn > 1
            AND zgrup = '1050' .

      CLEAR : it_pcp00-act07.

      LOOP AT it_pcp05 .
        CLEAR : w_wtime .
        IF it_pcp05-zhedc <> 0.
          w_wtime = it_pcp05-ztotm / it_pcp05-zhedc .
        ENDIF.
*Get increae rate
        PERFORM overtime_code_book_read_v1 USING it_pcp00-zmons
                                                 it_pcp05-zgrup  .

        IF it_pcp05-zseqn = '1'.
          it_pcp00-act03 =   it_pcp00-houry *  it_pcp00-zhedc * w_wtime
                                    * it_pcp05-zextr * w_increse_rate1 .
        ELSE.
          it_pcp00-act07 =  it_pcp00-act07 + ( it_pcp00-houry
                     *  it_pcp00-zhedc *   w_wtime  * it_pcp05-zextr
                         * w_increse_rate1 ).
*        it_pcp00-act07 =  it_pcp00-act07 + ( it_pcp00-houry
*                        *  w_wtime  * it_pcp05-zextr
*                        *  w_increse_rate1 ).
        ENDIF.

      ENDLOOP.
    ELSE.
      MESSAGE e001 WITH 'JOB CODE ERROR'.
    ENDIF.
  ENDSELECT.
ENDFORM.                    " BY_WEEK_OVERTIME_CAL_P
*&---------------------------------------------------------------------*
*&      Form  ATTENDVANCE_INC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM attendvance_inc.


  SELECT zval1 zcode
        INTO (zthr_pcp02-zval1,zthr_pcp02-zcode)
        FROM zthr_pcp02
        WHERE zmodl = '02'
         AND zgrup = '1070'
         AND zcode  IN ('10000','10010').

    CASE zthr_pcp02-zcode.
      WHEN '10000'.                                         " 01~06
        MOVE : zthr_pcp02-zval1 TO w_attpay .
      WHEN '10010' .
        MOVE : zthr_pcp02-zval1 TO w_att_rate.
    ENDCASE.
  ENDSELECT.

  CLEAR : r_attjo , r_attjo[].

  SELECT zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1065' .

    r_attjo-sign   = 'I'.
    r_attjo-option = 'EQ'.
    MOVE zthr_pcp02-zval1 TO r_attjo-low.

    APPEND r_attjo.  CLEAR r_attjo.

  ENDSELECT.
  DELETE r_attjo WHERE sign = ''.
  IF r_attjo[] IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Not Define 02-1065 Job Code'.
  ENDIF.

ENDFORM.                    " ATTENDVANCE_INC
*&---------------------------------------------------------------------*
*&      Form  CALULATION_ATTEND_INC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_attend_inc.

  DATA : w_val04 LIKE it_pcp00-act04.

  IF it_pcp00-zobjc IN  r_attjo AND  it_pcp00-zperg = '1'.
    it_pcp00-act04 = w_attpay * w_att_rate ."* it_pcp00-zhedc.

  ENDIF.
ENDFORM.                    " CALULATION_ATTEND_INC
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601340_AD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601340_ad.

  it_pcp00-act13 = ( it_pcp00-ansal / 12 )  * it_code-1340.
*                 *  it_pcp00-zhedc.

ENDFORM.                    " CALCULATION_601340_AD
*&---------------------------------------------------------------------*
*&      Form  READ_OTHER_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_other_code.

* 601300 Pension
* (2-1110-10000 ) Total benefit
  SELECT SINGLE zval1 INTO it_code-1300
         FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1110'
       AND zcode = '10000'.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH '(2-1110-10000 ) Total benefit not defind'.
  ENDIF.

* 601340 ad & d
* (2-1190-10000 )
  SELECT SINGLE zval1 INTO it_code-1340
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1190'
       AND zcode = '10000'.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1190-10000 )  601340 ad & d  not defind'.
  ENDIF.


* 601350 sort term Disability
* (2-1190-10070 )
  SELECT SINGLE zval1 INTO it_code-1350
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1190'
       AND zcode = '10070'.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1190-10070 )  601350 sort term'.
  ENDIF.

* 601360 Long Term Disability
* (2-1190-10020 )
  SELECT SINGLE zval1 INTO it_code-1360
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1190'
       AND zcode = '10020'.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1190-10020 )  601360 Long Term Disability'.
  ENDIF.

* 601380 Social Security
* (2-1190-10080 )
  SELECT SINGLE zval1 INTO it_code-1380
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1190'
       AND zcode = '10030'.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1190-10030 )  601380 Social Security'.
  ENDIF.

* 601390 Medicare
* (2-1190-10040 )
  SELECT SINGLE zval1 INTO it_code-1390
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1190'
       AND zcode = '10040'.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1190-10040 )  601390 Medicare'.
  ENDIF.

* 601430 Life insurance
* (2-1190-10010 )
  SELECT SINGLE zval1 INTO it_code-1430
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1190'
       AND zcode = '10010'.

  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1190-10010 )  601430 Life insurance  not defind'.
  ENDIF.

* 601320 Health Insurance
* (2-1150-10000 )
  SELECT SINGLE zval1 INTO it_code-1320
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1150'
       AND zcode = '10000'.

  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1150-10000 )    not defind'.
  ENDIF.

* 601400 SUTA
* (2-1190-100510 )
  SELECT SINGLE zval1  zval2
             INTO (it_code-1400, it_code-1401)
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1190'
       AND zcode = '10050'.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1190-100510 )  601400 SUTA  not defind'.
  ENDIF.

* 601400 FUTA
* (2-1190-100510 )
  SELECT SINGLE zval1  zval2
             INTO (it_code-1410, it_code-1411)
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1190'
       AND zcode = '10060'.

  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1190-100510 )  601400 FUTA  not defind'.
  ENDIF.

* (2-1160-10000 )
  SELECT SINGLE zval1 INTO it_code-1321
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1160'
       AND zcode = '10000'.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1160-10000 )    not defind'.
  ENDIF.

* (2-1160-10010 )
  SELECT SINGLE zval1 INTO it_code-1322
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1160'
       AND zcode = '10010'.

  IF sy-subrc NE 0.
    MESSAGE e001 WITH
     '(2-1160-10010 )    not defind'.
  ENDIF.





** 601500  Relocation Expense
*
*  SELECT SINGLE ZVAL1 ZVAL2 INTO (IT_CODE-1500, IT_CODE-1501)
*     FROM ZTHR_PCP02
*     WHERE ZMODL = '02'
*       AND ZGRUP = '1190'
*       AND ZCODE = '10060'.
*
*  IF SY-SUBRC NE 0.
*    MESSAGE E001 WITH
*     '(2-1190-10060 )  601500  Relocation Expense  not defind'.
*  ENDIF.


  CLEAR :  r_mang[],  r_mang.

  SELECT zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1200' .

    r_mang-sign   = 'I'.
    r_mang-option = 'EQ'.
    MOVE zthr_pcp02-zval1 TO r_mang-low.
    APPEND r_mang.  CLEAR r_mang.

  ENDSELECT.

ENDFORM.                    " READ_OTHER_CODE
*&---------------------------------------------------------------------*
*&      Form  calculation_601360
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601360.
** 601360 Long term Disability
*
  it_pcp00-act15 = ( it_pcp00-ansal / 12 ) * it_code-1360.
*                  *  it_pcp00-zhedc.
ENDFORM.                    " calculation_601360
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601430
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601430.
** 601430 Life Insurance
*Original source
*  it_pcp00-act20 = ( it_pcp00-ansal / 12 ) * it_code-1430
*                 *  it_pcp00-zhedc.

  it_pcp00-act20 = ( it_pcp00-ansal / 12 ) * it_code-1430.
*                 *  it_pcp00-zhedc.

ENDFORM.                    " CALCULATION_601430
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601380
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601380.
* 601380;Social Security

  DATA : it_1380 LIKE zthr_pcp02 OCCURS 0 WITH HEADER LINE.
  DATA : z_act(14),num(2) TYPE n VALUE 1.
  FIELD-SYMBOLS : <act> TYPE ANY.REFRESH it_1380.

  IF it_pcp00-zperg NE '9'.
    SELECT  * INTO TABLE it_1380
       FROM zthr_pcp02
       WHERE zmodl  = '02'
        AND zgrup  = '1220'
        AND zval2  = '601380'.

    CLEAR it_pcp00-act16.

    LOOP AT it_1380.
      num = it_1380-zval1.
      CONCATENATE 'IT_PCP00-ACT' num INTO z_act.
      ASSIGN (z_act) TO <act>.
      IF z_act EQ 'IT_PCP00-ACT06' OR z_act EQ 'IT_PCP00-ACT07'.
        it_pcp00-act16 =   it_pcp00-act16 + ( <act> / it_pcp00-zhedc ) .
      ELSE.
        it_pcp00-act16 =   it_pcp00-act16 + <act> .
      ENDIF.
    ENDLOOP.
    it_pcp00-act16 =  it_pcp00-act16 * it_code-1380.
  ELSE.
    CLEAR it_pcp00-act16.
  ENDIF.
ENDFORM.                    " CALCULATION_601380
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601390
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601390.
* 601390 Medicare

  DATA : it_1390 LIKE zthr_pcp02 OCCURS 0 WITH HEADER LINE.
  DATA : z_act(14),num(2) TYPE n VALUE 1.
  FIELD-SYMBOLS : <act> TYPE ANY.REFRESH it_1390.

  IF it_pcp00-zperg NE '9'.
    SELECT  * INTO TABLE it_1390
       FROM zthr_pcp02
       WHERE zmodl  = '02'
        AND zgrup  = '1220'
        AND zval2  = '601390'.

    CLEAR it_pcp00-act17.

    LOOP AT it_1390.
      num = it_1390-zval1.
      CONCATENATE 'IT_PCP00-ACT' num INTO z_act.
      ASSIGN (z_act) TO <act>.
      IF z_act EQ 'IT_PCP00-ACT06' OR z_act EQ 'IT_PCP00-ACT07'.
        it_pcp00-act17 =   it_pcp00-act17 + ( <act> / it_pcp00-zhedc ) .
      ELSE.
        it_pcp00-act17 =   it_pcp00-act17 + <act> .
      ENDIF.
    ENDLOOP.
    it_pcp00-act17 =  it_pcp00-act17 * it_code-1390.
  ELSE.
    CLEAR it_pcp00-act17.
  ENDIF.

ENDFORM.                    " CALCULATION_601390
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601300.
  DATA : w_1300 TYPE p DECIMALS 2.

  IF it_pcp00-zperg NE '9'.
    it_pcp00-act09 = ( it_pcp00-ansal / 12 ) * it_code-1300 .
*     *   it_pcp00-zhedc.    .
  ELSE.
    CLEAR it_pcp00-act09.
  ENDIF.
ENDFORM.                    " CALCULATION_601300
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601320
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601320.
* 601320 Health Insurance
  DATA : w_rate  TYPE p DECIMALS 2,
         w_rate2 TYPE p DECIMALS 2.

  MOVE : it_code-1320 TO w_rate.
  w_rate2 = 1 - w_rate.

*  it_pcp00-act11 = ( it_code-1321 * it_pcp00-zhedc * w_rate )
*                     +
*                   ( it_code-1322 * it_pcp00-zhedc * w_rate2 ).

  it_pcp00-act11 = ( it_code-1321 *   w_rate ) +
                    ( it_code-1322 *  w_rate2 ).


ENDFORM.                    " CALCULATION_601320
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601330
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601330.

  IF  it_pcp00-zcost IN r_1170.
    IF it_pcp00-zobjc IN r_1180.
      READ TABLE it_1175 WITH KEY zcode = '10200'.
      IF sy-subrc EQ 0.
        it_pcp00-act12 = ( it_pcp00-ansal / 12 )
*                        * it_1175-zval1 *  it_pcp00-zhedc.
                         * it_1175-zval1 .

      ENDIF.
    ELSE.
      READ TABLE it_1175 WITH KEY zcode = '10100'.
      IF sy-subrc EQ 0.
        it_pcp00-act12 = ( it_pcp00-ansal / 12 )
*                        * it_1175-zval1 *  it_pcp00-zhedc.
                         * it_1175-zval1 .

      ENDIF.
    ENDIF.
  ELSE.
    READ TABLE it_1175 WITH KEY zcode = '10000'.
    IF sy-subrc EQ 0.
      it_pcp00-act12 = ( it_pcp00-ansal / 12 )
*                        * it_1175-zval1 *  it_pcp00-zhedc.
                          * it_1175-zval1 .

    ENDIF.
  ENDIF.

ENDFORM.                    " CALCULATION_601330
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601340
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601340.

* 601340
  IF it_pcp00-zperg = '9' AND it_pcp00-zsubg = 'U2'.
*   601100
    it_pcp00-act17 = it_pcp00-act01 * it_code-1390.
*  ELSEIF IT_PCP00-ZPERG = '1' AND IT_PCP00-ZSUBG = 'U2'.
  ELSEIF ( it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U2' ) OR
          ( it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U3' ) .

*   601110
    it_pcp00-act17 = it_pcp00-act02 * it_code-1390.
  ELSE.
*  601120
    it_pcp00-act17 = it_pcp00-act03 * it_code-1390.
  ENDIF.

ENDFORM.                    " CALCULATION_601340
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601400
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601400.

  it_pcp00-act18 = ( it_code-1400 * it_code-1401 ) / 12.

ENDFORM.                    " CALCULATION_601400
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601410
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601410.

  it_pcp00-act19 = ( it_code-1410 * it_code-1411 ) / 12.

ENDFORM.                    " CALCULATION_601410
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_601500
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_601500.

  IF it_pcp00-zobjc IN r_mang.
    MOVE it_code-1500 TO it_pcp00-act21.
  ELSE.
    MOVE it_code-1501 TO it_pcp00-act21.
  ENDIF.

ENDFORM.                    " CALCULATION_601500
*&---------------------------------------------------------------------*
*&      Form  Final_Data_Excel_Down_Load
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM final_data_excel_down_load.

  CALL SCREEN 9100.

ENDFORM.                    " Final_Data_Excel_Down_Load
*&---------------------------------------------------------------------*
*&      Form  CHANGING_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM changing_field_catalog.

  DATA: ls_fcat TYPE lvc_s_fcat,
        l_lin   TYPE i ,
        l_index LIKE sy-tabix.

  DATA: l_dd03p LIKE dd03p OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_buffer_active        = ' '
            i_structure_name       = 'ZSHR_PCP00'
            i_client_never_display = 'X'
            i_bypassing_buffer     = ' '
       CHANGING
            ct_fieldcat            = it_fcat[]
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DO 10 TIMES .

    l_index = l_index + 1.
    READ TABLE it_fcat INTO ls_fcat INDEX  l_index .
    IF sy-subrc EQ 0.
      ls_fcat-key = 'X'.
      MODIFY it_fcat FROM ls_fcat INDEX l_index.

    ENDIF.


  ENDDO.
  CALL FUNCTION 'DDIF_TABL_GET'
       EXPORTING
            name      = 'ZSHR_PCP00'
            state     = 'A'
            langu     = sy-langu
       TABLES
            dd03p_tab = l_dd03p.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*  BREAK-POINT.
  LOOP AT l_dd03p.

    CHECK l_dd03p-fieldname(3) = 'ACT'.
    READ TABLE it_fcat INTO ls_fcat
                        WITH KEY  fieldname =  l_dd03p-fieldname .

    IF sy-subrc EQ 0 .
      MOVE sy-tabix TO l_index.
      ls_fcat-reptext   = l_dd03p-ddtext+15(40).
      ls_fcat-scrtext_s = l_dd03p-scrtext_s.
      ls_fcat-scrtext_m = l_dd03p-ddtext+15(40).
      ls_fcat-scrtext_l = l_dd03p-ddtext+15(40).
      MODIFY it_fcat FROM ls_fcat INDEX l_index.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHANGING_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  GET_401K_WAGE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_401k_wage_type.

  SELECT zval1 zval2 INTO (zthr_pcp02-zval1, zthr_pcp02-zval2)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1220'
                      AND zval2 = '601310'.
    r_w401-sign   = 'I'.
    r_w401-option = 'EQ'.

    r_w401-low = zthr_pcp02-zval1.
    APPEND r_w401. CLEAR r_w401.

  ENDSELECT.

ENDFORM.                    " GET_401K_WAGE_TYPE
*&---------------------------------------------------------------------*
*&      Form  CALCURATE_MONTHLY_401K
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_BEGDA  text
*----------------------------------------------------------------------*
FORM calcurate_monthly_401k USING    p_begda.
  DATA :  l_end_da LIKE sy-datum.
  DATA :  l_mon  TYPE i.
  DATA :  l_spmon TYPE spmon,
          x_spmon TYPE spmon.

  DATA :  l_day	        TYPE	i,
          l_month	        TYPE	i,
          l_year	        TYPE	i,
          l_cal_day	TYPE	i.

  DATA :  BEGIN OF it_wpay  OCCURS 0,
          spmon TYPE spmon,
          betrg LIKE w_betrg,
          END OF it_wpay.
  DATA : wa_cbetrg  TYPE	maxbt,
         wa_nbetrg  TYPE	maxbt.
  DATA : wa_endday TYPE d.
  DATA : wa_xcount(2) TYPE n,
         wa_index     TYPE i.



* Defferience day mont year
  CALL FUNCTION 'HR_PT_ADD_MONTH_TO_DATE'
       EXPORTING
            dmm_datin = sy-datum
            dmm_count = '12'
            dmm_oper  = '+'
            dmm_pos   = 'BEG'
       IMPORTING
            dmm_daout = l_end_da.


  CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
       EXPORTING
            beg_da   = p_begda
            end_da   = sy-datum
       IMPORTING
            no_month = l_month
            no_year  = l_year.

  l_mon = ( 12 * l_year ) + l_month.

  CLEAR: w_betrg.

  LOOP AT it_payrt INTO wa_payrt.
* check
    IF wa_payrt-inter-versc-fpbeg < p_begda AND  " middle entering
       wa_payrt-inter-versc-fpend > p_begda.
      CONTINUE.
    ELSEIF wa_payrt-inter-versc-fpbeg < w_endda AND
           wa_payrt-inter-versc-fpbeg > w_endda.
      CONTINUE.
    ELSE.
* amount caculation

      LOOP AT wa_payrt-inter-rt INTO wa_reslt WHERE lgart IN r_w401.
        l_spmon = wa_payrt-inter-versc-fpend(6).
        x_spmon = wa_payrt-inter-versc-fpbeg(6).

        IF l_spmon NE x_spmon .
          CALL FUNCTION 'ZHR_PCP_WEEK_TO_MONTHY_PAY'
               EXPORTING
                    bdate     = wa_payrt-inter-versc-fpbeg
                    edate     = wa_payrt-inter-versc-fpend
                    payment   = wa_reslt-betrg
               IMPORTING
                    c_payment = wa_cbetrg
                    n_payment = wa_nbetrg.
          CLEAR it_wpay.
          it_wpay-spmon = wa_payrt-inter-versc-fpbeg(6).
          it_wpay-betrg = wa_cbetrg.
          COLLECT it_wpay. CLEAR it_wpay.
          it_wpay-spmon = wa_payrt-inter-versc-fpend(6).
          it_wpay-betrg = wa_nbetrg.
          COLLECT it_wpay. CLEAR it_wpay.
          wa_endday = wa_payrt-inter-versc-fpend.
        ELSE.
*          W_BETRG = W_BETRG + WA_RESLT-BETRG.
          it_wpay-spmon = wa_payrt-inter-versc-fpbeg(6).
          it_wpay-betrg = wa_reslt-betrg.
          COLLECT it_wpay. CLEAR it_wpay.
          wa_endday = wa_payrt-inter-versc-fpend.
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDLOOP.
* Per monthly amount caculation.
  READ TABLE it_wpay WITH KEY spmon = wa_endday(6).
  IF sy-subrc EQ 0.
    wa_index = sy-tabix.

    IF    wa_endday+6(2) >= '30'.
      wa_xcount = 12  - wa_endday+4(2) .
      DO wa_xcount TIMES.
        it_wpay-spmon = it_wpay-spmon + 1.
        COLLECT it_wpay.
      ENDDO.
    ELSE.
      wa_index = wa_index - 1.
      IF wa_index > 0.
        READ TABLE it_wpay INDEX  wa_index .
        IF sy-subrc EQ 0 .
          wa_xcount = 12  - it_wpay-spmon+4(2) .
          DO wa_xcount TIMES.
            it_wpay-spmon = it_wpay-spmon + 1.
            COLLECT it_wpay.
          ENDDO.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " CALCURATE_MONTHLY_401K
*&---------------------------------------------------------------------*
*&      Form  GET_EE_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ee_group.
  CLEAR : r_persg[], r_persg.

  SELECT zval1 zval2 INTO (zthr_pcp02-zval1, zthr_pcp02-zval2)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1230' .
    r_persg-sign =  'I'.
    r_persg-option = 'EQ'.
    r_persg-low = zthr_pcp02-zval1.
    APPEND r_persg. CLEAR r_persg.
  ENDSELECT.

ENDFORM.                    " GET_EE_GROUP
*&---------------------------------------------------------------------*
*&      Form  POP_UP_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pop_up_message  USING p_text
                     CHANGING p_answer.
  DATA : answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1 = p_text
            textline2 = p_text
            titel     = 'Personal cost plan Confirm Box'
       IMPORTING
            answer    = answer.
  MOVE : answer TO p_answer .
ENDFORM.                    " POP_UP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  data_delete_history
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_delete_history.

  DELETE FROM zthr_pcp00 WHERE zyear = w_zyear
                          AND zvers = w_zvers .

  DELETE FROM zthr_pcpxx WHERE zyear = w_zyear
                           AND zvers = w_zvers .

  DELETE FROM zthr_pcp05 WHERE zyear = w_zyear
                           AND zvers = w_zvers .

  DELETE FROM zthr_pcp06 WHERE zyear = w_zyear
                           AND zvers = w_zvers .

  DELETE FROM zthr_pcp07 WHERE zyear = w_zyear
                           AND zvers = w_zvers .

  DELETE FROM zthr_pcp08 WHERE zyear = w_zyear
                           AND zvers = w_zvers .

  DELETE FROM zthr_ahc01 WHERE zyear = w_zyear
                           AND zvers = w_zvers .


ENDFORM.                    " data_delete_history
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM authority_check.
  DATA : w_pernr LIKE sy-uname,
         l_zval1 LIKE zthr_pcp02-zval1,
         z_zval5 LIKE zthr_pcp02-zval5.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*       EXPORTING
*            INPUT  = SY-UNAME
*       IMPORTING
*            OUTPUT = W_PERNR.
*
  CONCATENATE sy-uname '%' INTO l_zval1.
*... test data (delete after go live)
*  W_PERNR = '00100104'.
*  L_ZVAL1 = '100104%'.
*
  CLEAR zthr_pcp02.
  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1005'
                      AND zval1 LIKE l_zval1.
  IF sy-subrc = 0.
*   W_MASTER = ZTHR_PCP02-ZVAL1.
    CLEAR z_zval5.
    SELECT zval5 INTO z_zval5
      FROM zthr_pcp02 UP TO 1 ROWS
         WHERE zmodl = '02'
           AND zgrup = '1030'
           AND zval1 = w_zvers+1(2)
           AND zval2 = w_zyear.
    ENDSELECT.
    IF z_zval5 IS INITIAL.
      MESSAGE e001 WITH text-001.
    ENDIF.
  ELSE.
    MESSAGE e013.
  ENDIF.
ENDFORM.                    " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  DATA_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_selection.
  RANGES : r_kostl FOR csks-kostl,
           r_zperg FOR zthr_pcp00-zperg,
           r_zsubg FOR zthr_pcp00-zsubg,
           r_zobjc FOR zthr_pcp00-zobjc.

  DATA : ls_pcp LIKE LINE OF it_pcp,
         l_index TYPE sy-tabix.
  DATA : z_act(12),num(2) TYPE n,f_num(2) TYPE n.
  FIELD-SYMBOLS : <act> TYPE ANY.

  IF zthr_pcp00-zperg <> '' .
    r_zperg-option = 'EQ'.
    r_zperg-sign   = 'I'.
    r_zperg-low    = zthr_pcp00-zperg .
    APPEND r_zperg.
  ENDIF.
  IF zthr_pcp00-zsubg <> '' .
    r_zsubg-option = 'EQ'.
    r_zsubg-sign   = 'I'.
    r_zsubg-low    = zthr_pcp00-zsubg.
    APPEND r_zsubg.
  ENDIF.
  IF zthr_pcp00-zobjc <> '' .
    r_zobjc-option = 'EQ'.
    r_zobjc-sign   = 'I'.
    r_zobjc-low    = zthr_pcp00-zobjc.
    APPEND r_zobjc.
  ENDIF.
  IF zthr_pcp00-zcost <> ''.
    r_kostl-option = 'EQ'.
    r_kostl-sign   = 'I'.
    r_kostl-low    = zthr_pcp00-zcost.
    APPEND r_kostl.
  ENDIF.
  REFRESH it_pcp.

  IF w_zmons IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_pcp
             FROM zthr_pcp00
             WHERE zyear = w_zyear
               AND zvers = w_zvers
               AND zpera = w_werks
               AND zcost IN r_kostl
               AND zperg IN r_zperg
               AND zsubg IN r_zsubg
               AND zobjc IN r_zobjc.

  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_pcp
             FROM zthr_pcp00
             WHERE zyear = w_zyear
               AND zvers = w_zvers
               AND zpera = w_werks
               AND zmons = w_zmons
               AND zcost IN r_kostl
               AND zperg IN r_zperg
               AND zsubg IN r_zsubg
               AND zobjc IN r_zobjc.
  ENDIF.
  IF sy-subrc EQ 0.
  ENDIF.

  CLEAR l_index.
  LOOP AT it_pcp INTO ls_pcp .
    l_index = l_index + 1.
    SELECT SINGLE ktext INTO ls_pcp-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = ls_pcp-zcost
                  AND datbi = '99991231'.
    IF sy-subrc <> 0.
      SELECT SINGLE zval1 INTO ls_pcp-ktext "read cost center text
          FROM zthr_pcp02
           WHERE zmodl EQ '02'
             AND ( zgrup = '1260' OR zgrup = '1270' )
             AND zctxt EQ  ls_pcp-zcost.
    ENDIF.

    SELECT SINGLE short INTO ls_pcp-short
      FROM hrp1000 WHERE plvar = '01'
                     AND otype = 'C'
                     AND objid = ls_pcp-zobjc
                     AND istat = '1'
                     AND endda = '99991231'
                     AND langu = sy-langu.
    f_num = 1.
    DO 5 TIMES.
      CONCATENATE 'LS_PCP-ACT' f_num INTO z_act.
      ASSIGN (z_act) TO <act>.
      IF num <> 10.
        <act> = <act> *  ls_pcp-zhedc.
      ENDIF.
      f_num = f_num + 1.
    ENDDO.

    num = 8.
    DO 13 TIMES.
      CONCATENATE 'LS_PCP-ACT' num INTO z_act.
      ASSIGN (z_act) TO <act>.
      IF num <> 10.
        <act> = <act> *  ls_pcp-zhedc.
      ENDIF.
      num = num + 1.
    ENDDO.

    ls_pcp-act22 = ls_pcp-act22 * ls_pcp-zhedc.

    ls_pcp-zsaly    = ls_pcp-act01 + ls_pcp-act02 + ls_pcp-act03
     + ls_pcp-act04 + ls_pcp-act05 + ls_pcp-act06 + ls_pcp-act07
     + ls_pcp-act08 + ls_pcp-act09 + ls_pcp-act10 + ls_pcp-act11
     + ls_pcp-act12 + ls_pcp-act13 + ls_pcp-act14 + ls_pcp-act15
     + ls_pcp-act16 + ls_pcp-act17 + ls_pcp-act18 + ls_pcp-act19
     + ls_pcp-act20 + ls_pcp-act22.

    MODIFY it_pcp FROM ls_pcp INDEX l_index.
    CLEAR:f_num, num, ls_pcp.
  ENDLOOP.
*data sorting
  PERFORM data_sorting.

ENDFORM.                    " DATA_SELECTION
*&---------------------------------------------------------------------*
*&      Form  601330_cc_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM 601330_cc_code.

  CLEAR :  r_1170[],  r_1170.
  CLEAR :  r_1180[],  r_1180.
  CLEAR zthr_pcp02.

  SELECT zval1 zval4
      INTO (zthr_pcp02-zval1, zthr_pcp02-zval4)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1170' .

    r_1170-sign   = 'I'.
    r_1170-option = 'EQ'.
    MOVE zthr_pcp02-zval1 TO r_1170-low.

    PERFORM numeric_check USING r_1170-low
                          CHANGING p_type.

    IF p_type  NE 'CHAR'.
      UNPACK r_1170-low TO r_1170-low.
    ENDIF.
    APPEND r_1170.  CLEAR r_1170.

  ENDSELECT.

  SELECT zval1 zval4
      INTO (zthr_pcp02-zval1, zthr_pcp02-zval4)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1180' .

    r_1180-sign   = 'I'.
    r_1180-option = 'EQ'.
    MOVE zthr_pcp02-zval1 TO r_1180-low.
    UNPACK r_1180-low TO r_1180-low.
    APPEND r_1180.  CLEAR r_1180.

  ENDSELECT.

  DELETE r_1180 WHERE sign = ''.
  DELETE r_1170  WHERE sign = ''.
  IF r_1180[] IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Not Define 02-1180  Code'.
  ENDIF.
  IF r_1170[] IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Not Define 02-1170  Code'.
  ENDIF.

* 601330  insurance
  CLEAR zthr_pcp02.
  SELECT  zval1 zcode INTO (zthr_pcp02-zval1, zthr_pcp02-zcode)
     FROM zthr_pcp02
     WHERE zmodl = '02'
       AND zgrup = '1175' .
    MOVE : zthr_pcp02-zcode TO it_1175-zcode,
           zthr_pcp02-zval1 TO it_1175-zval1.

    APPEND it_1175. CLEAR it_1175.
  ENDSELECT .

  IF it_1175[] IS INITIAL.
    MESSAGE e001 WITH
           '(2-1175- )  CODE   not defind'.
  ENDIF.

ENDFORM.                    " 601330_cc_code
*&---------------------------------------------------------------------*
*&      Form  perform_DATETIME_DIFFERENCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_PAYRT_INTER_VERSC_FPEND  text
*      -->P_WA_PAYRT_INTER_VERSC_FPBEG  text
*      -->P_WA_DIFFDAY  text
*----------------------------------------------------------------------*
FORM perform_datetime_difference USING    p_fpend TYPE d
                                          p_fpbeg TYPE d
                                          p_day TYPE p.

  CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
    EXPORTING
      date1                  = p_fpend
      time1                  = sy-uzeit
      date2                  = p_fpbeg
      time2                  = sy-uzeit
   IMPORTING
     datediff               = p_day
*   TIMEDIFF               =
*   EARLIEST               =
   EXCEPTIONS
     invalid_datetime       = 1
     OTHERS                 = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " perform_DATETIME_DIFFERENCE
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_NORMAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_normal.
*  IF it_pcp00-zperg = '9' AND it_pcp00-zsubg = 'U2'.
**   601100
*    it_pcp00-act01 = it_pcp00-mthly * it_pcp00-zhedc.
*  ELSEIF ( it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U2' ) OR
*           ( it_pcp00-zperg = '1' AND it_pcp00-zsubg = 'U3' ).
**601110
*    it_pcp00-act02 = it_pcp00-mthly * it_pcp00-zhedc.
*  ELSE.
**  601120
**    IT_PCP00-ACT03 = IT_PCP00-MTHLY * IT_PCP00-ZHEDC.
*  ENDIF.
*
ENDFORM.                    " CALCULATION_NORMAL
*&---------------------------------------------------------------------*
*&      Form  caculation_holiy_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_AHDAY  text
*----------------------------------------------------------------------*
FORM caculation_holiy_day
                            USING p_stell LIKE pa0001-stell
                            CHANGING p_ahday pp_ahday.
  DATA : l_cday TYPE i.
  CLEAR l_cday .

  IF p_stell IN r_paid .
    l_cday = p_ahday + 15.
  ELSE.
    l_cday = p_ahday + 10.
  ENDIF.

  pp_ahday = l_cday / 12.

ENDFORM.                    " caculation_holiy_day
*&---------------------------------------------------------------------*
*&      Form  JOB_CODE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_code_selection.
  CLEAR :  r_jobcode[],  r_jobcode.
  CLEAR:  r_otcode[],   r_otcode.
  CLEAR zthr_pcp02.

  SELECT zval1 zval4
      INTO (zthr_pcp02-zval1, zthr_pcp02-zval4)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1040' .

    r_jobcode-sign   = 'I'.
    r_jobcode-option = 'EQ'.
    MOVE zthr_pcp02-zval1 TO r_jobcode-low.
    CONDENSE zthr_pcp02-zval4.
    IF zthr_pcp02-zval4 = 'OT'.
      r_otcode-sign   = 'I'.
      r_otcode-option = 'EQ'.
      MOVE zthr_pcp02-zval1 TO r_otcode-low.
    ENDIF.

    APPEND r_jobcode.  CLEAR r_jobcode.
    APPEND r_otcode.  CLEAR r_otcode.

  ENDSELECT.
  DELETE r_jobcode WHERE sign = ''.
  DELETE r_otcode  WHERE sign = ''.
  IF r_jobcode[] IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Not Define 02-1040 Job Code'.
  ENDIF.
  IF r_otcode[] IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Not Define 02-1040 OT Code'.
  ENDIF.
* OT JOB CODE

ENDFORM.                    " JOB_CODE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  CALCURATE_MONTHLY_SALARY_zz
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_BEGDA  text
*----------------------------------------------------------------------*
FORM calcurate_monthly_salary_zz USING    t_begda.

  DATA : wa_diffday TYPE i.
  DATA : l_zval1     TYPE p DECIMALS 2.
  DATA : l_bdate TYPE d,
         l_edate TYPE d.

  DATA : ov_molga TYPE molga,
         it_rgdir TYPE hrpy_tt_rgdir.
  DATA : ls_rgdir LIKE LINE OF it_rgdir.
  DATA : lt_result TYPE pay99_result.
  DATA : ls_rt TYPE pc207.
  CLEAR:it_payrt.

  TABLES : t500l.


****  pay result .
  CALL FUNCTION 'CA_CU_READ_RGDIR_NEW'
       EXPORTING
            persnr          = pa0001-pernr
       IMPORTING
            molga           = ov_molga
       TABLES
            cu_ca_rgdir     = it_rgdir
       EXCEPTIONS
            no_record_found = 1
            OTHERS          = 2.

  DELETE it_rgdir WHERE
    NOT ( fpend BETWEEN w_begda and w_endda ) OR
    payty = 'A'.

  SELECT SINGLE relid INTO t500l-relid
                FROM t500l
                WHERE molga = ov_molga.


*
  CLEAR: w_betrg, w_wrkmo, w_zhouy, w_bonus,w_nhouy,w_401k ,
         w_total , w_ansal, w_nhce,w_anhce.
  CLEAR: l_bdate , l_edate.
  CLEAR : it_fpper, it_fpper[].

  LOOP AT it_rgdir INTO ls_rgdir.
    IF ls_rgdir-fpbeg <  t_begda AND  " middle entering
       ls_rgdir-fpend >  t_begda.
      CONTINUE.
    ELSEIF ls_rgdir-fpbeg < w_endda AND
           ls_rgdir-fpbeg > w_endda.
      CONTINUE.
    ELSEIF ls_rgdir-srtza EQ 'P'.
      CONTINUE.
    ELSE.

      CHECK ls_rgdir-fpbeg >= w_begda .
      PERFORM begin_end_day_cal USING  ls_rgdir-fpbeg
                                       ls_rgdir-fpend
                                CHANGING l_bdate
                                         l_edate .

      PERFORM read_payroll_result USING t500l-relid
                                        pa0001-pernr
                                        ls_rgdir-seqnr
                                        ls_rgdir-srtza
                                        ls_rgdir-fpper
                                        ls_rgdir-fpbeg.
    ENDIF.
  ENDLOOP.

  PERFORM salery_retroactive .
****** total amount caulation .
  PERFORM annual_salery_to_monthly  USING l_bdate
                                          l_edate .


ENDFORM.                    " CALCURATE_MONTHLY_SALARY_zz
*&---------------------------------------------------------------------*
*&      Form  PAY_INCRESE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pay_increse_rate.
* increase rate payment
*
  DATA : l_zval1 TYPE p DECIMALS 2.
  CLEAR : l_zval1, zthr_pcp02, w_inrate.

  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '2'
                      AND zgrup = '1060'
                      AND zcode = '10020'.
  MOVE zthr_pcp02-zval1 TO l_zval1.
  IF l_zval1 < 1.
    l_zval1 = l_zval1 + 1.
  ENDIF.

  MOVE : l_zval1 TO w_inrate.

ENDFORM.                    " PAY_INCRESE_RATE
*&---------------------------------------------------------------------*
*&      Form  ANNUAL_SALERY_TO_MONTHLY_SALER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM annual_salery_to_monthly USING p_bdate
                                    p_edate.

  IF w_wrkmo = 0.
    w_wrkmo = 1.
  ENDIF.

  w_total = w_betrg.
  CLEAR pa0008.
  SELECT SINGLE trfar trfgb trfgr trfst ansal bet01
            INTO (pa0008-trfar, pa0008-trfgb, pa0008-trfgr,
                  pa0008-trfst, pa0008-ansal, pa0008-bet01)
                 FROM pa0008
                 WHERE pernr = pa0001-pernr
                  AND endda = '99991231' .

  IF pa0001-persg = '9' AND pa0001-persk = 'U2'.
    it_calsl-kostl = pa0001-kostl.
    w_betrg = w_betrg / w_wrkmo.
*    W_ZHOUY =  W_BETRG / ( 2080  / 12 ) .
    w_nhouy = ( w_betrg * w_inrate ) / ( 2080  / 12 ) .
* new salary/hourly
    w_zhouy =  pa0008-ansal /  2080   .
    w_ansal = pa0008-ansal.

    it_calsl-zsenr = it_cpy00-zsenr.

  ELSEIF ( ( pa0001-persg = '1' AND pa0001-persk = 'U2' ) OR
               ( pa0001-persg = '1' AND pa0001-persk = 'U3' ) ).

    it_calsl-kostl = pa0001-kostl.
    it_calsl-zsenr = it_cpy00-zsenr.
    w_zhouy =  w_betrg / (  w_wrkmo * 80 )  .
    w_nhouy = w_zhouy *  w_inrate .
    w_betrg = ( w_betrg / (  w_wrkmo * 80 ) ) * ( 2080 / 12 ) .

** new salary/hourly

    w_zhouy =  pa0008-ansal /  2080   .
    w_ansal = pa0008-ansal.

  ELSE.
    it_calsl-kostl = pa0001-kostl.
    it_calsl-zsenr = it_cpy00-zsenr.
    w_nhouy = w_zhouy *  w_inrate .
    w_betrg = ( w_betrg / (  w_wrkmo * 80 ) ) * ( 2080 / 12 ) .

* new salary/hourly
    IF pa0001-abkrs = '11' AND pa0008-bet01 = 0.
      SELECT SINGLE betrg INTO t510-betrg
             FROM t510
             WHERE molga  = '10'
               AND trfar = pa0008-trfar
               AND trfgb = pa0008-trfgb
*             AND TRFKZ = PA0008-TRFKZ
               AND trfgr = pa0008-trfgr
               AND endda = '99991231' .
      w_zhouy =  t510-betrg    .
      w_ansal =  t510-betrg *  2080.
    ELSEIF pa0001-abkrs = '11' AND pa0008-bet01 <> 0.
      w_zhouy =  pa0008-bet01    .
      w_ansal =  pa0008-bet01 *  2080.
    ELSE.
      w_zhouy =  pa0008-ansal /  2080   .
      w_ansal = pa0008-ansal.
    ENDIF.

  ENDIF.


ENDFORM.                    " ANNUAL_SALERY_TO_MONTHLY_SALER
*&---------------------------------------------------------------------*
*&      Form  begin_end_day_cal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_RGDIR_FPBEG  text
*      -->P_LS_RGDIR_FPEND  text
*      <--P_L_BDATE  text
*      <--P_L_EDATE  text
*----------------------------------------------------------------------*
FORM begin_end_day_cal USING    p_fpbeg
                                p_fpend
                       CHANGING p_bdate
                                p_edate.

*   DATE CACULATION
  IF p_bdate IS INITIAL.
    p_bdate = p_fpbeg.
  ENDIF.

  IF p_edate IS INITIAL.
    p_edate = p_fpend.
  ENDIF.

  IF p_bdate >  p_fpbeg.
    p_bdate = p_fpbeg.
  ENDIF.

  IF p_edate < p_fpend.
    p_edate = p_fpend.
  ENDIF.

ENDFORM.                    " begin_end_day_cal
*&---------------------------------------------------------------------*
*&      Form  read_payroll_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T500L_RELID  text
*      -->P_PA0001_PERNR  text
*      -->P_LS_RGDIR_SEQNR  text
*----------------------------------------------------------------------*
FORM read_payroll_result USING    p_relid
                                  p_pernr
                                  p_seqnr
                                  p_srtza
                                  p_fpper
                                  p_fpbeg.

  DATA : lt_result TYPE pay99_result.
  DATA : ls_rt TYPE pc207.
  DATA : ls_crt LIKE LINE OF lt_result-inter-crt.

*  pay result get
  CLEAR : lt_result.

  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
       EXPORTING
            clusterid                    = p_relid
            employeenumber               = p_pernr
            sequencenumber               = p_seqnr
            read_only_international      = 'X'
       CHANGING
            payroll_result               = lt_result
       EXCEPTIONS
            illegal_isocode_or_clusterid = 1
            error_generating_import      = 2
            import_mismatch_error        = 3
            subpool_dir_full             = 4
            no_read_authority            = 5
            no_record_found              = 6
            versions_do_not_match        = 7
            OTHERS                       = 8.

*... G/L account - 601100
  IF pa0001-persg = '9' AND pa0001-persk = 'U2'.
    IF p_srtza EQ 'A'.
      w_wrkmo = w_wrkmo + 1.
    ENDIF.

    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_month.
      READ TABLE it_wage WITH KEY val1 = ls_rt-lgart
                                  val2 = '601100' .
      CHECK it_wage-val3 > p_fpbeg .

      w_betrg = w_betrg + ls_rt-betrg.

      IF p_srtza EQ 'A'.
        MOVE :         ls_rt-betrg    TO it_fpper-betrg.
      ELSE.
        ls_rt-betrg = ls_rt-betrg * -1.
      ENDIF.
      MOVE : p_fpper        TO it_fpper-fpper,
             ls_rt-betrg    TO it_fpper-xetrg.
      COLLECT it_fpper. CLEAR it_fpper.

    ENDLOOP.

*... G/L account - 601110
  ELSEIF ( ( pa0001-persg = '1' AND pa0001-persk = 'U2' ) OR
           ( pa0001-persg = '1' AND pa0001-persk = 'U3' ) ).

    IF p_srtza EQ 'A'.
      w_wrkmo = w_wrkmo + 1.
    ENDIF.

*    total wage amount
    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_wekly.

      READ TABLE it_wage WITH KEY val1 = ls_rt-lgart
                                  val2 = '601110' .
      CHECK it_wage-val3 > p_fpbeg .

      w_betrg = w_betrg + wa_reslt-betrg.

      IF p_srtza EQ 'A'.
        MOVE :         ls_rt-betrg    TO it_fpper-betrg.
      ELSE.
        ls_rt-betrg = ls_rt-betrg * -1.
      ENDIF.
      MOVE : p_fpper        TO it_fpper-fpper,
             ls_rt-betrg    TO it_fpper-xetrg.
      COLLECT it_fpper. CLEAR it_fpper.

    ENDLOOP.
*Gross amount
*    LOOP AT lt_result-inter-crt INTO ls_crt WHERE lgart = '/102'.
    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart = '/102'.

      CHECK w_wrkmo <> 0 .
*      w_nhce =  ls_crt-betrg / w_wrkmo .
*      w_nhce = w_nhce * 26 .
      w_nhce = w_nhce + ls_rt-betrg .
    ENDLOOP.
*Actual amount
    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_w401.
      CHECK w_wrkmo <> 0 .
      READ TABLE lt_result-inter-rt INTO ls_rt WITH KEY  lgart = '/102'.
      IF sy-subrc = 0.
        w_anhce =  ls_rt-betrg .
        w_anhce = w_anhce + w_anhce .
      ENDIF.
    ENDLOOP.

*Actual 401k amount
    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_w401.
*      W_401K = W_401K + LS_RT-BETRG.
*      w_401k = w_401k + ABS( ls_rt-betrg ).
      w_401k = w_401k + ls_rt-betrg.
    ENDLOOP.

*... G/L account - 601120
  ELSE.
    IF p_srtza EQ 'A'.
      w_wrkmo = w_wrkmo + 1.
    ENDIF.

    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_houry.

      READ TABLE it_wage WITH KEY val1 = ls_rt-lgart
                                  val2 = '601120' .
      CHECK it_wage-val3 > p_fpbeg .

      IF p_srtza EQ 'A'.
        MOVE :         ls_rt-betrg    TO it_fpper-betrg.
      ELSE.
        ls_rt-betrg = ls_rt-betrg * -1.
      ENDIF.
      MOVE : p_fpper        TO it_fpper-fpper,
             ls_rt-betrg    TO it_fpper-xetrg.
      COLLECT it_fpper. CLEAR it_fpper.

      w_betrg = w_betrg + wa_reslt-betrg.
    ENDLOOP.

    LOOP AT lt_result-inter-rt INTO ls_crt WHERE lgart = '/102'.
      CHECK w_wrkmo <> 0 .
*      w_nhce =  ls_crt-betrg / w_wrkmo .
*      w_nhce = w_nhce * 26 .
      w_nhce = w_nhce +  ls_rt-betrg .
    ENDLOOP.
*Actual amount
    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_w401.
      CHECK w_wrkmo <> 0 .
      READ TABLE lt_result-inter-rt INTO ls_rt WITH KEY  lgart = '/102'.
      IF sy-subrc = 0.
        w_anhce =  ls_rt-betrg .
        w_anhce = w_anhce + w_anhce .
      ENDIF.
    ENDLOOP.

    LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_w401.
      w_401k = w_401k + ls_rt-betrg.
*      w_401k = w_401k + ABS( ls_rt-betrg ).
    ENDLOOP.

  ENDIF.
*... G/L account - 601200
  LOOP AT lt_result-inter-rt INTO ls_rt WHERE lgart IN r_bonus.
*    W_BONUS = W_BONUS + WA_RESLT-BETRG.
    w_bonus = w_bonus + ls_rt-betrg.
  ENDLOOP.


ENDFORM.                    " read_payroll_result
*&---------------------------------------------------------------------*
*&      Form  salery_retroactive
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salery_retroactive.
  DATA : lx LIKE it_fpper .

  CLEAR : lx.

  LOOP AT it_fpper .

    IF it_fpper-betrg <> it_fpper-xetrg .
      lx-xetrg = it_fpper-xetrg.
    ENDIF.
    lx-betrg  = lx-betrg + it_fpper-betrg - lx-xetrg.
    CLEAR lx-xetrg.

  ENDLOOP.

  w_betrg = lx-betrg.


ENDFORM.                    " salery_retroactive
*&---------------------------------------------------------------------*
*&      Form  WORK_EXTRA_PREMIUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM work_extra_premium.
  CLEAR zthr_pcp02.
  CLEAR : it_extr , it_extr[].

  SELECT zctxt zval1 INTO (zthr_pcp02-zctxt, zthr_pcp02-zval1)
         FROM zthr_pcp02
         WHERE zmodl = '02'
           AND zgrup = '1010' .
    MOVE : zthr_pcp02-zval1 TO it_extr-rate.
    CASE zthr_pcp02-zctxt.
      WHEN 'Weekday'.
        MOVE : '1' TO it_extr-zseqn.
      WHEN 'Overtime work'.
        MOVE : '2' TO it_extr-zseqn.
      WHEN 'Saturday'.
        MOVE : '3' TO it_extr-zseqn.
      WHEN 'Sunday'.
        MOVE : '4' TO it_extr-zseqn.
      WHEN 'Holiday'.
        MOVE : '5' TO it_extr-zseqn.
    ENDCASE.
    APPEND it_extr.
  ENDSELECT.

ENDFORM.                    " WORK_EXTRA_PREMIUM
*&---------------------------------------------------------------------*
*&      Form  original_hire_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM original_hire_date  USING p_begin .
  DATA : l_end   LIKE sy-datum,
         l_ahday TYPE i,
         l_begin LIKE sy-datum.

  DATA : l_code LIKE p0041-dar01.
  DATA : l_date LIKE p0041-dat01.

  CONCATENATE w_zyear '1231' INTO l_end.
  CLEAR l_begin .
*Original Hire Date
  SELECT SINGLE *
                FROM pa0041
                WHERE pernr = pa0001-pernr
                  AND endda = '99991231' .

  DO 12 TIMES VARYING l_code FROM pa0041-dar01 NEXT pa0041-dar02
              VARYING l_date FROM pa0041-dat01 NEXT pa0041-dat02 .
    IF l_code = 'Z1'.
      l_begin = l_date.
      EXIT.
    ENDIF.

  ENDDO.
  IF l_begin IS INITIAL.
    l_begin = p_begin .
  ENDIF.

* Defferience day mont year
  CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
       EXPORTING
            beg_da  = l_begin
            end_da  = l_end
       IMPORTING
            no_year = l_ahday.


  IF l_ahday = 0. l_ahday = 1. ENDIF.

  it_cpy00-zsenr = l_ahday.

ENDFORM.                    " original_hire_date
*&---------------------------------------------------------------------*
*&      Form  total_cal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM total_cal.
  it_pcp00-zsaly = it_pcp00-act01 + it_pcp00-act02 + it_pcp00-act03
                 + it_pcp00-act04 + it_pcp00-act05 + it_pcp00-act06
                 + it_pcp00-act07 + it_pcp00-act08 + it_pcp00-act09
                 + it_pcp00-act10 + it_pcp00-act11 + it_pcp00-act12
                 + it_pcp00-act13 + it_pcp00-act14 + it_pcp00-act15
                 + it_pcp00-act16 + it_pcp00-act17 + it_pcp00-act18
                 + it_pcp00-act19 + it_pcp00-act20 + it_pcp00-act21
                 + it_pcp00-act22 .
ENDFORM.                    " total_cal
*&---------------------------------------------------------------------*
*&      Form  JOB_Paid-Leave-Up-to-Manager
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_paid-leave-up-to-manager.
  CLEAR :  r_paid[],  r_paid.
  CLEAR zthr_pcp02.

  SELECT zval1 zval4
      INTO (zthr_pcp02-zval1, zthr_pcp02-zval4)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1080' .

    r_paid-sign   = 'I'.
    r_paid-option = 'EQ'.
    MOVE zthr_pcp02-zval1 TO r_paid-low.

    APPEND r_paid.  CLEAR r_paid.

  ENDSELECT.
  DELETE r_paid WHERE sign = ''.
  IF r_paid[] IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Not Define 02-1080 Job Code'.
  ENDIF.

ENDFORM.                    " JOB_Paid-Leave-Up-to-Manager
*&---------------------------------------------------------------------*
*&      Form  calculation_691350
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_691350.
  it_pcp00-act14 = ( it_pcp00-ansal / 12 ) * it_code-1350.
*                   *  it_pcp00-zhedc.
ENDFORM.                    " calculation_691350
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_691360
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_691360.

  it_pcp00-act15 = ( it_pcp00-ansal / 12 ) * it_code-1360.
*                   *  it_pcp00-zhedc.

ENDFORM.                    " CALCULATION_691360
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_691380
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_691380.
  IF it_pcp00-zperg NE '9'.
    it_pcp00-act16 = ( it_pcp00-ansal / 12 ) * it_code-1380.
  ELSE.
    CLEAR it_pcp00-act16.
  ENDIF.
ENDFORM.                    " CALCULATION_691380
*&---------------------------------------------------------------------*
*&      Form  CALCULATION_691390
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculation_691390.
  IF it_pcp00-zperg NE '9'.
    it_pcp00-act17 = ( it_pcp00-ansal / 12 ) * it_code-1390.
  ELSE.
    CLEAR it_pcp00-act17.
  ENDIF.
ENDFORM.                    " CALCULATION_691390
*&---------------------------------------------------------------------*
*&      Form  overtime_code_book_read_v1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM overtime_code_book_read_v1 USING p_mon p_zgroup.

  CLEAR : w_increse_rate1, w_increse_rate2.
  CLEAR : zthr_pcp02.
  SELECT SINGLE zval1 zval2
         INTO (zthr_pcp02-zval1,zthr_pcp02-zval2)
         FROM zthr_pcp02
         WHERE zmodl = '02'
          AND zgrup = '1060'
          AND zcode  = p_mon.
  CASE p_zgroup.
    WHEN '1040'."offical
      MOVE : zthr_pcp02-zval1 TO w_increse_rate1 .
      IF w_increse_rate1 < 1.
        w_increse_rate1 = w_increse_rate1 + 1 .
      ENDIF.

    WHEN '1050'."Production
      MOVE : zthr_pcp02-zval2 TO w_increse_rate1 .
      IF w_increse_rate1 < 1.
        w_increse_rate1 = w_increse_rate1 + 1 .
      ENDIF.
  ENDCASE.

ENDFORM.                    " overtime_code_book_read_v1
*&---------------------------------------------------------------------*
*&      Form  new_costcenter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM new_costcenter TABLES lt_pcp00 STRUCTURE it_pcp00.
  DATA : lt_pcp02 LIKE zthr_pcp02 ,
         lt_pcp00_temp LIKE lt_pcp00 OCCURS 0 WITH HEADER LINE,
         wt_pcp00 LIKE lt_pcp00.
  DATA : f_num(2) TYPE n.

  CLEAR :lt_pcp02,lt_pcp00.
  CLEAR f_num.

  SELECT * INTO  lt_pcp02  FROM zthr_pcp02
    WHERE zmodl EQ '02'
      AND zgrup EQ '1260'.
    IF sy-subrc = 0.
      CONDENSE lt_pcp02-zrmrk NO-GAPS.
      f_num = 1.
      DO 40 TIMES.
        SEARCH lt_pcp02-zrmrk FOR f_num.
        IF sy-subrc = 0.
          MOVE f_num TO  lt_pcp00-zmons.
          PERFORM new_data_append TABLES lt_pcp00
                                  USING  lt_pcp02 w_zyear w_zvers.
          f_num = f_num + 1.
        ELSE.
          f_num = f_num + 1.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDSELECT.

  lt_pcp00_temp[] = lt_pcp00[].
  REFRESH lt_pcp00.
  LOOP AT lt_pcp00_temp.
    MOVE-CORRESPONDING lt_pcp00_temp TO wt_pcp00.
    READ TABLE lt_pcp00 WITH KEY zyear = wt_pcp00-zyear
                                 zmons = wt_pcp00-zmons
                                 zvers = wt_pcp00-zvers
                                 zcost = wt_pcp00-zcost
                                 zpera = wt_pcp00-zpera
                                 zperg = wt_pcp00-zperg
                                 zsubg = wt_pcp00-zsubg
                                 zobjc = wt_pcp00-zobjc
                                 zsenr = wt_pcp00-zsenr.
    IF sy-subrc <> 0.
      INSERT wt_pcp00 INTO TABLE lt_pcp00.
    ELSE.
      wt_pcp00-zhedc =  wt_pcp00-zhedc + lt_pcp00-zhedc.
      lt_pcp00-zhedc =  wt_pcp00-zhedc.

      MODIFY lt_pcp00 TRANSPORTING  zhedc
       WHERE zyear = wt_pcp00-zyear
         AND zmons = wt_pcp00-zmons
         AND zvers = wt_pcp00-zvers
         AND zcost = wt_pcp00-zcost
         AND zpera = wt_pcp00-zpera
         AND zperg = wt_pcp00-zperg
         AND zsubg = wt_pcp00-zsubg
         AND zobjc = wt_pcp00-zobjc
         AND zsenr = wt_pcp00-zsenr.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " new_costcenter
*&---------------------------------------------------------------------*
*&      Form  new_data_append
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM new_data_append TABLES lt_pcp00 STRUCTURE it_pcp00
                     USING  lt_pcp02 LIKE zthr_pcp02
                            w_zyear LIKE w_zyear
                            w_zvers  LIKE w_zvers.

  DATA : l_act01  LIKE it_pcp00-act01,
         l_mthly  LIKE it_pcp00-mthly,
         l_houry  LIKE it_pcp00-houry,
         l_ancur  LIKE it_pcp00-ancur VALUE 'USD',
         l_header LIKE  lt_pcp00-zhedc.

  CLEAR l_header.
  lt_pcp00-zyear = w_zyear.
  lt_pcp00-zvers = w_zvers.
  lt_pcp00-zcost = lt_pcp02-zctxt.
  lt_pcp00-zpera = lt_pcp02-zval2(4).
  lt_pcp00-zperg = lt_pcp02-zval2+5(1).
  lt_pcp00-zsubg = lt_pcp02-zval2+7(2).
  lt_pcp00-zobjc = lt_pcp02-zval4.
  lt_pcp00-zsenr = '1'.
  lt_pcp00-zhedc = lt_pcp02-zval2+10(2).

  CLEAR : zthr_pcp02.

  SELECT SINGLE zval1 zval5 INTO
              (zthr_pcp02-zval1, zthr_pcp02-zval5)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1250'
                      AND zval2 = lt_pcp00-zperg
                      AND zval4 = lt_pcp00-zobjc.

  MOVE : zthr_pcp02-zval1 TO    l_act01,    " monthly
         zthr_pcp02-zval5 TO    l_mthly .   " annaly

  l_act01 = l_act01 .
  l_houry = l_mthly / 2080  .

  MOVE : l_houry TO lt_pcp00-houry ,
         l_act01 TO lt_pcp00-mthly ,
         l_act01 TO lt_pcp00-omthly ,
         l_ancur TO lt_pcp00-ancur.
  lt_pcp00-ansal =   l_mthly.

  IF lt_pcp00-zperg = '9' AND lt_pcp00-zsubg = 'U2'.
    lt_pcp00-act01 = lt_pcp00-mthly * lt_pcp00-zhedc .
  ELSEIF ( ( lt_pcp00-zperg = '1' AND lt_pcp00-zsubg = 'U2' ) OR
           ( lt_pcp00-zperg = '1' AND lt_pcp00-zsubg = 'U3' ) ).
    lt_pcp00-act02 = lt_pcp00-mthly * lt_pcp00-zhedc .
  ELSE.
    lt_pcp00-act03 = lt_pcp00-mthly * lt_pcp00-zhedc .
  ENDIF.

  lt_pcp00-erdat = sy-datum.
  GET TIME.
  lt_pcp00-erzet = sy-uzeit.
  lt_pcp00-ernam = sy-uname.
  APPEND lt_pcp00.CLEAR lt_pcp00.

ENDFORM.                    " new_data_append
*&---------------------------------------------------------------------*
*&      Form  numeric_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_1170_LOW  text
*      <--P_P_TYPE  text
*----------------------------------------------------------------------*
FORM numeric_check USING    p_value
                   CHANGING lp_type.

  CALL FUNCTION 'NUMERIC_CHECK'
       EXPORTING
            string_in = p_value
       IMPORTING
            htype     = lp_type.

ENDFORM.                    " numeric_check
*&---------------------------------------------------------------------*
*&      Form  data_sorting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_sorting.
  DATA : BEGIN OF it_sort OCCURS 0,
         val1 LIKE zthr_pcp02-zval1,
         val2 LIKE zthr_pcp02-zval2,
         END OF it_sort.

  DATA : BEGIN OF  gt_pcp OCCURS 0,
          zval1 TYPE i.
          INCLUDE STRUCTURE zshr_pcp00.
  DATA : END OF gt_pcp.
  DATA  :gs_pcp LIKE gt_pcp,
         ls_pcp LIKE LINE OF it_pcp,
         l_index TYPE sy-tabix.

  CLEAR : it_sort, it_sort[].
  SELECT zval1 zval2 INTO (it_sort-val1, it_sort-val2)
   FROM zthr_pcp02
   WHERE zmodl = '02'
     AND zgrup = '1240' .
    APPEND it_sort.
  ENDSELECT.

  LOOP AT it_pcp INTO ls_pcp.
    MOVE-CORRESPONDING ls_pcp TO gt_pcp.
    APPEND gt_pcp.
  ENDLOOP.

  REFRESH it_pcp.
  LOOP AT gt_pcp INTO gs_pcp.
* OBJID
    READ TABLE it_sort WITH KEY val2 = gs_pcp-zobjc.
    IF sy-subrc EQ 0 .
      MOVE it_sort-val1 TO gs_pcp-zval1.
      MODIFY gt_pcp FROM gs_pcp ."INDEX l_index.
    ENDIF.
  ENDLOOP.
*SORT IT_AHC01  ASCENDING  BY ZCOST .
  SORT gt_pcp BY zval1.
  SORT gt_pcp
                ASCENDING BY  zcost  zperg   DESCENDING
                                     zval1   ASCENDING
                                     zsenr   ASCENDING.
  LOOP AT gt_pcp INTO gs_pcp.
    MOVE-CORRESPONDING gs_pcp TO ls_pcp.
    APPEND ls_pcp TO it_pcp.
  ENDLOOP.

  REFRESH gt_pcp.
ENDFORM.                    " data_sorting
*&---------------------------------------------------------------------*
*&      Form  header_count
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header_count.
  CHECK it_pcp00-zhedc = 0.
  CLEAR :it_pcp00-zsaly,it_pcp00-act01,it_pcp00-act02,it_pcp00-act03,
         it_pcp00-act04,it_pcp00-act05,it_pcp00-act06,it_pcp00-act07,
         it_pcp00-act08,it_pcp00-act09,it_pcp00-act10,it_pcp00-act11,
         it_pcp00-act12,it_pcp00-act13,it_pcp00-act14,it_pcp00-act15,
         it_pcp00-act16,it_pcp00-act17,it_pcp00-act18,it_pcp00-act19,
         it_pcp00-act20,it_pcp00-act21,it_pcp00-act22,it_pcp00-omthly,
         it_pcp00-mthly,it_pcp00-houry,it_pcp00-ansal,it_pcp00-ancur.

ENDFORM.                    " header_count
*&---------------------------------------------------------------------*
*&      Form  make_basic_data_new_costcenter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_basic_data_add_new.
  DATA: l_begda     LIKE sy-datum,
        l_years     TYPE i,
        l_count(2)  TYPE n,
        l_zval1     TYPE p DECIMALS 2,
        l_entry     LIKE hida OCCURS 1 WITH HEADER LINE.
  DATA : wa_answer TYPE c,
         wa_title(50) TYPE c.
*  TABLES : pa0000.
  IF w_xyear IS INITIAL.
    MESSAGE e001(zmhr) WITH 'Input Source year'.
  ENDIF.
*
  IF w_zyear = space OR w_zvers = space.
    MESSAGE w001 WITH 'Please make a selection'.
    EXIT.
  ENDIF.
*
  CLEAR it_pcpxx. REFRESH it_pcpxx.
  CLEAR it_pcp00. REFRESH it_pcp00.
  CLEAR it_cpy00. REFRESH it_cpy00.
  CLEAR it_calsl. REFRESH it_calsl.

  wa_title = 'Caution: New data will be updated !'.
  PERFORM pop_up_message USING wa_title
                         CHANGING wa_answer .
  IF wa_answer <> 'J'.
    EXIT.
  ENDIF.

  CLEAR zthr_pcp00.
*add : New costs are added by new year business plan
  PERFORM new_costcenter_add TABLES it_pcp00.

  MODIFY zthr_pcp00 FROM TABLE it_pcp00.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s001 WITH 'Transaction was processed successfully'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s001 WITH 'Error during processing of Basic DB Creation'.
  ENDIF.

ENDFORM.                    " make_basic_data_new_costcenter
*&---------------------------------------------------------------------*
*&      Form  new_costcenter_add
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PCP00  text
*----------------------------------------------------------------------*
FORM new_costcenter_add TABLES lt_pcp00 STRUCTURE it_pcp00.
  DATA : lt_pcp02 LIKE zthr_pcp02 ,
         lt_pcp00_temp LIKE lt_pcp00 OCCURS 0 WITH HEADER LINE,
         wt_pcp00 LIKE lt_pcp00.
  DATA : f_num(2) TYPE n.

  CLEAR :lt_pcp02,lt_pcp00.
  CLEAR f_num.

  SELECT * INTO  lt_pcp02  FROM zthr_pcp02
    WHERE zmodl EQ '02'
      AND zgrup EQ '1270'.
    IF sy-subrc = 0.
      CONDENSE lt_pcp02-zrmrk NO-GAPS.
      f_num = 1.
      DO 40 TIMES.
        SEARCH lt_pcp02-zrmrk FOR f_num.
        IF sy-subrc = 0.
          MOVE f_num TO  lt_pcp00-zmons.
          PERFORM new_data_append TABLES lt_pcp00
                                  USING  lt_pcp02 w_zyear w_zvers.
          f_num = f_num + 1.
        ELSE.
          f_num = f_num + 1.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDSELECT.

  lt_pcp00_temp[] = lt_pcp00[].
  REFRESH lt_pcp00.
  LOOP AT lt_pcp00_temp.
    MOVE-CORRESPONDING lt_pcp00_temp TO wt_pcp00.
    READ TABLE lt_pcp00 WITH KEY zyear = wt_pcp00-zyear
                                 zmons = wt_pcp00-zmons
                                 zvers = wt_pcp00-zvers
                                 zcost = wt_pcp00-zcost
                                 zpera = wt_pcp00-zpera
                                 zperg = wt_pcp00-zperg
                                 zsubg = wt_pcp00-zsubg
                                 zobjc = wt_pcp00-zobjc
                                 zsenr = wt_pcp00-zsenr.
    IF sy-subrc <> 0.
      INSERT wt_pcp00 INTO TABLE lt_pcp00.
    ELSE.
      wt_pcp00-zhedc =  wt_pcp00-zhedc + lt_pcp00-zhedc.
      lt_pcp00-zhedc =  wt_pcp00-zhedc.

      MODIFY lt_pcp00 TRANSPORTING  zhedc
       WHERE zyear = wt_pcp00-zyear
         AND zmons = wt_pcp00-zmons
         AND zvers = wt_pcp00-zvers
         AND zcost = wt_pcp00-zcost
         AND zpera = wt_pcp00-zpera
         AND zperg = wt_pcp00-zperg
         AND zsubg = wt_pcp00-zsubg
         AND zobjc = wt_pcp00-zobjc
         AND zsenr = wt_pcp00-zsenr.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " new_costcenter_add
*&---------------------------------------------------------------------*
*&      Form  make_basic_data_delete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_basic_data_delete.
*  DATA : wa_answer TYPE c,
*         wa_title(50) TYPE c.
**  TABLES : pa0000.
*  IF w_xyear IS INITIAL.
*    MESSAGE e001(zmhr) WITH 'Input Source year'.
*  ENDIF.
**
*  IF w_zyear = space OR w_zvers = space.
*    MESSAGE w001 WITH 'Please make a selection'.
*    EXIT.
*  ENDIF.
**
*  wa_title = 'Caution: New data will be deleted !'.
*  PERFORM pop_up_message USING wa_title
*                         CHANGING wa_answer .
*  IF wa_answer <> 'J'.
*    EXIT.
*  ENDIF.
*  REFRESH it_del.
*  PERFORM get_delete_data.
*  PERFORM delete_table.

ENDFORM.                    " make_basic_data_delete
*&---------------------------------------------------------------------*
*&      Form  delete_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_table.
  LOOP AT it_del.
    DELETE FROM zthr_pcp00 WHERE zyear = w_zyear
                             AND zvers = w_zvers
                             AND zcost = it_del-zctxt.

    DELETE FROM zthr_pcpxx WHERE zyear = w_zyear
                             AND zvers = w_zvers
                             AND zcost = it_del-zctxt.

    DELETE FROM zthr_pcp05 WHERE zyear = w_zyear
                             AND zvers = w_zvers
                             AND zcost = it_del-zctxt.

    DELETE FROM zthr_pcp06 WHERE zyear = w_zyear
                             AND zvers = w_zvers ."@@@

    DELETE FROM zthr_pcp07 WHERE zyear = w_zyear
                             AND zvers = w_zvers
                             AND zcost = it_del-zctxt.

    DELETE FROM zthr_pcp08 WHERE zyear = w_zyear
                             AND zvers = w_zvers
                              AND zcost = it_del-zctxt.

    DELETE FROM zthr_ahc01 WHERE zyear = w_zyear
                             AND zvers = w_zvers
                             AND zcost = it_del-zctxt.
  ENDLOOP.
ENDFORM.                    " delete_table
*&---------------------------------------------------------------------*
*&      Form  get_delete_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_delete_data.

  SELECT zctxt INTO CORRESPONDING FIELDS OF TABLE it_del
        FROM zthr_pcp02 WHERE zmodl = '02'
                         AND  zgrup = '1280'.

ENDFORM.                    " get_delete_data
