FUNCTION z_ffi_get_cash_flow.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(IOPTION)
*"     REFERENCE(ILCURR) OPTIONAL
*"     REFERENCE(IGROUP) LIKE  T038-GLIED OPTIONAL
*"     REFERENCE(IORIGN) LIKE  T036-ORIGN OPTIONAL
*"     REFERENCE(IWAERS) LIKE  T001-WAERS OPTIONAL
*"     REFERENCE(IAVRGE) OPTIONAL
*"     REFERENCE(IWOVER) OPTIONAL
*"     REFERENCE(IALSEL) OPTIONAL
*"     REFERENCE(IPREAD) OPTIONAL
*"     REFERENCE(IFUNC) OPTIONAL
*"  EXPORTING
*"     REFERENCE(EWRKDAY) TYPE  I
*"  TABLES
*"      TBUKRS
*"      TDATUM
*"      TDISPW OPTIONAL
*"      THKONT OPTIONAL
*"      FTAB01 OPTIONAL
*"      FTAB02 OPTIONAL
*"      FTAB03 OPTIONAL
*"      FTAB04 OPTIONAL
*"      FTAB05 OPTIONAL
*"      FTAB06 OPTIONAL
*"      FWEEK OPTIONAL
*"----------------------------------------------------------------------
*  IOPTION
*  01 : Call ZRFIT07 -> cash flow report(actual) => not used
*  02 : Call ZRFIT07 -> cash flow report(plan-actual)
*  03 : Call ZRFIT02 - [monthly]Actual -> cash flow report
*  04 : Call ZRFIT02 - [weekly]Actual -> cash flow report
*  05 : Call ZRFIT02 - [monthly]Working plan -> cash flow report
*  06 : Call ZRFIT02 - [weekly]Working plan -> cash flow report
*  07 : Call ZRFIT02 - [monthly]Freezing plan -> cash flow report
*  08 : Call ZRFIT02 - [weekly]Freezing plan -> cash flow report
*  09 : Call ZRFIT02 - [monthly]Frozen plan -> cash flow report
*  10 : Call ZRFIT02 - [weekly]Frozen plan -> cash flow report
*
*  IALSEL(Call ZRFIT07) - actual selection date
*  1 : current date - current date actual data
*  2 : previous month - previous month actual data
*  3 : previous year - previous year actual data
*
*  IFUNC(Call ZRFIT07, ZRFIT02) : two meaning
*  1) working plan vs. actual => determine pay date flag(plan)
*    0 : distribution rule 'X', payment cycle 'X'
*    1 : distribution rule 'O', payment cycle 'X'
*    2 : distribution rule 'X', payment cycle 'O'
*    3 : distribution rule 'O', payment cycle 'O'
*  2) ZRFIT07 > previous year actual vs. current year actual
*    X : Report format change(actual: prev. amt vs. current amt)
*"----------------------------------------------------------------------
  s_bukrs[] = tbukrs[].
  s_datum[] = tdatum[].
  s_dispw[] = tdispw[].
  s_hkont[] = thkont[].
  p_lcurr   = ilcurr.
  p_glied   = igroup.
* p_orign   = iorign.
  p_avrge   = iavrge.
  sv_waers  = iwaers.
  sv_option = ioption.
  sv_wover  = iwover.
  sv_psel   = ialsel.  "actual selection date
  sv_pread  = ipread.  "plan cbo read option
  sv_func   = ifunc.   "plan => distribution, payment cycle..

  clear : it_cmal03[], it_cmal03, it_cmal02[], it_cmal02.

  IF sv_pread EQ 'X'.
    it_cmal03[] = ftab01[].  "saved plan data
  ENDIF.

  CLEAR: wtab[], wtab.

* BREAK-POINT.
  PERFORM get_account TABLES r_bank icmap.
  PERFORM set_date.

**********************************************************************
*    CM Actual data
**********************************************************************
  CASE sv_option.
    WHEN '01'. "call zrfit07 => not used
      PERFORM get_cm_actual TABLES s_dat01.
      PERFORM make_actual_flow_cm TABLES s_datum.  "<== it_cmal01
      PERFORM append_balance_glt0 TABLES s_datum s_gjahr.

    WHEN '02'. "call zrfit07
*----> if sv_func = 'X', 'Actual' previous year vs. current year
      IF sv_func EQ 'X'.
        INSERT LINES OF pv_date INTO TABLE s_datum.
        PERFORM get_cm_actual TABLES s_datum.
        PERFORM make_actual_flow_cm TABLES s_datum.  "<== it_cmal01

        READ TABLE pv_date INDEX 1.
        DELETE s_datum WHERE low EQ pv_date-low.
        PERFORM append_balance_glt0 TABLES s_datum s_gjahr.

      ELSE.
        IF sv_psel EQ '1'.  "actual : current date
          PERFORM get_cm_actual TABLES s_datum.
          PERFORM make_actual_flow_cm TABLES s_datum.  "<== it_cmal01
          PERFORM append_balance_glt0 TABLES s_datum s_gjahr.
        ELSE.               "actual : previous month/year actual
          PERFORM get_cm_actual TABLES pv_date.
          PERFORM make_actual_flow_cm TABLES pv_date.  "<== it_cmal01
          PERFORM append_balance_glt0 TABLES pv_date pv_gjahr.
        ENDIF.
**********************************************************************
*    CM Plan data
**********************************************************************
        IF sv_pread EQ space. "if 'X', read plan CBO..
          PERFORM before_setting.
          IF NOT grping01[] IS INITIAL OR NOT grping02[] IS INITIAL.
            PERFORM get_flowtype.

            PERFORM get_fdes.
            PERFORM get_fdt1.
            PERFORM get_fdsr.
            PERFORM get_fdsb.
*&------Added to get details w.r.t MM documents
            perform get_fdm1.
            PERFORM mapping_group.
            PERFORM mapping_account.

            CASE sv_func.
              WHEN '0'.   "don't adjust

              WHEN '1'.   "only distribution rule
                PERFORM distribution_bylevel.
              WHEN '2'.   "only payment cycle
                PERFORM payment_cycle_bygroup.
              WHEN '3'.   "all adjust
                PERFORM distribution_bylevel.
                PERFORM payment_cycle_bygroup.
            ENDCASE.
            DELETE it_fdes01 WHERE NOT datum IN s_datum.

            PERFORM make_plan_flow_cm TABLES s_datum.    "<== it_cmal03
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '03' OR '04'.  "call zrfit02 - actuals montly/weekly
      PERFORM get_cm_actual TABLES s_datum.
      PERFORM make_actual_flow_cm TABLES s_datum.  "<== it_cmal04
*     SORT s_datum BY low DESCENDING.
      PERFORM append_balance_glt0 TABLES s_datum s_gjahr.

    WHEN '05' OR '06'.  "call zrfit02 - working plan monthly/weekly
      PERFORM before_setting.
      CHECK NOT grping01[] IS INITIAL OR NOT grping02[] IS INITIAL.
      PERFORM get_source TABLES wtab.
      CHECK NOT grping01[] IS INITIAL.
      PERFORM get_flowtype.

      PERFORM get_fdes.
      PERFORM get_fdt1.
      PERFORM get_fdsr.
      PERFORM get_fdsb.
      PERFORM mapping_group.
      PERFORM mapping_account.

      CASE sv_func.
        WHEN '0'.   "don't adjust

        WHEN '1'.   "only distribution rule
          PERFORM distribution_bylevel.
        WHEN '2'.   "only payment cycle
          PERFORM payment_cycle_bygroup.
        WHEN '3'.   "all adjust
          PERFORM distribution_bylevel.
          PERFORM payment_cycle_bygroup.
      ENDCASE.
      DELETE it_fdes01 WHERE NOT datum IN s_datum.

*     PERFORM freezing_wrk_plan.
*     PERFORM get_fdes_freezing.
      PERFORM make_plan_flow_cm TABLES s_datum.    "<== it_cmal03

    WHEN '07' OR '08'.  "call zrfit02 - freezing plan monthly/weekly
      PERFORM get_fdes_freezing.
      PERFORM make_plan_flow_cm TABLES s_datum.    "<== it_cmal03

    WHEN '09' OR '10'.  "call zrfit02 - frozen plan monthly/weekly
      PERFORM get_fdes_frozen.
      PERFORM make_plan_flow_cm TABLES s_datum.    "<== it_cmal03
  ENDCASE.

  CASE sv_option.
**********************************************************************
*    call ZRFIT07 - not used
**********************************************************************
    WHEN '01'. "call zrfit07
      IF it_cmal01[] IS INITIAL.
        MESSAGE s001. EXIT.
      ENDIF.
*.....calculate daily average
      IF p_avrge EQ 'X'.
        LOOP AT it_cmal01.
          PERFORM check_wrkday USING it_cmal01-amount it_cmal01-sumamt
                                     sv_wrkday.
          MODIFY it_cmal01.  CLEAR it_cmal01.
        ENDLOOP.
      ENDIF.
      ftab01[] = it_cmal01[].
**********************************************************************
*    call ZRFIT07 - daily cash flow report(plan/actual)
**********************************************************************
    WHEN '02'. "call zrfit07
*.....calculate daily average
      IF p_avrge EQ 'X'.
        IF sv_psel EQ '1'.
          LOOP AT it_cmal01.
           PERFORM check_wrkday USING it_cmal01-amount it_cmal01-sumamt
                                        sv_wrkday.  "actuals working day
            MODIFY it_cmal01.  CLEAR it_cmal01.
          ENDLOOP.
        ELSE.
          LOOP AT it_cmal01.
           PERFORM check_wrkday USING it_cmal01-amount it_cmal01-sumamt
                                        pv_wrkday.  "actuals working day
            MODIFY it_cmal01.  CLEAR it_cmal01.
          ENDLOOP.
        ENDIF.
        LOOP AT it_cmal03.
          PERFORM check_wrkday USING it_cmal03-amount it_cmal03-sumamt
                                     sv_wrkday.  "plan working day
          MODIFY it_cmal03.  CLEAR it_cmal03.
        ENDLOOP.
      ENDIF.

      CASE sv_func.
        WHEN 'X'.
          LOOP AT it_cmal01.  "prev.year vs. current actual data
            MOVE-CORRESPONDING it_cmal01 TO it_cmal03.
            COLLECT it_cmal03. CLEAR it_cmal03.
          ENDLOOP.
        WHEN OTHERS.
          LOOP AT it_cmal01.  "actual data
            MOVE-CORRESPONDING it_cmal01 TO it_cmal03.
            it_cmal03-sumamt = it_cmal01-amount.
            it_cmal03-amount = ''.
            COLLECT it_cmal03. CLEAR it_cmal03.
          ENDLOOP.
      ENDCASE.
*     DELETE it_cmal03 WHERE amount = 0 AND sumamt = 0.

      IF it_cmal03[] IS INITIAL.
        MESSAGE s001. EXIT.
      ENDIF.

*-----> list include initial amount..
      PERFORM append_initial_group_daily.
      DELETE it_cmal03 WHERE sumflg EQ space
                       AND   fstflg EQ space
                       AND   scdflg EQ space.
      ftab01[] = it_cmal03[].
**********************************************************************
*    call ZRFIT02 - actual/plan
**********************************************************************
    WHEN '03' OR '04'. "call zrfit02-actual(monthly/weekly)
*      DELETE it_cmal04 WHERE mamt01 = 0
*                       AND   mamt02 = 0
*                       AND   mamt03 = 0
*                       AND   mamt04 = 0
*                       AND   mamt05 = 0
*                       AND   mamt06 = 0
*                       AND   mamt07 = 0
*                       AND   mamt08 = 0
*                       AND   mamt09 = 0
*                       AND   mamt10 = 0
*                       AND   mamt11 = 0
*                       AND   mamt12 = 0.
*-----> list include initial amount..
      PERFORM append_initial_group_monthly.
      DELETE it_cmal04 WHERE sumflg EQ space
                       AND   fstflg EQ space
                       AND   scdflg EQ space.

      ftab01[] = it_cmal04[].
    WHEN '05' OR '06' OR
         '07' OR '08' OR
         '09' OR '10'. "call zrfit02-plan(monthly/weekly)
*-----> list include initial amount..
      PERFORM append_initial_group_plan.
      DELETE it_plan01 WHERE sumflg EQ space
                       AND   fstflg EQ space
                       AND   scdflg EQ space.

      ftab01[] = it_plan01[].
  ENDCASE.

  ftab02[] = icmap[].
  ftab03[] = icmal[].
  ftab04[] = it_fdes01[].
  ftab05[] = it_fdes01_d[].
  ftab06[] = it_fdm1.
  fweek[]  = s_week[].
  ewrkday  = sv_wrkday.
ENDFUNCTION.
