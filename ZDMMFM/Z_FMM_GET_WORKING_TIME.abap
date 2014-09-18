FUNCTION z_fmm_get_working_time.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATUM) LIKE  SY-DATUM
*"     REFERENCE(I_DAY) TYPE  I
*"     REFERENCE(I_ARBPL) LIKE  CRHD-ARBPL
*"  EXPORTING
*"     REFERENCE(E_DATE_CURR) LIKE  SY-DATUM
*"     REFERENCE(E_TPROG_CURR) LIKE  KAPA-TPROG
*"     REFERENCE(E_DATE_NEXT) LIKE  SY-DATUM
*"     REFERENCE(E_TPROG_NEXT) LIKE  KAPA-TPROG
*"  TABLES
*"      T_WORKING_TIME STRUCTURE  ZSMM_WORKING_TIME OPTIONAL
*"      T_1T STRUCTURE  ZSMM_WORKING_TIME_FOR_1T OPTIONAL
*"  EXCEPTIONS
*"      CANNOT_READ_DAYNAME
*"      INCORRECT_SHIFT_INFO
*"      INCORRECT_CAPA_INFO
*"----------------------------------------------------------------------

**---
  CLEAR : it_1t, it_1t[], t_working_time, t_working_time[],
          it_work_date, it_work_date[],
          it_work_time, it_work_time[].
  CLEAR : w_arbpl, w_date_curr, w_tprog_curr, w_date_next, w_tprog_next.

  MOVE: i_arbpl TO w_arbpl.

  PERFORM get_calendar_id.
  PERFORM get_working_date USING i_datum i_day.
  PERFORM get_work_time_per_day.
  PERFORM get_break_time_per_day.
  PERFORM get_1t_time_info.

**----- Move 1T ITAB to output ITAB
  DATA: lw_begzt LIKE sy-uzeit,
        lw_endzt LIKE sy-uzeit.

  LOOP AT it_1t.
    CLEAR: t_working_time.

    MOVE: it_1t-datum  TO t_working_time-datum,
          it_1t-tprog  TO t_working_time-tprog,
          it_1t-index  TO t_working_time-index,
          it_1t-begzt  TO t_working_time-wofrm,
          it_1t-endzt  TO t_working_time-woend,
          it_1t-paplan TO t_working_time-paplan.

    MOVE: it_1t-begzt+8(6) TO lw_begzt,
          it_1t-endzt+8(6) TO lw_endzt.

    IF     lw_begzt < lw_endzt.
      t_working_time-wosec = lw_endzt - lw_begzt + 1.
    ELSEIF lw_begzt > lw_endzt.
      t_working_time-wosec = 86400 - ( lw_begzt - lw_endzt ) + 1.
    ENDIF.

    t_working_time-opsec = t_working_time-wosec.
    APPEND t_working_time.
  ENDLOOP.

*----- Append break time
  READ TABLE rg_paplan INDEX 1.
  IF sy-subrc NE 0.
    CLEAR: rg_paplan.
    MOVE: 'I'    TO rg_paplan-sign,
          'EQ'   TO rg_paplan-option,
          'Tlqk' TO rg_paplan-low.
    APPEND rg_paplan.
  ENDIF.

  SELECT * INTO TABLE it_break
           FROM tc37p
          WHERE schgrup =  w_mosid
            AND paplan  IN rg_paplan
            AND padauer <= c_padauer.

  SORT it_1t BY datum endzt DESCENDING.

  LOOP AT it_break.
    it_break-paubeg = it_break-paubeg + 1.
    LOOP AT t_working_time WHERE paplan = it_break-paplan.
      CLEAR: it_1t.
      READ TABLE it_1t WITH KEY datum = t_working_time-datum
                       BINARY SEARCH.

      IF it_break-paubeg >= '000000'         AND
         it_break-paubeg <= it_1t-endzt+8(6) AND
         it_1t-datum     NE it_1t-endzt(8).
        CONCATENATE it_1t-endzt(8) it_break-paubeg
               INTO t_working_time-brfrm.
      ELSE.
        CONCATENATE it_1t-datum it_break-paubeg
               INTO t_working_time-brfrm.
      ENDIF.

      IF it_break-pauend >= '000000'         AND
         it_break-pauend <= it_1t-endzt+8(6) AND
         it_1t-datum     NE it_1t-endzt(8).
        CONCATENATE it_1t-endzt(8) it_break-pauend
               INTO t_working_time-brend.
      ELSE.
        CONCATENATE it_1t-datum it_break-pauend
               INTO t_working_time-brend.
      ENDIF.

      CHECK t_working_time-wofrm <= t_working_time-brfrm AND
            t_working_time-woend >= t_working_time-brend.

*      CHECK t_working_time-wofrm+8(6) <= it_break-paubeg AND
*            t_working_time-woend+8(6) >= it_break-pauend.
*
*      CONCATENATE t_working_time-wofrm(8) it_break-paubeg
*             INTO t_working_time-brfrm.
*
*      CONCATENATE t_working_time-woend(8) it_break-pauend
*             INTO t_working_time-brend.

      IF     it_break-paubeg < it_break-pauend.
        t_working_time-brsec = it_break-pauend - it_break-paubeg + 1.
      ELSEIF it_break-paubeg > it_break-pauend.
        t_working_time-brsec = 86400 -
                              ( it_break-paubeg - it_break-pauend ) + 1.
      ENDIF.

      t_working_time-opsec = t_working_time-wosec -
                             t_working_time-brsec.
      MODIFY t_working_time.
    ENDLOOP.
  ENDLOOP.


  DATA: lw_index LIKE sy-index.
  LOOP AT t_working_time.
    AT NEW tprog.
      lw_index = lw_index + 1.
    ENDAT.
    MOVE: lw_index TO t_working_time-shidx.
    MODIFY t_working_time.
  ENDLOOP.

  t_1t[] = it_1t[].
  PERFORM get_current_next_shift.
  MOVE: w_date_curr  TO e_date_curr,
        w_tprog_curr TO e_tprog_curr,
        w_date_next  TO e_date_next,
        w_tprog_next TO e_tprog_next.

  SORT t_1t           BY datum begzt.
  SORT t_working_time BY datum wofrm.
ENDFUNCTION.
