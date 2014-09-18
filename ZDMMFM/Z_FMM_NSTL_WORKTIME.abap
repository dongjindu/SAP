FUNCTION z_fmm_nstl_worktime.
*"----------------------------------------------------------------------
*"*"Global interface:
*"  IMPORTING
*"     VALUE(I_DATUM) LIKE  SY-DATUM
*"     VALUE(I_ARBPL) LIKE  CRHD-ARBPL DEFAULT 'T'
*"  TABLES
*"      T_NSTL_WORKTIME STRUCTURE  ZSMM_NSTL_WORKTIME OPTIONAL
*"----------------------------------------------------------------------

**---
  CLEAR : it_1t, it_1t[], t_nstl_worktime, t_nstl_worktime[].
  clear : w_arbpl.

  move: i_arbpl to w_arbpl.

  PERFORM get_calendar_id.
  PERFORM get_working_day.
  PERFORM get_work_time_per_day.
  PERFORM get_break_time_per_day.
  PERFORM get_1t_time_info.

**---
  LOOP AT it_1t.
    MOVE-CORRESPONDING it_1t TO t_nstl_worktime.
    APPEND t_nstl_worktime.
  ENDLOOP.

ENDFUNCTION.
