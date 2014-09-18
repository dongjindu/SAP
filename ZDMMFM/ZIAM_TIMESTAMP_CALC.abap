FUNCTION ZIAM_TIMESTAMP_CALC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IV_REFDATE) TYPE  ZIAM_REFDATE
*"     VALUE(IV_XBACKWARD) TYPE  ZIAM_BOOLE_D OPTIONAL
*"     VALUE(IV_DAYS) TYPE  INT4 DEFAULT '0'
*"     VALUE(IV_HOURS) TYPE  INT4 DEFAULT '0'
*"     VALUE(IV_MINUTES) TYPE  INT4 DEFAULT '0'
*"     VALUE(IV_SECONDS) TYPE  INT4 DEFAULT '0'
*"  EXPORTING
*"     VALUE(EV_DATE) TYPE  ZIAM_REFDATE
*"----------------------------------------------------------------------

  CONSTANTS:
    min_per_day TYPE i VALUE 1440,
    sec_per_day TYPE i VALUE 86400.

  DATA:
    sec_left        TYPE i,
    sec_total       TYPE i,
    additional_days TYPE i,
    lv_start_date   TYPE dats,
    lv_start_time   TYPE tims,
    lv_result_date  TYPE dats,
    lv_result_time  TYPE tims.

*------ timestamp to date/time ----------------------------------------
  CONVERT TIME STAMP iv_refdate TIME ZONE sy-zonlo INTO DATE
lv_start_date TIME lv_start_time.

  sec_total = ( iv_hours * 60 + iv_minutes ) * 60 + iv_seconds.
  additional_days = sec_total DIV sec_per_day.
  sec_left = sec_total MOD sec_per_day.

  IF iv_xbackward IS INITIAL.
*------ start date plus delta -----------------------------------------
    lv_result_time = lv_start_time + sec_left.
    IF lv_result_time <= lv_start_time AND sec_left > 0. "really <= !
      ADD 1 TO additional_days.
    ENDIF.

    lv_result_date = lv_start_date + iv_days + additional_days.

  ELSE.
*------ start date minus delta ----------------------------------------
    lv_result_time = lv_start_time - sec_left.
    IF lv_result_time > lv_start_time AND sec_left > 0. "really greater
      ADD 1 TO additional_days.
    ENDIF.

    lv_result_date = lv_start_date - iv_days - additional_days.

  ENDIF.

*------ date/time to timestamp ----------------------------------------
  CONVERT DATE lv_result_date TIME lv_result_time INTO TIME STAMP
ev_date TIME ZONE sy-zonlo.





ENDFUNCTION.
