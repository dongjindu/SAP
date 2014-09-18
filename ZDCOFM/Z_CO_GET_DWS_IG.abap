FUNCTION z_co_get_dws_ig .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(SCHKZ) TYPE  SCHKN
*"     VALUE(DATUM) TYPE  DATUM DEFAULT SY-DATUM
*"  EXPORTING
*"     VALUE(TPROG) TYPE  TPROG
*"  EXCEPTIONS
*"      NOT_FOUND_WORK_SCHEDULE_RULES
*"      INVALID_DATE
*"      NOT_FOUND_PERIOD_WORK_SCHEDULE
*"----------------------------------------------------------------------


  CLEAR : t508a, t551a..
  SELECT * FROM t508a WHERE "zeity EQ '1'
                            "mofid EQ 'U1'
                       schkz EQ schkz
                       AND endda >= datum
                       AND begda <= datum ORDER BY endda DESCENDING.
    EXIT.
  ENDSELECT.

  IF t508a-zmodn IS INITIAL.
    RAISE not_found_work_schedule_rules.
  ENDIF.

  CALL FUNCTION 'DATE_GET_WEEK'
       EXPORTING
            date         = datum
       IMPORTING
            week         = week
       EXCEPTIONS
            date_invalid = 1
            OTHERS       = 2.
  IF sy-subrc <> 0.
    RAISE invalid_date.
  ENDIF.

  CALL FUNCTION 'DATE_COMPUTE_DAY'
       EXPORTING
            date = datum
       IMPORTING
            day  = exp_day.

  IF schkz(4) EQ '8000'.
    DATA $d_diff TYPE i.
    DATA $d_mod TYPE p DECIMALS 2.
    DATA $d_week TYPE  p DECIMALS 2.
    DATA $wonum  TYPE  i.

    $d_diff = datum - t508a-bzpkt.
    $d_mod = $d_diff MOD 21.

    $wonum = 1.
    IF $d_mod >= 7.
      DO 2 TIMES.
        ADD 1 TO $wonum.
        $d_week = $d_mod - 7.
        IF $d_week < 7.
          EXIT.
        ENDIF.
        $d_mod = $d_week.
      ENDDO.
    ENDIF.

    SELECT SINGLE * FROM t551a WHERE motpr EQ t508a-motpr
                                 AND zmodn EQ t508a-zmodn
                                 AND wonum EQ $wonum.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM t551a WHERE motpr EQ t508a-motpr
                                   AND zmodn EQ t508a-zmodn.
      IF sy-subrc NE 0.
        RAISE not_found_period_work_schedule.
      ENDIF.

    ENDIF.
    CONCATENATE 'T551A-TPRG' exp_day INTO fname.
    ASSIGN (fname) TO <fs>.
    tprog = <fs>.

  ELSE.

    SELECT SINGLE * FROM t551a WHERE motpr EQ t508a-motpr
                                AND zmodn EQ t508a-zmodn
                                AND wonum EQ week+4(2).
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM t551a WHERE motpr EQ t508a-motpr
                                   AND zmodn EQ t508a-zmodn.
      IF sy-subrc NE 0.
        RAISE not_found_period_work_schedule.
      ENDIF.

    ENDIF.
    CONCATENATE 'T551A-TPRG' exp_day INTO fname.
    ASSIGN (fname) TO <fs>.
    tprog = <fs>.

  ENDIF.
ENDFUNCTION.
