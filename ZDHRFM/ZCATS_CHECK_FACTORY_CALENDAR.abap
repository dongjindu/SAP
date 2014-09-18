FUNCTION ZCATS_CHECK_FACTORY_CALENDAR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(UTIMES) TYPE  SY-TABIX
*"             VALUE(DATEFROM) TYPE  CATSDATEFR
*"             VALUE(I_CALENDAR) TYPE  CATSCALE
*"             VALUE(I_PERNR) TYPE  PERNR_D
*"             VALUE(I_WORKDAY_W) TYPE  C
*"             VALUE(I_EXT_CALL) TYPE  C DEFAULT SPACE
*"       TABLES
*"              BLOCKED_DAYS
*"----------------------------------------------------------------------

  DATA: BEGIN OF I_BLOCKED_DAYS OCCURS 0,
          DATE TYPE SY-DATUM,
        END OF I_BLOCKED_DAYS.
* data: i_tc_catsd_cols type cxtab_column occurs 10 with header line.


  DATA: UCOUNT LIKE SY-TABIX.
  DATA: UINDICATOR LIKE SCAL-INDICATOR.
  DATA: WORKDATE LIKE SY-DATUM.
* Profile with calendar check ?
* CHECK TCATS-CALENDAR EQ YX.
  REFRESH I_BLOCKED_DAYS.
* set fields according to factory calendar
  WORKDATE = DATEFROM - 1.
* i_tc_catsd_cols[] = tc_catsd_cols[].

  DO DAYS_ON_SCREEN TIMES.
    WORKDATE = WORKDATE + 1.
    IF WORKDAY IS INITIAL AND NOT I_CALENDAR EQ YX.
      CLEAR UINDICATOR.
    ELSE.
      CALL FUNCTION 'CATS_CHECK_FACTORY_DATE'
           EXPORTING
                TMP_PERNR    = I_PERNR
                TMP_DATE     = WORKDATE
                TMP_EXTERNAL = i_ext_call
           CHANGING
                TMP_IND      = UINDICATOR.
    ENDIF.
    COUNT1 = SY-INDEX.
* Modify screen
    DO UTIMES TIMES.
      IF SY-INDEX = 1.
        TFIELD = CATSD_DAY-TEXT.
        TFIELD+CATSD_DAY-OFFSET = COUNT1.
      ELSEIF SY-INDEX = 2.
        TFIELD = CATSD_BEGUZ-TEXT.
        TFIELD+CATSD_BEGUZ-OFFSET = COUNT1.
      ELSEIF SY-INDEX = 3.
        TFIELD = CATSD_ENDUZ-TEXT.
        TFIELD+CATSD_ENDUZ-OFFSET = COUNT1.
      ENDIF.
      CONDENSE TFIELD NO-GAPS.
      LOOP AT TC_CATSD-COLS INTO ITC_CATSD_COLS WHERE SCREEN-NAME
                                                           = TFIELD.
        IF NOT UINDICATOR IS INITIAL.
          IF I_WORKDAY_W IS INITIAL.
            ITC_CATSD_COLS-SCREEN-INPUT = OFF.
          ENDIF.
          I_BLOCKED_DAYS-DATE = WORKDATE.
          APPEND I_BLOCKED_DAYS.
        ELSE.
          ITC_CATSD_COLS-SCREEN-INPUT = ON.
        ENDIF.
        MODIFY TC_CATSD-COLS FROM ITC_CATSD_COLS.
      ENDLOOP.
    ENDDO.
  ENDDO.

  BLOCKED_DAYS[] = I_BLOCKED_DAYS[].
* tc_catsd_cols[] = i_tc_catsd_cols[].

ENDFUNCTION.
