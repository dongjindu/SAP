FUNCTION z_fpp_shift_plan_qty.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATE) TYPE  SY-DATUM DEFAULT 20120731
*"     REFERENCE(I_SHOP) TYPE  CRHD-ARBPL DEFAULT 'T'
*"     REFERENCE(I_SHIFT) TYPE  KAPA-SCHNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_QTY) TYPE  I
*"----------------------------------------------------------------------
  DATA: l_wtime     LIKE zvpp_capacity-endzt ,
        l_date      TYPE d ,
        l_wdate     TYPE d,
        l_einzt     LIKE tc37a-einzt ,
        l_1st_einzt LIKE tc37a-einzt,
        lt_capa     LIKE TABLE OF zvpp_capacity  WITH HEADER LINE,
        l_shift     LIKE kapa-schnr,
        l_uph       TYPE zuph,
        l_day       LIKE scal-indicator,
        l_daily_qty TYPE i,
        l_shift_qty TYPE i,
        l_daily_gap TYPE i,
        l_shift_cnt TYPE i,
        l_diff_net_time.

  DATA: BEGIN OF lt_time_capa OCCURS 0,
          shift  TYPE kapa-schnr,
          einzt  TYPE tc37a-einzt,
          uph    TYPE zuph,
          qty    TYPE i,
        END OF   lt_time_capa.
  DATA: wa_time_capa LIKE lt_time_capa.

  CLEAR: e_qty.

  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = '+'
      date                         = i_date
      factory_calendar_id          = 'HM'
    IMPORTING
      date                         = l_wdate
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      OTHERS                       = 7.
  IF sy-subrc NE 0 OR i_date NE l_wdate.
    e_qty = 0.
    EXIT.
  ENDIF.

*view: KAKO,KAKT,KAZY,KAPA,CRHD,CRCA
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl =  i_shop
     AND datub >= i_date.

  SORT lt_capa BY datub .
  READ TABLE lt_capa INDEX 1.
  l_date = lt_capa-datub    .

*Returns weekday for a date
  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date = i_date
    IMPORTING
      day  = l_day.

  REFRESH lt_time_capa.
  LOOP AT lt_capa WHERE datub = l_date AND tagnr = l_day.
    CLEAR: l_einzt.
*---Shift definition
    SELECT SINGLE einzt INTO l_einzt
      FROM tc37a
     WHERE schgrup  = lt_capa-mosid
       AND kaptprog = lt_capa-tprog
       AND endda   >= i_date
       AND begda   <= i_date     .

    l_wtime = l_wtime + l_einzt    .  "total working time

    lt_time_capa-shift = lt_capa-schnr.
    lt_time_capa-einzt = l_einzt.
    CALL FUNCTION 'Z_FPP_GET_UPH'
      EXPORTING
        date  = i_date
        shift = lt_time_capa-shift
        shop  = i_shop
      IMPORTING
        uph   = lt_time_capa-uph.

    lt_time_capa-qty = lt_time_capa-uph * lt_time_capa-einzt / 3600.
    l_shift_qty = l_shift_qty + lt_time_capa-qty.
    APPEND lt_time_capa.
*---<< BS Bae. 12/20/2013. Bug fix
    IF sy-tabix = 1.
      l_1st_einzt = lt_time_capa-einzt.
    ENDIF.

    IF l_1st_einzt NE lt_time_capa-einzt.
      l_diff_net_time = 'X'.
    ENDIF.
*--->> BS Bae. 12/20/2013.
  ENDLOOP.

*---<< BS Bae. 12/20/2013. Bug fix
** UPH
*  call function 'Z_FPP_GET_UPH'
*    exporting
*      date = i_date
*      shop = i_shop
*    importing
*      uph  = l_uph.
*
** calc for day
*  e_qty =  l_uph * l_wtime / 3600 .
*
*  read table lt_time_capa with key shift = i_shift into wa_time_capa.
*  if sy-subrc = 0.
**---adjust rounding to last shift
*    describe table lt_time_capa lines sy-tabix.
*    if i_shift = sy-tabix.
*      loop at lt_time_capa.
*        sum.
*      endloop.
*      e_qty = e_qty - lt_time_capa-qty.
*      e_qty = wa_time_capa-qty + e_qty.
*    else.
*      e_qty = wa_time_capa-qty .
*    endif.
*  endif.

*---// Get daily gap quantity
  clear: l_diff_net_time.   "<< If delete this line,
                            "   this function can calculate daily qty
                            "   for diffrent shift working time

  IF l_diff_net_time IS INITIAL. " Net time is same for each shift
    CALL FUNCTION 'Z_FPP_GET_UPH'
      EXPORTING
        date = i_date
        shop = i_shop
      IMPORTING
        uph  = l_uph.

    l_daily_qty = l_uph * l_wtime / 3600.
  ELSE.
    l_daily_qty = l_shift_qty.
  ENDIF.

  l_daily_gap = l_daily_qty - l_shift_qty.

  IF i_shift EQ 0.
    e_qty = l_daily_qty.
  ELSE.
    DESCRIBE TABLE lt_time_capa LINES l_shift_cnt.

    READ TABLE lt_time_capa WITH KEY shift = i_shift.
    IF sy-subrc NE 0.
      e_qty = 0. EXIT.
    ENDIF.

    IF l_shift_cnt NE i_shift.
      e_qty = lt_time_capa-qty.
    ELSE.
      e_qty = lt_time_capa-qty + l_daily_gap.
    ENDIF.
  ENDIF.
*--->> BS Bae. 12/20/2013.
ENDFUNCTION.
