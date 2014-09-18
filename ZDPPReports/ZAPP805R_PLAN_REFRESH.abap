************************************************************************
* Program Name      : ZAPP805R_PLAN_REFRESH
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K90228
* Addl Documentation:
* Description       : Plan Order Refresh
*
* Modification Logs
*   Date         Developer    RequestNo    Description
*#1 02/23/2005   100701       UD1K914664
* Change : Change the Time Tule(Tack-Time --> Lead Time)
*#2 02/25/2005   wskim        UD1K914664   Rescheduling
*#3 02/28/2005   wskim        UD1K914687   Reschduling - include
*                                           table  ZTPP_PMT07JB_C
*#4 04/07/2005   chris        UD1K915416   Add logic to use either trim
*                                          input or body input schedule
**               FR Wang                   Check plan open date < =
*                                                     start date

************************************************************************
REPORT  zapp805r_plan_refresh   MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ztpp_input_plan,ausp , usr01,ztpp_pmt07jb_a.


*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF it_data        OCCURS 0,
        objek                 LIKE ausp-objek.      " Vehicle Code
        INCLUDE STRUCTURE     ztpp_input_plan.
DATA: END OF it_data.

DATA: BEGIN OF it_master      OCCURS 0,
        seq                   TYPE i  ,             " Sequence
        date                  TYPE d  ,             " Date
        day                   LIKE kapa-tagnr,      " Day
        shift                 LIKE kapa-schnr,      " Shift
        time                  TYPE kapendzt  ,      " Times for working
        tun                   TYPE ld_lantu  ,      " Unit  for Time
        uph                   TYPE zvpp_ld-lrate,   " UPH
      END OF it_master.

DATA: it_disp            LIKE TABLE OF ztpp_alc_binput WITH HEADER LINE.
*---Start #3 wskim 02/28/2005
DATA: it_7jb    LIKE ztpp_pmt07jb_a OCCURS 0 WITH HEADER LINE,
      it_cfile  LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE,
      it_cfile2 LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE.
*---end
*--->added by chris on 04/27/2005
DATA: it_inputplan LIKE ztpp_input_plan OCCURS 0 WITH HEADER LINE.
*--->end

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: wa_disp             LIKE it_disp                             ,
      wa_data             LIKE it_data                             ,
      wa_wdate            LIKE sy-datum                            ,
      wa_kalid            LIKE kako-kalid                          ,
      wa_uzeit            LIKE sy-uzeit                            ,
      wa_index            LIKE sy-tabix                            ,
      wa_snd_jobs         TYPE i                                   ,
      wa_rcv_jobs         TYPE i                                   ,
      wa_taskname(4)      TYPE n VALUE '0001'                      ,
      wa_excp_flag        TYPE c                                   ,
      wa_error            TYPE c                                   ,
      wa_flag             TYPE c                                   ,
      wa_hour             TYPE i                                   .
*---start #2 wskim
DATA : f_count TYPE i.
*---end
DATA : it_hold  LIKE ztpp_hold_car OCCURS 0 WITH HEADER LINE.
DATA : it_hold_sch LIKE ztpp_hold_car OCCURS 0 WITH HEADER LINE.
DATA : g_last_seq_date   LIKE sy-datum.
DATA : g_shop LIKE crhd-arbpl.
DATA : BEGIN OF it_cabn OCCURS 0,
        atnam   LIKE cabn-atnam,
        atinn   LIKE cabn-atinn,
       END OF it_cabn.
*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
FIELD-SYMBOLS: <wa_dfield>    TYPE any.

*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS: p_dates  TYPE d   DEFAULT sy-datum    OBLIGATORY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_trim RADIOBUTTON GROUP rd.
SELECTION-SCREEN COMMENT 10(25) text-012 FOR FIELD p_trim.
PARAMETERS: p_body RADIOBUTTON GROUP rd.
SELECTION-SCREEN COMMENT 40(25) text-011 FOR FIELD p_body.
SELECTION-SCREEN END OF LINE.
* 07/17/2013 - T00306 Start
SELECTION-SCREEN SKIP.
PARAMETERS: p_srvgrp LIKE rzllitab-classname OBLIGATORY
                     DEFAULT 'PG_SEQ'.
* 07/17/2013 - T00306 End
SELECTION-SCREEN END OF BLOCK b1.

PARAMETERS: p_seq TYPE c DEFAULT ' ' NO-DISPLAY.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
AT SELECTION-SCREEN.
*----------------------------------------------------------------------
* set the shop for schedule
  PERFORM set_shop.
  " Get the Date for the Production Reporting Date(Last Date)
  wa_wdate = p_dates - 1.
  PERFORM read_shop_calid   USING wa_kalid.
  PERFORM read_working_date USING '-'  wa_kalid  wa_wdate.
  PERFORM get_day          USING wa_wdate it_master-day .
  PERFORM get_working_time USING wa_wdate it_master-time it_master-day .
  PERFORM get_uph          USING wa_wdate it_master-uph
                                 it_master-shift  g_shop.
  it_master-seq    = 99.
  it_master-date = wa_wdate .
  APPEND it_master.  CLEAR: it_master.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM set_information USING p_dates.
  PERFORM read_inputplan .
  MESSAGE s000(zmpp) WITH 'Start create_summary'.
  PERFORM create_summary .
*----start#2 wskim 02/25/2005
  MESSAGE s000(zmpp) WITH 'Start rescheduling_inputplan'.
  PERFORM rescheduling_inputplan.

  MESSAGE s000(zmpp) WITH 'Start recreate_ztpp_pmt07jb_c'.
  PERFORM recreate_ztpp_pmt07jb_c.
*----end


*----requested by MY HUR changed by chris
*    schedule the body input plan by body shop UPH
*    and update to field RD01 in ztpp_input_plan
  "set the body shop flag
  p_body = 'X'.
*    set shop for shcedule and read the UPH and time
  MESSAGE s000(zmpp) WITH 'Start get_body_shop_data'.
  PERFORM get_body_shop_data.


*----end of change on 04/27/2005
*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------



*&---------------------------------------------------------------------*
*&      Form  READ_INPUTPLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_inputplan.
  DATA: l_count(05) TYPE n.
  DATA: l_text(30).
  DATA: wa_data LIKE it_data.
  DATA: l_tabix TYPE i.

  IF g_shop = 'B'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ztpp_input_plan
     WHERE status = '00' .
  ELSEIF g_shop = 'T'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
     FROM ztpp_input_plan
    WHERE status IN ('00','01','02','03','04','05')
      AND mitu NE 'Y'.

*--->> BS Bae. 12/19/2013. for first day quantity.
*    excluding the 'XX' 'XY' cars for status 01--05
*    LOOP AT it_data WHERE status NE '00'.

*    excluding the 'XX' 'XY' cars for all of car
    LOOP AT it_data.
*---<< BS Bae. 12/19/2013.
      IF it_data-work_order+12(2) = 'XX' OR
         it_data-work_order+12(2) = 'XY' .
        DELETE it_data.
      ENDIF.
    ENDLOOP.

  ENDIF.

  DESCRIBE TABLE it_data LINES  wa_hour .
  IF wa_hour = 0.
    DELETE FROM ztpp_alc_binput CLIENT SPECIFIED WHERE mandt = sy-mandt.
    LEAVE PROGRAM .
  ENDIF.

  SORT it_data BY serial .
* reading all waiting holding cars
  PERFORM read_hold_cars.

* output the qty of each status for verify and check
  SORT it_data BY status DESCENDING.
  LOOP AT it_data.
    l_count = l_count + 1.
    l_tabix = sy-tabix + 1.
    CLEAR: wa_data.
    READ TABLE it_data INTO wa_data INDEX l_tabix .
    IF wa_data-status NE it_data-status.
      CONCATENATE it_data-status 'status total :' INTO l_text
         SEPARATED BY space.
      WRITE:/ l_text , l_count.
      CLEAR: l_count.
    ENDIF.
  ENDLOOP.
** changed by furong on 08/16/2005
  SORT it_data BY serial.
** end of change
ENDFORM.                    " READ_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  CREATE_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_summary.
  DATA: l_worder             LIKE mara-matnr,
        l_flag               TYPE c ,
        l_max                TYPE i ,
        l_hours              TYPE i ,
        l_pos                TYPE i ,
        l_pos1               TYPE i ,
        l_serial             LIKE it_data-body_ser,
        l_count              TYPE i ,
        f_num                TYPE i ,
        l_loop               TYPE i ,
        l_gap_qty            TYPE i ,
        l_skip               TYPE c ,
        l_hold               TYPE c ,
        l_yn                 TYPE c ,
        l_chk                TYPE p DECIMALS 3,
        l_tabix              LIKE sy-tabix,
        l_index              LIKE sy-tabix.

* requested by my HUR changed by chris
* refresh and reschedule the holding cars
  PERFORM refresh_hold_cars.
* end of change on 04/08/2005

  DESCRIBE TABLE it_data LINES l_max.
  l_loop = l_index = 1.

  " First Day Data...
  SORT it_master BY seq .
  LOOP AT it_master WHERE seq > 80 AND seq < 90 .
    IF l_index > l_max.
      EXIT.
    ENDIF.

* enhance rounding error
*    l_chk = it_master-time / 3600 .
*    l_pos1 = it_master-uph * l_chk.
    CALL FUNCTION 'Z_FPP_SHIFT_PLAN_QTY'
      EXPORTING
        i_date  = it_master-date
        i_shop  = 'T'
        i_shift = it_master-shift
      IMPORTING
        e_qty   = l_pos1.

*   check how many holding cars has been scheduled for this date
    PERFORM check_count USING l_count
                              it_master-date.

    l_pos1 = l_pos1 - l_count.          "deduct the shceduled qty
    l_count = l_pos1.

*---<< 12.02.2013 BS Bae.
*---   if l_count < 0, plan quantity is accumulate in same day
    IF l_count < 0.
      l_count = l_pos1 = 0.
    ENDIF.
*--->> 12.02.2013 BS Bae.

    LOOP AT it_data FROM l_index TO l_max.
      IF l_count = 0.
        EXIT.
      ENDIF.
*     if the car is xx xy car don't count it.
      PERFORM check_xxxy USING it_data l_yn.
*     if the car is holding car skip it
      PERFORM check_hold_car USING it_data it_master-date l_hold.
      IF l_hold = 'N'.    "Not a holding car, do refresh
        PERFORM refresh_date_plaf  USING it_master-date .
        IF l_yn = 'N' OR g_shop = 'B'. "Don't count XX XY car
          l_count = l_count - 1.
          g_last_seq_date = it_master-date.
          f_count = l_count.
        ENDIF.
      ENDIF.
      f_num = f_num + 1.
    ENDLOOP.
    l_index = l_index + f_num .     "next begin postion
    CLEAR f_num.
  ENDLOOP.

  " Daily Data...
  MESSAGE s000(zmpp) WITH 'starting 21 days data in summary'.
  DO 21 TIMES.
    l_tabix = l_tabix + 1 .
    READ TABLE it_master WITH KEY seq = l_tabix.
    IF l_index > l_max.
      EXIT.
    ENDIF.

* enhance rounding error
*    l_chk = it_master-time / 3600 .
*    l_pos1 = it_master-uph * l_chk.
    CALL FUNCTION 'Z_FPP_SHIFT_PLAN_QTY'
      EXPORTING
        i_date = it_master-date
        i_shop = 'T'
      IMPORTING
        e_qty  = l_pos.

*   check how many holding cars has been scheduled for this date
    PERFORM check_count USING l_count
                              it_master-date.

    l_pos = l_pos - l_count.          "deduct the shceduled qty
    l_count = l_pos.

*---<< 12.02.2013 BS Bae.
*---   if l_count < 0, plan quantity is accumulate in same day
    IF l_count < 0.
      l_count = l_pos1 = 0.
    ENDIF.
*--->> 12.02.2013 BS Bae.

    LOOP AT it_data FROM l_index TO l_max.
      IF l_count = 0.
        EXIT.
      ENDIF.
*     if the car is xx xy car don't count it.
      PERFORM check_xxxy USING it_data l_yn.

*     if the car is holding car skip it
      PERFORM check_hold_car USING it_data it_master-date l_hold.
      IF l_hold = 'N'.    "Not a holding car, do refresh
        PERFORM refresh_date_plaf  USING it_master-date .
        IF g_shop = 'B' OR
          l_yn = 'N'.               "xx xy car don't count
          l_count = l_count - 1.
*         recording the last date scheduled
          g_last_seq_date  = it_master-date.
          f_count = l_count.       "qty left
        ENDIF.
      ENDIF.

      f_num = f_num + 1.
    ENDLOOP.

    l_index = l_index + f_num .
    CLEAR: f_num.
  ENDDO.
  MESSAGE s000(zmpp) WITH 'finished 21 days data in summary'.


  WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.
ENDFORM.                    " CREATE_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = pa_type
      date                         = pa_wdate
      factory_calendar_id          = pa_kalid
    IMPORTING
      date                         = pa_wdate
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = g_shop   .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  SET_INFORMATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_information USING p_dates.
  DATA: l_date               TYPE d ,
        l_count              TYPE i .

  " Set the BASIC Information for the UPH & Work Time...
  CLEAR: l_count.                 l_date = p_dates  .
  PERFORM read_working_date USING '+'  wa_kalid  l_date .
*  if l_date = p_dates .
  it_master-seq    = '80'.       it_master-date   = l_date .
  PERFORM get_day          USING l_date it_master-day  .
  PERFORM get_worktime1    USING l_date it_master-time it_master-day .
*  else.
*    l_date = p_dates .
*    perform get_day         using l_date it_master-day  .
*    it_master-seq    = '81'.       it_master-date   = l_date .
*    append it_master.       clear: it_master.
*  endif.

  " From D+1 Day To D+21 Day..  (Only Working Dates in FACTORY-Calendar)
*  l_date = p_dates .
  DO 21 TIMES.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+'  wa_kalid  l_date.
    PERFORM get_day          USING l_date it_master-day  .
    PERFORM get_working_time USING l_date it_master-time it_master-day.
    PERFORM get_uph        USING l_date it_master-uph
                                 it_master-shift g_shop.
    it_master-seq    = l_count.
    it_master-date   = l_date .
    APPEND it_master.  CLEAR: it_master.
  ENDDO.
ENDFORM.                    " SET_INFORMATION

*&---------------------------------------------------------------------*
*&      Form  GET_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
FORM get_working_time USING    pa_wdate  pa_wktime  pa_day.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_date        TYPE d ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = g_shop
     AND datub >= pa_wdate .

  SORT lt_capa BY datub .
  READ TABLE lt_capa INDEX 1.
  l_date = lt_capa-datub    .

  LOOP AT lt_capa WHERE datub = l_date AND tagnr = pa_day .
    CLEAR: l_einzt.
    SELECT SINGLE einzt INTO l_einzt
      FROM tc37a
     WHERE schgrup  = lt_capa-mosid
       AND kaptprog = lt_capa-tprog
       AND endda   >= pa_wdate
       AND begda   <= pa_wdate     .
    l_wtime = l_wtime + l_einzt    .
  ENDLOOP.
  pa_wktime = l_wtime .
ENDFORM.                    " GET_WORKING_TIME

*&---------------------------------------------------------------------*
*&      Form  GET_WORKTIME1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
FORM get_worktime1 USING    pa_wdate  pa_wktime  pa_day.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_date        TYPE d ,
        l_flag        TYPE c ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = g_shop
     AND datub >= pa_wdate .

  SORT lt_capa BY datub tagnr schnr .
  READ TABLE lt_capa INDEX 1.
  l_date = lt_capa-datub    .

  LOOP AT lt_capa WHERE datub = l_date AND tagnr = pa_day .
    CLEAR: l_einzt.
    SELECT SINGLE einzt INTO l_einzt
      FROM tc37a
     WHERE schgrup  = lt_capa-mosid
       AND kaptprog = lt_capa-tprog
       AND endda   >= pa_wdate
       AND begda   <= pa_wdate     .
    it_master-time  = l_einzt      .
    it_master-shift = lt_capa-schnr.
    it_master-seq   = 80 + lt_capa-schnr .
    PERFORM get_uph   USING pa_wdate it_master-uph
                            it_master-shift g_shop.
    APPEND it_master.
    l_flag = 'X' .
  ENDLOOP.
  IF l_flag = space.
    PERFORM get_uph   USING pa_wdate it_master-uph
                            it_master-shift g_shop.
    APPEND it_master.
  ENDIF.
  CLEAR: it_master.
  pa_wktime = l_wtime .
ENDFORM.                    " GET_WORKTIME1

*&---------------------------------------------------------------------*
*&      Form  GET_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM get_day USING    pa_wdate  pa_day.
  DATA: l_day         LIKE scal-indicator .

  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date = pa_wdate
    IMPORTING
      day  = l_day.

  pa_day = l_day.
ENDFORM.                    " GET_DAY

*&---------------------------------------------------------------------*
*&      Form  GET_UPH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_UPH  text
*----------------------------------------------------------------------*
FORM get_uph USING    pa_wdate  pa_uph  pa_shift p_shop.
  DATA: w_uph LIKE ztpp_status-uph.

  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      date  = pa_wdate
      shift = pa_shift
      shop  = p_shop
    IMPORTING
      uph   = w_uph.
  pa_uph = w_uph.

*  DATA lw_ld          LIKE zvpp_ld .
*data: lt_ld   like zvpp_ld occurs 0 with header line.
*
*  IF pa_shift IS INITIAL .
** requested by MY HUR changed by chris
** because two shift could exist, read one record
** only one shift is calculated
**    SELECT SINGLE * INTO lw_ld
**      FROM zvpp_ld
**     WHERE ld_perst <= pa_wdate
**       AND ld_pered >= pa_wdate
**       AND arbpl     = 'T'      .
*    SELECT * INTO table lt_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND arbpl     = 'T'      .
*
** end of change on 06/13/2005
*  ELSE.
** requested by MY HUR changed by chris
** because two shift could exist, read one record
** only one shift is calculated
** and one shift could have more than one record
** to difine diferent rate for different period
** of time
**    SELECT SINGLE * INTO lw_ld
**      FROM zvpp_ld
**     WHERE ld_perst <= pa_wdate
**       AND ld_pered >= pa_wdate
**       AND ld_shift  = pa_shift
**       AND arbpl     = 'T'      .
*    SELECT * INTO table lt_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND ld_shift  = pa_shift
*       AND arbpl     = 'T'      .
*
*
*  ENDIF.
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
** add by chris on 06/13/2005
*    loop at lt_ld.
*      lw_ld-lrate = lw_ld-lrate + lt_ld-lrate.
*      lw_ld-lantu = lw_ld-lantu + lt_ld-lantu.
*    endloop.
** end of add.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
ENDFORM.                    " GET_UPH

*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATE_PLAF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MASTER_DATE  text
*----------------------------------------------------------------------*
FORM refresh_date_plaf USING    pa_date.
  DATA: l_porder       LIKE bapi_pldord-pldord_num.
  DATA: l_fin_date     LIKE sy-datum.

  CLEAR l_porder.
*bug fix if no body+serial, no need to read VM 7/31/12
  IF it_data-body_ser = ''.
    EXIT.
  ENDIF.
  PERFORM read_planorder   USING l_porder.

* get the plan order finish date
*  l_fin_date = pa_date + 2.
  l_fin_date = pa_date.
  PERFORM read_working_date USING '+'  wa_kalid  l_fin_date.

* for testing using sequencial run 7/31/12
  IF p_seq = 'X'.
    CALL FUNCTION 'Z_FPP_PLANORDER_RESCHEDULE'
      EXPORTING
        p_porder   = l_porder
        p_date     = pa_date
        p_fin_date = l_fin_date.

  ELSE.
    DO .
      CALL FUNCTION 'Z_FPP_PLANORDER_RESCHEDULE'
        STARTING NEW TASK wa_taskname DESTINATION IN GROUP p_srvgrp
        PERFORMING return_step1 ON END OF TASK
        EXPORTING
          p_porder              = l_porder
          p_date                = pa_date
          p_fin_date            = l_fin_date
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2
          resource_failure      = 3
          OTHERS                = 4.

      CASE sy-subrc.
        WHEN 0.
          wa_taskname = wa_taskname  + 1.
          wa_snd_jobs = wa_snd_jobs  + 1.
          CLEAR: wa_excp_flag .
          EXIT.
        WHEN 1 OR 2.
          wa_excp_flag = 'X'.
        WHEN 3.
*Receive reply to asynchronous RFC calls
          IF wa_excp_flag = space.
            wa_excp_flag = 'X'.
*First attempt for RESOURCE_Failure handling
            WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs UP TO '0.01' SECONDS.
          ELSE.
*Second attempt for RESOURCE_Failure handling
            WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs UP TO '0.1' SECONDS.
          ENDIF.
          IF sy-subrc = 0.
            CLEAR wa_excp_flag. " Reset flag
*        ELSE.
*          EXIT.
          ENDIF.
      ENDCASE.
    ENDDO.
  ENDIF.

ENDFORM.                    " REFRESH_DATE_PLAF

*&---------------------------------------------------------------------*
*&      Form  READ_PLANORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_PORDER  text
*----------------------------------------------------------------------*
FORM read_planorder USING    pa_porder.
  DATA: l_vehicle   LIKE mara-matnr   ,
        lt_val      LIKE TABLE OF zspp_vin_value       WITH HEADER LINE.

  CLEAR: lt_val, lt_val[].
  lt_val-atnam = 'P_PLAN_ORDER'.  APPEND lt_val.
  CONCATENATE it_data-modl it_data-body_ser  INTO l_vehicle.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object    = l_vehicle
      mode      = 'R'
      ctype     = '002'
*     DISPLAY   = 'D'
    TABLES
      val_table = lt_val.

  READ TABLE lt_val INDEX 1.
  pa_porder = lt_val-atwrt .

ENDFORM.                    " READ_PLANORDER

*&---------------------------------------------------------------------*
*&      Form  return_step1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM return_step1  USING p_taskname.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_PLANORDER_RESCHEDULE'
         EXCEPTIONS
         communication_failure       = 1
         system_failure              = 2
         resource_failure            = 3
         OTHERS                      = 4.

  CHECK sy-subrc = 0.
  wa_rcv_jobs  = wa_rcv_jobs + 1.
ENDFORM.                    " return_step1
*&---------------------------------------------------------------------*
*&      Form  rescheduling_inputplan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rescheduling_inputplan.
  DATA : it_input   LIKE ztpp_input_plan OCCURS 0 WITH HEADER LINE.
  DATA : z_rsnum    LIKE ztpp_input_plan-rsnum,
         z_seq_date LIKE  ztpp_input_plan-seq_date.

  REFRESH it_input.CLEAR : z_rsnum,z_seq_date.
* find the last scheduled date
*"comment by chris--because some holding cars has been scheduled
* for future date
*  SELECT MAX( rsnum ) INTO z_rsnum
*    FROM ztpp_input_plan
*      WHERE mitu EQ space
*        AND status EQ '00'.

  PERFORM get_workday_uph USING g_last_seq_date.
*---Start#3 wskim 03/14/2005
*get date the last SEQ_DATE
  SELECT MAX( seq_date ) INTO z_seq_date
    FROM ztpp_input_plan
      WHERE  mitu EQ space.

  PERFORM  filtering_date USING z_seq_date.
*---End
  PERFORM read_inputplan_re .
  PERFORM create_summary_re USING f_count z_seq_date.

ENDFORM.                    " rescheduling_inputplan
*&---------------------------------------------------------------------*
*&      Form  get_workday_uph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_workday_uph USING p_renum.

  REFRESH it_master.CLEAR it_master.
*date conversion : user format
  MOVE p_renum TO wa_wdate.
*  PERFORM date_conversion USING p_renum
*                          CHANGING wa_wdate.

  PERFORM read_shop_calid   USING wa_kalid.
  PERFORM read_working_date USING '-'  wa_kalid  wa_wdate.
  PERFORM get_day          USING wa_wdate it_master-day .
  PERFORM get_working_time USING wa_wdate it_master-time it_master-day .
  PERFORM get_uph        USING wa_wdate it_master-uph
                               it_master-shift  g_shop.
  it_master-seq    = 99.
  it_master-date = wa_wdate .
  APPEND it_master.  CLEAR: it_master.

  PERFORM set_information USING wa_wdate.

ENDFORM.                    " get_workday_uph
*&---------------------------------------------------------------------*
*&      Form  date_conversion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RENUM  text
*      <--P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM date_conversion  USING    p_before_date
                     CHANGING p_after_date.

  SELECT SINGLE *  FROM usr01
        WHERE bname = sy-uname.

  CASE usr01-datfm.
    WHEN '1'. "DD.MM.YYYY
      p_after_date+4(4) =   p_before_date+0(4).
      p_after_date+2(2) =   p_before_date+4(2).
      p_after_date+0(2) =   p_before_date+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      p_after_date+4(4) =   p_before_date+0(4).
      p_after_date+0(2) =   p_before_date+4(2).
      p_after_date+2(2) =   p_before_date+6(2).
  ENDCASE.

ENDFORM.                    " date_conversion
*&---------------------------------------------------------------------*
*&      Form  read_inputplan_re
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_inputplan_re .


  REFRESH it_data.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_input_plan
   WHERE  status = space .
* comment by chris--because xx xy car can not be counted for Trim input
*                   plan but need to be counted for body input plan
*  SELECT COUNT( * ) INTO z_count
*    FROM ztpp_input_plan
*       WHERE status EQ '00'
*         AND mitu EQ space
*         AND rsnum EQ p_rsnum.
* end of chris comment-out

*  DESCRIBE TABLE it_data LINES  wa_hour .
*  IF wa_hour = 0.
*    DELETE FROM ztpp_alc_binput CLIENT SPECIFIED WHERE mandt = sy-mandt
  .
*    LEAVE PROGRAM .
*  ENDIF.

  SORT it_data BY serial .
  LOOP AT it_data.
    CLEAR it_data-rd01.
    MODIFY it_data FROM it_data INDEX sy-tabix.

  ENDLOOP.



ENDFORM.                    " read_inputplan_re
**&---------------------------------------------------------------------
*
**&      Form  create_summary_re
**&---------------------------------------------------------------------
*
**       This form logic has been changed by chris on 04/08/2005
**       New logic  use the same form name
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM create_summary_re USING f_count p_seq_date.
*  DATA: l_worder             LIKE mara-matnr,
*        l_flag               TYPE c ,
*        l_max                TYPE i ,
*        l_hours              TYPE i ,
*        l_pos                TYPE i ,
*        l_loop               TYPE i ,
*        l_skip               TYPE c ,
*        l_chk                TYPE p DECIMALS 3,
*        l_tabix              LIKE sy-tabix,
*        l_index              LIKE sy-tabix,
*        l_serial             LIKE ztpp_input_plan-serial,
*        f_num TYPE i.
*  DATA : it_input LIKE ztpp_input_plan OCCURS 0 WITH HEADER LINE,
*         it_7jba LIKE ztpp_pmt07jb_a OCCURS 0 WITH HEADER LINE.
*  DATA : z_num LIKE ztpp_pmt07jb_a-ssr1.
*
*
*  REFRESH :it_input,it_7jba.
*
*  CLEAR :l_serial,f_num,l_index.
*  DESCRIBE TABLE it_data LINES l_max.
*  l_loop = l_index = 1.
*
*  " First Days Data...
*  SORT it_master BY seq .
*
*
*  LOOP AT it_master WHERE  seq EQ '99' .
*    LOOP AT it_data.
*      l_chk = it_master-time / 3600         .
*      l_pos = ( it_master-uph * l_chk ).
*      IF l_pos = f_count.
*        CONTINUE.
*      ELSE.
*        MOVE it_master-date TO it_data-rsnum .
*        l_serial = it_data-serial.
*        f_count = f_count + 1.
*        f_num = f_num + 1.
*        MODIFY it_data FROM it_data.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
*
**  l_serial = l_serial + 1.
*  l_max = l_max - f_num.
*  IF f_num = 0.
*    READ TABLE it_data INDEX 1.
*    l_serial = it_data-serial.
*  ELSE.
*    l_serial = l_serial + 1.
*  ENDIF.
*
*  LOOP AT it_master WHERE seq < 80.
*    l_chk = it_master-time / 3600 .
*    l_pos = ( it_master-uph * l_chk  ).
*    DO l_pos TIMES.
*      IF l_max   <  l_index.
*        EXIT.
*      ENDIF.
*      READ TABLE it_data WITH KEY serial = l_serial.
*      IF sy-subrc = 0.
*        LOOP AT  it_data WHERE serial = l_serial.
*          MOVE it_master-date TO it_data-rsnum .
*          MODIFY it_data FROM it_data.
*        ENDLOOP.
*        l_index = l_index + 1.
*      ELSE.
*        EXIT.
*      ENDIF.
*      l_serial = l_serial + 1.
*    ENDDO.
*  ENDLOOP.
*
*  CLEAR z_num.
*  LOOP AT it_data.
*    MOVE-CORRESPONDING it_data TO it_input.
**---Start#3 wskim 03/14/2005
*    IF it_data-rsnum EQ '00000000'.
*      MOVE p_seq_date TO it_input-rsnum.
*    ENDIF.
**---End
*    APPEND it_input.
*  ENDLOOP.
*
*  LOOP AT it_input.
*    DELETE FROM ztpp_input_plan
*         WHERE seq_date = it_input-seq_date
*           AND serial = it_input-serial.
*  ENDLOOP.
*  INSERT ztpp_input_plan FROM TABLE it_input
*                     ACCEPTING DUPLICATE KEYS .
*
*  IF sy-subrc = 0.
*
*    SELECT * INTO TABLE it_7jba
*     FROM ztpp_pmt07jb_a
*      WHERE  gubb = 'A'.
*
*    DELETE FROM  ztpp_pmt07jb_a WHERE gubb = 'A'.
*    REFRESH it_7jb.
*
**    SORT it_7jba BY sqdt ssr1.
*    LOOP AT it_7jba.
*      MOVE-CORRESPONDING it_7jba TO ztpp_pmt07jb_a.
*      READ TABLE it_input INDEX sy-tabix.
*      MOVE : it_input-rsnum TO ztpp_pmt07jb_a-sqdt.
*      MOVE-CORRESPONDING ztpp_pmt07jb_a TO it_7jb.
*
*      APPEND it_7jb.
*      CLEAR : ztpp_pmt07jb_a, it_7jb.
*    ENDLOOP.
*
**    SORT it_7jb BY sqdt ssr1.
*    LOOP AT it_7jb.
*      ON CHANGE OF it_7jb-sqdt.
*        CLEAR z_num.
*      ENDON.
*      z_num = z_num + 1.
*      MOVE z_num       TO  it_7jb-ssr1.
*
*      MODIFY it_7jb FROM it_7jb.
*      CLEAR it_7jb.
*    ENDLOOP.
*
*    MODIFY ztpp_pmt07jb_a FROM TABLE it_7jb.
*    IF sy-subrc = 0.
*      message s000 with 'Successfully Updated'.
*      COMMIT WORK.
*    ENDIF.
*  ENDIF.
*ENDFORM.                    " create_summary_re

*&---------------------------------------------------------------------*
*&      Form  create_summary_re
*&---------------------------------------------------------------------*
*       this is the new reshceduling logic
*----------------------------------------------------------------------*
* in this logic, XX XY cars can be scheduled but don't count if trim
* input UPH is used ,because
*     'xx' 'xy' cars will go to body shop but will not go to
*     trim shop, so if we use the trim input uph to schedule
*     the cars, we must include enough cars for trim input
*     after excluding the 'xx' 'xy' cars
* holding cars will be scheduled if the planed schedule date is the
*     same as the date we are doing reschedule
*
*----------------------------------------------------------------------*
FORM create_summary_re USING f_count p_seq_date.
  DATA: l_worder             LIKE mara-matnr,
        l_flag               TYPE c ,
        l_max                TYPE i ,
        l_hours              TYPE i ,
        l_pos                TYPE i ,
        l_loop               TYPE i ,
        l_cnt                TYPE i ,
        l_data_error         TYPE c ,
        l_skip               TYPE c ,
        l_chk                TYPE p DECIMALS 3,
        l_tabix              LIKE sy-tabix,
        l_index              LIKE sy-tabix,
        l_serial             LIKE ztpp_input_plan-serial,
        f_num                TYPE i,
        l_xxxy_cnt           TYPE i.
  DATA : it_input LIKE ztpp_input_plan OCCURS 0 WITH HEADER LINE,
         it_7jba  LIKE ztpp_pmt07jb_a OCCURS 0 WITH HEADER LINE.
  DATA : z_num LIKE ztpp_pmt07jb_a-ssr1.
  DATA : l_yn, l_hold, l_xx.
  DATA: l_lines TYPE i,
        l_lines_n(5) TYPE n.

  DATA: BEGIN OF lt_7jb_tmp OCCURS 0.
          INCLUDE STRUCTURE ztpp_pmt07jb_a.
  DATA:   serial LIKE ztpp_input_plan-serial,
        END   OF lt_7jb_tmp.

* output total qty of blank status
  DESCRIBE TABLE it_data LINES l_lines.
  l_lines_n = l_lines.
  WRITE:/ 'Blank status total :', l_lines_n.

  REFRESH :it_input,it_7jba.

  CLEAR :l_serial,f_num,l_index.
  DESCRIBE TABLE it_data LINES l_max.
  l_loop = l_index = 1.


  " First Day Data...
  SORT it_master BY seq .

* continue to schedule for the scheduled last date
* f_count:  qty left for the last day
  LOOP AT it_master WHERE  seq EQ '99' .
*---<< BS Bae. 12/20/2013.
*    l_chk = it_master-time / 3600    .
*    l_pos = ( it_master-uph * l_chk ).

    CALL FUNCTION 'Z_FPP_SHIFT_PLAN_QTY'
      EXPORTING
        i_date  = it_master-date
        i_shop  = 'T'
        i_shift = it_master-shift
      IMPORTING
        e_qty   = l_pos.
*--->> BS Bae. 12/20/2013.

    LOOP AT it_data.
      IF  f_count = 0.
        EXIT.
      ENDIF.
*     check if the car is 'xx''xy' car
      PERFORM check_xxxy USING it_data l_xx.

*---<< BS Bae. 12/20/2013. MIT want BIP, BIW plan date is blank
*      MOVE it_master-date TO it_data-rsnum .
*--->> BS Bae. 12/20/2013.

      l_serial = it_data-serial.

      IF g_shop = 'T'.              "using trim UPH
        IF l_xx = 'N'.              "If xx,xy, don't count it
*---<< BS Bae. 12/20/2013. MIT wants BIP, BIW plan date is blank
          MOVE it_master-date TO it_data-rsnum .
*--->> BS Bae. 12/20/2013.

          f_count = f_count - 1.
*---<< BS Bae. 1/10/2014. LTP PIR Issue. 7JB user date for test car
        ELSE.
          MOVE it_master-date TO it_data-rsnum .
*--->> BS Bae. 1/10/2014.
        ENDIF.
      ELSE.                         "Using body UPH
        f_count = f_count - 1.
      ENDIF.
      MODIFY it_data FROM it_data.

      f_num = f_num + 1.
    ENDLOOP.
  ENDLOOP.

* next serial number
  IF f_num = 0.
    READ TABLE it_data INDEX 1.
    l_serial = it_data-serial.
  ELSE.
    l_serial = l_serial + 1.
  ENDIF.

* next index of internal table it_data
  f_num = f_num + 1.

  LOOP AT it_master WHERE seq < 80.
    IF l_max   <  f_num .  " no more car left for schedule
      EXIT.
    ENDIF.

*---<< BS Bae. 12/20/2013.
*    l_chk = it_master-time / 3600 .
*    l_pos =  ( it_master-uph * l_chk  ).

    CALL FUNCTION 'Z_FPP_SHIFT_PLAN_QTY'
      EXPORTING
        i_date  = it_master-date
        i_shop  = 'T'
        i_shift = it_master-shift
      IMPORTING
        e_qty   = l_pos.
*--->> BS Bae. 12/20/2013.


*   check how many holding cars have been scheduled for this date
    PERFORM check_count USING f_count
                              it_master-date.

    l_pos = l_pos - f_count.          "deduct the shceduled qty
    f_count = l_pos.
    CLEAR: l_xxxy_cnt.
    LOOP AT it_data FROM f_num TO l_max.
      IF f_count = 0.
        EXIT.
      ENDIF.
      l_serial = it_data-serial.
*     check if the car is 'xx''xy' car
      PERFORM check_xxxy USING it_data l_xx.

*---<< BS Bae. 12/20/2013. MIT want BIP, BIW plan date is blank
*      MOVE it_master-date TO it_data-rsnum .
*--->> BS Bae. 12/20/2013.

      IF g_shop = 'T'.              "using trim UPH
        IF l_xx = 'N'.              "If xx xy car, don't count it
*---<< BS Bae. 12/20/2013. MIT want BIP, BIW plan date is blank
          MOVE it_master-date TO it_data-rsnum .
*--->> BS Bae. 12/20/2013.

          f_count = f_count - 1.
*---<< BS Bae. 1/10/2014. LTP PIR Issue. 7JB user date for test car
        ELSE.
          MOVE it_master-date TO it_data-rsnum .
          l_xxxy_cnt = l_xxxy_cnt + 1.
*--->> BS Bae. 1/10/2014.
        ENDIF.
      ELSE.                         "Using body UPH,always count it
        f_count = f_count - 1.
      ENDIF.

      MODIFY it_data FROM it_data.  "schedule date
    ENDLOOP.
    f_num = f_num + l_pos + l_xxxy_cnt.     "next date's begin index

  ENDLOOP.

  CLEAR z_num.
  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_input.
*---Start#3 wskim 03/14/2005
    IF it_data-rsnum EQ '00000000' OR
       it_data-rsnum IS INITIAL.
*---<< BS Bae. 12/20/2013. MIT want BIP, BIW plan date is blank
*      MOVE p_seq_date TO it_input-rsnum.

      MOVE p_seq_date TO it_input-rsnum.
*--->> BS Bae. 12/20/2013.
    ENDIF.
*---End
    APPEND it_input.
  ENDLOOP.

* reading the original data
  SELECT * INTO TABLE it_7jba
     FROM ztpp_pmt07jb_a
      WHERE  gubb = 'A'.
  .
  REFRESH it_7jb.

  SORT it_7jba BY sqdt ssr1.

*--->> BS BAE. 1/10/2014
*** chnaged by Furong on 15 Aug
**  SORT IT_INPUT BY SEQ_DATE SEQ_SERIAL.
*  SORT it_input BY serial.
**
*** end of change
**   transfer the rescheduled sequence date to 7jb_a table
*  LOOP AT it_7jba.
*    MOVE-CORRESPONDING it_7jba TO ztpp_pmt07jb_a.
*
*    READ TABLE it_input INDEX sy-tabix.
*
** * check if the two data has the same sequence
*    IF sy-subrc NE 0 OR
*      it_7jba-ordr NE it_input-work_order+0(9) OR
*      it_7jba-dist NE it_input-work_order+9(5) OR
*      it_7jba-extc NE it_input-extc            OR
*      it_7jba-intc NE it_input-intc.
*      IF l_data_error NE 'X'.
*        WRITE:/ '7jb data has different sequence with input plan data'.
*        l_data_error = 'X'.
*      ENDIF.
*    ENDIF.
*
*    MOVE : it_input-rsnum TO ztpp_pmt07jb_a-sqdt.
*    MOVE-CORRESPONDING ztpp_pmt07jb_a TO it_7jb.
*
*    APPEND it_7jb.
*    CLEAR : ztpp_pmt07jb_a, it_7jb.
*  ENDLOOP.
** delete old records of input_plan physical table before seq_date change
*  LOOP AT it_input.
*    DELETE FROM ztpp_input_plan
*         WHERE seq_date = it_input-seq_date
*           AND serial = it_input-serial.
*  ENDLOOP.
*
*  IF sy-subrc NE 0.
*    WRITE:/ 'input_plan records deletion error'.
*    EXIT.
*  ENDIF.
*
** create new sequence serial number and also save the
** number to input_plan table to keep consistency
*  LOOP AT it_7jb.
*    ON CHANGE OF it_7jb-sqdt.
*      CLEAR z_num.
*    ENDON.
*    z_num = z_num + 1.
*    MOVE z_num       TO  it_7jb-ssr1.
*    "read input plan data
*    READ TABLE it_input INDEX sy-tabix.
*    IF sy-subrc NE 0.
*      WRITE:/ 'Data was inconsistenct, check data/program'.
*    ELSE.
*      it_input-seq_date = it_7jb-sqdt.  "keep the two seq_date same
*      it_input-seq_serial = z_num.      "keep the two serial no same
*      MODIFY it_input FROM it_input INDEX sy-tabix
*                      TRANSPORTING seq_date seq_serial.
*    ENDIF.
*
*    MODIFY it_7jb FROM it_7jb.
*    CLEAR it_7jb.
*  ENDLOOP.
** update the input_plan physical table

  SORT it_input BY plnt line modl seq_date seq_serial seq_code.

  LOOP AT it_7jba.
    CLEAR: lt_7jb_tmp.
    MOVE-CORRESPONDING it_7jba TO lt_7jb_tmp.
    CLEAR: it_input.
    READ TABLE it_input WITH KEY plnt       = it_7jba-plnt
                                 line       = it_7jba-line
                                 modl       = it_7jba-modl
                                 seq_date   = it_7jba-sqdt
                                 seq_serial = it_7jba-ssr1
                                 seq_code   = it_7jba-sqcd
                        BINARY SEARCH.
    IF sy-subrc       NE 0 OR
      it_7jba-ordr    NE it_input-work_order+0(9) OR
      it_7jba-dist    NE it_input-work_order+9(5) OR
      it_7jba-extc    NE it_input-extc            OR
      it_7jba-intc    NE it_input-intc.
      l_data_error = 'X'.
      WRITE:/ '7jb data has different sequence with input plan data',
              it_7jba-plnt,
              it_7jba-line,
              it_7jba-modl,
              it_7jba-sqdt,
              it_7jba-ssr1,
              it_7jba-sqcd.
      MOVE: '999998'        TO lt_7jb_tmp-serial.
    ELSE.
      MOVE: it_input-rsnum  TO lt_7jb_tmp-sqdt,
            it_input-serial TO lt_7jb_tmp-serial.
    ENDIF.

    APPEND lt_7jb_tmp.
  ENDLOOP.

* delete old records of input_plan physical table before seq_date change
  LOOP AT it_input.
    DELETE FROM ztpp_input_plan
         WHERE seq_date = it_input-seq_date
           AND serial   = it_input-serial.

    IF it_input-work_order+12(2) EQ 'XX' OR
       it_input-work_order+12(2) EQ 'XY'.
      CLEAR: it_input-rsnum.
      MODIFY it_input.
    ENDIF.
  ENDLOOP.

* Reset sequence serial
  SORT lt_7jb_tmp BY sqdt serial.
  SORT it_input BY serial.
  LOOP AT lt_7jb_tmp.
    ON CHANGE OF lt_7jb_tmp-sqdt.
      CLEAR: z_num.
    ENDON.

    z_num = z_num + 1.

    CLEAR: it_7jb.
    MOVE-CORRESPONDING lt_7jb_tmp TO it_7jb.
    MOVE: z_num TO it_7jb-ssr1.

    APPEND it_7jb.

    READ TABLE it_input WITH KEY serial = lt_7jb_tmp-serial
                        BINARY SEARCH.
    IF sy-subrc       NE 0.
      WRITE:/ 'Data was inconsistenct',
              lt_7jb_tmp-plnt,
              lt_7jb_tmp-line,
              lt_7jb_tmp-modl,
              lt_7jb_tmp-sqdt,
              lt_7jb_tmp-ssr1,
              lt_7jb_tmp-sqcd.
      CONTINUE.
    ENDIF.

    it_input-seq_date   = it_7jb-sqdt.
    it_input-seq_serial = z_num.

    MODIFY it_input INDEX sy-tabix.
  ENDLOOP.
*---<< BS BAE. 1/10/2014

  INSERT ztpp_input_plan FROM TABLE it_input
                 ACCEPTING DUPLICATE KEYS .
  IF sy-subrc = 0.
    DELETE FROM  ztpp_pmt07jb_a WHERE gubb = 'A'.


    MODIFY ztpp_pmt07jb_a FROM TABLE it_7jb.
    IF sy-subrc EQ 0.
      MESSAGE s000 WITH 'Successfully Updated'.
      COMMIT WORK.
    ELSE.
      WRITE:/ '7jb_a table update fail'.
    ENDIF.
*--->> BS BAE. 1/10/2014
  ELSE.
    MESSAGE i000 WITH 'Input Plan master update failed.'.
    WRITE:/ 'Input Plan master update failed.'.
*---<< BS BAE. 1/10/2014
  ENDIF.
ENDFORM.                    " create_summary_re
*&---------------------------------------------------------------------*
*&      Form  date_conversion_r
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MASTER_DATE  text
*      <--P_IT_DATA_RSNUM  text
*----------------------------------------------------------------------*
FORM date_conversion_r  USING    p_before_date
                     CHANGING p_after_date.

  SELECT SINGLE *  FROM usr01
        WHERE bname = sy-uname.

  CASE usr01-datfm.
    WHEN '1'. "DD.MM.YYYY
      p_after_date+4(4) =   p_before_date+0(4).
      p_after_date+2(2) =   p_before_date+4(2).
      p_after_date+0(2) =   p_before_date+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      p_after_date+4(4) =   p_before_date+0(4).
      p_after_date+0(2) =   p_before_date+4(2).
      p_after_date+2(2) =   p_before_date+6(2).
  ENDCASE.

ENDFORM.                    " date_conversion_r
*&---------------------------------------------------------------------*
*&      Form  RECREATE_ZTPP_PMT07JB_C
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  7JB_A (GUBB = 'A'(MRP)) CHANGED, THIS PROCEDURE MUST BE USED TO
*  CREATE THE 7JB_C TABLE FOR PIR CREATION PROCEDURE
*----------------------------------------------------------------------*
FORM recreate_ztpp_pmt07jb_c.
*it_7jb_c
  DELETE FROM ztpp_pmt07jb_c
      CLIENT SPECIFIED WHERE mandt = sy-mandt .

  PERFORM saving_ztpp_pmt07jb_c.

ENDFORM.                    " RECREATE_ZTPP_PMT07JB_C
*&---------------------------------------------------------------------*
*&      Form  saving_ztpp_pmt07jb_c
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saving_ztpp_pmt07jb_c.
  DATA: l_lines           TYPE i,
        l_plmng           LIKE ztpp_pmt07jb_c-plnmg .
  DATA: l_len TYPE i,
          l_new_dealer(1).

  LOOP AT it_7jb.
    CLEAR:it_cfile.
    CASE it_7jb-gubb .
      WHEN 'A' .  it_cfile-prgrs = '1' .
      WHEN 'B' .  it_cfile-prgrs = '2' .
      WHEN 'C' .  it_cfile-prgrs = '3' .
    ENDCASE .
    it_cfile-werks = 'P001' .
    it_cfile-pbdnr = it_7jb-ordr .
    PERFORM call_nation USING it_7jb-dist it_cfile-pbdnr .

** Changed by Furong on 10/10/07 for EBOM
*    CONCATENATE it_7jb-moye it_7jb-dist it_7jb-bmdl INTO it_cfile-matnr
*.
*    CONCATENATE it_cfile-matnr it_7jb-ocnn          INTO it_cfile-matnr
*                                                    SEPARATED BY space.
    l_len = strlen( it_7jb-bmdl ).
    IF l_len = 7.
      CONCATENATE it_7jb-moye it_7jb-dist it_7jb-bmdl INTO it_cfile-matnr.
      CONCATENATE it_cfile-matnr it_7jb-ocnn          INTO it_cfile-matnr
                                                       SEPARATED BY space.
    ELSE.
      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
        EXPORTING
          old_dealer = it_7jb-dist+3(2)
        IMPORTING
          new_dealer = l_new_dealer.
      CONCATENATE it_7jb-moye it_7jb-dist+0(3) l_new_dealer it_7jb-bmdl
                    INTO it_cfile-matnr.
      CONCATENATE it_cfile-matnr it_7jb-ocnn INTO it_cfile-matnr.
    ENDIF.
** End of change

    it_cfile-pdatu = it_7jb-sqdt .
    it_cfile-pdatu = it_7jb-sqdt .
    it_cfile-cogub = 'E'         .
    it_cfile-inexc = it_7jb-extc .
    it_cfile-plnmg = it_7jb-pqty .
    it_cfile-pver  = it_7jb-pver .    APPEND it_cfile .

    it_cfile-cogub = 'I'         .
    it_cfile-inexc = it_7jb-intc .
    it_cfile-plnmg = it_7jb-pqty .
    it_cfile-pver  = it_7jb-pver .    APPEND it_cfile .
  ENDLOOP.

  SORT it_cfile BY pdatu pbdnr matnr cogub inexc .

  LOOP AT it_cfile .
    CLEAR it_cfile2 .
    MOVE-CORRESPONDING it_cfile TO it_cfile2.
    COLLECT it_cfile2 .
  ENDLOOP.

  DESCRIBE TABLE it_cfile2 LINES l_lines.
  MODIFY ztpp_pmt07jb_c  FROM TABLE it_cfile2 .
*  MODIFY ztpp_pmt07jb_c1 FROM TABLE it_cfile2 .
  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " saving_ztpp_pmt07jb_c
*&---------------------------------------------------------------------*
*&      Form  call_nation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_7JB_DIST  text
*      -->P_IT_CFILE_PBDNR  text
*----------------------------------------------------------------------*
FORM call_nation USING    pa_dist  pa_pbdnr.
  DATA: l_code               LIKE ztpp_nation_def-n_code .

  CALL FUNCTION 'Z_FPP_NATION_CODE'
    EXPORTING
      dist   = pa_dist
    IMPORTING
      n_code = l_code.

  CONCATENATE pa_pbdnr l_code INTO pa_pbdnr .
ENDFORM.                    " call_nation
*&---------------------------------------------------------------------*
*&      Form  filtering_Date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filtering_date USING p_seq_date.

  DATA : wa_master LIKE it_master.

  LOOP AT it_master INTO wa_master WHERE date > p_seq_date.
    DELETE TABLE it_master FROM wa_master .
  ENDLOOP.


ENDFORM.                    " filtering_Date
*&---------------------------------------------------------------------*
*&      Form  set_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_shop.
  IF p_body = 'X'.
    g_shop = 'B'.
  ELSE.
    g_shop = 'T'.
  ENDIF.
ENDFORM.                    " set_shop
*&---------------------------------------------------------------------*
*&      Form  check_xxxy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA  text
*      -->P_L_YN  text
*----------------------------------------------------------------------*
FORM check_xxxy USING    pt_data  LIKE it_data
                         p_yn.
  IF pt_data-work_order+12(2) EQ 'XX' OR
     pt_data-work_order+12(2) EQ 'XY'.
    p_yn = 'Y'.
  ELSE.
    p_yn = 'N'.
  ENDIF.

ENDFORM.                    " check_xxxy
*&---------------------------------------------------------------------*
*&      Form  check_hold_car
*&---------------------------------------------------------------------*
*      Check if the car is holding for future schedule
*----------------------------------------------------------------------*
FORM check_hold_car USING    pt_data  LIKE it_data
                             p_date   LIKE sy-datum
                             p_yn.

  READ TABLE it_hold WITH KEY modl = pt_data-modl
                          body_ser = pt_data-body_ser.
  IF sy-subrc = 0.
    p_yn = 'Y'.
  ELSE.
    p_yn = 'N'.
  ENDIF.

ENDFORM.                    " check_hold_car

*&---------------------------------------------------------------------*
*&      Form  read_hold_cars
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_hold_cars.

  REFRESH it_hold.

  SELECT * INTO TABLE it_hold FROM ztpp_hold_car
         WHERE  ( status EQ 'W' ) OR ( status EQ space ).

ENDFORM.                    " read_hold_cars
*&---------------------------------------------------------------------*
*&      Form  schedule_hold_cars
*&---------------------------------------------------------------------*
* some holding cars should be schecduled on this date may has been
* skiped during previous days' schedule. So find those skiped holding
* cars and scheduled first.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
FORM schedule_hold_cars USING    p_index p_serial p_date p_pos.
  DATA: wa_data  LIKE it_data.
  DATA: l_yn     .
  CLEAR: p_index.
  wa_data = it_data.     "keep the point.
* loop those cars with lower serial number and same schedule date
  LOOP AT it_hold WHERE  body_ser LT p_serial AND
                         res_date = p_date.
    IF p_index = p_pos. "can not schedule more than planned qty
      EXIT.
    ENDIF.
    CLEAR: it_data.
    READ TABLE it_data  WITH KEY modl =  it_hold-modl
                                         body_ser = it_hold-body_ser.
    IF sy-subrc = 0.
*       check if it's xx xy cars
      PERFORM check_xxxy USING wa_data l_yn.

*       schedule this car
      PERFORM refresh_date_plaf  USING it_hold-res_date .
*       add counter
      IF g_shop = 'T' AND l_yn = 'Y'.   "XX XY car, don't count
        p_index = p_index + 1.
      ENDIF.

    ENDIF.
  ENDLOOP.
  it_data = wa_data.
ENDFORM.                    " schedule_hold_cars
*&---------------------------------------------------------------------*
*&      Form  refresh_hold_cars
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_hold_cars.
  DATA: l_pass.
  DATA: l_date LIKE  sy-datum.

  PERFORM read_atinn USING 'P_RP_SATUS'.
  PERFORM read_atinn USING 'P_RP06_SHOP_DATE'.
  LOOP AT it_hold.
*   check the reporting point
    PERFORM check_rp USING l_pass l_date.

*   PLANNED SCHEDULE DATE HAS PASSED
    IF it_hold-res_date LT sy-datum.
      it_hold-status = 'P'.
      it_hold-res_date = l_date.
      MODIFY it_hold.
      CONTINUE.
    ENDIF.

*   CAR HAS PASSED THE TRIM INPUT POINT
    IF  l_pass = 'Y'.
      it_hold-status = 'P'.
      it_hold-res_date = l_date.
      MODIFY it_hold.
      CONTINUE.
    ENDIF.

    CLEAR: it_data.
    it_data-modl = it_hold-modl.
    it_data-body_ser = it_hold-body_ser.
    PERFORM refresh_date_plaf  USING it_hold-res_date .
    it_hold_sch  = it_hold.
    APPEND it_hold_sch.
  ENDLOOP.

* UPDATE THE DATABASE
  MODIFY ztpp_hold_car FROM TABLE it_hold.
  IF sy-subrc = 0.
    MESSAGE s000 WITH text-003.
    COMMIT WORK.
  ELSE.
    MESSAGE s000 WITH text-004.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " refresh_hold_cars
*&---------------------------------------------------------------------*
*&      Form  check_count
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COUNT  text
*      -->P_IT_MASTER_DATE  text
*----------------------------------------------------------------------*
FORM check_count USING    p_count
                          p_date.

  CLEAR: p_count.
  LOOP AT it_hold_sch WHERE res_date = p_date.
    p_count = p_count + 1.
  ENDLOOP.

ENDFORM.                    " check_count
*&---------------------------------------------------------------------*
*&      Form  get_body_shop_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_body_shop_data.
  PERFORM clear_varible.
  PERFORM set_shop.
*  " Get the Date for the Production Reporting Date(Last Date)
  wa_wdate = p_dates - 1.
  PERFORM read_shop_calid   USING wa_kalid.
  PERFORM read_working_date USING '-'  wa_kalid  wa_wdate.
  PERFORM get_day          USING wa_wdate it_master-day .
  PERFORM get_working_time USING wa_wdate it_master-time it_master-day .
  PERFORM get_uph        USING wa_wdate it_master-uph
                               it_master-shift g_shop.
  it_master-seq    = 99.
  it_master-date = wa_wdate .
  APPEND it_master.  CLEAR: it_master.
* get 21 days information
  PERFORM set_information USING p_dates.
  PERFORM read_inputplan_b .
* making the schedule.
  PERFORM schedule_body_input.

**** schedule the vehicle with status blank.

* read the neccesory data and do the schedule

  MESSAGE s000(zmpp) WITH 'Start reschedule_inputplan_body'.
  PERFORM reschedule_inputplan_body.

ENDFORM.                    " get_body_shop_data
*&---------------------------------------------------------------------*
*&      Form  schedule_body_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM schedule_body_input.
  DATA: l_worder             LIKE mara-matnr,
          l_flag               TYPE c ,
          l_max                TYPE i ,
          l_hours              TYPE i ,
          l_pos                TYPE i ,
          l_pos1               TYPE i ,
          l_serial             LIKE it_data-body_ser,
          l_count              TYPE i ,
          f_num                TYPE i ,
          l_loop               TYPE i ,
          l_skip               TYPE c ,
          l_hold               TYPE c ,
          l_yn                 TYPE c ,
          l_chk                TYPE p DECIMALS 3,
          l_tabix              LIKE sy-tabix,
          l_index              LIKE sy-tabix.



  DESCRIBE TABLE it_data LINES l_max.
  l_loop = l_index = 1.

  " First Day Data...
  SORT it_master BY seq .
  LOOP AT it_master WHERE seq > 80 AND seq < 90 .
    IF l_index > l_max.
      EXIT.
    ENDIF.
    l_chk = it_master-time / 3600 .
    l_pos1 = it_master-uph * l_chk.
*   check how many holding cars has been scheduled for this date
    PERFORM check_count USING l_count
                              it_master-date.

    l_pos1 = l_pos1 - l_count.          "deduct the shceduled qty
    l_count = l_pos1.
    LOOP AT it_data FROM l_index TO l_max.
      IF l_count = 0.
        EXIT.
      ENDIF.
*     if the car is holding car skip it
      PERFORM check_hold_car USING it_data it_master-date l_hold.
      IF l_hold = 'N'.    "Not a holding car, do refresh
*        PERFORM refresh_date_plaf  USING it_master-date.
        MOVE-CORRESPONDING it_data TO it_inputplan.
        it_inputplan-rd01 = it_master-date.
        APPEND it_inputplan. CLEAR: it_inputplan.
        l_count = l_count - 1.
        g_last_seq_date  = it_master-date.
        f_count = l_count.
      ENDIF.
      f_num = f_num + 1.
    ENDLOOP.
    l_index = l_index + f_num .     "next begin postion
    CLEAR f_num.
  ENDLOOP.

  " Daily Data...

  DO 21 TIMES.
    l_tabix = l_tabix + 1 .
    READ TABLE it_master WITH KEY seq = l_tabix.
    IF l_index > l_max.
      EXIT.
    ENDIF.
    l_chk = it_master-time / 3600 .
    l_pos =  it_master-uph * l_chk  .
*   check how many holding cars has been scheduled for this date
    PERFORM check_count USING l_count
                              it_master-date.

    l_pos = l_pos - l_count.          "deduct the shceduled qty
    l_count = l_pos.

    LOOP AT it_data FROM l_index TO l_max.
      IF l_count = 0.
        EXIT.
      ENDIF.

*     if the car is holding car skip it
      PERFORM check_hold_car USING it_data it_master-date l_hold.
      IF l_hold = 'N'.    "Not a holding car, do refresh

        MOVE-CORRESPONDING it_data TO it_inputplan.
        it_inputplan-rd01 = it_master-date.
        APPEND it_inputplan. CLEAR: it_inputplan.
        l_count = l_count - 1.
*       recording the last date scheduled
        g_last_seq_date  = it_master-date.
        f_count = l_count.       "qty left

      ENDIF.

      f_num = f_num + 1.
    ENDLOOP.

    l_index = l_index + f_num .
    CLEAR: f_num.
  ENDDO.

ENDFORM.                    " schedule_body_input
*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_varible.
  CLEAR: it_master           ,
         it_master[]         ,
         it_data             ,
         it_data[]           ,
         wa_disp             ,
         wa_data             ,
         wa_wdate            ,
         wa_kalid            ,
         wa_uzeit            ,
         wa_index            ,
         wa_snd_jobs         ,
         wa_rcv_jobs         ,
         wa_taskname(4)      ,
         wa_excp_flag        ,
         wa_error            ,
         wa_flag             ,
         wa_hour             ,
         f_count             ,
         it_hold             ,
         it_hold[]           ,
         it_hold_sch         ,
         g_last_seq_date     .

ENDFORM.                    " CLEAR_VARIBLE
*&---------------------------------------------------------------------*
*&      Form  reschedule_inputplan_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reschedule_inputplan_body.
  DATA : it_input   LIKE ztpp_input_plan OCCURS 0 WITH HEADER LINE.
  DATA : z_rsnum    LIKE ztpp_input_plan-rsnum,
         z_seq_date LIKE  ztpp_input_plan-seq_date.


  PERFORM get_workday_uph USING g_last_seq_date.

*get date the last SEQ_DATE
  SELECT MAX( seq_date ) INTO z_seq_date
    FROM ztpp_input_plan
      WHERE  mitu EQ space.

* delete date after the maximum sequence date
  PERFORM  filtering_date USING z_seq_date.

* read the input plan data with status blank
  PERFORM read_inputplan_re .
*
* reshcedule the vehicles with status blank
* this only update the input plan date, no
* update for plan order
  PERFORM schedule_bland USING f_count z_seq_date.

ENDFORM.                    " reschedule_inputplan_body
*&---------------------------------------------------------------------*
*&      Form  schedule_bland
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_F_COUNT  text
*      -->P_Z_SEQ_DATE  text
*----------------------------------------------------------------------*
FORM schedule_bland USING f_count p_seq_date.
  DATA: l_worder             LIKE mara-matnr,
        l_flag               TYPE c ,
        l_max                TYPE i ,
        l_hours              TYPE i ,
        l_pos                TYPE i ,
        l_loop               TYPE i ,
        l_cnt                TYPE i ,
        l_skip               TYPE c ,
        l_chk                TYPE p DECIMALS 3,
        l_tabix              LIKE sy-tabix,
        l_index              LIKE sy-tabix,
        l_serial             LIKE ztpp_input_plan-serial,
        f_num TYPE i.
  DATA : it_input LIKE ztpp_input_plan OCCURS 0 WITH HEADER LINE,
         it_7jba  LIKE ztpp_pmt07jb_a OCCURS 0 WITH HEADER LINE.
  DATA : z_num LIKE ztpp_pmt07jb_a-ssr1.
  DATA : l_yn, l_hold, l_xx.

  REFRESH :it_input,it_7jba.

  CLEAR :l_serial,f_num,l_index.
  DESCRIBE TABLE it_data LINES l_max.
  l_loop = l_index = 1.


  " First Day Data...
  SORT it_master BY seq .

* continue to schedule for the scheduled last date
* f_count:  qty left for the last day
  LOOP AT it_master WHERE  seq EQ '99' .
    l_chk = it_master-time / 3600    .
    l_pos = ( it_master-uph * l_chk ).

    LOOP AT it_data.
      IF  f_count = 0.
        EXIT.
      ENDIF.

      MOVE it_master-date TO it_data-rd01 .
      l_serial = it_data-serial.

      f_count = f_count - 1.

      MODIFY it_data FROM it_data.

      f_num = f_num + 1.
    ENDLOOP.
  ENDLOOP.

* next serial number
  IF f_num = 0.
    READ TABLE it_data INDEX 1.
    l_serial = it_data-serial.
  ELSE.
    l_serial = l_serial + 1.
  ENDIF.
* next index of internal table it_data
  f_num = f_num + 1.

  LOOP AT it_master WHERE seq < 80.
    IF l_max   <  f_num .  " no more car left for schedule
      EXIT.
    ENDIF.

    l_chk = it_master-time / 3600 .
    l_pos =  ( it_master-uph * l_chk  ).
*   check how many holding cars have been scheduled for this date
    PERFORM check_count USING f_count
                              it_master-date.

    l_pos = l_pos - f_count.          "deduct the shceduled qty
    f_count = l_pos.
    LOOP AT it_data FROM f_num TO l_max.
      IF f_count = 0.
        EXIT.
      ENDIF.
      l_serial = it_data-serial.
*     check if the car is 'xx''xy' car
      PERFORM check_xxxy USING it_data l_xx.

      MOVE it_master-date TO it_data-rd01 .
      IF g_shop = 'T'.              "using trim UPH
        IF l_xx = 'N'.              "If xx xy car, don't count it
          f_count = f_count - 1.
        ENDIF.
      ELSE.                         "Using body UPH,always count it
        f_count = f_count - 1.
      ENDIF.
      MODIFY it_data FROM it_data.  "schedule date

    ENDLOOP.
    f_num = f_num + l_pos.            "next date's begin index

  ENDLOOP.

  CLEAR z_num.
  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_inputplan.
*---Start#3 wskim 03/14/2005
    IF it_data-rd01 EQ '00000000'.
      MOVE p_seq_date TO it_inputplan-rd01.
    ENDIF.
*---End
    APPEND it_inputplan.
  ENDLOOP.
*
* update the input plan table

  UPDATE ztpp_input_plan FROM TABLE it_inputplan.
  IF sy-subrc = 0.
    MESSAGE s000 WITH 'Successfully Updated Body Plan'.
    COMMIT WORK.
  ELSE.
    MESSAGE s000 WITH 'Failed to Update Body Plan'.
  ENDIF.

ENDFORM.                    " schedule_bland
*&---------------------------------------------------------------------*
*&      Form  read_inputplan_B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_inputplan_b.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
     FROM ztpp_input_plan
     WHERE status = '00' AND
           mitu NE 'Y'.

  DESCRIBE TABLE it_data LINES  wa_hour .
  IF wa_hour = 0.
    DELETE FROM ztpp_alc_binput CLIENT SPECIFIED WHERE mandt = sy-mandt.
    LEAVE PROGRAM .
  ENDIF.

  SORT it_data BY serial .
* reading all waiting holding cars
  PERFORM read_hold_cars.

* set the initial last date
  g_last_seq_date = p_dates.

ENDFORM.                    " read_inputplan_B
*&---------------------------------------------------------------------*
*&      Form  check_rp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rp USING p_pass p_shop_date.
  DATA: wa_hold LIKE it_hold .
  DATA: l_atwrt LIKE ausp-atwrt.
  DATA: l_status LIKE it_data-status.

  CLEAR: p_pass, it_data.

  READ TABLE it_cabn WITH KEY atnam = 'P_RP_STATUS'.
  PERFORM get_status USING l_status.
*  if the car has gone to trim shop
  IF sy-subrc = 0 AND l_status GE '06'.
    PERFORM get_shop_date USING 'P_RP06_SHOP_DATE' l_atwrt.
    p_shop_date = l_atwrt.
    p_pass = 'Y'.
  ENDIF.
ENDFORM.                    " check_rp
*&---------------------------------------------------------------------*
*&      Form  GET_SHOP_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0197   text
*----------------------------------------------------------------------*
FORM get_shop_date USING  p_atnam p_atwrt.
  DATA: l_objek  LIKE ausp-objek.
  DATA: l_atinn   LIKE ausp-atinn.
* OBJEK KEY
  CONCATENATE it_hold-modl it_hold-body_ser INTO l_objek.
* GET ATINN
  READ TABLE it_cabn WITH KEY atnam = p_atnam.
* READ THE CHARACTERIC VALUE
  SELECT SINGLE atwrt INTO p_atwrt
   FROM ausp
   WHERE objek = l_objek
    AND  atinn = l_atinn.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'HOLDING CAR HAS NO RP06 SHOP DATE'.
  ENDIF.

ENDFORM.                    " GET_SHOP_DATE
*&---------------------------------------------------------------------*
*&      Form  read_atinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_atinn USING p_atnam.

  SELECT SINGLE atnam atinn INTO it_cabn
    FROM cabn
    WHERE atnam = p_atnam.
  IF sy-subrc = 0.
    APPEND it_cabn.
    CLEAR: it_cabn.
  ENDIF.

ENDFORM.                    " read_atinn
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_STATUS  text
*----------------------------------------------------------------------*
FORM get_status USING    p_status.
  SELECT SINGLE status INTO p_status
   FROM ztpp_input_plan
   WHERE modl = it_hold-modl
    AND  body_ser = it_hold-body_ser.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'HOLDING CAR HAS NO STATUS'.
  ENDIF.
ENDFORM.                    " GET_STATUS
