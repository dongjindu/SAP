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
DATA: it_7jb LIKE ztpp_pmt07jb_a OCCURS 0 WITH HEADER LINE,
      it_cfile    LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE,
      it_cfile2  LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE.
*---end
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
DATA : G_SHOP.
*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
FIELD-SYMBOLS: <wa_dfield>    TYPE ANY.

*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS: p_dates  TYPE d   DEFAULT sy-datum    OBLIGATORY.
selection-screen skip.
selection-screen begin of line.
parameters : p_trim radiobutton group rd.
selection-screen comment 10(25) text-012 for field p_trim.
parameters: p_body radiobutton group rd.
selection-screen comment 40(25) text-011 for field p_body.
selection-screen end of line.
* 07/17/2013 - T00306 Start
selection-screen skip.
parameters: p_srvgrp like rzllitab-classname obligatory
                     default 'PG_SEQ'.
* 07/17/2013 - T00306 End
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
AT SELECTION-SCREEN.
*----------------------------------------------------------------------
* set the shop for schedule
  perform set_shop.
  " Get the Date for the Production Reporting Date(Last Date)
  wa_wdate = p_dates - 1.
  PERFORM read_shop_calid   USING wa_kalid.
  PERFORM read_working_date USING '-'  wa_kalid  wa_wdate.
  PERFORM get_day          USING wa_wdate it_master-day .
  PERFORM get_working_time USING wa_wdate it_master-time it_master-day .
  PERFORM get_uph          USING wa_wdate it_master-uph it_master-shift.
  it_master-seq    = 99.
  it_master-date = wa_wdate .
  APPEND it_master.  CLEAR: it_master.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM set_information USING p_dates.
  PERFORM read_inputplan .
  PERFORM create_summary .
*----start#2 wskim 02/25/2005
  PERFORM rescheduling_inputplan.
  PERFORM recreate_ztpp_pmt07jb_c.
*----end
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

  if g_shop = 'B'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ztpp_input_plan
     WHERE status = '00'   .
  ELSEIF G_SHOP = 'T'.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ztpp_input_plan
     WHERE status IN ('00','01','02','03','04','05')
       and mitu ne 'Y'.
*    excluding the 'XX' 'XY' cars for status 01--05
     LOOP AT it_data where status ne '00'.
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
        l_loop               TYPE i ,
        l_skip               TYPE c ,
        l_chk                TYPE p DECIMALS 3,
        l_tabix              LIKE sy-tabix,
        l_index              LIKE sy-tabix.

  DESCRIBE TABLE it_data LINES l_max.
  l_loop = l_index = 1.

  " First Day Data...
  SORT it_master BY seq .
  LOOP AT it_master WHERE seq > 80 AND seq < 90 .
    l_chk = it_master-time / 3600         .
    l_pos = l_pos +  it_master-uph * l_chk   .
    IF l_pos >= l_max.
      wa_index = l_max - l_index.
      l_pos = l_max.
      l_flag = 'X' .
    ENDIF.
    LOOP AT it_data FROM l_index TO l_pos.
      PERFORM refresh_date_plaf  USING it_master-date .
    ENDLOOP.
    l_index = l_pos + 1 .
  ENDLOOP.

  " Daily Data...
  DO 21 TIMES.
    l_tabix = l_tabix + 1 .
    READ TABLE it_master WITH KEY seq = l_tabix.
    IF it_master-uph = 0.
*     l_pos   = l_pos + 1  .
    ELSE.
      l_chk = it_master-time / 3600 .
      IF l_flag = 'X'.
        EXIT.
      ENDIF.
      l_pos = l_pos +  it_master-uph * l_chk  .
      IF l_pos >= l_max.
        wa_index = l_max - l_index.
        l_pos = l_max.
        l_flag = 'X' .
      ENDIF.
      LOOP AT it_data FROM l_index TO l_pos.
        PERFORM refresh_date_plaf  USING it_master-date .
      ENDLOOP.
    ENDIF.
    l_index = l_pos + 1 .
  ENDDO.

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
  IF l_date = p_dates .
    it_master-seq    = '80'.       it_master-date   = l_date .
    PERFORM get_day          USING l_date it_master-day  .
    PERFORM get_worktime1    USING l_date it_master-time it_master-day .
  ELSE.
    l_date = p_dates .
    PERFORM get_day         USING l_date it_master-day  .
    it_master-seq    = '81'.       it_master-date   = l_date .
    APPEND it_master.       CLEAR: it_master.
  ENDIF.

  " From D+1 Day To D+21 Day..  (Only Working Dates in FACTORY-Calendar)
  l_date = p_dates .
  DO 21 TIMES.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+'  wa_kalid  l_date.
    PERFORM get_day          USING l_date it_master-day  .
    PERFORM get_working_time USING l_date it_master-time it_master-day.
    PERFORM get_uph          USING l_date it_master-uph it_master-shift.
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
    PERFORM get_uph   USING pa_wdate it_master-uph it_master-shift.
    APPEND it_master.
    l_flag = 'X' .
  ENDLOOP.
  IF l_flag = space.
    PERFORM get_uph   USING pa_wdate it_master-uph it_master-shift.
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
FORM get_uph USING    pa_wdate  pa_uph  pa_shift .
  data: w_uph  like ZTPP_STATUS-UPH.

  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      date          = pa_wdate
      SHIFT         = pa_shift
      SHOP          = 'T'
    IMPORTING
      UPH           = w_uph
            .
  pa_uph  = w_uph.

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
** add by chris on 06/13/2005
*    loop at lt_ld.
*      lw_ld-lrate = lw_ld-lrate + lt_ld-lrate.
*      lw_ld-lantu = lw_ld-lantu + lt_ld-lantu.
*    endloop.
** end of add.
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
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

  PERFORM read_planorder   USING l_porder.

  DO .
    CALL FUNCTION 'Z_FPP_PLANORDER_RESCHEDULE'
        STARTING NEW TASK wa_taskname DESTINATION IN GROUP p_srvgrp
         PERFORMING return_step1 ON END OF TASK
      EXPORTING
        p_porder                    = l_porder
        p_date                      = pa_date
      EXCEPTIONS
        communication_failure       = 1
        system_failure              = 2
        RESOURCE_FAILURE            = 3
        OTHERS                      = 4 .

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
      object             = l_vehicle
      mode               = 'R'
      ctype              = '002'
*     DISPLAY            = 'D'
    TABLES
      val_table          = lt_val .

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
         RESOURCE_FAILURE            = 3
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
*find the last scheduled date
  SELECT MAX( rsnum ) INTO z_rsnum
    FROM ztpp_input_plan
      WHERE mitu EQ space
        AND status EQ '00'.

  PERFORM get_workday_uph USING z_rsnum.
*---Start#3 wskim 03/14/2005
*get date the last SEQ_DATE
  SELECT MAX( seq_date ) INTO z_seq_date
    FROM ztpp_input_plan
      WHERE plnt = '1'
        AND line = '1'
        AND mitu EQ space.

  PERFORM  filtering_date USING z_seq_date.
*---End
  PERFORM read_inputplan_re USING z_rsnum
                            CHANGING f_count.
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
  PERFORM get_uph          USING wa_wdate it_master-uph it_master-shift.
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
FORM read_inputplan_re USING p_rsnum
                       CHANGING z_count.
  REFRESH it_data.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_input_plan
   WHERE  status = space .

  SELECT COUNT( * ) INTO z_count
    FROM ztpp_input_plan
       WHERE status EQ '00'
         AND mitu EQ space
         AND rsnum EQ p_rsnum.

*  DESCRIBE TABLE it_data LINES  wa_hour .
*  IF wa_hour = 0.
*    DELETE FROM ztpp_alc_binput CLIENT SPECIFIED WHERE mandt = sy-mandt
.
*    LEAVE PROGRAM .
*  ENDIF.

  SORT it_data BY serial .
  LOOP AT it_data.
    CLEAR it_data-rsnum.
    MODIFY it_data FROM it_data INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " read_inputplan_re
*&---------------------------------------------------------------------*
*&      Form  create_summary_re
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_summary_re USING f_count p_seq_date.
  DATA: l_worder             LIKE mara-matnr,
        l_flag               TYPE c ,
        l_max                TYPE i ,
        l_hours              TYPE i ,
        l_pos                TYPE i ,
        l_loop               TYPE i ,
        l_skip               TYPE c ,
        l_chk                TYPE p DECIMALS 3,
        l_tabix              LIKE sy-tabix,
        l_index              LIKE sy-tabix,
        l_serial             LIKE ztpp_input_plan-serial,
        f_num TYPE i.
  DATA : it_input LIKE ztpp_input_plan OCCURS 0 WITH HEADER LINE,
         it_7jba LIKE ztpp_pmt07jb_a OCCURS 0 WITH HEADER LINE.
  DATA : z_num LIKE ztpp_pmt07jb_a-ssr1.


  REFRESH :it_input,it_7jba.

  CLEAR :l_serial,f_num,l_index.
  DESCRIBE TABLE it_data LINES l_max.
  l_loop = l_index = 1.

  " First Day Data...
  SORT it_master BY seq .


  LOOP AT it_master WHERE  seq EQ '99' .
    LOOP AT it_data.
      l_chk = it_master-time / 3600    .
      l_pos = ( it_master-uph * l_chk ).
      IF l_pos = f_count.
        CONTINUE.
      ELSE.
        MOVE it_master-date TO it_data-rsnum .
        l_serial = it_data-serial.
        f_count = f_count + 1.
        f_num = f_num + 1.
        MODIFY it_data FROM it_data.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

*  l_serial = l_serial + 1.
  l_max = l_max - f_num.
  IF f_num = 0.
    READ TABLE it_data INDEX 1.
    l_serial = it_data-serial.
  ELSE.
    l_serial = l_serial + 1.
  ENDIF.

  LOOP AT it_master WHERE seq < 80.
    l_chk = it_master-time / 3600 .
    l_pos = ( it_master-uph * l_chk  ).
    DO l_pos TIMES.
      IF l_max   <  l_index.
        EXIT.
      ENDIF.
      READ TABLE it_data WITH KEY serial = l_serial.
      IF sy-subrc = 0.
        LOOP AT  it_data WHERE serial = l_serial.
          MOVE it_master-date TO it_data-rsnum .
          MODIFY it_data FROM it_data.
        ENDLOOP.
        l_index = l_index + 1.
      ELSE.
        EXIT.
      ENDIF.
      l_serial = l_serial + 1.
    ENDDO.
  ENDLOOP.

  CLEAR z_num.
  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_input.
*---Start#3 wskim 03/14/2005
    IF it_data-rsnum EQ '00000000'.
    MOVE p_seq_date TO it_input-rsnum.
    ENDIF.
*---End
    APPEND it_input.
  ENDLOOP.

  LOOP AT it_input.
    DELETE FROM ztpp_input_plan
         WHERE seq_date = it_input-seq_date
           AND serial = it_input-serial.
  ENDLOOP.
  INSERT ztpp_input_plan FROM TABLE it_input
                     ACCEPTING DUPLICATE KEYS .

  IF sy-subrc = 0.

    SELECT * INTO TABLE it_7jba
     FROM ztpp_pmt07jb_a
      WHERE  gubb = 'A'.

    DELETE FROM  ztpp_pmt07jb_a WHERE gubb = 'A'.
    REFRESH it_7jb.

*    SORT it_7jba BY sqdt ssr1.
    LOOP AT it_7jba.
      MOVE-CORRESPONDING it_7jba TO ztpp_pmt07jb_a.
      READ TABLE it_input INDEX sy-tabix.
      MOVE : it_input-rsnum TO ztpp_pmt07jb_a-sqdt.
      MOVE-CORRESPONDING ztpp_pmt07jb_a TO it_7jb.

      APPEND it_7jb.
      CLEAR : ztpp_pmt07jb_a, it_7jb.
    ENDLOOP.

*    SORT it_7jb BY sqdt ssr1.
    LOOP AT it_7jb.
      ON CHANGE OF it_7jb-sqdt.
        CLEAR z_num.
      ENDON.
      z_num = z_num + 1.
      MOVE z_num       TO  it_7jb-ssr1.

      MODIFY it_7jb FROM it_7jb.
      CLEAR it_7jb.
    ENDLOOP.

    MODIFY ztpp_pmt07jb_a FROM TABLE it_7jb.
    IF sy-subrc = 0.
      message s000 with 'Successfully Updated'.
      COMMIT WORK.
    ENDIF.
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
    CONCATENATE it_7jb-moye it_7jb-dist it_7jb-bmdl INTO it_cfile-matnr.
    CONCATENATE it_cfile-matnr it_7jb-ocnn          INTO it_cfile-matnr
                                                    SEPARATED BY space.
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
  if p_body = 'X'.
     g_shop = 'B'.
  else.
     g_shop = 'T'.
  endif.
ENDFORM.                    " set_shop
