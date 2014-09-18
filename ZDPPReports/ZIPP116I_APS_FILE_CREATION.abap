************************************************************************
* Program Name      : ZIPP116I_APS_FILE_CREATION
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2004.03.22.
* Specifications By : KIM JONG WON
* Development Request No : UD1K906972
* Addl Documentation:
* Description       : Creation of APS's Files By Lump Job.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
* 01/17/2007 Haseeb Mohammad UD1K930018  Stop deleting the batch jobs *
*                                        from sm37
************************************************************************
report   zipp116i_aps_file_creation   message-id zmpp         .

data: jobc                    like tbtcjob-jobcount           ,
      jobn                    like  tbtcjob-jobname           ,
      immediate               like btch0000-char1  value  'X' ,
      c_prog                  like sy-repid                   .

data: global_job          like table of tbtcjob        with header line,
      it_joblist          like table of tbtcjob        with header line,
      global_start_date   like table of tbtcstrt       with header line,
      global_step_tbl     like table of tbtcstep       with header line.

data: wa_time type sy-uzeit ,
      wa_date type sy-datum .

ranges:  r_jobnam FOR tbtcp-jobname,
         r_pronam FOR tbtcp-progname,
         r_date FOR tbtcp-sdldate,
         r_time FOR tbtcp-sdltime.

selection-screen begin of block b1 with frame.
parameters   p_run      as checkbox   default 'X'.
selection-screen end of block b1.
**********************************************
initialization.
**********************************************
  wa_date = sy-datum .
  wa_time = sy-uzeit .


***********************************************
start-of-selection.
***********************************************
  check p_run = 'X'.
  write:/(25) 'Starting Date & Time :',
          wa_date, ' ', wa_time.
*
* 6GB
  c_prog = 'ZIPP107I_APS_6GB1'.
  perform run_batch_job using c_prog '6GB'.
* 1AA
  c_prog = 'ZIPP103I_APS_1AA1'.
  perform run_batch_job using c_prog '1AA'.
* 3FB
  c_prog = 'ZIPP105I_APS_3FB1'.
  perform run_batch_job using c_prog '3FB'.
* 3BB
  c_prog = 'ZIPP104I_APS_3BB1'.
  perform run_batch_job using c_prog '3BB'.
* 3NB
  c_prog = 'ZIPP106I_APS_3NB1'.
  perform run_batch_job using c_prog '3NB'.
* 6EA
  c_prog = 'ZIPP108I_APS_6EA1'.
  perform run_batch_job using c_prog '6EA'.
* 7CW
  c_prog = 'ZIPP109I_APS_7CW1'.
  perform run_batch_job using c_prog '7CW'.
* 7DW
  c_prog = 'ZIPP110I_APS_7DW1'.
  perform run_batch_job using c_prog '7DW'.
* 7GB
  c_prog = 'ZIPP111I_APS_7GB1'.
  perform run_batch_job using c_prog '7GB'.

*
  data: l_error type c.
  c_prog = 'ZIPP107I_APS_6GB1'.
  perform check_6gb_job_ending using c_prog .
  perform check_6gb_success using l_error.

  if l_error = 'X'.
    perform check_job_ending using 'X'.
    write:/(25) 'Ending Date & Time :',
            sy-datum, ' ', sy-uzeit .
    perform write_job_log using 'X'.  "<-- Error occurred

  else.
* 5AB
    c_prog = 'ZIPP113I_APS_5AB1'.
    perform run_batch_job using c_prog '5AB'.
* 6FB
    c_prog = 'ZIPP114I_APS_6FB1'.
    perform run_batch_job using c_prog '6FB'.
*
    perform check_job_ending using ' '.
    write:/(25) 'Ending Date & Time :',
            sy-datum, ' ', sy-uzeit .
    perform write_job_log using ' '.
  endif.
*Haseeb Commented this as to see all the jobs in SM37,
*  PERFORM delete_joblist.


*&---------------------------------------------------------------------*
*&      Form  RUN_BATCH_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_PROG  text
*      -->P_0093   text
*----------------------------------------------------------------------*
form run_batch_job using    p_prog
                            p_table.
  refresh: global_job, global_step_tbl.
  clear:   global_job, global_step_tbl.
*
  concatenate 'APS_' p_table '_CRE' into jobn.
*
  global_job-jobname = jobn.             "S_PROG.
  global_job-jobclass = 'A'.             "
  global_job-newflag = 'O'.
  global_step_tbl-program = 'RSBTCPT3'.  "dummy
  global_step_tbl-typ = 'A'.             "
  global_step_tbl-status = 'P'.          "scheduled
  global_step_tbl-authcknam = sy-uname.
  append global_step_tbl.
  append global_job.

  call function 'BP_JOB_CREATE'
       exporting
            job_cr_dialog       = 'N'
            job_cr_head_inp     = global_job
       importing
            job_cr_head_out     = global_job
       tables
            job_cr_steplist     = global_step_tbl
       exceptions
            cant_create_job     = 1
            invalid_dialog_type = 2
            invalid_job_data    = 3
            job_create_canceled = 4
            others              = 5.

  jobc = global_job-jobcount.
  jobn = global_job-jobname.

  data: p_mark type c value 'X'.
  submit (p_prog)
    and  return
    with p_run = p_mark
    via job jobn number jobc.

  call function 'JOB_CLOSE'
       exporting
            jobcount  = jobc
            jobname   = jobn
            strtimmed = immediate
       exceptions
            others    = 4.
endform.                    " RUN_BATCH_JOB
*&---------------------------------------------------------------------*
*&      Form  write_job_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_job_log using p_error .
  data: l_jobname(20) type c.
* 6GB
  l_jobname = 'APS_6GB_CRE'.
  perform call_func_job_log using l_jobname .
* 1AA
  l_jobname = 'APS_1AA_CRE'.
  perform call_func_job_log using l_jobname .
* 3BB
  l_jobname = 'APS_3BB_CRE'.
  perform call_func_job_log using l_jobname .
* 3FB
  l_jobname = 'APS_3FB_CRE'.
  perform call_func_job_log using l_jobname .
* 3NB
  l_jobname = 'APS_3NB_CRE'.
  perform call_func_job_log using l_jobname .
* 6EA
  l_jobname = 'APS_6EA_CRE'.
  perform call_func_job_log using l_jobname .
* 7CW
  l_jobname = 'APS_7CW_CRE'.
  perform call_func_job_log using l_jobname .
* 7DW
  l_jobname = 'APS_7DW_CRE'.
  perform call_func_job_log using l_jobname .
* 7GB
  l_jobname = 'APS_7GB_CRE'.
  perform call_func_job_log using l_jobname .

  if p_error = 'X'.
    format color col_negative.
    write:/10(30) '6GB Creation is Failed...' .
    format reset .
  else.
* 5AB
    l_jobname = 'APS_5AB_CRE'.
    perform call_func_job_log using l_jobname .
* 6FB
    l_jobname = 'APS_6FB_CRE'.
    perform call_func_job_log using l_jobname .
  endif.

endform.                    " write_job_log
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNC_JOB_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_JOBNAME  text
*----------------------------------------------------------------------*
form call_func_job_log using    p_jobname.
  data: jobcount           like tbtcjob-jobcount, "job ID
        select_info        like btcselect,
        btc_joblist_edit   like btch0000-int4 value 21,
        btc_joblist_show   like btch0000-int4 value 22,
        btc_joblist_select like btch0000-int4 value 23,
        btc_joblog_show    like btch0000-int4 value 24.

  data: begin of job_list occurs 0.
          include structure tbtcjob.
  data: end of job_list.

  select_info-jobname = p_jobname.
  select_info-jobgroup = '*'.
  select_info-username = sy-uname.
  select_info-from_date = wa_date.
  wa_time = wa_time - 10.
  select_info-from_time = wa_time.
  select_info-to_date = sy-datum.
  select_info-to_time = sy-uzeit.

  select_info-prelim = 'X'.
  select_info-schedul = 'X'.
  select_info-ready = 'X'.
  select_info-running = 'X'.
  select_info-finished = 'X'.
  select_info-aborted = 'X'.

  call function 'BP_JOB_SELECT'
       exporting
            jobselect_dialog    = 'N'  " no dialog
            jobsel_param_in     = select_info
       importing
            jobsel_param_out    = select_info
       tables
            jobselect_joblist   = job_list
       exceptions
            invalid_dialog_type = 1
            jobname_missing     = 2
            no_jobs_found       = 3
            selection_canceled  = 4
            username_missing    = 5
            others              = 6.

  check sy-subrc = 0.
  read table job_list index 1 .
  perform joblog using job_list-jobname
                       job_list-jobcount
                       job_list-joblog
                       job_list-authckman.

endform.                    " CALL_FUNC_JOB_LOG
*&---------------------------------------------------------------------*
*&      Form  joblog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_JOB_LIST_JOBNAME  text
*      -->P_JOB_LIST_JOBCOUNT  text
*      -->P_JOB_LIST_joblog  text
*      -->P_JOB_LIST_AUTHCKMAN  text
*----------------------------------------------------------------------*
form joblog using p_job_list_jobname
                  p_job_list_jobcount
                  p_job_list_joblog
                  p_job_list_authckman.

  data: begin of joblog_tab occurs 10.
          include structure tbtc5.
  data: end of joblog_tab.

  call function 'BP_JOBLOG_READ'
       exporting
            joblog                = p_job_list_joblog
            client                = p_job_list_authckman
       tables
            joblogtbl             = joblog_tab
       exceptions
            cant_read_joblog      = 2
            joblog_does_not_exist = 4
            joblog_is_empty       = 6
            others                = 99.
  data: l_flag.
  format color col_heading.
  skip. skip.
  write:/ p_job_list_jobname.
  write:/10(08) 'Date',
           (10) 'Time',
           (04) 'Type',
           (60) 'Message'.
  format reset .
  loop at joblog_tab where msgid = '00' or msgid = 'ZMPP' .
    if joblog_tab-msgtype = 'W' or
       joblog_tab-msgtype = 'E'   .
      format color col_negative.
    else.
      if l_flag = 'X'.
        l_flag = ' '.
        format color col_normal.
      else.
        l_flag = 'X'.
        format color col_key .
      endif.
    endif.
    write:/10(08) joblog_tab-entertime,
             (10) joblog_tab-enterdate,
             (04) joblog_tab-msgtype,
             (60) joblog_tab-text .
  endloop.

endform. " JOBLOG
*&---------------------------------------------------------------------*
*&      Form  check_job_ending
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_job_ending using p_error .
  data: l_prog type sy-repid .
  commit work .

  clear: r_jobnam, r_jobnam[], r_pronam, r_pronam[],
         r_date,   r_date[],   r_time,   r_time[]  .
* 1AA
  l_prog = 'ZIPP103I_APS_1AA1'.
* r_pronam = 'IEQZIPP103I_APS_1AA1'.  APPEND r_pronam.
  r_jobnam = 'IEQAPS_1AA_CRE'      .  APPEND r_jobnam.
  perform check_each_job_ending using l_prog .
* 3BB
  l_prog = 'ZIPP104I_APS_3BB1'.
* r_pronam = 'IEQZIPP104I_APS_3BB1'.  APPEND r_pronam.
  r_jobnam = 'IEQAPS_3BB_CRE'      .  APPEND r_jobnam.
  perform check_each_job_ending using l_prog .
* 3FB
  l_prog = 'ZIPP105I_APS_3FB1'.
* r_pronam = 'IEQZIPP105I_APS_3FB1'.  APPEND r_pronam.
  r_jobnam = 'IEQAPS_3FB_CRE'      .  APPEND r_jobnam.
  perform check_each_job_ending using l_prog .
* 3NB
  l_prog = 'ZIPP106I_APS_3NB1'.
* r_pronam = 'IEQZIPP106I_APS_3NB1'.  APPEND r_pronam.
  r_jobnam = 'IEQAPS_3NB_CRE'      .  APPEND r_jobnam.
  perform check_each_job_ending using l_prog .
* 6EA
  l_prog = 'ZIPP108I_APS_6EA1'.
* r_pronam = 'IEQZIPP108I_APS_6EA1'.  APPEND r_pronam.
  r_jobnam = 'IEQAPS_6EA_CRE'      .  APPEND r_jobnam.
  perform check_each_job_ending using l_prog .
* 7CW
  l_prog = 'ZIPP109I_APS_7CW1'.
* r_pronam = 'IEQZIPP109I_APS_7CW1'.  APPEND r_pronam.
  r_jobnam = 'IEQAPS_7CW_CRE'      .  APPEND r_jobnam.
  perform check_each_job_ending using l_prog .
* 7DW
  l_prog = 'ZIPP110I_APS_7DW1'.
* r_pronam = 'IEQZIPP110I_APS_7DW1'.  APPEND r_pronam.
  r_jobnam = 'IEQAPS_7DW_CRE'      .  APPEND r_jobnam.
  perform check_each_job_ending using l_prog .
* 7GB
  l_prog = 'ZIPP111I_APS_7GB1'.
* r_pronam = 'IEQZIPP111I_APS_7GB1'.  APPEND r_pronam.
  r_jobnam = 'IEQAPS_7GB_CRE'      .  APPEND r_jobnam.
  perform check_each_job_ending using l_prog .

  if p_error <> 'X'.
* 5AB
    l_prog = 'ZIPP113I_APS_5AB1'.
*   r_pronam = 'IEQZIPP113I_APS_5AB1'.  APPEND r_pronam.
    r_jobnam = 'IEQAPS_5AB_CRE'      .  APPEND r_jobnam.
    perform check_each_job_ending using l_prog .
* 6FB
    l_prog = 'ZIPP114I_APS_6FB1'.
*   r_pronam = 'IEQZIPP114I_APS_6FB1'.  APPEND r_pronam.
    r_jobnam = 'IEQAPS_6FB_CRE'      .  APPEND r_jobnam.
    perform check_each_job_ending using l_prog .
  endif.
endform.                    " check_job_ending
*&---------------------------------------------------------------------*
*&      Form  check_each_job_ending
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_PROG  text
*----------------------------------------------------------------------*
form check_each_job_ending using    p_prog.
  data: it_joblist like table of tbtcjob with header line.

  do.
    clear it_joblist.
    refresh it_joblist.
    call function 'BP_FIND_JOBS_WITH_PROGRAM'
         exporting
              abap_program_name             = p_prog
              dialog                        = 'N'
         tables
              joblist                       = it_joblist
         exceptions
              no_jobs_found                 = 1
              program_specification_missing = 2
              invalid_dialog_type           = 3
              job_find_canceled             = 4
              others                        = 5.

    if sy-subrc <> 0.
      exit.
    endif.
    read table it_joblist with key jobname(3) = 'APS'
                                   status = 'S'.
    if sy-subrc = 0.
      continue.
    else.
      read table it_joblist with key jobname(3) = 'APS'
                                     status = 'R'.
      if sy-subrc = 0.
        continue.
      else.
        exit.
      endif.
    endif.
  enddo.
endform.                    " check_each_job_ending

*&---------------------------------------------------------------------*
*&      Form  CHECK_6GB_SUCCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_SERULT  text
*----------------------------------------------------------------------*
form check_6gb_success using    p_error.
  data: jobcount           like tbtcjob-jobcount, "job ID
        select_info        like btcselect,
        btc_joblist_edit   like btch0000-int4 value 21,
        btc_joblist_show   like btch0000-int4 value 22,
        btc_joblist_select like btch0000-int4 value 23,
        btc_joblog_show    like btch0000-int4 value 24.

  data: begin of job_list occurs 0.
          include structure tbtcjob.
  data: end of job_list.

  select_info-jobname = 'APS_6GB'.
  select_info-jobgroup = '*'.
  select_info-username = sy-uname.
  select_info-from_date = wa_date.
  wa_time = wa_time - 10.
  select_info-from_time = wa_time.
  select_info-to_date = sy-datum.
  select_info-to_time = sy-uzeit.

  select_info-prelim = 'X'.
  select_info-schedul = 'X'.
  select_info-ready = 'X'.
  select_info-running = 'X'.
  select_info-finished = 'X'.
  select_info-aborted = 'X'.

  call function 'BP_JOB_SELECT'
       exporting
            jobselect_dialog    = 'N'  " no dialog
            jobsel_param_in     = select_info
       importing
            jobsel_param_out    = select_info
       tables
            jobselect_joblist   = job_list
       exceptions
            invalid_dialog_type = 1
            jobname_missing     = 2
            no_jobs_found       = 3
            selection_canceled  = 4
            username_missing    = 5
            others              = 6.

  check sy-subrc = 0.
  read table job_list index 1 .
*  PERFORM joblog USING job_list-jobname
*                       job_list-jobcount
*                       job_list-joblog
*                       job_list-authckman.
  data: begin of joblog_tab occurs 10.
          include structure tbtc5.
  data: end of joblog_tab.

  call function 'BP_JOBLOG_READ'
       exporting
            joblog                = job_list-joblog
            client                = job_list-authckman
       tables
            joblogtbl             = joblog_tab
       exceptions
            cant_read_joblog      = 2
            joblog_does_not_exist = 4
            joblog_is_empty       = 6
            others                = 99.
  data: l_flag.
*  FORMAT COLOR COL_HEADING.
*  SKIP. SKIP.
*  WRITE:/ job_list-jobname.
*  WRITE:/10(08) 'Date',
*           (10) 'Time',
*           (04) 'Type',
*           (60) 'Message'.
*  FORMAT RESET .
  loop at joblog_tab where msgid = '00' or msgid = 'ZMPP' .
    if joblog_tab-msgtype = 'W' or
       joblog_tab-msgtype = 'E'   .
*      FORMAT COLOR COL_NEGATIVE.
      p_error = 'X'.
    else.
*      IF l_flag = 'X'.
*        l_flag = ' '.
*        FORMAT COLOR COL_NORMAL.
*      ELSE.
*        l_flag = 'X'.
*        FORMAT COLOR COL_KEY .
*      ENDIF.
    endif.
*    WRITE:/10(08) joblog_tab-entertime,
*             (10) joblog_tab-enterdate,
*             (04) joblog_tab-msgtype,
*             (60) joblog_tab-text .

  endloop.

endform.                    " CHECK_6GB_SUCCESS
*&---------------------------------------------------------------------*
*&      Form  check_each_job_end_6gb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_PROG  text
*----------------------------------------------------------------------*
form check_6gb_job_ending using    p_prog.
  wait up to 10 seconds.
  do.
    clear it_joblist.
    refresh it_joblist.
    call function 'BP_FIND_JOBS_WITH_PROGRAM'
         exporting
              abap_program_name             = p_prog
              dialog                        = 'N'
         tables
              joblist                       = it_joblist
         exceptions
              no_jobs_found                 = 1
              program_specification_missing = 2
              invalid_dialog_type           = 3
              job_find_canceled             = 4
              others                        = 5.

    if sy-subrc <> 0.
      exit.
    endif.
    read table it_joblist with key jobname(3) = 'APS'
                                   status = 'S'.
    if sy-subrc = 0.
      continue.
    else.
      read table it_joblist with key jobname(3) = 'APS'
                                     status = 'R'.
      if sy-subrc = 0.
        continue.
      else.
        exit.
      endif.
    endif.
  enddo.

endform.                    " check_each_job_end_6gb

*&---------------------------------------------------------------------*
*&      Form  delete_joblist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_joblist.
  DATA: it_tbtcp LIKE TABLE OF tbtcp WITH HEADER LINE.

  SELECT * FROM tbtcp
           INTO TABLE it_tbtcp
           WHERE jobname  IN r_jobnam
             AND progname IN r_pronam
             AND sdldate  IN r_date
             AND sdltime  IN r_time.

  IF sy-subrc = 0.
    DELETE tbtcp FROM TABLE it_tbtcp.
  ENDIF.
ENDFORM.                    " delete_joblist
