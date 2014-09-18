report zaco92a_wip_convert .
tables: mbew.

parameter: p_bukrs   type bukrs memory id BUK.
parameter: s_yymm(6) type n default '200406'.
parameter: e_yymm(6) type n default '200612'.
parameter: p_data(10) type c default 'WIP'.
select-options: s_bklas for mbew-bklas,
                s_matnr for mbew-matnr.

data: wa_jobcount like  tbtcjob-jobcount,
      wa_jobname  like  tbtcjob-jobname,
      wa_report   like  sy-repid.

data: yymm(6) type n,
      yy(4)   type n,
      s_txt(50) type c.

initialization.
  s_bklas-option = 'EQ'.
  s_bklas-sign   = 'I'.
  s_bklas-low = '3000'. append s_bklas.
  s_bklas-low = '3001'. append s_bklas.
  s_bklas-low = '3002'. append s_bklas.
  s_bklas-low = '3003'. append s_bklas.
  s_bklas-low = '3004'. append s_bklas.
  s_bklas-low = '3005'. append s_bklas.
  s_bklas-low = '7900'. append s_bklas.
  s_bklas-low = '7920'. append s_bklas.

start-of-selection.
*start
  yymm = s_yymm.
  do.
    concatenate 'Processing...' p_data ';' yymm into s_txt.
    call function 'FI_PROGRESS_INDICATOR'
         exporting
              text = s_txt.

    case p_data.
      when 'WIP'.
        submit zaco92a_wip_new
                with p_yymm = yymm
                with r_bct  = 'X'
                with r_db   = space
           and return.
        commit work.

      when 'MH'.
        wa_jobname = 'MH_ROLLUP'.
        perform call_job_open using wa_jobname wa_jobcount.
        submit zaco92a_mh_roll_up
                with p_kokrs = 'H201'
                with p_poper = yymm+4(2)
                with p_bdatj = yymm(4)
                with p_cc_grp = 'MH'
                with p_act = 'X'
                with p_bpl = space
            and return.
        if sy-subrc <> 0.
        else.
          perform call_job_close using wa_jobname wa_jobcount.
        endif.

      when 'SHOP'.
        submit zaco19u_shop_par
                with p_bdatj = yymm(4)
                with p_kokrs = 'H201'
                with p_perab = yymm+4(2)
                with p_pr_cnt = 1
                with p_ta_cnt = 10
                with p_wt_scn = 5
            and return.

      when 'SHOPABP'.
        wa_jobname = 'SHOP_PLN'.
        perform call_job_open using wa_jobname wa_jobcount.

        submit zaco09u_shop_new
           via job wa_jobname number wa_jobcount and return
                with p_bdatj = yymm(4)
                with p_perab = yymm+4(2)
                with p_kokrs = 'H201'
                with p_bpl = 'X'.

        if sy-subrc <> 0.
        else.
          perform call_job_close using wa_jobname wa_jobcount.
        endif.

      when 'MLS'.
        wa_jobname = 'MLSUM'.
        perform call_job_open using wa_jobname wa_jobcount.

        submit zaco11r_ml01
           via job wa_jobname number wa_jobcount and return
                with p_bdatj = yymm(4)
                with p_poper = yymm+4(2)
                with p_curtp = '10'
                with p_kokrs = 'H201'
                with p_up = 'X'
                with p_db = space
                with p_ml = 'X'
                with s_bklas in s_bklas
                with s_matnr in s_matnr.

        if sy-subrc <> 0.
        else.
          perform call_job_close using wa_jobname wa_jobcount.
        endif.

      when 'MLI'.
        wa_jobname = 'ML_ITEM'.
        perform call_job_open using wa_jobname wa_jobcount.

        submit zaco11r_ml03
           via job wa_jobname number wa_jobcount and return
                with p_bdatj = yymm(4)
                with s_poper = yymm+4(2)
                with p_curtp = '10'
                with p_kokrs = 'H201'
                with p_up = 'X'
                with p_db = space
                with s_bklas in s_bklas
                with s_matnr in s_matnr.
        if sy-subrc <> 0.
        else.
          perform call_job_close using wa_jobname wa_jobcount.
        endif.

      when 'MMVAR'.
        wa_jobname = 'Actual Variance'.
        perform call_job_open using wa_jobname wa_jobcount.

        submit zacou111
           via job wa_jobname number wa_jobcount and return
                with p_bukrs = p_bukrs
                with p_gr    = 'X'
                with p_im    = 'X'
                with p_iv    = 'X'
                with p_rv    = 'X'
                with p_month = yymm
                with p_test  = space.
        if sy-subrc <> 0.
        else.
          perform call_job_close using wa_jobname wa_jobcount.
        endif.

    endcase.

    yymm = yymm + 1.
    if yymm+4(2) = '13'.
      yy = yymm(4) + 1.
      yymm(4) = yy.
      yymm+4(2) = '01'.
    endif.
    if yymm > e_yymm. exit. endif.
  enddo.
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_OPEN
*&---------------------------------------------------------------------*
form call_job_open using p_jobname p_jobcount.
  call function 'JOB_OPEN'
    exporting
*      DELANFREP              = ' '
*      JOBGROUP               = ' '
      jobname                = p_jobname
*      SDLSTRTDT              = NO_DATE
*      SDLSTRTTM              = NO_TIME
    importing
      jobcount               = p_jobcount
    exceptions
      cant_create_job        = 1
      invalid_job_data       = 2
      jobname_missing        = 3
      others                 = 4.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
endform.                    " CALL_JOB_OPEN
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_SUBMIT
*&---------------------------------------------------------------------*
form call_job_submit using p_jobname
                           p_report
                           p_jobcount.

  call function 'JOB_SUBMIT'
    exporting
*      ARCPARAMS                         =
      authcknam                         = sy-uname
*      COMMANDNAME                       = ' '
*      OPERATINGSYSTEM                   = ' '
*      EXTPGM_NAME                       = ' '
*      EXTPGM_PARAM                      = ' '
*      EXTPGM_SET_TRACE_ON               = ' '
*      EXTPGM_STDERR_IN_JOBLOG           = 'X'
*      EXTPGM_STDOUT_IN_JOBLOG           = 'X'
*      EXTPGM_SYSTEM                     = ' '
*      EXTPGM_RFCDEST                    = ' '
*      EXTPGM_WAIT_FOR_TERMINATION       = 'X'
      jobcount                          = p_jobcount
      jobname                           = p_jobname
*      LANGUAGE                          = SY-LANGU
*      PRIPARAMS                         = ' '
      report                            = p_report
*      VARIANT                           = ' '
*    IMPORTING
*      STEP_NUMBER                       =
      exceptions
      bad_priparams                     = 1
      bad_xpgflags                      = 2
      invalid_jobdata                   = 3
      jobname_missing                   = 4
      job_notex                         = 5
      job_submit_failed                 = 6
      lock_failed                       = 7
      program_missing                   = 8
      prog_abap_and_extpg_set           = 9
      others                            = 10.

*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
endform.                    " CALL_JOB_SUBMIT
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
form call_job_close using p_jobname p_jobcount.
  call function 'JOB_CLOSE'
    exporting
*   AT_OPMODE                         = ' '
*   AT_OPMODE_PERIODIC                = ' '
*   CALENDAR_ID                       = ' '
*   EVENT_ID                          = ' '
*   EVENT_PARAM                       = ' '
*   EVENT_PERIODIC                    = ' '
      jobcount                          = p_jobcount
      jobname                           = p_jobname
*   LASTSTRTDT                        = NO_DATE
*   LASTSTRTTM                        = NO_TIME
*   PRDDAYS                           = 0
*   PRDHOURS                          = 0
*   PRDMINS                           = 0
*   PRDMONTHS                         = 0
*   PRDWEEKS                          = 0
*   PREDJOB_CHECKSTAT                 = ' '
*   PRED_JOBCOUNT                     = ' '
*   PRED_JOBNAME                      = ' '
*   SDLSTRTDT                         = NO_DATE
*   SDLSTRTTM                         = NO_TIME
*   STARTDATE_RESTRICTION             = BTC_PROCESS_ALWAYS
     strtimmed                         = 'X'  "IMMEDIATE
*   TARGETSYSTEM                      = ' '
*   START_ON_WORKDAY_NOT_BEFORE       = SY-DATUM
*   START_ON_WORKDAY_NR               = 0
*   WORKDAY_COUNT_DIRECTION           = 0
*   RECIPIENT_OBJ                     =
*   TARGETSERVER                      = ' '
*   DONT_RELEASE                      = ' '
* IMPORTING
*   JOB_WAS_RELEASED                  =
   exceptions
     cant_start_immediate              = 1
     invalid_startdate                 = 2
     jobname_missing                   = 3
     job_close_failed                  = 4
     job_nosteps                       = 5
     job_notex                         = 6
     lock_failed                       = 7
     others                            = 8.

  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                    " CALL_JOB_CLOSE
