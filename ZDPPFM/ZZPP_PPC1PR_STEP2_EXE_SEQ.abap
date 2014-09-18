FUNCTION ZZPP_PPC1PR_STEP2_EXE_SEQ.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(IF_DATE_RANGE) TYPE  PPCPR_POSTDATE_RANGE
*"     REFERENCE(IF_USER_RANGE) TYPE  PPCPR_USERNAME_RANGE OPTIONAL
*"     VALUE(IF_LIMIT) TYPE  PPC_COUNT DEFAULT 500
*"     VALUE(IF_PROTOCOL_SHOW) TYPE  PPC_PROTO DEFAULT CHARX
*"     VALUE(IF_COMPLMODE) TYPE  AS4FLAG DEFAULT 'D'
*"     REFERENCE(GROUP_NAME) TYPE  RZLLI_APCL
*"     VALUE(I_FLAG)
*"  EXPORTING
*"     VALUE(EF_LOGNUMBER) TYPE  BALOGNR
*"  EXCEPTIONS
*"      PROTOCOL_ERROR
*"      ENQUEUE_ERROR
*"      NOTHING_SELECTED
*"----------------------------------------------------------------------
* this function is copied from ppcpr_step2_exe and modified to use
* parallel process for each package in same ppc order
*
  field-symbols:
    <fs_step2_agg>    type ppcpr_type_step2_agg.

  data:
*   internal tables for the main data
    lt_ppc_step2_agg  type ppcpr_type_tab_step2_agg,
    lt_failed_ccoll   type ppcpr_type_tab_step2_agg.
  data:
    lf_log_handle  type balloghndl,
    lt_log_handle  type bal_t_logh,
    ls_statistics  type ppcpr_type_statist,
    ls_msg         type ppcpr_type_msg,
    lf_sysubrc     type sysubrc,
    lf_cc_count    type i,
    lf_nummat_ok   type i,
    lf_nummat_fail type i,
    lf_numcc_ok    type i,
    lf_numcc_fail  type i,
    lf_msgtype     type c,
    lf_dummy       type c,
    lf_start_time  type t,
    lf_end_time    type t.

  data:
**   work areas for main data
    wa_ppc_step2 like ppc_step2,
*    ls_ppc_step2_agg  type ppcpr_type_step2_agg,
**   internal tables for the main data
****    lt_ppc_step2_agg  type ppcpr_type_tab_step2_agg,
****    lt_failed_ccoll   type ppcpr_type_tab_step2_agg,
*    lt_ppc_temp       type ppcpr_type_tab_step2_agg,
    ls_ppcagg         type ppcpr_type_step2_agg,
    lt_msg_log        type zspp_log_msg_T
*    w_count           type i.
.
  types: begin of s_ppc_del,
           jobname(04)  type n.
           include type ppcpr_type_step2_agg.
  types: end of s_ppc_del.
  data: lt_ppc_del type table of s_ppc_del.
  data: ls_ppc_del type s_ppc_del.
  data:
*    lf_sysubrc     type sysubrc,
    w_count        type i,
    lf_tabindex    type i,
    w_excep_flag   type c,
    w_package(03)  type n,  "package number for each ppc order
    l_sndjob(03)   type n,
    l_tabix        like sy-tabix.
  data:
    lf_rt1 type i,
    lf_rt2 type i.
*  DATA: LT_INDEXLIST TYPE ZPPCPR_MATPOSID.
  data: ls_step2_log like ztpp_bfstep2_log.

*----------------------
* START OR PROCESSING
*----------------------


* --> initialisation, log entries
  clear ws_statistics.
  ws_statistics-stdat = sy-datlo.
  ws_statistics-sttim = sy-timlo.
  ws_statistics-uname = sy-uname.
  refresh it_rollback_job.
  refresh lt_ppc_del.
*  set run time clock resolution low. " <- DELETE  RICSI

  perform protocol_start_step2
                using lf_log_handle.
  perform protocol_top_line_step2
                using lf_log_handle
                      ls_statistics.
  perform progress_status
                using cf_statusone lf_rt1 lf_rt1.

* --> set the appropiate locks for the transaction
  perform lock_ppc_go2
                using lf_log_handle.


* if rollback happened and some components remain in
* PPC_STEP2 try two more times



  DO 3 times.
    w_count = w_count + 1.
* --> DATA SELECTION, PACKAGE CREATION
  perform ppc_step2_enqueue
                using lf_log_handle.
  perform read_step2_table
        using  lf_log_handle
               if_limit
               if_date_range
               if_user_range
      changing lt_ppc_step2_agg
               lt_failed_ccoll.
*                using lt_ppc_step2_agg
*                      lt_failed_ccoll
*                      if_date_range
*                      if_user_range
*                      wf_log_handle.

*  perform split_if_needed               " <-DEL RICSI
*                using lt_ppc_step2_agg  " <-DEL RICSI
*                      if_limit.         " <-DEL RICSI
*  perform fill_matpostids               " <-DEL RICSI
*                using lt_ppc_step2_agg. " <-DEL RICSI

  sort LT_PPC_STEP2_AGG by aufnr. " IS THIS BECAUSE OF THE PARALLEL PROC

* FOR TEST REASON****************
*  DATA: L_ACCASSOBJ LIKE LS_ppc_step2_agg-accassobj.
**  DATA:  L_FLAG TYPE C.
*  IF I_FLAG = 'X'.
*  LOOP AT LT_PPC_STEP2_AGG INTO LS_PPC_STEP2_AGG.
*    IF SY-TABIX NE 1.
*      IF L_ACCASSOBJ eq LS_PPC_STEP2_AGG-ACCASSOBJ.
*        append ls_ppc_step2_agg to lt_ppc_temp.
*      else.
*        exit.
*      ENDIF.
*      CONTINUE.
*    ENDIF.
*    L_ACCASSOBJ = LS_PPC_STEP2_AGG-ACCASSOBJ.
*    append ls_ppc_step2_agg to lt_ppc_temp.
*  ENDLOOP.
*   refresh lt_ppc_step2_agg.
*   move lt_ppc_temp to lt_ppc_step2_agg.
*   refresh lt_ppc_temp.
*  ENDIF.

* END--FOR TEST *************************

*  perform ppc_step2_dequeue. " WHY DID WE DEQUEUE THIS??? IS IT A MODIF

* --> START OF PROTOCOL FOR THE LUW's
  if not lt_failed_ccoll is initial.
    perform protocol_costcoll_read_error
                using lt_failed_ccoll
                      lf_log_handle.
  endif.
  describe table lt_ppc_step2_agg lines lf_cc_count.
  perform protocol_total_ccoll
                using lf_log_handle
                      lf_cc_count.

  perform progress_status
                using cf_statustwo wf_cc_count lf_rt1.


* --> DATA DISPATCHING
  loop at lt_ppc_step2_agg assigning <fs_step2_agg>.
*                          into ls_ppc_step2_agg.
    l_tabix   = sy-tabix + 1.
    w_package  = w_package + 1.  " for test
*   prepare...
    get time field lf_start_time.
    get run time field lf_rt1.
    lf_tabindex = sy-tabix.
    perform progress_status
                using cf_statusthree lf_tabindex wf_cc_count.

*    MOVE LS_PPC_STEP2_AGG-indexlist TO LT_INDEXLIST.

*   process data...
*   submit parallel processes
    data: " lf_nummat_ok   type INT4,
          " lf_nummat_fail type INT4,
          lf_return      type CHAR_LG_01.

    DO.
    call function 'ZZFPP_PPC1PR_STEP2_SINGLE_EXE'
*      starting new task taskname
*      destination in group group_name
*      PERFORMING task_receive ON END OF TASK
      exporting
        if_log_handle   = lf_log_handle
        if_post_date    = <fs_step2_agg>-post_date
        if_accassobj    = <fs_step2_agg>-accassobj
        if_plant        = <fs_step2_agg>-plant
        if_version      = <fs_step2_agg>-version
        if_baugr        = <fs_step2_agg>-baugr
        if_rmprofile    = <fs_step2_agg>-rmprofile
        if_aufnr        = <fs_step2_agg>-aufnr
        if_flginfodest  = <fs_step2_agg>-flg_dest
        if_conflogsys   = <fs_step2_agg>-confsys
        if_compcount    = <fs_step2_agg>-compcount
        if_complmode    = if_complmode
*      importing
*        ef_nummat_ok    = lf_nummat_ok
*        ef_nummat_fail  = lf_nummat_fail
*        return          = lf_return
      tables
        it_complist     = <fs_step2_agg>-complist
        it_indexlist    = <fs_step2_agg>-indexlist
        if_log_msg      = lt_msg_log
      exceptions
        SYSTEM_FAILURE        = 1
        COMMUNICATION_FAILURE = 2
        RESOURCE_FAILURE      = 3.

    case sy-subrc.
      when 0.
        taskname = taskname + 1.
        snd_jobs = snd_jobs + 1.
        clear: w_excep_flag.
*       SAVE THE RECORDS FOR DELETION
        move-corresponding <fs_step2_agg> to ls_ppc_del.
        ls_ppc_del-jobname = taskname - 1.
        APPEND ls_ppc_del TO lt_ppc_del.
        exit.
      when 1 or 2.
        w_excep_flag = 'X'.
      when 3.
        if w_excep_flag = space.
          w_excep_flag = 'X'.
          wait until rcv_jobs >= snd_jobs up to '0.1' seconds.
        else.
          wait until rcv_jobs >= snd_jobs up to '1.0' seconds.
        endif.
        if sy-subrc eq 0.
          clear w_excep_flag.
        endif.
     endcase.
    ENDDO.

*   check if this is the last package of current PCC
    clear: ls_ppcagg.
    READ TABLE lt_ppc_step2_agg into ls_ppcagg index l_tabix.
    if ls_ppcagg-accassobj NE <fs_step2_agg>-accassobj.

      wait until rcv_jobs >= snd_jobs .

                " for test

      IF sy-subrc = 0  AND  w_excep_flag = space
         and job_error ne 'X'.

*for test
         if snd_jobs ne w_package .
        " some job submi fail, record ppc order
           ls_step2_log-aufnr  = <fs_step2_agg>-aufnr.
           ls_step2_log-ACCOBJ = <fs_step2_agg>-accassobj.
           l_sndjob = snd_jobs.
           concatenate 'Should submit and submited:' w_package l_sndjob
                into ls_step2_log-message separated by space.
           insert into ztpp_bfstep2_log values ls_step2_log.
         endif.
         clear: w_package.
*end of for test

        CLEAR: rcv_jobs, snd_jobs, job_error.
        taskname  = '5001'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
              wait = 'X'.


        "message

      else.

        taskname  = '5001'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .

**for test
*        if snd_jobs ne w_package .
*        " some job submi fail, record ppc order
*           ls_step2_log-aufnr  = ls_ppc_step2_agg-aufnr.
*           ls_step2_log-ACCOBJ = ls_ppc_step2_agg-accassobj.
*           l_sndjob = snd_jobs.
*           concatenate 'Should submit and submited:' w_package l_sndjob
*                into ls_step2_log-message separated by space.
*           insert into ztpp_bfstep2_log values ls_step2_log.
*         endif.
*         commit work.
*         clear: w_package.
** end of for test

       CLEAR: rcv_jobs, snd_jobs, job_error.


      endif.

*   delete the backflushed records in table ppc_step2 for this order.
     loop at lt_ppc_del into ls_ppc_del.
*      check if this one has been rollbacked.
       read table it_rollback_job with key jobname = ls_ppc_del-jobname.
       if sy-subrc ne 0.

         PERFORM delete_processed_items_s
                     USING LS_PPC_del-indexlist.
       endif.
     endloop.
*
     commit work and wait.
     refresh it_rollback_job.
     refresh lt_ppc_del.

*    write the message for this ppc order

      perform add_message_ppc using lf_log_handle.


*   update statistics and performance analysis...
    add wf_nummat_ok   to ws_statistics-nummat_ok.
    add wf_nummat_fail to ws_statistics-nummat_fail.
    get run time field lf_rt2.
    <fs_step2_agg>-packagenr = lf_rt2 - lf_rt1.
    modify lt_ppc_step2_agg from <fs_step2_agg>
        transporting packagenr.

    endif.




  endloop.                " end of big loop over packages


* --> FINAL PROTOCOL TASKS
  perform progress_status
            using  cf_statussix wf_cc_count wf_cc_count.


  perform protocol_statistics_step2
            using  ws_statistics
                   lf_log_handle
                   ef_lognumber
                   wf_numcc_ok
                   wf_numcc_fail
                   lt_ppc_step2_agg.

* after the previous time running, clear varibles
  clear ws_statistics.
  ws_statistics-stdat = sy-datlo.
  ws_statistics-sttim = sy-timlo.
  ws_statistics-uname = sy-uname.
  CLEAR: wf_numcc_ok, wf_numcc_fail.



* check if still has component remaining
  select single * into wa_ppc_step2 from ppc_step2
     where MATPOSTID ne space.
  if sy-subrc ne 0.
    exit.
  endif.
* do the next time run because rollback happened and try again
* save the second time start message
  perform protocol_start_next_try using lf_log_handle
                                        w_count.



  ENDDO.         " 3 TIMES



  if if_protocol_show = charx.
    perform protocol_show using wf_log_handle.
  endif.


* unlocks the ppc_step2 process
  perform unlock_ppc_go2.

* END





ENDFUNCTION.
