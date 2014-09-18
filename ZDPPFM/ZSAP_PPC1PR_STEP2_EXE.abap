function zsap_ppc1pr_step2_exe.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IF_DATE_RANGE) TYPE  PPCPR_POSTDATE_RANGE
*"     REFERENCE(IF_USER_RANGE) TYPE  PPCPR_USERNAME_RANGE OPTIONAL
*"     VALUE(IF_LIMIT) TYPE  PPC_COUNT DEFAULT 500
*"     VALUE(IF_PROTOCOL_SHOW) TYPE  PPC_PROTO DEFAULT CHARX
*"     VALUE(IF_COMPLMODE) TYPE  C DEFAULT CHARD
*"     VALUE(IS_PAR) TYPE  ZSAP_STEP2_PAR_SETTING OPTIONAL
*"  EXPORTING
*"     VALUE(EF_LOGNUMBER) TYPE  BALOGNR
*"  EXCEPTIONS
*"      PROTOCOL_ERROR
*"      ENQUEUE_ERROR
*"      NOTHING_SELECTED
*"----------------------------------------------------------------------


  field-symbols:
    <fs_step2_agg>    type ppcpr_type_step2_agg,
    <fs_step2_agg_2>    type ty_ppcpr_type_step2_agg.

  data:
*   internal tables for the main data
    lt_ppc_step2_agg  type ppcpr_type_tab_step2_agg,
    lt_failed_ccoll   type ppcpr_type_tab_step2_agg.

  data:
*    lf_log_handle  TYPE balloghndl,
*    lt_log_handle  TYPE bal_t_logh,
    ls_statistics  type ppcpr_type_statist,
    ls_msg         type ppcpr_type_msg,
    lf_sysubrc     type sysubrc,
    lf_cc_count    type i,
*    lf_nummat_ok   type i,
*    lf_nummat_fail type i,
*    lf_numcc_ok    TYPE i,
*    lf_numcc_fail  TYPE i,
    lf_msgtype     type c,
    lf_dummy       type c,
    lf_start_time  type t,
    lf_end_time    type t.
  data: lv_compcount type ppc_compnumber,
        lv_not_ok type xfeld.
  field-symbols <cmp> type ppc_apocomplist_ext.

*----------------------
* START OR PROCESSING
*----------------------

* --> initialisation, log entries
  clear ls_statistics.
  ls_statistics-stdat = sy-datlo.
  ls_statistics-sttim = sy-timlo.
  ls_statistics-uname = sy-uname.

  perform protocol_start_step2(saplppc1pr)
                using gf_log_handle.

  perform protocol_top_line_step2(saplppc1pr)
      using gf_log_handle
            ls_statistics.



* --> DATA SELECTION, PACKAGE CREATION
  perform ppc_step2_enqueue(saplppc1pr)
                using gf_log_handle.

  perform read_step2_table(saplppc1pr)
      using    gf_log_handle
               if_limit
               if_date_range
               if_user_range
      changing lt_ppc_step2_agg
               lt_failed_ccoll.


*  perform fill_bits changing lt_ppc_step2_agg.


* --> START OF PROTOCOL FOR THE LUW's
  if not lt_failed_ccoll is initial.
    perform protocol_costcoll_read_error(saplppc1pr)
                using lt_failed_ccoll
                      gf_log_handle.
    refresh lt_failed_ccoll.
  endif.

  describe table lt_ppc_step2_agg lines lf_cc_count.
  perform protocol_total_ccoll(saplppc1pr)
                using gf_log_handle
                      lf_cc_count.

* create taskname
  data: lv_num type num8,
        lv_wpnum type i,
        l_msg_txt(150) type c.
  data: lt_enq type table of seqg3,
        lv_common type i,
        lv_arg type eqegraarg.
  field-symbols:
   <fs_comp> type ppc_apocomplist_ext,
   <fs_enq>  type seqg3.
  loop at lt_ppc_step2_agg assigning <fs_step2_agg>.
    lv_num = lv_num + 1.
    append initial line to gt_ppc_step2_agg assigning <fs_step2_agg_2>.
    move-corresponding <fs_step2_agg> to <fs_step2_agg_2>.
    concatenate 'STEP2_' lv_num into <fs_step2_agg_2>-taskname.
    delete lt_ppc_step2_agg.
  endloop.
  free lt_ppc_step2_agg.
* start packages
  call function 'SPBT_INITIALIZE'
    exporting
      group_name                   = is_par-p_srvgrp
    importing
      max_pbt_wps                  = lv_wpnum
    exceptions
      invalid_group_name           = 1
      internal_error               = 2
      pbt_env_already_initialized  = 3
      currently_no_resources_avail = 4
      no_pbt_resources_found       = 5.
  if sy-subrc ne 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  elseif lv_wpnum > is_par-p_max_wp and is_par-p_max_wp ne 0.
    lv_wpnum = is_par-p_max_wp.
  elseif lv_wpnum > 1.
*    as per our experience there are not so many WP available
*    so let's decrease the number by one
    lv_wpnum = lv_wpnum - 1.
  endif.

*  gr_accassobj-option = 'EQ'.
*  gr_accassobj-sign = 'I'.
* start processing
  do.
    read table gt_ppc_step2_agg transporting no fields
                                with key status = space.
    if sy-subrc ne 0.
      read table gt_ppc_step2_agg transporting no fields
                            with key status = 'R'.
      if sy-subrc ne 0.
        read table gt_ppc_step2_agg transporting no fields
                      with key status = 'E'.
        if sy-subrc ne 0.
          wait until g_run = 0.
          exit.
        endif.
      endif.
    endif.
    if gt_ppc_step2_agg[] is initial.
      exit.
    endif.
* --> DATA DISPATCHING
    loop at gt_ppc_step2_agg assigning <fs_step2_agg_2>
                             where ( status eq space or status eq 'R' or status eq 'E' ).

      lv_compcount = <fs_step2_agg_2>-compcount.
      if g_run > 0.
*       wait in order to receive packages
        wait up to '0.1' seconds.
        wait until g_run < lv_wpnum.
      endif.
*      continue with this package only if it is not currently processed
*      check not <fs_step2_agg_2>-accassobj in gr_accassobj or gr_accassobj[] is initial.
*      gr_accassobj-low = <fs_step2_agg_2>-accassobj.
*      APPEND gr_accassobj. " do not use the PCC chefor the time being

*      BREAK-POINT.
      if is_par-p_cpr is not initial.   " check CPR by ENQUE_READ
*     check the existing locking entries
        call function 'ENQUE_READ'
          exporting
            gclient = sy-mandt
            gname   = 'MARC'
            guname  = sy-uname
          tables
            enq     = lt_enq.
        lv_arg(3) = sy-mandt.
*     check common parts
        clear lv_common.
        if lines( lt_enq ) <  lv_compcount.
          sort <fs_step2_agg_2>-complist by matnr.
          loop at lt_enq assigning <fs_enq>.
            read table <fs_step2_agg_2>-complist transporting no fields
                                                 with key matnr = <fs_enq>-garg+3(18)
                                                 binary search.
            if sy-subrc eq 0.
              lv_common = lv_common + 1.
            endif.
          endloop.
        else.
          sort lt_enq by garg.
          loop at <fs_step2_agg_2>-complist assigning <fs_comp>.
            lv_arg+3 = <fs_comp>-matnr.
            lv_arg+21 = <fs_comp>-werks.
            read table lt_enq assigning <fs_enq>
                              with key garg = lv_arg binary search.
            if sy-subrc eq 0.
              lv_common = lv_common + 1.
            endif.
          endloop.
        endif.
        if ( lv_compcount * is_par-p_cpr / 100 ) < lv_common.
*         search for a new package
          continue.
        endif.
      else.
* check component list:
        clear lv_not_ok.
        loop at <fs_step2_agg_2>-complist assigning <cmp>.
          read table gt_cmp_run transporting no fields
                                with key matnr = <cmp>-matnr binary search.
          if sy-subrc eq 0.
            lv_not_ok = 'X'.
            exit.
          endif.
        endloop.
        if lv_not_ok is not initial.
          continue.
        endif.
        perform add_cmp using    <fs_step2_agg_2>-complist
         changing gt_cmp_run.
      endif.

      call function 'PPC1PR_ASTAT'
        exporting
          if_action  = gc_astatopen
          if_openkey = gc_ppcpa_key5.

      get time field lf_start_time.



      do 2 times.
        g_run = g_run + 1.
        <fs_step2_agg_2>-status = 'B'.
*       process data...
        call function 'ZSAP_PPC1PR_STEP2_SINGLE_EXE'
          starting new task <fs_step2_agg_2>-taskname
          destination in group is_par-p_srvgrp
          performing step2_receive on end of task
          exporting
            if_log_handle         = gf_log_handle
            if_post_date          = <fs_step2_agg_2>-post_date
            if_accassobj          = <fs_step2_agg_2>-accassobj
            if_plant              = <fs_step2_agg_2>-plant
            if_version            = <fs_step2_agg_2>-version
            if_baugr              = <fs_step2_agg_2>-baugr
            if_rmprofile          = <fs_step2_agg_2>-rmprofile
            if_aufnr              = <fs_step2_agg_2>-aufnr
            if_flginfodest        = <fs_step2_agg_2>-flg_dest
            if_conflogsys         = <fs_step2_agg_2>-confsys
            if_compcount          = lv_compcount
            if_complmode          = if_complmode
          changing
            it_complist           = <fs_step2_agg_2>-complist
            it_indexlist          = <fs_step2_agg_2>-indexlist
          exceptions
            communication_failure = 1  message l_msg_txt
            system_failure        = 2  message l_msg_txt
            resource_failure      = 3.
        if sy-subrc ne 0.
          lv_not_ok = 'X'.
          g_run = g_run - 1.
          <fs_step2_agg_2>-status = 'R'.
          case sy-subrc.
            when 1.
              wait up to is_par-p_retryc seconds.
            when 2.
              wait up to is_par-p_retrys seconds.
            when 3.
              wait up to is_par-p_retryr seconds.
          endcase.
        else.
          clear lv_not_ok.
          if is_par-p_cpr is not initial.
            wait up to is_par-p_st_new seconds.
          endif.
          exit.
        endif.
      enddo.
      if lv_not_ok is not initial.
        perform remove_cmp using    <fs_step2_agg_2>-complist
                           changing gt_cmp_run.
      endif.

*   analyse the return code
      if sy-subrc ne 0.
*  This part has been moved into the receive_result
**     if errors, then rollback...
*        CLEAR: ls_msg. "lf_nummat_ok, lf_nummat_fail.
*        MOVE-CORRESPONDING syst TO ls_msg.
*        ADD 1 TO lf_numcc_fail.
*        ROLLBACK WORK.
*
**     ...and update the protocol accordingly
*        MESSAGE i203 INTO lf_dummy.
*
*        PERFORM protocol_msg_add(saplppc1pr) USING gf_log_handle
*                  cf_al_msgid lf_msgtype '203'
*                  space space space space
*                  cf_al_detlevel3.
*
*        PERFORM protocol_msg_add(saplppc1pr) USING gf_log_handle
*                ls_msg-msgid ls_msg-msgty ls_msg-msgno
*                ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3
*                ls_msg-msgv4 cf_al_detlevel4.

*        CALL FUNCTION 'BAL_DB_SAVE'
*          EXPORTING
*            i_in_update_task = space
*            i_t_log_handle   = lt_log_handle.

      else.

**     if ok, then commit work
*        ADD 1 TO lf_numcc_ok.
*        COMMIT WORK AND WAIT.

      endif.


**   update statistics and performance analysis...
*      ADD lf_nummat_ok   TO ls_statistics-nummat_ok.
*      ADD lf_nummat_fail TO ls_statistics-nummat_fail.

*      GET TIME FIELD lf_end_time.
*      SUBTRACT lf_start_time FROM lf_end_time.
*      <fs_step2_agg>-packagenr = lf_end_time MOD 86400.


    endloop.                " end of big loop over packages
    wait up to is_par-p_st_new seconds.
  enddo.

* end of parallel process

  call function 'PPC1PR_ASTAT'
    exporting
      if_action  = gc_astatclose
      if_openkey = gc_ppcpa_key5.

* release the locks
  perform ppc_step2_dequeue(saplppc1pr).

* --> FINAL PROTOCOL TASKS
  perform protocol_statistics_step2(saplppc1pr)
      using    gf_log_handle
               gf_numcc_ok
               gf_numcc_fail
               lt_ppc_step2_agg
      changing ef_lognumber
               ls_statistics.

  if if_protocol_show = charx.
    perform protocol_show(saplppc1pr) using gf_log_handle.
  endif.





* END
  free lt_ppc_step2_agg.



endfunction.

*&---------------------------------------------------------------------*
*&      Form  step2_receive
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TASK_NAME  text
*----------------------------------------------------------------------*
form step2_receive using task_name.
  data:     lf_nummat_ok   type ppc_compnumber,
            lf_nummat_fail type ppc_compnumber,
            l_msg_txt(150) type c,
            lf_dummy       type c,
            lf_msgtype     type c,
            ls_msg         type ppcpr_type_msg.
  field-symbols:
  <fs_step2_agg>    type ty_ppcpr_type_step2_agg.

  read table gt_ppc_step2_agg assigning <fs_step2_agg>
                              with key taskname = task_name binary search.
*  delete gr_accassobj where low = <fs_step2_agg>-accassobj.
*--> Receive Results
  receive results from function 'ZSAP_PPC1PR_STEP2_SINGLE_EXE'
      importing
        ef_nummat_ok    = lf_nummat_ok
        ef_nummat_fail  = lf_nummat_fail
    exceptions
        protocol_error  = 1
        profile_error   = 2
        matpos_overflow = 4
      communication_failure = 6 message l_msg_txt
      system_failure        = 7 message l_msg_txt
    others                  = 8.
  if sy-subrc eq 0.
    <fs_step2_agg>-status = 'F'.
    <fs_step2_agg>-nummat_ok = lf_nummat_ok.
    <fs_step2_agg>-nummat_fail = lf_nummat_fail.
* increase the global counter
    add 1 to gf_numcc_ok.
  else.
*    message id sy-msgid type 'A' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    <fs_step2_agg>-status = 'E'.
    <fs_step2_agg>-nummat_ok = lf_nummat_ok.
    <fs_step2_agg>-nummat_fail = lf_nummat_fail.
*  maintain protocol
    clear: ls_msg. "lf_nummat_ok, lf_nummat_fail.
    move-corresponding syst to ls_msg.
    add 1 to gf_numcc_fail.
*        ROLLBACK WORK.

*     ...and update the protocol accordingly
    message i203 into lf_dummy.

    perform protocol_msg_add(saplppc1pr) using gf_log_handle
              cf_al_msgid lf_msgtype '203'
              space space space space
              cf_al_detlevel3.

    perform protocol_msg_add(saplppc1pr) using gf_log_handle
            ls_msg-msgid ls_msg-msgty ls_msg-msgno
            ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3
            ls_msg-msgv4 cf_al_detlevel4.

    call function 'BAL_DB_SAVE'
      exporting
        i_in_update_task = space
        i_t_log_handle   = gt_log_handle.
  endif.

  perform remove_cmp using    <fs_step2_agg>-complist
                   changing gt_cmp_run.
  g_run = g_run - 1.
endform.                    "step2_receive
