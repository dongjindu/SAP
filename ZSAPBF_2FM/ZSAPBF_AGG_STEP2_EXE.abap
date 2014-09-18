FUNCTION zsapbf_agg_step2_exe.                              "#EC ENHOK
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LOG_HANDLE) TYPE  BALLOGHNDL
*"     VALUE(IV_COMPLMODE) TYPE  CHAR1
*"     VALUE(IS_PARALLEL) TYPE  ZSAPBF_PPC_PARALLEL
*"     VALUE(IT_PPC2_AGG) TYPE  ZSAPBF_TT_PPCPR_TYPE_STEP2_AGG
*"     VALUE(IT_UI_OPTIONS) TYPE  ZSAPBF_TT_UI_OPTION
*"     VALUE(IV_TASKNAME) TYPE  ZSAPBF_TASKNAME
*"     VALUE(IT_PLANT_SETTING) TYPE  ZSAPBF_TT_PLANT_PARA_SETTING
*"       OPTIONAL
*"     VALUE(IT_COMP_POSTID) TYPE  ZSAPBF_TT_COMP_POSTID OPTIONAL
*"     VALUE(IV_SHOW_LOG_ALL) TYPE  AS4FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(EV_TASKNAME) TYPE  PS4S_MASS_TASK_ID
*"     VALUE(EV_STATUS) TYPE  CHAR1
*"     VALUE(ET_PPC2_AGG) TYPE  ZSAPBF_TT_PPCPR_TYPE_STEP2_AGG
*"     VALUE(ES_STATISTICS) TYPE  ZSAPBF_PPCPR_TYPE_STATIST
*"     VALUE(EV_NUMCC_OK) TYPE  INT4
*"     VALUE(EV_NUMCC_FAIL) TYPE  INT4
*"     VALUE(ET_MESSAGE) TYPE  ZSAPBF_TT_BALMIP
*"     VALUE(ET_MESSAGE2) TYPE  ZSAPBF_TT_MESSAGE
*"  EXCEPTIONS
*"      INIT_ERROR
*"----------------------------------------------------------------------


*  DATA: BEGIN OF ls_plant_control,
*          counter TYPE i,
*          index   TYPE i,
*        END OF ls_plant_control.
*  DATA: ls_plant_control2 LIKE ls_plant_control.

*  DATA: lt_plant_control LIKE SORTED TABLE OF ls_plant_control
*          WITH UNIQUE KEY counter.
*  DATA:
*    ls_running_tasks TYPE ty_running_tasks,
*    lv_check_ok      TYPE xfeld,
*    lv_control_idx   TYPE i,
*    lv_plant_lines   TYPE i,
*    lt_msg           TYPE bapiret2_t,
*    lv_start_time    TYPE t,
*    ls_ppc2_agg      TYPE ppcpr_type_step2_agg,
*    lt_complist_available TYPE ppc_t_apocomplist_ext,
*    lt_complist_conflict  TYPE ppc_t_apocomplist_ext.

*  DATA: lv_wait TYPE p LENGTH 4 DECIMALS 1 VALUE '0.2'.
*
*  DATA: lv_available  TYPE i,
*        lv_conflict   TYPE i,
*        lv_last_index TYPE i.
*
*  DATA: lv_total_succ_comp TYPE i,
*        lv_total_fail_comp TYPE i.

**HMMA - NEED FURTHER CUSTOMIZATION!!! - ANDY
*/Comment START
*
**CDP2 Start
*  DATA: lo_badi_tss2_para_method TYPE REF TO zsapbf_badi_tss2_para_method.
**CDP2 End
*
*  gt_comp_postid[] = it_comp_postid[].
*
**  FIELD-SYMBOLS:
**    <ls_ppc2_agg>   TYPE ppcpr_type_step2_agg. "zsapbf_s_step2_agg.
*
** Parallel Processing
*  CHECK is_parallel-para_flag = charx.
*
**CDP2 Start
*  GET BADI lo_badi_tss2_para_method
*    FILTERS
*      para_method = is_parallel-zz_cust_par_s.
*
*  CALL BADI lo_badi_tss2_para_method->para_method_old
*    EXPORTING
*      iv_log_handle    = iv_log_handle
*      iv_complmode     = iv_complmode
*      it_ppc2_agg      = it_ppc2_agg
*      it_ui_options    = it_ui_options
*      iv_taskname      = iv_taskname
*      it_plant_setting = it_plant_setting
*      iv_show_log_all  = iv_show_log_all
*    CHANGING
*      cs_parallel      = is_parallel
*      ev_taskname      = ev_taskname
*      ev_status        = ev_status
*      et_ppc2_agg      = et_ppc2_agg
*      es_statistics    = es_statistics
*      ev_numcc_ok      = ev_numcc_ok
*      ev_numcc_fail    = ev_numcc_fail
*      et_message       = et_message
*      et_message2      = et_message2
*    EXCEPTIONS
*      init_error       = 1
*      OTHERS           = 2.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*    RAISING init_error.
*  ENDIF.
*  EXIT.
*/Comment END

*  CALL FUNCTION 'ZSAPBF_AGG_STEP2_PROCESS'
*    EXPORTING
*      iv_log_handle = iv_log_handle
*      iv_complmode  = iv_complmode
*      is_parallel   = is_parallel
*      it_ppc2_agg   = it_ppc2_agg
*      it_ui_options = it_ui_options
*      iv_taskname   = iv_taskname
*    IMPORTING
*      ev_taskname   = ev_taskname
*      ev_status     = ev_status
*      et_ppc2_agg   = et_ppc2_agg
*      es_statistics = es_statistics
*      ev_numcc_ok   = ev_numcc_ok
*      ev_numcc_fail = ev_numcc_fail
*      et_message    = et_message
*      et_message2   = et_message2.

** Get total count of data
**  DESCRIBE TABLE et_ppc2_agg LINES lv_plant_lines.
*  DESCRIBE TABLE it_ppc2_agg LINES lv_plant_lines.
**CDP2 End
*  DO lv_plant_lines TIMES.
*    ls_plant_control-counter = sy-index.
*    ls_plant_control-index   = sy-index.
*    INSERT ls_plant_control INTO TABLE lt_plant_control.
*  ENDDO.
*  lv_last_index = ls_plant_control-index.
*  CLEAR ls_plant_control.
*
** now start the processing
*  WHILE NOT lt_plant_control[] IS INITIAL.
*    LOOP AT lt_plant_control INTO ls_plant_control.
*      GET TIME FIELD lv_start_time.
*
*      lv_control_idx = sy-tabix.
**CDP2 Start
**      READ TABLE et_ppc2_agg ASSIGNING <ls_ppc2_agg>
**        INDEX ls_plant_control-index.
*      READ TABLE it_ppc2_agg ASSIGNING <ls_ppc2_agg>
*        INDEX ls_plant_control-index.
**CDP2 End
*
*      IF sy-subrc IS INITIAL.
*
** Wait to get the results
*        WAIT UP TO lv_wait SECONDS.
*
** Start from the begining again
*        IF gv_start_first = 'X'.
*          CLEAR gv_start_first.
*          EXIT.
*        ENDIF.
*
**PCC Check
*        READ TABLE gt_running_tasks TRANSPORTING NO FIELDS
*          WITH KEY accassobj = <ls_ppc2_agg>-accassobj
*                   status    = gc_running.
*
*        IF sy-subrc IS INITIAL.
*          CONTINUE.
*        ENDIF.
*
** CPRC Kind of Check
*        IF NOT <ls_ppc2_agg>-complist[] IS INITIAL.
*
*          CALL FUNCTION 'ZSAPBF_CPRC_DO_CHECK'
*            EXPORTING
*              it_comp                = <ls_ppc2_agg>-complist
*              iv_cprc_type           = gc_two_step
*            IMPORTING
*              et_msg                 = lt_msg
*              et_comp_tss2_available = lt_complist_available
*              et_comp_tss2_conflict  = lt_complist_conflict.
*        ENDIF.
*
*        IF lt_complist_available IS INITIAL. "no available
*          CONTINUE.
*        ELSEIF lt_complist_conflict IS INITIAL. "all can be processed
** This set of confirmation can be processed now.
*          DELETE lt_plant_control INDEX lv_control_idx.
*        ELSE. "partial can be processed
*          CLEAR <ls_ppc2_agg>-complist. FREE <ls_ppc2_agg>-complist.
*          <ls_ppc2_agg>-complist = lt_complist_available.
*          DELETE lt_plant_control INDEX lv_control_idx.
*
** Process conflict components
*          ls_ppc2_agg = <ls_ppc2_agg>.
*          CLEAR ls_ppc2_agg-complist. FREE ls_ppc2_agg-complist.
*          ls_ppc2_agg-complist = lt_complist_conflict.
*          APPEND ls_ppc2_agg TO et_ppc2_agg.
*
*          " Conflict components has to be once again processed.
*          lv_last_index = lv_last_index + 1. "no. of entries in ct_ppc2_agg
*          ls_plant_control2-counter = lv_last_index.
*          ls_plant_control2-index   = lv_last_index.
*          INSERT ls_plant_control2 INTO TABLE lt_plant_control.
*        ENDIF.
*
*        CLEAR: lt_complist_available, lt_complist_conflict.
*
** Store the details for the task
*        CLEAR ls_running_tasks.
*        ls_running_tasks-index     = ls_plant_control-index.
*        ls_running_tasks-plant     = <ls_ppc2_agg>-plant.
*        ls_running_tasks-zccar     = <ls_ppc2_agg>-zz_ccar.
*        ls_running_tasks-zcyear    = <ls_ppc2_agg>-zz_cyear.
*        ls_running_tasks-accassobj = <ls_ppc2_agg>-accassobj.
*        ls_running_tasks-status    = gc_running.
*        ls_running_tasks-start_time = lv_start_time.
*
*
**--------------------- Starting new task ---------------------
*        PERFORM start_parallelization
*              USING    ls_running_tasks
*                       <ls_ppc2_agg>
*                       is_parallel
*                       iv_complmode
*              CHANGING iv_taskname
*                       ev_status.
*        CASE ev_status.
**--> when everything is ok?
*          WHEN cf_al_ok.
**--> No Free Resources
*          WHEN cf_al_ress_error.
*            EXIT.
**--> max. processes reached and no process is coming back
*          WHEN cf_al_nort_error.
*            EXIT.
**Communication Error
*          WHEN cf_al_comm_error.
*            EXIT.
**System Error
*          WHEN cf_al_syst_error.
*            EXIT.
*        ENDCASE.
*
*        IF gf_appl_errors >= gc_max_appl_errors.
*          ev_status = cf_al_appl_error.
*          EXIT.
*        ENDIF.
*
*      ELSE.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*
*
*  ENDWHILE.
*
*  IF is_parallel-para_flag = charx
*    AND NOT et_ppc2_agg IS INITIAL.
*    "Wait up to the end of TSS2
*    WAIT UNTIL gf_received_jobs >= gf_started_jobs.         "448314
*    PERFORM tasklist_history_create.
*  ENDIF.
*  es_statistics = gs_statistics.
*  ev_numcc_ok   = gv_numcc_ok.
*  ev_numcc_fail = gv_numcc_fail.
*  et_message = gt_message.
*  et_message2 = gt_message2.
ENDFUNCTION.
