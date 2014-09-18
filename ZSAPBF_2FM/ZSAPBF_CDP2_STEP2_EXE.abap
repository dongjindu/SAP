FUNCTION zsapbf_cdp2_step2_exe.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_DATE_RANGE) TYPE  PPCPR_POSTDATE_RANGE
*"     REFERENCE(IT_USER_RANGE) TYPE  PPCPR_USERNAME_RANGE OPTIONAL
*"     REFERENCE(IT_PLANT_RANGE) TYPE  ZSAPBF_TT_WERKS_RANGE
*"     REFERENCE(IT_CONFTIME_RANGE) TYPE  ZSAPBF_TT_CONFTIME_RANGE
*"     VALUE(IV_LIMIT) TYPE  PPC_COUNT DEFAULT 500
*"     VALUE(IV_DELAY) TYPE  ZSAPBF_PPC_DELAY DEFAULT 600
*"     VALUE(IV_PROTOCOL_SHOW) TYPE  PPC_PROTO DEFAULT CHARX
*"     VALUE(IV_SHOW_LOG_ALL) TYPE  AS4FLAG OPTIONAL
*"     VALUE(IV_COMPLMODE) TYPE  CHAR1 DEFAULT CHARD
*"     VALUE(IS_PARALLEL) TYPE  ZSAPBF_PPC_PARALLEL OPTIONAL
*"     VALUE(IT_UI_OPTIONS) TYPE  ZSAPBF_TT_UI_OPTION OPTIONAL
*"     VALUE(IT_PLANT_SETTING) TYPE  ZSAPBF_TT_PLANT_PARA_SETTING
*"       OPTIONAL
*"  EXPORTING
*"     VALUE(EV_LOGNUMBER) TYPE  BALOGNR
*"     REFERENCE(ET_RETURN) TYPE  BAPIRET2_T
*"  EXCEPTIONS
*"      COMMUNICATION_ERROR
*"      SYSTEM_ERROR
*"      RESOURCES_ERROR
*"      MAX_PROCESSES_ERROR
*"      MAX_APPL_ERROR
*"      INIT_ERROR
*"      PROTOCOL_ERROR
*"----------------------------------------------------------------------
************************************************************************
**
**   Function Module: ZSAPBF_STEP2_EXE
**
**   Development request: DI Backflush
**
**   Short description: Function for Two Step Step 2
**
**   Created by: SAPCD07
**
**   Date: 25/08/2008
**
************************************************************************
** VERS DATE______ CHANGED BY___ SHORT DESCRIPTION____________________
** §001 25/08/2008 SAPCD07       Initial version
** §002 10/10/2008 SAPCD07       Display time delay in log
** §003 23/10/2008 SAPCD07       Msg#824440:Add detail log for Para mode
** §004 14/05/2010 SAPCD08       Copy to CDP2 version
************************************************************************

  DATA:
    lt_failed_ccoll  TYPE ppcpr_type_tab_step2_agg,

    lv_taskname      TYPE ps4s_mass_task_id,
    lv_status        TYPE c,
    lv_cc_count      TYPE i,
    lv_extnum        TYPE balnrext.

  DATA: lt_plant_model_lock  TYPE zsapbf_tt_plant_model.
  CONSTANTS: lc_ppc_typ_tss2 TYPE c VALUE '3'.  "Lock plant for Two Step TSS2

*§ Begin: CSS # 824440 2008
  DATA: lv_all TYPE zsapbf_show_log_all,
        lt_log_handle TYPE bal_t_logh.
  DATA: ls_message2 TYPE ty_message,
        lt_message  TYPE zsapbf_tt_balmip.
*§ End: CSS # 824440 2008

*----------------------
* START OR PROCESSING
*----------------------
* --> initialisation, log entries
  PERFORM initialization CHANGING is_parallel
                                  gs_plant_statistics
                                  lv_taskname.

  PERFORM protocol_start_step2 IN PROGRAM saplppc1pr
                USING gv_log_handle.

  PERFORM protocol_top_line_step2 "IN PROGRAM saplppc1pr
      USING gv_log_handle
            iv_limit
            iv_delay
            it_conftime_range
            it_plant_range
            it_user_range
            it_date_range
            gs_plant_statistics
            is_parallel
            it_ui_options
            it_plant_setting.

* --> DATA SELECTION, PACKAGE CREATION
  PERFORM lock_plant_by_process IN PROGRAM zsapbf_step2_go_tss2
                                USING it_plant_range
                                      lc_ppc_typ_tss2
                             CHANGING lt_plant_model_lock.

  PERFORM ppc_step2_enqueue_shared
                USING gv_log_handle.

* --> DATA DISPATCHING
  PERFORM cdp2_step2_agg_exe USING lt_plant_model_lock
                                   it_plant_range
                                   it_date_range
                                   it_user_range
                                   it_conftime_range
                                   iv_limit
                                   gv_log_handle
                                   iv_complmode
                                   is_parallel
                                   it_ui_options
                                   it_plant_setting
                                   iv_show_log_all
                          CHANGING lv_taskname
                                   lv_status
                                   gt_ppc2_agg
                                   gs_plant_statistics.

*§ Begin: CSS # 824440 2008
  APPEND gv_log_handle TO lt_log_handle.

  IF is_parallel-para_flag = charx.
    GET PARAMETER ID 'ZSAPBF_SHOW_LOG_ALL' FIELD lv_all.
* Commented by SAPCD08 on 2010.05.19
*    IF sy-subrc <> 0 OR lv_all = charx.
    IF ( sy-subrc <> 0 OR lv_all = charx ) AND
      NOT ( iv_show_log_all = charx ).
      lv_extnum = cf_al_extnumber_step2.
      PERFORM save_log_to_db USING gv_log_handle
                                   gt_message
                                   lv_extnum.

      IF NOT gt_message2 IS INITIAL.
* End of 1st log
        DATA: lv_lognumber TYPE balognr.

        LOOP AT gt_message2 INTO ls_message2.
          lt_message = ls_message2-data.
          PERFORM lognr_read_save USING    gv_log_handle
                                  CHANGING lv_lognumber.

* Start the 2nd log
          PERFORM protocol_start_step2 IN PROGRAM saplppc1pr
                        USING gv_log_handle.

          PERFORM protocol_top_line_step2 "IN PROGRAM saplppc1pr
              USING gv_log_handle
                    iv_limit
                    iv_delay
                    it_conftime_range
                    it_plant_range
                    it_user_range
                    it_date_range
                    gs_plant_statistics
                    is_parallel
                    it_ui_options
                    it_plant_setting.

          PERFORM save_log_to_db USING gv_log_handle
                                       lt_message
                                       lv_extnum.
          APPEND gv_log_handle TO lt_log_handle.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
*§ End: CSS # 824440 2008

* --> FINAL PROTOCOL TASKS
  PERFORM protocol_statistics_tss2
      USING    gv_log_handle
               gv_numcc_ok
               gv_numcc_fail
*               lv_enderror
               lv_lognumber " CSS # 824440 2008
               lv_status
               gt_ppc2_agg
      CHANGING ev_lognumber
               gs_plant_statistics.

* release the locks
* Message 861794/2008 + 889403/2008, DNE 20-Oct-2008 Begin
*  PERFORM ppc_step2_dequeue IN PROGRAM saplppc1pr.
  PERFORM ppc_step2_dequeue_shared.
* Message 861794/2008, DNE 20-Oct-2008 End

* DNE 27-Oct-2008 Begin
* Message 879617/ 2008: Move Locking to main function module
  PERFORM unlock_plant_by_process IN PROGRAM zsapbf_step2_go_tss2
                                  USING lc_ppc_typ_tss2
                                        lt_plant_model_lock.
* DNE 27-Oct-2008 End

  IF iv_protocol_show = charx.
    "log display for 999999 items
    PERFORM protocol_show USING lt_log_handle.
*    PERFORM protocol_show IN PROGRAM saplppc1pr
*                          USING gv_log_handle.
  ENDIF.


* error handling
  CASE lv_status.
    WHEN cf_al_ok.                     "all OK
      EXIT.
    WHEN cf_al_appl_error.             "application error
      MESSAGE e011
      RAISING max_appl_error.
    WHEN cf_al_syst_error.             "system error
      MESSAGE e012
      RAISING system_error.
    WHEN cf_al_comm_error.             "communication error
      MESSAGE e013
      RAISING communication_error.
    WHEN cf_al_nort_error.             "no process reversal
      MESSAGE e014
      RAISING max_processes_error.
    WHEN cf_al_ress_error.             "no resource
      MESSAGE e015
      RAISING resources_error.
  ENDCASE.

*  IF lv_enderror = charx.          "not all started processes reversed
*    MESSAGE e016
*    RAISING no_process_return_at_end.
*  ENDIF.


ENDFUNCTION.
