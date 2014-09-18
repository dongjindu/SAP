*----------------------------------------------------------------------*
***INCLUDE ZSAPBF_CPZP_CORRECTION_MASSF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_PCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pcc USING iv_year TYPE gjahr
                   iv_month TYPE monat
             CHANGING
                   et_aufnr TYPE tt_aufnr.
  DATA: lt_aufnr TYPE tt_aufnr.
  DATA: ls_aufnr TYPE ts_aufnr.
  DATA lv_first_day TYPE sydatum.
  DATA lv_last_day TYPE sydatum.
  DATA lt_accassobj TYPE tt_accassobj.

  lv_first_day+0(4) = iv_year.
  lv_first_day+4(2) = iv_month.
  lv_first_day+6(2) = '01'.
  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_first_day
    IMPORTING
      last_day_of_month = lv_last_day
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Commented by SAPCD08 on 2010.07.08
*  SELECT aufnr FROM ckmlmv013
*               INTO TABLE lt_aufnr
*              WHERE aufnr IN so_aufnr
*                AND prwrk = p_werks
*                AND pmatn IN so_pmatn
*                AND verid IN so_verid.

* Start : Added by SAPCD08 on 2010.07.08, replacing the block commentated above.
  SELECT b~aufnr b~cc_guid FROM ckmlmv013 AS a
              INNER JOIN qrp002 AS b
                 ON a~mandt = b~mandt
                AND a~aufnr = b~aufnr
               INTO TABLE lt_aufnr
              WHERE a~aufnr IN so_aufnr
                AND a~prwrk = p_werks
                AND a~pmatn IN so_pmatn
                AND a~verid IN so_verid.
* End : Added by SAPCD08 on 2010.07.08, replacing the block commentated above.

  IF lt_aufnr IS INITIAL.
    MESSAGE i408.
    STOP.
  ENDIF.

*Start: Commented by SAPCD08 on 2010.07.08 again, due to so bad performance
**{ Changed by SAPCDP08, Useless code cleanning
**  SELECT aufnr FROM qrp002
**               INTO TABLE et_aufnr
**               FOR ALL ENTRIES IN lt_aufnr
**              WHERE aufnr = lt_aufnr-aufnr.
**
** CLEAR et_aufnr.
*
*  SELECT aufnr FROM qrp002
*               JOIN ppc_ord_inf
*               ON qrp002~cc_guid = ppc_ord_inf~accassobj
*               JOIN ppc_head
*               ON ppc_ord_inf~orderid = ppc_head~orderid
*               INTO TABLE et_aufnr
*               FOR ALL ENTRIES IN lt_aufnr
*              WHERE qrp002~aufnr = lt_aufnr-aufnr
**                AND ppc_ord_inf~dummy_order = space
*                AND ppc_head~flg_del EQ space
*                AND ppc_head~flg_synch = 'X'
*                AND ppc_head~postdate GE lv_first_day
*                AND ppc_head~postdate LE lv_last_day
*    %_HINTS ORACLE '&max_blocking_factor  50& &max_in_blocking_factor  50&'.
**} End of change

* Changed by SAPCD08 on 2010.07.08
*End: Commented by SAPCD08 on 2010.07.08 again, due to so bad performance

****** Start : Added by SAPCD08 on 2010.07.08**, Commented again due to so bad performance 2010.07.19
*  SELECT a~aufnr FROM qrp002 AS a
**               JOIN ppc_ord_inf AS b
**               ON a~cc_guid = b~accassobj
**               JOIN ppc_head
**               ON ppc_ord_inf~orderid = ppc_head~orderid
*               INTO TABLE et_aufnr   "lt_aufnr
*               FOR ALL ENTRIES IN lt_aufnr
*              WHERE a~cc_guid = lt_aufnr-cc_guid
***               AND ppc_ord_inf~dummy_order = space
**                AND ppc_head~flg_del EQ space
**                AND ppc_head~flg_synch = 'X'
**                AND ppc_head~postdate GE lv_first_day
**                AND ppc_head~postdate LE lv_last_day
*                AND EXISTS ( SELECT c~accassobj FROM ppc_head AS c
*                              WHERE c~accassobj   = a~cc_guid
**                                AND c~flg_del     = space
*                                AND c~flg_asynch  = 'X' "flg_asynch : no problem!!
*                                AND c~postdate GE lv_first_day
*                                AND c~postdate LE lv_last_day )
*    %_HINTS ORACLE '&max_blocking_factor  10& &max_in_blocking_factor  10&'.
****** End : Added by SAPCD08 on 2010.07.08**, Commented again due to so bad performance 2010.07.19

  DATA lt_aufnr_temp LIKE qrp002 OCCURS 0 WITH HEADER LINE.

***** Start : Added by SAPCD08 on 2010.07.19***, Better performance have been approved ****************
  SORT  lt_aufnr BY cc_guid.

  SELECT DISTINCT a~accassobj AS cc_guid
    FROM ppc_head AS a
    INTO CORRESPONDING FIELDS OF TABLE lt_aufnr_temp
     FOR ALL ENTRIES IN lt_aufnr
   WHERE a~accassobj = lt_aufnr-cc_guid
*     AND a~flg_asynch = 'X' "Commentated by Sung-Kon James Kim 2011/01/26
     AND a~postdate BETWEEN lv_first_day AND lv_last_day
    %_HINTS ORACLE '&max_blocking_factor  20& &max_in_blocking_factor  20&'
            ORACLE 'INDEX_FFS ("PPC_HEAD", "PPC_HEAD~BUD")'.

  LOOP AT lt_aufnr_temp.
    READ TABLE lt_aufnr WITH KEY cc_guid = lt_aufnr_temp-cc_guid INTO ls_aufnr.

    IF sy-subrc = 0.
      APPEND ls_aufnr TO et_aufnr.
      CLEAR ls_aufnr.
    ENDIF.

  ENDLOOP.
***** End : Added by SAPCD08 on 2010.07.19****, Better performance have been approved ****************


  IF et_aufnr IS INITIAL.
    MESSAGE i408.
    STOP.
  ENDIF.

ENDFORM.                    " GET_PCC
**&---------------------------------------------------------------------*
**&      Form  PARALLELIZATION
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_LT_AUFNR  text
**----------------------------------------------------------------------*
FORM parallelization USING is_parallel     TYPE ppc_parallel
*                           it_aufnr TYPE tt_aufnr
*                  CHANGING ct_aufnr_task TYPE tt_aufnr_task
                           .
  DATA: ls_aufnr TYPE ts_aufnr,
        lv_status TYPE c,
        lv_taskname TYPE tv_taskname.
  DATA: ls_aufnr_task TYPE ts_aufnr_task.
*  DATA:
*    lv_control_idx TYPE i.

  DATA: lt_aufnr TYPE tt_aufnr.
  DATA: lv_pcc_exist TYPE flag.
*  lt_aufnr = it_aufnr.

* Add by SAPCD10 on 2010.07.09
***** Start : Parallel Process Initialization *****
  DATA: l_free_wps    TYPE i.
  DATA: l_par_process TYPE int4 . "Added by James Sung-Kon Kim 2011.03.02

  PERFORM check_parallel_servergroup CHANGING l_free_wps.

  l_par_process = p_wps * p_wpssub . "Added by James Sung-Kon Kim 2011.03.02

*  IF l_free_wps <  p_wps. "Disabled by James Sung-Kon Kim 2011.03.02
  IF l_free_wps <  l_par_process. "Added by James Sung-Kon Kim 2011.03.02

    MESSAGE s011 WITH l_free_wps l_par_process DISPLAY LIKE 'E'.
*    EXIT. "Disabled by James Sung-Kon Kim 2011.03.02
    STOP. "Added by James Sung-Kon Kim 2011.03.02

  ENDIF.
***** End : Parallel Process Initialization   *****

** now start processing
*  WHILE NOT lt_aufnr[] IS INITIAL.

*---- Start; Added 2010.07.08 to get better performance

  PERFORM get_pcc_new CHANGING lt_aufnr.

  PERFORM check_ppc_parallel USING lt_aufnr
                                   p_wps. "Important, Be carefull, Still use p_wps by James 2011.03.02

  CLEAR lt_aufnr.
  IF gt_aufnr IS NOT INITIAL.
    LOOP AT gt_aufnr INTO st_aufnr.
      MOVE st_aufnr TO ls_aufnr.
      APPEND ls_aufnr TO lt_aufnr.
      CLEAR ls_aufnr.
    ENDLOOP.
  ELSE.
    MESSAGE i408.
    STOP.
  ENDIF.

*---- End; Added 2010.07.08 to get better performance

  LOOP AT lt_aufnr INTO ls_aufnr.
*      lv_control_idx = sy-tabix.
*      DELETE lt_aufnr INDEX lv_control_idx.

* Get task information for each parallelization task
    IF lv_taskname < gc_max_taskname.
      lv_taskname = lv_taskname + 1.
    ELSE.
      lv_taskname = 1.
    ENDIF.
    ls_aufnr_task-taskname = lv_taskname.
    ls_aufnr_task-aufnr = ls_aufnr-aufnr.
*      APPEND ls_aufnr_task TO gt_aufnr_task.

* Start new task
    PERFORM start_new_task USING ls_aufnr
                                 is_parallel
                                 p_wpssub     "Added by James Sung-Kon Kim 2011.03.02
                        CHANGING ls_aufnr_task
                                 lv_status
                                 lv_taskname.
*
    CASE lv_status.
***************************
**--> No Free Resources
***************************
      WHEN cf_al_ress_error.
        CONTINUE."EXIT.
*
***************************************************
**--> max. processess, and no process is coming back
***************************************************
      WHEN cf_al_nort_error.
        CONTINUE."EXIT.
*
************************
** Communcations Error
************************
      WHEN cf_al_comm_error.
        CONTINUE."EXIT.
*
****************
** System Error
****************
      WHEN cf_al_syst_error.
        CONTINUE."EXIT.

****************
** Application Error
****************
      WHEN cf_al_appl_error.
        CONTINUE."EXIT.

****************
** No Error
****************
      WHEN cf_al_ok.

    ENDCASE.
*
    IF gf_appl_errors >= gc_max_appl_errors.
      lv_status = cf_al_appl_error.
      CONTINUE."EXIT.
    ENDIF.

  ENDLOOP.

*  ENDWHILE.
*
**--> Get the result for all the tasks
*  " Waiting for the already started tasks

* Commentated by SAPCD10 on 2010.07.08 ; 1 line bellow
*  WAIT UNTIL gf_received_jobs >= gf_started_jobs.

* Start : Changed by SAPCD10 on 2010.07.08
  DO.
    IF gf_received_jobs >= gf_started_jobs.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.
* End : Changed by SAPCD10 on 2010.07.08


  IF p_test = 'X'.
    CALL FUNCTION 'ZSAPBF_CPZP_CORRECT_DISPLAY'
      EXPORTING
        iv_test    = 'X'
        iv_error   = p_error
        iv_num_pcc = p_numpcc
      TABLES
        it_cpzp    = gt_cpzp
        it_cpzp_bk = gt_cpzp_bk.
  ENDIF.

ENDFORM.                    " PARALLELIZATION

**&---------------------------------------------------------------------*
**&      Form  PARALLELIZATION_TEST_RUN
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_LT_AUFNR  text
**----------------------------------------------------------------------*
FORM parallelization_test_run USING it_aufnr TYPE tt_aufnr  "#EC CALLED
                                    is_parallel     TYPE ppc_parallel
*                           CHANGING ct_aufnr_task TYPE tt_aufnr_task
                           .
  DATA: ls_aufnr TYPE ts_aufnr,
        lv_status TYPE c,
        lv_taskname TYPE tv_taskname.
  DATA: ls_aufnr_task TYPE ts_aufnr_task.
  DATA:
    lv_control_idx TYPE i.

  DATA: lt_aufnr TYPE tt_aufnr.
  lt_aufnr = it_aufnr.

** now start processing
  WHILE NOT lt_aufnr[] IS INITIAL.

    LOOP AT lt_aufnr INTO ls_aufnr.
      lv_control_idx = sy-tabix.
      DELETE lt_aufnr INDEX lv_control_idx.

* Get task information for each parallelization task
      IF lv_taskname < gc_max_taskname.
        lv_taskname = lv_taskname + 1.
      ELSE.
        lv_taskname = 1.
      ENDIF.
      ls_aufnr_task-taskname = lv_taskname.
      ls_aufnr_task-aufnr = ls_aufnr-aufnr.
*      APPEND ls_aufnr_task TO gt_aufnr_task.

* Start new task
      PERFORM start_new_task_test_run USING ls_aufnr
                                            is_parallel
                                   CHANGING ls_aufnr_task
                                            lv_status
                                            lv_taskname.
*
      CASE lv_status.
***************************
**--> No Free Resources
***************************
        WHEN cf_al_ress_error.
          EXIT.
*
***************************************************
**--> max. processess, and no process is coming back
***************************************************
        WHEN cf_al_nort_error.
          EXIT.
*
************************
** Communcations Error
************************
        WHEN cf_al_comm_error.
          EXIT.
*
****************
** System Error
****************
        WHEN cf_al_syst_error.
          EXIT.

****************
** Application Error
****************
        WHEN cf_al_appl_error.
          EXIT.

****************
** No Error
****************
        WHEN cf_al_ok.

      ENDCASE.
*
      IF gf_appl_errors >= gc_max_appl_errors.
        lv_status = cf_al_appl_error.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDWHILE.
*
**--> Get the result for all the tasks
*  " Waiting for the already started tasks
  WAIT UNTIL gf_received_jobs >= gf_started_jobs.

*
*
ENDFORM.                    " PARALLELIZATION_TEST_RUN

*&---------------------------------------------------------------------*
*&      Form  SEQUENTIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_AUFNR  text
*----------------------------------------------------------------------*
FORM sequential USING iv_year TYPE gjahr
                      iv_month TYPE monat
                      iv_test TYPE flag
                      iv_error TYPE flag
                      iv_updcur TYPE flag
*                      it_aufnr TYPE tt_aufnr
             CHANGING et_aufnr_task TYPE tt_aufnr_task.
  FIELD-SYMBOLS: <fs_aufnr> TYPE ts_aufnr.
  DATA: ls_aufnr_task TYPE ts_aufnr_task.
  DATA: lt_aufnr TYPE tt_aufnr.
  DATA: lv_pcc_exist TYPE flag.

  PERFORM get_pcc USING iv_year iv_month CHANGING lt_aufnr.

  LOOP AT lt_aufnr ASSIGNING <fs_aufnr>.

    ls_aufnr_task-aufnr = <fs_aufnr>-aufnr.
    CALL FUNCTION 'ZSAPBF_CPZP_CORRECT_SINGLE'
      EXPORTING
        iv_year         = iv_year
        iv_month        = iv_month
        iv_aufnr        = <fs_aufnr>-aufnr
        iv_test         = iv_test
        iv_error        = iv_error
        iv_updcur       = iv_updcur
      IMPORTING
        ev_update_error = ls_aufnr_task-status.
    APPEND ls_aufnr_task TO et_aufnr_task.
  ENDLOOP.

  IF iv_test = 'X'.
    CALL FUNCTION 'ZSAPBF_CPZP_CORRECT_DISPLAY'
      EXPORTING
        iv_test    = 'X'
        iv_error   = iv_error
        iv_num_pcc = p_numpcc.
  ENDIF.
ENDFORM.                    " SEQUENTIAL
*&---------------------------------------------------------------------*
*&      Form  START_NEW_TASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_AUFNR  text
*      -->P_IV_LOG_HANDLE  text
*      -->P_IV_EXT_COMMIT  text
*      -->P_IV_COMMIT_WAIT  text
*      -->P_IV_CHRON  text
*      -->P_IS_PARALLEL  text
*      -->P_LT_TRANSFER  text
*      <--P_CV_TASKNAME  text
*      <--P_EV_STATUS  text
*----------------------------------------------------------------------*
FORM start_new_task
               USING is_aufnr      TYPE ts_aufnr
                     is_parallel   TYPE ppc_parallel
                     iv_wpssub     TYPE int4          "Added by James Sung-Kon Kim 2011.03.02
            CHANGING cs_aufnr_task TYPE ts_aufnr_task
                     ev_status TYPE c
                     cv_taskname TYPE tv_taskname.

  DATA:
*    ls_tasklist       TYPE tasklist_history,
*    lv_helpchar       TYPE c,
    lv_comm_counter   TYPE i,
    lv_syst_counter   TYPE i,
    lv_wait_counter   TYPE i,
    lv_temp_jobs      TYPE i VALUE 0,
    lv_retcode        TYPE sysubrc,
    lv_msg_txt(120)   TYPE c.                               "#EC NEEDED

  WHILE lv_comm_counter <= is_parallel-retry_commun AND
        lv_syst_counter <= is_parallel-retry_system.


*--> Now start the process
*   call RFC transfer function.
    CALL FUNCTION 'ZSAPBF_CPZP_CORRECT_SINGLE'
      STARTING NEW TASK cs_aufnr_task-taskname
      DESTINATION IN GROUP is_parallel-servergroup
      PERFORMING mat_transf_receive_process ON END OF TASK
      EXPORTING
*        iv_year               = p_year
*        iv_month              = p_month
*        iv_aufnr              = is_aufnr
*        iv_test               = ''
        iv_year         = p_year
        iv_month        = p_month
        iv_aufnr        = is_aufnr-aufnr
        iv_test         = p_test
        iv_error        = p_error
        iv_updcur       = p_updcur
        is_parallel     = is_parallel "Added by James Sung-Kon Kim 2011.03.02
        iv_wpssub       = iv_wpssub   "Added by James Sung-Kon Kim 2011.03.02
  EXCEPTIONS
        communication_failure = 1  MESSAGE lv_msg_txt
        system_failure        = 2  MESSAGE lv_msg_txt
        resource_failure      = 3.

    lv_retcode = sy-subrc.
* Error handling
    CASE lv_retcode.
* everything ok
      WHEN 0.
        gf_started_jobs = gf_started_jobs + 1.
        ev_status = cf_al_ok.

* append to the result table
        cs_aufnr_task-status = ev_status.
        APPEND cs_aufnr_task TO gt_aufnr_task.

*        IF sy-batch = 'X'.
*          MESSAGE s001(00) WITH text-006 cv_taskname.
*        ENDIF.

        EXIT.                                             "end WHILE

* Communcation Error
      WHEN 1.
        ev_status = cf_al_comm_error.
        lv_comm_counter = lv_comm_counter + 1.
        CONTINUE.                                         "in WHILE

* System Error
      WHEN 2.
        ev_status = cf_al_syst_error.
        lv_syst_counter = lv_syst_counter + 1.
        CONTINUE.                                         "in WHILE

* No Free Resources
      WHEN 3.
        lv_temp_jobs = gf_received_jobs + 1.
        CLEAR lv_wait_counter.

        WHILE lv_wait_counter < is_parallel-retry_resource.
          WAIT UNTIL gf_received_jobs >= lv_temp_jobs
          UP TO is_parallel-waittime SECONDS.
          CASE sy-subrc.
            WHEN 0 OR 4.
              " New free process
*              PERFORM tasklist_history_create.
              EXIT.
            WHEN 8.
              "No free process yet
*              PERFORM tasklist_history_create.
              lv_wait_counter = lv_wait_counter + 1.
          ENDCASE.
        ENDWHILE.
        "No free processes after n tries
        IF lv_wait_counter = is_parallel-retry_resource.
          ev_status = cf_al_ress_error.
          EXIT.                                           "end WHILE
          "Now it is free. Try again!
        ELSE.
*          ev_status = cf_al_ok.
          CONTINUE.                                       "in WHILE
        ENDIF.

    ENDCASE.
*--> End of Error Handling
  ENDWHILE.

  CLEAR cs_aufnr_task.

*--> Check: In case of errors, exit.
  IF lv_comm_counter > is_parallel-retry_commun.
    ev_status = cf_al_comm_error.
    EXIT.
  ENDIF.
  IF lv_syst_counter > is_parallel-retry_system.
    ev_status = cf_al_syst_error.
    EXIT.
  ENDIF.

  CHECK NOT ev_status = cf_al_ress_error.

* here comes the successful ones
*--> Has the maximum value for processes reached
  lv_temp_jobs = gf_started_jobs - gf_received_jobs.
  IF lv_temp_jobs >= is_parallel-wps_request.
* Wait till a process comes back
    lv_temp_jobs = gf_received_jobs + 1.
    CLEAR lv_wait_counter.
    WHILE lv_wait_counter < is_parallel-retry_resource.
      WAIT UNTIL gf_received_jobs >= lv_temp_jobs
      UP TO is_parallel-waittime SECONDS.
      CASE sy-subrc.
        WHEN 0 OR 4.
          "new free process
          EXIT.
        WHEN 8.
          "no free processes yet
          lv_wait_counter = lv_wait_counter + 1.
      ENDCASE.
    ENDWHILE.

* Still no free process? -> Does not matter?
    IF lv_wait_counter = is_parallel-retry_resource.
      ev_status = cf_al_nort_error.
*      MESSAGE e515 WITH 'ZSAPBF_CPZP_CORRECT_SINGLE'.
**        INTO lv_helpchar.
      IF cv_taskname < gc_max_taskname.
        cv_taskname = cv_taskname + 1.
      ELSE.
        cv_taskname = 1.
      ENDIF.
    ELSE.
      ev_status = cf_al_ok.
    ENDIF.
*    PERFORM tasklist_history_create.
  ENDIF.


ENDFORM.                    " START_NEW_TASK

*&---------------------------------------------------------------------*
*&      Form  START_NEW_TASK_TEST_RUN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_AUFNR  text
*      -->P_IV_LOG_HANDLE  text
*      -->P_IV_EXT_COMMIT  text
*      -->P_IV_COMMIT_WAIT  text
*      -->P_IV_CHRON  text
*      -->P_IS_PARALLEL  text
*      -->P_LT_TRANSFER  text
*      <--P_CV_TASKNAME  text
*      <--P_EV_STATUS  text
*----------------------------------------------------------------------*
FORM start_new_task_test_run
               USING is_aufnr      TYPE ts_aufnr
                     is_parallel    TYPE ppc_parallel
            CHANGING cs_aufnr_task TYPE ts_aufnr_task
                     ev_status TYPE c
                     cv_taskname TYPE tv_taskname.

  DATA:
*    ls_tasklist       TYPE tasklist_history,
*    lv_helpchar       TYPE c,
    lv_comm_counter   TYPE i,
    lv_syst_counter   TYPE i,
    lv_wait_counter   TYPE i,
    lv_temp_jobs      TYPE i VALUE 0,
    lv_retcode        TYPE sysubrc,
    lv_msg_txt(120)   TYPE c.                               "#EC NEEDED

  WHILE lv_comm_counter <= is_parallel-retry_commun AND
        lv_syst_counter <= is_parallel-retry_system.

*--> Now start the process
*   call RFC transfer function.
    CALL FUNCTION 'ZSAPBF_CPZP_CORRECT_SINGLE'
      STARTING NEW TASK cs_aufnr_task-taskname
      DESTINATION IN GROUP is_parallel-servergroup
      PERFORMING test_run_receive_process ON END OF TASK
      EXPORTING
        iv_year               = p_year
        iv_month              = p_month
        iv_aufnr              = is_aufnr
        iv_test               = p_test
        iv_error              = p_error
      EXCEPTIONS
        communication_failure = 1  MESSAGE lv_msg_txt
        system_failure        = 2  MESSAGE lv_msg_txt
        resource_failure      = 3.

    lv_retcode = sy-subrc.
* Error handling
    CASE lv_retcode.
* everything ok
      WHEN 0.
        gf_started_jobs = gf_started_jobs + 1.
        ev_status = cf_al_ok.

* append to the result table
        cs_aufnr_task-status = ev_status.
        APPEND cs_aufnr_task TO gt_aufnr_task.

*        IF sy-batch = 'X'.
*          MESSAGE s001(00) WITH text-006 cv_taskname.
*        ENDIF.

        EXIT.                                             "end WHILE

* Communcation Error
      WHEN 1.
        ev_status = cf_al_comm_error.
        lv_comm_counter = lv_comm_counter + 1.
        CONTINUE.                                         "in WHILE

* System Error
      WHEN 2.
        ev_status = cf_al_syst_error.
        lv_syst_counter = lv_syst_counter + 1.
        CONTINUE.                                         "in WHILE

* No Free Resources
      WHEN 3.
        lv_temp_jobs = gf_received_jobs + 1.
        CLEAR lv_wait_counter.

        WHILE lv_wait_counter < is_parallel-retry_resource.
          WAIT UNTIL gf_received_jobs >= lv_temp_jobs
          UP TO is_parallel-waittime SECONDS.
          CASE sy-subrc.
            WHEN 0 OR 4.
              " New free process
*              PERFORM tasklist_history_create.
              EXIT.
            WHEN 8.
              "No free process yet
*              PERFORM tasklist_history_create.
              lv_wait_counter = lv_wait_counter + 1.
          ENDCASE.
        ENDWHILE.
        "No free processes after n tries
        IF lv_wait_counter = is_parallel-retry_resource.
          ev_status = cf_al_ress_error.
          EXIT.                                           "end WHILE
          "Now it is free. Try again!
        ELSE.
*          ev_status = cf_al_ok.
          CONTINUE.                                       "in WHILE
        ENDIF.

    ENDCASE.
*--> End of Error Handling
  ENDWHILE.



  CLEAR cs_aufnr_task.

*--> Check: In case of errors, exit.
  IF lv_comm_counter > is_parallel-retry_commun.
    ev_status = cf_al_comm_error.
    EXIT.
  ENDIF.
  IF lv_syst_counter > is_parallel-retry_system.
    ev_status = cf_al_syst_error.
    EXIT.
  ENDIF.





  CHECK NOT ev_status = cf_al_ress_error.

* here comes the successful ones
*--> Has the maximum value for processes reached
  lv_temp_jobs = gf_started_jobs - gf_received_jobs.
  IF lv_temp_jobs >= is_parallel-wps_request.
* Wait till a process comes back
    lv_temp_jobs = gf_received_jobs + 1.
    CLEAR lv_wait_counter.
    WHILE lv_wait_counter < is_parallel-retry_resource.
      WAIT UNTIL gf_received_jobs >= lv_temp_jobs
      UP TO is_parallel-waittime SECONDS.
      CASE sy-subrc.
        WHEN 0 OR 4.
          "new free process
          EXIT.
        WHEN 8.
          "no free processes yet
          lv_wait_counter = lv_wait_counter + 1.
      ENDCASE.
    ENDWHILE.

* Still no free process? -> Does not matter?
    IF lv_wait_counter = is_parallel-retry_resource.
      ev_status = cf_al_nort_error.
*      MESSAGE e515 WITH 'ZSAPBF_CPZP_CORRECT_SINGLE'.
**        INTO lv_helpchar.
      IF cv_taskname < gc_max_taskname.
        cv_taskname = cv_taskname + 1.
      ELSE.
        cv_taskname = 1.
      ENDIF.
    ELSE.
      ev_status = cf_al_ok.
    ENDIF.
*    PERFORM tasklist_history_create.
  ENDIF.


ENDFORM.                    " START_NEW_TASK_TEST_RUN

*&---------------------------------------------------------------------*
*&      Form  PARALLEL_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_PARALLEL  text
*----------------------------------------------------------------------*
FORM parallel_fill USING es_parallel TYPE ppc_parallel.

  CLEAR es_parallel.
  es_parallel-para_flag       = p_par.
  es_parallel-servergroup     = p_sgr.
  es_parallel-wps_request     = p_wps. "Important, Be carefull, Still use p_wps by James 2011.03.02
  es_parallel-del_history     = p_del_hl.
  es_parallel-waittime        = p_wttime.

  es_parallel-retry_commun    = p_retryc.
  es_parallel-retry_system    = p_retrys.
  es_parallel-retry_resource  = p_retryr.


ENDFORM.                    " PARALLEL_FILL
*&---------------------------------------------------------------------*
*&      Form  MAT_TRANSF_RECEIVE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mat_transf_receive_process                             "#EC CALLED
    USING iv_taskname_receive.                              "#EC *

*  DATA: ls_tasklist_receive TYPE tasklist_history.

  DATA: lv_msg_txt(120) TYPE c,                             "#EC NEEDED
*        lv_helpchar     TYPE c,
        lv_subrc        TYPE sysubrc,
        lv_aufnr        TYPE aufnr,
        lv_update_error  TYPE c,
*        lv_plant_tabix  TYPE i,
        lv_status TYPE c.

  DATA lt_cpzp    TYPE zsapbf_tt_cpzp.
  DATA lt_cpzp_bk TYPE zsapbf_tt_cpzp.

*  DATA lt_message TYPE  balmi_t.

  FIELD-SYMBOLS: <fs_aufnr_task> TYPE ts_aufnr_task.

*--> Receive Results
  RECEIVE RESULTS FROM FUNCTION 'ZSAPBF_CPZP_CORRECT_SINGLE'
    IMPORTING
      ev_update_error   = lv_update_error
      ev_aufnr          = lv_aufnr
    TABLES
      et_cpzp           = lt_cpzp                           "#EC ENHOK
      et_cpzp_bk        = lt_cpzp_bk                        "#EC ENHOK
    EXCEPTIONS
      communication_failure = 1 message lv_msg_txt
      system_failure        = 2 message lv_msg_txt
      OTHERS                = 3.

  APPEND LINES OF lt_cpzp    TO gt_cpzp.
  APPEND LINES OF lt_cpzp_bk TO gt_cpzp_bk.

*   analyse the return code
  lv_subrc = sy-subrc.
  gf_received_jobs = gf_received_jobs + 1.

  CASE lv_subrc.
    WHEN 0.
* Error free handling
      lv_status = lv_update_error.

    WHEN 1.
* Communication Error
      lv_status = cf_al_comm_error.

    WHEN 2.
* System Error
      lv_status = cf_al_syst_error.

    WHEN 3.
* Application Error
      lv_status = cf_al_appl_error.
      gf_appl_errors = gf_appl_errors + 1.

  ENDCASE.

  READ TABLE gt_aufnr_task ASSIGNING <fs_aufnr_task>
                            WITH KEY aufnr = lv_aufnr.
  IF sy-subrc EQ 0.
    IF lv_update_error IS INITIAL.
      <fs_aufnr_task>-status = lv_status.              "System error
    ELSE.
      <fs_aufnr_task>-status = lv_update_error.       "Application error
    ENDIF.
  ENDIF.

ENDFORM.                    " MAT_TRANSF_RECEIVE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  TEST_RUN_RECEIVE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM test_run_receive_process                               "#EC CALLED
    USING iv_taskname_receive.                              "#EC *

*  DATA: ls_tasklist_receive TYPE tasklist_history.

  DATA: lv_msg_txt(120) TYPE c,                             "#EC NEEDED
*        lv_helpchar     TYPE c,
        lv_subrc        TYPE sysubrc,
        lv_aufnr        TYPE aufnr,
        lv_update_error  TYPE c,
*        lv_plant_tabix  TYPE i,
        lv_status TYPE c.

*  DATA lt_message TYPE  balmi_t.

  FIELD-SYMBOLS: <fs_aufnr_task> TYPE ts_aufnr_task.

*--> Receive Results
  RECEIVE RESULTS FROM FUNCTION 'ZSAPBF_CPZP_CORRECT_SINGLE'
    IMPORTING
      ev_update_error   = lv_update_error
      ev_aufnr          = lv_aufnr
    EXCEPTIONS
      communication_failure = 1 message lv_msg_txt
      system_failure        = 2 message lv_msg_txt
      OTHERS                = 3.

*   analyse the return code
  lv_subrc = sy-subrc.
  gf_received_jobs = gf_received_jobs + 1.

  CASE lv_subrc.
    WHEN 0.
* Error free handling
      lv_status = lv_update_error.

    WHEN 1.
* Communication Error
      lv_status = cf_al_comm_error.

    WHEN 2.
* System Error
      lv_status = cf_al_syst_error.

    WHEN 3.
* Application Error
      lv_status = cf_al_appl_error.
      gf_appl_errors = gf_appl_errors + 1.

  ENDCASE.

  READ TABLE gt_aufnr_task ASSIGNING <fs_aufnr_task>
                            WITH KEY aufnr = lv_aufnr.
  IF sy-subrc EQ 0.
    IF lv_update_error IS INITIAL.
      <fs_aufnr_task>-status = lv_status.              "System error
    ELSE.
      <fs_aufnr_task>-status = lv_update_error.       "Application error
    ENDIF.
  ENDIF.

ENDFORM.                    " TEST_RUN_RECEIVE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify .
  IF p_backup = 'X'.
    CLEAR p_par.
    LOOP AT SCREEN.
      IF screen-group1 EQ '30'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'P_PAR'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 EQ '30'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_test = 'X'.
*    CLEAR p_par.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_ERROR' OR
         screen-name EQ 'P_NUMPCC'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    CLEAR p_error.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_ERROR' OR
         screen-name EQ 'P_NUMPCC'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_run = 'X'.
    LOOP AT SCREEN.
      CASE screen-name .
        WHEN 'P_PAR' OR 'P_UPDCUR' .
          screen-input = '1'.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

  ELSE.
    LOOP AT SCREEN.
      CASE screen-name .
        WHEN 'P_UPDCUR' .
          screen-input = '0'.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
*    CLEAR p_error.
*    LOOP AT SCREEN.
*      IF screen-name EQ 'P_PAR'.
*        screen-input = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
  ENDIF.

  IF p_par = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 EQ '40'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 EQ '40'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

**** Start : Added by James Sung-Kon Kim 2011.03.02
  LOOP AT SCREEN .
    IF screen-name EQ 'P_WPSTOT' .
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF .
  ENDLOOP .
**** End : Added by James Sung-Kon Kim 2011.03.02

ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  SCREEN_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_check.
  DATA:
*        lv_month TYPE monat,
        lv_answer TYPE char1,
*        lv_guid TYPE qrp_accassobj,
        lv_current TYPE c.

  DATA : l_par_process  TYPE  int4 . "Added by James Sung-Kon Kim 2011.03.02
  l_par_process = p_wps * p_wpssub . "Added by James Sung-Kon Kim 2011.03.02

* Add by SAPCD10 on 2010.07.09
  CLEAR gv_ucomm.
  gv_ucomm = sy-ucomm.

**** Start; Added by James Sung-Kon Kim 2011/03/02
  IF p_wps IS NOT INITIAL AND p_wpssub IS NOT INITIAL.
    p_wpstot = p_wps * p_wpssub.
  ELSE.
    p_wpstot = 0.
  ENDIF.
**** End; Added by James Sung-Kon Kim 2011/03/02

***** Start : Check Parallel Process limit ***** added by James Sung-Kon Kim 2011.03.02
  IF p_par = 'X' AND p_wps IS INITIAL.
    CLEAR gv_ucomm.
    MESSAGE e012.
  ENDIF.

  IF p_par = 'X' AND p_wpssub IS INITIAL.
    CLEAR gv_ucomm.
    MESSAGE e013.
  ENDIF.
***** End : Check Parallel Process limit ***** added by James Sung-Kon Kim 2011.03.02

***** Start : Check Parallel Process limit *****
*  IF p_par = 'X' AND p_wps > 40. "Disabled by James Sung-Kon Kim 2011.03.02

  IF p_par = 'X' AND l_par_process > 40. "Disabled by James Sung-Kon Kim 2011.03.02
    CLEAR gv_ucomm.

    MESSAGE e009 WITH '40'.

  ENDIF.
***** End : Check Parallel Process limit   *****

***** Start : Check Parallel Logon Server Group *****
  IF p_par = 'X' AND p_sgr IS INITIAL.
    IF p_par = 'X'.
      LOOP AT SCREEN.
        IF screen-group1 EQ '40'.
          screen-input = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT SCREEN.
        IF screen-group1 EQ '40'.
          screen-input = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLEAR gv_ucomm.

    MESSAGE e010.

  ENDIF.
***** End : Check Parallel Logon Server Group   *****

* check date
  PERFORM period_check USING p_werks
                             p_year "Added by James Kim to fix bug 2011/01/20
                             p_month
                    CHANGING lv_current
                             gv_gjper_curr
                             gv_gjper_prev
                             .
  IF lv_current = 'X' AND p_run = 'X'.
*    MESSAGE e410. "Changed from "e410" to "w417" by James Kim to fix bug 2011/01/27
    MESSAGE w417.
  ENDIF.

  IF p_run IS INITIAL . "Added by James Kim to fix bug 2011/01/27
    CLEAR p_updcur .
  ENDIF .

  IF p_run = 'X' AND p_updcur = 'X'. "Added by James Kim to fix bug 2011/01/27
    MESSAGE w418.
  ENDIF.

*  IF p_run = 'X'.
*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
*        titlebar       = text-407
*        text_question  = text-408
*        text_button_1  = text-409
*        icon_button_1  = 'ICON_SYSTEM_OKAY'
*        text_button_2  = text-410
*        icon_button_2  = 'ICON_SYSTEM_CANCEL'
*      IMPORTING
*        answer         = lv_answer
*      EXCEPTIONS
*        text_not_found = 1
*        OTHERS         = 2.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    IF lv_answer NE '1'.
*      STOP.
*    ENDIF.
*
*    CLEAR p_error.
*  ENDIF.


*-B RWU- 20081217 give message all PCC may case performance issue
* Replaced by SAPCD10 on 2010.07.08
*  IF so_aufnr IS INITIAL.
  IF so_aufnr IS INITIAL AND
    ( sy-ucomm = 'ONLI' OR sy-ucomm = 'SPOS' OR sy-ucomm = 'SJOB' OR sy-ucomm = 'PRIN' ).
    CLEAR lv_answer.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = text-407
        text_question  = text-411
        text_button_1  = text-409
        icon_button_1  = 'ICON_SYSTEM_OKAY'
        text_button_2  = text-410
        icon_button_2  = 'ICON_SYSTEM_CANCEL'
      IMPORTING
        answer         = lv_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lv_answer NE '1'.
      STOP.
    ENDIF.
  ENDIF.
* -E RWU-

ENDFORM.                    " SCREEN_CHECK
*&---------------------------------------------------------------------*
*&      Form  PERIOD_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SO_AUFNR_LOW  text
*      -->P_P_MONTH  text
*      <--P_LV_CURRENT  text
*----------------------------------------------------------------------*
FORM period_check USING iv_werks TYPE werks_d
                        iv_year TYPE gjahr "Added by James Kim to fix bug 2011/01/20
                        iv_month TYPE monat
               CHANGING ev_current TYPE flag
                        lv_gjper_curr
                        lv_gjper_prev.

  DATA: lv_gjper_post TYPE co_gjper.
*        lv_werks TYPE werks_d


  CHECK iv_month IS NOT INITIAL.
*
*  SELECT SINGLE prwrk
*           FROM ckmlmv013
*           INTO lv_werks
*          WHERE aufnr = iv_aufnr.

  PERFORM periods_get IN PROGRAM saplqrprp USING iv_werks
                                                 sy-datlo
                                        CHANGING lv_gjper_post
                                                 lv_gjper_curr
                                                 lv_gjper_prev.

  IF iv_month = lv_gjper_curr+5(2) AND
     iv_year = lv_gjper_curr(4).  " Added by James Kim to fix bug 2011/01/20
    ev_current = 'X'.
  ELSEIF iv_month = lv_gjper_prev+5(2) AND
     iv_year = lv_gjper_prev(4).  " Added by James Kim to fix bug 2011/01/20
    ev_current = ''.
  ELSE.

**** Important; In Order To allow for the past period before previous period.  2011/01/27
**** Start; Added by James Kim 2011/01/27
    lv_gjper_post = iv_year * 1000 + iv_month.

    IF lv_gjper_post GT lv_gjper_curr.
      MESSAGE e419.
    ELSE.
      lv_gjper_curr = iv_year * 1000 + iv_month.
      lv_gjper_prev = iv_year * 1000 + iv_month.
**** End; Added by James Kim 2011/01/27
*    MESSAGE e405.. "Changed from "e405" to "w416" by James Kim to fix bug 2011/01/27
      MESSAGE w416.
    ENDIF.

  ENDIF.
ENDFORM.                    " PERIOD_CHECK
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_AUFNR_TASK  text
*----------------------------------------------------------------------*
FORM output_result USING it_aufnr_task TYPE tt_aufnr_task.
  DATA: ls_variant  TYPE disvariant,
        lt_fieldcat TYPE STANDARD TABLE OF slis_fieldcat_alv,
        ls_layout TYPE  slis_layout_alv.
  FIELD-SYMBOLS: <fs_aufnr_task> TYPE ts_aufnr_task.
  FIELD-SYMBOLS: <fs_fieldcat> TYPE slis_fieldcat_alv.


  ls_layout-colwidth_optimize = charx.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSAPBF_AUFNR_TASK'
      i_inclname       = sy-cprog
    CHANGING
      ct_fieldcat      = lt_fieldcat
    EXCEPTIONS
      OTHERS           = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ls_variant-report   = sy-repid.
  ls_variant-username = sy-uname.

  LOOP AT it_aufnr_task ASSIGNING <fs_aufnr_task>.
    CASE <fs_aufnr_task>-status.
      WHEN 'A'.
        <fs_aufnr_task>-description = text-060. "'Application error'.
      WHEN 'S'.
        <fs_aufnr_task>-description = text-061. "'System error'.
      WHEN 'C'.
        <fs_aufnr_task>-description = text-062. "'Communication error'.
      WHEN 'R'.
        <fs_aufnr_task>-description = text-063. "'Resource error'.
      WHEN 'L'.
        <fs_aufnr_task>-description = text-064. "'Locking error'.
      WHEN 'U'.
        <fs_aufnr_task>-description = text-065. "'Update error'.
      WHEN 'N'.
        <fs_aufnr_task>-description = text-066. "'No process error'.
      WHEN 'E'.
        <fs_aufnr_task>-description = text-068. "'No data in correction period (Successful)'
      WHEN 'P'.
        <fs_aufnr_task>-description = text-069. "'No difference in correction period (Successful)'
      WHEN ' '.
        <fs_aufnr_task>-description = text-067. "'Update Successful (Successful)'.
    ENDCASE.
  ENDLOOP.


  LOOP AT lt_fieldcat ASSIGNING <fs_fieldcat>.
    CASE <fs_fieldcat>-fieldname.
      WHEN 'TASKNAME'.
        <fs_fieldcat>-no_out = 'X'.
    ENDCASE.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_grid_title       = space
      it_fieldcat        = lt_fieldcat
      i_callback_program = 'ZSAPBF_CPZP_CORRECTION_MASSIVE' "sy-cprog
      is_variant         = ls_variant
      i_save             = 'A'
      is_layout          = ls_layout
    TABLES
      t_outtab           = it_aufnr_task
    EXCEPTIONS
      OTHERS             = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


*  WRITE:/ text-050 , 20 text-051.
*  SKIP.
*  WRITE:/ sy-uline(255).
*  LOOP AT it_aufnr_task ASSIGNING <fs_aufnr_task>.
*    IF <fs_aufnr_task>-status IS NOT INITIAL.
*      WRITE:/ <fs_aufnr_task>-aufnr,  text-053.
*    ELSE.
*      WRITE:/ <fs_aufnr_task>-aufnr,  text-054.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " OUTPUT_RESULT
*&---------------------------------------------------------------------*
*&      Form  INTIAL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM intial_screen .
** 08/08/13 by Furong
*  p_month = sy-datlo+4(2) - 1.
    p_month = sy-datlo+4(2).
** End
  IF p_month = 0.
    p_month = 12.
    p_year = p_year - 1.
  ENDIF.

ENDFORM.                    " INTIAL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  CHECK_PARALLEL_SERVERGROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_parallel_servergroup  CHANGING p_free_wps.

  DATA: l_max_wps  TYPE i,
        l_free_wps TYPE i.


* Determine Free work process
  CALL FUNCTION 'SPBT_INITIALIZE'
    EXPORTING
      group_name                     = p_sgr
    IMPORTING
      max_pbt_wps                    = l_max_wps
      free_pbt_wps                   = p_free_wps
    EXCEPTIONS
      invalid_group_name             = 1
      internal_error                 = 2
      pbt_env_already_initialized    = 3
      currently_no_resources_avail   = 4
      no_pbt_resources_found         = 5
      cant_init_different_pbt_groups = 6
      OTHERS                         = 7.

  CASE sy-subrc.

    WHEN 0.
* do nothing

    WHEN 1. "Added by James Sung-Kon Kim 2011.03.02
      "MESSAGE ID 'ppc1pr' TYPE 'E' NUMBER 503.
      MESSAGE s503(ppc1pr) DISPLAY LIKE 'E'.   "  RAISING init_error.
      STOP.

    WHEN 3.
** Remarks: General message table should be updated with the messages
** Also, messages with type 'X' should be converted to 'E' and the
** program should be stopped from running.
      CALL FUNCTION 'SPBT_GET_CURR_RESOURCE_INFO'
        IMPORTING
          max_pbt_wps                 = l_max_wps
          free_pbt_wps                = p_free_wps
        EXCEPTIONS
          internal_error              = 1
          pbt_env_not_initialized_yet = 2
          OTHERS                      = 3.

      IF sy-subrc <> 0.
        p_free_wps = 0.
      ENDIF.

    WHEN OTHERS.
      p_free_wps = 0.
  ENDCASE.

ENDFORM.                    " CHECK_PARALLEL_SERVERGROUP
*&---------------------------------------------------------------------*
*&      Form  GET_PCC_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_AUFNR  text
*----------------------------------------------------------------------*
FORM get_pcc_new  CHANGING et_aufnr TYPE tt_aufnr.
  DATA: lt_aufnr TYPE tt_aufnr.

* Commented by SAPCD08 on 2010.07.08
*  SELECT aufnr FROM ckmlmv013
*               INTO TABLE lt_aufnr
*              WHERE aufnr IN so_aufnr
*                AND prwrk = p_werks
*                AND pmatn IN so_pmatn
*                AND verid IN so_verid.

* Start : Added by SAPCD08 on 2010.07.08, replacing the block commentated above.
  SELECT b~aufnr b~cc_guid
    FROM ckmlmv013 AS a
   INNER JOIN qrp002 AS b
      ON a~mandt = b~mandt
     AND a~aufnr = b~aufnr
    INTO TABLE et_aufnr
   WHERE a~aufnr IN so_aufnr
     AND a~prwrk = p_werks
     AND a~pmatn IN so_pmatn
     AND a~verid IN so_verid.
* End : Added by SAPCD08 on 2010.07.08, replacing the block commentated above.

  IF et_aufnr IS INITIAL.
    MESSAGE i408.
    STOP.
  ENDIF.

ENDFORM.                    " GET_PCC_NEW
*&---------------------------------------------------------------------*
*&      Form  CHECK_PCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_AUFNR  text
*      <--P_LV_PCC_EXIST  text
*----------------------------------------------------------------------*
FORM check_pcc  USING    ls_aufnr
                         ls_accassobj
                CHANGING lv_pcc_exist.
  TABLES: qrp002, ppc_head.
  DATA lv_first_day TYPE sydatum.
  DATA lv_last_day  TYPE sydatum.
  DATA lt_accassobj TYPE tt_accassobj.
  DATA lv_accassobj TYPE qrp_accassobj.

  lv_first_day+0(4) = p_year.
  lv_first_day+4(2) = p_month.
  lv_first_day+6(2) = '01'.
  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_first_day
    IMPORTING
      last_day_of_month = lv_last_day
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Commented by SAPCD10 on 2010.07.13
*  SELECT SINGLE * FROM qrp002 AS a
**               JOIN ppc_ord_inf AS b
**               ON a~cc_guid = b~accassobj
**               JOIN ppc_head
**               ON ppc_ord_inf~orderid = ppc_head~orderid
**               INTO TABLE et_aufnr
**               FOR ALL ENTRIES IN lt_aufnr
*              WHERE aufnr = ls_aufnr
**                AND ppc_ord_inf~dummy_order = space
**                AND ppc_head~flg_del EQ space
**                AND ppc_head~flg_synch = 'X'
**                AND ppc_head~postdate GE lv_first_day
**                AND ppc_head~postdate LE lv_last_day
*                AND EXISTS ( SELECT mandt FROM ppc_head
*                              WHERE accassobj   = a~cc_guid
*                                AND flg_del     = space
*                                AND flg_asynch  = 'X' "flg_asynch : no problem!!
*                                AND postdate GE lv_first_day
*                                AND postdate LE lv_last_day ).

  SELECT SINGLE a~accassobj FROM ppc_head AS a
    INTO lv_accassobj
   WHERE a~accassobj = ls_accassobj
*     AND a~flg_del     = space " Deleted for HMMA case, PPC_HEAD will be deleted after shipping.
     AND a~flg_asynch  = 'X' "flg_asynch : no problem!!
     AND a~postdate GE lv_first_day
     AND a~postdate LE lv_last_day.

  IF sy-subrc = 0.
    lv_pcc_exist = 'X'.
  ELSE.
    CLEAR lv_pcc_exist.
  ENDIF.


ENDFORM.                    " CHECK_PCC
*&---------------------------------------------------------------------*
*&      Form  CHECK_PPC_PARALLEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_AUFNR  text
*----------------------------------------------------------------------*
FORM check_ppc_parallel  USING  pt_aufnr TYPE tt_aufnr
                                p_wps.

  DATA: ps_aufnr   TYPE ts_aufnr.
*  DATA: lt_aufnr   TYPE tt_aufnr.
  DATA: lt_aufnr LIKE qrp002 OCCURS 0 WITH HEADER LINE.
  DATA: lv_tot_cnt TYPE sytabix,
        lv_std_cnt TYPE sytabix,
        lv_cur_cnt TYPE sytabix,
        lv_all_cnt TYPE sytabix.
  DATA: lv_flag    TYPE flag.
  DATA: lv_task_cnt(4) TYPE n.
  DATA: l_index     LIKE sy-tabix.
  DATA lv_first_day TYPE sydatum.
  DATA lv_last_day  TYPE sydatum.

  lv_first_day+0(4) = p_year.
  lv_first_day+4(2) = p_month.
  lv_first_day+6(2) = '01'.

  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_first_day
    IMPORTING
      last_day_of_month = lv_last_day
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DESCRIBE TABLE pt_aufnr LINES lv_tot_cnt.

  IF lv_tot_cnt <= p_wps.
    lv_flag = ''.
  ELSE.
    lv_std_cnt = lv_tot_cnt / p_wps.
    lv_flag = 'X'.
  ENDIF.

* Task List Create
  DO p_wps TIMES.
    lv_task_cnt = lv_task_cnt + 1.
    CONCATENATE 'CPZP_Aufnr' lv_task_cnt INTO gt_task-name.
    gt_task-classname = p_sgr.
    gt_task-status = 'I'.
    APPEND gt_task.
    CLEAR gt_task.
  ENDDO.

  LOOP AT pt_aufnr INTO ps_aufnr.
    lv_cur_cnt = lv_cur_cnt + 1.
    lv_all_cnt = lv_all_cnt + 1.

    CLEAR lt_aufnr.
    MOVE-CORRESPONDING ps_aufnr TO lt_aufnr.
    APPEND lt_aufnr.

    IF lv_flag = 'X'.
      IF lv_cur_cnt = lv_std_cnt OR lv_all_cnt = lv_tot_cnt.
* Parallel Processing
        DO.
          READ TABLE gt_task WITH KEY status = 'I'.
          IF sy-subrc = 0.
            l_index = sy-tabix.

            CALL FUNCTION 'ZSAPBF_CHECK_AUFNR'
              STARTING NEW TASK gt_task-name
              DESTINATION IN GROUP gt_task-classname
              PERFORMING return_data ON END OF TASK
              EXPORTING
                iv_first_day          = lv_first_day
                iv_last_day           = lv_last_day
              TABLES
                it_aufnr              = lt_aufnr
              EXCEPTIONS
                communication_failure = 1
                system_failure        = 2
                resource_failure      = 3.
            IF sy-subrc <> 0.
              WAIT UNTIL g_rcv >= g_snd UP TO 1 SECONDS.
            ELSE.
              READ TABLE gt_task INDEX l_index.
              gt_task-status = 'W'.
              MODIFY gt_task INDEX l_index.
*           send data
              g_snd = g_snd + 1.
              EXIT.
            ENDIF.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.

        ENDDO.

        CLEAR: lv_cur_cnt, lt_aufnr[].
      ENDIF.
    ELSE.
* Parallel Processing
      DO.
        READ TABLE gt_task WITH KEY status = 'I'.
        IF sy-subrc = 0.
          l_index = sy-tabix.

          CALL FUNCTION 'ZSAPBF_CHECK_AUFNR'
            STARTING NEW TASK gt_task-name
            DESTINATION IN GROUP gt_task-classname
            PERFORMING return_data ON END OF TASK
            EXPORTING
              iv_first_day          = lv_first_day
              iv_last_day           = lv_last_day
            TABLES
              it_aufnr              = lt_aufnr
            EXCEPTIONS
              communication_failure = 1
              system_failure        = 2
              resource_failure      = 3.
          IF sy-subrc <> 0.
            WAIT UNTIL g_rcv >= g_snd UP TO 1 SECONDS.
          ELSE.
            READ TABLE gt_task INDEX l_index.
            gt_task-status = 'W'.
            MODIFY gt_task INDEX l_index.
*           send data
            g_snd = g_snd + 1.
            EXIT.
          ENDIF.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.

      ENDDO.

      CLEAR lt_aufnr[].
    ENDIF.
  ENDLOOP.

  WAIT UNTIL g_rcv >= g_snd.

ENDFORM.                    " CHECK_PPC_PARALLEL
*&---------------------------------------------------------------------*
*&      Form  RETURN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM return_data  USING taskname.

  DATA: lv_tabix TYPE sytabix.
  DATA: lt_aufnr TYPE TABLE OF aufnr_s.

  CLEAR: lt_aufnr.

  RECEIVE RESULTS FROM FUNCTION 'ZSAPBF_CHECK_AUFNR'
    TABLES
      et_aufnr          = lt_aufnr
    EXCEPTIONS
      invalid_bzobj    = 1
      keko_not_found   = 2
      meta_model_error = 3
      ckhs_not_found   = 4
      other_reason     = 5
      OTHERS           = 6.

  g_rcv = g_rcv + 1.

  IF sy-subrc = 0.

    APPEND LINES OF lt_aufnr TO gt_aufnr.

* receive data
    READ TABLE gt_task WITH KEY name = taskname.
    IF sy-subrc = 0.    "Register data
      lv_tabix = sy-tabix.
      gt_task-status = 'I'.
      MODIFY gt_task INDEX lv_tabix.
      CLEAR lv_tabix.
    ENDIF.

  ENDIF.

ENDFORM.                    " RETURN_DATA
