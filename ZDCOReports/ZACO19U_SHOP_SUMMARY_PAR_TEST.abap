*&---------------------------------------------------------------------*
*& Report  ZCO_SHOP_SUMMARY_PAR
*& Shop Summary - Cost Component break down
*&  P_PR_CNT : how many line item is assigned 1 Task
*&  P_TA_CNT : how many task is process in parallelly
*&  P_WT_SCN : how many time would wait until starting next task
*&  Call Z_SHOP_CC_PARALLEL (This function submit original Shop Cost
*&  Summary program ZACO19U_SHOP_SUMMARY_NEW )
*&---------------------------------------------------------------------*
REPORT zco_shop_summary_par .
TABLES : keko.
DATA : g_count(5) TYPE n.

*PARAMETER for PROGRESSING
DATA : line_cnt           TYPE i,
       tot_data_cnt(5)    TYPE n, "Total DATA count
       task_cnt(5)        TYPE n, "processing TASK count
       stored_task_cnt(5) TYPE n, "STORED TASK count
       stored_data_cnt(5) TYPE n, "STORED DATA count
       rest_data_cnt(5)   TYPE n. "Remained DATA count

TYPES: BEGIN OF typ_log_msg,
       msgty(1)     TYPE c,
       msglevl(1)   TYPE c,
       msg(100)     TYPE c,
       END OF typ_log_msg.
DATA: log_msg_t TYPE typ_log_msg,
      log_msg_s TYPE typ_log_msg.

*-RFC parallelprocessing
TYPES: BEGIN OF tasklist_type,
       taskname(04) TYPE c, "Task administration
       status(1)    TYPE c,
       record(20)   TYPE c,
*RFC
       END OF tasklist_type.

DATA: info LIKE rfcsi, c,  "Message text
      jobs TYPE i VALUE 10,  "Number of parallel jobs
      snd_jobs TYPE i VALUE 1,  "Sent jobs
      rcv_jobs TYPE i VALUE 1,  "Received replies
      excp_flag(1) TYPE c,  "Number of RESOURCE_FAILUREs
      taskname(4) TYPE n VALUE '0001',  "Task name administration
      tasklist   TYPE TABLE OF tasklist_type WITH HEADER LINE,
      tasklist_g TYPE TABLE OF tasklist_type WITH HEADER LINE,
      wa_tasklist TYPE tasklist_type.
*-RFC parallelprocessing


DATA : it_mat      LIKE zsco_shop_mat OCCURS 0 WITH HEADER LINE.
DATA : it_fsc_mat  LIKE zsco_shop_fsc_mat OCCURS 0 WITH HEADER LINE.
DATA : it_fsc_temp LIKE zsco_shop_fsc_mat OCCURS 0 WITH HEADER LINE.
*processed materials
DATA : it_fsc_prc  LIKE zsco_shop_fsc_mat OCCURS 0 WITH HEADER LINE.


************************************************************************
* SELECTION SCREEN ( SELECT-OPTIONS & PARAMETERS )
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS : p_kokrs LIKE csks-kokrs   MEMORY ID cac  OBLIGATORY.

* Posted Yr.
PARAMETERS : p_bdatj LIKE keko-bdatj MEMORY ID bdtj OBLIGATORY.
* periods
PARAMETERS : p_perab LIKE covja-perab MEMORY ID vpe
             MODIF ID per OBLIGATORY.
SELECT-OPTIONS : s_matnr FOR keko-matnr.

SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
PARAMETERS :  p_pr_cnt  TYPE i DEFAULT 1,
              p_ta_cnt  TYPE i DEFAULT 5,
              p_wt_scn  TYPE i DEFAULT 5.
SELECTION-SCREEN END OF BLOCK bl2.


************************************************************************
* START-OF-SELECTION                                                   *
************************************************************************
START-OF-SELECTION.

*data : begin of it_mat2 occurs 0,
*       matnr type matnr,
*       end of it_mat2.
*
*data : it_ztco_shop_sum like ztco_shop_sum occurs 0 with header line.
*
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE  it_ztco_shop_sum
*           FROM ztco_shop_sum
*           WHERE bdatj  =  p_bdatj
*             and POPER  = p_perab.
*
*   loop at it_ztco_shop_sum.
*      it_mat2-matnr = it_ztco_shop_sum-artnr.
*      collect it_mat2.
*   endloop.
*
*  break-point.
  PERFORM get_shop_summary .

  PERFORM call_paralle_function.

*&---------------------------------------------------------------------*
*&      Form  select_ext_spec
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_shop_summary .
  DATA : l_index(2) TYPE n.

* Read Base Information
  PERFORM read_materials.

  DESCRIBE TABLE it_fsc_temp LINES line_cnt.
*  SORT it_fsc_temp by matnr.

** define working task
  DO p_ta_cnt TIMES.
    taskname = taskname + 1.

*   CONCATENATE 'SHOP_' l_index INTO tasklist-taskname.
    tasklist-taskname = taskname.
    tasklist-status = 'I'.

    APPEND tasklist. CLEAR tasklist.
  ENDDO.

ENDFORM.                    " select_ext_spec
*&---------------------------------------------------------------------*
*&      Form  call_paralle_function
*&---------------------------------------------------------------------*
FORM call_paralle_function .
  DATA : l_start TYPE i,
         l_end   TYPE i,
         l_tot   TYPE i,
         l_prc   TYPE i.

* P_PR_CNT : line count for one tasking
  l_start = 1.
  l_end   = p_pr_cnt.

  CHECK NOT it_fsc_temp[] IS INITIAL.
  SORT it_fsc_temp BY mtart matnr verid.
  DESCRIBE TABLE it_fsc_temp LINES l_tot.

  CLEAR: rcv_jobs, snd_jobs.

  DO.
*    g_count = g_count + 1.
*    it_return-times = g_count.
*    APPEND it_return.

* make data packet for processing
    REFRESH: it_fsc_mat.
    CLEAR: l_start.
    LOOP AT it_fsc_temp.
      READ TABLE it_fsc_prc WITH KEY matnr = it_fsc_temp-matnr.
      IF sy-subrc <> 0.
        APPEND it_fsc_temp TO it_fsc_mat.
        l_start = l_start + 1.
      ENDIF.
      IF l_start >= l_end. EXIT. ENDIF.
    ENDLOOP.
*   DELETE it_fsc_temp FROM l_start TO l_end.

* Call function for submit real program.
    IF it_fsc_mat[] IS INITIAL.
      WAIT UP TO p_wt_scn SECONDS.  "waiting
    ELSE.
      PERFORM call_rfc_function TABLES it_mat
                                       it_fsc_mat.
    ENDIF.




    DESCRIBE TABLE it_fsc_prc  LINES l_prc.
    IF it_fsc_temp[] IS INITIAL ." OR l_tot = l_prc.
      EXIT.
    ENDIF.

  ENDDO.

* Receive remaining asynchronous replies
  WAIT UNTIL rcv_jobs >= snd_jobs.


*  LOOP AT tasklist_g INTO wa_tasklist.
*    WRITE:/   'Received task:',
*              wa_tasklist-taskname, wa_tasklist-record.
*  ENDLOOP.

*  DO.
*    READ TABLE it_task WITH KEY status = 'W'.
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*    WAIT UP TO 5 SECONDS.
*  ENDDO.


ENDFORM.                    " call_paralle_function
*&---------------------------------------------------------------------*
*&      Form  CALL_RFC_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXT  text
*      -->P_SY_INDEX  text
*----------------------------------------------------------------------*
FORM call_rfc_function  TABLES   pt_mat     STRUCTURE it_mat
                                 pt_fsc_mat STRUCTURE it_fsc_mat.

  DATA : msg_txt(80),
         l_index  TYPE sy-tabix,
         l_text(100),
         l_serial TYPE i ,
         l_subrc  TYPE subrc.

  DO.
    READ TABLE tasklist WITH KEY status = 'I'.
    IF sy-subrc <> 0.
      WAIT UP TO p_wt_scn SECONDS.  "waiting
    ELSE.

      l_index = sy-tabix.
      READ TABLE pt_fsc_mat INDEX 1.
*submit parallel processes
      CALL FUNCTION 'Z_SHOP_CC_PARALLEL_TEST'
         STARTING NEW TASK tasklist-taskname
         DESTINATION IN GROUP DEFAULT
         PERFORMING return_info ON END OF TASK
         EXPORTING
            p_kokrs     = p_kokrs
            p_bdatj     = p_bdatj
            p_perab     = p_perab
         TABLES
            it_mat      = pt_mat
            it_fsc_mat  = pt_fsc_mat
         EXCEPTIONS
            communication_failure = 1
            system_failure        = 2
            resource failure      = 3.

      CASE sy-subrc.
        WHEN 0.
* Administration of asynchronous tasks
          tasklist-status = 'W'.
          MODIFY tasklist INDEX l_index.
          APPEND LINES OF pt_fsc_mat TO it_fsc_prc.

          CONCATENATE 'Started task: ' tasklist-taskname
                      pt_fsc_mat-matnr
                      INTO l_text SEPARATED BY space.
          PERFORM progress_ind USING '10' l_text.

          snd_jobs = snd_jobs + 1.
          CLEAR: excp_flag.
          EXIT.  "Job processing finished

        WHEN 1 OR 2.
* Handling of communication and system failure
          excp_flag = 'X'.
*          CONCATENATE 'System failure:  ' it_fsc_mat-matnr
*                 INTO l_text SEPARATED BY space.
*          PERFORM progress_ind USING '50' l_text.

        WHEN 3.  "No resources available at present
* Receive reply to asynchronous RFC calls
          IF excp_flag = space.
            excp_flag = 'X'.
* First attempt for RESOURCE_FAILURE handling
            WAIT UNTIL rcv_jobs >= snd_jobs UP TO '0.1' SECONDS.
          ELSE.
* Second attempt for RESOURCE_FAILURE handling
            WAIT UNTIL rcv_jobs >= snd_jobs UP TO '1.0' SECONDS.
          ENDIF.
          IF sy-subrc = 0.
            CLEAR excp_flag.  "Reset flag
          ELSE.  "No replies
            "Endless loop handling
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDDO.

ENDFORM.                    " CALL_RFC_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  GET_RESULT
*&---------------------------------------------------------------------*
FORM return_info  USING taskname.

  DATA: lt_log_msg TYPE typ_log_msg.
  DATA : l_subrc TYPE subrc,
         l_artnr TYPE matnr,
         l_text(50),
         l_err(25).
  DATA : ls_fsc_mat  LIKE zsco_shop_fsc_mat OCCURS 0 WITH HEADER LINE.

* Get Result
  RECEIVE RESULTS FROM FUNCTION 'Z_SHOP_CC_PARALLEL'
     IMPORTING
        subrc      = l_subrc
        artnr      = l_artnr
     TABLES
        it_fsc_mat = ls_fsc_mat
     EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        resourcefailure       = 3.

  IF sy-subrc = 0.
    LOOP AT ls_fsc_mat.
*complete
      DELETE it_fsc_temp WHERE matnr = ls_fsc_mat-matnr.
      wa_tasklist-taskname = tasklist-taskname.
      wa_tasklist-record   = ls_fsc_mat-matnr.
      APPEND wa_tasklist TO tasklist_g.
    ENDLOOP.

  ELSE.
* Handling of communication and system failure
    LOOP AT ls_fsc_mat.
      CONCATENATE ls_fsc_mat-matnr 'Communication and system failure'
             INTO l_text SEPARATED BY space.
      PERFORM progress_ind USING '90' l_text.
*re-processing
      DELETE it_fsc_prc  WHERE matnr = ls_fsc_mat-matnr.
    ENDLOOP.
  ENDIF.

* TASK RELEASE.
  READ TABLE tasklist WITH KEY taskname = taskname
                      INTO wa_tasklist.
  IF sy-subrc = 0.  "Register data
    wa_tasklist-status = 'I'.
    MODIFY tasklist INDEX sy-tabix FROM wa_tasklist.
  ENDIF.

  rcv_jobs = rcv_jobs + 1.  "Receiving data

* store the message
* append lines of lt_log_msg to log_msg_t.

ENDFORM.                    " GET_RESULT



*Selection texts
*---------------
*SP_PR_CNT        Processing Count
*SP_TA_CNT        Task Count
*&---------------------------------------------------------------------*
*&      Form  read_materials
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_materials.

  RANGES: r_bwkey FOR ckmlhd-bwkey.
  TABLES: t001k, tckh4.

* Read Active Cost Component Structure
  CLEAR tckh4.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF tckh4
                  FROM tckh4
                 WHERE elehk = 'H1'
                   AND aktiv = 'X'.

* select materials
  r_bwkey-sign = 'I'. r_bwkey-option = 'EQ'.
  SELECT * FROM t001k WHERE bukrs = p_kokrs.
    r_bwkey-low = t001k-bwkey. APPEND r_bwkey.
  ENDSELECT.

* MARA + MBEW(current period) = MACKU, MARC
  DATA: l_abrechdat LIKE ckmlhd-abrechdat.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE  it_mat
           FROM ( mbew INNER JOIN mara
                          ON mbew~matnr = mara~matnr
                       INNER JOIN marc
                          ON mbew~matnr = marc~matnr
                         AND mbew~bwkey = marc~werks
                       INNER JOIN ckmlmv011
                          ON ckmlmv011~matnr = mbew~matnr
                         AND ckmlmv011~bwkey = mbew~bwkey
                       INNER JOIN ckmlrunperiod
                          ON ckmlmv011~laufid = ckmlrunperiod~run_id
                       INNER JOIN ckmlhd
                          ON ckmlmv011~kalnr = ckmlhd~kalnr )
           WHERE ckmlrunperiod~gjahr = p_bdatj
             AND ckmlrunperiod~poper = p_perab
             AND ckmlmv011~bwkey IN r_bwkey
             AND ckmlhd~abrechdat <> l_abrechdat .

* Read FSC/HALB Mat.
  SORT it_mat BY werks mtart.

  CLEAR : it_fsc_mat, it_fsc_mat[].
  LOOP AT it_mat WHERE matnr IN s_matnr
                   AND kalst > 0.
* LOOP AT it_mat WHERE kalst > 1.
    CLEAR it_fsc_temp.
    MOVE-CORRESPONDING it_mat  TO it_fsc_temp .
    APPEND it_fsc_temp.
  ENDLOOP.
  CLEAR:  it_fsc_temp, it_mat.

*sort for binary search
  SORT it_mat BY matnr bwkey bwtar.

  CHECK NOT it_fsc_temp[] IS INITIAL.
  PERFORM read_obj_for_pcc.

ENDFORM.                    " read_materials
*&---------------------------------------------------------------------*
*&      Form  progress_ind
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0400   text
*      -->P_L_TEXT  text
*----------------------------------------------------------------------*
FORM progress_ind USING    p_%
                           p_text.
  CALL FUNCTION 'FI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = p_%
            text       = p_text.


ENDFORM.                    " progress_ind
*&---------------------------------------------------------------------*
*&      Form  read_obj_for_pcc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_obj_for_pcc.
  DATA: BEGIN OF lt_pcc OCCURS 0,
          objnr       LIKE aufk-objnr,
          aufnr       LIKE aufk-aufnr,
          pkosa       LIKE ckmlmv013-pkosa,  "Cost Collector
          kalnr_proc  LIKE ckmlmv013-kalnr_proc,
          prwrk       LIKE ckmlmv013-prwrk,
          pmatn       LIKE ckmlmv013-pmatn,
          verid       LIKE ckmlmv013-verid,
          klvarp      LIKE afko-klvarp,      "CostingVariant-plan
        END OF lt_pcc.


* Create index of CKMLMV013!!!  (MANDT/PRWRK/PMATN)
  SELECT aufk~objnr aufk~aufnr
         ckmlmv013~pkosa   ckmlmv013~kalnr_proc
         ckmlmv013~prwrk   ckmlmv013~pmatn      ckmlmv013~verid
         afko~klvarp
     INTO CORRESPONDING FIELDS OF TABLE lt_pcc
     FROM ckmlmv013
        INNER JOIN aufk
           ON aufk~aufnr   = ckmlmv013~pkosa
        INNER JOIN afko
           ON afko~aufnr   = aufk~aufnr
     FOR ALL entries IN it_fsc_temp
     WHERE ckmlmv013~prwrk = it_fsc_temp-werks
       AND ckmlmv013~pmatn = it_fsc_temp-matnr
       AND ckmlmv013~loekz = space       "deletion
       AND ckmlmv013~autyp = '05'.       "PCC


  SORT lt_pcc BY pmatn prwrk.
  LOOP AT it_fsc_temp.
    CLEAR lt_pcc.
    READ TABLE lt_pcc WITH KEY pmatn = it_fsc_temp-matnr
                               prwrk = it_fsc_temp-werks
                      BINARY SEARCH.
    IF sy-subrc <> 0 .
      DELETE it_fsc_temp.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " read_obj_for_pcc
