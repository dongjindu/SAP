*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_STEP2_GO_TSS2F01
*&---------------------------------------------------------------------*
FORM parameter_check.

* 1) Allowed number of components
  IF ( p_limit GT gc_maxlimit )
    OR ( p_limit LT gc_lowlimit ).
    MESSAGE e004 WITH p_limit gc_lowlimit gc_maxlimit.
  ENDIF.

** 2) Check package size,
*  IF p_size >  gc_max_size.
*    MESSAGE e005.
*  ENDIF.

* 3) Parallelization --> Server groupe required
  IF p_par = gc_charx AND p_sgr IS INITIAL.
    MESSAGE e001.
  ENDIF.
* 4) Parallelization --> min. 2 Processes
  IF p_par = gc_charx AND p_wps < 2.
    MESSAGE e003.
  ENDIF.

* 5) Check time delay
  IF p_delay < gc_min_sec.
    MESSAGE w024(zsapbf).
  ENDIF.

* 6) Check backflush entry date/time
  PERFORM check_conftime CHANGING   gr_conftime.

ENDFORM.                               " PARAMETER_CHECK

*&---------------------------------------------------------------------*
*&      Form  PARALLEL_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ES_PARALLEL  text
*----------------------------------------------------------------------*
FORM parallel_fill CHANGING es_parallel TYPE zsapbf_ppc_parallel.


  CLEAR es_parallel.
  es_parallel-para_flag       = p_par.
  es_parallel-servergroup     = p_sgr.
  es_parallel-wps_request     = p_wps.
  es_parallel-del_history     = p_del_hl.
  es_parallel-waittime        = p_wttime.
*  es_parallel-waitendtime     = p_wtendt.
  es_parallel-retry_commun    = p_retryc.
  es_parallel-retry_system    = p_retrys.
  es_parallel-retry_resource  = p_retryr.
*  es_parallel-retry_endwait   = p_retrye.
*  es_parallel-zz_pack_size_t1 = p_size.

*CDP2 Start
*  es_parallel-zz_meth_par_s = p_meth.
  IF p_meth2 = 'X'.
   es_parallel-zz_cust_par_s = p_meth.
  ENDIF.
*CDP2 End


ENDFORM.                               " PARALLEL_FILL
*&---------------------------------------------------------------------*
*&      Form  LOCK_PLANT_BY_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*       Lock by plant and model
*----------------------------------------------------------------------*
FORM lock_plant_by_process
  USING
    pir_werks_range TYPE shp_werks_range_t
    piv_para_step   TYPE char1
  CHANGING
    pet_plant_model  TYPE zsapbf_tt_plant_model.

*--- local data declarations ------------------------------------------
  DATA:
*    lt_plants_lock TYPE plants,
    lt_plant_model TYPE zsapbf_tt_plant_model,
    lt_msg         TYPE bapiret2_t,
    lv_tabix       TYPE sytabix.

  FIELD-SYMBOLS: <lfs_plant_model> TYPE zsapbf_s_plant_model.

*--- form body ---------------------------------------------------------
  FREE: lt_plant_model, pet_plant_model.

  EXIT. "HMMA - ANDY

* Get the relevant plant / model combinations
  CALL FUNCTION 'ZSAPBF_GET_PLANT_MODEL'
    EXPORTING
      it_plant_range = pir_werks_range
    IMPORTING
      et_plant_model = lt_plant_model
      et_msg         = lt_msg.

  IF lt_msg is INITIAL.

    LOOP AT lt_plant_model ASSIGNING <lfs_plant_model>.
      lv_tabix = sy-tabix.

      CALL FUNCTION 'ENQUEUE_EZSAPBF_LOCK_TA'
        EXPORTING
          mode_zsapbf_s_lock_ta = 'E' "'X'
          mandt                 = sy-mandt
          plant                 = <lfs_plant_model>-plant
          tran                  = piv_para_step
          _scope                = '1'
          _wait                 = ' '
        EXCEPTIONS
          foreign_lock          = 1
          system_failure        = 2
          OTHERS                = 3.
      IF sy-subrc = 0.
        APPEND <lfs_plant_model> TO pet_plant_model.
        DELETE lt_plant_model INDEX lv_tabix.
      ELSE.
*       Show comprehensive error dialog Box when called in dialogue.
        IF sy-batch IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                           DISPLAY LIKE 'E'.
          STOP.
        ELSE.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

      CLEAR lv_tabix.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " LOCK_PLANT_BY_PROCESS
*&---------------------------------------------------------------------*
*&      Form  UNLOCK_PLANT_BY_PROCESS
*&---------------------------------------------------------------------*
*       Unlock plant model
*----------------------------------------------------------------------*
FORM unlock_plant_by_process
  USING
    piv_para_step   TYPE char1
    pit_plant_model TYPE zsapbf_tt_plant_model.

*--- local data declarations ------------------------------------------
  FIELD-SYMBOLS: <lfs_plant_model> TYPE zsapbf_s_plant_model.


*--- form body ---------------------------------------------------------
  LOOP AT pit_plant_model ASSIGNING <lfs_plant_model>.

    CALL FUNCTION 'DEQUEUE_EZSAPBF_LOCK_TA'
      EXPORTING
        mode_zsapbf_s_lock_ta = 'E' "'X'
        mandt                 = sy-mandt
*        plant                 = <lfs_plants_lock>
        plant                 = <lfs_plant_model>-plant
        tran                  = piv_para_step
        x_plant               = ' '
        x_tran                = ' '
        _scope                = '3'
        _synchron             = ' '
        _collect              = ' '.

  ENDLOOP.

ENDFORM.                    " UNLOCK_PLANT_BY_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CHECK_CONFTIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--ET_CONFTIME  text
*----------------------------------------------------------------------*
FORM check_conftime
     CHANGING et_conftime_range TYPE zsapbf_tt_conftime_range.

  DATA:
    lv_conftime_start TYPE ppc_conftime,
    lv_conftime_end   TYPE ppc_conftime,
    lv_conftime_delay TYPE ppc_conftime,
    lv_number_delay   TYPE p LENGTH 8,
    lv_number_end     TYPE p LENGTH 8,
    lv_end_date TYPE zsapbf_ppc_enddate,
    lv_end_time TYPE zsapbf_ppc_endtime,
    rs_conftime TYPE zsapbf_s_conftime_range,
    lv_seconds  TYPE p LENGTH 16.

*--> "To" date should not be lesser than "From" date
  IF NOT p_dtend IS INITIAL.
    IF p_dtend < p_dtsta.
      MESSAGE e946(zsapbf_ppc0).
    ENDIF.
  ENDIF.

*--> when "To" date is same as "From" date,
* "To" time should not be lesser than "From" time
  IF ( p_dtend EQ p_dtsta ) AND
     ( p_tiend <  p_tista ).
    MESSAGE e947(zsapbf_ppc0).
  ENDIF.

*--> when "From" time is used, "From" date should not be empty
  IF NOT p_tista IS INITIAL.
    IF p_dtsta IS INITIAL.
      MESSAGE e948(zsapbf_ppc0).
    ENDIF.
  ENDIF.

*--> when "To" time is used, "To" date should not be empty
  IF NOT p_tiend IS INITIAL.
    IF p_dtend IS INITIAL.
      MESSAGE e949(zsapbf_ppc0).
    ENDIF.
  ENDIF.

*--> calculate timestamps
  IF NOT p_dtsta IS INITIAL.
    CONVERT DATE p_dtsta TIME p_tista
            INTO TIME STAMP lv_conftime_start
            TIME ZONE sy-zonlo.
  ENDIF.

* Set 'to time' = min(safty timeframe, creation timeframe)
  lv_seconds = sy-datlo * 86400 + sy-timlo - p_delay.
  lv_end_date = lv_seconds DIV 86400.
  lv_end_time = lv_seconds MOD 86400.
  CONVERT DATE lv_end_date TIME lv_end_time
    INTO TIME STAMP lv_conftime_delay TIME ZONE sy-zonlo.
  lv_number_delay = lv_conftime_delay.

  IF NOT p_dtend IS INITIAL.
    CONVERT DATE p_dtend TIME p_tiend
          INTO TIME STAMP lv_conftime_end
          TIME ZONE sy-zonlo.
    lv_number_end = lv_conftime_end.
    IF lv_number_end > lv_number_delay.
      lv_conftime_end = lv_conftime_delay.
    ENDIF.
  ELSE.
    lv_conftime_end = lv_conftime_delay.
  ENDIF.


*--> Fill Ranges table
  FREE et_conftime_range.
  rs_conftime-sign   = 'I'.
  rs_conftime-option = 'BT'.
  rs_conftime-low    = lv_conftime_start.
  rs_conftime-high   = lv_conftime_end.
  APPEND rs_conftime TO et_conftime_range.

ENDFORM.                    " CHECK_CONFTIME
