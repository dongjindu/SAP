FUNCTION ZIDOC_START_INBOUND.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(PI_INBOUND_PROCESS_DATA) LIKE  TEDE2 STRUCTURE  TEDE2
*"       DEFAULT SPACE
*"     VALUE(PI_CALLED_ONLINE) LIKE  SWWCOMMIT-DIALOGFLAG DEFAULT SPACE
*"     VALUE(PI_DO_COMMIT) LIKE  EDIGENERAL-DO_COMMIT DEFAULT 'X'
*"     VALUE(PI_START_EVENT_ENABLED) LIKE  EDIGENERAL-STARTEVENT
*"       DEFAULT 'X'
*"     VALUE(PI_ORG_UNIT) LIKE  SWHACTOR STRUCTURE  SWHACTOR DEFAULT
*"       SPACE
*"     VALUE(SUCC_SHOW_FLAG) LIKE  EDIGENERAL-SUCC_FLAG DEFAULT ' '
*"  TABLES
*"      T_CONTROL_RECORDS STRUCTURE  EDIDC
*"      T_DATA_RECORDS STRUCTURE  EDIDD OPTIONAL
*"      T_IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"  EXCEPTIONS
*"      INVALID_DOCUMENT_NUMBER
*"      ERROR_BEFORE_CALL_APPLICATION
*"      INBOUND_PROCESS_NOT_POSSIBLE
*"      OLD_WF_START_FAILED
*"      WF_TASK_ERROR
*"      SERIOUS_INBOUND_ERROR
*"----------------------------------------------------------------------
* local variables
  DATA:
    l_t_unprocessed_idocs       LIKE edidc-docnum
                                OCCURS 0 WITH HEADER LINE,
    l_t_status_records          LIKE edi_ds
                                OCCURS 0 WITH HEADER LINE,
    l_status_message            LIKE edids,
    l_partner_data              LIKE edp21,
    l_workitem_create_error     TYPE tidoc_bool,
    l_error_occurred            TYPE tidoc_bool.

  LOOP AT t_control_records.
* 1. test the actual status, only some status values are possible for
*    starting inbound process
* 2. return idoc control record if necessary
    PERFORM idoc_status_check
                           USING
                              t_control_records.
* modify is necessary for the case that only the idoc number was filled
    MODIFY t_control_records.

  ENDLOOP.

* IF NOT pi_inbound_process_data IS REQUESTED.
  IF pi_inbound_process_data IS INITIAL.
* need to get inbound process data first
* get first control record
    READ TABLE t_control_records INDEX 1.
* get inbound process data
    CALL FUNCTION 'IDOC_INBOUND_PROCESS_DATA_GET'
      EXPORTING
        pi_control_record       = t_control_records
      IMPORTING
        pe_inbound_process_data = pi_inbound_process_data
      EXCEPTIONS
        partner_not_usuable     = 1
        event_code_missing      = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      PERFORM status_message_fill  USING  l_status_message
                                          sy-msgid
                                          sy-msgty
                                          sy-msgno
                                          sy-msgv1
                                          sy-msgv2
                                          sy-msgv3
                                          sy-msgv4.
* idocs cannot be processed
      LOOP AT t_control_records.
* write idoc status
        PERFORM idoc_status_error_start_inb
                                         USING
                                            t_control_records-docnum
                                            l_status_message.
      ENDLOOP.

* standard error handling: outside
      MESSAGE ID      l_status_message-stamid
              TYPE    l_status_message-statyp
              NUMBER  l_status_message-stamno
              WITH    l_status_message-stapa1 l_status_message-stapa2
                      l_status_message-stapa3 l_status_message-stapa4
              RAISING error_before_call_application.

    ENDIF.
  ENDIF.

* use processing data to decide what to do
  CASE pi_inbound_process_data-edivr2.

    WHEN '1'                           "    ALE / multi step task
      OR '2'                           "    ALE / single step task
      OR '3'                           " no ALE / multi step task
      OR '4'.                          " no ALE / single step task
* provide organisational unit if necessary
      IF pi_org_unit IS INITIAL.
        pi_org_unit-otype = 'US'.
        pi_org_unit-objid = sy-uname.
      ENDIF.
* sap business workflow: single step or multi step task
      LOOP AT t_control_records.
* pass all of the idocs into the application
        CALL FUNCTION 'IDOC_WORKITEM_INBOUND_CREATE'
          EXPORTING
            pi_task_number   = pi_inbound_process_data-evenid
            pi_idoc_number   = t_control_records-docnum
            pi_called_online = pi_called_online
            pi_org_unit      = pi_org_unit
            succ_show_flag   = succ_show_flag
          EXCEPTIONS
            wf_task_error    = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
* workitem could not be created/started
          l_workitem_create_error = c_true.
* save system information
          PERFORM status_message_fill  USING  l_status_message
                                              sy-msgid
                                              sy-msgty
                                              sy-msgno
                                              sy-msgv1
                                              sy-msgv2
                                              sy-msgv3
                                              sy-msgv4.
* exit from loop
          EXIT.
        ENDIF.
      ENDLOOP.

* perform error handling, i.e. set status in case of workitem error
      IF l_workitem_create_error = c_true.
        LOOP AT t_control_records.
* set status 63 -> error
          PERFORM idoc_status_error_start_inb
                                           USING
                                              t_control_records-docnum
                                              l_status_message.
        ENDLOOP.
* tell user of function, standard error handling: outside
        MESSAGE ID      l_status_message-stamid
                TYPE    l_status_message-statyp
                NUMBER  l_status_message-stamno
                WITH    l_status_message-stapa1 l_status_message-stapa2
                        l_status_message-stapa3 l_status_message-stapa4
                RAISING wf_task_error.
      ENDIF.

    WHEN '6'                       "    ALE / function module
      OR '8'.
* unfortunately we need to move the idoc numbers into a second table
      LOOP AT t_control_records.
        l_t_unprocessed_idocs = t_control_records-docnum.
        APPEND l_t_unprocessed_idocs.
      ENDLOOP.

* pass idocs into application
*     call ALE function 'IDOC_INPUT'
* nur für BECK-test
*      if sy-uname eq 'XMB_USER'.
      CALL FUNCTION 'IDOC_INPUT'
           EXPORTING
             mass_processing          = c_true
             direct_call              = c_true
             end_event_enabled        = c_true
             idoc_start_event_enabled = pi_start_event_enabled
             do_commit                = pi_do_commit
             process_code             = pi_inbound_process_data-evcode
           TABLES
             unprocessed_idocs        = l_t_unprocessed_idocs
             idoc_data                = t_data_records
             idoc_control             = t_control_records
           EXCEPTIONS
             idoc_open_lock           = 1
             idoc_open_not_exist      = 1
             idoc_open_invalid        = 2
             idoc_open_already        = 2
             idoc_write_number_invalid = 2
             idoc_write_status_invalid = 1
             idoc_write_no_status = 2
             idoc_write_lock = 1
             idoc_write_db_error = 2
             idoc_close_not_open = 2
             idoc_close_db_error = 2
             idoc_close_parameter_error = 2
             idoc_close_no_status_written = 1
             idocs_do_not_exist = 2
             idocs_have_no_data_records = 2
             unprocessed_idocs_empty = 2
             OTHERS                   = 2.
      IF sy-subrc = 2. "don't take care of all others
* just raise exception, do not set status
        MESSAGE ID      sy-msgid
                TYPE    sy-msgty
                NUMBER  sy-msgno
                WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING serious_inbound_error.
      ENDIF.
*      else.
*      "beck - später zu löschen
***      CALL FUNCTION 'ZIDOC_INPUT'
***           EXPORTING
***             mass_processing          = c_true
***             direct_call              = c_true
***             end_event_enabled        = c_true
***             idoc_start_event_enabled = pi_start_event_enabled
***             do_commit                = pi_do_commit
***             process_code             =
* pi_inbound_process_data-evcode
***           TABLES
***             unprocessed_idocs        = l_t_unprocessed_idocs
***             idoc_data                = t_data_records
***             idoc_control             = t_control_records
***             T_IDOC_STATUS            = T_IDOC_STATUS
***           EXCEPTIONS
***             idoc_open_lock           = 1
***             idoc_open_not_exist      = 1
***             idoc_open_invalid        = 2
***             idoc_open_already        = 2
***             idoc_write_number_invalid = 2
***             idoc_write_status_invalid = 1
***             idoc_write_no_status = 2
***             idoc_write_lock = 1
***             idoc_write_db_error = 2
***             idoc_close_not_open = 2
***             idoc_close_db_error = 2
***             idoc_close_parameter_error = 2
***             idoc_close_no_status_written = 2
***             idocs_do_not_exist = 2
***             idocs_have_no_data_records = 2
***             unprocessed_idocs_empty = 2
***             OTHERS                   = 2.
***      IF sy-subrc = 2. "don't take care of all others
**** just raise exception, do not set status
***        MESSAGE ID      sy-msgid
***                TYPE    sy-msgty
***                NUMBER  sy-msgno
***                WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
***                RAISING serious_inbound_error.
***      ENDIF.
*     endif. " Beck - später zu löschen
    WHEN '5'                           " no ALE / old wf
      OR '7'.                          "    ALE / old wf
* get one of the control records (doesn't matter which one)
      READ TABLE t_control_records INDEX 1.
* get partner data in order to fill next processor  of old workflow
      PERFORM partner_data_get
                            USING
                               t_control_records
                               l_partner_data
                               l_status_message.

      IF l_status_message IS INITIAL.

        IF l_partner_data-usrtyp = 'US'. " must be a user, no orgtyp
* 2.1 WF
          LOOP AT t_control_records.
* set status so that idoc can be processed also from old wf
            l_t_status_records-docnum = t_control_records-docnum.
            l_t_status_records-logdat = sy-datum.
            l_t_status_records-logtim = sy-uzeit.
            l_t_status_records-repid  = sy-repid.
            l_t_status_records-uname  = sy-uname.
            l_t_status_records-status = '50'.
            l_t_status_records-statyp = 'I'.
            l_t_status_records-stamqu = 'SAP'.
            l_t_status_records-stamid = 'EA'.
            l_t_status_records-stamno = '705'.
            APPEND l_t_status_records.
* write status record 50
            PERFORM idoc_status_records_write
                                           TABLES
                                               l_t_status_records
                                           USING
                                               t_control_records-docnum.
* pass all of the idocs into the application
            PERFORM old_workflow_start
                                    USING
                                       t_control_records
                                       pi_inbound_process_data-evenid
                                       l_partner_data-usrkey.
          ENDLOOP.
        ELSE.
* idocs cannot be processed: userkey does not belong to a user
          PERFORM status_message_fill  USING  l_status_message
                                              'EA'
                                              'E'
                                              '707'
                                              space
                                              space
                                              space
                                              space.
* set error flag
          l_error_occurred = c_true.
        ENDIF.
      ELSE.
* set error flag, status message is filled by form routine
        l_error_occurred = c_true.
      ENDIF.

      IF l_error_occurred = c_true.
        LOOP AT t_control_records.
* write idoc status and start error handling
          PERFORM idoc_status_error_start_inb
                                           USING
                                              t_control_records-docnum
                                              l_status_message.
        ENDLOOP.

* partner data not usable or userkey does not belong to a user
        MESSAGE ID      l_status_message-stamid
                TYPE    l_status_message-statyp
                NUMBER  l_status_message-stamno
                WITH    l_status_message-stapa1
                        l_status_message-stapa2
                        l_status_message-stapa3
                        l_status_message-stapa4
                RAISING error_before_call_application.
      ENDIF.
  ENDCASE.

ENDFUNCTION.
