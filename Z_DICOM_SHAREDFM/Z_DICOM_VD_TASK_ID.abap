function z_dicom_vd_task_id.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_TASKID) TYPE  CHAR14
*"  EXPORTING
*"     VALUE(RETURN) TYPE  BAPIRETURN1
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------
  data:
      lv_taskid      like rhobjects-object,
      error_msg like sy-msgv1.

  clear:
    return,
    error_msg.

  if conf_taskid is initial.
    error_msg = 'Task ID is empty'.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = error_msg.
    raise general_error.
  else.
    lv_taskid = conf_taskid.
    call function 'RH_GET_TASK_ATTRIBUTES'
      exporting
        act_object_ext       = lv_taskid
      exceptions
        no_active_plvar      = 1
        no_org_object        = 2
        org_object_not_found = 3
        no_task_type         = 4
        task_group           = 5
        others               = 6.

    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.
  endif.





endfunction.
