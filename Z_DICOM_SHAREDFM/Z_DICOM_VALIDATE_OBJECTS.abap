function z_dicom_validate_objects.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2 OPTIONAL
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20 OPTIONAL
*"     VALUE(CONF_AR_OBJECT) TYPE  CHAR10 OPTIONAL
*"     VALUE(CONF_TASKID) TYPE  CHAR14 OPTIONAL
*"     VALUE(CONF_DOC_AVAILABLE) TYPE  CHAR1 OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------
  data:
      return          type bapireturn1,
      lv_sap_object   like toaom-sap_object.

  if conf_doc_available = 'X'.
*     With image, check archive link settings
    call function 'Z_DICOM_VD_ARCHIVELINK_INFO'
      exporting
        conf_archiv_id = conf_archiv_id
        conf_ar_object = conf_ar_object
        conf_doc_type  = conf_doc_type
      importing
        sap_object     = lv_sap_object
*        return         = return
      exceptions
        general_error  = 1
        others         = 2.

    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.

  else.
*     Without image, validate Task ID
    call function 'Z_DICOM_VD_TASK_ID'
      exporting
        conf_taskid   = conf_taskid
      importing
        return        = return
      exceptions
        general_error = 1
        others        = 2.

    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.
  endif.





endfunction.
