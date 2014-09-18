function z_dicom_execute_bdc_failsafe.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2 OPTIONAL
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20 OPTIONAL
*"     VALUE(CONF_AR_OBJECT) TYPE  CHAR10
*"     VALUE(CONF_DOC_AVAILABLE) TYPE  CHAR1 OPTIONAL
*"     VALUE(ARCHIV_DOC_ID) TYPE  CHAR40 OPTIONAL
*"     VALUE(TRANSACTION_CODE) TYPE  CHAR4
*"     VALUE(SAP_OBJECTID_CONSTRUCTOR) TYPE  CHAR255
*"     VALUE(CONF_TASKID) TYPE  CHAR14 OPTIONAL
*"  TABLES
*"      BDCDATA STRUCTURE  ZBDCDATA OPTIONAL
*"      Z_KEYVALUE STRUCTURE  ZDICOM_KV OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"      TRANSACTION_EXECUTE_ERROR
*"      ARCHIVELINK_ERROR
*"----------------------------------------------------------------------


  call function 'Z_DICOM_EXECUTE_BDC'
    exporting
      conf_archiv_id            = conf_archiv_id
      conf_doc_type             = conf_doc_type
      conf_ar_object            = conf_ar_object
      conf_doc_available        = conf_doc_available
      archiv_doc_id             = archiv_doc_id
      transaction_code          = transaction_code
      sap_objectid_constructor  = sap_objectid_constructor
    tables
      bdcdata                   = bdcdata
      z_keyvalue                = z_keyvalue
    exceptions
      general_error             = 1
      transaction_execute_error = 2
      archivelink_error         = 3
      others                    = 4.
  if sy-subrc <> 0.

    call function 'Z_DICOM_CREATE_WORKITEM_EX'
      exporting
        conf_archiv_id     = conf_archiv_id
        conf_doc_type      = conf_doc_type
        conf_ar_object     = conf_ar_object
        conf_taskid        = conf_taskid
        conf_doc_available = conf_doc_available
        archiv_doc_id      = archiv_doc_id
        ta_code            = transaction_code
      tables
        bdcdata            = bdcdata
      exceptions
        general_error      = 1
        others             = 2.

    if sy-subrc <> 0.

      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = 'Failed to create workitem.'.

      raise general_error.

    endif.

  endif.

endfunction.
