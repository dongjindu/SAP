function z_dicom_create_wi_ex.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(CONF_ARCHIV_ID) TYPE  CHAR2 OPTIONAL
*"     REFERENCE(CONF_DOC_TYPE) TYPE  CHAR20 DEFAULT 'FAX'
*"     REFERENCE(CONF_AR_OBJECT) TYPE  CHAR10 OPTIONAL
*"     REFERENCE(CONF_TASKID) TYPE  CHAR14 OPTIONAL
*"     REFERENCE(CONF_DOC_AVAILABLE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(ARCHIV_DOC_ID) TYPE  CHAR40 OPTIONAL
*"     REFERENCE(TA_CODE) TYPE  CHAR4 OPTIONAL
*"  TABLES
*"      DOCDATA STRUCTURE  OARFCDATA OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  if conf_doc_available = 'X'.
*   Create workitem with image
    call function 'Z_DICOM_WORKITEM_IMAGE_GEN'
      exporting
        archiv_id     = conf_archiv_id
        document_type = conf_doc_type
        archiv_obj    = conf_ar_object
        archiv_doc_id = archiv_doc_id
      tables
        document_data = docdata
      exceptions
        general_error = 1
        others        = 2.
  else.
*   Create workitem without image
    call function 'Z_DICOM_WORKITEM_NOIMAGE_GEN'
      exporting
        task_id         = conf_taskid
        transactioncode = ta_code
      tables
        document_data   = docdata
      exceptions
        general_error   = 1
        others          = 2.
  endif.

* Check for error
  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

endfunction.
