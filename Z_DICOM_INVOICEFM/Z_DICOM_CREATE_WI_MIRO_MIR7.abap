function z_dicom_create_wi_miro_mir7.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2 OPTIONAL
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20 OPTIONAL
*"     VALUE(CONF_AR_OBJECT) TYPE  CHAR10 OPTIONAL
*"     VALUE(CONF_TASKID) TYPE  CHAR14 OPTIONAL
*"     VALUE(CONF_DOC_AVAILABLE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(ARCHIV_DOC_ID) TYPE  CHAR40 OPTIONAL
*"     VALUE(TRANSACTIONCODE) TYPE  CHAR4 OPTIONAL
*"  TABLES
*"      DOCUMENT_DATA STRUCTURE  OARFCDATA OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  data:
    return          type bapireturn1,
    lv_sap_object   like toaom-sap_object.

  if conf_doc_available = 'X'.
*   Image release enabled - check archive link settings
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

    if sy-subrc ne 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.
  else.
*   Image release disabled - validate Task ID.
    call function 'Z_DICOM_VD_TASK_ID'
      exporting
        conf_taskid   = conf_taskid
      importing
        return        = return
      exceptions
        general_error = 1
        others        = 2.

    if sy-subrc ne 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.
  endif.

  if archiv_doc_id ne space.
*     ArchiveDocID not empty - create workitem with image
    call function 'Z_DICOM_WORKITEM_IMAGE_GEN'
      exporting
        archiv_id     = conf_archiv_id
        document_type = conf_doc_type
        archiv_obj    = conf_ar_object
        archiv_doc_id = archiv_doc_id
      tables
        document_data = document_data
      exceptions
        general_error = 1
        others        = 2.
  else.
*     ArchiveDocID is empty - create workitem without image
    call function 'Z_DICOM_WORKITEM_NOIMAGE_GEN'
      exporting
        task_id         = conf_taskid
        transactioncode = transactioncode
      tables
        document_data   = document_data
      exceptions
        general_error   = 1
        others          = 2.
  endif.

* Raise error if failed to create workitem
  if sy-subrc ne 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

endfunction.
