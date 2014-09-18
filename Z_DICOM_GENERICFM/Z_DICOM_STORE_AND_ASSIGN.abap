function z_dicom_store_and_assign.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2 OPTIONAL
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20 OPTIONAL
*"     VALUE(CONF_AR_OBJECT) TYPE  CHAR10
*"     VALUE(CONF_DOC_AVAILABLE) TYPE  CHAR1 OPTIONAL
*"     VALUE(ARCHIV_DOC_ID) TYPE  CHAR40 OPTIONAL
*"     VALUE(SAP_OBJECTID_PART1) TYPE  CHAR50
*"     VALUE(SAP_OBJECTID_PART2) TYPE  CHAR50 OPTIONAL
*"     VALUE(SAP_OBJECTID_PART3) TYPE  CHAR50 OPTIONAL
*"     VALUE(SAP_OBJECTID_PART4) TYPE  CHAR50 OPTIONAL
*"     VALUE(SAP_OBJECTID_PART5) TYPE  CHAR50 OPTIONAL
*"     VALUE(SAP_OBJECTID_PART6) TYPE  CHAR50 OPTIONAL
*"     VALUE(SAP_OBJECTID_PART7) TYPE  CHAR50 OPTIONAL
*"     VALUE(SAP_OBJECTID_PART8) TYPE  CHAR50 OPTIONAL
*"     VALUE(SAP_OBJECTID_PART9) TYPE  CHAR50 OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"      NO_IMAGE_AVAILABLE
*"      ARCHIVELINK_ERROR
*"      SAP_OBJECTID_INVALID
*"----------------------------------------------------------------------

  if conf_doc_available <> 'X'.
    raise no_image_available.
  endif.



*  With image, check archive link settings
  data lv_sap_object like toaom-sap_object.
  call function 'Z_DICOM_VD_ARCHIVELINK_INFO'
    exporting
      conf_archiv_id = conf_archiv_id
      conf_ar_object = conf_ar_object
      conf_doc_type  = conf_doc_type
    importing
      sap_object     = lv_sap_object
    exceptions
      general_error  = 1
      others         = 2.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.


  data sap_objectid type char50.
  concatenate sap_objectid_part1 sap_objectid_part2 sap_objectid_part3
      sap_objectid_part4 sap_objectid_part5 sap_objectid_part6
      sap_objectid_part7 sap_objectid_part8 sap_objectid_part9
      into sap_objectid.

  if sy-subrc <> 0.
    raise sap_objectid_invalid.
  endif.


  call function 'ARCHIV_CONNECTION_INSERT'
    exporting
      archiv_id             = conf_archiv_id
      arc_doc_id            = archiv_doc_id
      ar_object             = conf_ar_object
      object_id             = sap_objectid
      sap_object            = lv_sap_object
      doc_type              = conf_doc_type
    exceptions
      error_connectiontable = 1
      others                = 2.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = 'ERROR_CONNECTIONTABLE'.
    raise archivelink_error.
  endif.

endfunction.
