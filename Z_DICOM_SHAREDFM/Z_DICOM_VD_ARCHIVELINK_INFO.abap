function z_dicom_vd_archivelink_info.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2
*"     VALUE(CONF_AR_OBJECT) TYPE  CHAR10
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20
*"     VALUE(SKIP_AR_OBJECT) TYPE  CHAR1 DEFAULT ''
*"  EXPORTING
*"     VALUE(SAP_OBJECT) LIKE  TOAOM-SAP_OBJECT
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"----------------------------------------------------------------------
  data:
      error_msg like sy-msgv1.

* Check is required fields are missing
  if conf_archiv_id eq space.
    clear error_msg.
    error_msg = 'Archive ID is missing'.

    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = error_msg.
    raise general_error.
  endif.

  if skip_ar_object eq space.
    if conf_ar_object eq space.
      clear error_msg.
      error_msg = 'Archive Object is missing'.

      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = error_msg.
      raise general_error.
    endif.
  endif.

  if conf_doc_type eq space.
    clear error_msg.
    error_msg = 'Document Type is missing'.

    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = error_msg.
    raise general_error.
  endif.

* Determine SAP_OBJECT from table TOAOM
  if conf_ar_object eq space.
    select single sap_object into sap_object from toaom
           where archiv_id eq conf_archiv_id
           and doc_type  eq conf_doc_type
           and ar_status eq 'X'.
  else.
    select single sap_object into sap_object from toaom
           where archiv_id eq conf_archiv_id
           and ar_object eq conf_ar_object
           and doc_type  eq conf_doc_type
           and ar_status eq 'X'.
  endif.

* Return error if no entry found
  if sap_object eq space.
    if conf_ar_object eq space.
      concatenate 'Combination of' conf_archiv_id ',' conf_doc_type
                  'is invalid' into error_msg separated by space.
    else.
     concatenate 'Combination of' conf_archiv_id ',' conf_ar_object ','
                  conf_doc_type 'is invalid' into error_msg separated
                  by space.
    endif.

    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = error_msg.
    raise general_error.
  endif.

endfunction.
