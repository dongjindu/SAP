function z_dicom_mimetype_get.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DOC_CLASS) TYPE  CHAR20
*"  EXPORTING
*"     VALUE(DATA_MIMETYPE) TYPE  CHAR128
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"----------------------------------------------------------------------
  data:
      error_msg like sy-msgv1.

* Check is required fields are missing
  if doc_class eq space.
    clear error_msg.
    error_msg = 'Document Type is missing.'.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = error_msg.
    raise general_error.
  endif.

* Determine MIMEType from table TOADD
  select single mimetype into data_mimetype from toadd
         where doc_type eq doc_class.

* Return error if no entry found
  if data_mimetype eq space.
    concatenate 'Document type ' doc_class ' not found.'
    into error_msg.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = error_msg.
    raise general_error.
  endif.





endfunction.
