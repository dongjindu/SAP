function z_dicom_store_using_barcode.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20
*"     VALUE(BARCODEIN) TYPE  CHAR40
*"     VALUE(CONF_DOC_AVAILABLE) TYPE  CHAR1 OPTIONAL
*"     VALUE(ARCHIV_DOC_ID) TYPE  CHAR40 OPTIONAL
*"     VALUE(SKIP_BARCODE_CHECK) TYPE  CHAR1 OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  data:
    lv_bapiret2   type bapiret2,
    it_bapibarc   type bapibarc occurs 0 with header line,
    return        type bapireturn1,
    error_msg     like sy-msgv1.

* Validate Archive Link Info
  call function 'Z_DICOM_VD_ARCHIVELINK_INFO'
    exporting
      conf_archiv_id = conf_archiv_id
      conf_ar_object = ''
      conf_doc_type  = conf_doc_type
      skip_ar_object = 'X'
    exceptions
      general_error  = 1
      others         = 2.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

* Validate Barcode
  call function 'Z_DICOM_VALIDATE_BARCODE'
    exporting
      p_barcodein          = barcodein
      p_skip_barcode_check = skip_barcode_check
    changing
      p_return             = return
    exceptions
      general_error        = 1
      others               = 2.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

* Validate Archive Document ID
  if archiv_doc_id eq space.
    clear error_msg.
    error_msg = 'Archive document ID is empty'.

    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = error_msg.
    raise general_error.
  endif.

  clear it_bapibarc.
  it_bapibarc-barcode = barcodein.
  it_bapibarc-contrep = conf_archiv_id.
  it_bapibarc-docid   = archiv_doc_id.
  it_bapibarc-ardate  = sy-datum.
  it_bapibarc-doctype = conf_doc_type.
  append it_bapibarc.

* Write Linkage into table for Barcode and ArchivLink Object
  call function 'BAPI_BARCODE_SENDLIST'
    importing
      return       = lv_bapiret2
    tables
      barcodetable = it_bapibarc.

  move-corresponding lv_bapiret2 to return.

  if return-type eq 'E'.
    sy-msgid = return-id.
    sy-msgty = return-type.
    sy-msgno = return-number.
    sy-msgv1 = return-message_v1.
    sy-msgv2 = return-message_v2.
    sy-msgv3 = return-message_v3.
    sy-msgv4 = return-message_v4.

    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = error_msg.
    raise general_error.
  endif.

endfunction.
