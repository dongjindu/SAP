function z_dicom_show_workitem_generic.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(TCODE) LIKE  T020-TCODE
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"----------------------------------------------------------------------
  data:
    docdata         type table  of oarfcdata,
    workitem_id     type swwwihead-wi_id,
    it_requester    type table of swotobjid with header line,
    bdcdata         type table of bdcdata    with header line,
    it_msg          type table of bdcmsgcoll with header line.

* Retrieve workitem info
  call function 'SWO_QUERY_REQUESTER'
    tables
      requester = it_requester
    exceptions
      not_found = 1
      others    = 2.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

* Get workitem ID
  read table it_requester with key objtype = 'WORKITEM'.
  workitem_id = it_requester-objkey.

* Retrieve document data from container
  call function 'Z_DICOM_READWIDOCDATA'
    exporting
      workitem_id = workitem_id
    tables
      docdata     = docdata
    exceptions
      not_found   = 1
      others      = 2.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

* Convert DocumentData to BDCData
  call function 'Z_DICOM_DOC2BDCDATA'
    tables
      docdata = docdata
      bdcdata = bdcdata.

* Call Transaction.
  refresh it_msg.
  call transaction tcode using bdcdata messages into it_msg mode 'E'.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

endfunction.
