function z_dicom_show_workitem_basic.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(TCODE) LIKE  T020-TCODE
*"     REFERENCE(BUS_OBJECT_NAME) TYPE  SAEANWDID
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------
  data:
    sap_objectid    type saeobjid,
    docdata         type table  of oarfcdata,
    workitem_id     type swwwihead-wi_id,
    it_requester    type table of swotobjid with header line,
    errmsg          like sy-msgv1.

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
      workitem_id   = workitem_id
    tables
      docdata       = docdata
    exceptions
      not_found     = 1
      others        = 2.

  if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
  endif.

* Fill transaction screen
  case tcode.
    when 'FB60'.
*     Transaction FB60
      call function 'Z_DICOM_CALL_FB60'
        importing
          sap_objectid  = sap_objectid
        tables
          document_data = docdata.

    when 'FV60' or 'FV65' or 'FB65'.
*     Transaction FV60, FV65, FB65
      call function 'Z_DICOM_CALL_FV6X'
        exporting
          transcode     = tcode
        importing
          sap_objectid  = sap_objectid
        tables
          document_data = docdata.

    when 'MIRO'.
*     Transaction MIRO
      call function 'Z_DICOM_CALL_MIRO'
        importing
          sap_objectid  = sap_objectid
        tables
          document_data = docdata.

    when 'MIR7'.
*     Transaction MIR7
      call function 'Z_DICOM_CALL_MIRX'
        exporting
          transcode     = tcode
        importing
          sap_objectid  = sap_objectid
        tables
          document_data = docdata.

    when others.
*     Other transactions not supported
      concatenate 'transactioncode=' tcode ' not supported' into errmsg.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = errmsg.
      raise general_error.
  endcase.

endfunction.
