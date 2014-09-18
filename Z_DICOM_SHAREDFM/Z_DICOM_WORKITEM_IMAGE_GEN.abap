function z_dicom_workitem_image_gen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ARCHIV_ID) LIKE  TOAAR-ARCHIV_ID DEFAULT 'A3'
*"     VALUE(DOCUMENT_TYPE) LIKE  TOADD-DOC_TYPE DEFAULT 'FAX'
*"     VALUE(ARCHIV_OBJ) LIKE  TOAOM-AR_OBJECT DEFAULT 'ZDMSI_FB60'
*"     VALUE(ARCHIV_DOC_ID) TYPE  CHAR40
*"  TABLES
*"      DOCUMENT_DATA STRUCTURE  OARFCDATA
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"----------------------------------------------------------------------

  data:
      document_entry      type oarfcin       occurs 1 with header line,
      document_data2      type oarfcdata     occurs 0 with header line.

  move archiv_id     to document_entry-archiv_id.
  move archiv_doc_id to document_entry-arc_doc_id.
  move archiv_obj    to document_entry-ar_object.
  move document_type to document_entry-doc_type.

  if not document_data[] is initial.
    document_data2[] = document_data[].
    clear:
      document_data[],
      document_data.

    loop at document_data2.
      clear document_data.
      document_data-name = zcontain_name.
      document_data-wert = document_data2-name.
      append document_data.

      clear document_data.
      document_data-name = zcontain_name.
      document_data-wert = document_data2-wert.
      append document_data.
    endloop.
  endif.

* Create Work Item
  call function 'ARCHIV_PROCESS_RFCINPUT'
    exporting
      document_entry      = document_entry
    tables
      document_data       = document_data
    exceptions
      no_authorization    = 1
      no_workitem_created = 2
      error_parameter     = 3
      others              = 4.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

endfunction.
