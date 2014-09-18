function z_dicom_post_miro_image_link.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ARCHIV_ID) LIKE  TOAV0-ARCHIV_ID OPTIONAL
*"     VALUE(ARC_DOC_ID) LIKE  TOAV0-ARC_DOC_ID OPTIONAL
*"     VALUE(AR_OBJECT) LIKE  TOAOM-AR_OBJECT OPTIONAL
*"     VALUE(DOC_TYPE) LIKE  TOADV-DOC_TYPE OPTIONAL
*"     VALUE(INVOICEDOCNUMBER) TYPE  CHAR10 OPTIONAL
*"     VALUE(FISCALYEAR) TYPE  BAPI_INCINV_FLD-FISC_YEAR OPTIONAL
*"  CHANGING
*"     REFERENCE(RETURN1) TYPE  BAPIRETURN1
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  data:
    object_id type char50.

* Perform linkage only if there is ArchiveDocID
  if arc_doc_id ne space.
*   Populate ObjectID
    concatenate invoicedocnumber fiscalyear into object_id.

*   Perform image link
    call function 'ARCHIV_CONNECTION_INSERT'
      exporting
        archiv_id             = archiv_id
        arc_doc_id            = arc_doc_id
        ar_object             = ar_object
        object_id             = object_id
        sap_object            = 'BUS2081'
        doc_type              = doc_type
      exceptions
        error_connectiontable = 1
        others                = 2.

*   Check for error
    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = 'ERROR_CONNECTIONTABLE'.
      raise general_error.
    endif.
  endif.

endfunction.
