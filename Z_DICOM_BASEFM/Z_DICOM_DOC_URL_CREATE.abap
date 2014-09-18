function z_dicom_doc_url_create.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR30
*"     VALUE(CONF_SIGNATURE) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(CONF_DOCTYPE) TYPE  CHAR20
*"  EXPORTING
*"     VALUE(ARCHIV_DOC_ID) TYPE  CHAR40
*"     VALUE(MIMETYPE) TYPE  CHAR128
*"  TABLES
*"      ZURL_ROWS STRUCTURE  ZURL_ROW
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------
  data:
      lv_url(4096).

* Get the MIME type
  call function 'Z_DICOM_MIMETYPE_GET'
    exporting
      doc_class     = conf_doctype
    importing
      data_mimetype = mimetype
    exceptions
      general_error = 1
      others        = 2.
  if sy-subrc <> 0.
    clear
      archiv_doc_id.

    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

  call function 'SCMS_DOC_URL_CREATE'
    exporting
      stor_cat           = ' '
      crep_id            = conf_archiv_id
      comp_id            = 'data'
      doc_prot           = 'crud'
      signature          = conf_signature
      http_url_only      = 'X'
    importing
      url                = lv_url
      doc_id_out         = archiv_doc_id
    exceptions
      dp_not_supported   = 1
      http_not_supported = 2
      tree_not_supported = 3
      not_supported      = 4
      error_signature    = 5
      error_parameter    = 6
      error_config       = 7
      error              = 8
      others             = 9.
  if sy-subrc <> 0.
    clear
      archiv_doc_id.

    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  else.

    clear zurl_rows.
    refresh zurl_rows.

    zurl_rows = lv_url+0(255).
    append zurl_rows.

    zurl_rows = lv_url+255(255).
    append zurl_rows.

    zurl_rows = lv_url+510(255).
    append zurl_rows.

    zurl_rows = lv_url+765(255).
    append zurl_rows.

    zurl_rows = lv_url+1020(255).
    append zurl_rows.

    zurl_rows = lv_url+1275(255).
    append zurl_rows.

    zurl_rows = lv_url+1530(255).
    append zurl_rows.

    zurl_rows = lv_url+1785(255).
    append zurl_rows.

    zurl_rows = lv_url+2040(255).
    append zurl_rows.

    zurl_rows = lv_url+2295(255).
    append zurl_rows.

    zurl_rows = lv_url+2550(255).
    append zurl_rows.

    zurl_rows = lv_url+2805(255).
    append zurl_rows.

    zurl_rows = lv_url+3060(255).
    append zurl_rows.

    zurl_rows = lv_url+3315(255).
    append zurl_rows.

    zurl_rows = lv_url+3570(255).
    append zurl_rows.

    zurl_rows = lv_url+3825(255).
    append zurl_rows.

    zurl_rows = lv_url+4080(10).
    append zurl_rows.

  endif.





endfunction.
