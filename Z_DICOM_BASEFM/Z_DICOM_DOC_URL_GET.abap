function z_dicom_doc_url_get.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR30
*"     VALUE(CONF_DOCTYPE) TYPE  CHAR20
*"     VALUE(CONF_SIGNATURE) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(ARCHIV_DOC_ID) TYPE  CHAR40
*"  EXPORTING
*"     VALUE(URL) TYPE  CHAR255
*"  TABLES
*"      ZURL_ROWS STRUCTURE  ZURL_ROW OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"----------------------------------------------------------------------
  data:
      lv_url(4096).

* The following unused variable remain here to maintain the interface
* compability
  clear url.
  clear conf_doctype.

  call function 'SCMS_URL_GENERATE'
    exporting
      command         = 'info'
      contrep         = conf_archiv_id
      docid           = archiv_doc_id
      accessmode      = 'c'
      signature       = conf_signature
      docprot         = 'crud'
      security        = ''
    importing
      absolute_uri    = lv_url
    exceptions
      error_parameter = 1
      error_signature = 2
      others          = 3.

  if sy-subrc <> 0.
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
