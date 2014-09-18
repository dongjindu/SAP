function z_dicom_get_field_info.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FIELDNAME) TYPE  CHAR30
*"     VALUE(TABNAME) TYPE  CHAR30
*"  EXPORTING
*"     VALUE(FIELD_DESC) TYPE  CHAR255
*"     VALUE(FIELD_TYPE) TYPE  CHAR10
*"     VALUE(FIELD_LENGTH) TYPE  CHAR10
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------
  data:tb_info type dfies occurs 1 with header line.

  call function 'DDIF_FIELDINFO_GET'
    exporting
      tabname        = tabname
      fieldname      = fieldname
    tables
      dfies_tab      = tb_info
    exceptions
      not_found      = 1
      internal_error = 2
      others         = 3.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg     = ''.

    raise not_found.
  endif.

  loop at tb_info.
    field_type = tb_info-datatype.
    field_length = tb_info-leng.
    field_desc = tb_info-fieldtext.
  endloop.

endfunction.
