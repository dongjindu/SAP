function z_dicom_readwidocdata.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(WORKITEM_ID) LIKE  SWWWIHEAD-WI_ID
*"  TABLES
*"      DOCDATA STRUCTURE  OARFCDATA
*"  EXCEPTIONS
*"      NOT_FOUND
*"--------------------------------------------------------------------
  data:
      wa           type oarfcdata,
      count_index  type i,
      wi_container type table of swr_cont with header line.

  call function 'SAP_WAPI_READ_CONTAINER'
    exporting
      workitem_id            = workitem_id
    tables
      simple_container       = wi_container.

  refresh docdata.
  count_index = 0.

  loop at wi_container.
    if wi_container-element = zcontain_name.
      case count_index.
        when 0.
          wa-name = wi_container-value.
          count_index = 1.
        when 1.
          wa-wert = wi_container-value.
          append wa to docdata.
          count_index = 0.
      endcase.
    endif.
  endloop.

  if sy-subrc   <> 0 or
     docdata[]  is initial.
    raise not_found.
  endif.

endfunction.
