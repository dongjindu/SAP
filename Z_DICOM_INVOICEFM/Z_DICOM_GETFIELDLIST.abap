function z_dicom_getfieldlist.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(NAME) TYPE  CHAR255
*"  EXPORTING
*"     VALUE(KEYLIST) TYPE  CHAR1024
*"  TABLES
*"      DOCUMENT_DATA STRUCTURE  OARFCDATA
*"  EXCEPTIONS
*"      NOT_FOUND
*"--------------------------------------------------------------------
  data:
      lenkeylist type i,
      len        type i.

  lenkeylist = 0.
  loop at document_data.
    if document_data-name = name.
      len = strlen( document_data-wert ) + lenkeylist.

      if len < 1024.
        if lenkeylist = 0.
          concatenate keylist document_data-wert into keylist.
        else.
          concatenate keylist document_data-wert+1 into keylist.
        endif.
        lenkeylist =  strlen( keylist ).
      endif.
    else.
      if name = 'HEADER' and document_data-name(7) = 'HEADER-'.
        len = strlen( document_data-wert ) + lenkeylist.
        if len < 1024.
          if lenkeylist = 0.
            concatenate keylist document_data-wert into keylist.
          else.
            concatenate keylist document_data-wert+1 into keylist.
          endif.
          lenkeylist =  strlen( keylist ).
        endif.
      endif.
    endif.
  endloop.

  if lenkeylist = 0.
    raise not_found.
  endif.





endfunction.
