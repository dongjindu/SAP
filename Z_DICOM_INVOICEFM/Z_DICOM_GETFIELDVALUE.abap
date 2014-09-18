function z_dicom_getfieldvalue.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FELDLIST) TYPE  CHAR1024
*"     VALUE(FIELDNAME) TYPE  CHAR255
*"     REFERENCE(INDEX) TYPE  I DEFAULT 0
*"  EXPORTING
*"     VALUE(FIELDVALUE) TYPE  CHAR255
*"  EXCEPTIONS
*"      NOT_FOUND
*"--------------------------------------------------------------------
  data:
      searchstr type char255,
      separator type char1,
      startpos  type i,
      strl      type i,
      ncounter  type i.

  compute ncounter = index + 1.

  move feldlist(1) to separator.
  concatenate separator fieldname '=' into searchstr.

  do ncounter times.

    if feldlist cs searchstr.
      strl = strlen( searchstr ).

      startpos = sy-fdpos + strl.
      shift feldlist by startpos places.

      move '' to fieldvalue.
      move feldlist to fieldvalue.

      if feldlist cs separator.
        if sy-fdpos > 0.
          move feldlist to fieldvalue(sy-fdpos).
          shift feldlist by sy-fdpos places.
        endif.
      endif.
    else.
      raise not_found.
    endif.
  enddo.

endfunction.
