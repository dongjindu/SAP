function z_dicom_changecompcode.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMP_ID) TYPE  BUKRS
*"--------------------------------------------------------------------
  data:
      w_bukrs type bukrs.

  data:
    bdcdata type table of bdcdata with header line,
    status_msg  type table of bdcmsgcoll with header line.


  get parameter id 'BUK' field w_bukrs.

  if w_bukrs <> comp_id.
    bdcdata-program  = 'SAPMF05A'.
    bdcdata-dynpro   = '1100'.
    bdcdata-dynbegin = 'X'.
    insert table bdcdata.

    clear bdcdata.
    bdcdata-fnam     = 'BDC_OKCODE'.
    bdcdata-fval     = '/ECCDE'.
    insert table bdcdata.

    clear bdcdata.
    bdcdata-program  = 'SAPLACHD'.
    bdcdata-dynpro   = '1000'.
    bdcdata-dynbegin = 'X'.
    insert table bdcdata.

    clear bdcdata.
    bdcdata-fnam     = 'BKPF-BUKRS'.
    bdcdata-fval     = comp_id.
    insert table bdcdata.

    call transaction 'FB60'
      using bdcdata
      messages into status_msg
      mode 'N'.
  endif.

endfunction.
