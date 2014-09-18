function z_dicom_call_mirx.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DYNPROMODE) TYPE  CHAR1 DEFAULT 'E'
*"     VALUE(WORKFLOW) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(TRANSCODE) LIKE  T020-TCODE DEFAULT 'MIR7'
*"  EXPORTING
*"     VALUE(SAP_OBJECT) TYPE  SAEANWDID
*"     VALUE(SAP_OBJECTID) TYPE  SAEOBJID
*"     VALUE(RETURN) TYPE  BAPIRET1
*"  TABLES
*"      DOCUMENT_DATA STRUCTURE  OARFCDATA
*"--------------------------------------------------------------------
  data:
      w_bukrs      type bukrs,
      fvalue       type char255,
      bfirstitem   type i,
      bglaccount   type i,
      w_doctyp     type c,
      w_ctuparams  like ctu_params.

  data:
    bdcdata      type table of bdcdata with header line,
    it_msg       type table of bdcmsgcoll with header line.

  data:
    begin of wf_id,
      mblnr      like mkpf-mblnr,
      mjahr      like mkpf-mjahr,
    end of wf_id.

  clear return.
  bglaccount = 1.
  bfirstitem = 1.

  if document_data[] is initial.
    write:/ 'Document data not supplied.'.
  else.
    clear bdcdata.
    bdcdata-program  = 'SAPLMR1M'.
    bdcdata-dynpro   = '6000'.
    bdcdata-dynbegin = 'X'.
    insert table bdcdata.

*   Initialize CompanyCode
    clear fvalue.
    perform zd_get_value
      tables document_data
      using  'HDR_BSC-COMP_CODE'
      changing fvalue.
    if fvalue ne space.
      w_bukrs = fvalue(4).
    endif.
    set parameter id 'BUK' field w_bukrs.

*   Prepare Header data.
    perform zd_prepare_miro_header
      tables document_data
             bdcdata.

*   Prepare PO Numbers
    perform zd_prepare_miro_po_numbers
      tables document_data
             bdcdata
      using  w_doctyp.

*   Prepare Header Data 2
    perform zd_prepare_miro_header_2
      tables document_data
             bdcdata.

*   Prepare Position Data
    perform zd_prepare_miro_position_data
      tables document_data
             bdcdata.
  endif.

  clear bdcdata.
  bdcdata-program  = 'SAPLMR1M'.
  bdcdata-dynpro   = '6000'.
  bdcdata-dynbegin = 'X'.
  insert table bdcdata.

  clear bdcdata.
  bdcdata-fnam = 'BDC_OKCODE'.
  bdcdata-fval = '=HEADER_TOTAL'.
  insert table bdcdata.


** Read Header - Again

* Invoice Post ?
  if workflow ne 'X'.
    clear bdcdata.
    bdcdata-program  = 'SAPLMR1M'.
    bdcdata-dynpro   = '6000'.
    bdcdata-dynbegin = 'X'.
    insert table bdcdata.

    clear bdcdata.
    bdcdata-fnam = 'BDC_OKCODE'.
    bdcdata-fval = '/11'.
    insert table bdcdata.
  endif.

* Set BDC setting to not simulate BDC.
  w_ctuparams-nobinpt = 'X'.
  w_ctuparams-dismode = dynpromode.

* Call Transaction.
  call transaction transcode
    using bdcdata
    messages into it_msg
    options from w_ctuparams.

* Check Result.
  if sy-subrc = 0.
    loop at it_msg where msgid = 'M8'
                     and msgnr = '388'.
      wf_id-mblnr = it_msg-msgv1.
      exit.
    endloop.

    if wf_id-mblnr ne space.
      get parameter id 'GJR' field wf_id-mjahr.

      sap_objectid = wf_id.
      sap_object = 'BUS2081'.
    else.
      loop at it_msg.

        sy-msgty = it_msg-msgtyp.
        sy-msgid = it_msg-msgid.
        sy-msgno = it_msg-msgnr.
        sy-msgv1 = it_msg-msgv1.
        sy-msgv2 = it_msg-msgv2.
        sy-msgv3 = it_msg-msgv3.
        sy-msgv4 = it_msg-msgv4.

        perform zd_gen_bapireturn1
          using sy-msgty
                sy-msgid
                sy-msgno
                sy-msgv1
                sy-msgv2
                sy-msgv3
                sy-msgv4
          changing return.

        write:/ return-type, 3 return-message.

      endloop.

      clear:
        sap_object,
        sap_objectid.
    endif.
  else.
    perform zd_gen_bapireturn1
      using sy-msgty
            sy-msgid
            sy-msgno
            sy-msgv1
            sy-msgv2
            sy-msgv3
            sy-msgv4
      changing return.

    clear:
      sap_objectid,
      sap_object.
  endif.





endfunction.
