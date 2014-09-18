function z_dicom_call_fv6x.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DYNPROMODE) TYPE  CHAR1 DEFAULT 'E'
*"     VALUE(WORKFLOW) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(TRANSCODE) LIKE  T020-TCODE DEFAULT 'FV60'
*"  EXPORTING
*"     VALUE(SAP_OBJECT) TYPE  SAEANWDID
*"     VALUE(SAP_OBJECTID) TYPE  SAEOBJID
*"     VALUE(RETURN) TYPE  BAPIRET1
*"  TABLES
*"      DOCUMENT_DATA STRUCTURE  OARFCDATA
*"--------------------------------------------------------------------
  types:
      begin of imgkey_str,
        archiv_id type saearchivi,
        doc_id type saeardoid,
      end of imgkey_str.

  data:
    w_bukrs     type bukrs,
    fitempos(8) type n,
    icnt        type i,
    lcnt        type i,
    w_item_name like oarfcdata-name,
    fvalue      type char255,
    w_ctuparams like ctu_params.

  data:
    bdcdata     type table of bdcdata    with header line,
    it_msg      type table of bdcmsgcoll with header line.

  data:
    some_doc_data type oarfcdata occurs 0 with header line.

  data:
    begin of wf_id,
      bukrs like bkpf-bukrs,
      belnr like bkpf-belnr,
      gjahr like bkpf-gjahr,
    end of wf_id.

  if document_data[] is initial.
    write:/ 'Document data is not supplied'.
  else.

*   Fill Dialog
    bdcdata-program  = 'SAPMF05A'.
    bdcdata-dynpro   = '1100'.
    bdcdata-dynbegin = 'X'.
    insert table bdcdata.

    perform zd_retrieve_value_list
      tables document_data
             some_doc_data
      using  'HDR_BSC-COMP_CODE'.
    read table some_doc_data index 1.
    if sy-subrc eq 0.
      w_bukrs = some_doc_data-wert(4).
    else.
      clear
        w_bukrs.
    endif.

*   Read Header
    if w_bukrs ne space.
      if w_bukrs ne space.
        if workflow = 'X'.
          clear bdcdata.
          bdcdata-program  = 'SAPLACHD'.
          bdcdata-dynpro   = '1000'.
          bdcdata-dynbegin = 'X'.
          insert table bdcdata.

          clear bdcdata.
          bdcdata-fnam     = 'BKPF-BUKRS'.
          bdcdata-fval     = w_bukrs.
          insert table bdcdata.

          clear bdcdata.
          bdcdata-fnam     = 'BDC_OKCODE'.
          bdcdata-fval     = '/00'.
          insert table bdcdata.

          clear bdcdata.
          bdcdata-program  = 'SAPMF05A'.
          bdcdata-dynpro   = '1100'.
          bdcdata-dynbegin = 'X'.
          insert table bdcdata.
        else.
          call function 'Z_DICOM_CHANGECOMPCODE'
            exporting
              comp_id = w_bukrs.

          clear bdcdata.
          bdcdata-fnam     = 'BDC_OKCODE'.
          bdcdata-fval     = '=BU'.
          insert table bdcdata.

        endif.

      endif.

      perform zd_retrieve_value_list
        tables document_data
               some_doc_data
        using  'HDR_BSC-'.

*     Posting Type
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'POSTING_TYPE'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'RF05A-BUSCS'.
        bdcdata-fval     = fvalue(1).
        insert table bdcdata.
      endif.


*     Invoice date - COMPULSORY
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'INVOICE_DATE'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-BLDAT'.
        bdcdata-fval = fvalue(10).
        insert table bdcdata.
      endif.

*     Posting date
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'POSTING_DATE'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-BUDAT'.
        bdcdata-fval = fvalue(10).
        insert table bdcdata.
      endif.

*     Vendor - COMPULSORY
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'VENDOR_NO'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-ACCNT'.
        bdcdata-fval     = fvalue(16).
        insert table bdcdata.
      endif.

*     Reference
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'INVOICE_NO'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-XBLNR'.
        bdcdata-fval     = fvalue(16).
        insert table bdcdata.
      endif.

*     Amount
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'AMOUNT'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-WRBTR'.
        bdcdata-fval     = fvalue(13).
        insert table bdcdata.
      endif.

*     Currency
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'DOCUMENT_CUR'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-WAERS'.
        bdcdata-fval     = fvalue(5).
        insert table bdcdata.
      endif.

*     Cross CC No.
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'CROSS_CC_NO'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-BVORG'.
        bdcdata-fval     = fvalue(16).
        insert table bdcdata.
      endif.

*     GL Indicator.
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'GL_INDICATOR'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-UMSKZ'.
        bdcdata-fval     = fvalue(1).
        insert table bdcdata.
      endif.

*     Tax amount
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'TAX_AMT'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-WMWST'.
        bdcdata-fval     = fvalue(13).
        insert table bdcdata.
      endif.

*     Item text
      perform zd_read_value
        tables some_doc_data
        using  'HDR_BSC-'
               'ITM_TEXT'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-SGTXT'.
        bdcdata-fval     = fvalue(50).
        insert table bdcdata.
      endif.

    endif.

    perform zd_retrieve_value_list
      tables document_data
             some_doc_data
      using  'HDR_PYT-'.
    if not some_doc_data[] is initial.
      clear bdcdata.
      bdcdata-program  = 'SAPMF05A'.
      bdcdata-dynpro   = '1100'.
      bdcdata-dynbegin = 'X'.
      insert table bdcdata.

      clear bdcdata.
      bdcdata-fnam     = 'BDC_OKCODE'.
      bdcdata-fval     = '=PAYM'.
      insert table bdcdata.

      clear bdcdata.
      bdcdata-program  = 'SAPMF05A'.
      bdcdata-dynpro   = '1100'.
      bdcdata-dynbegin = 'X'.
      insert table bdcdata.

*     Basement date
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'BASELINE_DATE'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-ZFBDT'.
        bdcdata-fval     = fvalue(10).
        insert table bdcdata.
      endif.

*     Discount
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'DISCOUNT'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-WSKTO'.
        bdcdata-fval     = fvalue(13).
        insert table bdcdata.
      endif.

*     Discount Based
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'DISCOUNT_BASE'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam    = 'INVFO-SKFBT'.
        bdcdata-fval    = fvalue(13).
        insert table bdcdata.
      endif.

*     Payment method
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'METHOD'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam    = 'INVFO-ZLSCH'.
        bdcdata-fval    = fvalue(1).
        insert table bdcdata.
      endif.

*     Payment currency
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'PAYMENT_CUR'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam   = 'INVFO-PYCUR'.
        bdcdata-fval   = fvalue(5).
        insert table bdcdata.
      endif.

*     Invoice Reference
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'INV_REF'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-REBZG'.
        bdcdata-fval = fvalue(10).
        insert table bdcdata.
      endif.

*     Invoice fiscal year
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'INV_FISC_YR'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-REBZJ'.
        bdcdata-fval = fvalue(4).
        insert table bdcdata.
      endif.

*     Invoice line item
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'INV_LINE_ITM'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-REBZZ'.
        bdcdata-fval = fvalue(3).
        insert table bdcdata.
      endif.

*     Partner Bank Key
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'PRT_BANK_TYPE'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-BVTYP'.
        bdcdata-fval = fvalue(4).
        insert table bdcdata.
      endif.

*     Instruction Key 1
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'INSTR_KEY_1'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-DTWS1'.
        bdcdata-fval = fvalue(2).
        insert table bdcdata.
      endif.

*     Instruction Key 2
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'INSTR_KEY_2'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-DTWS2'.
        bdcdata-fval = fvalue(2).
        insert table bdcdata.
      endif.

*     Instruction Key 3
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'INSTR_KEY_3'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-DTWS3'.
        bdcdata-fval = fvalue(2).
        insert table bdcdata.
      endif.

*     Instruction Key 4
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'INSTR_KEY_4'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-DTWS4'.
        bdcdata-fval = fvalue(2).
        insert table bdcdata.
      endif.

*     Individual Payee
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'IND_PAYEE'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-REGUL'.
        bdcdata-fval = fvalue(1).
        insert table bdcdata.
      endif.

*     Payment Term.
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'TERM_KEY'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-ZTERM'.
        bdcdata-fval = fvalue(4).
        insert table bdcdata.
      endif.

*     Fixed Term
      perform zd_get_value
        tables some_doc_data
        using  'HDR_PYT-TERM_FIXED'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-ZBFIX'.
        bdcdata-fval = fvalue(1).
        insert table bdcdata.
      endif.

*     Payment Block key
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'BLOCK_KEY'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-ZLSPR'.
        bdcdata-fval = fvalue(1).
        insert table bdcdata.
      endif.

*     Payment Amount
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'AMOUNT'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-PYAMT'.
        bdcdata-fval = fvalue(13).
        insert table bdcdata.
      endif.

*     Payment House Key
      perform zd_read_value
        tables some_doc_data
        using  'HDR_PYT-'
               'HSE_BANK'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-HBKID'.
        bdcdata-fval = fvalue(5).
        insert table bdcdata.
      endif.
    endif.

*   Populate Details tab.
    perform zd_retrieve_value_list
      tables document_data
             some_doc_data
      using  'HDR_DTL-'.
    if not some_doc_data[] is initial.
      clear bdcdata.
      bdcdata-program  = 'SAPMF05A'.
      bdcdata-dynpro   = '1100'.
      bdcdata-dynbegin = 'X'.
      insert table bdcdata.

      clear bdcdata.
      bdcdata-fnam     = 'BDC_OKCODE'.
      bdcdata-fval     = '=MORE'.
      insert table bdcdata.

      clear bdcdata.
      bdcdata-program  = 'SAPMF05A'.
      bdcdata-dynpro   = '1100'.
      bdcdata-dynbegin = 'X'.
      insert table bdcdata.

*     Assignment no.
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'ASSIGN_NO'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'INVFO-ZUONR'.
        bdcdata-fval     = fvalue(18).
        insert table bdcdata.
      endif.

*     Head Text
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'DOC_HD_TXT'
        changing fvalue.

      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam    = 'INVFO-BKTXT'.
        bdcdata-fval    = fvalue(25).
        insert table bdcdata.
      endif.

*     Business Area
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'BUS_AREA'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam   = 'INVFO-GSBER'.
        bdcdata-fval   = fvalue(4).
        insert table bdcdata.
      endif.

*     Contract no.
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'CONTRACT_NO'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam   = 'INVFO-VERTN'.
        bdcdata-fval   = fvalue(13).
        insert table bdcdata.
      endif.

*     Contract type
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'CONTRACT_TYPE'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam  = 'INVFO-VERTT'.
        bdcdata-fval  = fvalue(1).
        insert table bdcdata.
      endif.

*     Reference key for item line
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'REF_LINE_ITM'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam  = 'INVFO-XREF3'.
        bdcdata-fval  = fvalue(20).
        insert table bdcdata.
      endif.

*     Plan level
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'PLAN_LEVEL'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-FDLEV'.
        bdcdata-fval = fvalue(2).
        insert table bdcdata.
      endif.

*     Trading partner business area
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'TRD_PART_AREA'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-PARGB'.
        bdcdata-fval = fvalue(4).
        insert table bdcdata.
      endif.

*     Flow type
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'FLOW_TYPE'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-VBEWA'.
        bdcdata-fval = fvalue(4).
        insert table bdcdata.
      endif.

*     Excemted
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'INT_CAL_EXC'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-ZINKZ'.
        bdcdata-fval = fvalue(2).
        insert table bdcdata.
      endif.

*     Plan Date
      perform zd_read_value
        tables some_doc_data
        using  'HDR_DTL-'
               'PLAN_DATE'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'INVFO-FDTAG'.
        bdcdata-fval = fvalue(10).
        insert table bdcdata.
      endif.
    endif.

*   Populate Tax tab.
    perform zd_retrieve_value_list
      tables document_data
             some_doc_data
      using  'HDR_TAX-'.
    if not some_doc_data[] is initial.
      clear bdcdata.
      bdcdata-program  = 'SAPMF05A'.
      bdcdata-dynpro   = '1100'.
      bdcdata-dynbegin = 'X'.
      insert table bdcdata.

      clear bdcdata.
      bdcdata-fnam     = 'BDC_OKCODE'.
      bdcdata-fval     = '=TAX'.
      insert table bdcdata.

      clear bdcdata.
      bdcdata-program  = 'SAPMF05A'.
      bdcdata-dynpro   = '1100'.
      bdcdata-dynbegin = 'X'.
      insert table bdcdata.

*     Amount
      perform zd_read_value
        tables some_doc_data
        using  'HDR_TAX-'
               'AMOUNT'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'RTAX1U12-FWSTE(01)'.
        bdcdata-fval = fvalue(13).
        insert table bdcdata.
      endif.

*     Code
      perform zd_read_value
        tables some_doc_data
        using  'HDR_TAX-'
               'CODE'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam = 'RTAX1U12-MWSKZ(01)'.
        bdcdata-fval = fvalue(2).
        insert table bdcdata.
      endif.
    endif.

*   Populate Notes tab.
    perform zd_retrieve_value_list
      tables document_data
             some_doc_data
      using  'HDR_NTS-'.
    if not some_doc_data[] is initial.
      clear bdcdata.
      bdcdata-program  = 'SAPMF05A'.
      bdcdata-dynpro   = '1100'.
      bdcdata-dynbegin = 'X'.
      insert table bdcdata.

      clear bdcdata.
      bdcdata-fnam     = 'BDC_OKCODE'.
      bdcdata-fval     = '=TEXT'.
      insert table bdcdata.

      clear bdcdata.
      bdcdata-program  = 'SAPLFTXT'.
      bdcdata-dynpro   = '0100'.
      bdcdata-dynbegin = 'X'.
      insert table bdcdata.

      clear bdcdata.
      bdcdata-fnam     = 'BDC_CURSOR'.
      bdcdata-fval     = 'RTEXT-LTEXT(02)'.
      insert table bdcdata.

*     Note
      perform zd_read_value
        tables some_doc_data
        using  'HDR_NTS-'
               'LONG_TXT1'
        changing fvalue.
      if fvalue ne space.
        clear bdcdata.
        bdcdata-fnam     = 'RTEXT-LTEXT(02)'.
        bdcdata-fval     = fvalue(100).
        insert table bdcdata.
      endif.

    endif.

*   Reset to Main tab.
    clear bdcdata.
    bdcdata-program  = 'SAPMF05A'.
    bdcdata-dynpro   = '1100'.
    bdcdata-dynbegin = 'X'.
    insert table bdcdata.

    clear bdcdata.
    bdcdata-fnam     = 'BDC_OKCODE'.
    bdcdata-fval     = '=MAIN'.
    insert table bdcdata.

    clear bdcdata.
    bdcdata-program  = 'SAPMF05A'.
    bdcdata-dynpro   = '1100'.
    bdcdata-dynbegin = 'X'.
    insert table bdcdata.

    lcnt = 0.
    icnt = 1.
    do.
*     Read Positon Data
      fitempos = icnt.
      concatenate 'ITEM-' fitempos into fvalue.
      w_item_name = fvalue.

      perform zd_retrieve_value_list
        tables document_data
               some_doc_data
        using  fvalue.

      if some_doc_data[] is initial.
        exit.
      endif.

      icnt = icnt + 1.
      lcnt = lcnt + 1.

*       Item Account
      perform zd_read_value
        tables some_doc_data
        using  w_item_name
               '_ITEM_ACCOUNT'
        changing fvalue.

      if fvalue ne space.

        perform zd_insert_bdc_fv6x_item
          tables bdcdata
          using  lcnt
                 'HKONT'
                 fvalue
                 13.

      endif.

*       Item Amount
      perform zd_read_value
        tables some_doc_data
        using  w_item_name
               '_ITEM_AMOUNT'
        changing fvalue.

      if fvalue ne space.
        perform zd_insert_bdc_fb60_item
          tables bdcdata
          using  lcnt
                 'WRBTR'
                 fvalue
                 13.
      endif.

      perform zd_insert_bdc_item:
        tables some_doc_data bdcdata
          using w_item_name '_ITEM_TAX_CODE'
                lcnt        'MWSKZ' 2,
        tables some_doc_data bdcdata
          using w_item_name '_ITEM_TAX_JUR_CO'
                lcnt        'TXJCD' 15,
        tables some_doc_data bdcdata
          using w_item_name '_ITEM_ASSIGN_NO'
                lcnt        'ZUONR' 18,
        tables some_doc_data bdcdata
          using w_item_name '_ITEM_VALUE_DATE'
                lcnt        'VALUT' 10,
        tables some_doc_data bdcdata
          using w_item_name '_ITEM_TEXT'
                lcnt        'SGTXT' 50,
        tables some_doc_data bdcdata
          using w_item_name '_ITEM_TRAD_PARTNER_ID'
                lcnt        'VBUND' 50,
        tables some_doc_data bdcdata
          using w_item_name '_ITEM_BSN_AREA'
                lcnt        'GSBER' 4,
        tables some_doc_data bdcdata
          using w_item_name '_ITEM_TRAD_PARTNER_BSN_AREA'
                lcnt        'PARGB'  4,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_COST_CENTER'
                lcnt         'KOSTL' 10,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_FINANCE_BUDGET_ITM'
                lcnt         'FKONT' 3,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_SALES_ORD_NO'
                lcnt         'AUFNR' 10,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_SALES_ORD_ITMS'
                lcnt         'POSN2' 12,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_SALES_ORD_SCHED'
                lcnt         'ETEN2' 12,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_PLANT'
                lcnt         'WERKS' 4,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_PURCHASE_DOC_NO'
                lcnt         'EBELN' 10,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_PURCHASE_DOC_ITEM_NO'
                lcnt         'EBELP' 5,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_PROFIT_CENTER'
                lcnt         'PRCTR' 10,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_PARTNER_PROFIT_CENTER'
                lcnt         'PPRCT' 10,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_WBS_ELEMENT'
                lcnt         'PROJK' 8,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_FUND_CENTER'
                lcnt         'FISTL' 16,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_COST_OBJECT'
                lcnt         'KSTRG' 12,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_ACCT_ASSIGN_NETW_NO'
                lcnt         'NPLNR' 12,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_OPERATION_NO'
                lcnt         'VORNR' 4,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_QTY'
                lcnt         'MENGE' 13,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_BASED_UNIT_MEASURE'
                lcnt         'MEINS' 3,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_ACTIVITY_TYPE'
                lcnt         'LSTAR' 6,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_PERSONAL_NO'
                lcnt         'PERNR' 8,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_TRANS_TYPE'
                lcnt         'BEWAR' 3,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_MATERIAL_NO'
                lcnt         'MATNR' 18,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_VALUATED_TYPE'
                lcnt         'BWTAR' 10,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_EARMARK_FUND_DOC_NO'
                lcnt         'KBLNR' 10,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_EARMARK_FUND_DOC_ITM'
                lcnt         'KBLPOS' 3,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_FUNCTION_AREA'
                lcnt         'FKBER' 4,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_BUSINESS_PROCESS'
                lcnt         'PRZNR' 12,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_BUSINESS_ENTITY_NO'
                lcnt         'SWENR' 8,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_BUILDING_NO'
                lcnt         'SGENR' 8,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_PROPERTY_NO'
                lcnt         'SGRNR' 8,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_RETAIL_UNIT'
                lcnt         'SMENR' 8,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_LEASE_OUT_NO'
                lcnt         'SMIVE' 13,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_SERVICE_CHG_KEY'
                lcnt         'SNKSL' 4,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_SETTLE_UNIT'
                lcnt         'SEMPSL' 5,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_SETTLEMENT_REF_DATE'
                lcnt         'DABRZ' 10,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_MANAGE_CONTACT_NO'
                lcnt         'SVWNR' 13,
        tables some_doc_data bdcdata
          using w_item_name  '_ITEM_REAL_EST_CONTRACT_NO'
                lcnt         'RECNNR' 13.
    enddo.

  endif.

* Change to NOT BDC session.
  w_ctuparams-nobinpt = 'X'.
  w_ctuparams-dismode = dynpromode.
  w_ctuparams-racommit = 'X'.

  refresh it_msg.

* Start Transaction.
  call transaction transcode
    using bdcdata
    messages into it_msg
    options from w_ctuparams.

  if sy-subrc eq 0.
*   Check new document created.
    clear wf_id.
    loop at it_msg where msgid = 'FP'
                     and msgnr = '001'.
      wf_id-belnr = it_msg-msgv1.
      exit.
    endloop.

    if wf_id-belnr ne space.

*     get new document number
      get parameter id 'BUK' field wf_id-bukrs.
      get parameter id 'GJR' field wf_id-gjahr.

      if wf_id-belnr ne space.
        sap_objectid = wf_id.
        sap_object = 'BKPF'.
      else.
        clear sap_objectid.
      endif.
    else.

      perform zd_gen_bapireturn1
        using sy-msgty
              'OA'
              '031'
              sy-msgv1
              sy-msgv2
              sy-msgv3
              sy-msgv4
        changing return.

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

        write:/ return-message.
      endloop.

      clear:
        sap_objectid,
        sap_object.
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
