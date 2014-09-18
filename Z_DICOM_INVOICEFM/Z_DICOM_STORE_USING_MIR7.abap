function z_dicom_store_using_mir7.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2 OPTIONAL
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20 OPTIONAL
*"     VALUE(CONF_AR_OBJECT) TYPE  CHAR10 OPTIONAL
*"     VALUE(CONF_TASKID) TYPE  CHAR14 DEFAULT 'TS80000144'
*"     VALUE(CONF_POST_METHOD) TYPE  CHAR1 DEFAULT '1'
*"     VALUE(CONF_DOC_AVAILABLE) TYPE  CHAR1 OPTIONAL
*"     VALUE(ARCHIV_DOC_ID) TYPE  CHAR40 OPTIONAL
*"     VALUE(BSC_COMP_CODE) TYPE  CHAR4 OPTIONAL
*"     VALUE(BSC_POSTING_TYPE) TYPE  CHAR1 DEFAULT '1'
*"     VALUE(BSC_INVOICE_NO) TYPE  CHAR16 OPTIONAL
*"     VALUE(BSC_INVOICE_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(BSC_POSTING_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(BSC_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(BSC_TAX_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(BSC_CURRENCY) TYPE  CHAR5 OPTIONAL
*"     VALUE(BSC_TEXT) TYPE  CHAR50 OPTIONAL
*"     VALUE(BSC_TAX_CALC) TYPE  CHAR1 OPTIONAL
*"     VALUE(PYT_BASELINE_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(PYT_DISCOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(PYT_METHOD) TYPE  CHAR1 OPTIONAL
*"     VALUE(PYT_INV_REF) TYPE  CHAR10 OPTIONAL
*"     VALUE(PYT_PRT_BANK_TYPE) TYPE  CHAR4 OPTIONAL
*"     VALUE(PYT_TERM_KEY) TYPE  CHAR4 OPTIONAL
*"     VALUE(PYT_TERM_FIXED) TYPE  CHAR1 OPTIONAL
*"     VALUE(PYT_BLOCK_KEY) TYPE  CHAR1 OPTIONAL
*"     VALUE(PYT_HSE_BANK) TYPE  CHAR5 OPTIONAL
*"     VALUE(PYT_REFERENCE) TYPE  CHAR30 OPTIONAL
*"     VALUE(DTL_UNPLAN_DEL_COST) TYPE  CHAR13 OPTIONAL
*"     VALUE(DTL_CURRENCY) TYPE  CHAR5 OPTIONAL
*"     VALUE(DTL_DOC_TYPE) TYPE  CHAR2 OPTIONAL
*"     VALUE(DTL_INV_RECPT_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(DTL_ASSIGN_NO) TYPE  CHAR18 OPTIONAL
*"     VALUE(DTL_DOC_HD_TXT) TYPE  CHAR25 OPTIONAL
*"     VALUE(DTL_EXCHANGE_RATE) TYPE  CHAR9 OPTIONAL
*"     VALUE(DTL_INV_PARTY) TYPE  CHAR10 OPTIONAL
*"     VALUE(DTL_GL_ACCT) TYPE  CHAR10 OPTIONAL
*"     VALUE(DTL_CTR_BANK) TYPE  CHAR3 OPTIONAL
*"     VALUE(DTL_SUPP_CTRY) TYPE  CHAR3 OPTIONAL
*"     VALUE(DTL_SERV_INDT) TYPE  CHAR1 OPTIONAL
*"     VALUE(DTL_PLAN_LVL) TYPE  CHAR2 OPTIONAL
*"     VALUE(DTL_PLAN_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(TAX_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(TAX_CODE) TYPE  CHAR2 OPTIONAL
*"     VALUE(PYT_INV_FISC_YR) TYPE  CHAR4 OPTIONAL
*"     VALUE(CONF_LI_MATCHING) TYPE  CHAR1 DEFAULT '0'
*"  EXPORTING
*"     VALUE(INVOICEDOCNUMBER) TYPE  CHAR10
*"     VALUE(FISCALYEAR) TYPE  CHAR4
*"  TABLES
*"      ITEM_DATA STRUCTURE  ZMIRO_ITEM OPTIONAL
*"      PO_NUMBERS STRUCTURE  ZPONUMBER OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  data:
      nfiscal_year    type numc4,
      return          type bapireturn1,
      document_data   type oarfcdata occurs 0 with header line.

* Populate header and item data into internal table (only if mandatory is filled)
  if bsc_comp_code ne space and
     bsc_invoice_no ne space and
     bsc_amount ne space.

    call function 'Z_DICOM_GEN_DOC_DATA_MIRO'
      exporting
        bsc_comp_code       = bsc_comp_code
        bsc_posting_type    = bsc_posting_type
        bsc_invoice_no      = bsc_invoice_no
        bsc_invoice_date    = bsc_invoice_date
        bsc_posting_date    = bsc_posting_date
        bsc_amount          = bsc_amount
        bsc_tax_amount      = bsc_tax_amount
        bsc_currency        = bsc_currency
        bsc_text            = bsc_text
        pyt_baseline_date   = pyt_baseline_date
        pyt_discount        = pyt_discount
        pyt_method          = pyt_method
        pyt_inv_ref         = pyt_inv_ref
        pyt_prt_bank_type   = pyt_prt_bank_type
        pyt_term_key        = pyt_term_key
        pyt_term_fixed      = pyt_term_fixed
        pyt_block_key       = pyt_block_key
        pyt_hse_bank        = pyt_hse_bank
        pyt_reference       = pyt_reference
        dtl_unplan_del_cost = dtl_unplan_del_cost
        dtl_currency        = dtl_currency
        dtl_doc_type        = dtl_doc_type
        dtl_inv_recpt_date  = dtl_inv_recpt_date
        dtl_assign_no       = dtl_assign_no
        dtl_doc_hd_txt      = dtl_doc_hd_txt
        dtl_exchange_rate   = dtl_exchange_rate
        dtl_inv_party       = dtl_inv_party
        dtl_gl_acct         = dtl_gl_acct
        dtl_ctr_bank        = dtl_ctr_bank
        dtl_supp_ctry       = dtl_supp_ctry
        dtl_serv_indt       = dtl_serv_indt
        dtl_plan_lvl        = dtl_plan_lvl
        dtl_plan_date       = dtl_plan_date
        tax_amount          = tax_amount
        tax_code            = tax_code
        pyt_inv_fisc_yr     = pyt_inv_fisc_yr
        conf_li_matching    = conf_li_matching
      tables
        item_data           = item_data
        document_data       = document_data
        po_numbers          = po_numbers
      exceptions
        general_error       = 1
        others              = 2.

    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.
  endif.

* Determine booking type
  case conf_post_method.
    when '2'.
*       Booking type - Hidden
      call function 'Z_DICOM_POST_MIRO_MIR7'
        exporting
          conf_archiv_id      = conf_archiv_id
          conf_doc_type       = conf_doc_type
          conf_ar_object      = conf_ar_object
          archiv_doc_id       = archiv_doc_id
          bsc_comp_code       = bsc_comp_code
          bsc_posting_type    = bsc_posting_type
          bsc_invoice_no      = bsc_invoice_no
          bsc_invoice_date    = bsc_invoice_date
          bsc_posting_date    = bsc_posting_date
          bsc_amount          = bsc_amount
          bsc_tax_amount      = bsc_tax_amount
          bsc_currency        = bsc_currency
          bsc_text            = bsc_text
          bsc_tax_calc        = bsc_tax_calc
          pyt_baseline_date   = pyt_baseline_date
          pyt_discount        = pyt_discount
          pyt_method          = pyt_method
          pyt_inv_ref         = pyt_inv_ref
          pyt_prt_bank_type   = pyt_prt_bank_type
          pyt_term_key        = pyt_term_key
          pyt_term_fixed      = pyt_term_fixed
          pyt_block_key       = pyt_block_key
          pyt_hse_bank        = pyt_hse_bank
          pyt_reference       = pyt_reference
          dtl_unplan_del_cost = dtl_unplan_del_cost
          dtl_currency        = dtl_currency
          dtl_doc_type        = dtl_doc_type
          dtl_inv_recpt_date  = dtl_inv_recpt_date
          dtl_assign_no       = dtl_assign_no
          dtl_doc_hd_txt      = dtl_doc_hd_txt
          dtl_exchange_rate   = dtl_exchange_rate
          dtl_inv_party       = dtl_inv_party
          dtl_gl_acct         = dtl_gl_acct
          dtl_ctr_bank        = dtl_ctr_bank
          dtl_supp_ctry       = dtl_supp_ctry
          dtl_serv_indt       = dtl_serv_indt
          dtl_plan_lvl        = dtl_plan_lvl
          dtl_plan_date       = dtl_plan_date
          tax_amount          = tax_amount
          tax_code            = tax_code
          pyt_inv_fisc_yr     = pyt_inv_fisc_yr
        importing
          invoicedocnumber    = invoicedocnumber
          fiscalyear          = nfiscal_year
        tables
          item_data           = item_data
        changing
          return1             = return
        exceptions
          general_error       = 1
          others              = 2.

*      if return-type eq 'E'.
      if sy-subrc <> 0.
        call function 'Z_DICOM_MSG_POPULATE'
          exporting
            error_msg = ''.
        raise general_error.
      endif.
      write nfiscal_year to fiscalyear.

    when '3'.
*       Booking type - Semi Transparent
      call function 'Z_DICOM_POST_MIRO_MIR7'
        exporting
          conf_archiv_id      = conf_archiv_id
          conf_doc_type       = conf_doc_type
          conf_ar_object      = conf_ar_object
          archiv_doc_id       = archiv_doc_id
          bsc_comp_code       = bsc_comp_code
          bsc_posting_type    = bsc_posting_type
          bsc_invoice_no      = bsc_invoice_no
          bsc_invoice_date    = bsc_invoice_date
          bsc_posting_date    = bsc_posting_date
          bsc_amount          = bsc_amount
          bsc_tax_amount      = bsc_tax_amount
          bsc_currency        = bsc_currency
          bsc_text            = bsc_text
          bsc_tax_calc        = bsc_tax_calc
          pyt_baseline_date   = pyt_baseline_date
          pyt_discount        = pyt_discount
          pyt_method          = pyt_method
          pyt_inv_ref         = pyt_inv_ref
          pyt_prt_bank_type   = pyt_prt_bank_type
          pyt_term_key        = pyt_term_key
          pyt_term_fixed      = pyt_term_fixed
          pyt_block_key       = pyt_block_key
          pyt_hse_bank        = pyt_hse_bank
          pyt_reference       = pyt_reference
          dtl_unplan_del_cost = dtl_unplan_del_cost
          dtl_currency        = dtl_currency
          dtl_doc_type        = dtl_doc_type
          dtl_inv_recpt_date  = dtl_inv_recpt_date
          dtl_assign_no       = dtl_assign_no
          dtl_doc_hd_txt      = dtl_doc_hd_txt
          dtl_exchange_rate   = dtl_exchange_rate
          dtl_inv_party       = dtl_inv_party
          dtl_gl_acct         = dtl_gl_acct
          dtl_ctr_bank        = dtl_ctr_bank
          dtl_supp_ctry       = dtl_supp_ctry
          dtl_serv_indt       = dtl_serv_indt
          dtl_plan_lvl        = dtl_plan_lvl
          dtl_plan_date       = dtl_plan_date
          tax_amount          = tax_amount
          tax_code            = tax_code
          pyt_inv_fisc_yr     = pyt_inv_fisc_yr
        importing
          invoicedocnumber    = invoicedocnumber
          fiscalyear          = nfiscal_year
        tables
          item_data           = item_data
        changing
          return1             = return
        exceptions
          general_error       = 1
          others              = 2.

*      if return-type eq 'E'.
      if sy-subrc <> 0.
*         If hidden booking type fails, continue with transparent booking type
        call function 'Z_DICOM_CREATE_WI_MIRO_MIR7'
          exporting
            conf_archiv_id     = conf_archiv_id
            conf_doc_type      = conf_doc_type
            conf_ar_object     = conf_ar_object
            conf_taskid        = conf_taskid
            conf_doc_available = conf_doc_available
            archiv_doc_id      = archiv_doc_id
            transactioncode    = 'MIR7'
          tables
            document_data      = document_data
          exceptions
            general_error      = 1
            others             = 2.
      else.
        write nfiscal_year to fiscalyear.
      endif.

    when others.
*       Booking type - Transparent
      call function 'Z_DICOM_CREATE_WI_MIRO_MIR7'
        exporting
          conf_archiv_id     = conf_archiv_id
          conf_doc_type      = conf_doc_type
          conf_ar_object     = conf_ar_object
          conf_taskid        = conf_taskid
          conf_doc_available = conf_doc_available
          archiv_doc_id      = archiv_doc_id
          transactioncode    = 'MIR7'
        tables
          document_data      = document_data
        exceptions
          general_error      = 1
          others             = 2.
  endcase.

* Raise error if there is any
  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

endfunction.
