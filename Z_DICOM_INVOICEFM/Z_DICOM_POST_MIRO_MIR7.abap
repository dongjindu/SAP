function z_dicom_post_miro_mir7.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2 OPTIONAL
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20 OPTIONAL
*"     VALUE(CONF_AR_OBJECT) TYPE  CHAR10 OPTIONAL
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
*"  EXPORTING
*"     VALUE(INVOICEDOCNUMBER) TYPE  CHAR10
*"     VALUE(FISCALYEAR) TYPE  BAPI_INCINV_FLD-FISC_YEAR
*"  TABLES
*"      ITEM_DATA STRUCTURE  ZMIRO_ITEM OPTIONAL
*"  CHANGING
*"     REFERENCE(RETURN1) TYPE  BAPIRETURN1
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  data:
    err_msg like  sy-msgv1,
    return2 like  bapiret2 occurs 0 with header line,
    bapi_header_data like bapi_incinv_create_header occurs 0 with header line,
    bapi_item_data like bapi_incinv_create_item occurs 0 with header line.

* Populate header data
  call function 'Z_DICOM_POST_MIRO_MIR7_HEADER'
    exporting
      bsc_comp_code       = bsc_comp_code
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
      indicator           = 'X'
    tables
      header_data         = bapi_header_data
    exceptions
      general_error       = 1
      others              = 2.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

* Populate item data
  call function 'Z_DICOM_POST_MIRO_MIR7_ITEM'
    tables
      ac_item_data   = item_data
      bapi_item_data = bapi_item_data
    exceptions
      general_error  = 1
      others         = 2.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

* Create and post invoice
  call function 'BAPI_INCOMINGINVOICE_CREATE'
    exporting
      headerdata       = bapi_header_data
    importing
      invoicedocnumber = invoicedocnumber
      fiscalyear       = fiscalyear
    tables
      itemdata         = bapi_item_data
      return           = return2.

* Check posting status
  call function 'Z_DICOM_POST_CHECK_STATUS'
    tables
      return   = return2
    changing
      p_return = return1.

  if return1-type ne 'E'.
*   Successful - commit transaction
    call function 'BAPI_TRANSACTION_COMMIT'.

*   Link image with posted invoice
    call function 'Z_DICOM_POST_MIRO_IMAGE_LINK'
      exporting
        archiv_id        = conf_archiv_id
        arc_doc_id       = archiv_doc_id
        ar_object        = conf_ar_object
        doc_type         = conf_doc_type
        invoicedocnumber = invoicedocnumber
        fiscalyear       = fiscalyear
      changing
        return1          = return1
      exceptions
        general_error    = 1
        others           = 2.

    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.
  else.
*   Failed - rollback transaction
    call function 'BAPI_TRANSACTION_ROLLBACK'.

    err_msg = return1-message.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = err_msg.
    raise general_error.
  endif.

endfunction.
