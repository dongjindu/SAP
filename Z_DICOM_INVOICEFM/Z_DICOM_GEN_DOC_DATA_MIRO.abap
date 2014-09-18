function z_dicom_gen_doc_data_miro.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BSC_COMP_CODE) TYPE  CHAR4 OPTIONAL
*"     VALUE(BSC_POSTING_TYPE) TYPE  CHAR1 DEFAULT '1'
*"     VALUE(BSC_INVOICE_NO) TYPE  CHAR16 OPTIONAL
*"     VALUE(BSC_INVOICE_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(BSC_POSTING_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(BSC_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(BSC_TAX_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(BSC_CURRENCY) TYPE  CHAR5 OPTIONAL
*"     VALUE(BSC_TEXT) TYPE  CHAR50 OPTIONAL
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
*"     VALUE(CONF_LI_MATCHING) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      ITEM_DATA STRUCTURE  ZMIRO_ITEM OPTIONAL
*"      DOCUMENT_DATA STRUCTURE  OARFCDATA OPTIONAL
*"      PO_NUMBERS STRUCTURE  ZPONUMBER OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  data:
    status     like sy-msgv1,
    item_po    like zponumber occurs 0 with header line,
    item_new   type zmiro_item occurs 0 with header line.

* Insert header data - Basic tab
  perform generate_doc_data_miro_bsc
    tables document_data
    using  bsc_comp_code
           bsc_posting_type
           bsc_invoice_no
           bsc_invoice_date
           bsc_posting_date
           bsc_amount
           bsc_tax_amount
           bsc_currency
           bsc_text.

* Insert header data - Payment tab
  perform generate_doc_data_miro_pyt
    tables document_data
    using  pyt_baseline_date
           pyt_discount
           pyt_method
           pyt_inv_ref
           pyt_inv_fisc_yr
           pyt_prt_bank_type
           pyt_term_key
           pyt_term_fixed
           pyt_block_key
           pyt_hse_bank
           pyt_reference.

* Insert header data - Details tab
  perform generate_doc_data_miro_dtl
    tables document_data
    using  dtl_unplan_del_cost
           dtl_currency
           dtl_doc_type
           dtl_inv_recpt_date
           dtl_assign_no
           dtl_doc_hd_txt
           dtl_exchange_rate
           dtl_inv_party
           dtl_gl_acct
           dtl_ctr_bank
           dtl_supp_ctry
           dtl_serv_indt
           dtl_plan_lvl
           dtl_plan_date.

* Insert header data - Miscellaneous
  perform generate_doc_data_miro_misc
    tables document_data
    using  tax_amount
           tax_code.

* Populate PO numbers list from line item
  call function 'Z_DICOM_GEN_PO_FROM_LINEITEM'
    tables
      po_numbers = item_po
      item_data  = item_data.

  if not item_po[] is initial.
*   PO number found in line item table, replace PO number(s) from line item table
    call function 'Z_DICOM_PONUMBERS_REPLACE'
      tables
        po_numbers_header = po_numbers
        po_numbers_line   = item_po.
  else.
*   PO number found in POtable but not specified in line item. We assume
*   the invoice consists of all line items from a single PO
    call function 'Z_DICOM_PONUMBERS_FILL_ITEM'
      tables
        po_numbers    = po_numbers
        item_data     = item_data
      exceptions
        general_error = 1
        others        = 2.

    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.
  endif.

* Fill PO number(s) into workflow container
  perform generate_doc_data_ponumbers
    tables document_data
           po_numbers.

* Populate list items with correct sequence accordingly to data in SAP
  call function 'Z_DICOM_GEN_ITEMS_MIRO'
    exporting
      conf_li_matching = conf_li_matching
    importing
      status           = status
    tables
      item_data        = item_data
      item_new         = item_new
      po_numbers       = po_numbers
    exceptions
      general_error    = 1
      others           = 2.

* Raise error if there is any
  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

* Insert position data with new reaaranged list
  perform generate_doc_data_miro_pos
    tables document_data
           item_new.

endfunction.
