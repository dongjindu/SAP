function z_dicom_post_miro_mir7_header.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BSC_COMP_CODE) TYPE  CHAR4 OPTIONAL
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
*"     VALUE(INDICATOR) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      HEADER_DATA STRUCTURE  BAPI_INCINV_CREATE_HEADER OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  data:
    decimal_temp type bapicurr-bapicurr,
    internal_date type char8,
    internal_amount type char13.

*   BASIC TAB
  header_data-invoice_ind = indicator.
  header_data-comp_code = bsc_comp_code.
  header_data-currency = bsc_currency.
  header_data-ref_doc_no = bsc_invoice_no.
  header_data-calc_tax_ind = bsc_tax_calc.
  header_data-item_text = bsc_text.

*     Gross Amount - translate to internal amount format
  translate bsc_amount using ',.'.
  decimal_temp = bsc_amount .
  call function 'BAPI_CURRENCY_CONV_TO_INTERNAL'
    exporting
      currency             = bsc_currency
      amount_external      = decimal_temp
      max_number_of_digits = 13
    importing
      amount_internal      = internal_amount.
  header_data-gross_amount = internal_amount.

*     Invoice Date - translate to internal date format
  if bsc_invoice_date ne space.
    call function 'CONVERT_DATE_TO_INTERNAL'
      exporting
        date_external = bsc_invoice_date
      importing
        date_internal = internal_date.
    header_data-doc_date = internal_date.
  endif.

*     Posting Date - translate to internal date format
  if bsc_posting_date ne space.
    call function 'CONVERT_DATE_TO_INTERNAL'
      exporting
        date_external = bsc_posting_date
      importing
        date_internal = internal_date.
    header_data-pstng_date = internal_date.
  endif.

*   PAYMENT TAB
  header_data-pymt_meth = pyt_method.
  header_data-pmnttrms = pyt_term_key.
  header_data-pmnt_block = pyt_block_key.

*     Baseline Date - translate to internal date format
  if pyt_baseline_date ne space.
    call function 'CONVERT_DATE_TO_INTERNAL'
      exporting
        date_external = pyt_baseline_date
      importing
        date_internal = internal_date.
    header_data-bline_date = internal_date.
  endif.

*   DETAILS TAB
  header_data-doc_type = dtl_doc_type.
  header_data-exch_rate = dtl_exchange_rate.
  header_data-diff_inv = dtl_inv_party.

endfunction.
