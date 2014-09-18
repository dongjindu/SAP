function z_dicom_gen_doc_data_fv6x.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BSC_COMP_CODE) TYPE  CHAR4 OPTIONAL
*"     VALUE(BSC_POSTING_TYPE) TYPE  CHAR1 DEFAULT 'R'
*"     VALUE(BSC_INVOICE_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(BSC_POSTING_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(BSC_VENDOR_NO) TYPE  CHAR17 OPTIONAL
*"     VALUE(BSC_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(BSC_INVOICE_NO) TYPE  CHAR16 OPTIONAL
*"     VALUE(BSC_CROSS_COMP_CODE) TYPE  CHAR16 OPTIONAL
*"     VALUE(BSC_DOCUMENT_CUR) TYPE  CHAR5 OPTIONAL
*"     VALUE(BSC_GL_INDICATOR) TYPE  CHAR1 OPTIONAL
*"     VALUE(PYT_DISCOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(PYT_BASELINE_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(PYT_METHOD) TYPE  CHAR1 OPTIONAL
*"     VALUE(PYT_CURRENCY) TYPE  CHAR5 OPTIONAL
*"     VALUE(PYT_INV_REF) TYPE  CHAR10 OPTIONAL
*"     VALUE(PYT_PRT_BANK_TYPE) TYPE  CHAR4 OPTIONAL
*"     VALUE(PYT_INSTR_KEY1) TYPE  CHAR2 OPTIONAL
*"     VALUE(PYT_IND_PAYEE) TYPE  CHAR1 OPTIONAL
*"     VALUE(PYT_TERM_KEY) TYPE  CHAR4 OPTIONAL
*"     VALUE(PYT_TERM_FIXED) TYPE  CHAR1 OPTIONAL
*"     VALUE(PYT_BLOCK_KEY) TYPE  CHAR1 OPTIONAL
*"     VALUE(PYT_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(PYT_HSE_BANK) TYPE  CHAR5 OPTIONAL
*"     VALUE(DTL_ASSIGN_NO) TYPE  CHAR18 OPTIONAL
*"     VALUE(DTL_DOC_HD_TXT) TYPE  CHAR25 OPTIONAL
*"     VALUE(DTL_BUS_AREA) TYPE  CHAR4 OPTIONAL
*"     VALUE(DTL_CONTRACT_NO) TYPE  CHAR13 OPTIONAL
*"     VALUE(DTL_CONTRACT_TYPE) TYPE  CHAR1 OPTIONAL
*"     VALUE(DTL_REF_LINE_ITM) TYPE  CHAR20 OPTIONAL
*"     VALUE(DTL_PLAN_LEVEL) TYPE  CHAR2 OPTIONAL
*"     VALUE(DTL_TRD_PART_AREA) TYPE  CHAR4 OPTIONAL
*"     VALUE(DTL_FLOW_TYPE) TYPE  CHAR4 OPTIONAL
*"     VALUE(DTL_INT_CAL_EXC) TYPE  CHAR2 OPTIONAL
*"     VALUE(DTL_PLAN_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(TAX_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(TAX_CODE) TYPE  CHAR2 OPTIONAL
*"     VALUE(PYT_DISCOUNT_BASE) TYPE  CHAR13 OPTIONAL
*"     VALUE(PYT_INSTR_KEY2) TYPE  CHAR2 OPTIONAL
*"     VALUE(PYT_INSTR_KEY3) TYPE  CHAR2 OPTIONAL
*"     VALUE(PYT_INSTR_KEY4) TYPE  CHAR2 OPTIONAL
*"     VALUE(BSC_TAX_AMT) TYPE  CHAR13 OPTIONAL
*"     VALUE(BSC_ITM_TEXT) TYPE  CHAR50 OPTIONAL
*"     VALUE(PYT_INV_FISC_YR) TYPE  CHAR4 OPTIONAL
*"     VALUE(PYT_INV_LINE_ITM) TYPE  CHAR3 OPTIONAL
*"  TABLES
*"      ITEM_DATA STRUCTURE  ZFB60_ITEM OPTIONAL
*"      DOCUMENT_DATA STRUCTURE  OARFCDATA OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------
*{   INSERT         TS2K900069                                        1

* Fill DOCUMENT_DATA table structure with values
  perform generate_document_data_fv6x
   tables document_data
          item_data
   using  bsc_comp_code
          bsc_posting_type
          bsc_invoice_date
          bsc_posting_date
          bsc_vendor_no
          bsc_amount
          bsc_invoice_no
          bsc_cross_comp_code
          bsc_document_cur
          bsc_gl_indicator
          bsc_tax_amt
          bsc_itm_text
          pyt_discount
          pyt_baseline_date
          pyt_method
          pyt_currency
          pyt_inv_ref
          pyt_prt_bank_type
          pyt_instr_key1
          pyt_instr_key2
          pyt_instr_key3
          pyt_instr_key4
          pyt_ind_payee
          pyt_term_key
          pyt_term_fixed
          pyt_block_key
          pyt_amount
          pyt_hse_bank
          pyt_discount_base
          pyt_inv_fisc_yr
          pyt_inv_line_itm
          dtl_assign_no
          dtl_doc_hd_txt
          dtl_bus_area
          dtl_contract_no
          dtl_contract_type
          dtl_ref_line_itm
          dtl_plan_level
          dtl_trd_part_area
          dtl_flow_type
          dtl_int_cal_exc
          dtl_plan_date
          tax_amount
          tax_code.

*}   INSERT





endfunction.
