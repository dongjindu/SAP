function z_dicom_store_using_fb65.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2 OPTIONAL
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20 OPTIONAL
*"     VALUE(CONF_AR_OBJECT) TYPE  CHAR10 OPTIONAL
*"     VALUE(CONF_TASKID) TYPE  CHAR14 DEFAULT 'TS80000143'
*"     VALUE(CONF_DOC_AVAILABLE) TYPE  CHAR1 OPTIONAL
*"     VALUE(ARCHIV_DOC_ID) TYPE  CHAR40 OPTIONAL
*"     VALUE(BSC_COMP_CODE) TYPE  CHAR4 OPTIONAL
*"     VALUE(BSC_POSTING_TYPE) TYPE  CHAR1 DEFAULT 'G'
*"     VALUE(BSC_INVOICE_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(BSC_POSTING_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(BSC_VENDOR_NO) TYPE  CHAR17 OPTIONAL
*"     VALUE(BSC_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(BSC_INVOICE_NO) TYPE  CHAR16 OPTIONAL
*"     VALUE(ITEM_ACCOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(ITEM_AMOUNT) TYPE  CHAR13 OPTIONAL
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
*"     VALUE(PYT_INV_FISC_YR) TYPE  CHAR4 OPTIONAL
*"     VALUE(PYT_INV_LINE_ITM) TYPE  CHAR3 OPTIONAL
*"  TABLES
*"      ITEM_DATA STRUCTURE  ZFB60_ITEM OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------
  data:
      return          type bapireturn1,
      lv_sap_object   like toaom-sap_object,
      document_data   type oarfcdata occurs 0 with header line.

  if conf_doc_available = 'X'.
*   Check archive link settings
    call function 'Z_DICOM_VD_ARCHIVELINK_INFO'
      exporting
        conf_archiv_id = conf_archiv_id
        conf_ar_object = conf_ar_object
        conf_doc_type  = conf_doc_type
      importing
        sap_object     = lv_sap_object
*        return         = return
      exceptions
        general_error  = 1
        others         = 2.
    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.

  else.
* Validate Task ID.
    call function 'Z_DICOM_VD_TASK_ID'
      exporting
        conf_taskid   = conf_taskid
      importing
        return        = return
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

* If some of the basic fields are missing, assume it as scenario 02
  if bsc_comp_code eq space and bsc_invoice_no eq space and
     bsc_amount eq space.

    if conf_doc_available = 'X'.
      call function 'Z_DICOM_WORKITEM_IMAGE_GEN'
        exporting
          archiv_id     = conf_archiv_id
          document_type = conf_doc_type
          archiv_obj    = conf_ar_object
          archiv_doc_id = archiv_doc_id
        tables
          document_data = document_data
        exceptions
          general_error = 1
          others        = 2.
    else.
      call function 'Z_DICOM_WORKITEM_NOIMAGE_GEN'
        exporting
          task_id         = conf_taskid
          transactioncode = 'FB65'
        tables
          document_data   = document_data
        exceptions
          general_error   = 1
          others          = 2.
    endif.
  else.

*   No item data provided, insert one row into table from import param
    if item_data[] is initial.
      clear item_data.
      write item_account to item_data-item_account.
      write item_amount to item_data-item_amount.
      append item_data.
    endif.

    call function 'Z_DICOM_GEN_DOC_DATA_FB60'
      exporting
        bsc_comp_code       = bsc_comp_code
        bsc_posting_type    = bsc_posting_type
        bsc_invoice_date    = bsc_invoice_date
        bsc_posting_date    = bsc_posting_date
        bsc_vendor_no       = bsc_vendor_no
        bsc_amount          = bsc_amount
        bsc_invoice_no      = bsc_invoice_no
        bsc_cross_comp_code = bsc_cross_comp_code
        bsc_document_cur    = bsc_document_cur
        bsc_gl_indicator    = bsc_gl_indicator
        pyt_discount        = pyt_discount
        pyt_baseline_date   = pyt_baseline_date
        pyt_method          = pyt_method
        pyt_currency        = pyt_currency
        pyt_inv_ref         = pyt_inv_ref
        pyt_prt_bank_type   = pyt_prt_bank_type
        pyt_instr_key1      = pyt_instr_key1
        pyt_ind_payee       = pyt_ind_payee
        pyt_term_key        = pyt_term_key
        pyt_term_fixed      = pyt_term_fixed
        pyt_block_key       = pyt_block_key
        pyt_amount          = pyt_amount
        pyt_hse_bank        = pyt_hse_bank
        dtl_assign_no       = dtl_assign_no
        dtl_doc_hd_txt      = dtl_doc_hd_txt
        dtl_contract_no     = dtl_contract_no
        dtl_contract_type   = dtl_contract_type
        dtl_ref_line_itm    = dtl_ref_line_itm
        dtl_plan_level      = dtl_plan_level
        dtl_trd_part_area   = dtl_trd_part_area
        dtl_flow_type       = dtl_flow_type
        dtl_int_cal_exc     = dtl_int_cal_exc
        dtl_plan_date       = dtl_plan_date
        tax_amount          = tax_amount
        tax_code            = tax_code
        pyt_discount_base   = pyt_discount_base
        pyt_instr_key2      = pyt_instr_key2
        pyt_instr_key3      = pyt_instr_key3
        pyt_instr_key4      = pyt_instr_key4
        bsc_tax_amt         = bsc_tax_amt
        pyt_inv_fisc_yr     = pyt_inv_fisc_yr
        pyt_inv_line_itm    = pyt_inv_line_itm
      tables
        item_data           = item_data
        document_data       = document_data
      exceptions
        general_error       = 1
        others              = 2.
    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.

    if conf_doc_available = 'X'.
      call function 'Z_DICOM_WORKITEM_IMAGE_GEN'
        exporting
          archiv_id     = conf_archiv_id
          document_type = conf_doc_type
          archiv_obj    = conf_ar_object
          archiv_doc_id = archiv_doc_id
        tables
          document_data = document_data
        exceptions
          general_error = 1
          others        = 2.
    else.
      call function 'Z_DICOM_WORKITEM_NOIMAGE_GEN'
        exporting
          task_id         = conf_taskid
          transactioncode = 'FB65'
        tables
          document_data   = document_data
        exceptions
          general_error   = 1
          others          = 2.
    endif.
  endif.

  if sy-subrc ne 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.





endfunction.
