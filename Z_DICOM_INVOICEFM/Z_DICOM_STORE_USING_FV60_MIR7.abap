function z_dicom_store_using_fv60_mir7.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2 OPTIONAL
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20 OPTIONAL
*"     VALUE(CONF_AR_OBJECT_FX60) TYPE  CHAR10 OPTIONAL
*"     VALUE(CONF_TASKID_FX60) TYPE  CHAR14 DEFAULT 'TS80000143'
*"     VALUE(CONF_AR_OBJECT_MIRX) TYPE  CHAR10 OPTIONAL
*"     VALUE(CONF_TASKID_MIRX) TYPE  CHAR14 DEFAULT 'TS80000144'
*"     VALUE(CONF_DOC_AVAILABLE) TYPE  CHAR1 OPTIONAL
*"     VALUE(CONF_POST_METHOD_MIRX) TYPE  CHAR1 DEFAULT '1'
*"     VALUE(ARCHIV_DOC_ID) TYPE  CHAR40 OPTIONAL
*"     VALUE(BSC_COMP_CODE) TYPE  CHAR4 OPTIONAL
*"     VALUE(BSC_POSTING_TYPE_FX60) TYPE  CHAR1 DEFAULT 'R'
*"     VALUE(BSC_POSTING_TYPE_MIRX) TYPE  CHAR1 DEFAULT '1'
*"     VALUE(BSC_INVOICE_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(BSC_POSTING_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(BSC_VENDOR_NO) TYPE  CHAR17 OPTIONAL
*"     VALUE(BSC_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(BSC_INVOICE_NO) TYPE  CHAR16 OPTIONAL
*"     VALUE(BSC_TEXT_MIRX) TYPE  CHAR50 OPTIONAL
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
*"     VALUE(BSC_TAX_AMOUNT) TYPE  CHAR13 OPTIONAL
*"     VALUE(BSC_CURRENCY) TYPE  CHAR5 OPTIONAL
*"     VALUE(BSC_TAX_CALC) TYPE  CHAR1 OPTIONAL
*"     VALUE(PYT_REFERENCE) TYPE  CHAR30 OPTIONAL
*"     VALUE(DTL_UNPLAN_DEL_COST) TYPE  CHAR13 OPTIONAL
*"     VALUE(DTL_CURRENCY) TYPE  CHAR5 OPTIONAL
*"     VALUE(DTL_DOC_TYPE) TYPE  CHAR2 OPTIONAL
*"     VALUE(DTL_INV_RECPT_DATE) TYPE  CHAR10 OPTIONAL
*"     VALUE(DTL_EXCHANGE_RATE) TYPE  CHAR9 OPTIONAL
*"     VALUE(DTL_INV_PARTY) TYPE  CHAR10 OPTIONAL
*"     VALUE(DTL_GL_ACCT) TYPE  CHAR10 OPTIONAL
*"     VALUE(DTL_CTR_BANK) TYPE  CHAR3 OPTIONAL
*"     VALUE(DTL_SUPP_CTRY) TYPE  CHAR3 OPTIONAL
*"     VALUE(DTL_SERV_INDT) TYPE  CHAR1 OPTIONAL
*"     VALUE(DTL_PLAN_LVL) TYPE  CHAR2 OPTIONAL
*"     VALUE(CONF_LI_MATCHING) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(INVOICEDOCNUMBER) TYPE  CHAR10
*"     VALUE(FISCALYEAR) TYPE  CHAR4
*"  TABLES
*"      ITEM_DATA STRUCTURE  ZDICOM_ITEMS OPTIONAL
*"      PO_NUMBERS STRUCTURE  ZPONUMBER OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  data:
      fb60table type zfb60_item occurs 1 with header line,
      mirotable type zmiro_item occurs 1 with header line.

* Check if PO numbers provided
  if po_numbers[] is initial.
* Populate line items
    clear fb60table.
    loop at item_data.
      fb60table-item_account = item_data-item_account.
      fb60table-item_amount = item_data-item_amount.
      fb60table-item_tax_code = item_data-item_tax_code.
      fb60table-item_tax_jur_code = item_data-item_tax_jur_code.
      fb60table-item_assign_no = item_data-item_assign_no.
      fb60table-item_value_date = item_data-item_value_date.
      fb60table-item_text = item_data-item_text.
      fb60table-item_trad_partner_id = item_data-item_trad_partner_id.
      fb60table-item_bsn_area = item_data-item_busi_area.
      fb60table-item_trad_partner_bsn_area =
      item_data-item_trad_partner_bsn_area.
      fb60table-item_cost_center = item_data-item_cost_ctr.
      fb60table-item_finance_budget_itm =
      item_data-item_finance_budget_itm.
      fb60table-item_sales_ord_no = item_data-item_sales_ord_no.
      fb60table-item_sales_ord_itms = item_data-item_sales_ord_itms.
      fb60table-item_sales_ord_sched = item_data-item_sales_ord_sched.
      fb60table-item_plant = item_data-item_plant.
      fb60table-item_purchase_doc_no = item_data-item_purchase_doc_no.
      fb60table-item_purchase_doc_item_no =
      item_data-item_purchase_doc_item_no.
      fb60table-item_profit_center = item_data-item_profit_ctr.
      fb60table-item_partner_profit_center =
      item_data-item_partner_profit_center.
      fb60table-item_wbs_element = item_data-item_wbs_element.
      fb60table-item_fund_center = item_data-item_fund_ctr.
      fb60table-item_cost_object = item_data-item_cost_obj.
    fb60table-item_acct_assign_netw_no = item_data-item_acct_assignment.
      fb60table-item_operation_no = item_data-item_operation_no.
      fb60table-item_qty = item_data-item_quantity.
      fb60table-item_based_unit_measure = item_data-item_base_unit.
      fb60table-item_activity_type = item_data-item_activity_type.
      fb60table-item_personal_no = item_data-item_personal_no.
      fb60table-item_trans_type = item_data-item_trans_type.
      fb60table-item_material_no = item_data-item_material_no.
      fb60table-item_valuated_type = item_data-item_valuation_type.
      fb60table-item_earmark_fund_doc_no =
      item_data-item_earmark_fund_doc_no.
      fb60table-item_earmark_fund_doc_itm =
      item_data-item_earmark_fund_doc_itm.
      fb60table-item_function_area = item_data-item_func_area.
      fb60table-item_business_process = item_data-item_busi_place.
      fb60table-item_business_entity_no =
      item_data-item_business_entity_no.
      fb60table-item_building_no = item_data-item_building_no.
      fb60table-item_property_no = item_data-item_be_prop_no.
      fb60table-item_retail_unit = item_data-item_retail_unit.
      fb60table-item_lease_out_no = item_data-item_lease_out_no.
      fb60table-item_service_chg_key = item_data-item_service_no.
      fb60table-item_settle_unit = item_data-item_settle_unit.
    fb60table-item_settlement_ref_date = item_data-item_settle_ref_date.
      fb60table-item_manage_contact_no = item_data-item_mgt_ctr_no.
   fb60table-item_real_est_contract_no = item_data-item_real_est_ctr_no
.

      append fb60table.
    endloop.

* Call FV60 standard function
    call function 'Z_DICOM_STORE_USING_FV60'
      exporting
        conf_archiv_id      = conf_archiv_id
        conf_doc_type       = conf_doc_type
        conf_ar_object      = conf_ar_object_fx60
        conf_taskid         = conf_taskid_fx60
        conf_doc_available  = conf_doc_available
        archiv_doc_id       = archiv_doc_id
        bsc_comp_code       = bsc_comp_code
        bsc_posting_type    = bsc_posting_type_fx60
        bsc_invoice_date    = bsc_invoice_date
        bsc_posting_date    = bsc_posting_date
        bsc_vendor_no       = bsc_vendor_no
        bsc_amount          = bsc_amount
        bsc_invoice_no      = bsc_invoice_no
        item_account        = item_account
        item_amount         = item_amount
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
        item_data           = fb60table
      exceptions
        general_error       = 1
        others              = 2.

* Check status
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      raise general_error.
    endif.

  else.

* Populate line items
    clear mirotable.
    loop at item_data.
      mirotable-item_po = item_data-item_po.
      mirotable-item_amount = item_data-item_amount.
      mirotable-item_quantity = item_data-item_quantity.
      mirotable-item_measure_unit = item_data-item_measure_unit.
      mirotable-item_po_item_no = item_data-item_po_item_no.
      mirotable-item_po_text = item_data-item_po_text.
      mirotable-item_tax_code = item_data-item_tax_code.
      mirotable-item_non_cash_disc = item_data-item_non_cash_disc.
      mirotable-item_acct_assignment = item_data-item_acct_assignment.
      mirotable-item_service_no = item_data-item_service_no.
      mirotable-item_gl_acct = item_data-item_gl_acct.
      mirotable-item_busi_area = item_data-item_busi_area.
      mirotable-item_cost_ctr = item_data-item_cost_ctr.
      mirotable-item_wbs_element = item_data-item_wbs_element.
      mirotable-item_order_no = item_data-item_order_no.
      mirotable-item_asset_no = item_data-item_asset_no.
      mirotable-item_asset_sub_no = item_data-item_asset_sub_no.
      mirotable-item_asset_val_date = item_data-item_asset_val_date.
      mirotable-item_fund_ctr = item_data-item_fund_ctr.
      mirotable-item_commit_itm = item_data-item_commit_itm.
      mirotable-item_sales_ord_no = item_data-item_sales_ord_no.
      mirotable-item_no_in_sales_ord = item_data-item_no_in_sales_ord.
      mirotable-item_cost_obj = item_data-item_cost_obj.
      mirotable-item_func_area = item_data-item_func_area.
      mirotable-item_personal_no = item_data-item_personal_no.
      mirotable-item_profit_ctr = item_data-item_profit_ctr.
      mirotable-item_partner_acct_no = item_data-item_partner_acct_no.
    mirotable-item_acct_assig_ntwork = item_data-item_acct_assig_ntwork.
      mirotable-item_sched_line = item_data-item_sched_line.
      mirotable-item_fund_key = item_data-item_fund_key.
      mirotable-item_recover_indic = item_data-item_recover_indic.
      mirotable-item_control_area = item_data-item_control_area.
      mirotable-item_category = item_data-item_category.
      mirotable-item_rcv_qty = item_data-item_rcv_qty.
      mirotable-item_qty_invoiced = item_data-item_qty_invoiced.
      mirotable-item_ord_price_unit = item_data-item_ord_price_unit.
      mirotable-item_rcv_qty_in_po = item_data-item_rcv_qty_in_po.
      mirotable-item_inv_receipt_qty = item_data-item_inv_receipt_qty.
      mirotable-item_qty_ord = item_data-item_qty_ord.
      mirotable-item_net_ord_val = item_data-item_net_ord_val.
      mirotable-item_net_price = item_data-item_net_price.
      mirotable-item_currency = item_data-item_currency.
      mirotable-item_price_unit = item_data-item_price_unit.
      mirotable-item_ref_doc_itm = item_data-item_ref_doc_itm.
      mirotable-item_doc_no_ref_doc = item_data-item_doc_no_ref_doc.
      mirotable-item_gr_block_qty = item_data-item_gr_block_qty.
      mirotable-item_good_rcv_block = item_data-item_good_rcv_block.
      mirotable-item_ext_delv_note_no = item_data-item_ext_delv_note_no.
      mirotable-item_lading_bill_no = item_data-item_lading_bill_no.
      mirotable-item_delv_note_qty = item_data-item_delv_note_qty.
      mirotable-item_delv_note_unit = item_data-item_delv_note_unit.
      mirotable-item_incoterms1 = item_data-item_incoterms1.
      mirotable-item_final_inv_indc = item_data-item_final_inv_indc.
      mirotable-item_condition_type = item_data-item_condition_type.
      mirotable-item_condiftion_unit = item_data-item_condiftion_unit.
      mirotable-item_vendor_acct_no = item_data-item_vendor_acct_no.
      mirotable-item_material_no = item_data-item_material_no.
      mirotable-item_base_unit = item_data-item_base_unit.
      mirotable-item_valuation_type = item_data-item_valuation_type.
      mirotable-item_material_grp = item_data-item_material_grp.
      mirotable-item_origin_grp = item_data-item_origin_grp.
      mirotable-item_plant = item_data-item_plant.
      mirotable-item_valuation_area = item_data-item_valuation_area.
      mirotable-item_mpn_material = item_data-item_mpn_material.
      mirotable-item_desc = item_data-item_desc.
      mirotable-item_material_stock = item_data-item_material_stock.
      mirotable-item_cross_plant_cm = item_data-item_cross_plant_cm.
      mirotable-item_ean_upc = item_data-item_ean_upc.
      mirotable-item_vdr_material_no = item_data-item_vdr_material_no.
      mirotable-item_promotion_no = item_data-item_promotion_no.
      mirotable-item_busi_place = item_data-item_busi_place.
      mirotable-item_subq_cr_db_indc = item_data-item_subq_cr_db_indc.
      mirotable-item_mnl_block_indc = item_data-item_mnl_block_indc.
      mirotable-item_dlv_date = item_data-item_dlv_date.
      mirotable-item_created_by = item_data-item_created_by.
      mirotable-item_text = item_data-item_text.
      mirotable-item_activity_type = item_data-item_activity_type.
      mirotable-item_busi_process = item_data-item_busi_process.
      mirotable-item_inv_sign = item_data-item_inv_sign.
      mirotable-item_ship_cost_no = item_data-item_ship_cost_no.
      mirotable-item_ship_cost_itm = item_data-item_ship_cost_itm.
      mirotable-item_ship_cost_ctg = item_data-item_ship_cost_ctg.
      mirotable-item_ext_identity1 = item_data-item_ext_identity1.
      mirotable-item_ref_doc_no = item_data-item_ref_doc_no.
      mirotable-item_busi_enty_no = item_data-item_busi_enty_no.
      mirotable-item_building_no = item_data-item_building_no.
      mirotable-item_be_prop_no = item_data-item_be_prop_no.
      mirotable-item_rent_unit_no = item_data-item_rent_unit_no.
      mirotable-item_lease_out_no = item_data-item_lease_out_no.
      mirotable-item_serv_chg_key = item_data-item_serv_chg_key.
      mirotable-item_settle_unit = item_data-item_settle_unit.
      mirotable-item_mgt_ctr_no = item_data-item_mgt_ctr_no.
      mirotable-item_settle_ref_date = item_data-item_settle_ref_date.
      mirotable-item_real_est_ctr_no = item_data-item_real_est_ctr_no.
      mirotable-item_ord_prc_qty_blk = item_data-item_ord_prc_qty_blk.
      mirotable-item_qty_blk = item_data-item_qty_blk.
      mirotable-item_prc_blk = item_data-item_prc_blk.
      mirotable-item_date_blk = item_data-item_date_blk.
      mirotable-item_amt_blk = item_data-item_amt_blk.
      mirotable-item_qlt_blk = item_data-item_qlt_blk.
      mirotable-item_processed_indc = item_data-item_processed_indc.
      mirotable-item_no_disc_indc = item_data-item_no_disc_indc.

      append mirotable.

    endloop.

* Call standard MIR7 function
    call function 'Z_DICOM_STORE_USING_MIR7'
      exporting
        conf_archiv_id      = conf_archiv_id
        conf_doc_type       = conf_doc_type
        conf_ar_object      = conf_ar_object_mirx
        conf_taskid         = conf_taskid_mirx
        conf_post_method    = conf_post_method_mirx
        conf_doc_available  = conf_doc_available
        archiv_doc_id       = archiv_doc_id
        bsc_comp_code       = bsc_comp_code
        bsc_posting_type    = bsc_posting_type_mirx
        bsc_invoice_no      = bsc_invoice_no
        bsc_invoice_date    = bsc_invoice_date
        bsc_posting_date    = bsc_posting_date
        bsc_amount          = bsc_amount
        bsc_tax_amount      = bsc_tax_amount
        bsc_currency        = bsc_currency
        bsc_text            = bsc_text_mirx
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
        conf_li_matching    = conf_li_matching
      importing
        invoicedocnumber    = invoicedocnumber
        fiscalyear          = fiscalyear
      tables
        item_data           = mirotable
        po_numbers          = po_numbers
      exceptions
        general_error       = 1
        others              = 2.

* Check status
    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.

  endif.

endfunction.
