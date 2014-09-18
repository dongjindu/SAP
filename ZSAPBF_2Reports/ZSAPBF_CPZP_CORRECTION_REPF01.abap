*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_CPZP_CORRECTION_REPF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  programm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM programm USING iv_year TYPE gjahr
                    iv_month TYPE monat
                    iv_aufnr TYPE aufnr
                    iv_test TYPE flag
                    iv_updcur TYPE flag
                    is_parallel TYPE ppc_parallel "Added by James Sung-Kon Kim 2011.03.02
                    iv_wpssub TYPE int4           "Added by James Sung-Kon Kim 2011.03.02
           CHANGING ev_update_error TYPE char1
                    lt_cpzp TYPE zsapbf_tt_cpzp
                    lt_cpzp_backup TYPE zsapbf_tt_cpzp
                    lv_error_flag.
* variables
  DATA: lv_first_day TYPE sy-datum,
        lv_last_day TYPE sy-datum, "Added by Sung-Kon James Kim 2011/01/25
        lv_guid TYPE qrp002-cc_guid,
        lv_werks TYPE werks_d,
        lv_matnr TYPE matnr,
        lv_verid TYPE verid,
        lv_pkosa_objnr TYPE objnr,
        lv_kokrs TYPE kokrs.

  DATA: lv_gjper_post TYPE co_gjper,
        lv_gjper_curr TYPE co_gjper,
        lv_gjper_prev TYPE co_gjper.
* internal tables
*----------------------------------------------------------------------*
  DATA: lt_periods TYPE tt_period,        " correction periods
*----------------------------------------------------------------------*
* CPZP from database
*   lt_cpzp_backup TYPE STANDARD TABLE OF cpzp,
   lt_cpzp_last TYPE STANDARD TABLE OF cpzp.
*   lt_cpzp TYPE STANDARD TABLE OF cpzp.

  DATA: lt_ppc_ord_inf TYPE STANDARD TABLE OF ppc_ord_inf,

*----------------------------------------------------------------------*
        lt_ppc_head TYPE STANDARD TABLE OF ppc_head,

* small PPC_HEAD to compress confquant over reppoint and orderid
        lt_ppc_head_small TYPE STANDARD TABLE OF gt_ppc_head_small_typ
                                        WITH KEY orderid reppoint gjper,

****************tables for read documents*******************************
*----------------------------------------------------------------------*
* lt_material_components with ORDERID
       lt_mat_comp_orderid TYPE STANDARD TABLE OF gt_mat_comp_orderid_typ,
*----------------------------------------------------------------------*
* lt_material_components reversals with ORDERID
       lt_mat_rev_orderid TYPE STANDARD TABLE OF gt_mat_comp_orderid_typ,
*----------------------------------------------------------------------*
* lt_act_components with ORDERID
       lt_act_comp_orderid TYPE STANDARD TABLE OF gt_act_comp_orderid_typ,
*----------------------------------------------------------------------*
* lt_act_components reversals with ORDERID
       lt_act_rev_orderid TYPE STANDARD TABLE OF gt_act_comp_orderid_typ,
*----------------------------------------------------------------------*
* lt_mat_comp_var with ORDERID
       lt_mat_comp_var_orderid TYPE STANDARD TABLE OF gt_mat_comp_orderid_typ,
*----------------------------------------------------------------------*
* lt_mat_comp_var reversals with ORDERID
       lt_mat_rev_var_orderid TYPE STANDARD TABLE OF gt_mat_comp_orderid_typ,
*----------------------------------------------------------------------*
* lt_act_comp_var with ORDERID
       lt_act_comp_var_orderid TYPE STANDARD TABLE OF gt_act_comp_orderid_typ,
*----------------------------------------------------------------------*
* lt_act_comp_var reversals with ORDERID
       lt_act_rev_var_orderid TYPE STANDARD TABLE OF gt_act_comp_orderid_typ,

****************tables for calculation**********************************
*----------------------------------------------------------------------*
* compressed WIP-CREDIT components quantities
        lt_comp_method_credit TYPE STANDARD TABLE OF
           gt_comp_method_typ WITH KEY
           reppoint mat_number plant batch sales_doc sales_doc_item
           special_stock special_stock_val costing_num
           unit_of_measure orderid cost_center activity_type gjper,
*----------------------------------------------------------------------*
* compressed WIP-DEBIT activity quantities
        lt_act_method_debit TYPE STANDARD TABLE OF
               gt_act_method_typ
               WITH KEY reppoint ressource_guid act_objnr durunit orderid
               cost_center activity_type gjper,
*----------------------------------------------------------------------*
* compressed WIP-CREDIT activity quantities
        lt_act_method_credit TYPE STANDARD TABLE OF
               gt_act_method_typ
               WITH KEY reppoint ressource_guid act_objnr durunit orderid
               cost_center activity_type gjper,
*----------------------------------------------------------------------*
* compressed WIP-DEBIT components quantities
        lt_comp_method_debit TYPE STANDARD TABLE OF
           gt_comp_method_typ WITH KEY
           reppoint mat_number plant batch sales_doc sales_doc_item
           special_stock special_stock_val costing_num
           unit_of_measure orderid cost_center activity_type gjper,

****************tables for calculation scrap****************************
*----------------------------------------------------------------------*
* compressed SCRAP activity quantities
        lt_act_method_scrap TYPE STANDARD TABLE OF gt_act_method_typ
               WITH KEY reppoint ressource_guid act_objnr durunit orderid
                        cost_center activity_type gjper,
*----------------------------------------------------------------------*
* compressed WIP-CREDIT components quantities
        lt_comp_method_scrap TYPE STANDARD TABLE OF gt_comp_method_typ
                WITH KEY reppoint mat_number plant batch sales_doc sales_doc_item
                         special_stock special_stock_val costing_num
                         unit_of_measure orderid cost_center activity_type gjper.



*-----------------------------------------------------------*
*                    Main program part
*-----------------------------------------------------------*
* Guid select according to PCC order number
  PERFORM master_data_selection USING iv_aufnr
                             CHANGING lv_guid
                                      lv_pkosa_objnr
                                      lv_werks
                                      lv_matnr
                                      lv_verid
                                      lv_kokrs
                                      .
*-----------------------------------------------------------*
* Get the periods tables
  PERFORM get_period_list USING iv_year
                                iv_month
                                lv_werks
                                lv_kokrs
                       CHANGING lt_periods
                                lv_first_day
                                lv_last_day "Added by Sung-Kon James Kim 2011/01/25
                                .

  PERFORM get_periods_new USING iv_aufnr
                                iv_year
                                iv_month
                   CHANGING lv_gjper_post
                            lv_gjper_curr
                            lv_gjper_prev
                            .
*-----------------------------------------------------------*
* Read PPC orders according to guid
  PERFORM read_ppc_orderid USING lv_guid
                        CHANGING lt_ppc_ord_inf
                                 .
*-----------------------------------------------------------*
* Read confirmaiton from PPC head
  PERFORM read_ppc_headid USING lv_first_day
                                lv_last_day "Added by James Kim 2011/01/24
                                lt_ppc_ord_inf
                       CHANGING lt_ppc_head
                                lv_error_flag
                                .
  IF lv_error_flag = 'X'.
    ev_update_error = 'E'. " No data for the posting period "Added by James Kim 2011/01/27
    EXIT.
  ENDIF.

***** If condition clause have been added by James Sung-Kon Kim 2011.03.02
" IF is_parallel IS NOT INITIAL AND iv_wpssub IS NOT INITIAL.

*-----------------------------------------------------------*
* step 0 Read all PPC records from 1st day to today
"   PERFORM read_documents_parallel USING lt_ppc_ord_inf "Created by James Kim 2011.03.02
"                                         lt_ppc_head
"                                         lt_periods
"                                         is_parallel
"                                         iv_wpssub
"                                CHANGING lt_mat_comp_orderid     "component: forward
"                                         lt_mat_rev_orderid      "component: reverse
"                                         lt_mat_comp_var_orderid "component: forward variance
"                                         lt_mat_rev_var_orderid  "component: reverse variance
"                                         lt_act_comp_orderid     "activity: forward
"                                         lt_act_rev_orderid      "activity: reverse
"                                         lt_act_comp_var_orderid "activity: forward variance
"                                         lt_act_rev_var_orderid  "activity: reverse variance
"                                         .

" ELSE. "Added by James Sung-Kon Kim 2011.03.02

*-----------------------------------------------------------*
* step 0 Read all PPC records from 1st day to today
    PERFORM read_documents USING lt_ppc_ord_inf
                                 lt_ppc_head
                                 lt_periods
                        CHANGING lt_mat_comp_orderid     "component: forward
                                 lt_mat_rev_orderid      "component: reverse
                                 lt_mat_comp_var_orderid "component: forward variance
                                 lt_mat_rev_var_orderid  "component: reverse variance
                                 lt_act_comp_orderid     "activity: forward
                                 lt_act_rev_orderid      "activity: reverse
                                 lt_act_comp_var_orderid "activity: forward variance
                                 lt_act_rev_var_orderid  "activity: reverse variance
                                 .
" ENDIF. "Added by James Sung-Kon Kim 2011.03.02

*-----------------------------------------------------------*
* step 1 GI (forward - reverse)
  PERFORM substract_reversals_mat USING lt_mat_rev_orderid
                               CHANGING lt_mat_comp_orderid
                                        .
*-----------------------------------------------------------*
* step 2 Activity (forward - reverse)
  PERFORM substract_reversals_act USING lt_act_rev_orderid
                               CHANGING lt_act_comp_orderid
                                        .
*-----------------------------------------------------------*
* step 3 prepare the debit quantity from step 1 and 2
  PERFORM determin_wip_debit USING lt_mat_comp_orderid
                                   lt_act_comp_orderid
                          CHANGING lt_comp_method_debit
                                   lt_act_method_debit
                                   .
*-----------------------------------------------------------*
*For GR and SCRAP
* step 4.1 get the confirmation of GR and Assembly SCRAP
  PERFORM compress_wip_quant_credit USING lt_ppc_head
                                 CHANGING lt_ppc_head_small
                                          .
***** If condition clause have been added by James Sung-Kon Kim 2011.03.02
" IF is_parallel IS NOT INITIAL AND iv_wpssub IS NOT INITIAL.

* step 4.2 prepare the credit quantity's for these GR and Assembly SCRAP confirmations(consider all preceding RPs)
"   PERFORM determin_wip_credit_parallel USING lt_ppc_head_small
"                                              lt_periods
"                                              is_parallel
"                                              iv_wpssub
"                                     CHANGING lt_comp_method_credit
"                                              lt_act_method_credit
"                                              .
" ELSE.
* step 4.2 prepare the credit quantity's for these GR and Assembly SCRAP confirmations(consider all preceding RPs)
    PERFORM determin_wip_credit USING lt_ppc_head_small
                                      lt_periods
                             CHANGING lt_comp_method_credit
                                      lt_act_method_credit
                                      .
" ENDIF.
*-----------------------------------------------------------*
* step 5 Calculate the WIP
*  PERFORM substract_wip_credit USING lt_act_method_credit
*                                     lt_comp_method_credit
*                            CHANGING lt_comp_method_debit
*                                     lt_act_method_debit
*                                     .
*-----------------------------------------------------------*
* step 6 Build CPZP, fill GMPER(GI) and VARMN(Variance)
  PERFORM cpzp_build USING lt_comp_method_debit
                           lt_act_method_debit
                           lv_pkosa_objnr
                           lv_kokrs
                  CHANGING lt_cpzp
                           .
*-----------------------------------------------------------*
* step 7 fill GMSUM(GR+Assembly Scrap) and XMSUM (Assembly Scrap)
  PERFORM fill_gmsum USING lt_comp_method_credit
                           lt_act_method_credit
                           lv_pkosa_objnr
                           lv_kokrs
                  CHANGING lt_cpzp
                           .
*-----------------------------------------------------------*
* step 8 get component & activity scrap
  PERFORM compress_scrap_quant USING lt_ppc_head
                            CHANGING lt_ppc_head_small
                                     .

  PERFORM determin_scrap USING lt_ppc_head_small
                               lt_periods
                      CHANGING lt_comp_method_scrap
                               lt_act_method_scrap
                               .
*-----------------------------------------------------------*
* Step 9 fill XMPER(component scrap)
  PERFORM fill_xmper USING lt_comp_method_scrap
                           lt_act_method_scrap
                           lv_pkosa_objnr
                           lv_kokrs
                  CHANGING lt_cpzp
                           .
*-----------------------------------------------------------*

  PERFORM cpzp_sel_last USING lv_gjper_post  " 1 line revoked by Sung-Kon James Kim2010.09.15
                              "lv_gjper_curr
                              "lv_gjper_prev " 1 line commentated by Sung-Kon James Kim2010.09.15
                              lv_pkosa_objnr
                     CHANGING lt_cpzp_last   " the original CPZP of the last correction periods: Aug
                              .
*-----------------------------------------------------------*
* Step 10 fill ISTMN of gt_cpzp
* -B RWU- calculate ISTMN new
*  PERFORM insert_istmn USING lt_cpzp_last
*                    CHANGING lt_cpzp
*                             .
  PERFORM insert_istmn_new USING lv_gjper_post " 1 line revoked by Sung-Kon James Kim 2011/01/26
                                 lv_gjper_curr " 1 line revoked by Sung-Kon James Kim 2011/01/26
                                 "lv_gjper_prev " 1 line commentated by Sung-Kon James Kim2010.09.15
                        CHANGING lt_cpzp_last
                                 lt_cpzp
                             .
* -E RWU-
*-----------------------------------------------------------*
  IF iv_test IS INITIAL.
*    PERFORM write_cpzp USING lv_pkosa_objnr
*                             lv_gjper_post
*                             lv_gjper_curr
*                             lv_gjper_prev
*                             lt_cpzp
*                    CHANGING ev_update_error
*                             lt_cpzp_backup
    .
    PERFORM write_cpzp_new USING lv_pkosa_objnr
                                 lv_gjper_post
                                 lv_gjper_curr
                                 iv_updcur
                                 "lv_gjper_prev
                                 lt_cpzp
                        CHANGING ev_update_error
                                 lt_cpzp_backup
                                 .
  ELSE.
    PERFORM cpzp_sel_backup USING lv_gjper_post
                                  lv_pkosa_objnr
                         CHANGING lt_cpzp_backup
                           .

**** Start; Added by James Sung-Kon Kim 2011/03/07                                  .
    DELETE lt_cpzp_backup WHERE gjper NE lv_gjper_post.
**** End; Added by James Sung-Kon Kim 2011/03/07

  ENDIF.

**** Start; Commentated by James Sung-Kon Kim 2011/03/07
*  DELETE lt_cpzp_backup WHERE gjper NE lv_gjper_post.
**** End; Commentated by james sung-kon kim 2011/03/07
  DELETE lt_cpzp WHERE gjper NE lv_gjper_post.

  IF lt_cpzp IS INITIAL.
*    BREAK-POINT.
    ev_update_error = 'E'. " No data for the posting period
*    MESSAGE i415 WITH lv_pkosa_objnr.
*    EXIT.
  ENDIF.

ENDFORM.                    " programm
*&---------------------------------------------------------------------*
*&      Form  DETERMIN_READ_IPPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_READ_IPPE  text
*----------------------------------------------------------------------*
FORM determin_read_ippe.
  DATA ls_ppehdr TYPE ppehdr.
  CHECK gv_read_ippe IS INITIAL.
  SELECT SINGLE * INTO ls_ppehdr FROM ppehdr.               "#EC *
  IF sy-subrc IS INITIAL.
    gv_read_ippe = charx.
    EXIT.
  ENDIF.
ENDFORM.                    " DETERMIN_READ_IPPE
