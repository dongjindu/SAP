*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_CPZP_CORRECTION_FORTOP
*&---------------------------------------------------------------------*

TYPE-POOLS: slis.
INCLUDE <icon>.

*TABLES: "ppc_head,
*        ppc_ord_inf,
*        cpzptemp.

TYPES: BEGIN OF gt_ppc_head_small_typ,
         orderid TYPE ppc_head-orderid,
         reppoint TYPE ppc_head-reppoint,
         confquant TYPE ppc_head-confquant,
         gjper TYPE co_gjper,
       END OF gt_ppc_head_small_typ.
TYPES: tt_ppc_head_small TYPE STANDARD TABLE OF gt_ppc_head_small_typ.

TYPES: BEGIN OF gt_act_comp_objnr_typ,
         reppoint TYPE ppc_reppoint_int,
         ressource_guid TYPE ppc_resguid_int,
         mode_guid TYPE ppc_mode_guid_int,
         cost_center TYPE kostl,
         activity_type TYPE lstar,
         duration_var TYPE ppc_duration_var,
         duration_fix TYPE ppc_duration_fix,
         delta_dur_var TYPE ppc_delta_duration_var,
         delta_dur_fix TYPE ppc_delta_duration_fix,
         durunit TYPE ppc_durunit,
         act_objnr TYPE j_objnr,
         orderid TYPE ppc_head-orderid,
         gjper TYPE co_gjper,
       END OF gt_act_comp_objnr_typ.
TYPES: tt_act_comp_objnr TYPE STANDARD TABLE OF gt_act_comp_objnr_typ.

TYPES: BEGIN OF gt_act_method_typ,
         reppoint TYPE ppc_reppoint_int,
         ressource_guid TYPE ppc_resguid_int,
         cost_center TYPE kostl,
         activity_type TYPE lstar,
         act_objnr TYPE j_objnr,
         duration_var TYPE ppc_duration_var,
         duration_fix TYPE ppc_duration_fix,
         delta_dur_var TYPE ppc_delta_duration_var,
         delta_dur_fix TYPE ppc_delta_duration_fix,
         durunit TYPE ppc_durunit,
         orderid TYPE ppc_head-orderid,
         gjper TYPE co_gjper,
       END OF gt_act_method_typ.
TYPES: tt_act_method TYPE STANDARD TABLE OF gt_act_method_typ
             WITH KEY reppoint ressource_guid act_objnr durunit orderid
             cost_center activity_type gjper.

TYPES: BEGIN OF gt_comp_method_typ,
         reppoint TYPE ppc_reppoint_int,
         costing_num TYPE ck_kalnr1,
         mat_number TYPE matnr,
         plant TYPE werks_d,
         batch TYPE charg_d,
         sales_doc TYPE vbeln_va,
         sales_doc_item TYPE posnr_va,
         special_stock TYPE sobkz,
         special_stock_val TYPE kzbws,
         cost_center TYPE kostl,
         activity_type TYPE lstar,
         quantity TYPE menge_pos,
         delta_quantity TYPE menge_pos,
         unit_of_measure TYPE meins,
         orderid TYPE ppc_head-orderid,
         gjper TYPE co_gjper,
       END OF gt_comp_method_typ.
TYPES:tt_comp_method TYPE STANDARD TABLE OF gt_comp_method_typ WITH KEY
         reppoint mat_number plant batch sales_doc sales_doc_item
         special_stock special_stock_val costing_num
         unit_of_measure orderid cost_center activity_type gjper.

TYPES: BEGIN OF gt_ord_inf_key_typ,
            orderid TYPE ppc_ord_inf-orderid,
            accassobj TYPE ppc_ord_inf-accassobj,
          END OF gt_ord_inf_key_typ.


TYPES: BEGIN OF gt_mat_comp_orderid_typ,
         ppc_mat TYPE ppc_material_components,
         orderid TYPE ppc_head-orderid,
         gjper TYPE co_gjper,
       END OF gt_mat_comp_orderid_typ.
TYPES: tt_mat_comp_orderid
                       TYPE STANDARD TABLE OF gt_mat_comp_orderid_typ.

TYPES: BEGIN OF gt_cpzp_mat_typ,
         cpzp TYPE cpzp,
         mat_number TYPE gt_comp_method_typ-mat_number,
         plant TYPE werks_d,
         batch TYPE charg_d,
         sales_doc TYPE vbeln_va,
         sales_doc_item TYPE posnr_va,
         special_stock TYPE sobkz,
         special_stock_val TYPE kzbws,
         resource_guid TYPE ppc_resguid_int,
       END OF gt_cpzp_mat_typ.

TYPES: BEGIN OF gt_act_comp_orderid_typ,
         ppc_act TYPE ppc_activity_components,
         orderid TYPE ppc_head-orderid,
         gjper TYPE co_gjper,
       END OF gt_act_comp_orderid_typ.
TYPES: tt_act_comp_orderid
                       TYPE STANDARD TABLE OF gt_act_comp_orderid_typ.

TYPES: BEGIN OF ts_period,
       gjper TYPE co_gjper,
       startday TYPE sy-datum,
       endday TYPE sy-datum,
       END OF ts_period.
TYPES: tt_period TYPE STANDARD TABLE OF ts_period.

TYPES: BEGIN OF ts_pcc_head,
       aufnr TYPE aufnr,
       prwrk TYPE werks_d,
       pmatn TYPE matnr,
       verid TYPE verid,
       END OF ts_pcc_head.
TYPES: tt_pcc_head TYPE STANDARD TABLE OF ts_pcc_head.

TYPES: tt_slis_fieldcat_alv TYPE STANDARD TABLE OF slis_fieldcat_alv.
*---------------------------------------------------------------------*
DATA: charx TYPE c VALUE 'X'.
DATA: "gv_werks TYPE werks_d,
*      gv_matnr TYPE matnr,
*      gv_verid TYPE verid,
      gv_pkosa_objnr TYPE aufnr, "objnr,
      gv_records TYPE i,
      gv_year TYPE gjahr,
      gv_month TYPE monat.





*----------------------------------------------------------------------*


* field symbols
FIELD-SYMBOLS: <fs_ppc_ord_inf> TYPE ppc_ord_inf,
               <fs_ppc_head> TYPE ppc_head,
               <fs_ppc_head_small> TYPE gt_ppc_head_small_typ,
               <fs_comp_wip_credit> TYPE gt_mat_comp_orderid_typ,
               <fs_act_wip_credit> TYPE gt_act_comp_orderid_typ,
               <fs_act_wip_credit_objnr> TYPE gt_act_comp_objnr_typ,
               <fs_act_method_credit> TYPE gt_act_method_typ,
               <fs_comp_method_credit> TYPE gt_comp_method_typ,
               <fs_act_method_debit> TYPE gt_act_method_typ,
               <fs_comp_method_debit> TYPE gt_comp_method_typ,
               <fs_mat_rev_orderid> TYPE gt_mat_comp_orderid_typ,
               <fs_act_comp_orderid> TYPE gt_act_comp_orderid_typ,
*               <fs_mat_comp_var_orderid> TYPE gt_mat_comp_orderid_typ,
*               <fs_mat_rev_var_orderid> TYPE gt_mat_comp_orderid_typ,
*               <fs_act_comp_var_orderid> TYPE gt_act_comp_orderid_typ,
*               <fs_act_rev_var_orderid> TYPE gt_act_comp_orderid_typ,
               <fs_act_method_scrap> TYPE gt_act_method_typ,
               <fs_comp_method_scrap> TYPE gt_comp_method_typ,
               <fs_cpzp> TYPE cpzp.


*----------------------------------------------------------------------*

TYPES: BEGIN OF gty_pcc_obj_s,
         objnr TYPE	j_objnr,
       END OF gty_pcc_obj_s,
       gty_pcc_obj_t TYPE STANDARD TABLE OF gty_pcc_obj_s.

DATA gt_cpzp_display TYPE STANDARD TABLE OF zsapbf_cpzp_display.
DATA gt_cpzp_display_numpcc TYPE STANDARD TABLE OF zsapbf_cpzp_display.
DATA gt_pcc_obj TYPE gty_pcc_obj_t.
DATA gv_pcc_idx TYPE i VALUE 0.
DATA gv_num_pcc TYPE i VALUE 1.


DATA gt_act_ippe TYPE STANDARD TABLE OF zsapbf_act_ippe.
" Global dummy Ippe buffer.
DATA gt_cssl     TYPE STANDARD TABLE OF cssl." Global CSSL buffer.

DATA gv_read_ippe TYPE xfeld.


************************************************************************

DATA gv_objnr           LIKE cpzp-objnr.
DATA gv_gjper           LIKE cpzp-gjper.
DATA:gt_cpzp            TYPE zsapbf_tt_cpzp,
     gt_cpzp_backup     TYPE zsapbf_tt_cpzp.
DATA gv_update_error    TYPE char1.

DATA: BEGIN OF gt_alv_cpzp OCCURS 0,
        objnr        LIKE cpzp-objnr,
        f_objnr      LIKE cpzp-f_objnr,
        gjper        LIKE cpzp-gjper,
        zaehl        LIKE cpzp-zaehl,
        plnkn        LIKE cpzp-plnkn,
        vorne        LIKE cpzp-vorne,
        plnfl        LIKE cpzp-plnfl,
        istmn        LIKE cpzp-istmn,
        gmper        LIKE cpzp-gmper,
        xmper        LIKE cpzp-xmper,
        gmsum        LIKE cpzp-gmsum,
        istmn_n      LIKE cpzp-istmn,
        gmper_n      LIKE cpzp-gmper,
        xmper_n      LIKE cpzp-xmper,
        gmsum_n      LIKE cpzp-gmsum,
        istmn_v      LIKE cpzp-istmn,
        gmper_v      LIKE cpzp-gmper,
        xmper_v      LIKE cpzp-xmper,
        gmsum_v      LIKE cpzp-gmsum,
        xmsum        LIKE cpzp-xmsum,
        meinh        LIKE cpzp-meinh,
        aufak        LIKE cpzp-aufak,
        umrez        LIKE cpzp-umrez,
        umren        LIKE cpzp-umren,
        plnme        LIKE cpzp-plnme,
        kzzpk        LIKE cpzp-kzzpk,
        rewqty       LIKE cpzp-rewqty,
        varmn        LIKE cpzp-varmn,
      END OF gt_alv_cpzp.
DATA: gs_alv_cpzp LIKE gt_alv_cpzp.
