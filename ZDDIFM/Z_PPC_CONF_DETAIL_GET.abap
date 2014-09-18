FUNCTION Z_PPC_CONF_DETAIL_GET.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IT_CONFID) TYPE  PPC_T_HEADID_INTERN
*"  EXPORTING
*"     VALUE(ET_ORDER_DETAILS) TYPE  PPC_GM_FOR_ORDER_TAB
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------
* PPC_CONF_DETAIL_GET

  RANGES: r_headid  FOR ppc_head-headid.
* Structures
  DATA:
  l_confid16 TYPE ppc_headid_int,
  ls_confid TYPE ppc_confid_str,
*  ls_return TYPE bapiret2,
  ls_header_raw TYPE ppc_header_raw,
  ls_order_details TYPE ppc_gm_for_order,
  ls_gmpos TYPE ppc_gmpos,
  ls_gmact TYPE ppc_gmact,
  ls_comps_ext TYPE ppc_show_ext_gi,
  ls_acts_raw TYPE ppc_act_raw,
  l_logsys TYPE logsys.
* Tables
  DATA:
    lt_confid TYPE ppc_t_headid,
    lt_header_raw TYPE ppc_t_header_raw,
    lt_headids TYPE TABLE OF ppc_heads_short,
    lt_comps_ext TYPE TABLE OF ppc_show_ext_gi,
    lt_acts_raw TYPE ppc_t_act_raw,
    lt_gmpos TYPE ppc_gmpos_tab,
    lt_gmact TYPE ppc_gmact_tab.

  r_headid-sign = 'I'.
  r_headid-option = 'EQ'.

  LOOP AT it_confid INTO ls_confid.
    r_headid-low = ls_confid-id.
    APPEND r_headid.
  ENDLOOP.

  CALL FUNCTION 'PPC1DC_HEADS_SELECT'
   EXPORTING
     IF_MAX_REC = 1000
   TABLES
     ir_hedid               = r_headid
     et_header_raw          = lt_header_raw
     et_headids             = lt_headids
   EXCEPTIONS
     no_recs_found          = 1
     too_many_records       = 2
     OTHERS                 = 3.
  IF sy-subrc <> 0.
    es_return-type =  'E'.
    es_return-id =  'PPC0'.
    es_return-number =  '010'.
    exit.
  ENDIF.

* Lesen der Komponenten (HEADIDs bereits ermittelt)
  CALL FUNCTION 'PPC1DC_COMPS_FOR_HEADS_SELECT'
       TABLES
            it_headids   = lt_headids
            et_comps_ext = lt_comps_ext.
* Lesen der Leistungen (HEADIDs bereits ermittelt)
  CALL FUNCTION 'PPC1DC_ACTS_FOR_HEADS_SELECT'
       TABLES
            it_headids  = lt_headids
            et_acts_raw = lt_acts_raw.

* Logsystem read
* fetch the info from the database:
  SELECT SINGLE logsys FROM t000 INTO l_logsys
         WHERE mandt = sy-mandt.

  SORT lt_header_raw BY orderid reppoint.
  LOOP AT lt_header_raw INTO ls_header_raw.

    REFRESH:  lt_gmpos, lt_gmact.
    CLEAR ls_order_details.

    ls_order_details-ordid = ls_header_raw-orderid.
    ls_order_details-rptid = ls_header_raw-reppoint.
*   fill the components
    LOOP AT lt_comps_ext INTO ls_comps_ext
                         WHERE headid = ls_header_raw-headid.
      ls_gmpos-confid = ls_comps_ext-headid.
*     MATNR18 -> MATNR40
      CALL FUNCTION 'MATNR_BAPI_CONV_FROM_INTERNAL'
           EXPORTING
                matnr_int = ls_comps_ext-matnr  " 18
           IMPORTING
                matnr_ext = ls_gmpos-matnr. " 40
      ls_gmpos-logsys          = l_logsys.
      ls_gmpos-lgort           = ls_comps_ext-lgort.
      ls_gmpos-prvbe           = ls_comps_ext-prvbe.
      ls_gmpos-batch           = ls_comps_ext-charg.
      ls_gmpos-gmove_ind       = ls_comps_ext-gmove_ind.
      ls_gmpos-sync_ind        = ls_comps_ext-sync_ind.
      ls_gmpos-sobkz           = ls_comps_ext-sobkz.
      ls_gmpos-sales_doc       = ls_header_raw-kdauf.
      ls_gmpos-sales_doc_item  = ls_header_raw-kdpos.
*     ls_gmpos-lifnr
*     ls_gmpos-kunnr
      ls_gmpos-cquan           = ls_comps_ext-confquant.
      ls_gmpos-cunit           = ls_comps_ext-confunit.
      ls_gmpos-delta_confquant = ls_comps_ext-delta_confquant.
      APPEND ls_gmpos TO lt_gmpos.
    ENDLOOP.
*   fill the activities
    LOOP AT lt_acts_raw INTO ls_acts_raw
                        WHERE headid = ls_header_raw-headid.
      ls_gmact-confid        = ls_acts_raw-headid.
*     ls_gmact-POSNR
      ls_gmact-resource_guid = ls_acts_raw-resource_guid.
      ls_gmact-mode_guid     = ls_acts_raw-mode_guid.
      ls_gmact-duration_var  = ls_acts_raw-duration_var.
      ls_gmact-duration_fix  = ls_acts_raw-duration_fix.
      ls_gmact-durunit       = ls_acts_raw-base_unit.
      ls_gmact-delta_dur_var = ls_acts_raw-delta_dur_var.
      ls_gmact-delta_dur_fix = ls_acts_raw-delta_dur_fix.
      APPEND ls_gmact TO lt_gmact.
    ENDLOOP.

    ls_order_details-gmpos_tab[] = lt_gmpos[].
    ls_order_details-gmact_tab[] = lt_gmact[].
    APPEND ls_order_details TO et_order_details.
  ENDLOOP.
* call was successful
  es_return-type =  'S'.
  es_return-id =  'PPC0'.
  es_return-number =  '000'.

ENDFUNCTION.
