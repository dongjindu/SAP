FUNCTION z_ppc1tp_ord_rp_check.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(P_PLNUM) TYPE  PLNUM OPTIONAL
*"     REFERENCE(P_RPEXT) TYPE  PPC_REPPOINT_EXT OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_CONFQ) TYPE  PPC_HEADCONFQUANT
*"  EXCEPTIONS
*"      ORDER_ERROR
*"----------------------------------------------------------------------

  RANGES: r_ordid    FOR  ppc_head-orderid.
  DATA:   iv_orderid TYPE  ppc_orderid,
          iv_repext  TYPE ppc_reppoint_ext,
          lt_headids TYPE TABLE OF ppc_heads_short,
          lv_headids TYPE ppc_heads_short,
          lt_header_raw TYPE ppc_t_header_raw,
          lv_header_raw TYPE ppc_header_raw.
  DATA:
        lt_confid        TYPE ppc_t_headid_intern.

  iv_repext = p_rpext.
  r_ordid-sign   = 'I'.  r_ordid-option = 'EQ'.
  r_ordid-low    = p_plnum.
  APPEND r_ordid.

  CALL FUNCTION 'PPC1DC_HEADS_SELECT'
    EXPORTING
      if_orderref      = 'X'
      if_max_rec       = 1000
*     IF_NEGATE        =
    TABLES
      ir_ordid         = r_ordid
*     IR_EXORD         =
      et_header_raw    = lt_header_raw
    EXCEPTIONS
      no_recs_found    = 1
      too_many_records = 2
      OTHERS           = 3.
  IF sy-subrc NE 0.
    MESSAGE e213(ppc1pr) WITH iv_orderid RAISING order_error.
  ENDIF.

  DATA: ls_confid    TYPE ppc_confid_str.
  DATA: lf_ppc_rp    LIKE  ppc_rp.
  DATA: BEGIN OF lt_rp_count OCCURS 0,
          id    TYPE ppc_headid_int,
          rptid TYPE ppc_rptuid,
          rpext TYPE ppc_reppoint_ext,
          rpcnt TYPE ppc_headconfquant,
        END OF lt_rp_count.
  REFRESH: lt_rp_count.


* collect count of RP
*  SORT lt_header_raw BY conftime DESCENDING.
  LOOP AT lt_header_raw INTO lv_header_raw.
    CALL FUNCTION 'PPC1DC_RP_READ'
      EXPORTING
        if_reppoint = lv_header_raw-reppoint
      IMPORTING
        ef_ppc_rp   = lf_ppc_rp.
    IF lf_ppc_rp-reppoint_ext = iv_repext.
      lt_rp_count-rptid = lv_header_raw-reppoint.
      lt_rp_count-rpext = lf_ppc_rp-reppoint_ext.
      IF lv_header_raw-flg_reversal NE 'X'.  "Scrap???
        lt_rp_count-rpcnt = lv_header_raw-confquant.
      ELSE.
        lt_rp_count-rpcnt = - lv_header_raw-confquant.
      ENDIF.
      COLLECT lt_rp_count.

      ls_confid-id = lv_header_raw-headid.
      APPEND ls_confid TO lt_confid.
    ENDIF.

  ENDLOOP.

  READ TABLE lt_rp_count INDEX 1.
  clear e_confq.
  e_confq = lt_rp_count-rpcnt..

ENDFUNCTION.
