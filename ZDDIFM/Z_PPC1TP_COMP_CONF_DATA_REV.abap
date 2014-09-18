FUNCTION Z_PPC1TP_COMP_CONF_DATA_REV.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_PPC_COMP_CONF) TYPE  PPC_COMP_CONF
*"     VALUE(IS_PPC_APOHEADS) TYPE  BAPI_PPC_APOHEADS
*"     REFERENCE(IF_SOBKZ) TYPE  SOBKZ DEFAULT 'M'
*"     REFERENCE(IF_SKIPSYNC) TYPE  AS4FLAG OPTIONAL
*"     REFERENCE(IF_RPTONLY) TYPE  AS4FLAG OPTIONAL
*"     REFERENCE(IF_PPC) TYPE  AS4FLAG DEFAULT 'X'
*"     REFERENCE(IF_TEST) TYPE  AS4FLAG OPTIONAL
*"  TABLES
*"      IT_CONF_MATS STRUCTURE  PPC1TP_RESB OPTIONAL
*"      IT_CONF_ACTS STRUCTURE  BAPI_PPC_APOACTLISTS OPTIONAL
*"  EXCEPTIONS
*"      ORDER_ERROR
*"      LINE_ERROR
*"      BAPI_ERROR
*"      RUN_PPCGO
*"----------------------------------------------------------------------
* PPC1TP_COMP_CONF_DATA_REVERSE

*  DATA: ls_mdpa LIKE mdpa.

  IF if_ppc = space.  "planned order base
**---> Fill with the planned order components, HMMA - could be incorrect
*    PERFORM read_planned_order_for_reverse
*            TABLES        it_conf_mats
*            USING         is_ppc_comp_conf
*            CHANGING      is_ppc_apoheads.
*
** restrict the component list to the reversal target
*    PERFORM delete_mdpmx_by_enmng
*                 TABLES it_conf_mats
*                 USING  is_ppc_comp_conf
*                        if_rptonly.
*
** delete all the components in the stsor where greater than the prvbe
** in the stsor
*    PERFORM delete_mdpmx_by_prvbe
*                 TABLES it_conf_mats
*                 USING is_ppc_comp_conf.


  ELSE.
* read PPC for reversal (this is better way)
    PERFORM read_ppc_comp_acts
                 TABLES     it_conf_mats
                            it_conf_acts
                 USING      is_ppc_comp_conf
                            if_rptonly
                 CHANGING   IS_PPC_APOHEADS.

  ENDIF.

* simulation run
  IF if_test = 'X'.
    PERFORM display_comp_list  TABLES it_conf_mats.
    perform display_act_list   TABLES it_conf_acts.
  ELSE.

* PPC_ORD_INF; FUNCTION PPC1DC_ORD_INF_WRITE - ANDY
*    PERFORM check_ord_inf
*              USING  ls_mdpa
*                     is_ppc_comp_conf.

* Fill the PPC tables (Header, Item tables)
    PERFORM bapi_conf_reverse
              TABLES it_conf_mats
                     it_conf_acts
              USING  is_ppc_apoheads
                     is_ppc_comp_conf
                     if_sobkz
                     if_skipsync.
  ENDIF.

ENDFUNCTION.
