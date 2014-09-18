FUNCTION z_ppc1tp_comp_conf_data_write.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_PPC_APOHEADS) TYPE  BAPI_PPC_APOHEADS
*"     REFERENCE(IS_PPC_COMP_CONF) TYPE  PPC_COMP_CONF
*"     REFERENCE(IF_ACTC) TYPE  AS4FLAG DEFAULT ' '
*"     REFERENCE(IF_SOBKZ) TYPE  SOBKZ DEFAULT 'M'
*"     REFERENCE(IF_SKIPSYNC) TYPE  AS4FLAG
*"     REFERENCE(IF_COMPSYNC) TYPE  AS4FLAG
*"     REFERENCE(IF_TEST) TYPE  AS4FLAG
*"  TABLES
*"      IT_CONF_MATS STRUCTURE  PPC1TP_RESB
*"      IT_CONF_ACTS STRUCTURE  BAPI_PPC_APOACTLISTS
*"----------------------------------------------------------------------
* REFER: PPC1DM_WRITE_HANDLE
* Fill PPC tables with components and activities

*Structure          ZPPC_ACT_CONF
*RESOURCE_GUID	 PPC_RESGUID
*
*MODE_GUID	        PPC_MODE_GUID
*DURATION_VAR	        PPC_DURATION_VAR
*DURATION_FIX	        PPC_DURATION_FIX
*DELTA_DURATION_VAR	 PPC_DELTA_DURATION_VAR
*DELTA_DURATION_FIX	 PPC_DELTA_DURATION_FIX
*DURUNIT	        PPC_DURUNIT
*PC_EXT_SEND_CONF_DATA

******************* authority check ******************
*  AUTHORITY-CHECK OBJECT 'C_BCKFLUSH'
*     ID 'WERKS' FIELD is_apohead-werks
*     ID 'ACTVT' FIELD '31'.            " confirmation
*
*  CASE sy-subrc.
*    WHEN 0.                                                 "okay
*    WHEN 4.                            " no authority
*      MESSAGE e165(rm) WITH text-cnf is_apohead-werks
*         RAISING no_authority.
*    WHEN OTHERS.                       " problem with object
*      MESSAGE e786(rm) WITH 'C_BCKFLUSH'
*         RAISING no_authority.
*  ENDCASE.
*****************

*pre-requisite - maintain ZPPC1TP_RPV_MAINTAIN

  DATA: lv_sortp TYPE sortp.
  PERFORM read_rp USING    is_ppc_apoheads-reppoint
                  CHANGING lv_sortp.

* check duplicate posting - Z_PPC1TP_ORD_RP_CHECK

* Fill planned order components
  IF is_ppc_comp_conf-gi_ind = 'X'.
    PERFORM read_planned_order
                   TABLES   it_conf_mats
                   USING    is_ppc_apoheads
                            lv_sortp.
  ENDIF.

* Fill activity posting
  IF if_actc = 'X'.     "Activity Posting & Reversal
    PERFORM prepare_activites TABLES it_conf_acts
                              USING  is_ppc_apoheads lv_sortp.
  ENDIF.


  IF if_test = 'X'.
    PERFORM display_comp_list  TABLES it_conf_mats.
    PERFORM display_act_list   TABLES it_conf_acts.

  ELSE.

*-- if external order# is blank, use planned order#
    IF is_ppc_apoheads-ordernr = space.
      is_ppc_apoheads-ordernr = is_ppc_apoheads-orderid(12).
    ENDIF.

* PPC_ORD_INF; FUNCTION PPC1DC_ORD_INF_WRITE
    PERFORM check_ord_inf   USING  is_ppc_apoheads.
*    CALL FUNCTION 'QRP_APO_PKOSA_FIND'

* Fill the PPC tables (Header, Item tables)
    PERFORM bapi_conf_receive
              TABLES it_conf_mats
                     it_conf_acts
              USING  is_ppc_apoheads
                     is_ppc_comp_conf
                     if_sobkz
                     if_skipsync
                     if_compsync.
  ENDIF.
ENDFUNCTION.
