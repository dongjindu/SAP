FUNCTION z_ppc_rp_confirmation.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(P_HEAD) TYPE  BAPI_PPC_APOHEADS
*"     REFERENCE(P_SOBKZ) TYPE  SOBKZ DEFAULT 'M'
*"     REFERENCE(P_GR_IND) TYPE  XFELD OPTIONAL
*"     REFERENCE(P_GI_IND) TYPE  XFELD OPTIONAL
*"     REFERENCE(P_AC_IND) TYPE  XFELD OPTIONAL
*"     REFERENCE(P_PPC) TYPE  XFELD DEFAULT 'X'
*"     REFERENCE(P_RPTLY) TYPE  XFELD OPTIONAL
*"     REFERENCE(P_TEST) TYPE  XFELD DEFAULT 'X'
*"  EXPORTING
*"     VALUE(RETURN) LIKE  BAPIRET2 STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
*P_PLNUM	TYPE	PLNUM	                     	Planned order number
*P_PLEXT	TYPE	PLNUM	                     	External order number
*P_REPNT	TYPE	PPC_REPPOINT_EXT	              Reporting Point
*P_CONFQ	TYPE	PPC_HEADCONFQUANT	1	Backflush Quantity - Header Material
*P_BUDAT	TYPE	BUDAT	                     	Posting Date in the Document
*P_BLDAT	TYPE	BLDAT	                     	Document Date in Document
*P_GR_IND	TYPE	XFELD	                     	GR Indicator
*P_GI_IND	TYPE	XFELD	                     	GI Indicator
*P_AC_IND	TYPE	XFELD	                     	Activity Posting Indicator
*P_REV		TYPE	XFELD	                     	Reverse
*P_ASCRAP	TYPE	XFELD	                     	Assy Scrap
*P_ASCRSN	TYPE	MB_GRBEW			Reason for Movement
*P_PPC		TYPE	XFELD	'X'	                PPC based reversal
*P_RPTLY	TYPE	XFELD	                     	Reporting Point only reversal
*P_TEST	TYPE	XFELD	'X'			Test Run

*revese mode 'X' - keep only the components confirmed at this reporting point
*            ' ' - keep all the components confirmed UNTIL this reporting point
* PPC1CE_COMPONENTS_FOR_GR_GET ; function for GR costing...
*
* FIXME - need authorization object checking and allowed posting period (MMRV)
******************* authority check ******************
*  AUTHORITY-CHECK OBJECT 'C_BCKFLUSH'
*     ID 'WERKS' FIELD is_mdpa-PWWRK
*     ID 'ACTVT' FIELD '31'.            " confirmation
*
*  CASE sy-subrc.
*    WHEN 0.                                                 "okay
*    WHEN 4.                            " no authority
*      MESSAGE e165(rm) WITH 'Reverse' is_mdpa-PWWRK
*         RAISING no_authority.
*    WHEN OTHERS.                       " problem with object
*      MESSAGE e786(rm) WITH 'C_BCKFLUSH'
*         RAISING no_authority.
*  ENDCASE.
*****************

  DATA: ls_ppc_comp_conf TYPE ppc_comp_conf,
        ls_head          type BAPI_PPC_APOHEADS.
  DATA: l_message TYPE message_type.

  CLEAR: ls_ppc_comp_conf.
  CLEAR: l_message.
*        lt_ppc_comp_conf TYPE zppc_comp_conf.

  REFRESH: lt_mat, lt_act.

*prepare header
  ls_head = p_head.
  PERFORM prepare_ppc_confirm USING   p_gr_ind
                                      p_gi_ind
                             CHANGING ls_head
                                      ls_ppc_comp_conf.
*                                      ls_mdpa.



*fill into PPC tables
  IF p_head-flg_reversal = space.        "Confirmation
* ZPPC1TP_COMP_CONF_DATA_WRITE
** Fill mdpmx with the planned order components
*  perform read_planned_order
** Fill the PPC tables (Header, Item tables)
*  perform bapi_conf_receive
    CALL FUNCTION 'Z_PPC1TP_COMP_CONF_DATA_WRITE'
      EXPORTING
        is_ppc_comp_conf  = ls_ppc_comp_conf
        is_ppc_apoheads   = ls_head
        IF_ACTC           = p_ac_ind
        if_sobkz          = p_sobkz
        if_skipsync       = ' '
        if_compsync       = ' '   "HMMA specific???
        if_test           = p_test
      TABLES
        it_conf_mats      = lt_mat
        it_conf_acts      = lt_act
      EXCEPTIONS
        order_error       = 1
        line_error        = 2
        bapi_error        = 3
        duplicate_posting = 4
        OTHERS            = 5.

    IF sy-subrc IS INITIAL.
      PERFORM fill_message USING l_message
                           'S' 'PPC1PR' 008 space space space space.
    ELSE.
      PERFORM fill_message USING l_message
                           'E' sy-msgid sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.   "Reverse
    CALL FUNCTION 'Z_PPC1TP_COMP_CONF_DATA_REV'
      EXPORTING
        is_ppc_comp_conf = ls_ppc_comp_conf
        is_ppc_apoheads  = ls_head
        if_sobkz         = p_sobkz
        if_rptonly       = p_rptly
        if_ppc           = p_ppc
        if_skipsync      = ' '
        if_test          = p_test
      TABLES
        it_conf_mats     = lt_mat
        it_conf_acts     = lt_act
      EXCEPTIONS
        order_error      = 1
        line_error       = 2
        bapi_error       = 3
        run_ppcgo        = 4
        OTHERS           = 5.
    IF sy-subrc IS INITIAL.
*      message s008(ppc1pr).
      PERFORM fill_message USING l_message
                                 'S' 'PPC1PR' 008 space space space space.
**  plan order is deleted...or not exist
    ELSE.
      PERFORM fill_message USING l_message
                               'E' sy-msgid sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.


  IF NOT l_message IS INITIAL.
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = l_message-msgty
        cl     = l_message-msgid
        number = l_message-msgno
        par1   = l_message-msgv1
        par2   = l_message-msgv2
        par3   = l_message-msgv3
        par4   = l_message-msgv4
      IMPORTING
        return = return
      EXCEPTIONS
        OTHERS = 1.
    EXIT.
  ENDIF.

ENDFUNCTION.
