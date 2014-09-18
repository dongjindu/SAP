function zppc1tp_comp_conf_data_write.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IS_PPC_COMP_CONF) TYPE  ZPPC_COMP_CONF
*"     VALUE(IF_SKIPSYNC) TYPE  AS4FLAG OPTIONAL
*"     VALUE(IF_COMPSYNC) TYPE  AS4FLAG OPTIONAL
*"  TABLES
*"      IT_CONF_MATS STRUCTURE  ZPPC_MAT_CONF OPTIONAL
*"      IT_CONF_ACTS STRUCTURE  ZPPC_ACT_CONF OPTIONAL
*"  EXCEPTIONS
*"      ORDER_ERROR
*"      LINE_ERROR
*"      BAPI_ERROR
*"      DUPLICATE_POSTING
*"----------------------------------------------------------------------


  data: ls_mdpa like mdpa.


* Fill mdpmx with the planned order components
  perform read_planned_order
                 tables   it_conf_mats
                 using    is_ppc_comp_conf
                 changing ls_mdpa.

* Fill the PPC tables (Header, Item tables)
  perform bapi_conf_receive
            tables it_conf_mats
                   it_conf_acts
            using  ls_mdpa
                   is_ppc_comp_conf
                   if_skipsync
                   if_compsync.

endfunction.
