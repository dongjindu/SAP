FUNCTION z_fpp_aps_plandata.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_RP) TYPE  NUM02
*"     VALUE(P_SAP) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      T_PLANDATA STRUCTURE  ZTPP_INPUT_PLAN OPTIONAL
*"      T_SAP STRUCTURE  ZSPP_VM_GEN OPTIONAL
*"  EXCEPTIONS
*"      ETC_EXCEPTION
*"----------------------------------------------------------------------

  IF p_sap = 'X'.
    PERFORM  GATHER_SINGLE  TABLES T_SAP      USING P_RP.
  ELSE.
    PERFORM  gather_data    TABLES t_plandata USING p_rp.
  ENDIF.
ENDFUNCTION.
