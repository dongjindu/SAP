FUNCTION Z_FPP_PLANORDER_CREATION_PART3.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(P_7JB) LIKE  ZTPP_PMT07JB_B
*"  STRUCTURE  ZTPP_PMT07JB_B
*"             VALUE(P_DATE) LIKE  SY-DATUM
*"       EXCEPTIONS
*"              COMMUNICATION_FAILURE
*"              SYSTEM_FAILURE
*"              RESOURCE_FAILURE
*"----------------------------------------------------------------------
  DATA: l_date(10)           TYPE c          ,
        l_fsc                LIKE mara-matnr ,
        l_vin                LIKE mara-matnr ,
        l_mode               LIKE ztpp_common_vals-key2.

  wa_7jb  = p_7jb.
  wa_date = p_date.
  clear: wa_sorder, wa_plnum.

  " Vehicle Master Update for Plan Order & Reservation Number..
  PERFORM GET_VALUE .
  PERFORM generate_characterisitc_vm2 .
ENDFUNCTION.
