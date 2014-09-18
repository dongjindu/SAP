FUNCTION Z_FPP_VEHICLE_CREATION_PART2.
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
  DATA: l_SIZE               TYPE I          ,
        l_mode               LIKE ztpp_common_vals-key2.

  L_SIZE = STRLEN( P_7JB-VINN ) .
  CHECK L_SIZE = 17 .

  CONCATENATE P_7JB-MODL P_7JB-VINN+11(6) INTO WA_EQUNR .
  PERFORM class_assign .
ENDFUNCTION.
