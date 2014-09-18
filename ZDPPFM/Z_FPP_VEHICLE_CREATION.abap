FUNCTION Z_FPP_VEHICLE_CREATION.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_7JB) LIKE  ZTPP_PMT07JB_B STRUCTURE  ZTPP_PMT07JB_B
*"     VALUE(P_DATE) LIKE  SY-DATUM
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"      RESOURCE_FAILURE
*"----------------------------------------------------------------------
  DATA: l_date(10)           TYPE c          ,
        l_salesorder         LIKE vbak-vbeln ,
        l_fsc                LIKE mara-matnr ,
        l_vin                LIKE mara-matnr ,
        l_mode               LIKE ztpp_common_vals-key2.

  wa_7jb  = p_7jb .
  wa_date = P_DATE .
  CONCATENATE wa_7jb-ordr wa_7jb-dist INTO wa_material .

* l_mode = 'EMF'.      " wa_7jb-modl .
  l_mode = WA_7jb-modl .
  CONCATENATE l_mode wa_7jb-vinn+11(7) INTO wa_equnr.

  SELECT SINGLE fsc sales INTO (l_fsc, l_salesorder)
    FROM ztpp_wosum
   WHERE wo_ser = wa_7jb-ordr
     AND nation = wa_7jb-dist(3)
     AND dealer = wa_7jb-dist+3(2)
     AND extc   = wa_7jb-extc
     AND intc   = wa_7jb-intc     .

  " Processing of the Transaction MD11
  WRITE wa_7jb-sqdt TO l_date .
** Changed by Furong Wang for EBOM
*  CONCATENATE l_fsc       wa_7jb-ocnn INTO l_fsc     SEPARATED BY ' '.
** End of change
  " Processing the VINGeneration & Vehicla Master Creation
  PERFORM vin_vm_creation USING l_salesorder  l_fsc .

  if wa_error = 'X'.  exit.  endif.

  PERFORM class_assign .
  if wa_error = 'X'.  exit.  endif.
ENDFUNCTION.
