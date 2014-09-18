FUNCTION Z_FPP_PLANORDER_CREATION.
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
        l_fsc                LIKE mara-matnr ,
        l_vin                LIKE mara-matnr ,
        l_mode               LIKE ztpp_common_vals-key2.

  wa_7jb  = p_7jb.
  wa_date = p_date.
* l_mode  = 'EMF'.      " wa_7jb-modl .
  l_mode  = wa_7jb-modl .
  CONCATENATE l_mode wa_7jb-vinn+11(7) INTO wa_equnr.

  SELECT SINGLE fsc sales INTO (l_fsc, wa_sorder)
    FROM ztpp_wosum
   WHERE wo_ser = wa_7jb-ordr
     AND nation = wa_7jb-dist(3)
     AND dealer = wa_7jb-dist+3(2)
     AND extc   = wa_7jb-extc
     AND intc   = wa_7jb-intc     .

  " Processing of the Transaction MD11
  WRITE wa_7jb-sqdt TO l_date.

** Changed by Furong on 10/10/07 for EBOM
*  CONCATENATE wa_7jb-moye wa_7jb-dist  wa_7jb-bmdl   INTO wa_fsc     .
*  CONCATENATE wa_fsc      wa_7jb-ocnn INTO wa_fsc    SEPARATED BY ' '.
*
*  PERFORM call_bdc_planned_order USING  wa_fsc       wa_7jb-pver
*                                        l_date       wa_sorder   .
   PERFORM call_bdc_planned_order USING  l_fsc       wa_7jb-pver
                                        l_date       wa_sorder   .

** End of change

* IF wa_error = 'X'.  EXIT.  ENDIF.

  " Vehicle Master Update for Plan Order & Reservation Number..
  PERFORM vin_vm_creation2  USING wa_plnum wa_sorder   .

  if wa_error = 'X'.  exit.  endif.

  PERFORM generate_characterisitc_vm .
  if wa_error = 'X'.  exit.  endif.
ENDFUNCTION.
