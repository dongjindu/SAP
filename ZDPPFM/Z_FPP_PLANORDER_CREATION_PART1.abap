FUNCTION Z_FPP_PLANORDER_CREATION_PART1.
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
        l_vin                LIKE mara-matnr .

  wa_7jb  = p_7jb.
  wa_date = p_date.
  CONCATENATE wa_7jb-modl wa_7jb-vinn+11(7) INTO wa_equnr.

  SELECT SINGLE sales INTO wa_sorder
    FROM ztpp_wosum
   WHERE wo_ser = wa_7jb-ordr
     AND nation = wa_7jb-dist(3)
     AND dealer = wa_7jb-dist+3(2)
     AND extc   = wa_7jb-extc
     AND intc   = wa_7jb-intc     .

  " Processing of the Transaction MD11
  WRITE wa_7jb-sqdt TO l_date .

  CONCATENATE wa_7jb-moye wa_7jb-dist  wa_7jb-bmdl   INTO wa_fsc     .
  CONCATENATE wa_fsc      wa_7jb-ocnn INTO wa_fsc    SEPARATED BY ' '.

  PERFORM call_bdc_planned_order USING  wa_fsc       wa_7jb-pver
                                        l_date       wa_sorder   .

  CHECK WA_ERROR = SPACE.
  PERFORM SAVE_PLNUM             .
ENDFUNCTION.
