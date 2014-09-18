FUNCTION Z_SHOP_CC_PARALLEL_TEST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_KOKRS) TYPE  KOKRS
*"     VALUE(P_BDATJ) TYPE  BDATJ
*"     VALUE(P_PERAB) TYPE  CO_PERAB
*"  EXPORTING
*"     VALUE(SUBRC) TYPE  SUBRC
*"     VALUE(ARTNR) TYPE  MATNR
*"  TABLES
*"      IT_MAT STRUCTURE  ZSCO_SHOP_MAT OPTIONAL
*"      IT_FSC_MAT STRUCTURE  ZSCO_SHOP_FSC_MAT OPTIONAL
*"      IT_ERROR STRUCTURE  ZTCO_SHOP_ERR OPTIONAL
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"      RESOURCEFAILURE
*"----------------------------------------------------------------------

*& This function is intermediation function without special logic
*& Just submit

  DATA : l_text(100).
  DATA : l_time TYPE p DECIMALS 4.
  DATA : l_artnr TYPE matnr.
  DATA : l_artnr2 TYPE matnr.

  CATCH SYSTEM-EXCEPTIONS arithmetic_errors          = 1
                          export_buffer_no_memory    = 2
                          rmc_communication_failure  = 3
                          rmc_invalid_status         = 4
                          rmc_system_failure         = 5.

    EXPORT it_mat     = it_mat     TO  MEMORY ID 'SHOPCC'.
    EXPORT it_fsc_mat = it_fsc_mat TO  MEMORY ID 'SHOPCC2'.
    EXPORT p_kokrs  TO  MEMORY ID 'SHOPCC_KOKRS'.
    EXPORT p_bdatj  TO  MEMORY ID 'SHOPCC_BDATJ'.
    EXPORT p_perab  TO  MEMORY ID 'SHOPCC_PERAB'.
    EXPORT p_perab  TO  MEMORY ID 'SHOPCC_PERAB'.

    read table it_fsc_mat index 1.
    write:/ it_fsc_mat-matnr, '...processed.'.

    IMPORT l_artnr FROM MEMORY ID 'SHOPCC_ARTNR'.

  ENDCATCH.

  subrc = sy-subrc .
  artnr = l_artnr.
  IF NOT artnr IS INITIAL.
    subrc = 6.
  ENDIF.

ENDFUNCTION.
