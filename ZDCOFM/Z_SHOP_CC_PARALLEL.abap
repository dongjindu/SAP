FUNCTION z_shop_cc_parallel.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_KOKRS) TYPE  KOKRS
*"     VALUE(P_BDATJ) TYPE  BDATJ
*"     VALUE(P_PERAB) TYPE  CO_PERAB
*"     VALUE(P_CCS) TYPE  CHAR01 DEFAULT 'X'
*"  EXPORTING
*"     VALUE(SUBRC) TYPE  SUBRC
*"     VALUE(ARTNR) TYPE  CHAR50
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

  DATA : l_artnr(50).
  DATA : l_temp_artnr TYPE matnr.
  DATA : it_log LIKE ztco_batch_log OCCURS 0 WITH HEADER LINE.

* For log.
  LOOP AT it_fsc_mat.
    it_log-kokrs = p_kokrs.
    it_log-bdatj = p_bdatj.
    it_log-poper = p_perab.
    it_log-matnr = it_fsc_mat-matnr.
    it_log-REPID = 'SHOP_ACT'.
    it_log-flag  = '1' .

    it_log-erdat = sy-datum.
    it_log-erzet = sy-uzeit.
    it_log-ernam = sy-uname.

    APPEND it_log. CLEAR it_log.
  ENDLOOP.
  MODIFY ztco_batch_log FROM TABLE it_log.
  COMMIT WORK.


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

** By Furong on 07/10/14 (
*    SUBMIT zaco19u_shop_new2 AND RETURN.
    SUBMIT zaco19u_shop_new AND RETURN.
** )
    IMPORT l_artnr FROM MEMORY ID 'SHOPCC_ARTNR'.

  ENDCATCH.



  subrc = sy-subrc .

  artnr = l_artnr.
  IF NOT artnr IS INITIAL.
    subrc = 6.
  ENDIF.

ENDFUNCTION.
