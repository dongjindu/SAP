FUNCTION Z_ML03_PARALLEL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_KOKRS) TYPE  KOKRS
*"     VALUE(P_BDATJ) TYPE  BDATJ
*"     VALUE(P_PERAB) TYPE  CO_PERAB
*"     VALUE(P_CURTP) TYPE  CURTP
*"  EXPORTING
*"     VALUE(SUBRC) TYPE  SUBRC
*"     VALUE(ARTNR) TYPE  CHAR50
*"  TABLES
*"      T_MATS STRUCTURE  ZSCO_ML03_MATS OPTIONAL
*"      IT_ERROR STRUCTURE  ZTCO_SHOP_ERR OPTIONAL
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"      RESOURCEFAILURE
*"----------------------------------------------------------------------

*& This function is intermediation function without special logic
*& Just submit
  DATA : p_batch(1) type c.
  DATA : l_artnr(50).
  DATA : l_temp_artnr TYPE matnr.
  data : it_log like ztco_batch_log occurs 0 with header line.

 break-point.
* For log.
  LOOP AT t_mats.
    it_log-kokrs = p_kokrs.
    it_log-bdatj = p_bdatj.
    it_log-poper = p_perab.
    it_log-matnr = t_mats.
    it_log-repid = 'ML_DETAIL'.
    it_log-flag  = '1' .

    it_log-erdat = sy-datum.
    it_log-erzet = sy-uzeit.
    it_log-ernam = sy-uname.

    APPEND it_log. CLEAR it_log.
  ENDLOOP.
  modify ztco_batch_log FROM table it_log.
  COMMIT WORK.


  CATCH SYSTEM-EXCEPTIONS arithmetic_errors          = 1
                          export_buffer_no_memory    = 2
                          rmc_communication_failure  = 3
                          rmc_invalid_status         = 4
                          rmc_system_failure         = 5.

    EXPORT t_mats = t_mats TO  MEMORY ID 'ML03_MATS'.
    EXPORT p_kokrs  TO  MEMORY ID 'ML03_KOKRS'.
    EXPORT p_bdatj  TO  MEMORY ID 'ML03_BDATJ'.
    EXPORT p_perab  TO  MEMORY ID 'ML03_PERAB'.
    EXPORT p_curtp  TO  MEMORY ID 'ML03_CURTP'.
    EXPORT p_batch  TO  MEMORY ID 'ML03_BATCH'.

    SUBMIT ZACO11R_ML03 AND RETURN.

    IMPORT l_artnr FROM MEMORY ID 'ML03_ARTNR'.

  ENDCATCH.



  subrc = sy-subrc .

  artnr = l_artnr.
  IF NOT artnr IS INITIAL.
    subrc = 6.
  ENDIF.

ENDFUNCTION.
