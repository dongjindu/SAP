FUNCTION zmmf_if_create_info_record .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_INFO_RECORD) LIKE  ZSMM_IF008 STRUCTURE  ZSMM_IF008
*"  TABLES
*"      E_RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
  DATA: lv_seqno LIKE ztmm_if014-seqno.

  CLEAR: v_info_record, v_flag.
  CLEAR: it_return, e_return.

  REFRESH: it_return, e_return.

  MOVE: i_info_record TO v_info_record.
  MOVE: 'C'           TO v_flag. " create info record flag in log table

*---// interface data type conversion.
  PERFORM apply_conversion_rule.

*---// insert entry to table.
  PERFORM save_if_table.

  IF e_return-type = 'E'.
    EXIT.
  ENDIF.

*---// importing value existence check with further processing.
  PERFORM check_parameters.

  READ TABLE it_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    LOOP AT it_return.
      lv_seqno = sy-tabix. " Assign Message sequence number

      MOVE-CORRESPONDING it_return TO ztmm_if014.
      MOVE: v_serno  TO ztmm_if014-serno,
            lv_seqno TO ztmm_if014-seqno.
      INSERT ztmm_if014.
      MOVE-CORRESPONDING it_return TO e_return.
      APPEND e_return.
    ENDLOOP.

*** Modification for Re-processing - Inserted by YWYANG
    UPDATE ztmm_if008 SET   type = 'E'
                      WHERE serno = v_serno.
    COMMIT WORK AND WAIT.
*** 2006/02/22 - End of insert
    EXIT.
  ENDIF.

*---// creating info record.
  PERFORM create_info_record.

  LOOP AT it_return.
    lv_seqno = sy-tabix. " Assign Message sequence number

    MOVE-CORRESPONDING it_return TO ztmm_if014.
    MOVE: v_serno  TO ztmm_if014-serno,
          lv_seqno TO ztmm_if014-seqno.
    INSERT ztmm_if014.
    MOVE-CORRESPONDING it_return TO e_return.
    APPEND e_return.
  ENDLOOP.
ENDFUNCTION.
