FUNCTION zmmf_if_goods_receipt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_GR_HEADER) LIKE  ZSMM_IF003 STRUCTURE  ZSMM_IF003
*"  TABLES
*"      T_GR_ITEM STRUCTURE  ZSMM_IF004
*"      E_RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
  DATA: lv_seqno like ztmm_if013-e_seqno.

  CLEAR: v_header.
  CLEAR: it_item, it_data.
  CLEAR: it_return, e_return, ztmm_if013.
  CLEAR: it_crt_item, it_ret_item.

  REFRESH: it_item, it_data.
  REFRESH: it_return, e_return.
  REFRESH: it_crt_item, it_ret_item.

  MOVE: i_gr_header TO v_header.
  MOVE: t_gr_item[] TO it_item[].

*---< Importing item table changed adding flag for create and return
*---// Changed by YWYANG
*---// Interface data type conversion
  PERFORM apply_conversion_rule.

*---// Separate items depending on the flag 'C' for create
*---// and 'D' for return
  PERFORM save_if_table.

  IF e_return-type = 'E'. " Number Range Error Exit
    EXIT.
  ENDIF.
*---// Imported value existence check with further processing
  PERFORM check_parameters. " Check header and item parameters

  READ TABLE it_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    LOOP AT it_return.
      lv_seqno = sy-tabix. " Assign Message sequence number

      MOVE-CORRESPONDING it_return TO ztmm_if013.
      MOVE: v_serno  TO ztmm_if013-serno,
            lv_seqno TO ztmm_if013-e_seqno.
      INSERT ztmm_if013.
      MOVE-CORRESPONDING it_return TO e_return.
      APPEND e_return.
    ENDLOOP.

*** Modification for Re-processing - Inserted by YWYANG
    UPDATE ztmm_if012 SET   type = 'E'
                      WHERE serno = v_serno.
    COMMIT WORK AND WAIT.
*** 2006/02/22 - End of insert
    EXIT.
  ENDIF.
*---> 051219 end of change
*---// Call function create and return Goods Receipt
  PERFORM goods_receipt_perform.

  LOOP AT it_return.
    lv_seqno = sy-tabix. " Assign Message sequence number

    MOVE-CORRESPONDING it_return TO ztmm_if013.
    MOVE: v_serno  TO ztmm_if013-serno,
          lv_seqno TO ztmm_if013-e_seqno.
    INSERT ztmm_if013.
    MOVE-CORRESPONDING it_return TO e_return.
    APPEND e_return.
  ENDLOOP.
ENDFUNCTION.
