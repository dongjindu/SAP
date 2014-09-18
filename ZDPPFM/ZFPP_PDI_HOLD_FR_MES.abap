FUNCTION zfpp_pdi_hold_fr_mes.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  ZRESULT
*"     VALUE(E_MSG) TYPE  ZMSG2
*"  TABLES
*"      T_DATA STRUCTURE  ZSPP_PDI_HOLD
*"----------------------------------------------------------------------

  DATA: it_data LIKE TABLE OF ztpp_delay_if WITH HEADER LINE,
        it_data_h LIKE TABLE OF ztpp_delay_if_h WITH HEADER LINE.

  DATA: l_cn TYPE i,
        l_date_del TYPE sy-datum.

  DESCRIBE TABLE t_data LINES l_cn.

  IF l_cn = 0.
    MOVE: 'S' TO e_result,
          'No interface data' TO e_msg.
    EXIT.
  ENDIF.

  PERFORM locking_rtn USING sy-repid e_result.

  IF e_result = 'E'.
    MOVE: 'Previous I/F was not finished' TO e_msg.
    EXIT.
  ENDIF.

  SORT t_data BY model_code body_no zldat zlzet.

  LOOP AT t_data.
    MOVE-CORRESPONDING t_data TO it_data.
    CONCATENATE t_data-zldat t_data-zlzet INTO it_data-zldatim.

    select single rp_point into it_data-rp_status
      from ztpp_status
     where id = t_data-tp_status.

    it_data-zsystem = 'PDI HOLD'.
    it_data-zuser = sy-uname.
    it_data-zedat = sy-datum.
    it_data-zetim = sy-uzeit.
    it_data-zresult = 'I'.

    MOVE-CORRESPONDING t_data TO it_data_h.
    CONCATENATE t_data-zldat t_data-zlzet INTO it_data_h-zldatim.
    it_data_h-rp_status = it_data-rp_status.
    it_data_h-zsystem   = 'PDI HOLD'.
    it_data_h-zuser     = sy-uname.
    it_data_h-zedat     = sy-datum.
    it_data_h-zetim     = sy-uzeit.
    it_data_h-zresult   = 'I'.

    APPEND it_data.
    APPEND it_data_h.
  ENDLOOP.

  l_date_del = sy-datum - 90.
  DELETE FROM ztpp_delay_if_h WHERE zedat < l_date_del.
  INSERT ztpp_delay_if_h FROM TABLE it_data_h
                        ACCEPTING DUPLICATE KEYS .
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MODIFY ztpp_delay_if FROM TABLE it_data.
    IF sy-subrc = 0.
      MOVE: 'S'   TO e_result,
            space TO e_msg.
      COMMIT WORK.
    ELSE.
      MOVE: 'E' TO e_result,
            'Update failed for PDI Hold' TO e_msg.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    MOVE:'E' TO e_result,
         'Update failed for PDI Hold history' TO e_msg.
    ROLLBACK WORK.
  ENDIF.
ENDFUNCTION.
