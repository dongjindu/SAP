FUNCTION zfpp_rcv_mes_seq.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  ZRESULT
*"     VALUE(E_MSG) TYPE  ZMSG2
*"  TABLES
*"      T_DATA STRUCTURE  ZSPP_MES_SEQ
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"      RESOURCE_FAILURE
*"----------------------------------------------------------------------

  DATA: lt_data LIKE TABLE OF ztpp_mes_seq WITH HEADER LINE,
        lt_temp LIKE TABLE OF zspp_mes_seq WITH HEADER LINE,
        lt_data_h LIKE TABLE OF ztpp_mes_seq_h WITH HEADER LINE.

  DATA: l_cn TYPE i,
        l_cn_check TYPE i,
        l_date_del TYPE sy-datum.

  DESCRIBE TABLE t_data LINES l_cn.

  IF l_cn = 0.
    MOVE: 'S' TO e_result,
          'No interface data' TO e_msg.
    EXIT.
  ENDIF.

  SORT t_data BY model_code body_no.
  lt_temp[] = t_data[].

  DELETE ADJACENT DUPLICATES FROM lt_temp
         COMPARING model_code body_no.
  DESCRIBE TABLE lt_temp LINES l_cn_check.
  IF l_cn <> l_cn_check.
    MOVE: 'E' TO e_result,
          'Duplicate records found' TO e_msg.
    EXIT.
  ENDIF.

  l_date_del = sy-datum - 30.

  SELECT * INTO TABLE lt_data
    FROM ztpp_mes_seq.

  IF sy-subrc = 0.
    LOOP AT lt_data.
      MOVE-CORRESPONDING lt_data to lt_data_h.
      lt_data_h-create_on = sy-datum.
      lt_data_h-create_time = sy-uzeit.
      APPEND lt_data_h.
    ENDLOOP.
    INSERT ztpp_mes_seq_h FROM TABLE lt_data_h
           ACCEPTING DUPLICATE KEYS .
     IF sy-subrc = 0 OR sy-subrc = 4.
      DELETE FROM ztpp_mes_seq.
      IF sy-subrc <> 0.
        MOVE:'E' TO e_result,
        'Table deletion error' TO e_msg.
        ROLLBACK WORK.
        EXIT.
      ENDIF.
    ELSE.
      MOVE:'E' TO e_result,
          'The history table update errro' TO e_msg.
      ROLLBACK WORK.
      EXIT.
    ENDIF.
  ENDIF.

  DELETE FROM ztpp_mes_seq_h WHERE create_on < l_date_del.
*  IF sy-subrc = 0.
  refresh lt_data.
  LOOP AT t_data.
    MOVE-CORRESPONDING t_data TO lt_data.
    APPEND lt_data.
  ENDLOOP.
  MODIFY ztpp_mes_seq FROM TABLE lt_data.
  IF sy-subrc = 0.
    MOVE: 'S'   TO e_result,
          space TO e_msg.
    COMMIT WORK.
  ELSE.
    MOVE: 'E' TO e_result,
          'Table Update Error' TO e_msg.
    ROLLBACK WORK.
  ENDIF.
*  ELSE.
*    MOVE:'E' TO e_result,
*         'History table deletion error' TO e_msg.
*    ROLLBACK WORK.
*  ENDIF.
ENDFUNCTION.
