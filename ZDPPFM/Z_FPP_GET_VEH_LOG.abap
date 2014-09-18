FUNCTION z_fpp_get_veh_log.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  CHAR01
*"     VALUE(E_MSG) TYPE  CHAR30
*"  TABLES
*"      T_DATA STRUCTURE  ZSPP_VEH_LOG OPTIONAL
*"----------------------------------------------------------------------
  DATA : it_status LIKE ztpp_status OCCURS 0 WITH HEADER LINE.
  DATA : it_save LIKE ztpp_veh_log  OCCURS 0 WITH HEADER LINE.
  DATA : BEGIN OF it_data OCCURS 0.
          INCLUDE STRUCTURE zspp_veh_log.
  DATA :    dup_chk,
          END OF it_data.

  DATA : l_error_flag,
         l_duplicate.

  CLEAR : g_error_flag, it_data[], it_data, it_save[], it_save.

  SELECT * INTO TABLE it_status
  FROM ztpp_status.

  LOOP AT t_data.
    CLEAR : l_error_flag.

*-Duplicate check
    PERFORM check_duplicate USING     t_data  l_error_flag
                            CHANGING  l_duplicate.

*-Vin Check
    PERFORM check_vin USING t_data  CHANGING l_error_flag.

*-Repointing point check
    PERFORM check_rp TABLES it_status USING t_data
                     CHANGING l_error_flag.

*-Line back check
    IF t_data-flag = 'LP'.
      PERFORM check_lineback USING  t_data CHANGING l_error_flag.
    ENDIF.


    IF l_duplicate <> 'X'.
      t_data-zsdat  = sy-datum.
      t_data-zstim  = sy-uzeit.
    ENDIF.

    IF t_data-zzret <> 'E'.
      t_data-zzret  = 'S'.
    ENDIF.

    MOVE-CORRESPONDING t_data TO it_data.

    IF l_duplicate = 'X'.
*      it_data-dup_chk = 'X'.
      UPDATE ztpp_veh_log
      SET zedat = it_data-zedat
          zetim = it_data-zetim
        WHERE flag      =  it_data-flag
          AND p_status  =  it_data-p_status
          AND p_rp_serial   = it_data-p_rp_serial
          AND p_model       = it_data-p_model
          AND p_body_serial = it_data-p_body_serial.
    else.
      MOVE-CORRESPONDING it_data TO it_save.
      APPEND it_save.

    ENDIF.

    MODIFY t_data.
*    APPEND it_data.
  ENDLOOP.

*  LOOP AT it_data.
*    IF it_data-dup_chk = 'X'.
*
*      UPDATE ztpp_veh_log
*      SET zedat = it_data-zedat
*          zetim = it_data-zetim
*        WHERE flag      =  it_data-flag
*          AND p_status  =  it_data-p_status
*          AND p_rp_serial   = it_data-p_rp_serial
*          AND p_model       = it_data-p_model
*          AND p_body_serial = it_data-p_body_serial.
*      IF sy-subrc <> 0.
**        g_error_flag = 'X'.
*      ENDIF.
*
*    ELSE.
*      MOVE-CORRESPONDING it_data TO it_save.
*      APPEND it_save.
*    ENDIF.
*  ENDLOOP.

*  IF g_error_flag IS INITIAL.
  if it_save[] is not initial.
    MODIFY ztpp_veh_log FROM TABLE it_save.
    IF sy-subrc <> 0.
      g_error_flag = 'X'.
    ENDIF.
  endif.
*  ENDIF.

*-Return message
  IF g_error_flag IS INITIAL.
    COMMIT WORK.
    e_result = 'S'.
    e_msg    = 'I/F Success'.
  ELSE.
    ROLLBACK WORK.
    e_result = 'E'.
    e_msg    = 'I/F Error'.
  ENDIF.
ENDFUNCTION.
