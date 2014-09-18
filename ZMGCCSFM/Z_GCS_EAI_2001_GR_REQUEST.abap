FUNCTION z_gcs_eai_2001_gr_request.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      EAI_GR_REQUEST STRUCTURE  ZSMM_EAI_GR_REQUEST
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"----------------------------------------------------------------------
  DATA: l_lines TYPE i.

  it_gr_req[] = eai_gr_request[].
  IF NOT it_gr_req[] IS INITIAL.
*    LOOP AT it_gr_req.
*A__ BY PAUL
*      CONCATENATE it_gr_req-econt_no(3) it_gr_req-econt_no+4(7)
*                                      INTO it_gr_req-strg_bin.
*      MODIFY it_gr_req INDEX sy-tabix TRANSPORTING strg_bin.
*    ENDLOOP.
*    SELECT lgpla FROM lagp
*                 INTO TABLE it_lgpla
*                 FOR ALL ENTRIES IN it_gr_req
*                 WHERE lgpla = it_gr_req-strg_bin.
*    SORT it_lgpla BY lgpla.
*    DELETE ADJACENT DUPLICATES FROM it_lgpla COMPARING lgpla.
*-Create Storage BIN
*    PERFORM create_strg_bin.
*-Execute GR for Containers
    PERFORM create_gr.

    eai_gr_request[] = it_gr_req[].
    DELETE it_gr_req WHERE tait_targ_rslt = 'S'.
    DESCRIBE TABLE it_gr_req LINES l_lines.
    IF l_lines > 0.
      CLEAR   it_error.
      REFRESH it_error.
      MOVE: 'Container Number'  TO it_error-text(16),
            'Error Description' TO it_error-text+20(200).
      APPEND it_error.
      CLEAR it_error.

      LOOP AT it_gr_req.
       WRITE: it_gr_req-econt_no TO it_error-text(16) LEFT-JUSTIFIED,
                   it_gr_req-tait_targ_desc TO it_error-text+20(200).
        APPEND it_error.
        CLEAR it_error.
      ENDLOOP.
      PERFORM send_notification.
    ENDIF.
  ELSE.
    RAISE no_data_found.
    EXIT.
  ENDIF.
**S> 08/04/11 Paul : Add logic



ENDFUNCTION.
