FUNCTION z_fpp_get_ztppvr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      I_ZSPPVR STRUCTURE  ZSPPVR
*"  EXCEPTIONS
*"      SAPTABLE_FULL
*"      SAPTABLE_ERR_COMMIT
*"      SAPTABLE_ERR_ROLLBACK
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer    RequestNo    Description
*2004/12/08  Shiva       UD1K913430    Short dump when calling the bapi
*                                      'BAPI_PLANNEDORDER_CHANGE' ->
*                                       type conflict error.
*05/23/2005 Chris        UD1K916148    Allow RP26 to RP25 sequence. In
*                                      this case, move rp26 reporting
*                                      point to rp24 and update new re-
*                                      porting point rp25.
*06/13/2005 CHRIS        UD1K916447    Update the plan order open date
*                                      if the open date is later than
*                                      today for RP06
*07/29/2005 chris        UD1K917093    1. Delete alclog1 and alclog2
*                                      2. change the plan order start
*                                        and finish date to shop_date.
*03/13/2014 Victor       UD1K959902    1.Added check logic for 'B02'
*                                                          and 'B08'
*&----------------------------------------------------------------------

  DATA: l_bapiret2  LIKE bapiret2    ,
        l_flag      TYPE c,
        l_k04ser(5) TYPE n,
        l_index     TYPE i,
        l_error     TYPE c,
        l_errveh    LIKE ztpp_error_car-err_veh .
*        l_num(5) TYPE n VALUE '00001'.
  DATA: l_lastid LIKE zsppvr-p_vin,
        l_worder LIKE zsppvr-p_work_order,
        l_objek LIKE ausp-objek.
*------> Check PP Log
  CLEAR : ztpp_if_status .
  SELECT SINGLE *
              FROM ztpp_if_status
              WHERE tabname EQ 'ZTPPVR'  .
  IF ztpp_if_status-zgo EQ 'X' .
    i_zsppvr-zzret = 'E' .
    MODIFY i_zsppvr TRANSPORTING zzret WHERE zzret EQ space .
  ENDIF.
  CHECK ztpp_if_status-zgo NE 'X' .
*
**------> MOVE INBOUNDED TABLE TO ITAB
  LOOP AT i_zsppvr .
    CLEAR: l_bapiret2,  l_error,  l_errveh.

    " Check the Inconsystency of ALC Log Data.. (Skip the Success REC.)
    SELECT SINGLE zresult INTO i_zsppvr-zzret
      FROM ztppvr
     WHERE k04pdat = i_zsppvr-k04pdat
       AND k04ser  = i_zsppvr-k04ser  .
    IF sy-subrc = 0 AND i_zsppvr-zzret = 'S'.
      MODIFY i_zsppvr .
      CONTINUE.
    ENDIF.
*** Insert Logic requested by JH Shin. With confirmed by MH Moon.
*** Date: 12/02/2004.   Changed by Bobby.
    CLEAR: i_zsppvr-zzret, l_flag, wa_sdback, wa_skip.
    l_index  = l_index + 1 .
*** Insert Logic requested by JH Shin. With confirmed by MH Moon.
*** Date: 12/02/2004.   Changed by Bobby.

    wa_zsppvr = i_zsppvr.
    wa_zsdat = sy-datum.             "START DATE
    wa_zstim = sy-uzeit.             "START TIME
    wa_index = l_index .
    REFRESH: it_char, it_vin, it_alclog1, it_alclog2, it_message.
    CLEAR  : it_char, it_vin, it_alclog1, it_alclog2, it_message,
             wa_subrc, l_k04ser, it_wosum, l_flag, wa_skip, wa_check.

    CONCATENATE wa_zsppvr-p_work_order  wa_zsppvr-p_ext_color
                wa_zsppvr-p_int_color   INTO wa_matnr        .
    CONCATENATE wa_zsppvr-p_model wa_zsppvr-p_body_serial
                INTO wa_equipment.

*** Insert Logic requested by BW Park ... Date 12/05/2004.
*   " Check the Transparent Table's Full : L_ERROR Flag.
*    IF l_error = 'X'.
*      RAISE saptable_full.
*    ENDIF.

*   " Check the Error Vehicle
    SELECT SINGLE err_veh  INTO l_errveh
      FROM ztpp_error_car
     WHERE err_veh = wa_equipment.

    IF sy-subrc = 0 .
      PERFORM check_retry  USING l_flag .
      IF l_flag = 'E'.
        i_zsppvr-zzret = 'E'.
        wa_zsppvr = i_zsppvr.
        wa_zsppvr-zresult = 'E'.
        it_message-msg = 'Error Vehicle Car...'.
        MODIFY i_zsppvr .
        PERFORM update_ztppvr USING l_error   .

        IF l_error = 'X'.
          " Message Write...
          RAISE saptable_err_commit .
        ENDIF.
        CONTINUE.
      ENDIF.
    ENDIF.
*** End Log requested by BW Park.  Changed by Bobby.

    PERFORM check_error_condition  USING l_flag  l_error.
    IF l_error = 'X'.  RAISE saptable_full.  ENDIF.

    IF l_flag = 'X'.
      PERFORM save_error_car  USING wa_equipment l_error.
      i_zsppvr-zzret = 'E'.
      wa_zsppvr = i_zsppvr.
      wa_zsppvr-zresult = 'E'.
      MODIFY i_zsppvr .
      PERFORM update_ztppvr USING l_error   .
      IF l_error = 'X'.  RAISE saptable_full.  ENDIF.
      CONTINUE     .
    ENDIF.

    IF l_flag = 'S' or l_flag = 'E'.
      CONTINUE.
    ENDIF.

    CASE wa_zsppvr-flag.
      WHEN 'LT'.
        IF wa_zsppvr-p_status <> 'B02' "Victor 03.13.2014
          AND wa_zsppvr-p_status <> 'B08'.
*PROGRESS_REPORTING
          PERFORM progress_reporting.
          PERFORM table_update.
        ENDIF.
      WHEN 'LS'.
** Changed by Furong on 11/23/09

        CLEAR: l_lastid, l_worder, l_objek.

** Changed by Furong on 12/03/09
*        CONCATENATE I_ZSPPVR-P_WORK_ORDER I_ZSPPVR-P_DEST_CODE
*             INTO L_OBJEK.
        l_objek = i_zsppvr-p_work_order.
** End of change

        SELECT SINGLE atwrt INTO l_worder
         FROM ausp AS a
         INNER JOIN cabn AS b
         ON a~atinn = b~atinn
         WHERE objek = l_objek
           AND klart = '001'
           AND atnam = 'P_VIN_SPEC'.

        CALL FUNCTION 'Z_FPP_VIN_CHECK'
          EXPORTING
            w_order   = l_worder
            mode      = i_zsppvr-p_model
            body_no   = i_zsppvr-p_body_serial
          IMPORTING
            p_lastid  = l_lastid
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        IF l_lastid <> i_zsppvr-p_vin.
          i_zsppvr-zzret = 'E'.
          i_zsppvr-zresult = 'E'.
          i_zsppvr-zmsg  = 'VIN No Incorrect'.
          wa_zsppvr-zresult = 'E'.
          it_message-msg = 'VIN No Incorrect'.
          MODIFY i_zsppvr .
          PERFORM update_ztppvr  USING l_error.
          IF l_error = 'X'.  RAISE saptable_full.  ENDIF.
          CONTINUE.
        ELSE.
** End of change
*SPEC CHANGE
          PERFORM ls_function.
        ENDIF.
*        PERFORM ztpp_alclog1 USING 'POINT18'.
      WHEN 'LP'.
*PROGRESS_CHANGE
** on 02/13/13
        IF wa_status-id NE 'V01'.
          PERFORM lp_or_lm_function.
        ENDIF.
*        PERFORM LP_OR_LM_FUNCTION.
** End on 02/13/13
*        PERFORM ztpp_alclog1 USING 'POINT19'.
      WHEN 'LM'.
*MITU.
        PERFORM lp_or_lm_function.
*        PERFORM ztpp_alclog1 USING 'POINT17'.
    ENDCASE.

    IF wa_subrc = ' '.
      i_zsppvr-zzret = 'S'.
      wa_zsppvr-zresult = 'S'.
      MODIFY i_zsppvr .
      PERFORM update_ztppvr USING l_error   .
      PERFORM delete_errorcar .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = l_bapiret2.

      IF l_bapiret2-type = 'E' OR l_bapiret2-type = 'A'.
        RAISE saptable_err_rollback.
      ENDIF.

      i_zsppvr-zzret = 'E'.
      i_zsppvr-zmsg  = it_message-msg.
      wa_zsppvr-zresult = 'E'.
      MODIFY i_zsppvr .
      IF l_errveh NE wa_equipment.
        PERFORM save_error_car  USING wa_equipment l_error.
      ENDIF.
      PERFORM update_ztppvr  USING l_error.
      IF l_error = 'X'.  RAISE saptable_full.  ENDIF.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = l_bapiret2.

    IF   wa_zsppvr-flag = 'LT' AND
       ( wa_status-id = 'V05' OR wa_status-id = 'V07' ) .
      PERFORM call_sd_shipout USING wa_equipment .
    ENDIF.
    CLEAR: wa_subrc.
  ENDLOOP.

** Changed by Furong on 10/01/08 for APS II
*  DATA: L_ERDAT LIKE SY-DATUM.
*  SELECT SINGLE ERDAT INTO L_ERDAT
*    FROM ZTPP_CHANGE
*   WHERE ERDAT = SY-DATUM
*     AND CFLAG = 'S'.
** Changed by Furong on 10/24/08
*  READ TABLE I_ZSPPVR WITH KEY FLAG = 'LS'.
*  IF SY-SUBRC = 0.
*    SUBMIT ZIPP112I_APS_3AA1_2 WITH P_DATE = SY-DATUM AND RETURN.
*  ENDIF.
** End of change on 10/24/08
** End of change ON 10/01/08
ENDFUNCTION.
