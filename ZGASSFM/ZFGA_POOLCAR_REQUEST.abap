FUNCTION zfga_poolcar_request.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_MODE) TYPE  CHAR02
*"     VALUE(I_PERNR) TYPE  BAPI7004-PERNR OPTIONAL
*"     VALUE(I_APPROVER) TYPE  ZESS_EMP_WORK_FLOW OPTIONAL
*"     VALUE(I_YEAR) TYPE  CHAR04 OPTIONAL
*"     VALUE(I_REQSTAT) TYPE  ZTGA_PCREQUEST-REQUEST_STAT OPTIONAL
*"     VALUE(I_INDEX) TYPE  INT1 DEFAULT 1
*"     VALUE(I_CHECKOUT) TYPE  ZTGA_PCREQUEST-CHECKOUT_STAT OPTIONAL
*"     VALUE(I_REQUESTID) TYPE  ZTGA_PCREQUEST-REQUESTID OPTIONAL
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"     VALUE(E_GUBUN) TYPE  CHAR01
*"     VALUE(E_TOTAL) TYPE  INT4
*"  TABLES
*"      T_DATA STRUCTURE  ZSGA_PCREQUEST
*"      T_POOLCAR STRUCTURE  ZSGA_PCMASTER..
*"----------------------------------------------------------------------
*PR : Pool Car Requet
*OB : Outbox
*IB : Inbox
*RA : Pool Car Request Approval
*GL : Follow-up
*GC : Confirmation
*CO : Checkout
*CI : Checkin
*SL : Single Line
*CA : Cancel

  DATA : it_outbox LIKE STANDARD TABLE OF  ztga_pcrequest.

  CLEAR : v_conditions.

*  e_gubun = 'P'. "Pool car request


  IF  i_mode IS INITIAL.
    e_return-type    = 'E'.
    e_return-message = 'Input Parameter is missing'.
    EXIT.
  ENDIF.

  CASE i_mode.
    WHEN 'PR'.  "Pool Car Request : Insert/Update

      LOOP AT t_data.
*-      Check logic

        PERFORM poolcar_request  CHANGING t_data e_return.
        MODIFY t_data.
      ENDLOOP.

    WHEN 'RA'.   "Pool Car Request Approval
      IF i_pernr IS INITIAL OR i_reqstat IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Approver ID or Req.Status is missing'.
        EXIT.
      ENDIF.
      LOOP AT t_data.
        PERFORM update_approve   USING t_data i_pernr i_reqstat e_return.
      ENDLOOP.

    WHEN 'GC'. "Confirmation
      IF i_pernr IS INITIAL OR i_checkout IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Confirm. ID or Checkout Status is missing'.
        EXIT.
      ENDIF.
      LOOP AT t_data.
        PERFORM update_confirm   USING t_data i_pernr i_checkout e_return.
      ENDLOOP.

    WHEN 'CO'. "Checkout
      IF i_pernr IS INITIAL OR i_checkout IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Checkout ID or Checkout Status is missing'.
        EXIT.
      ENDIF.
      LOOP AT t_data.
        PERFORM update_checkout   USING t_data i_pernr i_checkout e_return.
      ENDLOOP.

    WHEN 'CA'. "Cancel
      IF i_pernr IS INITIAL OR i_checkout IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Checkout ID or Checkout Status is missing'.
        EXIT.
      ENDIF.
      LOOP AT t_data.
        PERFORM update_cancel   USING t_data i_pernr i_checkout e_return.
      ENDLOOP.

    WHEN 'CI'. "Checkin
      IF i_pernr IS INITIAL OR i_checkout IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Checkin ID or Checkout Status is missing'.
        EXIT.
      ENDIF.
      LOOP AT t_data.
        PERFORM update_checkin   USING t_data i_pernr i_checkout e_return.
      ENDLOOP.

    WHEN 'SL'.  "Single Line display
      IF i_requestid IS INITIAL OR i_pernr IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Request ID or Employee ID is missing'.
        EXIT.
      ENDIF.

      SELECT  SINGLE * INTO CORRESPONDING FIELDS OF  t_data
      FROM ztga_pcrequest
      WHERE requestid =  i_requestid.
      IF sy-subrc = 0.
        CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
          EXPORTING
            i_pernr = t_data-pernr
          TABLES
            t_data  = it_emp_info.

        READ TABLE it_emp_info INDEX 1.
        IF sy-subrc = 0.
          t_data-lastname   = it_emp_info-lastname.
          t_data-firstname  = it_emp_info-firstname.
          t_data-depart_id  = it_emp_info-orgeh.
          t_data-orgtx      = it_emp_info-orgtx.
        ENDIF.

        CLEAR : it_emp_info[], it_emp_info.
        CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
          EXPORTING
            i_pernr = t_data-hod_approval_id
          TABLES
            t_data  = it_emp_info.
        READ TABLE it_emp_info INDEX 1.
        IF sy-subrc = 0.
          t_data-appr_lastname   = it_emp_info-lastname.
          t_data-appr_firstname  = it_emp_info-firstname.
          t_data-appr_orgtx      = it_emp_info-orgtx.
        ENDIF.

*-the newest MVR date
        CLEAR : t_data-mvr_dt.
        SELECT MAX( mvr_dt ) INTO t_data-mvr_dt
        FROM ztga_pcrequest
        WHERE pernr =   t_data-pernr.

        APPEND t_data.

*-Pool car info
        IF t_data-poolcarid IS NOT INITIAL.
          SELECT SINGLE * INTO CORRESPONDING FIELDS OF wa_master
          FROM ztga_pcmaster
          WHERE poolcarid =  t_data-poolcarid.

          SELECT  * INTO CORRESPONDING FIELDS OF  wa_detail
          FROM ztga_pcmaster_l
           UP TO 1 ROWS
          WHERE   poolcarid =  t_data-poolcarid
          ORDER BY zdate DESCENDING zseq DESCENDING.
          ENDSELECT.

          MOVE-CORRESPONDING wa_master TO t_poolcar.
          MOVE-CORRESPONDING wa_detail TO t_poolcar.

          APPEND t_poolcar.
        ELSE.
          PERFORM select_poolcar    TABLES t_poolcar CHANGING e_total.
        ENDIF.

        e_return = 'S'.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFUNCTION.
