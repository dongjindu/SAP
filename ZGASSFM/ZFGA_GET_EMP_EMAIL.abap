FUNCTION zfga_get_emp_email.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_PERNR) TYPE  BAPI7004-PERNR
*"     VALUE(I_SUBTY) TYPE  SUBTY DEFAULT '0010'
*"     VALUE(I_ENDDA) TYPE  ENDDA DEFAULT '99991231'
*"     VALUE(I_REQUESTID) TYPE  ZTGA_PCREQUEST-REQUESTID OPTIONAL
*"  EXPORTING
*"     VALUE(E_DATA) TYPE  ZSGA_EMP_EMAIL
*"     VALUE(E_RETURN) TYPE  BAPIRETURN
*"----------------------------------------------------------------------
* 04.16.2013 Business Travel : only use this func. for Input TM information
*            Pool Car : Use this func. for input TM info and Approver both
*                       I_REQUESTID -> read from Pool car table

  DATA : l_confirm_id LIKE ztga_pcrequest-confirm_id.

  CLEAR : it_emp_info[], it_emp_info.
  CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
    EXPORTING
      i_pernr = i_pernr
    TABLES
      t_data  = it_emp_info.

  READ TABLE it_emp_info INDEX 1.
  IF sy-subrc = 0.
    e_data-pernr      = i_pernr.
    e_data-lastname   = it_emp_info-lastname.
    e_data-firstname  = it_emp_info-firstname.
    e_data-orgtx      = it_emp_info-orgtx.
  ENDIF.

  SELECT SINGLE usrid_long INTO e_data-email
  FROM pa0105
  WHERE pernr = i_pernr
    AND subty = i_subty
    AND endda = i_endda.

  IF i_requestid IS NOT INITIAL.  "Only Pool Car case
    SELECT  SINGLE * INTO  wa_request
    FROM  ztga_pcrequest
    WHERE requestid = i_requestid.
    IF sy-subrc = 0.
      CLEAR : it_emp_info[], it_emp_info.
      CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
        EXPORTING
          i_pernr = wa_request-hod_approval_id
        TABLES
          t_data  = it_emp_info.
      READ TABLE it_emp_info INDEX 1.
      IF sy-subrc = 0.
        e_data-appr_id        = it_emp_info-pernr.
        CONCATENATE it_emp_info-lastname it_emp_info-firstname
                      INTO e_data-appr_lastname SEPARATED BY space.
        e_data-appr_orgtx     = it_emp_info-orgtx.

        SELECT SINGLE usrid_long INTO e_data-appr_email
        FROM pa0105
        WHERE pernr = e_data-appr_id
          AND subty = i_subty
          AND endda = i_endda.
      ENDIF.
    ENDIF.

  ELSE.
    CLEAR : it_work_flow[], it_work_flow.
    CALL FUNCTION 'Z_HR_ESS_GET_WORKFLOW'
      EXPORTING
        employee_number    = i_pernr
        datum              = sy-datum
      TABLES
        zess_emp_work_flow = it_work_flow
        return             = it_return.
    READ TABLE it_work_flow INDEX 1.
    IF sy-subrc = 0.
      e_data-appr_id        = it_work_flow-hod_pernr.
      e_data-appr_lastname  = it_work_flow-hod_name.
      e_data-appr_orgtx     = it_work_flow-hod_org_name.

      SELECT SINGLE usrid_long INTO e_data-appr_email
      FROM pa0105
      WHERE pernr = e_data-appr_id
        AND subty = i_subty
        AND endda = i_endda.
    ENDIF.
  ENDIF.

  IF wa_request-confirm_id IS NOT INITIAL.
    CLEAR : it_emp_info[], it_emp_info.
    CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
      EXPORTING
        i_pernr = wa_request-confirm_id
      TABLES
        t_data  = it_emp_info.

    READ TABLE it_emp_info INDEX 1.
    IF sy-subrc = 0.
      CONCATENATE  it_emp_info-firstname  it_emp_info-lastname
                     INTO e_data-confirm_nm SEPARATED BY space.
    ENDIF.

  ENDIF.

ENDFUNCTION.
