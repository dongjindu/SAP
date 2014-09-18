FUNCTION ZFGA_REQUEST_LIST.
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
*"     VALUE(I_TYPE) TYPE  CHAR01
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"     VALUE(E_TOTAL) TYPE  INT4
*"  TABLES
*"      T_DATA STRUCTURE  ZSGA_REQUEST_LIST
*"      T_POOLCAR STRUCTURE  ZSGA_PCMASTER..
*"----------------------------------------------------------------------
*OB : Outbox
*IB : Inbox
*GL : Follow-up

  DATA : it_outbox LIKE STANDARD TABLE OF  ztga_pcrequest.

  CLEAR : v_conditions.

  IF  i_mode IS INITIAL.
    e_return-type    = 'E'.
    e_return-message = 'Input Parameter is missing'.
    EXIT.
  ENDIF.

  IF  i_type <> 'T' AND  i_type <> 'P'.
    e_return-type    = 'E'.
    CONCATENATE i_type  ':Input Parameter should be' `'T' or 'P'`
                          INTO e_return-message    SEPARATED BY space.
    EXIT.
  ENDIF.

  CASE i_mode.
    WHEN 'OB'.    "Outbox

      IF i_pernr IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Employee ID is missing'.
        EXIT.
      ENDIF.

      PERFORM select_outbox TABLES t_data
                            USING i_pernr i_year i_reqstat i_type.
      IF t_data[] IS INITIAL.
        e_return-type    = 'S'.
        e_return-message = 'There is No data with this condition'.
        EXIT.
      ELSE.
        PERFORM get_display_page_req TABLES t_data USING i_index
                                                   CHANGING e_total.
        e_return-type    = 'S'.
      ENDIF.

    WHEN 'IB'.   "Inbox
      IF  i_pernr IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Approver ID is missing'.
        EXIT.
      ENDIF.

      PERFORM select_inbox  TABLES t_data
                            USING  i_pernr i_year i_reqstat i_type.
      IF t_data[] IS INITIAL.
        e_return-type    = 'S'.
        e_return-message = 'There is No data with this condition'.
        EXIT.
      ELSE.
        PERFORM get_display_page_req TABLES t_data USING i_index
                                                   CHANGING e_total.
        e_return-type    = 'S'.
      ENDIF.

    WHEN 'GL'.    "Follow up
      IF i_pernr IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Employee ID is missing'.
        EXIT.
      ENDIF.

*-<only for GA team members
      CLEAR : it_emp_info[], it_emp_info.
      CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
        EXPORTING
          i_pernr = i_pernr
        TABLES
          t_data  = it_emp_info.

      READ TABLE it_emp_info INDEX 1.
      IF sy-subrc = 0.
        IF it_emp_info-orgeh =  '10004535'
          OR it_emp_info-orgeh =  '90002097'
          OR it_emp_info-orgeh =  '10025053'
          OR it_emp_info-orgeh =  '10004536'
          OR it_emp_info-orgeh =  '10013063'.
        ELSE.
          EXIT.
        ENDIF.

      ENDIF.
*->

      PERFORM select_followup TABLES t_data
                            USING i_pernr i_year i_checkout i_type.
      IF t_data[] IS INITIAL.
        e_return-type    = 'S'.
        e_return-message = 'There is No data with this condition'.
        EXIT.
      ELSE.
        PERFORM get_display_page_req TABLES t_data USING i_index
                                                   CHANGING e_total.
        e_return-type    = 'S'.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFUNCTION.
