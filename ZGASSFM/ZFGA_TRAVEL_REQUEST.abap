FUNCTION zfga_travel_request.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_MODE) TYPE  CHAR02
*"     VALUE(I_PERNR) TYPE  BAPI7004-PERNR OPTIONAL
*"     VALUE(I_APPROVER) TYPE  ZESS_EMP_WORK_FLOW OPTIONAL
*"     VALUE(I_YEAR) TYPE  CHAR04 OPTIONAL
*"     VALUE(I_REQSTAT) TYPE  ZSGA_BTREQUEST-REQUEST_STAT OPTIONAL
*"     VALUE(I_INDEX) TYPE  INT1 DEFAULT 1
*"     VALUE(I_REQUESTID) TYPE  ZSGA_BTREQUEST-REQUESTID OPTIONAL
*"     VALUE(I_SEQ) TYPE  ZSGA_BT_FILE-SEQ OPTIONAL
*"     VALUE(I_TEXT) TYPE  STRING OPTIONAL
*"     VALUE(I_GUBUN) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_CHECKOUT) TYPE  ZSGA_BTREQUEST-CHECKOUT_STAT OPTIONAL
*"     VALUE(I_FILE01) TYPE  ZSGA_BT_FILE OPTIONAL
*"     VALUE(I_FILE02) TYPE  ZSGA_BT_FILE OPTIONAL
*"     VALUE(I_FILE03) TYPE  ZSGA_BT_FILE OPTIONAL
*"     VALUE(I_FILE04) TYPE  ZSGA_BT_FILE OPTIONAL
*"     VALUE(I_FILE05) TYPE  ZSGA_BT_FILE OPTIONAL
*"     VALUE(I_FILE06) TYPE  ZSGA_BT_FILE OPTIONAL
*"     VALUE(I_FILE07) TYPE  ZSGA_BT_FILE OPTIONAL
*"     VALUE(I_FILE08) TYPE  ZSGA_BT_FILE OPTIONAL
*"     VALUE(I_FILE09) TYPE  ZSGA_BT_FILE OPTIONAL
*"     VALUE(I_FILE10) TYPE  ZSGA_BT_FILE OPTIONAL
*"     VALUE(I_FILE) TYPE  ZSGA_BT_FILE OPTIONAL
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"     VALUE(E_GUBUN) TYPE  CHAR01
*"     VALUE(E_TOTAL) TYPE  INT4
*"     VALUE(E_APP_DONE) TYPE  CHAR01
*"     VALUE(E_TEXT) TYPE  STRING
*"     VALUE(E_NEXT_EMAIL) TYPE  ZSGA_EMP_EMAIL
*"     VALUE(E_FILE01) TYPE  ZSGA_BT_FILE_LIST
*"     VALUE(E_FVAL) TYPE  XSTRING
*"  TABLES
*"      T_DATA STRUCTURE  ZSGA_BTREQUEST OPTIONAL
*"      T_DETAIL STRUCTURE  ZTGA_BTREQUEST_L OPTIONAL
*"      T_APPR STRUCTURE  ZSGA_BTAPPR_LINE OPTIONAL
*"      T_FILE STRUCTURE  ZSGA_BT_FILE_LIST OPTIONAL

*"----------------------------------------------------------------------
*TR : Travel Requet
*AP : Approval
*SL : Single Line
*GC : Confirmation
*CA : Cancel
*DL : Delete with temp. Saved
*FL : read one file
*CO : Update Actual Cost

*---Should delete I_GUBUN parameter
*  DATA : it_file LIKE ztga_bt_file OCCURS 0 WITH HEADER LINE.

  DATA : l_seq(2) TYPE n,
         l_file_nm(20).
  FIELD-SYMBOLS : <file01> TYPE any.

  CLEAR : v_conditions, it_btappr_line[], it_btappr_line,
           wa_btrequest, wa_btappr_line, v_error_flag.

  IF  i_mode IS INITIAL.
    e_return-type    = 'E'.
    e_return-message = 'Input Parameter is missing'.
    EXIT.
  ENDIF.

  CASE i_mode.
    WHEN 'TR'.  "Travel Request : Insert/Update

      LOOP AT t_data.

        PERFORM travel_request  TABLES t_detail t_appr t_file
                                CHANGING t_data e_return i_text e_text
                                         i_file01 i_file02 i_file03 i_file04
                                         i_file05 i_file06 i_file07 i_file08
                                         i_file09 i_file10 e_next_email.
        PERFORM update_table CHANGING e_return .
        MODIFY t_data.
      ENDLOOP.

    WHEN 'AP'.   "Approval /Reject
      IF i_pernr IS INITIAL OR i_reqstat   IS INITIAL
                            OR i_requestid IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Approver ID, Req.Status etc is missing'.
        EXIT.
      ENDIF.
      LOOP AT t_data.
        PERFORM update_bt_approve   TABLES  t_appr
                                   USING    i_pernr i_reqstat i_requestid
                                   CHANGING t_data  e_app_done
                                            e_next_email e_return.
      ENDLOOP.

    WHEN 'GC'. "Confirmation
      IF i_pernr IS INITIAL OR i_checkout IS INITIAL
                            OR i_requestid IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Confirm. ID or Checkout Status etc is missing'.
        EXIT.
      ENDIF.
      LOOP AT t_data.
        PERFORM update_btconfirm   USING t_data      i_pernr     i_checkout
                                         i_requestid e_return.
      ENDLOOP.

    WHEN 'CA'. "Cancel
      IF i_pernr IS INITIAL OR i_checkout IS INITIAL
                            OR i_requestid IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Checkout ID or Checkout Status etc is missing'.
        EXIT.
      ENDIF.
      LOOP AT t_data.
        PERFORM update_btcancel   USING t_data      i_pernr i_checkout
                                        i_requestid e_return.
      ENDLOOP.

    WHEN 'DL'.
      IF i_requestid IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Request ID  is missing'.
        EXIT.
      ENDIF.
      PERFORM delete_temp_saved USING i_requestid e_return.

   WHEN 'CO'.  "Update Actual Cost

      LOOP AT t_data.
        PERFORM Update_Actual_cost using t_data i_requestid e_return.
      ENDLOOP.

    WHEN 'FL'.  "Read One file
      IF i_requestid IS INITIAL AND i_seq IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Request ID or file Seq.  is missing'.
        EXIT.
      ENDIF.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF e_file01
      FROM ztga_bt_file
      WHERE requestid =  i_requestid
        AND seq       =  i_seq.

      SELECT SINGLE filevalue INTO e_fval
        FROM ztga_bt_file
      WHERE requestid =  i_requestid
        AND seq       =  i_seq.

    WHEN 'SL'.  "Single Line display
      IF i_requestid IS INITIAL OR i_pernr IS INITIAL.
        e_return-type    = 'E'.
        e_return-message = 'Request ID or Employee ID is missing'.
        EXIT.
      ENDIF.

*-header
      SELECT  SINGLE * INTO CORRESPONDING FIELDS OF  t_data
      FROM ztga_btrequest
      WHERE requestid =  i_requestid.
      IF sy-subrc = 0.
        SELECT SINGLE request_text INTO e_text
        FROM ztga_btrequest
        WHERE requestid =  i_requestid.

        CLEAR : it_emp_info[], it_emp_info.
        CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
          EXPORTING
            i_pernr = t_data-pernr
          TABLES
            t_data  = it_emp_info.

        READ TABLE it_emp_info INDEX 1.
        IF sy-subrc = 0.
          t_data-lastname   = it_emp_info-lastname.
          t_data-firstname  = it_emp_info-firstname.
          t_data-orgtx      = it_emp_info-orgtx.
        ENDIF.

        APPEND t_data.
        e_return = 'S'.

      ELSE.
        e_return-type    = 'E'.
        e_return-message = 'Request ID does not exist'.
        EXIT.
      ENDIF.

*-item
      SELECT  * INTO CORRESPONDING FIELDS OF TABLE t_detail
      FROM ztga_btrequest_l
      WHERE requestid =  i_requestid.

*-approval line
      SELECT * INTO CORRESPONDING FIELDS OF TABLE t_appr
      FROM ztga_btappr_line
      WHERE requestid =  i_requestid.
      LOOP AT t_appr.
        CLEAR : it_emp_info[], it_emp_info.
        CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
          EXPORTING
            i_pernr = t_appr-appr_id
          TABLES
            t_data  = it_emp_info.

        READ TABLE it_emp_info INDEX 1.
        IF sy-subrc = 0.
          t_appr-lastname   = it_emp_info-lastname.
          t_appr-firstname  = it_emp_info-firstname.
          t_appr-orgtx      = it_emp_info-orgtx.
        ENDIF.
        MODIFY t_appr.
      ENDLOOP.

*-Attach file
      SELECT * INTO CORRESPONDING FIELDS OF TABLE t_file
      FROM ztga_bt_file
      WHERE requestid =  i_requestid.

*      SELECT * INTO CORRESPONDING FIELDS OF i_file
*      FROM ztga_bt_file
*      WHERE requestid =  i_requestid.
*        CONCATENATE 'E_FILE' i_file-seq INTO l_file_nm.
*        ASSIGN (l_file_nm) TO <file01>.
*        <file01>  = i_file.
*
*      ENDSELECT.



**-cooperator
*      SELECT * INTO CORRESPONDING FIELDS OF TABLE t_coop
*      FROM  ztga_btcooper
*      WHERE requestid =  i_requestid.
*      LOOP AT t_coop.
*        CLEAR : it_emp_info[], it_emp_info.
*        CALL FUNCTION 'ZFGA_POOLCAR_EMP_INFO'
*          EXPORTING
*            i_pernr = t_coop-coopid
*          TABLES
*            t_data  = it_emp_info.
*
*        READ TABLE it_emp_info INDEX 1.
*        IF sy-subrc = 0.
*          t_coop-lastname   = it_emp_info-lastname.
*          t_coop-firstname  = it_emp_info-firstname.
*          t_coop-orgtx      = it_emp_info-orgtx.
*        ENDIF.
*        MODIFY t_coop.
*      ENDLOOP.

    WHEN OTHERS.
  ENDCASE.

ENDFUNCTION.
