FUNCTION z_hr_ess_get_reimbursement.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) TYPE  BAPI7004-PERNR
*"     VALUE(YEAR) TYPE  PABRJ
*"     VALUE(MONTH) TYPE  PABRP
*"  TABLES
*"      REIMBURSEMENT STRUCTURE  ZESS_EMP_REIMBURSEMENT
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 04/05/2011 VALERIAN   UD1K951284  Initial Program Development
*-----------------------------------------------------------------------

  CONSTANTS: c_cocode TYPE bukrs VALUE 'H201',
             c_paytyp TYPE blart VALUE 'ZP'.

  DATA: l_first_day TYPE sy-datum,
        l_last_day  TYPE sy-datum,
        l_lifnr     TYPE lifnr,
        l_stat2     TYPE pa0000-stat2.

  DATA: t_bsak      TYPE bsak OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_bsak_pay OCCURS 0,
          augdt     TYPE bsak-augdt,
          augbl     TYPE bsak-augbl,
          shkzg     TYPE bsak-shkzg,
          dmbtr     TYPE bsak-dmbtr,
          sgtxt     TYPE bsak-sgtxt,
        END OF t_bsak_pay.

  DATA: t_bsak_fin LIKE t_bsak_pay OCCURS 0 WITH HEADER LINE.

* Check if the employee is still active.
  CLEAR l_stat2.
  SELECT stat2 INTO l_stat2
    FROM pa0000
   UP TO 1 ROWS
   WHERE pernr = employee_number
     AND endda = '99991231'.
  ENDSELECT.

  IF l_stat2 = '0'.
    return-type = 'E'.
    return-code = '5F'.
    return-log_msg_no = '739'.

    MESSAGE ID return-code TYPE return-type NUMBER return-log_msg_no
            WITH return-message_v1 return-message_v2
                 return-message_v3 return-message_v4
       INTO return-message.
    APPEND return.
    EXIT.
  ENDIF.

  UNPACK employee_number TO l_lifnr.

  CONCATENATE year month '01' INTO l_first_day.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = l_first_day
       IMPORTING
            last_day_of_month = l_last_day
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.

  IF sy-subrc <> 0.
    return-type = sy-msgty.
    return-code = sy-msgid.
    return-log_msg_no = sy-msgno.
    return-message_v1 = sy-msgv1.
    return-message_v2 = sy-msgv2.
    return-message_v3 = sy-msgv3.
    return-message_v4 = sy-msgv4.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
       INTO return-message.
    APPEND return.
    EXIT.
  ENDIF.

  REFRESH reimbursement.

* Get all relevant documents
  SELECT * INTO CORRESPONDING FIELDS OF TABLE t_bsak
    FROM bsak
   WHERE bukrs = c_cocode
     AND lifnr = l_lifnr
     AND augdt BETWEEN l_first_day AND l_last_day.

  IF sy-subrc = 0.
    SORT t_bsak BY augbl.

    LOOP AT t_bsak WHERE blart = c_paytyp.
      CHECK t_bsak-augbl = t_bsak-belnr.
      IF t_bsak-shkzg = 'H'.
        t_bsak-dmbtr = t_bsak-dmbtr * -1.
      ENDIF.

* Get and collect the payment documents
      MOVE-CORRESPONDING t_bsak TO t_bsak_pay.
      CLEAR t_bsak_pay-sgtxt.
      t_bsak_pay-shkzg = 'S'.
      COLLECT t_bsak_pay.
    ENDLOOP.

* Get the invoice documents
    LOOP AT t_bsak_pay.
      LOOP AT t_bsak WHERE augbl = t_bsak_pay-augbl.
        CHECK t_bsak-augbl <> t_bsak-belnr.
        IF t_bsak-shkzg = 'S'.
          t_bsak-dmbtr = t_bsak-dmbtr * -1.
        ENDIF.

        MOVE-CORRESPONDING t_bsak TO t_bsak_fin.
        t_bsak_fin-shkzg = 'H'.
        APPEND t_bsak_fin.
      ENDLOOP.
    ENDLOOP.

* Combine the documents together
    APPEND LINES OF t_bsak_pay TO t_bsak_fin.

    SORT t_bsak_fin BY augdt augbl shkzg.

* Transfer result to output table
    LOOP AT t_bsak_fin.
      MOVE-CORRESPONDING t_bsak_fin TO reimbursement.
      APPEND reimbursement.
    ENDLOOP.
  ENDIF.

* Give error message if no document is found
  IF reimbursement[] IS INITIAL.
    return-type = 'E'.
    return-code = '4B'.
    return-log_msg_no = '083'.

    MESSAGE ID return-code TYPE return-type NUMBER return-log_msg_no
            WITH return-message_v1 return-message_v2
                 return-message_v3 return-message_v4
       INTO return-message.
    APPEND return.
    EXIT.
  ENDIF.

ENDFUNCTION.
