FUNCTION ZFHR_ESS_GET_TRAVELADVANCE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) TYPE  BAPI7004-PERNR
*"     VALUE(YEAR) TYPE  PABRJ
*"     VALUE(MONTH) TYPE  PABRP
*"  TABLES
*"      IT_DATA STRUCTURE  ZSHR_EMP_TRAVELADVANCE
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 04/05/2011 VALERIAN   UD1K951284  Initial Program Development
*-----------------------------------------------------------------------

  CONSTANTS: c_cocode TYPE bukrs VALUE 'H201',
             c_paytyp_zp TYPE blart VALUE 'ZP',
             c_paytyp_kz TYPE blart VALUE 'KZ'.
.

  DATA: l_first_day TYPE sy-datum,
        l_last_day  TYPE sy-datum,
        l_lifnr     TYPE lifnr,
        l_stat2     TYPE pa0000-stat2.

  DATA: t_BSIK      TYPE BSIK OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_BSIK_pay OCCURS 0,
          BUDAT     TYPE BSIK-BUDAT,
          augbl     TYPE BSIK-augbl,
          shkzg     TYPE BSIK-shkzg,
          dmbtr     TYPE BSIK-dmbtr,
          sgtxt     TYPE BSIK-sgtxt,
        END OF t_BSIK_pay.

*  DATA: t_BSIK_fin LIKE t_BSIK_pay OCCURS 0 WITH HEADER LINE.

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

  REFRESH it_data.

* Get all relevant documents
  SELECT * INTO CORRESPONDING FIELDS OF TABLE t_bsik
    FROM bsik
   WHERE bukrs = c_cocode
     AND lifnr = l_lifnr
     AND BUDAT BETWEEN l_first_day AND l_last_day
     and UMSKZ = 'N'.

  IF sy-subrc = 0.
    SORT t_bsik BY augbl.

    LOOP AT t_bsik where blart = c_paytyp_kz or blart = c_paytyp_zp.
*      CHECK t_bsik-augbl = t_bsik-belnr.
      IF t_bsik-shkzg = 'H'.
        t_bsik-dmbtr = t_bsik-dmbtr * -1.
      ENDIF.

* Get and collect the payment documents
      MOVE-CORRESPONDING t_bsik TO t_bsik_pay.
*      CLEAR t_bsik_pay-sgtxt.
      t_bsik_pay-shkzg = 'S'.
      COLLECT t_bsik_pay.
    ENDLOOP.
    SORT t_bsik_PAY BY BUDAT augbl shkzg.
** Get the invoice documents
*    LOOP AT t_bsik_pay.
*      LOOP AT t_bsik WHERE augbl = t_bsik_pay-augbl.
*        CHECK t_bsik-augbl <> t_bsik-belnr.
*        IF t_bsik-shkzg = 'S'.
*          t_bsik-dmbtr = t_bsik-dmbtr * -1.
*        ENDIF.
*
*        MOVE-CORRESPONDING t_bsik TO t_bsik_fin.
*        t_bsik_fin-shkzg = 'H'.
*        APPEND t_bsik_fin.
*      ENDLOOP.
*    ENDLOOP.
*
** Combine the documents together
*    APPEND LINES OF t_bsik_pay TO t_bsik_fin.

*    SORT t_bsik_fin BY BUDAT augbl shkzg.

* Transfer result to output table
    LOOP AT t_bsik_PAY.
      MOVE-CORRESPONDING t_bsik_PAY TO it_data.
      APPEND iT_data.
    ENDLOOP.
  ENDIF.

* Give error message if no document is found
  IF iT_data[] IS INITIAL.
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
