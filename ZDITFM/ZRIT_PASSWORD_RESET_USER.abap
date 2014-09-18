FUNCTION zrit_password_reset_user.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BNAME) TYPE  XUBNAME
*"     VALUE(I_PASSWORD) TYPE  XUNCODE
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  ZRESULT
*"     VALUE(E_MSG) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
  DATA: lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: l_name LIKE bapibname-bapibname,
        lw_password LIKE bapipwd,
        lw_passwordx LIKE bapipwdx,
        lw_logondata LIKE bapilogond,
        lw_logondatax LIKE bapilogonx.

  DATA: l_stat2 LIKE pa0000-stat2.

  IF sy-uname <> 'RFCESS' AND sy-uname <> 'HIS20037'.
    e_result = 'E'.
    e_msg = 'Invalid User'.
    EXIT.
  ENDIF.
** check user validality
  SELECT SINGLE bname INTO l_name
      FROM usr02
      WHERE bname = i_bname.
*        AND ( gltgb = '00000000' OR
*            gltgb > sy-datum ).

  IF sy-subrc <> 0.
    e_result = 'E'.
    e_msg = 'Invalid SAP User'.
    EXIT.
  ENDIF.

  IF i_bname+0(1) <> 'H'.
    SELECT SINGLE stat2 INTO l_stat2
           FROM pa0000
           WHERE pernr = i_bname
             AND endda = '99991231'.

    IF l_stat2 = 0.
      e_result = 'E'.
      e_msg = 'Invalid HMMA User'.
      EXIT.
    ENDIF.
  ENDIF.

  l_name = i_bname.

  lw_password-bapipwd = i_password.
  lw_passwordx-bapipwd = 'X'.

  CLEAR: lw_logondata-gltgv,
         lw_logondata-gltgb.

  lw_logondatax-gltgb = 'X'.
  lw_logondatax-gltgv = 'X'.

  CALL FUNCTION 'BAPI_USER_CHANGE'
    EXPORTING
      username   = l_name
      logondata  = lw_logondata
      logondatax = lw_logondatax
      password   = lw_password
      passwordx  = lw_passwordx
    TABLES
      return     = lt_return.

  READ TABLE lt_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    e_result = 'E'.
    e_msg = lt_return-message.
    ROLLBACK WORK.
  ELSE.
    e_result  = 'S'.
    e_msg = 'Successfully Reset'.
    COMMIT WORK.

    REFRESH lt_return.
    CALL FUNCTION 'BAPI_USER_UNLOCK'
      EXPORTING
        username = l_name
      TABLES
        return   = lt_return.
  ENDIF.

ENDFUNCTION.
