FUNCTION zrit_validation_user.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BNAME) TYPE  XUBNAME
*"     VALUE(I_EMAIL) TYPE  AD_SMTPADR
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  ZRESULT_WEB
*"     VALUE(E_MSG) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
  DATA: lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: l_name LIKE bapibname-bapibname,
*          lw_logondata LIKE bapilogond,
*        lw_logondatax LIKE bapilogonx,
        lw_address LIKE bapiaddr3,
        lw_bapislockd LIKE bapislockd.

  DATA: l_stat2 LIKE pa0000-stat2.
** check user validality
  SELECT SINGLE bname INTO l_name
      FROM usr02
      WHERE bname = i_bname.
*        AND ( gltgb = '00000000' OR
*            gltgb > sy-datum ).

  IF sy-subrc <> 0.
    e_result = '1'.
    e_msg = 'Invalid SAP User'.
    EXIT.
  ENDIF.

  IF i_bname+0(1) <> 'H'.
    SELECT SINGLE stat2 INTO l_stat2
           FROM pa0000
           WHERE pernr = i_bname
             AND endda = '99991231'.

    IF l_stat2 = 0.
      e_result = '1'.
      e_msg = 'Invalid HMMA User'.
      EXIT.
    ENDIF.
  ENDIF.

  l_name = i_bname.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username     = l_name
    IMPORTING
      address      = lw_address
*     COMPANY      =
*     SNC          =
*     REF_USER     =
*     ALIAS        =
*     UCLASS       =
*     LASTMODIFIED =
*     islocked     = lw_bapislockd
    TABLES
      return       = lt_return.

  READ TABLE lt_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    e_result = 'E'.
    e_msg = lt_return-message.
    EXIT.
  ENDIF.

  IF to_upper( lw_address-e_mail ) = to_upper( i_email ).
    e_result  = 'S'.
    e_msg = 'Successfully Reset'.
  ELSE.
    e_result = '2'.
    e_msg = 'Invalid email'.
  ENDIF.

ENDFUNCTION.
