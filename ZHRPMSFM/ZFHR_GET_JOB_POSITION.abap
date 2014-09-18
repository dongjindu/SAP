FUNCTION zfhr_get_job_position.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_PERNR) TYPE  PERSNO
*"     REFERENCE(I_BEGDA) TYPE  HAP_AP_START_DATE OPTIONAL
*"     REFERENCE(I_ENDDA) TYPE  HAP_AP_END_DATE OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_PLANS) TYPE  PLANS
*"     REFERENCE(E_STELL) TYPE  STELL
*"     REFERENCE(E_ORGEH) TYPE  ORGEH
*"     REFERENCE(E_KEYDT) TYPE  SY-DATUM
*"----------------------------------------------------------------------

  DATA: l_keydt   TYPE sy-datum.

  CHECK i_pernr IS NOT INITIAL.

  IF i_begda IS INITIAL AND i_endda IS INITIAL.
    l_keydt = sy-datum.
  ELSE.
    IF sy-datum =< i_begda.
      l_keydt = i_begda.
    ELSEIF i_begda < sy-datum AND sy-datum < i_endda.
      l_keydt = sy-datum.
    ELSEIF sy-datum >= i_endda.
      l_keydt = i_endda.
    ELSE.
      l_keydt = sy-datum.
    ENDIF.
  ENDIF.

* export org, position, job code
  SELECT SINGLE orgeh plans stell FROM pa0001
    INTO (e_orgeh, e_plans, e_stell)
    WHERE pernr = i_pernr
      AND endda >= l_keydt
      AND begda <= l_keydt.

* export key date
  e_keydt = l_keydt.

ENDFUNCTION.
