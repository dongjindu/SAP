FUNCTION zfhr_get_personal_info.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_PERNR) TYPE  PERSNO
*"     REFERENCE(I_BEGDA) TYPE  HAP_AP_START_DATE OPTIONAL
*"     REFERENCE(I_ENDDA) TYPE  HAP_AP_END_DATE OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_PINFO) TYPE  ZSHR_PINFO
*"     REFERENCE(E_KEYDT) TYPE  SY-DATUM
*"----------------------------------------------------------------------

  TABLES: pa0000, pa0001.

  DATA: l_keydt TYPE sy-datum.

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

* check active employee
  SELECT SINGLE * FROM pa0000
    WHERE pernr = i_pernr
** On 02/25/14  only select active employee
       AND endda >= sy-datum
       AND begda <= sy-datum
*      AND endda >= l_keydt
*      AND begda <= l_keydt
** End
      AND stat2 = '3'.
  IF sy-subrc = 0.
*   get pa0001
    SELECT SINGLE * FROM pa0001
      WHERE pernr = pa0000-pernr
        AND endda >= l_keydt
        AND begda <= l_keydt.
    MOVE-CORRESPONDING pa0001 TO es_pinfo.
    MOVE: pa0000-stat2 TO es_pinfo-stat2.
  ENDIF.

* export key date
  e_keydt = l_keydt.

ENDFUNCTION.
