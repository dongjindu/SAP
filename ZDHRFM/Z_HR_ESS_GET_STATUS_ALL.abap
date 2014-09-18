FUNCTION z_hr_ess_get_status_all.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ENTIRE_LIST) TYPE  CHAR1 OPTIONAL
*"     VALUE(DATUM) TYPE  DATUM DEFAULT SY-DATUM
*"     VALUE(PERNR) TYPE  P_PERNR OPTIONAL
*"  TABLES
*"      ZESS_EMP_TM_LIST STRUCTURE  ZESS_EMP_TM_LIST
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  __cls : it_emp_status, it_emp_status_pre.

  DATA : from_date TYPE datum,
         to_date TYPE datum.

  IF NOT pernr IS INITIAL.
    CLEAR entire_list.
  ENDIF.

  IF entire_list EQ true.

    SELECT pernr stat2 aedtm begda INTO TABLE it_emp_status
     FROM pa0000  WHERE endda EQ '99991231'.

    LOOP AT it_emp_status.

*      SELECT SINGLE * FROM PA0001  WHERE PERNR EQ IT_EMP_STATUS-PERNR
*                           AND ENDDA EQ '99991231'.
*      IF SY-SUBRC EQ 0.
*        IF PA0001-PERSG EQ '4'.
*          CONTINUE.
*        ENDIF.
*      ENDIF.

      zess_emp_tm_list-pernr = it_emp_status-pernr.
      zess_emp_tm_list-stat2 = it_emp_status-stat2.
      zess_emp_tm_list-begda = it_emp_status-begda.
      APPEND zess_emp_tm_list.

    ENDLOOP.

    return-type = 'S'.
    return-message = 'Success!'.
    APPEND return.

  ELSE.

    IF pernr IS INITIAL.

      to_date = datum.
      from_date = to_date - 7.


      SELECT pernr stat2 aedtm begda INTO TABLE it_emp_status
       FROM pa0000  WHERE endda EQ '99991231'
                      AND aedtm BETWEEN from_date AND to_date.
    ELSE.

      SELECT pernr stat2 aedtm begda INTO TABLE it_emp_status
       FROM pa0000  WHERE pernr = pernr
                      AND endda EQ '99991231'.
    ENDIF.

    IF sy-subrc EQ 0.

      SELECT pernr stat2 aedtm begda INTO TABLE it_emp_status_pre
     FROM pa0000
     FOR ALL ENTRIES IN it_emp_status
     WHERE pernr EQ it_emp_status-pernr
       AND endda NE '99991231'.


      SORT it_emp_status_pre BY pernr ASCENDING
                          aedtm DESCENDING.
      DELETE ADJACENT DUPLICATES FROM it_emp_status_pre COMPARING pernr.

    ENDIF.

    SORT : it_emp_status BY pernr,
           it_emp_status_pre BY pernr.

    LOOP AT it_emp_status.

*      SELECT SINGLE * FROM pa0001  WHERE pernr EQ it_emp_status-pernr
*                           AND endda EQ '99991231'.
*      IF sy-subrc EQ 0.
*        IF pa0001-persg EQ '4'.
*          CONTINUE.
*        ENDIF.
*      ENDIF.

      READ TABLE it_emp_status_pre WITH KEY pernr = it_name-pernr
      BINARY SEARCH.

      IF  ( it_emp_status_pre-stat2 NE it_emp_status-stat2 ) OR
      sy-subrc NE o.
        zess_emp_tm_list-pernr = it_emp_status-pernr.
        zess_emp_tm_list-stat2 = it_emp_status-stat2.
        zess_emp_tm_list-begda = it_emp_status-begda.
        APPEND zess_emp_tm_list.
      ENDIF.

    ENDLOOP.
  ENDIF.

  READ TABLE zess_emp_tm_list INDEX 1.

  IF sy-subrc EQ 0.

    return-type = 'S'.
    return-message = 'Success!'.
    APPEND return.

  ENDIF.

ENDFUNCTION.
