function z_hr_ess_get_name_all .
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

  __cls : it_name, it_name_pre.

  data : from_date type datum,
         to_date type datum.

  if not pernr is initial.
    clear entire_list.
  endif.

  if entire_list eq true.

    select pernr nachn vorna aedtm begda into table it_name
     from pa0002  where endda eq '99991231'.

    loop at it_name.

*      SELECT SINGLE * FROM pa0001  WHERE pernr EQ it_name-pernr
*                           AND endda EQ '99991231'.
*      IF sy-subrc EQ 0.
*        IF pa0001-persg EQ '4'.
*          CONTINUE.
*        ENDIF.
*      ENDIF.

      zess_emp_tm_list-pernr = it_name-pernr.
      zess_emp_tm_list-lastname = it_name-nachn.
      zess_emp_tm_list-firstname = it_name-vorna.
      zess_emp_tm_list-begda = it_name-begda.
      append zess_emp_tm_list.

    endloop.

    return-type = 'S'.
    return-message = 'Success!'.
    append return.

  else.

    if pernr is initial.

      to_date = datum.
      from_date = to_date - 7.

      select pernr nachn vorna aedtm begda into table it_name
       from pa0002  where endda eq '99991231'
                      and aedtm between from_date and to_date.
    else.

      select pernr nachn vorna aedtm begda into table it_name
       from pa0002  where pernr eq pernr
                      and endda eq '99991231'.

    endif.

    if sy-subrc eq 0.

      select pernr nachn vorna aedtm begda into table it_name_pre
     from pa0002
     for all entries in it_name
     where pernr eq it_name-pernr
       and endda ne '99991231'.

      sort it_name_pre by pernr ascending
                          aedtm descending.
      delete adjacent duplicates from it_name_pre comparing pernr.

    endif.

    sort : it_name by pernr,
           it_name_pre by pernr.

    loop at it_name.

*      SELECT SINGLE * FROM pa0001  WHERE pernr EQ it_name-pernr
*                           AND endda EQ '99991231'.
*      IF sy-subrc EQ 0.
*        IF pa0001-persg EQ '4'.
*          CONTINUE.
*        ENDIF.
*      ENDIF.

    read table it_name_pre with key pernr = it_name-pernr binary search.

      if  ( it_name_pre-nachn ne it_name-nachn or
          it_name_pre-vorna ne it_name-vorna ) or sy-subrc ne 0.
        zess_emp_tm_list-pernr = it_name-pernr.
        zess_emp_tm_list-lastname = it_name-nachn.
        zess_emp_tm_list-firstname = it_name-vorna.
        zess_emp_tm_list-begda = it_name-begda.
        append zess_emp_tm_list.
      endif.

    endloop.
  endif.

  read table zess_emp_tm_list index 1.

  if sy-subrc eq 0.

    return-type = 'S'.
    return-message = 'Success!'.
    append return.

  endif.

endfunction.
