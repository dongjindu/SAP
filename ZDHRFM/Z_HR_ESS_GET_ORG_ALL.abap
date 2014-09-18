FUNCTION z_hr_ess_get_org_all.
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

  __cls : it_org, it_org_pre.

  DATA it_email LIKE it_org OCCURS 0 WITH HEADER LINE.
  DATA $ok.
  DATA : from_date TYPE datum,
         to_date TYPE datum.

  IF NOT pernr IS INITIAL.
    CLEAR entire_list.
  ENDIF.

  SELECT orgeh orgtx INTO TABLE it_org_tx
  FROM  t527x WHERE sprsl EQ sy-langu
                AND endda EQ '99991231'.
  SORT it_org_tx BY orgeh.

  IF entire_list EQ true.

    SELECT pernr persg persk orgeh aedtm begda INTO TABLE it_org
     FROM pa0001  WHERE endda EQ '99991231'.

    LOOP AT it_org.

*      SELECT SINGLE * FROM PA0001  WHERE PERNR EQ IT_ORG-PERNR
*                           AND ENDDA EQ '99991231'.
*      IF SY-SUBRC EQ 0.
*        IF PA0001-PERSG EQ '4'.
*          CONTINUE.
*        ENDIF.
*      ENDIF.

      zess_emp_tm_list-pernr = it_org-pernr.
      zess_emp_tm_list-persg = it_org-persg.
      zess_emp_tm_list-persk = it_org-persk.
      zess_emp_tm_list-orgeh = it_org-orgeh.
      zess_emp_tm_list-begda = it_org-begda.
      READ TABLE it_org_tx WITH KEY orgeh = it_org-orgeh
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        zess_emp_tm_list-orgtx = it_org_tx-orgtx.
      ENDIF.

      CLEAR zess_emp_tm_list-email.

      SELECT SINGLE usrid_long INTO zess_emp_tm_list-email
      FROM pa0105 WHERE pernr EQ it_org-pernr
                    AND usrty EQ '0010'
                    AND endda EQ '99991231'.
      APPEND zess_emp_tm_list.

    ENDLOOP.

    return-type = 'S'.
    return-message = 'Success!'.
    APPEND return.

  ELSE.

    IF pernr IS INITIAL.

      to_date = datum.
      from_date = to_date - 7.

      SELECT pernr INTO TABLE it_email
       FROM pa0105  WHERE endda EQ '99991231'
                      AND aedtm BETWEEN from_date AND to_date.
    ELSE.

      SELECT pernr INTO TABLE it_email
       FROM pa0105  WHERE pernr EQ pernr
                     AND endda EQ '99991231'.


    ENDIF.

    IF NOT it_email[]  IS INITIAL.
      SORT it_email BY pernr.

      SELECT pernr persg persk orgeh aedtm begda INTO TABLE it_org
       FROM pa0001
       FOR ALL ENTRIES IN it_email
       WHERE endda EQ '99991231'
                 AND ( aedtm BETWEEN from_date AND to_date OR
                       pernr EQ it_email-pernr ) .
    ELSE.
      SELECT pernr persg persk orgeh aedtm begda INTO TABLE it_org
       FROM pa0001  WHERE endda EQ '99991231'
                      AND aedtm BETWEEN from_date AND to_date.
    ENDIF.
    IF sy-subrc EQ 0.

      SELECT pernr persg persk  orgeh aedtm begda INTO TABLE it_org_pre
     FROM pa0001
     FOR ALL ENTRIES IN it_org
     WHERE pernr EQ it_org-pernr
       AND endda NE '99991231'.

      SORT it_org_pre BY pernr ASCENDING
                         aedtm DESCENDING.
      DELETE ADJACENT DUPLICATES FROM it_org_pre COMPARING pernr.

    ENDIF.

    SORT : it_org BY pernr,
           it_org_pre BY pernr.

    LOOP AT it_org.

*      SELECT SINGLE * FROM PA0001  WHERE PERNR EQ IT_ORG-PERNR
*                           AND ENDDA EQ '99991231'.
*      IF SY-SUBRC EQ 0.
*        IF PA0001-PERSG EQ '4'.
*          CONTINUE.
*        ENDIF.
*      ENDIF.

      CLEAR $ok.
      READ TABLE it_org_pre WITH KEY pernr = it_org-pernr BINARY SEARCH.

      IF  ( it_org_pre-orgeh NE it_org-orgeh ) OR sy-subrc NE 0.
        $ok = true.
      ELSE.

*        if IT_ORG_PRE-aedtm eq IT_ORG-aedtm.
*          $ok = true.
*        endif.
*
        READ TABLE it_email WITH KEY pernr = it_org-pernr BINARY SEARCH.
        IF sy-subrc EQ 0.
          $ok = true.
        ENDIF.

      ENDIF.
      IF $ok EQ true.
        zess_emp_tm_list-pernr = it_org-pernr.
        zess_emp_tm_list-persg = it_org-persg.
        zess_emp_tm_list-persk = it_org-persk.
        zess_emp_tm_list-orgeh = it_org-orgeh.
        zess_emp_tm_list-begda = it_org-begda.

        READ TABLE it_org_tx WITH KEY orgeh = it_org-orgeh
        BINARY SEARCH.
        IF sy-subrc EQ 0.
          zess_emp_tm_list-orgtx = it_org_tx-orgtx.
        ENDIF.
        CLEAR zess_emp_tm_list-email.
        SELECT SINGLE usrid_long INTO zess_emp_tm_list-email
        FROM pa0105 WHERE pernr EQ it_org-pernr
                      AND usrty EQ '0010'
                      AND endda EQ '99991231'.

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
