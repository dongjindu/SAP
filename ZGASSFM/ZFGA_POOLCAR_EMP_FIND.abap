FUNCTION zfga_poolcar_emp_find.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_FIRST) TYPE  PA0002-VORNA OPTIONAL
*"     VALUE(I_LAST) TYPE  PA0002-NACHN OPTIONAL
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  BAPIRETURN
*"  TABLES
*"      T_DATA STRUCTURE  ZSGA_EMP_EMAIL...
*"----------------------------------------------------------------------

  DATA : BEGIN OF it_emp OCCURS 0.
          INCLUDE STRUCTURE zsga_emp_email.
  DATA : endda LIKE t527x-endda.
  DATA : endda2 LIKE pa0002-endda.
  DATA : END OF it_emp.

  DATA :  l_cond(20), l_cond2(20).

  CLEAR : v_conditions, v_conditions2.

  IF t_data[] IS  INITIAL.
    IF i_first IS NOT INITIAL.
      CONDENSE i_first.
      TRANSLATE i_first TO UPPER CASE.
      CONCATENATE `'%`  i_first  `%'` INTO l_cond.
      CONCATENATE 'C~VNAMC LIKE ' l_cond INTO v_conditions
                                              SEPARATED BY space.
    ENDIF.

    IF i_last IS NOT INITIAL.
      CONDENSE i_last.
      TRANSLATE i_last TO UPPER CASE.
      CONCATENATE `'%`  i_last  `%'` INTO l_cond2.
      CONCATENATE 'C~NCHMC LIKE ' l_cond2 INTO v_conditions2
                                              SEPARATED BY space.
    ENDIF.

    TRY.
        SELECT  a~pernr    a~orgeh  b~orgtx b~endda  c~nachn AS lastname
                c~vorna AS firstname c~endda AS endda2
          INTO CORRESPONDING FIELDS OF TABLE it_emp
        FROM pa0001 AS a  LEFT OUTER JOIN t527x AS b
                          ON a~orgeh  = b~orgeh
*                          LEFT OUTER JOIN pa0002 AS c
                          INNER JOIN pa0002 AS c
                          ON a~pernr  = c~pernr
        WHERE (v_conditions)
          AND (v_conditions2)
          AND a~endda = '99991231'.

      CATCH cx_sy_dynamic_osql_syntax.
        e_return-type = 'E'.
        e_return-message = 'SQL Error. Please contact IT person'.
        EXIT.
    ENDTRY.

  ELSE.     "find list with e-mail info(GA team members)
    TRY.
        SELECT  a~pernr    a~orgeh  b~orgtx b~endda  c~nachn AS lastname
                c~vorna AS firstname c~endda AS endda2
          INTO CORRESPONDING FIELDS OF TABLE it_emp

        FROM pa0001 AS a  LEFT OUTER JOIN t527x AS b
                          ON a~orgeh  = b~orgeh
                          LEFT OUTER JOIN pa0002 AS c
                          ON a~pernr  = c~pernr
          FOR ALL ENTRIES IN t_data
        WHERE a~pernr = t_data-pernr
          AND a~endda = '99991231'.

      CATCH cx_sy_dynamic_osql_syntax.
        e_return-type = 'E'.
        e_return-message = 'SQL Error. Please contact IT person'.
        EXIT.
    ENDTRY.
  ENDIF.

  DELETE it_emp WHERE endda  <> '99991231'.
  DELETE it_emp WHERE endda2 <> '99991231'.

  CLEAR : t_data[], t_data.
  IF it_emp[] IS INITIAL.
    e_return-type    = 'E'.
    e_return-message = 'No Employee infomation'.
  ELSE.
    LOOP AT it_emp.
      MOVE-CORRESPONDING it_emp TO t_data.

      SELECT SINGLE usrid_long INTO t_data-email
      FROM pa0105
      WHERE pernr = it_emp-pernr
        AND subty = '0010'
        AND endda = '99991231'.
      APPEND t_data. CLEAR : t_data.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
