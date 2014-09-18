FUNCTION zfga_poolcar_coop_find.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_PERNR) TYPE  BAPI7004-PERNR
*"     VALUE(I_FIRST) TYPE  PA0002-VORNA OPTIONAL
*"     VALUE(I_LAST) TYPE  PA0002-NACHN OPTIONAL
*"     VALUE(I_ORGTX) TYPE  CHAR40 OPTIONAL
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  BAPIRETURN
*"  TABLES
*"      T_DATA STRUCTURE  ZSGA_EMP_EMAIL
*"----------------------------------------------------------------------

*-02.25.2013 Only find manager and above

  DATA : BEGIN OF it_emp OCCURS 0.
          INCLUDE STRUCTURE zsga_emp_email.
  DATA : endda  LIKE t527x-endda.
  DATA : endda2 LIKE pa0002-endda.
  DATA : stell  LIKE pa0001-stell.
  DATA : orgeh  LIKE pa0001-orgeh.
  DATA : END OF it_emp.

  DATA : it_hrp1000 LIKE hrp1000 OCCURS 0 WITH HEADER LINE.
  DATA : it_stru_tmp LIKE struc OCCURS 0 WITH HEADER LINE.
  DATA : it_stru     LIKE struc OCCURS 0 WITH HEADER LINE.
  DATA : it_appr_order LIKE ztga_btappr_ordr OCCURS 0 WITH HEADER LINE.

  DATA : l_cond(40), l_cond2(40),
         l_object_seq LIKE ztga_btappr_ordr-object_seq,
         comp_orgtx   TYPE char40,
         l_sobid LIKE hrp1001-sobid,
         l_name  TYPE pa0002-nachn,
         chk_last, chk_first.

*  IF t_data[] IS  INITIAL.
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

  CLEAR : l_cond, l_cond2.
  IF i_orgtx IS NOT INITIAL.
    CONDENSE i_orgtx.
    TRANSLATE i_orgtx TO UPPER CASE.
    CONCATENATE `'%`  i_orgtx  `%'` INTO l_cond.
    CONCATENATE 'MC_STEXT LIKE ' l_cond INTO v_conditions3
                                            SEPARATED BY space.
  ENDIF.

  IF i_orgtx IS INITIAL.
    TRY.
        SELECT  a~pernr    a~orgeh  c~nachn AS lastname  "b~stext AS orgtx  b~endda
                c~vorna AS firstname c~endda AS endda2 a~stell d~stext AS job
          INTO CORRESPONDING FIELDS OF TABLE it_emp
        FROM pa0001 AS a
*          LEFT OUTER JOIN t527x AS b
*                          ON a~orgeh  = b~orgeh
*                      LEFT OUTER JOIN hrp1000 AS b
*                          ON a~orgeh  = b~objid
                          INNER JOIN pa0002 AS c
                          ON a~pernr  = c~pernr
                          INNER JOIN hrp1000 AS d
                          ON a~stell  = d~objid
        WHERE (v_conditions)
          AND (v_conditions2)
          AND a~endda = '99991231'
          AND c~endda = '99991231'
          AND a~plans <> '99999999'
          AND d~otype EQ 'C'
          AND d~plvar EQ '01'
          AND d~istat EQ '1'
          AND d~endda EQ '99991231'.


      CATCH cx_sy_dynamic_osql_syntax.
        e_return-type = 'E'.
        e_return-message = 'SQL Error. Please contact IT person'.
        EXIT.
    ENDTRY.
  ELSE.
    TRY.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE it_hrp1000
        FROM hrp1000
        WHERE otype EQ 'O'
          AND plvar EQ '01'
          AND istat EQ '1'
          AND begda <= sy-datum
          AND endda EQ '99991231'
          AND (v_conditions3).

      CATCH cx_sy_dynamic_osql_syntax.
        e_return-type = 'E'.
        e_return-message = 'SQL Error. Please contact IT person'.
        EXIT.
    ENDTRY.
  ENDIF.

*  ELSE.     "find list with e-mail info(GA team members)
*    TRY.
*        SELECT  a~pernr    a~orgeh  c~nachn AS lastname " b~stext AS orgtx b~endda
*                c~vorna AS firstname c~endda AS endda2  a~stell d~stext AS job
*          INTO CORRESPONDING FIELDS OF TABLE it_emp
*        FROM pa0001 AS a
**          LEFT OUTER JOIN t527x AS b
**                          ON a~orgeh  = b~orgeh
**                      LEFT OUTER JOIN hrp1000 AS b
**                          ON a~orgeh  = b~objid
*                          INNER JOIN pa0002 AS c
*                          ON a~pernr  = c~pernr
*                          INNER JOIN hrp1000 AS d
*                          ON a~stell  = d~objid
*          FOR ALL ENTRIES IN t_data
*        WHERE a~pernr = t_data-pernr
*          AND a~endda = '99991231'
*          AND c~endda = '99991231'
*          AND a~plans <> '99999999'   "not valid TM #
*          AND d~otype EQ 'C'
*          AND d~plvar EQ '01'
*          AND d~istat EQ '1'
*          AND d~endda EQ '99991231'.
*
*      CATCH cx_sy_dynamic_osql_syntax.
*        e_return-type = 'E'.
*        e_return-message = 'SQL Error. Please contact IT person'.
*        EXIT.
*    ENDTRY.
*  ENDIF.

*  DELETE it_emp WHERE endda  <> '99991231'.
*  DELETE it_emp WHERE endda2 <> '99991231'.

  SELECT SINGLE b~object_seq INTO l_object_seq
  FROM pa0001 AS a INNER JOIN ztga_btappr_ordr AS b
                  ON a~stell  = b~object_id
  WHERE a~pernr = i_pernr
    AND a~endda = '99991231'.
  IF sy-subrc <> 0.
    e_return-type = 'E'.
    e_return-message = 'No Job found for this Requestor'.
    EXIT.
  ENDIF.

*-  manager and above
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_appr_order
  FROM ztga_btappr_ordr
  WHERE object_seq >= '17'
  AND object_seq   >=  l_object_seq.

****----In case user doesn't input Department
  IF i_orgtx IS INITIAL.
    CLEAR : t_data[], t_data.
    IF it_emp[] IS INITIAL.
      e_return-type    = 'E'.
      e_return-message = 'No Employee infomation'.
    ELSE.
      LOOP AT it_emp.
        SELECT SINGLE stext INTO it_emp-orgtx FROM hrp1000
        WHERE objid EQ it_emp-orgeh
          AND endda EQ '99991231'
          AND otype EQ 'O'
          AND langu EQ sy-langu.

        READ TABLE it_appr_order WITH KEY object_id = it_emp-stell.
        IF sy-subrc <> 0.
          CONTINUE.   "Ignore below level
        ENDIF.

        IF i_orgtx IS NOT INITIAL.
          comp_orgtx = it_emp-orgtx.
          TRANSLATE comp_orgtx TO UPPER CASE.
          IF comp_orgtx NS i_orgtx.   "Contain No String
            CONTINUE.
          ENDIF.
        ENDIF.

        MOVE-CORRESPONDING it_emp TO t_data.

        SELECT SINGLE usrid_long INTO t_data-email
        FROM pa0105
        WHERE pernr = it_emp-pernr
          AND subty = '0010'
          AND endda = '99991231'.
        APPEND t_data. CLEAR : t_data.
      ENDLOOP.
    ENDIF.

****----In case User input Department
  ELSE.
    LOOP AT it_hrp1000.
*-List of all sub-level org-Units
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = 'O'
          act_objid      = it_hrp1000-objid
          act_wegid      = 'ORGEH'
          act_plvar      = '01'
          act_begda      = sy-datum
          act_endda      = sy-datum
*         act_depth      = 3
        TABLES
          result_struc   = it_stru_tmp
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.

      APPEND LINES OF it_stru_tmp TO it_stru.
      CLEAR : it_stru_tmp[], it_stru_tmp.

*-List of all higher-level Org-Units
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = 'O'
          act_objid      = it_hrp1000-objid
          act_wegid      = 'O-O'
          act_plvar      = '01'
          act_begda      = sy-datum
          act_endda      = sy-datum
        TABLES
          result_struc   = it_stru_tmp
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.
      APPEND LINES OF it_stru_tmp TO it_stru.
      CLEAR : it_stru_tmp[], it_stru_tmp .
    ENDLOOP.

    SORT it_stru BY objid.
    DELETE ADJACENT DUPLICATES FROM it_stru COMPARING objid.

    LOOP AT it_stru.
*-  relevant coordinator
      CLEAR : l_sobid.
      SELECT SINGLE sobid INTO l_sobid FROM hrp1001
      WHERE plvar =  '01'
        AND istat = '1'
        AND otype = 'O'
        AND endda = '99991231'
        AND subty = 'BZ01'
        AND sclas = 'S'
        AND objid =  it_stru-objid.

      IF l_sobid IS NOT INITIAL.
        CLEAR : hrp1001.
        SELECT SINGLE * FROM hrp1001
              WHERE  plvar EQ '01'
                AND  otype EQ 'S'
                AND  istat EQ '1'
                AND  begda <= sy-datum
                AND  endda >= sy-datum
                AND  subty EQ 'A008'
                AND  sclas EQ 'P'
                AND  objid EQ l_sobid.
        IF sy-subrc = 0.
          t_data-pernr  = hrp1001-sobid.

          SELECT SINGLE * FROM pa0001 WHERE pernr EQ hrp1001-sobid
                                 AND endda EQ '99991231'.

*          CHECK pa0001-plans <> '99999999'.   "not valid TM #

          SELECT SINGLE stext INTO t_data-orgtx FROM hrp1000
          WHERE objid EQ pa0001-orgeh
          AND endda EQ '99991231'
          AND otype EQ 'O'
          AND langu EQ sy-langu.

          SELECT SINGLE stext INTO t_data-job
          FROM hrp1000
          WHERE otype EQ 'C'
            AND plvar EQ '01'
            AND istat EQ '1'
            AND objid EQ pa0001-stell
            AND endda EQ '99991231'.

          SELECT SINGLE usrid_long INTO t_data-email
          FROM pa0105
          WHERE pernr = t_data-pernr
            AND subty = '0010'
            AND endda = '99991231'.

          SELECT SINGLE * FROM pa0002 WHERE pernr EQ hrp1001-sobid
                                 AND endda EQ '99991231'.

          t_data-lastname   = pa0002-nachn.
          t_data-firstname  = pa0002-vorna.

          IF i_last IS NOT INITIAL.
            l_name  = t_data-lastname.
            TRANSLATE l_name TO UPPER CASE.
            IF l_name NS i_last.   "Contain  Not String
              chk_last = 'N'.
            ENDIF.
          ENDIF.

          IF i_first IS NOT INITIAL.
            l_name  = t_data-firstname.
            TRANSLATE l_name TO UPPER CASE.
            IF l_name NS i_first.   "Contain  Not String
              chk_first = 'N'.
            ENDIF.
          ENDIF.

          IF pa0001-plans <> '99999999'.   "not valid TM #
            IF i_last IS INITIAL AND i_first IS INITIAL.
              APPEND t_data. CLEAR : t_data.
            ELSE.
              IF chk_last <> 'N' AND chk_first <> 'N'.
                APPEND t_data. CLEAR : t_data.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR : chk_last, chk_first.
*   Jobs of the relevant organizational unit
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = 'O'
          act_objid      = it_stru-objid
          act_wegid      = 'O_S_C'
          act_plvar      = '01'
          act_begda      = sy-datum
          act_endda      = sy-datum
        TABLES
          result_struc   = it_stru_tmp
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.

      LOOP AT it_stru_tmp.
        IF it_stru_tmp-otype  = 'S'.  "Position
          CLEAR : hrp1001.
          SELECT SINGLE * FROM hrp1001
                WHERE  plvar EQ '01'
                  AND  otype EQ 'S'
                  AND  istat EQ '1'
                  AND  begda <= sy-datum
                  AND  endda >= sy-datum
                  AND  subty EQ 'A008'
                  AND  sclas EQ 'P'
                  AND  objid EQ it_stru_tmp-objid.
        ENDIF.
        IF it_stru_tmp-otype  = 'C'.  "Job
*        READ TABLE it_objid WITH KEY = it_stru_tmp-objid.
          READ TABLE it_appr_order WITH KEY object_id = it_stru_tmp-objid.
          IF sy-subrc = 0.
            CLEAR : t_data.

            CHECK hrp1001-sobid IS NOT INITIAL.

            READ TABLE t_data WITH KEY pernr  = hrp1001-sobid.
            IF sy-subrc = 0.  "Already exist
              CONTINUE.
            ENDIF.

            SELECT SINGLE stext INTO t_data-job
             FROM hrp1000
                   WHERE otype EQ 'C'
                     AND plvar EQ '01'
                     AND istat EQ '1'
                     AND objid EQ it_stru_tmp-objid
                     AND endda EQ '99991231'.

            t_data-pernr  = hrp1001-sobid.

            SELECT SINGLE * FROM pa0001 WHERE pernr EQ hrp1001-sobid
                                   AND endda EQ '99991231'.
            CHECK pa0001-plans <> '99999999'.   "not valid TM #

            SELECT SINGLE stext INTO t_data-orgtx FROM hrp1000
            WHERE objid EQ pa0001-orgeh
            AND endda EQ '99991231'
            AND otype EQ 'O'
            AND langu EQ sy-langu.

          SELECT SINGLE usrid_long INTO t_data-email
          FROM pa0105
          WHERE pernr = t_data-pernr
            AND subty = '0010'
            AND endda = '99991231'.

            SELECT SINGLE * FROM pa0002 WHERE pernr EQ hrp1001-sobid
                                   AND endda EQ '99991231'.
            t_data-lastname   = pa0002-nachn.
            t_data-firstname  = pa0002-vorna.

            IF i_last IS NOT INITIAL.
              l_name  = t_data-lastname.
              TRANSLATE l_name TO UPPER CASE.
              IF l_name NS i_last.   "Contain  Not String
                chk_last = 'N'.
              ENDIF.
            ENDIF.

            IF i_first IS NOT INITIAL.
              l_name  = t_data-firstname.
              TRANSLATE l_name TO UPPER CASE.
              IF l_name NS i_first.   "Contain  Not String
                chk_first = 'N'.
              ENDIF.
            ENDIF.

            IF i_last IS INITIAL AND i_first IS INITIAL.
              APPEND t_data. CLEAR : t_data.
            ELSE.
              IF chk_last <> 'N' AND chk_first <> 'N'.
                APPEND t_data. CLEAR : t_data.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
  ENDIF.

  DELETE  t_data WHERE pernr =  i_pernr.  "Delete requestor
  SORT t_data BY pernr.
  DELETE ADJACENT DUPLICATES FROM t_data COMPARING pernr.

  e_return-type = 'S'.
  e_return-message = 'Sucess!'.
ENDFUNCTION.
