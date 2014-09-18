FUNCTION z_hr_ess_get_workflow.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"     VALUE(DATUM) TYPE  DATUM DEFAULT SY-DATUM
*"  TABLES
*"      ZESS_EMP_WORK_FLOW STRUCTURE  ZESS_EMP_WORK_FLOW
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  DATA : $level(2) TYPE n,
         $grp_name(40),
         $objid LIKE hrp1000-objid,
         $orgeh LIKE pa0001-orgeh,
         $position(40),
         $plans LIKE pa0001-plans,
         $division(40).

  SELECT SINGLE * FROM pa0001 WHERE pernr EQ employee_number
                         AND endda EQ '99991231'.
  IF sy-subrc NE 0.
    return-type = 'E'.
    return-message = 'Invalid Employee Number'.
    APPEND return.
    EXIT.
  ENDIF.

  SELECT SINGLE stext INTO zess_emp_work_flow-org_name FROM hrp1000
  WHERE objid EQ pa0001-orgeh
  AND endda EQ '99991231'
  AND otype EQ 'O'
  AND langu EQ sy-langu.

  $plans = pa0001-plans.

  PERFORM get_pos_cd USING pa0001-plans
                           pa0001-orgeh
                           datum
                  CHANGING $level
                           $grp_name
                           $position.

  IF $position CP '*COORDINATOR OF LEGAL*'.
    $level = '01'.
  ENDIF.

  zess_emp_work_flow-level = $level.
  zess_emp_work_flow-costcenter = pa0001-kostl.
  SELECT SINGLE ltext INTO zess_emp_work_flow-costcenter_text FROM cskt
                             WHERE spras EQ sy-langu
                                AND kokrs EQ 'H201'
                                AND kostl EQ pa0001-kostl
                                AND datbi EQ '99991231'.

  IF $level EQ '01' AND $position CP  '*COORDI*'.

    SELECT SINGLE objid INTO $objid FROM hrp1001
    WHERE objid EQ pa0001-plans
    AND endda EQ '99991231'
    AND otype EQ 'S'
    AND subty EQ 'A012'.

    IF sy-subrc EQ 0.
      $level = '02'.
    ELSE.
      $level = '04'.
    ENDIF.
  ENDIF.


  IF $position CP '*COORDINATOR OF QUALITY ASSURANCE*'.
    SELECT SINGLE objid INTO $objid FROM hrp1001
    WHERE objid EQ pa0001-plans
    AND endda EQ '99991231'
    AND otype EQ 'S'
    AND subty EQ 'A012'.

    IF sy-subrc EQ 0.
      $level = '01'.
    ELSE.
      $level = '03'.
    ENDIF.

  ENDIF.

  CASE $level.
    WHEN '00'.
      zess_emp_work_flow-hod_name = 'N/A'.
      zess_emp_work_flow-hod_org_name = 'N/A'.
      zess_emp_work_flow-cord_name = 'N/A'.

    WHEN '01'.

      SELECT SINGLE objid INTO $objid FROM hrp1000
      WHERE mc_stext LIKE '%PRESIDENT OF HMMA%'
      AND endda EQ '99991231'
      AND otype EQ 'S'
      AND langu EQ sy-langu.

      IF sy-subrc EQ 0.
        PERFORM get_hod_name USING $objid
                          CHANGING zess_emp_work_flow-hod_pernr
                                   zess_emp_work_flow-hod_name
                                   zess_emp_work_flow-hod_org_name.
      ENDIF.

    WHEN '02'.
      $orgeh = pa0001-orgeh.
      PERFORM get_parent_level USING $orgeh
                                     '*DIVISION*'
                            CHANGING zess_emp_work_flow-hod_pernr
                                  zess_emp_work_flow-hod_name
                                  zess_emp_work_flow-hod_org_name.
      IF $position CP  '*COORDI*'.
      ELSE.
        $orgeh = pa0001-orgeh.
        PERFORM get_parent_level_coor USING $orgeh
                              CHANGING zess_emp_work_flow-cord_pernr
                                       zess_emp_work_flow-cord_name
                                       zess_emp_work_flow-cord_org_name.
      ENDIF.

    WHEN '03'.

      $orgeh = pa0001-orgeh.

      IF $position CP '*COORDINATOR OF QUALITY ASSURANCE*'.

        SELECT SINGLE * FROM hrp1001
                    WHERE  plvar EQ '01'
                      AND  otype EQ 'S'
                      AND  istat EQ '1'
                      AND  begda <= datum
                      AND  endda >= datum
                      AND  subty EQ 'A002'
                      AND  sclas EQ 'S'
                      AND  objid EQ $plans.

        IF sy-subrc EQ 0.
        ELSE.

          SELECT SINGLE objid INTO $objid FROM hrp1001
          WHERE sobid EQ $orgeh
          AND endda EQ '99991231'
          AND otype EQ 'S'
          AND subty EQ 'A012'.

          IF sy-subrc EQ 0.

            SELECT SINGLE mc_stext INTO $division FROM hrp1000
            WHERE objid EQ $orgeh
            AND endda EQ '99991231'
            AND otype EQ 'O'
            AND langu EQ sy-langu.

            IF sy-subrc EQ 0.
              IF $division CP '*COORDINATOR*'.
                SELECT SINGLE objid INTO $objid FROM hrp1001
                WHERE sobid EQ $objid
                AND endda EQ '99991231'
                AND otype EQ 'P'
                AND subty EQ 'B008'.


                SELECT SINGLE * FROM pa0002 WHERE pernr EQ $objid
                                       AND endda EQ '99991231'.

                CONCATENATE pa0002-nachn pa0002-vorna
                   INTO zess_emp_work_flow-hod_name SEPARATED BY space .

                zess_emp_work_flow-hod_pernr = $objid.

                SELECT SINGLE * FROM pa0001 INTO *pa0001
                          WHERE pernr EQ $objid
                                       AND endda EQ '99991231'.

               SELECT SINGLE stext INTO zess_emp_work_flow-hod_org_name
                   FROM hrp1000
                  WHERE objid EQ *pa0001-orgeh
                  AND endda EQ '99991231'
                  AND otype EQ 'O'
                  AND langu EQ sy-langu.

              ENDIF.
            ENDIF.

          ENDIF.

        ENDIF.

      ELSE.

        PERFORM get_parent_level USING $orgeh
                                       '*DEPARTMENT*'
                              CHANGING zess_emp_work_flow-hod_pernr
                                    zess_emp_work_flow-hod_name
                                    zess_emp_work_flow-hod_org_name.

        IF $position CP  '*COORDI*'.
        ELSE.
          $orgeh = pa0001-orgeh.
          PERFORM get_parent_level_coor USING $orgeh
                                CHANGING zess_emp_work_flow-cord_pernr
                                         zess_emp_work_flow-cord_name
                                         zess_emp_work_flow-cord_org_name.
        ENDIF.

      ENDIF.

    WHEN '04'.

      $orgeh = pa0001-orgeh.

      SELECT SINGLE * FROM hrp1001
                  WHERE  plvar EQ '01'
                    AND  otype EQ 'S'
                    AND  istat EQ '1'
                    AND  begda <= datum
                    AND  endda >= datum
                    AND  subty EQ 'A002'
                    AND  sclas EQ 'S'
                    AND  objid EQ $plans.

      IF sy-subrc EQ 0.

      ELSE.

        SELECT SINGLE objid INTO $objid FROM hrp1001
        WHERE sobid EQ $orgeh
        AND endda EQ '99991231'
        AND otype EQ 'S'
        AND subty EQ 'A012'.

        IF sy-subrc EQ 0.

          SELECT SINGLE mc_stext INTO $division FROM hrp1000
          WHERE objid EQ $orgeh
          AND endda EQ '99991231'
          AND otype EQ 'O'
          AND langu EQ sy-langu.

          IF sy-subrc EQ 0.

*            IF $division CP '*COORDINATOR*'.
            SELECT SINGLE objid INTO $objid FROM hrp1001
            WHERE sobid EQ $objid
            AND endda EQ '99991231'
            AND otype EQ 'P'
            AND subty EQ 'B008'.


            SELECT SINGLE * FROM pa0002 WHERE pernr EQ $objid
                                   AND endda EQ '99991231'.

            CONCATENATE pa0002-nachn pa0002-vorna
               INTO zess_emp_work_flow-hod_name SEPARATED BY space .

            zess_emp_work_flow-hod_pernr = $objid.

            SELECT SINGLE * FROM pa0001 INTO *pa0001
                      WHERE pernr EQ $objid
                                   AND endda EQ '99991231'.

            SELECT SINGLE stext INTO zess_emp_work_flow-hod_org_name
             FROM hrp1000
            WHERE objid EQ *pa0001-orgeh
            AND endda EQ '99991231'
            AND otype EQ 'O'
            AND langu EQ sy-langu.

*            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.

  ENDCASE.

  IF $position CP '*COORDINATOR OF LEGAL*'.

    SELECT SINGLE objid INTO $objid
                FROM hrp1000
                      WHERE otype EQ 'O'
                        AND plvar EQ '01'
                        AND istat EQ '1'
                        AND begda <= datum
                        AND endda EQ '99991231'
                        AND mc_stext LIKE '%HR%DIVISION%'.
    IF sy-subrc EQ 0.

      SELECT SINGLE objid INTO $objid FROM hrp1001
      WHERE sobid EQ $objid
      AND endda EQ '99991231'
      AND otype EQ 'S'
      AND subty EQ 'A012'.

      IF sy-subrc EQ 0.

        SELECT SINGLE objid INTO $objid FROM hrp1001
        WHERE sobid EQ $objid
        AND endda EQ '99991231'
        AND otype EQ 'P'
        AND subty EQ 'B008'.

        IF sy-subrc EQ 0.


          SELECT SINGLE * FROM pa0002 WHERE pernr EQ $objid
                                 AND endda EQ '99991231'.

          CONCATENATE pa0002-nachn pa0002-vorna
              INTO zess_emp_work_flow-hod_name SEPARATED BY space .

          zess_emp_work_flow-hod_pernr = $objid.

          SELECT SINGLE * FROM pa0001 INTO *pa0001
                    WHERE pernr EQ $objid
                                 AND endda EQ '99991231'.

          SELECT SINGLE stext INTO zess_emp_work_flow-hod_org_name
           FROM hrp1000
          WHERE objid EQ *pa0001-orgeh
          AND endda EQ '99991231'
          AND otype EQ 'O'
          AND langu EQ sy-langu.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.


  APPEND zess_emp_work_flow.

  return-type = 'S'.
  return-message = 'Sucess!'.
  APPEND return.

ENDFUNCTION.
